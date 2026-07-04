//! Whole-corpus prevalence pipeline.
//!
//! For each `(work, parser)` pair: fetch (or compute and cache) the AAT JSON,
//! run every detector, accumulate per-row {works_with_feature,
//! total_occurrences, top-K work ids}.

use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
    sync::Mutex,
    time::Duration,
};

use anyhow::{Context, Result};
use rayon::prelude::*;
use serde_json::Value;

use crate::{
    adapter::AdapterBinary,
    cache::{AdapterFingerprintInputs, ParserCache, compute_adapter_sha, input_sha},
    detectors::{DetectorContext, DetectorRegistry},
    source_corpus::read_source_work,
};

#[derive(Debug, Clone)]
pub struct PrevalenceConfig {
    pub repo_root: PathBuf,
    pub corpus_root: PathBuf,
    pub cache_root: PathBuf,
    pub parsers: Vec<String>,
    pub work_ids: Vec<String>,
    pub adapter_timeout: Duration,
    pub use_cache: bool,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct RowPrevalence {
    pub works_with_feature: u64,
    pub total_occurrences: u64,
    pub coverage_basis: String,
    pub sample_works: Vec<String>,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct PerParserPrevalence {
    pub parser_id: String,
    pub adapter_sha: String,
    pub rows: BTreeMap<String, RowPrevalence>,
    pub works_processed: u64,
    pub works_failed: u64,
}

/// Run corpus-wide prevalence estimation for all configured parsers.
///
/// # Errors
///
/// Returns an error when adapter cache/index resolution fails or a work cannot be
/// processed due to IO/parsing/runtime issues.
pub fn run_prevalence(
    cfg: &PrevalenceConfig,
    registry: &DetectorRegistry,
    index_resolver: &dyn IndexResolver,
) -> Result<Vec<PerParserPrevalence>> {
    let cache = ParserCache::new(&cfg.cache_root);
    let row_ids: Vec<String> = registry.rows().map(|s| s.to_string()).collect();
    let mut results = Vec::new();
    for parser_id in &cfg.parsers {
        let inputs = AdapterFingerprintInputs::for_parser(&cfg.repo_root, parser_id)?;
        let adapter_sha = compute_adapter_sha(&inputs)?;
        let adapter = AdapterBinary::for_parser(&cfg.repo_root, parser_id)?;
        eprintln!(
            "[ab-coverage] {parser_id}: adapter_sha={} workers={}",
            &adapter_sha[..12],
            rayon::current_num_threads()
        );

        let counters: Vec<Mutex<RowCounter>> = row_ids
            .iter()
            .map(|id| Mutex::new(RowCounter::new(id.clone())))
            .collect();
        let processed = Mutex::new(0u64);
        let failed = Mutex::new(0u64);

        cfg.work_ids.par_iter().for_each(|work_id| {
            match process_work(
                work_id,
                &cfg.corpus_root,
                &adapter,
                &cache,
                &adapter_sha,
                cfg.adapter_timeout,
                cfg.use_cache,
                index_resolver,
            ) {
                Ok((aat, source)) => {
                    for (idx, rid) in row_ids.iter().enumerate() {
                        let occurrences = registry.detect(
                            rid,
                            &DetectorContext {
                                aat: &aat,
                                source: &source,
                            },
                        );
                        if occurrences > 0 {
                            counters[idx]
                                .lock()
                                .unwrap()
                                .observe(work_id.clone(), occurrences);
                        }
                    }
                    *processed.lock().unwrap() += 1;
                }
                Err(err) => {
                    eprintln!("[ab-coverage] {parser_id}: {work_id}: {err:#}");
                    *failed.lock().unwrap() += 1;
                }
            }
        });

        let basis = "full_corpus".to_string();
        let mut rows = BTreeMap::new();
        for counter in counters {
            let counter = counter.into_inner().unwrap();
            rows.insert(
                counter.row_id.clone(),
                RowPrevalence {
                    works_with_feature: counter.works_with_feature,
                    total_occurrences: counter.total_occurrences,
                    coverage_basis: basis.clone(),
                    sample_works: counter.top_works(),
                },
            );
        }
        results.push(PerParserPrevalence {
            parser_id: parser_id.clone(),
            adapter_sha,
            rows,
            works_processed: processed.into_inner().unwrap(),
            works_failed: failed.into_inner().unwrap(),
        });
    }
    Ok(results)
}

/// Maps a work_id to a corpus-relative path or `archive::entry`. The corpus
/// pipeline uses `ab-index` output as the canonical mapping.
pub trait IndexResolver: Sync {
    /// Resolve a work id to a source path or archive selector.
    ///
    /// # Errors
    ///
    /// Returns an error when the work id is unknown or resolution fails.
    fn resolve(&self, work_id: &str) -> Result<String>;
}

pub struct StaticIndexResolver {
    pub map: BTreeMap<String, String>,
}

impl IndexResolver for StaticIndexResolver {
    fn resolve(&self, work_id: &str) -> Result<String> {
        self.map
            .get(work_id)
            .cloned()
            .ok_or_else(|| anyhow::anyhow!("work_id not in index: {work_id}"))
    }
}

#[allow(clippy::too_many_arguments)]
fn process_work(
    work_id: &str,
    corpus_root: &Path,
    adapter: &AdapterBinary,
    cache: &ParserCache,
    adapter_sha: &str,
    timeout: Duration,
    use_cache: bool,
    index_resolver: &dyn IndexResolver,
) -> Result<(Value, String)> {
    let indexed_path = index_resolver.resolve(work_id)?;
    let source = read_source_work(corpus_root, work_id, &indexed_path)?;
    let key = input_sha(&source.bytes);

    let aat_bytes = if use_cache {
        if let Some(bytes) = cache.read(&adapter.parser_id, adapter_sha, &key)? {
            bytes
        } else {
            let bytes = adapter
                .invoke(&source.bytes, timeout)
                .with_context(|| format!("invoke {} on {work_id}", adapter.parser_id))?;
            cache.write(&adapter.parser_id, adapter_sha, &key, &bytes)?;
            bytes
        }
    } else {
        adapter
            .invoke(&source.bytes, timeout)
            .with_context(|| format!("invoke {} on {work_id}", adapter.parser_id))?
    };
    let aat: Value =
        serde_json::from_slice(&aat_bytes).with_context(|| format!("parse AAT for {work_id}"))?;
    Ok((aat, source.decoded.text))
}

#[derive(Debug)]
struct RowCounter {
    row_id: String,
    works_with_feature: u64,
    total_occurrences: u64,
    /// Heap-of-5: lowest count at top. Stored sorted ascending.
    samples: Vec<(u64, String)>,
}

impl RowCounter {
    fn new(row_id: String) -> Self {
        Self {
            row_id,
            works_with_feature: 0,
            total_occurrences: 0,
            samples: Vec::with_capacity(6),
        }
    }

    fn observe(&mut self, work_id: String, count: u64) {
        if count == 0 {
            return;
        }
        self.works_with_feature += 1;
        self.total_occurrences += count;
        self.samples.push((count, work_id));
        // Keep top 5 by occurrence (desc), ties broken by lex work_id ASC.
        self.samples
            .sort_by(|a, b| b.0.cmp(&a.0).then_with(|| a.1.cmp(&b.1)));
        self.samples.truncate(5);
    }

    fn top_works(self) -> Vec<String> {
        self.samples.into_iter().map(|(_, id)| id).collect()
    }
}
