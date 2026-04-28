//! `ab-coverage` — runs the prevalence pipeline.
//!
//! Inputs:
//!   --matrix    path to data/aozora-syntax-coverage.toml
//!   --index     path to ab-index JSON output (uses .works[] entries to
//!               resolve work_id -> indexed source path)
//!   --corpus    corpus root (default: references/aozorabunko)
//!   --work-ids  optional JSON file with a list of work_id strings; default
//!               is "all works in --index"
//!   --parsers   comma-separated parser ids (default: aozora2,aozora-rs,aozora2html)
//!   --cache-root  default target/parser-cache
//!   --no-cache  disable read+write of the parser cache
//!   --jobs      rayon thread count (default: cores)
//!   --timeout   per-work adapter timeout (default: 180s)
//!   --output    JSON summary path (default: scratch/ab-coverage-<ts>/summary.json)
//!   --merge     if set, also writes findings into --matrix as
//!               corpus_prevalence.{works_with_feature,total_occurrences,
//!               coverage_basis,sample_works}.

use std::{collections::BTreeMap, fs, path::PathBuf, time::Duration};

use ab_coverage::{
    detectors::DetectorRegistry,
    matrix::CoverageMatrix,
    merge::{PrevalenceFindings, PrevalenceRow, apply_prevalence_findings},
    prevalence::{PrevalenceConfig, StaticIndexResolver, run_prevalence},
};
use anyhow::{Context, Result, bail};
use clap::Parser;
use serde_json::Value;

#[derive(Parser, Debug)]
#[command(version, about = "Whole-corpus syntax prevalence pipeline.")]
struct Cli {
    #[arg(long, default_value = "data/aozora-syntax-coverage.toml")]
    matrix: PathBuf,
    #[arg(long)]
    index: PathBuf,
    #[arg(long, default_value = "references/aozorabunko")]
    corpus: PathBuf,
    #[arg(long)]
    work_ids: Option<PathBuf>,
    #[arg(long, default_value = "aozora2,aozora-rs,aozora2html")]
    parsers: String,
    #[arg(long, default_value = "target/parser-cache")]
    cache_root: PathBuf,
    #[arg(long)]
    no_cache: bool,
    #[arg(long)]
    jobs: Option<usize>,
    #[arg(long, default_value = "180")]
    timeout_secs: u64,
    #[arg(long)]
    output: Option<PathBuf>,
    #[arg(long)]
    merge: bool,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    if let Some(jobs) = cli.jobs {
        rayon::ThreadPoolBuilder::new()
            .num_threads(jobs)
            .build_global()
            .context("rayon thread pool")?;
    }

    let repo_root = std::env::current_dir().context("cwd")?;
    let matrix = CoverageMatrix::from_toml(&cli.matrix)
        .with_context(|| format!("load matrix {}", cli.matrix.display()))?;
    let registry = DetectorRegistry::from_matrix(matrix.rows());

    // Index resolver: load ab-index JSON, build work_id -> indexed_path map.
    let index_bytes =
        fs::read(&cli.index).with_context(|| format!("read index {}", cli.index.display()))?;
    let index_doc: Value = serde_json::from_slice(&index_bytes)
        .with_context(|| format!("parse index {}", cli.index.display()))?;
    let mut id_to_path: BTreeMap<String, String> = BTreeMap::new();
    if let Some(arr) = index_doc.get("works").and_then(Value::as_array) {
        for work in arr {
            let id = work
                .get("id")
                .and_then(Value::as_str)
                .ok_or_else(|| anyhow::anyhow!("work missing id"))?;
            let path = work
                .get("txt_path")
                .or_else(|| work.get("indexed_path"))
                .or_else(|| work.get("source_path"))
                .and_then(Value::as_str)
                .ok_or_else(|| anyhow::anyhow!("work {id} missing txt_path"))?;
            id_to_path.insert(id.to_string(), path.to_string());
        }
    } else {
        bail!("index has no .works array");
    }

    // Work-ids: explicit JSON list, or all index entries.
    let work_ids: Vec<String> = if let Some(path) = &cli.work_ids {
        let bytes = fs::read(path).with_context(|| format!("read {}", path.display()))?;
        let v: Value = serde_json::from_slice(&bytes)?;
        v.as_array()
            .ok_or_else(|| anyhow::anyhow!("--work-ids must be a JSON array"))?
            .iter()
            .filter_map(|x| x.as_str().map(|s| s.to_string()))
            .collect()
    } else {
        id_to_path.keys().cloned().collect()
    };

    let parsers: Vec<String> = cli
        .parsers
        .split(',')
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect();

    let cfg = PrevalenceConfig {
        repo_root: repo_root.clone(),
        corpus_root: cli.corpus.clone(),
        cache_root: cli.cache_root.clone(),
        parsers,
        work_ids,
        adapter_timeout: Duration::from_secs(cli.timeout_secs),
        use_cache: !cli.no_cache,
    };

    let resolver = StaticIndexResolver { map: id_to_path };
    let start = std::time::Instant::now();
    let results = run_prevalence(&cfg, &registry, &resolver)?;
    let elapsed = start.elapsed();
    eprintln!(
        "[ab-coverage] done in {:.1}s ({} parsers x {} works)",
        elapsed.as_secs_f64(),
        cfg.parsers.len(),
        cfg.work_ids.len()
    );

    // Aggregate across parsers: union sample_works, sum occurrences.
    let mut row_summary: BTreeMap<String, PrevalenceRow> = BTreeMap::new();
    for per in &results {
        for (rid, prev) in &per.rows {
            let entry = row_summary
                .entry(rid.clone())
                .or_insert_with(|| PrevalenceRow {
                    works_with_feature: 0,
                    total_occurrences: 0,
                    coverage_basis: Some("full_corpus".to_string()),
                    sample_works: Vec::new(),
                });
            entry.works_with_feature = entry.works_with_feature.max(prev.works_with_feature);
            entry.total_occurrences = entry.total_occurrences.max(prev.total_occurrences);
            for w in &prev.sample_works {
                if !entry.sample_works.contains(w) && entry.sample_works.len() < 5 {
                    entry.sample_works.push(w.clone());
                }
            }
        }
    }

    let summary = serde_json::json!({
        "elapsed_secs": elapsed.as_secs_f64(),
        "per_parser": results,
        "aggregated": row_summary,
    });
    let output = cli
        .output
        .unwrap_or_else(|| PathBuf::from("scratch/ab-coverage/summary.json"));
    if let Some(parent) = output.parent() {
        fs::create_dir_all(parent).ok();
    }
    fs::write(&output, serde_json::to_vec_pretty(&summary)?)
        .with_context(|| format!("write {}", output.display()))?;
    eprintln!("[ab-coverage] summary -> {}", output.display());

    if cli.merge {
        let findings = PrevalenceFindings { rows: row_summary };
        let n = apply_prevalence_findings(&cli.matrix, &findings)?;
        eprintln!(
            "[ab-coverage] merged prevalence into {} ({} rows)",
            cli.matrix.display(),
            n
        );
    }

    Ok(())
}
