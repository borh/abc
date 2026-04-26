use std::{collections::BTreeMap, fs, path::Path};

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use walkdir::WalkDir;

#[derive(Debug, Serialize)]
pub struct MetricsSummary {
    pub adapter: String,
    pub works: usize,
    pub fallbacks: usize,
    pub stage_totals_ms: BTreeMap<String, f64>,
    pub slowest_works: Vec<SlowWork>,
}

#[derive(Debug, Serialize)]
pub struct SlowWork {
    pub work_id: String,
    pub total_ms: f64,
    pub dominant_stage: String,
    pub fallback_used: bool,
    pub stages_ms: BTreeMap<String, f64>,
}

#[derive(Debug, Deserialize)]
struct AatRoot {
    work_id: String,
    meta: AatMeta,
}

#[derive(Debug, Deserialize)]
struct AatMeta {
    adapter: String,
    metrics: AatMetrics,
}

#[derive(Debug, Deserialize)]
struct AatMetrics {
    decode_ms: f64,
    body_selection_ms: f64,
    tokenize_ms: f64,
    scopenize_ms: f64,
    retokenize_ms: f64,
    aat_build_ms: f64,
    projection_check_ms: f64,
    fallback_build_ms: f64,
    fallback_used: bool,
}

pub fn summarize_aat_metrics(root: &Path) -> Result<MetricsSummary> {
    let mut adapter = String::new();
    let mut works = 0usize;
    let mut fallbacks = 0usize;
    let mut stage_totals_ms = BTreeMap::new();
    let mut slowest_works = Vec::new();

    for entry in WalkDir::new(root) {
        let entry = entry?;
        if !entry.file_type().is_file()
            || entry
                .path()
                .extension()
                .is_none_or(|extension| extension != "json")
        {
            continue;
        }

        let bytes = fs::read(entry.path())
            .with_context(|| format!("failed to read {}", entry.path().display()))?;
        let root: AatRoot = serde_json::from_slice(&bytes)
            .with_context(|| format!("failed to parse {}", entry.path().display()))?;

        works += 1;
        if adapter.is_empty() {
            adapter = root.meta.adapter.clone();
        }
        if root.meta.metrics.fallback_used {
            fallbacks += 1;
        }

        let stages = stages(&root.meta.metrics);
        for (name, value) in &stages {
            *stage_totals_ms.entry(name.clone()).or_insert(0.0) += value;
        }
        let total_ms = stages.values().sum();
        let dominant_stage = stages
            .iter()
            .max_by(|a, b| a.1.total_cmp(b.1))
            .map(|(name, _)| name.clone())
            .unwrap_or_default();
        slowest_works.push(SlowWork {
            work_id: root.work_id,
            total_ms,
            dominant_stage,
            fallback_used: root.meta.metrics.fallback_used,
            stages_ms: stages,
        });
    }

    anyhow::ensure!(
        works > 0,
        "no AAT metric JSON files found under {}",
        root.display()
    );
    slowest_works.sort_by(|a, b| b.total_ms.total_cmp(&a.total_ms));
    slowest_works.truncate(20);

    Ok(MetricsSummary {
        adapter,
        works,
        fallbacks,
        stage_totals_ms,
        slowest_works,
    })
}

fn stages(metrics: &AatMetrics) -> BTreeMap<String, f64> {
    BTreeMap::from([
        ("decode".to_owned(), metrics.decode_ms),
        ("body_selection".to_owned(), metrics.body_selection_ms),
        ("tokenize".to_owned(), metrics.tokenize_ms),
        ("scopenize".to_owned(), metrics.scopenize_ms),
        ("retokenize".to_owned(), metrics.retokenize_ms),
        ("aat_build".to_owned(), metrics.aat_build_ms),
        ("projection_check".to_owned(), metrics.projection_check_ms),
        ("fallback_build".to_owned(), metrics.fallback_build_ms),
    ])
}
