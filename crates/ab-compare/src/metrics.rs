use std::{collections::BTreeMap, fs, path::Path};

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use walkdir::WalkDir;

#[derive(Debug, Serialize)]
pub struct MetricsSummary {
    pub adapter: String,
    pub works: usize,
    pub fallbacks: usize,
    pub fallback_reason_counts: BTreeMap<String, usize>,
    pub stage_totals_ms: BTreeMap<String, f64>,
    pub node_totals: BTreeMap<String, usize>,
    pub slowest_works: Vec<SlowWork>,
    pub source_supplement_hotspots: Vec<NodeHotspot>,
    pub source_fallback_hotspots: Vec<NodeHotspot>,
    pub fallback_hotspots: Vec<FallbackHotspot>,
}

#[derive(Debug, Serialize)]
pub struct SlowWork {
    pub work_id: String,
    pub total_ms: f64,
    pub dominant_stage: String,
    pub fallback_used: bool,
    pub stages_ms: BTreeMap<String, f64>,
}

#[derive(Debug, Serialize)]
pub struct NodeHotspot {
    pub work_id: String,
    pub nodes: usize,
    pub total_ms: f64,
    pub fallback_used: bool,
    pub stages_ms: BTreeMap<String, f64>,
}

#[derive(Debug, Serialize)]
pub struct FallbackHotspot {
    pub work_id: String,
    pub reason: String,
    pub source_bytes: usize,
    pub validation_body_bytes: usize,
    pub parser_nodes: usize,
    pub parser_normalized_nodes: usize,
    pub source_fallback_nodes: usize,
    pub total_ms: f64,
    pub dominant_stage: String,
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
    #[serde(default)]
    parser_nodes: usize,
    #[serde(default)]
    parser_normalized_nodes: usize,
    #[serde(default, alias = "regex_supplement_nodes")]
    source_supplement_nodes: usize,
    #[serde(default, alias = "regex_fallback_nodes")]
    source_fallback_nodes: usize,
    fallback_used: bool,
    #[serde(default)]
    fallback_reason: String,
    #[serde(default)]
    source_bytes: usize,
    #[serde(default)]
    validation_body_bytes: usize,
}

pub fn summarize_aat_metrics(root: &Path) -> Result<MetricsSummary> {
    let mut adapter = String::new();
    let mut works = 0usize;
    let mut fallbacks = 0usize;
    let mut fallback_reason_counts = BTreeMap::new();
    let mut stage_totals_ms = BTreeMap::new();
    let mut node_totals = BTreeMap::new();
    let mut slowest_works = Vec::new();
    let mut source_supplement_hotspots = Vec::new();
    let mut source_fallback_hotspots = Vec::new();
    let mut fallback_hotspots = Vec::new();

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
            *fallback_reason_counts
                .entry(fallback_reason(&root.meta.metrics).to_owned())
                .or_insert(0) += 1;
        }

        let stage_map = stages(&root.meta.metrics);
        for (name, value) in &stage_map {
            *stage_totals_ms.entry(name.clone()).or_insert(0.0) += value;
        }
        for (name, value) in node_counts(&root.meta.metrics) {
            *node_totals.entry(name).or_insert(0) += value;
        }
        let total_ms = stage_map.values().sum();
        let dominant_stage_name = dominant_stage(&stage_map);
        let work_id = root.work_id;
        slowest_works.push(SlowWork {
            work_id: work_id.clone(),
            total_ms,
            dominant_stage: dominant_stage_name.clone(),
            fallback_used: root.meta.metrics.fallback_used,
            stages_ms: stage_map.clone(),
        });
        source_supplement_hotspots.push(NodeHotspot {
            work_id: work_id.clone(),
            nodes: root.meta.metrics.source_supplement_nodes,
            total_ms,
            fallback_used: root.meta.metrics.fallback_used,
            stages_ms: stage_map.clone(),
        });
        source_fallback_hotspots.push(NodeHotspot {
            work_id: work_id.clone(),
            nodes: root.meta.metrics.source_fallback_nodes,
            total_ms,
            fallback_used: root.meta.metrics.fallback_used,
            stages_ms: stage_map.clone(),
        });
        if root.meta.metrics.fallback_used {
            fallback_hotspots.push(FallbackHotspot {
                work_id,
                reason: fallback_reason(&root.meta.metrics).to_owned(),
                source_bytes: root.meta.metrics.source_bytes,
                validation_body_bytes: root.meta.metrics.validation_body_bytes,
                parser_nodes: root.meta.metrics.parser_nodes,
                parser_normalized_nodes: root.meta.metrics.parser_normalized_nodes,
                source_fallback_nodes: root.meta.metrics.source_fallback_nodes,
                total_ms,
                dominant_stage: dominant_stage_name,
                stages_ms: stage_map,
            });
        }
    }

    anyhow::ensure!(
        works > 0,
        "no AAT metric JSON files found under {}",
        root.display()
    );
    slowest_works.sort_by(|a, b| b.total_ms.total_cmp(&a.total_ms));
    slowest_works.truncate(20);
    source_supplement_hotspots.sort_by(|a, b| {
        b.nodes
            .cmp(&a.nodes)
            .then_with(|| b.total_ms.total_cmp(&a.total_ms))
    });
    source_supplement_hotspots.retain(|work| work.nodes > 0);
    source_supplement_hotspots.truncate(20);
    source_fallback_hotspots.sort_by(|a, b| {
        b.nodes
            .cmp(&a.nodes)
            .then_with(|| b.total_ms.total_cmp(&a.total_ms))
    });
    source_fallback_hotspots.retain(|work| work.nodes > 0);
    source_fallback_hotspots.truncate(20);
    fallback_hotspots.sort_by(|a, b| {
        b.source_fallback_nodes
            .cmp(&a.source_fallback_nodes)
            .then_with(|| b.total_ms.total_cmp(&a.total_ms))
    });
    fallback_hotspots.truncate(20);

    Ok(MetricsSummary {
        adapter,
        works,
        fallbacks,
        fallback_reason_counts,
        stage_totals_ms,
        node_totals,
        slowest_works,
        source_supplement_hotspots,
        source_fallback_hotspots,
        fallback_hotspots,
    })
}

fn fallback_reason(metrics: &AatMetrics) -> &str {
    if metrics.fallback_reason.is_empty() {
        "unknown"
    } else {
        &metrics.fallback_reason
    }
}

fn dominant_stage(stages: &BTreeMap<String, f64>) -> String {
    stages
        .iter()
        .max_by(|a, b| a.1.total_cmp(b.1))
        .map(|(name, _)| name.clone())
        .unwrap_or_default()
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

fn node_counts(metrics: &AatMetrics) -> BTreeMap<String, usize> {
    BTreeMap::from([
        ("parser".to_owned(), metrics.parser_nodes),
        (
            "parser_normalized".to_owned(),
            metrics.parser_normalized_nodes,
        ),
        (
            "source_supplement".to_owned(),
            metrics.source_supplement_nodes,
        ),
        ("source_fallback".to_owned(), metrics.source_fallback_nodes),
    ])
}
