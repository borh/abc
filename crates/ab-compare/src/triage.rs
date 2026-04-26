use std::collections::{BTreeMap, BTreeSet};

use serde::Serialize;
use serde_json::Value;

use crate::{CompareSummary, aat_diff::AatCompareSummary, metrics::MetricsSummary};

#[derive(Debug, Serialize)]
pub struct TriageReport {
    pub adapters: AdapterPair,
    pub works_count: Option<usize>,
    pub result_differences: ResultDifferenceTriage,
    pub semantic_summary: SemanticSummaryTriage,
    pub fallbacks: Option<FallbackTriage>,
    pub source_supplements: Option<SourceSupplementTriage>,
    pub recommended_next_targets: Vec<String>,
}

#[derive(Debug, Serialize)]
pub struct AdapterPair {
    pub a: String,
    pub b: String,
}

#[derive(Debug, Serialize)]
pub struct ResultDifferenceTriage {
    pub total: usize,
    pub by_property: BTreeMap<String, TriageBucket>,
    pub by_feature: BTreeMap<String, TriageBucket>,
    pub by_property_and_feature: BTreeMap<String, TriageBucket>,
}

#[derive(Debug, Serialize)]
pub struct TriageBucket {
    pub count: usize,
    pub work_ids: Vec<String>,
}

#[derive(Debug, Serialize)]
pub struct SemanticSummaryTriage {
    pub structural_difference_count: Option<usize>,
    pub same_visible_structural_difference_count: Option<usize>,
    pub difference_counts: Option<BTreeMap<String, usize>>,
}

#[derive(Debug, Serialize)]
pub struct FallbackTriage {
    pub works: usize,
    pub source_fallback_nodes: usize,
    pub hotspots: Vec<Hotspot>,
}

#[derive(Debug, Serialize)]
pub struct SourceSupplementTriage {
    pub total_nodes: usize,
    pub hotspots: Vec<Hotspot>,
}

#[derive(Debug, Serialize)]
pub struct Hotspot {
    pub work_id: String,
    pub nodes: usize,
    pub total_ms: f64,
    pub fallback_used: bool,
}

pub fn build_triage_report(
    index: &Value,
    comparison: &CompareSummary,
    aat: Option<&AatCompareSummary>,
    metrics: Option<&MetricsSummary>,
) -> TriageReport {
    let features = features_by_work(index);
    let mut by_property = BTreeMap::new();
    let mut by_feature = BTreeMap::new();
    let mut by_property_and_feature = BTreeMap::new();

    for difference in &comparison.result_differences {
        push_bucket(&mut by_property, &difference.property, &difference.work_id);
        let work_features = features
            .get(&difference.work_id)
            .cloned()
            .unwrap_or_else(|| BTreeSet::from(["unindexed".to_owned()]));
        for feature in work_features {
            push_bucket(&mut by_feature, &feature, &difference.work_id);
            push_bucket(
                &mut by_property_and_feature,
                &format!("{}:{feature}", difference.property),
                &difference.work_id,
            );
        }
    }

    let semantic_summary = SemanticSummaryTriage {
        structural_difference_count: aat.map(|summary| summary.structural_difference_count),
        same_visible_structural_difference_count: aat
            .map(|summary| summary.same_visible_structural_difference_count),
        difference_counts: aat
            .map(|summary| summary.semantic_summary_hash_difference_counts.clone()),
    };
    let fallbacks = metrics.map(|metrics| FallbackTriage {
        works: metrics.fallbacks,
        source_fallback_nodes: *metrics.node_totals.get("source_fallback").unwrap_or(&0),
        hotspots: metrics
            .source_fallback_hotspots
            .iter()
            .map(|work| Hotspot {
                work_id: work.work_id.clone(),
                nodes: work.nodes,
                total_ms: work.total_ms,
                fallback_used: work.fallback_used,
            })
            .collect(),
    });
    let source_supplements = metrics.map(|metrics| SourceSupplementTriage {
        total_nodes: *metrics.node_totals.get("source_supplement").unwrap_or(&0),
        hotspots: metrics
            .source_supplement_hotspots
            .iter()
            .map(|work| Hotspot {
                work_id: work.work_id.clone(),
                nodes: work.nodes,
                total_ms: work.total_ms,
                fallback_used: work.fallback_used,
            })
            .collect(),
    });

    TriageReport {
        adapters: AdapterPair {
            a: comparison.adapter_a.clone(),
            b: comparison.adapter_b.clone(),
        },
        works_count: index
            .get("works_count")
            .and_then(Value::as_u64)
            .map(|count| count as usize),
        result_differences: ResultDifferenceTriage {
            total: comparison.result_differences.len(),
            by_property,
            by_feature,
            by_property_and_feature,
        },
        semantic_summary,
        fallbacks,
        source_supplements,
        recommended_next_targets: recommended_next_targets(aat, metrics),
    }
}

fn features_by_work(index: &Value) -> BTreeMap<String, BTreeSet<String>> {
    let mut features = BTreeMap::new();
    let Some(works) = index.get("works").and_then(Value::as_array) else {
        return features;
    };
    for work in works {
        let Some(id) = work.get("id").and_then(Value::as_str) else {
            continue;
        };
        let entry = features.entry(id.to_owned()).or_insert_with(BTreeSet::new);
        if let Some(values) = work.get("features").and_then(Value::as_array) {
            for feature in values {
                if let Some(feature) = feature.as_str() {
                    entry.insert(feature.to_owned());
                }
            }
        }
    }
    features
}

fn push_bucket(buckets: &mut BTreeMap<String, TriageBucket>, key: &str, work_id: &str) {
    let bucket = buckets.entry(key.to_owned()).or_insert(TriageBucket {
        count: 0,
        work_ids: Vec::new(),
    });
    bucket.count += 1;
    if bucket.work_ids.len() < 10 && !bucket.work_ids.iter().any(|id| id == work_id) {
        bucket.work_ids.push(work_id.to_owned());
    }
}

fn recommended_next_targets(
    aat: Option<&AatCompareSummary>,
    metrics: Option<&MetricsSummary>,
) -> Vec<String> {
    let mut targets = Vec::new();
    if let Some(metrics) = metrics {
        if metrics.fallbacks > 0 {
            targets.push("inspect_fallback_hotspots".to_owned());
        }
        if metrics
            .node_totals
            .get("source_supplement")
            .copied()
            .unwrap_or_default()
            > 0
        {
            targets.push("replace_source_supplements_with_parser_nodes".to_owned());
        }
    }
    if aat.is_some_and(|summary| !summary.semantic_summary_hash_difference_counts.is_empty()) {
        targets.push("triage_semantic_summary_differences".to_owned());
    }
    targets
}
