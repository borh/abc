use std::collections::{BTreeMap, BTreeSet};

use ab_diff_utils::FrequencyTable;
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
    pub coverage_mismatches: Option<CoverageMismatchTriage>,
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
    pub by_property: FrequencyTable<String, String>,
    pub by_feature: FrequencyTable<String, String>,
    pub by_property_and_feature: FrequencyTable<String, String>,
}

#[derive(Debug, Serialize)]
pub struct CoverageMismatchTriage {
    pub count: usize,
    pub by_reason: FrequencyTable<String, String>,
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
    pub by_reason: BTreeMap<String, usize>,
    pub hotspots: Vec<FallbackHotspot>,
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

#[derive(Debug, Serialize)]
pub struct FallbackHotspot {
    pub work_id: String,
    pub reason: String,
    pub features: Vec<String>,
    pub source_bytes: usize,
    pub validation_body_bytes: usize,
    pub parser_nodes: usize,
    pub parser_normalized_nodes: usize,
    pub source_fallback_nodes: usize,
    pub total_ms: f64,
    pub dominant_stage: String,
    pub stages_ms: BTreeMap<&'static str, f64>,
}

pub fn build_triage_report(
    index: &Value,
    comparison: &CompareSummary,
    aat: Option<&AatCompareSummary>,
    metrics: Option<&MetricsSummary>,
) -> TriageReport {
    let features = features_by_work(index);
    let mut by_property = FrequencyTable::default();
    let mut by_feature = FrequencyTable::default();
    let mut by_property_and_feature = FrequencyTable::default();

    for difference in &comparison.result_differences {
        by_property.record(difference.property.clone(), difference.work_id.clone());
        let work_features = features
            .get(&difference.work_id)
            .cloned()
            .unwrap_or_else(|| BTreeSet::from(["unindexed".to_owned()]));
        for feature in work_features {
            by_feature.record(feature.clone(), difference.work_id.clone());
            by_property_and_feature.record(
                format!("{}:{feature}", difference.property),
                difference.work_id.clone(),
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
        by_reason: metrics.fallback_reason_counts.clone(),
        hotspots: metrics
            .fallback_hotspots
            .iter()
            .map(|work| FallbackHotspot {
                work_id: work.work_id.clone(),
                reason: work.reason.clone(),
                features: features
                    .get(&work.work_id)
                    .cloned()
                    .unwrap_or_else(|| BTreeSet::from(["unindexed".to_owned()]))
                    .into_iter()
                    .collect(),
                source_bytes: work.source_bytes,
                validation_body_bytes: work.validation_body_bytes,
                parser_nodes: work.parser_nodes,
                parser_normalized_nodes: work.parser_normalized_nodes,
                source_fallback_nodes: work.source_fallback_nodes,
                total_ms: work.total_ms,
                dominant_stage: work.dominant_stage.clone(),
                stages_ms: work.stages_ms.clone(),
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
    let coverage_mismatches = aat.and_then(|summary| {
        let mut by_reason = FrequencyTable::new(20);
        let mut count = 0usize;
        for difference in summary
            .structural_differences
            .iter()
            .chain(summary.coverage_differences.iter())
        {
            if let Some(coverage) = &difference.coverage_mismatch {
                count += 1;
                let reason = match (coverage.a_had_fallback, coverage.b_had_fallback) {
                    (true, false) => coverage
                        .a_fallback_reason
                        .clone()
                        .unwrap_or_else(|| "unknown".to_owned()),
                    (false, true) => coverage
                        .b_fallback_reason
                        .clone()
                        .unwrap_or_else(|| "unknown".to_owned()),
                    (true, true) => format!(
                        "a={}, b={}",
                        coverage.a_fallback_reason.as_deref().unwrap_or("unknown"),
                        coverage.b_fallback_reason.as_deref().unwrap_or("unknown"),
                    ),
                    (false, false) => "unknown".to_owned(),
                };
                by_reason.record(reason, difference.work_id.clone());
            }
        }
        (count > 0).then_some(CoverageMismatchTriage { count, by_reason })
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
        coverage_mismatches,
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
    let has_coverage_issues = aat.is_some_and(|summary| {
        summary.coverage_only_difference_count > 0
            || summary
                .structural_differences
                .iter()
                .any(|difference| difference.coverage_mismatch.is_some())
    });
    if has_coverage_issues {
        targets.push("inspect_coverage_mismatches".to_owned());
    }
    if aat.is_some_and(|summary| !summary.semantic_summary_hash_difference_counts.is_empty()) {
        targets.push("triage_semantic_summary_differences".to_owned());
    }
    targets
}
