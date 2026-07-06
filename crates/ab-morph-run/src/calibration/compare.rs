//! Ranking comparison: Kendall τ-b over shared patterns, overlap/Jaccard,
//! and score-block field diffs.

use std::collections::BTreeMap;
use std::path::Path;

use anyhow::Result;
use serde::Serialize;

use super::read_ranking;
use crate::summary::InterestingSummary;

/// Kendall τ-b over (left_rank, right_rank) pairs. `None` when fewer than
/// two pairs or when a side is entirely tied (denominator 0).
pub fn kendall_tau_b(pairs: &[(usize, usize)]) -> Option<f64> {
    let n = pairs.len();
    if n < 2 {
        return None;
    }
    let (mut concordant, mut discordant, mut ties_left, mut ties_right) = (0i64, 0i64, 0i64, 0i64);
    for i in 0..n {
        for j in (i + 1)..n {
            let dl = pairs[i].0.cmp(&pairs[j].0);
            let dr = pairs[i].1.cmp(&pairs[j].1);
            use std::cmp::Ordering::Equal;
            match (dl, dr) {
                (Equal, Equal) => {}
                (Equal, _) => ties_left += 1,
                (_, Equal) => ties_right += 1,
                (a, b) if a == b => concordant += 1,
                _ => discordant += 1,
            }
        }
    }
    let n0 = (n * (n - 1) / 2) as i64;
    let denom = (((n0 - ties_left) as f64) * ((n0 - ties_right) as f64)).sqrt();
    if denom == 0.0 {
        return None;
    }
    Some((concordant - discordant) as f64 / denom)
}

#[derive(Debug, Clone, Serialize)]
pub struct RankingComparison {
    pub left_len: usize,
    pub right_len: usize,
    pub overlap: usize,
    pub jaccard: f64,
    pub kendall_tau_b: Option<f64>,
    /// Score-block fields whose values differ (field: left vs right).
    pub score_version_mismatches: Vec<String>,
}

pub fn compare_rankings(
    left: &InterestingSummary,
    right: &InterestingSummary,
) -> RankingComparison {
    let left_ranks: BTreeMap<&str, usize> = left
        .rows
        .iter()
        .enumerate()
        .map(|(i, row)| (row.pattern_id.as_str(), i + 1))
        .collect();
    let right_ranks: BTreeMap<&str, usize> = right
        .rows
        .iter()
        .enumerate()
        .map(|(i, row)| (row.pattern_id.as_str(), i + 1))
        .collect();
    let pairs = left_ranks
        .iter()
        .filter_map(|(id, lr)| right_ranks.get(id).map(|rr| (*lr, *rr)))
        .collect::<Vec<_>>();
    let union = left_ranks.len() + right_ranks.len() - pairs.len();
    RankingComparison {
        left_len: left_ranks.len(),
        right_len: right_ranks.len(),
        overlap: pairs.len(),
        jaccard: if union == 0 {
            1.0
        } else {
            pairs.len() as f64 / union as f64
        },
        kendall_tau_b: kendall_tau_b(&pairs),
        score_version_mismatches: score_block_mismatches(left, right),
    }
}

fn score_block_mismatches(left: &InterestingSummary, right: &InterestingSummary) -> Vec<String> {
    let (l, r) = (
        serde_json::to_value(&left.score_version).expect("block serializes"),
        serde_json::to_value(&right.score_version).expect("block serializes"),
    );
    let (l, r) = (l.as_object().unwrap(), r.as_object().unwrap());
    l.iter()
        .filter(|(key, value)| r.get(*key) != Some(value))
        .map(|(key, value)| {
            format!(
                "{key}: {value} vs {}",
                r.get(key).unwrap_or(&serde_json::Value::Null)
            )
        })
        .collect()
}

pub fn run_compare_rankings(left: &Path, right: &Path) -> Result<RankingComparison> {
    Ok(compare_rankings(&read_ranking(left)?, &read_ranking(right)?))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fixture_summary(ids: &[&str], rank_scope: &str) -> InterestingSummary {
        serde_json::from_value(serde_json::json!({
            "score_version": {
                "score_version": 1,
                "pattern_id_version": 1,
                "rrf_k": 60,
                "lambda_missing_policy": "rank-floor",
                "rank_scope": rank_scope,
                "score_mode": "rrf",
                "sample_seed": null,
                "anomaly_w_cov": 5.0,
                "signal_profile": ["coverage", "rarity", "impact", "span"],
                "feature_profile": "core",
                "rarity_basis": "source",
                "granularity_profile": "suw",
                "cause_classification_profile": "absent",
                "literal_context_policy": null,
                "surprise": "absent",
            },
            "run_id": "run-a",
            "rows": ids.iter().map(|id| serde_json::json!({
                "pattern_id": id,
                "pattern": "pattern",
                "kind": "feature",
                "rrf_score": 0.0,
                "signal_profile": [],
                "signals": [],
                "examples": 0,
                "source_count": 0,
                "text_count": 0,
                "sample_source_ids": [],
                "sample_text_ids": [],
                "region_examples": [],
            })).collect::<Vec<_>>(),
            "anomalies": [],
        }))
        .expect("fixture summary deserializes")
    }

    #[test]
    fn kendall_tau_b_extremes_and_ties() {
        // Perfect agreement.
        let pairs: Vec<(usize, usize)> = (1..=5).map(|r| (r, r)).collect();
        assert!((kendall_tau_b(&pairs).unwrap() - 1.0).abs() < 1e-12);
        // Perfect reversal.
        let pairs: Vec<(usize, usize)> = (1..=5).map(|r| (r, 6 - r)).collect();
        assert!((kendall_tau_b(&pairs).unwrap() + 1.0).abs() < 1e-12);
        // Hand-computed 4-element example with one discordant pair:
        // left 1,2,3,4 / right 1,2,4,3 → C=5, D=1, τ = 4/6.
        let pairs = vec![(1, 1), (2, 2), (3, 4), (4, 3)];
        assert!((kendall_tau_b(&pairs).unwrap() - 4.0 / 6.0).abs() < 1e-12);
        // Fewer than 2 pairs: undefined.
        assert_eq!(kendall_tau_b(&[(1, 1)]), None);
        // All-tied on one side: denominator 0 → None.
        assert_eq!(kendall_tau_b(&[(1, 1), (1, 2), (1, 3)]), None);
    }

    #[test]
    fn comparison_reports_overlap_and_block_mismatches() {
        let left = fixture_summary(&["p1", "p2", "p3"], "within-kind");
        let right = fixture_summary(&["p2", "p1", "p4"], "global");
        let cmp = compare_rankings(&left, &right);
        assert_eq!(cmp.overlap, 2);
        assert!((cmp.jaccard - 2.0 / 4.0).abs() < 1e-12);
        // p1: ranks (1,2); p2: ranks (2,1) → one discordant pair, τ = -1.
        assert!((cmp.kendall_tau_b.unwrap() + 1.0).abs() < 1e-12);
        assert!(cmp
            .score_version_mismatches
            .iter()
            .any(|m| m.contains("rank_scope")));
    }
}
