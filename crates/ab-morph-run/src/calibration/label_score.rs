//! Label scoring (spec §Calibration Plan step 5): turns a filled blind
//! labeling TSV (Task 4's export, owner-annotated `verdict` column) plus its
//! `mapping.json` sidecar into per-method p@k and nDCG@k. Pooled evaluation:
//! the ideal ranking (IDCG) is drawn from the union of *all* labeled
//! patterns, not from each method's own surfaced set, so methods that
//! surface different pools remain comparable.

use std::collections::{BTreeMap, BTreeSet};
use std::fs::File;
use std::io::BufReader;
use std::path::Path;

use anyhow::{Context, Result, bail};
use serde::Serialize;

use super::label_export::MappingEntry;

/// p@k relevance: a verdict counts as relevant when it indicates the ranker
/// surfaced an analyzer defect or dictionary gap (D5; the report invites
/// dispute — rescoring from the same labels is free).
const RELEVANT_VERDICTS: &[&str] = &["bug", "expected-dictionary"];
/// nDCG gains (D5).
const VERDICT_GAINS: &[(&str, f64)] = &[
    ("bug", 3.0),
    ("expected-dictionary", 2.0),
    ("corpus-artifact", 1.0),
    ("expected-policy", 1.0),
    ("noise", 0.0),
    ("unclear", 0.0),
];

/// Output of [`run_score_labels`]: per-method p@k/nDCG@k plus a verdict
/// histogram, self-documenting via the `relevance`/`gains` fields so the
/// JSON artifact carries its own scoring rule.
#[derive(Debug, Clone, Serialize)]
pub struct LabelScores {
    pub k: usize,
    pub relevance: &'static str,
    pub gains: &'static str,
    pub verdict_counts: BTreeMap<String, usize>,
    pub methods: BTreeMap<String, MethodScores>,
}

#[derive(Debug, Clone, Serialize)]
pub struct MethodScores {
    /// Total pooled patterns this method ranked (i.e. its rank list length,
    /// before truncating to top-k).
    pub labeled: usize,
    pub precision_at_k: f64,
    pub ndcg_at_k: f64,
}

const RELEVANCE_DOC: &str = "a verdict counts as relevant when it indicates the ranker \
    surfaced an analyzer defect or dictionary gap: bug, expected-dictionary";
const GAINS_DOC: &str =
    "bug=3, expected-dictionary=2, corpus-artifact=1, expected-policy=1, noise=0, unclear=0";

/// Reads `labels` (filled Task 4 TSV) and `mapping` (Task 4's `mapping.json`
/// sidecar) and computes p@k/nDCG@k per method.
///
/// # Errors
///
/// Returns an error when: the TSV is missing a `label_id` or `verdict`
/// column; the TSV has a duplicated `label_id`; the TSV's `label_id` set
/// does not exactly match `mapping`'s keys (missing/deleted row, or an
/// unknown/typo'd id); any row has an empty `verdict` (lists all such ids);
/// any row has a `verdict` outside the six known values; or a method's
/// ranked pool is smaller than `k` (mapping and rankings out of sync).
pub fn run_score_labels(labels: &Path, mapping: &Path, k: usize) -> Result<LabelScores> {
    if k == 0 {
        bail!("k must be at least 1");
    }

    let rows = parse_label_rows(labels)?;
    let mapping = read_mapping(mapping)?;

    // Set coherence first (spec 1b): before any verdict is inspected, the
    // TSV's label_id set must exactly match mapping.json's keys.
    check_no_duplicate_label_ids(&rows)?;
    check_label_id_sets_match(&rows, &mapping)?;

    let verdicts = validate_verdicts(&rows)?;

    let mut verdict_counts: BTreeMap<String, usize> = VERDICT_GAINS
        .iter()
        .map(|(v, _)| ((*v).to_owned(), 0))
        .collect();
    for verdict in verdicts.values() {
        *verdict_counts
            .get_mut(verdict.as_str())
            .expect("verdict already validated against the known set") += 1;
    }

    let gains: BTreeMap<&str, f64> = verdicts
        .iter()
        .map(|(id, verdict)| {
            (
                id.as_str(),
                verdict_gain(verdict).expect("validated verdict"),
            )
        })
        .collect();

    // Pooled ideal: the k best gains over the whole labeled union (same for
    // every method — this is what makes methods with different surfaced
    // sets comparable).
    let mut pooled_gains_desc: Vec<f64> = gains.values().copied().collect();
    pooled_gains_desc.sort_by(|a, b| b.partial_cmp(a).expect("gains are finite"));
    let idcg = dcg_at_k(&pooled_gains_desc[..pooled_gains_desc.len().min(k)]);

    let mut method_names: BTreeSet<String> = BTreeSet::new();
    for entry in mapping.values() {
        method_names.extend(entry.ranks.keys().cloned());
    }

    let mut methods = BTreeMap::new();
    for method in method_names {
        let mut ranked: Vec<(usize, &str)> = mapping
            .iter()
            .filter_map(|(id, entry)| entry.ranks.get(&method).map(|rank| (*rank, id.as_str())))
            .collect();
        ranked.sort_by_key(|(rank, _)| *rank);
        let labeled = ranked.len();
        if labeled < k {
            bail!(
                "method `{method}` has only {labeled} pooled pattern(s) ranked, fewer than k={k} \
                 (mapping and rankings out of sync)"
            );
        }
        let top_k = &ranked[..k];
        let relevant_count = top_k
            .iter()
            .filter(|(_, id)| RELEVANT_VERDICTS.contains(&verdicts[*id].as_str()))
            .count();
        let top_k_gains: Vec<f64> = top_k.iter().map(|(_, id)| gains[id]).collect();
        let dcg = dcg_at_k(&top_k_gains);
        methods.insert(
            method,
            MethodScores {
                labeled,
                precision_at_k: precision_at_k(relevant_count, k),
                ndcg_at_k: ndcg_at_k(dcg, idcg),
            },
        );
    }

    Ok(LabelScores {
        k,
        relevance: RELEVANCE_DOC,
        gains: GAINS_DOC,
        verdict_counts,
        methods,
    })
}

/// `verdict`'s nDCG gain (D5), or `None` for a verdict outside the six known
/// values.
fn verdict_gain(verdict: &str) -> Option<f64> {
    VERDICT_GAINS
        .iter()
        .find(|(known, _)| *known == verdict)
        .map(|(_, gain)| *gain)
}

/// `|relevant among top-k| / k`. `relevant_count` must already be capped at
/// `k` by the caller (it is a count over the method's own top-k slice).
fn precision_at_k(relevant_count: usize, k: usize) -> f64 {
    relevant_count as f64 / k as f64
}

/// `Σ gain_i / log2(i+1)` for `i = 1..=gains.len()` (1-based position);
/// `gains` is expected to already be truncated/ordered to the top-k the
/// caller wants scored.
fn dcg_at_k(gains: &[f64]) -> f64 {
    gains
        .iter()
        .enumerate()
        .map(|(zero_based_index, gain)| {
            let position = zero_based_index as f64 + 1.0;
            gain / (position + 1.0).log2()
        })
        .sum()
}

/// `dcg / idcg`, defined as `0.0` when `idcg` is `0.0` (an all-zero-gain
/// pool, or `k` larger than the pool) rather than propagating NaN.
fn ndcg_at_k(dcg: f64, idcg: f64) -> f64 {
    if idcg == 0.0 { 0.0 } else { dcg / idcg }
}

fn read_mapping(path: &Path) -> Result<BTreeMap<String, MappingEntry>> {
    let file = File::open(path).with_context(|| format!("failed to open {}", path.display()))?;
    serde_json::from_reader(BufReader::new(file))
        .with_context(|| format!("failed to parse mapping JSON {}", path.display()))
}

/// One TSV data row's `(label_id, verdict)`, in file order (verdict may be
/// empty — that is a validation error handled by [`validate_verdicts`], not
/// here).
fn parse_label_rows(path: &Path) -> Result<Vec<(String, String)>> {
    let content = std::fs::read_to_string(path)
        .with_context(|| format!("failed to read {}", path.display()))?;
    let mut lines = content.lines().filter(|line| !line.starts_with('#'));
    let header = lines
        .next()
        .with_context(|| format!("labels TSV {} has no header row", path.display()))?;
    let columns: Vec<&str> = header.split('\t').collect();
    let label_id_index = columns
        .iter()
        .position(|column| *column == "label_id")
        .with_context(|| {
            format!(
                "labels TSV {} header has no `label_id` column",
                path.display()
            )
        })?;
    let verdict_index = columns
        .iter()
        .position(|column| *column == "verdict")
        .with_context(|| {
            format!(
                "labels TSV {} header has no `verdict` column",
                path.display()
            )
        })?;

    let mut rows = Vec::new();
    for line in lines {
        if line.is_empty() {
            continue;
        }
        let fields: Vec<&str> = line.split('\t').collect();
        let label_id = fields.get(label_id_index).copied().unwrap_or("").to_owned();
        let verdict = fields.get(verdict_index).copied().unwrap_or("").to_owned();
        rows.push((label_id, verdict));
    }
    Ok(rows)
}

fn check_no_duplicate_label_ids(rows: &[(String, String)]) -> Result<()> {
    let mut seen = BTreeSet::new();
    let mut duplicates = BTreeSet::new();
    for (label_id, _) in rows {
        if !seen.insert(label_id.clone()) {
            duplicates.insert(label_id.clone());
        }
    }
    if !duplicates.is_empty() {
        bail!(
            "labels TSV has duplicate label_id(s): {}",
            join_sorted(&duplicates)
        );
    }
    Ok(())
}

/// Set coherence (spec 1b): the TSV's `label_id` set must equal
/// `mapping`'s key set exactly.
fn check_label_id_sets_match(
    rows: &[(String, String)],
    mapping: &BTreeMap<String, MappingEntry>,
) -> Result<()> {
    let tsv_ids: BTreeSet<String> = rows.iter().map(|(label_id, _)| label_id.clone()).collect();
    let mapping_ids: BTreeSet<String> = mapping.keys().cloned().collect();

    let missing_from_tsv: BTreeSet<String> = mapping_ids.difference(&tsv_ids).cloned().collect();
    let unknown_in_tsv: BTreeSet<String> = tsv_ids.difference(&mapping_ids).cloned().collect();

    if missing_from_tsv.is_empty() && unknown_in_tsv.is_empty() {
        return Ok(());
    }

    let mut message = String::from("labels TSV label_id set does not match mapping.json's key set");
    if !missing_from_tsv.is_empty() {
        message.push_str(&format!(
            "; present in mapping.json but missing from the TSV (deleted row?): {}",
            join_sorted(&missing_from_tsv)
        ));
    }
    if !unknown_in_tsv.is_empty() {
        message.push_str(&format!(
            "; present in the TSV but unknown to mapping.json (typo?): {}",
            join_sorted(&unknown_in_tsv)
        ));
    }
    bail!(message);
}

/// Validates every row has a non-empty, known verdict, returning
/// `label_id -> verdict`. Empty verdicts are reported together (spec: list
/// ALL unlabeled ids); an invalid (non-empty, unknown) verdict is reported
/// on the first offending row.
fn validate_verdicts(rows: &[(String, String)]) -> Result<BTreeMap<String, String>> {
    let unlabeled: BTreeSet<String> = rows
        .iter()
        .filter(|(_, verdict)| verdict.is_empty())
        .map(|(label_id, _)| label_id.clone())
        .collect();
    if !unlabeled.is_empty() {
        bail!(
            "labels TSV has unlabeled (empty verdict) row(s): {}",
            join_sorted(&unlabeled)
        );
    }

    let mut result = BTreeMap::new();
    for (label_id, verdict) in rows {
        if verdict_gain(verdict).is_none() {
            let known: Vec<&str> = VERDICT_GAINS.iter().map(|(v, _)| *v).collect();
            bail!(
                "labels TSV row `{label_id}` has invalid verdict `{verdict}` (expected one of: {})",
                known.join(", ")
            );
        }
        result.insert(label_id.clone(), verdict.clone());
    }
    Ok(result)
}

fn join_sorted(ids: &BTreeSet<String>) -> String {
    ids.iter().cloned().collect::<Vec<_>>().join(", ")
}

#[cfg(test)]
mod tests {
    use super::*;

    /// `(label_id, pattern_id, ranks)` fixture shapes for `mapping.json`
    /// test data, factored out of the test bodies to keep clippy's
    /// `type_complexity` lint quiet.
    type OwnedMappingEntry = (&'static str, &'static str, Vec<(&'static str, usize)>);
    type MappingEntryRef<'a> = (&'a str, &'a str, &'a [(&'a str, usize)]);

    // --- Pure metric functions ----------------------------------------

    #[test]
    fn precision_at_k_is_relevant_count_over_k() {
        assert!((precision_at_k(2, 3) - 2.0 / 3.0).abs() < 1e-12);
        assert!((precision_at_k(0, 5) - 0.0).abs() < 1e-12);
        assert!((precision_at_k(5, 5) - 1.0).abs() < 1e-12);
    }

    #[test]
    fn dcg_at_k_matches_hand_computation() {
        // gains [3, 2, 1] at positions 1, 2, 3:
        //   term_1 = 3 / log2(2) = 3 / 1               = 3.0
        //   term_2 = 2 / log2(3) = 2 / 1.584962500721156 = 1.261859507142915
        //   term_3 = 1 / log2(4) = 1 / 2               = 0.5
        //   dcg = 3.0 + 1.261859507142915 + 0.5 = 4.761859507142915
        let dcg = dcg_at_k(&[3.0, 2.0, 1.0]);
        assert!((dcg - 4.761_859_507_142_915).abs() < 1e-9);
    }

    #[test]
    fn ndcg_at_k_divides_dcg_by_idcg_and_defines_zero_idcg_as_zero() {
        assert!((ndcg_at_k(2.0, 4.0) - 0.5).abs() < 1e-12);
        assert_eq!(ndcg_at_k(0.0, 0.0), 0.0);
    }

    // --- TSV parsing / validation --------------------------------------

    fn write_labels_tsv(dir: &Path, rows: &[(&str, &str)]) -> std::path::PathBuf {
        let path = dir.join("labels.tsv");
        let mut content = String::from(
            "# Blind pooled labeling export. Fill in `verdict`.\n\
             label_id\tkind\tpattern\texamples\tsource_count\ttext_count\tsnippet_1\tverdict\tnotes\n",
        );
        for (label_id, verdict) in rows {
            content.push_str(&format!(
                "{label_id}\tfeature\tpattern-{label_id}\t1\t1\t1\tsnippet\t{verdict}\t\n"
            ));
        }
        std::fs::write(&path, content).unwrap();
        path
    }

    fn write_mapping_json(dir: &Path, entries: &[MappingEntryRef<'_>]) -> std::path::PathBuf {
        let path = dir.join("mapping.json");
        let mut mapping = serde_json::Map::new();
        for (label_id, pattern_id, ranks) in entries {
            let mut rank_object = serde_json::Map::new();
            for (method, rank) in *ranks {
                rank_object.insert((*method).to_owned(), serde_json::json!(rank));
            }
            mapping.insert(
                (*label_id).to_owned(),
                serde_json::json!({
                    "pattern_id": pattern_id,
                    "ranks": rank_object,
                }),
            );
        }
        std::fs::write(&path, serde_json::to_string_pretty(&mapping).unwrap()).unwrap();
        path
    }

    /// 5-pattern / 2-method fixture. Verdicts (gain): L1=bug(3),
    /// L2=expected-dictionary(2), L3=corpus-artifact(1),
    /// L4=expected-policy(1), L5=noise(0).
    ///
    /// Method `a` ranks in gain-descending order: L1, L2, L3, L4, L5 — its
    /// top-3 IS the pooled ideal, so nDCG@3 should come out to 1.0.
    /// Method `b` ranks: L5, L1, L4, L2, L3 — top-3 is L5(0), L1(3), L4(1).
    const FIXTURE_ROWS: &[(&str, &str)] = &[
        ("L1", "bug"),
        ("L2", "expected-dictionary"),
        ("L3", "corpus-artifact"),
        ("L4", "expected-policy"),
        ("L5", "noise"),
    ];

    fn fixture_mapping_entries() -> Vec<OwnedMappingEntry> {
        vec![
            ("L1", "p1", vec![("a", 1), ("b", 2)]),
            ("L2", "p2", vec![("a", 2), ("b", 4)]),
            ("L3", "p3", vec![("a", 3), ("b", 5)]),
            ("L4", "p4", vec![("a", 4), ("b", 3)]),
            ("L5", "p5", vec![("a", 5), ("b", 1)]),
        ]
    }

    #[test]
    fn scores_hand_computed_fixture_for_p_at_3_and_ndcg_at_3() {
        let dir = tempfile::tempdir().unwrap();
        let labels = write_labels_tsv(dir.path(), FIXTURE_ROWS);
        let entries = fixture_mapping_entries();
        let entries_ref: Vec<MappingEntryRef<'_>> = entries
            .iter()
            .map(|(id, pat, ranks)| (*id, *pat, ranks.as_slice()))
            .collect();
        let mapping = write_mapping_json(dir.path(), &entries_ref);

        let scores = run_score_labels(&labels, &mapping, 3).unwrap();

        assert_eq!(scores.k, 3);
        assert_eq!(scores.verdict_counts["bug"], 1);
        assert_eq!(scores.verdict_counts["expected-dictionary"], 1);
        assert_eq!(scores.verdict_counts["corpus-artifact"], 1);
        assert_eq!(scores.verdict_counts["expected-policy"], 1);
        assert_eq!(scores.verdict_counts["noise"], 1);
        assert_eq!(scores.verdict_counts["unclear"], 0);

        // Pooled ideal top-3 gains (desc over all 5 labeled patterns):
        // 3 (bug), 2 (expected-dictionary), 1 (corpus-artifact, tied with
        // expected-policy but either 1 works — same value).
        //   idcg = 3/log2(2) + 2/log2(3) + 1/log2(4)
        //        = 3.0 + 1.261859507142915 + 0.5 = 4.761859507142915

        // Method `a` top-3 by rank: L1(bug,3), L2(dict,2), L3(artifact,1).
        //   relevant = {L1, L2} (bug, expected-dictionary) → 2/3
        //   dcg = 3/log2(2) + 2/log2(3) + 1/log2(4) = 4.761859507142915
        //     (identical to idcg, since a's top-3 IS the pooled-ideal top-3)
        //   ndcg = 4.761859507142915 / 4.761859507142915 = 1.0
        let a = &scores.methods["a"];
        assert_eq!(a.labeled, 5);
        assert!((a.precision_at_k - 2.0 / 3.0).abs() < 1e-9);
        assert!((a.ndcg_at_k - 1.0).abs() < 1e-9);

        // Method `b` top-3 by rank: L5(noise,0), L1(bug,3), L4(policy,1).
        //   relevant = {L1} (bug only; noise and expected-policy don't
        //   count) → 1/3
        //   dcg = 0/log2(2) + 3/log2(3) + 1/log2(4)
        //       = 0.0 + 1.892789260714372 + 0.5 = 2.392789260714372
        //   ndcg = 2.392789260714372 / 4.761859507142915 (same terms fed
        //   through `dcg_at_k`/`ndcg_at_k` below, so the expected value is
        //   exact rather than a hand-rounded literal).
        let b = &scores.methods["b"];
        assert_eq!(b.labeled, 5);
        assert!((b.precision_at_k - 1.0 / 3.0).abs() < 1e-9);
        let expected_dcg_b = dcg_at_k(&[0.0, 3.0, 1.0]);
        let expected_idcg = dcg_at_k(&[3.0, 2.0, 1.0]);
        assert!((b.ndcg_at_k - expected_dcg_b / expected_idcg).abs() < 1e-9);
    }

    #[test]
    fn unlabeled_rows_error_lists_all_unlabeled_ids() {
        let dir = tempfile::tempdir().unwrap();
        let rows: Vec<(&str, &str)> = vec![
            ("L1", "bug"),
            ("L2", ""),
            ("L3", "corpus-artifact"),
            ("L4", ""),
            ("L5", "noise"),
        ];
        let labels = write_labels_tsv(dir.path(), &rows);
        let entries = fixture_mapping_entries();
        let entries_ref: Vec<MappingEntryRef<'_>> = entries
            .iter()
            .map(|(id, pat, ranks)| (*id, *pat, ranks.as_slice()))
            .collect();
        let mapping = write_mapping_json(dir.path(), &entries_ref);

        let err = run_score_labels(&labels, &mapping, 3).unwrap_err();
        let message = err.to_string();
        assert!(message.contains("L2"), "{message}");
        assert!(message.contains("L4"), "{message}");
        assert!(!message.contains("L1"), "{message}");
    }

    #[test]
    fn invalid_verdict_error_names_label_id_and_value() {
        let dir = tempfile::tempdir().unwrap();
        let rows: Vec<(&str, &str)> = vec![
            ("L1", "bug"),
            ("L2", "definitely-not-a-verdict"),
            ("L3", "corpus-artifact"),
            ("L4", "expected-policy"),
            ("L5", "noise"),
        ];
        let labels = write_labels_tsv(dir.path(), &rows);
        let entries = fixture_mapping_entries();
        let entries_ref: Vec<MappingEntryRef<'_>> = entries
            .iter()
            .map(|(id, pat, ranks)| (*id, *pat, ranks.as_slice()))
            .collect();
        let mapping = write_mapping_json(dir.path(), &entries_ref);

        let err = run_score_labels(&labels, &mapping, 3).unwrap_err();
        let message = err.to_string();
        assert!(message.contains("L2"), "{message}");
        assert!(message.contains("definitely-not-a-verdict"), "{message}");
    }

    #[test]
    fn set_mismatch_deleted_tsv_row_is_a_hard_error() {
        let dir = tempfile::tempdir().unwrap();
        // TSV is missing L5 (owner deleted the row) but mapping still has it.
        let rows: Vec<(&str, &str)> = FIXTURE_ROWS[..4].to_vec();
        let labels = write_labels_tsv(dir.path(), &rows);
        let entries = fixture_mapping_entries();
        let entries_ref: Vec<MappingEntryRef<'_>> = entries
            .iter()
            .map(|(id, pat, ranks)| (*id, *pat, ranks.as_slice()))
            .collect();
        let mapping = write_mapping_json(dir.path(), &entries_ref);

        let err = run_score_labels(&labels, &mapping, 3).unwrap_err();
        let message = err.to_string();
        assert!(message.contains("L5"), "{message}");
        assert!(message.contains("missing"), "{message}");
    }

    #[test]
    fn set_mismatch_unknown_tsv_id_is_a_hard_error() {
        let dir = tempfile::tempdir().unwrap();
        // TSV has a typo'd id L9 that mapping.json has never heard of.
        let mut rows = FIXTURE_ROWS[..4].to_vec();
        rows.push(("L9", "noise"));
        let labels = write_labels_tsv(dir.path(), &rows);
        let entries = fixture_mapping_entries();
        let entries_ref: Vec<MappingEntryRef<'_>> = entries
            .iter()
            .map(|(id, pat, ranks)| (*id, *pat, ranks.as_slice()))
            .collect();
        let mapping = write_mapping_json(dir.path(), &entries_ref);

        let err = run_score_labels(&labels, &mapping, 3).unwrap_err();
        let message = err.to_string();
        assert!(message.contains("L9"), "{message}");
        assert!(message.contains("unknown"), "{message}");
    }

    #[test]
    fn set_mismatch_duplicated_tsv_id_is_a_hard_error() {
        let dir = tempfile::tempdir().unwrap();
        let mut rows = FIXTURE_ROWS.to_vec();
        rows.push(("L1", "bug")); // L1 appears twice.
        let labels = write_labels_tsv(dir.path(), &rows);
        let entries = fixture_mapping_entries();
        let entries_ref: Vec<MappingEntryRef<'_>> = entries
            .iter()
            .map(|(id, pat, ranks)| (*id, *pat, ranks.as_slice()))
            .collect();
        let mapping = write_mapping_json(dir.path(), &entries_ref);

        let err = run_score_labels(&labels, &mapping, 3).unwrap_err();
        let message = err.to_string();
        assert!(message.contains("duplicate"), "{message}");
        assert!(message.contains("L1"), "{message}");
    }

    #[test]
    fn method_with_fewer_than_k_pooled_patterns_is_a_hard_error() {
        let dir = tempfile::tempdir().unwrap();
        let labels = write_labels_tsv(dir.path(), FIXTURE_ROWS);
        // Method `b` only ranks 2 of the 5 pooled patterns; k=3 exceeds that.
        let entries: Vec<MappingEntryRef<'_>> = vec![
            ("L1", "p1", &[("a", 1), ("b", 1)]),
            ("L2", "p2", &[("a", 2), ("b", 2)]),
            ("L3", "p3", &[("a", 3)]),
            ("L4", "p4", &[("a", 4)]),
            ("L5", "p5", &[("a", 5)]),
        ];
        let mapping = write_mapping_json(dir.path(), &entries);

        let err = run_score_labels(&labels, &mapping, 3).unwrap_err();
        let message = err.to_string();
        assert!(message.contains('b'), "{message}");
        assert!(message.contains("fewer than k"), "{message}");
    }
}
