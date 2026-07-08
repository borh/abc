# Reusable Alignment Probe Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the first reusable alignment-probe slice: a spanless token-sequence alignment kernel, a standalone ABC alignment-probe schema, and a Rust TEI-EAJ Melos-shaped characterization adapter over self-contained fixtures.

**Architecture:** Keep the generic comparison kernel in `ab-diff-utils::align`, with no TEI, XML, AAT, parser-IR, or morphology dependencies. Keep TEI-EAJ interpretation in `ab-aat-to-parser-ir::tei_eaj_alignment_probe`, where XML/body comparison context can produce adapter diagnoses such as `tail_addition` and `segmentation_only`. Keep the existing Python TEI-EAJ comparison path unchanged in this slice.

**Tech Stack:** Rust 2024 workspace crates, `serde`, `serde_json`, `proptest`, `roxmltree`, JSON Schema draft 2020-12, ABC Clojure schema tests via `m3.json-schema`, Nix flake checks.

## Global Constraints

- Do not add CollateX as a dependency.
- Do not create a new `ab-diff-align` crate in this slice; start with `ab-diff-utils::align`.
- Do not rewrite or weaken `ab-morph-diff`; same-source span alignment remains the stronger tokenizer comparison path.
- Do not add PyO3, a Python subprocess protocol, or a migration of `abc/tools/tei_eaj_compare.py` in this slice.
- The generic kernel token type is exactly `ordinal`, `text`, and `normalized`.
- The generic kernel region kind set is exactly `equal`, `insertion`, `deletion`, `substitution`, `likely_moved_block`, and `unclassified_mismatch`.
- `segmentation_only` is an adapter diagnosis, not a generic region kind.
- TEI-EAJ tokenization uses XML parsing, not HTML parsing.
- TEI-EAJ tokenization uses body sentence/text-run units, not whole paragraphs, so one ABC `<p>` can align against many TEI-EAJ `<p>` elements.
- TEI-EAJ token text follows the current base-text exclusion policy: skip `rt`, `rp`, `note`, `span type=rt`, `span type=rp`, and `span rend=notes`.
- Probe output is informational report data, not ABC artifact identity.
- The reusable schema is `abc/schemas/alignment-probe-v1.schema.json` with version `0.1.0`.
- Every emitted probe carries `schema_version`, `evidence_level`, `algorithm_id`, `algorithm_config`, `algorithm_config_hash`, `tokenization_id`, and `normalization_id`.
- `algorithm_config_hash` is the `sha256:<hex>` hash of canonical JSON for `algorithm_config`.
- `diagnosis_counts` keys are adapter-defined and open-ended; their values are non-negative integers.
- The sum of `diagnosis_counts` equals `diagnosis_event_count`.
- `adapter_context` is optional, bounded, adapter-owned JSON object data.
- Full aligned tables are local debug output only; regular fixtures/reports store bounded samples.

---

## File Map

**Rust kernel**
- Modify: `ab-validator/crates/ab-diff-utils/Cargo.toml`
- Modify: `ab-validator/crates/ab-diff-utils/src/lib.rs`
- Modify: `ab-validator/crates/ab-diff-utils/src/hashing.rs`
- Create: `ab-validator/crates/ab-diff-utils/src/align.rs`

**Rust TEI-EAJ adapter**
- Modify: `ab-validator/Cargo.toml`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/Cargo.toml`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/lib.rs`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/main.rs`
- Create: `ab-validator/crates/ab-aat-to-parser-ir/src/tei_eaj_workset.rs`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/structural_probe.rs`
- Create: `ab-validator/crates/ab-aat-to-parser-ir/src/tei_eaj_alignment_probe.rs`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs`
- Create fixtures under: `ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/tei-eaj-alignment-probe/`

**ABC schema**
- Create: `abc/schemas/alignment-probe-v1.schema.json`
- Modify: `abc/schemas/tei-eaj-comparison.schema.json`
- Modify: `abc/test/abc/tools/schema_test.clj`
- Modify: `abc/test/abc/tools/tei_eaj_comparison_schema_test.clj`
- Create: `abc/fixtures/alignment-probe/tail-addition.valid.json`
- Create: `abc/fixtures/alignment-probe/bad-diagnosis-count.invariant-violation.json`
- Create: `abc/fixtures/alignment-probe/generic-segmentation-kind.invalid.json`

**Docs / generated evidence**
- Create: `abc/docs/handoffs/tei-eaj-alignment-probe-fixture-workset.json`
- Create: `abc/docs/handoffs/tei-eaj-alignment-probe-melos.json`
- Create: `abc/docs/handoffs/tei-eaj-alignment-probe-melos.md`

---

### Task 1: Generic Spanless Alignment Kernel

**Files:**
- Modify: `ab-validator/crates/ab-diff-utils/Cargo.toml`
- Modify: `ab-validator/crates/ab-diff-utils/src/lib.rs`
- Modify: `ab-validator/crates/ab-diff-utils/src/hashing.rs`
- Create: `ab-validator/crates/ab-diff-utils/src/align.rs`

**Interfaces:**
- Consumes: ordered `ComparisonToken` slices with `ordinal`, `text`, and `normalized`.
- Produces:
  - `pub fn align_pair(left: &[ComparisonToken], right: &[ComparisonToken], config: &AlignmentConfig) -> AlignmentResult`
  - `pub fn algorithm_config_hash(config: &AlignmentConfig) -> String`
  - `ComparisonEvidence`, shared by future report adapters.

- [ ] **Step 1: Add test dependency**

In `ab-validator/crates/ab-diff-utils/Cargo.toml`, add:

```toml
[dev-dependencies]
proptest.workspace = true
```

- [ ] **Step 2: Add canonical JSON hashing for probe configs**

In `ab-validator/crates/ab-diff-utils/src/hashing.rs`, add a JCS-compatible
canonicalizer for the integer/string/object/array JSON values used by alignment
configs:

```rust
/// Returns canonical JSON for ABC hashable values that do not contain floats.
///
/// This helper is intentionally conservative: it supports the JSON value kinds
/// used by alignment configs and returns an error for floating-point numbers.
pub fn canonical_json_string(value: &Value) -> anyhow::Result<String> {
    Ok(match value {
        Value::Null => "null".to_owned(),
        Value::Bool(value) => value.to_string(),
        Value::Number(value) => {
            if value.is_i64() || value.is_u64() {
                value.to_string()
            } else {
                anyhow::bail!("canonical_json_string does not support floating point numbers");
            }
        }
        Value::String(value) => serde_json::to_string(value)?,
        Value::Array(values) => {
            let inner = values
                .iter()
                .map(canonical_json_string)
                .collect::<anyhow::Result<Vec<_>>>()?
                .join(",");
            format!("[{inner}]")
        }
        Value::Object(values) => {
            let mut entries: Vec<_> = values.iter().collect();
            entries.sort_by(|(left, _), (right, _)| left.cmp(right));
            let inner = entries
                .into_iter()
                .map(|(key, value)| {
                    Ok(format!(
                        "{}:{}",
                        serde_json::to_string(key)?,
                        canonical_json_string(value)?
                    ))
                })
                .collect::<anyhow::Result<Vec<_>>>()?
                .join(",");
            format!("{{{inner}}}")
        }
    })
}

/// Hashes canonical JSON bytes and returns `"sha256:{hex}"`.
pub fn hash_json_canonical(value: &Value) -> anyhow::Result<String> {
    Ok(hash_bytes(canonical_json_string(value)?.as_bytes()))
}
```

Add tests in the same module:

```rust
#[test]
fn canonical_json_string_sorts_object_keys() {
    let value = serde_json::json!({"b": 2, "a": {"d": 4, "c": 3}});
    assert_eq!(
        canonical_json_string(&value).unwrap(),
        r#"{"a":{"c":3,"d":4},"b":2}"#
    );
}

#[test]
fn hash_json_canonical_matches_alignment_config_fixture_hash() {
    let value = serde_json::json!({
        "anchor_ngram_size": 3,
        "max_tokens_per_window": 512,
        "max_chars_per_window": 8192,
        "near_match": "disabled",
        "move_detection": "exact-normalized-sequence-v1",
        "scoring": {
            "match": 2,
            "gap": -1,
            "substitution": -1
        }
    });
    assert_eq!(
        hash_json_canonical(&value).unwrap(),
        "sha256:1d05a5ac38086965f8e995448b98f160a42ff03ed2fdfe7b09dbed9124be6d2c"
    );
}
```

- [ ] **Step 3: Export the align module and canonical hash helper**

In `ab-validator/crates/ab-diff-utils/src/lib.rs`, add:

```rust
pub mod align;
```

and replace the existing `pub use hashing::{...}` export with this export block:

```rust
pub use align::{
    AlignmentConfig, AlignmentKind, AlignmentRegion, AlignmentResult, AlignmentSummary,
    ComparisonEvidence, ComparisonToken, MoveDetection, NearMatch, ScoringConfig, align_pair,
    algorithm_config_hash,
};
pub use hashing::{
    canonical_json_string, hash_bytes, hash_json, hash_json_canonical, hash_string_sequence,
    hash_string_sequence_raw,
};
```

- [ ] **Step 4: Write failing kernel tests**

Create `ab-validator/crates/ab-diff-utils/src/align.rs` with the public model and test module first. The initial implementation can return `unimplemented!()` for `align_pair`; the tests below must fail before the implementation is added.

```rust
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ComparisonEvidence {
    SourceSpanAligned,
    TokenSequenceAligned,
    HashOnly,
    FirstDifferenceOnly,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ComparisonToken {
    pub ordinal: usize,
    pub text: String,
    pub normalized: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AlignmentKind {
    Equal,
    Insertion,
    Deletion,
    Substitution,
    LikelyMovedBlock,
    UnclassifiedMismatch,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AlignmentRegion {
    pub kind: AlignmentKind,
    pub left_range: [usize; 2],
    pub right_range: [usize; 2],
    pub left_text_sample: String,
    pub right_text_sample: String,
    pub truncated: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AlignmentSummary {
    pub total_regions: usize,
    pub equal_regions: usize,
    pub insertion_regions: usize,
    pub deletion_regions: usize,
    pub substitution_regions: usize,
    pub likely_moved_block_regions: usize,
    pub unclassified_mismatch_regions: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AlignmentResult {
    pub summary: AlignmentSummary,
    pub regions: Vec<AlignmentRegion>,
    pub truncated: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ScoringConfig {
    pub r#match: i32,
    pub gap: i32,
    pub substitution: i32,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum NearMatch {
    Disabled,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum MoveDetection {
    Disabled,
    ExactNormalizedSequenceV1,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AlignmentConfig {
    pub anchor_ngram_size: usize,
    pub max_tokens_per_window: usize,
    pub max_chars_per_window: usize,
    pub near_match: NearMatch,
    pub move_detection: MoveDetection,
    pub scoring: ScoringConfig,
}

impl Default for AlignmentConfig {
    fn default() -> Self {
        Self {
            anchor_ngram_size: 3,
            max_tokens_per_window: 512,
            max_chars_per_window: 8192,
            near_match: NearMatch::Disabled,
            move_detection: MoveDetection::ExactNormalizedSequenceV1,
            scoring: ScoringConfig {
                r#match: 2,
                gap: -1,
                substitution: -1,
            },
        }
    }
}

pub fn algorithm_config_hash(config: &AlignmentConfig) -> String {
    let value = serde_json::to_value(config).expect("alignment config must serialize");
    crate::hashing::hash_json_canonical(&value).expect("alignment config must canonicalize")
}

pub fn align_pair(
    _left: &[ComparisonToken],
    _right: &[ComparisonToken],
    _config: &AlignmentConfig,
) -> AlignmentResult {
    unimplemented!("Task 1 implements the alignment kernel")
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    fn tok(ordinal: usize, text: &str) -> ComparisonToken {
        ComparisonToken {
            ordinal,
            text: text.to_owned(),
            normalized: text.to_owned(),
        }
    }

    #[test]
    fn equal_sequences_emit_one_equal_region() {
        let tokens = vec![tok(0, "メロス"), tok(1, "は"), tok(2, "激怒した。")];
        let result = align_pair(&tokens, &tokens, &AlignmentConfig::default());
        assert_eq!(result.summary.total_regions, 1);
        assert_eq!(result.summary.equal_regions, 1);
        assert_eq!(result.regions[0].kind, AlignmentKind::Equal);
        assert_eq!(result.regions[0].left_range, [0, 3]);
        assert_eq!(result.regions[0].right_range, [0, 3]);
    }

    #[test]
    fn tail_insertion_is_single_insertion_region() {
        let left = vec![tok(0, "勇者は、ひどく赤面した。"), tok(1, "（古伝説と、シルレルの詩から。）")];
        let right = vec![tok(0, "勇者は、ひどく赤面した。")];
        let result = align_pair(&left, &right, &AlignmentConfig::default());
        assert_eq!(result.summary.insertion_regions, 1);
        assert_eq!(result.regions.last().unwrap().kind, AlignmentKind::Insertion);
        assert_eq!(result.regions.last().unwrap().left_text_sample, "（古伝説と、シルレルの詩から。）");
        assert_eq!(result.regions.last().unwrap().right_text_sample, "");
    }

    #[test]
    fn one_token_difference_is_substitution_not_delete_plus_insert() {
        let left = vec![tok(0, "メロス"), tok(1, "怒った")];
        let right = vec![tok(0, "メロス"), tok(1, "激怒した")];
        let result = align_pair(&left, &right, &AlignmentConfig::default());
        assert_eq!(result.summary.substitution_regions, 1);
        assert_eq!(result.summary.insertion_regions, 0);
        assert_eq!(result.summary.deletion_regions, 0);
    }

    #[test]
    fn oversized_window_becomes_truncated_unclassified_region() {
        let config = AlignmentConfig {
            max_tokens_per_window: 1,
            max_chars_per_window: 4,
            ..AlignmentConfig::default()
        };
        let left = vec![tok(0, "abcdef"), tok(1, "ghijkl")];
        let right = vec![tok(0, "mnopqr"), tok(1, "stuvwx")];
        let result = align_pair(&left, &right, &config);
        assert!(result.truncated);
        assert_eq!(result.regions[0].kind, AlignmentKind::UnclassifiedMismatch);
        assert!(result.regions[0].truncated);
        assert_eq!(result.regions[0].left_text_sample, "abcdef");
    }

    proptest! {
        #[test]
        fn equal_normalized_sequences_are_one_equal_region(values in proptest::collection::vec("[ぁ-んァ-ン一-龯]{1,4}", 1..20)) {
            let tokens: Vec<_> = values.iter().enumerate().map(|(i, value)| tok(i, value)).collect();
            let result = align_pair(&tokens, &tokens, &AlignmentConfig::default());
            prop_assert_eq!(result.summary.total_regions, 1);
            prop_assert_eq!(result.summary.equal_regions, 1);
            prop_assert!(!result.truncated);
        }
    }
}
```

- [ ] **Step 5: Run the failing kernel tests**

Run:

```bash
cd ab-validator
cargo test -p ab-diff-utils align::tests -- --nocapture
```

Expected: fails because `align_pair` is not implemented.

- [ ] **Step 6: Implement the minimal deterministic kernel**

Replace `align_pair` with:

```rust
pub fn align_pair(
    left: &[ComparisonToken],
    right: &[ComparisonToken],
    config: &AlignmentConfig,
) -> AlignmentResult {
    if window_exceeds_limits(left, right, config) {
        let region = make_region(
            AlignmentKind::UnclassifiedMismatch,
            left,
            right,
            0,
            left.len(),
            0,
            right.len(),
            true,
            config,
        );
        return summarize(vec![region]);
    }

    let script = edit_script(left, right, config);
    summarize(coalesce_script(left, right, &script, config))
}
```

Add these private helpers in the same file:

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Edit {
    Equal(usize, usize),
    Insert(usize),
    Delete(usize),
    Substitute(usize, usize),
}

fn window_exceeds_limits(
    left: &[ComparisonToken],
    right: &[ComparisonToken],
    config: &AlignmentConfig,
) -> bool {
    let token_count = left.len().max(right.len());
    let char_count: usize = left
        .iter()
        .chain(right.iter())
        .map(|token| token.text.chars().count())
        .sum();
    token_count > config.max_tokens_per_window || char_count > config.max_chars_per_window
}

fn edit_script(
    left: &[ComparisonToken],
    right: &[ComparisonToken],
    config: &AlignmentConfig,
) -> Vec<Edit> {
    let n = left.len();
    let m = right.len();
    let mut score = vec![vec![0_i32; m + 1]; n + 1];
    for i in 1..=n {
        score[i][0] = score[i - 1][0] + config.scoring.gap;
    }
    for j in 1..=m {
        score[0][j] = score[0][j - 1] + config.scoring.gap;
    }
    for i in 1..=n {
        for j in 1..=m {
            let diagonal = score[i - 1][j - 1]
                + if left[i - 1].normalized == right[j - 1].normalized {
                    config.scoring.r#match
                } else {
                    config.scoring.substitution
                };
            let delete = score[i - 1][j] + config.scoring.gap;
            let insert = score[i][j - 1] + config.scoring.gap;
            score[i][j] = diagonal.max(delete).max(insert);
        }
    }

    let mut edits = Vec::new();
    let mut i = n;
    let mut j = m;
    while i > 0 || j > 0 {
        if i > 0 && j > 0 {
            let diagonal = score[i - 1][j - 1]
                + if left[i - 1].normalized == right[j - 1].normalized {
                    config.scoring.r#match
                } else {
                    config.scoring.substitution
                };
            if score[i][j] == diagonal {
                if left[i - 1].normalized == right[j - 1].normalized {
                    edits.push(Edit::Equal(i - 1, j - 1));
                } else {
                    edits.push(Edit::Substitute(i - 1, j - 1));
                }
                i -= 1;
                j -= 1;
                continue;
            }
        }
        if i > 0 && score[i][j] == score[i - 1][j] + config.scoring.gap {
            edits.push(Edit::Insert(i - 1));
            i -= 1;
        } else {
            edits.push(Edit::Delete(j - 1));
            j -= 1;
        }
    }
    edits.reverse();
    edits
}
```

Implement `coalesce_script`, `make_region`, `sample_text`, and `summarize` so adjacent edits with the same `AlignmentKind` become one region, `left_range`/`right_range` are half-open token index ranges, samples are joined token text, and samples are truncated to `DEFAULT_MAX_SAMPLE_CHARS = 160`. Report adapters may apply stricter output sampling limits before writing probe JSON.

- [ ] **Step 7: Run kernel tests to green**

Run:

```bash
cd ab-validator
cargo test -p ab-diff-utils align::tests -- --nocapture
```

Expected: all `align::tests` pass.

- [ ] **Step 8: Commit Task 1**

```bash
git add ab-validator/crates/ab-diff-utils/Cargo.toml \
        ab-validator/crates/ab-diff-utils/src/lib.rs \
        ab-validator/crates/ab-diff-utils/src/hashing.rs \
        ab-validator/crates/ab-diff-utils/src/align.rs
git commit -m "feat(diff-utils): add spanless alignment kernel"
```

---

### Task 2: Anchor Partitioning and Exact Move Detection

**Files:**
- Modify: `ab-validator/crates/ab-diff-utils/src/align.rs`

**Interfaces:**
- Consumes: Task 1 `AlignmentConfig`, `AlignmentRegion`, and `align_pair`.
- Produces: stable repeated-token handling, anchor-split windows, and `likely_moved_block` classification for exact normalized unmatched sequences.

- [ ] **Step 1: Add failing anchor and move tests**

Add these tests to `align::tests`:

```rust
#[test]
fn repeated_tokens_keep_stable_order() {
    let left = vec![tok(0, "A"), tok(1, "B"), tok(2, "A"), tok(3, "C")];
    let right = vec![tok(0, "A"), tok(1, "A"), tok(2, "B"), tok(3, "C")];
    let result = align_pair(&left, &right, &AlignmentConfig::default());
    assert!(result.regions.iter().any(|region| region.kind == AlignmentKind::Equal));
    assert_eq!(result.regions.last().unwrap().kind, AlignmentKind::Equal);
    assert_eq!(result.regions.last().unwrap().left_text_sample, "C");
}

#[test]
fn unique_anchor_splits_oversized_outer_window() {
    let config = AlignmentConfig {
        anchor_ngram_size: 1,
        max_tokens_per_window: 2,
        max_chars_per_window: 32,
        ..AlignmentConfig::default()
    };
    let left = vec![tok(0, "L1"), tok(1, "L2"), tok(2, "ANCHOR"), tok(3, "L3")];
    let right = vec![tok(0, "R1"), tok(1, "R2"), tok(2, "ANCHOR"), tok(3, "R3")];
    let result = align_pair(&left, &right, &config);
    assert!(result.regions.iter().any(|region| {
        region.kind == AlignmentKind::Equal && region.left_text_sample == "ANCHOR"
    }));
    assert!(result.regions.len() >= 3);
}

#[test]
fn exact_unmatched_sequence_can_be_marked_likely_moved() {
    let left = vec![tok(0, "A"), tok(1, "X"), tok(2, "B")];
    let right = vec![tok(0, "A"), tok(1, "B"), tok(2, "X")];
    let result = align_pair(&left, &right, &AlignmentConfig::default());
    assert!(result
        .regions
        .iter()
        .any(|region| region.kind == AlignmentKind::LikelyMovedBlock));
}
```

- [ ] **Step 2: Run the new tests and verify failure**

Run:

```bash
cd ab-validator
cargo test -p ab-diff-utils align::tests -- --nocapture
```

Expected: the anchor/move tests fail before implementation.

- [ ] **Step 3: Add anchor partitioning**

Add private functions:

```rust
fn unique_anchors(
    left: &[ComparisonToken],
    right: &[ComparisonToken],
    ngram_size: usize,
) -> Vec<(usize, usize)> {
    if ngram_size == 0 {
        return Vec::new();
    }
    let mut left_counts = std::collections::BTreeMap::<Vec<String>, Vec<usize>>::new();
    let mut right_counts = std::collections::BTreeMap::<Vec<String>, Vec<usize>>::new();
    for (idx, window) in left.windows(ngram_size).enumerate() {
        left_counts
            .entry(window.iter().map(|token| token.normalized.clone()).collect())
            .or_default()
            .push(idx);
    }
    for (idx, window) in right.windows(ngram_size).enumerate() {
        right_counts
            .entry(window.iter().map(|token| token.normalized.clone()).collect())
            .or_default()
            .push(idx);
    }
    let mut anchors = Vec::new();
    for (ngram, left_positions) in left_counts {
        if left_positions.len() == 1
            && let Some(right_positions) = right_counts.get(&ngram)
            && right_positions.len() == 1
        {
            anchors.push((left_positions[0], right_positions[0]));
        }
    }
    anchors.sort();
    non_crossing_anchors(anchors)
}

fn non_crossing_anchors(anchors: Vec<(usize, usize)>) -> Vec<(usize, usize)> {
    let mut result = Vec::new();
    let mut last_right = None;
    for (left_idx, right_idx) in anchors {
        if last_right.is_none_or(|prev| right_idx > prev) {
            result.push((left_idx, right_idx));
            last_right = Some(right_idx);
        }
    }
    result
}
```

Update `align_pair` so it aligns windows between unique anchors and emits equal anchor regions. Use the Task 1 DP inside each bounded window.

- [ ] **Step 4: Add exact move detection**

After `coalesce_script`, add a pass:

```rust
fn mark_exact_moves(regions: &mut [AlignmentRegion]) {
    let insertions: Vec<(usize, String)> = regions
        .iter()
        .enumerate()
        .filter(|(_, region)| region.kind == AlignmentKind::Insertion)
        .map(|(idx, region)| (idx, region.left_text_sample.clone()))
        .collect();
    let deletions: Vec<(usize, String)> = regions
        .iter()
        .enumerate()
        .filter(|(_, region)| region.kind == AlignmentKind::Deletion)
        .map(|(idx, region)| (idx, region.right_text_sample.clone()))
        .collect();
    for (insert_idx, inserted) in &insertions {
        for (delete_idx, deleted) in &deletions {
            if !inserted.is_empty() && inserted == deleted {
                regions[*insert_idx].kind = AlignmentKind::LikelyMovedBlock;
                regions[*delete_idx].kind = AlignmentKind::LikelyMovedBlock;
            }
        }
    }
}
```

Call this only when `config.move_detection == MoveDetection::ExactNormalizedSequenceV1`.

- [ ] **Step 5: Run full alignment tests**

Run:

```bash
cd ab-validator
cargo test -p ab-diff-utils align -- --nocapture
```

Expected: Task 1 and Task 2 alignment tests pass.

- [ ] **Step 6: Commit Task 2**

```bash
git add ab-validator/crates/ab-diff-utils/src/align.rs
git commit -m "feat(diff-utils): add anchor and move alignment cases"
```

---

### Task 3: Standalone Alignment Probe Schema

**Files:**
- Create: `abc/schemas/alignment-probe-v1.schema.json`
- Modify: `abc/schemas/tei-eaj-comparison.schema.json`
- Modify: `abc/test/abc/tools/schema_test.clj`
- Modify: `abc/test/abc/tools/tei_eaj_comparison_schema_test.clj`
- Create: `abc/fixtures/alignment-probe/tail-addition.valid.json`
- Create: `abc/fixtures/alignment-probe/bad-diagnosis-count.invariant-violation.json`
- Create: `abc/fixtures/alignment-probe/generic-segmentation-kind.invalid.json`

**Interfaces:**
- Consumes: the probe contract in `abc/docs/superpowers/specs/2026-07-08-reusable-alignment-probe-design.md`.
- Produces: `$ref`-able schema and fixtures for later Rust-generated probe output.

- [ ] **Step 1: Create valid fixture**

Create `abc/fixtures/alignment-probe/tail-addition.valid.json`:

```json
{
  "schema_version": "alignment-probe-v1",
  "evidence_level": "token_sequence_aligned",
  "algorithm_id": "abc-pairwise-token-align-v1",
  "algorithm_config_hash": "sha256:1d05a5ac38086965f8e995448b98f160a42ff03ed2fdfe7b09dbed9124be6d2c",
  "algorithm_config": {
    "anchor_ngram_size": 3,
    "max_tokens_per_window": 512,
    "max_chars_per_window": 8192,
    "near_match": "disabled",
    "move_detection": "exact-normalized-sequence-v1",
    "scoring": {
      "match": 2,
      "gap": -1,
      "substitution": -1
    }
  },
  "tokenization_id": "tei-body-sentence-like-text-run-v1",
  "normalization_id": "tei-eaj-base-text-no-ws-v1",
  "left_witness": "abc",
  "right_witness": "tei_eaj",
  "summary": {
    "total_regions": 2,
    "equal_regions": 1,
    "insertion_regions": 1,
    "deletion_regions": 0,
    "substitution_regions": 0,
    "likely_moved_block_regions": 0,
    "unclassified_mismatch_regions": 0
  },
  "diagnosis_event_count": 1,
  "diagnosis_counts": {
    "tail_addition": 1
  },
  "samples": [
    {
      "kind": "insertion",
      "diagnosis": "tail_addition",
      "left_range": [1, 2],
      "right_range": [1, 1],
      "left_text": "（古伝説と、シルレルの詩から。）",
      "right_text": "",
      "truncated": false,
      "adapter_context": {
        "left_path": "/TEI/text/body/p[2]",
        "right_path": null,
        "left_features": ["source-attribution"],
        "right_features": []
      }
    }
  ],
  "truncated": false,
  "limits": {
    "max_tokens_per_window": 512,
    "max_samples": 8,
    "max_sample_chars": 160
  }
}
```

- [ ] **Step 2: Create invalid fixtures**

Create `abc/fixtures/alignment-probe/bad-diagnosis-count.invariant-violation.json` by copying the valid fixture and changing:

```json
"diagnosis_event_count": 2
```

Create `abc/fixtures/alignment-probe/generic-segmentation-kind.invalid.json` by copying the valid fixture and changing:

```json
"kind": "segmentation"
```

- [ ] **Step 3: Add schema**

Create `abc/schemas/alignment-probe-v1.schema.json` with:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://w3id.org/abc/schemas/alignment-probe-v1.schema.json",
  "title": "ABC Alignment Probe v1",
  "version": "0.1.0",
  "$comment": "summary counts are exhaustive over kernel regions. diagnosis_counts keys are adapter-defined. sum(diagnosis_counts values) must equal diagnosis_event_count. adapter_context is adapter-owned and carries no generic semantics.",
  "type": "object",
  "additionalProperties": false,
  "required": [
    "schema_version",
    "evidence_level",
    "algorithm_id",
    "algorithm_config_hash",
    "algorithm_config",
    "tokenization_id",
    "normalization_id",
    "left_witness",
    "right_witness",
    "summary",
    "diagnosis_event_count",
    "diagnosis_counts",
    "samples",
    "truncated",
    "limits"
  ],
  "properties": {
    "schema_version": { "const": "alignment-probe-v1" },
    "evidence_level": {
      "enum": [
        "source_span_aligned",
        "token_sequence_aligned",
        "hash_only",
        "first_difference_only"
      ]
    },
    "algorithm_id": { "type": "string", "minLength": 1 },
    "algorithm_config_hash": { "$ref": "#/$defs/hash" },
    "algorithm_config": { "$ref": "#/$defs/algorithmConfig" },
    "tokenization_id": { "type": "string", "minLength": 1 },
    "normalization_id": { "type": "string", "minLength": 1 },
    "left_witness": { "type": "string", "minLength": 1 },
    "right_witness": { "type": "string", "minLength": 1 },
    "summary": { "$ref": "#/$defs/summary" },
    "diagnosis_event_count": { "$ref": "#/$defs/nonNegativeInteger" },
    "diagnosis_counts": {
      "type": "object",
      "propertyNames": { "type": "string", "minLength": 1 },
      "additionalProperties": { "$ref": "#/$defs/nonNegativeInteger" }
    },
    "samples": {
      "type": "array",
      "items": { "$ref": "#/$defs/sample" }
    },
    "truncated": { "type": "boolean" },
    "limits": { "$ref": "#/$defs/limits" }
  },
  "$defs": {
    "hash": {
      "type": "string",
      "pattern": "^sha256:[0-9a-f]{64}$"
    },
    "nonNegativeInteger": {
      "type": "integer",
      "minimum": 0
    },
    "range": {
      "type": "array",
      "prefixItems": [
        { "$ref": "#/$defs/nonNegativeInteger" },
        { "$ref": "#/$defs/nonNegativeInteger" }
      ],
      "items": false,
      "minItems": 2,
      "maxItems": 2
    },
    "regionKind": {
      "enum": [
        "equal",
        "insertion",
        "deletion",
        "substitution",
        "likely_moved_block",
        "unclassified_mismatch"
      ]
    },
    "algorithmConfig": {
      "type": "object",
      "additionalProperties": true,
      "required": [
        "anchor_ngram_size",
        "max_tokens_per_window",
        "max_chars_per_window",
        "near_match",
        "move_detection",
        "scoring"
      ],
      "properties": {
        "anchor_ngram_size": { "$ref": "#/$defs/nonNegativeInteger" },
        "max_tokens_per_window": { "$ref": "#/$defs/nonNegativeInteger" },
        "max_chars_per_window": { "$ref": "#/$defs/nonNegativeInteger" },
        "near_match": { "type": "string", "minLength": 1 },
        "move_detection": { "type": "string", "minLength": 1 },
        "scoring": {
          "type": "object",
          "additionalProperties": { "type": "integer" }
        }
      }
    },
    "summary": {
      "type": "object",
      "additionalProperties": false,
      "required": [
        "total_regions",
        "equal_regions",
        "insertion_regions",
        "deletion_regions",
        "substitution_regions",
        "likely_moved_block_regions",
        "unclassified_mismatch_regions"
      ],
      "properties": {
        "total_regions": { "$ref": "#/$defs/nonNegativeInteger" },
        "equal_regions": { "$ref": "#/$defs/nonNegativeInteger" },
        "insertion_regions": { "$ref": "#/$defs/nonNegativeInteger" },
        "deletion_regions": { "$ref": "#/$defs/nonNegativeInteger" },
        "substitution_regions": { "$ref": "#/$defs/nonNegativeInteger" },
        "likely_moved_block_regions": { "$ref": "#/$defs/nonNegativeInteger" },
        "unclassified_mismatch_regions": { "$ref": "#/$defs/nonNegativeInteger" }
      }
    },
    "sample": {
      "type": "object",
      "additionalProperties": false,
      "required": [
        "kind",
        "diagnosis",
        "left_range",
        "right_range",
        "left_text",
        "right_text",
        "truncated"
      ],
      "properties": {
        "kind": { "$ref": "#/$defs/regionKind" },
        "diagnosis": { "type": "string", "minLength": 1 },
        "left_range": { "$ref": "#/$defs/range" },
        "right_range": { "$ref": "#/$defs/range" },
        "left_text": { "type": "string" },
        "right_text": { "type": "string" },
        "truncated": { "type": "boolean" },
        "adapter_context": {
          "type": "object",
          "additionalProperties": true,
          "maxProperties": 32
        }
      }
    },
    "limits": {
      "type": "object",
      "additionalProperties": false,
      "required": [
        "max_tokens_per_window",
        "max_samples",
        "max_sample_chars"
      ],
      "properties": {
        "max_tokens_per_window": { "$ref": "#/$defs/nonNegativeInteger" },
        "max_samples": { "$ref": "#/$defs/nonNegativeInteger" },
        "max_sample_chars": { "$ref": "#/$defs/nonNegativeInteger" }
      }
    }
  }
}
```

- [ ] **Step 4: Reference schema from TEI-EAJ comparison schema**

In `abc/schemas/tei-eaj-comparison.schema.json`, add this property under `$defs.fileComparison.properties`:

```json
"alignment_probe": {
  "anyOf": [
    { "$ref": "alignment-probe-v1.schema.json" },
    { "type": "null" }
  ]
}
```

Do not add `alignment_probe` to the `required` list.

- [ ] **Step 5: Add Clojure schema tests**

In `abc/test/abc/tools/schema_test.clj`, add the schema version:

```clojure
"schemas/alignment-probe-v1.schema.json" "0.1.0"
```

Extend the namespace requires with:

```clojure
[abc.tools.hash :as hash]
```

Add this test:

```clojure
(defn- diagnosis-count-sum [probe]
  (reduce + (vals (get probe "diagnosis_counts"))))

(deftest alignment-probe-schema-fixtures-validate-test
  (let [probe-schema (schema/read-schema "schemas/alignment-probe-v1.schema.json")
        valid (files/read-json "fixtures/alignment-probe/tail-addition.valid.json")
        bad-count (files/read-json "fixtures/alignment-probe/bad-diagnosis-count.invariant-violation.json")
        bad-kind (files/read-json "fixtures/alignment-probe/generic-segmentation-kind.invalid.json")]
    (is (= "alignment-probe-v1" (get valid "schema_version")))
    (is (nil? (schema/validation-errors probe-schema valid)))
    (is (seq (schema/validation-errors probe-schema bad-kind)))
    (is (= (get valid "algorithm_config_hash")
           (hash/format-sha256
            (hash/sha256-json-jcs (get valid "algorithm_config")))))
    (is (= (get valid "diagnosis_event_count")
           (diagnosis-count-sum valid)))
    (is (not= (get bad-count "diagnosis_event_count")
              (diagnosis-count-sum bad-count)))))
```

The `bad-diagnosis-count.invariant-violation.json` fixture is expected to pass
JSON Schema shape validation and fail the semantic invariant above. JSON Schema
draft 2020-12 cannot express the arithmetic relation between an object value
sum and a sibling field.

In `abc/test/abc/tools/tei_eaj_comparison_schema_test.clj`, add:

```clojure
(deftest tei-eaj-comparison-export-accepts-optional-alignment-probe-test
  (let [comparison-schema (schema/read-schema comparison-schema-path)
        fixture (files/read-json comparison-fixture-path)
        probe (files/read-json "fixtures/alignment-probe/tail-addition.valid.json")
        fixture-with-probe (assoc-in fixture ["files" 0 "alignment_probe"] probe)]
    (is (nil? (schema/validation-errors comparison-schema fixture-with-probe)))))
```

- [ ] **Step 6: Run focused ABC schema tests**

Run:

```bash
nix build .#checks.x86_64-linux.abc-tei-eaj-comparison-tests --print-build-logs
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: both checks pass.

- [ ] **Step 7: Commit Task 3**

```bash
git add abc/schemas/alignment-probe-v1.schema.json \
        abc/schemas/tei-eaj-comparison.schema.json \
        abc/test/abc/tools/schema_test.clj \
        abc/test/abc/tools/tei_eaj_comparison_schema_test.clj \
        abc/fixtures/alignment-probe/tail-addition.valid.json \
        abc/fixtures/alignment-probe/bad-diagnosis-count.invariant-violation.json \
        abc/fixtures/alignment-probe/generic-segmentation-kind.invalid.json
git commit -m "feat(abc): add alignment probe schema"
```

---

### Task 4: Rust TEI-EAJ Alignment Probe Adapter

**Files:**
- Modify: `ab-validator/Cargo.toml`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/Cargo.toml`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/lib.rs`
- Create: `ab-validator/crates/ab-aat-to-parser-ir/src/tei_eaj_workset.rs`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/structural_probe.rs`
- Create: `ab-validator/crates/ab-aat-to-parser-ir/src/tei_eaj_alignment_probe.rs`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs`

**Interfaces:**
- Consumes:
  - `ab_diff_utils::align::{align_pair, AlignmentConfig, ComparisonToken}`
  - existing TEI-EAJ workset export JSON rows.
- Produces:
  - `pub fn run_tei_eaj_alignment_probe(config: TeiEajAlignmentProbeConfig) -> anyhow::Result<TeiEajAlignmentProbeReport>`
  - `pub fn write_tei_eaj_alignment_probe_reports(report: &TeiEajAlignmentProbeReport, summary_json: &Path, report_md: &Path) -> anyhow::Result<()>`

- [ ] **Step 1: Add crate dependencies**

In `ab-validator/Cargo.toml`, add the XML parser dependency under
`[workspace.dependencies]`:

```toml
roxmltree = "0.20"
```

In `ab-validator/crates/ab-aat-to-parser-ir/Cargo.toml`, add:

```toml
ab-diff-utils.workspace = true
roxmltree.workspace = true
```

If either dependency already exists in the file, keep the existing entry and do not duplicate it.

- [ ] **Step 2: Extract the shared TEI-EAJ workset model**

Create `ab-validator/crates/ab-aat-to-parser-ir/src/tei_eaj_workset.rs`:

```rust
use std::{path::Path, path::PathBuf};

use anyhow::{Context, Result};
use serde::Deserialize;

use crate::schema::read_json;

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct TeiEajWorksetExport {
    pub(crate) schema_version: String,
    pub(crate) summary: TeiEajExportSummary,
    #[serde(default)]
    pub(crate) tei_eaj_source: Option<TeiEajSourceExport>,
    #[serde(default)]
    pub(crate) candidate_work_ids: Vec<String>,
    #[serde(default)]
    pub(crate) missing_abc_counterpart_work_ids: Vec<String>,
    #[serde(default)]
    pub(crate) no_work_id_files: Vec<String>,
    #[serde(default)]
    pub(crate) files: Vec<TeiEajFileExport>,
}

#[derive(Debug, Clone, Default, Deserialize)]
pub(crate) struct TeiEajExportSummary {
    #[serde(default)]
    pub(crate) tei_eaj_file_count: u64,
    #[serde(default)]
    pub(crate) tei_eaj_work_id_count: u64,
    #[serde(default)]
    pub(crate) compared_file_count: u64,
    #[serde(default)]
    pub(crate) missing_counterpart_count: u64,
    #[serde(default)]
    pub(crate) no_work_id_count: u64,
}

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct TeiEajSourceExport {
    pub(crate) revision: Option<String>,
    pub(crate) root: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct TeiEajFileExport {
    pub(crate) work_id: Option<String>,
    pub(crate) title: Option<String>,
    pub(crate) tei_eaj_file: String,
    pub(crate) level: Option<String>,
    pub(crate) state: Option<String>,
    pub(crate) comparison_status: String,
    pub(crate) abc_tei: Option<String>,
    pub(crate) tei_eaj_p_count: Option<u64>,
    pub(crate) tei_eaj_note_count: Option<u64>,
    pub(crate) abc_p_count: Option<u64>,
    pub(crate) abc_note_count: Option<u64>,
    pub(crate) base_text_equal: Option<bool>,
}

pub(crate) fn read_tei_eaj_workset(path: &Path) -> Result<TeiEajWorksetExport> {
    let workset_value = read_json(path)
        .with_context(|| format!("failed to read TEI-EAJ workset {}", path.display()))?;
    serde_json::from_value(workset_value)
        .with_context(|| format!("failed to parse TEI-EAJ workset {}", path.display()))
}

pub(crate) fn resolve_tei_eaj_path(workset: &TeiEajWorksetExport, relative: &str) -> PathBuf {
    let path = PathBuf::from(relative);
    if path.is_absolute() {
        return path;
    }
    workset
        .tei_eaj_source
        .as_ref()
        .and_then(|source| source.root.as_deref())
        .map_or(path.clone(), |root| Path::new(root).join(path))
}
```

Modify `ab-validator/crates/ab-aat-to-parser-ir/src/structural_probe.rs`:

- remove the private `TeiEajWorksetExport`, `TeiEajExportSummary`,
  `TeiEajSourceExport`, and `TeiEajFileExport` definitions;
- remove `schema::read_json` from the existing `use crate::{...}` import;
- import the shared model:

```rust
use crate::tei_eaj_workset::{read_tei_eaj_workset, TeiEajFileExport, TeiEajWorksetExport};
```

- replace the existing `read_json` plus `serde_json::from_value` block in
  `run_tei_eaj_structural_expansion` with:

```rust
let workset = read_tei_eaj_workset(&config.workset_path)?;
```

Keep `impl TeiEajFileExport { fn to_summary(...) }` in `structural_probe.rs`;
the fields are `pub(crate)` specifically so both structural and alignment probes
use the same parsed workset value without creating a second dialect.

- [ ] **Step 3: Export modules**

In `ab-validator/crates/ab-aat-to-parser-ir/src/lib.rs`, add:

```rust
pub(crate) mod tei_eaj_workset;
pub mod tei_eaj_alignment_probe;
```

- [ ] **Step 4: Add failing Melos-shaped adapter test**

In `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs`, add:

```rust
#[test]
fn tei_eaj_alignment_probe_classifies_melos_tail_addition() {
    let temp = tempfile::tempdir().unwrap();
    let workset_path = temp.path().join("workset.json");
    let abc_tei = temp.path().join("abc-melos.xml");
    let tei_eaj_root = temp.path().join("tei-eaj");
    let tei_eaj_file = tei_eaj_root.join("data/complete/tei_lib_lv4/1567_tei.xml");
    std::fs::create_dir_all(tei_eaj_file.parent().unwrap()).unwrap();

    std::fs::write(
        &abc_tei,
        r#"<TEI xmlns="http://www.tei-c.org/ns/1.0"><text><body><p>メロスは激怒した。勇者は、ひどく<ruby><rb>赤面した</rb><rt>せきめんした</rt></ruby>。<note>底本注</note>（古伝説と、シルレルの詩から。）</p></body></text></TEI>"#,
    )
    .unwrap();
    std::fs::write(
        &tei_eaj_file,
        r#"<TEI xmlns="http://www.tei-c.org/ns/1.0"><text><body><p>メロスは激怒した。</p><p>勇者は、ひどく赤面した。</p></body></text></TEI>"#,
    )
    .unwrap();
    std::fs::write(
        &workset_path,
        format!(
            r#"{{
  "schema_version": "tei-eaj-aozora-workset-export-v1",
  "tei_eaj_source": {{"revision": "fixture", "root": "{}"}},
  "abc_inputs": {{"counterparts": [{{"path": "{}", "work_id": "1567"}}], "tei_dirs": [], "tei_specs": []}},
  "summary": {{
    "tei_eaj_file_count": 1,
    "tei_eaj_work_id_count": 1,
    "abc_counterpart_count": 1,
    "compared_file_count": 1,
    "missing_counterpart_count": 0,
    "no_work_id_count": 0,
    "uncompared_file_count": 0,
    "base_text_equal_count": 0,
    "base_text_mismatch_count": 1,
    "base_text_relation_counts": {{"tei_eaj_subset_of_abc": 1}}
  }},
  "candidate_work_ids": ["1567"],
  "missing_abc_counterpart_work_ids": [],
  "no_work_id_files": [],
  "files": [{{
    "work_id": "1567",
    "work_id_method": "filename_work_id",
    "tei_eaj_file": "data/complete/tei_lib_lv4/1567_tei.xml",
    "state": "complete",
    "level": "Level 4",
    "title": "走れメロス",
    "abc_tei": "{}",
    "comparison_status": "compared",
    "base_text_equal": false,
    "base_text_relation": "tei_eaj_subset_of_abc",
    "base_text_length_delta": 16,
    "abc_body_base_text_length": 40,
    "tei_eaj_body_base_text_length": 24,
    "abc_p_count": 1,
    "tei_eaj_p_count": 2,
    "abc_note_count": 1,
    "tei_eaj_note_count": 0,
    "first_difference": {{
      "index": 15,
      "abc": "メロスは激怒した。勇者は、ひどく赤面した。（古伝説と、シルレルの詩から。）",
      "tei_eaj": "メロスは激怒した。勇者は、ひどく赤面した。"
    }}
  }}]
}}"#,
            tei_eaj_root.display(),
            abc_tei.display(),
            abc_tei.display()
        ),
    )
    .unwrap();

    let report = ab_aat_to_parser_ir::tei_eaj_alignment_probe::run_tei_eaj_alignment_probe(
        ab_aat_to_parser_ir::tei_eaj_alignment_probe::TeiEajAlignmentProbeConfig {
            workset_path,
            max_rows: None,
        },
    )
    .unwrap();

    assert_eq!(report.rows.len(), 1);
    let probe = report.rows[0].alignment_probe.as_ref().unwrap();
    assert_eq!(probe.diagnosis_counts.get("tail_addition"), Some(&1));
    assert_eq!(
        probe.diagnosis_event_count,
        probe.diagnosis_counts.values().sum::<usize>()
    );
    assert_eq!(probe.samples[0].diagnosis, "tail_addition");
    assert_eq!(probe.samples[0].left_text, "（古伝説と、シルレルの詩から。）");
    assert!(!probe.samples[0].left_text.contains("せきめん"));
    assert!(!probe.samples[0].left_text.contains("底本注"));
    assert_eq!(
        probe.samples[0]
            .adapter_context
            .get("left_features")
            .and_then(|value| value.as_array())
            .unwrap()[0],
        serde_json::json!("source-attribution")
    );
}
```

- [ ] **Step 5: Run the failing adapter test**

Run:

```bash
cd ab-validator
cargo test -p ab-aat-to-parser-ir tei_eaj_alignment_probe_classifies_melos_tail_addition -- --nocapture
```

Expected: compile failure because `tei_eaj_alignment_probe` does not exist.

- [ ] **Step 6: Implement adapter data model**

Create `ab-validator/crates/ab-aat-to-parser-ir/src/tei_eaj_alignment_probe.rs` with public structs:

```rust
use std::{collections::BTreeMap, path::PathBuf};

use ab_diff_utils::{
    AlignmentConfig, AlignmentKind, AlignmentResult, ComparisonEvidence, ComparisonToken,
    align_pair, algorithm_config_hash,
};
use anyhow::{Context, Result};
use serde::Serialize;
use serde_json::Value;

#[derive(Debug, Clone)]
pub struct TeiEajAlignmentProbeConfig {
    pub workset_path: PathBuf,
    pub max_rows: Option<usize>,
}

#[derive(Debug, Clone, Serialize)]
pub struct TeiEajAlignmentProbeReport {
    pub schema_version: String,
    pub rows: Vec<TeiEajAlignmentProbeRow>,
}

#[derive(Debug, Clone, Serialize)]
pub struct TeiEajAlignmentProbeRow {
    pub work_id: Option<String>,
    pub title: Option<String>,
    pub tei_eaj_file: String,
    pub alignment_probe: Option<AlignmentProbe>,
}

#[derive(Debug, Clone, Serialize)]
pub struct AlignmentProbe {
    pub schema_version: String,
    pub evidence_level: ComparisonEvidence,
    pub algorithm_id: String,
    pub algorithm_config_hash: String,
    pub algorithm_config: Value,
    pub tokenization_id: String,
    pub normalization_id: String,
    pub left_witness: String,
    pub right_witness: String,
    pub summary: Value,
    pub diagnosis_event_count: usize,
    pub diagnosis_counts: BTreeMap<String, usize>,
    pub samples: Vec<ProbeSample>,
    pub truncated: bool,
    pub limits: Value,
}

#[derive(Debug, Clone, Serialize)]
pub struct ProbeSample {
    pub kind: String,
    pub diagnosis: String,
    pub left_range: [usize; 2],
    pub right_range: [usize; 2],
    pub left_text: String,
    pub right_text: String,
    pub truncated: bool,
    pub adapter_context: BTreeMap<String, Value>,
}
```

Do not add private workset deserialization structs in this module. The adapter
must import and use `crate::tei_eaj_workset::{read_tei_eaj_workset,
resolve_tei_eaj_path}` so structural and alignment probes consume one workset
contract.

- [ ] **Step 7: Implement TEI token extraction**

Use `roxmltree::Document::parse` to extract TEI body sentence/text-run tokens.
Add:

```rust
use roxmltree::{Document, Node};

#[derive(Debug, Clone)]
struct TeiTokenContext {
    path: String,
    features: Vec<String>,
}

#[derive(Debug, Clone)]
struct TeiToken {
    token: ComparisonToken,
    context: TeiTokenContext,
}

fn extract_body_tokens(xml: &str) -> Result<Vec<TeiToken>> {
    let document = Document::parse(xml).context("failed to parse TEI XML")?;
    let body = document
        .descendants()
        .find(|node| node.is_element() && node.tag_name().name() == "body")
        .context("TEI body not found")?;
    let mut tokens = Vec::new();
    for element in body
        .descendants()
        .filter(|node| node.is_element() && matches!(node.tag_name().name(), "p" | "head"))
    {
        let base_text = base_text_excluding_apparatus(element);
        for (run_idx, run) in split_sentence_like_runs(&base_text).into_iter().enumerate() {
            let normalized = remove_unicode_whitespace(&run);
            if normalized.is_empty() {
                continue;
            }
            let mut features = Vec::new();
            if normalized.starts_with('（') && normalized.contains("シルレルの詩から") {
                features.push("source-attribution".to_owned());
            }
            let ordinal = tokens.len();
            tokens.push(TeiToken {
                token: ComparisonToken {
                    ordinal,
                    text: run,
                    normalized,
                },
                context: TeiTokenContext {
                    path: format!("{}#run{}", element_path(element), run_idx),
                    features,
                },
            });
        }
    }
    Ok(tokens)
}

fn base_text_excluding_apparatus(node: Node<'_, '_>) -> String {
    fn visit(node: Node<'_, '_>, out: &mut String) {
        if node.is_text() {
            out.push_str(node.text().unwrap_or_default());
            return;
        }
        if node.is_element() && skip_base_text_element(node) {
            return;
        }
        for child in node.children() {
            visit(child, out);
        }
    }

    let mut out = String::new();
    visit(node, &mut out);
    out
}

fn skip_base_text_element(node: Node<'_, '_>) -> bool {
    let name = node.tag_name().name();
    if matches!(name, "rt" | "rp" | "note") {
        return true;
    }
    name == "span"
        && (matches!(node.attribute("type"), Some("rt" | "rp"))
            || node.attribute("rend") == Some("notes"))
}

fn remove_unicode_whitespace(value: &str) -> String {
    value.chars().filter(|ch| !ch.is_whitespace()).collect()
}

fn split_sentence_like_runs(text: &str) -> Vec<String> {
    let mut runs = Vec::new();
    let mut current = String::new();
    let mut boundary_pending = false;
    for ch in text.chars() {
        if boundary_pending && !matches!(ch, '」' | '』' | '）' | '】' | '〉' | '》' | ')' | ']') {
            push_nonblank(&mut runs, &mut current);
            boundary_pending = false;
        }
        current.push(ch);
        if matches!(ch, '。' | '？' | '！' | '?' | '!') {
            boundary_pending = true;
        }
    }
    push_nonblank(&mut runs, &mut current);
    runs
}

fn push_nonblank(runs: &mut Vec<String>, current: &mut String) {
    if !remove_unicode_whitespace(current).is_empty() {
        runs.push(std::mem::take(current));
    } else {
        current.clear();
    }
}

fn element_path(node: Node<'_, '_>) -> String {
    let mut parts = Vec::new();
    for ancestor in node.ancestors().filter(|candidate| candidate.is_element()) {
        let name = ancestor.tag_name().name();
        let index = ancestor
            .prev_siblings()
            .filter(|sibling| sibling.is_element() && sibling.tag_name().name() == name)
            .count()
            + 1;
        parts.push(format!("{name}[{index}]"));
    }
    parts.reverse();
    format!("/{}", parts.join("/"))
}
```

The adapter tokenization id for this extractor is
`tei-body-sentence-like-text-run-v1`. It is intentionally not
`tei-body-structural-text-v1`, because paragraph structure is not the token
unit.

- [ ] **Step 8: Implement probe construction**

Implement:

```rust
pub fn run_tei_eaj_alignment_probe(
    config: TeiEajAlignmentProbeConfig,
) -> Result<TeiEajAlignmentProbeReport> {
    let workset = crate::tei_eaj_workset::read_tei_eaj_workset(&config.workset_path)?;
    let mut rows = Vec::new();
    for file in &workset.files {
        if config.max_rows.is_some_and(|limit| rows.len() >= limit) {
            break;
        }
        if file.comparison_status != "compared" || file.base_text_equal == Some(true) {
            continue;
        }
        let Some(abc_tei) = file.abc_tei.as_deref() else {
            continue;
        };
        let left_xml = std::fs::read_to_string(&abc_tei)
            .with_context(|| format!("failed to read ABC TEI {}", abc_tei))?;
        let right_path = crate::tei_eaj_workset::resolve_tei_eaj_path(
            &workset,
            &file.tei_eaj_file,
        );
        let right_xml = std::fs::read_to_string(&right_path)
            .with_context(|| format!("failed to read TEI-EAJ TEI {}", right_path.display()))?;
        let left = extract_body_tokens(&left_xml)?;
        let right = extract_body_tokens(&right_xml)?;
        let probe = build_probe(&left, &right)?;
        assert_probe_invariants(&probe)?;
        rows.push(TeiEajAlignmentProbeRow {
            work_id: file.work_id.clone(),
            title: file.title.clone(),
            tei_eaj_file: file.tei_eaj_file.clone(),
            alignment_probe: Some(probe),
        });
    }
    Ok(TeiEajAlignmentProbeReport {
        schema_version: "tei-eaj-alignment-probe-report-v1".to_owned(),
        rows,
    })
}
```

`build_probe` must call `align_pair`, map kernel regions to samples, and
classify a final insertion whose left context has `"source-attribution"` as
`tail_addition`. It must emit `tokenization_id:
"tei-body-sentence-like-text-run-v1"`.

Add:

```rust
fn assert_probe_invariants(probe: &AlignmentProbe) -> Result<()> {
    let diagnosis_sum: usize = probe.diagnosis_counts.values().sum();
    if diagnosis_sum != probe.diagnosis_event_count {
        anyhow::bail!(
            "alignment probe diagnosis_event_count {} does not match diagnosis_counts sum {}",
            probe.diagnosis_event_count,
            diagnosis_sum
        );
    }
    let recomputed_hash = ab_diff_utils::hash_json_canonical(&probe.algorithm_config)
        .context("failed to hash emitted algorithm_config")?;
    if recomputed_hash != probe.algorithm_config_hash {
        anyhow::bail!(
            "alignment probe algorithm_config_hash {} does not match emitted algorithm_config hash {}",
            probe.algorithm_config_hash,
            recomputed_hash
        );
    }
    Ok(())
}
```

- [ ] **Step 9: Run the adapter test to green**

Run:

```bash
cd ab-validator
cargo test -p ab-aat-to-parser-ir tei_eaj_alignment_probe_classifies_melos_tail_addition -- --nocapture
```

Expected: test passes.

- [ ] **Step 10: Commit Task 4**

```bash
git add ab-validator/Cargo.toml \
        ab-validator/crates/ab-aat-to-parser-ir/Cargo.toml \
        ab-validator/crates/ab-aat-to-parser-ir/src/lib.rs \
        ab-validator/crates/ab-aat-to-parser-ir/src/tei_eaj_workset.rs \
        ab-validator/crates/ab-aat-to-parser-ir/src/structural_probe.rs \
        ab-validator/crates/ab-aat-to-parser-ir/src/tei_eaj_alignment_probe.rs \
        ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs
git commit -m "feat(aat): add TEI-EAJ alignment probe adapter"
```

---

### Task 5: CLI and Melos Characterization Report

**Files:**
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/main.rs`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs`
- Create: `ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/tei-eaj-alignment-probe/abc-melos-single-p.xml`
- Create: `ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/tei-eaj-alignment-probe/tei-eaj-melos-split-p.xml`
- Create: `abc/docs/handoffs/tei-eaj-alignment-probe-fixture-workset.json`
- Create: `abc/docs/handoffs/tei-eaj-alignment-probe-melos.json`
- Create: `abc/docs/handoffs/tei-eaj-alignment-probe-melos.md`

**Interfaces:**
- Consumes: Task 4 `run_tei_eaj_alignment_probe`.
- Produces: `ab-aat-to-parser-ir tei-eaj-alignment-probe --workset ... --summary-json ... --report-md ...`

- [ ] **Step 1: Add failing CLI test**

In `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs`, add:

```rust
#[test]
fn cli_tei_eaj_alignment_probe_writes_reports() {
    let temp = tempfile::tempdir().unwrap();
    let workset = temp.path().join("workset.json");
    let summary = temp.path().join("alignment-summary.json");
    let report = temp.path().join("alignment-report.md");
    std::fs::write(&workset, minimal_melos_alignment_workset(temp.path())).unwrap();

    let status = std::process::Command::new(env!("CARGO_BIN_EXE_ab-aat-to-parser-ir"))
        .arg("tei-eaj-alignment-probe")
        .arg("--workset")
        .arg(&workset)
        .arg("--summary-json")
        .arg(&summary)
        .arg("--report-md")
        .arg(&report)
        .status()
        .unwrap();

    assert!(status.success());
    let summary_json: serde_json::Value =
        ab_aat_to_parser_ir::schema::read_json(&summary).unwrap();
    assert_eq!(
        summary_json.pointer("/rows/0/alignment_probe/diagnosis_counts/tail_addition"),
        Some(&serde_json::json!(1))
    );
    let report_text = std::fs::read_to_string(report).unwrap();
    assert!(report_text.contains("TEI-EAJ Alignment Probe"));
    assert!(report_text.contains("tail_addition"));
}
```

Define `minimal_melos_alignment_workset` next to the existing integration test helpers:

```rust
fn minimal_melos_alignment_workset(root: &std::path::Path) -> String {
    let abc_tei = root.join("abc-melos.xml");
    let tei_eaj_root = root.join("tei-eaj");
    let tei_eaj_file = tei_eaj_root.join("data/complete/tei_lib_lv4/1567_tei.xml");
    std::fs::create_dir_all(tei_eaj_file.parent().unwrap()).unwrap();
    std::fs::write(
        &abc_tei,
        r#"<TEI xmlns="http://www.tei-c.org/ns/1.0"><text><body><p>メロスは激怒した。勇者は、ひどく<ruby><rb>赤面した</rb><rt>せきめんした</rt></ruby>。<note>底本注</note>（古伝説と、シルレルの詩から。）</p></body></text></TEI>"#,
    )
    .unwrap();
    std::fs::write(
        &tei_eaj_file,
        r#"<TEI xmlns="http://www.tei-c.org/ns/1.0"><text><body><p>メロスは激怒した。</p><p>勇者は、ひどく赤面した。</p></body></text></TEI>"#,
    )
    .unwrap();
    format!(
        r#"{{
  "schema_version": "tei-eaj-aozora-workset-export-v1",
  "tei_eaj_source": {{"revision": "fixture", "root": "{}"}},
  "abc_inputs": {{"counterparts": [{{"path": "{}", "work_id": "1567"}}], "tei_dirs": [], "tei_specs": []}},
  "summary": {{
    "tei_eaj_file_count": 1,
    "tei_eaj_work_id_count": 1,
    "abc_counterpart_count": 1,
    "compared_file_count": 1,
    "missing_counterpart_count": 0,
    "no_work_id_count": 0,
    "uncompared_file_count": 0,
    "base_text_equal_count": 0,
    "base_text_mismatch_count": 1,
    "base_text_relation_counts": {{"tei_eaj_subset_of_abc": 1}}
  }},
  "candidate_work_ids": ["1567"],
  "missing_abc_counterpart_work_ids": [],
  "no_work_id_files": [],
  "files": [{{
    "work_id": "1567",
    "work_id_method": "filename_work_id",
    "tei_eaj_file": "data/complete/tei_lib_lv4/1567_tei.xml",
    "state": "complete",
    "level": "Level 4",
    "title": "走れメロス",
    "abc_tei": "{}",
    "comparison_status": "compared",
    "base_text_equal": false,
    "base_text_relation": "tei_eaj_subset_of_abc",
    "base_text_length_delta": 16,
    "abc_body_base_text_length": 40,
    "tei_eaj_body_base_text_length": 24,
    "abc_p_count": 1,
    "tei_eaj_p_count": 2,
    "abc_note_count": 1,
    "tei_eaj_note_count": 0,
    "first_difference": {{
      "index": 24,
      "abc": "メロスは激怒した。勇者は、ひどく赤面した。（古伝説と、シルレルの詩から。）",
      "tei_eaj": "メロスは激怒した。勇者は、ひどく赤面した。"
    }}
  }}]
}}"#,
        tei_eaj_root.display(),
        abc_tei.display(),
        abc_tei.display()
    )
}
```

- [ ] **Step 2: Run the failing CLI test**

Run:

```bash
cd ab-validator
cargo test -p ab-aat-to-parser-ir cli_tei_eaj_alignment_probe_writes_reports -- --nocapture
```

Expected: test fails because the CLI subcommand does not exist.

- [ ] **Step 3: Add CLI subcommand**

In `ab-validator/crates/ab-aat-to-parser-ir/src/main.rs`, import:

```rust
use ab_aat_to_parser_ir::tei_eaj_alignment_probe::{
    TeiEajAlignmentProbeConfig, run_tei_eaj_alignment_probe,
    write_tei_eaj_alignment_probe_reports,
};
```

Add enum variant:

```rust
TeiEajAlignmentProbe {
    #[arg(long)]
    workset: PathBuf,
    #[arg(long)]
    summary_json: PathBuf,
    #[arg(long)]
    report_md: PathBuf,
    #[arg(long)]
    max_rows: Option<usize>,
},
```

Add match arm:

```rust
Command::TeiEajAlignmentProbe {
    workset,
    summary_json,
    report_md,
    max_rows,
} => {
    let report = run_tei_eaj_alignment_probe(TeiEajAlignmentProbeConfig {
        workset_path: workset,
        max_rows,
    })?;
    write_tei_eaj_alignment_probe_reports(&report, &summary_json, &report_md)?;
    eprintln!("wrote {} TEI-EAJ alignment probe row(s)", report.rows.len());
}
```

- [ ] **Step 4: Implement report writers**

In `tei_eaj_alignment_probe.rs`, add:

```rust
pub fn write_tei_eaj_alignment_probe_reports(
    report: &TeiEajAlignmentProbeReport,
    summary_json: &std::path::Path,
    report_md: &std::path::Path,
) -> Result<()> {
    std::fs::write(summary_json, serde_json::to_string_pretty(report)? + "\n")?;
    std::fs::write(report_md, render_tei_eaj_alignment_probe_markdown(report))?;
    Ok(())
}

pub fn render_tei_eaj_alignment_probe_markdown(report: &TeiEajAlignmentProbeReport) -> String {
    let mut out = String::from("# TEI-EAJ Alignment Probe\n\n");
    out.push_str("| work_id | title | tei_eaj_file | diagnosis_counts |\n");
    out.push_str("| --- | --- | --- | --- |\n");
    for row in &report.rows {
        let counts = row
            .alignment_probe
            .as_ref()
            .map(|probe| serde_json::to_string(&probe.diagnosis_counts).unwrap())
            .unwrap_or_else(|| "{}".to_owned());
        out.push_str(&format!(
            "| {} | {} | `{}` | `{}` |\n",
            row.work_id.as_deref().unwrap_or(""),
            row.title.as_deref().unwrap_or(""),
            row.tei_eaj_file,
            counts
        ));
    }
    out
}
```

- [ ] **Step 5: Run CLI test to green**

Run:

```bash
cd ab-validator
cargo test -p ab-aat-to-parser-ir cli_tei_eaj_alignment_probe_writes_reports -- --nocapture
```

Expected: test passes and reports contain `tail_addition`.

- [ ] **Step 6: Add self-contained Melos-shaped evidence fixtures**

Create `ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/tei-eaj-alignment-probe/abc-melos-single-p.xml`:

```xml
<TEI xmlns="http://www.tei-c.org/ns/1.0">
  <text>
    <body>
      <p>メロスは激怒した。勇者は、ひどく<ruby><rb>赤面した</rb><rt>せきめんした</rt></ruby>。<note>底本注</note>（古伝説と、シルレルの詩から。）</p>
    </body>
  </text>
</TEI>
```

Create `ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/tei-eaj-alignment-probe/tei-eaj-melos-split-p.xml`:

```xml
<TEI xmlns="http://www.tei-c.org/ns/1.0">
  <text>
    <body>
      <p>メロスは激怒した。</p>
      <p>勇者は、ひどく赤面した。</p>
    </body>
  </text>
</TEI>
```

Create `abc/docs/handoffs/tei-eaj-alignment-probe-fixture-workset.json`:

```json
{
  "schema_version": "tei-eaj-aozora-workset-export-v1",
  "tei_eaj_source": {
    "revision": "fixture",
    "root": "ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/tei-eaj-alignment-probe"
  },
  "abc_inputs": {
    "counterparts": [
      {
        "path": "ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/tei-eaj-alignment-probe/abc-melos-single-p.xml",
        "work_id": "1567"
      }
    ],
    "tei_dirs": [],
    "tei_specs": []
  },
  "summary": {
    "tei_eaj_file_count": 1,
    "tei_eaj_work_id_count": 1,
    "abc_counterpart_count": 1,
    "compared_file_count": 1,
    "missing_counterpart_count": 0,
    "no_work_id_count": 0,
    "uncompared_file_count": 0,
    "base_text_equal_count": 0,
    "base_text_mismatch_count": 1,
    "base_text_relation_counts": {
      "tei_eaj_subset_of_abc": 1
    }
  },
  "candidate_work_ids": ["1567"],
  "missing_abc_counterpart_work_ids": [],
  "no_work_id_files": [],
  "files": [
    {
      "work_id": "1567",
      "work_id_method": "filename_work_id",
      "tei_eaj_file": "tei-eaj-melos-split-p.xml",
      "state": "complete",
      "level": "Level 4",
      "title": "走れメロス",
      "abc_tei": "ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/tei-eaj-alignment-probe/abc-melos-single-p.xml",
      "comparison_status": "compared",
      "base_text_equal": false,
      "base_text_relation": "tei_eaj_subset_of_abc",
      "base_text_length_delta": 16,
      "abc_body_base_text_length": 40,
      "tei_eaj_body_base_text_length": 24,
      "abc_p_count": 1,
      "tei_eaj_p_count": 2,
      "abc_note_count": 1,
      "tei_eaj_note_count": 0,
      "first_difference": {
        "index": 24,
        "abc": "メロスは激怒した。勇者は、ひどく赤面した。（古伝説と、シルレルの詩から。）",
        "tei_eaj": "メロスは激怒した。勇者は、ひどく赤面した。"
      }
    }
  ]
}
```

- [ ] **Step 7: Generate checked-in Melos-shaped characterization report**

Run from repository root:

```bash
nix run .#ab-validator-ab-aat-to-parser-ir -- tei-eaj-alignment-probe \
  --workset abc/docs/handoffs/tei-eaj-alignment-probe-fixture-workset.json \
  --summary-json abc/docs/handoffs/tei-eaj-alignment-probe-melos.json \
  --report-md abc/docs/handoffs/tei-eaj-alignment-probe-melos.md
```

Expected:

- JSON report has one row for the self-contained Melos-shaped fixture.
- The row has `alignment_probe.diagnosis_counts.tail_addition = 1`.
- The row has `alignment_probe.tokenization_id = "tei-body-sentence-like-text-run-v1"`.
- Markdown report contains `tail_addition` and `tei-eaj-melos-split-p.xml`.

Do not run this checked-in report step against
`abc/docs/handoffs/tei-eaj-aozora-workset-export.json`: that handoff currently
references `paper/demo-melos-real/tei.xml`, which is not checked into the repo.
A local diagnostic run against the real workset is allowed only after the ABC
TEI path exists.

- [ ] **Step 8: Commit Task 5**

```bash
git add ab-validator/crates/ab-aat-to-parser-ir/src/main.rs \
        ab-validator/crates/ab-aat-to-parser-ir/src/tei_eaj_alignment_probe.rs \
        ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs \
        ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/tei-eaj-alignment-probe/abc-melos-single-p.xml \
        ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/tei-eaj-alignment-probe/tei-eaj-melos-split-p.xml \
        abc/docs/handoffs/tei-eaj-alignment-probe-fixture-workset.json \
        abc/docs/handoffs/tei-eaj-alignment-probe-melos.json \
        abc/docs/handoffs/tei-eaj-alignment-probe-melos.md
git commit -m "feat(aat): expose TEI-EAJ alignment probe CLI"
```

---

### Task 6: Verification and Merge

**Files:**
- No planned source edits unless verification reveals a real failure.

**Interfaces:**
- Consumes: all previous tasks.
- Produces: merged and pushed `main` with the alignment probe slice.

- [ ] **Step 1: Run focused Rust tests**

```bash
cd ab-validator
cargo test -p ab-diff-utils hashing::tests -- --nocapture
cargo test -p ab-diff-utils align -- --nocapture
cargo test -p ab-aat-to-parser-ir tei_eaj_alignment_probe -- --nocapture
```

Expected: all targeted Rust tests pass.

- [ ] **Step 2: Run focused ABC schema checks**

```bash
nix build .#checks.x86_64-linux.abc-tei-eaj-comparison-tests --print-build-logs
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: both checks pass.

- [ ] **Step 3: Run format checks for touched languages**

```bash
cd ab-validator
cargo fmt --check
cd ..
nix run .#python-quality
```

Expected: Rust formatting is clean, Python quality remains clean because this slice did not touch Python.

- [ ] **Step 4: Run full release gate when implementation is complete**

```bash
nix flake check --print-build-logs
```

Expected: `all checks passed!`

- [ ] **Step 5: Review changed files**

```bash
git status --short
git log --oneline origin/main..HEAD
```

Expected:

- only intentional alignment-probe files are modified;
- unrelated local paths such as `dictionary/` or adapter `Cargo.lock` changes are not included unless the user explicitly asks.

- [ ] **Step 6: Merge and push**

```bash
git checkout main
git merge --ff-only <alignment-probe-implementation-branch>
git push origin main
```

Expected: `main` advances to the implementation branch and `origin/main` receives the slice.

---

## Self-Review

**Spec coverage:**
- Generic kernel: Tasks 1 and 2.
- No `segmentation` generic kind: Task 1 region enum and Task 3 invalid fixture.
- Evidence-level vocabulary: Task 1 `ComparisonEvidence` and Task 3 schema.
- Standalone `$ref`-able schema: Task 3.
- Config hash and config object: Task 1 canonical hash function, Task 3 fixture rehash check, Task 4 producer invariant.
- Diagnosis count invariant: Task 3 fixture invariant check and Task 4 Rust producer invariant; JSON Schema intentionally does not enforce this arithmetic relation.
- Rust-only first consumption surface: Tasks 4 and 5; Python integration is not touched.
- Melos-shaped baseline characterization: Tasks 4 and 5 use a self-contained fixture that reproduces the known paragraph asymmetry without depending on the stale handoff path.
- `ab-morph-diff` preservation: global constraint and no task touches `ab-morph-diff`.

**Known follow-ups after this plan:**
- Wire Rust-generated probes into the regular Python TEI-EAJ report path.
- Run against `abc/docs/handoffs/tei-eaj-aozora-workset-export.json` after `paper/demo-melos-real/tei.xml` or its successor is generated and checked in.
- Add `ab-compare` normalized-visible adapter for schema/version diffs.
- Add optional near-match scoring with `algorithm_id` or `algorithm_config_hash` rotation.
- Add move group ids if exact move examples need pairing in reports.
