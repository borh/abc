# Streaming Compact Morph Comparisons Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Let compact morph corpus runs compute summary rows and bounded examples without materializing a full `Comparison { regions, feature_diffs }` for every analyzer pair.

**Architecture:** Full output stays on `compare_pair`. Compact output uses a streaming accumulator built on shared `ab-morph-diff` primitives: `align.rs` owns region traversal and `stats.rs` owns boundary metrics. This avoids duplicate alignment/stat logic, preserves current compact example ordering, and keeps the expected memory win scoped to per-document comparison peaks rather than dictionary residency.

**Tech Stack:** Rust workspace crates `ab-morph-diff` and `ab-morph-run`, serde JSONL/zstd output, existing morph model types, existing compact row schema.

---

## File Structure

- Modify `crates/ab-morph-diff/src/align.rs`: expose `visit_regions_with_source_len` and implement `align_regions_with_source_len` by collecting visitor output.
- Modify `crates/ab-morph-diff/src/stats.rs`: expose `BoundaryMetrics` and `derive_boundary_metrics` for reuse.
- Modify `crates/ab-morph-diff/src/model.rs`: add compact comparison/result model types.
- Create `crates/ab-morph-diff/src/streaming.rs`: implement compact accumulator over the shared visitor; no copied alignment or boundary code.
- Modify `crates/ab-morph-diff/src/lib.rs`: export compact model types and compact comparison API.
- Modify `crates/ab-morph-run/src/compact.rs`: convert compact comparison results into existing compact JSONL row types.
- Modify `crates/ab-morph-run/src/lib.rs`: route `OutputProfile::Compact` through the streaming compact API.
- Modify `docs/superpowers/reports/2026-04-29-morph-run-memory.md`: document behavior and expected memory impact.

---

### Task 1: Refactor alignment into a shared visitor

**Files:**
- Modify: `crates/ab-morph-diff/src/align.rs`

- [ ] **Step 1: Add failing visitor equivalence test**

Add this test to the existing `#[cfg(test)] mod tests` in `align.rs`:

```rust
#[test]
fn visit_regions_matches_align_regions_order_and_shape() {
    let source = "今日明日";
    let from = a("from", source, vec![m(source, "今日", 0, 2), m(source, "明日", 2, 4)]);
    let to = a(
        "to",
        source,
        vec![m(source, "今", 0, 1), m(source, "日", 1, 2), m(source, "明日", 2, 4)],
    );

    let collected = align_regions(&from, &to).unwrap();
    let mut visited = Vec::new();
    visit_regions_with_source_len(&from, &to, source.chars().count(), |index, region| {
        assert_eq!(index, visited.len());
        visited.push(region);
    })
    .unwrap();

    assert_eq!(visited, collected);
}
```

- [ ] **Step 2: Run test to verify red**

Run: `cargo test -p ab-morph-diff visit_regions_matches_align_regions_order_and_shape`

Expected: FAIL to compile because `visit_regions_with_source_len` does not exist.

- [ ] **Step 3: Extract visitor**

Replace `align_regions_with_source_len` with a collector and move the current loop body unchanged into a new visitor:

```rust
pub(crate) fn align_regions_with_source_len(
    from: &Analysis,
    to: &Analysis,
    source_len: usize,
) -> Result<Vec<Region>, MorphDiffError> {
    let mut regions = Vec::new();
    visit_regions_with_source_len(from, to, source_len, |_index, region| {
        regions.push(region);
    })?;
    Ok(regions)
}

pub(crate) fn visit_regions_with_source_len(
    from: &Analysis,
    to: &Analysis,
    source_len: usize,
    mut visit: impl FnMut(usize, Region),
) -> Result<(), MorphDiffError> {
    // Move the existing align_regions_with_source_len loop here unchanged.
    // Replace every `regions.push(region)` with:
    // visit(region_index, region);
    // region_index += 1;
    Ok(())
}
```

Implementation rule: do not copy this traversal into `streaming.rs`; future alignment fixes must have one source of truth.

- [ ] **Step 4: Run alignment tests**

Run: `cargo test -p ab-morph-diff align::tests`

Expected: all alignment tests pass.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-diff/src/align.rs
git commit -m "refactor: expose morph region visitor"
```

---

### Task 2: Extract shared boundary metric helpers

**Files:**
- Modify: `crates/ab-morph-diff/src/stats.rs`

- [ ] **Step 1: Add failing boundary helper test**

Add this test to `stats.rs`:

```rust
#[test]
fn derive_boundary_metrics_matches_stats_fields() {
    let source = "今日明日";
    let from = a("from", source, vec![m(source, "今日", 0, 2), m(source, "明日", 2, 4)]);
    let to = a("to", source, vec![m(source, "今", 0, 1), m(source, "日", 1, 2), m(source, "明日", 2, 4)]);
    let regions = vec![Region::Segmentation(SegmentationDiff {
        text_span: 0..2,
        from_indices: 0..1,
        to_indices: 0..2,
        from_surfaces: vec!["今日".to_owned()],
        to_surfaces: vec!["今".to_owned(), "日".to_owned()],
        kind: SegmentationKind::Split,
    })];

    let stats = derive_stats_with_source_len(&from, &to, &regions, &[], source.chars().count());
    let metrics = derive_boundary_metrics(&from, &to, &[], source.chars().count());

    assert_eq!(metrics.precision, stats.boundary_precision);
    assert_eq!(metrics.recall, stats.boundary_recall);
    assert_eq!(metrics.f1, stats.boundary_f1);
}
```

- [ ] **Step 2: Run test to verify red**

Run: `cargo test -p ab-morph-diff derive_boundary_metrics_matches_stats_fields`

Expected: FAIL to compile because `derive_boundary_metrics` does not exist.

- [ ] **Step 3: Extract helper**

Add above `derive_stats`:

```rust
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct BoundaryMetrics {
    pub(crate) precision: Option<f64>,
    pub(crate) recall: Option<f64>,
    pub(crate) f1: Option<f64>,
}

pub(crate) fn derive_boundary_metrics(
    from: &Analysis,
    to: &Analysis,
    ignored_spans: &[std::ops::Range<usize>],
    source_len: usize,
) -> BoundaryMetrics {
    let from_boundaries = comparable_boundaries(from, ignored_spans, source_len);
    let to_boundaries = comparable_boundaries(to, ignored_spans, source_len);
    let shared = from_boundaries.intersection(&to_boundaries).count();
    let precision = ratio(shared, to_boundaries.len());
    let recall = ratio(shared, from_boundaries.len());
    let f1 = match (precision, recall) {
        (Some(precision), Some(recall)) if precision + recall > 0.0 => Some(2.0 * precision * recall / (precision + recall)),
        _ => None,
    };
    BoundaryMetrics { precision, recall, f1 }
}
```

Then update `derive_stats_with_source_len` to call this helper and populate `boundary_precision`, `boundary_recall`, and `boundary_f1` from it.

- [ ] **Step 4: Run stats tests**

Run: `cargo test -p ab-morph-diff stats::tests`

Expected: all stats tests pass.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-diff/src/stats.rs
git commit -m "refactor: share morph boundary metrics"
```

---

### Task 3: Add compact streaming result model types

**Files:**
- Modify: `crates/ab-morph-diff/src/model.rs`
- Modify: `crates/ab-morph-diff/src/lib.rs`

- [ ] **Step 1: Add compact model types**

Add after `Comparison` in `model.rs`:

```rust
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct CompactComparison {
    pub from_analyzer: AnalyzerId,
    pub to_analyzer: AnalyzerId,
    pub text_id: TextId,
    pub stats: ComparisonStats,
    pub examples: Vec<CompactComparisonExample>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CompactComparisonExample {
    pub region_index: usize,
    pub kind: CompactExampleKind,
    pub text_span: Range<usize>,
    pub from_indices: Range<usize>,
    pub to_indices: Range<usize>,
    pub from_surfaces: Vec<String>,
    pub to_surfaces: Vec<String>,
    pub feature_changes: Option<Vec<CompactFeatureChange>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum CompactExampleKind {
    Split,
    Merge,
    Resegment,
    CoverageMissingFrom,
    CoverageMissingTo,
    CoverageUnequal,
    CoverageInvalidInput,
    FeatureDiff,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CompactFeatureChange {
    pub key: FeatureKey,
    pub from: Option<String>,
    pub to: Option<String>,
}
```

- [ ] **Step 2: Export the types**

In `lib.rs`, add to the existing `pub use model::{ ... }` list:

```rust
CompactComparison, CompactComparisonExample, CompactExampleKind, CompactFeatureChange,
```

- [ ] **Step 3: Run compile check**

Run: `cargo check -p ab-morph-diff`

Expected: compile succeeds.

- [ ] **Step 4: Commit**

```bash
git add crates/ab-morph-diff/src/model.rs crates/ab-morph-diff/src/lib.rs
git commit -m "feat: add compact morph comparison model"
```

---

### Task 4: Implement compact streaming accumulator

**Files:**
- Create: `crates/ab-morph-diff/src/streaming.rs`
- Modify: `crates/ab-morph-diff/src/lib.rs`

- [ ] **Step 1: Add tests and red stub**

Create `streaming.rs` with a real stub so the red failure is a panic, not a missing symbol:

```rust
use std::collections::BTreeSet;
use std::ops::Range;

use crate::{
    Analysis, CompactComparison, CompactComparisonExample, CompactExampleKind,
    CompactFeatureChange, CoverageMismatchKind, FeatureMap, MorphDiffError, Region,
    SegmentationKind,
};

pub(crate) fn compare_pair_compact_with_source_text(
    _from: &Analysis,
    _to: &Analysis,
    _source_text: &str,
    _feature_context_keys: &[crate::FeatureKey],
    _max_examples: usize,
) -> Result<CompactComparison, MorphDiffError> {
    unimplemented!("streaming compact comparison")
}
```

Add three tests in the same file with concrete fixtures:

```rust
#[test]
fn compact_streaming_stats_match_full_comparison() {
    let source = "今日は晴れ";
    let from = analysis("from", vec![
        m(source, "今日", 0, 2, features(&[])),
        m(source, "は", 2, 3, features(&[("pos", Some("助詞"))])),
        m(source, "晴れ", 3, 5, features(&[])),
    ]);
    let to = analysis("to", vec![
        m(source, "今", 0, 1, features(&[])),
        m(source, "日", 1, 2, features(&[])),
        m(source, "は", 2, 3, features(&[("pos", Some("名詞"))])),
        m(source, "晴れ", 3, 5, features(&[])),
    ]);

    let full = crate::compare_pair_with_source_text(&from, &to, source, &[]).unwrap();
    let compact = crate::compare_pair_compact_with_source_text(&from, &to, source, &[], 10).unwrap();

    assert_eq!(compact.stats, full.stats);
}

#[test]
fn compact_streaming_preserves_structural_example_precedence() {
    let source = "今日明日";
    let from = analysis("from", vec![
        m(source, "今日", 0, 2, features(&[("pos", Some("名詞"))])),
        m(source, "明日", 2, 4, features(&[])),
    ]);
    let to = analysis("to", vec![
        m(source, "今日", 0, 2, features(&[("pos", Some("副詞"))])),
        m(source, "明", 2, 3, features(&[])),
        m(source, "日", 3, 4, features(&[])),
    ]);

    let compact = crate::compare_pair_compact_with_source_text(&from, &to, source, &[], 1).unwrap();

    assert_eq!(compact.examples.len(), 1);
    assert_eq!(compact.examples[0].kind, CompactExampleKind::Split);
}

#[test]
fn compact_streaming_feature_examples_include_changed_payload() {
    let source = "今日";
    let from = analysis("from", vec![m(source, "今日", 0, 2, features(&[("pos", Some("名詞"))]))]);
    let to = analysis("to", vec![m(source, "今日", 0, 2, features(&[("pos", Some("副詞"))]))]);

    let compact = crate::compare_pair_compact_with_source_text(&from, &to, source, &[], 10).unwrap();

    assert_eq!(compact.examples.len(), 1);
    assert_eq!(compact.examples[0].kind, CompactExampleKind::FeatureDiff);
    assert_eq!(compact.examples[0].feature_changes.as_ref().unwrap()[0].key, "pos");
}
```

Define local `features`, `m`, and `analysis` helpers in the test module using the same patterns as `ab-morph-diff/src/lib.rs` tests. Do not use mocks.

- [ ] **Step 2: Register module and API**

In `lib.rs`, add `mod streaming;` and:

```rust
pub fn compare_pair_compact_with_source_text(
    from: &Analysis,
    to: &Analysis,
    source_text: &str,
    feature_context_keys: &[FeatureKey],
    max_examples: usize,
) -> Result<CompactComparison, MorphDiffError> {
    streaming::compare_pair_compact_with_source_text(from, to, source_text, feature_context_keys, max_examples)
}
```

- [ ] **Step 3: Run tests to verify red**

Run: `cargo test -p ab-morph-diff compact_streaming`

Expected: tests compile and fail with panic containing `streaming compact comparison`.

- [ ] **Step 4: Implement accumulator**

Implement `compare_pair_compact_with_source_text` with these rules:

- Validate `text_id` equality before validation, matching `compare_pair`.
- Validate both analyses with `validate_analysis_against_source`.
- Ignore `feature_context_keys` in compact output for now by naming the argument `_feature_context_keys`; compact feature examples have no `same_context` field.
- Call `crate::align::visit_regions_with_source_len`; do not duplicate `next_start`, `next_end`, `covers_exactly`, or `segmentation_kind`.
- Track stats counters in `CompactAccumulator`.
- Store structural examples in `region_examples` and feature examples in `feature_examples`.
- Limit `region_examples` to `max_examples`.
- Limit `feature_examples` to `max_examples.saturating_sub(region_examples.len())` at insertion time.
- At finish, concatenate structural examples first, then feature examples.
- Compute boundary metrics with `crate::stats::derive_boundary_metrics`.

The accumulator shape should be:

```rust
struct CompactAccumulator<'a> {
    from: &'a Analysis,
    to: &'a Analysis,
    max_examples: usize,
    region_examples: Vec<CompactComparisonExample>,
    feature_examples: Vec<CompactComparisonExample>,
    one_to_one_regions: usize,
    one_to_one_with_feature_diff_regions: BTreeSet<usize>,
    segmentation_regions: usize,
    coverage_mismatch_regions: usize,
    split_regions: usize,
    merge_regions: usize,
    resegment_regions: usize,
    from_morphemes_in_segmentation: usize,
    to_morphemes_in_segmentation: usize,
    ignored_spans: Vec<Range<usize>>,
}
```

`changed_features(from: &FeatureMap, to: &FeatureMap)` must return sorted changes by collecting keys in a `BTreeSet`, matching existing deterministic output order.

- [ ] **Step 5: Run streaming tests**

Run: `cargo test -p ab-morph-diff compact_streaming`

Expected: all streaming tests pass.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-morph-diff/src/lib.rs crates/ab-morph-diff/src/streaming.rs
git commit -m "feat: stream compact morph comparisons"
```

---

### Task 5: Wire compact streaming into `ab-morph-run`

**Files:**
- Modify: `crates/ab-morph-run/src/compact.rs`
- Modify: `crates/ab-morph-run/src/lib.rs`

- [ ] **Step 1: Add compact conversion helpers**

In `compact.rs`, add conversions:

```rust
impl ComparisonSummaryRow {
    pub(crate) fn from_compact_comparison(source_id: String, comparison: &CompactComparison) -> Self {
        let stats = &comparison.stats;
        Self {
            source_id,
            text_id: comparison.text_id.clone(),
            from_analyzer: comparison.from_analyzer.clone(),
            to_analyzer: comparison.to_analyzer.clone(),
            from_morphemes: stats.from_morphemes,
            to_morphemes: stats.to_morphemes,
            one_to_one_regions: stats.one_to_one_regions,
            one_to_one_with_feature_differences: stats.one_to_one_with_feature_differences,
            segmentation_regions: stats.segmentation_regions,
            coverage_mismatch_regions: stats.coverage_mismatch_regions,
            split_regions: stats.split_regions,
            merge_regions: stats.merge_regions,
            resegment_regions: stats.resegment_regions,
            from_morphemes_in_segmentation: stats.from_morphemes_in_segmentation,
            to_morphemes_in_segmentation: stats.to_morphemes_in_segmentation,
            boundary_precision: stats.boundary_precision,
            boundary_recall: stats.boundary_recall,
            boundary_f1: stats.boundary_f1,
        }
    }
}

pub(crate) fn example_rows_from_compact_comparison(
    source_id: String,
    source_text: &str,
    comparison: &CompactComparison,
) -> Vec<ComparisonExampleRow> {
    comparison
        .examples
        .iter()
        .map(|example| example_row_from_compact_example(&source_id, source_text, comparison, example))
        .collect()
}

fn example_row_from_compact_example(
    source_id: &str,
    source_text: &str,
    comparison: &CompactComparison,
    example: &DiffCompactExample,
) -> ComparisonExampleRow {
    let byte_span = byte_span_from_char_span(source_text, &example.text_span);
    ComparisonExampleRow {
        source_id: source_id.to_owned(),
        text_id: comparison.text_id.clone(),
        from_analyzer: comparison.from_analyzer.clone(),
        to_analyzer: comparison.to_analyzer.clone(),
        region_index: example.region_index,
        kind: compact_kind(example.kind).to_owned(),
        byte_start: byte_span.start,
        byte_end: byte_span.end,
        char_start: example.text_span.start,
        char_end: example.text_span.end,
        source_excerpt: excerpt(source_text, &example.text_span),
        from_surfaces: example.from_surfaces.clone(),
        to_surfaces: example.to_surfaces.clone(),
        feature_changes: example.feature_changes.as_ref().map(|changes| {
            changes
                .iter()
                .map(|change| FeatureChangeRow {
                    key: change.key.clone(),
                    from: change.from.clone(),
                    to: change.to.clone(),
                })
                .collect()
        }),
    }
}

fn compact_kind(kind: CompactExampleKind) -> &'static str {
    match kind {
        CompactExampleKind::Split => "split",
        CompactExampleKind::Merge => "merge",
        CompactExampleKind::Resegment => "resegment",
        CompactExampleKind::CoverageMissingFrom => "coverage_missing_from",
        CompactExampleKind::CoverageMissingTo => "coverage_missing_to",
        CompactExampleKind::CoverageUnequal => "coverage_unequal",
        CompactExampleKind::CoverageInvalidInput => "coverage_invalid_input",
        CompactExampleKind::FeatureDiff => "feature_diff",
    }
}
```

- [ ] **Step 2: Add runner-level example equivalence test**

In `compact.rs`, add a test that proves the hard compatibility claim:

```rust
#[test]
fn compact_streaming_examples_match_full_compact_examples() {
    let source = "今日明日";
    let analyses = vec![
        equivalence_analysis(
            "from",
            source,
            vec![
                equivalence_m(source, "今日", 0, 2, FeatureMap::new()),
                equivalence_m(source, "明日", 2, 4, FeatureMap::new()),
            ],
        ),
        equivalence_analysis(
            "to",
            source,
            vec![
                equivalence_m(source, "今", 0, 1, FeatureMap::new()),
                equivalence_m(source, "日", 1, 2, FeatureMap::new()),
                equivalence_m(source, "明日", 2, 4, FeatureMap::new()),
            ],
        ),
    ];

    let full = ab_morph_diff::compare_pair_with_source_text(&analyses[0], &analyses[1], source, &[]).unwrap();
    let full_rows = example_rows_from_comparison("source-a".to_owned(), source, &full, &analyses, 10);

    let compact = ab_morph_diff::compare_pair_compact_with_source_text(&analyses[0], &analyses[1], source, &[], 10).unwrap();
    let compact_rows = example_rows_from_compact_comparison("source-a".to_owned(), source, &compact);

    assert_eq!(compact_rows, full_rows);
}

fn equivalence_analysis(analyzer: &str, source: &str, morphemes: Vec<Morpheme>) -> Analysis {
    Analysis {
        analyzer: analyzer.to_owned(),
        text_id: "t1".to_owned(),
        source_text: source.to_owned(),
        morphemes,
    }
}

fn equivalence_m(
    source: &str,
    surface: &str,
    start: usize,
    end: usize,
    features: FeatureMap,
) -> Morpheme {
    let byte_start = source.char_indices().nth(start).map(|(idx, _)| idx).unwrap_or(source.len());
    let byte_end = source.char_indices().nth(end).map(|(idx, _)| idx).unwrap_or(source.len());
    Morpheme {
        surface: surface.to_owned(),
        byte_span: byte_start..byte_end,
        char_span: start..end,
        features,
    }
}
```

If `ComparisonExampleRow` does not derive `PartialEq`, add `PartialEq` to its derive list.

- [ ] **Step 3: Run equivalence test to verify red or pass after helpers**

Run: `cargo test -p ab-morph-run compact_streaming_examples_match_full_compact_examples`

Expected before wiring: FAIL because compact helpers or streaming API are not wired. Expected after Step 4: PASS.

- [ ] **Step 4: Wire `write_comparison_rows`**

In `lib.rs`, import:

```rust
use ab_morph_diff::{compare_pair_compact_with_source_text, ...};
```

Change `write_comparison_rows` so:

- `OutputProfile::Full` calls `compare_pair` and writes `ComparisonRow` as today.
- `OutputProfile::Compact` calls `compare_pair_compact_with_source_text` and writes `ComparisonSummaryRow::from_compact_comparison` plus `example_rows_from_compact_comparison`.
- Compact mode no longer constructs `Comparison`.

- [ ] **Step 5: Run compact runner tests**

Run:

```bash
cargo test -p ab-morph-run compact_profile_writes_summary_rows_without_full_regions compact_parallel_profile_writes_compressed_summary_rows compact_parallel_resume_appends_zstd_outputs_by_source_id compact_streaming_examples_match_full_compact_examples
```

Expected: all selected tests pass.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-morph-run/src/compact.rs crates/ab-morph-run/src/lib.rs
git commit -m "feat: use streaming compact morph output"
```

---

### Task 6: Verify and document expected memory behavior

**Files:**
- Modify: `docs/superpowers/reports/2026-04-29-morph-run-memory.md`

- [ ] **Step 1: Run full verification**

Run:

```bash
cargo fmt --all -- --check
cargo test -p ab-morph-diff
cargo test -p ab-morph-run
cargo build --release -p ab-morph-run
```

Expected: all commands pass.

- [ ] **Step 2: Run smoke if AAT scratch exists**

Run:

```bash
target/release/ab-morph-run analyze-aat   --aat-dir scratch/morph-full-corpus/aats   --analyzer vibrato   --analyzer sudachi-c   --output-profile compact   --analyses-output scratch/morph-streaming-smoke/analyses.jsonl.zst   --comparisons-output scratch/morph-streaming-smoke/comparisons.jsonl.zst   --examples-output scratch/morph-streaming-smoke/examples.jsonl.zst   --errors-output scratch/morph-streaming-smoke/errors.jsonl.zst   --jobs 8   --progress
```

Expected if scratch AAT has not been regenerated: FAIL clearly with `--aat-dir must point to an existing directory`.

Expected if scratch AAT exists: PASS and emit a final progress line.

- [ ] **Step 3: Update memory report**

Append:

```markdown
## Streaming compact comparison update

Compact output now uses `compare_pair_compact_with_source_text`, which accumulates `ComparisonStats` and bounded examples without storing a full `Vec<Region>` or `Vec<FeatureDiff>` for each comparison. Full output still uses `compare_pair` and remains behavior-compatible.

The expected memory impact is limited to per-document peaks for unusually large works. Typical full-corpus RSS/PSS is expected to look similar to the previous shared-analyzer run because dictionary residency and morpheme vectors dominate ordinary corpus runs.
```

If Step 2 produced a real progress line, include the actual elapsed/RSS/PSS values.

- [ ] **Step 4: Commit**

```bash
git add docs/superpowers/reports/2026-04-29-morph-run-memory.md
git commit -m "docs: report streaming compact comparison behavior"
```

---

## Self-Review

**Spec coverage:** The plan covers streaming compact comparisons, preserves full mode, reuses shared alignment and boundary helpers, proves stats equivalence, and adds runner-level example-row equivalence.

**Placeholder scan:** No unresolved implementation placeholders remain. The plan uses comments only where it directs moving existing code unchanged or mapping existing fields exactly.

**Type consistency:** `CompactComparison`, `CompactComparisonExample`, `CompactExampleKind`, and `CompactFeatureChange` are defined before use. The public API is consistently named `compare_pair_compact_with_source_text`.

**Review integration:** The plan incorporates the review by refactoring shared visitor/boundary helpers before streaming, adding example-row equivalence tests, using a real red stub, removing unused context plumbing from the accumulator, avoiding unused `BTreeMap` and `mut` bindings, bounding feature backlog by remaining budget, and softening memory expectations.
