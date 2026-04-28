# Morpheme Diff Core Engine Design

Status: approved for implementation planning
Date: 2026-04-28

Scope: build the core span-based morpheme comparison engine described in
`docs/morpheme-diff-algorithm-spec.md`, without integrating real analyzers yet.
This phase accepts typed in-memory analyses in tests and produces deterministic
pairwise comparison data. Vibrato and Sudachi adapters are intentionally deferred
until the core alignment model is stable.

## 1. Motivation

Japanese morphological analyzers disagree in two separable ways:

- segmentation: where token boundaries fall;
- annotation: what features are assigned to a token span.

The engine must not compare feature maps across different spans. If one side
emits `今日` and another emits `今` + `日`, the primary fact is segmentation
split, not a POS difference between `今日` and `今`.

The first implementation target is therefore the analyzer-independent core:
validate spans, align regions, compare features only for one-to-one regions, and
produce deterministic structured facts.

## 2. New Crate: `ab-morph-diff`

Location: `crates/ab-morph-diff/`

Workspace crate depending on:

- `serde` for output serialization.

No dependency on Vibrato, Sudachi, or adapter crates in this phase.
No dependency on `anyhow` in this phase: the core crate exposes typed errors,
and `anyhow` can wrap them later at CLI or adapter boundaries. No dependency on
`ab-diff-utils` in this phase either, because aggregation is deferred until real
adapter and corpus data exist.

## 3. Public Data Model

### 3.1 Identifiers and Feature Keys

Use strings for identifiers in the first phase to avoid premature type systems
around analyzer registries and corpus metadata:

```rust
pub type AnalyzerId = String;
pub type TextId = String;
pub type FeatureKey = String;
```

Feature maps are deterministic:

```rust
pub type FeatureMap = BTreeMap<FeatureKey, Option<String>>;
```

`None` is an explicit missing value. A missing value on one side and present
value on the other side is a real feature difference.

This flat string model is a deliberate phase-1 trade-off. It fits UniDic-style
fields and keeps the core analyzer-independent. If a later analyzer needs nested
or repeated feature values, the adapter should initially encode stable string
values and the model can be revisited with concrete evidence.

### 3.2 Morpheme and Analysis

```rust
pub struct Morpheme {
    pub surface: String,
    pub byte_span: Range<usize>,
    pub char_span: Range<usize>,
    pub features: FeatureMap,
}

pub struct Analysis {
    pub analyzer: AnalyzerId,
    pub text_id: TextId,
    pub source_text: String,
    pub morphemes: Vec<Morpheme>,
}
```

Both byte and character spans are stored:

- byte spans make substring validation cheap and unambiguous in Rust;
- char spans produce user-facing stable offsets for reports.

Adapters added later are responsible for producing both spans. This crate only
validates them.

### 3.3 Comparison Regions

```rust
pub enum Region {
    OneToOne(AlignedMorpheme),
    Segmentation(SegmentationDiff),
    CoverageMismatch(CoverageMismatch),
}

pub struct AlignedMorpheme {
    pub text_span: Range<usize>,
    pub from_index: usize,
    pub to_index: usize,
}

pub struct SegmentationDiff {
    pub text_span: Range<usize>,
    pub from_indices: Range<usize>,
    pub to_indices: Range<usize>,
    pub from_surfaces: Vec<String>,
    pub to_surfaces: Vec<String>,
    pub kind: SegmentationKind,
}

pub enum SegmentationKind {
    Split,
    Merge,
    Resegment,
}

pub struct CoverageMismatch {
    pub text_span: Range<usize>,
    pub from_indices: Range<usize>,
    pub to_indices: Range<usize>,
    pub reason: CoverageMismatchKind,
}

pub enum CoverageMismatchKind {
    MissingFrom,
    MissingTo,
    UnequalCoverage,
    InvalidInput,
}
```

`text_span` uses character offsets in all public regions. Byte spans stay on
morphemes for validation.

For missing-side coverage mismatches, the missing side uses an empty range
anchored at the next-to-be-consumed morpheme index at the start of the region.
For example, if `from` has consumed morphemes `0..3` and `to` covers the next
span with `to[5]`, the region uses `from_indices: 3..3` and
`to_indices: 5..6`. Consumers must treat empty ranges as insertion/gap anchors,
not as omitted data.

### 3.4 Feature Differences

```rust
pub struct FeatureDiff {
    pub region_index: usize,
    pub text_span: Range<usize>,
    pub surface: String,
    pub from_index: usize,
    pub to_index: usize,
    pub changed: BTreeMap<FeatureKey, ChangedValue>,
    pub same_context: BTreeMap<FeatureKey, Option<String>>,
}

pub struct ChangedValue {
    pub from: Option<String>,
    pub to: Option<String>,
}
```

Feature diffs are emitted only for `Region::OneToOne`.
`region_index` points to the owning `regions` entry in `Comparison`. The other
fields are intentionally denormalized to keep JSONL rows and future reports easy
to consume without repeated region lookups.

### 3.5 Comparison Output

```rust
pub struct Comparison {
    pub from_analyzer: AnalyzerId,
    pub to_analyzer: AnalyzerId,
    pub text_id: TextId,
    pub regions: Vec<Region>,
    pub feature_diffs: Vec<FeatureDiff>,
    pub stats: ComparisonStats,
}
```

Minimum stats:

```rust
pub struct ComparisonStats {
    pub from_morphemes: usize,
    pub to_morphemes: usize,
    pub one_to_one_regions: usize,
    pub one_to_one_with_feature_differences: usize,
    pub segmentation_regions: usize,
    pub coverage_mismatch_regions: usize,
    pub split_regions: usize,
    pub merge_regions: usize,
    pub resegment_regions: usize,
    pub from_morphemes_in_segmentation: usize,
    pub to_morphemes_in_segmentation: usize,
    pub boundary_precision: Option<f64>,
    pub boundary_recall: Option<f64>,
    pub boundary_f1: Option<f64>,
}
```

Boundary metrics treat `from` as reference for recall and `to` as predicted for
precision. Empty-boundary cases produce `None` rather than inventing a perfect or
zero score. If coverage mismatch regions exist, boundary metrics ignore any
boundary whose offset lies inside a coverage mismatch span. Boundaries in
one-to-one and segmentation regions remain comparable because both analyses
cover the same source text there. If filtering leaves no comparable internal
boundaries, all three boundary metric fields are `None`.

The split/merge/resegment fields are redundant with `segmentation_regions`, but
the redundancy is intentional for stable JSON output and simpler dashboards.

## 4. Module Boundaries

### 4.1 `model.rs`

Owns all public data types and serde derives.

No algorithm logic beyond small constructors for tests if needed.

### 4.2 `validate.rs`

Validates one `Analysis` before comparison.

Rules:

- morphemes sorted by byte span;
- no overlapping morphemes: touching spans such as `0..2` followed by `2..4`
  are valid, while `0..2` followed by `1..3` is invalid;
- `byte_span` bounds are valid UTF-8 boundaries inside `source_text`;
- `char_span` matches the byte span's character offsets;
- `surface == source_text[byte_span]`;
- full coverage is not required, because gaps must become coverage mismatch
  regions rather than hard validation failures.

Hard validation failures are reserved for internally inconsistent analyzer
output: invalid spans, overlap, out-of-order spans, or surface/source mismatch.

### 4.3 `align.rs`

Builds comparison regions from two validated analyses.

Algorithm:

1. Walk both morpheme vectors from left to right.
2. If the next morphemes have identical char spans, emit `OneToOne`.
3. Otherwise start a region at the earliest next start offset.
4. Grow the region end until all morphemes from both analyses that intersect the
   region are consumed.
5. If both sides cover the same source span with different grouping, emit
   `Segmentation`.
6. If either side has a gap or the consumed spans do not cover the same source
   range, emit `CoverageMismatch`.

Classification:

- `Split`: one `from` morpheme maps to multiple `to` morphemes;
- `Merge`: multiple `from` morphemes map to one `to` morpheme;
- `Resegment`: both sides have multiple morphemes or a non-simple regrouping.

The step-2 one-to-one short-circuit is correct only after validation proves
morpheme spans within each analysis are sorted and non-overlapping. With that
invariant, no later morpheme can also intersect the identical span.

### 4.4 `features.rs`

Consumes one-to-one regions and emits feature differences.

Rules:

- feature key set is the union of both morpheme feature maps;
- `Option<String>` values are compared directly;
- `same_context` includes unchanged keys requested by the caller;
- no feature comparison happens for segmentation or coverage mismatch regions.

The first implementation may expose a simple `compare_features(comparison, context_keys)` helper
or keep this inside `compare_pair`, as long as feature comparison remains a
separate function internally.

### 4.5 `stats.rs`

Derives `ComparisonStats` from regions, feature diffs, and source analyses.

Boundary precision/recall/F1 is based on internal morpheme boundaries only:
exclude the outer `0` and `source_text.chars().count()` boundaries. Ignore
boundaries inside coverage mismatch regions.

### 4.6 Deferred Aggregation Schema

Aggregation is not part of phase 1. With no real analyzer adapters and no corpus
volume, implementing `aggregate.rs` now would mostly freeze guesses. Phase 1
therefore stops at deterministic per-text `Comparison` values.

The following schema should guide the later aggregation phase so the core output
does not block corpus-level summaries:

```rust
pub struct SegmentationKey {
    pub kind: SegmentationKind,
    pub from_surfaces: Vec<String>,
    pub to_surfaces: Vec<String>,
}

pub struct FeatureConfusionKey {
    pub from: Option<String>,
    pub to: Option<String>,
}

pub struct RegionExample {
    pub text_id: TextId,
    pub region_index: usize,
    pub text_span: Range<usize>,
    pub source: String,
    pub from_surfaces: Vec<String>,
    pub to_surfaces: Vec<String>,
}
```

`RegionExample` deliberately does not embed full feature snapshots in this
phase. Future aggregation can add a separate `FeatureExample` if UI/reporting
needs complete feature maps. Example budgets should be caller-provided rather
than hardcoded to `ab-diff-utils::DEFAULT_MAX_EXAMPLES`, so corpus runs and tests
can use different limits deliberately.

All aggregate keys must be structured serializable values, not formatted display
strings. Future aggregation consumes `Comparison` values. It does not call
alignment or inspect analyzer-specific output.

### 4.7 `lib.rs`

Re-exports the stable public API:

- model types;
- `validate_analysis`;
- `compare_pair`.

## 5. Initial Public API

```rust
pub fn validate_analysis(analysis: &Analysis) -> Result<(), MorphDiffError>

pub fn compare_pair(
    from: &Analysis,
    to: &Analysis,
    feature_context_keys: &[FeatureKey],
) -> Result<Comparison, MorphDiffError>
```

`compare_pair` validates both analyses first, then checks cross-analysis
preconditions, aligns, compares features, and derives stats.

Cross-analysis preconditions:

- `from.text_id == to.text_id`;
- `from.source_text == to.source_text`.

Violation is a typed `MorphDiffError` variant. Identical char spans are only
meaningful when both analyses refer to the same source string.

`feature_context_keys` is a set-like input. Duplicate keys are ignored. Output
ordering is deterministic because `same_context` is a `BTreeMap`, not the caller
input order.

Use a small typed error enum with `thiserror` or plain custom `Display` rather
than returning stringly errors from algorithm internals. The implementation plan
should prefer a hand-rolled enum implementing `Display` and `std::error::Error`
unless adding `thiserror` is already justified elsewhere.

Minimum error variants:

```rust
pub enum MorphDiffError {
    TextIdMismatch { from: TextId, to: TextId },
    SourceTextMismatch { text_id: TextId },
    OutOfOrderSpan { analyzer: AnalyzerId, text_id: TextId, index: usize },
    OverlappingSpan { analyzer: AnalyzerId, text_id: TextId, previous: usize, current: usize },
    InvalidByteSpan { analyzer: AnalyzerId, text_id: TextId, index: usize },
    CharSpanMismatch { analyzer: AnalyzerId, text_id: TextId, index: usize },
    SurfaceMismatch { analyzer: AnalyzerId, text_id: TextId, index: usize },
}
```

## 6. Testing Strategy

Unit tests are the primary validation for this phase. They should construct
`Analysis` values directly and avoid analyzer libraries.

Required tests:

- empty input produces no regions and zero counts;
- punctuation-only one-to-one input compares normally;
- identical segmentation and identical features produce one-to-one regions and
  no feature diffs;
- identical segmentation with different POS emits one feature diff;
- missing feature on one side emits a feature diff with `None`;
- `今日` vs `今` + `日` emits `Split` and no feature diff;
- `で` + `は` vs `では` emits `Merge` and no feature diff;
- `abc` + `def` vs `ab` + `cdef` emits `Resegment`;
- repeated surfaces align by span, not by string search;
- multi-byte text validates byte and char spans correctly;
- mixed Japanese and Latin text validates byte and char spans correctly;
- out-of-order spans are rejected;
- overlapping spans are rejected;
- byte spans that are not UTF-8 character boundaries are rejected;
- surface/source mismatch is rejected;
- identical text id with differing source text is rejected;
- differing text ids are rejected;
- one analyzer producing zero morphemes for non-empty input yields a
  `MissingFrom` or `MissingTo` coverage mismatch rather than a validation error;
- one side with a gap emits coverage mismatch when the other side covers that
  span;
- multiple segmentation regions in one sentence are emitted as separate regions;
- segmentation region adjacent to feature-diff region stays as two regions;
- boundary precision/recall/F1 are deterministic and exclude outer boundaries.
- boundary precision/recall/F1 ignore boundaries inside coverage mismatch spans;
- property tests generate sorted non-overlapping span partitions and assert that
  output regions are sorted, non-overlapping, and do not emit feature diffs for
  non-one-to-one regions.

## 7. Non-Goals

- No Vibrato adapter in this phase.
- No Sudachi adapter in this phase.
- No dictionary discovery or environment-variable handling in this phase.
- No aggregation implementation in this phase.
- No CLI in this phase unless needed for manual debugging.
- No multiway alignment in this phase.
- No weighted/noisy alignment in this phase.
- No HTML reports or presentation layer.
- No persistent storage format selection.

## 8. Future Adapter Notes

The next phase should reuse the concrete patterns observed in `../vibrato-pipe`:

- load Vibrato rkyv dictionaries from `.dic.zst`, `.dic`, or dictionary
  directories;
- prefer `Dictionary::from_zstd` and `Dictionary::from_path` where available;
- create `Tokenizer::new(dict).ignore_space(true)`;
- derive UniDic feature fields from `token.feature().split(',')`;
- compute source spans by accumulating emitted token surfaces and validating
  against source text;
- report normalization or skipped text as coverage mismatches rather than
  silently repairing.

Sudachi integration needs a separate context pass because no Sudachi usage was
found in the searched `../vibrato-pipe` source/docs. The adapter should still
produce the same `Analysis` model.

## 9. Design Review

This design keeps four concerns separate:

- analyzer output production;
- span validation;
- region alignment;
- future corpus aggregation.

It deliberately chooses simple string identifiers and deterministic maps for the
first implementation. That avoids coupling the core engine to dictionary loading,
analyzer selection, CLI formats, or future multiway comparison.

The main risk is region alignment around gaps. The implementation plan should
therefore start with validation and small alignment fixtures, then add property
tests before any future aggregation work.
