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

- `ab-diff-utils` for `FrequencyTable` and hashing helpers;
- `anyhow` for error contexts;
- `serde` for output serialization.

No dependency on Vibrato, Sudachi, or adapter crates in this phase.

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

### 3.4 Feature Differences

```rust
pub struct FeatureDiff {
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
zero score.

## 4. Module Boundaries

### 4.1 `model.rs`

Owns all public data types and serde derives.

No algorithm logic beyond small constructors for tests if needed.

### 4.2 `validate.rs`

Validates one `Analysis` before comparison.

Rules:

- morphemes sorted by byte span;
- no overlapping morphemes;
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
exclude the outer `0` and `source_text.chars().count()` boundaries.

### 4.6 `aggregate.rs`

Provides first-pass corpus-level summaries without persistence.

Minimum artifacts for this phase:

```rust
pub struct AggregateSummary {
    pub segmentation_transformations: FrequencyTable<SegmentationKey, RegionExample>,
    pub feature_confusions: BTreeMap<FeatureKey, FrequencyTable<FeatureConfusionKey, RegionExample>>,
}
```

Keys must be structured serializable values, not formatted display strings.

Aggregation consumes `Comparison` values. It does not call alignment or inspect
analyzer-specific output.

### 4.7 `lib.rs`

Re-exports the stable public API:

- model types;
- `validate_analysis`;
- `compare_pair`;
- aggregation summary types.

## 5. Initial Public API

```rust
pub fn validate_analysis(analysis: &Analysis) -> Result<(), MorphDiffError>

pub fn compare_pair(
    from: &Analysis,
    to: &Analysis,
    feature_context_keys: &[FeatureKey],
) -> Result<Comparison, MorphDiffError>
```

`compare_pair` validates both analyses first, then aligns, compares features,
and derives stats.

Use a small typed error enum with `thiserror` or plain custom `Display` rather
than returning stringly errors from algorithm internals. The implementation plan
should choose the dependency based on existing workspace conventions; `anyhow`
can wrap these errors at CLI boundaries later.

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
- out-of-order spans are rejected;
- overlapping spans are rejected;
- byte spans that are not UTF-8 character boundaries are rejected;
- surface/source mismatch is rejected;
- one side with a gap emits coverage mismatch when the other side covers that
  span;
- segmentation region adjacent to feature-diff region stays as two regions;
- boundary precision/recall/F1 are deterministic and exclude outer boundaries.

## 7. Non-Goals

- No Vibrato adapter in this phase.
- No Sudachi adapter in this phase.
- No dictionary discovery or environment-variable handling in this phase.
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
- corpus aggregation.

It deliberately chooses simple string identifiers and deterministic maps for the
first implementation. That avoids coupling the core engine to dictionary loading,
analyzer selection, CLI formats, or future multiway comparison.

The main risk is region alignment around gaps. The implementation plan should
therefore start with validation and small alignment fixtures before adding
aggregation.
