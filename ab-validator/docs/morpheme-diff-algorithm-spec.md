# Morpheme Diff Algorithm Spec

Status: draft for a future Rust implementation.

Scope: specify how to compare Japanese morphological analyses of the same text.
The document is intentionally independent of any existing implementation.

## 1. Problem Statement

Japanese morphological analyzers often disagree in two different ways:

1. **Segmentation**: the analyzers partition the same text into different
   morpheme units.
2. **Annotation**: the analyzers agree on a morpheme span, but assign different
   features such as part of speech, lemma, conjugation, reading, or dictionary
   form.

These must not be collapsed into a single "token diff". A feature comparison is
meaningful only when both analyzers refer to the same surface span. If one side
has `今日` and the other has `今` + `日`, the primary fact is a segmentation
split; comparing the POS of `今日` against `今` is not a sound linguistic
comparison.

The core task is:

```text
Given:
  one source text
  N morphological analyses of that text
  a selected feature set

Produce:
  pairwise and aggregate descriptions of segmentation disagreement,
  feature disagreement on aligned morphemes,
  coverage or normalization failures,
  and analysis artifacts suitable for corpus-level inspection.
```

## 2. Terms

- **Source text**: the original string being analyzed.
- **Morpheme**: one analyzer output unit with a surface string, source span, and
  feature map.
- **Boundary**: a character offset at which a morpheme starts or ends.
- **Region**: a contiguous source-text span used as the unit of comparison.
- **One-to-one region**: exactly one morpheme from each analysis covers the same
  span.
- **Segmentation region**: both analyses cover the same text span, but with
  different morpheme groupings.
- **Coverage mismatch**: one or both analyses fail to account for the same
  source-text span.
- **Feature diff**: a field-level difference between two morphemes in a
  one-to-one region.

## 3. Requirements

The algorithm should:

1. compare multiple analyses of the same text pairwise;
2. preserve source-text spans for every morpheme;
3. separate segmentation differences from feature differences;
4. classify segmentation differences as split, merge, or resegmentation;
5. represent missing feature values explicitly;
6. produce deterministic output;
7. report analyzer coverage failures separately from linguistic disagreement;
8. support corpus-level aggregation without losing sentence-level evidence.

The algorithm should not:

1. compare feature maps inside segmentation disagreements;
2. rely on formatted report strings as data keys;
3. silently repair analyzer output that does not cover the source text;
4. assume token indices are comparable across analyses without span alignment.

## 4. Available Approaches

### 4.1 Token Sequence Diff

Diff the two surface-token sequences directly, using a standard sequence diff
algorithm.

Example:

```text
A: [今日, は, 晴れ]
B: [今, 日, は, 晴れ]
```

Advantages:

- simple to implement;
- works well when most tokens are identical;
- can reuse standard diff libraries.

Limits:

- token indices are analyzer-specific;
- edit-script shape may depend on library heuristics;
- multi-token replacements need post-processing to discover that joined
  surfaces are equal;
- text coverage problems and segmentation choices can be conflated.

Use this only for quick visual reports or as a fallback diagnostic, not as the
core data model.

### 4.2 Character Diff of Joined Surfaces

Join each analyzer's surfaces into a string and run a character diff.

Advantages:

- good at detecting coverage and normalization problems;
- independent of morpheme boundaries.

Limits:

- throws away token structure;
- cannot distinguish splits from merges by itself;
- cannot support feature comparison.

Use this as a validation tool, not as the primary morpheme diff.

### 4.3 Boundary Set Comparison

Represent each analysis as a set of morpheme boundary offsets. Compare boundary
sets across analyses.

Example:

```text
source: 今日
A boundaries: {0, 2}
B boundaries: {0, 1, 2}
```

Advantages:

- directly captures segmentation disagreement;
- cheap to compute;
- useful for corpus-level boundary stability metrics.

Limits:

- does not preserve the grouped morphemes on each side;
- does not classify complex regions without additional span grouping;
- does not compare features.

Use this as an analysis artifact and as a building block for span alignment.

### 4.4 Span-Based Region Alignment

Assign each morpheme a source-text span. Build comparison regions by merging
overlapping morpheme spans from both analyses.

Advantages:

- models the actual invariant: analyzers partition the same text;
- naturally separates one-to-one alignment from segmentation disagreement;
- does not depend on edit-script heuristics;
- handles split, merge, and resegmentation uniformly.

Limits:

- requires reliable span reconstruction;
- parser adapters must handle normalization explicitly;
- slightly more work than token diff.

This is the recommended core approach.

### 4.5 Weighted Alignment

Model alignment as a dynamic-programming problem with costs for boundary
changes, surface mismatches, and feature differences.

Advantages:

- can align noisy data;
- can choose a globally minimal explanation;
- useful when analyzers normalize text differently.

Limits:

- cost weights are subjective;
- output can be harder to explain;
- risks hiding coverage errors as low-cost alignments.

Use this only for exploratory tooling or noisy OCR-like inputs. For clean
same-source analyzer comparison, prefer deterministic span alignment.

### 4.6 Multiway Alignment

Align all N analyses at once rather than pairwise.

Advantages:

- useful for consensus analysis;
- can identify stable and unstable boundaries across many analyzers;
- avoids duplicating repeated pairwise facts.

Limits:

- more complex data model;
- pairwise reports are still needed for many workflows;
- feature comparison becomes sparse when not all analyzers agree on a span.

Recommended strategy: implement pairwise comparison first, then derive multiway
artifacts from shared boundary and region indexes.

## 5. Recommended Algorithm

Use strict span-based region alignment as the primary algorithm.

### 5.1 Data Model

Illustrative Rust-style shapes:

```rust
struct Morpheme {
    surface: String,
    byte_span: Range<usize>,
    char_span: Range<usize>,
    features: BTreeMap<FeatureKey, Option<String>>,
}

struct Analysis {
    analyzer: AnalyzerId,
    text_id: TextId,
    morphemes: Vec<Morpheme>,
}

enum Region {
    OneToOne(AlignedMorpheme),
    Segmentation(SegmentationDiff),
    CoverageMismatch(CoverageMismatch),
}

struct AlignedMorpheme {
    text_span: Range<usize>,
    from_index: usize,
    to_index: usize,
}

struct SegmentationDiff {
    text_span: Range<usize>,
    from_indices: Range<usize>,
    to_indices: Range<usize>,
    from_surfaces: Vec<String>,
    to_surfaces: Vec<String>,
    kind: SegmentationKind,
}

enum SegmentationKind {
    Split,
    Merge,
    Resegment,
}

struct FeatureDiff {
    text_span: Range<usize>,
    surface: String,
    changed: BTreeMap<FeatureKey, ChangedValue>,
    same_context: BTreeMap<FeatureKey, Option<String>>,
}
```

Use deterministic maps for stable serialization, testing, and report diffs.

### 5.2 Span Construction

Every parser adapter must map analyzer output back to the source text.

Strict rule:

```text
concat(morpheme.surface) == source_text
```

If an analyzer emits normalized surfaces, skipped characters, or inserted
content, the adapter must either:

- expose the exact source spans anyway; or
- return a coverage error; or
- explicitly emit recoverable coverage-mismatch records.

Do not silently search ahead in the source text unless the adapter records the
skipped span and reason. Silent recovery makes downstream statistics
untrustworthy.

### 5.3 Region Alignment

For one pair of analyses:

1. Validate that both analyses are sorted by non-overlapping source spans.
2. Walk both morpheme lists from left to right.
3. Start a region at the earliest unconsumed morpheme start.
4. Grow the region end until every morpheme from both analyses that intersects
   the region has been consumed.
5. Classify the region:
   - one morpheme on each side with identical spans: `OneToOne`;
   - one or more morphemes on both sides, same source span, different grouping:
     `Segmentation`;
   - missing, overlapping, or unequal source coverage: `CoverageMismatch`.

Equivalent implementation: build the union of all morpheme start and end
boundaries, inspect adjacent boundary windows, then merge connected windows into
regions.

### 5.4 Feature Comparison

For each `OneToOne` region:

1. take the union of feature keys from both morphemes;
2. compare values as `Option<String>`;
3. emit no `FeatureDiff` if all values are equal;
4. otherwise emit changed fields and optional same-valued context fields.

Missing values are data:

```text
from: Some("名詞")
to:   None
```

is a feature difference, not an absent comparison.

### 5.5 Pairwise Output

Each pairwise comparison should produce:

```rust
struct Comparison {
    from_analyzer: AnalyzerId,
    to_analyzer: AnalyzerId,
    text_id: TextId,
    regions: Vec<Region>,
    feature_diffs: Vec<FeatureDiff>,
    stats: ComparisonStats,
}
```

Minimum stats:

- total morphemes on each side;
- one-to-one regions;
- one-to-one regions with feature differences;
- segmentation regions;
- morphemes affected by segmentation on each side;
- split, merge, and resegment counts;
- coverage mismatch count;
- boundary precision/recall/F1, treating one side as reference when desired.

## 6. Analysis Artifacts

The diff engine should produce structured artifacts that are useful before any
HTML, CSV, or notebook presentation is built. These artifacts are the main
reason to keep the algorithm span-based and deterministic.

### 6.1 Region Ledger

A row per comparison region.

Fields:

- text id;
- sentence or passage id;
- analyzer pair;
- character span;
- source substring;
- region kind;
- surfaces from A;
- surfaces from B;
- feature diff ids, if any.

Why it is useful:

- gives a lossless audit trail;
- supports concordance views;
- lets users inspect surprising aggregate counts.

Example:

```text
span  text  kind          A             B
0..2  今日  segmentation  [今日]        [今, 日]
2..3  は    one_to_one    [は]          [は]
3..5  晴れ  feature_diff  [晴れ]        [晴れ]
```

### 6.2 Segmentation Transformation Table

A frequency table keyed by grouped surfaces and segmentation kind.

Fields:

- analyzer pair;
- kind: split, merge, resegment;
- from surfaces;
- to surfaces;
- frequency;
- example text ids;
- example spans.

Why it is useful:

- shows systematic dictionary or analyzer differences;
- surfaces high-value examples for linguistic review;
- distinguishes common harmless splits from rare suspicious behavior.

Example:

```text
kind   from       to          count
Split  [今日]     [今, 日]    184
Merge  [で, は]   [では]      96
```

### 6.3 Boundary Disagreement Profile

A boundary-level artifact across a corpus.

Fields:

- analyzer or analyzer pair;
- boundary offset or normalized position;
- preceding and following characters;
- whether each analyzer places a boundary;
- local character n-gram;
- frequency.

Useful derived metrics:

- boundary precision, recall, and F1 against a selected reference;
- boundary entropy across all analyzers;
- most unstable left/right character contexts.

Why it is useful:

- reveals whether disagreements cluster around particles, compounds, symbols,
  kana/kanji transitions, punctuation, or foreign text.

### 6.4 Feature Confusion Matrices

For one-to-one regions only, produce confusion matrices per feature.

Common matrices:

- POS level 1;
- POS level 2;
- conjugation type;
- conjugation form;
- lemma;
- reading or pronunciation.

Fields:

- feature key;
- analyzer pair;
- from value;
- to value;
- frequency;
- example spans.

Why it is useful:

- shows systematic annotation differences after segmentation noise has been
  removed;
- prevents POS disagreement rates from being inflated by tokenization
  disagreement.

### 6.5 Feature Delta Inventory

A frequency table of full changed feature maps for aligned morphemes.

Fields:

- source surface;
- changed feature map;
- same-valued context fields;
- frequency;
- examples.

Why it is useful:

- captures multi-field annotation patterns, not only independent feature
  counts;
- identifies cases such as "same lemma, different POS" or "same POS, different
  conjugation form".

Example:

```text
surface: ある
changed:
  pos-1: 動詞 -> 形容詞
  lemma: 有る -> ある
same:
  c-form: 終止形
count: 37
```

### 6.6 Analyzer Bias Summary

A corpus-level profile for each analyzer and analyzer pair.

Metrics:

- average morphemes per character;
- average morphemes per sentence;
- split tendency relative to another analyzer;
- merge tendency relative to another analyzer;
- percentage of one-to-one regions;
- percentage of one-to-one regions with feature diffs;
- coverage error rate.

Why it is useful:

- makes analyzer behavior comparable at a glance;
- separates "more fine-grained tokenizer" from "different annotator".

### 6.7 Concordance Examples

For every high-frequency segmentation transformation or feature delta, keep a
bounded list of examples.

Fields:

- text id;
- local context before and after the span;
- source substring;
- analyzer outputs for the span;
- selected feature maps;
- stable link or offset into the source text.

Why it is useful:

- aggregate tables are not enough for linguistic judgment;
- examples make it possible to distinguish meaningful patterns from artifacts.

### 6.8 Coverage Audit

A report of cases where analyzer output does not cleanly map to source spans.

Fields:

- analyzer;
- text id;
- source span;
- emitted surface;
- expected source substring;
- mismatch kind: deletion, insertion, normalization, overlap, out-of-order,
  unknown;
- adapter decision: hard error or recoverable mismatch.

Why it is useful:

- protects the validity of all downstream comparisons;
- identifies adapter bugs and analyzer normalization behavior early.

### 6.9 Multiway Boundary Consensus

When comparing more than two analyzers, produce a boundary consensus table.

Fields:

- text id;
- boundary offset;
- analyzers that place a boundary;
- analyzers that do not;
- support count;
- entropy or disagreement score.

Why it is useful:

- identifies stable morpheme boundaries independent of any one analyzer;
- supports consensus-token views and majority-boundary baselines.

### 6.10 Interestingness Ranking

Rank examples for human review.

Signals:

- high-frequency transformation;
- low-frequency but high-impact transformation;
- disagreement involving major POS categories;
- disagreement involving lemma changes;
- long source span;
- high boundary entropy;
- coverage mismatch;
- disagreement among otherwise similar analyzers.

Why it is useful:

- large corpora produce too many diffs;
- review should start with cases most likely to reveal analyzer behavior or
  data problems.

## 7. Aggregation Model

Keep diffing and aggregation separate.

Diffing produces sentence-level facts:

- regions;
- segmentation diffs;
- feature diffs;
- coverage mismatches.

Aggregation derives corpus-level tables:

- counts by analyzer pair;
- counts by text, genre, period, or corpus partition;
- frequency tables of segmentation transformations;
- feature confusion matrices;
- analyzer bias metrics;
- ranked examples.

Recommended storage shape:

```text
comparisons/<pair>/<text_id>/regions
comparisons/<pair>/<text_id>/feature_diffs
comparisons/<pair>/<text_id>/coverage
aggregates/<pair>/segmentation_transformations
aggregates/<pair>/feature_confusions
aggregates/multiway/boundary_consensus
```

The exact persistence format can be JSONL, Parquet, SQLite, or another
structured format. The important constraint is that rows remain stable,
typed, and reproducible.

## 8. Edge Cases

The implementation should explicitly test:

1. empty input;
2. punctuation-only input;
3. repeated surface strings;
4. multi-byte characters;
5. mixed Japanese and Latin text;
6. analyzer output with normalized surfaces;
7. unknown tokens;
8. one analyzer producing zero morphemes for non-empty input;
9. overlapping spans from a parser adapter;
10. out-of-order spans;
11. multiple segmentation regions in one sentence;
12. segmentation region immediately adjacent to a feature-diff region;
13. feature key present only on one side;
14. N-way comparison where only some analyzers agree on a boundary.

## 9. Validation Invariants

Before comparison:

```text
morpheme spans are sorted
morpheme spans do not overlap within one analysis
surface text maps to source spans
feature maps are deterministic
```

After region alignment:

```text
regions are sorted
regions do not overlap
every source span is either covered by a region or reported as coverage mismatch
feature diffs exist only for one-to-one regions
segmentation diffs have at least one morpheme on both sides
```

After aggregation:

```text
aggregate counts equal sums of source comparison rows
example references point to existing region rows
frequency table keys are structured values, not display strings
```

## 10. Recommendation

Implement strict span-based region alignment first. It gives the cleanest model
of the problem and produces the richest analysis artifacts.

Use token sequence diff and character diff as optional diagnostics, not as the
foundation. Add multiway consensus after pairwise comparison is stable. Consider
weighted alignment only for noisy sources where exact source-span coverage is
not available.

The highest-value implementation boundary is:

```text
parser adapter -> validated spans -> region alignment -> feature comparison -> artifacts -> reports
```

Each stage should be testable without the next one.
