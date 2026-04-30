# N-way Morph Diff Design

Date: 2026-04-30

## Goal

Add true generic N-way morpheme diffing and reporting so a run with analyzers such as `vibrato`, `sudachi-a`, and `sudachi-c` can answer questions that pairwise rows cannot:

- Which source spans have the same segmentation in all analyzers?
- Which spans partition analyzers into groups with identical tokenization?
- Which feature/POS values differ across all analyzers on the same comparable span?
- Which feature differences are observable inside split/merge/resegment spans without pretending unrelated tokens align?
- What are the exact corpus-level top segmentation and feature patterns, independent of bounded pairwise examples?

## Core decision

N-way comparison is the primitive. Pairwise comparison is the `N = 2` view.

The existing public pairwise API and JSONL formats stay stable, but the core alignment concept should not have two independent implementations. The implementation should generalize region construction once, then project it into:

- existing pairwise `Comparison` / `CompactComparison` rows;
- new compact N-way rows;
- exact N-way reports.

This avoids long-term drift between pairwise and N-way alignment, stats, and reporting semantics.

## Current state

`ab-morph-run analyze-aat` accepts multiple analyzers today, but it emits pairwise comparisons for every analyzer pair. This gives useful pairwise ranking, but it does not provide a first-class N-way fact like:

```text
span: 0..2
vibrato:   [今日]
sudachi-a: [今日]
sudachi-c: [今, 日]
```

The current `summarize-differences` command aggregates bounded pairwise examples, so it is a triage report, not a comprehensive corpus-level confusion matrix.

## Design principles

1. Span regions are the durable unit; edit scripts are not.
2. Same-source text is a hard precondition.
3. N-way alignment is canonical; pairwise is a projection.
4. Coverage, segmentation, and feature facts are independent. Do not collapse them behind a precedence enum.
5. Use analyzer IDs as values in model and wire rows. Do not expose positional analyzer-index contracts.
6. Exact reports aggregate N-way rows, not bounded examples.
7. Bounded examples are presentation, not core comparison data.
8. Wire fields must be structured serde data, not `Debug` strings or delimiter-joined keys.
9. Stay generic for any `N >= 2`.

## Scope

In scope:

- Generic region construction over `N >= 2` analyses.
- Pairwise projection from the generic region model, preserving existing pairwise outputs.
- Compact N-way JSONL rows from `ab-morph-run`.
- Corpus-level N-way summary commands.
- Feature/POS reporting in strict one-token regions and in segmentation regions where evidence is sound.
- Workflow documentation using `vibrato + sudachi-a + sudachi-c`.

Out of scope for phase 1:

- Cross-AAT or cross-version text comparison.
- Full-detail N-way output containing every morpheme feature on every row.
- Majority-vote adjudication.
- Linguistic equivalence mapping between analyzer dictionaries.
- Removing existing pairwise CLI outputs.

## Data model

### SharedRegion

Internal primitive used by both pairwise and N-way projections:

```rust
struct SharedRegion {
    region_index: usize,
    text_span: Range<usize>,
    per_analyzer: Vec<AnalyzerRegion>,
}
```

`per_analyzer` is in run analyzer order, but public grouping data uses `AnalyzerId`, not indices.

### AnalyzerRegion

```rust
struct AnalyzerRegion {
    analyzer: AnalyzerId,
    indices: Range<usize>,
    surfaces: Vec<String>,
    covers_exactly: bool,
}
```

`covers_exactly` is true only when the analyzer's morphemes fully cover the region with no internal gap.

### NwayComparison

```rust
pub struct NwayComparison {
    pub text_id: TextId,
    pub analyzers: Vec<AnalyzerId>,
    pub regions: Vec<NwayRegion>,
    pub stats: NwayStats,
}
```

No `examples` field lives in the core comparison. Examples are selected views over `regions` by the runner/reporting layer.

### NwayRegion

```rust
pub struct NwayRegion {
    pub region_index: usize,
    pub text_span: Range<usize>,
    pub per_analyzer: Vec<NwayAnalyzerRegion>,
    pub segmentation_groups: Vec<NwaySegmentationGroup>,
    pub feature_groups: Vec<NwayFeatureGroup>,
}
```

Derived convenience methods can answer:

- `has_coverage_mismatch = per_analyzer.iter().any(|a| !a.covers_exactly)`
- `has_segmentation_disagreement = segmentation_groups.len() > 1`
- `has_feature_disagreement = feature_groups.iter().any(|g| g.values.len() > 1)`
- `is_agreement = !has_coverage_mismatch && !has_segmentation_disagreement && !has_feature_disagreement`

The model intentionally does not force these orthogonal facts into one precedence-based enum. `per_analyzer[*].covers_exactly` is the source of truth for coverage; coverage aggregate booleans are derived.

### NwayAnalyzerRegion

```rust
pub struct NwayAnalyzerRegion {
    pub analyzer: AnalyzerId,
    pub indices: Range<usize>,
    pub surfaces: Vec<String>,
    pub covers_exactly: bool,
}
```

### Segmentation groups

```rust
pub struct NwaySegmentationGroup {
    pub surfaces: Vec<String>,
    pub analyzers: Vec<AnalyzerId>,
}
```

For the example above:

```text
[今日] -> vibrato, sudachi-a
[今, 日] -> sudachi-c
```

### Feature groups

```rust
pub struct NwayFeatureGroup {
    pub key: FeatureKey,
    pub scope: NwayFeatureScope,
    pub values: Vec<NwayFeatureValueGroup>,
}

pub enum NwayFeatureScope {
    WholeRegion,
    TokenPosition { position: usize },
    Surface { surface: String },
}

pub struct NwayFeatureValueGroup {
    pub value: Option<String>,
    pub analyzers: Vec<AnalyzerId>,
}
```

Feature evidence rules:

- `WholeRegion`: emit when every exact-covering analyzer has exactly one morpheme over the region.
- `TokenPosition`: emit when all exact-covering analyzers have the same token count and the same token surface at that position.
- `Surface`: emit only when the surface appears at most once per analyzer in the region and appears in at least two analyzers.

The surface restriction avoids implying token alignment for duplicate surfaces such as repeated `日` inside one region.

### NwayStats

```rust
pub struct NwayStats {
    pub analyzers: usize,
    pub regions: usize,
    pub agreement_regions: usize,
    pub regions_with_feature_disagreement: usize,
    pub regions_with_segmentation_disagreement: usize,
    pub regions_with_coverage_mismatch: usize,
    pub whitespace_regions: usize,
    pub lexical_regions: usize,
    pub unanimous_boundary_count: usize,
    pub variable_boundary_count: usize,
}
```

The `regions_with_*` counts are overlapping by design. A single region can contribute to coverage, segmentation, and feature disagreement counts. Only `agreement_regions` is exclusive: it counts regions with no coverage mismatch, no segmentation disagreement, and no feature disagreement. Therefore `agreement_regions + regions_with_feature_disagreement + regions_with_segmentation_disagreement + regions_with_coverage_mismatch` is not expected to equal `regions`.

Pairwise boundary precision/recall/F1 remains available from pairwise projections. N-way phase 1 adds N-way boundary stability counts; it does not replace pairwise P/R/F1.

## Alignment algorithm

The aligner scans all analyses together by character span:

1. Validate `analyses.len() >= 2`.
2. Validate all analyses have the same `text_id`.
3. Validate all analyses against the shared `source_text`.
4. Maintain one morpheme cursor per analyzer.
5. If all analyzers have a next morpheme and all next morphemes share the same `char_span`, emit that span directly.
6. Otherwise seed a region from the minimum next start and minimum next end across analyzers.
7. Grow the region until every morpheme intersecting the region is consumed and no consumed morpheme extends the region.
8. For each analyzer, collect consumed indices and exact coverage.
9. Derive segmentation groups and feature groups.
10. Emit one shared region.

Pairwise comparison uses this same alignment with two analyses, then projects the resulting shared regions into existing `Region`, `FeatureDiff`, and `ComparisonStats` shapes.

## Feature-key semantics

Pairwise comparison currently uses `feature_context_keys` only for `same_context` evidence; it does not gate which features are compared. N-way needs a separate concept because exact feature-pattern reports often need to restrict the comparison universe.

Use two parameters in N-way APIs:

- `compare_keys`: gates which feature keys are compared. Empty means all observed keys. Non-empty means only those keys.
- `context_keys`: optional extra context keys for projections that need surrounding unchanged evidence. Pairwise keeps its existing `feature_context_keys` meaning.

Do not reuse the name `feature_context_keys` for N-way comparison gating.

## Runner output

Add optional compact output:

```bash
--nway-output scratch/morph-full-corpus-compact-nway/nway.jsonl.zst
```

Phase 1 writes compact N-way rows only. The reason is storage and rendering scope: full-detail N-way rows would duplicate much of the full morpheme data and need a separate inspection UI/report. The existing full pairwise output remains available for targeted debugging.

Each N-way JSONL row includes exact stats and bounded example regions:

```json
{
  "source_id": "...",
  "text_id": "...",
  "source_script_category": "japanese",
  "analyzers": ["vibrato:...", "sudachi-a", "sudachi-c"],
  "stats": {},
  "examples": []
}
```

Examples are selected from `NwayRegion` values after comparison. They are not part of `NwayComparison`. Compact example rows should include only disagreement evidence: omit agreement-only feature groups where `values.len() == 1`.

## Resume semantics

N-way output must be resume-safe.

A source is considered complete for resume only if all requested artifact families for that run contain the source identity:

- analyses output;
- errors output, for failed sources;
- comparisons output, when requested;
- examples output, when requested;
- N-way output, when requested.

Phase 1 should implement this inclusive completeness check. If that proves too large during execution, the fallback is to reject `--resume --nway-output` with a clear error rather than silently skipping missing N-way rows.

## Reports

Add `summarize-nway` over `nway.jsonl(.zst)`. The default grouping is `source-id`, matching existing compact summaries. Use `text-id` only when intentionally grouping duplicate corpus records by logical work.


```bash
ab-morph-run summarize-nway \
  --nway scratch/.../nway.jsonl.zst \
  --group-by source-id \
  --sort-by regions-with-segmentation-disagreement \
  --script-category japanese
```

Add `summarize-nway-patterns` over `nway.jsonl(.zst)`:

```bash
ab-morph-run summarize-nway-patterns \
  --nway scratch/.../nway.jsonl.zst \
  --kind segmentation \
  --script-category japanese

ab-morph-run summarize-nway-patterns \
  --nway scratch/.../nway.jsonl.zst \
  --kind feature \
  --feature-key pos1 \
  --script-category japanese
```

Pattern rows should expose structured fields. A human-readable `display` string is allowed, but it is not the data key. Pattern `examples` counts matching regions, not unique source rows: one N-way row with `K` matching regions contributes `K`.

Canonicalization rules for aggregation:

- Analyzer IDs within a group are sorted lexicographically.
- Segmentation groups are sorted by `(surfaces, analyzers)`.
- Surface order inside `surfaces` is preserved because it is morpheme order.
- Feature value groups are sorted by `(value, analyzers)`, with `None` before `Some`.
- Feature pattern groups are sorted by `(key, scope, values)`.

These rules prevent equivalent N-way partitions from fragmenting into separate buckets.

Segmentation pattern data shape:

```json
{
  "groups": [
    {"surfaces": ["今日"], "analyzers": ["vibrato", "sudachi-a"]},
    {"surfaces": ["今", "日"], "analyzers": ["sudachi-c"]}
  ]
}
```

Feature pattern data shape:

```json
{
  "key": "pos1",
  "scope": {"kind": "whole_region"},
  "values": [
    {"value": "名詞", "analyzers": ["vibrato", "sudachi-a"]},
    {"value": "空白", "analyzers": ["sudachi-c"]}
  ]
}
```

## Error handling

- Fewer than two analyses is a typed error.
- Text ID mismatch is a typed error.
- Source text mismatch is a typed error for APIs that take analysis-owned source text.
- In the runner, analyzer failures remain per-analyzer error rows. N-way output for a source is skipped unless at least two analyzers succeeded.

## Analyzer ID stability

Analyzer IDs in N-way rows are value identities. The runner assumes they are stable within a run and reasonably stable across comparable runs. Versioned IDs such as `vibrato:unidic-cwj-202512` are acceptable and preferred because they make dictionary changes explicit.

## Storage and performance

N-way rows should remain compact:

- no `source_text`;
- no full morpheme feature maps;
- bounded example regions only;
- exact stats and structured pattern groups retained.

The comprehensive reports are exact over emitted N-way rows. If a report needs full local evidence, use targeted rerun.
