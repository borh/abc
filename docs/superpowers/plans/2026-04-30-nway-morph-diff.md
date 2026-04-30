# N-way Morph Diff Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add generic true N-way morpheme diffing and exact N-way corpus reporting.

**Architecture:** Make N-way region alignment the primitive. Preserve existing pairwise public APIs and wire formats by projecting the `N = 2` case into existing pairwise types, then add compact N-way JSONL rows and exact N-way reports.

**Tech Stack:** Rust, serde JSONL, zstd streaming output/input, existing `ab-morph-diff`, `ab-morph-run`, `ab-morph-analyzers`, and compact reporting patterns.

---

## File map

- Create: `crates/ab-morph-diff/src/nway.rs`
  - Generic shared region construction, N-way comparison projection, feature grouping, stats.
- Modify: `crates/ab-morph-diff/src/model.rs`
  - Add N-way model structs and structured serde-friendly enums.
- Modify: `crates/ab-morph-diff/src/lib.rs`
  - Export N-way types and `compare_nway_with_source_text`.
- Modify: `crates/ab-morph-diff/src/align.rs`
  - Delegate pairwise alignment to the generic shared-region builder in `nway.rs`; do not keep an independent pairwise span-growth implementation.
- Modify: `crates/ab-morph-diff/src/features.rs`
  - Keep existing pairwise `FeatureDiff` output by projecting from N-way feature groups for `N = 2` where feasible.
- Modify: `crates/ab-morph-diff/src/stats.rs`
  - Keep existing pairwise stats output stable; use shared boundary helpers where possible.
- Create: `crates/ab-morph-run/src/nway.rs`
  - Compact N-way row structs, example-region projection, and row conversion.
- Modify: `crates/ab-morph-run/src/lib.rs`
  - Add `nway_output` and `max_nway_examples_per_text` plumbing through serial and parallel execution.
- Modify: `crates/ab-morph-run/src/main.rs`
  - Add `--nway-output`, `--max-nway-examples-per-text`, `summarize-nway`, and `summarize-nway-patterns`.
- Modify: `crates/ab-morph-run/src/summary.rs`
  - Add N-way summary and pattern aggregators.
- Modify: `docs/morph-corpus-workflow.md`
  - Add `vibrato + sudachi-a + sudachi-c` N-way workflow examples.
- Create: `docs/superpowers/reports/2026-04-30-nway-morph-smoke.md`
  - Record smoke-run commands and representative top patterns.

---

## Task 1: Add N-way model types without example duplication

**Files:**
- Modify: `crates/ab-morph-diff/src/model.rs`
- Modify: `crates/ab-morph-diff/src/lib.rs`

- [ ] **Step 1: Write the failing model test**

Add a test proving the model can represent three analyzers split into two segmentation groups without analyzer indices:

```rust
#[test]
fn nway_model_uses_analyzer_ids_in_segmentation_groups() {
    let group = crate::NwaySegmentationGroup {
        surfaces: vec!["今日".to_owned()],
        analyzers: vec!["vibrato".to_owned(), "sudachi-a".to_owned()],
    };

    assert_eq!(group.analyzers, vec!["vibrato", "sudachi-a"]);
}
```

- [ ] **Step 2: Run the failing test**

```bash
cargo test -p ab-morph-diff nway_model_uses_analyzer_ids_in_segmentation_groups
```

Expected: compile failure because N-way types do not exist.

- [ ] **Step 3: Add N-way model contracts**

Add model types matching the design spec:

- `NwayComparison`
- `NwayRegion`
- `NwayAnalyzerRegion`
- `NwaySegmentationGroup`
- `NwayFeatureGroup`
- `NwayFeatureScope`
- `NwayFeatureValueGroup`
- `NwayStats` with fields named `regions_with_feature_disagreement`, `regions_with_segmentation_disagreement`, and `regions_with_coverage_mismatch`. Use the same snake_case names in JSON rows and kebab-case equivalents in CLI flags.

Do not add `NwayExample` to `ab-morph-diff`. Examples are a runner/report projection over regions.

Do not add a precedence-based `NwayRegionKind` as the core model. Do not add a stored `has_coverage_mismatch` field. If convenience methods are useful, derive them from:

- `per_analyzer.iter().any(|a| !a.covers_exactly)`
- `segmentation_groups.len()`
- `feature_groups[*].values.len()`

- [ ] **Step 4: Use stable serde shapes**

For `NwayFeatureScope`, use serde tags instead of Debug strings:

```rust
#[serde(tag = "kind", rename_all = "snake_case")]
```

Expected serialized examples:

```json
{"kind":"whole_region"}
{"kind":"token_position","position":0}
{"kind":"surface","surface":"今日"}
```

- [ ] **Step 5: Export types**

Update `crates/ab-morph-diff/src/lib.rs` exports.

- [ ] **Step 6: Run the focused test**

```bash
cargo test -p ab-morph-diff nway_model_uses_analyzer_ids_in_segmentation_groups
```

Expected: pass.

- [ ] **Step 7: Commit**

```bash
git add crates/ab-morph-diff/src/model.rs crates/ab-morph-diff/src/lib.rs
git commit -m "feat: add nway morph diff model"
```

---

## Task 2: Add generic shared region alignment

**Files:**
- Create: `crates/ab-morph-diff/src/nway.rs`
- Modify: `crates/ab-morph-diff/src/lib.rs`

- [ ] **Step 1: Write failing three-analyzer alignment test**

Add a test where `vibrato` and `sudachi-a` produce `[今日]`, while `sudachi-c` produces `[今, 日]`. Assert one N-way region, no coverage mismatch, two segmentation groups, and analyzer IDs in those groups.

- [ ] **Step 2: Run the failing test**

```bash
cargo test -p ab-morph-diff compare_nway_groups_three_analyzer_segmentation_partition
```

Expected: compile failure because `compare_nway_with_source_text` is missing.

- [ ] **Step 3: Implement `compare_nway_with_source_text` contract**

Public signature:

```rust
pub fn compare_nway_with_source_text(
    analyses: &[Analysis],
    source_text: &str,
    compare_keys: &[FeatureKey],
    context_keys: &[FeatureKey],
) -> Result<NwayComparison, MorphDiffError>
```

`compare_keys` gates feature comparison. Empty means all observed keys. `context_keys` preserves the pairwise context-key concept for projections that need unchanged evidence. Runner call sites that do not need filtering or context pass `&[]` for both slices.

Rules:

- require at least two analyses;
- require identical `text_id`;
- validate every analysis against `source_text`;
- align by shared source character spans;
- emit `NwayRegion` rows with per-analyzer token coverage;
- derive segmentation groups by identical `surfaces` vectors;
- compare all observed feature keys when `compare_keys` is empty;
- compare only selected keys when `compare_keys` is non-empty;
- do not use `context_keys` to gate feature comparison.

Do not accept `max_examples`; example selection is not part of diff production.

- [ ] **Step 4: Add feature-group tests**

Add tests for:

- whole-region feature disagreement when all analyzers have one token on the same span;
- token-position feature disagreement when all analyzers have the same token surfaces in a segmentation region;
- surface-scoped feature evidence only when a surface appears at most once per analyzer in the region;
- duplicate surface case does not emit surface-scoped evidence.

- [ ] **Step 5: Run focused tests**

```bash
cargo test -p ab-morph-diff compare_nway
```

Expected: all new N-way tests pass.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-morph-diff/src/lib.rs crates/ab-morph-diff/src/nway.rs
git commit -m "feat: compare nway morph analyses"
```

---

## Task 3: Project pairwise comparison from shared N-way alignment

**Files:**
- Modify: `crates/ab-morph-diff/src/align.rs`
- Modify: `crates/ab-morph-diff/src/features.rs`
- Modify: `crates/ab-morph-diff/src/stats.rs`
- Modify: `crates/ab-morph-diff/src/lib.rs`

- [ ] **Step 1: Add golden pairwise behavior tests**

Add tests against the existing public pairwise APIs only. These tests must not call new N-way helpers. Cover:

- one-to-one agreement;
- split;
- merge;
- resegment;
- coverage mismatch;
- one-to-one feature diff;
- compact pairwise examples.

Each test should assert the existing `Comparison` or `CompactComparison` shape, not just stats.

- [ ] **Step 2: Run golden tests before refactor**

```bash
cargo test -p ab-morph-diff pairwise_golden
```

Expected: tests pass on current code before refactoring. These are behavior-preserving safety tests.

- [ ] **Step 3: Implement pairwise projection**

Keep public signatures stable:

```rust
compare_pair(...)
compare_pair_with_source_text(...)
compare_pair_compact_with_source_text(...)
```

Internally, pairwise region construction must use the same shared region builder as N-way. Project `N = 2` regions into existing `Region` values. `align.rs` should delegate to `nway.rs`; it must not retain an independent span-growth algorithm. Preserve `FeatureDiff::same_context` by building it separately from the projected pairwise regions using the existing `feature_context_keys` logic; do not try to recover pairwise context from N-way feature groups.

Phase 1 keeps `compare_pair_compact_with_source_text` example selection inside the existing compact pairwise API for compatibility. N-way example selection is kept outside the core N-way diff API; phase 2 can unify both onto a shared presentation helper.

- [ ] **Step 4: Preserve existing pairwise behavior**

Run:

```bash
cargo test -p ab-morph-diff
```

Expected: all existing and new tests pass. Existing pairwise public output remains stable.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-diff/src/align.rs crates/ab-morph-diff/src/features.rs crates/ab-morph-diff/src/stats.rs crates/ab-morph-diff/src/lib.rs
git commit -m "refactor: derive pairwise morph diff from shared regions"
```

---

## Task 4: Add compact N-way runner output with explicit resume semantics

**Files:**
- Create: `crates/ab-morph-run/src/nway.rs`
- Modify: `crates/ab-morph-run/src/lib.rs`
- Modify: `crates/ab-morph-run/src/main.rs`

- [ ] **Step 1: Write failing CLI parse test**

Add a CLI test for:

```bash
ab-morph-run analyze-aat \
  --aat one.json \
  --analyzer vibrato \
  --analyzer sudachi-a \
  --analyzer sudachi-c \
  --output-profile compact \
  --analyses-output analyses.jsonl.zst \
  --nway-output nway.jsonl.zst \
  --max-nway-examples-per-text 25
```

Assert parsed `nway_output == Some("nway.jsonl.zst")` and `max_nway_examples_per_text == Some(25)`.

- [ ] **Step 2: Run the failing test**

```bash
cargo test -p ab-morph-run parses_nway_output_flags
```

Expected: compile failure because fields do not exist.

- [ ] **Step 3: Add runner row types**

Create `crates/ab-morph-run/src/nway.rs` with compact row types:

- `NwayComparisonRow`
- `NwayExampleRegionRow`
- `NwaySegmentationGroupRow`
- `NwayFeatureGroupRow`
- `NwayFeatureValueGroupRow`

Rows must use analyzer IDs directly. Scope fields must be structured serde data, not `format!("{:?}")`. Example rows must drop agreement-only feature groups where `values.len() == 1`.

- [ ] **Step 4: Select examples outside core diff**

Implement a runner helper:

```rust
fn select_nway_example_regions(comparison: &NwayComparison, max_examples: usize) -> Vec<&NwayRegion>
```

It should select non-agreement regions by derived facts, not a stored `kind` enum.

- [ ] **Step 5: Write serial N-way output**

Thread `nway_output` and `max_nway_examples_per_text` through serial execution. For each source with at least two successful analyses:

- compute `compare_nway_with_source_text(&analyses, &document.text, &[], &[])`;
- project compact row with bounded example regions;
- write to `nway_output`.

If `--nway-output` is used without compact output profile, return a clear error explaining phase-1 storage/rendering scope.

- [ ] **Step 6: Define resume behavior**

Implement one of these two behaviors, with tests:

- Preferred: include `nway_output` in resume completeness checks when requested.
- Acceptable phase-1 fallback: reject `--resume --nway-output` with a clear error.

Do not silently resume from `analyses_output` only when `nway_output` is requested.

- [ ] **Step 7: Add parallel shard plumbing**

Mirror existing shard handling for comparisons/examples:

- shard-local nway path;
- worker receives shard path;
- merge shard nway files into final nway output;
- test `--jobs 2` compact N-way output.

- [ ] **Step 8: Run tests**

```bash
cargo test -p ab-morph-run nway
```

Expected: new CLI, serial, resume, and parallel tests pass.

- [ ] **Step 9: Commit**

```bash
git add crates/ab-morph-run/src/lib.rs crates/ab-morph-run/src/main.rs crates/ab-morph-run/src/nway.rs
git commit -m "feat: emit compact nway morph rows"
```

---

## Task 5: Add exact N-way summary reports

**Files:**
- Modify: `crates/ab-morph-run/src/summary.rs`
- Modify: `crates/ab-morph-run/src/main.rs`
- Modify: `crates/ab-morph-run/src/lib.rs`

- [ ] **Step 1: Write failing summary test**

Create a small `nway.jsonl` fixture with two rows and assert `summarize_nway` groups by source ID, filters `script_category`, and sorts by `regions_with_segmentation_disagreement` descending.

- [ ] **Step 2: Run failing test**

```bash
cargo test -p ab-morph-run summarize_nway_groups_exact_region_counts
```

Expected: compile failure because summary API is missing.

- [ ] **Step 3: Implement `summarize_nway`**

Add:

- `NwaySummaryOptions`
- `NwaySummarySort`
- `NwaySummaryRow`
- `summarize_nway(path, options)`

Use overlapping count names everywhere: `regions_with_feature_disagreement`, `regions_with_segmentation_disagreement`, and `regions_with_coverage_mismatch`. JSON row fields use those exact snake_case names. CLI sort flags use `regions-with-feature-disagreement`, `regions-with-segmentation-disagreement`, and `regions-with-coverage-mismatch`. Do not imply these counts sum to `regions`.

Stream `nway.jsonl(.zst)` with `for_each_jsonl_or_zst_line`; do not materialize the entire file.

- [ ] **Step 4: Add CLI command**

Add `summarize-nway` with:

- `--nway`
- `--group-by source-id|text-id`
- `--sort-by regions-with-segmentation-disagreement|regions-with-feature-disagreement|regions-with-coverage-mismatch|variable-boundary-count`
- `--script-category`
- `--limit`
- `--json`

- [ ] **Step 5: Run tests**

```bash
cargo test -p ab-morph-run summarize_nway
```

Expected: summary and CLI parse tests pass.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-morph-run/src/summary.rs crates/ab-morph-run/src/main.rs crates/ab-morph-run/src/lib.rs
git commit -m "feat: summarize nway morph rows"
```

---

## Task 6: Add structured N-way pattern reports

**Files:**
- Modify: `crates/ab-morph-run/src/summary.rs`
- Modify: `crates/ab-morph-run/src/main.rs`
- Modify: `crates/ab-morph-run/src/lib.rs`

- [ ] **Step 1: Write failing segmentation-pattern test**

Create a `nway.jsonl` fixture where two rows have the same structured segmentation groups:

```json
[
  {"surfaces":["今日"],"analyzers":["vibrato","sudachi-a"]},
  {"surfaces":["今","日"],"analyzers":["sudachi-c"]}
]
```

Assert one aggregated pattern with `examples == 2`.

- [ ] **Step 2: Write failing feature-pattern test**

Create a fixture with a structured feature group:

```json
{
  "key":"pos1",
  "scope":{"kind":"whole_region"},
  "values":[
    {"value":"名詞","analyzers":["vibrato","sudachi-a"]},
    {"value":"空白","analyzers":["sudachi-c"]}
  ]
}
```

Assert filtering with `--feature-key pos1` aggregates it.

- [ ] **Step 3: Run failing tests**

```bash
cargo test -p ab-morph-run summarize_nway_patterns
```

Expected: compile failure because pattern APIs are missing.

- [ ] **Step 4: Implement structured pattern aggregation**

Add:

- `NwayPatternOptions`
- `NwayPatternKind`
- `NwayPatternRow`
- `summarize_nway_patterns(path, options)`

Pattern `examples` counts matching regions across N-way rows. If one row contains five matching regions, it contributes five. Default grouping for `summarize-nway` is `source-id`, matching existing compact summaries. Use `text-id` only when intentionally grouping duplicate corpus records by logical work.

Use structured canonical keys internally. Human-readable display strings are allowed in table output but must not be the only data representation. Canonicalization rules:

- sort analyzer IDs within groups;
- sort segmentation groups by `(surfaces, analyzers)`;
- preserve surface order inside each surfaces vector;
- sort feature value groups by `(value, analyzers)`, with `None` before `Some`;
- sort feature patterns by `(key, scope, values)`.

- [ ] **Step 5: Add CLI command**

Add `summarize-nway-patterns` with:

- `--nway`
- `--kind segmentation|feature`
- `--feature-key`
- `--script-category`
- `--limit`
- `--json`

- [ ] **Step 6: Run tests**

```bash
cargo test -p ab-morph-run summarize_nway_patterns
```

Expected: pass.

- [ ] **Step 7: Commit**

```bash
git add crates/ab-morph-run/src/summary.rs crates/ab-morph-run/src/main.rs crates/ab-morph-run/src/lib.rs
git commit -m "feat: summarize nway morph patterns"
```

---

## Task 7: Workflow and smoke run

**Files:**
- Modify: `docs/morph-corpus-workflow.md`
- Create: `docs/superpowers/reports/2026-04-30-nway-morph-smoke.md`

- [ ] **Step 1: Update workflow commands**

Add a compact N-way run using `vibrato`, `sudachi-a`, and `sudachi-c`:

```bash
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  target/release/ab-morph-run analyze-aat \
    --aat-dir scratch/morph-full-corpus/aats \
    --analyzer vibrato \
    --analyzer sudachi-a \
    --analyzer sudachi-c \
    --output-profile compact \
    --analyses-output scratch/morph-full-corpus-compact-nway/analyses.jsonl.zst \
    --comparisons-output scratch/morph-full-corpus-compact-nway/comparisons.jsonl.zst \
    --examples-output scratch/morph-full-corpus-compact-nway/examples.jsonl.zst \
    --nway-output scratch/morph-full-corpus-compact-nway/nway.jsonl.zst \
    --errors-output scratch/morph-full-corpus-compact-nway/errors.jsonl.zst \
    --manifest-output scratch/morph-full-corpus-compact-nway/manifest.json \
    --jobs 8 \
    --progress-interval-seconds 30
```

- [ ] **Step 2: Add report examples**

Document:

```bash
target/release/ab-morph-run summarize-nway \
  --nway scratch/morph-full-corpus-compact-nway/nway.jsonl.zst \
  --script-category japanese \
  --sort-by regions-with-segmentation-disagreement \
  --limit 25

target/release/ab-morph-run summarize-nway-patterns \
  --nway scratch/morph-full-corpus-compact-nway/nway.jsonl.zst \
  --kind segmentation \
  --script-category japanese \
  --limit 25

target/release/ab-morph-run summarize-nway-patterns \
  --nway scratch/morph-full-corpus-compact-nway/nway.jsonl.zst \
  --kind feature \
  --feature-key pos1 \
  --script-category japanese \
  --limit 25
```

- [ ] **Step 3: Run smoke over 10 AAT files**

Generate a small input dir with symlinks, then run `analyze-aat` with `--nway-output`, `--jobs 2`, and all three analyzers.

- [ ] **Step 4: Generate smoke summaries**

Run `summarize-nway` and `summarize-nway-patterns` for segmentation and `pos1`.

- [ ] **Step 5: Write smoke report**

Create `docs/superpowers/reports/<current-date>-nway-morph-smoke.md` using the actual execution date, for example `docs/superpowers/reports/2026-04-30-nway-morph-smoke.md`. Include commands, artifact sizes, error row count, N-way row count, top segmentation patterns, and top `pos1` patterns.

- [ ] **Step 6: Final verification**

```bash
cargo test -p ab-morph-diff
cargo test -p ab-morph-run
cargo fmt --all -- --check
git diff --check
```

Expected: all pass.

- [ ] **Step 7: Commit**

```bash
git add docs/morph-corpus-workflow.md docs/superpowers/reports/2026-04-30-nway-morph-smoke.md
git commit -m "docs: add nway morph workflow"
```

---

## Self-review

Spec coverage:

- N-way as primitive: Tasks 2 and 3.
- Pairwise as `N = 2` projection while preserving API: Task 3.
- No `NwayExample` duplication in core: Tasks 1 and 4.
- Analyzer IDs instead of positional indices: Tasks 1, 4, 6.
- Orthogonal coverage/segmentation/feature facts: Tasks 1 and 2.
- Resume semantics: Task 4.
- Surface-scope duplicate restriction: Task 2.
- Structured wire/report keys: Tasks 1, 4, 6.
- Generic N analyzers and `vibrato + sudachi-a + sudachi-c` workflow: Task 7.

Known risks:

- Refactoring pairwise projection from shared regions is the highest-risk task because it must preserve existing pairwise output exactly.
- Full phase-1 N-way reporting remains compact; detailed inspection still depends on targeted reruns.
- N-way boundary stats intentionally complement, not replace, pairwise precision/recall/F1.
