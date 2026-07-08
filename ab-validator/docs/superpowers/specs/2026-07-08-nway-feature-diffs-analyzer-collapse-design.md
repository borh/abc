# N-way Feature-Diff Per-Analyzer Row-Collapse — Design (Issue #4)

**Status:** Approved design (2026-07-08). Ready for implementation planning.

**Goal.** Collapse the `nway_feature_diffs` warehouse table from *one row per
(feature disagreement, analyzer)* to *one row per (feature disagreement, distinct
feature value)*, carrying the agreeing analyzers as a sorted `analyzers VARCHAR[]`
column. This is the deferred "#4" lever from
`docs/superpowers/plans/2026-07-08-adjudication-perf-content-neutral.md`.

**Why.** `nway_feature_diffs` is by far the largest warehouse table
(~23.4 B rows on the full corpus, latest hinoki measurement). Each emitted row
is currently one analyzer's contribution to a disagreement; analyzers that agree
on the same value emit separate, near-identical rows. Collapsing to one row per
distinct value — with the analyzers as an array — removes that redundancy at the
source. The row reduction equals the average number of analyzers sharing a value
per value-group (estimated 25–50 % fewer rows), which cuts producer `push_row`
work, warehouse-write CPU, and on-disk parquet size together.

## Decisions (locked)

1. **Clean break, regenerate.** Bump `SCHEMA_VERSION` 2 → 3. The writer emits
   only the v3 (collapsed) layout; no code path reads a v2 `nway_feature_diffs`.
   Existing warehouses are regenerated from corpus. This matches the current
   versioning contract — `sql.rs` only *forward-guards* (rejects
   `runs.schema_version` greater than the reader's max); there is no multi-version
   reader today, so no dual-read path is introduced.

2. **Physical-only (output contract preserved).** The parquet table collapses,
   but the DuckDB base view `warehouse_nway_feature_diffs` `UNNEST`s the array
   back to per-analyzer rows, so `top_feature_differences`, the summary report,
   and any external SQL produce **byte-identical output** to today. The collapse
   is a storage/write optimization, not a semantic API change.

## Current state (what exists today)

- **Producer** — `crates/ab-morph-run/src/warehouse/rows.rs`:
  - `push_region_rows` (production, direct-to-Arrow) at ~`rows.rs:412` iterates
    `for group in &region.feature_groups` (emitting only when
    `group.values.len() >= 2`, i.e. a genuine disagreement), then
    `for value_group in &group.values { for analyzer_id in &value_group.analyzers
    { batch.feature_diffs.push_row(..., analyzer_id.as_str()) } }` — the inner
    loop is the expansion this design collapses.
  - `push_region_rows_reference` (`#[cfg(test)]`, ~`rows.rs:470`) mirrors it into
    `Vec<NwayFeatureDiffRow>` for the differential tests.
  - `region.feature_groups[].values` is `Vec<NwayFeatureValueGroup>`
    (`crates/ab-morph-diff/src/model.rs:342`), each
    `{ value: Option<FeatureValue>, analyzers: Vec<AnalyzerId> }` with the
    `analyzers` vector already sorted ascending (`AnalyzerId = String`,
    `model.rs:9`).
- **Schema / writer** — `crates/ab-warehouse/src/`:
  - `SCHEMA_VERSION = 2` (`schema.rs:4`).
  - `NwayFeatureDiffRow` (`schema.rs:378`) has scalar `analyzer_id: Arc<str>` as
    its 10th field; `column_names(NwayFeatureDiffs)` lists `"analyzer_id"` last
    (`schema.rs:175`).
  - `NwayFeatureDiffsColumns` builder (`writer.rs:773`), `nway_feature_diffs_schema()`
    (`writer.rs:1420`), `append_nway_feature_diffs` (`writer.rs:469`), the
    single-batch writer (~`writer.rs:508`), and the `decode_feature_diff_rows`
    test accessor all use a scalar `StringBuilder` for `analyzer_id` (parquet
    column index 9, 0-based).
- **SQL views** — `crates/ab-warehouse/sql/morph_views.sql` and
  `crates/ab-morph-run/sql/morph_views.sql` (two mirrored copies kept in sync by
  `scripts/sync-schema-mirror.py`):
  - `warehouse_nway_feature_diffs` = `SELECT * FROM read_parquet('__RUN_DIR__/nway_feature_diffs.parquet')`.
  - `top_feature_differences` builds on that view and `GROUP BY … , analyzer_id`.
- **Rust consumers** — `crates/ab-morph-run/src/summary/`:
  - *SQL-string consumers* (`summary_body.rs`, `interesting.rs`): many stanzas
    already compute `list(f.analyzer_id ORDER BY f.analyzer_id) AS analyzers` and
    `count(DISTINCT analyzer_id) > 1` over `nway_feature_diffs` — i.e. they
    re-aggregate the expanded rows back into an analyzer list.
  - *Direct Arrow batch readers* (`summary_body.rs` ~3225/3280/3339/3377): read
    `analyzer_id` as a scalar `string_column(&batch, 9)` (and index 3 in the
    region-analyzer reader) straight from the parquet, bypassing the view.

## Target design

### Schema (v3)

`nway_feature_diffs` keeps 10 columns; only the last changes:

| # | column | v2 type | v3 type |
|---|--------|---------|---------|
| 0–8 | run_id … feature_value | *(unchanged)* | *(unchanged)* |
| 9 | ~~`analyzer_id`~~ → `analyzers` | `Utf8` (scalar) | `List<Utf8>` |

- `analyzers` is a **non-null list of non-null Utf8**, elements **sorted
  ascending** (taken directly from the already-sorted
  `NwayFeatureValueGroup.analyzers`) — deterministic ordering keeps output
  content-stable across shards/runs.
- `feature_value` remains nullable `Option<Utf8>`.
- A row is still emitted only for genuine disagreements (`group.values.len() >= 2`).
- `SCHEMA_VERSION` → 3.
- `NwayFeatureDiffRow` 10th field becomes `analyzers: Vec<Arc<str>>` (or
  `Vec<String>`); `column_names(NwayFeatureDiffs)` last entry → `"analyzers"`.

### Producer

In `push_region_rows`, replace the inner per-analyzer loop with a single push per
value-group:

```rust
for value_group in &group.values {
    batch.feature_diffs.push_row(
        ids.run_id.as_ref(), ids.source_id.as_ref(), ids.text_id.as_ref(),
        region.region_index as u64, group.key.as_ref(),
        scope_type.as_ref(), scope_position, scope_surface.as_deref(),
        value_group.value.as_deref(),
        &value_group.analyzers,          // &[AnalyzerId] -> List<Utf8>
    );
}
```

`NwayFeatureDiffsColumns::push_row`'s final parameter changes from `&str` to
`&[impl AsRef<str>]` (append a `ListBuilder<StringBuilder>` entry). The
`#[cfg(test)]` `push_region_rows_reference` collapses identically, so the
existing "direct-Arrow builder == reference" differential/byte test is retained
against the new shape. `feature_pattern_counts` is fed from
`record_region_feature_group(…, &group.values)` and is **unaffected** by the row
collapse (it never consumed the n-way rows).

### Views (preserve output contract)

Both mirrored `morph_views.sql` copies change the base view to re-expand the
array, so every downstream view/query keeps its exact column set and row shape:

```sql
CREATE OR REPLACE VIEW warehouse_nway_feature_diffs AS
SELECT run_id, source_id, text_id, region_index, feature_key, scope_type,
       scope_position, scope_surface, feature_value, analyzer_id
FROM read_parquet('__RUN_DIR__/nway_feature_diffs.parquet'),
     UNNEST(analyzers) AS t(analyzer_id);
```

`top_feature_differences` and all other view consumers are unchanged — they read
the compatibility view, not the raw parquet. The two copies are updated through
`scripts/sync-schema-mirror.py` (edit the source of truth, run the sync, verify
the mirror matches — the plan's task will pin which file is canonical).

### Rust consumers

- **SQL-string consumers** (`summary_body.rs`, `interesting.rs`): route their
  `FROM` at the unnesting `warehouse_nway_feature_diffs` view (or add an inline
  `UNNEST(analyzers) AS t(analyzer_id)` to their raw-parquet `FROM`). Their
  existing `list(f.analyzer_id …) AS analyzers` / `count(DISTINCT analyzer_id)`
  expressions then produce identical results. No result-shape change.
- **Direct Arrow batch readers** (`summary_body.rs` ~3225–3393): column 9 is now
  a `ListArray`. These readers iterate the list and emit one logical fact per
  element — reproducing the per-analyzer stream they consumed before. This is the
  one place where output can silently drift, so it carries dedicated unit
  coverage (below) and is the focus of the corpus `EXCEPT ALL` check.
- **Test fixtures**: the many `append_nway_feature_diffs(&[NwayFeatureDiffRow{…}])`
  call sites across `summary_body.rs` tests move to the `analyzers: vec![…]`
  shape (mechanical, but broad).

## Unchanged by construction

`feature_pattern_counts`, `nway_region_oracle_evidence`, `morpheme_features`, and
every table other than `nway_feature_diffs` are untouched. The view-layer contract
(`top_feature_differences`, summary report output, external SQL) is preserved.

## Validation — equivalence, not identity

Because the physical schema changes, the content bar is **equivalence under
`UNNEST`**: expanding v3's `analyzers` reproduces v2's per-analyzer multiset
exactly (row *order* may differ under parallel sharding; the *multiset* must not).

1. **Unit tests**
   - Retained-reference differential: direct-Arrow `push_region_rows` output
     decodes (`decode_feature_diff_rows`) equal to `push_region_rows_reference`,
     row-for-row, on a fixture region with (a) a value shared by ≥2 analyzers,
     (b) a value held by exactly 1 analyzer, and (c) a null `feature_value`.
   - **Collapse-equivalence**: `UNNEST`ing a region's collapsed rows equals the
     pre-collapse per-analyzer expansion (construct the expected `(cols…, analyzer)`
     tuples inline from `feature_groups`), asserting element order within
     `analyzers` is ascending.
   - Direct-Arrow reader round-trip: a batch reader over a written v3 parquet
     yields the same per-analyzer facts as the same reader over the equivalent v2
     rows (guards the `summary_body.rs` ListArray readers).
2. **Corpus (hinoki), reusing the `adjperf` harness**
   - Baseline = a pre-#4 commit (v2); branch = #4 (v3), same corpus / analyzers /
     `--jobs 0` / zstd 3.
   - Order-independent fingerprint `(count, sum(hash(*)), bit_xor(hash(*)))` over
     `UNNEST(analyzers)`-expanded v3 vs v2 `nway_feature_diffs` — must match.
   - Confirmatory `EXCEPT ALL` both directions empty on a shard subset (the full
     23 B-row `EXCEPT ALL` is too heavy; sample deterministically).
   - Row-count parity on all *other* tables (must be identical); record the
     `nway_feature_diffs` row-count reduction, plus wall / peak-RSS /
     `phase-timings` deltas as the payoff.

> A fingerprint mismatch on `UNNEST(v3)` vs v2 is a content regression — stop and
> diagnose; it means the collapse or a consumer changed emitted values.

## Risks

- **Direct-Arrow readers** (`summary_body.rs`) are the primary drift risk — a
  missed `UNNEST` there changes report inputs silently. Mitigated by the reader
  round-trip unit test and the corpus `EXCEPT ALL`.
- **Mirror drift** between the two `morph_views.sql` copies — mitigated by driving
  the edit through `scripts/sync-schema-mirror.py` and asserting the mirror.
- **Test-fixture churn** is broad but mechanical; the compiler enforces the struct
  change, so no fixture can be silently missed.

## Task decomposition (for the plan)

1. Schema + writer + `NwayFeatureDiffRow` to the list shape (`SCHEMA_VERSION` → 3,
   `nway_feature_diffs_schema`, `NwayFeatureDiffsColumns`/`push_row`/decode
   accessor), with byte/decode unit tests.
2. Producer collapse in `rows.rs` (`push_region_rows` + reference) with the
   differential and collapse-equivalence tests.
3. Base-view `UNNEST` in both `morph_views.sql` mirrors + `sync-schema-mirror.py`
   run and mirror assertion.
4. SQL-string consumers routed through the unnesting view.
5. Direct-Arrow readers unnest in Rust, with the reader round-trip test; migrate
   the `append_nway_feature_diffs` test fixtures.
6. Corpus validation record (baseline v2 vs branch v3, `UNNEST` equivalence + perf
   deltas).

## Out of scope

- Surfacing `analyzers[]` in views or the summary report (foreclosed by the
  physical-only decision).
- Any change to `feature_pattern_counts`, oracle evidence, or other tables.
- Multi-version / dual-read support (foreclosed by the clean-break decision).
