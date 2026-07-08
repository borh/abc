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
- **Schema / writer / DDL** — `crates/ab-warehouse/src/` + `crates/ab-warehouse/sql/`:
  - `SCHEMA_VERSION = 2` (`schema.rs:4`).
  - `NwayFeatureDiffRow` (`schema.rs:378`) has scalar `analyzer_id: Arc<str>` as
    its 10th field; `column_names(NwayFeatureDiffs)` lists `"analyzer_id"` last
    (`schema.rs:175`).
  - `NwayFeatureDiffsColumns` builder (`writer.rs:773`), `nway_feature_diffs_schema()`
    (`writer.rs:1420`), `append_nway_feature_diffs` (`writer.rs:469`), the
    single-batch writer (~`writer.rs:508`), and the `decode_feature_diff_rows`
    test accessor all use a scalar `StringBuilder` for `analyzer_id` (parquet
    column index 9, 0-based).
  - **DDL** `crates/ab-warehouse/sql/schema.sql` `CREATE TABLE nway_feature_diffs`
    (~line 140) declares `analyzer_id VARCHAR` (line 150). A test —
    `schema_sql_columns_match_documented_parquet_columns` (`sql.rs:123`) —
    asserts `schema.sql` column names equal `WarehouseTable::column_names()`, so
    the DDL is a hard-coupled surface, not optional.
  - **Reader version gates:** `sql.rs` forward-guards on
    `crate::schema::SCHEMA_VERSION` (its human-readable reject message hardcodes
    "(2)", `sql.rs:109`); `interesting.rs` caps at
    `READER_MAX_SCHEMA_VERSION = 2` (`interesting.rs:51`). Snapshot schema
    contracts (recently refreshed in-tree) also pin the version/columns.
- **SQL views** — the **canonical, runtime-loaded** file is
  `crates/ab-warehouse/sql/morph_views.sql`, compiled via
  `MORPH_VIEWS_SQL_TEMPLATE = include_str!("../sql/morph_views.sql")`
  (`sql.rs:9`). `crates/ab-morph-run/sql/morph_views.sql` is a **stale, unused**
  copy (still `schema_version = 1`, missing several views, referenced from no
  Rust code) — it is NOT a mirror and is NOT kept in sync by any tooling.
  - `warehouse_nway_feature_diffs` = `SELECT * FROM read_parquet('__RUN_DIR__/nway_feature_diffs.parquet')`.
  - `top_feature_differences` builds on that view and `GROUP BY … , analyzer_id`.
- **Rust consumers** — `crates/ab-morph-run/src/summary/`:
  - *View-backed SQL* (`top_feature_differences` etc.): reach `nway_feature_diffs`
    only through `warehouse_nway_feature_diffs`.
  - *Standalone SQL builders reading raw parquet directly* — `summary_body.rs`
    (`list(f.analyzer_id …) AS analyzers` at ~`:1932`; the concat/fingerprint
    query at ~`:2158`) and `interesting_sql.rs` (`{analyzer_flags}` over
    `FROM read_parquet({features}) f` at ~`:401`). These generate COPY/SELECT
    text that does **not** load `MORPH_VIEWS_SQL_TEMPLATE`, so the compatibility
    view cannot reach them — they reference `f.analyzer_id` on the raw parquet.
  - *Reference accumulator API* — `WarehouseFeaturePatternAccumulator::record`
    (`lib.rs:588`) and its helper `warehouse_feature_pattern_from_rows`
    (`lib.rs:830`) consume a `&[NwayFeatureDiffRow]` slice and group by
    `fact.analyzer_id` into `BTreeMap<Option<Arc<str>>, Vec<Arc<str>>>`. This is
    the still-`pub`, test/bench-only reference (production feeds pattern counts
    via `record_region_feature_group`), driven by the
    `warehouse_feature_pattern_accumulator` micro-bench and the
    `feature_pattern_accumulator_linear_scan_matches_reference_grouping` test.
  - *Direct Arrow batch reader* — exactly one nway reader:
    `read_warehouse_feature_diffs` (`summary_body.rs:3363`) reads the scalar
    `analyzer_id` at `string_column(&batch, 9)` (line 3377) and yields a
    per-analyzer `WarehouseFeatureDiffFact` stream. (The other scalar
    `analyzer_id` reads nearby — `:3225`, `:3280`, `:3339` — serve `Errors`,
    `Morphemes`, and `NwayRegionAnalyzers`, not `nway_feature_diffs`, and do not
    change.)

## Target design

### Schema (v3)

`nway_feature_diffs` keeps 10 columns; only the last changes:

| # | column | v2 type | v3 type |
|---|--------|---------|---------|
| 0–8 | run_id … feature_value | *(unchanged)* | *(unchanged)* |
| 9 | ~~`analyzer_id`~~ → `analyzers` | `Utf8` (scalar) | `List<Utf8>` |

- `analyzers` is a **non-null list**; its element order is **sorted ascending**
  (taken directly from the already-sorted `NwayFeatureValueGroup.analyzers`) and
  every element is non-null with ≥1 element — a *producer invariant*, asserted by
  test. On the Arrow side the child item stays nullable: the existing
  `utf8_list(name)` helper (`writer.rs:1278`) builds `List<Field("item", Utf8,
  nullable=true)>`, list itself non-null. Reuse it (avoids a new Arrow helper);
  DuckDB reads it as `VARCHAR[]`. The "no null / non-empty / ascending" guarantees
  are enforced by the producer + a unit test, not by the Arrow field nullability.
- `feature_value` remains nullable `Option<Utf8>`.
- A row is still emitted only for genuine disagreements (`group.values.len() >= 2`).
- **Version + DDL surface (all must move together):**
  - `SCHEMA_VERSION` 2 → 3 (`schema.rs:4`).
  - `NwayFeatureDiffRow` 10th field → `analyzers: Vec<Arc<str>>`;
    `column_names(NwayFeatureDiffs)` last entry → `"analyzers"`.
  - `nway_feature_diffs_schema()` (`writer.rs:1420`) field → `utf8_list("analyzers")`;
    `NwayFeatureDiffsColumns`/`push_row`/single-batch-writer/`decode_feature_diff_rows`
    to the list builder.
  - **`crates/ab-warehouse/sql/schema.sql`**: `analyzer_id VARCHAR` →
    `analyzers VARCHAR[]` in `CREATE TABLE nway_feature_diffs` (required — the
    `sql.rs:123` test couples DDL to `column_names()`).
  - **Reader gates:** `READER_MAX_SCHEMA_VERSION` 2 → 3 (`interesting.rs:51`);
    update the `sql.rs:109` reject-message literal to "(3)"; refresh the pinned
    snapshot schema contracts.

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

The **canonical** `crates/ab-warehouse/sql/morph_views.sql` (the one compiled via
`include_str!`) changes its base view to re-expand the array, so every downstream
view/query keeps its exact column set and row shape:

```sql
CREATE OR REPLACE VIEW warehouse_nway_feature_diffs AS
SELECT run_id, source_id, text_id, region_index, feature_key, scope_type,
       scope_position, scope_surface, feature_value, analyzer_id
FROM read_parquet('__RUN_DIR__/nway_feature_diffs.parquet'),
     UNNEST(analyzers) AS t(analyzer_id);
```

`top_feature_differences` and other view-backed consumers are unchanged — they
read this compatibility view, not raw parquet. The stale
`crates/ab-morph-run/sql/morph_views.sql` copy is unused at runtime; the plan
either updates it for hygiene or explicitly leaves it, but **no** sync script
governs it (there is no SQL mirror tooling — `scripts/sync-schema-mirror.py`
handles ABC JSON schema contracts only).

> **The view is not enough by itself.** The standalone SQL builders in
> §"Rust consumers" read raw parquet directly and never load this template, so
> the collapse also needs an inline compatibility layer for them.

### Rust consumers

- **Standalone raw-parquet SQL builders** — `summary_body.rs` (`:1932`, `:2158`)
  and `interesting_sql.rs` (`:401`) emit generated SQL that does
  `FROM read_parquet({features})` and references `f.analyzer_id` directly. The
  compatibility view cannot reach them. **Introduce one shared inline
  compatibility fragment** — a helper that renders the raw-parquet source as an
  UNNEST-expanded relation, e.g. a `WITH nway_feature_diffs_expanded AS (SELECT …,
  UNNEST(analyzers) AS analyzer_id FROM read_parquet({features}))` CTE (or a
  `(SELECT … , UNNEST(analyzers) AS analyzer_id FROM …)` subquery) — and route
  every raw-parquet builder through it. Their existing
  `list(f.analyzer_id …) AS analyzers`, `count(DISTINCT analyzer_id)`, and
  `f.analyzer_id || …` expressions then produce identical results. DRY: define the
  fragment once, not per call site.
- **Direct Arrow batch reader** — only `read_warehouse_feature_diffs`
  (`summary_body.rs:3363`) changes: column 9 is now a `ListArray`. It iterates the
  list and emits one `WarehouseFeatureDiffFact` per element, reproducing the
  per-analyzer stream its callers consume. This is the one Rust drift point, so it
  carries the reader round-trip unit test (below) and is a focus of the corpus
  `EXCEPT ALL`. (The `Errors`/`Morphemes`/`NwayRegionAnalyzers` scalar readers are
  untouched.)
- **Reference accumulator API (`WarehouseFeaturePatternAccumulator::record`)** —
  `record` and `warehouse_feature_pattern_from_rows` consume per-analyzer
  `NwayFeatureDiffRow`s. When the struct collapses, adapt them to the collapsed
  shape: each row already carries `feature_value → analyzers`, so
  `warehouse_feature_pattern_from_rows` builds its `by_value` map by inserting the
  row's `analyzers` directly instead of accumulating one analyzer per fact. Update
  the `warehouse_feature_pattern_accumulator` micro-bench and the
  `feature_pattern_accumulator_linear_scan_matches_reference_grouping` test
  fixtures to the collapsed rows. (This reference is already production-decoupled —
  production uses `record_region_feature_group` — so no behavior change ships;
  only the test/bench oracle moves.)
- **Test fixtures**: the many `append_nway_feature_diffs(&[NwayFeatureDiffRow{…}])`
  call sites across `summary_body.rs`/`lib.rs` tests move to the
  `analyzers: vec![…]` shape (mechanical; the compiler enforces completeness).

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
   - **List-invariant test**: the written `analyzers` field is `List<Utf8>` with
     the list non-null, and every emitted list is non-empty, null-free, and sorted
     ascending (the producer invariant the Arrow child-nullability does not
     enforce).
   - Direct-Arrow reader round-trip: `read_warehouse_feature_diffs` over a written
     v3 parquet yields the same `WarehouseFeatureDiffFact` stream as the
     equivalent v2 rows.
   - **Accumulator parity**: the adapted `WarehouseFeaturePatternAccumulator::record`
     over collapsed rows produces the same pattern strings as the pre-collapse
     per-analyzer path (keeps the reference oracle meaningful).
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

- **Raw-parquet SQL builders** (`summary_body.rs`, `interesting_sql.rs`) are the
  primary drift risk: they bypass the view and reference `f.analyzer_id` directly,
  so a missed site produces a *hard SQL error* (column gone) rather than silent
  drift — but a subtly wrong inline UNNEST could change results. Mitigated by the
  single shared compatibility fragment + the corpus `EXCEPT ALL`.
- **The one direct-Arrow reader** (`read_warehouse_feature_diffs`) is where output
  could drift silently; mitigated by the reader round-trip test.
- **DDL / version-gate omissions** would fail loudly — `sql.rs:123` breaks if
  `schema.sql` is not updated; a reader-max cap left at 2 would reject v3 runs.
  Both are explicit tasks below.
- **Test-fixture churn** is broad but mechanical; the compiler enforces the struct
  change, so no fixture can be silently missed.

## Task decomposition (for the plan)

1. **Schema/writer/DDL/versioning** — `SCHEMA_VERSION` → 3;
   `NwayFeatureDiffRow.analyzers` + `column_names`; `nway_feature_diffs_schema`
   via `utf8_list`; `NwayFeatureDiffsColumns`/`push_row`/single-batch-writer/
   `decode_feature_diff_rows`; `schema.sql` `analyzers VARCHAR[]`;
   `READER_MAX_SCHEMA_VERSION` → 3 + `sql.rs` reject-message + snapshot schema
   contracts. Tests: byte/decode round-trip, `schema_sql_columns_match…`, the
   List-invariant test.
2. **Producer collapse** — `rows.rs` `push_region_rows` + `push_region_rows_reference`
   to one row per value-group; differential (direct == reference) +
   collapse-equivalence (UNNEST == pre-collapse expansion) tests.
3. **Reference accumulator** — adapt `WarehouseFeaturePatternAccumulator::record` +
   `warehouse_feature_pattern_from_rows` to collapsed rows; update the micro-bench
   and `feature_pattern_accumulator_linear_scan_matches_reference_grouping`;
   accumulator-parity test.
4. **Canonical view** — base-view `UNNEST` in `crates/ab-warehouse/sql/morph_views.sql`;
   assert `top_feature_differences` output unchanged; handle the stale
   `ab-morph-run/sql/morph_views.sql` copy (update or leave, no fake sync).
5. **Standalone SQL builders** — one shared inline UNNEST compatibility fragment,
   routed through `summary_body.rs` (`:1932`, `:2158`) and `interesting_sql.rs`
   (`:401`); assert generated output unchanged.
6. **Direct-Arrow reader + fixtures** — `read_warehouse_feature_diffs` unnests the
   `ListArray`; reader round-trip test; migrate all `append_nway_feature_diffs`
   fixtures to the collapsed shape.
7. **Corpus validation record** — baseline v2 vs branch v3 on hinoki:
   `UNNEST(v3)` fingerprint == v2, `EXCEPT ALL` empty (shard subset), other-table
   row-count parity, plus `nway_feature_diffs` row reduction + wall/RSS/phase-timings.

## Out of scope

- Surfacing `analyzers[]` in views or the summary report (foreclosed by the
  physical-only decision).
- Any change to `feature_pattern_counts`, oracle evidence, or other tables.
- Multi-version / dual-read support (foreclosed by the clean-break decision).
