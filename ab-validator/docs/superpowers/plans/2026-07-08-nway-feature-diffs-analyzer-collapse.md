# N-way Feature-Diff Per-Analyzer Row-Collapse Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Collapse the `nway_feature_diffs` warehouse table from one row per (feature disagreement, analyzer) to one row per (disagreement, distinct feature value), carrying the agreeing analyzers as a sorted `analyzers VARCHAR[]` column — cutting the largest warehouse table (~23.4 B rows) by 25–50 % while keeping all downstream output byte-identical.

**Architecture:** The parquet column `analyzer_id: Utf8` (index 9) becomes `analyzers: List<Utf8>`. The producer (`push_region_rows`) writes one list-valued row per value-group instead of expanding per analyzer. Every reader re-expands via `UNNEST` (SQL) or list iteration (Rust) so `top_feature_differences`, the summary report, and external SQL are unchanged. This is a clean-break schema bump (`SCHEMA_VERSION` 2 → 3); existing warehouses are regenerated.

**Tech Stack:** Rust (workspace `ab-validator`), Arrow/Parquet (`arrow_array`, `parquet`), DuckDB SQL views, `cargo test`, hinoki for corpus validation.

**Spec:** `docs/superpowers/specs/2026-07-08-nway-feature-diffs-analyzer-collapse-design.md`

## Global Constraints

- **Schema version:** `SCHEMA_VERSION` 2 → 3 (`crates/ab-warehouse/src/schema.rs:4`). Clean break — no code reads a v2 `nway_feature_diffs`; existing warehouses regenerate.
- **Column contract:** table keeps 10 columns; only column 9 changes: `analyzer_id: Utf8` → `analyzers: List<Utf8>`. List is non-null; elements are non-null, non-empty (≥1), and **sorted ascending** — a producer invariant enforced by test, NOT by Arrow child-nullability (reuse `utf8_list`, whose child item stays `nullable=true`).
- **Output contract (physical-only):** `warehouse_nway_feature_diffs` and every raw-parquet SQL builder re-expand `analyzers` so all query/report output is byte-identical to today. Do not surface `analyzers[]` in any view, report, or external contract.
- **Validation bar:** equivalence under `UNNEST` — expanding v3's `analyzers` reproduces v2's per-analyzer multiset exactly (row order may differ; the multiset must not).
- **Canonical SQL file:** `crates/ab-warehouse/sql/morph_views.sql` (compiled via `include_str!` at `sql.rs:9`) is the only runtime-loaded view file. `crates/ab-morph-run/sql/morph_views.sql` is stale/unused; no sync tooling governs SQL (`scripts/sync-schema-mirror.py` handles ABC JSON only).
- **DDL coupling:** `crates/ab-warehouse/sql/schema.sql` must match `WarehouseTable::column_names()` — enforced by `schema_sql_columns_match_documented_parquet_columns` (`sql.rs:123`).
- **Cross-crate breaking change:** Tasks 1 and 2 are a coupled pair. Task 1 changes the `NwayFeatureDiffRow` type in the lower crate `ab-warehouse`; its gate is `cargo test -p ab-warehouse`. The **workspace does not build again until Task 2 completes** — this is expected, not a regression. Do not attempt a workspace build as Task 1's gate.
- **Unchanged by construction:** `feature_pattern_counts`, `nway_region_oracle_evidence`, `morpheme_features`, and all other tables. Do not touch their schemas or producers.

---

### Task 1: `ab-warehouse` — v3 physical layout (`analyzers List<Utf8>`)

**Files:**
- Modify: `crates/ab-warehouse/src/schema.rs` (`SCHEMA_VERSION`, `NwayFeatureDiffRow`, `column_names`)
- Modify: `crates/ab-warehouse/src/writer.rs` (`nway_feature_diffs_schema`, `NwayFeatureDiffsColumns`, `append_nway_feature_diffs`, `string_list_array`; tests)
- Modify: `crates/ab-warehouse/sql/schema.sql` (nway DDL)
- Modify: `crates/ab-warehouse/src/sql.rs` (reject-message literal)
- Test: existing `nway_feature_diffs_direct_builder_matches_reference_bytes` and `schema_sql_columns_match_documented_parquet_columns` (both in `ab-warehouse`), plus a new list-invariant test.

**Interfaces:**
- Produces (consumed by Task 2):
  - `NwayFeatureDiffRow { …, analyzers: Vec<Arc<str>> }` (10th field replaces `analyzer_id: Arc<str>`).
  - `NwayFeatureDiffsColumns::push_row(run_id: &str, source_id: &str, text_id: &str, region_index: u64, feature_key: &str, scope_type: &str, scope_position: Option<u64>, scope_surface: Option<&str>, feature_value: Option<&str>, analyzers: &[impl AsRef<str>])`.
  - `WarehouseTable::column_names(NwayFeatureDiffs)` ends with `"analyzers"`.
  - `SCHEMA_VERSION == 3`.

- [ ] **Step 1: Update the byte-identity test fixtures to the list shape (make it fail to compile first)**

In `crates/ab-warehouse/src/writer.rs` tests, find `nway_feature_diffs_direct_builder_matches_reference_bytes`. It builds `NwayFeatureDiffRow` literals and calls both `append_nway_feature_diffs` (reference) and `NwayFeatureDiffsColumns::push_row` (direct), then asserts the written parquet bytes are equal. Change every `analyzer_id: "…".into()` to `analyzers: vec!["…".into()]` (single-element for existing cases) and add one row whose value is shared by two analyzers, e.g.:

```rust
NwayFeatureDiffRow {
    run_id: "run".into(), source_id: "s".into(), text_id: "t".into(),
    region_index: 0, feature_key: "pos".into(), scope_type: "whole_region".into(),
    scope_position: None, scope_surface: None, feature_value: Some("名詞".into()),
    analyzers: vec!["sudachi-c".into(), "vibrato".into()], // sorted ascending
},
```
Update the matching `push_row(…)` call's final argument from `"analyzer"` to a slice: `&["sudachi-c", "vibrato"]`.

- [ ] **Step 2: Run the test — expect a compile error**

Run: `cargo test -p ab-warehouse nway_feature_diffs_direct_builder_matches_reference_bytes 2>&1 | head -30`
Expected: FAIL to compile — `no field \`analyzers\` on type NwayFeatureDiffRow` / `push_row` arity or type mismatch.

- [ ] **Step 3: Change the `NwayFeatureDiffRow` struct and column names**

In `crates/ab-warehouse/src/schema.rs`: bump the version and retype the field.

```rust
pub const SCHEMA_VERSION: u32 = 3;   // was 2
```
```rust
pub struct NwayFeatureDiffRow {
    pub run_id: Arc<str>,
    pub source_id: Arc<str>,
    pub text_id: Arc<str>,
    pub region_index: u64,
    pub feature_key: Arc<str>,
    pub scope_type: Arc<str>,
    pub scope_position: Option<u64>,
    pub scope_surface: Option<Arc<str>>,
    pub feature_value: Option<Arc<str>>,
    pub analyzers: Vec<Arc<str>>,   // was analyzer_id: Arc<str>
}
```
In the same file's `column_names` match arm for `Self::NwayFeatureDiffs`, change the last entry `"analyzer_id"` to `"analyzers"`.

- [ ] **Step 4: Make `string_list_array` accept `&[Arc<str>]` and switch the Arrow schema field**

In `crates/ab-warehouse/src/writer.rs`, generalize the existing helper (currently `fn string_list_array<'a>(values: impl Iterator<Item = &'a [String]>)`) so it also accepts `&[Arc<str>]`:

```rust
fn string_list_array<'a, S: AsRef<str> + 'a>(values: impl Iterator<Item = &'a [S]>) -> ArrayRef {
    let mut builder = ListBuilder::new(StringBuilder::new());
    for list in values {
        for value in list {
            builder.values().append_value(value.as_ref());
        }
        builder.append(true);
    }
    Arc::new(builder.finish())
}
```
(The existing `losing_analyzers`/`surfaces` callers pass `&[String]`, which still satisfies `S: AsRef<str>`.)

In `nway_feature_diffs_schema()`, replace the last field `utf8("analyzer_id", false)` with `utf8_list("analyzers")`.

- [ ] **Step 5: Switch the row-based writer and the direct column builder to the list column**

In `append_nway_feature_diffs` (`writer.rs:469`), change the final array in the `vec![…]` from
`string_array(rows.iter().map(|row| row.analyzer_id.as_ref()))` to
`string_list_array(rows.iter().map(|row| row.analyzers.as_slice()))`.

In `NwayFeatureDiffsColumns` (`writer.rs:773`): change the field, constructor, `push_row`, and `finish`:

```rust
pub struct NwayFeatureDiffsColumns {
    run_id: StringBuilder,
    source_id: StringBuilder,
    text_id: StringBuilder,
    region_index: UInt64Builder,
    feature_key: StringBuilder,
    scope_type: StringBuilder,
    scope_position: UInt64Builder,
    scope_surface: StringBuilder,
    feature_value: StringBuilder,
    analyzers: ListBuilder<StringBuilder>,   // was analyzer_id: StringBuilder
}
```
In `new()`, replace `analyzer_id: StringBuilder::new(),` with `analyzers: ListBuilder::new(StringBuilder::new()),`.

In `push_row`, change the final parameter and body:
```rust
    #[allow(clippy::too_many_arguments)]
    pub fn push_row(
        &mut self,
        run_id: &str,
        source_id: &str,
        text_id: &str,
        region_index: u64,
        feature_key: &str,
        scope_type: &str,
        scope_position: Option<u64>,
        scope_surface: Option<&str>,
        feature_value: Option<&str>,
        analyzers: &[impl AsRef<str>],
    ) {
        self.run_id.append_value(run_id);
        self.source_id.append_value(source_id);
        self.text_id.append_value(text_id);
        self.region_index.append_value(region_index);
        self.feature_key.append_value(feature_key);
        self.scope_type.append_value(scope_type);
        self.scope_position.append_option(scope_position);
        self.scope_surface.append_option(scope_surface);
        self.feature_value.append_option(feature_value);
        for analyzer in analyzers {
            self.analyzers.values().append_value(analyzer.as_ref());
        }
        self.analyzers.append(true);
    }
```
In `finish()`, replace `Arc::new(self.analyzer_id.finish())` with `Arc::new(self.analyzers.finish())`.

- [ ] **Step 6: Update the DDL and the reject-message literal**

In `crates/ab-warehouse/sql/schema.sql`, in `CREATE TABLE nway_feature_diffs (…)`, change the last column line `analyzer_id VARCHAR` to `analyzers VARCHAR[]`.

In `crates/ab-warehouse/src/sql.rs`, update the human-readable reject message (`sql.rs:109`) so its parenthetical maximum reads `(3)` instead of `(2)` (the `WHERE schema_version <= {SCHEMA_VERSION}` clause already interpolates the const and needs no edit).

- [ ] **Step 7: Run the byte-identity and schema-DDL tests — expect PASS**

Run: `cargo test -p ab-warehouse nway_feature_diffs_direct_builder_matches_reference_bytes schema_sql_columns_match_documented_parquet_columns 2>&1 | tail -20`
Expected: PASS (2 tests). If `schema_sql_columns…` fails, the DDL edit (Step 6) or `column_names` edit (Step 3) is wrong.

- [ ] **Step 8: Add the list-invariant test**

Add to `crates/ab-warehouse/src/writer.rs` tests:

```rust
#[test]
fn nway_feature_diffs_analyzers_is_non_null_sorted_list() {
    use arrow_array::{Array, ListArray};
    let mut columns = NwayFeatureDiffsColumns::new();
    columns.push_row("r", "s", "t", 0, "pos", "whole_region", None, None,
                     Some("名詞"), &["sudachi-c", "vibrato"]);
    let batch = columns.finish();
    // Field: List<Utf8>, list itself non-null.
    let field = batch.schema().field(9).clone();
    assert_eq!(field.name(), "analyzers");
    assert!(!field.is_nullable(), "analyzers list column must be non-null");
    let list = batch.column(9).as_any().downcast_ref::<ListArray>().unwrap();
    assert!(!list.is_null(0), "no null list entries");
    let values = list.value(0);
    let strs = values.as_any().downcast_ref::<arrow_array::StringArray>().unwrap();
    let got: Vec<&str> = (0..strs.len()).map(|i| strs.value(i)).collect();
    assert_eq!(got, vec!["sudachi-c", "vibrato"], "elements preserved in ascending order, non-empty");
}
```

- [ ] **Step 9: Run the new test — expect PASS**

Run: `cargo test -p ab-warehouse nway_feature_diffs_analyzers_is_non_null_sorted_list -v 2>&1 | tail -15`
Expected: PASS.

- [ ] **Step 10: Run the full `ab-warehouse` crate test suite**

Run: `cargo test -p ab-warehouse 2>&1 | tail -15`
Expected: PASS (0 failures). Do NOT run a workspace build — `ab-morph-run` will not compile until Task 2 (expected, per Global Constraints).

- [ ] **Step 11: Commit**

```bash
git add crates/ab-warehouse/src/schema.rs crates/ab-warehouse/src/writer.rs \
        crates/ab-warehouse/sql/schema.sql crates/ab-warehouse/src/sql.rs
git commit -m "feat(warehouse)!: nway_feature_diffs analyzers VARCHAR[] column (schema v3)"
```

---

### Task 2: `ab-morph-run` — collapse producer + re-expand all consumers

**Files:**
- Modify: `crates/ab-morph-run/src/warehouse/rows.rs` (`push_region_rows`, `push_region_rows_reference`, `decode_feature_diff_rows`)
- Modify: `crates/ab-morph-run/src/lib.rs` (`warehouse_feature_pattern_from_rows`; `NwayFeatureDiffRow` test fixtures)
- Modify: `crates/ab-morph-run/src/summary/summary_body.rs` (`read_warehouse_feature_diffs`, the two raw-parquet SQL builders, fixtures)
- Modify: `crates/ab-morph-run/src/summary/interesting_sql.rs` (raw-parquet SQL builder)
- Modify: `crates/ab-warehouse/sql/morph_views.sql` (canonical `warehouse_nway_feature_diffs` view)
- Optionally modify: `crates/ab-morph-run/sql/morph_views.sql` (stale copy — see Step 12)
- Test: `batched_nway_fact_rows_match_collected_rows` (rows.rs), `push_region_rows_emits_maximal_contiguous_feature_diff_runs` (lib.rs), `feature_pattern_accumulator_linear_scan_matches_reference_grouping` (lib.rs), plus new collapse-equivalence, reader round-trip, and view-output tests.

**Interfaces:**
- Consumes (from Task 1): `NwayFeatureDiffRow.analyzers: Vec<Arc<str>>`; `NwayFeatureDiffsColumns::push_row(…, analyzers: &[impl AsRef<str>])`.
- Produces: no new public types. `WarehouseFeatureDiffFact` (`summary_body.rs:185`) stays `{ key, feature_value, analyzer_id: String }` — a per-analyzer fact stream, reconstructed by unnesting.

- [ ] **Step 1: Collapse the producer `push_region_rows`**

In `crates/ab-morph-run/src/warehouse/rows.rs`, in `push_region_rows` (~`:444`), replace the nested per-analyzer loop with one push per value-group:

```rust
        for value_group in &group.values {
            batch.feature_diffs.push_row(
                ids.run_id.as_ref(),
                ids.source_id.as_ref(),
                ids.text_id.as_ref(),
                region.region_index as u64,
                group.key.as_ref(),
                scope_type.as_ref(),
                scope_position,
                scope_surface.as_deref(),
                value_group.value.as_deref(),
                &value_group.analyzers,      // &[AnalyzerId] = &[String], already sorted
            );
        }
```

- [ ] **Step 2: Collapse the reference `push_region_rows_reference`**

In the same file (~`:491`), replace its inner per-analyzer loop so it builds one collapsed `NwayFeatureDiffRow` per value-group:

```rust
        for value_group in &group.values {
            rows.feature_diffs.push(NwayFeatureDiffRow {
                run_id: std::sync::Arc::clone(&ids.run_id),
                source_id: std::sync::Arc::clone(&ids.source_id),
                text_id: std::sync::Arc::clone(&ids.text_id),
                region_index: region.region_index as u64,
                feature_key: std::sync::Arc::clone(&group.key),
                scope_type: std::sync::Arc::clone(&scope_type),
                scope_position,
                scope_surface: scope_surface.clone(),
                feature_value: value_group.value.clone(),
                analyzers: value_group.analyzers.iter().map(|a| ids.analyzer(a)).collect(),
            });
        }
```

- [ ] **Step 3: Update the decode accessor to read the list column**

In the same file, in `decode_feature_diff_rows` (~`:521`), replace the column-9 scalar read with a list read and map to the `analyzers` vec. Change the `analyzer_id` local and the row construction:

```rust
    use arrow_array::ListArray;
    // …columns 0..8 unchanged…
    let analyzers = batch
        .column(9)
        .as_any()
        .downcast_ref::<ListArray>()
        .expect("nway_feature_diffs analyzers column is a ListArray");

    (0..batch.num_rows())
        .map(|row| {
            let list = analyzers.value(row);
            let strs = list
                .as_any()
                .downcast_ref::<StringArray>()
                .expect("analyzers list items are Utf8");
            NwayFeatureDiffRow {
                run_id: run_id.value(row).into(),
                source_id: source_id.value(row).into(),
                text_id: text_id.value(row).into(),
                region_index: region_index.value(row),
                feature_key: feature_key.value(row).into(),
                scope_type: scope_type.value(row).into(),
                scope_position: (!scope_position.is_null(row)).then(|| scope_position.value(row)),
                scope_surface: (!scope_surface.is_null(row)).then(|| scope_surface.value(row).into()),
                feature_value: (!feature_value.is_null(row)).then(|| feature_value.value(row).into()),
                analyzers: (0..strs.len()).map(|i| strs.value(i).into()).collect(),
            }
        })
        .collect()
```

- [ ] **Step 4: Adapt the reference accumulator (one-line change)**

In `crates/ab-morph-run/src/lib.rs`, in `warehouse_feature_pattern_from_rows` (`:830`), each fact now carries a list. Change the accumulation from `push` to `extend`:

```rust
    for fact in facts {
        by_value
            .entry(fact.feature_value.clone())
            .or_default()
            .extend(fact.analyzers.iter().cloned());   // was .push(fact.analyzer_id.clone())
    }
```
(`keyed_core_diffs` does not reference the analyzer field, so no other accumulator change is needed.)

- [ ] **Step 4b: Bump the summary reader's max schema version**

In `crates/ab-morph-run/src/summary/interesting.rs` (`:51`), change `pub(crate) const READER_MAX_SCHEMA_VERSION: u32 = 2;` to `= 3;`. Leave `SCORE_VERSION` (the artifact score version, a separate concern) at `2` — do not touch it. Without this bump the summary reader rejects the v3 runs that Task 2's workspace tests produce. (There is no separate warehouse schema-snapshot file to refresh — `schema.sql` + the `sql.rs:123` test are the pinned DDL contract, both handled in Task 1.)

- [ ] **Step 5: Re-expand the direct-Arrow reader**

In `crates/ab-morph-run/src/summary/summary_body.rs`, in `read_warehouse_feature_diffs` (`:3363`), replace the scalar column-9 read with the existing list helpers, emitting one `WarehouseFeatureDiffFact` per analyzer:

```rust
        let feature_value = string_column(&batch, 8)?;
        let analyzers = list_string_column(&batch, 9)?;   // was string_column(&batch, 9)?
        for row in 0..batch.num_rows() {
            let key = WarehouseFeatureGroupKey {
                region: WarehouseRegionKey {
                    run_id: run_id.value(row).to_owned(),
                    source_id: source_id.value(row).to_owned(),
                    text_id: text_id.value(row).to_owned(),
                    region_index: region_index.value(row),
                },
                feature_key: feature_key.value(row).to_owned(),
                scope_type: scope_type.value(row).to_owned(),
                scope_position: nullable_u64_value(scope_position, row),
                scope_surface: nullable_string_value(scope_surface, row),
            };
            let feature_value_row = nullable_string_value(feature_value, row);
            for analyzer_id in list_string_value(analyzers, row)? {
                facts.push(WarehouseFeatureDiffFact {
                    key: key.clone(),
                    feature_value: feature_value_row.clone(),
                    analyzer_id,
                });
            }
        }
```
(`list_string_column` and `list_string_value` already exist at `summary_body.rs:3615`/`:3631`. `WarehouseFeatureGroupKey` derives `Clone`; if not, clone its fields inline.)

- [ ] **Step 6: Add a shared inline UNNEST compatibility fragment**

The raw-parquet SQL builders read `read_parquet({features})` directly and reference `f.analyzer_id`, bypassing the view. Add one helper near the top of `crates/ab-morph-run/src/summary/summary_body.rs` (and make it `pub(super)` so `interesting_sql.rs` can use it):

```rust
/// Renders a `nway_feature_diffs` source relation that re-expands the collapsed
/// `analyzers` list into a scalar `analyzer_id` column, so SQL written against
/// the pre-v3 per-analyzer shape keeps working. `features_sql` is the
/// already-quoted argument to `read_parquet(...)`.
pub(super) fn nway_feature_diffs_expanded_source(features_sql: &str) -> String {
    format!(
        "(SELECT src.run_id, src.source_id, src.text_id, src.region_index, \
                 src.feature_key, src.scope_type, src.scope_position, \
                 src.scope_surface, src.feature_value, u.analyzer_id \
          FROM read_parquet({features_sql}) AS src, \
               UNNEST(src.analyzers) AS u(analyzer_id))"
    )
}
```

- [ ] **Step 7: Route every `summary_body.rs` raw-parquet builder through the fragment**

In `crates/ab-morph-run/src/summary/summary_body.rs` there are **seven** SQL builders that read the feature-diffs parquet directly with the identical clause `FROM read_parquet({features}) AS f` and reference `f.analyzer_id` (via `list(f.analyzer_id …)`, `count(DISTINCT analyzer_id)`, and `f.analyzer_id || …`). They are at lines **1774, 1932, 2019, 2158, 2252, 2334, 2413** (the file has two near-duplicate builder blocks, ~1700–2020 and ~2120–2413 — do not skip the second block).

In each enclosing builder function, compute the expanded source once before its `format!`:
```rust
let source = nway_feature_diffs_expanded_source(&features);
```
and replace that function's `FROM read_parquet({features}) AS f` with `FROM {source} AS f`. Every `f.analyzer_id`, `f.feature_key`, `count(DISTINCT analyzer_id)`, … reference then resolves against the expanded relation unchanged — the fragment projects all ten columns (`run_id … feature_value`, plus the unnested `analyzer_id`).

Caution: the SQL-shape assertion tests around `summary_body.rs:4854–4872` use `.contains("read_parquet('…nway_feature_diffs.parquet')")` — that substring still appears inside the expanded subquery, so they pass. Only update those tests if one asserts the exact literal `FROM read_parquet(…) AS f` structure (it now reads `FROM (SELECT … UNNEST …) AS f`).

- [ ] **Step 8: Route the `interesting_sql.rs` builder through the fragment**

In `crates/ab-morph-run/src/summary/interesting_sql.rs`, the anomalies query (~`:401`) does `FROM read_parquet({features}) f` and injects `{analyzer_flags}` referencing `f.analyzer_id`. Compute `let source = crate::summary::summary_body::nway_feature_diffs_expanded_source(&features);` and replace `FROM read_parquet({features}) f` with `FROM {source} f` (leave `{analyzer_flags}` unchanged).

- [ ] **Step 9: Re-expand the canonical view**

In `crates/ab-warehouse/sql/morph_views.sql`, replace the `warehouse_nway_feature_diffs` view body:

```sql
CREATE OR REPLACE VIEW warehouse_nway_feature_diffs AS
SELECT run_id, source_id, text_id, region_index, feature_key, scope_type,
       scope_position, scope_surface, feature_value, analyzer_id
FROM read_parquet('__RUN_DIR__/nway_feature_diffs.parquet'),
     UNNEST(analyzers) AS t(analyzer_id);
```
Leave `top_feature_differences` (which builds on this view and groups by `analyzer_id`) unchanged.

- [ ] **Step 10: Update all `NwayFeatureDiffRow` / `append_nway_feature_diffs` test fixtures**

Across `crates/ab-morph-run/src/summary/summary_body.rs` and `crates/ab-morph-run/src/lib.rs` test modules, every `NwayFeatureDiffRow { …, analyzer_id: "X".into() }` literal becomes `analyzers: vec!["X".into()]`. Where a test previously constructed two rows differing only by `analyzer_id` (same region/key/scope/value) to represent agreement, collapse them into a single row with `analyzers: vec!["A".into(), "B".into()]` (sorted). The compiler lists every site; fix until `cargo build -p ab-morph-run --tests` compiles.

- [ ] **Step 11: Run the retained differential + accumulator tests — expect PASS**

Run: `cargo test -p ab-morph-run batched_nway_fact_rows_match_collected_rows push_region_rows_emits_maximal_contiguous_feature_diff_runs feature_pattern_accumulator_linear_scan_matches_reference_grouping 2>&1 | tail -20`
Expected: PASS (3 tests). These confirm the collapsed direct-Arrow path still equals the collapsed reference, and the accumulator grouping is unchanged.

- [ ] **Step 12: Handle the stale SQL copy**

`crates/ab-morph-run/sql/morph_views.sql` is not `include_str!`-loaded anywhere (grep confirms). For hygiene, mirror the same `warehouse_nway_feature_diffs` UNNEST edit into it so the two files don't diverge further; do not add any sync tooling. (If the reviewer prefers, deleting the stale file is acceptable — but that is out of scope for this task; default to the mirrored edit.)

- [ ] **Step 13: Add the collapse-equivalence test**

Add to `crates/ab-morph-run/src/warehouse/rows.rs` tests a test that a region with (a) a value shared by ≥2 analyzers, (b) a value held by exactly 1 analyzer, and (c) a null `feature_value` produces collapsed rows whose `UNNEST` equals the pre-collapse per-analyzer expansion built inline from `feature_groups`:

```rust
#[test]
fn collapsed_rows_unnest_to_per_analyzer_expansion() {
    // Build a NwayRegion with one feature_group whose values are:
    //   Some("名詞") => ["sudachi-c", "vibrato"], Some("動詞") => ["sudachi-a"], None => ["mecab"]
    // (construct via the test helpers this module already uses for NwayRegion).
    let region = /* … existing test-region builder … */;
    let mut batch = NwayFactBatch::new();
    let mut counts = WarehouseFeaturePatternAccumulator::new();
    push_region_rows(&ids(), source_text, &char_map, &region, &mut batch, &mut counts);
    let collapsed = decode_feature_diff_rows(&batch.feature_diffs.finish());

    // Expected per-analyzer expansion (UNNEST of collapsed).
    let mut expanded: Vec<(String, String)> = Vec::new(); // (feature_value_or_∅, analyzer)
    for row in &collapsed {
        for a in &row.analyzers {
            expanded.push((
                row.feature_value.as_deref().unwrap_or("∅").to_owned(),
                a.to_string(),
            ));
        }
    }
    expanded.sort();
    let mut want = vec![
        ("名詞".to_owned(), "sudachi-c".to_owned()),
        ("名詞".to_owned(), "vibrato".to_owned()),
        ("動詞".to_owned(), "sudachi-a".to_owned()),
        ("∅".to_owned(), "mecab".to_owned()),
    ];
    want.sort();
    assert_eq!(expanded, want);
    // And each collapsed row's analyzers are ascending & non-empty.
    for row in &collapsed {
        assert!(!row.analyzers.is_empty());
        let mut sorted = row.analyzers.clone();
        sorted.sort();
        assert_eq!(row.analyzers, sorted, "analyzers must be ascending");
    }
}
```
Adapt the region construction to this module's existing test helpers (see the fixtures used by `batched_nway_fact_rows_match_collected_rows`).

- [ ] **Step 14: Add the reader round-trip test**

Add to `crates/ab-morph-run/src/summary/summary_body.rs` tests: write a small warehouse with `append_nway_feature_diffs(&[row with analyzers: vec!["sudachi-c","vibrato"]])`, then call `read_warehouse_feature_diffs(run_dir)` and assert it returns two `WarehouseFeatureDiffFact`s — one per analyzer — with identical `key`/`feature_value` and `analyzer_id` in `{"sudachi-c","vibrato"}`. Model the warehouse setup on the nearest existing `append_nway_feature_diffs`-based test in this file.

- [ ] **Step 15: Add the view-output-unchanged test (or extend an existing one)**

If a test already asserts `top_feature_differences` output over a fixture warehouse, confirm it still passes unchanged. If none exists, add one that creates a warehouse with collapsed rows, applies `MORPH_VIEWS_SQL_TEMPLATE`, and asserts `SELECT * FROM warehouse_nway_feature_diffs ORDER BY …` yields exactly the per-analyzer rows (one per analyzer, `analyzer_id` scalar) matching the pre-collapse expectation.

- [ ] **Step 16: Run the full workspace test suite — expect PASS (workspace green restored)**

Run: `cargo test --workspace 2>&1 | tail -25`
Expected: PASS (0 failures). This is the checkpoint that the cross-crate breaking change is fully reconciled.

- [ ] **Step 17: Commit**

```bash
git add crates/ab-morph-run/src/warehouse/rows.rs crates/ab-morph-run/src/lib.rs \
        crates/ab-morph-run/src/summary/summary_body.rs \
        crates/ab-morph-run/src/summary/interesting_sql.rs \
        crates/ab-warehouse/sql/morph_views.sql crates/ab-morph-run/sql/morph_views.sql \
        crates/ab-morph-run/src/summary/interesting.rs
git commit -m "feat(morph-run): collapse nway feature-diffs producer; re-expand all consumers"
```

---

### Task 3: Corpus validation record (hinoki)

**Files:**
- Modify: `docs/superpowers/plans/2026-07-08-nway-feature-diffs-analyzer-collapse.md` (append a Validation Record section)
- Uses: the `adjperf` orchestration harness on hinoki (`/db/ab-validator/adjperf/run.sh`), adapted for this comparison.

**Interfaces:** none (ops + documentation). This task has no unit-test cycle; its "test" is the corpus fingerprint parity.

- [ ] **Step 1: Pick the baseline and branch commits**

Baseline = the commit immediately before Task 1 (v2, scalar `analyzer_id`); branch = the Task 2 head (v3, `analyzers[]`). Record both short SHAs.

- [ ] **Step 2: Run baseline (v2) and branch (v3) full-corpus passes on hinoki**

Adapt `/db/ab-validator/adjperf/run.sh` (single-instance `flock`, `just dictionary-build-all`, 4 analyzers, `--jobs 0`, `--parquet-zstd-level 3`) to build+run both commits into `runs/collapse-base` and `runs/collapse-head`. Each pass ~40 min; capture `.err`/`.out` and `time -v`.

- [ ] **Step 3: Fingerprint parity under `UNNEST`**

For `nway_feature_diffs`, compute the order-independent fingerprint on the **UNNESTed** v3 vs the raw v2 (exclude `run_id`, which differs per run):
```sql
-- v3 (branch), expanded back to per-analyzer:
SELECT count(*) AS n, sum(hash(source_id,text_id,region_index,feature_key,scope_type,
       scope_position,scope_surface,feature_value,analyzer_id)) AS h_sum,
       bit_xor(hash(source_id,text_id,region_index,feature_key,scope_type,
       scope_position,scope_surface,feature_value,analyzer_id)) AS h_xor
FROM read_parquet('runs/collapse-head/nway_feature_diffs.parquet/*.parquet'),
     UNNEST(analyzers) AS t(analyzer_id);
-- v2 (baseline): same SELECT with analyzer_id read directly, no UNNEST.
```
All three of `(n, h_sum, h_xor)` must match. PASS/FAIL.

- [ ] **Step 4: Confirmatory `EXCEPT ALL` on a shard subset**

On a deterministic shard subset (the full 23 B-row `EXCEPT ALL` is too heavy), assert both directions empty between UNNESTed-v3 and v2. PASS/FAIL.

- [ ] **Step 5: Other-table parity + payoff metrics**

Assert row-count parity on all tables except `nway_feature_diffs` (must be identical). Record the `nway_feature_diffs` row-count reduction (v2 → v3), plus wall / peak-RSS / `phase-timings` deltas.

- [ ] **Step 6: Write the Validation Record and commit**

Append a "Validation Record (hinoki) — <date>" section to this plan with the baseline/branch SHAs, the fingerprint PASS/FAIL, the row reduction, and the perf deltas. Commit:
```bash
git add docs/superpowers/plans/2026-07-08-nway-feature-diffs-analyzer-collapse.md
git commit -m "docs(perf): record nway feature-diff collapse validation (hinoki)"
```

---

## Out of scope

- Surfacing `analyzers[]` in views, the summary report, or any external contract (foreclosed by the physical-only decision).
- Multi-version / dual-read support for v2 warehouses (foreclosed by clean break).
- Any change to `feature_pattern_counts`, `nway_region_oracle_evidence`, or other tables.
