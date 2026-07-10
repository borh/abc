# DuckDB Shape-Adaptive Feature Diffs + Hydrate Polish Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make every `nway_feature_diffs` consumer (DuckDB SQL and in-memory Rust) work on both on-disk shapes of the table — pre-v3 scalar `analyzer_id` and v3 collapsed `analyzers` list — and resolve the six recorded hydrate-interesting polish follow-ups.

**Architecture:** A `FeatureDiffsShape` enum detected once per run dir by sniffing the parquet footer (authoritative — never `runs.schema_version`), threaded as a parameter through the pure SQL builders; the in-memory reader picks its analyzer column by name. The hydrate polish items are local edits to `hydrate/source_context.rs`, `hydrate/analyses.rs`, `hydrate/metadata.rs`, `hydrate/render.rs`.

**Tech Stack:** Rust (ab-morph-run crate), arrow/parquet crates, DuckDB CLI via `run_duckdb_statement`.

## Background (read before Task 1)

Commit `2a9282fc` (2026-07-08, schema v3) collapsed the `nway_feature_diffs` producer to one row per feature value-group with an `analyzers: List<Utf8>` column, and rewrote all consumers to re-expand via `nway_feature_diffs_expanded_source` (`src/summary/summary_body.rs:35`), which does `UNNEST(src.analyzers)`. Runs written before that commit — including `/db/ab-validator/morph-warehouse/runs/calib-triage-1000` (schema v1) and `.../full-2026-07-06_160136-jobs8` (schema v2) — store one row per analyzer with a **scalar `analyzer_id`** column. On those runs:

- the DuckDB engine fails: `Binder Error: Table "src" does not have a column named "analyzers". Candidate bindings: "analyzer_id"`;
- the in-memory engine fails too: `read_warehouse_feature_diffs` (`summary_body.rs:3401`) downcasts column 9 to a `ListArray`.

The reader version gate (`interesting.rs` ~1429) only rejects `schema_version > 3`; pre-v3 runs pass it. v1→v2 was additive (`projection_spans` table), so the analyzer column of `nway_feature_diffs` is the **only** cross-version shape divergence readers must handle.

Both shapes are semantically interconvertible: consumers that need per-analyzer rows get pass-through (scalar) or UNNEST (collapsed); consumers that re-collapse with `list(f.analyzer_id ORDER BY f.analyzer_id)` produce identical lists either way (the producer writes `analyzers` ascending).

## Global Constraints

- **Output equivalence:** for the same logical data, `summarize-warehouse-interesting` output must be byte-identical across `{duckdb, in-memory} × {scalar, collapsed}` — pinned by test.
- **Determinism:** BTreeMap/sorted iteration everywhere; no wall-clock reads outside `main.rs`.
- **Closed hydrate error vocabulary:** `aat-missing`, `projection-mismatch`, `markup-unreconstructable`, `work-record-missing`, `person-record-missing`, `works-sidecar-missing`. Nothing else may appear in `errors[]` (Task 8 removes the stray `snippet:` prefix).
- **Shape detection is footer-based only.** Never branch on `runs.schema_version` — version metadata can disagree with the files actually on disk.
- **Gates:** `cargo test -p ab-morph-run` (lib + bin), `cargo clippy --workspace --all-targets -- -D warnings`, `cargo fmt --check` all green per task. DuckDB-gated tests need a `duckdb` binary: run with `AB_DUCKDB_BIN=/nix/store/yvjqlzi9lfci2l08fws8wc3bhnrps1pa-duckdb-1.5.2/bin/duckdb` (also on PATH via `duckdb_available()` fallback — export `PATH="$PATH:/nix/store/yvjqlzi9lfci2l08fws8wc3bhnrps1pa-duckdb-1.5.2/bin"` when running tests).
- Spec amendments (`docs/superpowers/specs/2026-07-10-hydrate-interesting-examples-design.md`) land in the same task as their code change.
- All paths below are relative to `ab-validator/`.

---

### Task 1: `FeatureDiffsShape` detection + shape-aware expanded source

**Files:**
- Modify: `crates/ab-morph-run/src/summary/summary_body.rs` (the `nway_feature_diffs_expanded_source` fn at line 35 and its doc comment at line 31; new enum + detection fn beside it; tests at the end of the `tests` module)

**Interfaces:**
- Produces: `pub(crate) enum FeatureDiffsShape { CollapsedAnalyzers, ScalarAnalyzerId }`; `pub(crate) fn nway_feature_diffs_shape(run_dir: &Path) -> Result<FeatureDiffsShape>`; `pub(super) fn nway_feature_diffs_expanded_source(features_sql: &str, shape: FeatureDiffsShape) -> String`.
- Note: this task changes `nway_feature_diffs_expanded_source`'s signature, which breaks its 6 call sites. To keep the tree compiling per-commit, Task 1 and Task 2 are committed **together as one commit** if needed — but prefer: Task 1 adds the enum/detection/new-signature AND mechanically updates the call sites to pass `FeatureDiffsShape::CollapsedAnalyzers` (temporary, preserving current behavior); Task 2 then replaces those temporaries with real detection. This keeps each commit green.

- [ ] **Step 1: Write the failing tests** (append inside `mod tests` in `summary_body.rs`)

```rust
    fn scalar_feature_diffs_schema() -> Arc<Schema> {
        Arc::new(Schema::new(vec![
            Field::new("run_id", DataType::Utf8, false),
            Field::new("source_id", DataType::Utf8, false),
            Field::new("text_id", DataType::Utf8, false),
            Field::new("region_index", DataType::UInt64, false),
            Field::new("feature_key", DataType::Utf8, false),
            Field::new("scope_type", DataType::Utf8, false),
            Field::new("scope_position", DataType::UInt64, true),
            Field::new("scope_surface", DataType::Utf8, true),
            Field::new("feature_value", DataType::Utf8, true),
            Field::new("analyzer_id", DataType::Utf8, false),
        ]))
    }

    fn write_empty_parquet(path: &Path, schema: Arc<Schema>) {
        let batch = RecordBatch::new_empty(schema.clone());
        let file = File::create(path).unwrap();
        let mut writer = ArrowWriter::try_new(file, schema, None).unwrap();
        writer.write(&batch).unwrap();
        writer.close().unwrap();
    }

    #[test]
    fn feature_diffs_shape_detects_scalar_and_collapsed() {
        use crate::warehouse::schema::WarehouseTable;
        // Scalar (pre-v3) single file.
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join(WarehouseTable::NwayFeatureDiffs.file_name());
        write_empty_parquet(&path, scalar_feature_diffs_schema());
        assert_eq!(
            nway_feature_diffs_shape(dir.path()).unwrap(),
            FeatureDiffsShape::ScalarAnalyzerId
        );

        // Collapsed (v3) directory-of-parts: shape read from the first part.
        let dir = tempfile::tempdir().unwrap();
        let parts = dir.path().join(WarehouseTable::NwayFeatureDiffs.file_name());
        fs::create_dir_all(&parts).unwrap();
        let collapsed = Arc::new(Schema::new(vec![
            Field::new("run_id", DataType::Utf8, false),
            Field::new(
                "analyzers",
                DataType::List(Arc::new(Field::new("item", DataType::Utf8, true))),
                false,
            ),
        ]));
        write_empty_parquet(&parts.join("part-000.parquet"), collapsed);
        assert_eq!(
            nway_feature_diffs_shape(dir.path()).unwrap(),
            FeatureDiffsShape::CollapsedAnalyzers
        );

        // Neither column: descriptive error.
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join(WarehouseTable::NwayFeatureDiffs.file_name());
        let bogus = Arc::new(Schema::new(vec![Field::new("run_id", DataType::Utf8, false)]));
        write_empty_parquet(&path, bogus);
        let err = nway_feature_diffs_shape(dir.path()).unwrap_err().to_string();
        assert!(err.contains("neither"), "unexpected error: {err}");

        // Missing file: error, not a silent default.
        let dir = tempfile::tempdir().unwrap();
        assert!(nway_feature_diffs_shape(dir.path()).is_err());
    }

    #[test]
    fn expanded_source_passes_scalar_shape_through_without_unnest() {
        let collapsed =
            nway_feature_diffs_expanded_source("'/x.parquet'", FeatureDiffsShape::CollapsedAnalyzers);
        assert!(collapsed.contains("UNNEST(src.analyzers)"));
        let scalar =
            nway_feature_diffs_expanded_source("'/x.parquet'", FeatureDiffsShape::ScalarAnalyzerId);
        assert!(!scalar.contains("UNNEST"));
        assert!(scalar.contains("src.analyzer_id"));
        // Both expose the same 10-column relation shape.
        for source in [&collapsed, &scalar] {
            for column in [
                "run_id", "source_id", "text_id", "region_index", "feature_key",
                "scope_type", "scope_position", "scope_surface", "feature_value",
            ] {
                assert!(source.contains(column), "{column} missing from {source}");
            }
        }
    }
```

(`Arc`, `Schema`, `Field`, `DataType`, `RecordBatch`, `ArrowWriter`, `File`, `fs` are already imported at the top of `summary_body.rs`; add `use` lines inside the tests module only if the compiler asks.)

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p ab-morph-run --lib feature_diffs_shape 2>&1 | tail -5` — expected: compile error (`FeatureDiffsShape` not found).

- [ ] **Step 3: Implement** — replace the existing fn + doc comment at `summary_body.rs:31-43` with:

```rust
/// The on-disk shape of `nway_feature_diffs`'s analyzer column. Schema v3
/// (2026-07-08) collapsed per-analyzer rows into one row per feature
/// value-group with an `analyzers: List<Utf8>` column; runs written before
/// that carry one row per analyzer with a scalar `analyzer_id` column.
/// Real pre-v3 runs remain on disk, so every reader handles both.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum FeatureDiffsShape {
    /// Schema >= 3: `analyzers: List<Utf8>`, one row per value-group.
    CollapsedAnalyzers,
    /// Pre-v3: scalar `analyzer_id: Utf8`, one row per analyzer.
    ScalarAnalyzerId,
}

/// Sniffs the parquet footer of `nway_feature_diffs` (single file or first
/// sorted part of a directory-of-parts) for the analyzer column shape. The
/// footer is authoritative — `runs.schema_version` is never consulted, so a
/// run dir whose version metadata disagrees with its actual files still
/// reads correctly.
pub(crate) fn nway_feature_diffs_shape(run_dir: &Path) -> Result<FeatureDiffsShape> {
    let path = run_dir.join(WarehouseTable::NwayFeatureDiffs.file_name());
    let file_path = if path.is_dir() {
        let mut parts = fs::read_dir(&path)
            .with_context(|| format!("failed to read {}", path.display()))?
            .map(|entry| entry.map(|entry| entry.path()))
            .collect::<std::result::Result<Vec<_>, _>>()
            .with_context(|| format!("failed to read {}", path.display()))?;
        parts.retain(|part| {
            part.extension()
                .is_some_and(|extension| extension == "parquet")
        });
        parts.sort();
        parts
            .into_iter()
            .next()
            .with_context(|| format!("{} contains no parquet parts", path.display()))?
    } else {
        path
    };
    let file =
        File::open(&file_path).with_context(|| format!("failed to open {}", file_path.display()))?;
    let builder = ParquetRecordBatchReaderBuilder::try_new(file).with_context(|| {
        format!(
            "failed to read parquet metadata from {}",
            file_path.display()
        )
    })?;
    let schema = builder.schema();
    if schema.field_with_name("analyzers").is_ok() {
        Ok(FeatureDiffsShape::CollapsedAnalyzers)
    } else if schema.field_with_name("analyzer_id").is_ok() {
        Ok(FeatureDiffsShape::ScalarAnalyzerId)
    } else {
        bail!(
            "{} has neither an `analyzers` list column (schema v3) nor a scalar `analyzer_id` column (pre-v3)",
            file_path.display()
        )
    }
}

/// Renders a `nway_feature_diffs` source relation exposing a scalar
/// `analyzer_id` column regardless of on-disk shape: pre-v3 files already
/// store one row per analyzer and pass through; v3 files re-expand the
/// collapsed `analyzers` list. SQL written against the pre-v3 per-analyzer
/// shape keeps working either way. `features_sql` is the already-quoted
/// argument to `read_parquet(...)`.
pub(super) fn nway_feature_diffs_expanded_source(
    features_sql: &str,
    shape: FeatureDiffsShape,
) -> String {
    match shape {
        FeatureDiffsShape::CollapsedAnalyzers => format!(
            "(SELECT src.run_id, src.source_id, src.text_id, src.region_index, \
                     src.feature_key, src.scope_type, src.scope_position, \
                     src.scope_surface, src.feature_value, u.analyzer_id \
              FROM read_parquet({features_sql}) AS src, \
                   UNNEST(src.analyzers) AS u(analyzer_id))"
        ),
        FeatureDiffsShape::ScalarAnalyzerId => format!(
            "(SELECT src.run_id, src.source_id, src.text_id, src.region_index, \
                     src.feature_key, src.scope_type, src.scope_position, \
                     src.scope_surface, src.feature_value, src.analyzer_id \
              FROM read_parquet({features_sql}) AS src)"
        ),
    }
}
```

Then update the 6 now-broken call sites to pass `FeatureDiffsShape::CollapsedAnalyzers` **as a temporary placeholder preserving current behavior** (Task 2 replaces them):
- `summary_body.rs:1706` (in `warehouse_pattern_duckdb_sql`)
- `summary_body.rs:1945` (in `materialize_core_feature_pattern_counts_duckdb_sql`)
- `summary_body.rs:2034` (in `warehouse_feature_pattern_select_sql`)
- `summary_body.rs:2136` (in `warehouse_region_examples_duckdb_sql`)
- `summary_body.rs:2228` (in `warehouse_pattern_examples_duckdb_sql`)
- `interesting_sql.rs:383` (in `feature_stage_query`) — use `crate::summary::summary_body::FeatureDiffsShape::CollapsedAnalyzers`

- [ ] **Step 4: Run the tests**

Run: `cargo test -p ab-morph-run --lib feature_diffs_shape expanded_source 2>&1 | tail -5` then the full lib suite `cargo test -p ab-morph-run --lib 2>&1 | tail -3`. Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-run/src/summary/summary_body.rs crates/ab-morph-run/src/summary/interesting_sql.rs
git commit -m "feat(morph-run): footer-sniffed FeatureDiffsShape; shape-aware expanded source"
```

---

### Task 2: Thread real shape detection to every consumer

**Files:**
- Modify: `crates/ab-morph-run/src/summary/summary_body.rs` (builders at 1699, 1938, 2022, 2129, ~2220; the materialize orchestration around 1139-1156; SQL snapshot tests ~4900-5300)
- Modify: `crates/ab-morph-run/src/summary/warehouse.rs` (wrappers at 24, 28, 35)
- Modify: `crates/ab-morph-run/src/summary/interesting_sql.rs` (`feature_stage_query` at 374, `collect_patterns_duckdb` at 741, `anomalies_duckdb` at 833)

**Interfaces:**
- Consumes: Task 1's `FeatureDiffsShape`, `nway_feature_diffs_shape`.
- Produces: `warehouse_pattern_duckdb_sql(run_dir, options, shape)`, `warehouse_region_examples_duckdb_sql(run_dir, options, shape)`, `warehouse_pattern_examples_duckdb_sql(run_dir, options, shape)`, `materialize_core_feature_pattern_counts_duckdb_sql(run_dir, output_path, feature_key, shape)`, `warehouse_feature_pattern_select_sql(..., shape)` (append the param last in each), `feature_stage_query(regions, analyzers, features, filter, feature_predicate, analyzer_ids, shape)`. The `warehouse.rs` wrapper signatures **stay `-> String`**.

- [ ] **Step 1: Write the failing test** (append inside `mod tests` in `summary_body.rs`)

```rust
    #[test]
    fn warehouse_pattern_sql_adapts_to_scalar_shape() {
        let dir = tempfile::tempdir().unwrap();
        let options = WarehousePatternOptions {
            kind: NwayPatternKind::Feature,
            ..Default::default()
        };
        let collapsed = warehouse_pattern_duckdb_sql(
            dir.path(),
            &options,
            FeatureDiffsShape::CollapsedAnalyzers,
        );
        assert!(collapsed.contains("UNNEST(src.analyzers)"));
        let scalar = warehouse_pattern_duckdb_sql(
            dir.path(),
            &options,
            FeatureDiffsShape::ScalarAnalyzerId,
        );
        assert!(!scalar.contains("UNNEST"));
        assert!(scalar.contains("src.analyzer_id"));
    }
```

(If `WarehousePatternOptions` does not implement `Default`, construct it the same way the nearest existing test at ~4922 does, changing only `kind`.)

- [ ] **Step 2: Run it to make sure it fails**

Run: `cargo test -p ab-morph-run --lib warehouse_pattern_sql_adapts 2>&1 | tail -5` — expected: compile error (wrong arity).

- [ ] **Step 3: Thread the parameter.** Mechanical, one pattern per layer:

1. Add `shape: FeatureDiffsShape` as the **last** parameter to the five `summary_body.rs` builders listed above and to `feature_stage_query` in `interesting_sql.rs`; inside each, replace the placeholder `FeatureDiffsShape::CollapsedAnalyzers` from Task 1 with the `shape` argument. `warehouse_pattern_duckdb_sql` forwards `shape` to its internal `warehouse_feature_pattern_select_sql` calls.
2. `warehouse.rs` wrappers (keep `-> String`), each becomes:

```rust
pub fn warehouse_pattern_duckdb_sql(run_dir: &Path, options: &WarehousePatternOptions) -> String {
    // Shape only matters when the SQL actually reads nway_feature_diffs;
    // on detection failure (e.g. table absent) keep the v3 SQL so the
    // missing-file error still surfaces from DuckDB itself, exactly as
    // before shape detection existed.
    let shape = super::summary_body::nway_feature_diffs_shape(run_dir)
        .unwrap_or(super::summary_body::FeatureDiffsShape::CollapsedAnalyzers);
    super::summary_body::warehouse_pattern_duckdb_sql(run_dir, options, shape)
}
```

   Same pattern (comment once, on the first wrapper) for `warehouse_region_examples_duckdb_sql` and `warehouse_pattern_examples_duckdb_sql`. `warehouse_feature_pattern_counts_duckdb_sql` reads the materialized counts table, not feature diffs — leave it untouched.
3. In `materialize_warehouse_core_feature_pattern_counts` (the fallible orchestration in `summary_body.rs` whose loop at ~1146 calls `materialize_core_feature_pattern_counts_duckdb_sql`): add `let shape = nway_feature_diffs_shape(run_dir)?;` before the loop and pass it through. This path always reads the table, so a detection failure is a real error.
4. In `interesting_sql.rs`: in `collect_patterns_duckdb` add `let shape = crate::summary::summary_body::nway_feature_diffs_shape(run_dir)?;` right after the `features` path literal (line ~749) and pass `shape` to both `feature_stage_query` calls it makes; in `anomalies_duckdb` do the same before its `feature_stage_query` call (~880). Import `FeatureDiffsShape` and `nway_feature_diffs_shape` at the top of the file alongside the existing `summary_body` imports.
5. Update every existing SQL snapshot test in `summary_body.rs` (~4922, 4948, 4967, 5006, 5035, 5054, 5077, 5146, 5288) to pass `FeatureDiffsShape::CollapsedAnalyzers` explicitly — their pinned SQL strings must not change.

- [ ] **Step 4: Run the full suite**

Run: `cargo test -p ab-morph-run 2>&1 | tail -5` and `cargo clippy -p ab-morph-run --all-targets -- -D warnings`. Expected: PASS, no warnings.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-run/src/summary/
git commit -m "fix(morph-run): detect nway_feature_diffs shape per run dir in all duckdb consumers"
```

---

### Task 3: Shape-adaptive in-memory reader + engine/shape equivalence tests

**Files:**
- Modify: `crates/ab-morph-run/src/summary/summary_body.rs` (`read_warehouse_feature_diffs` at 3401)
- Modify: `crates/ab-morph-run/src/summary/interesting.rs` (tests module: scalar rewrite helper + equivalence test near `duckdb_engine_matches_in_memory_engine` at ~2414)

**Interfaces:**
- Consumes: `write_fixture` (`interesting.rs:1791`), `duckdb_available()` (~2384), `read_warehouse_parquet_file`, `WarehouseTable`.
- Produces: `read_warehouse_feature_diffs` accepting both shapes (same signature); test helper `rewrite_feature_diffs_as_scalar(run_dir: &Path)`.

- [ ] **Step 1: Write the failing tests** (append inside `mod tests` in `interesting.rs`)

```rust
    /// Rewrites `run_dir/nway_feature_diffs.parquet` from the v3 collapsed
    /// shape into the pre-v3 scalar `analyzer_id` shape (one row per
    /// analyzer, expanded in the list's stored — ascending — order,
    /// mirroring the pre-v3 producer's emission order).
    fn rewrite_feature_diffs_as_scalar(run_dir: &Path) {
        use arrow_array::{Array, ListArray, StringArray, UInt64Array};
        use parquet::arrow::ArrowWriter;

        let path = run_dir.join(WarehouseTable::NwayFeatureDiffs.file_name());
        let batches =
            crate::summary::summary_body::read_warehouse_parquet_file(&path).unwrap();
        let scalar_schema = Arc::new(Schema::new(vec![
            Field::new("run_id", DataType::Utf8, false),
            Field::new("source_id", DataType::Utf8, false),
            Field::new("text_id", DataType::Utf8, false),
            Field::new("region_index", DataType::UInt64, false),
            Field::new("feature_key", DataType::Utf8, false),
            Field::new("scope_type", DataType::Utf8, false),
            Field::new("scope_position", DataType::UInt64, true),
            Field::new("scope_surface", DataType::Utf8, true),
            Field::new("feature_value", DataType::Utf8, true),
            Field::new("analyzer_id", DataType::Utf8, false),
        ]));
        let mut strings: [Vec<Option<String>>; 8] = Default::default();
        let mut region_indexes = Vec::<u64>::new();
        let mut scope_positions = Vec::<Option<u64>>::new();
        for batch in &batches {
            let column = |index: usize| {
                batch
                    .column(index)
                    .as_any()
                    .downcast_ref::<StringArray>()
                    .unwrap()
            };
            let region_index = batch
                .column(3)
                .as_any()
                .downcast_ref::<UInt64Array>()
                .unwrap();
            let scope_position = batch
                .column(6)
                .as_any()
                .downcast_ref::<UInt64Array>()
                .unwrap();
            let analyzers = batch
                .column(9)
                .as_any()
                .downcast_ref::<ListArray>()
                .unwrap();
            for row in 0..batch.num_rows() {
                let list = analyzers.value(row);
                let items = list.as_any().downcast_ref::<StringArray>().unwrap();
                for item in 0..items.len() {
                    for (target, source_index) in
                        strings.iter_mut().zip([0usize, 1, 2, 4, 5, 7, 8, 9])
                    {
                        if source_index == 9 {
                            target.push(Some(items.value(item).to_owned()));
                        } else {
                            let array = column(source_index);
                            target.push(
                                (!array.is_null(row)).then(|| array.value(row).to_owned()),
                            );
                        }
                    }
                    region_indexes.push(region_index.value(row));
                    scope_positions
                        .push((!scope_position.is_null(row)).then(|| scope_position.value(row)));
                }
            }
        }
        let string_array = |values: &Vec<Option<String>>| -> Arc<dyn Array> {
            Arc::new(StringArray::from(values.clone()))
        };
        let batch = RecordBatch::try_new(
            scalar_schema.clone(),
            vec![
                string_array(&strings[0]),
                string_array(&strings[1]),
                string_array(&strings[2]),
                Arc::new(UInt64Array::from(region_indexes.clone())),
                string_array(&strings[3]),
                string_array(&strings[4]),
                Arc::new(UInt64Array::from(scope_positions.clone())),
                string_array(&strings[5]),
                string_array(&strings[6]),
                string_array(&strings[7]),
            ],
        )
        .unwrap();
        let file = std::fs::File::create(&path).unwrap();
        let mut writer = ArrowWriter::try_new(file, scalar_schema, None).unwrap();
        writer.write(&batch).unwrap();
        writer.close().unwrap();
    }

    #[test]
    fn engines_agree_across_feature_diff_shapes() {
        let dir = tempfile::tempdir().unwrap();
        let run_dir = write_fixture(dir.path());
        let options = |engine| WarehouseInterestingOptions {
            engine,
            ..Default::default()
        };
        let collapsed_memory =
            summarize_warehouse_interesting(&run_dir, options(InterestingEngine::InMemory))
                .unwrap();
        rewrite_feature_diffs_as_scalar(&run_dir);
        let scalar_memory =
            summarize_warehouse_interesting(&run_dir, options(InterestingEngine::InMemory))
                .unwrap();
        assert_eq!(
            serde_json::to_string(&collapsed_memory).unwrap(),
            serde_json::to_string(&scalar_memory).unwrap(),
            "in-memory engine must be shape-invariant"
        );
        if duckdb_available() {
            let scalar_duckdb =
                summarize_warehouse_interesting(&run_dir, options(InterestingEngine::Duckdb))
                    .unwrap();
            assert_eq!(
                serde_json::to_string(&collapsed_memory).unwrap(),
                serde_json::to_string(&scalar_duckdb).unwrap(),
                "duckdb engine on scalar shape must match"
            );
        }
    }
```

Adapt option construction to how `duckdb_engine_matches_in_memory_engine` (~2414) builds `WarehouseInterestingOptions` — reuse its exact pattern (including any non-Default fields it sets and the fixed `built at` style fields) rather than `..Default::default()` if that test does it differently. The assertion contract is what matters: **in-memory collapsed == in-memory scalar == duckdb scalar**, as serialized JSON.

- [ ] **Step 2: Run to verify failure**

Run: `PATH="$PATH:/nix/store/yvjqlzi9lfci2l08fws8wc3bhnrps1pa-duckdb-1.5.2/bin" cargo test -p ab-morph-run --lib engines_agree_across 2>&1 | tail -8` — expected: FAIL (in-memory reader panics/errs downcasting column 9 on the scalar file).

- [ ] **Step 3: Make `read_warehouse_feature_diffs` shape-adaptive.** Replace its body's column-9 handling (`summary_body.rs:3415` and the inner loop at 3430-3436):

```rust
pub(super) fn read_warehouse_feature_diffs(
    run_dir: &Path,
) -> Result<Vec<WarehouseFeatureDiffFact>> {
    let mut facts = Vec::new();
    for batch in read_warehouse_table(run_dir, WarehouseTable::NwayFeatureDiffs)? {
        let run_id = string_column(&batch, 0)?;
        let source_id = string_column(&batch, 1)?;
        let text_id = string_column(&batch, 2)?;
        let region_index = u64_column(&batch, 3)?;
        let feature_key = string_column(&batch, 4)?;
        let scope_type = string_column(&batch, 5)?;
        let scope_position = u64_column(&batch, 6)?;
        let scope_surface = string_column(&batch, 7)?;
        let feature_value = string_column(&batch, 8)?;
        // The analyzer column differs by schema era (see FeatureDiffsShape):
        // v3 collapsed `analyzers` list vs pre-v3 scalar `analyzer_id`.
        let schema = batch.schema();
        let analyzers_list = match schema.index_of("analyzers") {
            Ok(index) => Some(list_string_column(&batch, index)?),
            Err(_) => None,
        };
        let analyzer_scalar = match analyzers_list {
            Some(_) => None,
            None => {
                let index = schema.index_of("analyzer_id").context(
                    "nway_feature_diffs has neither `analyzers` nor `analyzer_id` column",
                )?;
                Some(string_column(&batch, index)?)
            }
        };
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
            if let Some(list) = analyzers_list {
                for analyzer_id in list_string_value(list, row)? {
                    facts.push(WarehouseFeatureDiffFact {
                        key: key.clone(),
                        feature_value: feature_value_row.clone(),
                        analyzer_id,
                    });
                }
            } else if let Some(scalar) = analyzer_scalar {
                facts.push(WarehouseFeatureDiffFact {
                    key,
                    feature_value: feature_value_row,
                    analyzer_id: scalar.value(row).to_owned(),
                });
            }
        }
    }
    Ok(facts)
}
```

(Note the scalar arm moves `key`/`feature_value_row` instead of cloning — adjust borrows if the compiler objects by cloning in the list arm only, as shown.)

- [ ] **Step 4: Run the tests**

Run: `PATH="$PATH:/nix/store/yvjqlzi9lfci2l08fws8wc3bhnrps1pa-duckdb-1.5.2/bin" cargo test -p ab-morph-run 2>&1 | tail -5` (confirm `engines_agree_across_feature_diff_shapes` PASSes and nothing regressed), plus clippy + fmt.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-run/src/summary/
git commit -m "fix(morph-run): in-memory feature-diff reader accepts pre-v3 scalar shape; engine/shape equivalence test"
```

---

### Task 4: Gaiji renders the same at every nesting level, byte-verified when exact

**Files:**
- Modify: `crates/ab-morph-run/src/hydrate/source_context.rs` (`inline_child_text` gaiji arm at ~333; `render_node` gaiji arm at ~384; tests)
- Modify: `docs/superpowers/specs/2026-07-10-hydrate-interesting-examples-design.md` (§Layer 3 step 2)

**Interfaces:**
- Produces: private `fn gaiji_markup(node: &serde_json::Value) -> Option<String>` used by both arms.

- [ ] **Step 1: Write the failing tests** (in `source_context.rs` tests)

```rust
    #[test]
    fn gaiji_renders_marker_form_at_every_nesting_level_and_byte_verifies() {
        // Top level, span length exactly equal to the rendered marker
        // (`※［＃小書き片仮名ン］` = 4 marker chars + 7 description chars,
        // all 3-byte UTF-8 = 33 bytes) ⇒ Verbatim, not approximate.
        let aat = json!({
            "version": 1, "work_id": "src-gaiji",
            "blocks": [{"kind": "paragraph", "content": [
                {"kind": "gaiji", "description": "小書き片仮名ン", "resolved": "ン",
                 "span": {"byte_start": 0, "byte_end": 33, "line_start": 1, "line_end": 1}}
            ]}],
            "meta": {"adapter": "aozora", "adapter_version": "fixture",
                     "source_encoding": "windows-31j", "parse_complete": true,
                     "source_hash": "sha256:00", "warnings": []}
        });
        let (text, spans) = ab_plaintext::visible_text_projection_with_spans(&aat);
        assert_eq!(text, "ン");
        let (markup, _) = reconstruct_markup(&aat, &spans, 0, 1).unwrap();
        assert_eq!(markup.text, "※［＃小書き片仮名ン］");
        assert!(
            markup.approximate_pointers.is_empty(),
            "byte-exact gaiji marker must be verbatim"
        );

        // Nested inside a style node: same marker form (previously the
        // nested arm preferred `resolved`, diverging from the top level).
        let aat = json!({
            "version": 1, "work_id": "src-style-gaiji",
            "blocks": [{"kind": "paragraph", "content": [
                {"kind": "style", "class": "bouten", "content": [
                    {"kind": "gaiji", "description": "小書き片仮名ン", "resolved": "ン"}
                ],
                 "span": {"byte_start": 0, "byte_end": 39, "line_start": 1, "line_end": 1}}
            ]}],
            "meta": {"adapter": "aozora", "adapter_version": "fixture",
                     "source_encoding": "windows-31j", "parse_complete": true,
                     "source_hash": "sha256:00", "warnings": []}
        });
        let span = ab_plaintext::ProjectionSpan {
            projected_char_start: 0,
            projected_char_end: 1,
            aat_pointer: "/blocks/0/content/0".to_owned(),
            inline_kind: "style".to_owned(),
            is_ruby_base: false,
            is_gaiji: false,
            is_note: false,
        };
        let (markup, _) = reconstruct_markup(&aat, &[span], 0, 1).unwrap();
        assert_eq!(markup.text, "※［＃小書き片仮名ン］");
    }

    #[test]
    fn gaiji_without_description_falls_back_to_resolved_as_approximate() {
        let aat = json!({
            "version": 1, "work_id": "src-gaiji-resolved",
            "blocks": [{"kind": "paragraph", "content": [
                {"kind": "gaiji", "description": "", "resolved": "ン",
                 "span": {"byte_start": 0, "byte_end": 20, "line_start": 1, "line_end": 1}}
            ]}],
            "meta": {"adapter": "aozora", "adapter_version": "fixture",
                     "source_encoding": "windows-31j", "parse_complete": true,
                     "source_hash": "sha256:00", "warnings": []}
        });
        let (_, spans) = ab_plaintext::visible_text_projection_with_spans(&aat);
        let (markup, _) = reconstruct_markup(&aat, &spans, 0, 1).unwrap();
        assert_eq!(markup.text, "ン");
        assert_eq!(
            markup.approximate_pointers,
            vec!["/blocks/0/content/0".to_owned()]
        );
    }
```

(If the projection helper skips description-less gaiji differently, build the span manually as the existing legacy-raw test does.)

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p ab-morph-run --lib gaiji 2>&1 | tail -8` — expected: FAIL (nested renders `ン`; top-level exact-length still flagged approximate).

- [ ] **Step 3: Implement.** Add the shared helper above `inline_child_text`:

```rust
/// The Aozora marker form of a gaiji node — `※［＃description］` when a
/// description is present, else the resolved character. Both callers use
/// this so gaiji renders identically at top level and nested inside
/// style/tcy; byte-length verification (top level only) decides
/// verbatim-vs-approximate.
fn gaiji_markup(node: &serde_json::Value) -> Option<String> {
    if let Some(description) = node
        .get("description")
        .and_then(serde_json::Value::as_str)
        .filter(|description| !description.is_empty())
    {
        return Some(format!("※［＃{description}］"));
    }
    node.get("resolved")
        .and_then(serde_json::Value::as_str)
        .filter(|resolved| !resolved.is_empty())
        .map(str::to_owned)
}
```

Replace the `inline_child_text` gaiji arm with `"gaiji" => gaiji_markup(node),` and the `render_node` gaiji arm with:

```rust
        "gaiji" => {
            let rendered = gaiji_markup(node)?;
            // Same byte-length rule as ruby: a marker that tiles its span
            // exactly is verbatim sanitized-source markup; anything else
            // (elided code suffix, resolved-character fallback) is a
            // semantic approximation.
            if rendered.len() as u64 == span_len {
                Some(Rendered::Verbatim(rendered))
            } else {
                Some(Rendered::Approximate(rendered))
            }
        }
```

The existing `markup_reconstruction_flags_gaiji_as_approximate` test still passes (33-byte rendering vs 20-byte span ⇒ approximate). If any existing test asserts nested gaiji renders `resolved`, update it to the marker form.

- [ ] **Step 4: Run tests**: `cargo test -p ab-morph-run 2>&1 | tail -3` — expected: PASS.

- [ ] **Step 5: Amend the spec** — in §Layer 3 step 2 of the design doc, replace

`- \`gaiji\` → \`※［＃description］\`, \`style\`/\`tcy\` → inner text — semantic forms, always flagged approximate.`

with

`- \`gaiji\` → \`※［＃description］\` (falling back to the resolved character when the description is empty) at every nesting level; byte-length-verified like ruby, so an exactly-tiling marker is verbatim and anything else is flagged approximate. \`style\`/\`tcy\` → inner text — semantic, always approximate.`

- [ ] **Step 6: Commit**

```bash
git add crates/ab-morph-run/src/hydrate/source_context.rs docs/superpowers/specs/2026-07-10-hydrate-interesting-examples-design.md
git commit -m "fix(hydrate): uniform gaiji marker rendering at all nesting levels, byte-verified"
```

---

### Task 5: Gap records stop misattributing the following node

**Files:**
- Modify: `crates/ab-morph-run/src/hydrate/source_context.rs` (`AozoraMarkup`, `reconstruct_markup`, tests)
- Modify: `crates/ab-morph-run/src/hydrate/render.rs` (approximate-header condition at ~199)
- Modify: `docs/superpowers/specs/2026-07-10-hydrate-interesting-examples-design.md` (§Layer 3 step 3 + the `aozora_markup` line of the JSON example)

**Interfaces:**
- Produces: `pub struct ByteGap { pub byte_start: u64, pub byte_end: u64 }`; `AozoraMarkup` gains `pub gaps: Vec<ByteGap>`.

- [ ] **Step 1: Write the failing test** (in `source_context.rs` tests)

```rust
    #[test]
    fn markup_gap_is_recorded_as_byte_range_not_pointer() {
        // Two text nodes with a 10-byte hole (a non-projecting marker)
        // between them.
        let aat = json!({
            "version": 1, "work_id": "src-gap",
            "blocks": [{"kind": "paragraph", "content": [
                {"kind": "text", "value": "AB",
                 "span": {"byte_start": 0, "byte_end": 2, "line_start": 1, "line_end": 1}},
                {"kind": "text", "value": "CD",
                 "span": {"byte_start": 12, "byte_end": 14, "line_start": 1, "line_end": 1}}
            ]}],
            "meta": {"adapter": "aozora", "adapter_version": "fixture",
                     "source_encoding": "windows-31j", "parse_complete": true,
                     "source_hash": "sha256:00", "warnings": []}
        });
        let (text, spans) = ab_plaintext::visible_text_projection_with_spans(&aat);
        assert_eq!(text, "ABCD");
        let (markup, _) = reconstruct_markup(&aat, &spans, 0, 4).unwrap();
        assert_eq!(markup.text, "AB…CD");
        assert_eq!(markup.gaps, vec![ByteGap { byte_start: 2, byte_end: 12 }]);
        // The node after the gap rendered verbatim — it must NOT be listed
        // as approximate just because a gap precedes it.
        assert!(markup.approximate_pointers.is_empty());
    }
```

- [ ] **Step 2: Run to verify failure**: `cargo test -p ab-morph-run --lib markup_gap 2>&1 | tail -5` — expected: compile error (`ByteGap` unknown).

- [ ] **Step 3: Implement.**

Add below `AatNodeRef`:

```rust
/// A byte range inside the covering span that no contributing node
/// rendered (a non-projecting marker sits there); shown as `…` in the
/// slice text.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ByteGap {
    pub byte_start: u64,
    pub byte_end: u64,
}
```

Add `pub gaps: Vec<ByteGap>,` to `AozoraMarkup` (after `approximate_pointers`), and update its doc comment to mention that `gaps` — not `approximate_pointers` — records unrendered ranges. In `reconstruct_markup`: add `let mut gaps = Vec::new();` beside `approximate`, replace the gap branch body

```rust
        if let Some(prev) = prev_byte_end
            && node_start > prev
        {
            rendered.push('…');
            gaps.push(ByteGap {
                byte_start: prev,
                byte_end: node_start,
            });
        }
```

(dropping the `approximate.push(node_ref.pointer.clone())` there), and include `gaps` in the returned `AozoraMarkup`. In `render.rs`, the header condition becomes:

```rust
            let header = if markup.approximate_pointers.is_empty() && markup.gaps.is_empty() {
                "Aozora markup:"
            } else {
                "Aozora markup (approximate):"
            };
```

- [ ] **Step 4: Run tests**: `cargo test -p ab-morph-run 2>&1 | tail -3` — expected: PASS (e2e JSON assertions in `hydrate/mod.rs` don't pin an exhaustive `aozora_markup` key set; if one asserts exact JSON, extend it with `"gaps": []`).

- [ ] **Step 5: Amend the spec.** §Layer 3 step 3: replace "gaps (non-projecting markers inside the region) render as `…` and flag the slice approximate" with "gaps (non-projecting markers inside the region) render as `…` and are recorded per-slice as `gaps: [{byte_start, byte_end}]`; a slice with gaps is approximate as a whole, but the nodes around a gap keep their own byte-verified status". In the JSON example, change the `aozora_markup` line to `"aozora_markup": { "text": "…《…》…", "byte_start": 123, "byte_end": 456, "approximate_pointers": [], "gaps": [] },`.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-morph-run/src/hydrate/ docs/superpowers/specs/2026-07-10-hydrate-interesting-examples-design.md
git commit -m "fix(hydrate): record markup gaps as byte ranges instead of blaming the following node"
```

---

### Task 6: Segmentation column reads `nway_region_analyzers.surfaces` (spec §Layer 2)

**Files:**
- Modify: `crates/ab-morph-run/src/hydrate/analyses.rs` (`AnalyzerAnalysis`, `group_analyses`, tests)
- Modify: `crates/ab-morph-run/src/hydrate/render.rs` (`render_segmentation`)

**Interfaces:**
- Produces: `AnalyzerAnalysis` gains `pub surfaces: Vec<String>` (serialized); grouping key becomes `(covers_exactly, surfaces, tokens)`.

Rationale: the spec says the segmentation column comes from `nway_region_analyzers`, but the implementation derives it from morpheme tokens; and when two analyzers have no overlapping tokens recorded, they are wrongly grouped as "identical analyses" even if their surfaces differ. This closes the `RegionAnalyzerRow.surfaces` read-but-unused follow-up by using it.

- [ ] **Step 1: Write the failing test** (in `analyses.rs` tests)

```rust
    #[test]
    fn differing_surfaces_prevent_grouping_and_render_from_surfaces() {
        use crate::hydrate::tables::RegionAnalyzerRow;
        use std::collections::BTreeMap;

        // No tokens recorded at all: without surfaces in the key these two
        // would collapse into one "identical" group despite segmenting
        // differently.
        let rows = vec![
            RegionAnalyzerRow {
                analyzer_id: "sudachi-a".to_owned(),
                covers_exactly: true,
                morpheme_start: 0,
                morpheme_end: 2,
                surfaces: vec!["今".to_owned(), "日".to_owned()],
            },
            RegionAnalyzerRow {
                analyzer_id: "vibrato".to_owned(),
                covers_exactly: true,
                morpheme_start: 0,
                morpheme_end: 1,
                surfaces: vec!["今日".to_owned()],
            },
        ];
        let tokens = BTreeMap::new();
        let groups = group_analyses(&rows, &tokens, "src-a");
        assert_eq!(groups.len(), 2);
        assert_eq!(groups[0].surfaces, vec!["今", "日"]);
        assert_eq!(groups[1].surfaces, vec!["今日"]);
    }
```

And in `render.rs` tests (unit-level, no fixture needed — add near the existing test):

```rust
    #[test]
    fn segmentation_cell_prefers_region_analyzer_surfaces() {
        use crate::hydrate::analyses::AnalyzerAnalysis;
        let analysis = AnalyzerAnalysis {
            analyzer_ids: vec!["vibrato".to_owned()],
            covers_exactly: true,
            surfaces: vec!["今日".to_owned(), "は".to_owned()],
            tokens: vec![],
        };
        assert_eq!(super::render_segmentation(&analysis), "今日｜は");
    }
```

- [ ] **Step 2: Run to verify failure**: `cargo test -p ab-morph-run --lib differing_surfaces segmentation_cell 2>&1 | tail -5` — expected: compile error (no `surfaces` field).

- [ ] **Step 3: Implement.**

In `analyses.rs`: add `pub surfaces: Vec<String>,` to `AnalyzerAnalysis` (between `covers_exactly` and `tokens`); in `group_analyses` extend the match predicate to `group.covers_exactly == row.covers_exactly && group.surfaces == row.surfaces && group.tokens == row_tokens` and the `None` arm to include `surfaces: row.surfaces.clone(),`. Update the module doc comment ("agree on `(covers_exactly, surfaces, tokens)`"). In `render.rs`:

```rust
fn render_segmentation(analysis: &AnalyzerAnalysis) -> String {
    // Spec §Layer 2: segmentation comes from nway_region_analyzers'
    // surfaces; tokens (morphemes join) are only a fallback for rows
    // written without surfaces.
    if !analysis.surfaces.is_empty() {
        return analysis.surfaces.join("｜");
    }
    analysis
        .tokens
        .iter()
        .map(|token| token.surface.as_str())
        .collect::<Vec<_>>()
        .join("｜")
}
```

Fix the existing `analyses.rs` grouping test (its `row` helper builds `surfaces: vec![]` — give the sudachi-c and vibrato rows identical surfaces, e.g. `vec!["猫である".to_owned()]`, and sudachi-a `vec!["猫".to_owned(), "である".to_owned()]`, keeping the grouping outcome the same) and any e2e assertion in `hydrate/mod.rs` / `render.rs` whose expected segmentation cell changes (the e2e fixture's `nway_region_analyzers` rows carry surfaces consistent with their tokens, so cells should not change content — verify, don't assume).

- [ ] **Step 4: Run tests**: `cargo test -p ab-morph-run 2>&1 | tail -3` — expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-run/src/hydrate/
git commit -m "fix(hydrate): segmentation from region-analyzer surfaces; surfaces join the grouping key"
```

---

### Task 7: Author fallback when the ABC work record has no contributors

**Files:**
- Modify: `crates/ab-morph-run/src/hydrate/metadata.rs` (`resolve_work_meta` at ~157; doc comment; tests)

- [ ] **Step 1: Write the failing test**

```rust
    #[test]
    fn contributor_less_work_record_falls_back_to_warehouse_author() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("works")).unwrap();
        std::fs::create_dir_all(dir.path().join("persons")).unwrap();
        // Work record present but with no contributors array.
        std::fs::write(
            dir.path().join("works/000080.json"),
            serde_json::json!({"work": {"work_id": "000080", "title": "煙管"}}).to_string(),
        )
        .unwrap();
        std::fs::write(
            dir.path().join("persons/000879.json"),
            serde_json::json!({
                "person_id": "000879", "family_name": "芥川", "given_name": "竜之介"
            })
            .to_string(),
        )
        .unwrap();
        let (meta, errors) = resolve_work_meta(Some(&work_row()), Some(dir.path()));
        assert!(errors.is_empty());
        // The warehouse author_person_id still resolves through persons/.
        assert_eq!(meta.unwrap().display_author(), "芥川竜之介");
    }
```

- [ ] **Step 2: Run to verify failure**: `cargo test -p ab-morph-run --lib contributor_less 2>&1 | tail -5` — expected: FAIL (`display_author()` returns `""`).

- [ ] **Step 3: Implement.** In `resolve_work_meta`, the fallback condition at ~157 drops the `record_available` guard:

```rust
    // A work record that exists but lists no contributors must not
    // silently drop the warehouse's author_person_id — fall back whenever
    // no contributor was collected, whatever the reason.
    if contributor_pairs.is_empty()
        && let Some(author_person_id) = &work_row.author_person_id
    {
        contributor_pairs.push((author_person_id.clone(), "著者".to_owned()));
    }
```

Delete the now-unused `record_available` variable and its assignments. Update the fn doc comment's third bullet to mention the contributor-less-record fallback.

- [ ] **Step 4: Run tests**: `cargo test -p ab-morph-run 2>&1 | tail -3` — expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-run/src/hydrate/metadata.rs
git commit -m "fix(hydrate): synthesize author contributor when ABC work record lists none"
```

---

### Task 8: Retire the out-of-vocabulary `snippet:` error code

**Files:**
- Modify: `crates/ab-morph-run/src/hydrate/source_context.rs` (`hydrate_region` at ~69; tests)
- Modify: `crates/ab-morph-run/src/hydrate/render.rs` (`layer_error` prefix at ~176)
- Modify: `docs/superpowers/specs/2026-07-10-hydrate-interesting-examples-design.md` (error table row for `projection-mismatch`)

- [ ] **Step 1: Write the failing test** (extend `hydrate_region_degrades_markup_but_keeps_snippet` in `source_context.rs`; after the out-of-range `hydrate_region(0, 999, 2)` block add)

```rust
        // Every error uses the closed vocabulary — the snippet failure is a
        // projection disagreement, not its own ad-hoc code.
        for error in &layers.errors {
            let code = error.split(':').next().unwrap();
            assert!(
                ["projection-mismatch", "markup-unreconstructable"].contains(&code),
                "unexpected error code in {error:?}"
            );
        }
```

- [ ] **Step 2: Run to verify failure**: `cargo test -p ab-morph-run --lib hydrate_region_degrades 2>&1 | tail -5` — expected: FAIL (`snippet: …` present).

- [ ] **Step 3: Implement.** In `hydrate_region`, change

```rust
            Err(err) => {
                errors.push(format!("snippet: {err}"));
                None
            }
```

to

```rust
            Err(err) => {
                // An example span the projected text cannot contain is a
                // projection disagreement (closed vocabulary) — same code
                // as the source-level char-count gate.
                errors.push(format!("projection-mismatch: {err}"));
                None
            }
```

In `render.rs`, change `layer_error(&example.errors, &["snippet:"])` to `layer_error(&example.errors, &["projection-mismatch"])` and trim its doc comment's parenthetical (the fallback sentence stays: `aat-missing` still takes down the snippet layer without a snippet-specific code). Update any test asserting a `snippet:` prefix.

- [ ] **Step 4: Run tests**: `cargo test -p ab-morph-run 2>&1 | tail -3` — expected: PASS.

- [ ] **Step 5: Amend the spec.** In the error table, change the `projection-mismatch` row's meaning to "re-projected char count ≠ `sources.source_chars`, **or an example span outside the projected text**" (effect column unchanged).

- [ ] **Step 6: Commit**

```bash
git add crates/ab-morph-run/src/hydrate/ docs/superpowers/specs/2026-07-10-hydrate-interesting-examples-design.md
git commit -m "fix(hydrate): out-of-range example spans report projection-mismatch, not snippet:"
```

---

### Task 9 (controller-executed, not a subagent task): real-run validation and efficiency measurement

- [ ] Build the release binary at the branch head: `cargo build --release -p ab-morph-run`.
- [ ] calib-triage-1000 (schema v1, 668 MB feature diffs): `AB_DUCKDB_BIN=… target/release/ab-morph-run summarize-warehouse-interesting --run-dir /db/ab-validator/morph-warehouse/runs/calib-triage-1000 --engine duckdb --limit 10` — must succeed (was the reproduced Binder Error).
- [ ] full-2026-07-06_160136-jobs8 (schema v2, 14 GB feature diffs): same command under `/usr/bin/time -v`; record wall clock and max RSS. This is the "efficient at full scale" evidence — the in-memory engine cannot do this run.
- [ ] Spot-check determinism: run the calib command twice with `--format json --output <scratch>/a.json` / `b.json`; files byte-identical.

## Out of scope (recorded, not fixed here)

- `crates/ab-warehouse/sql/morph_views.sql`'s `UNNEST(analyzers)` base view is v3-only; pre-v3 run dirs need the pre-v3 view from their own era. Ad-hoc analysis concern, not a reader-code path.
- Skipping the UNNEST→`list()` round-trip for v3 data in the re-collapsing consumers (a possible optimization; byte-identical-output discipline makes it riskier than it is valuable today).
