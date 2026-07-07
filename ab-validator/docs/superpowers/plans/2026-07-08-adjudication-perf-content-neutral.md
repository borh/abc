# Adjudication Perf — Content-Neutral Refactors — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Reduce adjudication (row-building) CPU on the warehouse fact path via three content-neutral refactors, with byte/row-identical output.

**Architecture:** Three independent levers on the oracle/n-way row-building path — defer oracle evidence-map construction to the emit path; replace a per-document grouping `BTreeMap` with a linear scan over already-contiguous runs; and build Arrow columns directly for the two giant tables instead of materializing intermediate `Vec<RowStruct>`. Each is proven equivalent by a differential test against the retained old implementation and quantified by a criterion micro-bench.

**Tech Stack:** Rust workspace `ab-validator/`, crates `ab-morph-run` (oracle, warehouse row builders, pattern accumulator) and `ab-warehouse` (arrow/parquet writer). `arrow`/`parquet` v56, criterion benches.

Reference spec: `docs/superpowers/specs/2026-07-08-adjudication-perf-content-neutral-design.md`.

## Global Constraints

- **CONTENT-NEUTRAL — the binding bar.** No emitted row, value, row count, column, ordering, null bitmap, or on-disk parquet byte may change for ANY table. These are refactors of *how* rows are built, never *what* is emitted.
- **Byte-identity constraints (Lever 2 especially):** preserve schema field order + count, nullable-column null handling (`append_option`/`append_null`), the `Utf8`/`UInt64` physical types (do NOT switch to dictionary), and the flush/batch boundaries (50k rows for morpheme features; 10k regions for n-way facts) and `WAREHOUSE_MAX_ROW_GROUP_SIZE = 50_000`.
- **Genuine differential tests:** for #1 and #2, RETAIN the old implementation as a private `*_reference` function the test calls, so `assert_eq!(new, reference)` is a real equivalence check, not self-referential. Remove the reference fn only if/when a later round retires it; for this round keep it (behind `#[cfg(test)]` if it would otherwise be dead code).
- **Add a criterion micro-bench per lever** in the owning crate, quantifying the win.
- **Do NOT** touch the `nway_feature_diffs` schema (that is the deferred #4 row-collapse). This round keeps `analyzer_id` scalar.
- Match surrounding code style.

## File Structure

- `crates/ab-morph-run/src/oracle/ruby.rs` — Lever 3 (defer evidence-map build).
- `crates/ab-morph-run/src/lib.rs` — Lever 1 (`WarehouseFeaturePatternAccumulator` linear scan).
- `crates/ab-morph-run/src/warehouse/rows.rs` + `crates/ab-warehouse/src/writer.rs` — Lever 2 (direct arrow column builders for `morpheme_features` and `nway_feature_diffs`).
- `crates/ab-morph-run/benches/` and/or `crates/ab-warehouse/benches/` — new micro-benches.

---

### Task 1: Lever 3 — defer oracle evidence-map construction to the emit path

**Files:**
- Modify: `crates/ab-morph-run/src/oracle/ruby.rs` (per-analyzer loop `210-230`, emit gate `232`, emit-path build `234-277`)
- Test: same file's `#[cfg(test)]` module (existing `all_match_emits_nothing` at `~397`)
- Bench: `crates/ab-morph-run/benches/` (new `oracle_adjudicate.rs` or add to an existing bench)

**Interfaces:**
- Consumes: unchanged `adjudicate` signature and inputs.
- Produces: identical `nway_region_oracle_evidence` rows; only the intermediate allocation timing changes.

- [ ] **Step 1: Add a match-heavy differential/behavior test**

In the `ruby.rs` test module, add a test that adjudicates an input where the FIRST base fully matches (all analyzers agree → emits nothing) and a LATER base mismatches (emits a row), asserting: (a) exactly one row emitted, (b) its `evidence_detail` deserializes to the same `ruby_base`/`ruby_reading`/`ruby_reading_norm`/`classification`/`per_analyzer` as before. Reuse the fixtures/helpers of the existing `all_match_emits_nothing` and `evidence_detail_keeps_raw_reading` tests. This pins that deferring the map build changes nothing on either path.

- [ ] **Step 2: Run the test against current code to confirm it passes (baseline green)**

Run: `cargo test -p ab-morph-run oracle::ruby`
Expected: PASS (documents current behavior before the refactor).

- [ ] **Step 3: Refactor the per-analyzer loop to defer map/clone work**

In `adjudicate` (`ruby.rs:206-234`): in the per-analyzer loop, compute `analyzer_reading()` and `is_match` as today, but do NOT build the `detail` map entries or clone winner/loser name Strings there. Instead retain per analyzer the minimal data needed to rebuild evidence on the emit path (e.g. a small `Vec<(analyzer_handle, Reading, is_match)>` holding references/handles, not owned clones), and track `any_loser`. Keep the emit gate `if !any_loser { continue; }`. AFTER the gate, build the `detail: BTreeMap` and the winner/loser `Vec<String>` exactly as the current `234-264` code does, then serialize and push the row unchanged.

Keep `analyzer_reading()` running for every analyzer (it yields `is_match`).

- [ ] **Step 4: Run the oracle tests**

Run: `cargo test -p ab-morph-run oracle::ruby`
Expected: PASS — including `all_match_emits_nothing`, `evidence_detail_keeps_raw_reading`, the classification tests, `oracle_pipeline_from_aat_to_parquet`, and the new Step 1 test.

- [ ] **Step 5: Add a criterion micro-bench**

Add a bench that calls `adjudicate` on a match-heavy input (e.g. 1000 bases, ~34% fully-matching to mirror corpus) and reports throughput. Register it in the crate's `Cargo.toml` `[[bench]]`. Run `cargo bench -p ab-morph-run --bench oracle_adjudicate` once to confirm it builds and record the number in the commit message.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-morph-run/src/oracle/ruby.rs crates/ab-morph-run/benches/ crates/ab-morph-run/Cargo.toml
git commit -m "perf(oracle): defer ruby evidence-map build to the emit path"
```

---

### Task 2: Lever 1 — `WarehouseFeaturePatternAccumulator` BTreeMap → linear scan

**Files:**
- Modify: `crates/ab-morph-run/src/lib.rs` (`record` at `542-600`, target map `556-573`)
- Test: `crates/ab-morph-run/src/lib.rs` `#[cfg(test)]` (none exists for this accumulator — add)
- Bench: `crates/ab-morph-run/benches/` (add an accumulator bench)

**Interfaces:**
- Consumes: `&[NwayFeatureDiffRow]` (already contiguous by group key), unchanged.
- Produces: identical `feature_pattern_counts` output (`into_rows()`).

- [ ] **Step 1: Add a differential test with a retained reference implementation**

Extract the current `groups: BTreeMap` grouping into a private `#[cfg(test)] fn group_feature_diffs_reference(diffs) -> Vec<(WarehouseFeatureGroupKey, Vec<&NwayFeatureDiffRow>)>` (the map-based version). Write a test that builds a hand-crafted `feature_diffs` slice — INCLUDING core and non-core keys, multiple regions, multiple scopes, and an interleaved ordering that would break a naive scan if runs were not maximal — feeds it through both a fresh `WarehouseFeaturePatternAccumulator` (new linear path) and one using the reference grouping, and asserts `into_rows()` is byte-identical between them. Also assert the maximal-runs invariant directly (no two non-adjacent entries share a group key in the emitted order).

- [ ] **Step 2: Run to confirm it passes on current code**

Run: `cargo test -p ab-morph-run feature_pattern`
Expected: PASS (reference == current, documenting baseline).

- [ ] **Step 3: Replace the map with a linear run-scan**

In `record`, replace the `groups: BTreeMap<...>` build-and-iterate (`lib.rs:556-573`) with a single linear pass over the core-key-filtered `feature_diffs` that detects run boundaries where the `WarehouseFeatureGroupKey` changes (via `slice::chunk_by` on the key, or a manual boundary scan), processing each run exactly as the old `for (group, facts) in groups` body did. Add a `debug_assert!` that each new run's key was not seen earlier in this `record` call (maximal-runs invariant) — a `HashSet` of seen keys behind `cfg(debug_assertions)` is acceptable since it is debug-only.

- [ ] **Step 4: Run the test**

Run: `cargo test -p ab-morph-run feature_pattern`
Expected: PASS — new linear path == reference on all inputs, including the interleaved stress case.

- [ ] **Step 5: Add a criterion micro-bench**

Bench `record` (or `into_rows` end-to-end) over a synthetic multi-region `feature_diffs` slice. Register and run once; record the number.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-morph-run/src/lib.rs crates/ab-morph-run/benches/ crates/ab-morph-run/Cargo.toml
git commit -m "perf(warehouse): linear-scan feature-pattern grouping over contiguous runs"
```

---

### Task 3: Lever 2a — direct Arrow columns for `morpheme_features`

**Files:**
- Modify: `crates/ab-warehouse/src/writer.rs` (`append_morpheme_features` `315-336`, array helpers `952-985`)
- Modify: `crates/ab-morph-run/src/warehouse/rows.rs` (`morpheme_feature_rows_for_range` `129-164`) and its call site (`pipeline.rs:945-967`) — only if the producer is changed to append directly; otherwise leave the builder and change only the writer's transposition.
- Test: `crates/ab-warehouse/src/writer.rs` `#[cfg(test)]`
- Bench: `crates/ab-warehouse/benches/` (new)

**Interfaces:**
- Produces: byte-identical `morpheme_features.parquet`.

- [ ] **Step 1: Add a byte-identity characterization test**

In `writer.rs` tests, build a fixed `Vec<MorphemeFeatureRow>` (covering: repeated ids, a `None` `feature_value`, multiple analyzers/morphemes). Write it to a temp parquet via the CURRENT `append_morpheme_features` path (call it the reference bytes), and via the NEW direct-builder path, then assert the two output files are byte-identical (read both files' bytes, `assert_eq!`). Since Step 3 replaces the old path, capture the reference bytes in the test by keeping the old column-transposition as a private `#[cfg(test)] fn append_morpheme_features_reference` producing the same batch.

- [ ] **Step 2: Run to confirm the reference path is captured (green baseline)**

Run: `cargo test -p ab-warehouse morpheme_features`
Expected: PASS (reference path == itself; establishes the golden bytes).

- [ ] **Step 3: Introduce a direct column builder and switch the append path**

Add a `MorphemeFeaturesColumns` builder holding the 7 arrow builders in schema order — `StringBuilder` for `run_id, source_id, text_id, analyzer_id, feature_key`, `UInt64Builder` for `morpheme_index`, and a nullable `StringBuilder` for `feature_value` (use `append_option`/`append_null`). Give it `push_row(&MorphemeFeatureRow)` and `finish() -> Result<()>` that builds the `RecordBatch` (schema `morpheme_features_schema()`) and writes it via `write_batch` (recording `write_time`). Route `append_morpheme_features` through it. **Preserve** the 50k flush granularity and column order exactly. (Producer-side: if it is cleaner to have `pipeline.rs` push directly into the builder instead of building a `Vec<MorphemeFeatureRow>` first, do so — but the minimal change is to keep the `Vec` builder and only replace the writer's multi-pass transposition; choose the smaller diff that still removes the intermediate, and note which in the report.)

- [ ] **Step 4: Run the byte-identity test + existing writer tests**

Run: `cargo test -p ab-warehouse`
Expected: PASS — new path bytes == reference bytes; `empty_parquet_schemas_match_documented_columns` and round-trip tests still green.

- [ ] **Step 5: Add a criterion micro-bench**

Bench appending a large synthetic `morpheme_features` batch (e.g. 500k rows) through the new path. Register and run once; record the number.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-warehouse/src/writer.rs crates/ab-warehouse/benches/ crates/ab-warehouse/Cargo.toml crates/ab-morph-run/src/warehouse/rows.rs crates/ab-morph-run/src/pipeline.rs
git commit -m "perf(warehouse): build morpheme_features arrow columns directly"
```

---

### Task 4: Lever 2b — direct Arrow columns for `nway_feature_diffs`

**Files:**
- Modify: `crates/ab-warehouse/src/writer.rs` (`append_nway_feature_diffs` `420-444`)
- Modify: `crates/ab-morph-run/src/warehouse/rows.rs` (`push_region_rows` diffs loop `307-328`) and the n-way batch driver (`lib.rs:453-489`) — only if the producer appends directly.
- Test: `crates/ab-warehouse/src/writer.rs` `#[cfg(test)]`
- Bench: `crates/ab-warehouse/benches/`

**Interfaces:**
- Produces: byte-identical `nway_feature_diffs.parquet`. `analyzer_id` stays scalar (schema unchanged).

- [ ] **Step 1: Add a byte-identity characterization test**

As Task 3 Step 1, for `nway_feature_diffs`: fixed `Vec<NwayFeatureDiffRow>` covering `None` `scope_position`, `None` `scope_surface`, `None` `feature_value`, multiple analyzers. Reference bytes via a retained `#[cfg(test)] fn append_nway_feature_diffs_reference`; assert new path == reference bytes.

- [ ] **Step 2: Run baseline green**

Run: `cargo test -p ab-warehouse nway_feature_diffs`
Expected: PASS.

- [ ] **Step 3: Introduce the direct column builder (10 columns) and switch the path**

`NwayFeatureDiffsColumns` with the 10 arrow builders in schema order — `StringBuilder` for `run_id, source_id, text_id, feature_key, scope_type, analyzer_id`, `UInt64Builder` for `region_index`, nullable `UInt64Builder` for `scope_position` (`append_option`), nullable `StringBuilder` for `scope_surface` and `feature_value`. `push_row` + `finish` → `RecordBatch` via `nway_feature_diffs_schema()`, written through `write_batch`. Preserve the 10k-region flush granularity and column order.

- [ ] **Step 4: Run tests**

Run: `cargo test -p ab-warehouse`
Expected: PASS — new bytes == reference; schema/round-trip tests green.

- [ ] **Step 5: Add a criterion micro-bench**

Bench a large synthetic `nway_feature_diffs` batch through the new path. Register and run once; record the number.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-warehouse/src/writer.rs crates/ab-warehouse/benches/ crates/ab-morph-run/src/warehouse/rows.rs crates/ab-morph-run/src/lib.rs
git commit -m "perf(warehouse): build nway_feature_diffs arrow columns directly"
```

---

## Validation Record (hinoki) — fill after merge + build

The content-neutral bar: every emitted table must be **row/byte-identical** to a current-`main` baseline.

- [ ] Build the branch on hinoki (release). Confirm the 4 micro-benches' local numbers are recorded in commits.
- [ ] **Full-corpus run** on the branch (auto-jobs, zstd 3): record wall, peak RSS, and the `phase-timings` line.
      - wall: ____   RSS: ____   analysis %: ____   **adjudication %: ____** (expect a fall vs 74.3%)   warehouse-write %: ____   other %: ____
- [ ] **Parity vs a current-`main` baseline run** via `scripts/oracle-validation-diff.sh`:
      - all 10 non-oracle tables **row-identical** (incl. `feature_pattern_counts`, `morpheme_features`, `nway_feature_diffs`): PASS/FAIL ____
      - oracle keyed 4-bucket diff: dropped=0, newly_emitted=0, classification_changed=0: PASS/FAIL ____
- [ ] **Byte spot-check:** `du -sb` of the two runs' `morpheme_features` / `nway_feature_diffs` / `feature_pattern_counts` dirs match (or explain any parquet-metadata-only delta). ____
- [ ] Record the aggregate adjudication-CPU delta and wall-clock delta vs the `ab7ec42` baseline (wall 43:09, adjudication 74.3%).

## Deferred (documented, not this round)

- **#4 Per-analyzer row-collapse** (`nway_feature_diffs` → `analyzers VARCHAR[]`) — own spec next: breaking schema migration (`SCHEMA_VERSION 2→3`), coordinated consumer rewrites (`interesting_sql.rs`, `summary_body.rs`, `morph_views.sql` incl. `top_feature_differences` `UNNEST`), semantic-diff validation. Highest single payoff (~25–50% fewer n-way rows). Note: it will revisit Task 4's n-way arrow builder.
- Key-pruning — **foreclosed** (keep all feature keys).
- ZSTD/write-path tuning, Lever 2 (intra-worker analyzer parallelism) — foreclosed by the measured split.
