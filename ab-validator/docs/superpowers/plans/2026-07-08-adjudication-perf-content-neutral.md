# Adjudication Perf — Content-Neutral Refactors — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Reduce adjudication (row-building) CPU on the warehouse fact path via three content-neutral refactors, with byte/row-identical output.

**Architecture:** Three independent levers on the oracle/n-way row-building path — defer oracle evidence-map construction to the emit path; replace a per-document grouping `BTreeMap` with a linear scan over already-contiguous runs; and build Arrow columns directly for the two giant tables instead of materializing intermediate `Vec<RowStruct>`. Each is proven equivalent by a differential test against the retained old implementation and quantified by a criterion micro-bench.

**Tech Stack:** Rust workspace `ab-validator/`, crates `ab-morph-run` (oracle, warehouse row builders, pattern accumulator) and `ab-warehouse` (arrow/parquet writer). `arrow`/`parquet` v56, criterion benches.

Reference spec: `docs/superpowers/specs/2026-07-08-adjudication-perf-content-neutral-design.md`.

## Global Constraints

- **CONTENT-NEUTRAL — the binding bar.** No emitted row, value, row count, column, null bitmap, or analyzer-list element order may change for ANY table. These are refactors of *how* rows are built, never *what* is emitted.
- **Two levels of identity — do not conflate (see spec §Validation):**
  - **Unit level = literal byte identity** on a fixed, single-threaded input (the per-task differential/byte tests). For Lever 2, that means byte-identical *parquet* for fixed rows.
  - **Corpus level = value/multiset identity, NOT file-byte identity.** Full-corpus parquet is not byte-stable run-to-run (parallel sharding/merge reorder rows), so the corpus bar is an order-independent multiset fingerprint per table (`count` + `sum(hash(*))` + `bit_xor(hash(*))`), NOT `du`/row-count. Row *order* is expected to differ; row *contents as a multiset* must not.
- **Byte-identity constraints (Lever 2 especially):** preserve schema field order + count, nullable-column null handling (`append_option`/`append_null`), the `Utf8`/`UInt64` physical types (do NOT switch to dictionary), and the flush/batch boundaries (50k rows for morpheme features; 10k regions for n-way facts) and `WAREHOUSE_MAX_ROW_GROUP_SIZE = 50_000`.
- **Lever 2 is PRODUCER-SIDE:** the `Arc`-clone atomic traffic lives in the row producers (`rows.rs`), not the writer transposition. A writer-only change that keeps the `Vec<RowStruct>` producer is OUT OF SCOPE — it leaves the target cost intact. The producer must append directly into the column builder; the `Vec<Row>` intermediate is removed.
- **Genuine differential tests:** for #1, #2, and #3, RETAIN the old implementation as a private `#[cfg(test)] *_reference` function the test calls, so `assert_eq!(new, reference)` is a real equivalence check, not self-referential. Keep the reference for this round.
- **Add a criterion micro-bench per lever** in the owning crate, quantifying the win. (`ab-warehouse` has no bench harness yet — add `criterion.workspace = true` under `[dev-dependencies]` and an explicit `[[bench]]` entry; commit `Cargo.toml`.)
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

- [ ] **Step 1: Retain the old loop as `adjudicate_reference` and add a full-row differential test**

Before refactoring, copy the current `adjudicate` body verbatim into a private `#[cfg(test)] fn adjudicate_reference(...)` with the identical signature. Then add a differential test that feeds BOTH `adjudicate` and `adjudicate_reference` the same input and asserts the returned row `Vec`s are **exactly equal** — full `NwayRegionOracleEvidenceRow` equality including the byte-for-byte `evidence_detail` String and the `losing_analyzers` element order (NOT a deserialize-and-spot-check; compare the whole rows with `assert_eq!`). The input MUST include: a fully-matching base (emits nothing), a single-loser base, and a **multiple-losers** base (≥2 analyzers mismatch, to exercise `losing_analyzers` ordering and multi-entry `detail`). Reuse the fixtures of `all_match_emits_nothing` / `evidence_detail_keeps_raw_reading`.

- [ ] **Step 2: Run to confirm reference == current (baseline green)**

Run: `cargo test -p ab-morph-run oracle::ruby`
Expected: PASS (reference is a verbatim copy of current → trivially equal, establishing the golden comparison).

- [ ] **Step 3: Refactor the per-analyzer loop to defer map/clone work**

In `adjudicate` (`ruby.rs:206-234`): in the per-analyzer loop, compute `analyzer_reading()` and `is_match` as today, but do NOT build the `detail` map entries or clone winner/loser name Strings there. Instead retain per analyzer the minimal data needed to rebuild evidence on the emit path (e.g. a small `Vec<(analyzer_handle, Reading, is_match)>` holding references/handles, not owned clones), and track `any_loser`. Keep the emit gate `if !any_loser { continue; }`. AFTER the gate, build the `detail: BTreeMap` and the winner/loser `Vec<String>` exactly as the current `234-264` code does — in the same iteration order — then serialize and push the row unchanged. `adjudicate_reference` stays as-is (the differential test now compares the two implementations).

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

- [ ] **Step 3: Replace the map with a linear run-scan (release-safe invariant)**

In `record`, replace the `groups: BTreeMap<...>` build-and-iterate (`lib.rs:556-573`) with a single linear pass over the core-key-filtered `feature_diffs` that detects run boundaries where the `WarehouseFeatureGroupKey` changes (via `slice::chunk_by` on the key, or a manual boundary scan), processing each run exactly as the old `for (group, facts) in groups` body did. Guards (BOTH required — a debug-only assert is insufficient because a future producer change could silently under-merge in release):
  - **Release-safe:** assert `region_index` is monotone non-decreasing across the scan (O(1) state; the primary contiguity guarantee) and return an error / bail rather than silently miscount if violated.
  - **Dev tripwire:** a `debug_assert!` (behind `cfg(debug_assertions)`, `HashSet` of seen keys) that no group key recurs non-adjacently.
Document on `record` that it relies on `push_region_rows` emitting maximal contiguous runs, naming the Step 3b test.

- [ ] **Step 3b: Add a producer-invariant test (CI guard)**

Add a test that runs the REAL producer `push_region_rows` over representative multi-region input (multiple regions, multiple feature keys/scopes per region) and asserts the emitted `feature_diffs` form maximal contiguous group-key runs: `region_index` monotone non-decreasing, and no `WarehouseFeatureGroupKey` recurs non-adjacently. This fails CI if a future producer change breaks the invariant `record` depends on.

- [ ] **Step 4: Run the tests**

Run: `cargo test -p ab-morph-run feature_pattern`
Expected: PASS — new linear path == reference on all inputs (including the interleaved stress case), and the producer-invariant test green.

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
- Modify: `crates/ab-warehouse/src/writer.rs` (`append_morpheme_features` `315-336`, array helpers `952-985`; expose a batch-append)
- Modify: `crates/ab-morph-run/src/warehouse/rows.rs` (`morpheme_feature_rows_for_range` `129-164`) and its call site (`pipeline.rs:945-967`) — **required**: the producer appends directly into the column builder (this is where the `Arc`-clone traffic is; see Global Constraints).
- Add: `crates/ab-warehouse/Cargo.toml` bench setup; `crates/ab-warehouse/benches/warehouse_columns.rs`
- Test: `crates/ab-warehouse/src/writer.rs` `#[cfg(test)]`

**Interfaces:**
- Produces: byte-identical `morpheme_features.parquet`; the `Vec<MorphemeFeatureRow>` intermediate is eliminated.

- [ ] **Step 1: Add a byte-identity characterization test (retained reference path)**

Keep the CURRENT `Vec<Row>`→transposition as a private `#[cfg(test)] fn append_morpheme_features_reference(&mut self, &[MorphemeFeatureRow])`. In `writer.rs` tests, build a fixed `Vec<MorphemeFeatureRow>` (covering: repeated ids, a `None` `feature_value`, multiple analyzers/morphemes). Write it to a temp parquet via `append_morpheme_features_reference` (reference bytes) and via the NEW direct-builder path (fed the same logical rows), then `assert_eq!` the two files' bytes (or compare `sha256`).

- [ ] **Step 2: Run to confirm the reference path is green (golden bytes)**

Run: `cargo test -p ab-warehouse morpheme_features`
Expected: PASS.

- [ ] **Step 3: Add a direct column builder and make the PRODUCER build it directly**

Add a `MorphemeFeaturesColumns` builder (7 arrow builders in schema order — `StringBuilder` for `run_id, source_id, text_id, analyzer_id, feature_key`, `UInt64Builder` for `morpheme_index`, nullable `StringBuilder` for `feature_value` via `append_option`), with `push_row(...)` (or per-field `push`) and `finish() -> RecordBatch`. Change `morpheme_feature_rows_for_range` (and its `pipeline.rs` caller) to **append each morpheme-feature directly into the builder** instead of returning `Vec<MorphemeFeatureRow>`; the writer exposes a method that writes the finished `RecordBatch` (via `write_batch`, recording `write_time`). The `Vec<MorphemeFeatureRow>` intermediate MUST be gone from the production path. **Preserve** the 50k flush granularity and exact column order. A writer-only change that keeps the `Vec<Row>` producer does NOT satisfy this task.

- [ ] **Step 4: Run the byte-identity test + existing writer tests**

Run: `cargo test -p ab-warehouse -p ab-morph-run`
Expected: PASS — new path bytes == reference bytes; `empty_parquet_schemas_match_documented_columns` and round-trip tests still green; pipeline tests green.

- [ ] **Step 5: Add criterion bench setup + a micro-bench**

In `crates/ab-warehouse/Cargo.toml` add `criterion.workspace = true` under `[dev-dependencies]` and a `[[bench]] name = "warehouse_columns" harness = false` entry. Add `benches/warehouse_columns.rs` benching the direct build+append of a large synthetic `morpheme_features` batch (e.g. 500k rows). Run `cargo bench -p ab-warehouse --bench warehouse_columns` once; record the number.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-warehouse/src/writer.rs crates/ab-warehouse/Cargo.toml crates/ab-warehouse/benches/ crates/ab-morph-run/src/warehouse/rows.rs crates/ab-morph-run/src/pipeline.rs
git commit -m "perf(warehouse): build morpheme_features arrow columns directly in the producer"
```

---

### Task 4: Lever 2b — direct Arrow columns for `nway_feature_diffs`

**Files:**
- Modify: `crates/ab-warehouse/src/writer.rs` (`append_nway_feature_diffs` `420-444`)
- Modify: `crates/ab-morph-run/src/warehouse/rows.rs` (`push_region_rows` diffs loop `307-328`) and the n-way batch driver (`lib.rs:453-489`) — **required**: the producer appends directly into the column builder.
- Modify: `crates/ab-warehouse/Cargo.toml` (bench harness added in Task 3); `crates/ab-warehouse/benches/warehouse_columns.rs`
- Test: `crates/ab-warehouse/src/writer.rs` `#[cfg(test)]`

**Interfaces:**
- Produces: byte-identical `nway_feature_diffs.parquet`; the `Vec<NwayFeatureDiffRow>` intermediate is eliminated. `analyzer_id` stays scalar (schema unchanged — #4 is deferred).

- [ ] **Step 1: Add a byte-identity characterization test (retained reference path)**

As Task 3 Step 1, for `nway_feature_diffs`: keep the current transposition as `#[cfg(test)] fn append_nway_feature_diffs_reference`. Fixed `Vec<NwayFeatureDiffRow>` covering `None` `scope_position`, `None` `scope_surface`, `None` `feature_value`, and multiple analyzers per value-group. Assert new direct-builder path bytes == reference path bytes.

- [ ] **Step 2: Run baseline green**

Run: `cargo test -p ab-warehouse nway_feature_diffs`
Expected: PASS.

- [ ] **Step 3: Add the direct column builder (10 columns) and make the PRODUCER build it directly**

`NwayFeatureDiffsColumns` with the 10 arrow builders in schema order — `StringBuilder` for `run_id, source_id, text_id, feature_key, scope_type, analyzer_id`, `UInt64Builder` for `region_index`, nullable `UInt64Builder` for `scope_position` (`append_option`), nullable `StringBuilder` for `scope_surface` and `feature_value`. `push_row`/per-field `push` + `finish` → `RecordBatch` via `nway_feature_diffs_schema()`, written via the writer's batch-append. Change `push_region_rows` (and the n-way batch driver in `lib.rs:453-489`) to append directly into the builder — no `Vec<NwayFeatureDiffRow>` in the production path. Preserve the 10k-region flush granularity and exact column order. (`analyzer_id` stays a scalar column this round.)

- [ ] **Step 4: Run tests**

Run: `cargo test -p ab-warehouse -p ab-morph-run`
Expected: PASS — new bytes == reference; schema/round-trip + pipeline tests green.

- [ ] **Step 5: Add a criterion micro-bench**

Add an `nway_feature_diffs` case to `benches/warehouse_columns.rs` (bench harness already registered in Task 3) — direct build+append of a large synthetic batch. Run `cargo bench -p ab-warehouse --bench warehouse_columns` once; record the number.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-warehouse/src/writer.rs crates/ab-warehouse/Cargo.toml crates/ab-warehouse/benches/ crates/ab-morph-run/src/warehouse/rows.rs crates/ab-morph-run/src/lib.rs
git commit -m "perf(warehouse): build nway_feature_diffs arrow columns directly in the producer"
```

---

## Validation Record (hinoki) — 2026-07-08

**Scope note.** By the time this round reached its full-corpus run, `main` had advanced past the merged perf
round (`68761e1`) with changes that alter emitted output *unconditionally*: the Phase-B sentence splitter
(`sentences.rs` inline-container / whitespace-only paragraph split) and the new `adjudicated_reading` column
on `nway_region_oracle_evidence`. A `main`-baseline **content-parity** check was therefore no longer meaningful —
it would mismatch by design, not by regression. The perf round's content-neutrality is already established at the
unit level (retained `*_reference` fns + byte-identity / full-row differential tests, all green, merged), so the
corpus run was reframed to its remaining purpose: a **cumulative performance measurement**. `--ortho-detect`
stays at its `Off` default in both runs, so it contributes nothing to the delta.

**Runs** (both: 4 analyzers = vibrato + vibrato:unidic-novel-202512 + sudachi-a + sudachi-c, `--jobs 0`
[auto-jobs 18], `--parquet-zstd-level 3`, same corpus, GNU `time -v`):

- **Baseline** `c969fd5` (pre-follow-up) — run_id `adjperf-base`, EXIT=0
- **Head** `6c3f0d2` (latest `main`: perf round + folded-in changes) — run_id `adjperf-head`, EXIT=0

| Metric | `c969fd5` | `6c3f0d2` | Δ |
| --- | --- | --- | --- |
| Wall clock | 45:41.7 | 30:48.7 | **−32.6 %** (−14m53s) |
| Max RSS | 34.1 GiB | 32.5 GiB | −4.7 % |
| phase total (Σ 590 shards) | 43106.6 s | 28488.3 s | −33.9 % |
| — analysis | 4692.7 s (10.9 %) | 3919.5 s (13.8 %) | −16.5 % |
| — **adjudication** | 32217.5 s (74.7 %) | 18508.1 s (65.0 %) | **−42.6 %** (−13709 s) |
| — warehouse-write | 4504.1 s (10.4 %) | 4429.6 s (15.5 %) | −1.7 % (flat) |
| — other | 1692.2 s (3.9 %) | 1631.0 s (5.7 %) | −3.6 % |

**Reading.** The adjudication phase — the lever this round targeted — fell −42.6 % (−13,709 s summed), which
accounts for essentially the entire wall-clock win. Its share of total CPU dropped 74.7 % → 65.0 %.
Warehouse-write stayed flat (−1.7 %), reconfirming the earlier finding that write was never the bottleneck.
Baseline numbers (wall 45:41, adjudication 74.7 %) are consistent with the earlier `ab7ec42` reference
(wall 43:09, adjudication 74.3 %).

**Row counts** (informational — parity NOT asserted; equal counts ≠ byte-identity): all 11 tables came out
**identical** base vs head (sources 17,885; projection_spans 10,832,338; morphemes 662,984,226;
morpheme_features 12,575,103,913; nway_regions 161,142,784; nway_feature_diffs 23,356,986,673;
nway_region_oracle_evidence 2,329,135; feature_pattern_counts 8,394,223; errors 0). Reassuring that the
folded-in semantic changes did not perturb scale on this corpus, but not a content-neutrality claim.

## Deferred (documented, not this round)

- **#4 Per-analyzer row-collapse** (`nway_feature_diffs` → `analyzers VARCHAR[]`) — own spec next: breaking schema migration (`SCHEMA_VERSION 2→3`), coordinated consumer rewrites (`interesting_sql.rs`, `summary_body.rs`, `morph_views.sql` incl. `top_feature_differences` `UNNEST`), semantic-diff validation. Highest single payoff (~25–50% fewer n-way rows). Note: it will revisit Task 4's n-way arrow builder.
- Key-pruning — **foreclosed** (keep all feature keys).
- ZSTD/write-path tuning, Lever 2 (intra-worker analyzer parallelism) — foreclosed by the measured split.
