# Adjudication Perf — Content-Neutral Refactors — Design

**Status:** design (pending user review)
**Date:** 2026-07-08
**Predecessor:** `2026-07-07-warehouse-write-perf-design.md` — its hinoki Validation Record
(measured on `ab7ec42`) established the run is **adjudication-bound (74.3% of summed CPU)**,
not write-bound. Analysis (11%) and warehouse-write (11%) levers are foreclosed; the target is
the oracle / n-way **row-building** path.

## Problem

The dominant cost is constructing the billions of warehouse fact rows (n-way feature diffs,
morpheme features, oracle evidence). Three refactors on that path are **content-neutral** — they
change *how* rows are built, not *what* is emitted — so every table's output must remain
byte/row-identical. (The one non-neutral lever, per-analyzer row-collapse, is a schema migration
deferred to its own spec — see Non-goals.)

## Scope (this round)

Three content-neutral levers, each validated by a differential test (old == new), a criterion
micro-bench, and a final full-corpus hinoki parity run:

1. **BTreeMap → linear scan** in `WarehouseFeaturePatternAccumulator`.
2. **Direct Arrow column building** for `morpheme_features` and `nway_feature_diffs` (replacing the
   intermediate `Vec<RowStruct>` transposition).
3. **Defer oracle evidence-map construction** to the emit path in `oracle::ruby::adjudicate`.

### Non-goals / deferred

- **#4 Per-analyzer row-collapse** (`nway_feature_diffs` → `analyzers VARCHAR[]`) — highest single
  payoff (~25–50% fewer rows) but a **breaking schema migration** (`SCHEMA_VERSION 2→3`, coordinated
  consumer rewrites, `top_feature_differences` view semantics via `UNNEST`, semantic-diff validation
  rather than byte-identity). Its own spec, next round.
- Key-pruning — **foreclosed** (keep all feature keys).
- ZSTD/write-path tuning, Lever 2 (intra-worker analyzer parallelism) — foreclosed by the measured split.

## Design

### Lever 1 — `WarehouseFeaturePatternAccumulator`: BTreeMap → linear scan

- **Site:** `ab-validator/crates/ab-morph-run/src/lib.rs:542-600`; the target is the per-`record`
  `groups: BTreeMap<WarehouseFeatureGroupKey, Vec<&NwayFeatureDiffRow>>` at `lib.rs:556-573`.
- **Change:** the `feature_diffs` slice arrives already grouped in **contiguous runs** by
  `(region_index, feature_key, scope_*)` — `region_index` is monotone and each `feature_group` is
  emitted once as a block (`warehouse/rows.rs:307-328`). Replace the map-then-iterate with a linear
  `chunk_by` (or manual run-boundary scan) over the slice, filtered to `WAREHOUSE_CORE_FEATURE_KEYS`.
- **Correctness invariant (release-safe, not debug-only):** equivalence requires runs to be *maximal*
  — no group key recurs non-adjacently in the slice `record` receives. A debug-only assertion is
  insufficient: in release, a future producer change that broke contiguity would **silently
  under-merge / drop patterns**. Two guards, both required:
  1. A **producer-level property test** proving `push_region_rows` always emits `feature_diffs` in
     maximal contiguous group-key runs (monotone `region_index`; each region's `feature_groups`
     visited once with distinct keys) on representative input. This fails CI if a future producer
     change breaks the invariant `record` depends on.
  2. A **release-safe local guard inside `record`**: assert `region_index` is monotone non-decreasing
     across the scan (O(1) state, the primary contiguity guarantee) and error rather than silently
     miscount if violated; keep the fuller `debug_assert` (no non-adjacent key recurrence) as the
     dev tripwire. Document that `record` relies on the producer invariant, naming the test.
- **Content-neutrality:** final `feature_pattern_counts` output drains `self.patterns` (itself a
  `BTreeMap`) and aggregates commutatively (`+=`, `BTreeSet::insert`, sorted pattern strings), so it is
  independent of `groups` iteration order → byte-identical.
- **Test gap:** no test exercises this accumulator today. Add a characterization/differential test.

### Lever 2 — Direct Arrow column building (`morpheme_features`, `nway_feature_diffs`)

- **Sites:** builders `warehouse/rows.rs:129-164` (`morpheme_feature_rows_for_range`) and
  `rows.rs:307-328` (n-way diffs); transposition `ab-warehouse/src/writer.rs:315-336`
  (`append_morpheme_features`) and `writer.rs:420-444` (`append_nway_feature_diffs`).
- **Change (PRODUCER-SIDE, not writer-only):** the `Arc` atomic-refcount traffic this lever targets
  is incurred **in the producer** (`rows.rs:153-160` and `rows.rs:314-325` each `Arc::clone` 5–8 ids
  per row while building `Vec<RowStruct>`), *not* in the writer's transposition. A writer-only change
  (transpose `&[Row]` → columns without touching the producer) would leave that traffic intact and is
  explicitly **out of scope**. Introduce a per-table column-builder (`StringBuilder`/`UInt64Builder`
  fields, nullable via `append_option`, with `push_row(...)`/`finish() -> RecordBatch`) and change the
  **producers** to append each field directly into the builder as rows are generated — the `Vec<Row>`
  intermediate is removed entirely. The writer exposes a batch-append that the finished builder feeds
  (reuse/extend `append_record_batch`, `writer.rs:500-575`, or a thin per-table wrapper). This
  eliminates the `Vec<Row>` backing store + struct memcpy **and** the ~10¹¹-scale `Arc` clone/drop
  atomic traffic on the two hottest tables. (The string *byte* copy into the arrow value buffer is
  unchanged — `Utf8`, not dictionary — so it is not part of the win; do not switch physical type.)
- **Must preserve (byte-identity constraints):** schema field **order** and **count**; **null
  handling** for the nullable columns (`feature_value`; plus `scope_position`, `scope_surface` for
  diffs) via `append_option`/`append_null`; the `Utf8`/`UInt64` physical types; and the **flush /
  batch boundaries** (50k rows for morpheme features per `WAREHOUSE_MORPHEME_ROW_BATCH_SIZE`; 10k
  regions per `visit_nway_fact_row_batches`) and `WAREHOUSE_MAX_ROW_GROUP_SIZE = 50_000`, since
  row-group partitioning affects on-disk bytes.
- **Guardrails already present:** `empty_parquet_schemas_match_documented_columns`
  (`writer.rs`) and `RecordBatch::try_new` field/array validation. **Add** a byte-diff
  characterization test (build a fixed input through both the old and new paths; assert identical
  serialized parquet bytes) since no cross-run byte-identity test exists.
- **Independence:** the two tables are independent → two reviewable units (morpheme_features first, as
  it is the simpler 7-column shape; then the 10-column n-way diffs).
- **Note on #4 coupling:** the deferred row-collapse changes the n-way diff schema. Doing #2 for
  n-way now means #4 later revisits that builder. Accepted: #2's morpheme_features half is
  unaffected by #4, and the n-way arrow-builder is a localized change either way. Sequencing #4 after
  #2 keeps this round content-neutral.

### Lever 3 — Defer oracle evidence-map construction to the emit path

- **Site:** `ab-validator/crates/ab-morph-run/src/oracle/ruby.rs:183-280`; the per-analyzer loop
  `ruby.rs:210-230` and the emit gate `if losers.is_empty() { continue; }` at `ruby.rs:232`.
- **Correction to the lever's framing:** the `serde_json::to_string` is *already* gated (runs only
  after the emit decision, `ruby.rs:257`). The wasted work on the ~1.2M fully-matching bases (~34%)
  is building-then-discarding the `detail: BTreeMap` of `AnalyzerRubyReadingEvidence` and the
  winner/loser **name-String clones** *before* the gate.
- **Change:** in the per-analyzer loop, keep each analyzer's `Reading` + `is_match` (and track
  `any_loser` / winner count) **without** cloning names or inserting into `detail`. After the
  `losers.is_empty()` gate, build the `detail` map and winner/loser `Vec`s only on the emit path,
  exactly as today, so the serialized `evidence_detail` / `classification` / `winning_analyzer` /
  `losing_analyzers` are identical.
- **Content-neutrality:** the emit decision depends solely on `losers.is_empty()` (from per-analyzer
  `is_match`), unaffected by when `detail` is built. `analyzer_reading()` must still run per analyzer
  (it produces `is_match`).
- **Guarded by:** existing `all_match_emits_nothing` and the evidence-content tests, **plus a genuine
  full-row differential test**: retain the current loop as a `#[cfg(test)] fn adjudicate_reference`
  and assert the refactored `adjudicate` produces **exactly equal rows** — including the byte-for-byte
  `evidence_detail` JSON string and `losing_analyzers` element order — on inputs that include a
  **multiple-losers** case and a match-then-mismatch mix. Deserializing `evidence_detail` (as the
  existing tests do) is not enough: it can miss changed JSON bytes or list ordering. Add a micro-bench
  over `adjudicate` on a match-heavy input.

## Validation

Two distinct notions of identity, at two levels — do not conflate them:

- **Unit level = literal byte identity** (fixed, single-threaded input). Each lever keeps its old
  implementation as a `#[cfg(test)] *_reference` fn; the test drives a fixed input through both paths
  and asserts equality. For Lever 2 this is **parquet byte identity**: write the same fixed rows via
  the reference path and the new path to temp files and `assert_eq!` their bytes (or `sha256`). For
  Lever 1/3 it is exact structural equality of the produced rows (all columns; for oracle, the exact
  `evidence_detail` string + `losing_analyzers` order). Plus a criterion micro-bench per lever.

- **Corpus level = value/multiset identity, NOT file-byte identity.** Full-corpus parquet files are
  **not** byte-stable run-to-run even on identical code: parallel sharding and merge order vary, so
  row order and file layout differ. The correct corpus bar is therefore *multiset-of-rows* identity,
  order-independent. `scripts/oracle-validation-diff.sh` as written only checks **row counts** and
  oracle key/classification **buckets** — that can pass with changed feature values, row order,
  analyzer-list order, or `evidence_detail`. **Strengthen it** to a real value-level diff per affected
  table (`feature_pattern_counts`, `morpheme_features`, `nway_feature_diffs`, and
  `nway_region_oracle_evidence` including the full `evidence_detail` column):
  - Primary (scales to 23.4B rows): an **order-independent aggregate fingerprint** —
    `SELECT count(*), sum(hash(<all columns>)) AS h1, bit_xor(hash(<all columns>)) AS h2` over each
    table on both the baseline and branch runs; all three must match. (`sum`+`bit_xor` together guard
    against the degenerate cases of either alone; DuckDB `hash` of a `VARCHAR[]` is element-order
    sensitive, so analyzer-list reordering is caught.)
  - Confirmatory on the smaller tables: bidirectional `EXCEPT ALL` (`A EXCEPT ALL B` and
    `B EXCEPT ALL A` both empty) over all columns — exact multiset equality.
- **Content-neutral bar (PASS):** every affected table's `(count, sum-hash, bit_xor-hash)` matches the
  baseline, the oracle keyed diff shows dropped=0 / newly=0 / changed=0, AND every per-lever unit
  differential/byte test is green. Record the new `phase-timings` split (**adjudication % should
  fall**), wall-clock, and peak RSS in the plan's Validation Record. Any table-fingerprint deviation
  is a regression, not an expected change.

## Expected outcome

Lower adjudication CPU (fewer allocations + atomic bumps + map churn on the row-building path) with
**zero output change**, quantified per-lever by micro-bench and in aggregate by the phase-timings
re-measure — and a clean, validated base on which the #4 row-collapse migration can build next.
