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
- **Correctness invariant:** equivalence requires runs to be *maximal* — no two distinct entries in a
  region's `feature_groups` share the same `(feature_key, scope_type, scope_position, scope_surface)`.
  Add a `debug_assert` (or an explicit within-`record` guard) and a test that stresses an interleaved
  ordering, so a future change that breaks maximality fails loudly rather than silently under-merging.
- **Content-neutrality:** final `feature_pattern_counts` output drains `self.patterns` (itself a
  `BTreeMap`) and aggregates commutatively (`+=`, `BTreeSet::insert`, sorted pattern strings), so it is
  independent of `groups` iteration order → byte-identical.
- **Test gap:** no test exercises this accumulator today. Add a characterization/differential test.

### Lever 2 — Direct Arrow column building (`morpheme_features`, `nway_feature_diffs`)

- **Sites:** builders `warehouse/rows.rs:129-164` (`morpheme_feature_rows_for_range`) and
  `rows.rs:307-328` (n-way diffs); transposition `ab-warehouse/src/writer.rs:315-336`
  (`append_morpheme_features`) and `writer.rs:420-444` (`append_nway_feature_diffs`).
- **Change:** introduce a per-table column-builder (e.g. `StringBuilder`/`UInt64Builder` fields with a
  `push_row(...)` and `finish() -> RecordBatch`). The producers append fields directly into the arrow
  builders instead of materializing `Vec<RowStruct>`; the writer's per-table append consumes the
  builder's finished batch. Eliminates the `Vec<Row>` backing store + struct memcpy and the
  ~10¹¹-scale `Arc` atomic refcount traffic (build clones + drop decrements) on the two hottest tables.
  (The string *byte* copy into the arrow value buffer is unchanged — `Utf8`, not dictionary — so it is
  not part of the win; do not switch physical type.)
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
- **Guarded by:** existing `all_match_emits_nothing` and the evidence-content tests
  (`evidence_detail_keeps_raw_reading`, classification tests, etc.). Add a micro-bench over
  `adjudicate` on a match-heavy input.

## Validation

- **Per-lever (in-repo):** a differential unit test asserting old-path output == new-path output on a
  fixed input (for #1 and #2, keep the old implementation available to the test — e.g. a private
  `*_reference` fn — so the assertion is genuine, not self-referential); a criterion micro-bench in
  the owning crate quantifying the per-lever win.
- **Whole-branch (hinoki):** one full-corpus run on the branch; compare against a current-`main`
  baseline via `scripts/oracle-validation-diff.sh` (all non-oracle tables **row-identical**; oracle
  keyed 4-bucket diff all-`unchanged`) **plus** a byte/row check on `feature_pattern_counts`,
  `morpheme_features`, and `nway_feature_diffs`. Record the new `phase-timings` split — **adjudication
  % should fall** — alongside wall-clock and peak RSS, in the plan's Validation Record.
- **Content-neutral bar:** for this round, "PASS" = every emitted table is row-identical and the
  oracle diff shows dropped=0 / newly=0 / changed=0. Any deviation is a regression, not an expected change.

## Expected outcome

Lower adjudication CPU (fewer allocations + atomic bumps + map churn on the row-building path) with
**zero output change**, quantified per-lever by micro-bench and in aggregate by the phase-timings
re-measure — and a clean, validated base on which the #4 row-collapse migration can build next.
