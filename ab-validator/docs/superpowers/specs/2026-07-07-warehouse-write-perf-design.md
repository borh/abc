# Warehouse Write-Path Performance — Design

**Status:** design (pending user review)
**Date:** 2026-07-07
**Predecessor:** `2026-07-07-oracle-followups-perf-design.md` (P1 front-load + auto_jobs recalibration landed on `main` = 2234512; full run ~67 → 51.6 min)

## Problem

After P1 + jobs 8→16, the full-corpus warehouse run (~51.6 min on hinoki) is
believed to be **write-bound**, not analysis-bound. The two dominant tables are
`nway_feature_diffs` (~23.4 B rows) and `morpheme_features` (~12.6 B rows). But
this belief is **documented, never measured** — there is no timing
instrumentation in `ab-morph-run`, so we cannot say what fraction of wall-time
is tokenize/remap vs. oracle/nway adjudication vs. parquet encode.

Two facts from the code survey (`pipeline.rs`, `writer.rs`) shape the approach:

1. **Encode is on every worker's critical path.** Each worker thread performs
   morphological analysis *and* ZSTD-3 parquet encoding sequentially on the same
   thread (`pipeline.rs:713-887`); there is no separate encoder thread. So encode
   time is not hidden behind analysis.
2. **ZSTD level is hard-coded and non-configurable** at level 3
   (`writer.rs:867-874`), applied uniformly to all 13 tables. The two giant
   tables compress to ~0.07–0.17 bytes/row, i.e. they are extremely
   compressible — a regime where a lower ZSTD level trades a small disk increase
   for a potentially large drop in encode CPU.

## Scope (this round)

Per the scope decision, this round is **measure + cheap wins only**:

1. **Phase-timing instrumentation** — confirm the write-bound hypothesis with
   real per-phase numbers before investing in any structural lever.
2. **Configurable ZSTD level** — a knob to trade encode-time for disk on the
   critical path, defaulted conservatively and re-defaulted only from a measured
   hinoki sweep.

Everything else is explicitly **out of scope** and deferred (see below).

### Non-goals / deferrals

- **Data-volume reduction by key-pruning** — foreclosed by the user decision
  "I need all keys / raw features". `nway_feature_diffs` and `morpheme_features`
  stay complete (all feature keys, every morpheme). Not this round, not later.
- **Per-analyzer row-collapse** (store `analyzer_id` list per value-group instead
  of one row per analyzer) and **id-column encoding** — the only remaining
  volume levers compatible with the "keep all keys" constraint. Deferred to a
  future round; this round's instrumentation will quantify whether they are
  worth it.
- **Lever 2 (intra-worker analyzer parallelism)** — feasible but speeds
  *analysis*, which is not the binding constraint on a write-bound run. Deferred;
  the instrumentation will produce the analysis-fraction number that decides
  whether it is ever worth doing.
- **Decoupling encode onto a separate thread per worker** — a real structural
  lever if encode proves to be a large fraction, but higher-complexity and
  memory-interacting. Deferred; gated on the measurement.

## Design

### Component 1 — Phase-timing instrumentation

**Goal:** attribute each worker's wall-time across three per-document phases and
report an aggregate at end of run.

- **Phases measured** (in `run_analyze_aat_serial`, `pipeline.rs`):
  - `analysis` — the per-analyzer tokenize + span-remap loop
    (`pipeline.rs:713-805`).
  - `adjudication` — oracle / N-way region fact construction after the loop
    (`pipeline.rs:807-959`, including `oracle::ruby::adjudicate` and the nway
    row builders).
  - `write` — the parquet `append_*` calls (`pipeline.rs:820-887`) **plus** the
    per-shard writer `close()`/`finalize()`. ZSTD encoding happens lazily at
    50k-row row-group flushes and at close, so both must be inside this timer to
    attribute encode correctly.
- **Mechanism:** `std::time::Instant` deltas accumulated into a small
  `PhaseTimings { analysis, adjudication, write }` (of `std::time::Duration`)
  value. `run_analyze_aat_serial` returns it alongside its existing
  `StringStatsReport`. `Instant::now()` around three regions per document is
  negligible overhead relative to the work.
- **Aggregation:** the parallel drivers (`run_analyze_aat_warehouse_parallel`,
  `run_analyze_aat_parallel`) sum the per-worker `PhaseTimings` after the join
  barrier and emit one summary line to stderr, in the same style as the existing
  `auto-jobs:` line — e.g.
  `phase-timings: analysis=Xs (A%) adjudication=Ys (B%) write=Zs (C%) [summed across N workers]`.
  Summed CPU-time across workers (not wall-clock) is the right signal for "where
  does the work go"; the run's wall-clock is already available from
  `/usr/bin/time`.
- **Always on, low-cost.** No feature flag — the timers are cheap and the summary
  line is one stderr print. This keeps the measurement available for every future
  run, not just a one-off sweep.
- **Determinism note:** timing values are non-deterministic, so they are logged
  to stderr only — never written into any parquet/JSONL output that a test
  asserts on. Tests cover that `PhaseTimings` accumulates and sums correctly
  (with injected/fake durations or monotonic-ordering assertions), not wall-clock
  magnitudes.

### Component 2 — Configurable ZSTD level

**Goal:** make the parquet compression level a run parameter so it can be swept
and re-defaulted from data.

- **Plumbing:** `writer_properties()` (`writer.rs:867`) currently takes no
  arguments and hard-codes `ZstdLevel::try_new(3)`. Thread a
  `zstd_level: i32` (validated to parquet's accepted 1..=22 range) from the CLI
  through `WarehouseWriter::create_for_tables` (`writer.rs`, called at
  `pipeline.rs:494`) into `open_table_writer` → `writer_properties(level)`.
- **CLI surface:** a new flag, e.g. `--parquet-zstd-level <N>` on the
  warehouse-run subcommand, plumbed through the run options struct. Invalid
  levels are rejected at parse time with a clear error.
- **Default:** **unchanged at 3** until the sweep picks a new default. The knob
  lands first; the default only moves on evidence.
- **Codec choice constraint:** the `parquet` crate is built with
  `features = ["arrow", "zstd"]` only (`ab-validator/Cargo.toml`), so LZ4/snappy
  are not available without a dependency change. This round stays within ZSTD
  levels — no new features/dependencies.

### Component 3 — Measurement + sweep methodology (hinoki)

1. Build the instrumented binary on hinoki.
2. **Baseline run at `--parquet-zstd-level 3`** (current default) to record the
   phase-timing split and confirm/refute the write-bound hypothesis. This is the
   deliverable that decides whether the deferred structural levers are ever worth
   pursuing.
3. **Sweep candidate at `--parquet-zstd-level 1`**, comparing wall-clock,
   output size (`du`), and the phase split.
4. If level 1 gives a material wall-clock win at acceptable disk cost (disk is
   abundant — 3.6 TB free), change the default; otherwise keep 3. Record the
   decision in the plan's Validation Record.

## Validation

A codec-level change is **logically content-neutral** — same rows, same values,
different compression. So:

- **Row-count parity** across all tables between the level-3 baseline and the
  level-1 run, using the existing non-oracle parity check in
  `scripts/oracle-validation-diff.sh` (all 13 tables must match exactly).
- **Read-back check:** a sample of each rewritten table must re-read/parse
  correctly (DuckDB `SELECT count(*)` + a `LIMIT` scan) to confirm the alternate
  compression level round-trips.
- **Unit tests:** `PhaseTimings` accumulation/summation; `writer_properties`
  honors the passed level; the CLI flag parses and rejects out-of-range values.

## Expected outcome

- A hard number for the analysis / adjudication / write split — turning
  "believed write-bound" into a measured fact that directs all future perf work.
- A codec knob, and (if the sweep supports it) a lower default ZSTD level that
  shaves encode time off the critical path with no content change.
