# Reliable and Faster Full-Corpus Warehouse Runs

**Date:** 2026-07-05
**Status:** Approved
**Scope:** `ab-morph-run analyze-aat` warehouse-mode runs (`run_analyze_aat_warehouse_parallel` and the serial shard path it drives).

## Goal

A full-corpus warehouse run (17,885 AAT sources × 4 analyzers) currently requires babysitting: the `jobs = nproc` default OOMs under earlyoom (68.5 GiB RSS observed at jobs=32), shard staging lives in shared `/db/ab-validator/tmp` for the whole run and dies if anything cleans that directory, and a failed run loses all 40+ minutes of work. Goal: runs complete unattended at an automatically chosen safe parallelism, staging is immune to external tmp cleanup, and the freed memory ceiling converts directly into wall-clock improvement (~42 min at jobs 10 today; target ~20-25 min at auto-jobs near nproc).

Non-goals (explicitly deferred, per the 2026-07-03 perf decision report): the RSS-monitored `WAREHOUSE_REGULAR_BATCH_SIZE` sweep (§3.5), intra-worker analyzer parallelism / work-stealing (§3.15), and shard-level resumability.

## Background (measured facts)

- The 2026-07-03 report pins peak RSS at 50.6 GiB (jobs=32, 3 analyzers) and attributes the ceiling to the per-analysis source-text clone (§3.10). The live site is `crates/ab-morph-run/src/pipeline.rs:737`: `analysis.source_text = document.text.clone();` — executed once per analyzer per document, so a batch of 32 documents × 4 analyzers holds up to 128 independent copies of full document texts per worker.
- At jobs=32 with 4 analyzers the run was SIGTERM'd by earlyoom (10% MemAvailable threshold) at 68.5 GiB RSS. At jobs=10 the same run completes in ~42 min with ~22-core-equivalent CPU utilization available (the report measured 2,241% CPU at jobs=32).
- Shard staging accumulates ~40 GB under one `$TMPDIR/ab-morph-run-warehouse-<pid>-<nanos>/` root that is removed only after the final merge — indistinguishable from abandoned debris. A manual cleanup of stale tmp data during a live run deleted its staging and produced the observed `failed to write <staging>/views.sql` shard-finalize failure.

## Design

### 1. Share the source text (`Arc<str>`) instead of cloning per analysis

`ab-morph-diff`'s `Analysis.source_text` changes from `String` to `Arc<str>`. The pipeline allocates the document text once per document — the ortho-normalized string when normalization fires, otherwise the original — and every per-analyzer `Analysis` shares that allocation. `Arc<str>` derefs to `&str`, so read-side consumers (`warehouse/rows.rs`, nway comparison, validation) continue to compile with at most trivial adjustments. Precedent: constant id columns already share `Arc<str>` (commit 1a27b4a).

Any remaining construction sites that build `Analysis` from owned strings (`ab-morph-diff/src/lib.rs:192`, adapter crates, tests) convert with `Arc::from(...)` at the boundary. Serialization (where present) is unaffected: `Arc<str>` serializes as a string.

### 2. Memory-aware auto-jobs in the binary

`--jobs 0` currently means "caller decides" and the justfile substitutes `$(nproc)`. New semantics, implemented in `ab-morph-run` (not the justfile):

```
auto_jobs = clamp(1, nproc, floor(MemAvailable_bytes × 0.7 / per_job_bytes))
per_job_bytes = base_per_job + analyzer_count × per_analyzer_increment
```

`base_per_job` and `per_analyzer_increment` are named constants calibrated from the two measured full-run data points (50.6 GiB / 32 jobs / 3 analyzers ≈ 1.6 GiB per job; 68.5 GiB / 32 jobs / 4 analyzers ≈ 2.1 GiB per job → increment ≈ 0.5 GiB, base ≈ 0.1 GiB — recalibrate against the §3.10-fixed pipeline during validation and record the final constants in the plan). `MemAvailable` is read from `/proc/meminfo`, mirroring the summarizer's DuckDB budget discipline. The chosen value and its inputs are logged at startup.

An explicit `--jobs N` is honored unchanged but logs a warning when `N` exceeds the computed budget. The justfile drops its `$(nproc)` substitution and passes `0` through.

### 3. Run-owned staging under the warehouse directory

The shard temp root moves from `std::env::temp_dir().join("ab-morph-run-warehouse-<pid>-<nanos>")` to `<warehouse_dir>/.staging/<run_id>/`:

- Ownership is visible: the transient data lives inside the warehouse it is building; no tmp-cleanup routine (human or automated) has any business there.
- Same filesystem as `runs/`, so the final `fs::rename` publish stays atomic and cheap (this was already true only by convention — TMPDIR happened to be on /db; now it is true by construction, and the `TMPDIR=`/`TMP=`/`TEMP=` exports drop out of the justfile recipe).
- A `pid` marker file is written at root creation. At startup, `analyze-aat` scans `<warehouse_dir>/.staging/` and removes any entry whose recorded PID is no longer alive (`/proc/<pid>` absent), so crashed or killed runs never leave debris that a human is tempted to clean by hand.
- Collision rule unchanged: a live `.staging/<run_id>/` with a live PID for the same `run_id` is a hard error ("run already in progress").

### 4. Error behavior

| Situation | Behavior |
|---|---|
| `MemAvailable` unreadable (non-Linux, restricted /proc) | auto-jobs falls back to `min(nproc, 8)` with a logged warning |
| Explicit `--jobs` exceeds budget | honored; warning logged with budget arithmetic |
| `.staging/<run_id>` exists with live PID | hard error, exit 1 |
| `.staging/<other>` with dead PID | removed at startup, logged |
| earlyoom pressure mid-run | out of scope to prevent entirely; auto-jobs budget (70% of MemAvailable) is the mitigation |

## Test Plan

- Unit: auto-jobs arithmetic (budget clamps at 1 and nproc; per-analyzer scaling; fallback path), orphan-staging cleanup (fake marker with dead/live PID), collision hard error.
- Existing suites: `ab-morph-diff` + `ab-morph-run --features test-analyzer` (152 tests) exercise `source_text` consumers extensively and gate the `Arc<str>` change.
- The tiny-corpus end-to-end (3 AAT files, warehouse mode) verifies staging appears under `.staging/<run_id>/` and vanishes after publish.

## Validation (full scale)

One full-corpus run with the new pipeline at `--jobs 0`:

1. Expected: peak RSS well under half of today's per-job footprint; auto-jobs lands near nproc; wall-clock ~20-25 min. Record actuals.
2. Row-count verification per analyzer (`analyses`, `morphemes`, `nway_regions` counts) against `full-2026-07-05_114245-jobs10` — counts must match exactly (the analysis is deterministic given identical analyzer set and dictionaries).
3. On success the new run becomes canonical and `full-2026-07-05_114245-jobs10` is deleted (owner decision 2026-07-05: keep one verified warehouse).

## Decisions

| # | Decision | Rationale |
|---|---|---|
| 1 | `Arc<str>` model change over borrowing rework | Minimal ripple; precedent exists; removes the dominant memory multiplier |
| 2 | Auto-jobs in the binary, not the justfile | The binary knows analyzer count and reads MemAvailable at the moment it matters; recipes stay dumb |
| 3 | Staging under `<warehouse_dir>/.staging/<run_id>/` | Ownership visible; atomic publish by construction; enables safe orphan cleanup |
| 4 | PID-marker liveness for orphan cleanup | Cheap, no daemon, no lockfile protocol; `/proc/<pid>` suffices on the target platform |
| 5 | §3.5 batch sweep, §3.15 work-stealing, resumability deferred | Owner chose "reliability + easy speed"; the deferred items need dedicated measurement windows |
