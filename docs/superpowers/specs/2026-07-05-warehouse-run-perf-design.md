# Reliable and Faster Full-Corpus Warehouse Runs

**Date:** 2026-07-05
**Status:** Approved
**Scope:** `ab-morph-run analyze-aat` warehouse-mode runs (`run_analyze_aat_warehouse_parallel` and the serial shard path it drives).

## Goal

A full-corpus warehouse run (17,885 AAT sources × 4 analyzers) currently requires babysitting: the `jobs = nproc` default OOMs under earlyoom (68.5 GiB RSS observed at jobs=32), shard staging lives in shared `/db/ab-validator/tmp` for the whole run and dies if anything cleans that directory, and a failed run loses all 40+ minutes of work. Goal: runs complete unattended at an automatically chosen safe parallelism, staging is immune to external tmp cleanup, and the freed memory ceiling converts directly into wall-clock improvement (~42 min at jobs 10 today; target ~20-25 min at auto-jobs near nproc).

Non-goals (explicitly deferred, per the 2026-07-03 perf decision report): the RSS-monitored `WAREHOUSE_REGULAR_BATCH_SIZE` sweep (§3.5), intra-worker analyzer parallelism / work-stealing (§3.15), and shard-level resumability. Note on §3.15: it may prove the *larger* throughput lever than memory-freed job scaling — the report measured only ~22 of 32 cores busy at jobs=32, and whether that gap closes with more jobs or needs intra-worker parallelism is exactly what its deferred scaling-curve measurement decides. The `Arc<str>` change is required groundwork for either path, so nothing here is wasted if §3.15 wins.

## Background (measured facts)

- The 2026-07-03 report pins peak RSS at 50.6 GiB (jobs=32, 3 analyzers) and attributes the ceiling to the per-analysis source-text clone (§3.10). The live site is `crates/ab-morph-run/src/pipeline.rs:737`: `analysis.source_text = document.text.clone();` — executed once per analyzer per document, so a batch of 32 documents × 4 analyzers holds up to 128 independent copies of full document texts per worker.
- At jobs=32 with 4 analyzers the run was SIGTERM'd by earlyoom (10% MemAvailable threshold) at 68.5 GiB RSS. At jobs=10 the same run completes in ~42 min with ~22-core-equivalent CPU utilization available (the report measured 2,241% CPU at jobs=32).
- Shard staging accumulates ~40 GB under one `$TMPDIR/ab-morph-run-warehouse-<pid>-<nanos>/` root that is removed only after the final merge — indistinguishable from abandoned debris. A manual cleanup of stale tmp data during a live run deleted its staging and produced the observed `failed to write <staging>/views.sql` shard-finalize failure.

## Design

### 1. Share the source text (`Arc<str>`) instead of cloning per analysis

`ab-morph-diff`'s `Analysis.source_text` changes from `String` to `Arc<str>`. The pipeline allocates the document text once per document — the ortho-normalized string when normalization fires, otherwise the original — and every per-analyzer `Analysis` shares that allocation. `Arc<str>` derefs to `&str`, so read-side consumers (`warehouse/rows.rs`, nway comparison, validation) continue to compile with at most trivial adjustments. Precedent: constant id columns already share `Arc<str>` (commit 1a27b4a).

Mechanical scope, stated honestly: ~25 construction sites across `ab-morph-diff`, `ab-morph-run`, `ab-morph-analyzers`, and both adapters build `Analysis` from owned strings; each converts with `Arc::from(...)` at the boundary. The compiler enumerates every site, so this is volume, not risk. Serialization (where present) is unaffected: `Arc<str>` serializes as a string.

### 2. Memory-aware auto-jobs in the binary

`--jobs 0` currently means "caller decides" and the justfile substitutes `$(nproc)`. New semantics, implemented in `ab-morph-run` (not the justfile):

```
auto_jobs = clamp(1, nproc, floor((MemAvailable × 0.7 − fixed_overhead) / per_job_bytes))
per_job_bytes = base_per_job + analyzer_count × per_analyzer_increment
```

`base_per_job` and `per_analyzer_increment` are named constants. The pre-fix data points (50.6 GiB / 32 jobs / 3 analyzers; 68.5 GiB / 32 jobs / 4 analyzers → increment ≈ 0.5 GiB, base ≈ 0.1 GiB) serve only as initial placeholders. **Calibration procedure (replaces "recalibrate during validation"):** after the `Arc<str>` fix lands, run a ~2,000-source subset at `--jobs 4`, `--jobs nproc/2`, and `--jobs nproc`, capturing peak RSS per run (`/usr/bin/time -v`). Fit the two constants linearly from those three points and record fit + residuals in the implementation plan. A subset suffices because per-job memory is batch-local (`WAREHOUSE_REGULAR_BATCH_SIZE = 32` documents in flight per worker), not corpus-size-dependent — the killed jobs=32 run reached its RSS plateau within minutes. A full-corpus jobs=1 sweep was considered and rejected: ~16 h of machine time (the report measured 30:48 wall at jobs=32) for a point the subset measures in minutes. If the three subset points are visibly non-linear, budget from the worst measured per-job value instead of the fit — over-conservatism costs minutes; under-budgeting costs the run. The 2026-07-06 sweep's fit surfaced a large fixed intercept (13,147,661 kB, ≈ 12.5 GiB of shared dictionary allocations independent of job count), so the formula above was amended to `jobs = (0.7 × MemAvailable − fixed_overhead) / per_job_bytes` (controller-approved deviation from the original shape, 2026-07-06), since leaving the intercept unbudgeted let hosts under ~20 GiB MemAvailable pick job counts that would OOM.

`MemAvailable` is read from `/proc/meminfo` once at startup, mirroring the summarizer's DuckDB budget discipline (read once, budget, run — mid-run pressure from other processes is mitigated by the 30% headroom, not prevented). The chosen value and its inputs are logged at startup.

An explicit `--jobs N` is honored unchanged but logs a warning when `N` exceeds the computed budget. The justfile drops its `$(nproc)` substitution and passes `0` through.

### 3. Run-owned staging under the warehouse directory

The shard temp root moves from `std::env::temp_dir().join("ab-morph-run-warehouse-<pid>-<nanos>")` to `<warehouse_dir>/.staging/shards-<run_id>/`:

- Ownership is visible: the transient data lives inside the warehouse it is building; no tmp-cleanup routine (human or automated) has any business there.
- Same filesystem as `runs/`, so the final `fs::rename` publish stays atomic and cheap (this was already true only by convention — TMPDIR happened to be on /db; now it is true by construction, and the `TMPDIR=`/`TMP=`/`TEMP=` exports drop out of the justfile recipe).
- A `pid` marker file is written at root creation. At startup, `analyze-aat` scans `<warehouse_dir>/.staging/` and removes any entry whose recorded PID is dead. **Liveness = `/proc/<pid>` exists AND `/proc/<pid>/cmdline` contains `ab-morph-run`** — the cmdline check closes the PID-reuse hole (a recycled PID belonging to an unrelated process no longer blocks cleanup). Residual risk (PID recycled *to another ab-morph-run*) is accepted: the wrongly-skipped orphan is caught by the next startup scan after that process exits, and `just morph-warehouse-clean` remains the manual escape hatch.
- Collision rule unchanged: a live `.staging/shards-<run_id>/` with a live PID for the same `run_id` is a hard error ("run already in progress").
- **Atomic publish boundary (verified in code):** `finalize_staging_run` publishes via a single `fs::rename(staging_dir → runs/<run_id>)` of the whole run directory — a consumer that sees `runs/<run_id>` sees every table at once, never a partial run. This guarantee is what same-filesystem staging preserves by construction.

### 4. Error behavior

| Situation | Behavior |
|---|---|
| `MemAvailable` unreadable, zero, or unparseable | auto-jobs falls back to `max(1, min(nproc / 4, 8))` with a logged warning |
| Explicit `--jobs` exceeds budget | honored; warning logged with budget arithmetic |
| `.staging/shards-<run_id>` exists with live PID | hard error, exit 1 |
| `.staging/shards-<other>` with dead PID | removed at startup, logged |
| earlyoom pressure mid-run | out of scope to prevent entirely; auto-jobs budget (70% of MemAvailable) is the mitigation |

## Test Plan

- Unit: auto-jobs arithmetic (budget clamps at 1 and nproc; per-analyzer scaling; `MemAvailable` zero/unparseable/absent → fallback formula; machines where the budget exceeds nproc; the explicit-`--jobs`-over-budget warning path), orphan-staging cleanup (fake marker: dead PID, live PID + matching cmdline, live PID + foreign cmdline), collision hard error.
- Existing suites: `ab-morph-diff` + `ab-morph-run --features test-analyzer` (152 tests) exercise `source_text` consumers extensively and gate the `Arc<str>` change.
- The tiny-corpus end-to-end (3 AAT files, warehouse mode) verifies staging appears under `.staging/shards-<run_id>/` and vanishes after publish.

## Validation (full scale)

One full-corpus run with the new pipeline at `--jobs 0`:

1. Expected: peak RSS well under half of today's per-job footprint; auto-jobs lands near nproc; wall-clock ~20-25 min. Record actuals.
2. Row-count verification per analyzer (`analyses`, `morphemes`, `nway_regions` counts) against `full-2026-07-05_114245-jobs10` — counts must match exactly (the analysis is deterministic given identical analyzer set and dictionaries).
3. On success the new run becomes canonical and `full-2026-07-05_114245-jobs10` is deleted (owner decision 2026-07-05: keep one verified warehouse).
4. **Failure procedure:** if any count differs, the prior run stays canonical and is NOT deleted; the new run is retained for diffing (per-analyzer, per-table counts narrow the discrepancy to an analyzer or a pipeline stage); no deletion happens until a diagnosed, verified run exists. Peak RSS above budget or wall-clock regression are recorded as calibration findings, not deletion triggers.

## Decisions

| # | Decision | Rationale |
|---|---|---|
| 1 | `Arc<str>` model change over borrowing rework | Minimal ripple; precedent exists; removes the dominant memory multiplier |
| 2 | Auto-jobs in the binary, not the justfile | The binary knows analyzer count and reads MemAvailable at the moment it matters; recipes stay dumb |
| 3 | Staging under `<warehouse_dir>/.staging/shards-<run_id>/` | Ownership visible; atomic publish by construction; enables safe orphan cleanup |
| 4 | PID-marker + cmdline liveness for orphan cleanup | Cheap, no daemon, no lockfile protocol; the cmdline check closes the PID-reuse hole; residual same-binary recycling accepted (next scan / manual clean) |
| 5 | §3.5 batch sweep, §3.15 work-stealing, resumability deferred | Owner chose "reliability + easy speed"; the deferred items need dedicated measurement windows |
