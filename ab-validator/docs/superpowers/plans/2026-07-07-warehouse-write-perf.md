# Warehouse Write-Path Performance Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Measure where the full-corpus warehouse run actually spends wall-time (analysis vs. adjudication vs. parquet encode) and add a ZSTD-level knob to trade encode time for disk on the write-bound critical path.

**Architecture:** Two independent, low-risk changes. (1) A per-worker phase timer, accumulated inside `WarehouseWriter` for pure parquet write/encode time and around the analyzer loop for analysis time, summed across workers and logged once to stderr. (2) A configurable parquet ZSTD compression level threaded from a new CLI flag through `WarehouseWriter::create_for_tables` into `writer_properties`.

**Tech Stack:** Rust workspace `ab-validator/`, crates `ab-warehouse` (parquet `ArrowWriter`, `parquet = "56"` with `features=["arrow","zstd"]`) and `ab-morph-run` (pipeline, clap CLI). `std::time::{Instant, Duration}`.

Reference spec: `docs/superpowers/specs/2026-07-07-warehouse-write-perf-design.md`.

## Global Constraints

- **No content change.** These changes must not alter any row, value, or row count in any table. Timing is stderr-only and never written to parquet/JSONL. The ZSTD level changes only compression, not logical content.
- **Default ZSTD level stays 3** in this branch. The knob lands; the default moves only from a measured hinoki sweep (recorded in the Validation Record).
- **No new dependencies / no new parquet features.** Stay within ZSTD levels 1..=22 (the parquet crate is built with `features=["arrow","zstd"]` only).
- **Keep all feature keys.** Do not prune, gate, or restrict `nway_feature_diffs` / `morpheme_features` content (user decision: "I need all keys / raw features"). This plan touches only timing and compression.
- Match surrounding code style. The stderr summary line mirrors the existing `auto-jobs:` line style (`auto_jobs.rs:105-113`).

## File Structure

- `crates/ab-warehouse/src/writer.rs` — add a `write_time: Duration` accumulator to `WarehouseWriter`; wrap every `ArrowWriter::write`/`close`; expose it; add the ZSTD level parameter.
- `crates/ab-morph-run/src/pipeline.rs` — `PhaseTimings` struct; time the analyzer loop and total; read the writer's write-time; return timings from `run_analyze_aat_serial`; sum + log in the two parallel drivers and the serial entry point.
- `crates/ab-morph-run/src/main.rs` — `--parquet-zstd-level` CLI flag on the warehouse-run subcommand, plumbed into the run options.

---

### Task 1: Pure parquet write-time accumulation in `WarehouseWriter`

**Files:**
- Modify: `crates/ab-warehouse/src/writer.rs` (struct at `31-46`, `create_for_tables` at `54`, `write_batch` at `876-884`, `append_record_batch` at `463-536`, `close_writer` at `886-891`, `finalize` at `538-552`)
- Test: `crates/ab-warehouse/src/writer.rs` (existing `#[cfg(test)] mod tests`)

**Interfaces:**
- Produces: `WarehouseWriter::write_time(&self) -> std::time::Duration` (total time spent in `ArrowWriter::write` during appends so far); `WarehouseWriter::finalize(self) -> Result<Duration>` now **returns** the accumulated write time including the `close()` (row-group tail flush) deltas.
- Consumes: nothing new.

- [ ] **Step 1: Write the failing test**

Add to the `tests` module in `writer.rs`:

```rust
#[test]
fn write_time_accumulates_across_appends() {
    use std::time::Duration;
    let tmp = tempfile::tempdir().unwrap();
    let paths = test_paths(tmp.path(), "wt-run"); // mirror existing test-paths helper
    let mut w = WarehouseWriter::create_for_tables(paths, WarehouseTable::ALL).unwrap();
    assert_eq!(w.write_time(), Duration::ZERO);
    // Append enough error rows to force at least one write() call.
    let rows: Vec<ErrorRow> = (0..1000).map(|i| sample_error_row(i)).collect();
    w.append_errors(&rows).unwrap();
    assert!(w.write_time() > Duration::ZERO, "append must record write time");
    let before_close = w.write_time();
    let total = w.finalize().unwrap();
    assert!(total >= before_close, "finalize folds in close() time");
}
```

Use the crate's existing test helpers for building `WarehousePaths` and a sample row (search the test module for the current pattern; reuse it rather than inventing one).

- [ ] **Step 2: Run it to confirm it fails**

Run: `cargo test -p ab-warehouse write_time_accumulates_across_appends`
Expected: FAIL — `write_time` / new `finalize` return type do not exist.

- [ ] **Step 3: Add the accumulator field**

In the struct (`writer.rs:31`):

```rust
pub struct WarehouseWriter {
    paths: WarehousePaths,
    write_time: std::time::Duration,
    runs: Option<ArrowWriter<File>>,
    // ... existing fields unchanged ...
}
```

Initialize it in `create_for_tables` (`writer.rs:67`, in the `Ok(Self { ... })`):

```rust
Ok(Self {
    write_time: std::time::Duration::ZERO,
    runs: open_optional_table_writer(&paths, tables, WarehouseTable::Runs, runs_schema())?,
    // ... existing fields unchanged ...
    paths, // if paths is moved here already, keep existing placement
})
```

(Keep the existing `paths` handling; only add the `write_time` initializer.)

- [ ] **Step 4: Time the two write choke points and the close**

`write_batch` is the choke point for all `append_*` methods. Change it to record elapsed into a caller-supplied accumulator:

```rust
fn write_batch<W: std::io::Write + Send>(
    writer: &mut ArrowWriter<W>,
    schema: Arc<Schema>,
    columns: Vec<ArrayRef>,
    write_time: &mut std::time::Duration,
) -> Result<()> {
    let batch = RecordBatch::try_new(schema, columns)?;
    let start = std::time::Instant::now();
    writer.write(&batch)?;
    *write_time += start.elapsed();
    Ok(())
}
```

Update **every** `append_*` method (the `write_batch(...)` calls at `169,189,207,228,250,267,292,311,338,367,393,418,441`) to pass `&mut self.write_time` as the final argument, e.g.:

```rust
write_batch(
    self.morphemes.as_mut().expect("morphemes writer"),
    morphemes_schema(),
    columns,
    &mut self.write_time,
)?;
```

(`self.<table>` and `self.write_time` are disjoint fields, so the borrow is legal inside each method.)

For `append_record_batch` (`463-536`), which calls `.write(&batch)?` directly per table, wrap the whole match once:

```rust
let start = std::time::Instant::now();
match table {
    // ... existing arms, each `writer.write(&batch)?` unchanged ...
}
self.write_time += start.elapsed();
Ok(())
```

Change `close_writer` to return the close duration:

```rust
fn close_writer(writer: Option<ArrowWriter<File>>) -> Result<std::time::Duration> {
    if let Some(writer) = writer {
        let start = std::time::Instant::now();
        writer.close()?;
        return Ok(start.elapsed());
    }
    Ok(std::time::Duration::ZERO)
}
```

Update the two non-`WarehouseWriter` `close_writer` callers (tests at `1413`/`1482` use `.close()` directly — unaffected; only the `finalize` call sites change).

- [ ] **Step 5: Expose the getter and update `finalize`**

Add the getter (near the other `impl WarehouseWriter` methods):

```rust
pub fn write_time(&self) -> std::time::Duration {
    self.write_time
}
```

Update `finalize` (`538-552`) to fold in close time and return the total:

```rust
pub fn finalize(mut self) -> Result<std::time::Duration> {
    self.write_time += close_writer(self.runs.take())?;
    self.write_time += close_writer(self.run_analyzers.take())?;
    // ... same for every remaining table writer ...
    self.write_time += close_writer(self.errors.take())?;
    Ok(self.write_time)
}
```

- [ ] **Step 6: Fix `finalize` call sites**

Search the workspace for `.finalize()` on a `WarehouseWriter` (`rg "finalize\(\)" crates/`) and update each to capture (or discard) the returned `Duration`. In `pipeline.rs` the serial run's `writer.finalize()?;` becomes `let _warehouse_write_time = writer.finalize()?;` (Task 2 consumes it). If any call site does not need it yet, use `let _ = writer.finalize()?;`.

- [ ] **Step 7: Run the test**

Run: `cargo test -p ab-warehouse`
Expected: PASS (new test + all existing writer tests, including `writer_properties_use_bounded_row_groups_for_large_string_tables`).

- [ ] **Step 8: Commit**

```bash
git add crates/ab-warehouse/src/writer.rs
git commit -m "perf(warehouse): accumulate pure parquet write/encode time in WarehouseWriter"
```

---

### Task 2: Phase timings — instrument the pipeline and log the split

**Files:**
- Modify: `crates/ab-morph-run/src/pipeline.rs` (`run_analyze_aat_serial` `441-445` + input loop; `WarehouseShardOutput` `1122-1125`; warehouse driver aggregation `1090-1119`; the analyze driver `1374-...`; serial-path callers `199,394`)
- Test: `crates/ab-morph-run/src/pipeline.rs` (`#[cfg(test)] mod tests`)

**Interfaces:**
- Consumes: `WarehouseWriter::write_time()` / `finalize() -> Duration` from Task 1.
- Produces: `PhaseTimings { analysis: Duration, warehouse_write: Duration, total: Duration }` with `fn other(&self) -> Duration` = `total.saturating_sub(analysis + warehouse_write)` and `AddAssign` for summing across workers. `run_analyze_aat_serial` now returns `Result<(StringStatsReport, PhaseTimings)>`.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn phase_timings_sum_and_derive_other() {
    use std::time::Duration;
    let mut a = PhaseTimings {
        analysis: Duration::from_secs(2),
        warehouse_write: Duration::from_secs(3),
        total: Duration::from_secs(10),
    };
    let b = PhaseTimings {
        analysis: Duration::from_secs(1),
        warehouse_write: Duration::from_secs(1),
        total: Duration::from_secs(4),
    };
    a += b;
    assert_eq!(a.analysis, Duration::from_secs(3));
    assert_eq!(a.warehouse_write, Duration::from_secs(4));
    assert_eq!(a.total, Duration::from_secs(14));
    assert_eq!(a.other(), Duration::from_secs(7)); // 14 - 3 - 4
}

#[test]
fn phase_timings_other_saturates() {
    use std::time::Duration;
    // Overlap/measurement skew must never underflow.
    let t = PhaseTimings {
        analysis: Duration::from_secs(6),
        warehouse_write: Duration::from_secs(6),
        total: Duration::from_secs(10),
    };
    assert_eq!(t.other(), Duration::ZERO);
}
```

- [ ] **Step 2: Run to confirm failure**

Run: `cargo test -p ab-morph-run phase_timings`
Expected: FAIL — `PhaseTimings` undefined.

- [ ] **Step 3: Define `PhaseTimings`**

Near the top of `pipeline.rs` (after imports):

```rust
#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct PhaseTimings {
    pub(crate) analysis: std::time::Duration,
    pub(crate) warehouse_write: std::time::Duration,
    pub(crate) total: std::time::Duration,
}

impl PhaseTimings {
    pub(crate) fn other(&self) -> std::time::Duration {
        self.total
            .saturating_sub(self.analysis + self.warehouse_write)
    }
}

impl std::ops::AddAssign for PhaseTimings {
    fn add_assign(&mut self, rhs: Self) {
        self.analysis += rhs.analysis;
        self.warehouse_write += rhs.warehouse_write;
        self.total += rhs.total;
    }
}
```

- [ ] **Step 4: Instrument `run_analyze_aat_serial`**

Change the return type to `Result<(StringStatsReport, PhaseTimings)>` (`pipeline.rs:445`). Add at the start of the function body a `let run_start = std::time::Instant::now();` and a `let mut analysis_time = std::time::Duration::ZERO;`.

Wrap the per-document analyzer loop (`713-805`) to accumulate analysis time:

```rust
let analysis_start = std::time::Instant::now();
for analyzer in analyzers {
    // ... existing loop body unchanged ...
}
analysis_time += analysis_start.elapsed();
```

At every `Ok(...)` / early-return exit, build and return the tuple. The warehouse write time comes from the writer: capture it from `finalize` (Task 1 Step 6 changed the serial `finalize` call). If `warehouse_writer` is `None` (non-warehouse serial path), `warehouse_write` is `Duration::ZERO`. Assemble:

```rust
let warehouse_write = warehouse_write_time; // from finalize(), else ZERO
let timings = PhaseTimings {
    analysis: analysis_time,
    warehouse_write,
    total: run_start.elapsed(),
};
Ok((string_stats, timings))
```

Update the two serial-path callers at `pipeline.rs:199` and `:394` to destructure `(string_stats, _timings)` (or log for the serial `jobs==1` path — see Step 6).

- [ ] **Step 5: Carry timings through the warehouse driver**

Add `timings: PhaseTimings` to `WarehouseShardOutput` (`1122-1125`). In the worker closure (`1033`), capture the tuple from `run_analyze_aat_serial` and store `timings` in the returned `WarehouseShardOutput`. In the aggregation loop (`1090-1094`), sum:

```rust
let mut phase_timings = PhaseTimings::default();
for output in &mut outputs {
    report.warnings.append(&mut output.warnings);
    phase_timings += output.timings;
}
```

- [ ] **Step 6: Log the summary line**

After aggregation, before returning the report in `run_analyze_aat_warehouse_parallel` (and analogously wherever the analyze driver / serial path finishes), emit one stderr line mirroring the `auto-jobs:` style:

```rust
let pct = |d: std::time::Duration, total: std::time::Duration| -> f64 {
    if total.is_zero() { 0.0 } else { 100.0 * d.as_secs_f64() / total.as_secs_f64() }
};
eprintln!(
    "phase-timings (summed across {n} workers): total={total:.1}s analysis={a:.1}s ({ap:.1}%) warehouse-write={w:.1}s ({wp:.1}%) other={o:.1}s ({op:.1}%)",
    n = shard_run_dirs.len(),
    total = phase_timings.total.as_secs_f64(),
    a = phase_timings.analysis.as_secs_f64(), ap = pct(phase_timings.analysis, phase_timings.total),
    w = phase_timings.warehouse_write.as_secs_f64(), wp = pct(phase_timings.warehouse_write, phase_timings.total),
    o = phase_timings.other().as_secs_f64(), op = pct(phase_timings.other(), phase_timings.total),
);
```

Do the same for the non-warehouse `run_analyze_aat_parallel` driver (`warehouse-write` will read 0%) and the `jobs==1` serial dispatch, so the line prints for every run.

- [ ] **Step 7: Run tests**

Run: `cargo test -p ab-morph-run`
Expected: PASS. Confirm existing pipeline/scheduler tests are unaffected (the return-type change is internal; only call sites updated).

- [ ] **Step 8: Commit**

```bash
git add crates/ab-morph-run/src/pipeline.rs
git commit -m "perf(morph-run): phase-timing instrumentation (analysis/write/other) logged per run"
```

---

### Task 3: Configurable parquet ZSTD level

**Files:**
- Modify: `crates/ab-warehouse/src/writer.rs` (`writer_properties` `867-874`, `open_table_writer` `841-852`, `open_optional_table_writer` `854-865`, `create_for_tables` `54`, `create` `50`)
- Modify: `crates/ab-morph-run/src/main.rs` (warehouse-run subcommand args + plumbing into the run options that reach `create_for_tables`)
- Test: `crates/ab-warehouse/src/writer.rs` tests; `crates/ab-morph-run/src/main.rs` CLI-parse tests (mirror `warehouse_validation_*` tests around `1358-1386`)

**Interfaces:**
- Produces: `writer_properties(zstd_level: i32) -> WriterProperties`; `WarehouseWriter::create_for_tables(paths, tables, zstd_level)`; CLI flag `--parquet-zstd-level <N>` (default 3, validated 1..=22).
- Consumes: nothing new.

- [ ] **Step 1: Write the failing test (writer honors level)**

```rust
#[test]
fn writer_properties_honor_zstd_level() {
    let props = writer_properties(1);
    assert_eq!(props.max_row_group_size(), 50_000);
    assert!(matches!(
        props.compression(&"any".into()),
        parquet::basic::Compression::ZSTD(_)
    ));
}
```

(If the parquet API does not expose the numeric level back via `WriterProperties`, assert only the codec is ZSTD and that `writer_properties(1)` and `writer_properties(22)` both build without panicking; the level plumbing is then verified by the CLI test in Step 5 and the hinoki sweep.)

- [ ] **Step 2: Run to confirm failure**

Run: `cargo test -p ab-warehouse writer_properties_honor_zstd_level`
Expected: FAIL — `writer_properties` takes no argument.

- [ ] **Step 3: Parameterize `writer_properties` and thread the level**

```rust
fn writer_properties(zstd_level: i32) -> WriterProperties {
    WriterProperties::builder()
        .set_max_row_group_size(WAREHOUSE_MAX_ROW_GROUP_SIZE)
        .set_compression(Compression::ZSTD(
            ZstdLevel::try_new(zstd_level).expect("zstd level validated at CLI parse"),
        ))
        .build()
}
```

Thread `zstd_level: i32` through `open_table_writer` and `open_optional_table_writer` (pass it to `writer_properties(zstd_level)` at `850`), and add a `zstd_level: i32` parameter to `create_for_tables` (pass through to each `open_optional_table_writer` call). Update `create` (`50`) to `create_for_tables(paths, WarehouseTable::ALL, 3)` (keep the level-3 default for that convenience constructor). Update the existing `writer_properties()` test call at `1143` to `writer_properties(3)`.

- [ ] **Step 4: Update `WarehouseWriter` call sites**

The production caller is `pipeline.rs:494`. Thread a `zstd_level` value from the warehouse run options down to that `create_for_tables` call. Add a `zstd_level: i32` field to whatever warehouse-options struct is passed into `run_analyze_aat_serial` (search for the struct holding `warehouse.paths` / `warehouse_profile`), defaulting to 3 where constructed in tests.

- [ ] **Step 5: Write the failing CLI test, then add the flag**

Add a clap arg on the warehouse-run subcommand in `main.rs` (mirror the `jobs` arg pattern at `main.rs:291`):

```rust
/// Parquet ZSTD compression level (1..=22). Higher = smaller files, slower encode.
#[arg(long, default_value_t = 3, value_parser = clap::value_parser!(i32).range(1..=22))]
parquet_zstd_level: i32,
```

Plumb `parquet_zstd_level` from the parsed args into the run-options struct so it reaches `create_for_tables`. Add a CLI-parse test mirroring the existing `jobs` parse tests (`1358-1386`) asserting the flag parses to the expected value and that an out-of-range value (e.g. `--parquet-zstd-level 99`) is rejected.

- [ ] **Step 6: Run tests + a real one-shot smoke run**

Run: `cargo test -p ab-warehouse -p ab-morph-run`
Expected: PASS.
Then a tiny end-to-end sanity run on a handful of AAT files at `--parquet-zstd-level 1` to confirm the flag reaches the writer and the run completes (no assertion on size here — that is the hinoki sweep).

- [ ] **Step 7: Commit**

```bash
git add crates/ab-warehouse/src/writer.rs crates/ab-morph-run/src/main.rs
git commit -m "perf(warehouse): make parquet ZSTD level configurable via --parquet-zstd-level (default 3)"
```

---

## Validation Record (hinoki) — COMPLETE

Measured on hinoki (AMD Ryzen 9 9950X, 32t, 88 GiB avail), full corpus (17,885 sources),
4 analyzers (vibrato, vibrato:unidic-novel-202512, sudachi-a, sudachi-c), auto-jobs=18.
The split was **re-measured on `ab7ec42`** after two waves of perf optimization landed on
main mid-round (`e25e4f5` tokenizer/converter, `ab7ec42` adjudication); the `a27642d`
figures below are kept as the *pre-optimization* baseline.

- [x] Built instrumented binary on hinoki (release). auto-jobs correctly resolved to **18**
      (MemAvailable 88.6 GiB, per-job 1.7 GiB, fixed-overhead 31 GiB) — validating last
      round's `auto_jobs` recalibration in production; peak RSS stayed inside budget with no thrash.

- [x] **Baseline (`--parquet-zstd-level 3`)**, `ab7ec42`:
      - **wall 43:09**, peak RSS **36.3 GiB**, size **43 GiB**, summed-CPU total 40,737s
      - **analysis 11.0% · adjudication 74.3% · warehouse-write 11.1% · other 3.6%**
      - Pre-optimization (`a27642d`) baseline for contrast: wall 51:33, RSS 62.9 GiB, CPU 46,509s,
        split 9.1 / 71.2 / 13.5 / 6.2%. The perf work cut wall −16%, RSS −42%, CPU −12%;
        parquet output unchanged (Arc/allocation changes are byte-identical).

- [x] **Candidate (`--parquet-zstd-level 1`)**, `ab7ec42`:
      - wall 41:29, size **45 GiB (+5.6%)**, warehouse-write CPU 4,407s vs L3's 4,504s (**−2%**)
      - split 11.1 / 74.1 / 11.3 / 3.5% (≈ identical to L3)

- [x] **Row-count parity** (L3 vs L1), all 10 emitted tables **IDENTICAL** — PASS.
      (morphemes 662,984,226; morpheme_features 12,575,103,913; nway_feature_diffs 23,356,986,673;
      nway_regions 161,142,784; …). Oracle keyed 4-bucket diff: dropped=0, newly_emitted=0,
      classification_changed=0, unchanged=2,329,135. Classification breakdown 41.2 / 39.9 / 18.9%
      — identical to the prior canonical run, so the perf commits are content-neutral too.

- [x] **Read-back check** (L1): DuckDB `count(*)` on `morpheme_features` (12,575,103,913) and
      `nway_feature_diffs` (23,356,986,673) both parse without error at zstd-1 — PASS.

- [x] **Default decision: KEEP ZSTD level 3.** The codec is **not** a useful lever for this
      workload: warehouse-write is only ~11% of the run, and the tables are so compressible that
      level-3 encode is already cheap — dropping to level 1 cut write CPU by just 2% (≈5s wall at
      18-way parallelism; the ~100s wall gap between the two runs is run-to-run noise) while costing
      +5.6% disk. Level 3 stays the default; the `--parquet-zstd-level` knob remains available for
      other workloads.

- [x] **Next-lever note: the run is adjudication-bound (74.3%), NOT write-bound.** This refutes the
      prior documented "write-bound" hypothesis and redirects all future perf work to the oracle/n-way
      **row-building** path — the deferred levers below. Confirmed foreclosed by the measurement:
      **Lever 2** (analysis 11%) and **write-path / ZSTD tuning** (write 11%, codec-insensitive) are
      both low-payoff. The single-threaded merge/compaction tail sits outside every per-worker `total`,
      so it is not captured in this split (track separately via wall-clock; it is small — the giant
      tables are not recompacted).

## Deferred (documented, not this round)

Post-measurement, the payoff order is settled: the adjudication (oracle/n-way row-building)
path is the only high-value target (74% of the run). ZSTD/write-path and Lever 2 are foreclosed
by the split.

**Adjudication levers (the real targets — future rounds):**
- Replace the per-group `BTreeMap` in `WarehouseFeaturePatternAccumulator` with a linear scan over
  the contiguous group runs.
- Build arrow columns directly instead of materializing intermediate row-struct `Vec`s (attacks the
  row-struct→arrow transposition for the 23.4B/12.6B tables).
- Skip oracle-evidence JSON for fully-matching ruby bases.
- Per-analyzer row-collapse in `nway_feature_diffs` (store analyzer list per value-group) — cuts the
  ×N-analyzer fan-out; reduces both adjudication row-building and write. NOTE: key-pruning remains
  **foreclosed** (keep all feature keys, per user decision).

**Foreclosed by the measured split:**
- **ZSTD/write-path tuning** — write is 11% and codec-insensitive (level 1 cut write CPU 2%).
  Default stays 3; the knob remains for other workloads.
- **Lever 2 (intra-worker analyzer parallelism)** — analysis is only 11%.
- **Decoupling encode onto a separate per-worker thread** — write is only 11%.

(Several of the adjudication levers above are already landed on main as of `e25e4f5`/`ab7ec42`
via separate perf work — cursor-based whitespace checks, Arc-ified n-way rows, key-independent
hoisting in `feature_groups` — which is why the re-measured wall dropped 51:33 → 43:09.)
