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

## Validation Record (hinoki)

Fill in after the branch is merged and built on hinoki. The measurement is the primary deliverable of this round.

- [ ] Build instrumented binary on hinoki (from `ab-validator/`, release).
- [ ] **Baseline (`--parquet-zstd-level 3`)** full-corpus run: record `/usr/bin/time` wall-clock, `du -sh` of the run dir, and the `phase-timings:` line. **This confirms or refutes the write-bound hypothesis.**
      - total wall: ____   analysis %: ____   adjudication %: ____   warehouse-write %: ____   other %: ____
- [ ] **Candidate (`--parquet-zstd-level 1`)** full-corpus run: same three measurements.
      - total wall: ____   output size: ____   Δ vs baseline: ____
- [ ] **Row-count parity** (baseline vs candidate) via `scripts/oracle-validation-diff.sh` non-oracle section: all 13 tables identical (content-neutral). PASS/FAIL: ____
- [ ] **Read-back check:** DuckDB `SELECT count(*)` + a `LIMIT 1000` scan on `morpheme_features` and `nway_feature_diffs` of the candidate run parse without error. PASS/FAIL: ____
- [ ] **Default decision:** if level 1 gives a material wall-clock win at acceptable disk cost, change the default to 1 (a one-line edit + test update) and note it here; otherwise keep 3. Decision: ____
- [ ] **Next-lever note:** record what the measured split implies for the deferred levers (Lever 2 analyzer parallelism only worth it if analysis % is large; per-analyzer row-collapse / id-encoding only worth it if warehouse-write % dominates; a large adjudication % would point at the oracle/nway row-building code as the lever instead). Note: the single-threaded merge/compaction tail is outside every per-worker `total`, so it is not in this split — track it separately via wall-clock.

## Deferred (documented, not this round)

- Data-volume reduction by key-pruning — **foreclosed** (keep all keys).
- Per-analyzer row-collapse in `nway_feature_diffs`; id-column dictionary/int encoding — future round, gated on the measured write %.
- Lever 2 (intra-worker analyzer parallelism) — gated on the measured analysis %.
- Decoupling encode onto a separate per-worker thread — gated on the measured write %.
