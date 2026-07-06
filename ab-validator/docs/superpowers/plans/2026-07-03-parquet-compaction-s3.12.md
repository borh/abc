# §3.12 Parquet Compaction Post-Merge Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Coalesce the 3 small-part warehouse tables (analyses, sources, feature_pattern_counts — each ~562 tiny parts with median <1 MiB) into a single Parquet file per table after the merge staging loop, cutting total parts from ~5,060 → ~3,377 on the full corpus. The 8 large tables (median ≥1 MiB) keep their staged parts unchanged.

**Architecture:** Add `pub fn compact_staged_table` to `ab-warehouse::writer`, next to `stage_parquet_table_part` — keeping all staging-layout knowledge in the warehouse crate (so it unit-tests in isolation, not requiring full shard-run fixtures). It computes the **true median** part size, checks the threshold (`part_count > 64 AND median < 1 MiB`), and if qualifying, streams all staged parts through the existing tested `append_parquet_table_file` into a writer scoped to a **same-filesystem sibling temp dir**, then atomically replaces the staged parts. `merge_warehouse_shard_runs` calls it in a one-line loop. The coalescer (`append_parquet_table_file`) is already implemented and tested but `#[cfg(test)]`-gated — Task 1 un-gates it.

**Tech Stack:** Rust (edition 2024), `parquet`'s `ParquetRecordBatchReaderBuilder`, the existing `WarehouseWriter` / `WarehousePaths` / `append_parquet_table_file` in `crates/ab-warehouse/src/writer.rs`.

## Global Constraints

- **Behavior-preserving for query results.** Same rows, same schema, same column order. Only the file/row-group shape changes. DuckDB/Parquet readers see identical data.
- **No semantic change folded in.** Per `codebase-simplification`, this is a standalone perf commit. The threshold predicate comes from the decision doc, not invented here. Do not fix bugs or reshape schemas in this plan.
- **No new crate deps.** Uses only `parquet` (already a dep), `ab-warehouse` internals, `std::fs`.
- **TDD.** Each behavior change has a failing characterization test first, then the implementation, then green.
- **Before/after measurement.** Task 4 re-runs `benchmarks/run-morph-corpus.sh` and asserts `parts.total_parquet_parts` dropped and the 3 small tables each collapsed. If wall time regresses >10%, stop and report.
- **Threshold values (verbatim from the decision doc):** `part_count > 64` AND `median_part_bytes < 1_048_576` (1 MiB). **True median** (50th percentile of sorted part sizes), not arithmetic mean.
- **`WAREHOUSE_REGULAR_BATCH_SIZE` (§3.5) is NOT touched** — that's a separate deferred finding.
- **`WAREHOUSE_MAX_ROW_GROUP_SIZE = 50_000` is NOT touched.** A coalesced table becomes one `.parquet` file containing `ceil(rows/50_000)` row groups (e.g. analyses: 265,335 rows → 1 file, 6 row groups). The part COUNT is 1; do not claim "one row group."

---

## Hammock Synthesis

Current settled facts (verified 2026-07-03 against merged main):

- `merge_warehouse_shard_runs` (`crates/ab-morph-run/src/pipeline.rs:1054`) opens a writer for `[Runs, RunAnalyzers]` only, then for each `table in options.warehouse_profile.merged_data_tables()` and each `shard_index, run_dir` calls `stage_parquet_table_part` (renames shard parts into `staging_dir/<table>.parquet/part-<shard:05>-<part:05>.parquet`). Merged-data tables are never opened as writers — they're just renamed parts.
- `WarehouseWriter::finalize` (`writer.rs:441`) closes open writers, writes `views.sql`, then `finalize_staging_run` does `fs::rename(staging_dir, final_dir)` — staged `<table>.parquet/` dirs land in `final_dir/<table>.parquet/`.
- ⚠️ `WarehouseWriter::create_for_tables` (`writer.rs:45`) **unconditionally wipes** `paths.staging_dir` (lines 50-52: `fs::remove_dir_all(&paths.staging_dir)`). Therefore the coalescer CANNOT use the shared `paths` for its writer — it would destroy the other tables' staged parts. The coalescer must write to a **same-filesystem sibling temp dir** and move the result in.
- The coalescer `append_parquet_table_file` (`writer.rs:459`) streams a parquet file's `RecordBatch`es into `writer.append_record_batch`, which respects `WAREHOUSE_MAX_ROW_GROUP_SIZE = 50_000` (`writer.rs:22,629`). Tested by `append_parquet_table_file_coalesces_tiny_row_groups` (`writer.rs:1004`). Currently `#[cfg(test)]` — Task 1 un-gates it.
- `parquet_table_part_paths` (`writer.rs:525`) is **private** (`fn`, no `pub`) — lists `*.parquet` files in a dir, sorted. The new `compact_staged_table` lives in the same module, so it has access without making the fn public.
- `WarehousePaths` (`schema.rs:180`): `staging_dir`, `final_dir` are `pub PathBuf`; `new(warehouse_dir, run_id)` builds them. `table_data_dir(table)` returns `final_dir.join(table.file_name())`.
- `WarehouseWriter` typed appenders all exist and are public: `append_sources(&[SourceRow])`, `append_analyses`, `append_morphemes`, etc. (`writer.rs:180,198,215,…`). `append_record_batch(table, batch)` is the generic path (`writer.rs:380`).
- Baseline (`benchmarks/baselines/morph-run-2026-07-03/parts.json`): 3 tables cross the threshold — analyses (562 parts, median ~1,382 B), sources (562 parts, median ~1,927 B), feature_pattern_counts (562 parts, median ~49,247 B). Combined <2 GiB. The 8 other tables are all >5 MiB median.
- `ab_morph_run` accesses `ab_warehouse` via `crates/ab-morph-run/src/warehouse/mod.rs:3-4` (`pub use ab_warehouse::schema; pub use ab_warehouse::writer;`). So `compact_staged_table` is reachable as `warehouse::writer::compact_staged_table`.

Module-boundary decision (addresses a reviewer finding): place `compact_staged_table` in `ab-warehouse::writer`, not in `pipeline.rs`. It needs staging-layout knowledge (part paths, `finalize_staging_run`'s rename semantics) that belongs in the warehouse crate. `pipeline.rs` then calls it in a one-line loop — no warehouse internals leak.

Crux: the part-count arithmetic. 3 qualifying tables × (562 → 1) = net −1,683 parts. 5,060 − 1,683 = **3,377** expected total after. Each coalesced file contains `ceil(rows / 50_000)` row groups (analyses: ceil(265,335/50,000)=6; sources: ceil(106,134/50,000)=3; feature_pattern_counts: ceil(42,773,093/50,000)=856). Part count per table = 1 (one `.parquet` file), regardless of row-group count.

---

## Task 1: Un-gate `append_parquet_table_file`; add part-stats helpers

**Files:**
- Modify: `crates/ab-warehouse/src/writer.rs:457-458` (remove `#[cfg(test)]` from `append_parquet_table_file`, add doc comment)
- Modify: `crates/ab-warehouse/src/writer.rs` (add `pub fn parquet_table_part_sizes`)
- Test: `crates/ab-warehouse/src/writer.rs` `#[cfg(test)]` module

**Interfaces:**
- Produces:
  - `pub fn append_parquet_table_file(writer: &mut WarehouseWriter, table: WarehouseTable, path: &Path) -> Result<()>` (production-visible)
  - `pub fn parquet_table_part_sizes(dir: &Path) -> Result<Vec<u64>>` — sorted sizes of `*.parquet` files in `dir` (enables median).

- [ ] **Step 1:** Write failing tests for `parquet_table_part_sizes`.

  Add to the `#[cfg(test)]` module at the bottom of `crates/ab-warehouse/src/writer.rs`:

  ```rust
  #[test]
  fn parquet_table_part_sizes_returns_sorted_sizes_ignoring_non_parquet() {
      let root = temp_dir("part-sizes");
      fs::create_dir_all(&root).unwrap();
      fs::write(root.join("part-00001.parquet"), b"longer-junk-bytes").unwrap();
      fs::write(root.join("part-00000.parquet"), b"short").unwrap();
      fs::write(root.join("views.sql"), b"select 1").unwrap();
      let sizes = parquet_table_part_sizes(&root).unwrap();
      assert_eq!(sizes, vec![b"short".len() as u64, b"longer-junk-bytes".len() as u64]);
  }
  ```

- [ ] **Step 2:** Run to verify it fails.

  ```bash
  cargo test -p ab-warehouse --lib parquet_table_part_sizes_returns_sorted_sizes_ignoring_non_parquet 2>&1 | tail -5
  ```
  Expected: `cannot find function \`parquet_table_part_sizes\``.

- [ ] **Step 3:** Implement the helper, just after `parquet_table_part_paths` (around line 545):

  ```rust
  /// Sorted on-disk byte sizes of `.parquet` files in `dir` (ignoring non-parquet
  /// files like `views.sql`). Used by merge compaction to compute the true median
  /// part size for the threshold decision.
  ///
  /// # Errors
  ///
  /// Returns an error if `dir` cannot be read or a part cannot be stat'd.
  pub fn parquet_table_part_sizes(dir: &Path) -> Result<Vec<u64>> {
      let paths = parquet_table_part_paths(dir)?;
      let mut sizes: Vec<u64> = paths
          .iter()
          .map(|p| fs::metadata(p).map(|m| m.len()))
          .collect::<std::result::Result<Vec<_>, _>>()
          .with_context(|| format!("failed to stat parts in {}", dir.display()))?;
      sizes.sort_unstable();
      Ok(sizes)
  }
  ```

- [ ] **Step 4:** Un-gate `append_parquet_table_file`.

  At `crates/ab-warehouse/src/writer.rs:457`, remove the `#[cfg(test)]` line. Replace it with this doc comment above `pub fn append_parquet_table_file`:

  ```rust
  /// Append every row from a parquet file into `writer`'s table, re-using the
  /// writer's `WAREHOUSE_MAX_ROW_GROUP_SIZE` row-group sizing. Used by merge-time
  /// compaction to coalesce many tiny staged parts into one well-sized file.
  ///
  /// # Errors
  ///
  /// Returns an error if `path` cannot be opened or its batches fail to write.
  ```

- [ ] **Step 5:** Run the warehouse tests.

  ```bash
  cargo test -p ab-warehouse --lib 2>&1 | rg "test result|FAILED|error\[" | head
  ```
  Expected: `test result: ok. 25 passed; 0 failed` (24 existing + the 1 new). The existing `append_parquet_table_file_coalesces_tiny_row_groups` stays green.

- [ ] **Step 6:** Commit Task 1.

  ```bash
  git add crates/ab-warehouse/src/writer.rs
  git commit -m "feat(warehouse): un-gate append_parquet_table_file; add parquet_table_part_sizes"
  ```

---

## Task 2: Add `compact_staged_table` in `ab-warehouse::writer`

**Files:**
- Modify: `crates/ab-warehouse/src/writer.rs` (add `compact_staged_table` + threshold consts)
- Test: `crates/ab-warehouse/src/writer.rs` `#[cfg(test)]` module (unit test, no shard fixtures needed)

**Interfaces:**
- Consumes: `append_parquet_table_file`, `parquet_table_part_paths`, `parquet_table_part_sizes`, `WarehouseWriter::create_for_tables`, `WarehousePaths`, `WarehouseTable` (all same-module — no cross-crate visibility issue).
- Produces: `pub fn compact_staged_table(paths: &WarehousePaths, table: WarehouseTable) -> Result<bool>` — returns `true` if it compacted, `false` if the table didn't qualify. Called by `merge_warehouse_shard_runs` in Task 3.

- [ ] **Step 1:** Write the failing characterization test.

  Add to the `#[cfg(test)]` module in `crates/ab-warehouse/src/writer.rs`:

  ```rust
  #[test]
  fn compact_staged_table_coalesces_many_small_parts_into_one_file() {
      // Build a staging dir with 65 tiny part files for Sources — just over the
      // 64-part threshold, each well under 1 MiB median.
      let root = temp_dir("compact-many-small");
      let paths = WarehousePaths::new(&root, "r");
      let sources_staged = paths.staging_dir.join(WarehouseTable::Sources.file_name());
      fs::create_dir_all(&sources_staged).unwrap();
      for i in 0..65u32 {
          // 100-byte parquet: write a real tiny sources parquet via ArrowWriter so
          // append_parquet_table_file can read it back. Reuse the test helpers used
          // by append_parquet_table_file_coalesces_tiny_row_groups (write_sources_part).
          let path = sources_staged.join(format!("part-{i:05}.parquet"));
          write_sources_part(&path, &format!("s{i}"), 1);
      }
      let compacted = compact_staged_table(&paths, WarehouseTable::Sources).unwrap();
      assert!(compacted, "should compact (65 parts, median <1 MiB)");
      let remaining: Vec<_> = fs::read_dir(&sources_staged)
          .unwrap()
          .filter_map(Result::ok)
          .filter(|e| e.path().extension().is_some_and(|x| x == "parquet"))
          .collect();
      assert_eq!(remaining.len(), 1, "coalesced to a single file");
  }

  #[test]
  fn compact_staged_table_skips_when_median_too_large() {
      // 65 parts but each >1 MiB → median ≥ 1 MiB → skip (no compaction).
      let root = temp_dir("compact-skip-large");
      let paths = WarehousePaths::new(&root, "r");
      let sources_staged = paths.staging_dir.join(WarehouseTable::Sources.file_name());
      fs::create_dir_all(&sources_staged).unwrap();
      for i in 0..65u32 {
          let path = sources_staged.join(format!("part-{i:05}.parquet"));
          // Write a real sources part, then pad the file to >1 MiB so it crosses
          // the median threshold.
          write_sources_part(&path, &format!("s{i}"), 1);
          let mut f = std::fs::OpenOptions::new().append(true).open(&path).unwrap();
          f.write_all(&vec![0u8; 1_100_000]).unwrap();
      }
      let compacted = compact_staged_table(&paths, WarehouseTable::Sources).unwrap();
      assert!(!compacted, "should NOT compact (median ≥1 MiB)");
      let remaining: Vec<_> = fs::read_dir(&sources_staged)
          .unwrap()
          .filter_map(Result::ok)
          .filter(|e| e.path().extension().is_some_and(|x| x == "parquet"))
          .collect();
      assert_eq!(remaining.len(), 65, "parts unchanged");
  }
  ```

  If `write_sources_part` is not already a test helper in the module, find the analogous helper in `append_parquet_table_file_coalesces_tiny_row_groups` (the existing test at `writer.rs:1004` writes a `sources.parquet` via `ArrowWriter` — copy that helper, renamed `write_sources_part(path, source_id, rows)`). The point of the test is the threshold logic + the post-condition (1 file vs N), not the parquet content; reuse whatever the existing test uses to write a readable sources parquet.

- [ ] **Step 2:** Run the tests to verify they fail.

  ```bash
  cargo test -p ab-warehouse --lib compact_staged_table 2>&1 | tail -5
  ```
  Expected: `cannot find function \`compact_staged_table\``.

- [ ] **Step 3:** Add the threshold consts near `WAREHOUSE_MAX_ROW_GROUP_SIZE` (around `writer.rs:22`):

  ```rust
  /// Merge-time compaction threshold (§3.12):
  /// coalesce a staged table iff it has more than this many parts AND the
  /// true median part is smaller than `COMPACTION_MAX_MEDIAN_PART_BYTES`.
  /// Values from `docs/superpowers/reports/2026-07-03-morph-perf-decision.md`.
  pub(crate) const COMPACTION_MIN_PART_COUNT: usize = 64;
  pub(crate) const COMPACTION_MAX_MEDIAN_PART_BYTES: u64 = 1_048_576; // 1 MiB
  ```

- [ ] **Step 4:** Implement `compact_staged_table`, placed just above `stage_parquet_table_part` (around `writer.rs:490`):

  ```rust
  /// Compact a table's staged parts into a single parquet file when it has many
  /// small parts. Threshold: `part_count > 64 AND median_part_bytes < 1 MiB`
  /// (§3.12 decision). Large tables (median ≥1 MiB) are returned untouched.
  ///
  /// Implementation: because `WarehouseWriter::create_for_tables` wipes its
  /// staging dir, the coalesced output is written to a **same-filesystem
  /// sibling temp dir** (`<paths.warehouse_dir>/.compact-<run_id>-<table>`),
  /// finalized there (staging→final via `finalize_staging_run`), then the
  /// single coalesced part is moved into the real staging dir, replacing the
  /// many small parts. Same-FS `fs::rename` is atomic; `fs::copy` is the
  /// fallback only if rename fails across mounts.
  ///
  /// Returns `true` if the table was compacted, `false` if it was left as-is.
  ///
  /// # Errors
  ///
  /// Returns an error if staging can't be read, the coalesce writer fails, or
  /// the part replacement can't be completed.
  pub fn compact_staged_table(paths: &WarehousePaths, table: WarehouseTable) -> Result<bool> {
      let staged = paths.staging_dir.join(table.file_name());
      if !staged.is_dir() {
          return Ok(false);
      }
      let sizes = parquet_table_part_sizes(&staged)?;
      if sizes.len() <= COMPACTION_MIN_PART_COUNT {
          return Ok(false);
      }
      let median = sizes[sizes.len() / 2];
      if median >= COMPACTION_MAX_MEDIAN_PART_BYTES {
          log::info!(
              "warehouse compaction: skipping {} ({} parts, median {}B ≥ {}B)",
              table.file_name(), sizes.len(), median, COMPACTION_MAX_MEDIAN_PART_BYTES
          );
          return Ok(false);
      }
      log::info!(
          "warehouse compaction: compacting {} ({} parts, median {}B) → 1 file",
          table.file_name(), sizes.len(), median
      );
      let part_paths = parquet_table_part_paths(&staged)?;

      // Coalesce into a same-FS sibling temp dir so rename is atomic.
      let compact_dir = paths.warehouse_dir.join(format!(
          ".compact-{}-{}", paths.run_id, table.file_name()
      ));
      if compact_dir.exists() {
          fs::remove_dir_all(&compact_dir)
              .with_context(|| format!("remove stale {}", compact_dir.display()))?;
      }
      let compact_paths = WarehousePaths::new(&compact_dir, "compact");
      let mut writer = WarehouseWriter::create_for_tables(compact_paths.clone(), &[table])?;
      for part in &part_paths {
          append_parquet_table_file(&mut writer, table, part)?;
      }
      writer.finalize()?;
      // finalize moved compact_paths.staging_dir → compact_paths.final_dir;
      // the single coalesced file is at final_dir/<table>.parquet/part-00000.parquet
      // (the writer writes one part because one writer = one part file).
      let compacted_dir = compact_paths.final_dir.join(table.file_name());
      // Replace the staged parts: delete old, move coalesced in.
      for part in &part_paths {
          fs::remove_file(part).with_context(|| format!("remove staged {}", part.display()))?;
      }
      for entry in fs::read_dir(&compacted_dir)? {
          let entry = entry?;
          let dest = staged.join(entry.file_name());
          fs::rename(entry.path(), &dest)
              .with_context(|| format!("move coalesced {} → {}", entry.path().display(), dest.display()))?;
      }
      // Clean up the temp compact dir (its staging dir is already moved by finalize).
      let _ = fs::remove_dir_all(&compact_dir);
      Ok(true)
  }
  ```

  Note on the median index: `sizes` is sorted ascending; `sizes[len/2]` is the 50th-percentile element. For an even count (e.g. 562), this picks element 281 — the standard "lower median," matching the decision doc's intent. For an odd count it's the exact middle.

- [ ] **Step 5:** Run the tests.

  ```bash
  cargo test -p ab-warehouse --lib compact_staged_table 2>&1 | rg "test result|FAILED|error\[" | head
  ```
  Expected: both new tests green.

- [ ] **Step 6:** Commit Task 2.

  ```bash
  git add crates/ab-warehouse/src/writer.rs
  git commit -m "feat(warehouse): compact_staged_table (§3.12 post-merge compaction)

  Adds compact_staged_table(paths, table) -> Result<bool>: after merge staging,
  if a table has >64 parts AND true-median part <1 MiB, rewrite all staged
  parts through append_parquet_table_file into a single file via a same-FS
  sibling temp writer (create_for_tables wipes its own staging, so the shared
  staging can't be reused). Large tables are skipped. Thresholds and median
  semantics from docs/superpowers/reports/2026-07-03-morph-perf-decision.md.

  Lives in ab-warehouse::writer (not pipeline.rs) so staging-layout knowledge
  stays cohesive and the fn is unit-testable in isolation. pipeline.rs will
  call it in a one-line loop (Task 3)."
  ```

---

## Task 3: Wire `compact_staged_table` into `merge_warehouse_shard_runs`

**Files:**
- Modify: `crates/ab-morph-run/src/pipeline.rs` (the `merge_warehouse_shard_runs` fn, ~line 1054)

**Interfaces:**
- Consumes: `warehouse::writer::compact_staged_table` (from Task 2).
- Produces: the merge calls compaction after staging completes for all tables.

- [ ] **Step 1:** Write the failing integration test in `crates/ab-morph-run/src/pipeline.rs` `#[cfg(test)]` module.

  This test builds 2 shard run-dirs each writing a 1-row `sources.parquet` (so 2 staged parts, well under threshold median), runs `merge_warehouse_shard_runs`, and asserts the merged `final_dir` has exactly 1 parquet file for sources. To get past the `>64` threshold with few shards, this test uses a dedicated small-shard variant OR you relax the test to build 65 tiny shards — choose whichever matches the existing test-fixture ergonomics. Prefer the 65-shard form so the threshold is actually exercised:

  ```rust
  #[test]
  fn merge_warehouse_shard_runs_coalesces_small_part_tables() {
      use ab_warehouse::schema::{SourceRow, WarehousePaths, WarehouseTable};
      use ab_warehouse::writer as wh;
      let root = tempfile::tempdir().unwrap();
      let warehouse_dir = root.path().join("wh");
      // 65 shards, each with a tiny 1-row sources.parquet → 65 staged parts,
      // median well under 1 MiB → compaction triggers.
      let mut shard_dirs = Vec::new();
      for i in 0..65u64 {
          let shard_dir = root.path().join(format!("shard-{i}"));
          fs::create_dir_all(&shard_dir).unwrap();
          let shard_paths = WarehousePaths::new(&shard_dir, "shard");
          let mut w = wh::WarehouseWriter::create(shard_paths.clone()).unwrap();
          w.append_sources(&[SourceRow {
              run_id: "r".to_owned(),
              source_id: format!("s{i}"),
              text_id: format!("t{i}"),
              aat_path: format!("aat/{i}.json"),
              source_bytes: 1,
              source_chars: 1,
          }]).unwrap();
          w.finalize().unwrap();
          shard_dirs.push(shard_paths.final_dir);
      }
      let options = WarehouseParallelOptions {
          warehouse_dir: warehouse_dir.clone(),
          run_id: "merged".to_owned(),
          warehouse_profile: WarehouseProfile::Full,
          analyzer_rows: vec![],
          input_mode: "test".to_owned(),
          input_path: "test".to_owned(),
      };
      merge_warehouse_shard_runs(&options, &shard_dirs).unwrap();
      let merged = WarehousePaths::new(&warehouse_dir, "merged");
      let sources_dir = merged.final_dir.join(WarehouseTable::Sources.file_name());
      let sources_parts: Vec<_> = fs::read_dir(&sources_dir)
          .unwrap()
          .filter_map(Result::ok)
          .filter(|e| e.path().extension().is_some_and(|x| x == "parquet"))
          .collect();
      assert_eq!(sources_parts.len(), 1, "sources should be coalesced to 1 file");
  }
  ```

  Verify the `WarehouseParallelOptions` field set against the actual struct (search `rg "struct WarehouseParallelOptions" -A8 crates/ab-morph-run/src/`) and the `SourceRow` field set against `crates/ab-warehouse/src/schema.rs` before running.

- [ ] **Step 2:** Run to verify it fails.

  ```bash
  cargo test -p ab-morph-run --lib merge_warehouse_shard_runs_coalesces_small_part_tables 2>&1 | tail -5
  ```
  Expected: FAIL with `sources_parts.len() == 1` (current merge leaves 65 staged parts → 65 in final_dir).

- [ ] **Step 3:** Wire in the call.

  In `merge_warehouse_shard_runs` (`crates/ab-morph-run/src/pipeline.rs`), after the staging double-loop:

  ```rust
          for (shard_index, run_dir) in shard_run_dirs.iter().enumerate() {
              stage_parquet_table_part(
                  &paths.staging_dir,
                  *table,
                  shard_index,
                  &run_dir.join(table.file_name()),
              )?;
          }
      }
  ```

  and **before** `writer.append_runs(&[RunRow { … }])?;`, add:

  ```rust
      // §3.12: coalesce small-part tables (analyses, sources, feature_pattern_counts
      // on the full corpus) into a single file each. Large tables are skipped
      // inside compact_staged_table.
      for &table in options.warehouse_profile.merged_data_tables() {
          warehouse::writer::compact_staged_table(&paths, table)?;
      }
  ```

  (`warehouse::writer` is the re-export from `crates/ab-morph-run/src/warehouse/mod.rs:4`. If the import path differs in pipeline.rs — check the top of the file — adjust accordingly.)

- [ ] **Step 4:** Run the ab-morph-run tests.

  ```bash
  cargo test -p ab-morph-run --lib 2>&1 | rg "test result|FAILED|error\[" | head
  ```
  Expected: `test result: ok. 88 passed; 0 failed` (87 existing + 1 new). Existing tests don't regress — compaction only triggers on >64 parts <1 MiB median, which existing fixtures (few shards) don't hit.

- [ ] **Step 5:** Commit Task 3.

  ```bash
  git add crates/ab-morph-run/src/pipeline.rs
  git commit -m "perf(morph-run): call compact_staged_table in merge_warehouse_shard_runs"
  ```

---

## Task 4: Full workspace regression, clippy, fmt

- [ ] **Step 1:** Full workspace tests.

  ```bash
  cargo test --workspace 2>&1 | grep -cE "FAILED|error\[|panicked:"
  ```
  Expected: `0`.

- [ ] **Step 2:** Clippy on touched crates.

  ```bash
  cargo clippy -p ab-warehouse -p ab-morph-run --all-targets 2>&1 | grep -E "^error|^warning" | head
  ```
  Expected: no output.

- [ ] **Step 3:** Format check.

  ```bash
  cargo fmt --all -- --check
  ```
  Expected: no diff. If diff, run `cargo fmt --all`, inspect, commit.

- [ ] **Step 4:** If any fixup was needed, commit it.

  ```bash
  git add -A && git commit -m "chore: fmt/clippy fixups from §3.12 compaction"
  ```

---

## Task 5: Before/after measurement via the bench harness

**Files:**
- Read: `benchmarks/run-morph-corpus.sh`
- Read: `benchmarks/baselines/morph-run-2026-07-03/parts.json` (before — on main)
- Generated (NOT committed): warehouse output under `/db/ab-validator/morph-warehouse-bench-after`

- [ ] **Step 1:** Capture the after measurement by re-running the harness (single j=32 run, ~31 min).

  ```bash
  SUDACHI="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic"
  AB_MORPH_AAT_DIR=/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter \
  AB_MORPH_WAREHOUSE_DIR=/db/ab-validator/morph-warehouse-bench-after \
  AB_MORPH_JOBS="32 32" \
  AB_BENCH_OUT="$(pwd)/benchmarks/baselines/morph-run-2026-07-03-after" \
  AB_SUDACHI_DICT="$SUDACHI" \
  timeout 3000 bash benchmarks/run-morph-corpus.sh 2>&1 | tail -5
  ```
  The harness builds ab-morph-run from the current (compacted) source, so this run uses the new code.

  Note: if the harness's `--argjson speedup` step errors on `null` (because `jobs_list="32 32"` has no jobs==1), that's fine — `parts.json` and `summary.json` (minus speedup) are already written by that point. Reconstruct `summary.json` from `time-j32.txt` + `parts.json` the same way the morph-perf baseline did (see commit `8ff1af4`).

- [ ] **Step 2:** Compare before/after.

  ```bash
  before=benchmarks/baselines/morph-run-2026-07-03/parts.json
  after=benchmarks/baselines/morph-run-2026-07-03-after/parts.json
  echo "--- total parts before / after ---"
  jq '.total_parquet_parts' "$before" "$after"
  echo "--- qualifying tables: parts before → after (expect 562 → 1) ---"
  for t in analyses sources feature_pattern_counts; do
    b=$(jq -r --arg t "$t" '.tables[] | select(.table==$t) | .parts' "$before")
    a=$(jq -r --arg t "$t" '.tables[] | select(.table==$t) | .parts' "$after")
    echo "$t: $b → $a"
  done
  echo "--- control: a large table (morphemes) should be unchanged ---"
  jq -r --arg t morphemes '.tables[] | select(.table==$t) | "morphemes parts: \(.parts)"' "$before" "$after"
  echo "--- wall time before / after ---"
  jq '.primary.wall_seconds' benchmarks/baselines/morph-run-2026-07-03/summary.json \
                             benchmarks/baselines/morph-run-2026-07-03-after/summary.json
  ```

- [ ] **Step 3:** Assert success criteria.

  Pass if:
  - `after.total_parquet_parts ≈ 3,377` (5,060 − 1,683; tolerate ±50 for run-to-run variance).
  - analyses, sources, feature_pattern_counts each `562 → 1`.
  - morphemes unchanged (562).
  - `after.wall_seconds ≤ before.wall_seconds × 1.10` (no >10% regression; coalescing <2 GiB is ~seconds vs the 30-min run).

  If wall time regressed >10%, STOP — do not commit the after data. Investigate (e.g. a large table crossing the threshold incorrectly — check the median computation).

- [ ] **Step 4:** Commit the after-measurement artifacts + the baseline doc.

  ```bash
  git add benchmarks/baselines/morph-run-2026-07-03-after/summary.json \
          benchmarks/baselines/morph-run-2026-07-03-after/parts.json
  git commit -m "docs: §3.12 after-measurement (parts 5060 → ~3377, wall no regression)"
  ```

- [ ] **Step 5:** Clean up generated warehouse output.

  ```bash
  rm -rf /db/ab-validator/morph-warehouse-bench-after
  ```

---

## Self-Review

- [x] **Spec coverage:** Decision doc §3.12 threshold (>64 parts AND median <1 MiB) → Task 2 implements exactly that, with the true median. Task 5 measures the before/after. The 8 large tables are explicitly untouched (predicate skips them via the median check). `compact_staged_table` is unit-tested in isolation (Task 2) AND integration-tested via the merge (Task 3).
- [x] **Placeholder scan:** No TBD/TODO. Every code step has complete code. The one "verify the struct field set before running" note in Task 3 Step 1 is a verification checkpoint with the search command given, not a placeholder — the test body is complete.
- [x] **Type consistency:** `WarehousePaths`, `WarehouseTable`, `WarehouseWriter` are canonical from `ab-warehouse::schema` / `ab_warehouse::writer`. `compact_staged_table(&WarehousePaths, WarehouseTable) -> Result<bool>` is called as `warehouse::writer::compact_staged_table(&paths, table)` (re-export verified at `warehouse/mod.rs:4`). `append_sources(&[SourceRow])` is verified real at `writer.rs:180`.
- [x] **Threshold values verbatim:** `COMPACTION_MIN_PART_COUNT = 64`, `COMPACTION_MAX_MEDIAN_PART_BYTES = 1_048_576` — match the decision doc. Median is the true 50th percentile (`sizes[len/2]` on a sorted vec), not arithmetic mean — addresses review finding #1.
- [x] **Module boundary:** `compact_staged_table` lives in `ab-warehouse::writer` (not pipeline.rs) so staging-layout knowledge stays cohesive and the fn is unit-testable in isolation — addresses review finding #7, which also resolves #3 (private `parquet_table_part_paths` is same-module access, no pub needed).
- [x] **Staging-wipe safety:** Uses a same-FS sibling temp dir (`<warehouse_dir>/.compact-<run_id>-<table>`), not `tempfile::tempdir()` (which may be tmpfs). `fs::rename` is atomic on the same FS — addresses review findings #4 and #6.
- [x] **Part-count projection:** 5,060 − (3 × 561) = 3,377 expected (addresses #2). Coalesced files contain `ceil(rows/50_000)` row groups, not "one row group" — wording corrected.
- [x] **Logging:** `log::info!` on both the compact and skip paths (addresses #8).
- [x] **TDD:** Task 1 = failing test → impl → green. Task 2 = two failing tests (compact + skip) → impl → green. Task 3 = failing merge test → wire → green. Task 5 = measurement gate.

---

## Execution Handoff

Plan complete and saved to `docs/superpowers/plans/2026-07-03-parquet-compaction-s3.12.md` (revised after critical review). Two execution options:

1. **Subagent-Driven (recommended)** — dispatch a fresh subagent per task, review between tasks, fast iteration.
2. **Inline Execution** — execute tasks in this session using executing-plans, batch execution with checkpoints.

Which approach?
