# §3.12 Parquet Compaction Post-Merge Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a compaction pass to `merge_warehouse_shard_runs` so the small-part tables (analyses, sources, feature_pattern_counts — each ~562 tiny parts with median <1 MiB) get coalesced into one row-group-sized file, cutting total Parquet parts from ~5,060 → ~3,940 on the full corpus. The 8 large tables keep their staged parts as-is (no benefit coalescing ~10 MiB/row-group tables into a 50 GiB file).

**Architecture:** After the existing staging loop copies shard parts into `staging_dir/<table>/`, add a per-table predicate: count parts and sum their bytes; if `part_count > 64 AND median_part_bytes < 1 MiB`, open a `WarehouseWriter` for that table, stream each staged part through the existing tested `append_parquet_table_file`, delete the staged parts, and let `finalize_staging_run` move the one coalesced file to `final_dir`. The predicate matches the decision doc's threshold exactly (`docs/superpowers/reports/2026-07-03-morph-perf-decision.md`), so no magic table list. The coalescer is already implemented and tested but is `#[cfg(test)]`-gated — Task 1 un-gates it.

**Tech Stack:** Rust (edition 2024), `parquet` crate's `ParquetRecordBatchReaderBuilder`, the existing `WarehouseWriter` / `WarehousePaths` / `append_parquet_table_file` in `crates/ab-warehouse/src/writer.rs`.

## Global Constraints

- **Behavior-preserving for query results.** Same rows, same schema, same column order. The only change is fewer files with larger row groups. DuckDB/Parquet readers see identical data.
- **No semantic change folded in.** Per `codebase-simplification`, this is a standalone perf commit. The threshold predicate comes from the decision doc, not invented here. Do not fix bugs or reshape schemas in this plan.
- **No new crate deps.** Uses only `parquet` (already a dep), `ab-warehouse` internals, `std::fs`.
- **TDD.** Each behavior change has a failing characterization test first, then the implementation, then green.
- **Before/after measurement.** Task 4 re-runs `benchmarks/run-morph-corpus.sh` and asserts `parts.total_parquet_parts` dropped and the 3 small tables each collapsed. If wall time regresses >10%, stop and report.
- **Threshold values (verbatim from the decision doc):** `part_count > 64` AND `median_part_bytes < 1_048_576` (1 MiB). Do not tune these numbers.
- **`WAREHOUSE_REGULAR_BATCH_SIZE` is NOT touched** in this plan (that's §3.5, deferred).

---

## Hammock Synthesis

Current settled facts (verified 2026-07-03 against the merged main):

- `merge_warehouse_shard_runs` (`crates/ab-morph-run/src/pipeline.rs:1054`) opens a writer for `[Runs, RunAnalyzers]` only (`WarehouseWriter::create_for_tables` at `writer.rs:45`), then for each `table in options.warehouse_profile.merged_data_tables()` and each `shard_index, run_dir` calls `stage_parquet_table_part` (renames the shard's parts into `staging_dir/<table>/part-<shard:05>-<part:05>.parquet`). The merged-data tables are never opened as writers — they're just renamed parts.
- `WarehouseWriter::finalize` (`writer.rs:441`) closes the open writers (Runs/RunAnalyzers only), writes `views.sql`, then `finalize_staging_run` does `fs::rename(staging_dir, final_dir)` — so anything staged under `staging_dir/<table>/` lands in `final_dir/<table>/`.
- The coalescer `append_parquet_table_file` (`writer.rs:459`) opens a parquet file via `ParquetRecordBatchReaderBuilder`, streams its `RecordBatch`es into `writer.append_record_batch(table, batch)`, which writes them into the table's `ArrowWriter` (which respects the 50k row-group size). It is tested by `append_parquet_table_file_coalesces_tiny_row_groups` (`writer.rs:1004`). It is `#[cfg(test)]`-gated — Task 1 removes that gate.
- `parquet_table_part_paths` (`writer.rs:525`, private) lists `*.parquet` files in a dir, sorted. `parquet_table_row_count` (`writer.rs:478`, pub) sums row counts.
- Baseline (`benchmarks/baselines/morph-run-2026-07-03/parts.json`): 3 tables cross the threshold — analyses (562 parts, median 1,382 B), sources (562 parts, median 1,927 B), feature_pattern_counts (562 parts, median 49,247 B). Combined <2 GiB. The 8 other tables are all >5 MiB median and stay as-is.
- `ab_morph_run` accesses `ab_warehouse` via `crates/ab-morph-run/src/warehouse/mod.rs:3-4` which re-exports `ab_warehouse::schema` and `ab_warehouse::writer`. So new pub items in `ab_warehouse::writer` are reachable from `ab_morph_run` as `warehouse::writer::…`.

Chosen direction: add a `compact_small_part_tables` step to `merge_warehouse_shard_runs` between the staging loop and `writer.finalize()`. For each merged-data table, compute part_count + total_bytes after staging; if the threshold is crossed, open a writer for that table, coalesce, and replace the staged parts. The writer's per-table row-group sizing (50k) automatically produces well-sized row groups.

One subtlety: `WarehouseWriter::create_for_tables` opens writers for exactly the tables in its arg list and `finalize` closes whichever are `Some`. So the coalescing pass must add the small-part tables to the `create_for_tables` arg list when they qualify, OR open a separate writer. Opening a separate writer per qualifying table is simpler and avoids reshaping the merge's writer construction — but `WarehousePaths` only allows one staging dir per run (the writers write into the same `staging_dir`). Verifying: `open_optional_table_writer` writes into `paths.staging_dir.join(table.file_name())` — so two writers for the same table would clash. Therefore the coalescing pass must (a) open the writer for the table, (b) read all staged parts for that table from `staging_dir/<table>/`, (c) coalesce them into the writer, (d) delete the original staged parts so `finalize` doesn't also move them. Because staging rewrites the *same* `<table>.parquet/` dir, the coalesced output must go to a distinct name the writer writes to, and the staged parts must be removed before `finalize`. The cleanest path: coalesce into the writer (which writes `<table>.parquet` in staging), then delete the `part-*.parquet` files that `stage_parquet_table_part` created.

---

## Task 1: Un-gate `append_parquet_table_file` and add a part-stats helper

**Files:**
- Modify: `crates/ab-warehouse/src/writer.rs:457` (remove `#[cfg(test)]` from `append_parquet_table_file`)
- Modify: `crates/ab-warehouse/src/writer.rs` (add `pub fn parquet_table_part_count_and_bytes`)
- Test: characterization via the existing `append_parquet_table_file_coalesces_tiny_row_groups` test (already green — this task only un-gates and adds a helper)

**Interfaces:**
- Produces: `pub fn append_parquet_table_file(writer: &mut WarehouseWriter, table: WarehouseTable, path: &Path) -> Result<()>` (production-visible) and `pub fn parquet_table_part_count_and_bytes(dir: &Path) -> Result<(usize, u64)>` returning `(part_count, sum_of_file_sizes_in_bytes)`.

- [ ] **Step 1:** Write a failing test for the new part-stats helper.

  Add to the `#[cfg(test)]` module at the bottom of `crates/ab-warehouse/src/writer.rs`:

  ```rust
  #[test]
  fn parquet_table_part_count_and_bytes_counts_only_parquet_files() {
      let root = temp_dir("part-stats-counts");
      fs::create_dir_all(&root).unwrap();
      // Two .parquet files + one non-parquet (must be ignored).
      fs::write(root.join("part-00000.parquet"), b"junk").unwrap();
      fs::write(root.join("part-00001.parquet"), b"junk-longer").unwrap();
      fs::write(root.join("views.sql"), b"select 1").unwrap();
      let (count, bytes) = parquet_table_part_count_and_bytes(&root).unwrap();
      assert_eq!(count, 2);
      assert_eq!(bytes, b"junk".len() as u64 + b"junk-longer".len() as u64);
  }
  ```

- [ ] **Step 2:** Run the test to verify it fails.

  ```bash
  cargo test -p ab-warehouse --lib parquet_table_part_count_and_bytes_counts_only_parquet_files 2>&1 | tail -5
  ```
  Expected: `error[E0425]: cannot find function, variable, or type \`parquet_table_part_count_and_bytes\`` (or `unresolved import`).

- [ ] **Step 3:** Implement the helper next to `parquet_table_part_paths`.

  In `crates/ab-warehouse/src/writer.rs`, just after the `parquet_table_part_paths` fn (around line 545), add:

  ```rust
  /// Count `.parquet` files in `dir` and sum their on-disk byte sizes.
  ///
  /// Used by the merge compaction predicate to decide whether a staged table
  /// has many small parts worth coalescing. Non-parquet files (e.g. `views.sql`)
  /// are ignored.
  ///
  /// # Errors
  ///
  /// Returns an error if `dir` cannot be read.
  pub fn parquet_table_part_count_and_bytes(dir: &Path) -> Result<(usize, u64)> {
      let paths = parquet_table_part_paths(dir)?;
      let bytes = paths
          .iter()
          .map(|p| fs::metadata(p).map(|m| m.len()))
          .collect::<std::result::Result<Vec<_>, _>>()
          .with_context(|| format!("failed to stat parts in {}", dir.display()))?;
      Ok((paths.len(), bytes.into_iter().sum()))
  }
  ```

- [ ] **Step 4:** Un-gate `append_parquet_table_file`.

  In `crates/ab-warehouse/src/writer.rs:457`, remove the `#[cfg(test)]` attribute line above `pub fn append_parquet_table_file`. Add a doc comment instead:

  ```rust
  /// Append every row from a parquet file into `writer`'s table, re-using the
  /// writer's row-group sizing. Used by merge-time compaction to coalesce many
  /// tiny staged parts into one well-sized file.
  ///
  /// # Errors
  ///
  /// Returns an error if `path` cannot be opened or its batches fail to write.
  pub fn append_parquet_table_file(
      writer: &mut WarehouseWriter,
      table: WarehouseTable,
      path: &Path,
  ) -> Result<()> {
  ```

- [ ] **Step 5:** Run the tests to verify they pass.

  ```bash
  cargo test -p ab-warehouse --lib 2>&1 | rg "test result|FAILED|error\[" | head
  ```
  Expected: `test result: ok. 24 passed; 0 failed` (the existing coalesce test stays green; the new stats test passes).

- [ ] **Step 6:** Commit Task 1.

  ```bash
  git add crates/ab-warehouse/src/writer.rs
  git commit -m "feat(warehouse): un-gate append_parquet_table_file; add part-stats helper"
  ```

---

## Task 2: Add the threshold predicate and a coalescing helper

**Files:**
- Modify: `crates/ab-morph-run/src/pipeline.rs` (add `compact_small_part_tables` helper + the threshold constant)
- Test: `crates/ab-morph-run/src/pipeline.rs` `#[cfg(test)]` module

**Interfaces:**
- Consumes: `warehouse::writer::{append_parquet_table_file, parquet_table_part_count_and_bytes}`, `WarehouseWriter::create_for_tables`, `WarehousePaths`, `WarehouseTable` (all from Task 1 / existing).
- Produces: `fn compact_small_part_tables(paths: &WarehousePaths, tables: &[WarehouseTable]) -> Result<()>` (private to `pipeline.rs`). Called by `merge_warehouse_shard_runs` after the staging loop.

- [ ] **Step 1:** Write the failing characterization test for the coalescing behavior.

  Add to the `#[cfg(test)]` module in `crates/ab-morph-run/src/pipeline.rs`. This test builds 2 shard run-dirs each writing a tiny `<table>.parquet` for `Sources` (so 2 staged parts, under the 1 MiB threshold), runs `merge_warehouse_shard_runs`, and asserts the merged `final_dir` has exactly 1 parquet file for sources (coalesced) while a control table that doesn't qualify keeps its parts. Reuse the test helpers already used in the file (look for existing `mod tests` fixtures that build shard dirs — e.g. wherever `WarehousePaths::new` and `WarehouseWriter` are used in tests).

  ```rust
  #[test]
  fn merge_warehouse_shard_runs_coalesces_small_part_tables() {
      use ab_warehouse::writer as wh;
      use ab_warehouse::schema::{WarehousePaths, WarehouseTable, SourceRow};
      use std::fs;
      let root = tempfile::tempdir().unwrap();
      let warehouse_dir = root.path().join("wh");
      // Two shard run dirs, each with a tiny sources.parquet (1 row, tiny bytes).
      let shard_rows = (0..2u64).map(|i| SourceRow {
          run_id: "r".to_owned(),
          source_id: format!("s{i}"),
          text_id: format!("t{i}"),
          aat_path: format!("aat/{i}.json"),
          source_bytes: 1,
          source_chars: 1,
      });
      let mut shard_dirs = Vec::new();
      for (i, row) in shard_rows.enumerate() {
          let shard_dir = root.path().join(format!("shard-{i}"));
          fs::create_dir_all(&shard_dir).unwrap();
          let paths = WarehousePaths::new(&shard_dir, "shard");
          let mut w = wh::WarehouseWriter::create(paths.clone()).unwrap();
          w.append_sources(&[row]).unwrap();
          w.finalize().unwrap();
          shard_dirs.push(paths.final_dir);
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
      // Coalesced to a single file (not 2 staged parts).
      let sources_parts: Vec<_> = fs::read_dir(&sources_dir)
          .unwrap()
          .filter_map(Result::ok)
          .filter(|e| e.path().extension().is_some_and(|x| x == "parquet"))
          .collect();
      assert_eq!(sources_parts.len(), 1, "sources should be coalesced to 1 part");
  }
  ```

  If the exact `WarehouseParallelOptions` field set or `WarehouseWriter::append_sources` signature differs, copy the shape from an existing test in the same file that constructs `WarehouseParallelOptions` (search `rg "WarehouseParallelOptions \{" crates/ab-morph-run/src/`). The intent is: 2 shards × tiny parts for Sources → assert 1 merged part.

- [ ] **Step 2:** Run the test to verify it fails.

  ```bash
  cargo test -p ab-morph-run --lib merge_warehouse_shard_runs_coalesces_small_part_tables 2>&1 | tail -5
  ```
  Expected: FAIL with an assertion like `sources_parts.len() == 1` failing (current behavior produces 2 parts).

- [ ] **Step 3:** Implement the compaction threshold constants and the `compact_small_part_tables` helper.

  Near the top of `crates/ab-morph-run/src/pipeline.rs` (next to the existing `WAREHOUSE_REGULAR_BATCH_SIZE` at `lib.rs:38` or wherever module-level consts live in `pipeline.rs`), add:

  ```rust
  /// Merge-time compaction threshold: coalesce a staged table iff it has more
  /// than this many parts AND the median part is smaller than `COMPACTION_MAX_MEDIAN_PART_BYTES`.
  /// Values from the §3.12 decision (docs/superpowers/reports/2026-07-03-morph-perf-decision.md).
  const COMPACTION_MIN_PART_COUNT: usize = 64;
  const COMPACTION_MAX_MEDIAN_PART_BYTES: u64 = 1_048_576; // 1 MiB
  ```

  Then add the helper (place it just above `merge_warehouse_shard_runs`):

  ```rust
  /// After staging, coalesce tables whose staged parts are many-and-tiny into a
  /// single file via the writer's row-group-sized output. Large tables (median
  /// part ≥ 1 MiB) keep their staged parts unchanged — coalescing a 50 GiB table
  /// row-group-by-row-group has no benefit and a real cost.
  ///
  /// Reads each staged part through `append_parquet_table_file`, then removes
  /// the original staged `part-*.parquet` files so `finalize_staging_run` moves
  /// only the coalesced file to `final_dir`.
  fn compact_small_part_tables(
      paths: &WarehousePaths,
      tables: &[WarehouseTable],
  ) -> Result<()> {
      use ab_warehouse::writer::append_parquet_table_file;
      for &table in tables {
          let staged = paths.staging_dir.join(table.file_name());
          if !staged.is_dir() {
              continue;
          }
          let (count, bytes) = parquet_table_part_count_and_bytes(&staged)?;
          if count == 0 {
              continue;
          }
          let median = bytes / count as u64;
          if count <= COMPACTION_MIN_PART_COUNT || median >= COMPACTION_MAX_MEDIAN_PART_BYTES {
              continue;
          }
          // Open a writer scoped to this table and stream the staged parts in.
          let mut writer = WarehouseWriter::create_for_tables(
              paths.clone(),
              &[table],
          )?;
          let parts = parquet_table_part_paths(&staged)?;
          for part in &parts {
              append_parquet_table_file(&mut writer, table, part)?;
          }
          writer.finalize()?;
          // Remove the original staged parts so finalize_staging_run doesn't also move them.
          // The coalesced output was written by the writer into the same dir.
          for part in parts {
              fs::remove_file(&part).with_context(|| format!("remove staged part {}", part.display()))?;
          }
      }
      Ok(())
  }
  ```

  If `parquet_table_part_paths` is needed here and is private in `ab-warehouse`, either re-export it (Task 1 could add it to the un-gating) or inline the list: `fs::read_dir(&staged)?.filter_map(|e| e.ok()).map(|e| e.path()).filter(|p| p.extension().is_some_and(|x| x == "parquet")).collect()`. Prefer re-using the existing helper; add `pub` to it in Task 1's step if needed.

  ⚠️ `WarehouseWriter::create_for_tables` calls `cleanup_stale_staging` and `fs::remove_dir_all(staging_dir)` at the top (`writer.rs:50-52`) — that would wipe the other tables' staged parts. **This is the critical bug to verify against before committing.** If `create_for_tables` wipes staging, the per-table writer must not use the shared `paths`; instead, write to a temp dir and move the coalesced file into `staging_dir/<table>.parquet` after. Verify in Step 4.

- [ ] **Step 4:** Verify (or fix) the staging-wipe concern.

  ```bash
  cargo test -p ab-morph-run --lib merge_warehouse_shard_runs_coalesces_small_part_tables 2>&1 | tail -10
  ```
  If it fails because `create_for_tables` wipes the shared staging dir, change the approach: write the coalesced file to a `tempfile::tempdir()`-scoped `WarehouseWriter`, then move the resulting `<table>.parquet` file into `paths.staging_dir.join(table.file_name())` (replacing the `part-*.parquet` files, which were already consumed). Concretely, replace the body of `compact_small_part_tables` with:

  ```rust
  fn compact_small_part_tables(paths: &WarehousePaths, tables: &[WarehouseTable]) -> Result<()> {
      use ab_warehouse::writer::append_parquet_table_file;
      for &table in tables {
          let staged = paths.staging_dir.join(table.file_name());
          if !staged.is_dir() { continue; }
          let (count, bytes) = parquet_table_part_count_and_bytes(&staged)?;
          if count == 0 { continue; }
          let median = bytes / count as u64;
          if count <= COMPACTION_MIN_PART_COUNT || median >= COMPACTION_MAX_MEDIAN_PART_BYTES {
              continue;
          }
          let parts = parquet_table_part_paths(&staged)?;
          // Coalesce into a temp dir to avoid the shared staging dir.
          let tmp = tempfile::tempdir()
              .with_context(|| "create temp dir for compaction")?;
          let tmp_paths = WarehousePaths::new(tmp.path(), "compact");
          let mut writer = WarehouseWriter::create_for_tables(tmp_paths.clone(), &[table])?;
          for part in &parts {
              append_parquet_table_file(&mut writer, table, part)?;
          }
          writer.finalize()?;
          // The tmp writer wrote to tmp_paths.final_dir (finalize_staging_run moved staging→final).
          // Remove the original staged parts, then move the single coalesced file in.
          for part in &parts {
              fs::remove_file(part).with_context(|| format!("remove {}", part.display()))?;
          }
          let coalesced = tmp_paths.final_dir.join(table.file_name());
          // final_dir for a dir-shaped table is <final_dir>/<table>.parquet (a dir of parts).
          // Move the coalesced parquet file (single file) into the staged dir as part-00000.parquet.
          if coalesced.is_dir() {
              for entry in fs::read_dir(&coalesced)? {
                  let entry = entry?;
                  let dest = staged.join(entry.file_name());
                  fs::rename(entry.path(), dest)
                      .or_else(|_| { fs::copy(entry.path(), &dest)?; Ok(()) })?;
              }
          } else {
              let dest = staged.join("part-00000.parquet");
              fs::rename(&coalesced, &dest)
                  .or_else(|_| { fs::copy(&coalesced, &dest)?; Ok(()) })?;
          }
      }
      Ok(())
  }
  ```

  Re-run the test until green. The temp-dir approach is the safe default — prefer it.

- [ ] **Step 5:** Wire the helper into `merge_warehouse_shard_runs`.

  In `crates/ab-morph-run/src/pipeline.rs`, inside `merge_warehouse_shard_runs`, after the `for table in options.warehouse_profile.merged_data_tables() { for (shard_index, run_dir) in … stage_parquet_table_part(…) }` loop and before `writer.append_runs(…)`, add:

  ```rust
  compact_small_part_tables(&paths, options.warehouse_profile.merged_data_tables())?;
  ```

  (The call must come after staging completes for all tables, since `compact_small_part_tables` reads the staged parts.)

- [ ] **Step 6:** Run the full ab-morph-run test suite.

  ```bash
  cargo test -p ab-morph-run --lib 2>&1 | rg "test result|FAILED|error\[" | head
  ```
  Expected: `test result: ok. 88 passed; 0 failed` (87 existing + the 1 new). No existing test regresses (the compaction only triggers on >64 parts <1 MiB median, which existing integration-fixtures don't hit — they use few shards).

- [ ] **Step 7:** Commit Task 2.

  ```bash
  git add crates/ab-morph-run/src/pipeline.rs
  git commit -m "perf(morph-run): compact small-part warehouse tables after merge

  merge_warehouse_shard_runs previously renamed shard parts into staging
  with no coalescing, leaving 562 tiny parquet parts per sharded table.
  On the full corpus, 3 tables (analyses, sources, feature_pattern_counts)
  crossed the §3.12 threshold (>64 parts AND median <1 MiB). Add
  compact_small_part_tables: for each merged-data table, if part_count>64
  and median_part_bytes<1MiB, rewrite all staged parts through the writer's
  row-group-sized output into a single file. Large tables (median >1 MiB)
  are left untouched. Thresholds from the §3.12 decision doc."
  ```

---

## Task 3: Full workspace regression and cargo fmt

**Files:**
- None (verification only)

- [ ] **Step 1:** Run the full workspace test suite.

  ```bash
  cargo test --workspace 2>&1 | grep -cE "FAILED|error\[|panicked:"
  ```
  Expected: `0` (no failures across any crate; ab-warehouse + ab-morph-run are the only touched crates).

- [ ] **Step 2:** Run clippy on the touched crates.

  ```bash
  cargo clippy -p ab-warehouse -p ab-morph-run --all-targets 2>&1 | grep -E "^error|^warning" | head
  ```
  Expected: no output.

- [ ] **Step 3:** Format check.

  ```bash
  cargo fmt --all -- --check
  ```
  Expected: no diff. If there's a diff, run `cargo fmt --all` and inspect the diff before committing.

- [ ] **Step 4:** If Steps 1–3 needed any fixup, commit it.

  ```bash
  git status --short
  # only commit if there are staged/pending changes from a fmt fixup
  git add -A && git commit -m "chore: fmt/clippy fixups from §3.12 compaction"
  ```

---

## Task 4: Before/after measurement via the bench harness

**Files:**
- Read: `benchmarks/run-morph-corpus.sh` (the harness)
- Read: `benchmarks/baselines/morph-run-2026-07-03/parts.json` (before-numbers — already on main)
- Generated (NOT committed): warehouse output under `/db/ab-validator/morph-warehouse-bench`

- [ ] **Step 1:** Capture the "after" measurement by re-running the harness.

  This run takes ~31 minutes (the j=32 primary run on the full 17,689-file corpus). Skip the jobs sweep entirely to keep it to one run:

  ```bash
  SUDACHI="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic"
  AB_MORPH_AAT_DIR=/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter \
  AB_MORPH_WAREHOUSE_DIR=/db/ab-validator/morph-warehouse-bench-after \
  AB_MORPH_JOBS="32 32" \
  AB_BENCH_OUT="$(pwd)/benchmarks/baselines/morph-run-2026-07-03-after" \
  AB_SUDACHI_DICT="$SUDACHI" \
  timeout 3000 bash benchmarks/run-morph-corpus.sh 2>&1 | tail -5
  ```
  Note: the harness builds `ab-morph-run` from the current (compacted) source, so the run uses the new code.

- [ ] **Step 2:** Compare before/after part counts.

  ```bash
  before=benchmarks/baselines/morph-run-2026-07-03/parts.json
  after=benchmarks/baselines/morph-run-2026-07-03-after/parts.json
  echo "--- total parts: before vs after ---"
  jq '.total_parquet_parts' "$before" "$after"
  echo "--- per qualifying table: parts before vs after ---"
  for t in analyses sources feature_pattern_counts; do
    b=$(jq -r --arg t "$t" '.tables[] | select(.table==$t) | .parts // 0' "$before")
    a=$(jq -r --arg t "$t" '.tables[] | select(.table==$t) | .parts // 0' "$after")
    echo "$t: before=$b after=$a"
  done
  echo "--- wall time before vs after ---"
  jq '.primary.wall_seconds' benchmarks/baselines/morph-run-2026-07-03/summary.json \
                             benchmarks/baselines/morph-run-2026-07-03-after/summary.json
  ```

- [ ] **Step 3:** Assert the success criteria.

  The run passes if:
  - `after.total_parquet_parts < before.total_parquet_parts` (dropped by roughly 3×561 = ~1,683).
  - The 3 small tables each have far fewer parts after (ideally 1, at most a small handful).
  - `after.wall_seconds ≤ before.wall_seconds × 1.10` (no >10% wall-time regression; coalescing <2 GiB should be ~seconds vs the 30-min run).

  If wall time regressed >10%, stop and report — do not commit the "after" data. Investigate whether the compaction is running on a table it shouldn't (e.g. a large table crossing the threshold incorrectly).

- [ ] **Step 4:** Commit the after-measurement artifacts.

  ```bash
  git add benchmarks/baselines/morph-run-2026-07-03-after/summary.json \
          benchmarks/baselines/morph-run-2026-07-03-after/parts.json
  git commit -m "docs: §3.12 after-measurement (compaction effects on part count + wall)"
  ```

- [ ] **Step 5:** Clean up the generated warehouse output.

  ```bash
  rm -rf /db/ab-validator/morph-warehouse-bench-after
  ```

---

## Self-Review

- [x] **Spec coverage:** Decision doc §3.12 (compaction for >64 parts AND median <1 MiB) → Task 2 implements exactly that threshold. Task 4 measures the before/after. The 8 large tables are explicitly untouched (predicate skips them). `append_parquet_table_file` existing test pins the coalescer behavior; new test pins the merge integration.
- [x] **Placeholder scan:** No TBD/TODO. The one place that says "verify in Step 4" (the staging-wipe concern) is an explicit verification checkpoint, not a placeholder — the fallback code is given inline.
- [x] **Type consistency:** `WarehousePaths`, `WarehouseTable`, `WarehouseWriter` are the canonical types from `ab-warehouse::schema` / `ab_warehouse::writer`. The helper signature is `fn compact_small_part_tables(paths: &WarehousePaths, tables: &[WarehouseTable]) -> Result<()>` and the call site passes `&paths` (a `WarehousePaths`) and `options.warehouse_profile.merged_data_tables()` (returns `&'static [WarehouseTable]`).
- [x] **Threshold values verbatim:** `COMPACTION_MIN_PART_COUNT = 64`, `COMPACTION_MAX_MEDIAN_PART_BYTES = 1_048_576` — match the decision doc exactly.
- [x] **TDD:** Task 1 = failing test → impl → green. Task 2 = failing merge test → impl → green. Task 4 = measurement gate (must drop parts, must not regress wall >10%).
- [x] **No semantic fold:** This is a perf commit; the threshold comes from the decision doc (a separate prior artifact), not invented here.

---

## Execution Handoff

Plan complete and saved to `docs/superpowers/plans/2026-07-03-parquet-compaction-§3.12.md`. Two execution options:

1. **Subagent-Driven (recommended)** — dispatch a fresh subagent per task, review between tasks, fast iteration.
2. **Inline Execution** — execute tasks in this session using executing-plans, batch execution with checkpoints.

Which approach?
