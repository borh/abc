# Ruby-Oracle Follow-ups + Run Perf (Lever 1) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land three independent, additive improvements to the ruby oracle and the
warehouse run — iteration-mark normalization (A), a first-class `classification` column
(B), and front-loaded large-document scheduling (P1) — then validate on hinoki.

**Architecture:** A is pure-function work in `reading_norm`. B threads a new non-null
string column from `adjudicate` through the warehouse schema/writer/DDL. P1 changes only
the dispatch order in `WarehouseWorkQueue`, not row content. All three are independent:
different files, no shared interfaces.

**Tech Stack:** Rust workspace at `ab-validator/`; crates `ab-morph-run` (oracle +
pipeline) and `ab-warehouse` (schema/writer/sql). Arrow/Parquet writer; DuckDB views.

## Global Constraints

- `SCHEMA_VERSION` stays **2** (see spec §B reader-stability caveat). Do not bump it.
- `classification` is a **non-null** VARCHAR with exactly three values:
  `"resolved"`, `"nonstandard_ruby"`, `"no_comparable_reading"`. Position it
  **immediately after `oracle_source`** in every list (struct, column_names, DDL,
  writer arrays, writer schema).
- The oracle view (`morph_views.sql`) is `SELECT *` — it needs **no edit**; it picks up
  the new column automatically.
- `reading_norm::normalize` must stay **idempotent**. Iteration-mark expansion tracks
  `last_kana` (never the prolongation mark `ー`), not `chars.last()`.
- P1 must preserve the `≤ large_lanes` concurrent-large invariant exactly and must not
  change any row's content — only processing/shard order. Factor the large-batch
  construction into one helper; do not duplicate the block verbatim.
- Every existing test in the touched crates must still pass (`cargo test -p ab-morph-run`,
  `cargo test -p ab-warehouse`).

---

## File Structure

- `crates/ab-morph-run/src/oracle/reading_norm.rs` — **Task 1 (A)**: iteration-mark pass
  + `unvoiced_base`/`voiced_form` helpers; unit tests.
- `crates/ab-warehouse/src/schema.rs` — **Task 2 (B)**: `classification` struct field +
  `column_names` entry.
- `crates/ab-warehouse/src/writer.rs` — **Task 2 (B)**: append array + schema field;
  round-trip test.
- `crates/ab-warehouse/sql/schema.sql` — **Task 2 (B)**: DDL column.
- `crates/ab-morph-run/src/oracle/ruby.rs` — **Task 2 (B)**: `adjudicate` computes
  `classification`; logic tests.
- `crates/ab-morph-run/src/pipeline.rs` — **Task 3 (P1)**: `WarehouseWorkQueue` dispatch
  order + `pop_large_batch` helper; queue tests.
- `scripts/oracle-validation-diff.sql` — **Validation asset** (not an SDD task): keyed
  4-bucket oracle diff + non-oracle parity, run on hinoki.

---

### Task 1: A — iteration marks in `reading_norm`

**Files:**
- Modify: `crates/ab-morph-run/src/oracle/reading_norm.rs` (the step-1 collect in
  `normalize`, ~line 11; add two helper fns; add tests).

**Interfaces:**
- Consumes: nothing new.
- Produces: no signature change — `normalize(&str) -> String` behavior is extended so
  `ゝ ゞ ヽ ヾ` expand instead of being dropped.

- [ ] **Step 1: Write the failing tests**

Add to the `tests` module in `reading_norm.rs`:

```rust
#[test]
fn iteration_marks_expand_previous_kana() {
    // 武士《ものゝふ》: ゝ repeats の → もののふ (was dropped → ものふ).
    assert_eq!(normalize("ものゝふ"), normalize("もののふ"));
    // 心《こゝろ》
    assert_eq!(normalize("こゝろ"), normalize("こころ"));
    // katakana mark
    assert_eq!(normalize("スヽメ"), normalize("ススメ"));
}

#[test]
fn voiced_iteration_mark_adds_dakuten() {
    // いすゞ: ゞ after す → ず → いすず.
    assert_eq!(normalize("いすゞ"), normalize("いすず"));
    // ゞ after a voiced kana repeats the voiced base: じゞ → じじ.
    assert_eq!(normalize("じゞ"), normalize("じじ"));
}

#[test]
fn iteration_mark_edges() {
    // chained marks follow the mark's own output
    assert_eq!(normalize("たゝゝ"), normalize("たたた"));
    // mark after ー repeats the pre-ー kana, not ー
    assert_eq!(normalize("たーゝ"), normalize("たーた"));
    // mark with no preceding kana is dropped
    assert_eq!(normalize("ゝあ"), normalize("あ"));
    // ゝ unvoices a preceding voiced kana (classical rule): がゝ → がか
    assert_eq!(normalize("がゝ"), normalize("がか"));
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p ab-morph-run --lib oracle::reading_norm 2>&1 | tail -20`
Expected: the new tests FAIL (e.g. `ものゝふ` normalizes to `ものふ` ≠ `もののふ`).

- [ ] **Step 3: Implement the iteration-mark pass and helpers**

Replace the step-1 collect in `normalize` (currently
`let mut chars: Vec<char> = reading.nfkc().filter_map(kana_to_hiragana).collect();`) with:

```rust
    // 1. NFKC, then katakana→hiragana keeping only kana; expand iteration marks
    //    (ゝ ゞ ヽ ヾ) against the last repeatable kana (never the prolongation mark
    //    ー) so historical ruby like ものゝふ → もののふ instead of dropping the
    //    mark (ものふ). ゝ/ヽ repeat the unvoiced base (classical); ゞ/ヾ the voiced
    //    form. The pushed kana updates last_kana so chained marks (ゝゝ) work.
    let mut chars: Vec<char> = Vec::new();
    let mut last_kana: Option<char> = None;
    for ch in reading.nfkc() {
        match ch {
            'ゝ' | 'ヽ' => {
                if let Some(prev) = last_kana {
                    let r = unvoiced_base(prev);
                    chars.push(r);
                    last_kana = Some(r);
                }
            }
            'ゞ' | 'ヾ' => {
                if let Some(prev) = last_kana {
                    let r = voiced_form(unvoiced_base(prev));
                    chars.push(r);
                    last_kana = Some(r);
                }
            }
            _ => {
                if let Some(h) = kana_to_hiragana(ch) {
                    chars.push(h);
                    if h != 'ー' {
                        last_kana = Some(h);
                    }
                }
            }
        }
    }
```

Add these two helpers (place them next to `kana_to_hiragana`):

```rust
/// Strip dakuten/handakuten to the base hiragana (が→か, ぱ→は); identity otherwise.
fn unvoiced_base(ch: char) -> char {
    match ch {
        'が' => 'か', 'ぎ' => 'き', 'ぐ' => 'く', 'げ' => 'け', 'ご' => 'こ',
        'ざ' => 'さ', 'じ' => 'し', 'ず' => 'す', 'ぜ' => 'せ', 'ぞ' => 'そ',
        'だ' => 'た', 'ぢ' => 'ち', 'づ' => 'つ', 'で' => 'て', 'ど' => 'と',
        'ば' | 'ぱ' => 'は', 'び' | 'ぴ' => 'ひ', 'ぶ' | 'ぷ' => 'ふ',
        'べ' | 'ぺ' => 'へ', 'ぼ' | 'ぽ' => 'ほ',
        other => other,
    }
}

/// Add dakuten to a base hiragana (か→が); identity when there is no voiced form.
fn voiced_form(ch: char) -> char {
    match ch {
        'か' => 'が', 'き' => 'ぎ', 'く' => 'ぐ', 'け' => 'げ', 'こ' => 'ご',
        'さ' => 'ざ', 'し' => 'じ', 'す' => 'ず', 'せ' => 'ぜ', 'そ' => 'ぞ',
        'た' => 'だ', 'ち' => 'ぢ', 'つ' => 'づ', 'て' => 'で', 'と' => 'ど',
        'は' => 'ば', 'ひ' => 'び', 'ふ' => 'ぶ', 'へ' => 'べ', 'ほ' => 'ぼ',
        other => other,
    }
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cargo test -p ab-morph-run --lib oracle::reading_norm 2>&1 | tail -20`
Expected: all `reading_norm` tests PASS (including the existing idempotence/script tests).

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-run/src/oracle/reading_norm.rs
git commit -m "feat(oracle): expand iteration marks ゝゞヽヾ in reading_norm"
```

---

### Task 2: B — first-class `classification` column

**Files:**
- Modify: `crates/ab-warehouse/src/schema.rs` (struct + `column_names` arm).
- Modify: `crates/ab-warehouse/src/writer.rs` (append array + schema fn; add round-trip test).
- Modify: `crates/ab-warehouse/sql/schema.sql` (DDL).
- Modify: `crates/ab-morph-run/src/oracle/ruby.rs` (`adjudicate` + tests).

**Interfaces:**
- Produces: `NwayRegionOracleEvidenceRow` gains `pub classification: String` (after
  `oracle_source`, before `winning_analyzer`). `adjudicate` sets it to one of
  `"resolved"` / `"nonstandard_ruby"` / `"no_comparable_reading"`.
- Consumes: `Reading.align` (already `"exact"` / `"boundary-misalign"` / `"no-reading"`).

- [ ] **Step 1: Write the failing tests**

In `ruby.rs` tests, extend `zero_match_is_nonstandard_ruby` and add three tests. Note the
existing test helpers `analysis`, `base`, `regions`, `morph` are already in scope.

```rust
#[test]
fn nonstandard_ruby_when_exact_but_no_match() {
    // both tile exactly & disagree with the editor → genuine reading gap.
    let a = analysis("vibrato", vec![morph("本気", 0..2, &[("kana", "ホンキ")])]);
    let b = analysis(
        "sudachi-c",
        vec![morph("本気", 0..2, &[("reading_form", "ホンキ")])],
    );
    let rows = adjudicate("r", "s", "t", &[base(0, 2, "マジ")], &[a, b], &regions());
    assert_eq!(rows.len(), 1);
    assert_eq!(rows[0].classification, "nonstandard_ruby");
}

#[test]
fn all_boundary_misalign_is_no_comparable_reading() {
    // every analyzer straddles the base end → boundary-misalign, zero winners.
    let a = analysis(
        "vibrato",
        vec![morph("本気說", 0..3, &[("kana", "ホンキセツ")])],
    );
    let b = analysis(
        "sudachi-c",
        vec![morph("本気說", 0..3, &[("reading_form", "ホンキセツ")])],
    );
    let rows = adjudicate("r", "s", "t", &[base(0, 2, "ほんき")], &[a, b], &regions());
    assert_eq!(rows.len(), 1);
    assert_eq!(rows[0].classification, "no_comparable_reading");
}

#[test]
fn all_no_reading_is_no_comparable_reading() {
    // exact tiling but every covered morpheme lacks a reading feature → no-reading.
    let a = analysis("vibrato", vec![morph("本気", 0..2, &[])]);
    let b = analysis("sudachi-c", vec![morph("本気", 0..2, &[])]);
    let rows = adjudicate("r", "s", "t", &[base(0, 2, "ほんき")], &[a, b], &regions());
    assert_eq!(rows.len(), 1);
    assert_eq!(rows[0].classification, "no_comparable_reading");
}

#[test]
fn resolved_row_classification() {
    let a = analysis("vibrato", vec![morph("東京", 0..2, &[("kana", "トウケイ")])]);
    let b = analysis(
        "sudachi-c",
        vec![morph("東京", 0..2, &[("reading_form", "トウキョウ")])],
    );
    let rows = adjudicate(
        "r", "s", "t", &[base(0, 2, "とうきょう")], &[a, b], &regions(),
    );
    assert_eq!(rows.len(), 1);
    assert_eq!(rows[0].classification, "resolved");
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p ab-morph-run --lib oracle::ruby 2>&1 | tail -20`
Expected: FAIL to compile — `NwayRegionOracleEvidenceRow` has no `classification` field yet.

- [ ] **Step 3: Add the column to the warehouse schema, writer, and DDL**

`schema.rs` — in `pub struct NwayRegionOracleEvidenceRow`, add after `pub oracle_source: String,`:

```rust
    pub classification: String,
```

`schema.rs` — in the `NwayRegionOracleEvidence` `column_names` arm, add `"classification",`
immediately after `"oracle_source",`.

`writer.rs` — in `append_nway_region_oracle_evidence`, add after the `oracle_source`
array line:

```rust
                string_array(rows.iter().map(|row| row.classification.as_str())),
```

`writer.rs` — in `nway_region_oracle_evidence_schema`, add after `utf8("oracle_source", false),`:

```rust
        utf8("classification", false),
```

`sql/schema.sql` — in the `CREATE TABLE nway_region_oracle_evidence` block, add after
`oracle_source VARCHAR,`:

```sql
  classification VARCHAR,
```

- [ ] **Step 4: Compute `classification` in `adjudicate`**

In `ruby.rs` `adjudicate`, track comparable readings in the per-analyzer loop. Add before
the loop:

```rust
        let mut any_comparable = false;
```

Inside the loop, after `let reading = analyzer_reading(analysis, base);`, add:

```rust
            if reading.align == "exact" {
                any_comparable = true;
            }
```

Replace the existing `classification` computation:

```rust
        let classification = if winners.is_empty() {
            "nonstandard_ruby"
        } else {
            "resolved"
        };
```

with:

```rust
        // resolved: ≥1 winner. nonstandard_ruby: no winner but ≥1 analyzer produced a
        // comparable (exact) reading — a genuine reading the dictionaries lack.
        // no_comparable_reading: no analyzer produced a comparable reading (all
        // boundary-misalign or no-reading) — not a dictionary signal.
        let classification = if !winners.is_empty() {
            "resolved"
        } else if any_comparable {
            "nonstandard_ruby"
        } else {
            "no_comparable_reading"
        };
```

In the `rows.push(NwayRegionOracleEvidenceRow { ... })`, add after `oracle_source: "ruby".to_owned(),`:

```rust
            classification: classification.to_owned(),
```

(The `evidence_detail` JSON already embeds `"classification": classification` — it now
carries the three-way value automatically.)

- [ ] **Step 5: Run the oracle tests**

Run: `cargo test -p ab-morph-run --lib oracle 2>&1 | tail -25`
Expected: all `oracle::ruby` and `oracle::reading_norm` tests PASS.

- [ ] **Step 6: Add the Parquet round-trip test in `ab-warehouse`**

In `writer.rs` tests (where `ParquetRecordBatchReaderBuilder` is already used), add a test
that writes an oracle row with a `classification` and reads the column back. Model it on
the existing oracle writer test; the key assertion:

```rust
#[test]
fn oracle_classification_column_round_trips() {
    let root = std::env::temp_dir().join(format!("oracle-cls-{}", std::process::id()));
    let paths = WarehousePaths::new(&root, "run-cls");
    let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
    writer
        .append_nway_region_oracle_evidence(&[NwayRegionOracleEvidenceRow {
            run_id: "run-cls".into(),
            source_id: "s".into(),
            text_id: "t".into(),
            region_index: 0,
            projected_char_start: 0,
            projected_char_end: 2,
            oracle_source: "ruby".into(),
            classification: "no_comparable_reading".into(),
            winning_analyzer: None,
            losing_analyzers: vec!["vibrato".into(), "sudachi-c".into()],
            evidence_detail: "{}".into(),
        }])
        .unwrap();
    writer.finalize().unwrap();

    let file = std::fs::File::open(
        paths
            .final_dir
            .join(WarehouseTable::NwayRegionOracleEvidence.file_name()),
    )
    .unwrap();
    let mut reader = ParquetRecordBatchReaderBuilder::try_new(file)
        .unwrap()
        .build()
        .unwrap();
    let batch = reader.next().unwrap().unwrap();
    let col = batch
        .column_by_name("classification")
        .unwrap()
        .as_any()
        .downcast_ref::<arrow::array::StringArray>()
        .unwrap();
    assert_eq!(col.value(0), "no_comparable_reading");
    let _ = std::fs::remove_dir_all(root);
}
```

(Adjust imports to match the file's existing test imports — `WarehousePaths`,
`WarehouseTable`, `NwayRegionOracleEvidenceRow`, `arrow::array` casts.)

- [ ] **Step 7: Run the warehouse tests**

Run: `cargo test -p ab-warehouse 2>&1 | tail -25`
Expected: all pass, including `oracle_classification_column_round_trips`.

- [ ] **Step 8: Commit**

```bash
git add crates/ab-warehouse/src/schema.rs crates/ab-warehouse/src/writer.rs \
        crates/ab-warehouse/sql/schema.sql crates/ab-morph-run/src/oracle/ruby.rs
git commit -m "feat(oracle): first-class classification column (resolved/nonstandard_ruby/no_comparable_reading)"
```

---

### Task 3: P1 — front-load large documents in `WarehouseWorkQueue`

**Files:**
- Modify: `crates/ab-morph-run/src/pipeline.rs` (`WarehouseWorkQueue::new` sort,
  `take_batch`; add `pop_large_batch` helper; add queue tests).

**Interfaces:**
- No public signature change. `take_batch` now dispatches large docs biggest-first up to
  `large_lanes`, interleaved with regular work, instead of draining all regular first.

- [ ] **Step 1: Write the failing tests**

Add a tests module (or extend the existing pipeline tests) exercising the queue directly.
`WarehouseWorkBatch`, `WarehouseWorkQueue` are `pub(crate)`. Sizes are read from real
files, so create temp files of the required sizes.

```rust
#[cfg(test)]
mod work_queue_tests {
    use super::WarehouseWorkQueue;
    use crate::LARGE_INPUT_THRESHOLD_BYTES;
    use std::path::PathBuf;

    fn tmp_file(dir: &std::path::Path, name: &str, size: u64) -> PathBuf {
        let p = dir.join(name);
        std::fs::write(&p, vec![b'x'; size as usize]).unwrap();
        p
    }

    #[test]
    fn front_loads_largest_first_within_lane_cap() {
        let dir = std::env::temp_dir().join(format!("wq-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let big = LARGE_INPUT_THRESHOLD_BYTES;
        let l30 = tmp_file(&dir, "l30", big + 30);
        let l20 = tmp_file(&dir, "l20", big + 20);
        let l10 = tmp_file(&dir, "l10", big + 10);
        let mut regulars = Vec::new();
        for i in 0..5 {
            regulars.push(tmp_file(&dir, &format!("r{i}"), 100));
        }
        let mut inputs = vec![l10.clone(), l30.clone(), l20.clone()];
        inputs.extend(regulars.clone());

        let mut q = WarehouseWorkQueue::new(inputs, 2); // large_lanes = 2

        // First two batches are the two BIGGEST large docs, before regular is drained.
        let b1 = q.take_batch().unwrap();
        assert!(b1.is_large);
        assert_eq!(b1.inputs, vec![l30.clone()]);
        let b2 = q.take_batch().unwrap();
        assert!(b2.is_large);
        assert_eq!(b2.inputs, vec![l20.clone()]);
        // Lane cap hit (2 active): next batch is REGULAR, not the 10-byte-over large doc.
        let b3 = q.take_batch().unwrap();
        assert!(!b3.is_large);
        // Freeing a large lane lets the last large doc dispatch.
        q.complete_batch(true);
        let mut saw_l10 = false;
        while let Some(b) = q.take_batch() {
            if b.is_large {
                assert_eq!(b.inputs, vec![l10.clone()]);
                saw_l10 = true;
            }
            if b.is_large {
                q.complete_batch(true);
            }
        }
        assert!(saw_l10, "the last large doc must eventually dispatch");
        let _ = std::fs::remove_dir_all(dir);
    }

    #[test]
    fn covers_every_input_exactly_once() {
        let dir = std::env::temp_dir().join(format!("wq2-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let big = LARGE_INPUT_THRESHOLD_BYTES;
        let mut inputs = vec![
            tmp_file(&dir, "l1", big + 1),
            tmp_file(&dir, "l2", big + 2),
        ];
        for i in 0..70 {
            inputs.push(tmp_file(&dir, &format!("r{i}"), 50));
        }
        let expected = inputs.len();
        let mut q = WarehouseWorkQueue::new(inputs, 1);
        let mut seen = 0;
        while let Some(b) = q.take_batch() {
            seen += b.inputs.len();
            if b.is_large {
                q.complete_batch(true);
            }
        }
        assert_eq!(seen, expected, "every input returned exactly once");
        let _ = std::fs::remove_dir_all(dir);
    }
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p ab-morph-run --lib work_queue 2>&1 | tail -20`
Expected: `front_loads_largest_first_within_lane_cap` FAILS — the current queue drains all
regular first, so `b1` is a regular batch, not `l30`.

- [ ] **Step 3: Sort `large` biggest-first**

In `WarehouseWorkQueue::new`, change the `large` sort from ascending to **descending** by
size (regular stays ascending). Replace the `large.sort_by(...)` block with:

```rust
        // Large docs biggest-first: front-loading the makespan-dominating inputs (P1).
        large.sort_by(|(left_path, left_size), (right_path, right_size)| {
            right_size
                .cmp(left_size)
                .then_with(|| left_path.cmp(right_path))
        });
```

- [ ] **Step 4: Add `pop_large_batch` and rewrite `take_batch`**

Add a private helper on `WarehouseWorkQueue` (single source of the large-batch
construction, so the two dispatch sites are not duplicated):

```rust
    /// Pop one large doc as its own batch if a large lane is free. Caller has already
    /// checked nothing; this enforces the `large_lanes` cap and increments the counter.
    fn pop_large_batch(&mut self) -> Option<WarehouseWorkBatch> {
        if self.active_large_batches >= self.large_lanes {
            return None;
        }
        let input = self.large.pop_front()?;
        let shard_index = self.next_shard_index;
        self.next_shard_index += 1;
        self.active_large_batches += 1;
        Some(WarehouseWorkBatch {
            shard_index,
            inputs: vec![input],
            is_large: true,
        })
    }
```

Rewrite `take_batch` to front-load large work, then regular, then drain remaining large in
the tail:

```rust
    pub(crate) fn take_batch(&mut self) -> Option<WarehouseWorkBatch> {
        // P1: front-load large docs (biggest-first) up to the memory-bounded lane count
        // so they overlap the abundant regular work instead of forming an idle tail.
        if let Some(batch) = self.pop_large_batch() {
            return Some(batch);
        }
        if !self.regular.is_empty() {
            let shard_index = self.next_shard_index;
            self.next_shard_index += 1;
            let mut inputs = Vec::new();
            for _ in 0..WAREHOUSE_REGULAR_BATCH_SIZE {
                let Some(input) = self.regular.pop_front() else {
                    break;
                };
                inputs.push(input);
            }
            return Some(WarehouseWorkBatch {
                shard_index,
                inputs,
                is_large: false,
            });
        }
        // Regular exhausted: keep draining large within the lane cap (the short tail).
        self.pop_large_batch()
    }
```

- [ ] **Step 5: Run the queue tests + full crate tests**

Run: `cargo test -p ab-morph-run --lib work_queue 2>&1 | tail -20`
Expected: both new tests PASS.
Run: `cargo test -p ab-morph-run 2>&1 | tail -15`
Expected: full crate passes (no regression in existing pipeline/merge tests).

- [ ] **Step 6: Commit**

```bash
git add crates/ab-morph-run/src/pipeline.rs
git commit -m "perf(warehouse): front-load large documents to remove the scheduling tail"
```

---

## Validation asset (not an SDD task — controller writes + runs on hinoki)

Create `scripts/oracle-validation-diff.sql` (DuckDB) implementing the spec's invariants,
parameterized by two run dirs (`PRIOR_DIR`, `NEW_DIR`):

- Non-oracle parity: for each non-oracle table, `SELECT COUNT(*)` from prior vs new; assert
  equal (report any mismatch).
- Oracle keyed diff on `(source_id, text_id, region_index, projected_char_start,
  projected_char_end)`:
  - `dropped` = keys in prior not in new (expected > 0),
  - `newly_emitted` = keys in new not in prior (expected ≈ 0),
  - `classification_changed` = shared keys with differing `classification`,
  - `unchanged` = shared keys, same fields.
- Classification breakdown: `SELECT classification, COUNT(*) FROM new GROUP BY 1`.

This is committed with the code (it documents the invariant), but it is exercised during
the hinoki validation, not by an SDD subagent.

## Hinoki validation procedure (controller, after merge)

Per the spec §Validation:
1. `git pull` on hinoki; rebuild release binary.
2. Subset correctness/parity smoke.
3. Jobs sweep `--jobs 8/12/16/0` on the full corpus with `/usr/bin/time -v` (wall-clock +
   peak RSS). Record the table; compare `--jobs 0` peak RSS to the `auto_jobs` prediction.
4. Canonical clean full regen at the sweep optimum; run `oracle-validation-diff.sql`
   against `full-2026-07-07_055139-jobs8`; confirm the four-bucket diff and non-oracle
   parity; spot-check `武士《ものゝふ》`-class rubies land in `dropped`/`→ resolved`.
5. If `--jobs 0` peak RSS exceeds the model materially, recalibrate `auto_jobs` (overlap
   surcharge) — data-driven only.
6. Record the sweep table, wall-clock delta, four-bucket diff, and classification
   breakdown in this plan's Validation Record.

---

## Validation Record

_(Filled in after the hinoki run.)_
