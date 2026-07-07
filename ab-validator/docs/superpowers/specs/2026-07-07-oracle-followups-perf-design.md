# Ruby-Oracle Follow-ups + Run Perf (Lever 1) — Design

**Date:** 2026-07-07
**Status:** design (awaiting review)
**Governs:** `crates/ab-morph-run/src/oracle/reading_norm.rs`, `.../oracle/ruby.rs`,
`crates/ab-warehouse` (schema/writer/sql), `crates/ab-morph-run/src/pipeline.rs`
(`WarehouseWorkQueue`).
**Predecessors:** `2026-07-07-ruby-oracle-design.md` (Phase 4 oracle),
`2026-07-05-warehouse-run-perf-design.md` (§3.15 deferred work).

## Goal

Three independent, additive improvements landed in one cycle, then validated by a
single full-corpus run on hinoki with an empirical jobs sweep:

- **A. Iteration marks** — expand `ゝ ゞ ヽ ヾ` in the kana canonicalizer so historical
  ruby like `武士《ものゝふ》` normalizes to `もののふ` and matches analyzer output,
  instead of dropping the mark (`ものふ`) and reporting a false non-match.
- **B. Classification split** — promote the oracle's outcome to a first-class
  `classification` column with three values, so the dictionary-development signal
  (an editor reading no analyzer produces, *when the analyzers could actually align*)
  is queryable without parsing `evidence_detail` JSON.
- **P1. Front-load large documents** — the parallel scheduler currently drains all
  small documents first and processes the 32 largest **last** at `jobs/4` lanes while
  most workers idle (the documented "~22 of 32 cores busy" tail). Start the largest
  documents first so they overlap with the abundant small work.

## Non-goals (each its own later effort)

- **Lever 2** — intra-worker analyzer parallelism (run the 4 analyzers concurrently
  per document). The larger but riskier throughput lever; gets its own spec + measurement.
- **Phase 5** — wiring `oracle_resolved_region_count` into the RRF interestingness rank.
- **Exact-tiling relaxation** — increasing coverage by adjudicating bases the analyzers
  do not exactly tile. Explicitly deferred (owner: "split classification only" this round).

---

## A. Iteration marks in `reading_norm::normalize`

### Problem

`kana_to_hiragana` keeps only `ぁ..ん` / `ァ..ン` (plus `ー`). The iteration marks
`ゝ`(U+309D) `ゞ`(U+309E) `ヽ`(U+30FD) `ヾ`(U+30FE) fall outside those ranges and are
dropped. An editor ruby `ものゝふ` therefore normalizes to `ものふ`, never matching the
analyzers' `もののふ`, and is mislabeled a non-match (inflating `nonstandard_ruby`).

### Rule

An iteration mark repeats the **previous emitted kana**:

- `ゝ` / `ヽ` (unvoiced repeat) → append the previous kana's **unvoiced base**
  (strip any dakuten/handakuten: `が→か`, so `ゝ` after `が` yields `か`).
- `ゞ` / `ヾ` (voiced repeat) → append the **voiced form of the previous kana's base**
  (`か`/`が` → `が`). If the base has no voiced form (e.g. `の`, `ま`), fall back to the
  unvoiced base (repeat as-is).
- A mark with no preceding kana (string start, or only non-kana before it) is dropped.

`ものゝふ`: previous kana `の` (no voicing) → `ゝ` appends `の` → `もののふ`. ✔

### Placement

Expansion happens **during the initial NFKC→hiragana collection** (step 1), so the
repeated kana is a normal hiragana that participates in every downstream layer
(lexical table, historical folds, long-vowel). Because the mark repeats the
*already-folded* previous hiragana, the katakana vs hiragana form of the mark is
irrelevant after this step.

New pass (replaces the step-1 `filter_map` collect):

```rust
let mut chars: Vec<char> = Vec::new();
for ch in reading.nfkc() {
    match ch {
        'ゝ' | 'ヽ' => {
            if let Some(&prev) = chars.last() {
                chars.push(unvoiced_base(prev));
            }
        }
        'ゞ' | 'ヾ' => {
            if let Some(&prev) = chars.last() {
                chars.push(voiced_form(unvoiced_base(prev)));
            }
        }
        _ => {
            if let Some(h) = kana_to_hiragana(ch) {
                chars.push(h);
            }
        }
    }
}
```

`unvoiced_base` / `voiced_form` are small match tables over the hiragana dakuten
pairs (か↔が さ↔ざ た↔だ は↔ば/ぱ, and the corresponding rows). `voiced_form` returns
the input unchanged when no voiced form exists.

### Tests

- `normalize("ものゝふ") == normalize("もののふ")` (the headline case).
- `normalize("こゞろ")` voiced repeat → `こゞろ == こごろ`? No — `こゝろ` (kokoro, 心) is
  the real word: `こゝろ == こころ`. Add both: `こゝろ == こころ`, and a voiced case
  `いすゞ == いすず` (`ゞ` after `す` → `ず`).
- Katakana mark: `normalize("スヽメ") == normalize("ススメ")`.
- Idempotence holds (expansion produces plain kana; re-normalizing is a no-op).
- Mark at start is dropped: `normalize("ゝあ") == normalize("あ")`.

---

## B. Classification split → first-class column

### Problem

Every zero-winner row is labeled `"nonstandard_ruby"` in `evidence_detail` today,
conflating two causes the validation run measured as ~942k vs ~959k:

1. **Genuine reading gap** — at least one analyzer *exactly tiled* the base and produced
   a real reading, but none matched the editor. The dictionary-development signal.
2. **Structural** — no analyzer could align (all `boundary-misalign` / `no-reading`).
   An artifact, not a reading gap.

They are only separable by parsing `evidence_detail` and checking every analyzer's
`align`. Promote the distinction to a column.

### Three-way `classification`

Computed in `adjudicate`, per emitted row:

| winners | any analyzer `align == "exact"` | `classification`   |
|---------|--------------------------------|--------------------|
| ≥1      | (any)                          | `resolved`         |
| 0       | yes                            | `nonstandard_ruby` |
| 0       | no                             | `unalignable`      |

Rationale for "any exact" (not "all exact"): one analyzer producing a real,
comparable reading that disagrees with the editor is already evidence of a genuine
reading the dictionaries lack; a co-occurring alignment failure in another analyzer
does not weaken that. `resolved` keeps its current meaning (unique winner *or*
ambiguous ≥2-winner — the winner column already distinguishes those).

`adjudicate` tracks a single `any_exact: bool` accumulated in the existing per-analyzer
loop (it already computes each `reading.align`). `evidence_detail`'s embedded
`classification` string uses the identical value (kept for provenance).

### Schema delta (additive)

Add `classification VARCHAR` to `nway_region_oracle_evidence`, positioned after
`oracle_source`:

- `schema.rs`: `NwayRegionOracleEvidenceRow.classification: String`; add
  `"classification"` to the `column_names` arm (after `"oracle_source"`).
- `writer.rs`: append it as a non-null string column in
  `append_nway_region_oracle_evidence` + `nway_region_oracle_evidence_schema()`.
- `sql/schema.sql`: `classification VARCHAR` after `oracle_source`.
- `sql/morph_views.sql` + `sql.rs`: add `classification` to the presence-probed
  `warehouse_nway_region_oracle_evidence` view select list.

`SCHEMA_VERSION` stays **2**: the table is a v2 presence-probed sidecar introduced in
the same cycle, no external consumer depends on its column set yet, and every run
regenerates from scratch. (If a consumer later pins the column set, that is when the
version moves — not now.)

### Tests

- `zero_match_is_nonstandard_ruby` (existing): both analyzers tile exactly but disagree →
  `classification == "nonstandard_ruby"`; assert the new column, not just the JSON.
- New `all_analyzers_unalignable_is_unalignable`: every analyzer `boundary-misalign`,
  zero winners → `classification == "unalignable"`.
- New `resolved_row_classification`: unique winner → `"resolved"`.
- `oracle_pipeline_from_aat_to_parquet` (existing e2e): assert the column round-trips to
  Parquet (read back one row's `classification`).

---

## P1. Front-load large documents in `WarehouseWorkQueue`

### Current behavior (the tail)

`take_batch` returns **all** regular batches (ascending size, 32 docs/batch) before it
ever touches the `large` bucket (`≥ 5 MiB`, 1 doc/batch), which it then drains at
`large_lanes = jobs/4`. On the corpus (17,885 files / 2,891 MiB; 32 large files /
295 MiB; biggest 34.3 MiB) this serializes ~149 MiB-equiv of the biggest work onto 2
lanes **after** all small work finishes, while `jobs − large_lanes` workers idle.
Ideal makespan at jobs=8 is 2891/8 ≈ 361 MiB-equiv; the deferred tail pushes it to
~473 (~+30%). The biggest single doc (34 MiB) is far below the ideal per-worker load,
so nothing intrinsic forces a tail — it is purely the drain order.

### Change

Front-load the large bucket **biggest-first**, keeping the `large_lanes` memory cap:

1. Sort `large` **descending** by size (regular stays ascending; tie-break by path).
2. `take_batch` prefers a large batch whenever a large lane is free
   (`active_large_batches < large_lanes` and `large` non-empty), else a regular batch,
   else (regular exhausted) a remaining large batch within the lane cap. Factor the
   large-batch construction into a private `pop_large_batch(&mut self)` helper so the
   two large-dispatch sites are not duplicated.

Net scheduling: the first `large_lanes` workers to pull work take the biggest docs at
t=0; every other worker takes regular work; as each large doc completes, the freed
worker takes the next-biggest large doc (cap preserved). The tail fills with cheap
regular batches instead of the most expensive documents.

Correctness is unaffected: `shard_index` is only a staging directory name, the merge
sorts `shard_run_dirs`, and the per-batch outputs are independent. The `≤ large_lanes`
concurrent-large invariant is preserved exactly.

### Memory effect (must be validated, not assumed)

Front-loading makes large-doc processing **overlap** regular-batch processing, whereas
before they were disjoint phases. Peak RSS therefore rises from
`max(jobs × regular_batch, large_lanes × large_doc)` toward
`large_lanes × large_doc + (jobs − large_lanes) × regular_batch`. The `auto_jobs`
model was calibrated on the old disjoint-phase scheduler, so it may now slightly
under-budget. This is acceptable and guarded, not ignored:

- The `large_lanes` cap still bounds concurrent large docs (the dominant term).
- The hinoki validation captures **peak RSS via `/usr/bin/time -v`** across the jobs
  sweep; any regression is measured before it can OOM a smaller host.
- hinoki has 88 GiB available against a projected 36–59 GiB peak, so the sweep runs
  with wide headroom.

### Tests (`WarehouseWorkQueue` unit tests)

- **Biggest-first, front-loaded:** inputs = 3 large (sizes 30/20/10 MiB) + 5 regular,
  `large_lanes = 2`. First two `take_batch` calls return the 30 then 20 MiB large docs
  (`is_large`), before regular is exhausted. Third call (lane cap hit, 2 active) returns
  a regular batch, not the 10 MiB doc.
- **Lane cap honored:** with 2 active large batches, `take_batch` never returns a third
  large batch until a `complete_batch(true)`.
- **Tail drain:** with regular exhausted and one large doc left under the cap,
  `take_batch` returns it.
- **Exact coverage:** every input is returned exactly once across a full drain
  (existing invariant, re-asserted under the new order).

---

## Validation (hinoki, single combined run + jobs sweep)

Prereq: `git pull` on hinoki, rebuild release binary.

1. **Correctness/parity first (subset):** run a bounded subset (e.g. all 32 large docs +
   a few hundred regular) at the current default and confirm the oracle still emits and
   the new column populates.
2. **Jobs sweep for P1:** on the full corpus, measure wall-clock **and peak RSS**
   (`/usr/bin/time -v`) at `--jobs 8`, `--jobs 12`, `--jobs 16`. Expected: P1 removes
   the tail so higher jobs now improve wall-clock (the pre-P1 recipe found 60 min@19 >
   42 min@10 *because of* the tail). Record the sweep table.
3. **Canonical combined run** at the sweep-chosen optimum. Confirm:
   - Purely additive: every prior table row-count-identical to
     `full-2026-07-07_055139-jobs8`.
   - `classification` column present; `resolved + nonstandard_ruby + unalignable` sum
     equals the prior total oracle row count; `nonstandard_ruby` count drops from the
     prior 1,901,333 toward the ~942k genuine-gap figure (the balance now `unalignable`).
   - Iteration-mark spot-checks: `武士《ものゝふ》`-class rubies now resolve/align where
     they previously fell into the structural bucket.
4. Record wall-clock delta and the classification breakdown in the plan's Validation
   Record.

## Testing strategy summary

- Pure unit tests for A (`reading_norm`) and the queue ordering for P1 — no I/O.
- `adjudicate` unit tests for B assert the column value directly.
- One Parquet round-trip test confirms the new column serializes/reads back.
- Full behavioral confidence comes from the hinoki validation run above.
