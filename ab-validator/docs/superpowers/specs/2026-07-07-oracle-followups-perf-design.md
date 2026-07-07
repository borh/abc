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

A mark repeats the previous **repeatable kana**, which is *not* the prolongation
mark `ー` (kept by `kana_to_hiragana`) — a mark after `ー` must repeat the kana
before it, and chained marks (`ゝゝ`) repeat each other's kana output. So the pass
tracks `last_kana` explicitly rather than reading `chars.last()`:

```rust
let mut chars: Vec<char> = Vec::new();
let mut last_kana: Option<char> = None; // last repeatable kana (never 'ー')
for ch in reading.nfkc() {
    match ch {
        'ゝ' | 'ヽ' => {
            if let Some(prev) = last_kana {
                let r = unvoiced_base(prev);
                chars.push(r);
                last_kana = Some(r); // so 'ゝゝ' chains
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

`unvoiced_base` / `voiced_form` are small match tables over the hiragana dakuten
pairs (か↔が さ↔ざ た↔だ は↔ば/ぱ, and the corresponding rows). `voiced_form` returns
the input unchanged when no voiced form exists.

### Tests

- `normalize("ものゝふ") == normalize("もののふ")` (the headline case).
- `normalize("こゞろ")` voiced repeat → `こゞろ == こごろ`? No — `こゝろ` (kokoro, 心) is
  the real word: `こゝろ == こころ`. Add both: `こゝろ == こころ`, and a voiced case
  `いすゞ == いすず` (`ゞ` after `す` → `ず`).
- Katakana mark: `normalize("スヽメ") == normalize("ススメ")`.
- Chained marks: `normalize("たゝゝ") == normalize("たたた")` (`last_kana` follows the
  mark's own output).
- Mark after `ー` repeats the pre-`ー` kana, not `ー`: `normalize("たーゝ") ==
  normalize("たーた")` (`last_kana` skips the prolongation mark). Contrived input, but
  pins the `last_kana`-vs-`chars.last()` decision.
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

The pivot is whether **any analyzer produced a *comparable reading*** — i.e. exactly
tiled the base **and** every covered morpheme had a reading feature, which the code
already records as `align == "exact"`. The other two `align` values both mean "no
comparable reading": `boundary-misalign` (couldn't tile) *and* `no-reading` (tiled, but
a morpheme lacked the reading feature). Grouping those two is deliberate — neither
yields a reading to weigh against the editor, so neither is a dictionary-development
signal. Computed in `adjudicate`, per emitted row:

| winners | any analyzer produced a comparable reading (`align == "exact"`) | `classification`       |
|---------|----------------------------------------------------------------|------------------------|
| ≥1      | (yes, by construction — a winner matched)                      | `resolved`             |
| 0       | yes                                                            | `nonstandard_ruby`     |
| 0       | no (all `boundary-misalign` or `no-reading`)                   | `no_comparable_reading`|

Rationale for "any" (not "all"): one analyzer producing a real, comparable reading that
disagrees with the editor is already evidence of a genuine reading the dictionaries
lack; a co-occurring alignment/reading failure in another analyzer does not weaken that.
`resolved` keeps its current meaning (unique winner *or* ambiguous ≥2-winner — the
winner column already distinguishes those). The `no_comparable_reading` bucket is not a
dead end: the exact per-analyzer reason (`boundary-misalign` vs `no-reading`) is still in
`evidence_detail.per_analyzer[*].align`.

`adjudicate` tracks a single `any_comparable: bool` accumulated in the existing
per-analyzer loop (set when `reading.align == "exact"`). `evidence_detail`'s embedded
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

**`SCHEMA_VERSION` stays 2, with an explicit reader-stability caveat.** Presence-probing
proves the *table* exists, not that `classification` exists — and the oracle view is
`SELECT * FROM read_parquet(...)`, so it creates cleanly over an old run but silently
omits the column. A *query* that references `classification` (a summarizer, or Phase 5's
RRF) would then fail on a pre-this-change oracle run. We do **not** bump `SCHEMA_VERSION`
(it is a warehouse-wide signal and the non-oracle tables are genuinely unchanged; bumping
would falsely mark the whole schema as evolved) and we do **not** add JSON-backfill code
(dead weight for a young sidecar). Instead this is documented and enforced by regen:

- The prior canonical oracle run (`full-2026-07-07_055139-jobs8`) is **pre-classification
  and not reader-stable** for any `classification`-referencing query.
- The validation run is a **clean full regen** that supersedes it; the plan's Validation
  Record states the prior run must be cleaned/superseded so no mixed-shape oracle sidecar
  is queried under the new views.

If a future need arises to read old and new oracle runs side by side, that is the trigger
to add a column-presence fallback — out of scope here.

### Tests

- `zero_match_is_nonstandard_ruby` (existing): both analyzers tile exactly but disagree →
  `classification == "nonstandard_ruby"`; assert the new column, not just the JSON.
- New `all_boundary_misalign_is_no_comparable_reading`: every analyzer `boundary-misalign`,
  zero winners → `classification == "no_comparable_reading"`.
- New `all_no_reading_is_no_comparable_reading`: analyzers exactly tile the base but every
  covered morpheme lacks a reading feature (`align == "no-reading"`), zero winners →
  `classification == "no_comparable_reading"` (guards the finding-3 distinction: tiled but
  unreadable is *not* labeled resolved/nonstandard).
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
- **The default `--jobs 0` (auto) path is validated too, not just explicit jobs.**
  `auto_jobs` is calibrated on the old disjoint-phase behavior, so its budget could now
  under-provision on a *smaller* host. The sweep includes a `--jobs 0` point; if its
  measured peak RSS exceeds the model's prediction for the chosen count by a meaningful
  margin, the plan recalibrates `auto_jobs` (a small overlap surcharge: raise
  `PER_ANALYZER_BYTES` or add a `large_lanes × large_doc_overhead` term) rather than
  leaving the default silently optimistic. Data-driven: no constant changes unless the
  measurement demands them.

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
   (`/usr/bin/time -v`) at `--jobs 8`, `--jobs 12`, `--jobs 16`, **and `--jobs 0`**
   (auto). Expected: P1 removes the tail so higher jobs now improve wall-clock (the
   pre-P1 recipe found 60 min@19 > 42 min@10 *because of* the tail). Record the sweep
   table (jobs, wall-clock, peak RSS). Compare the `--jobs 0` peak RSS to the `auto_jobs`
   model prediction; recalibrate per the memory-effect note only if the data demands it.
3. **Canonical combined run** at the sweep-chosen optimum, as a **clean full regen** that
   supersedes `full-2026-07-07_055139-jobs8` (see the schema reader-stability caveat —
   the prior oracle sidecar is pre-`classification` and must not be queried under the new
   views). Validate against the prior run with the invariants below.

**Validation invariants (finding 1 — A legitimately changes oracle rows, so a total-count
match is the *wrong* invariant):**

- **Non-oracle tables:** every table other than `nway_region_oracle_evidence` is
  **row-count-identical** to the prior run. A and B touch only the oracle path;
  `reading_norm` is oracle-only; P1 reorders shard processing but changes no row's
  content — so morphemes, projection_spans, nway_regions, etc. must not move.
- **Oracle table:** compare **keyed** on
  `(source_id, text_id, region_index, projected_char_start, projected_char_end)` and
  bucket the diff, rather than matching totals:
  - **dropped** — emitted before, absent now: iteration-mark expansion turned a
    false non-match into an all-match, and all-match rows are correctly *not* emitted
    (`ruby.rs` emit-iff-≥1-loser). **Expected > 0**; these are the A wins.
  - **classification-changed** — same key, `classification` moved (chiefly
    `nonstandard_ruby → no_comparable_reading` from B's split, or `→ resolved` from A).
  - **newly-emitted** — present now, absent before. **Expected ≈ 0**; a non-zero count
    means normalization turned a prior *match* into a non-match (a regression signal) and
    must be inspected, not waved through.
  - **unchanged** — same key, same fields.
  - Report all four counts. Sanity: `prior_total − dropped + newly ==` new total, and the
    new `nonstandard_ruby` count moves off the prior 1,901,333 toward the ~942k
    genuine-gap figure (balance now `no_comparable_reading`), minus whatever A dropped.
- **Iteration-mark spot-checks:** `武士《ものゝふ》`-class rubies appear in the **dropped**
  or `→ resolved` buckets, confirming they now match instead of inflating non-matches.

4. Record the sweep table, wall-clock delta, the four-bucket oracle diff, and the
   classification breakdown in the plan's Validation Record. This is the "small validation
   diff report" the invariant requires.

## Testing strategy summary

- Pure unit tests for A (`reading_norm`) and the queue ordering for P1 — no I/O.
- `adjudicate` unit tests for B assert the column value directly.
- One Parquet round-trip test confirms the new column serializes/reads back.
- Full behavioral confidence comes from the hinoki validation run above.
