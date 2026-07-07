# Phase 4: Ruby Oracle (`nway_region_oracle_evidence.parquet`) — Design

Date: 2026-07-07. Owner-approved scope (this session): the ruby oracle only —
the analysis-pass producer of `nway_region_oracle_evidence.parquet` — with the
four load-bearing forks resolved by the owner (interesting-only emission, full
normalization including historical kana, unique-winner-or-null adjudication,
producer-only cycle). Governing spec:
`docs/superpowers/specs/2026-07-05-interestingness-ranking-design.md`
(§Aozora Oracles / Ruby Oracle, §v2 Sidecar Tables / `nway_region_oracle_evidence.parquet`,
§v2 Scoring Additions, Implementation Phases §Phase 4, Decision 3, Decision 5).
This document specifies the *how*; the signal's downstream use is the governing
spec's Phase 5 and is not re-decided here.

Depends on: Phase 3 `projection_spans` (shipped — `SCHEMA_VERSION = 2`,
canonical warehouse `full-2026-07-06_160136-jobs8`, 3,524,294 ruby-base spans),
`docs/superpowers/specs/2026-07-06-projection-spans-design.md`.

## Goal

The analysis pass (`ab-morph-run analyze-aat --warehouse-dir`, Full profile)
writes `nway_region_oracle_evidence.parquet`: for each ruby-base span the
Aozora editor annotated, an adjudication of **which analyzer's reading matches
the editor's ruby reading and which disagree**. Aozora ruby (`東京《とうきょう》`)
is editor-sanctioned ground truth over a known character span at zero
annotation cost. ABC's full-corpus probe (ADR 0024,
`../abc/docs/adr/0024-parser-ir-span-and-ruby-direction.md`) measured
`ruby.direction` loss as the single largest AAT-divergence category
(1,764,113 occurrences); ruby is where divergence clusters, so this oracle is
v2's highest-leverage enrichment.

The oracle is computed **during the analysis pass** (Decision 5, and consistent
with Phase 3 Open Question 2): the pass already holds each analyzer's morphemes
with readings, the projected plaintext, the AAT ruby nodes (base + reading),
and the n-way disagreement regions — all in scope at the same point that emits
`projection_span_rows` in `run_analyze_aat_serial`. No second pass, no reliance
on the persisted `projection_spans.parquet`.

## Non-Goals

- The Phase 5 RRF `oracle` signal (`oracle_resolved_region_count`). This cycle
  produces the evidence table; ranking is untouched, so calibration does not
  re-open.
- The sibling oracles (`paired_edition`, `gold`, `silver_consensus`) and
  `nway_region_causes` — later Phase 4/6 work.
- Any change to which disagreements exist, how regions are computed, or how
  patterns score.
- A `SCHEMA_VERSION` bump (stays at 2 — see §Schema Versioning).

## Data grain and emission rule

Grain: **one row per ruby-base occurrence the oracle finds interesting.** The
owner-approved "interesting-only" policy reduces to a single self-contained
predicate over the per-analyzer readings of the base:

> **Emit a row iff at least one analyzer's normalized reading over the ruby base
> fails to match the normalized editor ruby reading.**

This partitions every ruby base into three outcomes:

| Per-analyzer match result | Outcome | Emitted? |
|---|---|---|
| All analyzers match | boring — a *reading* oracle has nothing to adjudicate (segmentation-only differences are cause-classification's domain, not ruby's) | **no** |
| Some match, some do not | **resolved** — winners vs losers | **yes** |
| No analyzer matches, but at least one analyzer produced a comparable exact reading | **nonstandard_ruby** — retained for manual review, not resolved | **yes** |
| No analyzer produced a comparable exact reading | **no_comparable_reading** — all analyzers were `boundary-misalign` or `no-reading`, so the ruby offers no dictionary comparison | **yes** |

The predicate needs only the per-analyzer readings; it does not require an
n-way disagreement to exist. This is what lets the oracle catch the
consensus-wrong case the governing spec names — all analyzers agree on a reading
yet all disagree with the editor — which is the "no analyzer matches" row.

**Precondition:** the oracle emits only for runs with **≥2 analyzers** (the same
precondition as n-way regions), so `region_index` always references a real n-way
region. A single-analyzer run produces no oracle rows.

## Reading extraction (per analyzer)

The concatenated analyzer reading over a ruby base is built from the readings of
the covered morphemes (§Alignment), read from the live `Morpheme.features` map:

| Analyzer family | Reading feature | Fallback |
|---|---|---|
| vibrato (`cwj`, `unidic-novel-202512`), vaporetto — all UniDic | `kana` (仮名形出現形, e.g. `トウキョウ`) | `pron` (発音形, e.g. `トーキョー`) when `kana` is absent or `*` |
| `sudachi-a`, `sudachi-c` | `reading_form` | — |

`kana` is preferred over `pron` because it is the modern-kana surface reading
(`トウキョウ` → `とうきょう`) and aligns to editor ruby without long-vowel
gymnastics; `pron` (`トーキョー`) needs the long-vowel canonicalization below and
is only the fallback. A covered morpheme with no reading available makes that
analyzer a **non-match** for the base with reason `no-reading` — never a silent
skip.

## Normalization (`reading_norm`)

A pure, deterministic canonicalizer applied identically to the concatenated
analyzer reading and to the editor ruby reading before equality comparison.
Layers, in order:

1. **NFKC**, then strip everything that is not a kana code point (interpuncts
   `・`, spaces, punctuation removed).
2. **Katakana → hiragana** fold (single target script; input ruby may be either).
3. **Discrete historical-kana (旧仮名 → 新仮名) substitutions:** `ゐ→い`, `ゑ→え`,
   `ぢ→じ`, `づ→ず`, `くわ→か`, `ぐわ→が`, and word-medial `は/ひ/ふ/へ/ほ →
   わ/い/う/え/お`.
4. **Long-vowel canonicalization:** collapse each vowel row's long-vowel family
   to one canonical marker — `おう/オー/あう`, the `ー` prolongation mark, and the
   historical `かう/さう…` o-row spellings all fold to a single long-o token;
   likewise for the other rows. This mechanically absorbs both `pron`'s `ー` and
   the bulk of 旧仮名 long-vowel divergence.
5. **Bounded lexical fold table** for genuinely non-mechanical historical
   readings (e.g. `てふ→ちょう`, `けふ→きょう`), pinned exhaustively by golden
   tests. Its membership is deliberately small and explicit.

**Honest degradation:** any historical form not covered by layers 3–5 does not
match. If at least one analyzer still produced a comparable exact reading, the
base falls to `nonstandard_ruby` and is *retained for review*; if every analyzer
was `boundary-misalign` or `no-reading`, it instead falls to
`no_comparable_reading`. The canonicalizer never invents a match. Growing the
fold table is a follow-up lever, not a correctness risk.

**False-match surface (low risk, by construction).** The long-vowel and medial-は
folds are aggressive unconditional substitutions (`かう→こう`, medial `ふ→う`), so
they can over-collapse genuine modern morae (`つかう→つこう`, `とうふ→とうう`).
Because `normalize` is applied *identically* to both the editor ruby and each
analyzer reading, this never corrupts a true match — both sides collapse the same
way. A *false* match would require a genuinely-wrong analyzer reading to collide
post-fold; since analyzers emit modern kana (long-o already `おう`, never the
historical a-row spelling), the aggressive folds effectively only fire on the
historical editor side and normalize it correctly. The residual false-match risk
is therefore low; the full-corpus validation's consensus-wrong spot-check is the
empirical guard, and tightening the fold conditioning is a follow-up lever.

## Alignment (base ↔ morphemes)

The ruby base's `projected_char_start..projected_char_end` and each morpheme's
`char_start..char_end` index the **same** projected-plaintext coordinate space
(morph analysis runs on the projected plaintext). Per analyzer:

- Covered morphemes = those fully contained in `[base_start, base_end)`.
- The analyzer's reading is **comparable only if the covered morphemes exactly
  tile the base**: contiguous, first morpheme's `char_start == base_start`, last
  morpheme's `char_end == base_end`.
- If a morpheme straddles a base boundary, the analyzer is a **non-match** with
  reason `boundary-misalign`. No reading is fabricated from characters outside
  the ruby span.

Rationale: a partial-overlap reading would include kana from outside the ruby
and produce false matches/mismatches. Treating misalignment as a scoped
non-match keeps every comparison meaningful and honest.

## Adjudication (unique-winner-or-null)

Given the per-analyzer match set over one ruby base:

- **Exactly one** analyzer matches → `winning_analyzer` = that analyzer id;
  `losing_analyzers` = all others.
- **Two or more** match → `winning_analyzer` = `null` (ambiguous — no forced
  winner); matchers are recorded in `evidence_detail.per_analyzer`;
  `losing_analyzers` = the non-matchers.
- **Zero** match with at least one exact comparable reading →
  `winning_analyzer` = `null`; `losing_analyzers` = all analyzers;
  `evidence_detail.classification = "nonstandard_ruby"`.
- **Zero** match with no exact comparable reading →
  `winning_analyzer` = `null`; `losing_analyzers` = all analyzers;
  `evidence_detail.classification = "no_comparable_reading"`.

`evidence_detail` (JSON, `Utf8`) is the honest debug/review record:

The typed serialization contract and the boundary rule that ruby readings stay
analyzer evidence rather than parser-IR text replacement are pinned in
`docs/superpowers/specs/2026-07-07-ruby-reading-evidence-contract.md`.

```json
{
  "ruby_base": "東京",
  "ruby_reading": "とうきやう",
  "ruby_reading_norm": "とうきょう",
  "classification": "resolved",
  "per_analyzer": {
    "vibrato:unidic-novel-202512": {"reading": "トウキョウ", "norm": "とうきょう", "match": true,  "align": "exact"},
    "sudachi-c":                    {"reading": null,         "norm": null,        "match": false, "align": "boundary-misalign"}
  }
}
```

`classification` ∈ `resolved` (≥1 match **and** ≥1 non-match — `winning_analyzer`
is set when exactly one matches, else `null` with every matcher flagged
`match: true` in `per_analyzer`), `nonstandard_ruby` (0 match, but ≥1 exact
comparable reading), and `no_comparable_reading` (0 match and no exact
comparable reading). `align` ∈ `exact`, `boundary-misalign`, `no-reading`.

## Table schema

`oracle_source` is always `"ruby"` this cycle. Two columns are added beyond the
governing spec's table (see Decision R1) so a row is uniquely keyed and
self-locating — a region may contain several ruby bases and a base may straddle
regions, so `region_index` alone is not a key.

| Column | Type | Purpose |
|---|---|---|
| `run_id`, `source_id`, `text_id` | `Utf8` | join keys |
| `region_index` | `UInt64` | first disagreement region overlapping the base; else the region containing `base_start` |
| `projected_char_start`, `projected_char_end` | `UInt64` | **(added)** exact base location in projected plaintext; part of the row key |
| `oracle_source` | `Utf8` | `"ruby"` |
| `winning_analyzer` | `Utf8` (nullable) | §Adjudication |
| `losing_analyzers` | `List<Utf8>` | §Adjudication |
| `evidence_detail` | `Utf8` | JSON, §Adjudication |

Row key: `(run_id, source_id, text_id, projected_char_start)`.

`region_index` maps the evidence to the n-way region Phase 5 will aggregate:
the first disagreement region overlapping `[base_start, base_end)`, or the region
containing `base_start` when the base sits entirely within agreement regions (the
consensus-wrong case still attaches to a real, if agreeing, region — a strong
signal).

## Schema versioning

`SCHEMA_VERSION` **stays at 2.** Per Decision 3 the bump fired at the *first*
analysis-pass-produced sidecar (`projection_spans`); subsequent additive sidecars
ship under the same version and are discovered by **presence-probing**, exactly
as `projection_spans` and `aozora_works` are. `morph_views.sql` gains a
`warehouse_nway_region_oracle_evidence` view, and `sql.rs` gains the same
section-strip treatment already used for `projection_spans` (drop the oracle
view block when the table file is absent), plus the accompanying assertion tests.
A pre-oracle v2 run therefore still opens cleanly; a post-oracle v2 run exposes
the view. No `READER_MAX_SCHEMA_VERSION` change.

## Module structure

- `ab-morph-run/src/oracle/reading_norm.rs` — the pure canonicalizer (layers
  above); no I/O; the property/golden-test target.
- `ab-morph-run/src/oracle/ruby.rs` — pure
  `fn adjudicate(ruby_bases, per_analyzer_morphemes, region_lookup)
  -> Vec<OracleEvidenceRow>`; no I/O.
- `ab-morph-run/src/warehouse/rows.rs` — `oracle_evidence_rows` mapper; wire the
  producer into `run_analyze_aat_serial` at the `projection_span_rows` site and
  add the table to the two-phase commit / per-shard merge set
  (`WarehouseWriter::create_for_tables`, `merge_warehouse_shard_runs`).
- `ab-warehouse/src/schema.rs` — `WarehouseTable::NwayRegionOracleEvidence`
  variant, `NwayRegionOracleEvidenceRow`, column list, filename
  `nway_region_oracle_evidence.parquet`.
- `ab-warehouse/src/writer.rs` — `append_nway_region_oracle_evidence`.
- `ab-warehouse/sql/schema.sql`, `ab-warehouse/sql/morph_views.sql`,
  `ab-warehouse/src/sql.rs` — table DDL, view, presence-probe section-strip +
  tests.

## Testing

- **Golden (`reading_norm`):** one pinned assertion per fold — katakana fold,
  `ゐゑ/ぢづ/くわ`, word-medial `は→わ` class, long-vowel collapse (`とうきょう` =
  `トーキョー` = `たうきやう` post-norm), and every entry of the bounded lexical table.
- **Property (hegel):** normalization idempotence (`norm(norm(x)) == norm(x)`);
  katakana and hiragana spellings of the same reading canonicalize equal;
  adjudication invariants (winner ∈ matchers; `{winner} ∪ losers` = all analyzers;
  `nonstandard_ruby ⇔ 0 matches with ≥1 exact comparable reading`;
  `no_comparable_reading ⇔ 0 matches with 0 exact comparable readings`;
  a row is emitted ⇔ ≥1 non-match).
- **Alignment:** exact-tile match, `boundary-misalign`, multi-morpheme base,
  `no-reading`.
- **End-to-end:** a small AAT fixture carrying a resolved base, a consensus-wrong
  base, and a nonstandard (artistic) base → assert exactly the expected rows,
  keys, winners, and `evidence_detail`.
- **Presence-probe:** `views.sql` drops the oracle section when the table is
  absent (mirror the existing `projection_spans` test).
- **Full-corpus validation** (~40 min at jobs=8; per handoff operational facts,
  tests via `cargo test -p ab-morph-run --features test-analyzer`): regenerate
  the canonical warehouse with the new table; record total row count, the
  resolved / nonstandard_ruby split, per-analyzer win counts, and a spot-check of
  the top consensus-wrong (`nonstandard_ruby` with high-agreement) cases. The new
  canonical run id is recorded in the implementation plan's validation record.

## Decisions

| # | Decision | Rationale |
|---|---|---|
| R1 | Add `projected_char_start`/`projected_char_end` to the governing spec's `nway_region_oracle_evidence` table | A region can hold multiple ruby bases and a base can straddle regions, so `(…, region_index)` is not a unique key; the offsets key the row and make it self-locating without a `projection_spans` join. Amends the governing spec's table def. |
| R2 | Emit iff ≥1 analyzer fails to match the ruby (interesting-only) | Owner fork 1. Captures resolved and consensus-wrong/nonstandard cases; drops the boring all-match majority; a reading oracle has nothing to say when all readings match. |
| R3 | Full normalization including bounded historical-kana folding | Owner fork 2. Pre-war ruby is heavily 旧仮名 while analyzers emit modern readings; without folding, legitimate matches read as mismatches and corrupt the signal. Ambiguous folds degrade to `nonstandard_ruby` when a comparable exact reading exists, else `no_comparable_reading`; both are retained, not discarded. |
| R4 | `winning_analyzer` set only on a unique match; null on ties or zero matches | Owner fork 3. No forced winner; `evidence_detail` carries the full per-analyzer picture. |
| R5 | Producer only — no Phase 5 RRF integration this cycle | Owner fork 4. Matches the governing spec's phase split; keeps the cycle independently verifiable and avoids re-opening calibration. |
| R6 | `SCHEMA_VERSION` stays 2; presence-probed views | Decision 3 bumped at the first analysis-pass sidecar; additive sidecars ship under v2 with presence-probing, as `projection_spans`/`aozora_works` already do. |
| R7 | UniDic `kana` (fallback `pron`) as the reading field | `kana` is the modern-kana surface reading and aligns to editor ruby without long-vowel handling; `pron` requires the long-vowel layer and is the fallback. |
| R8 | Reading comparable only on exact base↔morpheme tiling | A partial-overlap reading would include kana outside the ruby span and produce false matches; misalignment is a scoped, reasoned non-match. |
