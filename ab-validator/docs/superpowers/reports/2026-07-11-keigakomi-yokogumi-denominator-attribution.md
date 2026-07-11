# Keigakomi/yokogumi/jizume denominator attribution audit

**Date:** 2026-07-11
**Extends:** `2026-07-09-aozora-pipeline-construct-gap-backlog.md` (block-start
grep 200/183/1,373) and `2026-07-09-full-nix-denominator-recompute.md` (pinned
source-authority denominators 717/3,690/3,239, frozen in
`2026-07-09-corpus-adapter-fidelity.summary.json` — **not edited by this
report**).
**Tool:** `reports/aat-fidelity/denominator-attribution.py`, run against the
pinned nix corpus (`nix build .#aozorabunko-corpus`), store path
`/nix/store/sdr1imwrxfldvlwzs2d2fhs11vxncgpx-aozorabunko-corpus` — the
**identical** store path recorded as `source_inventory.corpus` in the frozen
fidelity summary, so this audit reads the exact same corpus snapshot that
produced 717/3,690/3,239.
**Output:** `2026-07-11-keigakomi-yokogumi-denominator-attribution.summary.json`
(script output, verbatim).

## Purpose

The backlog's block-start grep (200/183/1,373) and the frozen fidelity
denominators (717/3,690/3,239) count different things. This report decomposes
each construct token's marker occurrences by *form* on the pinned corpus, so
that (a) both prior figures can be located inside one decomposition, and (b)
readers of the future `keigakomi_block`/`yokogumi_block` classifiers know
which subset of the denominator those classifiers can ever match.

## Corpus layout note (methodology correction to the brief)

The brief's script assumed a flat directory of `*.txt` files. The pinned
`aozorabunko-corpus` derivation is a website mirror: each work's Shift_JIS
text is a single `.txt` member inside a per-work zip under
`cards/<id>/files/*.zip` (`rglob("*.txt")` only finds 8 stray plaintext
files — confirmed by direct inspection). The script was adjusted to walk
`cards/*/files/`, open each zip, and read its first non-`README.txt` `.txt`
entry (mirroring `crates/ab-index/src/index.rs`'s
`collect_source_files`/`zip_text_entries`, which is what produced the
`works_scanned: 17886` figure in the frozen fidelity summary). This is a
larger change than "adjust the glob pattern" — flagged explicitly rather than
silently expanded, per the task's honesty requirement. The classification
logic (`classify()`, the four marker forms) is transcribed unchanged from the
brief.

Four zip archives were unreadable (bad CRC / bad magic / not a zip) and were
skipped with a warning; **17,886 works were scanned** — matching
`source_inventory.works_scanned: 17886` in the frozen fidelity summary
exactly.

## Decomposition (script output, verbatim)

| construct | block_start | block_end | inline_attr | other | **total** | works | **denominator** | **backlog block-start** |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| keigakomi | 240 | 204 | 238 | 52 | **734** | 160 | 717 | 200 |
| yokogumi | 194 | 184 | 116 | 3,200 | **3,694** | 422 | 3,690 | 183 |
| jizume | 1,533 | 1,373 | — | — | **2,906** | 252 | 3,239 | 1,373 |

(Full JSON: `2026-07-11-keigakomi-yokogumi-denominator-attribution.summary.json`.)

## Reconciliation against the frozen denominators (717/3,690/3,239)

The four-form decomposition above (`block_start`/`block_end`/`inline_attr`/
`other`, a simple substring-based classifier) gets close but does not exactly
reconcile for all three constructs:

- **yokogumi: 3,694 vs 3,690 — reconciled (within 0.1%).**
- **jizume: 2,906 vs 3,239 — a real, named 333-occurrence (10.3%) shortfall.**
- **keigakomi: 734 vs 717 — over by 17 (2.4%), and the excess is partly
  spurious** (see residual below).

To find out *why*, the construct's own `source_patterns` regex lists from
`data/aozora-syntax-coverage.toml` (`decoration.keigakomi` line 2471,
`layout.yokogumi` line 2919, `indentation.jizume` line 2791 — the same
patterns the source-authority instrument that produced 717/3,690/3,239 is
built from) were applied directly to the same pinned corpus, deduplicating
overlapping pattern matches per construct (`re.finditer` over an alternation
of that construct's own patterns, so no marker is double-counted across two
patterns that both match it):

| construct | matrix-pattern count | frozen denominator | match |
| --- | ---: | ---: | --- |
| yokogumi | **3,690** | 3,690 | **exact** |
| jizume | **3,239** | 3,239 | **exact** |
| keigakomi | **673** | 717 | **short by 44 (6.1%)** |

**yokogumi and jizume reconcile exactly** once the construct's own official
pattern set (not a bare-token substring search) is used — confirming the
717/3,690/3,239 denominators are internally consistent with the same
`aozora-syntax-coverage.toml` matrix and the same pinned corpus, on two of
three constructs. **keigakomi does not fully reconcile even under the
matrix's own patterns** — 673 of 717 accounted for, a genuine unexplained
44-marker residual. Two encoding-related hypotheses were checked and ruled
out (no work in the corpus is UTF-8-encoded — `decode_source_bytes` tries a
BOM-gated UTF-8 path before falling back to Shift_JIS, and zero works in this
corpus carry a UTF-8 BOM or fully-valid multi-byte-UTF-8 body), so the gap is
not a decoding artifact on this end. **Named, not forced:** the residual is
reported as-is; no further explanation is claimed.

### Why the bare-substring decomposition (734/3,694/2,906) differs from the matrix-exact figures (673/3,690/3,239)

- **jizume's shortfall in the bare-substring pass (2,906, i.e. -333) is fully
  explained**: two of `indentation.jizume`'s six official patterns key on
  the token **字組み** ("character-grouping"), not literally 字詰め —
  `［＃ここから…字下げ、１行N字組みで］` (178 occurrences) and `［＃ここで字下げ、N字組み終わり］`
  (178 occurrences), plus a `字下げ、N字詰め` compound form (136) the bare
  scanner also missed. These three pattern-only occurrences plus the plain
  `字詰め` markers combined give exactly 3,239. The instrument's jizume
  denominator is not "per-line" — it is per-marker, but spans a slightly
  wider vocabulary (字組み) than the literal token searched.
- **keigakomi's excess in the bare-substring pass (734, i.e. +17 over 717)
  is a false reconciliation**, not a real one: it includes markers whose
  literal substring is `罫囲み` but which the matrix attributes to a
  *different* construct row entirely — `表罫囲み` ("table ruled box",
  1 occurrence directly observed, plus compound `ここから…表罫囲み` forms) is
  scored under a table/`figure` construct (`data/aozora-syntax-coverage.toml`
  line 3175), and `［＃ここで…段組み、罫囲み終わり］` is scored under the
  column-layout construct (line 3047). The bare scanner folds these into
  keigakomi because it only checks "does `罫囲み` appear as a substring,"
  while the matrix's construct-specific patterns correctly exclude them. Once
  cross-construct collisions are removed (matrix-exact count: 673), keigakomi
  is **short** of 717 by 44 — the true, unresolved direction of the gap.

## Reconciliation against the backlog's block-start grep (200/183/1,373)

- **keigakomi: 200 exact + 40 compound = 240 (block_start bucket).** The
  backlog's cited 200 is the pure `［＃ここから罫囲み］` form; the script's
  `block_start` bucket additionally counts 40 compound `ここから…罫囲み` markers
  that combine keigakomi with another clause (`５字下げ、２１字詰め、罫囲み`,
  `プログラム、表罫囲み`, `２字下げ、横書き、罫囲み`, etc.) — all still block-opens.
  200 is a strict subset of 240.
- **yokogumi: 183 (backlog) vs 194 (block_start) vs 184 (block_end).** Close
  but not exact to either; the backlog's own grep methodology (not
  reproduced here) evidently differs slightly from a pure
  `startswith("［＃ここから")` classifier. Notably, `block_end` (184) is
  nearer to 183 than `block_start` (194) is — treat as a coincidence worth
  flagging, not a confirmed label swap.
- **jizume: 1,373 (backlog) = 1,373 (block_end) exactly; block_start is 1,533.**
  The backlog table labels its 1,373 an "occ" figure under a "corpus
  start-form" header, but it is an **exact** match to this script's
  `block_end` (`［＃ここで字詰め終わり］`) count, not `block_start`. This is
  named as an open question about the backlog's own grep, not resolved here
  — the backlog report is frozen and not edited.
- **The large residual bucket is real and structural, not noise.** For
  yokogumi, `other` = 3,200, and 3,188 of those are the **bare** toggle form
  `［＃横組み］` (1,599) / `［＃横組み終わり］` (1,589) — a shorter notation
  variant lacking the `ここから`/`ここで` frame words that the backlog's grep
  (and this script's `block_start`/`block_end` buckets) do not recognize as
  block markers at all. For keigakomi, 50 of the 52 `other` occurrences are
  the same bare-toggle pattern (`［＃罫囲み］` × 25, `［＃罫囲み終わり］` × 25).
  **This means the verbose `ここから`/`ここで` block-start count is not the
  ceiling on how often these constructs appear as scoped blocks in the
  corpus** — it undercounts the true block-open/close rate roughly 8:1 for
  yokogumi once the bare form is included.

## The required reading: what denominates what

- **The all-form inventory figures (717/3,690/3,239) denominate the fidelity
  instrument's construct rates**, consistent with every other construct row
  in `2026-07-09-corpus-adapter-fidelity.summary.json` — they are not scoped
  to block-openers only, and (per the reconciliation above) for yokogumi and
  jizume they reconcile exactly against the construct's own official pattern
  vocabulary; keigakomi has a named, unresolved 44-marker (6.1%) shortfall
  under the same methodology.
- **The block-start subset is the ceiling for `keigakomi_block`/
  `yokogumi_block` numerators** as scoped in the backlog's classifier plan
  (items 2–3: typing `containerOpen` from `［＃ここから横組み／罫囲み］` … `［＃ここで
  …終わり］`). Per this report's decomposition, that subset is **240** for
  keigakomi and **194** for yokogumi under the verbose form alone — and, if a
  future classifier is extended to also recognize the bare `［＃横組み］`/
  `［＃罫囲み］` toggle form (`other` bucket above), the ceiling rises toward
  **3,394** (194 + 3,200) for yokogumi and **292** (240 + 52) for keigakomi.
  A block classifier scoped to the verbose form only can **never** reach a
  rate anywhere near 1.0 against the full 3,690/717 denominators — readers
  scoring `keigakomi_block`/`yokogumi_block` rates must read them against
  this attribution table, not against the raw 717/3,690/3,239 figures.
- **This report is the citation for that reading.**

## Rotation-A cross-reference (well-paired subset, not a new count)

The Phase 3 rotation-A container-rewrite delta audit
(`2026-07-11-phase3-capability-delta.summary.json`) classified **163** works
as `rewritten` out of 17,886 compared (`identical` 17,723, `span_confined` 0,
verdict `PASS`). That report's own review already connects this to the
backlog's per-construct work sets: 163 is "on the order of the keigakomi
(106) + yokogumi (88) backlog work sets with overlap (106 + 88 = 194; 163
implies ~31 works carrying both/overlapping or unpaired markers)"
(`2026-07-11-phase3-capability-delta.md`). This audit's own work-counts —
**160** works carrying any keigakomi marker form and **422** works carrying
any yokogumi marker form (all-form, not block-start-only) — are
consistent with, but not identical to, the backlog's block-start-only work
counts (106/88): the all-form work counts are supersets that also pick up
works whose only keigakomi/yokogumi occurrence is an inline-attribution or
bare-toggle form rather than a verbose block. The 163-work rotation-A figure
sits inside the backlog's narrower (block-start-only) work sets, as expected
for a container-rewrite measured against the verbose containerOpen/Close
grammar specifically.

## Summary table (all figures, one place)

| construct | denom (717/3,690/3,239) | matrix-exact | bare-substring total | block_start | backlog block-start | works (all-form) | works (backlog, block-start) |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| keigakomi | 717 | 673 (−44, unresolved) | 734 (+17, false reconcile) | 240 | 200 | 160 | 106 |
| yokogumi | 3,690 | 3,690 (exact) | 3,694 (+4) | 194 | 183 | 422 | 88 |
| jizume | 3,239 | 3,239 (exact) | 2,906 (−333, explained) | 1,533 | 1,373 (= block_end, not block_start) | 252 | 230 |
