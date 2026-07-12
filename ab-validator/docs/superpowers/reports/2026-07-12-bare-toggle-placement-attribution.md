# Bare-toggle placement + adoption-grammar attribution (横組み / 罫囲み) — Revision 2

**Date:** 2026-07-12 (Revision 2, same day; supersedes Revision 1 in place)
**Tool:** `reports/aat-fidelity/bare-toggle-placement.py` (Revision 2), run
on hinoki against the pinned nix corpus store path
`/nix/store/sdr1imwrxfldvlwzs2d2fhs11vxncgpx-aozorabunko-corpus` — the
identical store path recorded as `source_inventory.corpus` in the frozen
fidelity summary (`2026-07-09-corpus-adapter-fidelity.summary.json`).
**Output:** `2026-07-12-bare-toggle-placement-attribution.summary.json`
(script output, verbatim).

## Revision 2 — what changed and why

Revision 1's scanner used a private corpus reader (17,878 zip-borne
entries; no plaintext sources; silent skips; no recovery of the two
damaged-central-directory zips) and independent per-construct stack
pairing that did not model the proposed classifier grammar. Review
(P5-2/P5-3) rejected both. Revision 2:

- **Reads the production universe exactly.** The instrument imports the
  litigated reader of `reports/source-regions/terminal-provenance-split.py`
  (the instrument bound card-for-card to the Rust pipeline's 17,886-entry
  universe during Phase 4): `cards/*/files/` discovery of `.zip` and bare
  `.txt` candidates, content-sniffing zip-vs-plain dispatch, stdlib zip
  reading with the local-header-trusting fallback, Shift_JIS
  `errors="replace"` decode, `work_id_from_index_path` identity. The run
  **fails (exit 2) unless exactly 17,886 entries are read**; this run read
  17,886. Every excluded candidate is recorded with path and reason
  (9 total: 5 zips with no `.txt` member — `_ttz`/`_etc` auxiliary
  archives — and 4 zip-shaped files unreadable by both paths, the same
  set the Rust pipeline excludes); the two recoverable
  damaged-central-directory zips (`cards/001393/…50710_ruby_36965.zip`,
  `cards/001505/…58100_txt_60357.zip`) are read via the fallback and
  recorded as **recovered**, with hashes, not "unreadable". Per touched
  entry the summary records work id, source label (archive::member),
  reader path, decode mode, and source sha256.
- **Measures the adoption grammar, not raw pairing.** `classify_line` in
  the instrument is the normative Python model of the Phase 5 Contract 1
  two-pass algorithm (one global nesting stack; same-construct reopen,
  orphan open/close, and improper interleaving invalidate
  construct-per-line; candidates rolled back if their construct is
  invalidated; adoption only from valid lines). The Rust classifier must
  mirror this model test-for-test.

## Grammar-true result (the binding design-time expectations)

| counter | yokogumi | keigakomi |
| --- | ---: | ---: |
| **adopted pairs** | **1,582** | **25** |
| orphan opens (raw) | 10 | 0 |
| orphan closes | 0 | 0 |
| same-construct reopen events | 1 | 0 |

Whole-corpus grammar totals: 3,238 bare-toggle markers on 1,227 lines in
344 touched entries; **3,214 markers adopted** (2 × 1,607 pairs), **24
markers raw-preserved** (10 orphan opens + 14 rollback markers), 0
improper-interleave events, 1 proper cross-construct nesting (a yokogumi
pair inside a keigakomi pair), 0 lines mixing adopted and invalid groups,
11 entries carrying an invalid line.

The marker arithmetic closes exactly: 3,238 = 3,214 + 24.

**Note vs Revision 1:** naive per-construct pairing counted 1,589
yokogumi "same-line pairs". Under the actual grammar, 7 of those pairs
roll back — they share a line with an invalid same-construct marker — so
the adoptable count is **1,582**. This is precisely the class of
divergence Revision 1 could not see.

## What the declined markers are (all 24, fully attributed)

- **10 orphan opens = the editorial example line `（例）［＃横組み］`** in
  10 entries' 凡例/notation-key sections (work ids in the summary JSON's
  `touched_entries`, e.g. `000106_55753`, `000214_48790`, `001804_56765`):
  Aozora's own注記 syntax being *quoted as an example*, never closed.
  Raw preservation is the correct reading — a classifier that adopted
  these would be wrong, and any auto-close variant would have swallowed
  text into a phantom span.
- **14 rollback markers = one malformed line in `000094_42338`**
  (`nusumareta_tegami.txt`): a long quotation line carrying 14 yokogumi
  markers where a re-open occurs while a span is still pending; all 7
  candidate pairs on that line roll back, the whole line stays raw. The same work's other 14 pairs
  (on valid lines) adopt.

## Placement conclusion (unchanged from Revision 1, now over the full universe)

Zero bare-toggle markers stand alone on a line; zero pairs span lines.
Every adoptable occurrence is a same-line inline span (Latin runs inside
vertical text, boxed words inside headings, multiple pairs per line, one
cross-construct nesting). The bare forms are *inline span* markup; the
correct AAT target is the existing schema-v2 `inline_container` kinds
`"yokogumi"`/`"keigakomi"`, not the `*_block` classifiers. The
denominator-attribution report's block-ceiling arithmetic (194 + 3,200 →
3,394 yokogumi "blocks") remains correct as marker arithmetic but is
superseded as a block-count projection for these forms; classifier rates
for the bare forms must be read against the adopted-pair expectations
here (1,582 / 25).

## Form-frequency context (verbatim in the summary JSON)

Bare tokens inside the full marker vocabulary (top forms): yokogumi —
`［＃横組み］` 1,599, `［＃横組み終わり］` 1,589, `［＃ここから横組み］`
182, `［＃ここで横組み終わり］` 179, then the inline-attr
`［＃「…」は横組み］` family and compound layout forms; keigakomi —
`［＃ここから罫囲み］` 200, `［＃ここで罫囲み終わり］` 194, `［＃罫囲み］`
25, `［＃罫囲み終わり］` 25. The exact-token searches cannot
substring-match the verbose or inline-attr forms (both carry intervening
characters between `＃` and the construct token), so the bare counts are
not inflated by other families. (Raw token counts 1,599/1,589 differ from
the grammar's 1,582 adopted + 10 orphan-open + 14 rolled-back markers
only in that the grammar accounts for *adoptability*, not presence.)
