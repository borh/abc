# Bare-toggle placement attribution (横組み / 罫囲み)

**Date:** 2026-07-12
**Tool:** `reports/aat-fidelity/bare-toggle-placement.py`, run on hinoki
against the pinned nix corpus store path
`/nix/store/sdr1imwrxfldvlwzs2d2fhs11vxncgpx-aozorabunko-corpus` — the
identical store path recorded as `source_inventory.corpus` in the frozen
fidelity summary (`2026-07-09-corpus-adapter-fidelity.summary.json`) and
used by `2026-07-11-keigakomi-yokogumi-denominator-attribution.md`.
**Output:** `2026-07-12-bare-toggle-placement-attribution.summary.json`
(script output, verbatim).

## Purpose

The denominator-attribution audit (2026-07-11) established that the `other`
bucket for yokogumi/keigakomi is dominated by the **bare toggle** forms
`［＃横組み］`/`［＃横組み終わり］` (1,599/1,589) and
`［＃罫囲み］`/`［＃罫囲み終わり］` (25/25), and read them as a candidate
extension of the *block* classifier ceiling ("the ceiling rises toward
3,394 for yokogumi and 292 for keigakomi"). Before Phase 5 commits a
classifier design to that reading, this audit decomposes every bare-toggle
occurrence by **line placement** (marker alone on its line vs mid-line) and
**pairing shape** (open/close on the same line vs across lines, stack-paired
in document order).

## Result — bare toggles are inline spans, not blocks

| construct | opens | closes | line-isolated | mid-line | same-line pairs | cross-line pairs | unpaired opens | unpaired closes | works | works w/ unpaired |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| yokogumi | 1,599 | 1,589 | **0** | 3,188 | **1,589** | **0** | 10 | 0 | 338 | 10 |
| keigakomi | 25 | 25 | **0** | 50 | **25** | **0** | 0 | 0 | 8 | 0 |

Every bare-toggle marker in the corpus is **mid-line**, and every close
pairs with an open **on the same line** (1,589 + 25 pairs; zero cross-line
pairs, zero markers standing alone on a line). The samples in the summary
JSON show the actual usage: short Latin/horizontal runs inside vertical
text (`ウサギ［＃横組み］（Hare《ハーレ》）［＃横組み終わり］`) and boxed
words inside headings
(`［＃７字下げ］［＃大見出し］［＃罫囲み］［＃横組み］ハーレムの王…`),
including multiple pairs per line and cross-construct nesting of a
yokogumi pair inside a keigakomi pair.

The 10 surplus `［＃横組み］` opens (1,599 − 1,589) are unpaired: 10
distinct works each carry exactly one open with no close anywhere in the
work (work IDs in the summary JSON's `samples.yokogumi.unpaired_works`).

**Consequence for the classifier design (supersedes the block-ceiling
reading for these forms):** the bare-toggle forms are *inline span* markup.
Classifying them as `yokogumi_block`/`keigakomi_block` would misrepresent
100% of corpus usage; the correct target is the existing AAT schema v2
`inline_container` kinds `"yokogumi"`/`"keigakomi"`. The
denominator-attribution report's ceiling arithmetic (194 + 3,200 → 3,394
*blocks*) remains correct as marker arithmetic but wrong as a block-count
projection; future classifier rates for the bare forms must be read against
the same-line-pair counts here (1,589 / 25), not against a block
denominator.

## Form-frequency context (verbatim in the summary JSON)

The bare tokens sit inside the full marker vocabulary as follows (top
forms): yokogumi — `［＃横組み］` 1,599, `［＃横組み終わり］` 1,589,
`［＃ここから横組み］` 182, `［＃ここで横組み終わり］` 179, inline-attr
`［＃「…」は横組み］` family and compound layout forms in the tail;
keigakomi — `［＃ここから罫囲み］` 200, `［＃ここで罫囲み終わり］` 194,
`［＃罫囲み］` 25, `［＃罫囲み終わり］` 25. The exact-token searches used
here cannot substring-match the verbose or inline-attr forms (both carry
intervening characters between `＃` and the construct token), so the bare
counts are not inflated by other families.

## Denominator note (named, not forced)

This scan reads **17,878** zip-borne works (4 zip archives unreadable —
the same known-bad set the 2026-07-11 audit skipped). The frozen fidelity
figure of 17,886 additionally counts the 8 stray plaintext (non-zip)
sources noted in the 2026-07-11 audit's corpus-layout note; those 8 are
not scanned here. The Phase 5 delta audit runs over the pipeline's own
17,886-work traversal and its adoption counters are the binding figures;
this report's pair counts (1,589 / 25 / 10 unpaired) are the design-time
expectation they are checked against.
