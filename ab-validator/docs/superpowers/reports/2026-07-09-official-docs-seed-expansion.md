# Official-docs seed expansion (§5)

**Date:** 2026-07-09
**Extends:** `2026-07-08-aozora-parser-comparison-study.md` §4.5 (independent-instrument
corroboration); open-work item §5 of `2026-07-09-parser-comparison-followups-handoff.md`.
**Goal.** The §4.5 seed was 11 *clean canonical* cases — enough to show the P4suta
family weaknesses replicate, but (per its own caveat) it reproduces *relative* weakness,
not *absolute* rates, because it doesn't stress edge cases. Expand it toward edge-case
coverage so it functions as a fuller independent instrument.

## What changed

`reports/parser-conformance/author-official-seed.py`: **11 → 30 vectors**, all sourced
from the official 青空文庫 annotation docs (`aozora.gr.jp/annotation`, CC-BY 4.0;
verbatim examples where the doc provides them). The 19 additions are variant/edge forms,
not new families:

| family | source page | added edge cases |
| --- | --- | --- |
| 傍点 variants | `emphasis.html` | 白ゴマ・丸・二重丸・蛇の目・ばつ・左に傍点 (6) |
| 傍線 variants | `emphasis.html` | 二重傍線・波線・左に傍線 (3) |
| heading levels/forms | `heading.html` | 中見出し・小見出し・大見出し(range)・同行中見出し・窓中見出し (5) |
| gaiji forms | `external_character.html` | Unicode (U+…)・description-only・JIS X 0213 kana (3) |
| accent | `external_character.html` | accent-decomposed Latin 〔e'tiquette〕 (1) |
| ruby edge | `annotation/` | explicit ｜ marker on a Latin base (1) |

Each carries `[provenance:official]` + the specific spec page. Spans remain **nominal**
(the AAT comparison is kind-sequence; precise spans — needed only for the `inspect`
typed path — are the remaining step toward a full absolute-rate instrument, see caveat).

## Activation against the recommended parser (aozora)

Scored the full 30 through the harness (`run-aozora-notation-spec.py`) against aozora —
both its `inspect` typed projection and the `ab-aozora` AAT kind-sequence path.
Artifact: `2026-07-09-official-docs-seed-aozora-activation.json`.

**AAT kind-sequence over the 19 new edge cases: 14 pass, 5 diverge.**

- **Pass (recommended parser handles the edge case):** every sesame-dot variant
  (白ゴマ/丸/二重丸/蛇の目/ばつ, and left-margin 左に傍点), all three gaiji forms
  (Unicode, description-only, JIS X 0213 kana), 中見出し/小見出し, 同行中見出し, 窓中見出し,
  and Latin-base ruby. The core-construct edge variants are robust.
- **Diverge (new, finer-grained findings beyond §4.5's clean forms):**
  - **Underline variants** (二重傍線, 波線, 左に傍線) — fold away from a distinct
    `emphasis`, consistent with §4.5/§4.7's "bousen folds into a generic marker," now
    shown to hold across the underline sub-family.
  - **Accent-decomposed Latin** (〔e'tiquette〕) — not typed as `accent` (matches the §1
    HEAD-parser vocabulary scan: aozora emits no `accent` kind). An independent
    corroboration of that gap.
  - **Range-form heading** (［＃大見出し］…［＃大見出し終わり］) — diverges from the
    forward-reference form (`［＃「…」は大見出し］`) that passes; the container/range
    heading is not typed as `heading`. A new edge-case finding.

`inspect`-mode rows warn on span mismatches by construction (the seed's spans are
nominal); those are not capability findings.

## Status & remaining work

- The instrument is now **broad enough to surface sub-family and form-variant
  divergences**, not just family-level weakness — the §5 goal.
- **Precise spans** would let the `inspect` typed path score cleanly (removing the
  nominal-span warnings) — the last step to reproduce *absolute* rates. Deferred; it
  requires per-case byte-offset authoring.
- **Full 4-adapter re-score** (aozora2, aozora-rs, aozora2html, aozora-epub3) on the
  expanded set is a mechanical follow-up (build the adapters, add `--adapter` specs);
  this pass activated only the recommended parser, which is the decision-relevant one.
