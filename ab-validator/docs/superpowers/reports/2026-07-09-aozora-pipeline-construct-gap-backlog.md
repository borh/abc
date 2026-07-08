# aozora-pipeline construct-gap backlog — jizume / yokogumi / keigakomi

**Date:** 2026-07-09
**Parser:** `aozora-pipeline` (`P4suta/aozora`), HEAD-pinned `1a4f864` — the recommended
fork base (study §6).
**Extends:** `2026-07-08-aozora-parser-comparison-study.md` §4.9 (parity/support audit),
open-work item §2 of `2026-07-09-parser-comparison-followups-handoff.md`.
**Purpose:** turn §4.9's three named gaps into verified, actionable backlog items —
each checked against (a) the actual HEAD-parser output, (b) official 青空文庫 notation
docs, and (c) real corpus occurrences.

## TL;DR

The recommended parser drops three source constructs entirely — **`jizume` (字詰め)**,
**`yokogumi` (横組み)**, and **`keigakomi` (罫囲み)**. All three are **true drops**
(absent from the parser's entire emitted vocabulary), **not** measurement artifacts.

**The important finding:** the parser already *tokenizes* all three — it emits a
generic `containerOpen`/`containerClose` `raw` node **with the exact source string and
byte/line span preserved** — it simply never *classifies* them into typed nodes the way
it already does for `jisage_block`. So closing these gaps is a **classification task,
not a tokenization task**: low-risk, well-scoped, and a natural upstream contribution.

## Method / evidence

- **HEAD-parser vocabulary scan.** Fresh full-corpus scan of the re-pinned dump
  (`/db/ab-validator/aat-corpus/aozora-full-repin-1a4f864`, 17,886 works) over every
  `kind`, `style_type`, and `x-source-marker-kind`. No token containing `jizume`,
  `yokogumi`, `yoko`, `keigakomi`, `kakomi`, `keisen`, or `jitsuki` appears anywhere.
  (For contrast the parser *does* emit `chitsuki` 15,639 and `burasage` 12,386 as
  `style_type`s, `jisage_block` 44,021 as a `kind`, and `alignEnd` 2,722 as a marker —
  so the three below are singled-out gaps, not a blanket "no layout constructs.")
- **Official notation** verified against `https://www.aozora.gr.jp/annotation/etc.html`
  ("その他" section) — quoted verbatim below.
- **Corpus occurrences** from an independent Shift-JIS grep of the pinned corpus
  (`aozorabunko@0e9ea3e`, 17,603 readable works), counting the simple
  `［＃ここから…］` start form.
- **HEAD-parser disposition** inspected directly on works carrying each construct
  (e.g. `000034_55507` for jizume+yokogumi, `000096_2093`/`000067_1789` for keigakomi).

## The three gaps

| construct | official notation (start / end) | numeric param | source-authority denom¹ | corpus start-form² (works / occ) | parsers that represent it | HEAD-parser disposition |
| --- | --- | :---: | ---: | ---: | --- | --- |
| **jizume** 字詰め | `［＃ここから○字詰め］` / `［＃ここで字詰め終わり］` | yes (chars/line) | 3,239 | 230 / 1,373 | **aozora2 only** — `style_type: jizume` (1,212; rate 0.374, lossy) | `raw` + `containerOpen`/`containerClose`, source preserved, **untyped** |
| **yokogumi** 横組み | `［＃ここから横組み］` / `［＃ここで横組み終わり］` | no | 3,690 | 88 / 183 | aozora2 (`kind: yokogumi_block`, 170; 0.073), aozora2html (`style_type: yokogumi`, 1,367; 0.37), epub3 (`yokogumi_block`, 1) | `raw` + `containerOpen`/`containerClose`, source preserved, **untyped** |
| **keigakomi** 罫囲み | `［＃ここから罫囲み］` / `［＃ここで罫囲み終わり］` | no | **none (unscoreable)** | 106 / 200 | aozora-rs (`kind: keigakomi`, 200), aozora2html (`kind: keigakomi`, 420), epub3 (`kind: keigakomi_block`, 190) | `raw` + `containerOpen`/`containerClose`, source preserved, **untyped** |

¹ From `2026-07-08-corpus-adapter-fidelity.summary.json` (`classified`); keigakomi is in
`skipped` ("no keigakomi node observed in native blocks") — a gap in the *instrument*,
not just the parsers. See caveat below.
² Independent grep of the pinned nix corpus, simple `［＃ここから…］` start form only.

**Cross-validation:** the corpus has exactly **200** `罫囲み` start-annotations and
aozora-rs emits exactly **200** `keigakomi` nodes — an exact match that both validates
the grep and confirms aozora-rs represents keigakomi 1:1.

### What the HEAD parser actually emits (the key detail)

For `000034_55507` the parser preserves the annotations verbatim as generic container
markers, e.g.:

```json
{"kind": "raw", "source": "［＃ここから２３字詰め］", "x-source-marker-kind": "containerOpen",
 "span": {"byte_start": 70, "byte_end": 106, "line_start": 1, "line_end": 1}}
{"kind": "raw", "source": "［＃ここから横組み］",     "x-source-marker-kind": "containerOpen", ...}
{"kind": "raw", "source": "［＃ここで字詰め終わり］", "x-source-marker-kind": "containerClose", ...}
```

The parser has already done the hard part — it recognizes the scope boundary and keeps
the exact source text and span. The only missing step is a recognition rule mapping the
`source` string to a typed node (compare `jisage_block`, which *is* typed from its
`［＃ここからN字下げ］` container).

## Backlog items (prioritized)

**Parser (fork or upstream `P4suta/aozora`):** each item is "classify an existing
`containerOpen`/`containerClose` marker into a typed node," mirroring the existing
`jisage_block` path. All three preserve source+span already, so risk is low and each is
independently shippable.

1. **Type `jizume_block`** from `containerOpen source = ［＃ここからN字詰め］`, carrying the
   chars-per-line integer `N` as an attribute (and close on `［＃ここで字詰め終わり］`). Only
   aozora2 represents this today, so the fork gains sole Rust-side parity. **Watch for
   compound forms** — jizume co-occurs inside multi-clause containers such as
   `［＃ここから６字下げ、折り返して７字下げ、２１字詰め］` (rare: ~6 occ / 4 works, but real). The
   classifier must parse the compound container, not only the standalone form.
2. **Type `yokogumi_block`** from `［＃ここから横組み］` … `［＃ここで横組み終わり］`. Content is
   frequently Latin text or math (English quotations, `Ｘ＝人` formulae), so downstream
   IR/TEI mapping should expect non-vertical content inside. aozora2 and aozora2html
   model it; aozora-rs and epub3 also drop it.
3. **Type `keigakomi_block`** from `［＃ここから罫囲み］` … `［＃ここで罫囲み終わり］`. aozora-rs's
   `keigakomi` kind (200, exact corpus match) is the cleanest reference model.

**Instrument (source-authority denominators / study reproducibility):**

4. **Add a `keigakomi` source-authority denominator.** It is currently unscoreable
   (`skipped`), so no parser can be rate-scored on it — a gap in the measurement
   instrument, not only the parsers. First empirical figure to seed it: **106 works /
   200 start-annotations** on the pinned corpus. (Add `罫囲み` to the source-representability
   classifier / `build-inputs.json occ` path.)
5. **Audit the `yokogumi` denominator (3,690).** It is ~20× the observed start-form
   count (183) — far larger than any compound-form explanation — suggesting the local
   extraction counts a different unit (per affected line?) or over-matches. Resolve
   alongside handoff **§1** (full-nix denominator recompute); until then treat 3,690 as
   provisional. The jizume denominator (3,239 vs 1,373 observed) has the same root
   cause: denominators are from the *original local extraction*, not the pinned nix
   corpus (handoff §1).

## Scope / caveats

- **Small mass, real gap.** These three total well under ~0.5% of corpus construct
  mass — they do not move the study's §4.7 weighted ranking and were correctly out of
  the headline scope. They are the completeness frontier for a production parser, not a
  reason to revisit the fork choice.
- **Upstream-first is viable.** Because the parser already tokenizes and preserves
  source+span, items 1–3 are clean, self-contained additions well suited to upstream
  PRs against `P4suta/aozora` (verify against the same official docs cited here), with a
  fork fallback if not merged.
- **Notation verified 2026-07-09** against `aozora.gr.jp/annotation/etc.html`; corpus
  counts are the simple start form and are lower bounds (compound containers excluded).
