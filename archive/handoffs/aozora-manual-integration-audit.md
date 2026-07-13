# Aozora Manual Integration Audit

> Source of truth: `references/aozorabunko/aozora-manual/` (input/proofreading
> manuals), `references/aozorabunko/annotation/*.html` (13 per-construct spec
> pages), `references/parsers/AozoraEpub3-JDK21/chuki_tag.txt` (643-entry
> canonical marker→HTML table), and the real markup files in `cards/*/txt`
> (zipped SHIFT_JIS). This audit asks: how well do our schemas, AAT contract,
> and PARSER_REPORT integrate the prose spec and the real markup?

## 1. Three sources of truth, their sizes

| Source | Size | Role |
|---|---:|---|
| Manual `annotation/*.html` | 13 pages, 350 demonstrated markers (262 distinct after parametrization) | The prose spec — Aozora's own description of every 注記 |
| `chuki_tag.txt` (AozoraEpub3) | 643 marker rows | Canonical machine-readable marker→tag table; the de-facto parser reference |
| Real corpus `cards/*.txt` | 79 distinct markers (32 after parametrization) across a 4,000-file sample | What Aozora authors *actually* write |

## 2. Coverage at each layer (measured)

| Layer | Covered by next layer up |
|---|---|
| Real corpus constructs → Manual (parametrized) | **84%** (27 of 32 real-norm markers appear in the manual) |
| Real corpus constructs → chuki_tag.txt | **27%** (low because `chuki_tag.txt` uses bare names without `［＃…］`; the gap is mostly parametric gaiji/figure forms and `(*N)` 行右小書き refs, NOT missing constructs) |
| Manual → chuki_tag.txt | 43% (152 of 351) — `chuki_tag.txt` is the operational superset for many, but the manual documents semantics chuki_tag lacks) |

### The 5 real-corpus constructs NOT documented in the prose manual

1. `［＃「○○」、U+ＮBＮ、Ｎ-Ｎ］` — Unicode-codepoint gaiji form (manual covers JIS forms; the U+ form is a newer convention, documented only in `detail.html`)
2. `［＃「○○」の「○○」に代えて「○○」、第Ｎ水準…］` — nested substitution gaiji
3. `［＃図（figＮ_Ｎ.png、横Ｎ×縦Ｎ）入る］` — figure with explicit dimensions (manual's `graphics.html` shows figures but not this parametric shape)
4. `［＃（ツ）］`, `［＃（フ）］` — 返り点 (kunten) markers. These ARE documented but in the `返り点` reference list (one-kanji 一二三四上中下 + kana ツ/フ/ノ…), not as `［＃…］` annotation form in `annotation/kunten.html`. **Real authors use the bare `(ツ)` form.**

## 3. Where our schemas integrate this — and where they don't

| Manual construct family | Manual markers | Needs AAT node kind | Covered in AAT v1? |
|---|---:|---|---|
| Layout (レイアウト) | 24 | paragraph, heading, jisage_block, keigakomi_block, quote_block, yokogumi_block, caption_block | ✅ all present |
| Headings (見出し) | 38 | heading | ✅ |
| Gaiji (外字) | 6 | gaiji | ✅ |
| Emphasis (強調: 傍点/傍線/太字/斜体) | 96 | style (style_type: boten/bousen/...) | ✅ |
| Graphics (画像・キャプション) | 11 | figure, caption, caption_block | ✅ |
| Other (縦中横/上下付/横組み etc.) | 85 | tcy, font_size, raw, yokogumi, keigakomi | ✅ |
| Duplication (重複 rules) | 6 | — (markup nesting rule, not a node kind) | N/A — correctly modeled as parser behavior, not a kind |
| **Kunten (訓点)** | **24** | **— NONE** | ❌ **AAT v1 has no kunten node kind; real corpus emits `［＃（ツ）］` / `［＃（フ）］` / `［＃二］`** |

## 4. The integration gap (the real finding)

Three distinct, separable gaps:

### Gap A — PARSER_REPORT.md extracts but does not map

`references/PARSER_REPORT.md` (407 lines) does the hard work of deriving a
**278-feature taxonomy from `annotation/*.html` + `chuki_tag.txt`** and
produces a parser-completeness scorecard (sections 1-6) per parser. But
**it does not map any of those 278 features to AAT or parser-IR node kinds**
(confirmed: `grep -E 'aat|parser-ir|node kind|ab-ir' PARSER_REPORT.md` → 0 hits).
So the manual-to-schema *bridge* exists only in the heads of maintainers.

**Severity:** Strong suggestion. The taxonomy is the right artifact; it just
stops one step short of being a coverage matrix against our schemas.

### Gap B — Kunten has no schema home

AAT v1 enumerates 20 node kinds; none represents 返り点 / 訓点送り仮名 / 再読文字.
Yet the manual's `annotation/kunten.html` documents 24 kunten markers and the
real corpus emits them (`［＃（ツ）］`, `［＃二］`, `［＃レ］` in the sample —
the kanji/kana 返り点 notation for classical Chinese reading order).

`src/abc/annotation.clj` is the legacy parser that *does* handle kunten
internally (the `:annotation/type` registry includes kunten-shaped entries),
but the **modern AAT JSON contract has no `kunten` node**. So real works
using classical-text 返り点 (a small but non-zero fraction) would currently
either fail to map or lose the construct.

**Severity:** Follow-up. Real-corpus frequency is low (the sample showed kunten
markers, but they're confined to classical-text works). But it's the one
construct family documented in the manual, used in real data, and entirely
absent from the AAT v1 schema. Should be noted in the owned-mapping spec
as a known v1 vocabulary gap alongside warigaki.

### Gap C — Manual prose is uncited in canonical RFC/schemas

The architecture note, ADRs, and AAT schema cite the manual **zero** times
(confirmed: `grep -rE 'aozora-manual|annotation/|detail.html' docs/ schemas/`
in abc → only `references/PARSER_REPORT.md` and `src/abc/annotation.clj:126`,
which links to `annotation/etc.html`). The manual is the *source of truth*
for Aozora markup but is treated as external folklore, not a normative
reference. PARSER_REPORT is the only bridge.

**Severity:** Question. Citing an external website you don't control is a real
maintenance liability (the manual moves; CHI conventions shift). But the
*Aozora-specific design concerns* section of the architecture note already
lists gaiji, ruby scope, editor-note taxonomy, etc. — it just doesn't cite
*where* those categories come from. A short "Source: annotation/X.html" line
per policy would make the provenance auditable.

## 5. Net assessment

**We integrate the manual reasonably well, but indirectly.** The integration
is not absent — it's *latent*: `PARSER_REPORT.md` derives the full 278-feature
taxonomy from the manual, the AAT schema's 20 node kinds cover 7 of 8 manual
families, and 84% of real-corpus constructs after parametrization are
documented in the manual. The gap is that **this derivation is one-shot and
uncited**: there's no living link from the schema's `gaiji` node kind back to
`annotation/external_character.html`, no coverage matrix showing which of the
278 manual features each AAT kind is intended to model, and no test that
catches the one real gap (Kunten).

## 6. Concrete next steps (cheap to valuable)

| # | Action | Effort | Leverage |
|---|---|---|---|
| 1 | Add a **manual-coverage matrix** to `references/PARSER_REPORT.md` mapping each of the 8 construct families (and the 278 features) to AAT/parser-IR node kinds, with `Source: annotation/<page>.html` citations | ~half day | Converts PARSER_REPORT from a parser scorecard into a schema-coverage artifact; closes Gap A |
| 2 | Add `kunten` to the **owned-mapping-design.md** deferred-decisions list as a known v1 vocabulary gap (alongside warigaki); record that real corpus emits it | ~15 min | Closes Gap B at the doc level; surfaces it for a later AAT v2 |
| 3 | Add one **provenance footnote** per item in the architecture-note "Aozora-Specific Design Concerns" section pointing to its `annotation/*.html` source | ~30 min | Closes Gap C without making the manual normative; makes provenance auditable |
| 4 | Add a **regression fixture** for one classical-text work (a real kunten-bearing file from `cards/`) to the ab-validator fixtures, asserting the mapper records the loss rather than silently dropping it | ~1 hr | Turns Gap B from a doc note into a test-enforced invariant |

The cheapest high-leverage move is #2 (surfacing Kunten) — a few minutes that
prevents the owned-mapping design from shipping a v1 that silently loses
classical-text constructs. #1 is the highest-leverage substantive work but can
wait until the owned-mapping implementation lands.

---

*Audit run 2026-07-02. Probes: `prototypes/` style, ad hoc. Sources:
`references/aozorabunko/{aozora-manual,annotation}/*.html` (UTF-8, readable
directly — earlier mojibake was a Python-decoder bug, not an encoding issue),
`references/parsers/AozoraEpub3-JDK21/chuki_tag.txt` (643 rows), and real
zipped-SHIFT_JIS `cards/*.txt` across a 4,000-file sample.*
