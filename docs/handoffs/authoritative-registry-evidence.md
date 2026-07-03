# Authoritative Registry Evidence (scout, recovered)

> The scout subagent lacked a write tool and did not save its file. The
> evidence it was gathering is recovered here by the orchestrator (the
> prototype was the linchpin). Source: direct probes 2026-07-02.

## 1. annotation/*.html parseability — THE DECISIVE FINDING

**Mechanical extraction of `(marker, family, sub-family)` is reliable.**
Every page has clean `<h1>/<h2>/<h3>` headings carrying the sub-family
taxonomy, and `［＃...］` markers sit in the prose between them. Verified
across all 13 pages:

| Page | Markers | Headings (machine-extractable sub-families) |
|---|---:|---|
| emphasis.html | 113 | 強調 / 傍点 / 傍線 / 太字・斜体 |
| etc.html | 175 | その他 / 訂正・ママ / ルビ / 縦中横 / 割り注 / 行右小書き / 字詰め / 罫囲み / 横組み / 文字サイズ |
| heading.html | 64 | 見出し / 通常 / 同行 / 窓 / 形式とレベル / 目次 |
| kunten.html | 44 | 訓点 / 返り点 / 訓点送り仮名 / 混在 / 再読文字 |
| layout_1/2/3 | 130 | レイアウト / ページ段 / 字下げ / 左右中央 |
| external_character.html | 8 | 外字 / 第1第2水準なし / 特殊仮名 / アクセント |
| graphics.html | 15 | 画像とキャプション / 写真図版 / キャプション |
| duplication.html | 10 | 注記の重複 |
| extra.html | 16 | 本文終わり / 空白ページ / 使えなかった文字 |
| henkoten.html | 31 | 変更点 (historical — deprecation signals!) |
| index.html | 4 | **lists all top-level families** (the registry's family enum) |

So `index.html` provides the family enum, each per-family page provides the
sub-family tree via headings, and markers are mechanically extractable. The
semantics description is harder (it's prose between markers — needs LLM or
human for faithful summary) but the `(marker, family, sub-family, source_page)`
tuple is mechanically trustworthy.

## 2. detail.html — the comprehensive reference

`file --mime-encoding` reports `unknown-8bit`; it is **not UTF-8** (a prior
probe got 0 markers because of this). Needs encoding detection before use.
Likely SHIFT_JIS or CP932. **Defer**: per-family `annotation/*.html` pages are
sufficient and already machine-parseable; `detail.html` is a cross-reference
at most, not a primary source.

## 3. Real-corpus extraction — diminishing returns confirmed

A 4,000-file zipped-SHIFT_JIS sample yields 32 normalized markers (after
parametric normalization); 8,000 files yield the same set. The OBSERVED set
is stable at ~4,000 files. Extraction of 4,000 files takes ~1-2 minutes.
**Recommendation:** a periodically-refreshed fixture (not per-build) — the
OBSERVED tier changes slowly.

## 4. AozoraEpub3 license (chuki_tag.txt)

`references/parsers/AozoraEpub3-JDK21/LICENSE.txt` — AozoraEpub3 is licensed
software. `chuki_tag.txt` is a tabular **expression** of facts (which markers
Aozora defines). Facts aren't copyrightable; the specific tabular form + the
tag→HTML mappings ARE. So ABC may use `chuki_tag.txt` as a comparison/check
(diff our derived set against theirs for coverage analysis → records a
boolean `aozoraepub3_handles`), but may NOT transcribe its rows or mappings
into an ABC-owned registry. The license boundary is clean if we derive from
authoritative sources only.

## 5. Existing abc data-format conventions

- `schemas/*.schema.json` — JSON Schema Draft 2020-12 (the dominant pattern)
- `contexts/abc-v0.jsonld` — JSON-LD context
- `data/aat-parser-ir-compatibility.edn` — EDN registry (proposed in
  owned-mapping-design.md)
- The `registries/` subdirectory pattern is NOT yet established — introducing
  it would be a new convention (proposed in the design spec).

**Lean:** JSON Schema + JSON file under `abc/schemas/` + `abc/data/` to match
the dominant `schemas/*.json` pattern; the registry-of-registries
subdirectory is premature unless a 2nd registry lands.

## 6. Existing marker-registry attempts

Only two: the legacy `src/abc/annotation.clj` (dormant, line 126 links to
`annotation/etc.html`) and `references/PARSER_REPORT.md` (the drifted
hand-written taxonomy). No existing ABC-owned machine-readable registry.

## Net for the design

The linchpin unknown is resolved: **mechanical extraction is viable.** The
registry can be **generated** (not curated) for the `(marker, family,
sub-family, source)` tuple; only the prose `description` field needs
human/LLM curation, and that field can be left `NEEDS_REVIEW` initially. This
makes the registry a generated artifact with drift detection, exactly the
pattern that worked for `tei-profile.rng` and ADR 0001.
