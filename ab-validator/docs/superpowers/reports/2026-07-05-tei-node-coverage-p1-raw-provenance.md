# TEI Node Coverage P1 Raw Provenance

This report records the raw-node measurement slice requested by
`/tmp/tei-node-coverage-gaps-report.md`.

## Scope

This slice started as measurement and now records the first recovery policy for
direct `raw` AAT nodes. It does not add a parser-IR raw node, TEI `seg`, or TEI
`gap`. Instead it uses existing parser-IR nodes for the narrow recoverable
classes and keeps parser residue out of body text.

The measurement is emitted by `ab-aat-to-parser-ir audit-corpus` as the
top-level `raw_nodes` summary block.

## Full Five-Parser Audit Result

Command:

```bash
just aat-to-parser-ir-full-audit 24
```

The first measurement-only run established the pre-recovery baseline:

| metric | value |
|---|---:|
| files_attempted | 89,169 |
| files_succeeded | 67,906 |
| files_failed | 21,263 |
| raw nodes | 4,800,218 |
| files with raw | 21,917 |
| fatal direct raw failures | 20,958 |

After direct raw recovery, the regenerated five-parser audit is:

| metric | value |
|---|---:|
| files_attempted | 89,169 |
| files_succeeded | 88,698 |
| files_failed | 471 |
| raw nodes | 4,800,218 |
| files with raw | 21,917 |
| fatal direct raw failures | 0 |

After accent and inline-yokogumi recovery, the regenerated five-parser audit is:

| metric | value |
|---|---:|
| files_attempted | 89,169 |
| files_succeeded | 89,169 |
| files_failed | 0 |
| raw nodes | 4,800,218 |
| files with raw | 21,917 |
| fatal direct raw failures | 0 |

## Raw Nodes By Adapter

| adapter | raw nodes | files with raw | baseline fatal direct failures | post-recovery fatal direct failures |
|---|---:|---:|---:|---:|
| aozora-adapter | 4,727,752 | 16,890 | 16,890 | 0 |
| aozora2-adapter | 69,507 | 4,768 | 4,068 | 0 |
| aozora2html-adapter | 2,959 | 259 | 0 | 0 |
| aozora-rs-adapter | 0 | 0 | 0 | 0 |
| aozora-epub3-adapter | 0 | 0 | 0 | 0 |

The legacy `aozora` adapter still dominates raw volume, but direct `raw` nodes
are no longer fatal for any adapter. For the active TEI-generation comparison
set, the previous `aozora2` blocker is closed: `aozora2-adapter`
`fatal_direct_failures == 0`.

## Provenance Split

| inferred provenance | raw nodes |
|---|---:|
| parser-derived | 4,282,492 |
| source-derived | 517,726 |

The inferred provenance uses explicit `x-provenance` when present. Otherwise it
treats empty raw strings, parser tokens such as `BlockStart(...)`, and HTML
fragments such as `<br/>` as parser-derived; nonempty Aozora/source text is
treated as source-derived.

## Source Classes

| source class | raw nodes |
|---|---:|
| empty | 2,989,698 |
| text | 1,550,593 |
| aozora-marker | 195,397 |
| parser-token | 51,093 |
| editorial-note | 8,591 |
| html-fragment | 2,960 |
| aozora-command | 1,886 |

## Recovery Implication

The measurement confirms the report's warning: raw recovery cannot be a single
`gap` policy.

- Source-derived `改頁` / `改ページ` raw nodes are recovered as parser-IR
  `page-break` nodes.
- Other non-residue source-derived raw nodes are recovered as parser-IR
  `editor-note` nodes with `note.category = "misc"`.
- Parser-derived raw nodes are recorded as `UNSUPPORTED` divergence but emit no
  visible parser-IR node. Empty raw, parser tokens, and adapter HTML residue do
  not become TEI body text or plaintext.
- `aozora2html` `<br/>` raw nodes remain nonfatal and are still treated as
  parser residue by the direct raw recovery policy.

## Closed Conversion Failures

After direct raw recovery, 471 conversion failures remained in
`aozora2-adapter`:

| error | files |
|---|---:|
| unsupported inline kind: accent | 261 |
| unsupported inline kind in source attribution projection: accent | 140 |
| unsupported inline kind in visible projection: accent | 52 |
| unsupported inline kind: yokogumi | 11 |
| unsupported inline kind in source attribution projection: yokogumi | 5 |
| unsupported inline kind in visible projection: yokogumi | 2 |

Those are now recovered by measured v1 mapping rules:

- Direct `accent` nodes emit parser-IR `emphasis` with `text = resolved` and
  `style = code`, while recording the measured `AMBIGUITY` / `INVENTION` /
  `LOSS` rules for accent structure, code, and name.
- `accent` inside visible-text projections returns the resolved character and
  records the measured `LOSS` rule where the projection target has no accent
  structure.
- Inline `yokogumi` uses the existing generic inline-container recovery path:
  parser-IR `emphasis` with `style = "yokogumi"` and measured `UNSUPPORTED`
  divergence.
- Source-attribution projection and visible projection now handle both kinds,
  so neither path can still fail on these observed inline nodes.

The conversion gate is therefore closed: `top_errors == []` and all five
adapters have `files_failed == 0` in the regenerated audit.

## Goal Incorporation

`/tmp/tei-node-coverage-gaps-report.md` is now part of the active "Full TEI 2/3
generation via IR" goal. The fatal conversion blockers are closed, but the goal
remains open until the remaining TEI node-coverage gaps are either represented
in parser-IR or explicitly admitted as policy-scoped Level 4/enrichment work.

The incorporated backlog is:

1. P1: measure emphasis nesting depth, then migrate parser-IR `emphasis` to
   support inline children. This resolves nested emphasis identity and ruby
   reading loss inside emphasis.
2. P2: measure accent code taxonomy, then replace free-form accent codes in
   `emphasis.style` with a documented converter/schema/rendering policy using
   TEI `<hi>` with CSS `text-emphasis-style` as the Level 3 default.
3. P2: add first-class `warigaki` representation and render it as
   `<note place="inline" rend="割注">` with `<lb/>` split, following the
   jpn_classical guideline.
4. P2: align `gaiji.resolved` with source semantics by accepting string/null
   alongside the legacy boolean during schema migration.
5. P3: define unified `@rend` / `<rendition>` vocabulary for inline containers,
   including TCY and inline yokogumi. Conversion recovery is done; TEI
   vocabulary and ODD declaration are still open.
6. P3: decide block-container TEI mapping for keigakomi/yokogumi/jisage mixed
   cases, defaulting to `<div type="...">` where the structure is block-level.
7. P3: keep caption/quote pass-through measured, and only add semantic
   `<figure><head>`, `<quote>`, or `<cit>` mappings after adapter evidence
   exposes enough structure.

## Next Work

1. Treat the emphasis inline-children migration as closed by
   `docs/superpowers/reports/2026-07-05-tei-node-coverage-emphasis-nesting.md`:
   the five-parser audit now converts 89,169 AAT files with 0 failures while
   preserving nested ruby/emphasis structure in parser-IR.
2. Prepare the accent taxonomy measurement as the next semantic recovery step;
   conversion no longer fails, but the current `style = code` projection is a
   measured compatibility bridge, not the final TEI vocabulary.
3. Add first-class `warigaki`, `raw-source`, and gaiji resolved-value schema
   deltas before claiming full generated TEI Level 2/3 coverage. The `/tmp`
   node-coverage gap report is the active backlog source for those remaining
   parser-IR representability gaps.
