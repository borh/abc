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

## Current Remaining Conversion Failures

The remaining 471 conversion failures are all in `aozora2-adapter` and are no
longer raw-related:

| error | files |
|---|---:|
| unsupported inline kind: accent | 261 |
| unsupported inline kind in source attribution projection: accent | 140 |
| unsupported inline kind in visible projection: accent | 52 |
| unsupported inline kind: yokogumi | 11 |
| unsupported inline kind in source attribution projection: yokogumi | 5 |
| unsupported inline kind in visible projection: yokogumi | 2 |

## Goal Incorporation

`/tmp/tei-node-coverage-gaps-report.md` is now part of the active "Full TEI 2/3
generation via IR" goal. The raw crash blocker is closed, but the goal remains
open until the remaining TEI node-coverage gaps are either represented in
parser-IR or explicitly admitted as policy-scoped Level 4/enrichment work.

The incorporated backlog is:

1. P1: measure emphasis nesting depth, then migrate parser-IR `emphasis` to
   support inline children. This resolves nested emphasis identity and ruby
   reading loss inside emphasis.
2. P2: measure accent code taxonomy, then add a converter/schema/rendering
   policy for `accent` using TEI `<hi>` with CSS `text-emphasis-style` as the
   Level 3 default.
3. P2: add first-class `warigaki` representation and render it as
   `<note place="inline" rend="割注">` with `<lb/>` split, following the
   jpn_classical guideline.
4. P2: align `gaiji.resolved` with source semantics by accepting string/null
   alongside the legacy boolean during schema migration.
5. P3: define unified `@rend` / `<rendition>` vocabulary for inline containers,
   including TCY and inline yokogumi, and recover the remaining `yokogumi`
   conversion failures.
6. P3: decide block-container TEI mapping for keigakomi/yokogumi/jisage mixed
   cases, defaulting to `<div type="...">` where the structure is block-level.
7. P3: keep caption/quote pass-through measured, and only add semantic
   `<figure><head>`, `<quote>`, or `<cit>` mappings after adapter evidence
   exposes enough structure.

## Next Work

1. Commit this raw recovery slice after verification.
2. Start the emphasis inline-children measurement/schema plan; it is the
   highest-leverage remaining Parser IR gap by occurrence count.
3. In parallel, prepare the accent taxonomy measurement because it is now the
   dominant remaining conversion failure class.
