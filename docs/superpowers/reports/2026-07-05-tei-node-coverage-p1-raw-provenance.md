# TEI Node Coverage P1 Raw Provenance

This report records the raw-node measurement slice requested by
`/tmp/tei-node-coverage-gaps-report.md`.

## Scope

This slice is measurement only. It does not add a parser-IR raw node, TEI
`seg`, TEI `gap`, or converter recovery behavior. The purpose is to separate
source-derived raw text from parser residue before choosing a recovery policy.

The measurement is emitted by `ab-aat-to-parser-ir audit-corpus` as the
top-level `raw_nodes` summary block.

## Full Five-Parser Audit Result

Command:

```bash
just aat-to-parser-ir-full-audit 24
```

The full conversion audit still has the same conversion totals after adding
the measurement field:

| metric | value |
|---|---:|
| files_attempted | 89,169 |
| files_succeeded | 67,906 |
| files_failed | 21,263 |
| raw nodes | 4,800,218 |
| files with raw | 21,917 |
| fatal direct raw failures | 20,958 |

## Raw Nodes By Adapter

| adapter | raw nodes | files with raw | fatal direct raw failures |
|---|---:|---:|---:|
| aozora-adapter | 4,727,752 | 16,890 | 16,890 |
| aozora2-adapter | 69,507 | 4,768 | 4,068 |
| aozora2html-adapter | 2,959 | 259 | 0 |
| aozora-rs-adapter | 0 | 0 | 0 |
| aozora-epub3-adapter | 0 | 0 | 0 |

The legacy `aozora` adapter dominates raw volume and direct raw failures. For
the active TEI-generation comparison set, the recovery blocker is `aozora2`:
4,068 files still fail on direct `raw` nodes. `aozora2html` emits raw nodes, but
they are nonfatal in the current converter path.

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

- Source-derived raw nodes need preservation, likely as a parser-IR raw/source
  escape hatch that ABC can render as TEI `seg type="raw-source"` or a more
  specific node when classified.
- Parser-derived raw nodes should not be rendered as source text. Empty raw,
  parser tokens, and adapter HTML residue need either adapter fixes or a
  separate parser-residue diagnostic path.
- `aozora2html` `<br/>` raw nodes are nonfatal today and should be audited
  before any generic raw recovery turns them into body text.

## Next Work

1. Add a versioned parser-IR raw/source escape hatch or a narrower set of
   source-note/editor-note/page-break projections for the source-derived
   `aozora2` raw classes.
2. Keep parser-derived raw out of plaintext and TEI body text unless a later
   adapter-specific policy proves it is source-preserving.
3. Re-run the full audit after recovery and require `aozora2-adapter`
   `fatal_direct_failures == 0` before claiming the P1 raw crash blocker is
   closed.
