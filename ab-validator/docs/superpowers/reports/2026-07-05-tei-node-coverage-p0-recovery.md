# TEI Node Coverage P0 Recovery

This report records the first TEI node-coverage recovery slice after
`/tmp/tei-node-coverage-gaps-report.md`.

## Scope

The slice intentionally does not add final TEI semantics for captions or
quotations. It only removes fatal conversion behavior for block containers
whose children can still preserve visible content.

- `caption_block`: recover children and record the existing measured
  `STRUCTURAL` rule `S-01`.
- `quote_block`: recover children without emitting a divergence record, because
  the checked-in mapping artifact has no measured `quote_block` rule yet.

This keeps the AAT to parser-IR mapping guard intact: no unmeasured divergence
rule is invented at runtime.

## Five-Parser AAT Scan

Command shape:

```bash
rg -l '"kind"[[:space:]]*:[[:space:]]*"quote_block"' <aat-dir>
rg -o '"kind"[[:space:]]*:[[:space:]]*"caption_block"' <aat-dir>
```

| adapter | quote_block files | quote_block nodes | caption_block files | caption_block nodes |
|---|---:|---:|---:|---:|
| aozora-rs | 0 | 0 | 0 | 0 |
| aozora2 | 0 | 0 | 26 | 288 |
| aozora2html | 0 | 0 | 0 | 0 |
| aozora-epub3 | 0 | 0 | 0 | 0 |
| aozora | 0 | 0 | 0 | 0 |

Source-authority evidence still reports `structure.quote_block` in 6 works and
21 source occurrences. The gap is therefore adapter/AAT recognition, not source
absence: current AAT corpora do not expose `quote_block` nodes even though the
source inventory detects quote-block markers.

## Full Conversion Audit Delta

The five-adapter full conversion audit was regenerated after recovery:

| metric | before | after |
|---|---:|---:|
| files_attempted | 89,169 | 89,169 |
| files_succeeded | 67,904 | 67,906 |
| files_failed | 21,265 | 21,263 |
| parser_ir_nodes | 28,890,539 | 28,890,549 |
| divergence_records | 804,766 | 804,793 |
| divergence_occurrences | 46,686,087 | 46,686,120 |

The previous top error
`unsupported block kind without measured v1 divergence rule: caption_block`
dropped out of `top_errors`. Four former caption-block failures now expose
deeper child failures, primarily `raw`, which is the next P1 recovery target.

## Next Work

- Measure raw-source provenance and add raw recovery without mapping present
  source text to TEI `<gap>`.
- Add ruby `type="furigana"` in the ABC TEI renderer with a documented heuristic
  caveat.
- Add a measured `quote_block` rule only after an adapter emits `quote_block`
  AAT nodes in corpus evidence.
