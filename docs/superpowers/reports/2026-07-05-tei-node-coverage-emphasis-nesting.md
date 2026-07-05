# TEI Node Coverage Emphasis Nesting Measurement

This report records the measurement requested by
`/tmp/tei-node-coverage-gaps-report.md` before changing parser-IR `emphasis`
from flattened text to inline children.

## Scope

This is measurement only. It does not change the parser-IR schema and does not
add `inline_children` yet. The purpose is to quantify how often AAT inline
containers contain semantic inline nodes that current parser-IR `emphasis.text`
cannot preserve.

Measured container kinds:

- `style`
- `font_size`
- `tcy`
- `keigakomi`
- `caption`
- `yokogumi`
- `accent`

Measured semantic inline kinds inside containers:

- `ruby`
- `gaiji`
- `warigaki`
- `figure`
- `raw`
- `accent`

## Inputs

The measurement uses the same five adapter corpora as
`just aat-to-parser-ir-full-audit 24`:

| adapter | AAT root |
|---|---|
| aozora | `/db/ab-validator/aat-corpus/aozora-full-20260705T015007Z/aat/aozora-adapter` |
| aozora2 | `/db/ab-validator/aat-corpus/aozora2-full-20260705T083650Z-layout-fix5/aat/aozora2-adapter` |
| aozora2html | `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter` |
| aozora-rs | `/home/bor/Projects/ab-validator/scratch/morph-full-corpus/aats/aozora-rs-adapter` |
| aozora-epub3 | `/db/ab-validator/aat-corpus/aozora-epub3-full-20260704T050652Z-300s/aat/aozora-epub3-adapter` |

The generated measurement JSON was written to
`/tmp/tei-inline-container-nesting-measurement.json` during the run and is not
committed.

## Summary

| metric | value |
|---|---:|
| files scanned | 89,169 |
| inline container nodes | 836,634 |
| nested container nodes | 118,300 |
| semantic nodes inside containers | 1,733,601 |
| ruby nodes inside containers | 1,665,281 |
| max container depth | 244 |

The key result is that ruby inside emphasis/container contexts is not a rare
edge case. It appears in four adapters and accounts for most semantic nodes
inside containers. Flattening these containers to `emphasis.text` necessarily
drops ruby reading structure.

## Adapter Breakdown

| adapter | files | files with containers | container nodes | nested container nodes | semantic nodes inside containers | ruby inside | gaiji inside | warigaki inside | accent inside | raw inside | max depth |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| aozora | 17,886 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| aozora2 | 17,856 | 11,842 | 289,334 | 106,363 | 1,682,612 | 1,620,665 | 27,040 | 1,256 | 2,269 | 28,996 | 244 |
| aozora2html | 17,689 | 12,310 | 353,877 | 11,882 | 44,903 | 38,539 | 3,306 | 198 | 0 | 2,837 | 4 |
| aozora-rs | 17,894 | 5,471 | 28,492 | 0 | 5,673 | 5,664 | 9 | 0 | 0 | 0 | 0 |
| aozora-epub3 | 17,844 | 6,850 | 164,931 | 55 | 413 | 413 | 0 | 0 | 0 | 0 | 1 |

## Samples

Representative semantic nodes inside containers:

| adapter | path | pointer | kind | depth |
|---|---|---|---|---:|
| aozora2 | `000042_42227-66e4c2fa11f2.json` | `blocks[0].content[0].content[1]` | ruby | 1 |
| aozora2html | `000160_2637-c5c458bd3c35.json` | `blocks[223].content[1].content[1]` | ruby | 1 |
| aozora-rs | `000074_425-f04d408824dc.json` | `blocks[1].content[0].content[6]` | ruby | 1 |
| aozora-epub3 | `000040_380-8fdec5345092.json` | `blocks[23].content[1].content[0]` | ruby | 1 |

Representative nested containers:

| adapter | path | pointer | kind | depth |
|---|---|---|---|---:|
| aozora2 | `000042_42227-66e4c2fa11f2.json` | `blocks[0].content[0].content[3]` | tcy | 1 |
| aozora2html | `000106_55787-3d9d0da2e2ee.json` | `blocks[24].content[1].content[0]` | font_size | 1 |
| aozora-epub3 | `000168_59514-9abb1214e964.json` | `blocks[0].children[0].content[0].content[0]` | tcy | 1 |

## Interpretation

This measurement supports the schema direction in the gap report:

1. Parser-IR `emphasis` needs inline children. A single string field cannot
   preserve ruby, gaiji, warigaki, accent, raw, or figure nodes inside container
   contexts.
2. The migration must be versioned. The current `text` field is still needed
   for legacy parser-IR documents and for consumers that have not yet adopted
   recursive inline rendering.
3. Recursive rendering needs a depth policy. Most adapters have shallow
   container nesting, but `aozora2` currently exposes a maximum depth of 244.
   That depth is likely a parser artifact, but the schema and renderer still
   need an explicit recursion guard or normalization policy.
4. Ruby preservation is the highest-value first acceptance case. It is the
   dominant semantic node inside containers and directly affects TEI `<ruby
   type="furigana">` output.

## Next Work

1. Draft the parser-IR `emphasis.inline_children` schema migration with a
   transitional shape that accepts both `text` and `inline_children`.
2. Add renderer tests in ABC for nested `<hi>` and `<hi><ruby
   type="furigana">...</ruby></hi>`.
3. Add converter tests in ab-validator showing that style/font-size/yokogumi
   containers preserve inline child nodes rather than flattening to
   `emphasis.text`.
4. Include a recursion-depth guard or normalizer in the implementation plan
   before accepting deeply nested `aozora2` fixtures as-is.
