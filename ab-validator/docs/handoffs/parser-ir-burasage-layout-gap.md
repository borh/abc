# Parser-IR Layout Style Gap

Date: 2026-07-05

## Current Status

The `aozora` adapter now preserves Aozora hanging-indent and chitsuki/right-end
alignment markers as typed AAT layout intent:

```json
{
  "kind": "style",
  "style_type": "burasage",
  "x-indent-first": 0,
  "x-indent-rest": 1,
  "content": [{ "kind": "text", "value": "..." }],
  "x-provenance": "source-derived"
}
```

```json
{
  "kind": "style",
  "style_type": "chitsuki",
  "x-align": "right",
  "x-offset": 1,
  "content": [{ "kind": "text", "value": "..." }],
  "x-provenance": "source-derived"
}
```

This follows the existing source-representability row
`indentation.burasage` and `indentation.chitsuki`, whose AAT representation is
`style` and whose TEI projection is `p rend=burasage` / `p rend=chitsuki`.

## Measured Adapter Effect

On `references/aozorabunko/cards/000879/files/1126_ruby_14122.zip`, after the
heading, simple jisage, burasage, and chitsuki adapter slices, the raw marker
bucket is empty:

```json
[]
```

The 13 occurrences of:

```text
［＃ここから改行天付き、折り返して１字下げ］
```

now emit typed AAT `style_type = "burasage"` nodes.

The final occurrence of:

```text
［＃地から１字上げ］
```

now emits a typed AAT `style_type = "chitsuki"` node with `x-offset = 1`.

## Parser-IR Gap

`ab-aat-to-parser-ir` currently maps all AAT `style` inline containers to
Parser-IR `emphasis`. Conversion probes for the new AAT shapes yield:

```json
[{ "type": "emphasis", "text": "台詞", "style": "burasage" }]
```

```json
[{ "type": "emphasis", "text": "（大正十一年十二月）\n", "style": "chitsuki" }]
```

That preserves visible text, but it is not honest Level 3 layout IR. The current
Parser-IR `indentation` node has only:

```json
{ "type": "indentation", "depth": 1, "text": null }
```

It cannot represent the hanging-indent pair
`x-indent-first` / `x-indent-rest` without overloading `depth` and losing one
side of the pair, and it cannot represent chitsuki/right-end alignment offset
without overloading `depth` with a different coordinate meaning.

## Recommended Next Step

Do not treat `style_type = "burasage"` or `style_type = "chitsuki"` as
TEI-ready Parser-IR until the Parser-IR indentation/layout vocabulary is
extended.

Recommended ABC-owned schema delta:

- keep existing `indentation.depth` for backward compatibility,
- add optional `mode`, with at least `block`, `line`, `hanging`, `right-align`,
- add optional `first_line_depth` and `continuation_depth`,
- add optional `offset_from_end` for chitsuki/right-end alignment,
- render `mode = "hanging"` to TEI with explicit `rend` metadata,
- render `mode = "right-align"` to TEI with explicit `rend` metadata,
- keep plaintext projection as content-only.

After ABC publishes the new parser-IR schema hash, regenerate the
AAT-to-Parser-IR mapping and route AAT `style_type = "burasage"` to an
`indentation` or layout node instead of `emphasis`. Do the same for
`style_type = "chitsuki"`.
