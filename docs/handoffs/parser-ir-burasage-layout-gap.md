# Parser-IR Burasage Layout Gap

Date: 2026-07-05

## Current Status

The `aozora` adapter now preserves Aozora hanging-indent markers as typed AAT
layout intent:

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

This follows the existing source-representability row
`indentation.burasage`, whose AAT representation is `style` and whose TEI
projection is `p rend=burasage`.

## Measured Adapter Effect

On `references/aozorabunko/cards/000879/files/1126_ruby_14122.zip`, after the
heading, simple jisage, and burasage adapter slices, the remaining raw marker
bucket is:

```json
[{ "source": "［＃地から１字上げ］", "count": 1 }]
```

The 13 occurrences of:

```text
［＃ここから改行天付き、折り返して１字下げ］
```

now emit typed AAT `style_type = "burasage"` nodes.

## Parser-IR Gap

`ab-aat-to-parser-ir` currently maps all AAT `style` inline containers to
Parser-IR `emphasis`. A conversion probe for the new AAT shape yields:

```json
[{ "type": "emphasis", "text": "台詞", "style": "burasage" }]
```

That preserves visible text, but it is not honest Level 3 layout IR. The current
Parser-IR `indentation` node has only:

```json
{ "type": "indentation", "depth": 1, "text": null }
```

It cannot represent the hanging-indent pair
`x-indent-first` / `x-indent-rest` without overloading `depth` and losing one
side of the pair.

## Recommended Next Step

Do not treat `style_type = "burasage"` as TEI-ready Parser-IR until the
Parser-IR indentation vocabulary is extended.

Recommended ABC-owned schema delta:

- keep existing `indentation.depth` for backward compatibility,
- add optional `mode`, with at least `block`, `line`, `hanging`, `right-align`,
- add optional `first_line_depth` and `continuation_depth`,
- render `mode = "hanging"` to TEI with explicit `rend` metadata,
- keep plaintext projection as content-only.

After ABC publishes the new parser-IR schema hash, regenerate the
AAT-to-Parser-IR mapping and route AAT `style_type = "burasage"` to an
`indentation` node instead of `emphasis`.
