# Parser-IR Emphasis Inline Children Design

## Status

Draft for review. This design is the next schema-level step toward full TEI
Level 2/3 generation via parser-IR.

## Problem

Parser-IR currently represents emphasis and inline-container projections as:

```json
{
  "type": "emphasis",
  "text": "...",
  "style": "..."
}
```

That shape cannot preserve phrase-level structure inside highlighted or layout
containers. When AAT contains ruby, gaiji, warigaki, accent, raw/source notes,
or nested containers inside a style/font-size/tcy/yokogumi container,
ab-validator must flatten the child structure to `emphasis.text`. ABC can then
render `<hi>text</hi>`, but it cannot render nested TEI such as:

```xml
<hi rend="bold">
  <ruby type="furigana"><rb>東京</rb><rt>とうきょう</rt></ruby>
</hi>
```

This is not a rare edge case. The five-parser measurement in
`docs/superpowers/reports/2026-07-05-tei-node-coverage-emphasis-nesting.md`
found:

| metric | value |
|---|---:|
| files scanned | 89,169 |
| inline container nodes | 836,634 |
| nested container nodes | 118,300 |
| semantic nodes inside containers | 1,733,601 |
| ruby nodes inside containers | 1,665,281 |
| max container depth | 244 |

The goal is to preserve this structure through parser-IR so ABC can produce
TEI P5 phrase-level markup rather than flattening to body text.

## Design Goals

1. Preserve inline child structure inside parser-IR emphasis/container nodes.
2. Keep old parser-IR documents valid during migration.
3. Keep plaintext free of ruby readings and other metadata.
4. Keep ABC as the owner of parser-IR schema and TEI rendering.
5. Keep ab-validator as the producer of measured parser-IR evidence and
   divergence sidecars.
6. Make schema hash rotation explicit and compatible with existing registry
   behavior.

## Non-Goals

- This does not add final TEI vocabulary for every style value.
- This does not solve warigaki as first-class parser-IR.
- This does not solve gaiji `resolved` string/null migration.
- This does not remove the legacy `emphasis.text` field immediately.
- This does not normalize all `aozora2` deep container artifacts in the same
  slice; it defines the guard needed before recursive rendering.

## Schema Change

ABC should extend `schemas/parser-ir.schema.json` with a recursive inline-child
definition and update `emphasisNode`.

The transitional `emphasis` node accepts both legacy text and structured
children:

```json
{
  "type": "emphasis",
  "span": { "start": 0, "end": 2, "coordinate_system": "decoded_utf8" },
  "style": "bold",
  "text": "東京",
  "inline_children": [
    {
      "type": "ruby",
      "span": { "start": 0, "end": 2, "coordinate_system": "decoded_utf8" },
      "ruby": {
        "base": "東京",
        "reading": "とうきょう",
        "scope": "explicit",
        "direction": "right"
      }
    }
  ]
}
```

Rules:

- `type`, `span`, and `style` remain required.
- At least one of `text` or `inline_children` must be present.
- `text` is the legacy fallback and may be retained as a visible-text cache.
- `inline_children` is an array of recursive inline-capable parser-IR nodes.
- v1 inline children should include `text`, `ruby`, `gaiji`, `editor-note`,
  `emphasis`, `image`, `line-break`, and `page-break`.
- Block-level `heading`, `indentation`, `quote`, `caption`, and `source-note`
  should not be accepted inside `inline_children` in this slice.
- `additionalProperties: false` remains in force.

The schema should use a dedicated `$defs/inlineNode` rather than making the
top-level `node` definition recursively accept everything.

## Rendering Policy

ABC TEI rendering should prefer `inline_children` when present:

- `emphasis.inline_children` renders as nested TEI content inside `<hi>`.
- Legacy `emphasis.text` renders exactly as it does today.
- Nested `emphasis` renders nested `<hi>`.
- Nested `ruby` renders `<ruby type="furigana">`.
- Nested `gaiji` registers `<charDecl>` entries and renders `<g ref="#..."/>`.
- Nested `editor-note` renders `<note type="...">`.
- Nested `line-break` renders `<lb/>`.
- Nested `page-break` renders `<pb/>` if it appears inside a legal phrase
  context; otherwise the renderer should mark it omitted rather than producing
  invalid TEI.

Plaintext rendering should also prefer `inline_children` when present, but it
must keep the existing plaintext policy:

- ruby contributes base text only,
- editor notes/source notes are omitted,
- gaiji contributes resolved unicode or raw marker fallback,
- emphasis recursively contributes visible child text,
- no ruby readings or metadata are emitted.

## Recursion Guard

The measurement found `aozora2` container depth up to 244. That appears to be a
parser artifact rather than normal Aozora structure, but rendering must not rely
on unbounded recursion.

Implementation should include both:

- a validation or renderer recursion limit with a clear diagnostic, and
- a later normalization follow-up for `aozora2` deep style chains.

Recommended initial limit: 64 nested inline containers. This is far above
observed non-`aozora2` depth (`aozora2html` max 4, `aozora-epub3` max 1,
`aozora-rs` max 0) and low enough to avoid stack-risky pathological input.

If the converter sees deeper input before normalization exists, it should
flatten only the overflow portion to legacy `text` and record a divergence
entry. It should not fail the entire work.

## ab-validator Producer Policy

ab-validator should keep emitting legacy `text` while adding
`inline_children`.

For style/font-size/tcy/keigakomi/caption/yokogumi containers:

- Preserve the container as parser-IR `emphasis`.
- Set `style` to the existing style/kind value.
- Set `text` to the current visible projection for compatibility.
- Set `inline_children` to recursively converted child parser-IR inline nodes.
- Continue recording existing measured divergence rules while the mapping
  artifact still describes `emphasis.text` losses.

For accent:

- Keep the current compatibility bridge (`emphasis.text = resolved`,
  `style = code`) until the accent taxonomy/TEI vocabulary slice replaces it.
- Do not invent a richer accent node in this migration.

## Mapping and Compatibility

This schema change rotates the parser-IR schema hash. ABC should:

- keep existing compatibility registry entries for the old parser-IR hash,
- add new entries only after ab-validator regenerates measured conversion
  evidence with the new hash,
- update `validate_design_bundle` legacy accepted hashes only if needed for
  checked-in fixtures,
- update example parser-IR fixtures or add a new structured-emphasis fixture.

ab-validator should:

- sync the rotated ABC schema into `data/abc-schemas/schemas/parser-ir.schema.json`,
- regenerate `data/aat-to-parser-ir-mapping-v1.json` only after the executable
  mapping policy changes,
- rerun the full five-parser audit,
- require `files_failed == 0`,
- compare divergence deltas for `ruby.reading` and `(emphasis.text)` losses.

## Test Plan

ABC tests:

1. Parser-IR schema accepts `emphasis` with `inline_children`.
2. Parser-IR schema still accepts legacy `emphasis.text`.
3. TEI renderer emits nested `<hi><ruby type="furigana">...</ruby></hi>`.
4. TEI renderer emits nested `<hi><hi>...</hi></hi>`.
5. Plaintext renderer emits only visible base text for nested ruby.
6. Renderer handles depth over the configured limit with a diagnostic or
   omission, not unbounded recursion.
7. `nix run .#validate-design-bundle` passes with rotated hash handling.

ab-validator tests:

1. Converter emits `inline_children` for style containing ruby and text.
2. Converter emits nested `emphasis.inline_children` for nested style/tcy.
3. Converter keeps legacy `text` fallback equal to visible projection.
4. Parser-IR validates against the vendored rotated schema.
5. Full audit stays at `89,169 / 89,169` successes.
6. Full audit report records the new parser-IR schema hash.

## Sequencing

1. ABC: schema and renderer support for `inline_children`, including tests and
   schema hash rotation.
2. ab-validator: vendor the new schema and update converter output.
3. ab-validator: rerun the five-parser audit and compatibility candidate
   generation.
4. ABC: admit new compatibility entries keyed to the new parser-IR schema hash.
5. Follow-up: accent taxonomy, warigaki node, gaiji resolved type alignment,
   and inline/container TEI rendition vocabulary.

## Decision

Proceed with a transitional `emphasis.inline_children` migration. This is the
smallest schema change that directly addresses the measured high-volume loss:
ruby and other semantic inline nodes inside emphasis/container contexts. It
keeps legacy `text` compatibility while creating the recursive phrase-level
surface needed for TEI P5 Level 2/3 output.
