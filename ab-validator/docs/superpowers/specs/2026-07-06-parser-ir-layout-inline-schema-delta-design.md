# Parser-IR Layout Inline Schema Delta Design

Status: Proposed for ABC review
Date: 2026-07-06
Owner boundary: ABC owns Parser-IR schema and TEI rendering semantics. ab-validator owns measured evidence, adapter conversion behavior, and candidate schema-delta requirements.

## Goal Boundary

The publication-closure goal is not "force every Aozora construct into TEI
Level 2 or Level 3." The goal is to map every measured Parser-IR construct
into either:

- TEI P5 when TEI has an honest representation for the construct, or
- an ABC-owned custom preservation schema when exact Aozora marker identity,
  provenance, or measured metadata has no faithful TEI projection.

The Level 3 plain-prose gate remains useful, but it is only one admission lane.
Full closure requires TEI projection plus custom-schema preservation, with no
unknown representability gaps.

## Problem

The current publication coverage report no longer has unknown representability
gaps, but it still has 23 Parser-IR-schema-owned classified gaps:

- `heading_inline_content`: 2 rows, 277 measured occurrences
- `font_tcy`: 13 rows, 54,043 measured occurrences
- `keigakomi_yokogumi`: 8 rows, 517 measured occurrences

Source-authority prevalence confirms these are real Aozora constructs:

| Source inventory row | Works | Occurrences |
|---|---:|---:|
| `decoration.font_size` | 1,223 | 51,508 |
| `layout.tcy` | 725 | 19,794 |
| `decoration.keigakomi` | 157 | 717 |
| `layout.yokogumi` | 423 | 3,690 |

The existing converter already preserves visible text for these constructs by
projecting them through `emphasis`-like nodes or flattening heading text. That
is not enough for full publication coverage:

- `headingNode` currently stores only flat `text`, so gaiji/ruby/style inside
  headings cannot be preserved structurally in TEI `head`.
- `emphasisNode.style` is a string, so `font_size`, `tcy`, `keigakomi`, and
  `yokogumi` lose typed marker data such as font-size level or layout-scope
  kind.
- Current divergence records correctly flag these as schema deltas rather than
  converter bugs.

## Evidence

Current schema facts:

- `inlineNode` includes `text`, `ruby`, `gaiji`, `editor-note`, `emphasis`, and
  `line-break`.
- `emphasisNode` supports `text` and `inline_children`, but its styling payload
  is only `style: string`.
- `headingNode` requires `text` and `level`; it has no `inline_children`.
- `paragraphLayout` already represents block/paragraph layout facts such as
  `jisage`, `burasage`, `chitsuki`, `jizume`, and `line-jisage`.

Current converter facts:

- Inline `font_size`, `tcy`, `keigakomi`, and `yokogumi` are projected as
  `emphasis` nodes with `style` equal to the AAT kind.
- Heading content is flattened to a `heading` node with plain text, and
  measured losses remain for heading inline gaiji and related structured
  content.
- ABC TEI rendering already knows how to render `heading` nodes as `head` and
  `emphasis` nodes as `hi @rend`; the missing piece is exact IR structure, not
  a new parser benchmark.

## Decision

Add two Parser-IR capabilities:

1. **Structured heading content**
   - `headingNode` keeps `text` for compatibility and plaintext/search use.
   - `headingNode` gains optional `inline_children`.
   - When `inline_children` is present, TEI rendering should render those
     children inside `head`; `text` remains the visible text projection.

2. **Typed inline layout scope**
   - Add a first-class inline node for source layout/rendition spans.
   - Working name: `layout-span`.
   - It carries visible `text`, optional `inline_children`, and a typed
     `layout` object.
   - It is an `inlineNode`, so it can appear inside paragraphs, emphasis,
     warigaki, and heading `inline_children`.

This intentionally does not add separate node types for `font-size`, `tcy`,
`keigakomi`, and `yokogumi`. A single typed layout-scope node keeps the schema
small while preserving exact marker kind and fields.

## Proposed Schema Shape

Add `layoutSpanNode` to the root `node` union and to `inlineNode`:

```json
{
  "type": "layout-span",
  "span": {"start": 0, "end": 3, "coordinate_system": "decoded_utf8"},
  "text": "一二三",
  "inline_children": [
    {"type": "text", "span": {"start": 0, "end": 3}, "text": "一二三"}
  ],
  "layout": {
    "kind": "tcy",
    "source": "aat-inline",
    "marker": "縦中横"
  }
}
```

Required fields:

- `type`
- `span`
- `text`
- `layout`

Optional fields:

- `inline_children`

`layout` is a discriminated object:

```json
{
  "oneOf": [
    {
      "required": ["kind", "source", "size_type", "level"],
      "properties": {
        "kind": {"const": "font-size"},
        "source": {"$ref": "#/$defs/layoutScopeSource"},
        "size_type": {"type": "string"},
        "level": {"type": "integer", "minimum": 0}
      }
    },
    {
      "required": ["kind", "source"],
      "properties": {
        "kind": {"const": "tcy"},
        "source": {"$ref": "#/$defs/layoutScopeSource"},
        "marker": {"type": ["string", "null"]}
      }
    },
    {
      "required": ["kind", "source"],
      "properties": {
        "kind": {"const": "keigakomi"},
        "source": {"$ref": "#/$defs/layoutScopeSource"},
        "border": {"type": ["string", "null"]}
      }
    },
    {
      "required": ["kind", "source"],
      "properties": {
        "kind": {"const": "yokogumi"},
        "source": {"$ref": "#/$defs/layoutScopeSource"},
        "direction": {"type": "string", "enum": ["horizontal"]}
      }
    }
  ]
}
```

`layoutScopeSource` should be a small enum:

- `aat-inline`
- `aat-block`
- `source-derived`
- `heuristic`

For compatibility, existing `emphasisNode` remains valid. The converter may
continue emitting `emphasis` for ordinary styling such as bold/italic/kaeriten,
while emitting `layout-span` for Aozora layout constructs.

## Heading Shape

Update `headingNode` from flat text only to text plus optional structured
children:

```json
{
  "type": "heading",
  "span": {"start": 0, "end": 9, "coordinate_system": "decoded_utf8"},
  "text": "硝子",
  "level": 2,
  "inline_children": [
    {
      "type": "gaiji",
      "span": {"start": 0, "end": 3, "coordinate_system": "decoded_utf8"},
      "gaiji": {
        "raw_marker": "※［＃...］",
        "reference": null,
        "unicode": "硝",
        "ivs": null,
        "image_or_glyph_fallback": null,
        "resolved": true
      }
    }
  ]
}
```

Rules:

- `text` remains required.
- `inline_children` is optional.
- If `inline_children` exists, its visible projection must equal `text`.
- ABC TEI renderer should prefer `inline_children` when rendering `head`.
- Plaintext renderer continues to use `text`.

## TEI Projection

The schema delta does not decide final ABC TEI profile vocabulary, but it gives
ABC enough structure to define it:

| Parser-IR fact | Likely TEI projection |
|---|---|
| heading `inline_children` | Render child nodes inside `head`. |
| `layout-span.kind=font-size` | `hi @rend` / `@rendition`, e.g. CSS-style `font-size:*`. |
| `layout-span.kind=tcy` | `hi` or `seg` with declared text-combine/upright rendition. |
| `layout-span.kind=keigakomi` | inline `seg`/`hi` with bordered rendition, or block `div` when block-scoped. |
| `layout-span.kind=yokogumi` | `seg`/`hi` or `div` with horizontal-writing rendition. |

Exact source marker identity still belongs in the ABC custom preservation
contract when TEI `@rend` is not exact.

## Converter Requirements

After ABC accepts the schema delta, ab-validator should update conversion:

- Heading conversion:
  - Build heading `inline_children` using the same inline-child projection used
    by `emphasis`.
  - Keep `text` as the visible projection.
  - Stop emitting `heading_inline_content` schema-delta losses when the child
    kind is representable in `inline_children`.

- Inline layout conversion:
  - Convert AAT `font_size` to `layout-span.layout.kind=font-size` with
    `size_type` and `level`.
  - Convert AAT `tcy` to `layout-span.layout.kind=tcy`.
  - Convert AAT inline `keigakomi` and `yokogumi` to corresponding layout-span
    kinds.
  - Preserve nested content in `inline_children`.
  - Keep visible `text` for plaintext/search compatibility.

- Block layout conversion:
  - Keep existing `paragraph.layout` for `jisage`, `burasage`, `chitsuki`,
    `jizume`, and `line-jisage`.
  - Use block-level TEI/profile policy for `keigakomi_block` and
    `yokogumi_block`; do not force them into paragraph layout if they have
    nested block structure.

## Rejected Alternatives

### Add one node type per Aozora marker

Example: `fontSizeNode`, `tcyNode`, `keigakomiNode`, `yokogumiNode`.

Rejected because the schema would grow with source notation vocabulary and
would duplicate traversal/rendering behavior. These constructs share the same
shape: visible text, nested inline children, and typed layout metadata.

### Reuse `emphasis.style` only

Rejected because the current report already proves that string-only style is
not enough. It cannot preserve `font_size.level`, marker source, or typed
layout semantics without pushing everything into the custom sidecar.

### Put all layout facts only in the custom sidecar

Rejected because font-size, TCY, keigakomi, and yokogumi are publication
layout facts. TEI should receive a faithful publication projection; the sidecar
is for exact marker identity and provenance, not for hiding renderable content.

## Acceptance Criteria

The schema-delta is ready for implementation when ABC agrees to:

1. Add structured `heading.inline_children`.
2. Add a typed inline layout-scope node or an equivalent schema shape with the
   same information content.
3. Preserve visible `text` fields for plaintext/search compatibility.
4. Keep metadata-free plaintext behavior unchanged.
5. Rotate the Parser-IR schema hash and compatibility entries following the
   existing ADR 0024 precedent.

ab-validator can then implement a converter slice that reduces the
`parser_ir_schema` closure owner count from 23 toward zero.

## Self-Review

- Red-flag scan: no deferred-work markers.
- Internal consistency: heading text remains compatible; structured children
  provide exact TEI rendering; layout-span covers the current schema-delta
  families without creating one node per Aozora marker.
- Scope check: this is a schema-delta design, not ABC implementation.
- Ambiguity check: the proposed fields, rejected alternatives, and admission
  criteria are explicit.
