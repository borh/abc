# Parser-IR Layout Vocabulary Delta

Date: 2026-07-05

## Problem

The current AAT layer can now preserve Aozora layout markers as typed layout
intent, but Parser-IR still collapses those typed layout styles to emphasis.
That preserves base text, but it is not honest TEI Level 3 layout evidence.

The immediate measured cases are:

- `style_type = "burasage"` with `x-indent-first` and `x-indent-rest`.
- `style_type = "chitsuki"` with `x-align = "right"` and `x-offset`.

Both are paragraph layout properties. Treating them as inline `emphasis` loses
the distinction between visual emphasis and source layout. Treating them as the
existing `indentation` node would also be wrong for Level 3 paragraph-aware
documents, because the metadata belongs on the TEI `<p>` boundary.

## Evidence Ledger

| Claim | Type | Source | Confidence | Impact if wrong |
|---|---|---|---|---|
| The `aozora` adapter now emits typed AAT `burasage` and `chitsuki` styles. | Observation | `adapters/aozora/tests/integration.rs`; committed adapter tests | High | Parser-IR delta may target a shape that adapters do not produce. |
| Work `1126` now has zero raw markers from the `aozora` adapter. | Observation | Fresh `jq '[.. raw] | length'` scan after `c2c1444` | High | The slice would be optimizing the wrong adapter gap. |
| `ab-aat-to-parser-ir` currently maps AAT `style` to Parser-IR `emphasis`. | Observation | `crates/ab-aat-to-parser-ir/src/convert.rs`; conversion probes in `docs/handoffs/parser-ir-burasage-layout-gap.md` | High | Layout vocabulary may not be needed if converter already handled it. |
| Parser-IR `indentationNode` cannot represent burasage/chitsuki without overloading `depth`. | Observation | `../abc/schemas/parser-ir.schema.json` | High | Existing node extension could be sufficient. |
| ABC's Level 3 TEI renderer uses `paragraphs[]` to render TEI `<p>` boundaries. | Observation | `../abc/src/abc/tools/parser_ir_tei.clj` | High | Layout might belong somewhere other than the paragraph row. |
| TEI P5 allows source rendering information on `<p>` via `@rend`. | Observation | `../abc/references/TEI/P5/Source/Specs/p.xml` and `att.global.rendition.xml` | High | Proposed TEI projection might be non-standard. |

## Glossary

| Term | Definition |
|---|---|
| `jisage` | Aozora indentation from the head/start of a line or block. |
| `burasage` | Hanging indentation: first line and continuation lines use different indentation values. |
| `chitsuki` | End/right alignment, optionally offset from the end by `N` characters. |
| layout wrapper | An AAT inline `style` node whose `style_type` is source layout, not visual emphasis. |
| paragraph layout | Parser-IR metadata attached to a `paragraphs[]` row and rendered on the TEI `<p>` element. |

## Use Cases

| Actor | Objective | Current obstacle | Capability when solved |
|---|---|---|---|
| ABC TEI renderer | Emit TEI that records Aozora layout markers without changing base text. | Layout styles arrive as `emphasis` nodes. | Render `<p rend="...">` with layout metadata. |
| Plaintext renderer | Produce body text with no ruby or layout metadata. | Layout wrappers can appear as emphasis, which is misleading but currently still text-only. | Ignore paragraph layout and emit content-only text. |
| ab-validator conversion audit | Distinguish "layout preserved" from "layout flattened to emphasis". | Mapping treats all `style` as emphasis. | Count burasage/chitsuki as layout projection, not emphasis ambiguity. |
| TEI-EAJ comparison | Compare body text while separately explaining layout deltas. | Parser-IR lacks a place to cite paragraph layout evidence. | Report layout evidence from paragraph rows. |

## Decision

Add optional paragraph-level layout metadata to Parser-IR `paragraphs[]` rows.
Do not encode burasage/chitsuki as inline `emphasis`. Do not overload the
existing `indentation` node for Level 3 paragraph-aware documents.

Proposed JSON shape:

```json
{
  "id": "p000012",
  "span": { "start": 123, "end": 145, "coordinate_system": "decoded_utf8" },
  "span_source": "derived",
  "node_range": { "start": 42, "end": 45 },
  "role": "body",
  "source_pointer": "blocks[9]",
  "classification": "direct",
  "layout": {
    "kind": "burasage",
    "first_line_indent": 0,
    "continuation_indent": 1,
    "source": "aat-style"
  }
}
```

```json
{
  "layout": {
    "kind": "chitsuki",
    "align": "right",
    "offset_from_end": 1,
    "source": "aat-style"
  }
}
```

The `layout` object is optional. Existing paragraph rows remain schema-valid.

### Layout Object

Fields:

- `kind`: required enum. Initial values:
  - `jisage`
  - `burasage`
  - `chitsuki`
  - `jizume`
  - `line-jisage`
- `source`: required enum. Initial values:
  - `aat-block`
  - `aat-style`
  - `source-derived`
  - `heuristic`
- `indent`: optional integer, for simple `jisage` and line jisage.
- `first_line_indent`: optional integer, for `burasage`.
- `continuation_indent`: optional integer, for `burasage`.
- `align`: optional enum, initially `right`.
- `offset_from_end`: optional integer, for `chitsuki`.
- `width`: optional integer, for `jizume`.

The schema should use `additionalProperties: false`. Conditional validation can
be added later; the first slice may rely on fixture tests for kind-specific
required fields if conditional JSON Schema would make the delta too large.

## TEI Projection

The paragraph-aware TEI renderer should apply `layout` on the `<p>` it already
creates from `paragraphs[]`.

Suggested initial `@rend` tokens:

- `jisage indent(N)`
- `burasage first(N) rest(M)`
- `chitsuki align(right) offset-from-end(N)`
- `jizume width(N)`
- `line-jisage indent(N)`

TEI P5 evidence:

- `<p>` marks prose paragraphs and is a member of `att.global`.
- `att.global.rendition` provides `@rend`, whose values are source rendering
  tokens; TEI does not prescribe a closed vocabulary.

This keeps the encoding TEI P5-conformant while preserving Aozora-specific
layout information as source rendering, not semantic enrichment.

## Plaintext Projection

Plaintext ignores paragraph `layout`. It emits only the text nodes in the
paragraph's node range. This matches the standing rule that plaintext contains
no ruby or other metadata.

## AAT-to-Parser-IR Projection

When an AAT paragraph has a single top-level layout wrapper:

```json
{
  "kind": "style",
  "style_type": "burasage",
  "x-indent-first": 0,
  "x-indent-rest": 1,
  "content": [...]
}
```

the converter should:

1. Attach the corresponding `layout` object to the Parser-IR paragraph row.
2. Map the wrapper's `content[]` normally into Parser-IR nodes.
3. Not emit a Parser-IR `emphasis` node for the layout wrapper.

For mixed content where a layout wrapper is not the sole paragraph wrapper,
keep current behavior initially and record the case as a follow-up; do not
guess at partial-paragraph layout semantics in this slice.

## Rejected Alternatives

### Keep `style -> emphasis`

Rejected because it preserves text but lies about the feature class. Burasage
and chitsuki are layout, not emphasis.

### Extend `indentationNode` Only

Rejected for Level 3 paragraph-aware documents because the TEI target is the
paragraph boundary. The existing `indentation` node can remain as a legacy or
non-paragraph-aware representation, but it should not be the primary Level 3
layout carrier.

### Add A New `layout` Node

Rejected for this slice because it would add another node to the flat stream
that does not correspond to visible text. Paragraph rows already provide the
range and role boundary needed by ABC's TEI renderer.

## Dev Handoff

First implementation slice, ABC first:

1. Add `layout` to the Parser-IR paragraph schema.
2. Add TEI renderer tests for `burasage` and `chitsuki` paragraph rows.
3. Render paragraph layout as `@rend` on `<p>`.
4. Confirm plaintext rendering is unchanged.
5. Publish the new Parser-IR schema hash.

Then ab-validator:

1. Sync the ABC parser-IR schema.
2. Add failing converter tests for AAT `style_type = "burasage"` and
   `style_type = "chitsuki"`.
3. Map those wrappers to paragraph `layout` while unwrapping content normally.
4. Regenerate `data/aat-to-parser-ir-mapping-v1.json`.
5. Re-run focused `aozora` samples and the relevant conversion audit.

## Review Notes

No blocking design issue remains in the proposed direction. The main accepted
tradeoff is that conditional schema validation for each layout kind may lag the
first schema field addition, as long as fixture tests pin the two measured
layout styles before admission.
