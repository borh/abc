# Parser-IR Level 3 Structure Delta

Date: 2026-07-04

## Summary

ABC now accepts the parser-IR structure needed for the first Level 3 paragraph/source-note tracer bullet:

- optional top-level `paragraphs[]` range table over flat `nodes[]`,
- `source-note` parser-IR node,
- schema-derived renderer coverage for `source-note`,
- TEI two-path rendering:
  - old documents without `paragraphs[]` keep the existing flat `nodes[]` path,
  - documents with populated `paragraphs[]` render by paragraph ranges,
- plaintext source notes are kept separate from body text by appending back-placement notes after the body,
- optional paragraph `layout` metadata for Aozora indentation/alignment layout.

Older parser-IR schema hashes remain accepted for legacy ab-validator compatibility fixtures. The current schema hash is:

`sha256:d98eb9684e7a88f5b62693dd582e28f14834ff85011dc7297e7b41516f7be913`

## Renderer Policy

For TEI:

- `role = "body"` paragraph rows render as body `<p>` elements.
- `paragraphs[].layout` renders as `p@rend` tokens:
  - `jisage indent(N)`,
  - `burasage first(N) rest(M)`,
  - `chitsuki align(right) offset-from-end(N)`,
  - `jizume width(N)`,
  - `line-jisage indent(N)`.
- `source-note.placement = "back"` renders in TEI back matter as a source note.
- `source-note.placement = "body"` renders inline/local at the paragraph position.
- `source-note.placement = "front"` renders in front matter.
- `source-note.placement = "unknown"` must not silently become ordinary body text.

For plaintext:

- body text remains in source node order,
- paragraph layout is ignored and never appears in plaintext output,
- `placement = "back"` source notes are appended after a blank line,
- body-base-text comparison should ignore `source-note` nodes rather than using plaintext output as the base-text extractor.

## Validation

ABC validates paragraph coherence beyond JSON Schema:

- duplicate paragraph IDs fail,
- node ranges must be inside `nodes[]`,
- ranges must be monotonic and non-overlapping,
- direct source-note paragraphs must contain a `source-note` node.

There is still no manifest/admission field that formally claims "Level 3". When ABC adds that field, its validator should fail prose Level 3 claims unless parser-IR contains populated `paragraphs[]` and passes paragraph coherence validation.

## ab-validator Follow-Up

Next ab-validator work:

1. Sync `schemas/parser-ir.schema.json` into `data/abc-schemas/schemas/parser-ir.schema.json`.
2. Update schema hash expectations to `sha256:d98eb9684e7a88f5b62693dd582e28f14834ff85011dc7297e7b41516f7be913`.
3. Regenerate mapping from measured mapper rules against the new parser-IR schema.
4. Update `ab-aat-to-parser-ir` to emit:
   - `paragraphs[]` for AAT paragraph blocks,
   - `source-note` nodes for measured/heuristic source attribution,
   - `span_source` on paragraph rows.
   - `layout` on paragraph rows for AAT `jisage`, `burasage`, `chitsuki`, `jizume`, and `line-jisage`.
5. Rerun conversion audit and structural probes before adding new compatibility registry entries for the new schema hash.
