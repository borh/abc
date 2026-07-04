# Parser-IR Level 3 Structure Delta

Status: Proposed for ABC review
Date: 2026-07-04
Owner boundary: ABC owns parser-IR schema and publication semantics; ab-validator owns measured AAT conversion evidence and mapper implementation.

## Problem

Current parser-IR can render a valid linear text stream, but it cannot honestly represent Level 3 publication structure because paragraph boundaries and final source attribution are not first-class parser-IR values.

The problem is not measurement anymore. The current ab-validator evidence shows that paragraph structure exists upstream, while parser-IR drops it:

- `docs/superpowers/reports/2026-07-04-melos-structural-probe.md`
  - 4 adapter inputs measured for Melos.
  - 4 parser-IR paragraph gaps.
  - 4 source-attribution gaps.
- `docs/superpowers/reports/2026-07-04-tei-eaj-structural-expansion.md`
  - 62 TEI-EAJ rows scanned from ABC's pinned workset export.
  - 55 rows with AAT evidence.
  - 55 parser-IR gap rows.
  - 17 adapter gap rows.
  - 2 source-attribution gap rows.
  - 7 evidence gap rows.
- `data/aat-to-parser-ir-mapping-v1.json`
  - `S-10` records `blocks[].paragraph` as `STRUCTURAL` loss: "block container of kind 'paragraph' has no parser-IR node; boundary + span + style lost, only inlines emitted".
  - paragraph structural loss has millions of observed occurrences in the measured corpus.
- `data/abc-schemas/schemas/parser-ir.schema.json`
  - current `nodes[]` supports `text`, `ruby`, `gaiji`, `editor-note`, `emphasis`, `heading`, `indentation`, `page-break`, `image`, `caption`, and `quote`.
  - it has no `paragraph`, no paragraph range table, no paragraph ID, and no source-attribution/source-note node.

## Non-Goals

- Do not select the final Aozora parser here.
- Do not infer source authority from TEI-EAJ alone. TEI-EAJ is comparison evidence; source inventory remains the authority for Aozora source constructs.
- Do not redesign AAT v1.
- Do not implement ab-validator converter changes before ABC accepts a parser-IR schema shape.
- Do not require every adapter to preserve paragraphs before parser-IR supports paragraphs. Adapter gaps stay measured separately.

## Design Recommendation

Add two parser-IR concepts:

1. A top-level `paragraphs` array that records paragraph identity and node ranges over the existing flat `nodes[]` stream.
2. A `source-note` node type for source attribution and related producer-side source notes.

Keep `nodes[]` flat. Do not nest parser-IR nodes inside paragraph nodes.

### Why Top-Level `paragraphs`

The current parser-IR `nodes[]` array is already a flat, schema-derived publication surface. A top-level paragraph range table preserves compatibility with that surface while adding Level 3 structure.

Recommended shape:

```json
{
  "schema_id": "https://w3id.org/abc/schemas/parser-ir.schema.json",
  "schema_hash": "sha256:<new parser-ir schema hash>",
  "source": { "...": "..." },
  "nodes": [
    { "type": "text", "span": { "start": 0, "end": 24, "coordinate_system": "decoded_utf8" }, "text": "メロスは激怒した。" }
  ],
  "paragraphs": [
    {
      "id": "p000000",
      "span": { "start": 0, "end": 24, "coordinate_system": "decoded_utf8" },
      "span_source": "direct",
      "node_range": { "start": 0, "end": 1 },
      "role": "body",
      "source_pointer": "blocks[0]",
      "classification": "direct"
    }
  ],
  "warnings": [],
  "errors": []
}
```

Fields:

- `id`: document-local paragraph identity, generated in parser order as `p000000`, `p000001`, ...
- `span`: `$ref` to the same `#/$defs/span` schema used by nodes. If no AAT/source span exists, derive it from the first/last child node offsets when possible, otherwise synthesize a zero-width decoded-UTF8 span and record an `AMBIGUITY` divergence entry in the ab-validator divergence bundle.
- `span_source`: enum `direct`, `derived`, `synthesized`, `unknown`. This describes the paragraph span only; it does not replace `classification`, which describes the paragraph boundary/source classification.
- `node_range`: half-open range into `nodes[]`, `{start, end}`. `end` is exclusive.
- `role`: enum `body`, `source-note`, `unknown`.
- `source_pointer`: AAT/source pointer string when known, otherwise `null`.
- `classification`: enum `direct`, `heuristic`, `unknown`.

Invariants that JSON Schema cannot fully express and ABC validation should enforce:

- Every paragraph ID is unique.
- `0 <= node_range.start <= node_range.end <= nodes.length`.
- Paragraph ranges are monotonic and non-overlapping in v1.
- Nested AAT blocks are flattened into sequential paragraph rows in v1. Do not model parent/child paragraph nesting until ABC has a concrete renderer need for it.
- A paragraph with `role = "source-note"` contains at least one `source-note` node or has a `classification` other than `direct` with a diagnostic explaining the downgrade.
- Existing parser-IR without `paragraphs` remains schema-valid, but cannot satisfy a Level 3 admission gate.

### Why `source-note` Node

Melos demonstrates that final source attribution can be visible text in AAT but should not remain untyped body text for Level 3 TEI comparison:

- ABC body base text includes `（古伝説と、シルレルの詩から。）`.
- TEI-EAJ body base text excludes it.
- Current parser-IR emits it as ordinary text.

Recommended node shape:

```json
{
  "type": "source-note",
  "span": { "start": 9790, "end": 9839, "coordinate_system": "decoded_utf8" },
  "text": "（古伝説と、シルレルの詩から。）",
  "note_type": "source-attribution",
  "placement": "back",
  "classification": "heuristic",
  "source_pointer": "blocks[78]"
}
```

Fields:

- `text`: visible source-note text.
- `note_type`: enum `source-attribution`, `bibliographic-note`, `transcriber-note`, `unknown`.
- `placement`: enum `front`, `body`, `back`, `unknown`.
- `classification`: enum `direct`, `heuristic`, `unknown`.
- `source_pointer`: AAT/source pointer string when known, otherwise `null`.

The `source-note` schema must follow the existing node pattern and set `additionalProperties: false`.

This is intentionally separate from `editor-note`:

- `editor-note` represents Aozora/editorial annotation notes already recognized as note-like parser facts.
- `source-note` represents producer-side source metadata such as final attribution that affects body base-text comparison and TEI placement.

## Renderer Integration

The TEI renderer should use a two-path strategy:

1. If `paragraphs[]` is absent, use the current flat `nodes[]` rendering path unchanged for backward compatibility.
2. If `paragraphs[]` is present, switch at the top of rendering to a paragraph-table-driven path:
   - validate paragraph ranges before rendering,
   - iterate `paragraphs[]` in order,
   - slice `nodes[]` by each paragraph's half-open `node_range`,
   - reuse the existing node dispatch to render nodes inside each slice,
   - render `role = "body"` paragraphs as TEI body `<p>` elements,
   - render `role = "source-note"` paragraphs according to the contained `source-note.placement`.

Do not braid paragraph boundary decisions into the existing flat reduce loop. The old path remains the non-Level-3 fallback; the new path owns Level 3 paragraph behavior.

`source-note.placement` is not decorative:

- `placement = "back"` renders as a source note in TEI back matter, for example a `<note type="source">` under a back division.
- `placement = "body"` renders as an inline or local source note at the paragraph position.
- `placement = "front"` renders in front matter.
- `placement = "unknown"` must choose an explicit renderer policy and record a diagnostic; it must not silently emit ordinary body text.

Plaintext rendering must also be explicit. The default plaintext publication renderer should render body paragraphs as body text and append `placement = "back"` source notes after the body separated from body paragraphs. Body-base-text comparison must ignore `source-note` nodes rather than relying on the plaintext renderer as a base-text extractor.

## Rejected Alternatives

### Nested Parser-IR Nodes

Shape: make `paragraph` a node that owns child nodes.

Rejected because it forces every current renderer and mapper to switch from one flat stream to recursive traversal. It also repeats AAT's block tree rather than adding the missing publication identity to parser-IR.

### `paragraph_id` on Every Node

Shape: add an optional `paragraph_id` field to all node types.

Rejected for v1 because it touches every node schema and every producer branch. It also makes paragraph identity a property of each child instead of a value with its own span, role, and source pointer.

### Boundary Marker Nodes Only

Shape: add `paragraph-boundary` nodes to `nodes[]`.

Rejected because boundary markers are easy to emit but weak as a contract. They require consumers to reconstruct paragraph ranges and cannot attach paragraph-level metadata cleanly.

### Treat Source Attribution as `editor-note`

Shape: add `source-attribution` to `note.category` and emit an `editor-note`.

Rejected because source attribution is not merely an editorial annotation; it changes body base-text accounting and TEI placement. A dedicated `source-note` node is clearer and avoids overloading the existing note taxonomy.

## Mapping Consequences

After ABC accepts the schema:

- Regenerate `data/aat-to-parser-ir-mapping-v1.json` against the new parser-IR schema hash.
- `blocks[].paragraph` rules should move from `STRUCTURAL` loss to projection into `paragraphs[]`.
- Paragraph span synthesis should record `AMBIGUITY` when the source/AAT span is missing.
- Source attribution should initially be `classification = "heuristic"` unless source inventory provides a typed source marker.
- Source-note detection should emit a divergence entry keyed to the source/AAT pointer and parser-IR `source-note`, so the heuristic is auditable.
- Compatibility registry entries in ABC must be keyed to the new parser-IR schema hash and mapping hash.

## Schema Hash And Registry Rotation

This follows the ADR 0024 rotation precedent:

- Adding `paragraphs[]` and `source-note` changes the parser-IR schema hash.
- Existing ABC compatibility registry entries keyed to the old parser-IR schema hash remain valid for old-schema documents.
- New Level 3 compatibility entries must be added only after ab-validator provides measured evidence against the new parser-IR schema hash and the regenerated mapping hash.
- ABC should keep an old-shape parser-IR fixture to prove backward schema compatibility and add a separate Level 3 fixture with populated `paragraphs[]` and `source-note`.
- ABC design-bundle validation should validate both old-shape compatibility and new-shape Level 3 fixtures.
- ABC manifest construction must read `parser_ir.schema_hash` from the parser-IR document when constructing identity-bearing manifests. The schema hash is identity-bearing, so old-schema and new-schema parser-IR documents correctly produce different artifact identities.

## Admission Gates

ABC-side gates:

- Parser-IR schema validates old documents without `paragraphs`.
- Parser-IR schema validates new documents with `paragraphs[]` and `source-note` nodes.
- Schema-derived renderer coverage fails until plaintext and TEI policies cover `source-note`.
- TEI renderer uses `paragraphs[]` when present and falls back to current behavior only for non-Level-3 documents.
- Melos parser-IR fixture can render TEI body paragraphs without pretending the final source attribution is body base text.
- Any ABC manifest or admission record that claims Level 3 for prose fails validation if parser-IR lacks populated `paragraphs[]`.

ab-validator-side gates after ABC schema lands:

- `ab-aat-to-parser-ir` emits `paragraphs[]` for AAT paragraph blocks.
- Melos structural probe reports parser-IR paragraph representation as true for adapters that preserve AAT paragraphs.
- Melos source-attribution gap drops for adapters where the final attribution can be isolated.
- Full conversion audit validates against the new parser-IR schema hash.
- Mapping generation reports `blocks[].paragraph` no longer as `STRUCTURAL` loss.
- TEI-EAJ structural expansion distinguishes remaining adapter gaps from parser-IR gaps.

## Known Blockers

1. ABC must accept and own the parser-IR schema change.
2. ABC renderer coverage must add policies for `paragraphs[]` and `source-note`.
3. Source-note classification is still heuristic for Melos until source inventory exposes a typed source-note/source-attribution signal.
4. `aozora-rs` and `aozora2` often collapse paragraphs; those remain adapter evidence gaps, not blockers for parser-IR support.
5. 7 TEI-EAJ rows still lack enough local AAT/work-ID evidence in the current report.

## Recommended Next Step

Implement the ABC schema and renderer tracer bullet first. Do not update ab-validator's converter until ABC has a parser-IR schema hash and renderer policy for the new fields.

The first ABC tracer bullet should use a small fixture with:

- two body paragraphs,
- one final source attribution,
- a `paragraphs[]` table covering the body paragraphs and source-note paragraph,
- one `source-note` node with `note_type = "source-attribution"`,
- TEI rendering that excludes the source attribution from body base text.
