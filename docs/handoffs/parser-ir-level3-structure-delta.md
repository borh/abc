# Parser-IR Level 3 Structure Delta Handoff

Date: 2026-07-04

## Message For ABC

ab-validator's current evidence says parser-IR cannot yet honestly claim Level 3 paragraph/source-note structure. The gap is now a parser-IR protocol gap, not just a parser measurement gap.

Recommended ABC-first change:

1. Add optional top-level `paragraphs[]` to parser-IR as a range table over the existing flat `nodes[]`.
2. Add a dedicated `source-note` node for source attribution and related producer-side source notes.
3. Keep existing parser-IR documents schema-valid, but require the new structure for Level 3 admission.

Do not update ab-validator conversion behavior until ABC owns the schema shape and publishes the new parser-IR schema hash.

The hard renderer step should be specified as a two-path TEI renderer:

- no `paragraphs[]`: keep the existing flat `nodes[]` rendering path unchanged,
- populated `paragraphs[]`: branch at render entry, iterate paragraph rows, slice `nodes[]` by each half-open `node_range`, and reuse existing node dispatch inside each slice.

`source-note.placement` must drive output:

- `back`: render as source note in TEI back matter,
- `body`: render inline/local at the paragraph position,
- `front`: render in front matter,
- `unknown`: render through an explicit diagnostic policy, not as ordinary body text.

## Evidence

- `docs/superpowers/reports/2026-07-04-melos-structural-probe.md`
  - 4 adapters measured for Melos.
  - 4 parser-IR paragraph gaps.
  - 4 source-attribution gaps.
- `docs/superpowers/reports/2026-07-04-tei-eaj-structural-expansion.md`
  - 62 TEI-EAJ rows scanned.
  - 55 rows with AAT evidence.
  - 55 parser-IR gap rows.
  - 17 adapter gap rows.
  - 2 source-attribution gap rows.
  - 7 evidence gap rows.
- `data/aat-to-parser-ir-mapping-v1.json`
  - `S-10` records `blocks[].paragraph` as `STRUCTURAL` loss.
- `data/abc-schemas/schemas/parser-ir.schema.json`
  - current node union has no paragraph/range/source-note representation.

## Proposed Shape

Use the existing flat `nodes[]` as the publication stream and add:

```json
{
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
  ]
}
```

Add `source-note` as a node:

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

Schema details:

- `paragraph.span` should `$ref` ABC's existing span definition.
- `paragraph.span_source` should distinguish `direct`, `derived`, `synthesized`, and `unknown` spans.
- `source-note` should use `additionalProperties: false`, matching existing parser-IR node definitions.
- JSON uses `node_range`; any internal schema definition name may remain `nodeRange`.
- v1 flattens nested AAT blocks into sequential paragraph rows.

## First ABC Tracer Bullet

Create one parser-IR fixture with:

- two body paragraphs,
- one final source attribution,
- a `paragraphs[]` table covering body paragraphs and source-note paragraph,
- one `source-note` node with `note_type = "source-attribution"`,
- TEI rendering that excludes the source attribution from body base text.

Then publish the new parser-IR schema hash and renderer policy notes back to ab-validator.

Keep old-schema compatibility entries valid. Add new Level 3 compatibility registry entries only after ab-validator provides measured evidence against the new parser-IR schema hash and regenerated mapping hash. ABC should validate both an old-shape parser-IR fixture and a new Level 3 fixture, following the ADR 0024 schema-rotation precedent.

## ab-validator Follow-Up

After ABC lands the schema:

- sync `data/abc-schemas/schemas/parser-ir.schema.json`,
- regenerate mapping against the new schema hash,
- update `ab-aat-to-parser-ir` to emit `paragraphs[]` and `source-note`,
- rerun the full conversion audit, Melos structural probe, and TEI-EAJ structural expansion.
