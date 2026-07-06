# Parser-IR Level 3 Structure Delta Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use `superpowers:subagent-driven-development` or `superpowers:executing-plans` to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add ABC-owned parser-IR Level 3 paragraph and source-note structure, then update ab-validator conversion only after ABC admits the schema.

**Architecture:** ABC lands schema, renderer, and validation first. ab-validator then syncs the schema, regenerates mapping, and emits paragraphs/source-note with measured divergence evidence.

**Tech Stack:** JSON Schema draft 2020-12, Clojure ABC renderers/validators, Rust `ab-aat-to-parser-ir`, existing mapping generator, and ABC compatibility registry.

## Global Constraints

- ABC owns parser-IR schema and publication semantics.
- ab-validator owns measured AAT conversion evidence and mapping implementation.
- Do not change ab-validator converter behavior before ABC publishes the new parser-IR schema hash.
- Existing parser-IR documents without `paragraphs` remain schema-valid but are not Level 3.
- TEI-EAJ is comparison evidence; source inventory remains the Aozora source authority.
- Adapter paragraph collapse remains an adapter gap, not a reason to omit paragraph support.
- Generated corpus outputs stay out of `data/`; committed summaries belong under `docs/superpowers/reports/`.

## Task 1: ABC Schema Tracer Bullet

- [ ] Add an ABC parser-IR schema fixture that contains the current flat `nodes[]` shape and no `paragraphs`; assert it remains schema-valid.
- [ ] Add a second ABC fixture with:
  - [ ] two body text nodes,
  - [ ] one `source-note` node,
  - [ ] a top-level `paragraphs[]` table with half-open `node_range` entries,
  - [ ] a source-note paragraph with `role = "source-note"`.
- [ ] Modify `../abc/schemas/parser-ir.schema.json` to add:
  - [ ] optional top-level `paragraphs`,
  - [ ] a `paragraph` definition with `id`, `span`, `span_source`, `node_range`, `role`, `source_pointer`, and `classification`,
  - [ ] `paragraph.span` as a `$ref` to the existing `#/$defs/span`,
  - [ ] a `nodeRange` schema definition used by the JSON property `node_range`, with integer `start` and `end`,
  - [ ] a `source-note` node definition with `text`, `note_type`, `placement`, `classification`, and `source_pointer`,
  - [ ] `additionalProperties: false` on `source-note`, matching existing node definitions.
- [ ] Add ABC validator checks that JSON Schema cannot express:
  - [ ] paragraph IDs are unique,
  - [ ] `0 <= node_range.start <= node_range.end <= nodes.length`,
  - [ ] paragraph ranges are monotonic and non-overlapping in v1,
  - [ ] nested AAT blocks flatten to sequential paragraph rows in v1,
  - [ ] `role = "source-note"` paragraphs contain a `source-note` node or carry a diagnostic downgrade.
- [ ] Verify ABC manifest construction reads `parser_ir.schema_hash` from the parser-IR document rather than assuming a single current parser-IR schema hash.
- [ ] Run the ABC schema validation command used by `nix run .#validate-design-bundle`.

## Task 2: ABC Renderer Policy

- [ ] Extend ABC renderer coverage so `source-note` fails coverage until every renderer has an explicit policy.
- [ ] Update plaintext rendering so body paragraphs remain body text and `placement = "back"` source notes are appended after the body rather than silently folded into body base text.
- [ ] Update TEI rendering with a two-path strategy:
  - [ ] if `paragraphs[]` is absent, keep the current flat `nodes[]` reduce path unchanged,
  - [ ] if `paragraphs[]` is present, branch at render entry to a paragraph-table-driven path,
  - [ ] validate paragraph ranges before rendering,
  - [ ] iterate `paragraphs[]` in order and slice `nodes[]` by each half-open `node_range`,
  - [ ] reuse existing node dispatch for nodes inside each slice,
  - [ ] render `role = "body"` paragraphs as TEI body `<p>` elements,
  - [ ] route `source-note` by `placement`: `back` to TEI back matter, `body` inline/local, `front` front matter, `unknown` through an explicit diagnostic policy.
- [ ] Add a Melos-sized ABC fixture with:
  - [ ] two body paragraphs,
  - [ ] one final source attribution,
  - [ ] a `source-note` node with `note_type = "source-attribution"`,
  - [ ] expected TEI body paragraphs that exclude the source attribution from body base text.
- [ ] Run ABC renderer tests and `nix run .#tei-eaj-aozora-reports`.

## Task 3: ABC Registry And Handoff

- [ ] Regenerate the parser-IR schema hash after the ABC schema change.
- [ ] Follow the ADR 0024 schema-rotation precedent:
  - [ ] keep existing compatibility registry entries valid for old-schema documents,
  - [ ] add new Level 3 entries only after ab-validator provides evidence against the new parser-IR schema hash and mapping hash,
  - [ ] validate both an old-shape parser-IR fixture and a new Level 3 fixture in the design bundle.
- [ ] Update ABC compatibility policy so Level 3 paragraph evidence requires the new parser-IR schema hash.
- [ ] Add or update ABC registry tests that reject Level 3 claims made against the old schema hash.
- [ ] Add or update ABC admission validation so prose Level 3 claims fail when parser-IR lacks populated `paragraphs[]`.
- [ ] Write an ABC handoff containing:
  - [ ] the new parser-IR schema hash,
  - [ ] the updated fixture path,
  - [ ] renderer policy notes,
  - [ ] the compatibility registry expectation.
- [ ] Put the ABC-side handoff under `../abc/docs/handoffs/` with a matching Level 3 naming convention.
- [ ] Push the ABC commit before starting ab-validator converter changes.

## Task 4: ab-validator Schema Sync

- [ ] Copy ABC's updated `parser-ir.schema.json` into `data/abc-schemas/schemas/parser-ir.schema.json`.
- [ ] Update parser-IR schema hash expectations in ab-validator tests and reports.
- [ ] Add a failing ab-validator test that proves current converter output still lacks Level 3 paragraph evidence.
- [ ] Run:

```bash
cargo test -p ab-aat-to-parser-ir
tests/aat-to-parser-ir-cli-smoke.sh
tests/aat-parser-ir-mapping-policy-smoke.sh
```

## Task 5: ab-validator Mapping And Converter

- [ ] Regenerate mapping from executable mapper rules against the new parser-IR schema.
- [ ] Assert `blocks[].paragraph` no longer appears as `STRUCTURAL` loss in `data/aat-to-parser-ir-mapping-v1.json`.
- [ ] Update `ab-aat-to-parser-ir` to emit `paragraphs[]` for AAT paragraph blocks.
- [ ] Compute paragraph ranges by capturing the current node index before mapping a paragraph block, emitting that block's child nodes, then appending the paragraph row with `{start, end}` after the block is complete.
- [ ] Emit `span_source = "direct"` for source-backed spans, `span_source = "derived"` for spans computed from child node spans, and `span_source = "synthesized"` for zero-width fallback spans.
- [ ] Emit an `AMBIGUITY` divergence entry when paragraph spans are not direct.
- [ ] Add source-attribution classification for the Melos final attribution as `classification = "heuristic"` unless source inventory provides a direct marker.
- [ ] Emit `source-note` nodes and source-note paragraph rows for classified source attribution.
- [ ] Add unit tests for:
  - [ ] paragraph range ordering,
  - [ ] paragraph ranges within `nodes[]`,
  - [ ] source-note node schema validation,
  - [ ] source-note exclusion from body paragraph role.

## Task 6: Measurement Refresh

- [ ] Run the full conversion audit with the new schema:

```bash
just aat-to-parser-ir-full-audit JOBS=24
```

- [ ] Re-run the Melos structural probe:

```bash
just melos-structural-probe JOBS=24
```

- [ ] Re-run the TEI-EAJ structural expansion:

```bash
just tei-eaj-structural-expansion JOBS=24
```

- [ ] Check that parser-IR gap rows fall for rows where adapter paragraph evidence exists.
- [ ] Check that adapter gaps and evidence gaps remain separately classified.
- [ ] Commit refreshed Markdown reports only; keep generated corpus outputs outside git.

## Task 7: ABC Sync After Measurement

- [ ] Write an ab-validator-to-ABC sync note with:
  - [ ] new mapping hash,
  - [ ] new parser-IR schema hash,
  - [ ] conversion audit summary,
  - [ ] Melos structural probe deltas,
  - [ ] TEI-EAJ structural expansion deltas,
  - [ ] remaining adapter/evidence gaps.
- [ ] Send the sync note to ABC.
- [ ] ABC updates the compatibility registry only after the ab-validator measurement evidence cites the new schema and mapping hashes.

## Verification

Before merging ab-validator changes, run:

```bash
git diff --check
cargo test -p ab-aat-to-parser-ir
tests/aat-to-parser-ir-cli-smoke.sh
tests/aat-parser-ir-mapping-policy-smoke.sh
```

For documentation-only updates, `git diff --check` plus a scan for unresolved markers over the touched docs is sufficient.
