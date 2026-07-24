# Parser-IR Publication Rendering

## Implementation Status

Accepted and implemented for the v0 publication slice.

- `src/abc/tools/parser_ir_tei.clj` renders parser-IR values into a TEI body.
- `src/abc/tools/parser_ir_plaintext.clj` renders plaintext from the same
  parser-IR boundary.
- `src/abc/tools/materialize_publication.clj` adds the TEI header, writes
  plaintext and TEI files, runs project Relax NG and Schematron validation, and
  writes artifact manifests plus `tei-validation-result.json`.
- `nix run .#validate-design-bundle` checks the committed example publication
  outputs and TEI validation gates.

TEI generation therefore belongs to the publication-rendering layer. It is
downstream of parser-IR compatibility admission, upstream of query-pack
indexing, and orthogonal to the XTDB removal/query-runtime decision.

## Context

`../ab-validator` is the producer-side home for parser execution, candidate
comparison, AAT extraction, and parser-IR export. ABC consumes the exported
parser-IR as a downstream publication boundary and must not rebuild that
producer logic locally.

The publication layer also has a different job from artifact materialization.
Renderers should transform values into publication content, while file layout,
manifest assembly, and other materialization concerns stay separate.

## Decision

ABC renders publication artifacts from parser-IR, not from Aozora source text.

`../ab-validator` owns parser comparison, parser implementation, AAT
extraction, and parser-IR export.

ABC renderers are pure value transforms. The TEI body renderer is one of those
transforms.

File materialization and manifest construction are separate from renderers.

The current flat parser-IR cannot reconstruct original paragraph boundaries
with full fidelity. The first TEI body renderer therefore emits a valid linear
transcription and records structural limitations instead of inventing missing
layout detail.

Renderer coverage is schema-derived and fails closed when parser-IR adds node
types.

ABC uses the TEI-EAJ `aozora_tei` Level 2-5 vocabulary when discussing TEI
depth. The current publication renderer is a source-preserving Level 2-oriented
renderer with native TEI ruby and validation gates; generated Aozora ruby uses
`type="furigana"` as the default jpn_classical-aligned heuristic while parser-IR
remains the place to distinguish future non-furigana ruby classes. The renderer
must not be described as Level 3 for prose works until parser-IR carries
paragraph boundaries and source attribution/source-note blocks as
producer-preserved structure.

Japanese TEI markup-style decisions should cite the TEI-EAJ Japanese Guidelines
wiki (`https://github.com/TEI-EAJ/jp_guidelines/wiki`) when they depend on
Japanese TEI convention rather than only on ABC pipeline mechanics.

`TEI-EAJ/aozora_tei` is the canonical external comparison corpus for this
distinction. ABC pins it as the non-flake input `tei-eaj-aozora-tei`, and the
all-work and focused Melos comparison reports are documented in
`docs/handoffs/tei-eaj-aozora-comparison.md`. The all-work report treats
missing ABC counterparts as materialization backlog; it does not collapse
coverage gaps into parser or renderer mismatches.

## Consequences

ABC can validate and publish parser-IR-derived artifacts without executing
parser candidates locally.

The publication pipeline remains sensitive to parser-IR schema change: new
node types require renderer coverage updates before ABC can publish them.

Because renderers are pure transforms, materialization and manifest assembly
can evolve independently from the TEI body renderer logic.

## Historical Evidence

The former Level-3 condition and focused test inventory described historical
implementation status; neither is a current accepted observation.

## Rollback

If the parser-IR-derived publication boundary proves insufficient, keep the
renderer and materialization command names stable and replace
implementation internals behind the same Nix app / Clojure entry points. Do
not reinterpret publication artifacts rendered under this ADR under a different
renderer contract; supersede this ADR and record a new renderer/schema hash
coordinate instead.
