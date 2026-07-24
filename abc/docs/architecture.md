# ABC Architecture

Date: 2026-07-04
Status: Accepted v0 contract surface
Version: 1.0.0

This document is the canonical architecture surface for decisions that are
accepted by ADRs or exercised by the v0 design-bundle gate. Earlier survey
material remains in `docs/high-level-architecture-note.md`; still-open
alternatives live in `docs/design-survey.md`.

## Promotion Contract

A surveyed alternative described in `docs/design-survey.md` is promoted into
this architecture document, and acquires implementation obligation, only when
one of two things is true:

- an ADR recording the decision is accepted, or
- a concrete consumer or test in the v0 design bundle forces the behavior, such
  as a fixture gate, schema check, or external contract.

Until then, surveyed items are documentation-only evaluation records and carry
no implementation obligation. When promotion occurs, update the relevant ADR
status, add an `Implementation Status` section if the bundle already exercises
it, and keep `nix run .#validate-design-bundle` green.

## Project Shape

ABC is the acceptance and publication side of a reproducible Aozora Bunko
corpus pipeline. Its central question is not which program converts one file,
but which exact corpus input, adapter/parser evidence, mapping document, TEI
profile, tokenizer, schema, and analysis recipe produced a given artifact.

The project makes derived corpus artifacts addressable, validated, queryable,
and reproducible. It uses ordinary files plus content-addressed manifests as
the durable contract, with RDF/PROV-O, TEI, Linked Art JSON-LD, IIIF
applicability, and tokenizer outputs as derived views or analytical layers.

## Glossary

- ArtifactID: `sha256:<hex>` identifier computed from RFC 8785/JCS bytes of
  `manifest_identity_object`; it is not the byte hash of the materialized
  output.
- Content hash: byte identity of a materialized output, recorded separately as
  `content.content_hash`.
- Manifest: canonical JSON record describing an artifact, its identity inputs,
  validation status, provenance, sidecars, and publication metadata.
- `manifest_identity_object`: the JSON object whose canonical bytes are hashed
  to produce the ArtifactID.
- Schema hash: SHA-256 over RFC 8785/JCS bytes of the bundled JSON Schema
  value used to validate a manifest or IR document.
- Profile hash: content hash of an output profile or format descriptor, such
  as a TEI ODD or canonical output-format specification.
- AAT: Aozora Adapter Tree, the adapter-side parse output owned by
  `../ab-validator`.
- Parser-IR: ABC's publication-side parser interchange format, accepted through
  compatibility gates and rendered into plaintext/TEI publication artifacts.

## Principles

1. Keep parsing, validation, artifact identity, and analysis as separate
   responsibilities.
2. Use content-addressed artifact manifests, not mutable labels, as the root of
   reproducibility.
3. Persist recipes and provenance before large derived outputs.
4. Treat materialized outputs as caches unless they are publication artifacts.
5. Prefer standard public boundaries: JSON, JSON Schema, TEI/XML,
   RDF/PROV-O, SHACL, Turtle/JSON-LD, Nix derivations, and plain corpus
   exports.
6. Stratify reproducibility requirements by layer: exact for deterministic
   parsing/rendering/tokenization, bounded or equivalent for model outputs, and
   approximate for exploratory generated responses.

## Manifest Identity

The canonical manifest is JSON. RDF/PROV-O, JSON-LD, Turtle, and publication
views are derived from the JSON manifest, not competing canonical identities.

Artifact IDs are computed as:

```text
ArtifactID = sha256(RFC8785-JCS(manifest_identity_object))
```

`manifest_identity_object` must not include `artifact_id`. Absent dimensions
are explicit `null` values so omission does not change identity.

The current identity dimensions are:

<!-- manifest-identity-coordinates:start -->
```text
manifest_schema_hash
corpus_snapshot_hash
work_content_hash
metadata_record_hash
parser_build_hash
parser_config_hash
aat_parser_ir_mapping_hash
parser_ir_schema_hash
tei_profile_hash
tokenizer_build_hash
tokenizer_dictionary_hash
tokenizer_profile_hash
analysis_recipe_hash
annotation_policy_hash
output_format_spec_hash
```
<!-- manifest-identity-coordinates:end -->

The mapping document hash is identity-bearing when parser-IR is AAT-derived.
The mapping schema hash is provenance, not an identity dimension. This follows
ADR 0023 and closes the reproducibility conflict where two mapping documents
could validate against the same schema but produce different parser-IR.

Schema hashes use the same JCS implementation as ArtifactID computation. Source
file whitespace or member order in a JSON Schema file does not change a schema
hash; changes to the parsed bundled schema value do.

## Pipeline Layers

ABC models the corpus as layered, content-addressed artifacts:

The drift-checked pipeline topology is the generated
[system architecture diagram](architecture.mmd), derived from
`architecture-stages.edn` and cross-checked against registered schemas and
ADRs. The diagram is a documentation view, not a competing source of truth.

Two audience-facing SVG figures from the JADH 2026 presentation remain
committed as frozen deliverables:

- [Soranoha Reproducibility Architecture](figures/soranoha-reproducibility-architecture.svg)
- [Soranoha Publication Pipeline](figures/soranoha-publication-pipeline.svg)

They are frozen presentation views, not architecture sources; their
reproducible rendering pipeline (font embedding, SVG sanitization, drift
gate) has been retired. Future presentation-quality figures build a graph
value, emit DOT with `abc.tools.diagram.graphviz/dot`, and render on demand
with `dot -Tsvg`.

Each layer records the identity inputs that can invalidate it. A text-only
change invalidates that work's downstream artifacts. A parser, mapping, TEI
profile, tokenizer, dictionary, metadata, schema, or analysis-recipe change
invalidates only the layers that depend on that coordinate.

Person-record split and merge events use lineage-only drift sidecars
(`_events/` and `_indexes/`) defined by ADRs 0020 and 0021. Drift events do not
rotate existing manifests; consumers that need current identity interpretation
traverse the validated event log.

## Parser Boundary

`../ab-validator` owns parser candidate execution, adapter measurement, AAT
extraction, parser comparison, and the executable AAT-to-parser-IR conversion.
ABC owns the accepted contracts: parser-IR schema, mapping/divergence schemas,
manifest identity, compatibility registry, validation, and publication
rendering.

The stable handoff is file-based. ABC does not require `../ab-validator` during
`validate-design-bundle`; it validates checked fixtures and materializes
accepted bundles locally.

The planned monorepo migration removes this physical checkout boundary, not the
logical producer/consumer roles. Monorepo naming and component labels are
tracked in `docs/handoffs/monorepo-component-boundaries.md`; they do not imply
an accepted `abc` namespace or vocabulary rename.

AAT JSON is the normative adapter-side contract. Parser-IR is the ABC
publication-side contract. The two are deliberately distinct and bridged by a
producer-owned mapping document plus ABC compatibility registry. Compatibility
entries are exact over adapter, adapter version, AAT version, mapping id,
mapping version, mapping document hash, mapping schema hash, parser-IR schema
id, and parser-IR schema hash. Wildcard adapter entries are invalid.

## Publication Rendering

ABC renders publication artifacts from parser-IR, not from Aozora source text.
Renderers are pure value transforms; file layout, manifest construction, and
sidecar assembly are materialization concerns.

The current parser-IR publication layer renders plaintext and TEI body output.
Renderer coverage is schema-derived and fails closed when parser-IR adds node
types. The current flat parser-IR cannot reconstruct all original paragraph
boundaries; the TEI renderer therefore emits a valid linear transcription and
records structural limits instead of inventing missing layout.

## TEI And XML Profile

`schemas/tei-profile.odd` is the canonical TEI profile contract. Project Relax
NG and Schematron artifacts are reproducibly derived from that ODD, and the
bundle gate validates both structural and business-rule layers. Upstream
`tei_all.rng` remains a TEI P5 compatibility baseline, not the only TEI
validation target.

TEI validation failures are failed artifacts, not late manual discoveries.
The validation result sidecar records Relax NG and Schematron outcomes by
layer and rule ID.

## RDF, Linked Art, And IIIF

The canonical manifest can be materialized as deterministic RDF/PROV-O and
checked with SHACL. Linked Art JSON-LD is a derived cultural-heritage
publication view, not a competing identity system. The ABC JSON-LD context is
self-contained and must not fetch remote contexts during validation.

IIIF applicability is a separate publication-side record. Text-only v0 works
remain valid without IIIF; applicability records state when IIIF is applicable,
not applicable, or blocked by rights/source constraints.

## Metadata And Temporal Modeling

Work metadata records and separated person records are identity-bearing inputs
for metadata-dependent artifacts such as TEI headers and RDF views. Person
records may change independently of work text; downstream invalidation follows
the referenced record hashes.

Bibliographic dates use the accepted EDTF lexical subset and parser-side
normalization rules from ADRs 0015 and 0016. Precision-honest RDF values are
emitted where XSD can represent them; broader EDTF values are echoed with the
ABC EDTF datatype rather than coerced into false precision.

## Tokenization And Analysis

ABC does not publish one authoritative tokenized Aozora Bunko. Tokenizer
outputs are parallel analytical artifacts tied to source identity, parser or
publication output identity, tokenizer build, dictionary/profile, normalization
policy, and analysis recipe.

Tokenization belongs in the exact reproducibility tier only when the tokenizer
build, dictionary archive, locale-sensitive settings, and configuration are
pinned and empirically deterministic. Otherwise it is treated as a stable or
bounded analytical layer.

## Nix And Materialization

Nix is a recipe and materialization backend, not the source of corpus-delta
logic. The accepted v0 surface exposes bounded tools and checks through flake
apps. ADR 0003 remains Draft because the bounded-workset policy and cost
envelope for smoke-corpus/full-corpus materialization have not yet been
measured and accepted.

Development builds may use local external corpus paths. Publication manifests
must not be produced from an impure local path unless the source is first
reduced to a fixed-output or content-addressed snapshot whose hash is recorded.

The default future bias is an external manifest/index that asks Nix to realize
a bounded work set, batch, and profile combination. Nix should not eagerly
evaluate a full `works x parsers x profiles x tokenizers x analyses` matrix.

## Operational Surface

The current v0 operational surface is files plus CLI commands. No database
service is required to produce or validate the example bundle. Run summaries,
failure manifests, manifest indexes, and validation sidecars are ordinary
files.

Single-host atomic publication can use temporary paths and rename. Multi-host
writers require a future compare-and-swap or database/object-store publication
protocol and are out of scope for the accepted v0 contract.

## Observability And Retention

Batch runs should emit machine-readable summaries: works considered, works
changed, parse/render/validation failures, warning/error counts by code,
artifact counts and sizes, durations, cache hit rates, and tool versions.

Retention tiers are:

- Hot: local development outputs, freely rebuildable.
- Warm: shared cache for common artifacts.
- Cold: recipes, manifests, source locks, and generated indexes.
- Archived: public release manifests, schemas, canonicalization fixtures, and
  source/archive identifiers retained indefinitely.

Failure manifests are retained for accounting even when failed outputs do not
exist.

## Release Trust

Content addressing detects accidental drift; it does not prove a release came
from a trusted publisher. ADR 0004 remains Draft. Public releases need a
documented verification path covering signatures or Sigstore bundles,
provenance attestations, subject hashes, source/archive identifiers, and
license metadata.

Local v0 development artifacts remain unsigned.

## Validation Gates

The canonical local gate is:

```bash
nix run .#validate-design-bundle
```

The gate validates JSON Schemas, parser-IR fixtures, imported ab-validator
output, materialized manifests, manifest indexes, RDF views, SHACL, metadata
and person bundles, person drift sidecars, TEI project RNG/Schematron, Linked
Art, IIIF applicability, and canonicalization fixtures. Focused Clojure tests
are exposed through the flake checks.

CI should validate boundaries without attempting a full corpus build. Larger
parser-performance, tokenizer, bounded-workset, and release-security checks are
scheduled or release-candidate gates until their ADRs are accepted.

## Legacy Code Mapping

The legacy Clojure namespaces remain design evidence. Active v0 tooling lives
under `abc.tools.*`. Parser execution and comparison have moved to
`../ab-validator`; ABC's local parser-era code should not be treated as the
canonical future parser without a new ADR.

## Core References

- ADR 0001: Manifest Identity
- ADR 0006: v0 Design Bundle Validation CLI
- ADR 0007: External Parser Validation Boundary
- ADR 0008: ABC Tools Runtime
- ADR 0009: Imported Parser Output Materialization
- ADR 0010: Manifest Identity Hardening
- ADR 0012: TEI ODD, Relax NG, and Schematron Validation
- ADR 0013: Cultural-Heritage LOD Publication Profile
- ADR 0014: IIIF Applicability
- ADR 0015 and ADR 0016: Temporal Modeling and EDTF Level 1
- ADRs 0020-0022: Person Identity Drift
- ADR 0023: Owned AAT to Parser-IR Mapping and Compatibility Registry
- ADR 0024: Parser-IR Span Semantics and Ruby Direction
- ADR 0025: Parser-IR Publication Rendering
- RFC 8785 JSON Canonicalization Scheme
- JSON Schema Draft 2020-12
- W3C PROV-O
- W3C SHACL
- TEI P5
- Software Heritage persistent identifiers
