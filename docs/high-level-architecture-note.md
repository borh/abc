# ABC High-Level Architecture Note

Date: 2026-04-26
Status: Draft RFC
Version: 0.5

This note captures a direction for ABC while the language, storage, parser,
and pipeline choices are still open. It is intentionally high-level, but not
choice-free. The goal is to separate durable project responsibilities from
current implementation details and to make future choices testable against
real alternatives.

## Status and Scope

This is a draft architecture note, not an adopted implementation plan. It
commits to durable boundaries and near-term design deliverables; it does not
commit to a final programming language, parser, database, ontology stack, or
workflow engine.

Architecture decisions should be promoted from this note into ADRs as they
become concrete. Each ADR should record the decision owner, alternatives
considered, acceptance criteria, and rollback path. Open choices close when a
candidate passes agreed boundary tests: manifest validity, IR round-trip,
TEI validation, incremental rebuild behavior, and a small end-to-end artifact
bundle.

The note version tracks RFC text only, not artifact compatibility. Minor
versions may clarify or supersede recommendations; ADRs record adopted
decisions and should explicitly supersede earlier ADRs when direction changes.

## Glossary

- ArtifactID: `sha256:<hex>` derivation-coordinate identifier computed from
  the canonical `manifest_identity_object`; it is not the byte hash of the
  materialized output.
- Content hash: byte identity of a materialized output, recorded separately as
  `content.content_hash`.
- Manifest: canonical JSON record describing an artifact, its identity inputs,
  validation status, and provenance hooks.
- `manifest_identity_object`: the JSON object whose RFC 8785 JCS bytes are
  hashed to produce the ArtifactID.
- Schema hash: SHA-256 over RFC 8785 JCS bytes of the bundled schema JSON value
  used to validate a manifest or IR document.
- Profile hash: content hash of an output profile or format descriptor, such
  as a TEI ODD or canonical output-format specification.
- Parser IR: language-neutral JSON interchange format emitted by, or adapted
  from, an Aozora parser before TEI/RDF/tokenization layers.

## Project Shape

ABC should be treated as a reproducible Aozora Bunko corpus pipeline rather
than only as a converter. The central question is not "which program converts
one file?", but "which exact corpus input, parser, TEI profile, tokenizer,
schema, and analysis recipe produced this artifact, and how can that artifact
be validated or rebuilt?"

The project should make derived corpus artifacts addressable, validated,
queryable, and reproducible.

## Principles and Policies

1. Keep parsing, validation, artifact identity, and analysis as separate
   responsibilities.
2. Use content-addressed artifact manifests, not mutable labels, as the root
   of reproducibility.
3. Persist recipes and provenance before large derived outputs.
4. Treat materialized outputs as caches unless they are publication artifacts.
5. Keep implementation language choices replaceable until each boundary is
   better understood.
6. Prefer standard formats at boundaries: XML/TEI, RDF/PROV-O, JSON, JSON
   Schema, Turtle, Nix derivations, and plain corpus exports. EDN may remain
   useful internally for Clojure workflows, but should not be required at
   public boundaries.
7. Stratify reproducibility requirements by layer: exact for deterministic
   parsing and tokenization, statistically equivalent for ML outputs, and
   approximate for exploratory visualization or RAG responses.

## Existing Parser Ecosystem

The parser layer should not be treated as greenfield. Current candidates and
reference implementations include:

- `aozora-rs`: Rust library/CLI/GUI/WASM parser with an advertised
  performance claim around 25,000 pages per second on a large work. Treat this
  as a baseline to re-measure under ABC conditions, not as accepted evidence.
- `aozora-core`: Rust tokenizer/parser/gaiji-oriented core library.
- `aozora2`: Rust CLI for stripping or converting Aozora text.
- `aozora2html`: official Ruby converter to XHTML and the most important
  compatibility reference.
- `aozora-parser.js`: PEG-based JavaScript parser and useful grammar reference.
- `vscode-language-japanese-novel`: VSCode extension with a formal TextMate
  grammar (`novel.tmGrammar.json`) for Aozora-like markup; useful as a lexical
  classification reference alongside the PEG grammar.
- `pandoc-aozora-ruby`: Haskell Pandoc filter for Aozora ruby rendering;
  useful for comparing ruby output semantics.
- `canopy`: Java parser generator with an Aozora grammar example; relevant
  to maintainability and grammar-completeness evaluation.
- `narouconv` / `novel.js`: JavaScript parser for "narou" format, which shares
  markup conventions with Aozora Bunko; useful for cross-format comparison.
- `aozorabunko_html` and `aozora_json_scrape`: Ruby HTML converter and metadata
  scraper by takahashim; useful for HTML output comparison and metadata shape
  validation.
- Current ABC Clojure code: incomplete but useful as design evidence for
  metadata, annotation handling, plaintext extraction, tokenization, and TEI.

The parser decision should use must-pass criteria first, then tiebreakers.

Must-pass criteria:

- License and redistribution terms are compatible with ABC outputs.
- Ruby, gaiji, editor-note, and layout behavior can be tested and explained.
- The tool can emit, or be wrapped to emit, the project IR.
- It can run on the target corpus without unacceptable parse failures.
- Unsupported syntax is reported as structured warnings, not silently dropped.

Tiebreakers:

- Performance, measured locally against a fixed benchmark corpus.
- Release cadence and maintenance health.
- WASM/CLI/library availability.
- Ease of Nix packaging.
- Compatibility with `aozora2html` output for comparable constructs.

A new parser is justified only if existing tools fail must-pass criteria or
make the IR/provenance goals impractical.

## Manifest Specification

Every materialized artifact should be identified by a manifest whose important
components are content hashes. Human labels are useful metadata, but they must
not be the identity.

Initial manifest rules:

- Canonical format: the canonical manifest file is JSON. RDF/PROV-O,
  JSON-LD, and Turtle views are derived publication/query formats generated
  from the JSON manifest, not competing canonical identities.
- Schema language: new JSON schemas should target JSON Schema Draft 2020-12.
  If important consumers require draft-07, provide migration or compatibility
  tooling rather than lowering the canonical schema target.
- Hash algorithm: v0 uses SHA-256 with explicit `sha256:<hex>` identifiers.
  CID or multihash forms may be published as additional interoperability
  identifiers, but they are not the v0 ArtifactID format.
- Canonicalization: hash canonical bytes, not host-language data structures.
  Use RFC 8785 JCS for JSON manifests, XML C14N for XML content when XML is
  the hashed object, and sorted/canonicalized RDF serialization or normalized
  RDF datasets for graph hashes.
- Optional dimensions: represent absent dimensions explicitly as `null` in
  the canonical manifest schema. Do not let omission vs. null change identity.
- Schema version: include descriptive `manifest_schema_id` metadata and
  identity-bearing `manifest_schema_hash`. Each schema version is a distinct,
  hash-addressed artifact. Old manifests retain their old schema hash and
  remain valid identity records; backward compatibility means consumers can
  read multiple schema versions, not that new ArtifactIDs preserve old schema
  hashes.
- Format identity: hash a canonical output format/profile descriptor. Human
  labels such as `tei-minimal` are descriptive unless backed by that descriptor.
- Signatures: reserve an optional `signatures` array or detached signature
  convention, such as `artifact.manifest.json` plus
  `artifact.manifest.json.sig`, so publication signing can be added without
  redesigning release manifests.
- Supply-chain attestations: release manifests should be compatible with a
  later in-toto/SLSA provenance statement and Sigstore/Cosign-style
  verification bundle. These do not replace the ABC manifest, but can attest
  who built it, from which source, and under which release workflow.

Artifact IDs should be computed as:

```text
ArtifactID = sha256(JCS(manifest_identity_object))
```

The `manifest_identity_object` is a canonical JSON object containing the
identity fields below by name, with absent dimensions present as JSON `null`.
It must not include the artifact ID itself. `manifest_schema_hash` is computed
independently before the artifact manifest is hashed. For v0, schema hashes
are computed as RFC 8785 JCS over the bundled JSON Schema document, with no
external `$ref` resolution or default expansion during hashing. If schemas are
split across files, the bundle artifact is created first and its hash is the
identity-bearing schema hash.

Any arrays inside the identity object must have deterministic semantics before
JCS is applied. RFC 8785 sorts object keys and canonicalizes primitive JSON
values; it does not sort arrays. Either array order is part of the meaning and
specified by the schema, or the array is sorted by a stable key such as hash,
source span, or role before hashing. Non-identity arrays, such as signatures
or publication mirrors, should stay outside `manifest_identity_object` unless
the project intentionally wants them to change the artifact ID.

Fields of `manifest_identity_object`:

```text
{
  "manifest_schema_hash": "sha256:...",
  "corpus_snapshot_hash": "sha256:...",
  "work_content_hash": "sha256:...",
  "metadata_record_hash": "sha256:... | null",
  "parser_build_hash": "sha256:... | null",
  "parser_config_hash": "sha256:... | null",
  "parser_ir_schema_hash": "sha256:... | null",
  "tei_profile_hash": "sha256:... | null",
  "tokenizer_build_hash": "sha256:... | null",
  "tokenizer_dictionary_hash": "sha256:... | null",
  "analysis_recipe_hash": "sha256:... | null",
  "output_format_spec_hash": "sha256:..."
}
```

Some dimensions may be absent for a given output. Raw TEI does not need a
tokenizer coordinate; tokenized TEI does. Plaintext does not need an analysis
recipe; an authorship model does. Parser IR may not need a metadata record
hash if it is derived from text alone, but TEI headers, RDF, and downstream
metadata-dependent artifacts should include the metadata record hash.
Human-readable labels such as `tei-minimal`, `plaintext`, or `bert-layer-6`
belong in descriptive metadata unless they are backed by a canonical format or
profile specification hash.

`parser_build_hash` and `tokenizer_build_hash` should identify a reproducible
build input, not an arbitrary host-local executable. Acceptable choices are a
Nix derivation/output hash, or a canonical source + lockfile + build recipe
hash. A literal binary hash is acceptable only when the binary is
reproducibly built and its target triple/runtime assumptions are recorded.

Identity and non-identity manifest fields should remain separate:

| Field Kind | Examples | Identity Effect |
| --- | --- | --- |
| Identity input | work content, metadata record, parser build, config, schema, profile, tokenizer dictionary, output format spec | Participates in ArtifactID |
| Descriptive metadata | human label, title, author display name, release notes | Does not participate unless backed by a hashed input record |
| Publication/security metadata | signatures, mirrors, SWHIDs, CID aliases, release channel | Outside ArtifactID unless explicitly promoted by schema |
| Runtime observation | generatedAtTime, duration, host notes, warning counts | Outside ArtifactID; recorded for audit and diagnostics |

The manifest is the durable object. Large outputs can be rebuilt or cached.
The manifest should also be the root of a PROV-O lineage graph: it records the
entities used, the activity that generated the output, and the software agent
or recipe responsible for the derivation.

The RDF/PROV-O view should be generated by a deterministic manifest-to-RDF
mapping. The mapping implementation and mapping version should be recorded in
release metadata. For releases, the RDF view should be materialized alongside
the JSON manifest; for local development and query services it may be generated
on demand from the canonical JSON manifest and then indexed.

Minimal Turtle sketch. The `https://w3id.org/abc/` namespace is the provisional
stable project IRI base for v0 contract fixtures; publication may still require
an ADR or registry decision before external release.

```turtle
@prefix abc: <https://w3id.org/abc/> .
@prefix prov: <http://www.w3.org/ns/prov#> .
@prefix dcterms: <http://purl.org/dc/terms/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

abc:artifact-sha256-... a prov:Entity ;
  dcterms:format "application/tei+xml" ;
  prov:generatedAtTime "2026-04-26T00:00:00Z"^^xsd:dateTime ;
  prov:wasGeneratedBy abc:parse-and-render-sha256-... ;
  prov:wasDerivedFrom abc:ir-artifact-sha256-... .

abc:ir-artifact-sha256-... a prov:Entity ;
  dcterms:format "application/json" ;
  prov:wasGeneratedBy abc:parse-activity-sha256-... ;
  prov:wasDerivedFrom abc:work-sha256-... ;
  abc:hasWarningArtifact abc:warnings-sha256-... .

abc:warnings-sha256-... a prov:Entity ;
  dcterms:format "application/jsonl" ;
  prov:wasGeneratedBy abc:parse-activity-sha256-... .

abc:failed-artifact-sha256-... a prov:Entity ;
  abc:validationStatus "failed" ;
  abc:hasErrorArtifact abc:errors-sha256-... ;
  prov:wasGeneratedBy abc:parse-activity-sha256-... ;
  prov:invalidatedAtTime "2026-04-26T00:00:00Z"^^xsd:dateTime .

abc:parse-and-render-sha256-... a prov:Activity ;
  prov:used abc:ir-artifact-sha256-... ;
  prov:used abc:tei-profile-sha256-... ;
  prov:qualifiedAssociation [
    a prov:Association ;
    prov:agent abc:abc-software-agent-sha256-... ;
    prov:hadPlan abc:tei-render-plan-sha256-...
  ] .

abc:parse-activity-sha256-... a prov:Activity ;
  prov:used abc:corpus-snapshot-sha256-... ;
  prov:used abc:work-sha256-... ;
  prov:used abc:parser-build-sha256-... ;
  prov:used abc:parser-config-sha256-... ;
  prov:used abc:parser-ir-schema-sha256-... ;
  prov:qualifiedAssociation [
    a prov:Association ;
    prov:agent abc:parser-software-agent-sha256-... ;
    prov:hadPlan abc:parse-plan-sha256-...
  ] .
```

Manifest validation should have concrete schema checks, with JSON Schema as
the v0 build gate for the canonical JSON manifest. A first SHACL shape should
validate the RDF/PROV-O view derived from the canonical JSON manifest for
publication and query consumers, and should require each artifact entity to
have a content hash, format, generation activity, derivation source, schema
version, and validation status. The manifest schema should cover both
successful and failed artifacts, using a validation status field or
discriminated subtype.
Manifests for superseded artifacts should record replacement or invalidation
metadata, including `prov:invalidatedAtTime` where applicable, without deleting
the earlier manifest.

The JSON schema, SHACL shape, and RDF mapping must not evolve independently.
For v0, the JSON schema is the source of truth and the SHACL shape may be a
minimal hand-maintained publication check. Later tooling may generate SHACL
from the manifest schema or from a shared intermediate specification if manual
synchronization becomes a maintenance burden.

RDF canonicalization should use a named dataset canonicalization algorithm,
preferably RDFC-1.0. Blank nodes are acceptable in the derived RDF view for
compact structures such as qualified associations, but release hashes should be
computed over canonicalized datasets or skolemized graphs, not
serializer-dependent blank-node labels.

SHACL-DS should be watched, not adopted for v0. It may become relevant for
cross-graph validation once manifests are split across multiple named graphs
or releases, but it is currently an emerging research/tooling direction. The
first v0 check should remain ordinary SHACL over the derived RDF view for a
single artifact bundle.

## Packaging, FAIR, and Schema Evolution

The canonical manifest can stay small and identity-focused while publication
packages expose richer research metadata. RO-Crate should be evaluated as a
publication and exchange profile for ABC artifact bundles because it is based
on JSON-LD and Schema.org and is designed for packaging reusable research
objects. The evaluation should explicitly cover RO-Crate 1.2 Detached Crates
for publishing metadata separately from large data, Profile Crates for
declaring the ABC package/profile contract, and the stricter requirement that
entities have `@id`, `@type`, and reachability from the Root Data Entity. It
should not replace the canonical identity manifest unless it can preserve the
JCS/hash/null-dimension rules needed for stable artifact IDs.

FAIR alignment should be explicit:

- Findable: stable artifact IDs, optional CIDs, SWHIDs for archived software
  inputs, indexed metadata, and clear release manifests.
- Accessible: documented release bundles, query indexes, and ordinary file
  formats rather than database-only access.
- Interoperable: TEI, RDF/PROV-O, JSON Schema, SHACL, RO-Crate evaluation,
  and standard vocabularies where possible.
- Reusable: license metadata, SPDX license identifiers where possible,
  provenance, validation status, versioned schemas, and documented
  configuration for parser/tokenizer/TEI stages.

Schema evolution needs a registry pattern before public releases. Each schema
should have an ID, content hash, compatibility status, migration notes, and
test fixtures. Old schemas must remain addressable so old manifests can still
be validated. Backward-compatible additions preserve consumer readability
across schema versions; they do not make new artifacts reuse an old
`manifest_schema_hash`. Breaking changes should produce a new schema hash and,
if needed, migration tools that rewrite older manifests into the new schema
while preserving the old artifact identity as provenance.

CID/IPLD support should be investigated as an interoperability layer. The v0
identity rule remains SHA-256 over canonical JSON, but a release manifest may
also publish CID-style identifiers if the multicodec/content-type story is
clear for ABC artifacts.

## Pipeline Layers

```text
Aozora Bunko source
  -> source snapshot and work content identity
  -> Aozora parser IR keyed by text and parser inputs
  -> metadata join for TEI/RDF/downstream artifacts using hashed metadata records
  -> TEI / plaintext / RDF artifacts
  -> schema and profile validation
  -> tokenization and linguistic annotation
  -> features, embeddings, statistics, ML, visualization, RAG, publication
```

These layers define interfaces and responsibilities. They should not imply a
single language or runtime.

The metadata join connects parsed text artifacts with Aozora bibliographic
metadata: work ID, title, author/person records, role flags, publication dates,
copyright flags, source files, NDC classifications, and base-text references.
The Aozora work ID is the primary human-facing join key, but the manifest
should also record source path and content hash so path changes or repository
reorganization do not break artifact identity.

Metadata changes are their own input dimension. If Aozora updates an author
record, base-text reference, role flag, or date without changing text content,
text-derived parser IR can remain valid while TEI headers, RDF, and downstream
metadata-dependent artifacts receive new manifests.

Person-record splits, merges, and other bibliographic identity drift should be
recorded as metadata-version changes. Human-facing identity may change across
metadata snapshots; artifact identity remains tied to the exact metadata
record hash used at build time.

## Parser IR Boundary

The parser IR is the first major interface, but it should be designed with the
manifest in mind. At minimum, the IR needs to preserve:

- source spans and stable text offsets,
- raw text fragments,
- ruby base text and reading,
- gaiji references and resolved replacement characters,
- editor notes and layout annotations,
- emphasis, headings, indentation, page breaks, images, and captions,
- quotation markers where recoverable,
- parse warnings and unsupported constructs.

The interchange contract should start as JSON with a JSON Schema. JSON Schema
Draft 2020-12 is broadly usable from Rust, Clojure/JVM, Python, JavaScript,
and validation tools, and is sufficient for early manifests and regression
tests. Internal implementations may use typed ASTs, EDN, or other
representations, but those must round-trip through the JSON contract described
in the boundary-format policy above. If size or speed becomes a problem, CBOR,
Arrow, or another binary representation can be added as a secondary encoding
of the same logical schema.

Every parser IR document should include a self-describing schema reference,
such as `schema_id` and `schema_hash`, so consumers can validate the document
without relying only on the external artifact manifest.

Warnings and unsupported constructs should not be lost to logs. The parser
should emit a sidecar warning artifact, preferably JSON Lines keyed by source
span and warning code. The parser IR manifest should reference that warning
artifact so warning changes can be compared even when rendered TEI is
unchanged.

The parser IR schema should define a small error taxonomy for programmatic
handling: severity (`fatal`, `error`, `warning`, `info`), stable error code,
message, source span where available, affected construct, and recovery action
if one was taken. Corpus-level reports should aggregate by severity, code,
parser version, and work ID so parser changes can be reviewed at scale.

Parser IR is whole-document by default for v0. Chunked or streaming IR is
reserved as a future schema extension; large-file memory behavior should be
measured in the v0/v1 cost benchmarks before committing to a streaming design.

## Aozora-Specific Design Concerns

Several Aozora-specific policies need to become explicit schema/config inputs:

- Gaiji resolution: preserve the original marker, parsed JIS X 0213 or other
  reference, selected Unicode replacement, IVS information where available,
  image or glyph fallback, and unresolved status. The gaiji mapping table and
  resolution policy must be hashed as parser or rendering configuration.
  Authoritative sources for replacement selection include the Mojikiban
  (文字情報基盤) database and the CID NINJAL kana reference; see
  `references/character-normalization-research.md`.
- Ruby scope: preserve whether ruby scope was explicit with `｜`, inferred
  from preceding character classes, group ruby, mid-word ruby, nested or
  ambiguous ruby, and the exact source span used for the base text.
- Editor-note taxonomy: classify notes such as emphasis, headings, indentation,
  page breaks, images, captions, external characters, accent marks, and
  miscellaneous editorial instructions. Unknown notes should be structured
  warnings rather than discarded strings.
- Bibliographic drift: keep Aozora work ID and person ID as source identifiers,
  but also hash the relevant metadata record used for an artifact. Metadata
  edits should not masquerade as text edits. External bibliographic databases
  such as JBDB (`https://jbdb.jp/`) may be used for cross-validation or
  enrichment, but they do not replace the hashed metadata record as an identity
  input.
- Text normalization: record encoding, line-ending normalization, Unicode
  normalization, Japanese metadata normalization policy, and front/back matter
  policy as explicit configuration. NFC vs. NFKC choices for Japanese names
  and bibliographic strings affect `metadata_record_hash` and must not be
  implicit. Combining-dakuten kana generation and precomposed-vs.-sequence
  policy should be documented; see `references/character-normalization-research.md`.

## TEI and XML Profile

The TEI layer should own XML generation, TEI profile selection, and schema
validation. A minimum profile should be constrained enough to validate
reliably, but aligned with current Japanese TEI practice.

Anchors for the profile:

- TEI P5 release line, pinned per artifact through the TEI profile hash.
  Current upstream at the draft date is TEI P5 4.11.0; native `ruby`, `rb`,
  and `rt` support was introduced in the 4.2.0 "Ruby" release.
- TEI East Asian/Japanese SIG guidance.
- TEI-EAJ `jp_guidelines` and, where relevant, `jpn_classical`, treated as
  draft/trial community guidance rather than a frozen standard.
- TEI P5 Chapter 5, "Characters, Glyphs, and Writing Modes", for gaiji and
  character/glyph representation policy.
- ABC's earlier TEI output as reference behavior, not as a final target.

The old ABC TEI path should be reassessed for:

- validity against a concrete Relax NG schema,
- interoperability with TEI-EAJ conventions,
- treatment of ruby, gaiji, notes, and layout,
- separation of source transcription from linguistic enrichment,
- provenance and revision metadata.

The validation strategy should be explicit:

- Generate or select a TEI ODD/profile.
- Build the Relax NG schema from that profile.
- Validate TEI artifacts with Jing, either through the JVM API or an external
  `jing` command.
- Make validation a pipeline stage, so invalid TEI is a failed artifact rather
  than a late manual discovery.

Native Rust Relax NG support does not currently appear comparable to the
JVM/Jing ecosystem. That makes a JVM validation stage useful even if parsing
and other stages are Rust-based.

Jing should be the compatibility baseline, but v0 should also evaluate a
lighter runtime path before committing to a JVM in every validation loop.
`libxml2`/`xmllint` supports Relax NG validation in XML syntax and may be
adequate for the constrained TEI profile. It can replace or complement Jing
only if it validates the generated Relax NG schema correctly, reports useful
errors for ABC artifacts, and passes the same TEI fixture corpus.

The minimum profile should be documented as a TEI ODD and published with the
project so other Aozora pipeline implementations can validate against it. If
the profile diverges from TEI-EAJ guidance, the divergence should be explicit
and justified in the ODD documentation. Because TEI-EAJ guidance is still
evolving, ABC should document the exact conventions it adopts and keep a small
divergence/change note when upstream guidance changes.

## Ontology, RDF, and Querying

The manifest layer should commit to RDF/PROV-O as the provenance publication
format. That does not require committing immediately to OWL reasoning.

Open choices:

- Manifest storage: plain Turtle files, JSON-LD, an RDF store, or generated
  indexes over manifest files.
- Query interface: SPARQL if graph queries are central; simpler file/SQLite
  indexes if most access is coordinate lookup.
- Validation: SHACL may be enough for manifest shape validation; OWL DL
  reasoning should be required only if concrete inference needs appear. SHACL
  Advanced Features may be considered later for cross-artifact or temporal
  rules, but should not be a v0 dependency.
- OWL implementation: Horned-OWL is a serious Rust candidate; OWLAPI/Tawny
  remains a JVM candidate; RDF-only may be enough for the first version.
- PROV extensions: PROV-DICTIONARY and PROV-LINKS are W3C Notes with limited
  tooling, so treat them as exploratory only. PROV-DC mappings may help with
  Dublin Core-aligned publication metadata.
- Ontology packaging: if ABC publishes reusable vocabulary or TEI ODD profiles,
  evaluate distribution mechanisms such as Plow.pm or versioned Nix packages.
  See `references/ontology-research.md`.

The current Turtle output in ABC is useful but not sufficient by itself. The
redesign needs provenance, artifact identity, and version coordinates, not
only bibliographic triples.

Query storage should be selected from access patterns rather than from tool
preference:

| Query Need | Likely Storage |
| --- | --- |
| Lookup artifact by coordinate or hash | SQLite or file index over manifests |
| Traverse provenance chains | SPARQL over Turtle, Apache Jena, RDF store, or XTDB |
| Query corpus evolution over time | XTDB v2, Dolt-like store, or custom snapshot index |
| Full-text search over TEI/plaintext | SQLite FTS, OpenSearch, Tantivy, or similar |
| Analytical scans over tokens/features | Arrow/DataFusion, Parquet, DuckDB, or columnar files |

The first implementation can start with manifest files plus a generated SQLite
index if graph traversal is not yet central. The query runtime can then be
promoted by ADR when access patterns require it. These storage choices are not
mutually exclusive; the same manifest set can feed a SQLite coordinate index,
an RDF graph for provenance, and columnar outputs for analytical scans.

## Corpus Evolution

Aozora Bunko evolves through new works, corrections, metadata changes, and
source reorganization. The pipeline should therefore process deltas rather
than rebuilding everything by default.

Suggested layer model:

```text
Layer 0: raw corpus snapshot and work content hashes
Layer 1: parser IR, keyed by Layer 0 + parser build hash + config
Layer 2: TEI/plaintext/RDF, keyed by Layer 1 + profile hash
Layer 3: tokenized artifacts, keyed by Layer 2 + tokenizer/dictionary hash
Layer 4: features, embeddings, models, visualizations, and RAG indexes
```

Incremental update rules:

- If only one work changes, invalidate artifacts derived from that work.
- If parser build inputs change, invalidate parser IR and downstream layers
  using that parser build hash.
- If a TEI profile changes, preserve parser IR but rebuild TEI and downstream
  artifacts.
- If tokenizer or dictionary changes, preserve TEI/plaintext but rebuild
  tokenized and downstream artifacts.
- If analysis code changes, preserve corpus artifacts and rebuild only the
  affected analysis layer.

This can be implemented with Nix derivation dependencies, DVC-style dependency
graphs, a content-addressed store, or a database/index. The architecture should
preserve the invalidation model even if the first implementation is simple.

## Concurrency and Programmatic Access

Work-level parsing, rendering, validation, and tokenization should be treated
as embarrassingly parallel after the source snapshot, schema set, and manifest
index are fixed. Shared inputs are read-only; outputs are immutable and
content-addressed. Concurrent workers should write manifests and sidecars to
temporary paths and atomically publish them by rename or content-addressed
store insertion. If two workers produce the same hash, that is success; if they
produce different hashes for the same identity object, that is a reproducible
build failure to investigate.

Cache access needs simple concurrency rules before distributed execution:
readers may share released artifacts freely, writers must not mutate existing
manifests in place, and any mutable index over manifests should use locking or
transactional updates. A generated SQLite index can be rebuilt from manifests;
the manifest files remain the source of truth.

Those rules assume a single-host or single-writer publication step. Multi-host
execution on a CI farm or batch cluster needs a stronger protocol: object-store
compare-and-swap, database-backed publication, or another content-addressed
store with explicit multi-writer semantics. Defer that distributed story to the
API/runtime ADR rather than relying on rename semantics across hosts.

For v0, the programmatic API should be the file bundle, JSON schemas, and a
CLI-oriented workflow. REST, gRPC, authenticated query endpoints, streaming
APIs, and distributed scheduling should be deferred to an API/runtime ADR after
real query and batch-processing needs are known.

## Runtime Orchestration

The runtime stack should be kept deliberately small for v0. Rust parsing,
JVM/Jing validation, RDF tooling, Nix, and ML runtimes may all be useful, but
the first implementation should avoid requiring all of them for the smallest
artifact bundle.

If the JVM remains part of TEI validation, the pipeline should not start one
JVM process per work across the full corpus. Candidate patterns are: batch TEI
validation in one process, a long-running JVM worker/daemon, or a validation
service invoked by the orchestrator. The same rule applies to other expensive
runtimes: amortize startup costs across batches, and record the batch/work
membership in run summaries so failures remain attributable to individual
works.

The default v0 orchestrator can be a simple CLI or script that coordinates
manifest creation, parser invocation, validation, and index generation. A
heavier workflow engine should be adopted only after the v0 bundle exposes
coordination problems that a simpler script cannot handle.

## Observability and Retention

Batch runs should emit machine-readable run summaries rather than only console
logs. Minimum metrics: works considered, works changed, parse failures,
validation failures, warnings by code/severity, artifact counts and sizes,
parse/render/validation duration, cache hit rate, Nix evaluation time, and
tool versions. These metrics should be attached to benchmark runs and release
candidate validation so performance regressions are visible.

Progress reporting should work at corpus scale: per-work status, aggregate
counts, and final failure manifests should be available without reading every
large output. Alerting is not a v0 requirement, but release validation should
fail on unexpected fatal errors, schema-invalid manifests, invalid TEI where
valid TEI is required, or missing required sidecars.

Retention should align with the materialization tiers:

- Hot: local development outputs may be deleted whenever they are rebuildable.
- Warm: shared caches keep common artifacts subject to storage budgets.
- Cold: recipes, manifests, and source locks are retained while outputs may be
  regenerated.
- Archived: public release manifests, schemas, canonicalization fixtures, and
  source/archive identifiers are kept indefinitely.

Failure manifests should be retained for accounting even if failed outputs do
not exist. The corpus is primarily public bibliographic/text data, but privacy
and deletion policy should be revisited before importing external annotations,
user-contributed corrections, or non-public metadata.

Loose JSON/XML/sidecar files are acceptable for the single-work v0 bundle and
small smoke corpora, but not necessarily for full-corpus publication. At
full-corpus scale, using the pinned snapshot's work count and several sidecars
per layer, inode count and directory traversal can become material costs.
Full-corpus releases should evaluate batched storage for manifests and
sidecars: SQLite, a key-value store, or archive files such as tar/zip with a
generated index. The storage choice must preserve content-addressed identity
and allow manifests to be exported as ordinary files for review.

## Nix and Materialization

Nix is a good fit for deterministic artifact recipes, but the derivation space
must be treated carefully. The Aozora repository is large, and a naive
`fetchgit` or flake input can copy a large corpus snapshot into the Nix store.
The corpus input policy should be explicit:

- Published releases should use fixed-output or content-addressed snapshots
  with recorded hashes.
- Local development may use an external checkout path or filtered tree to
  avoid repeatedly copying the full corpus into the store.
- Work-level source extraction should minimize the Nix input closure where
  possible.
- Critical upstream sources should be archived to Software Heritage or a
  project-controlled mirror: corpus snapshots, dictionary archives, schema
  sources, parser releases, and TEI profiles.
- Archive triggers should be explicit: on public release, dependency update,
  parser release adoption, dictionary/profile change, and benchmark corpus
  freeze. Record SWHIDs or mirror hashes in manifests, and verify archived
  content against local content hashes before publishing a release.

A rough matrix can become large quickly:

```text
~17,800 works, depending on the pinned Aozora snapshot
  x parser variants
  x TEI profiles
  x tokenizer/dictionary combinations
  x output formats
  x analysis recipes
```

Any published cost estimate should name the Aozora snapshot date/hash used for
the work count, rather than treating 17,800 as timeless.

Cost envelopes should be measured early. A first benchmark should report
corpus input size, manifest index size, parser IR size per work, TEI size per
work, tokenized output size per work, parse time, TEI render time, validation
time, and Nix evaluation time for representative subsets. Those measurements
should decide batching and cache policy before broad materialization. The v0
single-work artifact bundle should still record these metrics for its example
work so later v1 benchmarks can compare against real baseline numbers.

The full Cartesian product should not be evaluated or materialized eagerly.
The default granularity should be work-level for correctness and cache reuse,
but not as one eagerly evaluated attrset containing every possible work and
variant. Candidate strategies:

- Generate derivations on demand from a work manifest and requested profile.
- Batch by author/card/release for publication builds when evaluation overhead
  dominates.
- Use a separate manifest/index to discover changed works, then ask Nix to
  build only the requested subset.
- Benchmark evaluation time and memory before committing to a matrix layout.

This acknowledges the tradeoff: one derivation per work gives precise
invalidation but can be expensive to evaluate; batch derivations evaluate
more cheaply but rebuild more than necessary; an external incremental index
can decide the subset that Nix should realize.

The external incremental index should be the default design bias. Nix should
receive an explicit build request for a bounded work set, batch, and profile
combination; it should not be asked to evaluate the full corpus delta or a
dynamic `works x parsers x profiles x tokenizers` matrix on every change.
Evaluation time and peak memory are acceptance criteria for the Nix ADR, not
implementation trivia.

The intended materialization model is virtual:

- Hot: local outputs used repeatedly during development.
- Warm: shared binary cache for common public artifacts.
- Cold: derivations or recipes that can be rebuilt when requested.
- Archived: publication releases with durable storage and stable manifests.

The design should also account for derivation churn. Nix can garbage-collect
outputs, but old recipe metadata and manifest indexes need a retention policy.
Release manifests should be kept indefinitely; exploratory derivations can be
expired or regenerated from pinned source locks.

`flake.lock` should pin corpus snapshots, parser source, tokenizer packages,
dictionary archives, schema/profile sources, and analysis code where exact
rebuilds matter. It should also record license and redistribution metadata for
corpus snapshots, dictionaries such as UniDic/IPADic, parser dependencies, and
released derived artifacts.

## Experiment and ML Reproducibility

Downstream research has different reproducibility requirements from parsing.
The pipeline should not promise byte-identical reproduction for stochastic ML
outputs when the scientific requirement is equivalence.

Recommended strata:

| Layer | Requirement | Examples |
| --- | --- | --- |
| Exact | Byte- or structure-identical | parser IR, TEI, plaintext, pinned deterministic tokenization |
| Stable IR | Cache stable intermediate vectors/features | character n-grams, stylometric features, BERT layer 6 or other selected embeddings, validated by exact match or cosine similarity |
| Bounded | Quantify sensitivity | embedding perturbation bounds, tokenizer/model sensitivity, factorial ANOVA |
| Equivalent | Statistical equivalence | classifiers, authorship attribution, genre models, TOST or metamorphic testing |
| Approximate | Retrieval-exact, generation-recorded | visualizations, RAG responses |

Experiment manifests should include feature extraction recipes, random seeds,
model versions, hardware/runtime notes when relevant, and validation criteria.
DVC or MLflow may be useful here, but they should attach to the artifact
manifest rather than replace it.

Tokenization belongs in the Exact tier only when the tokenizer build,
dictionary archive, locale-sensitive settings, and configuration are pinned in
the manifest and the tokenizer is empirically deterministic. Tokenizers that
cannot satisfy that bar should be treated as Stable IR or Bounded layers.
Known-good tokenizer configurations should be documented in an ADR or schema
appendix. At minimum, record tokenizer name/version, build/source hash,
dictionary archive hash, build options, locale/encoding settings,
thread/concurrency settings, and a deterministic regression fixture.

For RAG, the repeatable part should be the fixed corpus artifact, chunking
policy, embedding model, index, retrieval query, and retrieved citation spans.
Generated answers can be approximate, but they should be linked to the
retrieval artifact and model configuration that produced them.

## Downstream Requirement Examples

The coordinate and manifest model should be able to support known Aozora
research use cases:

- Authorship attribution: tokenizer/dictionary, stylometric features,
  classifier recipe, train/test split, and equivalence criteria.
- Genre classification: feature extraction, POS/token model, vector model, and
  evaluation protocol.
- Speech synthesis or audio alignment: plaintext, reading/ruby policy, audio
  source metadata, aligner version, and alignment recipe.
- RAG systems: chunking strategy, citation spans, embedding model, index
  recipe, and retrieval configuration.
- Parallel corpora: source text, translated text source, sentence alignment
  algorithm, and alignment confidence.
- Publication visualization: corpus statistics, network graphs, or narrative
  analytics backed by reproducible artifact coordinates. Design precedents are
  collected in `references/visualization-design-research.md`.

These should not all be implemented first. They are pressure tests for whether
the manifest is expressive enough.

## Risks and Non-Goals

Failure modes should be first-class. A failed parse, failed TEI validation, or
failed tokenizer run should produce a failure manifest containing input
identity, software/config identity, warning/error sidecars, and validation
status. This makes parser improvements diffable and prevents failed works from
disappearing from corpus accounting.

Trust and release integrity need a policy separate from content hashes.
Content addressing detects accidental drift; it does not prove that a release
manifest came from a trusted publisher. Publication releases should eventually
be signed with a project key or another trusted release mechanism. Local
development manifests need not be signed.

Before the first public release, the project should define a security model:
trusted publisher keys, signature format, key rotation, revocation policy, and
how consumers verify release manifests. The security ADR should evaluate SLSA
v1.2 Build L2 as the first public-release target, evaluate Build L3, and assess
in-toto attestations for release provenance plus Sigstore/Cosign keyless
signatures or bundles as an alternative to long-lived project keys. This is not
required for local v0 experiments but should be a release gate.

Threat model:

- Accidental drift: content hashes, canonicalization fixtures, and schema
  validation detect changed inputs or non-deterministic serialization.
- Malicious manifest substitution: release signatures, trusted publisher
  policy, and transparency logs or attestations detect untrusted releases.
- Dependency compromise: pinned source locks, archived upstream identifiers,
  and supply-chain attestations make dependency changes reviewable.
- Reproducibility regressions: smoke corpora, failure manifests, and benchmark
  metrics detect changed behavior even when builds still complete.

License and redistribution checks are part of artifact publication. Aozora
texts, parser code, TEI profiles, dictionaries, tokenizers, embeddings, and ML
models may have different redistribution terms. A release manifest should
record license metadata, preferably SPDX identifiers where possible, for each
input and derived artifact.

Non-goals for an initial version:

- No general-purpose Aozora editor.
- No complete OWL reasoning requirement unless a concrete inference task
  appears.
- No eager materialization of the full parser/tokenizer/analysis matrix.
- No promise of byte-identical reproduction for stochastic ML generation.
- No replacement for upstream Aozora Bunko bibliographic governance.

## Candidate Runtime Roles

### Parser Runtime

The parser runtime should own Aozora grammar fidelity and produce the stable
parser IR. Rust remains a strong candidate, but existing Rust parsers must be
evaluated before building a new parser from scratch.

### TEI and XML Runtime

The TEI/XML runtime should own TEI rendering, profile management, and Relax NG
validation. The JVM remains attractive for Jing and TEI-adjacent XML tooling.

### Ontology Runtime

The ontology runtime should first satisfy RDF/PROV-O manifest needs. Horned-OWL
should be considered if the project needs Rust-native OWL 2 manipulation or
large ontology processing. OWLAPI/Tawny should be considered if JVM reasoning
and mature OWL tooling matter more. A SHACL-only manifest validation path
defers the OWL decision indefinitely until a concrete reasoning task appears.

### Reproducibility Runtime

The reproducibility runtime should own artifact recipes, dependency pinning,
and materialization policy. Nix is the leading candidate, with DVC-style graphs
remaining useful for experiment workflows.

### Query Runtime

The query runtime should be chosen after manifest shape and access patterns are
clear. Candidates include plain files plus indexes, SQLite, XTDB, RDF stores,
DataFusion/Arrow, or a hybrid.

## Current Repository Reading

The current Clojure code contains useful concepts, but not every namespace
should carry forward equally:

- Keep/lift: `abc.aozora` metadata normalization concepts, especially work,
  person, source, date, and NDC handling.
- Keep/lift: `abc.stats` as deterministic feature and stylometry reference,
  but move it downstream of reproducible corpus artifacts.
- Lift/replace: `abc.annotation` as reference behavior and test cases for
  ruby, gaiji, plaintext, and sentence handling. It should not remain the
  production parser if a Rust or external parser becomes canonical.
- Replace/redesign: `abc.tei` should be rebuilt around a published TEI ODD,
  TEI P5 ruby support, provenance, and validation-first output.
- Replace/redesign: `abc.load` should become manifest-aware ingestion and
  batch orchestration rather than ad hoc persistence.
- Retire or demote: `abc.xtdb` and `abc.git` as central architecture. They may
  remain useful experiments or query/cache backends, but they conflict with a
  manifest-first model if treated as the source of identity.

These should be treated as design evidence, not final architecture. Some code
may become reference behavior, some may be replaced, and some may be lifted
into a new language-neutral model.

## Validation and Testing Strategy

The project needs separate validation loops:

- Parser compatibility: compare parser output with `aozora2html`, current ABC
  behavior, curated tricky Aozora examples, and the Himawari corpus encoding
  where alternate markup conventions exist.
- Parser performance: benchmark against `aozora-rs` or any adopted parser.
- Parser warnings: compare warning sidecars across parser versions and require
  reviewed changes for new warning classes.
- Error taxonomy: require stable error codes and severity levels in parser IR
  and failure manifests, and aggregate them across benchmark corpora.
- TEI validity: validate generated XML against the selected Relax NG schema,
  using Jing as the baseline and any lighter validator only after fixture
  parity is demonstrated.
- Manifest validity: validate canonical JSON with JSON Schema, and validate
  the derived RDF/PROV-O publication view with SHACL.
- Incremental correctness: confirm that changing one work invalidates only
  that work's derived artifacts unless shared recipes changed.
- ML reproducibility: use exact checks for deterministic features and
  equivalence tests for stochastic outputs.

## CI/CD

Continuous integration should validate boundaries without attempting a full
corpus build. The default PR gate should use a small smoke corpus, roughly
100 representative works or fewer if CI time requires it, selected to cover
ruby scope, gaiji, editor notes, images/captions, metadata joins, validation
failure cases, and large-work behavior. The target runtime should be under
five minutes on a recorded CI runner class, CPU, RAM, OS, Nix version, cache
state, and corpus fixture hash; revise the corpus size against that constraint
rather than treating 100 as fixed. The exact list should be versioned and
hashed like other benchmark inputs.

The CI gate should run JSON Schema checks for manifests and parser IR,
canonicalization fixtures, parser warning/error taxonomy checks, TEI validation
for generated samples, deterministic RDF view generation, and the generated
manifest index. Larger parser-performance and full-corpus validation runs can
be scheduled jobs or release-candidate gates rather than required on every PR.

## Near-Term Design Questions

These questions should be answered by the v0 deliverables and ADRs below,
rather than as separate prose decisions:

| Question | Primary Deliverable or ADR | Boundary Test |
| --- | --- | --- |
| What is the manifest schema? | `manifest.schema.json`, `manifest.shacl.ttl`, manifest-to-RDF fixtures | JSON validity, RDF view determinism, failure-manifest fixture |
| What is the parser IR? | `parser-ir.schema.json`, parser evaluation ADR | IR round-trip, warning/error sidecars, source-span fixtures |
| What is the minimum TEI profile? | `tei-profile.odd` | Relax NG validation, documented TEI-EAJ convention choices |
| Which parser path is preferred? | Parser ecosystem ADR | Must-pass criteria, benchmark corpus, compatibility review |
| What must be queryable? | Example bundle query index, operational ADR | Coordinate lookup and provenance traversal examples |
| What is publication-grade output? | RO-Crate evaluation, security ADR, Nix/materialization ADR | Signed/releasable bundle shape and retention policy |
| What is the operational API? | Operational ADR, `ci-smoke-corpus.md` | CLI/file workflow, CI runtime target, deferred network API decision |

## Suggested Next Step

Produce a small v0 design bundle before wider implementation:

1. `manifest.schema.json`: JSON Schema for artifact manifests, including
   Draft 2020-12 schema versioning, explicit null dimensions, hashes,
   validation status, signatures placeholder, failure manifest subtype, and
   license metadata.
2. `manifest.shacl.ttl`: SHACL shape for the RDF/PROV-O view of the manifest.
3. Deterministic `manifest-to-rdf` mapping fixtures showing when the RDF view
   is materialized for releases and how it can be generated on demand in local
   development.
4. `parser-ir.schema.json`: JSON Schema for parser IR v0, including ruby,
   gaiji, editor notes, spans, structured warning references, and error
   severity/code taxonomy.
5. `tei-profile.odd`: minimum TEI ODD aligned with TEI P5 ruby support and
   explicitly selected TEI-EAJ draft conventions.
6. One end-to-end example artifact bundle for a single Aozora work: source
   manifest, parser IR, warning sidecar, TEI XML, validation result, RDF/PROV-O
   manifest, failure-manifest fixture or subtype example, and a small query
   index entry.
7. `canonicalization-fixtures/`: small JSON, XML, and RDF examples with
   expected canonical bytes or hashes for cross-implementation identity tests.
8. `ro-crate-evaluation.md`: decide whether ABC release bundles should be
   expressed as an RO-Crate 1.2 profile, including Detached Crates, Profile
   Crates, entity reachability rules, and which metadata belongs in RO-Crate
   vs. the canonical identity manifest.
9. `tokenizer-determinism.md`: document known-good tokenizer configurations,
   fixture expectations, dictionary hashes, locale/encoding assumptions, and
   the rule for demoting non-deterministic tokenizers out of the Exact tier.
10. `ci-smoke-corpus.md`: define the representative PR validation corpus,
    selection rationale, expected runtime, and required checks.
11. One ADR for parser ecosystem evaluation criteria, one ADR for Nix corpus
   input/materialization policy, one ADR for supply-chain release security
   covering SLSA/in-toto/Sigstore choices, and one short operational ADR for
   runtime orchestration, concurrency, API surface, storage packing,
   observability, and retention.

The ADRs should be drafted in parallel with the schemas rather than after all
schemas are complete. In particular, parser evaluation criteria should inform
the parser IR schema, and the Nix materialization ADR should record how v0
example metrics will scale to subset and release builds.

After those are explicit, the project can experiment with Rust, Clojure/JVM,
Nix, Horned-OWL, Jing, RDF tooling, and query stores without entangling all
decisions at once.

## References

Web references should be archived at note publication time, using Software
Heritage for source repositories or an archival URL such as the Wayback Machine
for ordinary web pages, and the archive URL should be recorded with the release
notes when it affects an adopted ADR.

- Horned-OWL: https://github.com/phillord/horned-owl
- Jing Relax NG validator: https://relaxng.org/jclark/jing.html
- RELAX NG software list: https://relaxng.org/
- libxml2 Relax NG API: https://gnome.pages.gitlab.gnome.org/libxml2/html/relaxng_8h.html
- W3C PROV-O: https://www.w3.org/TR/prov-o/
- Nix derivations: https://nix.dev/manual/nix/latest/expressions/derivations.html
- DVC pipelines: https://dvc.org/docs/user-guide/pipelines
- RO-Crate specification: https://www.researchobject.org/ro-crate/specification/1.2/
- RO-Crate 1.2 changes: https://www.researchobject.org/ro-crate/whats-changed-in-1-2
- RFC 8785 JSON Canonicalization Scheme: https://www.rfc-editor.org/rfc/rfc8785
- JSON Schema Draft 2020-12: https://json-schema.org/draft/2020-12
- RDF Dataset Canonicalization 1.0: https://www.w3.org/TR/rdf-canon/
- FAIR principles: https://www.go-fair.org/fair-principles
- SLSA: https://slsa.dev/
- in-toto Attestation Framework: https://github.com/in-toto/attestation
- Sigstore: https://docs.sigstore.dev/
- Cosign: https://docs.sigstore.dev/cosign/
- SHACL-DS: https://github.com/Ikeragnell/SHACL-DS
- Software Heritage persistent identifiers: https://docs.softwareheritage.org/devel/swh-model/persistent-identifiers.html
- Multiformats CID: https://github.com/multiformats/cid
- TEI P5 releases: https://tei-c.org/guidelines/p5/
- TEI P5 characters, glyphs, and writing modes: https://www.tei-c.org/release/docs/tei-p5-docs/en/html/WD.html
- TEI-EAJ jp_guidelines: https://github.com/TEI-EAJ/jp_guidelines
- Local parser research: `references/parser-research.md`
- Local TEI research: `references/TEI-research.md`
- Local Aozora research: `references/aozora-bunko-research.md`
- Local versioning research: `references/versioning-research.md`
- Local Nix materialization research: `references/nix-materialization-research.md`
- Local ML pipeline research: `references/ml-pipelines-research.md`

## Changelog

- 0.5: Tightened ArtifactID and schema-hash semantics, pinned v0 hashes to
  `sha256:<hex>`, clarified RFC 8785/JCS array limits and bundled schema
  hashing, renamed binary coordinates to build/source hashes, separated
  identity vs. non-identity manifest fields, mapped near-term questions to
  deliverables, downgraded exploratory PROV/SHACL-DS items, added threat model,
  SPDX guidance, multi-host concurrency caveat, failure-manifest fixture, and
  reference archival policy.
- 0.4: Specified JSON Schema Draft 2020-12, added SLSA/in-toto/Sigstore
  release-security evaluation, expanded RO-Crate 1.2 evaluation, added SHACL-DS
  as a future cross-graph validation candidate, acknowledged TEI-EAJ draft
  status, refined ArtifactID array ordering and deterministic RDF view
  generation, added runtime orchestration and CI smoke-corpus guidance,
  sharpened Nix around an external incremental index, added storage-packing
  concerns, and added concurrency, API, observability, retention, tokenizer,
  and error-taxonomy deliverables.
- 0.3: Added RO-Crate and FAIR evaluation, schema evolution registry,
  Software Heritage archival triggers, TEI 4.11.0 currency, PROV extension
  candidates, tokenizer configuration documentation, query alternatives, and
  release security gate.
- 0.2: Clarified canonical manifest identity, RDF canonicalization,
  failure/signature hooks, tokenizer determinism, metadata hash identity, and
  v0 canonicalization fixtures.
- 0.1: Initial Draft RFC architecture baseline.
