# ABC Design Survey

Date: 2026-07-04
Status: Survey and incubation record

This document keeps evaluated but unaccepted alternatives out of the canonical
architecture surface. See `docs/architecture.md` for the Promotion Contract:
items here become implementation obligations only after an accepted ADR or a
concrete v0 design-bundle consumer/test forces them.

## Parser Ecosystem

ABC has evaluated multiple parser/reference sources: `aozora-rs`,
`aozora-core`, `aozora2`, `aozora2html`, `aozora-parser.js`,
`vscode-language-japanese-novel`, `pandoc-aozora-ruby`, `canopy`,
`narouconv`/`novel.js`, `aozorabunko_html`, `aozora_json_scrape`, and legacy
ABC Clojure parser code.

Promotion trigger: ADR 0002 accepts a parser candidate or parser-evaluation
path, and at least one ABC manifest is produced from that candidate's output in
a checked fixture or release-candidate run.

## Schema And Graph Validation Alternatives

SHACL is the accepted v0 RDF validation language. ShEx, `rudof`, SHACL 1.2,
and SHACL-DS remain survey items.

Promotion trigger: a partner or consumer contractually requires ShEx; SHACL
cannot express an accepted cross-graph/named-graph constraint; or an ADR
selects a new validation language and adds fixtures equivalent to the current
SHACL gate.

## CID, Multihash, And IPLD

The v0 ArtifactID format is `sha256:<hex>` over canonical
`manifest_identity_object`. CID or multihash forms may be useful as additional
interoperability identifiers, but they are not the accepted identity rule.

Promotion trigger: an ADR pins the multicodec/content-type story for ABC
artifacts and proves that CID aliases do not reinterpret existing ArtifactIDs.

## RO-Crate Packaging

RO-Crate 1.2 remains an evaluated publication packaging option, including
Detached Crates, Profile Crates, entity reachability, and Root Data Entity
requirements. It must not replace the canonical identity manifest unless it
preserves the JCS/hash/null-dimension rules.

Promotion trigger: an ADR accepts RO-Crate as a publication bundle format and
adds a generated crate fixture or validation path.

## Secondary Parser-IR Encodings

JSON with JSON Schema is the accepted parser-IR boundary. CBOR, Arrow, or
another binary/columnar representation may be added only as a secondary
encoding of the same logical schema.

Promotion trigger: a concrete consumer or benchmark shows JSON is a bottleneck,
and an ADR accepts a secondary encoding with round-trip tests to the JSON
contract.

## Lighter TEI Validation Runtime

Jing/JVM remains the compatibility baseline for TEI validation. `libxml2` /
`xmllint` may be useful for a lighter Relax NG path.

Promotion trigger: the lighter validator passes the same fixture corpus as
Jing for the project RNG and produces useful error reports; ADR 0012 or a new
ADR accepts it as equivalent or complementary.

## OWL And Ontology Runtime

The accepted v0 surface uses RDF/PROV-O plus SHACL. OWL reasoning, Horned-OWL,
OWLAPI/Tawny, and ontology packaging tools such as Plow.pm remain optional
evaluation topics.

Promotion trigger: a concrete inference or vocabulary-publication task appears
and an ADR selects the runtime, packaging mechanism, and validation fixtures.

## Query Runtime

Potential query runtimes include plain files plus indexes, generated SQLite,
SPARQL/RDF stores, XTDB v2-style bitemporal stores, Dolt-like snapshot stores,
DataFusion/Arrow, DuckDB, and hybrid layouts. XTDB v1 is retired and is not a
candidate. The architecture does not select a query runtime for v0.

`docs/handoffs/query-runtime-and-history-index.md` records the first bounded
query-pack probe over Aozora history scan output and paper-demo manifests. Its
provisional direction keeps canonical truth in ordinary files, uses generated
query packs as disposable indexes, prefers SQLite for small coordinate/history
lookup, and keeps DuckDB/Parquet for analytical facts.

TEI generation is not part of the query-runtime decision. It is the accepted
parser-IR publication-rendering path from ADR 0025; a future query pack may
index TEI artifact manifests and validation results, but does not own TEI
creation.

Promotion trigger: access-pattern evidence shows coordinate lookup,
provenance traversal, corpus evolution, full-text search, or analytical scans
need a concrete runtime; an ADR accepts it and records fixture/query gates.

## API And Service Interface

REST, gRPC, authenticated endpoints, streaming APIs, distributed scheduling,
and multi-host publication protocols are explicitly deferred. The accepted v0
API is ordinary files, JSON schemas, and CLI tools.

Promotion trigger: a consumer needs a network/service interface and an ADR
specifies authentication, publication semantics, backward compatibility, and
tests.

## Workflow Engine Adoption

The v0 orchestrator can remain a CLI/script surface. Workflow engines should
not be adopted because they are convenient; they need to solve a measured
coordination problem.

Promotion trigger: the CLI/script orchestrator fails on a real batch,
recovery, or scheduling problem and an ADR selects a workflow engine with
recorded run-summary semantics.

## Full-Corpus Storage Packing

Loose JSON/XML/sidecar files are acceptable for examples and small smoke
corpora. Full-corpus releases may need SQLite, key-value stores, tar/zip
archives with indexes, or another packed layout to control inode and traversal
costs.

Promotion trigger: release-candidate measurements show loose files exceed
storage, traversal, or review budgets; an ADR accepts a packed layout that can
still export ordinary manifest files.

## Bounded Workset Materialization

`docs/handoffs/bounded-workset-index-design.md` remains provisional. A first
synthetic evaluator-only probe is recorded in
`docs/handoffs/measurement-probes-2026-07-04.md`: per-work derivations, 100-work
batches, and one requested-set/CAS-style derivation all stayed under the
30-second / 2-GB envelope at 5,000 selected works on a local workstation, but
only the per-work shape showed evaluator growth. The current options are:

- on-demand derivation generation for selected work manifests,
- batch-by-author/card/release with external subset selection,
- content-addressed store plus manifest index without a Nix attrset matrix,
- DVC-style stage graph with Nix only as a pinned runtime provider.

Promotion trigger: the remaining disposable probe measures representative
manifests, cold build time, incremental single-work rebuild behavior,
derivation count, and per-work failure attribution on the recorded CI runner
class. ADR 0003 can then be accepted or revised.

## Authoritative Aozora Marker Registry

`docs/handoffs/authoritative-registry-design.md` remains provisional. The
13-page extraction probe in
`docs/handoffs/measurement-probes-2026-07-04.md` confirms that the manual pages
are mechanically harvestable: 614 marker occurrences collapse to 355 raw marker
strings and 262 normalized templates.

Promotion trigger: an ADR accepts the file format/location, drift gate,
description-review policy, and observed-only corpus governance. If descriptions
must be complete before promotion, all 262 normalized templates need review or
authoring; if `NEEDS_REVIEW` is allowed initially, the first manual pass can
focus on the 54 raw-marker heuristic bucket plus observed-only corpus
constructs.

## Release Security

ADR 0004 is Draft. SLSA v1.2, in-toto attestations, Sigstore/Cosign bundles,
detached signatures, key rotation, and revocation policy are release-gate
topics, not local v0 blockers.

Promotion trigger: before a public release, `docs/release-verification.md` or
an equivalent gate verifies manifest schema hash, artifact content hash,
signature or Sigstore bundle, provenance attestation subject hashes,
source/archive identifiers, and license metadata presence.

## Experiment And ML Runtimes

DVC, MLflow, embedding stores, vector indexes, and RAG runtime tooling remain
downstream experiment concerns. They should attach to ABC manifests rather than
replace them.

Promotion trigger: a concrete experiment artifact needs managed runs,
equivalence criteria, or retrieval provenance and an ADR accepts the runtime or
manifest extension.

## Downstream Requirement Examples

Authorship attribution, genre classification, speech/audio alignment,
parallel corpora, RAG systems, and publication visualizations are pressure
tests for the manifest model. They are not first implementation obligations.

Promotion trigger: a corresponding consumer, fixture, or paper/release demo
requires one of these outputs, and the identity inputs are captured in a schema
or ADR.

## Survey References

- RO-Crate 1.2 and Detached/Profile Crates
- Shape Expressions Language and `rudof`
- W3C SHACL 1.2 and SHACL-DS
- Multiformats CID and IPLD
- Horned-OWL, OWLAPI, Tawny-OWL, and Plow.pm
- XTDB v2-style bitemporal stores, Dolt, SQLite, DuckDB,
  DataFusion/Arrow, and RDF stores
- DVC and MLflow
- SLSA, in-toto, Sigstore, and Cosign
- IIIF Presentation API and Linked Art model references where publication
  views extend beyond the accepted v0 fixtures
