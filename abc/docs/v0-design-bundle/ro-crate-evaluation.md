# RO-Crate Evaluation v0

Status: Draft
Date: 2026-04-26

## Question

Should ABC release bundles be published as an RO-Crate 1.2 profile in addition
to the canonical JSON manifests?

## Candidate Use

RO-Crate is best treated as publication packaging, not as the canonical
ArtifactID source. The canonical identity manifest remains JSON with RFC 8785
JCS and `sha256:<hex>` ArtifactIDs.

## RO-Crate 1.2 Features To Evaluate

- Detached Crates: useful when metadata should be served separately from large
  TEI/token/output files.
- Profile Crates: useful for publishing an ABC release-bundle profile and
  expressing conformance.
- Entity reachability: every entity needs `@id`, `@type`, and reachability
  from the Root Data Entity.

## Candidate ABC Profile

An ABC RO-Crate profile would declare:

- Required reference to the canonical ABC manifest for every shipped artifact.
- Required schema/profile entities for manifest schema, parser IR schema, TEI
  ODD, SHACL shape, and manifest-to-RDF mapping version.
- Optional detached data entities for large TEI, token, embedding, or analysis
  outputs.
- Required license metadata using SPDX identifiers where possible.
- Required provenance link back to the ABC ArtifactID and PROV-O RDF view.

## Detached Crate Scenario

For a public release with large TEI files, the Root Data Entity can describe
the release metadata and point to detached data entities by ArtifactID, hash,
media type, and access URL. The canonical ABC manifest remains the integrity
source. The detached crate is useful when metadata and manifests are mirrored
or queried separately from large materialized files.

## Reachability Check

ABC sidecars, schemas, failure manifests, and validation reports should be
reachable from the Root Data Entity through `hasPart`, `conformsTo`, or a
profile-defined relation. No RO-Crate entity should exist only because it was
copied from the ABC manifest; if an entity cannot be reached, the ABC profile
must define the missing relationship or leave that metadata in the canonical
manifest only.

## Relationship to Linked Art and IIIF

RO-Crate packages the research object and release bundle. PROV-O describes
artifact derivation and build provenance. Linked Art describes
cultural-heritage entities where the mapping is clear. IIIF presents
image/facsimile resources when applicable. The canonical ABC manifest remains
the identity root.

| Layer | Purpose | Canonical? |
| --- | --- | ---: |
| ABC JSON manifest | Artifact identity and reproducibility | yes |
| PROV-O RDF | Provenance publication/query view | derived |
| SHACL | RDF validation | validation |
| Linked Art JSON-LD | Cultural-heritage interoperability | derived/evaluated |
| IIIF Presentation | Image/facsimile presentation | derived/conditional |
| RO-Crate | Release bundle packaging | derived |

## Evaluation Criteria

- Can every ABC artifact manifest be referenced without changing ArtifactID?
- Can sidecars, failure manifests, schemas, TEI profile, and smoke-corpus
  fixtures be represented clearly?
- Can SPDX license identifiers and provenance links be exposed cleanly?
- Can detached crates describe cold artifacts that are rebuildable but not
  shipped?
- Can Profile Crates express the ABC conformance rules without duplicating the
  JSON Schema identity rules?
- Can every schema, sidecar, failure manifest, and validation report remain
  reachable from the Root Data Entity?
- Does existing RO-Crate tooling validate the profile without requiring ABC to
  duplicate manifest identity rules?

## Provisional Position

Use RO-Crate for release packaging if it improves interoperability. Do not use
RO-Crate as the v0 identity manifest.
