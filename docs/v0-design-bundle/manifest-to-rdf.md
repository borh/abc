# Manifest To RDF Mapping v0

Status: Draft
Date: 2026-04-26

The canonical manifest is JSON. RDF/PROV-O is a deterministic publication and
query view generated from the JSON manifest.

## Generation Rule

For releases, materialize the RDF view beside the JSON manifest. For local
development and query services, the RDF view may be generated on demand. The
mapping implementation version must be recorded in release metadata.

## Core Mapping

| JSON Field | RDF Term | Notes |
| --- | --- | --- |
| `artifact_id` | artifact IRI and `abc:artifactId` | Provisional IRI pattern: `https://example.org/abc/artifact/{artifact_id-with-colon-escaped}` |
| `artifact_kind` | `dcterms:type` | Use the JSON enum value |
| `validation_status` | `abc:validationStatus` | Same enum as manifest schema |
| `manifest_identity_object.manifest_schema_hash` | `abc:schemaHash` | Required on artifact entities |
| `content.content_hash` | `abc:contentHash` | Required for successful materialized artifacts |
| `content.media_type` | `dcterms:format` | Omit if content is null |
| `provenance.generated_at` | `prov:generatedAtTime` | Timestamp for artifact generation |
| `provenance.activity_id` | `prov:wasGeneratedBy` | Activity node or IRI |
| `provenance.used[]` | `prov:used` on activity | Preserve array order only if meaningful; otherwise sort by hash before generation |
| `provenance.was_derived_from[]` | `prov:wasDerivedFrom` | Link artifact entity to source entities |
| `sidecars[]` | sidecar `prov:Entity` plus `abc:hasSidecar` | Warning/error sidecars are generated entities |

## Entity Pattern

The RDF view should include the same PROV-O structure as the architecture note:

- Artifact entity: `prov:Entity`, `abc:artifactId`, `abc:schemaHash`,
  optional `abc:contentHash`, `abc:validationStatus`, `dcterms:format`,
  `prov:wasGeneratedBy`, and `prov:wasDerivedFrom`.
- Activity entity: `prov:Activity`, `prov:used` for every input hash recorded
  in manifest provenance, and `prov:qualifiedAssociation` linking agent and
  plan where available.
- Sidecar entity: `prov:Entity` with role-specific links such as
  `abc:hasWarningArtifact`, `abc:hasErrorArtifact`, or `abc:hasSidecar`.
- Failure entity: `abc:FailureArtifact` and `prov:Entity`, with
  `abc:validationStatus "failed"`, `abc:hasErrorArtifact`, and no output
  `abc:contentHash` unless a failure report itself is the materialized content.
- Sidecar schema hashes: `abc:schemaHash` on a sidecar entity records the
  manifest/RDF view schema that described the sidecar in this publication
  graph. It does not claim that the JSONL warning or error file itself
  conforms to `manifest.schema.json`.

## Array Policy

Field-level array policy:

| Field | Policy |
| --- | --- |
| `provenance.used[]` | Sort lexicographically by full hash before RDF generation unless an ADR assigns semantic order |
| `provenance.was_derived_from[]` | Sort lexicographically by full hash before RDF generation |
| `sidecars[]` | Sort by `role`, then `hash`, then `path_hint` |
| `signatures[]` | Non-identity; sort by `signature_type`, then `signature_hash` for stable publication output |

## Determinism

- JSON input is validated before mapping.
- Arrays follow the field-level policy above before RDF generation.
- Blank nodes are allowed for compact associations in local views, but release
  graph hashes use RDFC-1.0 or skolemized IRIs.
- The generated Turtle format is not the canonical identity object.

## Fixture Acceptance

The v0 fixture set should include:

- A successful TEI manifest.
- A parser IR manifest with warning sidecar.
- A failure manifest.
- Expected generated Turtle for each fixture.
