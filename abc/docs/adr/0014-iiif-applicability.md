# ADR 0014: IIIF Applicability for ABC v0

Status: Accepted
Date: 2026-04-28
Accepted: 2026-04-29
Validation scope: fixture
Release authority: publication

## Implementation Status

As of 2026-07-09, `schemas/iiif-applicability.schema.json` and
`abc.tools.iiif/validate-applicability!` are the live applicability contract and
validator used by the design-bundle gate. Their contract cases are covered by
`test/abc/tools/iiif_test.clj`.

## Context

ABC v0 is primarily a text artifact pipeline. Some Aozora-derived works may
also include inline images, captions, facsimiles, page images, or
text-image-alignment needs. IIIF is relevant to those cases because it provides
interoperable APIs for presenting attributed digital objects online. The IIIF
Presentation API is specifically a presentation format for compound digital
objects, usually alongside the Image API; it is not the primary discovery
metadata format.

## Decision

ABC will not make IIIF a required v0 dependency for text-only artifacts.

ABC will define an IIIF integration path for works with facsimiles, source
images, illustrations, captions, or page-level presentation needs.

When IIIF is applicable, the IIIF Presentation manifest is a derived
publication artifact linked from the ABC manifest, TEI `@facs`/`graphic`
references, and RDF publication view.

Each work carries a small applicability decision record validated by
`schemas/iiif-applicability.schema.json`. The record's `status` is one of
`applicable`, `not_applicable`, or `rights_blocker`; `derived_manifest`
is required (non-empty string) when `status="applicable"` and must be
`null` otherwise. The decision policy below is the source of truth for
which status applies; the schema only enforces structural shape.

## Decision Matrix

| Case | IIIF action | Status |
| --- | --- | --- |
| Plain text only | No IIIF artifact required | `not_applicable` |
| Aozora text with inline image/caption references | Preserve in IR/TEI; evaluate IIIF if image URLs/assets are available | `not_applicable` until assets are available |
| Facsimile/page images available | Generate IIIF Presentation 3.0 manifest | `applicable` |
| Page-level annotations or text-image alignment | Use IIIF annotations/content search only after a separate ADR | `not_applicable` for v0 |
| Rights unclear | Do not publish IIIF image service links; record rights blocker | `rights_blocker` |

The example v0 bundle (羅生門 / 000127) is text-only; it carries
`status: "not_applicable"` with a written reason and `derived_manifest:
null`. No production IIIF Presentation manifests ship in v0; the
`applicable` row is reserved for future works with confirmed
facsimiles and clean rights.

## Hard Rule

The applicability record is a derived publication artifact, not an
identity input. JSON-LD compaction, expansion, schema evolution, or
status changes from `not_applicable` to `applicable` must not change
`ArtifactID`. The schema and ADR live alongside the bundle; any IIIF
Presentation manifest produced under `status="applicable"` is also
derived and must not feed `manifest_identity_object`.

## Acceptance Criteria

- **ADR-0014-C1 — structural-invariant:** `schemas/iiif-applicability.schema.json` declares JSON Schema 2020-12 and validates against that meta-schema. See `test/abc/tools/schema_validation_evidence_test.clj`.
- **ADR-0014-C2 — fixture-behavior:** The text-only `000127` applicability fixture validates with status `not_applicable`, a text-only reason, and `derived_manifest: null`. See `test/abc/tools/schema_validation_evidence_test.clj` and `test/abc/tools/iiif_test.clj`.
- **ADR-0014-C3 — fixture-behavior:** IIIF applicability validation rejects a missing work ID and both invalid status/derived-manifest combinations. See `test/abc/tools/iiif_test.clj` and `test/abc/tools/schema_validation_evidence_test.clj`.
- **ADR-0014-C4 — fixture-behavior:** IIIF applicability validation accepts an `applicable` record with a manifest path and a `rights_blocker` record with no derived manifest. See `test/abc/tools/iiif_test.clj`.

## References

- IIIF overview: https://iiif.io/
- IIIF Presentation API 3.0: https://iiif.io/api/presentation/3.0/
