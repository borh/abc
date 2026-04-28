# ADR 0014: IIIF Applicability for ABC v0

Status: Draft
Date: 2026-04-28

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

## Decision Matrix

| Case | IIIF action |
| --- | --- |
| Plain text only | No IIIF artifact required |
| Aozora text with inline image/caption references | Preserve in IR/TEI; evaluate IIIF if image URLs/assets are available |
| Facsimile/page images available | Generate IIIF Presentation 3.0 manifest |
| Page-level annotations or text-image alignment | Use IIIF annotations/content search only after a separate ADR |
| Rights unclear | Do not publish IIIF image service links; record rights blocker |

## Acceptance Criteria

- Documentation says exactly when IIIF is required, optional, or out of scope.
- A text-only v0 bundle is valid without IIIF.
- If images/facsimiles are present, the example bundle includes either an IIIF
  manifest or an explicit "not publishable due to rights/source absence" note.
- TEI image/caption handling remains valid even without IIIF.

## References

- IIIF overview: https://iiif.io/
- IIIF Presentation API 3.0: https://iiif.io/api/presentation/3.0/
