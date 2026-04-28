# ADR 0013: Cultural-Heritage LOD Publication Profile

Status: Draft
Date: 2026-04-28

## Context

ABC already has a canonical JSON manifest and a derived PROV-O RDF view for
artifact provenance. That is enough for reproducibility, but cultural-heritage
interoperability also needs an explicit position on Linked Art and CIDOC-CRM.

Linked Art is a JSON-LD application profile for cultural-heritage resources. It
uses a streamlined CIDOC-CRM-oriented model, but it is museum/art focused and
does not try to provide complete bibliographic or archival description. ABC
therefore should not replace its manifest or bibliographic model with Linked
Art in v0.

## Options

| Option | Meaning | Recommended? |
| --- | --- | ---: |
| A. PROV-O only | ABC publishes only artifact provenance and basic bibliographic metadata | Too weak unless explicitly justified |
| B. PROV-O + Linked Art crosswalk | Canonical ABC manifest remains unchanged; Linked Art JSON-LD is a derived publication view | Best default |
| C. Full Linked Art adoption | ABC models bibliographic/cultural objects natively in Linked Art | Probably too heavy for v0 |

## Decision

ABC will keep the canonical JSON manifest and PROV-O provenance view as the
identity and reproducibility core.

ABC will evaluate a Linked Art-compatible JSON-LD publication view for cultural
heritage interoperability. This view is derived, not canonical, in v0.

Linked Art alignment is adopted only for entities where the mapping is clear:
work/person/source/digital object/provenance event/identifier. Ambiguous or
bibliographic-only fields remain in ABC/DC terms until a better profile is
chosen.

## Hard Rule

Linked Art must not become another identity system. It is a
publication/interoperability view unless a later ADR explicitly promotes it.
JSON-LD compaction, expansion, context changes, or regenerated Linked Art views
must not change `ArtifactID`.

## Consequences

- `docs/lod/linked-art-crosswalk.md` records the candidate mappings.
- `docs/lod/json-ld-context-policy.md` records context versioning and hashing
  policy.
- Linked Art fixtures may be generated or explicitly marked not adopted for v0
  with reasons.
- The canonical ABC JSON manifest and PROV-O view remain valid without Linked
  Art fixtures until this ADR is accepted and implementation gates are enabled.

## References

- Linked Art data model: https://linked.art/model/
- Linked Art CIDOC-CRM profile: https://linked.art/model/profile/
