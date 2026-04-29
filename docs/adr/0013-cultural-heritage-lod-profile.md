# ADR 0013: Cultural-Heritage LOD Publication Profile

Status: Accepted
Date: 2026-04-28
Accepted: 2026-04-29

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

## Toolchain (pinned 2026-04-29)

- titanium-json-ld 1.7.0 (`com.apicatalog/titanium-json-ld`) for JSON-LD 1.1
  expansion.
- Jakarta JSON-P 2.0.1 runtime (`org.glassfish:jakarta.json:2.0.1`, already
  transitively pulled in by Apache Jena).

The harness `abc.tools.linked-art` reads a manifest plus its
metadata-record, deterministically builds a Linked Art-flavored JSON-LD
candidate, runs titanium expand against an in-memory document loader
that resolves only the ABC public context URI
`https://w3id.org/abc/contexts/abc-v0.jsonld` to bytes from
`contexts/abc-v0.jsonld`, and writes three byte-stable artifacts:
`linked-art-candidate.jsonld`, `linked-art-expanded.normalized.json`,
and `jsonld-context-validation-result.json`. Network fetches are
explicitly refused. The candidate's `@context` therefore references
only the ABC public context URI; CIDOC-CRM and Linked Art term
mappings live inside `contexts/abc-v0.jsonld`.

## Identity Invariant

`abc:artifactId` is preserved literally through JSON-LD expansion. The
harness extracts the value at the expanded `https://w3id.org/abc/vocab#artifactId`
predicate and asserts byte-equality with `manifest.json`'s `artifact_id`;
the bundle gate `validate-design-bundle` fails if expansion changes
that value.

## Acceptance Criteria

- `validate-design-bundle` regenerates the LOD fixtures into a temp
  directory and byte-compares against the committed
  `examples/v0/example-work/lod/{linked-art-candidate.jsonld,
  linked-art-expanded.normalized.json,
  jsonld-context-validation-result.json}`. Any drift fails the bundle.
- `validation-result.json` records `status: "ok"` and the recomputed
  context hash.
- `abc.tools.linked-art-test` covers determinism, byte-parity with
  the committed fixtures, the recomputed context hash, the identity
  invariant, and the loader's refusal to fetch external JSON-LD
  contexts.

## References

- Linked Art data model: https://linked.art/model/
- Linked Art CIDOC-CRM profile: https://linked.art/model/profile/
- titanium-json-ld release: https://github.com/filip26/titanium-json-ld/releases/tag/v1.7.0
