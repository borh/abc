# Linked Art Crosswalk v0

Status: Draft
Date: 2026-04-28

ABC keeps the canonical JSON manifest and PROV-O provenance view as the
identity and reproducibility core. Linked Art is evaluated only as a derived
JSON-LD publication/interoperability view.

Hard rule: do not let Linked Art become another identity system. A later ADR
must explicitly promote it before Linked Art terms can affect ArtifactID.

| ABC concept | Current vocabulary | Candidate Linked Art / CIDOC-CRM alignment | v0 decision |
| --- | --- | --- | --- |
| Aozora work metadata record | ABC + DC Terms | Textual/document-oriented Linked Art pattern if suitable | evaluate |
| Author/person | ABC + DC Terms | Person/Actor pattern | likely adopt as publication view |
| Source file / digital text | PROV-O Entity, DC format | Digital content pattern | likely adopt |
| Parser/render activity | PROV-O Activity | Keep PROV-O; Linked Art not primary | keep PROV-O |
| Artifact manifest | ABC manifest + PROV-O | Not a cultural object | do not force into Linked Art |
| TEI XML artifact | PROV-O Entity, DC format | Digital content view if published | optional |
| Gaiji/glyph decisions | TEI + ABC metadata | likely outside Linked Art core | keep TEI/ABC |
| Validation result | ABC + PROV-O | not Linked Art | keep ABC/PROV-O |

## Evaluation Rules

- Only map entities where semantics are clear.
- Keep ambiguous or bibliographic-only fields in ABC/DC terms until a better
  profile is chosen.
- Regenerate Linked Art JSON-LD from canonical manifests and metadata records.
- Record JSON-LD context hash as publication-profile metadata, not as an
  ArtifactID input.

## Example Fixture

`examples/v0/example-work/lod/linked-art-candidate.jsonld` is a candidate
publication view for the 羅生門 fixture. It is not canonical and can be
regenerated without changing the ABC manifest identity.
