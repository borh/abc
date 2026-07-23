# ADR 0018: Predicate Rename Batch 1 — `abc:reading` and `abc:copyrightExpired`

Status: Accepted
Date: 2026-04-29
Accepted: 2026-04-29
Validation scope: fixture
Release authority: none
Amends: ADR 0017
Amended by: ADR 0035 [scope: Boolean-to-external-rights RDF mapping]

## Implementation Status

As of 2026-07-09, `abc.tools.metadata-record/record->graph` emits the renamed
DCNDL and DCTERMS predicates and `schemas/manifest.shacl.ttl` enforces the live
rights contract. `test/abc/tools/metadata_record_test.clj` and
`test/abc/tools/shacl_test.clj` cover the implementation.

## Context

ADR 0017's audit identified two `abc:` predicates whose semantics map
exactly onto a standard alternative, and deferred the actual switches
to a follow-up ADR that batches them together with the schema-hash
cascade. This is that follow-up.

The two switches in this batch:

| From | To | Notes |
| --- | --- | --- |
| `abc:reading` (title kana) | `dcndl:titleTranscription` | DCNDL has a dedicated property for title kana readings; semantic match is exact. |
| `abc:copyrightExpired` (xsd:boolean) | `dcterms:rights` (IRI) | A boolean is less informative than a rights-statement URI; switching gives interop with cultural-heritage tooling. |

The other ADR 0017 candidates (`abc:familyNameReading` /
`abc:givenNameReading`, `abc:familyNameForSort` /
`abc:givenNameForSort`, `abc:familyNameRomaji` /
`abc:givenNameRomaji`) remain in the **needs-research** bucket: their
proposed standard alternatives (`dcndl:transcription`, `dcndl:sortKey`,
FOAF with `xml:lang="ja-Latn"`) operate on whole-name strings rather
than family/given **components**, so adopting them would lose the
component-level distinction the corpus needs. Those switches are
explicitly out of scope for v0 and require a separate ADR if a
component-aware NDL/RDA profile is identified.

## Decision

### `abc:reading` → `dcndl:titleTranscription`

`metadata-record.clj` emits the title kana inside the
`dcterms:title` blank node under the new predicate
`<http://ndl.go.jp/dcndl/terms/titleTranscription>`. The `dcndl:`
prefix is already declared by `abc.tools.rdf-prefixes` and shows up
in every committed `*.ttl` fixture (it carries `dcndl:NDC` for the
classification literal datatype).

### `abc:copyrightExpired` → `dcterms:rights` URI

The JSON metadata record continues to carry `copyright_expired:
boolean`. The RDF view derives a single `dcterms:rights` IRI from
that boolean per the table below; the SHACL shape constrains the
property to one of these two IRIs.

| `copyright_expired` | `dcterms:rights` IRI | Source |
| --- | --- | --- |
| `true` | `https://creativecommons.org/publicdomain/mark/1.0/` | Creative Commons Public Domain Mark 1.0 |
| `false` | `http://rightsstatements.org/vocab/InC/1.0/` | rightsstatements.org InC (in copyright) |

Boolean → URI is a one-way derivation in the RDF view; the canonical
JSON contract stays boolean to keep the metadata-record schema
identity-stable.

## Hard Rule

JSON contracts (`metadata-record.schema.json`,
`person-record.schema.json`) are **unchanged** by this batch. The
`title_reading` and `copyright_expired` fields keep the same names,
shapes, and presence rules. Therefore:

- `metadata_record_schema_hash` does not rotate.
- `person_record_schema_hash` does not rotate.
- `metadata_record_hash` for any committed work does not rotate.
- `manifest_identity_object.metadata_record_hash` and the
  `ArtifactID` content-address do not change.
- `manifest.json` for the example bundle is not regenerated.

What rotates:

- `examples/v0/example-work/metadata-record.ttl` (one fixture, 17,810
  corpus TTL views if regenerated, none of which are committed).
- `schemas/manifest.shacl.ttl` — one property shape.

## Acceptance Criteria

- **ADR-0018-C1 — fixture-behavior:** In the named metadata RDF cases, the generator emits `dcndl:titleTranscription` inside the title blank node, emits neither `abc:reading` nor `abc:copyrightExpired`, and true, false, and nil legacy copyright flags emit no external rights assertion. See `test/abc/tools/schema_validation_evidence_test.clj`.
- **ADR-0018-C2 — fixture-behavior:** `MetadataRecordWorkShape` permits zero or one `dcterms:rights`; when present it must be an IRI in the closed Public Domain Mark/InC set. See `test/abc/tools/schema_validation_evidence_test.clj`.
- **ADR-0018-C3 — fixture-behavior:** The metadata-bundle helper validates the committed metadata JSON and person inputs, SHACL graph, and byte-identical `metadata-record.ttl` fixture. See `test/abc/tools/schema_validation_evidence_test.clj`.

ADR 0035 containment supersedes the original Boolean-to-external-rights
derivation: legacy Boolean values now emit no external rights assertion.

## Historical Evidence

The broad `nix flake check` pass and equality of `manifest.json` and
`manifest_identity_object` with commit `bc3ea94` were observations at the
2026-04-29 migration revision. They are retained as historical evidence, not
as claims about the current manifest.

## Consequences

- One-time rotation of the example `metadata-record.ttl` fixture.
- The SHACL shape becomes more informative: `sh:in` enumerates the
  exact rights IRIs the v0 contract emits, so any future ADR that
  adds another rights statement (e.g. NoC-OKLR) must extend the
  `sh:in` list.
- The two ADR 0017 needs-research items remain deferred. Any future
  switch from a component-aware FOAF/DCNDL profile follows the same
  pattern: rotate the RDF view + SHACL shape if and only if the JSON
  contract is unchanged; otherwise schedule with the schema-hash
  cascade.

## References

- ADR 0017 (vocabulary review) — defer-switch table that scheduled
  these two renames.
- DCNDL terms: https://ndl.go.jp/dcndl/terms/
- rightsstatements.org: https://rightsstatements.org/page/InC/1.0/
- CC Public Domain Mark 1.0: https://creativecommons.org/publicdomain/mark/1.0/
