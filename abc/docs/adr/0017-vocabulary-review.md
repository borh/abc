# ADR 0017: Vocabulary Review and `abc:` Namespace Consistency

Status: Accepted
Date: 2026-04-29
Accepted: 2026-04-29
Validation scope: fixture
Release authority: publication
Amended by: ADR 0018

## Implementation Status

2026-07-09 follow-up note: the namespace bug fixed by this ADR was a spot fix —
the `abc:` namespace was unified and the three LOD fixtures regenerated — but
the class of bug is not structurally closed. The identity-invariant check in
`abc.tools.linked-art` only round-trips `artifactId` through JSON-LD expansion;
it does not assert that the Turtle manifest view (`manifest->ttl`) and the
JSON-LD / Linked Art view describe the same entity IRIs. The two serializations
run on independent code paths, so a future divergence on entity-level identity
between the two views could again pass every existing test. A harness-level
cross-view agreement check (entity IRIs + `artifactId` asserted equal across
TTL and JSON-LD) is an open item; see `docs/adr/README.md` ("Cross-view RDF
harmonization is not structurally closed").

## Context

The metadata-record + person-record milestones (specs at
`docs/superpowers/specs/archive/2026-04-27-metadata-data-model-design.md`
and `docs/superpowers/specs/2026-04-28-separated-person-records-design.md` —
both implemented but neither has its own accepted ADR) introduced
a number of `abc:`-prefixed predicates without a unified review against
candidate standard vocabularies. ADR 0013 then added a JSON-LD publication
view that emits `abc:` triples too. As of 2026-04-29 the corpus emits the
following `abc:` predicates and classes:

**Properties** — `artifactId`, `artifactKind`, `contentHash`, `schemaHash`,
`validationStatus`, `hasSidecar`, `hasErrorArtifact`, `sidecarRole`,
`orthographicStyle`, `copyrightExpired`, `reading` (title kana),
`familyNameReading`, `givenNameReading`, `familyNameForSort`,
`givenNameForSort`, `familyNameRomaji`, `givenNameRomaji`,
`edtfDateOfBirth`, `edtfDateOfDeath`, `canonicalManifest` (LOD only),
`AozoraWorkId` (Linked Art identifier type).

**Classes / shapes** — `Artifact`, `FailureArtifact`, `EDTF` (custom
literal datatype), `ArtifactShape`, `ActivityShape`, `FailureShape`,
`SidecarShape`, `MetadataRecordWorkShape`, `PersonRecordShape`.

The audit also surfaced a real namespace-consistency bug.

## Namespace consistency bug (v0 correctness)

The `abc:` prefix is declared in two different places with two different
namespace URIs:

| Source | Declaration |
| --- | --- |
| `schemas/manifest.shacl.ttl`, `manifest_to_rdf.clj`, `person_record.clj`, `materialize_import.clj`, all emitted `*.ttl` fixtures | `abc:` → `https://w3id.org/abc/` |
| `contexts/abc-v0.jsonld`, `linked_art.clj` | `abc:` → `https://w3id.org/abc/vocab#` |

Result: `abc:artifactId` resolves to two different IRIs depending on
which serialization writes it (`https://w3id.org/abc/artifactId` in TTL
vs. `https://w3id.org/abc/vocab#artifactId` in JSON-LD-expanded form).
This silently passed every existing test because the linked-art identity
invariant only round-trips through the JSON-LD form, and the TTL pipeline
never touches the JSON-LD context. But by RDF semantics these are two
unrelated predicates, which violates the v0 contract that "the manifest
RDF view and the LOD publication view describe the same entities."

## Decision

The canonical namespace for `abc:` is `https://w3id.org/abc/` (the form
used by the SHACL shapes, RDF generators, and every committed `*.ttl`
fixture). The JSON-LD context is updated to match.

This is a one-line fix in `contexts/abc-v0.jsonld` plus two URI-constant
updates in `abc.tools.linked-art` (the `aozora-work-id-type-uri` constant
and the expanded-form predicate that the identity-invariant check
extracts). The three LOD fixtures regenerate deterministically against
the new context; their `context_hash` rotates accordingly.

The alternative — moving everything to `https://w3id.org/abc/vocab#` —
was rejected because it would rotate every committed `*.ttl` fixture,
the `manifest_identity_object` canonicalization fixture, and the
`abc:EDTF` datatype URI, with no semantic gain.

## Per-predicate audit

The audit classified each `abc:` predicate as **keep**, **defer-switch**
(plausible standard alternative, not landing now), or **needs-research**.

### Keep — no clear standard alternative

| Predicate | Why kept |
| --- | --- |
| `abc:artifactId` | Content-address identity is project-defined; no standard predicate exists. |
| `abc:artifactKind` | Domain-specific artifact taxonomy; `dcterms:type` is too coarse. |
| `abc:contentHash` | Bytes-of-the-file digest; no widely-adopted standard predicate at v0 scope. |
| `abc:schemaHash` | Schema-version pin; project-specific. |
| `abc:validationStatus` | Domain-specific enum (`passed`/`warning`/`failed`/`not-run`). |
| `abc:hasSidecar`, `abc:hasErrorArtifact` | Artifact-graph relationships; `dcterms:hasPart` and `prov:hadDerivation` are looser. |
| `abc:sidecarRole` | Sidecar-role enum; tightly bound to ABC's sidecar policy. |
| `abc:orthographicStyle` | Aozora-specific Japanese orthography classification (新字新仮名 / 旧字旧仮名 / etc.); no standard vocab covers it. |
| `abc:edtfDateOfBirth`, `abc:edtfDateOfDeath` | Justified by ADR 0015: EDTF lexical echo with custom datatype is required to carry decade- and century-precision values that XSD cannot type. The parallel `rdag2:dateOfBirth/Death` triples already provide the standard-vocab view at XSD precision. |
| `abc:EDTF` (datatype) | Custom datatype required because EDTF Level 1 lexical strings are not a registered XSD datatype. |
| `abc:Artifact`, `abc:FailureArtifact` | Project-specific class hierarchy; `prov:Entity` is too coarse for SHACL targeting. |
| All `*Shape` classes | SHACL-internal validation contracts; not data. |
| `abc:canonicalManifest` (LOD) | Back-pointer from the publication view to the canonical manifest; specific to ABC's two-tier identity model. |
| `abc:AozoraWorkId` (LOD identifier type) | Aozora-specific identifier class; no AAT or LoC equivalent. |

### Defer-switch — plausible standard alternative, no migration in v0

These have non-trivial migration cost (every TTL fixture, schema hashes,
manifest identity hashes), so the decisions are recorded here but
implementation is deferred to a follow-up ADR that batches them with the
schema-hash cascade.

| Predicate | Candidate alternative | Notes |
| --- | --- | --- |
| `abc:reading` (title kana) | `dcndl:titleTranscription` | DCNDL has a dedicated property for title kana readings; semantic match is exact. |
| `abc:copyrightExpired` | `dcterms:rights` URI from rightsstatements.org / `creativecommons.org/publicdomain/mark/1.0/` | A boolean flag is less informative than a rights-statement URI; switching gives interop with cultural-heritage tooling. |

### Needs-research — proposed alternative does not preserve semantics

| Predicate | Proposed | Why not adopt |
| --- | --- | --- |
| `abc:familyNameReading`, `abc:givenNameReading` | `dcndl:transcription` | DCNDL `dcndl:transcription` is typically applied to the whole `foaf:Person` or to a name string, not to family/given **components**. Adopting it would lose the component-level distinction that the corpus needs. |
| `abc:familyNameForSort`, `abc:givenNameForSort` | `dcndl:sortKey` | Same component-vs-whole-name issue. |
| `abc:familyNameRomaji`, `abc:givenNameRomaji` | `foaf:familyName` / `foaf:givenName` with `xml:lang="ja-Latn"` | FOAF's `familyName`/`givenName` are typed `xsd:string`; attaching `xml:lang` is allowed by RDF 1.1 but not idiomatic, and tooling commonly drops the language tag. The current per-component romaji predicates are explicit. |

These three groups would need either a new component-aware NDL/RDA
profile, or acceptance of `foaf:Person` with `xml:lang`-tagged literals
and the loss of the family/given component distinction. Either path
needs a separate ADR.

## Hard Rule

This ADR fixes the namespace-consistency bug only. Predicate renames are
explicitly out of scope and will not happen until a follow-up ADR
schedules them with the schema-hash cascade. The audit table above is the
v0 contract surface for `abc:` predicates.

## Acceptance Criteria

- **ADR-0017-C1 — structural-invariant:** `contexts/abc-v0.jsonld` declares `abc` as `https://w3id.org/abc/`. See `test/abc/tools/linked_art_test.clj`.
- **ADR-0017-C2 — fixture-behavior:** Linked Art expansion preserves the manifest artifact ID at `https://w3id.org/abc/artifactId`. See `test/abc/tools/linked_art_test.clj`.
- **ADR-0017-C3 — fixture-behavior:** The three current LOD fixtures regenerate byte-identically and their committed context hash equals the JCS-recomputed context hash. See `test/abc/tools/linked_art_test.clj`.
- **ADR-0017-C4 — structural-invariant:** The bounded committed Turtle inventory uses `@prefix abc: <https://w3id.org/abc/>`. See `test/abc/tools/foundation_evidence_test.clj`.
- **ADR-0017-C5 — fixture-behavior:** Success-manifest, failure-manifest, and metadata-record Turtle generators remain byte-identical to their named committed fixtures. See `test/abc/tools/manifest_to_rdf_test.clj` and `test/abc/tools/metadata_record_test.clj`.

## Historical Evidence

The LOD fixtures and context hash rotated when this ADR unified the namespace
on 2026-04-29. The contemporaneous broad `nix flake check` pass and the
observation that existing Turtle fixtures did not regenerate are bounded to
that revision; current acceptance is expressed by the named parity checks.

## Consequences

- One-time rotation of the LOD context hash and three LOD fixtures.
- The audit table becomes the reference point for any future predicate
  rename: any switch from `abc:X` to a standard predicate goes through
  a new ADR that explicitly schedules the schema-hash cascade.
- The legacy `aozora:` namespace question (mentioned in
  `docs/next-steps.md`) is resolved: no v0 code or fixture references
  it; the only surviving copy is in `references/archive/aozora_lod_data/`
  for historical comparison.

## References

- Metadata-record data model (spec
  `docs/superpowers/specs/archive/2026-04-27-metadata-data-model-design.md`) —
  introduced `abc:reading`, `abc:orthographicStyle`,
  `abc:copyrightExpired`. (Implemented under ADR 0008's tools
  runtime; the data model itself was not adopted as a standalone ADR.)
- Separated person records (spec
  `docs/superpowers/specs/2026-04-28-separated-person-records-design.md`) —
  introduced `abc:familyName{Reading,ForSort,Romaji}` etc.
  (Implemented but, like the metadata-record data model, not adopted
  as a standalone ADR.)
- ADR 0013 (cultural-heritage LOD profile) — introduced the JSON-LD
  context where the namespace mismatch was discovered.
- ADR 0015 (temporal modeling) — justified `abc:edtfDateOf{Birth,Death}`
  + `abc:EDTF` datatype.
- DCNDL terms: https://ndl.go.jp/dcndl/terms/
- rightsstatements.org: https://rightsstatements.org/
