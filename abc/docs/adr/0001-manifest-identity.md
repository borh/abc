# ADR 0001: Manifest Identity

Status: Accepted
Date: 2026-04-26
Accepted: 2026-04-28
Validation scope: fixture
Release authority: publication
Supersedes: none
Amended by: ADR 0010, ADR 0023, ADR 0027, ADR 0033 [scope: work_content_hash equality relation]
Source: `docs/high-level-architecture-note.md` v0.5

## Implementation Status

Acceptance criteria satisfied as of 2026-04-28. The example-work bundle
demonstrates a real `metadata_record_hash` (Aozora 羅生門 / 芥川竜之介), with
schema → identity → SHACL → TEI-EAJ `<teiHeader>` covered end-to-end. Failure
manifest fixtures and canonicalization fixtures are committed and exercised by
`nix run .#validate-design-bundle`. See archived plan
`docs/superpowers/plans/archive/2026-04-27-metadata-data-model.md`.

2026-07-09 cross-language verification note: the Context below states that the
identity rule should work across Rust, Clojure/JVM, Python, JavaScript, RDF
tooling, and Nix. As of this date every implementation and every determinism
check is Clojure/JVM-only (see ADR 0011's two-run byte-identity test and ADR
0010's single-JCS-implementation note). The cross-language claim is therefore a
*target*, not a verified v0 property. Two factors mitigate the largest JCS
divergence risk (RFC 8785 number canonicalization): `manifest_identity_object`
is composed almost entirely of `sha256:<hex>` string fields, and the schema-hash
and request-set-hash disciplines operate on JSON Schema / JSON values rather
than on bare numeric payloads. Nothing in the schema currently forbids a future
identity field from carrying a number, date, or non-normalized string, so a
future amendment MUST treat such a field as requiring a cross-language
conformance fixture before it is admitted to
`manifest_identity_object`.

## Context

ABC needs reproducible artifact identity for a living corpus. File paths,
parser labels, TEI profile names, and metadata records can change independently
of work text. The project needs a stable identity rule that works across Rust,
Clojure/JVM, Python, JavaScript, RDF tooling, and Nix.

## Terminology Clarification

In v0, `artifact_id` identifies the intended derivation coordinate: the exact
corpus input, work content, metadata record where applicable, parser/build
inputs, schema/profile/config inputs, and output format contract.

`artifact_id` is not the byte hash of the materialized output. Output bytes are
identified by `content.content_hash`.

A release MUST NOT contain two successful manifests with the same `artifact_id`
and different `content.content_hash` values. Such a condition is a
reproducibility conflict and must be represented as a failed release validation
result.

## Decision

The v0 ArtifactID is:

```text
sha256:<hex of SHA-256(RFC8785-JCS(manifest_identity_object))>
```

The canonical manifest is JSON. RDF/PROV-O, Turtle, JSON-LD, RO-Crate, and
indexes are derived views or publication packages.

`artifact_id` is never nested inside `manifest_identity_object`; including it
there would make identity circular.

The `manifest_identity_object` contains only identity-bearing fields:

- `manifest_schema_hash`
- `corpus_snapshot_hash`
- `work_content_hash`
- `metadata_record_hash`
- `parser_build_hash`
- `parser_config_hash`
- `parser_ir_schema_hash`
- `tei_profile_hash`
- `tokenizer_build_hash`
- `tokenizer_dictionary_hash`
- `analysis_recipe_hash`
- `output_format_spec_hash`

This is the v0 baseline field set. Later ADRs amend this list: ADR 0023 adds
`aat_parser_ir_mapping_hash` and ADR 0027 adds `tokenizer_profile_hash`
(see the `Amended by:` header field above). The authoritative current field
set and nullability contract is the bundled `schemas/manifest.schema.json`;
this ADR records the identity *rule* (each field is identity-bearing, null
means "not applicable" not "unknown", arrays are sorted before JCS), not a
frozen field inventory. See `docs/adr/README.md` ("Manifest identity
invariants") for the global null-semantics and non-circularity invariants.

Absent dimensions are present as JSON `null`. Arrays inside identity fields
must either have schema-defined order or be sorted before hashing by a stable
key. RFC 8785 JCS does not sort arrays. For hash-only arrays that do not carry
semantic order, v0 sorts lexicographically by full `sha256:<hex>` string before
JCS is applied. Arrays outside `manifest_identity_object` are not
identity-bearing unless a later schema explicitly promotes them.

`output_format_spec_hash` is always present in v0. For failure manifests it
identifies the intended output contract, such as the parser IR schema or TEI
profile that the failed activity attempted to produce.

Schema hashes are SHA-256 over a bundled JSON Schema document canonicalized
with RFC 8785 JCS. Pretty-printing, source-file whitespace, and object member
order in the checked-in schema file do not affect the schema hash. External
`$ref` resolution and default expansion do not occur during hash computation.
If a schema is split across files, the bundled schema is created first and
becomes the schema artifact.

The v0 schema hash algorithm label is:

```text
sha256-rfc8785-jcs-bundled-json-schema-v0
```

## Consequences

- Every schema version is a distinct hash-addressed artifact.
- Old manifests retain their original `manifest_schema_hash`.
- Backward compatibility means consumers can read multiple schema versions; it
  does not mean future ArtifactIDs keep old schema hashes.
- Signatures, mirrors, SWHIDs, CID aliases, release channel, and generated time
  are non-identity fields unless a later schema explicitly promotes them.
- Parser and tokenizer coordinates use build/source hashes, not host-local
  executable paths.
- The schema hash appears only inside `manifest_identity_object` in v0
  manifests, avoiding a second root value that could diverge.

## Acceptance Criteria

- **ADR-0001-C1 — fixture-behavior:** Committed success/failure manifests and
  freshly generated parser-IR/warnings manifests validate against
  `schemas/manifest.schema.json`, as asserted by
  `test/abc/tools/foundation_evidence_test.clj`.
- **ADR-0001-C2 — fixture-behavior:** Inline canonical values pin null
  dimensions and semantic array order. Evidence:
  `fixtures/canonicalization/manifest-identity-object.canonical.json` and
  `test/abc/tools/validate_design_bundle_test.clj`.
- **ADR-0001-C3 — structural-invariant:** The manifest schema rejects
  `artifact_id` nested inside `manifest_identity_object`. Evidence:
  `test/abc/tools/code_as_spec_test.clj` and
  `fixtures/v0/invalid/manifest-nested-artifact-id/manifest.json`.
- **ADR-0001-C4 — fixture-behavior:** The committed failure manifest carries
  the exact asserted input and attempted-output identity coordinates,
  validation status, null content, error sidecar, and top-level-only
  `artifact_id`, as asserted by
  `test/abc/tools/foundation_evidence_test.clj`.
- **ADR-0001-C5 — structural-invariant:** Two successful entries with the same
  `artifact_id` and different `content.content_hash` values produce a
  reproducibility conflict. Evidence:
  `test/abc/tools/code_as_spec_test.clj` and
  `test/abc/tools/manifest_index_test.clj`.

## Future Verification

Before any non-Clojure implementation is relied on for identity, a
cross-language RFC 8785 JCS conformance fixture (canonical bytes reproduced
byte-for-byte by at least one non-JVM implementation) is committed and
exercised. Until then, the cross-language language in the Context is treated
as a target, not a satisfied criterion.

## Rollback

If this rule is insufficient, define a new manifest schema hash and emit new
ArtifactIDs. Do not reinterpret old manifests under the new rule.
