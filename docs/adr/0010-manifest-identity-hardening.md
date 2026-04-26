# ADR 0010: Manifest Identity Hardening

Status: Draft
Date: 2026-04-26
Supersedes: none
Source: `docs/adr/0001-manifest-identity.md` and
`docs/adr/0009-imported-output-materialization.md`

## Context

The first materialization tool generates parser IR and warning manifests from an
imported `ab-validator` bundle. That proves the boundary, but the first
implementation still contained two v0 shortcuts:

- `manifest_schema_hash` was a fixture constant.
- The identity canonicalization function was named as if it were general JSON
  canonicalization.

Manifest identity is too central to leave those shortcuts hidden.

## Decision

Materialized manifests compute `manifest_schema_hash` from the checked-in
`schemas/manifest.schema.json` bytes.

Imported bundles may provide `parser_ir_schema_hash` and
`diagnostic_schema_hash`. ABC records those supplied values because they describe
the producer's declared output contract. The design-bundle validation command
also records the current checked-in parser IR schema hash as diagnostic context
through tests and validation, but it does not silently replace producer-supplied
hashes.

The narrow identity serializer is named `v0-identity-json`, not
`canonical-json`. It is not a full JCS/RFC 8785 implementation. It supports only
the v0 identity-object value domain: maps with string keys, string values, and
`null`, sorted by key.

## Consequences

- Generated `artifact_id` values now change if `schemas/manifest.schema.json`
  changes.
- Generated manifests are more defensible, because schema identity is tied to a
  real repo artifact rather than a placeholder.
- Full JCS remains deferred. A future public-release ADR must replace
  `v0-identity-json` before treating generated ArtifactIDs as stable release
  identifiers.

## Acceptance Criteria

- Materialized manifests contain `manifest_schema_hash` equal to the SHA-256 of
  `schemas/manifest.schema.json`.
- Parser IR and warning materialized manifests have distinct `artifact_id`
  values.
- Tests cover `v0-identity-json`, schema-file hashing, and generated manifest
  identity fields.
- `nix run .#validate-design-bundle` still validates materialized manifests.

## Rollback

If schema hash calculation has to change, introduce a new manifest schema hash
and ArtifactID coordinate. Do not reinterpret manifests generated under this
rule.
