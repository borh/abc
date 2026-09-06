# Manifest Identity Hardening

## Implementation Status

Accepted after manifest identity and schema hashing moved to the shared JCS
path. `abc.tools.manifest/schema-hash` computes schema identity from parsed
JSON Schema values, `artifact-id` hashes the RFC 8785/JCS identity object,
`materialize-import!` records that schema hash in generated manifests, and
`validate-design-bundle` fails imported fixtures whose producer-declared parser
IR or diagnostic schema hashes do not match ABC's checked-in schemas. Focused
materialization tests cover schema hashing, artifact identity, generated
content hashes, deterministic output, and generated-at independence.

## Context

The first materialization tool generates parser IR and warning manifests from an
imported `ab-validator` bundle. That proves the boundary, but the first
implementation still contained two v0 shortcuts:

- `manifest_schema_hash` was a fixture constant.
- The first materializer draft tied `manifest_schema_hash` to checked-in file
  bytes, which conflicted with ADR 0001's bundled-schema JCS rule.

Manifest identity is too central to leave those shortcuts hidden.

## Decision

Materialized manifests compute `manifest_schema_hash` with the same algorithm
as ADR 0001:

```text
sha256-rfc8785-jcs-bundled-json-schema-v0
```

For the current single-file schema, "bundled schema" means the parsed JSON value
from `schemas/manifest.schema.json`. The hash is computed from the canonical
JCS bytes of that JSON value, not from the source file bytes.

Imported bundles may provide `parser_ir_schema_hash` and
`diagnostic_schema_hash`. ABC records those supplied values because they describe
the producer's declared output contract. The design-bundle validation command
also records the current checked-in parser IR schema hash as diagnostic context
through tests and validation, but it does not silently replace
producer-supplied hashes.

If a producer-supplied `parser_ir_schema_hash` or `diagnostic_schema_hash`
differs from the hash of ABC's checked-in schema used for validation,
validation MUST fail unless the imported bundle explicitly declares a compatible
external schema and ABC has a registered migration/compatibility rule.

ABC's v0 identity and schema hashing code uses one JCS implementation rather
than a separate ad hoc sorted JSON writer. Deterministic pretty JSON output for
fixtures remains a separate writer and is not the ArtifactID canonicalization
algorithm.

## Consequences

- Generated `artifact_id` values now change when the parsed bundled schema
  value changes.
- Generated manifests are more defensible, because schema identity is tied to a
  real schema artifact rather than a placeholder or pretty-printed source
  bytes.
- Source-file whitespace and object member order changes in
  `schemas/manifest.schema.json` do not change `manifest_schema_hash`.

## Rollback

If schema hash calculation has to change, introduce a new manifest schema hash
and ArtifactID coordinate. Do not reinterpret manifests generated under this
rule.
