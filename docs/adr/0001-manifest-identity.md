# ADR 0001: Manifest Identity

Status: Draft
Date: 2026-04-26
Supersedes: none
Source: `docs/high-level-architecture-note.md` v0.5

## Context

ABC needs reproducible artifact identity for a living corpus. File paths,
parser labels, TEI profile names, and metadata records can change independently
of work text. The project needs a stable identity rule that works across Rust,
Clojure/JVM, Python, JavaScript, RDF tooling, and Nix.

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
with RFC 8785 JCS. External `$ref` resolution and default expansion do not
occur during hash computation. If a schema is split across files, the bundled
schema is created first and becomes the schema artifact.

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

- `schemas/manifest.schema.json` validates success and failure manifests.
- A canonicalization fixture demonstrates null dimensions and array ordering.
- Example manifests keep `artifact_id` outside `manifest_identity_object`.
- Failure manifests include input identity, attempted recipe identity,
  validation status, and error sidecar references.

## Rollback

If this rule is insufficient, define a new manifest schema hash and emit new
ArtifactIDs. Do not reinterpret old manifests under the new rule.
