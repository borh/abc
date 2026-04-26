# ADR 0009: Imported Parser Output Materialization

Status: Draft
Date: 2026-04-26
Supersedes: none
Source: `docs/adr/0007-external-parser-validation-boundary.md` and
`docs/adr/0008-abc-tools-runtime.md`

## Context

ABC can validate an imported `ab-validator` output bundle, but it does not yet
turn that bundle into ABC artifact manifests. The next pipeline slice should
exercise ABC's central responsibility: materializing external parser output as
content-addressed artifact records.

## Decision

Add a Clojure command:

```bash
clojure -M:abc/materialize-import examples/ab-validator-output out/imported --generated-at 2026-04-26T00:00:00Z
```

Nix exposes the same command as:

```bash
nix run .#materialize-import -- examples/ab-validator-output out/imported --generated-at 2026-04-26T00:00:00Z
```

The command reads:

- `manifest-inputs.json`,
- `parser-ir.json`,
- `warnings.jsonl`.

It writes:

- `parser-ir.manifest.json`,
- `warnings.manifest.json`.

For v0, the command computes real content hashes and byte lengths for the local
files it materializes. It uses manifest input hashes supplied by
`ab-validator` for parser build, parser config, parser IR schema, diagnostic
schema, corpus snapshot, and work content. Parser IR uses `parser_ir_schema_hash`
as its `output_format_spec_hash`; warnings use `diagnostic_schema_hash` so the
two artifacts have distinct identities. Metadata, tokenizer, TEI, and analysis
dimensions are `null`.

## ArtifactID Rule

The materializer computes `artifact_id` as:

```text
sha256(RFC8785-JCS(manifest_identity_object))
```

The same JCS implementation is used for schema hashes and ArtifactID
computation. Deterministic pretty JSON output for generated regression tests is
not the ArtifactID canonicalization algorithm.

## Acceptance Criteria

- Generated parser IR and warnings manifests validate against
  `schemas/manifest.schema.json`.
- Generated content hashes match the actual imported files.
- Generated artifact IDs are distinct from content hashes.
- Producer-supplied parser IR and diagnostic schema hashes match the checked-in
  ABC schemas used for validation, unless a registered compatibility rule
  exists.
- The design-bundle validation command materializes the fixture into a temporary
  directory and validates the generated manifests.
- The Bash wrapper remains a compatibility shim; Clojure owns the logic.

## Rollback

If the materialized manifest shape changes, keep the command path stable and
version the manifest schema hash. Existing generated manifests remain valid only
under the schema hash used to produce them.
