# ADR 0011: Generated Fixture Policy

Date: 2026-04-26
Status: Accepted

## Context

The v0 design bundle imports parser comparison output from `examples/ab-validator-output/` and materializes ABC manifests from it. Without a checked-in generated fixture, changes to materialization code can silently alter manifest bytes while still passing schema validation.

The high-level architecture note treats artifact identity, canonicalization, and generated publication views as reproducibility boundaries. The materialized import fixtures are the first concrete regression target for that boundary.

## Decision

Check in generated materialized-import manifests under `examples/materialized-import/`.

The source of truth for those files is:

```bash
nix run .#materialize-import -- examples/ab-validator-output examples/materialized-import 2026-04-26T00:00:00Z
```

`nix run .#validate-design-bundle` regenerates the same manifests into a temporary directory and compares them byte-for-byte with the checked-in fixture files.

Generated JSON files must be written deterministically:

- Object keys are emitted in lexicographic order at every object level.
- Array order is preserved, because arrays are semantic order unless a producing function sorts them before writing.
- `manifest_identity_object` remains the only input to `artifact_id`.
- `artifact_id` is never nested inside `manifest_identity_object`.
- For the current manifest writer, `provenance.used` and `provenance.was_derived_from` are sorted by `artifact-manifest` before output so their semantic order is deterministic before JSON writing.

## Consequences

The checked-in fixture becomes a reviewable contract for materialized import output.

Changes to generated manifest bytes require an intentional fixture update in the same commit as the code or schema change that caused them.

Byte-for-byte comparison is stricter than structural comparison. This is intentional for v0 because it exposes nondeterministic writers before the project has multiple implementations.

## Acceptance Criteria

- `examples/materialized-import/parser-ir.manifest.json` exists and validates against `schemas/manifest.schema.json`.
- `examples/materialized-import/warnings.manifest.json` exists and validates against `schemas/manifest.schema.json`.
- `nix run .#validate-design-bundle` regenerates both files and fails if either checked-in fixture differs.
- `clojure -M:test` includes a focused test proving deterministic JSON output for maps with differently ordered input keys.
