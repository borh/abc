# ADR 0011: Generated Output Policy

Date: 2026-04-26
Status: Accepted
Accepted: 2026-04-28
Validation scope: fixture
Release authority: development
Depends on: ADR 0001, ADR 0008, ADR 0009, ADR 0010

## Implementation Status

Acceptance criteria satisfied as of 2026-04-28. `nix run .#validate-design-bundle`
materializes parser IR and warning manifests into a temporary directory and
validates them; deterministic-JSON and two-run byte-identity tests live in
`test/abc/tools/materialize_import_test.clj`. Implementation tracked in archived
plan `docs/superpowers/plans/archive/2026-04-26-generated-fixture-policy.md`.

## Context

The v0 design bundle imports parser comparison output from `examples/ab-validator-output/` and materializes ABC manifests from it. Generated materialized manifests include derivation hashes that legitimately change when schema hash algorithms or schema coordinates change, so checking them in under `examples/` creates high churn.

The high-level architecture note treats artifact identity, canonicalization, and generated publication views as reproducibility boundaries. The materialized import command still needs deterministic-output coverage, but the regression target should be generated in temporary directories during validation rather than committed as example output.

## Decision

Do not require generated materialized-import manifests under `examples/materialized-import/` to be committed.

The reproducible generation command is:

```bash
nix run .#materialize-import -- examples/ab-validator-output examples/materialized-import --generated-at 2026-04-26T00:00:00Z
```

`nix run .#validate-design-bundle` materializes the imported fixture into a temporary directory and validates the generated manifests against `schemas/manifest.schema.json`. Focused tests generate the same manifests twice into separate temporary directories and compare those outputs byte-for-byte.

Generated JSON files must be written deterministically:

- Object keys are emitted in lexicographic order at every object level.
- Array order is preserved, because arrays are semantic order unless a producing function sorts them before writing.
- `manifest_identity_object` remains the only input to `artifact_id`.
- `artifact_id` is never nested inside `manifest_identity_object`.
- For the current manifest writer, `provenance.used` and `provenance.was_derived_from` are sorted by `artifact-manifest` before output so their semantic order is deterministic before JSON writing.

Deterministic generated JSON is for byte-for-byte regression tests only. It is
not the ArtifactID canonicalization algorithm. ArtifactID computation remains
governed solely by ADR 0001 and ADR 0010.

## Consequences

Generated materialized output does not create routine `examples/` churn.

Changes to generated manifest bytes must still be intentional: schema validation, identity tests, and two-run deterministic-output tests cover the materializer without committing generated manifests.

Byte-for-byte comparison between two generated outputs is stricter than structural comparison. This is intentional for v0 because it exposes nondeterministic writers before the project has multiple implementations.

## Acceptance Criteria

- **ADR-0011-C1 — operational-behavior:** The supported
  `nix run .#validate-design-bundle` app materializes parser-IR and warnings
  manifests into a temporary directory and validates both against
  `schemas/manifest.schema.json`, with fixture orchestration asserted by
  `test/abc/tools/validate_design_bundle_test.clj`.
- **ADR-0011-C2 — fixture-behavior:** Deterministic JSON writing produces
  identical ordered bytes from differently ordered map inputs, as asserted by
  `test/abc/tools/materialize_import_test.clj`.
- **ADR-0011-C3 — fixture-behavior:** Two materialization runs with identical
  inputs produce byte-identical manifest files, as asserted by
  `test/abc/tools/materialize_import_test.clj`.

## Rollback

If committing generated materialized-import manifests under `examples/`
becomes necessary, keep the `materialize-import` command path and the
`manifest_identity_object` rule from ADR 0001 / ADR 0010 stable and only change
where generated output is written. Do not reinterpret manifests materialized
under the temporary-directory rule; a switch to committed generated output is a
storage-policy change, not an identity-rule change.
