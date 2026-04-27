# Next Steps

## Current State

The v0 contract harness milestone (archived as
`docs/archive/2026-04-27-v0-contract-harness.md`) is complete. The
canonical-JSON ↔ RDF/PROV-O loop is closed: every manifest produced or
carried by the v0 fixtures is validated against
`schemas/manifest.shacl.ttl` during `nix run .#validate-design-bundle`.

## Canonical Commands

```bash
nix run .#validate-design-bundle
nix run .#materialize-import -- examples/ab-validator-output out/imported --generated-at 2026-04-26T00:00:00Z
nix run .#manifest-to-rdf -- out/imported/parser-ir.manifest.json -o out/imported/parser-ir.ttl
nix flake check
bin/update-clj-nix-lock
```

## Candidate Next Milestones

In rough order of leverage, none committed:

1. **Failure-manifest Turtle parity test.** Now that the failure RDF
   mapping is exercised end-to-end, consider committing
   `examples/v0/example-work/failure-manifest.example.ttl` plus a
   byte-for-byte parity test (analogue of the success-case
   `manifest-to-rdf-matches-example-fixture-test`). Deferred from the
   2026-04-27 milestone to avoid churn during the mapping shakedown.
2. **TEI profile + validation pipeline.** Promote
   `schemas/tei-profile.odd` from stub to a real ODD aligned with TEI
   P5 4.11.0 ruby support. Generate Relax NG; add Jing or `xmllint`
   validation as a `validate-design-bundle` step. Unlocks replacing
   the design fixtures with a real Aozora work end to end.
3. **Parser-decision exercise (ADR 0002).** Run a candidate parser
   (e.g. `aozora-rs`) over one Aozora work into the parser IR
   contract. Either validates the boundary or surfaces gaps before
   more code is written.
4. **Tighten ArtifactShape.** The current `sh:or` in
   `schemas/manifest.shacl.ttl` duplicates the
   `abc:hasErrorArtifact sh:minCount 1` requirement in branch 2 and
   in `FailureShape`. Clean up if/when shapes are revisited.
5. **Move legacy namespaces behind clj-nix.** `abc.aozora`,
   `abc.tei`, `abc.stats` remain outside the v0 contract gate. Decide
   whether to bring them in or leave them dormant.

## Done Criteria For The Closed Milestone

See `docs/archive/2026-04-27-v0-contract-harness.md`. Briefly:
- `nix run .#validate-design-bundle` passes including
  `==> Validating SHACL shapes`.
- `nix flake check` passes including the focused-test check.
- `examples/v0/example-work/manifest.ttl` byte-for-byte parity test
  remains stable.
- Failure manifest fixture exercised end to end (JSON → graph → SHACL).
