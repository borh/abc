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

1. **TEI ODD promotion.** Promote `schemas/tei-profile.odd` from
   stub to a project-specific ODD aligned with TEI P5 4.11.0 ruby
   support. Generate a project-specific RelaxNG via `roma`/`teiroma`
   and use it instead of `tei_all.rng` in `validate-design-bundle`.
   v0 currently validates against full TEI; an ODD-derived schema
   tightens what's accepted. Prerequisite for replacing the design
   fixtures with a real Aozora work end to end.
2. **Parser-decision exercise (ADR 0002).** Run a candidate parser
   (e.g. `aozora-rs`) over one Aozora work into the parser IR
   contract. Either validates the boundary or surfaces gaps before
   more code is written.
3. **Move legacy namespaces behind clj-nix.** `abc.aozora`,
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
