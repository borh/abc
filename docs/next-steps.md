# Next Steps

## Current State

Two contract-harness milestones complete:

- **2026-04-27 v0 contract harness** (archived as
  `docs/archive/2026-04-27-v0-contract-harness.md`). Closes the
  canonical-JSON ↔ RDF/PROV-O loop: every manifest produced or
  carried by the v0 fixtures is validated against
  `schemas/manifest.shacl.ttl`.
- **2026-04-27 TEI P5 RelaxNG validation**. New
  `==> Validating TEI against P5 RelaxNG` step uses Jing in-process
  with the upstream `tei_all.rng` schema pinned via
  `pkgs.fetchurl` in the flake. `abc.tools.tei/validate!` returns
  structured violations; harness partitions warnings (Telemere log)
  from errors/fatals (throw). Spec at
  `docs/superpowers/specs/2026-04-27-tei-relaxng-validation-design.md`.

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

1. **Metadata data model mapping.** Promote
   `src/abc/aozora.clj`'s Malli registry (work, person, source, NDC,
   dates, references) into the v0 contract surface: JSON Schema for
   `metadata-record.schema.json`, canonical form policy backing
   `metadata_record_hash`, RDF/PROV mapping for metadata records,
   one example metadata-record fixture under `examples/v0/`, and
   harness validation of metadata manifests. Closes the
   `metadata_record_hash` story in `manifest_identity_object`
   (currently nullable but unused) and unblocks TEI `<teiHeader>`
   generation downstream.
2. **TEI ODD promotion.** Promote `schemas/tei-profile.odd` from
   stub to a project-specific ODD aligned with TEI P5 4.11.0 ruby
   support. Generate a project-specific RelaxNG via `roma`/`teiroma`
   and use it instead of `tei_all.rng` in `validate-design-bundle`.
   v0 currently validates against full TEI; an ODD-derived schema
   tightens what's accepted. Best after a real Aozora-work fixture
   lands, since the ODD shape depends on what TEI constructs the
   corpus actually uses.
3. **Parser-decision exercise (ADR 0002).** Run a candidate parser
   (e.g. `aozora-rs`) over one Aozora work into the parser IR
   contract. Either validates the boundary or surfaces gaps before
   more code is written. Currently parked while parser work
   happens in another project.
4. **Move legacy namespaces behind clj-nix.** `abc.aozora`,
   `abc.tei`, `abc.stats` remain outside the v0 contract gate.
   Decide whether to bring them in or leave them dormant. Most
   useful when one of #1 or #3 starts touching `abc.aozora`
   actively.

## Done Criteria For The Closed Milestones

See archive notes. Briefly:

- `nix run .#validate-design-bundle` passes through every step:
  materialize, JSON schema, manifest index, RDF views, SHACL
  shapes, ab-validator output, canonicalization, XML well-formedness,
  TEI RelaxNG, git-cliff config.
- `nix flake check` passes (focused-test sandbox skips TEI tests
  via `ABC_TEI_SCHEMA_SKIP=1`; end-to-end TEI validation runs
  through `nix run`).
- Example success and failure manifests have byte-for-byte parity
  tests for both JSON and Turtle representations.
