# Next Steps

## Current State

Four contract-harness milestones complete:

- **2026-04-27 v0 contract harness** (archived as
  `docs/archive/2026-04-27-v0-contract-harness.md`).
- **2026-04-27 TEI P5 RelaxNG validation** via Jing in-process,
  with the upstream `tei_all.rng` pinned via `pkgs.fetchurl`.
- **2026-04-28 Metadata data model + TEI header generation**.
  Validates a real Aozora CSV slice (羅生門 / 芥川竜之介, work
  000127, person 000879) end-to-end through schema → identity
  hash → SHACL → TEI-EAJ-aligned `<teiHeader>` → Jing.
  `nix run .#aozora-ingest` reproducibly builds the metadata-record
  from the local CSV slice.
- **2026-04-28 Separated person records.** Person bodies live in
  `examples/v0/example-persons/<person_id>.json`; works reference
  contributors via `{person_id, person_record_hash, relation_to_work}`.
  A person edit only invalidates works that reference that person.
  `nix run .#aozora-ingest --refresh-manifest` closes the work +
  manifest identity loop.

## Canonical Commands

```bash
nix run .#validate-design-bundle
nix run .#aozora-ingest -- --zip references/aozorabunko/index_pages/list_person_all_extended_utf8.zip --work-id 000127 --output examples/v0/example-work/metadata-record.json
nix run .#materialize-import -- examples/ab-validator-output out/imported --generated-at 2026-04-26T00:00:00Z
nix run .#manifest-to-rdf -- examples/v0/example-work/manifest.json -o out/manifest.ttl
nix flake check
bin/update-clj-nix-lock
```

## Candidate Next Milestones

In rough order of leverage, none committed:

1. **Corpus-scale CSV ingestion.** Stream the full Aozora CSV into
   many metadata-record + person-record artifacts; needed for any
   real corpus build. Two-stage CLI (persons-first, then per-work)
   pre-staged by the separated-persons milestone.
2. **Vocabulary review.** Audit the `abc:` predicates introduced by
   the metadata milestone (`abc:orthographicStyle`,
   `abc:copyrightExpired`, `abc:familyNameReading`, etc.) against
   possible standard alternatives once the legacy `aozora:`
   namespace question is resolved.
3. **TEI ODD promotion.** Promote `schemas/tei-profile.odd` from
   stub to a project-specific ODD aligned with TEI P5 4.11.0 ruby
   support. Generate a project-specific RelaxNG via `roma`/`teiroma`
   and use it instead of `tei_all.rng` in `validate-design-bundle`.
4. **Parser-decision exercise (ADR 0002).** Run a candidate parser
   (e.g. `aozora-rs`) over one Aozora work into the parser IR
   contract. Currently parked while parser work happens in another
   project.
5. **Person identity drift (Flavor 2).** Splits, merges, renames as
   PROV-style events; the separated-persons milestone scoped Flavor 1
   only.
6. **Move legacy namespaces behind clj-nix.** `abc.aozora`,
   `abc.tei`, `abc.stats` remain outside the v0 contract gate.

## Done Criteria For The Closed Milestones

See archive notes and milestone specs. Briefly:

- `nix run .#validate-design-bundle` passes through every step:
  materialize, JSON schema, manifest index, RDF views, SHACL
  shapes, **metadata record** (new), ab-validator output,
  canonicalization, XML well-formedness, TEI RelaxNG, git-cliff
  config.
- `nix flake check` passes (focused-test sandbox skips TEI tests
  via `ABC_TEI_SCHEMA_SKIP=1`; metadata-record tests run there).
- Example success and failure manifests have byte-for-byte parity
  tests for both JSON and Turtle representations. The metadata
  record adds a third fixture pair (`metadata-record.json` and
  `metadata-record.ttl`).
- Reproducible fixture: `nix run .#aozora-ingest` regenerates
  `metadata-record.json` byte-identically from the committed CSV
  slice.
