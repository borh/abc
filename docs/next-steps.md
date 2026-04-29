# Next Steps

## Current State

Five contract-harness milestones complete:

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
- **2026-04-29 Corpus-scale CSV ingestion.** `aozora-ingest --all
  --output-dir <DIR>` streams every work in
  `list_person_all_extended_utf8.csv` into 17,810 metadata-record
  files under `<DIR>/works/` plus 1,334 deduplicated person-record
  files under `<DIR>/persons/`. Schemas widened to reflect the
  corpus reality (multi-category and absent NDC, mononym persons
  with no `given_name`); single-work fixtures regenerate
  byte-identically against the new shape.
- **2026-04-29 Corpus SHACL sweep.** `validate-corpus --input-dir
  <DIR>` walks each `works/<work_id>.json`, composes its
  RDF view with cached person bodies, and runs the manifest SHACL
  shapes. The full corpus passes (17,810 / 17,810). Defensiveness
  fixes surfaced by the sweep: `:dc/subject` is omitted when `ndc`
  is null and `dc:subject` in the SHACL shape relaxed from
  `minCount 1` to `maxCount 1`.
- **2026-04-29 Temporal modeling (ADR 0015).** Partial dates
  (`1941`, `1904-01`) and 前N CE-relative BCE notation are
  preserved end-to-end instead of dropped. The schema
  `nullableDate` accepts a constrained EDTF lexical subset
  (`^-?\d{4}(-(0[1-9]|1[0-2])(-(0[1-9]|[12][0-9]|3[01]))?)?$ |
  null`) and `person-record/validate!` additionally rejects
  calendar-impossible dates (Feb 31, etc.) via
  `java.time.LocalDate`. The parser auto-corrects shape-only
  deviations (zero-padding month/day/year, stripping whitespace)
  and converts 前N to ISO 8601-2 / XSD 1.1 astronomical year
  numbering (前1→`0000`, 前427→`-0426`), maps the Japanese
  unknown-marker sentinels (`不詳`, `未詳`) to JSON null under
  rule `unknown-marker`, and collapses obvious double-dash typos
  (`1850-08--18` → `1850-08-18`). Each rewrite is recorded
  as a `parse_corrections` audit-trail entry under the person
  record's `source_csv_provenance` (the ingester computes
  `original_file_hash` from the CSV bytes and reads
  `retrieved_at` from the ZIP entry mtime; `source_url` comes
  from `--source-url` or stays null). The RDF view emits the
  most precise XSD type per value (`xsd:date` /
  `xsd:gYearMonth` / `xsd:gYear`) under `rdag2:dateOfBirth`/
  `rdag2:dateOfDeath`, plus an `abc:edtfDateOfBirth`/
  `abc:edtfDateOfDeath` echo carrying the canonical EDTF lexical
  string under the custom `abc:EDTF` datatype. SHACL relaxes the
  RDA Group 2 dates to `sh:or` of the three precision types and
  pattern-checks the EDTF echo. The corpus ingest path
  (`run-corpus!`) catches per-record `validate!` failures, logs
  a structured warning naming the offending field and value, and
  continues, so a handful of EDTF Level 1 shapes still in the
  source data (`192X` decade markers, `紀元前7世紀末` century-
  level prose) cannot abort an end-to-end run; the resulting
  output reports a `:works-skipped` count alongside
  `:works-written`. End-to-end against the full
  `list_person_all_extended_utf8.csv`: 17,806 / 17,810 works
  written, 1,331 / 1,334 persons written; the four skipped
  works reference two persons with century-level / decade-only
  dates that fall outside v0 grammar and need a future ADR for
  EDTF Level 1.

## Canonical Commands

```bash
nix run .#validate-design-bundle
nix run .#aozora-ingest -- --zip references/aozorabunko/index_pages/list_person_all_extended_utf8.zip --work-id 000127 --output examples/v0/example-work/metadata-record.json
nix run .#aozora-ingest -- --zip references/aozorabunko/index_pages/list_person_all_extended_utf8.zip --all --output-dir out/corpus
nix run .#validate-corpus -- --input-dir out/corpus
nix run .#materialize-import -- examples/ab-validator-output out/imported --generated-at 2026-04-26T00:00:00Z
nix run .#manifest-to-rdf -- examples/v0/example-work/manifest.json -o out/manifest.ttl
nix flake check
bin/update-clj-nix-lock
```

## Candidate Next Milestones

In rough order of leverage, none committed:

1. **Vocabulary review.** Audit the `abc:` predicates introduced by
   the metadata milestone (`abc:orthographicStyle`,
   `abc:copyrightExpired`, `abc:familyNameReading`, etc.) against
   possible standard alternatives once the legacy `aozora:`
   namespace question is resolved.
2. **TEI ODD promotion.** Promote `schemas/tei-profile.odd` from
   stub to a project-specific ODD aligned with TEI P5 4.11.0 ruby
   support. Generate project-specific Relax NG and Schematron via
   `roma`/`teiroma` or equivalent, then use both layers in
   `validate-design-bundle`.
3. **Cultural-heritage publication profile.** Evaluate the derived
   Linked Art JSON-LD crosswalk and IIIF applicability fixtures
   without changing canonical manifest identity.
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
