# Next Steps

## Current State

Seven contract-harness milestones complete:

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
- **2026-04-29 EDTF Level 1 decade and BCE century prose
  (ADR 0016).** v0.1 widens the EDTF lexical grammar admitted
  by `nullableDate` and the SHACL `abc:EDTF` pattern to the
  union `L0 ∪ \d{3}X ∪ \d{2}XX` (with optional leading `-`
  for BCE). The parser admits `192X`-style decade markers
  verbatim (no transformation, no audit entry) and translates
  Japanese BCE century prose `紀元前N世紀(初頭|初|末|半ば|前半|後半)?`
  to `-{N-1:02d}XX` under astronomical year numbering — lossless
  at century precision (`紀元前N世紀` covers the closed
  astronomical interval `[-(100N-1), -((N-1)·100)]`, which is
  exactly what the EDTF marker matches). The qualifier is
  preserved verbatim in the `parse_corrections` audit trail
  under rule `century-prose`; EDTF Level 1 has no sub-century
  precision, so the qualifier is intentionally not encoded in
  the canonical lexical form. CE century prose is deferred:
  conventional 7世紀 = 601..700 does not align with EDTF `06XX`
  = 0600..0699 without a 1-year loss, and the corpus contains
  no CE prose for persons. `record->graph` emits decade /
  century shapes only as the `abc:edtf*` echo typed `abc:EDTF`;
  the RDA Group 2 predicates (`rdag2:dateOfBirth/Death`) are
  omitted because XSD's coarsest temporal type is `xsd:gYear`
  and there is no precision-honest XSD literal at decade or
  century granularity. End-to-end corpus ingest now reaches
  17,810 / 17,810 works and 1,334 / 1,334 persons with zero
  skips: サッフォ (`紀元前7世紀末` / `紀元前6世紀初` →
  `-06XX` / `-05XX`) and シェミン マーガレット (`192X`) are
  restored to canonical identity. The schema-hash cascade
  rotates again under the widening; all fixtures regenerate
  byte-identically.
- **2026-04-29 TEI ODD promotion (ADR 0012).**
  `schemas/tei-profile.odd` is now the canonical TEI contract,
  and both `schemas/tei-profile.rng` and `schemas/tei-profile.sch`
  are reproducibly derived from it. The Nix derivation
  `tei-profile-artifacts` (defined inline in `flake.nix`) pins
  TEI Stylesheets v7.60.0, p5subset.xml from TEI P5 4.11.0, and
  Saxon-HE 12.9, runs `odd2odd.xsl → odd2relax.xsl` and
  `odd2odd.xsl → extract-isosch.xsl`, then applies a narrow
  build-artifact canonicalization step: strip generation-timestamp
  comments, rewrite the seven ABC `constraintSpec` pattern IDs
  from `schematron-constraint-<ident>-<seq>` back to the bare
  `<ident>`, and drop inherited TEI built-in patterns the v0 ABC
  Schematron evaluator does not implement (`<sch:let>` and
  `role="nonfatal"` semantics live there). The ODD picks up
  `<moduleRef key="gaiji"/>` (so `<g>` / `<charDecl>` / `<char>`
  are admitted under their real module) and
  `<moduleRef key="analysis"/>` (so `tei:w | tei:m | tei:pc`
  resolve under `abc-transcription-vs-annotation`); the warning
  `role` attributes for `abc-figure-accessibility` and
  `abc-transcription-vs-annotation` move into the ODD itself.
  Nine fixtures restructure `<publicationStmt>` into the
  `(publisher, idno)` form so they pass real RNG, and
  `figure-missing-desc.xml` wraps its `<figure>` in `<p>`. The
  two genuinely-structurally-invalid TEI fixtures
  (`missing-title.xml`, `ruby-missing-reading.xml`) are removed
  from the project-RNG validation list and remain Schematron-only
  fixtures; their ABC rules (`abc-tei-header-title`,
  `abc-ruby-complete`) are RNG-redundant for those cases. To
  regenerate after an ODD edit, run
  `nix run .#regenerate-tei-profile`. `nix flake check` runs the
  new `tei-profile-drift` check, which regenerates and byte-diffs
  against the committed artifacts; the build fails on any drift.

## Canonical Commands

```bash
nix run .#validate-design-bundle
nix run .#aozora-ingest -- --zip references/aozorabunko/index_pages/list_person_all_extended_utf8.zip --work-id 000127 --output examples/v0/example-work/metadata-record.json
nix run .#aozora-ingest -- --zip references/aozorabunko/index_pages/list_person_all_extended_utf8.zip --all --output-dir out/corpus
nix run .#validate-corpus -- --input-dir out/corpus
nix run .#materialize-import -- examples/ab-validator-output out/imported --generated-at 2026-04-26T00:00:00Z
nix run .#manifest-to-rdf -- examples/v0/example-work/manifest.json -o out/manifest.ttl
nix run .#regenerate-tei-profile
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
2. **TEI rule expansion.** ADR 0012's seven-rule Schematron set
   is deliberately narrow. Future widenings (additional ruby
   shapes, source-span coverage tightening, gaiji-resolution
   policy) extend the ODD's `constraintSpec` block and re-run
   the drift gate; the ABC v0 Schematron evaluator's lack of
   `<sch:let>` and `role="nonfatal"` support is the current
   ceiling on inherited TEI rules.
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
