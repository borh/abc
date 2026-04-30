# Next Steps

## Current State

Fifteen contract-harness milestones complete:

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
- **2026-04-29 Linked Art publication view (ADR 0013).**
  ABC now derives a Linked Art-flavored JSON-LD publication view from
  the canonical manifest plus its metadata record without making it an
  identity input. The harness `abc.tools.linked-art` pins
  titanium-json-ld 1.7.0 (Jakarta JSON-P 2.0.1 comes in transitively
  via Jena), builds a deterministic candidate
  (`linked-art-candidate.jsonld`), runs JSON-LD 1.1 expansion against
  an in-memory `DocumentLoader` that resolves only the ABC public
  context URI `https://w3id.org/abc/contexts/abc-v0.jsonld` to bytes
  from `contexts/abc-v0.jsonld` and explicitly refuses every other
  URL, then writes `linked-art-expanded.normalized.json` and
  `jsonld-context-validation-result.json` (status `ok`, recomputed
  context hash, identity-invariant block). `contexts/abc-v0.jsonld`
  is self-contained — no remote `linked.art` context fetch — and
  carries CIDOC-CRM term aliases (`HumanMadeObject` → `crm:E22`,
  `content` → `crm:P190_has_symbolic_content`,
  `language` → `crm:P72_has_language` with `@type: @id`, plus
  `Name`/`Identifier`/`Type`/`LinguisticObject`/`DigitalObject`).
  The three previously-stub LOD fixtures
  (`linked-art-candidate.jsonld` was hand-written and stale,
  `linked-art-expanded.normalized.json` was a `fixture_status:
  "placeholder"` stub, `jsonld-context-validation-result.json` was a
  `status: "not_run"` stub) are replaced with the harness's real
  byte-stable output. Identity invariant: the harness extracts the
  literal value at the expanded
  `https://w3id.org/abc/artifactId` predicate and asserts
  byte-equality with `manifest.json`'s `artifact_id`; mismatch raises
  and fails the bundle gate. `validate-design-bundle` regenerates
  the three LOD fixtures into a temp directory and byte-compares
  against the committed bytes (drift-gate); a draft IIIF
  applicability fixture under
  `examples/v0/example-work/iiif/applicability.json` is shape-checked
  in the same step (status ∈ {applicable, not_applicable,
  rights_blocker}; work_id matches `\d{6}`) ahead of ADR 0014's
  promotion. `abc.tools.linked-art-test` covers determinism, byte
  parity with the committed fixtures, the recomputed context hash,
  the identity invariant, and the loader's refusal to fetch
  external JSON-LD contexts. `docs/lod/linked-art-crosswalk.md` and
  `docs/lod/json-ld-context-policy.md` flip from Draft to Active.
- **2026-04-29 TEI Schematron rule expansion (ADR 0012 widening).**
  Three rules added to `schemas/tei-profile.odd`, regenerated through
  the same TEI-Stylesheets / Saxon pipeline as the seven baseline
  rules so the existing `tei-profile-drift` flake check keeps the
  generated `tei-profile.sch` honest:
  - `abc-ruby-base-non-empty` (error): `tei:ruby/tei:rb` must have
    non-empty content. Closes a loophole where
    `<rb></rb><rt>ねこ</rt>` passes the structural
    `abc-ruby-complete` check.
  - `abc-source-span-target-exists` (error): every fragment id in
    `@source` must resolve to an `@xml:id` in this document. The
    pre-existing `abc-source-span-reference` only enforced the `#`
    prefix; dangling local references like `#nonexistent-target`
    used to pass.
  - `abc-gaiji-chardecl-resolution` (error): `tei:g[starts-with(@ref,
    '#')]` must point to a `tei:charDecl/tei:char` declaration in
    the document. Pre-existing `abc-gaiji-reference` only required
    *some* `@ref`/`@corresp`/`@ana` attribute, so a `<g
    ref="#nonexistent-gaiji"/>` with no matching declaration used to
    pass. (The all-lowercase `chardecl` segment is required by the
    flake's pattern-id canonicalization regex; mixed-case ids would
    be filtered out as inherited TEI patterns.) Three new fixtures
    under `fixtures/tei/invalid/` (`ruby-empty-base.xml`,
    `source-span-dangling-ref.xml`, `gaiji-dangling-ref.xml`) cover
    the new rules; `validate-design-bundle` runs each through
    project-RNG and Schematron with the expected single-rule
    findings, and `abc.tools.schematron-test` adds a deftest per
    rule. The `source-span-external-ref.xml` fixture now correctly
    trips both `abc-source-span-reference` and
    `abc-source-span-target-exists` (the `external-ref` value has
    no `#` and no matching `@xml:id`); the test was updated to
    accept the set of findings rather than a single id. The v0
    Schematron evaluator's `<sch:let>` and `role="nonfatal"`
    limitations remain the ceiling on what inherited TEI rules can
    be promoted.
- **2026-04-29 Vocabulary review + `abc:` namespace consistency
  (ADR 0017).** Audited every `abc:` predicate and class introduced
  by the metadata, person-record, and LOD milestones against
  candidate standard vocabularies (DCNDL, FOAF, Dublin Core,
  schema.org, RDA Group 2, CIDOC-CRM, PROV-O). Documented the
  per-predicate decision (keep / defer-switch / needs-research) in
  ADR 0017's audit table; this becomes the v0 contract surface for
  `abc:` predicates. Surfaced a real correctness bug: the `abc:`
  prefix was declared as `https://w3id.org/abc/` in every committed
  TTL fixture and SHACL shape, but as `https://w3id.org/abc/vocab#`
  in `contexts/abc-v0.jsonld` and the linked-art harness, so the
  same predicate (`abc:artifactId`) resolved to two different IRIs
  depending on serialization. Fix: aligned the JSON-LD context to
  the TTL form (`https://w3id.org/abc/`); updated the linked-art
  harness's `aozora-work-id-type-uri` constant and the identity-
  invariant predicate URI (`https://w3id.org/abc/artifactId`); and
  regenerated the three LOD fixtures (context hash rotates to
  `sha256:1cf7c68d…`, identity invariant holds end-to-end). All
  TTL fixtures, manifest identity hashes, and `abc:EDTF` datatype
  URIs are unchanged. Predicate renames are explicitly out of
  scope and deferred to follow-up ADRs that batch them with the
  schema-hash cascade. The legacy `aozora:` namespace question is
  resolved as part of this audit: no v0 code or fixture references
  it; only the `references/archive/aozora_lod_data/` historical
  copy remains.
- **2026-04-29 IIIF applicability promotion (ADR 0014).** ADR 0014
  flips from Draft to Accepted. The per-work applicability decision
  record now has a real JSON Schema 2020-12 contract at
  `schemas/iiif-applicability.schema.json`, with conditional
  `allOf` clauses asserting that `derived_manifest` is a non-empty
  string when `status="applicable"` and `null` for
  `status="not_applicable"` or `"rights_blocker"`. The ad-hoc
  shape-check `validate-iiif-applicability!` in
  `abc.tools.validate-design-bundle` is replaced by the schema-driven
  gate `abc.tools.iiif/validate-applicability!`; the schema itself is
  meta-schema-validated alongside the other v0 schemas.
  `examples/v0/example-work/iiif/applicability.json` drops its
  documentation-style `policy` block (the policy lives in the ADR's
  decision matrix; each record carries only the per-work outcome) and
  validates clean as `not_applicable` for the text-only 羅生門
  example. Five contract fixtures under `fixtures/iiif/` cover the
  positive cases (`applicable.json` with a `derived_manifest` path,
  `rights_blocker.json`) and the conditional negatives
  (`invalid/missing-work-id.json`,
  `invalid/applicable-without-manifest.json`,
  `invalid/not-applicable-with-manifest.json`).
  `abc.tools.iiif-test` exercises all six paths and is wired into the
  flake's focused-test alias. No v0 production IIIF Presentation
  manifests ship in this milestone; the schema and gate close out the
  cultural-heritage publication profile started by ADR 0013, and the
  `applicable` row is reserved for future works with confirmed
  facsimiles and clean rights.
- **2026-04-29 Predicate rename batch 1 (ADR 0018).** The two clean
  defer-switch candidates from ADR 0017 land:
  `abc:reading` (title kana) → `dcndl:titleTranscription`, and
  `abc:copyrightExpired` (xsd:boolean) → `dcterms:rights` IRI from
  the closed set
  `{<https://creativecommons.org/publicdomain/mark/1.0/>,
    <http://rightsstatements.org/vocab/InC/1.0/>}` (mapping
  `true → PD-mark`, `false → InC`). The JSON metadata-record contract
  is unchanged: `title_reading` stays a string, `copyright_expired`
  stays a boolean. Therefore `metadata_record_schema_hash`,
  `person_record_schema_hash`, the example's `metadata_record_hash`,
  `manifest_identity_object`, and `manifest.json` all stay byte-for-byte
  identical — no schema-hash cascade rotates. What rotates: one
  property shape in `schemas/manifest.shacl.ttl` (now
  `sh:path dcterms:rights ; sh:nodeKind sh:IRI ; sh:in (…)`) and
  `examples/v0/example-work/metadata-record.ttl` (the title blank node
  switches `abc:reading` → `dcndl:titleTranscription`; the boolean
  triple is replaced by `dcterms:rights <…/publicdomain/mark/1.0/>`
  for 羅生門). The three ADR 0017 needs-research items
  (per-component name reading / sort / romaji) remain deferred —
  their proposed standard alternatives operate on whole-name strings,
  not family/given components, and adopting them would lose
  component-level distinction the corpus needs.
- **2026-04-29 TEI rule expansion batch 2.** Three new ABC Schematron
  rules added to `schemas/tei-profile.odd` and propagated through the
  pinned `tei-profile-artifacts` derivation: `abc-ruby-reading-non-empty`
  (mirrors the base check on `tei:rt`), `abc-char-resolution-form`
  (every `tei:charDecl/tei:char` declares at least one of
  `mapping`/`unicodeProp`/`localProp`/`desc`), and
  `abc-header-language-declared`
  (`teiHeader//profileDesc/langUsage/language[@ident]` is present and
  non-empty). Three invalid fixtures + three deftests cover the new
  rules; the inventory in `docs/tei-validation.md` advances from ten
  to thirteen ABC patterns. The fourth candidate from the previous
  milestone (parser-IR scope-qualifier expectation) is deferred — it
  needs a separate ADR to decide how
  `explicit`/`inferred`/`grouped`/`mid-word`/`ambiguous` are encoded
  in TEI before a Schematron rule can be written. The v0 evaluator's
  `<sch:let>` and `role="nonfatal"` ceiling on inherited TEI rules
  remains.

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

1. **Parser-decision exercise (ADR 0002).** Run a candidate parser
   (e.g. `aozora-rs`) over one Aozora work into the parser IR
   contract. Currently parked while parser work happens in another
   project.
2. **Person identity drift (Flavor 2).** Splits, merges, renames as
   PROV-style events; the separated-persons milestone scoped Flavor 1
   only.
3. **Move legacy namespaces behind clj-nix.** `abc.aozora`,
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
