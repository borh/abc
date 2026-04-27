# Metadata Data Model — Design

Status: Approved
Date: 2026-04-27
Source milestone: `docs/next-steps.md` candidate "Metadata data model mapping"; sub-project (B) decomposed during the 2026-04-27 brainstorm; bundled with ingestion + TEI header generation per option (c).

## Goal

Promote the existing `src/abc/aozora.clj` Malli registry into the v0 contract surface as a JSON Schema, canonical hashing rule, RDF/PROV mapping, and TEI `<teiHeader>` generator. Wire all four into `validate-design-bundle`. Drive the example fixture from a real Aozora work (Akutagawa Ryūnosuke's 羅生門, work 000127, person 000879) committed as a CSV slice.

This closes the `metadata_record_hash` story in `manifest_identity_object` (currently nullable but unused on the example fixture) and demonstrates the corpus pipeline end to end: real CSV → metadata-record JSON → identity hash → RDF view → TEI-EAJ-aligned `<teiHeader>` → Jing validation.

## Non-Goals

- No corpus-scale ingestion. The ingester is unit-tested; the v0 fixture is a single-work slice.
- No referenced/normalized person records. Persons are embedded inside the metadata-record (Q2 (a)). Cross-work person identity is a follow-on milestone.
- No metadata-record artifact manifest. Metadata records are input-tier (like schema files), not derived artifacts; their hash flows into other manifests via `metadata_record_hash` but they don't have their own manifest file.
- No TEI ODD promotion. The generated `<teiHeader>` validates against the upstream `tei_all.rng` already wired in (TEI ODD promotion is a separate planned milestone).
- No JBDB cross-validation, NDL-SH lookups, or Software Heritage IDs. The existing `rdfs:seeAlso` shape is reproduced from the input data; not enriched.
- No streaming CSV. `charred.api/read-csv` slurps the small committed slice into memory.
- No exhaustive vocabulary review. We adopt standard predicates where they fit (Dublin Core, BIBO, FOAF, DCNDL, RDA, Schema.org) and put project-specific concepts under `abc:`. The legacy `aozora:` namespace from `references/archive/aozora_lod_data/` is not adopted; that data is reference, not specification. A separate vocabulary-review milestone can revisit if the legacy namespace turns out to be authoritative.

## Architecture

Five small units, each with one responsibility. Pattern matches the SHACL and TEI milestones.

### `schemas/metadata-record.schema.json` (new)

JSON Schema (Draft 2020-12). Single record per work, with an embedded `persons[]` array.

Top-level shape:

```jsonc
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://w3id.org/abc/schemas/metadata-record.schema.json",
  "title": "ABC Metadata Record",
  "type": "object",
  "additionalProperties": false,
  "required": ["work", "persons"],
  "properties": {
    "work": { "$ref": "#/$defs/work" },
    "persons": {
      "type": "array",
      "items": { "$ref": "#/$defs/person" },
      "uniqueItems": true,
      "minItems": 1
    },
    "source_csv_provenance": { "$ref": "#/$defs/csvProvenance" }
  }
}
```

`work` carries the bibliographic fields Aozora ships in `list_inp_xxxx.csv` (work-id, title, title-reading, NDC, orthographic-style, copyright-flag, available/modified dates, base-text reference). `person` mirrors `list_person_all_extended_utf8.csv` (person-id, family/given name, kana readings, romaji, dates of birth/death, copyright-expired flag).

The schema's `$defs` enumerate the existing Malli registry's enums verbatim:
- `orthographic_style` ∈ {`新字新仮名`, `新字旧仮名`, `旧字新仮名`, `旧字旧仮名`, `その他`}
- `relation_to_work` ∈ {`著者`, `翻訳者`, `編者`, `校訂者`, `その他`}
- NDC code: pattern `^NDC \d+$` plus a `$comment` referencing the canonical NDC list

`source_csv_provenance` is optional metadata: source URL, retrieval date, and original-file hash for the committed CSV slice. Not part of identity (excluded from canonical hashing).

### `src/abc/tools/aozora_csv.clj` (new)

CSV-row → metadata-record converter using `charred.api/read-csv`. Pure functions; no I/O beyond reading the supplied path.

API:

- `read-rows [path]` → seq of column-name-keyed maps (charred returns header + rows; this helper zips into maps).
- `parse-work-row [row-map]` → partial metadata-record (the `work` sub-map).
- `parse-person-row [row-map]` → person sub-document.
- `merge-person-into-work [work-map person-maps]` → full metadata-record (one work + N persons).
- `csv->metadata-record [{:keys [work-csv-path person-csv-paths]}]` → end-to-end convenience.

NFC normalization is applied at `parse-*-row` boundaries to title, name, and reading fields, consistent with the existing `abc.aozora` registry's encoding policy.

The Aozora CSV files committed under `examples/v0/example-work/aozora-csv/` are the UTF-8 versions Aozora ships at `https://www.aozora.gr.jp/index_pages/list_inp.html` and the person index. If only Shift-JIS sources are available locally, transcoding to UTF-8 happens once at fixture-build time (documented in `aozora-csv/PROVENANCE.md`); the harness only reads the committed UTF-8 files.

### `src/abc/tools/metadata_record.clj` (new)

Identity, validation, and RDF mapping. API mirrors `abc.tools.manifest` and `abc.tools.manifest-to-rdf`.

- `validate! [metadata-record]` — validates against `metadata-record.schema.json` via the existing `abc.tools.schema` helper. Throws on violation.
- `record-hash [metadata-record]` → `sha256:<hex>`. Computed via `abc.tools.jcs` over the canonical JSON form of `manifest_identity_object`-equivalent content (everything except `source_csv_provenance`, which is non-identity by construction).
- `record->graph [metadata-record]` → Aristotle/Jena graph using the vocabulary mapping defined below. The existing `references/archive/aozora_lod_data/` files are reference shapes only, not authoritative; this milestone improves on them.
- `record->ttl [metadata-record]` → deterministic Turtle, same shape as `manifest-to-rdf/manifest->ttl`.

**RDF vocabulary alignment.**

`references/archive/aozora_lod_data/` is the project owner's earlier work and is treated here as a **reference shape, not a specification**. Several predicates in that data live under `http://purl.org/net/aozora/` — a project-specific namespace whose authoritativeness has not been re-verified. This milestone improves on that data, so:

- **Standard vocabularies first.** Where Dublin Core, BIBO, FOAF, DCNDL, RDA Group 2, or Schema.org cover a concept, use the standard predicate and cite where each predicate is defined.
- **Project-specific in `abc:`.** Where no clean standard exists, define the predicate under the project's own `https://w3id.org/abc/` namespace (already used for artifacts and activities). Avoid the legacy `purl.org/net/aozora/` namespace — if it turns out to be the right home for any predicate later, that's a separate vocabulary decision.
- **Vocabulary-validation pass.** During Sequencing step 4, each proposed predicate gets a brief justification (which vocab, what the spec says it means) recorded as a comment in `src/abc/tools/metadata_record.clj`. Predicates without a clean standard mapping or a clear project rationale get pulled out and re-discussed before the parity test locks them in.

**Proposed mapping (each row to be verified during implementation):**

Work entity, IRI `http://www.aozora.gr.jp/cards/<6-digit-author-id>/card<work-id>.html`:

| JSON field | RDF predicate | Source |
|---|---|---|
| (entity type) | `a bibo:Document, schema:CreativeWork` | BIBO; Schema.org |
| `work_id` | `dcterms:identifier` (with literal datatype `abc:aozoraWorkId` if discriminator needed) | Dublin Core |
| `title` | `dcterms:title` (blank node carrying `rdf:value` + `dcndl:transcription` for kana reading) | DC + DCNDL |
| `alt_title` | `dcterms:alternative` | Dublin Core |
| `ndc` | `dcterms:subject` with literal `"NDC <code>"^^dcndl:NDC` | DC + DCNDL |
| `orthographic_style` | `abc:orthographicStyle` (project-specific; no standard) | `abc:` |
| `copyright_expired` | `abc:copyrightExpired` (xsd:boolean; standardize-vs-bibo:status decision deferred) | `abc:` |
| `aozora_available` | `dcterms:available` (xsd:date) | Dublin Core |
| `aozora_modified` | `dcterms:modified` (xsd:date) | Dublin Core |
| `author` (person ref) | `dcterms:creator` → person IRI | Dublin Core |
| `translator/editor/reviser` | `dcterms:contributor` with `<role>` blank-node carrying the original Japanese role string in `dcterms:role` (or a project-specific `abc:relationToWork`) | DC + `abc:` |

Person entity, IRI `http://www.aozora.gr.jp/index_pages/person<id>.html`:

| JSON field | RDF predicate | Source |
|---|---|---|
| (entity type) | `a foaf:Person` | FOAF |
| `person_id` | `dcterms:identifier` (with `abc:aozoraPersonId` discriminator if needed) | Dublin Core |
| `family_name` (kanji) | `foaf:familyName` | FOAF |
| `given_name` (kanji) | `foaf:givenName` | FOAF |
| `family_name_reading` (kana) | `dcndl:transcription` on a wrapping blank node, OR project-specific `abc:familyNameReading` if DCNDL doesn't model partial transcriptions | DCNDL or `abc:` |
| `given_name_reading` (kana) | analogous | DCNDL or `abc:` |
| `family_name_romaji` | `schema:alternateName` with `xml:lang="ja-Latn"`, or `abc:familyNameRomaji` | Schema.org or `abc:` |
| `given_name_romaji` | analogous | Schema.org or `abc:` |
| `family_name_sort` | `abc:familyNameForSort` (project-specific; no standard) | `abc:` |
| `given_name_sort` | analogous | `abc:` |
| `date_of_birth` | `rdag2:dateOfBirth` (xsd:date) | RDA Group 2 |
| `date_of_death` | `rdag2:dateOfDeath` (xsd:date) | RDA Group 2 |
| `full_name` | `foaf:name` | FOAF |
| `external_links[]` | `rdfs:seeAlso` | RDFS |

Each "or `abc:`" row is a decision deferred to the implementation step; the choice is recorded in code with a short rationale. The `MetadataRecordShape` SHACL only enforces the predicates that survive the validation pass.

Prefix bindings registered via Aristotle's `arachne.aristotle.registry`:
```
abc:     https://w3id.org/abc/
bibo:    http://purl.org/ontology/bibo/
dc:      http://purl.org/dc/elements/1.1/
dcterms: http://purl.org/dc/terms/
dcndl:   http://ndl.go.jp/dcndl/terms/
foaf:    http://xmlns.com/foaf/0.1/
rdag2:   http://RDVocab.info/ElementsGr2/
rdfs:    http://www.w3.org/2000/01/rdf-schema#
schema:  https://schema.org/
xsd:     http://www.w3.org/2001/XMLSchema#
```

The legacy `aozora:` namespace from `references/archive/aozora_lod_data/` is **not** registered here. If a future vocabulary-review milestone establishes that `purl.org/net/aozora/` predicates are well-defined and worth adopting, they can be added then.

### `src/abc/tools/tei_header.clj` (new)

Pure metadata-record → TEI `<teiHeader>` element generator returning a `clojure.data.xml`-compatible element tree. Caller serializes via the existing `abc.tools` writer or `clojure.data.xml/emit-str`.

TEI-EAJ aligned (per Q4 (ii)):

- `<titleStmt>`:
  - `<title type="main" xml:lang="ja">` for the kanji title
  - `<title type="reading" xml:lang="ja-Hira">` for the kana reading
  - `<author>` containing `<persName xml:lang="ja">` (kanji) with `<surname>` + `<forename>`, plus a sibling `<persName xml:lang="ja-Hira">` (kana reading) and `<persName xml:lang="ja-Latn">` (romaji), and an `<idno type="aozora-person-id">` carrying the Aozora person ID
- `<publicationStmt>`:
  - `<idno type="aozora-work-id">`
  - `<publisher>` (TEI publisher of this digital edition; constant string for ABC, taken from a project config or hard-coded "ABC")
  - `<date when="<modified>">` from `dcterms:modified`
- `<sourceDesc><bibl>`:
  - Title and reading of the base-text edition
  - Publisher and year of the base-text edition
  - `<ref target="<source-file-url>">` to Aozora's source-file page
- `<encodingDesc>`:
  - `<charDecl>` carrying any project-specific gaiji declarations carried over from the existing fixture
- `<profileDesc>`:
  - `<langUsage><language ident="ja">日本語</language></langUsage>`
  - `<textClass><classCode scheme="NDC"><x:value of NDC code></classCode></textClass>`

Non-author roles emit additional `<respStmt>` blocks inside `<titleStmt>` with `<resp>` carrying the Japanese role string and `<persName>` blocks for the contributor. This keeps the TEI header faithful to Aozora's role taxonomy.

### `abc.tools.validate-design-bundle` (extension)

New step `==> Validating metadata record` between `==> Validating SHACL shapes` and `==> Validating TEI against P5 RelaxNG`:

1. Load `examples/v0/example-work/metadata-record.json` and validate it against `metadata-record.schema.json`.
2. Compute `record-hash`. Assert it matches `manifest_identity_object.metadata_record_hash` in `examples/v0/example-work/manifest.json`.
3. Generate the RDF graph and validate via SHACL (new `MetadataRecordShape` in `manifest.shacl.ttl` targeting `aozora:BibResource`).
4. Assert the generated Turtle matches `examples/v0/example-work/metadata-record.ttl` byte-for-byte (parity test).

The TEI step downstream picks up the existing `examples/v0/example-work/tei.xml`. As part of this milestone, the placeholder `<teiHeader>` in that file is **replaced** by the generated TEI-EAJ-aligned header. The TEI step continues to validate it against `tei_all.rng` unchanged.

## Data Flow

```
Real Aozora CSV slice (committed: aozora-csv/list_inp_127.csv, list_person_879.csv)
  → abc.tools.aozora-csv/read-rows + parse-work-row + parse-person-row
  → merge-person-into-work
  → metadata-record map (string-keyed JSON shape)
  → abc.tools.metadata-record/validate!         (against metadata-record.schema.json)
  → abc.tools.metadata-record/record-hash       → sha256:<hex>
  → abc.tools.metadata-record/record->graph     → SHACL validate (MetadataRecordShape)
                                                → record->ttl (deterministic, parity-tested)
  → abc.tools.tei-header/build                  → <teiHeader> element tree
  → injected into examples/v0/example-work/tei.xml at fixture-build time
                                                → harness's TEI step validates via Jing
```

The metadata-record's hash is the value plugged into other artifact manifests' `manifest_identity_object.metadata_record_hash`. For the v0 example-work, this means `manifest.json` is regenerated once with the real hash; subsequent runs assert byte-equality.

## Fixture Changes

| File | Status | Notes |
|---|---|---|
| `examples/v0/example-work/aozora-csv/list_inp_127.csv` | NEW | One-row slice from Aozora's `list_inp_xxxx.csv` for work 000127 (羅生門). UTF-8. CC0. |
| `examples/v0/example-work/aozora-csv/list_person_879.csv` | NEW | One-row slice from `list_person_all_extended_utf8.csv` for person 000879 (Akutagawa). UTF-8. CC0. |
| `examples/v0/example-work/aozora-csv/PROVENANCE.md` | NEW | Source URLs, snapshot date, transcoding notes if any, original-file SHA-256s for the upstream CSV files. |
| `examples/v0/example-work/metadata-record.json` | NEW | Generated by the ingester from the two CSVs. Committed; deterministic. |
| `examples/v0/example-work/metadata-record.ttl` | NEW | Generated RDF view. Byte-for-byte parity test. |
| `schemas/metadata-record.schema.json` | NEW | The contract. |
| `schemas/manifest.shacl.ttl` | MODIFY | Add `MetadataRecordShape`. |
| `examples/v0/example-work/manifest.json` | MODIFY | `manifest_identity_object.metadata_record_hash` becomes the real hash; `artifact_id` recomputes. |
| `examples/v0/example-work/manifest.ttl` | REGENERATE | Re-derived from the modified `manifest.json`. |
| `examples/v0/example-work/tei.xml` | MODIFY | Placeholder `<teiHeader>` replaced by the generated TEI-EAJ-aligned header. Body unchanged. |
| `flake.nix` | MODIFY | Add new src/test files to `contract-surface`; add `'abc.tools.aozora-csv-test`, `'abc.tools.metadata-record-test`, `'abc.tools.tei-header-test` to the focused-test alias. |
| `nix/clj-nix-deps.edn` | MODIFY | Same alias addition. |
| `src/abc/tools/validate_design_bundle.clj` | MODIFY | New `validate-metadata-record!` helper + harness step. |

No new Maven deps. Aristotle, Jena, charred, and the Malli/Jing stack already in tree carry the load.

## Test Plan

**`test/abc/tools/aozora_csv_test.clj`** (new):
- `parse-work-row` with full column set produces the expected map; field-by-field assertions.
- `parse-person-row` with kanji + kana + romaji triplet.
- Optional-column handling: missing fields produce `nil` or are omitted.
- NFC normalization applied to title, family/given names, and readings.
- Round-trip: parsing the committed `examples/v0/example-work/aozora-csv/list_inp_127.csv` produces the same `work` sub-map that's in the committed `metadata-record.json`.

**`test/abc/tools/metadata_record_test.clj`** (new):
- `validate!` accepts the example fixture.
- `validate!` rejects synthetic invalid records (missing `work_id`, bad orthographic-style enum, malformed NDC).
- `record-hash` is deterministic across two calls.
- `record-hash` is independent of map insertion order (JCS test, mirrors `manifest-to-rdf-is-deterministic-test`).
- `record-hash` excludes `source_csv_provenance` from identity.
- `record->graph` produces expected triples — assertions on `aozora:titleID`, `dc:subject`, `dcterms:creator`, `foaf:familyName`, `aozora:authorID`, `rdag2:dateOfBirth`.
- `record->ttl` byte-for-byte matches `examples/v0/example-work/metadata-record.ttl`.

**`test/abc/tools/tei_header_test.clj`** (new):
- `build` returns an XML element tree that serializes to a valid string.
- The serialized header validates against `tei_all.rng` via `abc.tools.tei/validate!` (honors `TEI_SCHEMA_PATH` / `ABC_TEI_SCHEMA_SKIP=1`).
- TEI-EAJ alignment assertions: `<persName>` triplet (kanji + kana + romaji); `<idno type="aozora-person-id">` present; `<classCode scheme="NDC">` present.
- Negative: a metadata-record missing `title` produces a header that fails `abc.tools.tei/validate!`.

**`test/abc/tools/validate_design_bundle_test.clj`** (extension):
- Smoke positive: `validate-metadata-record!` returns nil for the example fixture.
- Loud-fail when `metadata-record.json` content doesn't validate against the schema (in-test corruption + restore).
- Hash-mismatch detection: temporarily mutate `metadata_record_hash` in `manifest.json`, assert the harness throws with `metadata` in the message, restore.

**Existing tests preserved:** the SHACL parity tests, the failure-fixture parity test, the success-fixture parity test (`manifest.ttl`) all need regeneration after the `metadata_record_hash` change. This is a one-time re-derivation, not a test-design change.

## Acceptance Criteria

- `clojure -M:test` passes including the four new test namespaces.
- `nix run .#validate-design-bundle` prints a new step `==> Validating metadata record` between SHACL and TEI; final line `design bundle validation ok`; exit 0.
- `nix flake check` passes; the focused-test sandbox runs `aozora-csv-test`, `metadata-record-test`, and `tei-header-test` (the last skips when `ABC_TEI_SCHEMA_SKIP=1`).
- Re-running the ingester on the committed CSV slice produces byte-identical `metadata-record.json` (determinism guard).
- Restoring the previous (placeholder/null) `metadata_record_hash` in `manifest.json` causes the harness to fail with a hash-mismatch message naming the file.
- Restoring the original placeholder `<teiHeader>` in `tei.xml` causes the TEI step to fail (the generated TEI-EAJ header is the new authoritative one).
- The new RDF parity test for `metadata-record.ttl` fails if `record->ttl` output diverges.
- Existing parity tests (`manifest.ttl`, `failure-manifest.example.ttl`) continue to pass after their one-time regeneration.

## Sequencing

1. Write `schemas/metadata-record.schema.json`. Validate it loads via `abc.tools.schema/schema-valid!`.
2. TDD `abc.tools.aozora-csv` against synthetic CSV strings (column-mapping correctness, NFC, optional-field handling).
3. Commit the real CSV slice under `examples/v0/example-work/aozora-csv/` plus `PROVENANCE.md`. Run the ingester to produce `metadata-record.json`. Commit it.
4. TDD `abc.tools.metadata-record` (validate!, record-hash, record->graph, record->ttl). Generate and commit `metadata-record.ttl`.
5. Add `MetadataRecordShape` to `schemas/manifest.shacl.ttl`, only constraining predicates that survived the vocabulary-validation step inside `metadata-record.clj` (step 4). Confirm SHACL passes against the generated graph.
6. TDD `abc.tools.tei-header`. Verify generated header validates via Jing.
7. Replace the placeholder `<teiHeader>` in `examples/v0/example-work/tei.xml` with the generated bytes. Re-run the harness's TEI step.
8. Update `examples/v0/example-work/manifest.json`'s `metadata_record_hash` to the real value. Regenerate `manifest.ttl`. Update parity tests if they're hash-sensitive.
9. Wire `validate-metadata-record!` into the harness; add the smoke + loud-fail + hash-mismatch tests.
10. Update `flake.nix` `contract-surface` and the focused-test alias. Run `nix flake check`.
11. End-to-end verification: `nix run .#validate-design-bundle`, fixture regression checks (revert each modified fixture file in turn, confirm the harness fails appropriately).
12. Refresh `docs/next-steps.md`: drop "Metadata data model mapping", add follow-on candidates (separated person records / referenced normalization; corpus-scale CSV ingestion; TEI ODD promotion now informed by real header content).

## References

- `src/abc/aozora.clj` — existing Malli registry. Source of truth for field names and enums.
- `references/archive/aozora_lod_data/list_person_all_extended.ttl` and `author_selected_list.ttl` — earlier project work; reference shape only, not authoritative. This milestone improves on it via the vocabulary plan above.
- `https://www.aozora.gr.jp/index_pages/list_inp.html` — Aozora work index CSVs.
- `docs/high-level-architecture-note.md` §"Metadata join" and §"Aozora-Specific Design Concerns" — driving constraints (NFC policy, person-record drift, hashed metadata identity).
- `docs/superpowers/specs/2026-04-27-tei-relaxng-validation-design.md` — `abc.tools.tei` is reused for TEI-header validation in this milestone.
- TEI-EAJ jp_guidelines — `https://github.com/TEI-EAJ/jp_guidelines` (draft community guidance; the conventions adopted here are documented in the generated header itself).
