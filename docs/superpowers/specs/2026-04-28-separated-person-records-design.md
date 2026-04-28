# Separated Person Records Design

Date: 2026-04-28
Status: Draft
Depends on: ADR 0001 (Manifest Identity), archived plan
`docs/superpowers/plans/archive/2026-04-27-metadata-data-model.md`

## Goal

Split Person records out of the work-level `metadata-record.json` into their
own content-addressed artifact so that a bibliographic edit to a person (e.g.
a romaji correction on 芥川) only invalidates the works that reference that
person, instead of cascading through every work in the corpus.

This is "Flavor 1" of the cascading-invalidation problem: bibliographic
correction. Person identity drift — splits, merges, renames — is "Flavor 2"
and is out of scope. Flavor 2 needs a PROV-style event log that this
milestone does not introduce.

## Non-Goals

- No person split/merge/rename event log.
- No two-stage corpus-scale ingester. The new shape pre-stages it; the
  two-command CLI lands in the corpus-scale ingestion milestone.
- No cross-work "which works reference this person" index. That belongs to
  the query/index layer.
- No `aozora:` legacy-namespace vocabulary audit.
- No change to `manifest_identity_object`. The 12-field shape from ADR 0001
  is preserved; `metadata_record_hash` remains the only metadata-related
  identity dimension.

## File Layout

```
schemas/
  metadata-record.schema.json       # revised: work-only, contributors reference persons
  person-record.schema.json         # NEW
  manifest.shacl.ttl                # adds PersonRecordShape; existing
                                    # MetadataRecordPersonShape retired
examples/v0/
  example-persons/
    000879.json                     # NEW: 芥川竜之介, content-addressed
  example-work/
    metadata-record.json            # revised: contributors[] references 000879
```

`examples/v0/example-persons/` is a sibling of `example-work/`. At corpus
scale, multiple works reference the same person file there; for v0 the
directory holds one entry. The layout makes "edit person, only the
referencing works rebuild" visible on disk.

## Person Record Shape

`schemas/person-record.schema.json` (JSON Schema Draft 2020-12) covers a
single Person. Fields are everything currently embedded under `persons[i]`
in the metadata-record *minus* `relation_to_work`, plus self-identifying
schema fields.

```json
{
  "person_record_schema_id":   "https://w3id.org/abc/schemas/person-record.schema.json",  // const
  "person_record_schema_hash": "sha256:...",
  "person_id":                 "000879",
  "family_name":               "芥川",
  "given_name":                "竜之介",
  "family_name_reading":       "あくたがわ",
  "given_name_reading":        "りゅうのすけ",
  "family_name_sort":          "あくたかわ",
  "given_name_sort":           "りゆうのすけ",
  "family_name_romaji":        "Akutagawa",
  "given_name_romaji":         "Ryunosuke",
  "date_of_birth":             "1892-03-01",
  "date_of_death":             "1927-07-24",
  "person_copyright_expired":  true,
  "external_links":            [],
  "source_csv_provenance":     { "source_url": "...", "retrieved_at": "...", "original_file_hash": "..." }
}
```

The file does not carry its own `person_record_hash`. This matches the
current metadata-record pattern (the metadata-record file does not carry
`metadata_record_hash` either — it is computed and referenced externally).
Each Person's identity is materialized at every reference site, i.e. in
`contributors[i].person_record_hash` for every work that uses them, and the
harness recomputes it from the file at validation time.

Required fields: `person_record_schema_id`, `person_record_schema_hash`,
`person_id`, `family_name`, `given_name`, `person_copyright_expired`. All
currently-nullable fields stay nullable
under the same `nullableString` / `nullableDate` patterns. `external_links`
remains an array with `default: []`.

`relation_to_work` does not appear on the Person record. It is a property of
the Work–Person edge, not of the Person, and it moves to the work side
(see "Revised Metadata-Record Shape" below).

## Person Record Identity

`person_record_hash` is computed as:

```
sha256:<hex of SHA-256(JCS(record minus source_csv_provenance))>
```

Explicitly:

- **Included** in the identity hash: `person_record_schema_id`,
  `person_record_schema_hash`, `person_id`, all bibliographic fields,
  `external_links`. The schema fields are part of the identity so that a
  schema migration changes the hash even when bibliographic content is
  unchanged.
- **Excluded** from the identity hash: `source_csv_provenance`
  (documentation, not identity).
- **Canonicalization**: RFC 8785 JCS bytes. All nullable fields present as
  JSON `null`, not key-omission.
- **Array ordering**: there is no array to sort at the top level of the
  Person record. `external_links` is treated as carrying semantic order
  (rare in practice, but the rule mirrors the rest of the schema's
  arrays-are-ordered-unless-otherwise-stated convention).

The hash is not stored in the Person file. It exists at reference sites
(`contributors[i].person_record_hash`) and is recomputed from file contents
during validation.

`person_record_schema_hash` is the SHA-256/JCS hash of the bundled
`schemas/person-record.schema.json` JSON value, computed the same way as
`metadata_record_schema_hash` per ADR 0001.

## Revised Metadata-Record Shape

`persons[]` is removed. `contributors[]` takes its place, holding references
not bodies:

```json
{
  "metadata_record_schema_id":   "https://w3id.org/abc/schemas/metadata-record.schema.json",
  "metadata_record_schema_hash": "sha256:...",
  "work":                        { ... unchanged from current shape ... },
  "contributors": [
    {
      "person_id":           "000879",
      "person_record_hash":  "sha256:...",
      "relation_to_work":    "著者"
    }
  ],
  "source_csv_provenance":       { ... }
}
```

The metadata-record file does not carry its own `metadata_record_hash`
(this is unchanged from today's pattern). The hash is computed externally
and lives at reference sites — primarily
`manifest_identity_object.metadata_record_hash` in the work's
`manifest.json`, where it remains the only metadata-related identity
dimension.

`contributors[]` is sorted by `person_id` (lexicographic ASCII) before JCS
hashing. This is the same array-ordering rule the current `persons[]` uses.
Why: RFC 8785 JCS preserves array insertion order; it does not sort. Since
listing 芥川 before or after a hypothetical co-translator should not change
the work's identity, the canonical-identity form sorts before JCS to make
the hash order-independent.

`relation_to_work` keeps the existing enum: `"著者"`, `"翻訳者"`, `"編者"`,
`"校訂者"`, `"その他"`.

## Metadata-Record Identity

`metadata_record_hash` is computed as:

```
sha256:<hex of SHA-256(JCS(record minus source_csv_provenance, contributors[] sorted by person_id)))>
```

Editing person 000879's romaji:

1. The person file's bytes change; the JCS-recomputed `person_record_hash`
   for the file changes.
2. Every metadata-record whose `contributors[]` references 000879 now
   carries a stale `person_record_hash` value in that contributor entry.
3. Re-running the ingester rewrites those contributor references and the
   recomputed `metadata_record_hash` values change.
4. Manifests for those works rebuild via
   `manifest_identity_object.metadata_record_hash`.
5. Works that do not reference 000879 are byte-identical to before.

The cascading is bounded to the actual reference set, which is the goal.

## Identity-Field Summary

| Field | Identity? | Notes |
|---|---|---|
| `person_record_schema_hash` | input | bundled JSON Schema JCS hash, in file |
| `person_record_hash` | output | not in file; recomputed at reference sites |
| `metadata_record_schema_hash` | input | bundled JSON Schema JCS hash, in file |
| `metadata_record_hash` | output | not in file; lives in `manifest_identity_object.metadata_record_hash` |
| `contributors[].person_record_hash` | input to `metadata_record_hash` | flows person identity into work identity |
| `source_csv_provenance` (both files) | excluded | documentation, in file |
| `manifest_identity_object` | unchanged | `metadata_record_hash` stays the only metadata dimension |

## Ingester CLI

`abc.tools.aozora-ingest` keeps its single-command shape and gains two
flags:

```bash
nix run .#aozora-ingest -- \
  --zip references/aozorabunko/index_pages/list_person_all_extended_utf8.zip \
  --work-id 000127 \
  --output examples/v0/example-work/metadata-record.json \
  --persons-output-dir examples/v0/example-persons \
  --refresh-manifest examples/v0/example-work/manifest.json
```

Per-invocation flow:

1. Read the CSV slice. Extract the work row plus the rows for every
   contributing `person_id` referenced by that work.
2. For each contributor person row: build a Person record and compute
   `person_record_hash`. Decide what to do with
   `<persons-output-dir>/<person_id>.json`:
   - File does not exist → write it.
   - File exists → parse it as JSON (fail loudly if unparseable);
     validate against `schemas/person-record.schema.json` (fail loudly if
     invalid); recompute its `person_record_hash`. If the recomputed hash
     equals the just-built record's hash, the existing file is byte-equal
     to the just-built bytes (modulo `source_csv_provenance` which is
     excluded from the hash) → no-op rewrite. If the recomputed hash
     differs and `--overwrite` is not set, fail with a message naming
     the `person_id` and both hashes. If `--overwrite` is set, replace
     the file.
3. Build the work record's `contributors[]` using the just-computed (or
   just-verified) `person_record_hash` for each contributor.
4. Write the metadata-record file. Compute `metadata_record_hash` from the
   in-memory record (the hash is not stored in the file).
5. If `--refresh-manifest <path>` was passed: read the existing manifest,
   replace `manifest_identity_object.metadata_record_hash` with the
   just-computed value, recompute `artifact_id` (per ADR 0001's
   JCS-of-identity-object rule), and write the manifest back. Without
   `--refresh-manifest`, print the new `metadata_record_hash` on stdout
   so an external tool or human can update the manifest, and exit. The
   plain-stdout path is the v0 fallback that matches current behavior;
   `--refresh-manifest` closes the loop for the example bundle.

The corruption-safe idempotent check (parse + schema-validate before hash
compare) prevents silent propagation if a person file on disk is mangled.
Step 2's failure modes are explicit at every junction.

The refuse-to-overwrite default is the integrity safeguard: re-ingesting a
different work that references the same person should not silently rewrite
the person record. A real correction is an explicit `--overwrite` operation.

Two ingest runs of the same `--work-id` produce byte-identical work output
*and* byte-identical person files. Existing two-run determinism tests extend
to cover both.

## SHACL

`schemas/manifest.shacl.ttl` adds `PersonRecordShape` and reshapes the work
side:

- `PersonRecordShape` covers the person-as-entity fields. It mirrors the
  current `MetadataRecordPersonShape`'s constraints for ID format, name
  presence, copyright flag, link format, but drops `relation_to_work`.
- `MetadataRecordWorkShape` is updated: instead of validating embedded
  `persons[]`, it validates `contributors[]` shape — `person_id` format,
  `person_record_hash` format (`sha256:` + 64 hex), `relation_to_work` enum
  membership, and `minCount 1`.
- `MetadataRecordPersonShape` is retired.

Reference integrity (the `person_record_hash` actually matches a present
file) lives in the harness, not SHACL. SHACL operates on a single graph;
it does not load and rehash sibling files.

## TEI-Header Builder

`abc.tools.tei-header` currently takes the metadata-record map and reaches
into its `persons[]`. After separation it takes a single map with already-
resolved person bodies:

```clojure
{:work         {...}
 :contributors [{:relation-to-work "著者"
                 :person {... full person record ...}}]}
```

The caller resolves `person_id` → person body before invoking the builder.
The builder knows nothing about person_id lookups, hashes, or filesystem
layout — it receives a fully resolved data structure and renders TEI. This
keeps the rendering pure and composable: any caller that can produce a
work + resolved contributors can drive the TEI builder, not just
Aozora-derived flows.

The rendered TEI header bytes for the 羅生門 fixture are unchanged — same
data, reached differently.

Resolution itself lives in the harness path or a thin wrapper alongside
the ingester: load `examples/v0/example-persons/<person_id>.json` for each
contributor, hand the resolved set to `tei-header/build`. The current
top-level `(:strs [work persons])` destructuring in callers is replaced by
the new shape.

## Harness Step

The existing `==> Validating metadata record` step in
`abc.tools.validate-design-bundle` becomes:

1. For every file under `examples/v0/example-persons/`: validate against
   `schemas/person-record.schema.json`, then verify the embedded
   `person_record_schema_hash` equals the live JCS hash of
   `schemas/person-record.schema.json`. Mismatch means the record was
   generated against a different schema; fail loudly. (Mirrors the existing
   metadata-record schema-hash precondition.)
2. Validate the work's `metadata-record.json` against
   `schemas/metadata-record.schema.json` (with the same schema-hash
   precondition). Recompute `metadata_record_hash` from file contents and
   compare against the value in `examples/v0/example-work/manifest.json`'s
   `manifest_identity_object.metadata_record_hash`. Fail on mismatch.
3. **Reference integrity.** For each `contributors[i]`, load
   `examples/v0/example-persons/<person_id>.json`, recompute its
   `person_record_hash`, and fail if it disagrees with the contributor
   reference. The error message names the `person_id`, the referenced hash,
   and the recomputed hash.
4. Run SHACL with `PersonRecordShape` and the revised
   `MetadataRecordWorkShape` over the manifest's RDF view.

Step 2 catches "the manifest's metadata_record_hash dimension is stale
relative to the metadata-record file." Step 3 catches "the work's
contributor reference is stale relative to the person file." Both can fail
independently.

## Tests

- `abc.tools.metadata-record-test`: `contributors[]` sorted by `person_id`
  before hashing; identity hash recomputation; the new `contributors[]` JCS
  fixture; ingester two-run determinism for work output.
- `abc.tools.person-record-test` (new): schema round-trip;
  `person_record_hash` recomputation against a JCS canonicalization fixture
  for one person; nullable-vs-omitted enforcement; embedded
  `person_record_schema_hash` matches the live schema's JCS hash (drift
  detection).
- `abc.tools.aozora-ingest-test`: ingester emits N+1 deterministic outputs;
  refuse-to-overwrite default fires when a person file with a different
  hash is on disk; `--overwrite` allows replacement; corruption-safe
  idempotent check fails loudly when the on-disk person file is
  unparseable JSON or schema-invalid (separate cases);
  `--refresh-manifest` rewrites `manifest_identity_object.metadata_record_hash`
  and recomputes `artifact_id`, with a re-run being a byte-identical no-op.
- `abc.tools.validate-design-bundle-test`: reference-integrity loud-fail —
  mutate `examples/v0/example-persons/000879.json` to flip `family_name_romaji`,
  the harness fails with a message naming `000879` and both hashes.
- `abc.tools.tei-header-test`: same fixture-in / fixture-out with the new
  call shape; assert the rendered header bytes for the 羅生門 fixture are
  unchanged.

## Acceptance Criteria

1. `examples/v0/example-persons/000879.json` exists and validates against
   `schemas/person-record.schema.json`. Its JCS-recomputed
   `person_record_hash` matches the value referenced in
   `examples/v0/example-work/metadata-record.json` under
   `contributors[0].person_record_hash`.
2. `examples/v0/example-work/metadata-record.json` has
   `contributors: [{person_id: "000879", person_record_hash: "sha256:...", relation_to_work: "著者"}]`
   and a recomputed `metadata_record_hash`.
3. `examples/v0/example-work/manifest.json`'s
   `manifest_identity_object.metadata_record_hash` matches the new value
   (one fixture refresh, identical operation to the metadata milestone's
   final regen).
4. `examples/v0/example-work/tei.xml` regenerates byte-identically from
   the new TEI-header builder shape.
5. `nix run .#validate-design-bundle` exits 0; `nix flake check` passes.
6. `nix run .#aozora-ingest -- ... --persons-output-dir examples/v0/example-persons`
   regenerates the work record and the 000879 person file byte-identically
   from the committed CSV slice.
7. Mutating `examples/v0/example-persons/000879.json` (e.g., flipping
   `family_name_romaji`) without re-running ingest causes
   `validate-design-bundle` to fail with a message naming `000879` and the
   recomputed-vs-referenced hashes.
8. `MetadataRecordPersonShape` is removed from `manifest.shacl.ttl`;
   `PersonRecordShape` is present; `MetadataRecordWorkShape` validates
   `contributors[]`.
9. The harness fails loudly if a person file's embedded
   `person_record_schema_hash` disagrees with the live JCS hash of
   `schemas/person-record.schema.json` (mirrors the metadata-record
   schema-hash precondition).
10. `nix run .#aozora-ingest -- ... --refresh-manifest examples/v0/example-work/manifest.json`
    rewrites `examples/v0/example-work/manifest.json`'s
    `manifest_identity_object.metadata_record_hash` and recomputes
    `artifact_id`. Re-running with no upstream changes is a byte-identical
    no-op.
11. Mutating `examples/v0/example-persons/000879.json` to malformed JSON
    or schema-invalid contents and re-running ingest without
    `--overwrite` fails loudly (corruption-safe idempotent check).

## Risks and Open Questions

- **Person record naming on disk: place, not value.** Files are named
  `<person_id>.json`, which makes the path mutable across content changes
  — the *file at a fixed path* is rewritten when the person is corrected,
  and works that haven't been re-ingested still point at the old hash via
  their `contributors[i].person_record_hash`. A pure value-oriented
  alternative is to name files by hash (`<sha256>.json`); a correction
  produces a new file alongside the old one, and the work's contributor
  reference is the only place that needs to update.

  v0 keeps `<person_id>.json` for human browsability and direct lookup
  during validation, accepting that this is the "easy" (familiar) choice
  rather than the "simple" (unentangled) one. Concrete consequences:

  - The harness recomputes the hash from file *contents*, not the
    filename, so stale-hash references fail loudly rather than silently
    resolving to wrong data. This is what makes the place-oriented layout
    safe in practice.
  - Concurrent ingester runs touching the same person file are a real
    failure mode; the refuse-to-overwrite default is the v0 mitigation
    (no concurrent writers in the example flow).
  - Person renaming or split/merge (Flavor 2) would compound a path
    change with a content change, which a content-addressed layout would
    handle naturally. The Flavor 2 milestone may revisit naming.

  Future direction: if corpus-scale operation surfaces concurrent-writer
  pain or makes person renames common, a follow-on milestone can move
  Person files to content-addressed names and add an explicit
  `person_id` → current-hash index in the query/index layer. The
  current schema and harness make that a localized refactor rather than
  a re-design.
- **What if two works disagree about a person's body?** The default
  refuse-to-overwrite ingester behavior surfaces this as an explicit
  failure rather than a silent overwrite. Acceptable for v0; a
  reconciliation tool is a future need.
- **`source_csv_provenance` on Person.** The work's provenance asserts
  "this work row came from this CSV"; the Person record's provenance
  asserts "these person fields came from this CSV." They are independent
  provenance statements that happen to coincide for the v0 fixture. A
  future corpus snapshot that merges person data from a different CSV
  would surface the difference. Both blocks remain excluded from their
  respective identity hashes.
