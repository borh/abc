# ADR 0026: Analysis Artifact Identity

Status: Proposed
Date: 2026-07-07
Supersedes: none
Depends on: ADR 0001, ADR 0003, ADR 0010, ADR 0023, ADR 0025
Source: `docs/superpowers/specs/2026-07-07-analysis-artifact-identity-design.md`

## Implementation Status

Proposed. The first analysis-artifact identity slice is partially implemented:
`analysis-recipe.schema.json`, `analysis-result.schema.json`, the
`literary-basic-ja-v1` recipe fixture, analysis-result materialization,
manifest-index copied-field validation, request-set definitions, a
content-hash-pinning request-set resolver, resolved request-set golden files,
the snapshot-index schema, and the Soranoha request-set inspection command
exist. The request-set materialization planner and Nix materialization strategy
are still pending.

This ADR proposes only the first token-independent per-work analysis slice:
analysis over `parser-ir-plaintext-body-v1`. It does not propose standalone
tokenized artifacts, collection analysis packs, Parquet pack layouts, corpus
statistics inputs, run-provenance bridges, or PROV-O exports.

2026-07-09 dependency note: this ADR `Depends on` ADR 0003 and ADR 0025.
ADR 0025 is `Accepted`, but ADR 0003 remains `Draft` with its cost-envelope
acceptance criteria (real manifests, cold builds, the recorded CI runner,
<30s / <2GB) unmet — only synthetic evaluator-only probes exist (see ADR 0003
Implementation Status). The Nix Materialization section below already states
"ADR 0003 remains Draft, so release-scale use must respect its measured
evaluation envelope once that envelope is accepted"; that phrase is the
governing constraint, and any promotion of this ADR from `Proposed` to
`Accepted` for release-scale use must reuse an accepted ADR 0003 envelope rather
than the synthetic probe.

## Context

ABC already reserves tokenizer and analysis coordinates in manifest identity,
but current publication manifests leave those fields null. Legacy `abc.stats`
computed stylometric measures over pre-v0 annotated documents, while the Rust
morphology warehouse computes analyzer-comparison facts. Neither is an ABC
manifest contract for publishing literary analysis results.

Analysis identity has the same core requirement as publication identity:
consumers must know which source snapshot, parser output, text projection,
recipe, and output contract produced a value. The design must also avoid a
Cartesian materialization matrix over works, source versions, parsers,
tokenizers, dictionaries, profiles, recipes, and output formats.

## Decision

ABC will model literary and stylometric results as canonical per-work
`analysis` artifacts called analysis slices.

The first target slice coordinate is:

```text
subject coordinate
  + parser-ir-plaintext-body-v1 input view
  + analysis recipe hash
  + output format spec hash
  -> per-work analysis manifest and analysis-result sidecar
```

For this first slice, tokenizer fields are null because the input view is
text, not tokens.

### Manifest Identity

Per-work analysis slices use the existing ADR 0001 manifest identity rule:

```text
artifact_id = sha256(RFC8785-JCS(manifest_identity_object))
```

For `parser-ir-plaintext-body-v1` analysis slices:

- `corpus_snapshot_hash` and `work_content_hash` identify the subject text.
- `metadata_record_hash` is non-null only when the recipe makes metadata affect
  output bytes.
- `parser_build_hash`, `parser_config_hash`,
  `aat_parser_ir_mapping_hash`, and `parser_ir_schema_hash` are copied exactly
  from the producer parser-IR manifest, including nulls.
- `tei_profile_hash` is null.
- `tokenizer_build_hash` and `tokenizer_dictionary_hash` are null.
- `analysis_recipe_hash` is the JCS SHA-256 hash of the canonical analysis
  recipe JSON value.
- `output_format_spec_hash` identifies the analysis-result schema.

Null means "not applicable", not "unknown". If a builder cannot determine a
field required by the input-view identity map, it must emit a failed manifest
or no manifest. It must not emit a successful analysis manifest with null as an
unknown placeholder.

### Input View Contract

The accepted input view kind is `parser-ir-plaintext-body-v1`.

The analysis result content must record:

- producer parser-IR artifact id,
- producer content hash,
- plaintext policy hash,
- coordinate system for spans,
- subject coordinates needed for query use,
- analysis recipe hash,
- output schema hash,
- metric records controlled by the recipe.

The analysis manifest provenance must record the producer parser-IR artifact id
in both `provenance.used` and `provenance.was_derived_from`, and must record
the plaintext policy hash in `provenance.used`.

Release validation must re-check copied identity fields. For every analysis
manifest whose input-view map copies fields from a producer manifest, the
manifest-index validator must load the producer manifest named in provenance
and compare the copied fields byte-for-byte, including nulls. Schema validation
alone is insufficient.

### Recipe Identity

Analysis recipes are canonical JSON values hashed with the same RFC 8785 JCS
discipline as manifest identity. Human recipe ids are lookup labels, not
identity.

The recipe registry stores recipe/profile objects by content hash and stores
semantic id bindings in an append-only log. A binding log entry records at
least:

- `semantic_id`
- `content_hash`
- `valid_from`
- optional `supersedes_entry_hash`

The JCS hash of that binding entry is the `registry_entry_hash`. A current
semantic-id map is a generated view over the binding log, not mutable identity.

Resolvers may accept labels such as `literary-basic-ja-v1`, but resolved
request sets must record the selected content hash, `registry_entry_hash`, and
resolution time for audit. Only the content hash participates in artifact or
request-set identity.

### Request Sets

A request set turns possible coordinates into a bounded realized work set. It
is a canonical JSON value with:

- resolved subjects,
- input views and policy hashes,
- tokenizer profile hashes, empty for the first slice,
- analysis recipe hashes,
- missing policy,
- pack policy hash.

For the first slice, `pack_policy_hash` may identify a canonical no-pack or
prototype-output policy. It does not imply that collection analysis-pack
manifests are accepted by this ADR.

`request_set_id` is:

```text
sha256(RFC8785-JCS(request_set_identity_object))
```

`request_set_id`, generated time, operator, local paths, run id,
`resolved_labels`, and `batch_policy` are excluded from
`request_set_identity_object`.

Before hashing:

- subjects sort by `source_id`, then `work_content_hash`, then
  `metadata_record_hash`, with null treated as the empty string for comparison
  only,
- input views sort by `input_view_kind`, then `policy_hash`,
- tokenizer profile hashes sort lexicographically by full hash string,
- analysis recipe hashes sort lexicographically by full hash string.

JCS serialization retains JSON null for `metadata_record_hash`.

Multiple subject entries may share `source_id` when later sort keys differ.
Exact duplicate subject entries after normalization must be coalesced before
hashing; a resolver may record a non-identity warning about the duplicate.

Changing a request set can change pack identity later. It must not change the
identity of already-realized per-work slices.

### Manifest Index

The manifest index remains a generated view. It is not identity-bearing.

For this slice, the index must support:

- lookup of producer `parser-ir` artifacts by `work_content_hash`,
  `corpus_snapshot_hash`, and relevant parser/mapping/schema coordinates,
- lookup of existing `analysis` artifacts by the same identity coordinates used
  to compute `artifact_id`,
- reproducibility conflict detection: same successful `artifact_id` with
  different `content.content_hash` fails release validation,
- copied producer-field validation for `parser-ir-plaintext-body-v1`.

The inverse is not a conflict: different `artifact_id` values may cite the same
`content.content_hash` when different coordinates legitimately produce
identical bytes.

### Sidecar Roles

The first slice adds only the `analysis-result` sidecar role.

It does not add generic `recipe`, `token-table`, `analysis-table`,
`analysis-pack-index`, or `analysis-request-set` sidecar roles. Recipe JSON is
addressed through the recipe registry by content hash. Pack and token roles
belong to later ADRs.

### Nix Materialization

If this slice is exposed through Nix, the builder shape must preserve ADR 0003
constraints:

- Nix evaluation must not parse producer manifests to construct child
  identities. Producer identity fields are copied by the analysis tool at build
  time.
- Slice derivations take the producer artifact path, recipe object path,
  schemas, and policy files as named inputs.
- Recipe/profile registries are realized as per-hash store paths, not as a
  mutable registry directory read by the evaluator.
- Per-work analysis derivations do not take the full corpus snapshot store path
  as a direct input. They take the per-work producer artifact path.
- Effective determinism tier is the lowest tier across recipe, producer input,
  tokenizer profile if any, model weights if any, and auxiliary artifacts.
- Exact release replay must verify the declared `content.content_hash`.
  Stable or bounded outputs remain input-addressed and are substitutable only
  from trusted signed caches. Exploratory outputs are local-only.

The request set is identity-bearing; per-work, batch, or single-CAS realization
strategy is materialization metadata unless it changes output bytes. ADR 0003
remains Draft, so release-scale use must respect its measured evaluation
envelope once that envelope is accepted.

## Deferred Decisions

- Metric formulas beyond the minimal token-independent recipe fixture needed to
  exercise this identity contract.
- Standalone tokenized artifact identity.
- A dedicated tokenizer profile/config identity field.
- Collection analysis-pack manifest identity.
- Parquet analysis-pack table layout.
- Batch and single-CAS realization failure semantics.
- Corpus-statistics artifacts for corpus-normalized recipes.
- Mapping warehouse run ids to canonical request-set and artifact ids.
- Generated PROV-O JSON-LD provenance views.
- Public package formats such as RO-Crate.

## Consequences

ABC can publish token-independent analysis results without reviving legacy
`abc.stats` or making the morphology warehouse a canonical identity store.

The design keeps canonical identity in manifests, content hashes, recipes, and
request sets. SQLite, DuckDB, manifest indexes, and future analysis packs remain
generated places.

The first implementation must add schema, registry, manifest-index, and
validation surface before metric code is useful. This is intentional: the
identity contract is the durable boundary, while formulas can evolve as recipe
content hashes.

The manifest index becomes responsible for a new release validation: copied
identity fields in analysis manifests must match producer manifests. This adds
implementation work but prevents schema-valid identity drift.

## Acceptance Criteria

- `analysis-recipe.schema.json` and `analysis-result.schema.json` exist and are
  hashable with the ADR 0001 schema-hash discipline.
- A token-independent `literary-basic-ja-v1` recipe fixture exists as a
  canonical JSON value.
- `manifest.schema.json` accepts `artifact_kind = "analysis"` manifests with
  tokenizer fields null and an `analysis_recipe_hash` present.
- The manifest sidecar role enum accepts `analysis-result` and does not add
  pack-only roles in this slice.
- A fixture or prototype materializes one per-work `analysis` manifest from an
  existing parser-IR plaintext body input.
- The analysis manifest copies parser and mapping identity fields exactly from
  the producer parser-IR manifest.
- Release validation fails when a copied producer identity field differs from
  the producer manifest.
- Release validation fails when two successful analysis manifests share the
  same `artifact_id` and differ in `content.content_hash`.
- The first implementation is covered by `test/abc/tools/analysis_identity_test.clj`,
  `test/abc/tools/materialize_analysis_test.clj`, and
  `test/abc/tools/manifest_index_test.clj`, including the copied producer-field
  and duplicate-`artifact_id` failure cases above.
- A request-set fixture demonstrates non-circular hashing, canonical array
  sorting, JSON null preservation, semantic-id-to-hash resolution metadata, and
  duplicate subject coalescing before hashing.
- The manifest index can resolve the producer parser-IR artifact needed by the
  first analysis slice.
- If Nix is wired for the prototype, the derivation passes only per-hash recipe
  paths and producer artifact paths to the builder; it does not read a registry
  directory or pass the full corpus snapshot path as a direct analysis input.

## Rollback

If this identity rule is insufficient, supersede this ADR and introduce a new
manifest schema hash or recipe schema hash as needed. Do not reinterpret
analysis manifests emitted under this rule.
