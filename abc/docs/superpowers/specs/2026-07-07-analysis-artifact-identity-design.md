# Analysis Artifact Identity Design

Status: Proposed design
Date: 2026-07-07
Owner: ABC architecture track

This spec defines how ABC should model tokenization and literary or
stylometric analysis artifacts without materializing an impractical Cartesian
product of works, source snapshots, parsers, tokenizers, dictionaries, output
formats, and analysis recipes.

The design deliberately avoids implementing any metric code. It defines the
identity, provenance, request, and packaging rules needed before such code can
be safely introduced.

## Problem

ABC already reserves `tokenizer_build_hash`, `tokenizer_dictionary_hash`, and
`analysis_recipe_hash` in manifest identity, but current publication manifests
leave those fields null. Legacy `abc.stats` measured stylometric values over a
pre-v0 annotated document shape. The Rust morphology warehouse measures
analyzer behavior and stores morpheme facts, but it is not yet an ABC manifest
contract for literary analysis.

The system needs to answer questions such as:

- What token count, type count, STTR, Yule's K, or sentence-length statistic
  applies to this work?
- Which source snapshot, source edition, parser output, tokenizer build,
  dictionary, profile, and analysis formula produced that value?
- Can a release cite those results without rebuilding every combination of
  work, source git ref, parser, TEI profile, tokenizer, dictionary, and recipe?
- Can corpus-level tables be scanned efficiently without making a database row
  or run directory the canonical identity?

The hard part is not the metric formula. The hard part is preserving identity
and bounded materialization.

## Evidence

| Claim | Type | Source | Confidence | Impact if wrong |
|---|---|---|---|---|
| Manifest identity already names tokenizer and analysis coordinates. | Observation | `abc/docs/architecture.md`, `abc/docs/adr/0001-manifest-identity.md`, `abc/schemas/manifest.schema.json` | High | The design would need a larger manifest revision. |
| Current manifests can represent `artifact_kind = "tokenized"` and `"analysis"`, but sidecar roles do not yet include token or analysis tables. | Observation | `abc/schemas/manifest.schema.json` | High | Schema change scope would differ. |
| Current v0.4 identity requires one `work_content_hash`; it cannot cleanly identify a collection-wide analysis pack. | Observation | `abc/schemas/manifest.schema.json` | High | Pack design could be simpler if collection identities were already supported. |
| Nix materialization must not evaluate the full works x parsers x profiles x tokenizers x analyses matrix. | Decision | `abc/docs/adr/0003-nix-materialization.md` | High | Eager matrix designs would be acceptable. |
| ADR 0003's bounded-materialization cost envelope is not yet accepted. | Observation | `abc/docs/adr/0003-nix-materialization.md` | High | This spec's materialization claims would need stronger wording if the envelope were already proven. |
| Canonical identity should remain in files and hashes; query runtimes are generated views. | Proposed direction | `abc/docs/handoffs/query-runtime-and-history-index.md` | Medium-high | A database-first design would become viable. |
| The Rust morph warehouse is strong prior art for Parquet analytical facts and DuckDB scans. | Observation | `ab-validator/crates/ab-warehouse/src/schema.rs`, `ab-validator/docs/morph-corpus-workflow.md` | High | Pack format recommendation would change. |
| Tokenizer exactness requires pinned build, dictionary, locale/config, normalization, and fixtures. | Draft decision | `abc/docs/v0-design-bundle/tokenizer-determinism.md` | Medium-high | Exact-release policy would be too strict. |

Prior art used for shape, not as binding ABC contracts:

- W3C PROV-O for entity/activity/agent provenance:
  <https://www.w3.org/TR/prov-o/>
- RO-Crate for packaging research data with metadata:
  <https://www.researchobject.org/ro-crate/specification/1.2/introduction.html>
- Frictionless Data Package for a descriptor listing data resources:
  <https://specs.frictionlessdata.io/data-package/>
- Apache Parquet for columnar analytical storage:
  <https://parquet.apache.org/docs/file-format/>

## Non-Goals

- Do not define or implement stylometric formulas.
- Do not choose one authoritative tokenizer for Aozora Bunko.
- Do not make the morphology warehouse the canonical ABC identity store.
- Do not require an always-on database service.
- Do not make TEI/plaintext publication depend on analysis unless publication
  bytes actually embed analysis values.
- Do not solve long-term public package formats such as RO-Crate in this first
  slice, though the design should not block them.
- Do not claim that bounded Nix materialization is already proven. This design
  depends on ADR 0003's still-Draft cost envelope, and realization strategies
  must be measured before release-scale adoption.

## Glossary

| Term | Meaning |
|---|---|
| Subject coordinate | The text-bearing subject being analyzed: source component, source snapshot, source id/path, work id where available, work content hash, and optional metadata record hash. |
| Input view | The concrete text view consumed by tokenization or analysis, such as parser-IR plaintext body text, source visible text, TEI body text, or a token stream. |
| Tokenizer coordinate | Build, dictionary, profile/config, normalization, locale, granularity, determinism tier, and token-output schema coordinates. |
| Analysis recipe | A canonical JSON value defining input contract, metric ids, formulas, denominators, token filters, normalization assumptions, error behavior, and implementation/tool identity. |
| Analysis slice | A canonical per-work `analysis` manifest and result for one subject coordinate, one input view, and one analysis recipe. |
| Tokenized slice | A canonical per-work `tokenized` manifest and token stream for one subject coordinate, one input view, and one tokenizer coordinate. |
| Request set | A canonical resolved list of subjects, input views, tokenizer profiles, and analysis recipes requested for a run or release. |
| Analysis pack | A generated corpus-level query package over realized slices. It is rebuildable from manifests and slice outputs. |
| Run id | Operational locator for one execution. It is provenance/observability, not artifact identity. |

## Use Cases

| Actor | Objective | Current obstacle | Capability when solved |
|---|---|---|---|
| Release builder | Publish a bounded set of analysis results for a source snapshot | Manifest identity reserves fields but no analysis layer exists | Build only requested slices and cite stable artifact ids. |
| Literary analyst | Compare token-sensitive metrics across tokenizers | Tokenizer, dictionary, and recipe coordinates are not contractually attached to results | Query pack rows by recipe and tokenizer coordinate. |
| Parser maintainer | Measure whether parser or plaintext policy changes affect downstream stats | Input view identity is implicit | Distinguish parser-IR plaintext policy from tokenizer and metric recipe changes. |
| Paper author | Cite results used in a figure or table | Warehouse run ids are useful but not stable publication identity | Cite request set, pack manifest, and per-work artifact ids. |
| Release reviewer | Detect reproducibility conflicts | Analysis outputs are not in manifest index | Reuse manifest conflict rule: same artifact id with different content hash fails validation. |

## Design Direction

Use requested analysis bundles with canonical per-work slices.

The stable unit is the slice:

```text
subject coordinate
  + input view coordinate
  + optional tokenizer coordinate
  + analysis recipe hash
  + output schema hash
  -> tokenized or analysis artifact manifest
```

Corpus-level packs are generated views over slices:

```text
resolved request set
  -> missing-slice planner
  -> per-work tokenized / analysis manifests
  -> analysis pack tables and pack index
```

Do not build an attrset or warehouse for every possible combination. A request
set is the only thing that turns a possible coordinate into a realized target.

## Alternatives Considered

| Criterion | Status quo placeholders | Per-analysis full manifests only | Warehouse/run as canonical identity | Recommended requested slices + packs |
|---|---|---|---|---|
| Handles identity correctly | No. Hash fields stay null and consumers infer meaning out of band. | Strong for per-work results, weak for corpus scans. | Weak for releases because run ids and database rows become citation targets. | Strong: manifests identify slices; packs accelerate scans. |
| Avoids Cartesian explosion | Yes, by not doing analysis. | Partly. Realization can be bounded, but corpus use is awkward. | Partly. Runs can be bounded, but run scope tends to become implicit identity. | Yes. Request sets are explicit and resolved before building. |
| Fits current manifest schema | Already fits because nothing is materialized. | Fits per-work `analysis` and `tokenized` artifacts. | Does not fit ABC release identity discipline. | Fits per-work slices now; collection packs need a schema revision. |
| Supports DuckDB/Parquet scans | No. | Only with ad hoc exports. | Yes. | Yes, but as derived packs over canonical manifests. |
| Failure attribution | No new behavior. | Good per work. | Good inside run tables, weaker across releases. | Good per work and per request set. |
| Main risk | Design gap persists. | Too many small files if packs are not added. | Database/run becomes source of truth. | Requires schema work for pack identity and sidecar roles. |

## Identity Model

### Separate the Coordinates

The design must not collapse these different things:

- Subject identity: source snapshot, source path/id, work id, work content hash,
  and metadata hash when used.
- Input view identity: which text view was consumed and which artifact or policy
  produced it.
- Tokenizer identity: tokenizer build, dictionary, profile/config,
  normalization, locale, granularity, and determinism tier.
- Recipe identity: formulas and analysis policy.
- Output contract identity: result schema or table layout.
- Run identity: when and where the work was executed.
- Pack identity: the resolved request set and pack layout.

Only the first five affect slice artifact identity. Run identity belongs in
provenance. Pack identity belongs to a collection-level manifest or pack
descriptor, not to a per-work slice.

Not every coordinate is identity-bearing for every slice. The input-view
identity map determines which coordinates must be populated and which must be
null for a given `input_view_kind` and recipe class.

### Subject Coordinate

A subject coordinate should carry at least:

```json
{
  "source_component": "aozorabunko",
  "remote_url": "https://github.com/aozorabunko/aozorabunko",
  "git_ref": "0e9ea3e586...",
  "logical_path": "cards/.../files/....txt",
  "source_id": "aozora:cards/.../files/....txt",
  "work_id": "aozora:<card-or-work-id>",
  "corpus_snapshot_hash": "sha256:...",
  "work_content_hash": "sha256:...",
  "metadata_record_hash": "sha256:... or null"
}
```

`work_content_hash` remains the identity-bearing text coordinate in current
manifests. `source_id`, `logical_path`, `git_ref`, and `work_id` are still
required in request sets, result payloads, and query packs so consumers can
resolve which edition/source occurrence was analyzed.

If two source coordinates have the same consumed text and the same relevant
metadata, they may legitimately point at the same slice artifact. The per-work
or per-edition mapping lives in the request set and pack index. If a metric
depends on metadata or source location, that dependency must be represented in
the recipe and the relevant metadata/source coordinate must be identity-bearing
for the slice.

### Input View Coordinate

An input view coordinate should state:

- `input_view_kind`: for example `parser-ir-plaintext-body-v1`,
  `parser-ir-plaintext-full-v1`, `source-visible-text-v1`,
  `tei-body-text-v1`, or `token-stream-v1`.
- `producer_artifact_id`: the artifact id for parser-IR, plaintext, TEI, or
  tokenized input when available.
- `producer_content_hash`: the content hash when the producing artifact is
  external to the current manifest.
- `policy_hash`: text projection, filtering, source-region, or plaintext policy
  hash when the view is a projection rather than a raw artifact.
- `coordinate_system`: byte, Unicode scalar value, grapheme cluster, or token
  index semantics for spans.

The input view coordinate is part of the analysis recipe contract and should be
recorded in result content even when current `manifest_identity_object` does not
have a dedicated `input_view_hash` field.

For any input view whose producer is an ABC artifact, the producer artifact id
and producer content hash must also appear in manifest provenance:

- `provenance.used`: producer artifact id and any policy/schema hashes consumed
  by the analysis.
- `provenance.was_derived_from`: at least the subject `work_content_hash`, plus
  the producer artifact id when the analysis is materially derived from that
  artifact's bytes.

The result content repeats the input-view coordinate for query ergonomics; it
is not the only lineage record.

### Input View Identity Map

The per-work-slice ADR must define an input-view identity map before any
producer writes `analysis` manifests. Implementers must not decide transitive
dependencies ad hoc. For current v0.4 manifests, the rule is:

| `input_view_kind` | Manifest identity rule | Required result-content fields | Required provenance |
|---|---|---|---|
| `source-visible-text-v1` | Set `corpus_snapshot_hash` and `work_content_hash`; parser, mapping, TEI, and tokenizer fields are null unless the recipe explicitly depends on one. | `source_id`, `logical_path`, `work_content_hash`, `coordinate_system`, text projection policy hash. | Source manifest or source snapshot hash in `used`; `work_content_hash` in `was_derived_from`. |
| `parser-ir-plaintext-body-v1` | Copy `parser_build_hash`, `parser_config_hash`, `aat_parser_ir_mapping_hash`, and `parser_ir_schema_hash` exactly from the producer parser-IR manifest, including nulls. `tei_profile_hash` is null. | Producer parser-IR artifact id, producer content hash, plaintext policy hash, `coordinate_system`. | Producer parser-IR artifact id in `used` and `was_derived_from`; plaintext policy hash in `used`. |
| `parser-ir-plaintext-full-v1` | Same as `parser-ir-plaintext-body-v1`, with a distinct plaintext policy hash. | Same as body view, plus the full-text inclusion policy id. | Same as body view. |
| `tei-body-text-v1` | Copy parser, mapping, parser-IR schema, and `tei_profile_hash` exactly from the producer TEI manifest, including nulls. Tokenizer fields are null unless the analysis consumes tokens. | Producer TEI artifact id, producer content hash, TEI text-extraction policy hash, `coordinate_system`. | Producer TEI artifact id in `used` and `was_derived_from`; text-extraction policy hash in `used`. |
| `token-stream-v1` | Copy tokenizer build and dictionary hashes from the producer tokenized manifest. Copy parser/TEI fields from that tokenized manifest exactly. The analysis manifest's `analysis_recipe_hash` is the consuming analysis recipe, not the tokenization producer recipe. | Producer tokenized artifact id, tokenizer profile hash, token-output schema hash, token coordinate system. | Producer tokenized artifact id and tokenizer profile hash in `used`; producer tokenized artifact id in `was_derived_from`. |

The first implementation slice should support only
`parser-ir-plaintext-body-v1`. The ADR must still include the table row so
conflict detection can be enforced from a deterministic identity rule.

Release validation must re-check copied identity fields. For any `analysis` or
`tokenized` manifest whose identity map says a field is copied from a producer,
the manifest index validator must load the producer manifest named in
`provenance.used` / `provenance.was_derived_from` and compare the copied fields
byte-for-byte, including nulls. Schema validation alone is not enough because a
wrong copied hash can still have the right JSON type.

### Tokenizer Coordinate

The tokenizer coordinate should be modeled as a hash-addressed profile value:

```json
{
  "schema_id": "https://w3id.org/abc/schemas/tokenizer-profile.schema.json",
  "profile_id": "vibrato-unidic-suw-v1",
  "tokenizer_name": "vibrato",
  "tokenizer_version": "...",
  "tokenizer_build_hash": "sha256:...",
  "dictionary_name": "unidic",
  "dictionary_archive_hash": "sha256:...",
  "dictionary_build_options_hash": "sha256:...",
  "profile_config_hash": "sha256:...",
  "normalization_policy_hash": "sha256:...",
  "locale": "ja-JP",
  "encoding": "UTF-8",
  "thread_policy": "deterministic-single-or-ordered",
  "granularity": "suw",
  "determinism_tier": "exact",
  "token_output_schema_hash": "sha256:..."
}
```

Current manifest v0.4 has only `tokenizer_build_hash` and
`tokenizer_dictionary_hash`. That is insufficient to distinguish every
meaningful tokenizer coordinate. The first implementation may fold profile,
normalization, and granularity into `analysis_recipe_hash` for analysis slices,
but a dedicated tokenizer profile/config hash is a blocker for publishing
standalone `tokenized` artifacts as exact release artifacts. It is also a
blocker for the second ADR that accepts tokenized-slice and analysis-pack
release identity.

### Analysis Recipe

`analysis_recipe_hash` is the SHA-256 hash of a canonical JSON recipe value,
using the same RFC 8785 JCS discipline as manifest identity. It is not a human
label.

Recipe and tokenizer semantic ids are lookup labels, not identities. The
registry stores immutable recipe/profile objects by content hash and an
append-only semantic-binding log. A binding log entry records at least
`semantic_id`, `content_hash`, `valid_from`, and optional
`supersedes_entry_hash`; its own canonical JSON hash is the
`registry_entry_hash`. A "current" semantic-id index is a generated view over
that log. `valid_until` may be derived from later binding entries, but existing
binding entries are not rewritten.

The request-set resolver may accept semantic ids as input, but its output must
record the selected `content_hash`, the `registry_entry_hash`, and the
resolution time for audit. Only the selected content hash participates in
artifact or request-set identity.

The recipe value should include:

- `schema_id` and recipe schema hash.
- `recipe_id` and recipe semantic version.
- Supported `input_view_kind` values.
- Required tokenizer granularity or `tokenizer_required: false`.
- Metric ids and formula versions.
- Denominators and count units.
- Unicode normalization policy.
- Sentence segmentation policy.
- Token filters, feature filters, and POS/lemma field mappings.
- Treatment of ruby, gaiji, notes, front matter, back matter, whitespace, and
  source apparatus.
- Null, warning, and failure behavior.
- Required output schema hash.
- Tool/build hash or release-lock coordinate for exact reproduction.
- Determinism tier: `exact`, `stable`, `bounded`, or `exploratory`.

Recipe classes:

| Class | Tokenizer required | Examples | Identity consequence |
|---|---|---|---|
| Text-view metrics | No | character count, line count, sentence length over parser-IR plaintext | Tokenizer hashes remain null. |
| Token metrics | Yes | token count, type-token ratio, STTR, Yule's K over surfaces or lemmas | Tokenizer coordinate is identity-bearing. |
| Morph-feature metrics | Yes | POS distribution, lemma richness | Tokenizer plus feature profile are identity-bearing. |
| Comparative analyzer metrics | Yes, often multiple tokenizers/analyzers | boundary disagreement, coverage mismatch | Better represented in morph warehouse packs, with explicit analyzer-set coordinates. |
| Model-derived metrics | Maybe | embeddings, classifier outputs | Must record model weights and stochastic policy; non-exact outputs are not releasable as exact artifacts. |

Recipes that require corpus-level statistics, such as corpus-normalized
denominators, must consume those statistics as a separate hash-addressed input
artifact produced by a prior pass. They must not close over the source corpus
snapshot directly from a per-work slice derivation. The corpus-statistics
artifact is recorded in `provenance.used`.

### Output Contract

For a per-work analysis slice, the content should be a deterministic JSON value
or a deterministic small table with a strict schema. A JSON slice should carry:

```json
{
  "schema_id": "https://w3id.org/abc/schemas/analysis-result.schema.json",
  "schema_hash": "sha256:...",
  "subject": { "...": "..." },
  "input_view": { "...": "..." },
  "tokenizer_profile_hash": "sha256:... or null",
  "analysis_recipe_hash": "sha256:...",
  "metrics": [
    {
      "metric_id": "sttr-500",
      "value": 0.731,
      "value_type": "float64",
      "denominator": 500,
      "unit": "token",
      "status": "passed"
    }
  ],
  "warnings": []
}
```

Metric ids are controlled by the recipe. Ad hoc extra metric keys are invalid
unless the recipe declares an extension namespace.

## Manifest Policy

### Per-Work Slices

Current v0.4 manifests can represent per-work slices:

- `artifact_kind`: `tokenized` or `analysis`.
- `work_content_hash`: the consumed work text identity.
- `metadata_record_hash`: non-null only when metadata affects output bytes.
- `parser_*`, `parser_ir_schema_hash`, `tei_profile_hash`: populated only when
  the input view depends on those artifacts.
- `tokenizer_build_hash`, `tokenizer_dictionary_hash`: non-null only when the
  slice depends on tokenization.
- `analysis_recipe_hash`: non-null for analysis slices and for any tokenization
  profile workaround that must be captured before a dedicated profile hash
  exists.
- `output_format_spec_hash`: result schema, token stream schema, or table layout
  hash.

Null means not applicable. It must not mean unknown. If a required coordinate
cannot be determined, produce a failed manifest or no release artifact.
For any field required by the input-view identity map, a builder must not emit a
successful `analysis` manifest with null as a placeholder for unknown.

### Sidecar Roles

Sidecar roles should be added in the same phase as the artifacts that need
them.

The per-work-slice ADR should add:

- `analysis-result`

It should add `token-table` only if that ADR also implements tokenized slices.

The analysis-pack/tokenizer-profile ADR should add, if still needed:

- `token-table`
- `analysis-table`
- `analysis-pack-index`
- `analysis-request-set`

Do not add a generic `recipe` sidecar role in the first slice. The recipe
registry is the canonical home for recipe/profile JSON by content hash. If a
future release package needs portable recipe copies, it should define a
release-pack role whose content hash must equal the registry object hash being
cited.

These roles should be schema-controlled. If a sidecar affects the bytes of the
primary artifact, its governing hash must also be represented in identity
through `analysis_recipe_hash`, `output_format_spec_hash`, or a future explicit
dimension. Otherwise it is an informational or traversal sidecar.

### Collection Packs

Current v0.4 identity is work-centric because `work_content_hash` is required
and singular. An analysis pack is collection-level and should not fake its
identity by stuffing a request-set or workset hash into `work_content_hash`.

A later manifest schema should add either:

- `artifact_kind = "analysis-pack"` with a collection identity object, or
- a variant identity subject where `work_content_hash` is nullable and
  `request_set_id` or `workset_hash` is required.

Recommended collection identity fields:

```text
manifest_schema_hash
corpus_snapshot_hash
request_set_id
analysis_recipe_hash
tokenizer_profile_hash | null
pack_layout_schema_hash
output_format_spec_hash
```

The pack manifest should list per-work slice artifact ids in a deterministic
index sidecar or table. The pack is a generated view; the slice manifests are
the canonical per-work records.

## Request Set Policy

An analysis request set is a canonical JSON value that resolves dynamic
selectors into fixed content-addressed coordinates before any build starts.

Like `artifact_id`, `request_set_id` is derived from an identity object and is
not included in its own hash input.

Example shape:

```json
{
  "schema_id": "https://w3id.org/abc/schemas/analysis-request-set.schema.json",
  "schema_hash": "sha256:...",
  "request_set_id": "sha256:...",
  "request_set_identity_object": {
    "schema_hash": "sha256:...",
    "corpus_snapshot_hash": "sha256:...",
    "subjects": [
      {
        "source_id": "aozora:cards/.../files/....txt",
        "work_id": "aozora:...",
        "work_content_hash": "sha256:...",
        "metadata_record_hash": "sha256:... or null"
      }
    ],
    "input_views": [
      {
        "input_view_kind": "parser-ir-plaintext-body-v1",
        "policy_hash": "sha256:..."
      }
    ],
    "tokenizer_profile_hashes": [],
    "analysis_recipe_hashes": ["sha256:..."],
    "missing_policy": "build-missing-only",
    "pack_policy_hash": "sha256:..."
  },
  "resolved_labels": {
    "analysis_recipes": [
      {
        "recipe_id": "literary-basic-ja-v1",
        "analysis_recipe_hash": "sha256:...",
        "registry_entry_hash": "sha256:...",
        "resolved_at": "2026-07-07T00:00:00Z"
      }
    ],
    "tokenizer_profiles": []
  },
  "batch_policy": "100-works-or-512mb"
}
```

Rules:

- `request_set_id = sha256(RFC8785-JCS(request_set_identity_object))`.
- `request_set_id`, `resolved_labels`, `batch_policy`, generated time,
  operator, local paths, and run id are excluded from
  `request_set_identity_object`.
- `request_set_identity_object` includes every coordinate that can change pack
  membership or pack bytes: resolved subjects, input view policy hashes,
  tokenizer profile hashes, analysis recipe hashes, `missing_policy`, and
  `pack_policy_hash`.
- Semantic ids such as `literary-basic-ja-v1` and `vibrato-unidic-suw-v1` are
  resolver inputs and human labels. At resolve time they must be normalized to
  `analysis_recipe_hash` and `tokenizer_profile_hash`. Only content hashes
  participate in request-set identity. The resolver must also record the
  semantic-binding `registry_entry_hash` and resolution time in non-identity
  metadata so a human label in a paper can be audited back to the exact hash it
  named at resolution time.
- Ordering is canonical before JCS hashing:
  - `subjects`: sort by `source_id`, then `work_content_hash`, then
    `metadata_record_hash` with null treated as the empty string.
  - `input_views`: sort by `input_view_kind`, then `policy_hash`.
  - `tokenizer_profile_hashes`: sort lexicographically by full hash string.
  - `analysis_recipe_hashes`: sort lexicographically by full hash string.
- `missing_policy` is one of `require-existing`, `build-missing-only`, or
  `record-missing-status`.
- `batch_policy` affects execution and failure grouping, not per-work slice
  identity.
- Changing the request set can change the pack identity. It must not change the
  identity of already-realized per-work slices.
- The null-to-empty-string rule is comparator-only. JCS serialization retains
  JSON null for `metadata_record_hash`, matching `manifest_identity_object`'s
  nullable-hash convention.
- Multiple subject entries may share `source_id` when they differ in
  `work_content_hash` or `metadata_record_hash`, for example across editions or
  metadata revisions. The three-key subject sort distinguishes them. Exact
  duplicate subject entries after normalization must be coalesced before
  hashing; a resolver may record a non-identity warning about the duplicate.

## Materialization Flow

```text
source snapshot + manifests
  -> manifest/query index
  -> resolved analysis-request-set.json
  -> planner checks existing tokenized/analysis manifests
  -> build missing per-work tokenized slices
  -> build missing per-work analysis slices
  -> validate no reproducibility conflicts
  -> write analysis pack tables and pack index
```

The planner is allowed to use SQLite, DuckDB, or a file index as a generated
view. The planner must not become the source of artifact identity.

## Nix Materialization Constraints

These constraints keep the identity design compatible with ADR 0003's Nix
cache, closure, and evaluation goals. They do not replace ADR 0003; they state
which builder shapes this analysis design requires if Nix is used.

### Evaluation Envelope

The request set is identity-bearing; the realization strategy is a separate
operational axis. A resolver or planner must estimate the realized slice
cardinality before emitting Nix targets:

```text
realized_slice_count =
  |subjects| x |input_views| x |analysis_recipe_hashes| x max(1, |tokenizer_profile_hashes|)
```

If that count exceeds the measured ADR 0003 evaluation envelope for per-work
derivations, the planner must not emit one derivation per slice. It must choose
one of the bounded-workset alternatives:

- batch derivations with a recorded batch rule, or
- a single requested-set/CAS realization derivation that writes a populated
  content-addressed output tree.

The chosen realization strategy and batch rule are run/materialization metadata.
They are excluded from per-work slice identity and from
`request_set_identity_object` unless they change pack bytes.

### Build-Time Manifest Reading

Producer identity fields are copied by the analysis tool at build time, not by
Nix evaluation. A slice derivation takes named input store paths for:

- the producer artifact or tokenized artifact,
- the exact analysis recipe object,
- the exact tokenizer profile object when applicable,
- schemas and policy files required by the recipe.

The Nix expression must not parse producer manifests during evaluation to
construct child `manifest_identity_object` values. Evaluation should assemble
store paths and builder arguments; the builder reads manifests, copies required
identity fields according to the input-view map, computes output bytes, and
writes the child manifest.

### Determinism Tier and Derivation Kind

Cache policy uses the effective output determinism tier, not only the recipe's
declared tier. The effective tier is the lowest tier across the recipe, input
view producer, tokenizer profile, model weights, and any auxiliary artifacts
the recipe consumes. A stable recipe over a bounded tokenizer is bounded. An
exact recipe over exact text-view input remains exact.

The effective tier controls cache policy:

| Tier | Nix realization | Substitution policy |
|---|---|---|
| `exact` | Fixed-output or content-addressed release replay once `content_hash` is known. Initial discovery builds may be input-addressed until the output hash is recorded. | Content-addressed cache or trusted signed cache; release replay must verify the declared `content_hash`. |
| `stable` / `bounded` | Input-addressed derivation. | Trusted signed binary cache only; output remains useful but is not claimed as exact. |
| `exploratory` | Input-addressed or local script realization. | Local only; not published to release binary caches. |

Do not make non-exact analysis slices fixed-output release artifacts. Do not
publish exact claims for input-addressed outputs unless a later replay verifies
the manifest `content.content_hash` under the exact recipe and input
coordinates.

### Closure Boundary

Per-work analysis slice derivations must not take the corpus snapshot store path
as a direct input. Their named text input is the producer artifact store path
for the per-work parser-IR, plaintext, TEI, or tokenized artifact. The
`corpus_snapshot_hash` remains in manifest identity and provenance for citation
and audit.

The source snapshot is a Nix input at the source/parse tier only, where
per-work or per-batch producer artifacts are created. This prevents every
analysis slice closure from pulling the full corpus snapshot into consumers'
stores.

### Registry Realization

Recipe and tokenizer-profile registries must be realized as per-hash store
paths, not as a directory whose whole contents are an input to every slice.
Adding a new recipe/profile must not rotate the store path for historical
recipe/profile objects.

Acceptable shapes:

- a resolver outside Nix maps each `analysis_recipe_hash` or
  `tokenizer_profile_hash` to an exact file/store path and passes only those
  paths to the builder, or
- Nix exposes one file value per recipe/profile hash, and slice derivations
  reference only the specific hashes used by the request set.

Avoid `builtins.readDir` or `builtins.readFile` over a mutable registry
directory during evaluation for slice construction.

### Binary Cache Trust Boundary

ABC artifact identity and Nix cache identity are not the same coordinate.
Manifests distinguish `artifact_id` from `content.content_hash`; binary caches
and substituters operate at the store-path/content layer. Intentional reuse of
the same content under multiple artifact ids is allowed, but release review
must audit the shared `content_hash` provenance and signature policy because a
bad cached content object affects every artifact id that cites it.

### Batch Failure Model

If a future planner uses batch derivations, a per-work analysis failure is data
inside the batch output, not necessarily a Nix derivation failure. A batch
builder must run every member it can, emit per-work success or failed manifests,
and fail the derivation only when the batch-level contract is broken, such as a
malformed output tree, missing required indexes, or a tool/runtime failure that
prevents reliable per-work attribution. This preserves ADR 0003's requirement
that failures remain attributable to individual works.

The pack writer may use Parquet for corpus-scale metrics and token facts. It
should include columns sufficient to rejoin every row to canonical manifests:

- `request_set_id`
- `artifact_id`
- `content_hash`
- `source_id`
- `work_id`
- `corpus_snapshot_hash`
- `work_content_hash`
- `metadata_record_hash`
- `input_view_kind`
- `producer_artifact_id`
- `tokenizer_profile_hash`
- `tokenizer_build_hash`
- `tokenizer_dictionary_hash`
- `analysis_recipe_hash`
- `metric_id`
- `value`
- `value_type`
- `unit`
- `status`
- `warning_count`

The pack column list is itself an output-format contract. Its schema or table
layout hash is the pack's `output_format_spec_hash`.

Columns that carry ABC hashes use the same `sha256:<hex>` UTF-8 string form as
manifest identity fields. Nullable hash columns are nullable strings, not binary
hash blobs or empty strings.

## Error Handling

- If tokenization or analysis fails for one subject, write a failed manifest
  for that intended coordinate when the coordinate is known.
- Failed manifests should carry error sidecars using existing failure-manifest
  conventions.
- A request set with `record-missing-status` may produce pack rows with
  `status = "missing"` for unresolved slices, but those rows are not successful
  analysis artifacts.
- If two successful manifests share the same `artifact_id` and different
  `content.content_hash`, release validation fails as a reproducibility
  conflict.
- The inverse is not a conflict: two different `artifact_id` values may point
  at the same `content.content_hash` when two coordinates legitimately reuse
  identical bytes. Request sets and pack indexes preserve the coordinate
  mapping.
- If a tokenizer is not classified `exact`, artifacts may still be useful as
  stable or bounded analysis, but release policy must not present them as exact
  reproducible results.

## Publication Interaction

Analysis artifacts are independent analytical artifacts by default.

If a TEI or plaintext publication only links to an analysis sidecar, the
publication artifact id does not rotate when the analysis changes. The analysis
sidecar has its own identity.

If a TEI header, RDF view, or other publication artifact embeds analysis values
in its output bytes, then the publication artifact identity must include the
analysis recipe and tokenizer coordinates that affect those bytes. In that case
analysis is no longer merely an informational sidecar for that publication.

## Components

| Component | Purpose | Inputs | Outputs | State / Time / Identity |
|---|---|---|---|---|
| Recipe registry | Store canonical tokenizer profiles and analysis recipes | JSON recipe/profile files and semantic-binding log entries | Content hashes, binding-entry hashes, validation reports | Objects are append-only by hash; semantic-id "current" mappings are generated views over append-only binding events. |
| Manifest index | Locate existing slices and conflicts | Manifest files | Coordinate lookup rows | Generated view; rebuildable; not identity-bearing. |
| Request-set resolver | Turn selectors into fixed subject lists | Corpus snapshot, metadata, selector config | `analysis-request-set.json` | Request set is immutable by hash once resolved. |
| Slice planner | Decide which slices are missing | Request set, manifest index | Build plan | Operational state only; no artifact identity. |
| Tokenized slice producer | Materialize token streams | Input view, tokenizer profile | `tokenized` manifest and content | Per-work artifact identity. |
| Analysis slice producer | Materialize metrics | Input view or token stream, recipe | `analysis` manifest and content | Per-work artifact identity. |
| Pack writer | Build scan-optimized tables | Slice manifests and contents | Analysis pack tables and index | Generated collection view; future collection manifest identity. |

## Data Lifecycle

- Recipes, tokenizer profiles, and semantic-binding log entries are retained
  indefinitely once referenced by a release.
- Per-work successful slice manifests are canonical and append-only.
- For any release claiming exact reproducibility, every cited slice manifest and
  every referenced recipe/profile object must be retained for the lifetime of
  that release's cited artifacts.
- Failed slice manifests are retained at least for the release/run that
  attempted them, so missing values are auditable.
- Analysis packs are rebuildable and may be stored hot/warm/cold according to
  size and reuse. Their descriptors and request sets should be retained even if
  tables are regenerated later.
- Query indexes are disposable and must be rebuildable from manifests and
  retained request sets.

## Design Review

| Finding | Classification | Observation | Risk | Resolution |
|---|---|---|---|---|
| ADR 0003 cost envelope is still Draft. | Follow-up | The materialization policy has synthetic evidence but no accepted smoke-corpus measurement. | Readers may treat bounded materialization as already proven. | State this dependency explicitly; request-set planners must respect the measured ADR 0003 envelope before release-scale adoption. |
| Input view dependencies can drift across implementers. | Mitigated for first slice | Without a mapping, implementers could choose different parser/mapping fields for the same logical input view. | Same logical slice could produce different artifact ids. | Add the input-view identity map and require exact copying from producer manifests. First slice supports `parser-ir-plaintext-body-v1` only. |
| Copied producer identity fields can be wrong but schema-valid. | Mitigated for first slice | A builder can emit hashes with the right type but copied from the wrong producer or policy. | Single-builder pipelines may silently mint bad artifact ids until another implementation conflicts. | Release validation must compare copied fields against the producer manifest named in provenance. |
| Request-set identity can be circular or collision-prone. | Mitigated | A request set contains a derived id plus content coordinates, and multiple editions may share a `source_id`. | Including the id creates circular hashing; hashing only subjects collides across recipes/profiles; duplicate handling could differ. | Define `request_set_identity_object`, exclude `request_set_id`, include recipe/profile hashes and pack policy hash, fix array sort keys, allow shared `source_id` when later sort keys differ, and coalesce exact duplicate subjects before hashing. |
| Semantic ids can drift over time. | Mitigated | Recipe/profile labels may point at newer content over time. | A human citation to `literary-basic-ja-v1` is ambiguous without the selected binding. | Store recipe/profile objects by content hash and semantic-id bindings in an append-only log; resolver records content hash, binding-entry hash, and resolution time, while only content hashes affect identity. |
| Request-set cardinality can exceed Nix evaluation budget. | Blocking for second ADR | Identity-bounded request sets can still produce many derivations. | A stable `request_set_id` could name an unbuildable release target under ADR 0003's envelope. | Planner must choose per-work, batch, or single-CAS realization based on measured evaluation budget; realization strategy is separate from identity. |
| Producer identity copying can happen at the wrong time. | Mitigated | Copying producer fields could be done by evaluator or by builder. | Eval-time manifest parsing scales with slice count and harms cache behavior. | Require build-time copying by the analysis tool; Nix passes store paths and does not parse producer manifests during evaluation. |
| Determinism tier lacks cache semantics. | Mitigated | Exact/stable/bounded/exploratory were semantic tiers only, and recipe tier can differ from tokenizer or producer tier. | Exact outputs might miss content-addressed cache behavior; non-exact outputs might be forced into fixed-output builds; bounded dependencies could be hidden by an exact recipe. | Compute an effective tier as the lowest tier across recipe and inputs. Exact release replay verifies content hash; stable/bounded are input-addressed signed-cache; exploratory is local-only. |
| Per-work slices could close over full corpus snapshots. | Mitigated | `corpus_snapshot_hash` is identity-bearing, but the snapshot store path need not be a slice input. | Every slice closure could pull the full corpus into consumers' stores. | Per-work slice inputs are producer artifact paths; source snapshots are inputs only at source/parse tier. |
| Corpus-normalized recipes need extra inputs. | Follow-up | Some metrics may require corpus-level denominators or reference distributions. | A per-work slice might reintroduce full-corpus closure bloat through the recipe path. | Such recipes must consume a separate hash-addressed corpus-statistics artifact from a prior pass and record it in provenance. |
| Registry-as-directory can become a global cache-buster. | Blocking before Nix wiring | Append-only recipe/profile directories change when unrelated recipes are added. | Historical slice derivations would re-evaluate or rebuild when the registry grows. | Realize recipes/profiles as per-hash store paths and pass only the used hash paths to builders. |
| Binary cache trust is content-layer, not artifact-id-layer. | Mitigated | Multiple artifact ids may cite the same content hash. | A bad cached content object affects every artifact id that accepts it. | Release review must audit shared `content_hash` provenance and signature policy. |
| Batch derivation failure semantics are ambiguous. | Follow-up | Nix marks a failed derivation as failed, but ADR 0003 requires per-work failure attribution. | Batch builds could lose successful siblings or hide failures incorrectly. | Future batch builders should emit per-work success/failure manifests and fail only on batch-level contract failures. |
| Request-set null sort wording can be misread. | Mitigated | Null was treated as empty string for sorting. | Implementers could serialize null as `""`, changing `request_set_id`. | Clarify null-to-empty-string is comparator-only; JCS retains JSON null. |
| Pack column list needs an output-format hash. | Mitigated | The recommended Parquet columns define a layout. | Pack table layout or hash column types could drift without identity rotation. | State that the pack layout schema/table hash is the pack `output_format_spec_hash`, and hash columns use the manifest `sha256:<hex>` string form. |
| Required-null semantics can be misused. | Mitigated | Manifest schema permits nullable hash fields but cannot tell unknown from not applicable. | A partial successful manifest could hide an unknown required coordinate behind null. | The input-view identity map determines required fields; unknown required fields require a failed manifest or no successful manifest. |
| Tokenizer config is not fully represented in manifest v0.4 identity. | Blocking for exact standalone tokenized artifacts | v0.4 has build and dictionary hashes but no profile/config hash. | Two token streams with different granularity or normalization could collide if only current fields are used. | Add a tokenizer profile/config hash before accepting the tokenized-artifact release path. Until then, fold profile/config into recipe hash for analysis slices and avoid exact release claims for standalone tokenized artifacts. |
| Collection packs do not fit work-centric identity. | Blocking for pack manifests | v0.4 requires singular `work_content_hash`. | Pack manifests would misuse work identity or lose the request-set coordinate. | Limit v0.4 to per-work slices; add collection identity schema before publishing canonical packs. |
| Recipe sidecar could duplicate the registry. | Mitigated | Recipes already live in a content-addressed registry. | Two canonical homes for recipe JSON create drift and citation confusion. | Do not add a generic recipe sidecar in the first slice. Future portable release copies must assert byte equality with the registry object hash. |
| Pack-only sidecar roles could leak into the first slice. | Mitigated | Some proposed roles exist only for collection packs. | v0.4 would expose half-supported roles. | Add only `analysis-result` first; defer pack/request roles to the pack ADR. Add `token-table` only with tokenized slices. |
| Warehouse run ids are tempting citation targets. | Follow-up | The morph warehouse has immutable run dirs and useful Parquet facts, and existing users may already cite run ids. | Existing run citations can become orphaned if no bridge maps them to canonical artifact identity. | Treat warehouse runs as producers or generated packs; second ADR should define a run-provenance sidecar mapping run id to request-set id, slice artifact ids, and pack artifact ids. |
| PROV-O export is useful but not first-slice identity. | Follow-up | Manifest provenance already uses PROV-like fields. | Paper authors may need machine-readable provenance export without changing artifact identity. | Consider a generated PROV-O JSON-LD view over manifests and analysis provenance after the core slice contract is stable. |
| First-slice producer lookup is an implementation dependency. | Mitigated for first slice | The analysis builder needs the producer parser-IR manifest before it can copy fields. | The first slice could specify copying without providing a way to resolve the producer artifact. | Manifest index must support parser-IR lookup by `work_content_hash`, corpus snapshot, and relevant parser/input-view coordinates. |
| Vocabulary is broad for a first slice. | Mitigated for first ADR | The design introduces slice, request set, pack, run, recipe, tokenizer profile, and batch terms. | Implementers may pull deferred pack or batch concepts into the first ADR. | The first-slice ADR must list which glossary terms are in scope and which are deferred. |
| Publication sidecars can silently affect TEI if values are embedded later. | Mitigated | TEI currently does not consume analysis. | Future TEI headers could include metrics without rotating identity. | Rule: embedding analysis values makes analysis coordinates identity-bearing for that publication. |
| Metrics can share names while differing in formulas. | Mitigated | STTR and Yule's K depend on token filters, denominators, and formulas. | Consumers compare incompatible values. | Metric ids are scoped by recipe hash; formula details live in canonical recipe JSON. |
| Same content across source coordinates may share one slice. | Accepted tradeoff | Current manifest identity is content-oriented, not path-oriented. | A per-work query may need mapping rows even when artifact ids are shared. | Request sets and pack indexes carry `work_id`, `source_id`, and git/source coordinates. Promote source coordinate to identity only for recipes that depend on it. |

No blocking design finding remains for the first token-independent per-work
analysis slice as a prototype. If that slice is wired into Nix, it must use the
build-time producer-copy rule and per-hash recipe store paths from this spec.
Collection analysis packs require a manifest schema follow-up before they
become canonical release artifacts. Exact standalone tokenized artifacts
require a tokenizer profile/config hash before their release path is accepted.

## First Implementation Slice

The first implementation slice should be deliberately small:

1. Define `analysis-recipe.schema.json`, `analysis-result.schema.json`, and a
   single `literary-basic-ja-v1` recipe for token-independent parser-IR
   plaintext metrics.
2. Define the `parser-ir-plaintext-body-v1` input-view identity map and require
   exact copying of parser/mapping identity fields from the producer parser-IR
   manifest. Add a release validation check that compares those copied fields
   against the producer manifest.
3. Add the `analysis-result` sidecar role only.
4. Materialize one per-work `analysis` manifest from an existing parser-IR
   plaintext input, with tokenizer fields null.
5. Extend the manifest index enough to include analysis artifact kind,
   reproduce conflict detection, and resolve producer `parser-ir` artifacts by
   `work_content_hash`, corpus snapshot, and relevant parser/input-view
   coordinates.
6. If the prototype is exposed through Nix, pass the producer artifact and
   recipe as per-hash store-path inputs, copy producer identity fields at build
   time, avoid a registry-directory input, and keep the corpus snapshot out of
   the analysis-slice closure.
7. In the first ADR, mark request sets as in scope for bounded build planning,
   but mark collection packs, run-provenance bridges, batch realization,
   tokenized slices, tokenizer profile identity, and PROV-O JSON-LD export as
   deferred unless they are explicitly implemented.

Token-dependent metrics, tokenizer profile schemas, and collection packs should
follow only after the per-work slice contract is validated.

## ADR Follow-Ups

This spec should feed at least two ADRs:

1. Analysis artifact identity ADR: accepts per-work analysis slices, the
   `parser-ir-plaintext-body-v1` input-view identity map, recipe hash rules,
   semantic-binding log audit rules, request-set identity construction for
   bounded builds, copied-field release validation, Nix-safe first-slice
   realization rules, parser-IR producer lookup, publication interaction rules,
   and failure behavior.
2. Analysis pack and tokenizer profile ADR: accepts collection identity,
   request-set id reuse, tokenizer profile/config hash, Parquet pack layout,
   corpus-statistics input artifacts, run-provenance bridges, batch/CAS
   realization policy, exact cache/substitution policy, and optional PROV-O
   JSON-LD export.

Keeping these separate prevents collection/query packaging from blocking the
basic per-work analysis contract.
