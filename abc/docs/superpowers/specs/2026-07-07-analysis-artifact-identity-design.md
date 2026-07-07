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
but an ADR should add a dedicated tokenizer profile/config hash before ABC
publishes tokenized artifacts as exact release artifacts.

### Analysis Recipe

`analysis_recipe_hash` is the SHA-256 hash of a canonical JSON recipe value,
using the same RFC 8785 JCS discipline as manifest identity. It is not a human
label.

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

### Sidecar Roles

`manifest.schema.json` should add sidecar roles:

- `token-table`
- `analysis-result`
- `analysis-table`
- `analysis-pack-index`
- `analysis-request-set`
- `recipe`

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
  `request_set_hash` or `workset_hash` is required.

Recommended collection identity fields:

```text
manifest_schema_hash
corpus_snapshot_hash
request_set_hash
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
selectors into a fixed list before any build starts.

Example shape:

```json
{
  "schema_id": "https://w3id.org/abc/schemas/analysis-request-set.schema.json",
  "request_set_id": "sha256:...",
  "corpus_snapshot_hash": "sha256:...",
  "subjects": [
    {
      "source_id": "aozora:cards/.../files/....txt",
      "work_id": "aozora:...",
      "work_content_hash": "sha256:...",
      "metadata_record_hash": "sha256:... or null"
    }
  ],
  "input_views": ["parser-ir-plaintext-body-v1"],
  "tokenizer_profiles": ["vibrato-unidic-suw-v1"],
  "analysis_recipes": ["literary-basic-ja-v1"],
  "missing_policy": "build-missing-only",
  "batch_policy": "100-works-or-512mb",
  "pack_policy": "parquet-metrics-v1"
}
```

Rules:

- The request set hash is over the resolved subject list, not an unresolved
  query such as "all works by author X".
- Ordering is canonical. Arrays are sorted by declared stable keys before JCS
  hashing.
- `missing_policy` is one of `require-existing`, `build-missing-only`, or
  `record-missing-status`.
- `batch_policy` affects execution and failure grouping, not per-work slice
  identity.
- Changing the request set can change the pack identity. It must not change the
  identity of already-realized per-work slices.

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

The pack writer may use Parquet for corpus-scale metrics and token facts. It
should include columns sufficient to rejoin every row to canonical manifests:

- `request_set_hash`
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
| Recipe registry | Store canonical tokenizer profiles and analysis recipes | JSON recipe/profile files | Hashes, validation reports | Append-only by hash; semantic ids may supersede older versions. |
| Manifest index | Locate existing slices and conflicts | Manifest files | Coordinate lookup rows | Generated view; rebuildable; not identity-bearing. |
| Request-set resolver | Turn selectors into fixed subject lists | Corpus snapshot, metadata, selector config | `analysis-request-set.json` | Request set is immutable by hash once resolved. |
| Slice planner | Decide which slices are missing | Request set, manifest index | Build plan | Operational state only; no artifact identity. |
| Tokenized slice producer | Materialize token streams | Input view, tokenizer profile | `tokenized` manifest and content | Per-work artifact identity. |
| Analysis slice producer | Materialize metrics | Input view or token stream, recipe | `analysis` manifest and content | Per-work artifact identity. |
| Pack writer | Build scan-optimized tables | Slice manifests and contents | Analysis pack tables and index | Generated collection view; future collection manifest identity. |

## Data Lifecycle

- Recipes and tokenizer profiles are retained indefinitely once referenced by a
  release.
- Per-work successful slice manifests are canonical and append-only.
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
| Tokenizer config is not fully represented in manifest v0.4 identity. | Strong suggestion | v0.4 has build and dictionary hashes but no profile/config hash. | Two token streams with different granularity or normalization could collide if only current fields are used. | Add a tokenizer profile/config hash in a future schema. Until then, fold profile/config into recipe hash for analysis slices and avoid exact release claims for standalone tokenized artifacts. |
| Collection packs do not fit work-centric identity. | Blocking for pack manifests | v0.4 requires singular `work_content_hash`. | Pack manifests would misuse work identity or lose the request-set coordinate. | Limit v0.4 to per-work slices; add collection identity schema before publishing canonical packs. |
| Warehouse run ids are tempting citation targets. | Mitigated | The morph warehouse has immutable run dirs and useful Parquet facts. | A run path can mix operational execution scope with artifact identity. | Treat warehouse runs as producers or generated packs. Cite manifests, request-set hashes, and content hashes. |
| Publication sidecars can silently affect TEI if values are embedded later. | Mitigated | TEI currently does not consume analysis. | Future TEI headers could include metrics without rotating identity. | Rule: embedding analysis values makes analysis coordinates identity-bearing for that publication. |
| Metrics can share names while differing in formulas. | Mitigated | STTR and Yule's K depend on token filters, denominators, and formulas. | Consumers compare incompatible values. | Metric ids are scoped by recipe hash; formula details live in canonical recipe JSON. |
| Same content across source coordinates may share one slice. | Accepted tradeoff | Current manifest identity is content-oriented, not path-oriented. | A per-work query may need mapping rows even when artifact ids are shared. | Request sets and pack indexes carry `work_id`, `source_id`, and git/source coordinates. Promote source coordinate to identity only for recipes that depend on it. |

No blocking design finding remains for per-work analysis slices. Collection
analysis packs require a manifest schema follow-up before they become canonical
release artifacts.

## First Implementation Slice

The first implementation slice should be deliberately small:

1. Define `analysis-recipe.schema.json`, `analysis-result.schema.json`, and a
   single `literary-basic-ja-v1` recipe for token-independent parser-IR
   plaintext metrics.
2. Add sidecar roles for analysis result and recipe references.
3. Materialize one per-work `analysis` manifest from an existing parser-IR
   plaintext input, with tokenizer fields null.
4. Extend the manifest index enough to include analysis artifact kind and
   reproduce conflict detection.

Token-dependent metrics, tokenizer profile schemas, and collection packs should
follow only after the per-work slice contract is validated.

## ADR Follow-Ups

This spec should feed at least two ADRs:

1. Analysis artifact identity ADR: accepts per-work analysis slices, recipe hash
   rules, publication interaction rules, and failure behavior.
2. Analysis pack and tokenizer profile ADR: accepts collection identity,
   request-set hash, tokenizer profile/config hash, and Parquet pack layout.

Keeping these separate prevents collection/query packaging from blocking the
basic per-work analysis contract.
