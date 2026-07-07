# ADR 0027: Analysis Packs and Tokenizer Profiles

Status: Proposed
Date: 2026-07-07
Supersedes: none
Depends on: ADR 0001, ADR 0003, ADR 0004, ADR 0010, ADR 0025, ADR 0026
Source: `docs/adr/0026-analysis-artifact-identity.md`,
`docs/superpowers/specs/2026-07-07-soranoha-snapshot-publication-design.md`

## Implementation Status

Partially implemented.

ADR 0026's token-independent per-work analysis slice remains the only accepted
analysis artifact shape today. The tokenizer-profile schema and resolver
content-hash path are implemented as a pre-tokenization contract:
`schemas/tokenizer-profile.schema.json`,
`data/tokenizer-profiles/fixture-tokenizer-ja-v1.json`, and
`test/abc/tools/request_set_resolver_test.clj` cover profile label resolution
to content hashes and registry-entry hashes.

The manifest schema blocker remains open. ABC still cannot publish tokenized
slices, tokenizer-backed stylometric analysis, or collection analysis packs as
canonical snapshot artifacts until the remaining acceptance criteria below are
implemented.

Snapshot-publication work may proceed with TEI, plaintext, token-independent
analysis, request-set fixtures, and snapshot indexes. It must not claim
canonical tokenizer outputs or tokenizer-backed metrics until this ADR or a
successor is implemented.

## Context

ADR 0026 intentionally stopped at a per-work `analysis` slice over
`parser-ir-plaintext-body-v1`. It reserved request-set and pack vocabulary, but
deferred the hard parts that would otherwise create a Cartesian product:

- standalone `tokenized` artifact identity,
- a dedicated tokenizer profile/config coordinate,
- token stream sidecar roles and token coordinate systems,
- tokenizer-backed analysis recipe rules,
- collection pack identity and table layout,
- request-set materialization strategy and batch failure semantics.

The snapshot-publication design now needs named full-corpus request sets and a
static snapshot index. That snapshot layer can cite ADR 0026 request-set ids
for token-independent analysis, but tokenized artifacts need a stronger
identity rule than current manifest v0.4 fields provide.

Current manifest identity has `tokenizer_build_hash` and
`tokenizer_dictionary_hash`, but those two fields do not distinguish
normalization, locale, token granularity, dictionary build options, thread
policy, tokenizer configuration, or token output schema. Treating those as
implicit would make exact tokenized release claims false.

## Decision

ABC will extend the analysis-artifact model in three layers:

1. **Tokenized slices** are per-work `tokenized` artifacts identified by a
   tokenizer profile hash, producer input-view identity, and token output
   schema.
2. **Tokenizer-backed analysis slices** are per-work `analysis` artifacts
   derived from tokenized slices. Their recipe hash identifies the metric
   formula and token filters; the producer tokenized artifact identifies the
   token stream.
3. **Analysis packs** are collection-level generated query packages over
   realized tokenized and analysis slices. Packs accelerate scans but do not
   replace per-work manifests as the canonical conflict boundary.

The request set remains the only object that turns possible coordinates into
realized work. Nix must materialize requested coordinates only; it must not
enumerate every work, parser, tokenizer profile, recipe, and output format at
flake evaluation time.

## Tokenizer Profile Identity

ABC will introduce a hash-addressed tokenizer profile value. The profile hash
is the JCS SHA-256 hash of the canonical tokenizer profile JSON.

A tokenizer profile records at least:

- `schema_id` and tokenizer-profile schema hash,
- semantic `profile_id` for humans,
- tokenizer name, version, source/build hash, and build options hash,
- dictionary name, archive hash, build options hash, and license metadata,
- input normalization policy hash,
- locale and encoding,
- thread/concurrency policy,
- token granularity,
- token output schema hash,
- determinism tier,
- fixture evidence hash.

Tokenizer profile semantic ids use the same append-only binding-log discipline
as ADR 0026 recipes. A resolver may accept labels such as
`vibrato-unidic-suw-v1`, but the resolved request set records the selected
profile content hash, registry-entry hash, and resolution time. Only the
profile content hash participates in artifact or request-set identity.

### Manifest Schema Blocker

Publishing standalone `tokenized` artifacts as exact release artifacts requires
a manifest schema revision that adds `tokenizer_profile_hash` to
`manifest_identity_object`.

Until that schema exists:

- token-independent ADR 0026 analysis remains canonical;
- tokenizer profile hashes may appear in request sets, result content, and
  provenance for prototypes;
- standalone `tokenized` artifacts must not be described as exact canonical
  release artifacts;
- tokenizer-backed analysis must either remain non-canonical or fold every
  tokenizer-profile coordinate into the consuming `analysis_recipe_hash`, which
  is an interim prototype rule, not the accepted release path.

`tokenizer_build_hash` and `tokenizer_dictionary_hash` remain useful copied
producer fields, but they are not sufficient profile identity by themselves.
Once `tokenizer_profile_hash` is added to manifest identity, the profile hash
is the authoritative tokenizer coordinate. The build and dictionary hashes are
validated copies of profile subfields retained for ADR 0001 continuity,
querying, and human inspection. They must not be interpreted as independent
tokenizer-choice coordinates; if they diverge from the tokenizer profile
content, release validation fails.

### Transition Guardrail

Current manifest schema v0.4.1 already permits `artifact_kind = "tokenized"`,
but it cannot yet carry `tokenizer_profile_hash` in
`manifest_identity_object`. Until the manifest schema revision lands, release
validation must reject any successful `tokenized` manifest
(`artifact_kind = "tokenized"` and `validation_status` not equal to `failed`).
Prototype tokenized outputs may exist only as local or explicitly
non-canonical artifacts.

## Tokenized Slice Contract

A tokenized slice is a successful per-work manifest with
`artifact_kind = "tokenized"`.

The accepted first input view for tokenization is
`parser-ir-plaintext-body-v1`. Later input views require an input-view identity
map update.

For a tokenized slice:

- subject fields come from the consumed parser-IR/plaintext producer;
- parser and mapping fields are copied exactly from the producer manifest,
  including nulls;
- `tokenizer_build_hash` and `tokenizer_dictionary_hash` are copied from the
  tokenizer profile's corresponding fields;
- `tokenizer_profile_hash` is identity-bearing once the manifest schema
  supports it;
- `analysis_recipe_hash` is null;
- `output_format_spec_hash` identifies the token stream schema.

The token stream sidecar role is `token-stream`. A manifest schema update must
add this sidecar role before canonical tokenized slices are published.

The token stream content records:

- tokenizer profile hash,
- producer parser-IR/plaintext artifact id and content hash,
- input plaintext policy hash,
- token output schema hash,
- token coordinate system,
- token records,
- warnings.

The first accepted token coordinate system is:

```text
token-index-v1 + unicode-scalar-value-input-spans
```

Each token record has a stable token index and input span offsets in the
plaintext input view. If the tokenizer cannot provide deterministic spans back
to that input view, the profile cannot be Exact.

Canonical tokenized slices also require an accepted token output schema before
publication. Different token output schemas legitimately produce different
`output_format_spec_hash` values, but release artifacts must use an accepted
schema rather than accumulating ad hoc prototype schemas.

Release validation must re-check every copied producer field against the
producer manifest named in provenance. It must also verify that the tokenized
manifest's build and dictionary hashes match the tokenizer profile content
hash recorded in provenance.

Producer invalidation is not automatically transitive. If a producer manifest
is later superseded or marked `invalidated_at`, downstream tokenized and
analysis manifests retain the copied values they were built with until a
validation pass re-checks the copy chain and emits downstream invalidation or
replacement decisions. This ADR defines the copy-chain validation rule, not a
global invalidation propagation system.

## Tokenizer-Backed Analysis

A tokenizer-backed analysis slice consumes a producer tokenized artifact using
`input_view_kind = "token-stream-v1"`.

For that analysis slice:

- parser, mapping, TEI, tokenizer build, and dictionary fields are copied
  exactly from the producer tokenized manifest;
- `tokenizer_profile_hash` is copied from the producer tokenized manifest once
  the manifest schema supports it;
- `analysis_recipe_hash` is the consuming metric recipe, not the tokenizer
  profile hash and not the tokenized producer's output schema hash;
- `output_format_spec_hash` identifies the analysis-result schema.

The analysis recipe must state:

- required token granularity,
- token normalization assumptions,
- POS/lemma/surface field usage,
- token filters,
- denominator policy,
- sentence segmentation policy when metric formulas depend on sentence units,
- treatment of ruby, gaiji, notes, source apparatus, front matter, and back
  matter,
- exact null/warning/failure behavior.

Recipes that require corpus-level statistics, such as corpus-normalized
denominators, must consume those statistics as a separate hash-addressed input
artifact recorded in provenance. They must not close over the full corpus
snapshot from each per-work derivation.

## Request Sets

ADR 0026's `request_set_identity_object` remains the base analysis request
contract. This ADR extends its interpretation for tokenized and pack-producing
request sets.

The identity object includes:

- resolved subjects,
- input views and policy hashes,
- tokenizer profile hashes,
- analysis recipe hashes,
- missing policy,
- pack policy hash.

The resolver must normalize semantic labels to content hashes before hashing.
`request_set_id`, run id, local paths, generated time, operator, resolved-label
display data, and batch policy remain excluded from request-set identity.

The request-set identity object contains tokenizer profile content hashes only.
Registry entry hashes and resolution timestamps are resolver provenance and
audit data.

Array sorting rules from ADR 0026 remain binding. In particular, null-to-empty
string treatment for `metadata_record_hash` is comparator-only; JCS
serialization keeps JSON null.

Duplicate subject entries after normalization are coalesced before hashing and
may be reported as non-identity warnings.

The resolver must project the realized coordinate count before materialization:

```text
subjects x input_views x tokenizer_profiles x analysis_recipes
```

When the projected realized set would exceed ADR 0003's measured evaluation
envelope, the planner must choose a batch or single-CAS realization strategy,
or refuse to plan the request set for release. A request set can be
identity-valid but not materializable under the accepted cost envelope.

The planner must also record the effective determinism tier and the component
that determined it. A release-planned request set that mixes exact coordinates
with stable, bounded, or exploratory tokenizer profiles, recipes, model
weights, or auxiliary inputs must either be rejected for exact release planning
or explicitly accepted as a non-exact request set. Lowest-tier-wins is a
classification rule, not permission to silently downgrade a release claim.

## Analysis Packs

An analysis pack is a generated collection-level query package over realized
slice manifests and sidecars.

Packs are not the canonical per-work conflict boundary. They are rebuildable
views over:

- request-set identity,
- per-work manifests,
- per-work sidecar content hashes,
- pack output format schema,
- pack policy.

Pack identity is:

```text
pack_id = sha256(RFC8785-JCS(pack_identity_object))
```

`pack_identity_object` contains:

- `request_set_id`,
- `pack_policy_hash`,
- `pack_output_format_spec_hash`,
- `artifact_set_hash`,
- `missing_policy_hash`,
- `schema_hashes`.

`artifact_set_hash` is the JCS SHA-256 hash of sorted artifact references. Each
artifact reference contains:

- `artifact_id`,
- `artifact_kind`,
- `validation_status`,
- `manifest_content_hash`,
- `content_hash` or null for failure/skipped manifests,
- sidecar role when applicable.

Artifact references sort by `artifact_id`, then `artifact_kind`, then sidecar
role with null treated as the empty string for comparison only.

Pack run id, local output directory, batch size, generated time, operator, and
absolute paths are provenance or layout metadata. They do not participate in
pack identity unless the pack output bytes include them.

Because `artifact_set_hash` includes content hashes, pack identity is
content-coupled and therefore more volatile than request-set identity. A
single changed per-work output changes the pack id even when the requested
coordinate set is unchanged. This is intentional for release packs: a pack id
identifies the exact content set, not merely the work/recipe/profile selection.

### Pack Policy Value

`pack_policy_hash` is the JCS SHA-256 hash of a canonical pack policy JSON
value.

The minimal pack policy schema records:

- `schema_id`,
- `policy_id`,
- `pack_kind`, one of `none`, `parquet-pack-v1`, or `archive-index-v1`,
- included artifact kinds,
- included sidecar roles,
- pack index output-format spec hash,
- metric table output-format spec hashes,
- locator policy,
- compression policy,
- row group policy for tabular packs,
- missing-artifact behavior.

The `none` policy is a real value, not an absent policy. It sets
`pack_kind = "none"`, has empty included artifact/sidecar arrays, empty metric
table specs, and a pack index output-format spec hash for the no-pack
descriptor. Arrays sort lexicographically before hashing unless the pack
policy schema defines a more specific key.

### Pack Table Layout

The first pack table format is Parquet. The Parquet column list and column
types are themselves an output-format spec. Its JCS hash is the
`pack_output_format_spec_hash`.

Hash-valued columns use the same `sha256:<hex>` UTF-8 string representation as
manifest identity fields. Nullability must be declared in the output-format
spec, not inferred by readers.

A minimal pack index table includes:

- `request_set_id`,
- `pack_id`,
- `artifact_id`,
- `artifact_kind`,
- `validation_status`,
- `content_hash`,
- `manifest_content_hash`,
- `source_id`,
- `work_id`,
- `work_content_hash`,
- `metadata_record_hash`,
- `input_view_kind`,
- `tokenizer_profile_hash`,
- `analysis_recipe_hash`,
- `output_format_spec_hash`,
- `sidecar_role`,
- `sidecar_hash`,
- `relative_path` or archive/member locator,
- `warning_count`,
- `diagnostic_codes`.

Metric-specific tables may be added only when their schemas have their own
output-format spec hashes.

## Nix Materialization

Nix is the pinned materialization backend, not the analysis planner.

The request-set resolver and materialization planner run outside Nix
evaluation or as a bounded tool execution. Nix expressions must not parse all
producer manifests during evaluation to fabricate child identities.

Producer identity fields are copied by materializer tools at build time. Slice
derivations take named input store paths:

- producer artifact path,
- recipe/profile object path,
- schema path,
- policy path,
- auxiliary artifact paths when the recipe explicitly requires them.

Per-work derivations must not take the full corpus snapshot store path as a
direct input. Source snapshots are inputs to source and parser projection
tiers, which produce per-work or batch producer artifacts. Downstream
tokenization and analysis consume those bounded producer artifacts.

Recipe and tokenizer-profile registries must be realized as per-hash store
paths. A derivation must take the specific recipe/profile path selected by the
request set, not a mutable registry directory whose path changes whenever an
unrelated recipe is appended.

For example, adding `literary-experimental-ja-v1` to a registry must not change
the store path or cache key for a derivation that uses only
`literary-basic-ja-v1`. Passing a whole registry directory to every slice would
couple every historical build to unrelated registry growth and defeat
incremental cache reuse.

### Determinism and Cache Policy

Effective determinism tier is the lowest tier across:

- producer artifact,
- tokenizer profile,
- analysis recipe,
- model weights,
- auxiliary corpus-statistics artifacts,
- output writer.

The materialization policy is:

| Effective tier | Nix derivation kind | Substitution policy |
|---|---|---|
| `exact` | fixed-output where the output hash is known or verified by replay | content-addressed cache; may be substituted under normal signature policy |
| `stable` or `bounded` | input-addressed | trusted signed binary cache only |
| `exploratory` | input-addressed | local only; not published to binary caches |

Binary caches and Nix substituters are content-hash keyed, not artifact-id
keyed. If two artifact ids cite the same content hash, they are intentionally
cache-equivalent. Release reviewers must audit content-hash provenance because
one compromised store path affects every artifact id that cites that content.

### Batch Failure Model

Batch derivations must run all known members and emit per-work success,
failure, or skipped manifests into the output tree.

A per-work analysis/tokenization failure is represented as a failure manifest,
not as a failed Nix derivation. The batch derivation fails only when the
batch-level contract is unmet: malformed output tree, missing intended
coordinates, schema-invalid manifests, non-deterministic output, or tool crash
that prevents per-work attribution.

This keeps ADR 0003's failure-attribution requirement while still allowing
batch derivations to amortize evaluator overhead.

It also makes failures ordinary values that can be indexed, cited, counted,
and compared across releases. The alternative, failing the whole derivation on
the first per-work failure, discards successful sibling work and hides
attribution inside transient build logs.

## Run Id Bridge

Warehouse and materializer run ids are operational locators. They are not
publication identity.

Any run that produces canonical slices or packs must emit a run-provenance
record that maps:

- run id,
- request-set id,
- pack ids,
- produced artifact ids,
- failed/skipped artifact ids,
- tool versions and environment summary.

The run-provenance record is a lookup bridge from operational logs to
canonical hashes. It is not part of slice, request-set, or pack identity unless
an output format explicitly embeds it.

## Consequences

ABC gets a release path for tokenizer-backed stylometry without making a run
directory, warehouse table, or Nix attrset the canonical identity.

The design intentionally adds a manifest schema blocker for exact tokenized
artifacts. This is cheaper than publishing tokenized outputs under an identity
that omits normalization, granularity, or profile configuration.

Analysis packs become useful for DuckDB/Parquet scans while remaining
rebuildable views over per-work manifests. Per-work manifests remain the
reproducibility conflict boundary.

Batch materialization can be used at full-corpus scale without losing per-work
failure attribution, but Nix no longer sees ordinary per-work analysis failures
as derivation failures.

Snapshot-publication work can continue with token-independent analysis and can
include empty tokenizer-profile arrays. It must not promote tokenizer outputs
or tokenizer-backed metrics into the public snapshot promise before this ADR's
schema and validation work lands.

## Acceptance Criteria

- A `tokenizer-profile` schema exists and can be hashed with the ADR 0001 JCS
  schema-hash discipline.
- A manifest schema revision adds `tokenizer_profile_hash` to
  `manifest_identity_object`.
- Manifest sidecar roles include `token-stream`.
- Until that manifest schema revision lands, release validation rejects any
  successful `tokenized` manifest; `test/abc/tools/manifest_index_test.clj`
  covers this pre-schema guardrail.
- Snapshot request-set fixtures remain token-independent until canonical
  tokenizer profiles exist; `test/abc/tools/request_set_fixture_test.clj`
  requires empty `tokenizer_profile_hashes` arrays in those fixtures.
- An accepted token output schema exists before any canonical tokenized slice
  is accepted; its hash is used as the tokenized slice
  `output_format_spec_hash`.
- A tokenized fixture demonstrates `parser-ir-plaintext-body-v1` input,
  tokenizer profile hash, token output schema hash, and token index/input span
  coordinates.
- Release validation fails when copied parser/mapping identity fields in a
  tokenized manifest differ from the producer manifest.
- Release validation fails when tokenized manifest build/dictionary fields do
  not match the tokenizer profile content.
- A tokenizer-backed analysis fixture consumes a tokenized producer manifest
  via `token-stream-v1`.
- Request-set resolver tests cover semantic profile label resolution to
  content hash and registry-entry hash.
- Request-set planning estimates realized coordinate count and records whether
  the chosen realization strategy is per-work, batch, or single-CAS.
- Batch derivation tests show per-work failure manifests are emitted while the
  batch derivation succeeds when the batch-level contract is satisfied.
- A pack policy schema exists, including a canonical `none` policy and at
  least one pack-producing policy.
- A pack output-format spec defines Parquet columns and types; its hash is
  recorded as `pack_output_format_spec_hash`.
- A pack fixture records `pack_id`, `request_set_id`, `artifact_set_hash`, and
  artifact references sorted by the rule in this ADR.
- Pack validation detects same `artifact_id` with different content hashes and
  permits same `content_hash` under different artifact ids.
- Nix wiring passes per-hash recipe/profile paths and producer artifact paths;
  it does not pass registry directories or the full corpus snapshot path to
  per-work tokenization or analysis derivations.
- A run-provenance fixture maps a run id to request-set id, pack ids, produced
  artifact ids, and failed/skipped artifact ids.

## Deferred Decisions

- Which tokenizer profile is first accepted for public Japanese tokenization.
- Exact token record columns beyond token index and input span requirements.
- Exact tokenizer-backed metric formulas.
- Whether public research-data packaging uses RO-Crate, Frictionless Data
  Package, another descriptor format, or only the Soranoha snapshot index.
- Formal signing and in-toto/Sigstore attestation policy for public packs.
- Whether corpus-wide derived Parquet and DuckDB files are shipped in the first
  public snapshot or generated locally on demand.

## Rollback

If tokenizer profile identity proves insufficient, publish no canonical
tokenized artifacts under this ADR. Supersede this ADR and introduce a new
manifest schema hash or tokenizer-profile schema hash.

Do not reinterpret tokenized or tokenizer-backed analysis prototypes emitted
before the manifest schema includes `tokenizer_profile_hash` as exact release
artifacts.
