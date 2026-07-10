# ADR 0028: Ruby Annotation View for the Token and Analysis Chain

Status: Proposed
Date: 2026-07-09
Supersedes: none
Depends on: ADR 0023, ADR 0024, ADR 0025, ADR 0026, ADR 0027
Source: `docs/handoffs/ruby-annotation-probe-2026-07-09.md`, `docs/superpowers/specs/2026-07-09-ruby-annotation-view-design.md`

## Implementation Status

First slice implemented (2026-07-09/10). The measurement probe and its
evidence note exist (`prototypes/ruby-annotation-probe/`,
`docs/handoffs/ruby-annotation-probe-2026-07-09.md`), and the design decisions
in `docs/superpowers/specs/2026-07-09-ruby-annotation-view-design.md` are
built:

- **Annotation output schema v0.1.0** (`schemas/annotation-output.schema.json`)
  and the first policy value, `data/annotation-policies/ruby-gaiji-v1.json`
  (D2, D3, D4). `abc.tools.analysis-identity/annotation-policy-hash` hashes
  policy content under the ADR 0001 JCS discipline; covered by
  `test/abc/tools/annotation_identity_test.clj`.
- **Manifest schema v0.4.4** (`schemas/manifest.schema.json`, mirrored to the
  ab-validator pin at `ab-validator/data/abc-schemas/nix-schemas/manifest.schema.json`
  and `schema-contracts.json`): `artifact_kind = "annotation"`, sidecar role
  `body-annotations`, and a required-nullable `annotation_policy_hash` field on
  every `manifest_identity_object` (D1, D2, D8).
- **Renderer instrumentation**:
  `abc.tools.parser-ir-plaintext/render-with-annotations` adds unicode-scalar
  span tracking to the existing single-traversal reduce accumulator;
  `render`/`render-string` remain byte-identical (D5). Covered by
  `test/abc/tools/parser_ir_annotations_test.clj` (including an astral-plane
  gaiji codepoint case).
- **Materializer**: `abc.tools.materialize-annotations/materialize-annotations!`
  copies producer parser-IR identity fields into the annotation manifest, the
  ADR 0026 consumer pattern. Covered by
  `test/abc/tools/materialize_annotations_test.clj`.
- **Manifest-index validators**: `abc.tools.manifest-index/annotation-copied-field-errors`
  / `validate-annotation-copied-fields!` and
  `annotation-release-guardrail-errors` / `validate-annotation-release-guardrail!`,
  extending `test/abc/tools/manifest_index_test.clj`.
- **Token join**: `abc.tools.annotation-join/join` performs the
  span-intersection join against a token stream, producing the four probe
  classifications (`aligned-single`, `aligned-multi`, `stem-prefix`,
  `conflict`). Covered by `test/abc/tools/annotation_join_test.clj`. This is a
  generated view, not a canonical artifact (Decision item 4).
- **Design-bundle wiring**: `abc.tools.validate-design-bundle` materializes the
  annotation fixture, schema-validates it, and runs it through the
  manifest-index checks above, against committed example outputs
  (`examples/v0/example-work/body-annotations.json`).
- **D6 widened (2026-07-10)**: `abc.tools.analysis-identity/allowed-input-view-kinds`
  now includes `parser-ir-body-annotations-v1` alongside
  `parser-ir-plaintext-body-v1`; request-set schema v0.1.4's `$defs.inputView`
  is a `oneOf` over the plaintext three-key variant and the D6 annotation
  two-key variant (`{input_view_kind, policy_hash}`); and
  `data/request-sets/demo-annotation-ja.json` is a resolved fixture carrying
  both an annotation and a plaintext input view. Covered by
  `test/abc/tools/request_set_fixture_test.clj`: the machine-check
  `input-view-kinds-schema-and-allow-list-agree-test` binds the schema's
  `oneOf` kinds to the allow-list, and
  `annotation-input-view-resolves-in-demo-annotation-ja-test` resolves the
  fixture end-to-end. A request set can now actually resolve an annotation
  input view, and `abc.tools.analysis-identity/assert-input-view-coverage!`
  (enforced by the resolver, pinned fixture-wide by
  `definitions-and-recipes-input-view-coverage-machine-check-test` in
  `test/abc/tools/request_set_fixture_test.clj`) machine-checks the
  recipe-`supported_input_view_kinds` ↔ request-set-views agreement:
  every referenced recipe must be able to consume at least one view the
  request set provides (tokenizer profiles supply the derived
  `token-stream-v1` view). The converse — flagging a declared view no
  recipe consumes — is deliberately not checked: views are consumed
  outside the recipe system too (the publication flow consumes the
  plaintext view, as in `full-corpus-publication-basic-ja`; the annotation
  materializer consumes annotation views directly), so dead-view detection
  is not decidable from the recipe registry. Materializing annotation artifacts *per request set* landed 2026-07-10:
  `abc.tools.soranoha/materialize-snapshot-root!` resolves the annotation
  view once per run
  (`abc.tools.materialize-annotations/resolve-annotation-materialization`,
  fail-closed on unknown/duplicate policy hashes and on anything but exactly
  one aligned plaintext view), materializes per-work
  `annotations/body-annotations.json` + `annotation.manifest.json`, includes
  them in `snapshot-index.json` (snapshot-index schema v0.1.1 admits the
  `annotation` kind), and runs the annotation release guardrails in the
  batch path (`abc.tools.soranoha/validate-annotation-manifests!`). Covered
  by `test/abc/tools/soranoha_annotation_test.clj` and
  `test/abc/tools/materialize_annotations_test.clj`. Corpus-scale join
  statistics tooling exists (`soranoha annotation-join-stats`,
  `test/abc/tools/annotation_join_stats_test.clj`); the corpus run and its
  D7/D9 evidence are pending
  (`docs/superpowers/specs/2026-07-10-per-request-set-annotation-materialization-design.md`).
  Still future work: widening
  `schemas/analysis-recipe.schema.json`'s `supported_input_view_kinds` enum
  so a recipe can declare annotation-view consumption — that widening
  belongs with the first annotation-consuming recipe, because the schema's
  `tokenizer_required` conditionals (plaintext-policy/newline coupling)
  need a design decision for annotation-only recipes.
- **Schema-contracts registration**: `schemas/annotation-output.schema.json`
  is registered in `schemas/schema-contracts.json` and the monorepo-root
  `scripts/abc_schema_contracts.py`'s single `SCHEMA_FILES`, the deferred
  registration from the first slice, and also in the ab-validator vendored
  contracts (`ab-validator/data/abc-schemas/schema-contracts.json`),
  generated via that same `scripts/abc_schema_contracts.py`.

## Context

`parser-ir-plaintext-body-v1` keeps ruby base text and resolved gaiji but
drops ruby readings, span boundaries, and direction. The plaintext view is
correct as-is: it exists for consumers who want plaintext, and TEI remains the
information-rich publication view with native ruby. Neither view serves the
token/analysis chain's need for author readings.

The ruby-annotation probe measured what that costs and what fixes it:

- Author readings are gold annotation data: 31.3% differ substantively from
  the best-fitting period dictionary (`unidic-novel`) even after historical
  kana normalization — gikun, foreign-word glosses, name readings, lexical
  reading choices. No tokenizer output reconstructs them.
- Ruby spans are not word boundaries: 32.2% straddle a token edge (mostly
  stem ruby, prefix-consistent with the tokenizer reading), so ruby must not
  constrain segmentation.
- Joining works without tokenizer cooperation: ruby spans and token spans
  expressed in the same plaintext unicode-scalar coordinate system join
  deterministically by span intersection.

ADR 0027 (as amended) rejects TEI-body tokenization and ruby-constrained
tokenization, and defers ruby annotation to this ADR.

## Decision (proposed)

ABC will materialize a per-work, tokenizer-independent **ruby annotation
view** over the same parser-IR producer and plaintext projection the
tokenization chain already uses.

1. **The annotation view is a sibling projection, not a change to plaintext
   or TEI.** The plaintext renderer's output bytes, policy hash, and existing
   artifacts are untouched. ADR 0025 is not amended.
2. **Content**: one record per ruby node and per gaiji node (resolved and
   unresolved — settled as D3 below) contributing to the plaintext body:
   - span in plaintext unicode-scalar offsets (the ADR 0027 token coordinate
     family),
   - source span (`decoded_utf8`, ADR 0024),
   - `ruby.base`, `ruby.reading`, `ruby.direction`; `ruby.scope` is withheld
     (settled as D4 below),
   - gaiji records carry `raw_marker`, `unicode`, `resolved` (D3).
3. **Identity** follows the ADR 0026 consumer pattern: subject fields and
   parser/mapping fields copied exactly from the producer parser-IR manifest;
   an annotation policy hash identifying the projection rules and the
   plaintext policy the spans align to; `output_format_spec_hash` identifying
   the annotation output schema. Tokenizer and recipe fields are null.
4. **The token join is a generated view, not a canonical artifact.** A
   reading-supplanted token table is rebuildable by span intersection from
   (annotation view, token stream); it belongs in analysis recipes and pack
   tables, not in per-work manifest identity. This avoids multiplying
   canonical artifacts by tokenizer profile.
5. **Reading supplantation is recipe-controlled and optional.** A
   tokenizer-backed analysis recipe that consumes the annotation view must
   state (per the amended ADR 0027 recipe rules) whether author readings
   supplant tokenizer readings, and how join classifications are handled:
   `aligned-single`, `aligned-multi`, `stem-prefix`, `conflict`.
6. **Naming and contracts must not hardcode ruby.** The ruby view is the
   first instance of a general annotation-view family (see Generalization
   below). Artifact kind, sidecar role, coordinate contract, and identity
   shape are chosen for the family; ruby is one `annotation_kind` within it.

### Settled: identity, naming, and mechanics (formerly open questions D1-D6, D8)

The open questions below were settled in
`docs/superpowers/specs/2026-07-09-ruby-annotation-view-design.md` (D1-D9) and
built in the first slice. D7 and D9 remain deferred (see Open questions
below); the rest are decided:

- **D1 — Identity field mapping.** New nullable `annotation_policy_hash` field
  on `manifest_identity_object` (manifest schema v0.4.4), non-null exactly for
  `artifact_kind = "annotation"`. *Why*: folding the policy into
  `output_format_spec_hash` would conflate "what the output looks like" with
  "what was projected"; the v0.4.2 `tokenizer_profile_hash` addition already
  established the mechanics of an identity-field addition (schema-hash
  rotation, fixture updates, ab-validator pin bump). Two annotation policies
  over the same work must not collide on `artifact_id`, and now cannot.
- **D2 — Naming.** `artifact_kind = "annotation"`; sidecar role
  `body-annotations`; consumer input view kind
  `parser-ir-body-annotations-v1`; per-record `annotation_kind` values in the
  first slice: `ruby`, `gaiji`. *Why*: kind names stay family-generic; the
  per-record `annotation_kind` carries specialization, so future ML/LLM layers
  (`ner`, `speaker`, ...) are new record kinds or policy values, not new
  manifest machinery.
- **D3 — Gaiji records: included.** The view emits records for all gaiji
  nodes, resolved and unresolved, carrying `raw_marker`, `unicode`,
  `resolved`, and the plaintext span (empty span allowed when the node
  contributed no visible text). *Why*: recipes need to count/exclude gaiji
  regions (ADR 0027's mandatory gaiji-treatment recipe clause); the marginal
  renderer cost is one more branch in the same traversal. This also gives the
  schema a second, non-ruby `annotation_kind` in the first slice.
  `ruby.scope` (a separate, ABC-invented field) is withheld — see D4.
- **D4 — `ruby.scope`: withheld from v0.1.0.** The annotation output schema
  does not carry `ruby.scope`; `ruby.direction` is carried (schema-backed and
  measured, ADR 0024). *Why*: scope is an ABC mapping invention (I-01, ADR
  0024 deferred follow-ups); publishing invented values in an analysis-facing
  contract would launder them into downstream results. Returns when AAT
  supplies producer-measured scope.
- **D5 — Alignment production: instrument the plaintext renderer's
  traversal.** `abc.tools.parser-ir-plaintext/render-with-annotations` adds
  offset tracking to the existing reduce accumulator rather than re-deriving
  spans in a separate transform. *Why*: one traversal cannot drift from
  itself; a separate re-derivation would need permanent byte-equality
  validation against the renderer — which is the argument for sharing the
  traversal instead. Offsets are unicode scalar values (`codePointCount`), not
  UTF-16 units, tested with an astral-plane gaiji codepoint.
- **D6 — Request sets: annotation views participate as input views.**
  Annotation views appear in request-set `input_views` as
  `{"input_view_kind": "parser-ir-body-annotations-v1", "policy_hash": …}`.
  Widened 2026-07-10: the resolver allow-list and `request-set.schema.json`'s
  `inputView` `oneOf` now admit the annotation kind, and
  `data/request-sets/demo-annotation-ja.json` is a resolved fixture (see
  Implementation Status).
- **D8 — One identity slot for the family.** `annotation_policy_hash` serves
  both source-projection policies (ruby/gaiji, this slice) and future
  annotator profiles (ML/LLM) alike: both are hash-addressed canonical JSON
  values with binding-log semantic ids. `annotation_kind` lives in policy and
  content, not in manifest identity. *Why*: mirrors `tokenizer_profile_hash`
  — one coordinate, profile content carries the detail — avoiding the
  Cartesian growth of a distinct identity field per annotator family.

## Generalization: annotation views beyond ruby

Reaching TEI-EAJ `aozora_tei` Levels 4 and 5 will require annotations that
are not producer-preserved source structure: named entities, speaker and
dialogue attribution, quotation structure, dates/places, and similar layers
produced by ML or LLM models. The annotation-view contract is designed so
those layers are additional instances of the same shape, not a new design:

- **Same coordinate and join contract.** Every annotation view records spans
  in the plaintext unicode-scalar coordinate system (and source spans where
  derivable), and joins to token streams or other annotation views by span
  intersection. Model annotators consume the plaintext view; they do not get
  bespoke input paths.
- **Annotator profile identity.** Model-derived annotation views replace the
  trivial source-projection policy with a hash-addressed **annotator
  profile**, following the ADR 0027 tokenizer-profile discipline: model
  name/version, weights hash, configuration/prompt hash, output schema hash,
  determinism tier, and fixture evidence hash, with semantic ids bound
  through the same append-only binding-log rule. The identity slot this ADR
  introduces for the ruby projection policy is the same slot an annotator
  profile hash occupies.
- **Determinism tiers apply per ADR 0027's table.** The ruby view is
  source-derived and `exact`. Locally pinned model inference is at best
  `stable`/`bounded` (trusted signed cache only). Remote LLM API outputs are
  `exploratory`: local only, never substitutable, and never part of an exact
  release claim. Lowest-tier-wins already governs anything downstream that
  consumes them.
- **TEI Levels 4/5 are a rendering consequence, not a separate pipeline.** A
  future publication renderer (superseding or extending ADR 0025's contract)
  renders inline TEI from parser-IR plus selected annotation views as
  standoff inputs, attributing model-derived markup per annotator profile
  (TEI `@resp`/certainty). That renderer decision is out of scope here; this
  ADR's job is that annotation artifacts carry the identity and coordinates
  such a renderer will need.

## Rejected alternatives

- **Sidecar on the plaintext artifact**: couples annotation policy evolution
  to plaintext identity, and plaintext consumers do not want it.
- **TEI-body tokenization input view** and **ruby spans as segmentation
  constraints**: rejected in the amended ADR 0027; the probe evidence lives
  there.
- **Per-(work × tokenizer profile) canonical reading-overlay artifacts**: the
  join is cheap and deterministic; materializing it canonically would
  reintroduce the Cartesian growth ADR 0026/0027 were designed to avoid.

## Open questions (deferred; remaining before Proposed → Accepted)

Of the nine open questions originally listed here, seven are settled — see
"Settled: identity, naming, and mechanics" above (identity field mapping =
D1, artifact-kind/sidecar naming = D2, gaiji records = D3, `ruby.scope` = D4,
alignment production = D5, request-set participation = D6, identity-slot
naming for the generalized family = D8). Two remain open, per
`docs/superpowers/specs/2026-07-09-ruby-annotation-view-design.md` D7 and D9:

- **D7 — Span survival under text-rewriting normalization.** If a tokenizer
  or annotator profile's input normalization rewrites surface text (e.g. the
  morphology warehouse's M2 old-kana modernization), token spans refer to the
  normalized text, not the plaintext view the annotation spans anchor to.
  **Deferred decision** (recorded in the design spec, not yet built): the
  tokenizer-profile schema will gain `span_preservation` ∈
  `{"preserving", "offset-map", "none"}`; profiles that rewrite text must
  declare `offset-map` and emit one, or declare `none` and thereby forfeit
  annotation-join and Exact-tier-with-input-spans support. Not in the first
  slice — the ruby view itself is unaffected because it anchors to the
  plaintext view directly, not to any tokenizer's normalized text. Tracked as
  a future tokenizer-profile schema revision.
- **D9 — Model provenance floor for `exploratory` annotators** (remote LLM
  APIs): minimum recorded identity (API model id, request parameters, prompt
  hash, response capture) for a value that can never be exactly replayed, and
  how such artifacts are marked so release validation excludes them from
  exact claims. **Deferred guidance** (recorded in the design spec, not
  enforced by anything in this slice): a future model-annotator ADR must
  record at minimum API/model identifier and version, full request-parameter
  hash, prompt-template hash, response-capture hash, and capture timestamp.
  The first slice's policy schema is an open map under a versioned schema, so
  it leaves room for this without needing a schema change now; enforcement
  belongs to that future ADR.

## Acceptance Criteria

Status per criterion, first slice (Tasks 1-7):

- **Done.** An annotation output schema exists and is hashable under the
  ADR 0001 JCS discipline: `schemas/annotation-output.schema.json`,
  `abc.tools.analysis-identity/annotation-policy-hash`, verified by
  `test/abc/tools/annotation_identity_test.clj`.
- **Done.** A fixture materializes one per-work annotation view from the
  committed parser-IR example; spans verified against the committed
  plaintext fixture byte-for-byte (unicode-scalar offsets):
  `test/abc/tools/parser_ir_annotations_test.clj`,
  `test/abc/tools/materialize_annotations_test.clj`, and the design-bundle
  fixture wiring in `abc.tools.validate-design-bundle` against
  `examples/v0/example-work/body-annotations.json`.
- **Done.** Copied parser/mapping identity fields validate against the
  producer parser-IR manifest, reusing the ADR 0026 copied-field release
  validation: `abc.tools.manifest-index/annotation-copied-field-errors` and
  `annotation-release-guardrail-errors`, exercised in
  `test/abc/tools/manifest_index_test.clj`.
- **Done.** A joined token/reading fixture demonstrates span-intersection
  join against a token stream, including at least one stem-prefix and one
  conflict classification (probe data provides real cases):
  `abc.tools.annotation-join/join`, `test/abc/tools/annotation_join_test.clj`.
- **Not yet built.** A tokenizer-backed analysis recipe fixture that states a
  supplantation policy and consumes the join. The join primitive exists
  (above), and the recipe/request-set input-view coverage machine-check is
  in place (`abc.tools.analysis-identity/assert-input-view-coverage!`,
  `test/abc/tools/analysis_identity_test.clj`,
  `test/abc/tools/request_set_fixture_test.clj`) so such a recipe cannot be
  referenced by a request set that fails to feed it; no recipe schema or
  fixture consumes the join yet. Deferred to a later task.
- **Done (2026-07-10).** Annotation artifacts materialize per request set
  through the batch loop with release guardrails, and join-statistics
  tooling is fixture-tested: `test/abc/tools/soranoha_annotation_test.clj`,
  `test/abc/tools/annotation_join_stats_test.clj`.
- **Unchanged, inherited.** Renderer coverage remains schema-derived and
  fails closed on new parser-IR node types, matching the ADR 0025
  discipline: `render-with-annotations` shares the same `node-renderers` /
  `covered-node-types` tables as `render`, validated by the pre-existing
  `test/abc/tools/parser_ir_plaintext_test.clj`.
- **Done.** The annotation output schema demonstrates the family shape with
  at least one non-ruby `annotation_kind` fixture: the schema and the first
  policy (`data/annotation-policies/ruby-gaiji-v1.json`) carry two
  `annotation_kind` values, `ruby` and `gaiji` (D3), exercised together in
  `test/abc/tools/parser_ir_annotations_test.clj` and
  `test/abc/tools/materialize_annotations_test.clj`. Both kinds still derive
  from the same source-projection policy; a fully independent ML/LLM
  `annotation_kind` (e.g. `ner`) remains future work under D8/D9.
- **Done.** D6 (request-set participation): `allowed-input-view-kinds` and
  `request-set.schema.json`'s `inputView` `oneOf` both admit
  `parser-ir-body-annotations-v1`, and `data/request-sets/demo-annotation-ja.json`
  is a resolved fixture carrying the `{input_view_kind, policy_hash}` shape
  alongside a plaintext view: `test/abc/tools/request_set_fixture_test.clj`
  (`input-view-kinds-schema-and-allow-list-agree-test`,
  `annotation-input-view-resolves-in-demo-annotation-ja-test`). Materializing
  annotation artifacts per request set is not built in this slice — only the
  identity/schema layer accepts the view.

## Rollback

If the annotation view proves insufficient, supersede this ADR and introduce a
new annotation output schema hash. Do not reinterpret annotation artifacts
emitted under this rule, and do not retrofit annotation data into plaintext or
TEI artifact identity.
