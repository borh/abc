# Ruby Annotation View Design — Settling ADR 0028's Open Questions

Date: 2026-07-09
Status: settled design, pending ratification at ADR 0028 Proposed → Accepted
Evidence: `docs/handoffs/ruby-annotation-probe-2026-07-09.md`
Implements toward: `docs/adr/0028-ruby-annotation-view.md`

Each decision below records the choice, the reason, and what would flip it.
"Settled" means the implementation plan builds on it now; ratification happens
when ADR 0028 is accepted.

## D1. Identity field: new nullable `annotation_policy_hash`

**Decision.** Manifest schema v0.4.4 adds `annotation_policy_hash` (nullable
hash) to `manifest_identity_object`, required-with-null like every other
coordinate. It is non-null exactly for `artifact_kind = "annotation"`
manifests.

**Why.** Two annotation policies over the same work must not collide on
`artifact_id`. Folding the policy into `output_format_spec_hash` conflates
"what the output looks like" with "what was projected", and the family
precedent (v0.4.2 adding `tokenizer_profile_hash`) already establishes the
cost and mechanics of an identity-field addition: schema-hash rotation,
fixture updates, and the ab-validator importer pin bump.

**Flips if.** A manifest-schema freeze lands before implementation, in which
case the fallback is a distinct output-format spec per policy (accepted as
worse but identity-sound).

## D2. Naming

**Decision.** `artifact_kind = "annotation"`; sidecar role
`body-annotations`; consumer input view kind
`parser-ir-body-annotations-v1`; per-record `annotation_kind` values in the
first slice: `ruby`, `gaiji`.

**Why.** Kind names stay family-generic (ADR 0028's generalization section);
record-level `annotation_kind` carries the specialization, so ML/LLM layers
(`ner`, `speaker`, …) are new record kinds or new policy values, not new
manifest machinery.

## D3. Gaiji records: included

**Decision.** The view emits records for all gaiji nodes, resolved and
unresolved, carrying `raw_marker`, `unicode`, `resolved`, and the plaintext
span (empty span allowed when the node contributed no visible text).

**Why.** Recipes need to count/exclude gaiji regions (ADR 0027's mandatory
"treatment of gaiji" recipe clause), and the marginal cost is one more
renderer branch in the same traversal.

## D4. `ruby.scope`: withheld from v0.1.0

**Decision.** The annotation output schema does not carry `ruby.scope`.
`direction` is carried (schema-backed and measured, ADR 0024). Scope returns
when AAT supplies producer-measured scope (mapping invention I-01 retires).

**Why.** Scope is an ABC mapping invention. Publishing invented values in an
analysis-facing contract launders them into downstream results.

**Flips if.** A concrete recipe demonstrates it cannot classify joins without
scope — the probe's classification needed only spans, so this is unlikely.

## D5. Alignment production: instrument the plaintext renderer's traversal

**Decision.** `abc.tools.parser-ir-plaintext` gains offset tracking in its
existing reduce accumulator and a new entry point returning
`{:text :annotations :node_counts :omitted}`. `render`/`render-string`
outputs stay byte-identical (guarded by existing tests and committed fixture
hashes).

**Why.** One traversal cannot drift from itself. A separate re-derivation
transform would need byte-equality validation against the renderer forever —
that validation *is* the argument for sharing the traversal.

**Non-negotiable detail.** Offsets are **unicode scalar values** (ADR 0027
token coordinate family), not Java UTF-16 units: track with
`String#codePointCount`, and test with an astral-plane gaiji codepoint.

## D6. Request sets: annotation views participate as input views

**Decision.** Annotation views appear in request-set `input_views` as
`{"input_view_kind": "parser-ir-body-annotations-v1", "policy_hash": …}`.
No resolver code change; add a fixture.

*(First slice shipped the demonstration fixture only; the widening landed
2026-07-10: schema v0.1.4 `oneOf`, `allowed-input-view-kinds`, and the
resolved `demo-annotation-ja` fixture.)*

## D7. Span survival under text-rewriting normalization (M2)

**Decision (deferred to a tokenizer-profile schema rev, recorded here).** The
tokenizer-profile schema gains `span_preservation` ∈
`{"preserving", "offset-map", "none"}`. Profiles whose input normalization
rewrites text (e.g. warehouse M2 old-kana modernization) must declare
`offset-map` and emit one, or `none` — and `none` profiles cannot support
annotation joins or Exact tier with input spans. Not in the first slice; the
ruby view itself is unaffected (it anchors to the plaintext view directly).

## D8. One identity slot for the family

**Decision.** `annotation_policy_hash` serves both source-projection policies
(ruby/gaiji, this slice) and future annotator profiles (ML/LLM): both are
hash-addressed canonical JSON values with binding-log semantic ids.
`annotation_kind` lives in policy and content, not in manifest identity.

**Why.** Mirrors `tokenizer_profile_hash`: one coordinate, profile content
carries the detail. Distinct fields per annotator family would grow the
identity object per model type — exactly the Cartesian smell ADR 0026 avoids.

## D9. Provenance floor for `exploratory` annotators

**Decision (guidance, deferred).** When a model annotator ADR lands, its
profile must record at minimum: API/model identifier and version string,
full request-parameter hash, prompt template hash, response capture hash, and
capture timestamp. Recorded here so the first slice's policy schema leaves
room (policies are open maps under a versioned schema); enforcement belongs
to that future ADR.

## Consequences for the first slice

Implementation order and mechanics: see
`docs/superpowers/plans/2026-07-09-ruby-annotation-view-implementation.md`.
The slice adds: annotation output schema v0.1.0 + ruby-gaiji-v1 policy value,
manifest schema v0.4.4 (D1, D2), renderer instrumentation (D5), an
`annotation` materializer mirroring `abc.tools.materialize-tokenized`,
manifest-index copied-field validation, a span-intersection join with the
four probe classifications, and design-bundle wiring.
