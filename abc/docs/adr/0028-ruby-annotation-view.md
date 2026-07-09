# ADR 0028: Ruby Annotation View for the Token and Analysis Chain

Status: Proposed (skeleton — decision shape and evidence recorded; identity
field mapping and schemas not yet drafted)
Date: 2026-07-09
Supersedes: none
Depends on: ADR 0023, ADR 0024, ADR 0025, ADR 0026, ADR 0027
Source: `docs/handoffs/ruby-annotation-probe-2026-07-09.md`

## Implementation Status

Skeleton only. The measurement probe and its evidence note exist
(`prototypes/ruby-annotation-probe/`,
`docs/handoffs/ruby-annotation-probe-2026-07-09.md`). No schema, renderer,
or manifest work has started.

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
2. **Content**: one record per ruby node (and unresolved gaiji node — open
   question below) contributing to the plaintext body:
   - span in plaintext unicode-scalar offsets (the ADR 0027 token coordinate
     family),
   - source span (`decoded_utf8`, ADR 0024),
   - `ruby.base`, `ruby.reading`, `ruby.direction`,
   - `ruby.scope` only if the invented-value question below resolves to
     including it.
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

## Rejected alternatives

- **Sidecar on the plaintext artifact**: couples annotation policy evolution
  to plaintext identity, and plaintext consumers do not want it.
- **TEI-body tokenization input view** and **ruby spans as segmentation
  constraints**: rejected in the amended ADR 0027; the probe evidence lives
  there.
- **Per-(work × tokenizer profile) canonical reading-overlay artifacts**: the
  join is cheap and deterministic; materializing it canonically would
  reintroduce the Cartesian growth ADR 0026/0027 were designed to avoid.

## Open questions (to resolve before Proposed → Accepted)

- **Identity field mapping**: does the annotation policy hash get a new
  nullable `annotation_policy_hash` manifest identity field (family-consistent
  with the v0.4.2 `tokenizer_profile_hash` addition), or fold into
  `output_format_spec_hash`? Two annotation policies over the same work must
  not collide on `artifact_id`.
- **Artifact kind and sidecar role naming**: e.g. `artifact_kind =
  "annotation"` with sidecar role `body-annotations`, input view kind
  `parser-ir-body-annotations-v1` for consumers.
- **Gaiji records**: include unresolved gaiji (raw marker, reference) so
  analysis can count/exclude them, or keep the view ruby-only?
- **`ruby.scope`**: it is an ABC mapping invention (I-01, ADR 0024 deferred
  follow-ups) — publish it in an analysis-facing contract, or withhold until
  AAT supplies producer-measured scope?
- **Alignment production**: instrument the plaintext renderer to emit spans in
  the same traversal (guarantees byte-consistency with the plaintext view), or
  re-derive alignment in a separate pure transform validated against the
  rendered plaintext?
- **Whether the annotation view participates in request-set identity** as an
  input view with its own policy hash (ADR 0026 request-set input-view arrays
  already accommodate this).

## Acceptance criteria (sketch)

- An annotation output schema exists and is hashable under the ADR 0001 JCS
  discipline.
- A fixture materializes one per-work annotation view from the committed
  parser-IR example; spans verified against the committed plaintext fixture
  byte-for-byte (unicode-scalar offsets).
- Copied parser/mapping identity fields validate against the producer
  parser-IR manifest, reusing the ADR 0026 copied-field release validation.
- A joined token/reading fixture demonstrates span-intersection join against
  a `token-stream-v1` sidecar, including at least one stem-prefix and one
  conflict classification (probe data provides real cases).
- A tokenizer-backed analysis recipe fixture states a supplantation policy and
  consumes the join.
- Renderer coverage remains schema-derived and fails closed on new parser-IR
  node types, matching the ADR 0025 discipline.

## Rollback

If the annotation view proves insufficient, supersede this ADR and introduce a
new annotation output schema hash. Do not reinterpret annotation artifacts
emitted under this rule, and do not retrofit annotation data into plaintext or
TEI artifact identity.
