# P4B Parser Predicate Hardening Design

**Date:** 2026-07-17
**Status:** Approved design
**Parent:** `2026-07-15-parser-release-qualification-campaign-design.md`

## Purpose

P4B implements the last two missing predicate instruments before P5 may pin a
release candidate and perform the authoritative corpus capture:

- predicate 4, diagnostic-envelope completeness; and
- predicate 5, Parser-IR schema conformance.

P4B proves bounded instruments. It does not pin the final candidate, recapture
the release bundle, admit a compatibility row, or promote ADR 0039.

## Grounded constraints

The live predicate set already fixes both dimensions and observed keys:

- `:diagnostic_completeness = 1.0`, produced from the `ab-aozora` schema-v3
  diagnostic envelope; and
- `:parser_ir_schema_validation = 1.0`, produced by
  `ab-aat-to-parser-ir` using `validate_output_parser_ir`.

The predicate set therefore does not rotate in P4B. Instrument implementation
identity belongs in the qualification identity's `instrument_versions` value,
not in a rewritten predicate. Any later semantic predicate change requires its
own governance decision and full recapture.

The existing raw diagnostic schema requires every entry to carry `kind`,
`code`, `severity`, `source`, and `span`. Predicate 4 measures conformance to
that pinned contract. It does not measure diagnostic recall and does not consume
P4A diagnostic-authorization verdicts.

`ab-aat-to-parser-ir` already owns the compiled Parser-IR validator. Predicate 5
must deepen that authority rather than create a parallel validator.

## Decision

Deepen the existing shared `abc.tools.parser-rq-capture` protocol, then build
two independently captured measurement projections and one pure composer.

The instruments share the pinned candidate identity, corpus identity, and
immutable input blobs by value. They do not share record indexes, staging
state, attempt status, aggregate availability, or policy identity. The composer
authenticates their common qualification identity and installs their envelopes;
it does not compute either metric.

This preserves the two real relations:

1. diagnostic envelopes conform to their wire contract; and
2. generated Parser-IR values conform to the pinned Parser-IR schema.

Fusing them because one candidate execution can produce both artifacts would
turn operational convenience into semantic and lifecycle coupling.

The shared protocol is the single owner of logical-blob authentication, closed
membership authentication, capture-generation identity, envelope construction,
and total JSON-status-to-Clojure-outcome mapping. The two instruments own only
their measurement-specific policies, work states, arithmetic, and witnesses.

## Rejected alternatives

### One shared capture index with two projections

This reduces orchestration calls but braids membership, availability, and
schema evolution. A missing diagnostic blob could poison Parser-IR conformance,
or a Parser-IR lifecycle change could rotate predicate 4 evidence. Rejected.

### Extend P4A and P2

P4A authorizes diagnostic meaning for R2; it is not the authority for envelope
shape. P2 treats Parser-IR validity as a join precondition supporting
publication-structure measurement; it is not predicate 5's corpus authority.
Reusing their verdicts would create fake seams and circular evidence. Rejected.

### A second standalone Parser-IR validator

A duplicate validator could diverge from the production conversion path while
claiming the same predicate identity. Predicate 5 instead exposes the existing
compiled validator's outcome at a qualification-facing boundary. Rejected.

## Common capture protocol

### Classification and owner

**Protocol Design, then Deepen -- owner: `abc.tools.parser-rq-capture`.** The
existing module already owns logical blob authentication and observation
envelopes. P4B deepens that cohesive boundary with reusable closed-membership,
generation-identity, and total status-mapping operations instead of implementing
those invariants separately in each instrument.

The common interface accepts values and returns values. It does not know
diagnostic vocabulary, Parser-IR schemas, predicate arithmetic, candidate
execution, registry admission, or filesystem discovery beyond resolving and
authenticating an explicitly supplied blob locator.

This classification is disproved if the shared module needs an instrument's
policy fields or work-state enum. That would be a generic framework braiding
measurement semantics into transport; the behavior must remain in the owning
instrument instead.

Each instrument has four closed artifacts:

1. a capture manifest binding candidate, corpus, work membership, logical blob
   references, policy identity, and validator identity;
2. exactly one immutable work record per expected corpus member;
3. a closed index naming exactly those work records; and
4. a pure aggregate record from which an observation envelope is derived.

The expected work set comes from the committed qualification corpus. Filesystem
discovery never defines membership. Duplicate, missing, extra, or reordered
identity claims are checked against the closed set; input order has no semantic
effect.

Blob references use logical identity (`sha256:<digest>`, byte length, and media
type) plus a runtime-resolved locator. Reads re-hash bytes and never trust
metadata. Missing, mismatched, escaped, or symlinked locators are unavailable.

Capture writes a fresh generation and publishes only after its complete index
authenticates. Recapture creates a new immutable fact; it never edits an old
record in place.

The shared protocol owns the exhaustive transport mapping. Instrument schemas
declare their allowed JSON status strings and their corresponding Clojure
observation values; an unknown or unmapped status is unavailable due to protocol
identity failure, never guessed. Instrument code does not carry duplicate
ad-hoc mapping tables.

## Outcome algebra

Evidence availability and candidate quality are separate axes.

| Condition | Instrument outcome |
| --- | --- |
| Missing or unauthenticated bytes | unavailable |
| Candidate, corpus, policy, validator, or schema identity mismatch | unavailable |
| Malformed, duplicate, incomplete, or extra index membership | unavailable |
| Authenticated candidate output violates the measured contract | available failure |
| Authenticated candidate output satisfies the measured contract | available pass |

An authenticated bad candidate artifact is evidence against the candidate and
must not hide behind unavailable. Conversely, absent or unauthenticated evidence
must not be converted into a failure measurement.

When a malformed authenticated value makes a numeric denominator unknowable,
the observation uses a closed typed failure sentinel. It never fabricates
`0.0`. The existing exact-equality evaluator treats a non-unavailable sentinel
unequal to `1.0` as a failure.

## Predicate 4: diagnostic-envelope completeness

### Authority and policy

The instrument validates authenticated raw `ab-aozora` schema-v3 diagnostic
bytes against the exact pinned schema. Its policy identity is the JCS hash of a
closed value containing:

- policy ID and algorithm version;
- raw diagnostic schema ID and logical hash;
- diagnostic wire version;
- validator semantic-closure hash;
- expected work-set identity; and
- aggregate and vacuity semantics.

The instrument may reuse the same raw blob captured for P4A by logical identity.
It independently validates shape and never consumes P4A's authorization result,
gap dispositions, or R2 verdict.

### Work record

Every expected work has one record containing:

- work identity and attempt disposition;
- candidate and policy identity;
- authenticated raw diagnostic blob identity;
- emitted and complete diagnostic counts when derivable;
- conformance status;
- explicit vacuity state; and
- bounded validation witnesses; and
- when validation fails, a logical blob reference to the complete immutable
  validation ledger.

An authenticated schema-valid envelope is complete because the pinned schema
requires every entry's `kind`, `code`, `severity`, `source`, and `span`.

### Aggregate and observation

For schema-valid envelopes:

`diagnostic_completeness = complete_diagnostics / emitted_diagnostics`

If the closed corpus emits zero diagnostics, the legitimate result is `1.0`
with exact counts and `vacuous: true`. The observation must disclose at least
`diagnostic_count`, `works_with_diagnostics`, and `vacuous`; a bare scalar is
not an accepted capture product.

If authenticated diagnostic bytes cannot be parsed or schema-validated well
enough to know the emitted-entry denominator, the aggregate emits the typed
failure sentinel `:invalid-diagnostic-envelope`. It is available, fails the
predicate mechanically, and retains witnesses. It is neither unavailable nor
a fabricated numeric zero.

The JSON aggregate carries the closed string status
`"invalid_diagnostic_envelope"`. The pure Clojure derivation maps that status to
the envelope value `:invalid-diagnostic-envelope`; cross-language artifacts do
not encode Clojure keywords.

Predicate 4 does not determine whether the parser emitted every diagnostic it
ought to have emitted. Diagnostic recall remains a separately governed future
workstream.

## Predicate 5: Parser-IR schema conformance

### Authority and policy

The existing compiled validator in `ab-aat-to-parser-ir` remains the sole
validation authority. A qualification-facing capture path records the generated
Parser-IR value and the validator outcome at the point where both coexist,
instead of collapsing schema failure into an undifferentiated process error.

This is a deliberate failure-time behavior addition. Today the production
conversion path propagates `validate_compiled(...)?`, so schema rejection
hard-aborts and exposes no output. P4B introduces a structured internal
validation outcome that retains the generated value and validation witnesses in
the same pass. The existing production conversion API must continue mapping an
invalid outcome to the same hard error; only the new qualification-facing path
records `schema_invalid` and proceeds to its work record. No caller silently
changes from fail-fast conversion to permissive conversion.

Its policy identity is the JCS hash of a closed value containing:

- policy ID and algorithm version;
- exact Parser-IR schema ID and logical hash;
- validator semantic-closure hash;
- expected work-set identity;
- generated-output denominator semantics; and
- `no_output` semantics.

It depends on the pinned candidate and schema, not compatibility admission.

The proposed deepening is disproved if retaining the invalid generated value
requires re-running conversion, making a second validator pass, weakening the
production hard-abort contract, or duplicating the compiled validator. Any of
those results reopens the design rather than being patched into the plan.

### Work record

Every expected work has one record with one of these candidate outcomes:

- `schema_valid`: a generated Parser-IR value exists, its logical identity is
  recorded, and the production validator accepts it;
- `schema_invalid`: a generated value exists, its logical identity and bounded
  validation witnesses plus a logical reference to the complete validation
  ledger are recorded, and the production validator rejects it;
  or
- `no_output`: parsing or conversion ended before a Parser-IR value existed.

Infrastructure and identity failures are recorded separately as unavailable.
They are never expressed as `no_output`.

### Aggregate and observation

When at least one output was generated:

`parser_ir_schema_validation = schema_valid_outputs / generated_outputs`

The aggregate also discloses `expected_works`, `generated_outputs`,
`schema_valid_outputs`, `schema_invalid_outputs`, and `no_output_works`.
`no_output` cannot silently shrink the run because every expected work remains
in the authenticated index and the count is explicit. Predicate 1 independently
decides whether parse/conversion failure is release-acceptable.

If `generated_outputs = 0`, the observation emits the available typed failure
sentinel `:no-parser-ir-output`. It never reports `0/0`, `1.0`, or unavailable.

The JSON aggregate represents this state as `"no_parser_ir_output"`; the pure
Clojure derivation maps it to the envelope value `:no-parser-ir-output`.

Authenticated invalid JSON or schema-invalid Parser-IR is an available failure.
Missing or unauthenticated output evidence is unavailable.

## Semantic closure and reproducibility

Each validator policy binds a reviewed deterministic semantic-closure manifest.
A guard derives the actual transitive owned-code dependency closure and requires
set equality with that manifest. Adding, removing, or moving a helper therefore
fails until the reviewed closure and policy identity rotate.

The closure includes the schema and algorithm-bearing code, plus runtime or
toolchain coordinates that can change validation meaning. Incidental host facts
are disclosed as attempt context, not identity.

The final P5 qualification identity records two distinct entries:

- `diagnostic_envelope_completeness -> sha256:<policy-identity>`; and
- `parser_ir_schema_conformance -> sha256:<policy-identity>`.

Bounded P4B fixtures use fixture qualification identities. P5 pins the final
implementation commit before it captures authoritative observations.

## Pure composition

The composer accepts one authenticated result from each instrument and a target
qualification identity. It requires:

- exact full qualification-identity equality;
- exact expected corpus membership;
- the expected distinct policy identities;
- closed result schemas; and
- absence of pre-existing values under `:diagnostic_completeness` and
  `:parser_ir_schema_validation`.

It returns a new measurements value containing identity-bound envelopes. It
rejects scalar bypass, overwrite, mixed candidate revisions, mixed corpus
generations, and cross-policy composition.

The composer performs no execution, filesystem discovery, metric calculation,
registry lookup, admission decision, or status promotion.

Composition-time identity checking is a build-time guard over these two newly
installed envelopes. It prevents constructing and committing a locally mixed
measurements value. It is not the release authority. The gate-level
`coherent-observations?` precondition remains the authoritative backstop over
all nine observations and must independently reject any incoherent bundle,
including bundles produced without this composer. The scopes intentionally
overlap as defense in depth: the composer checks its construction boundary; the
gate owns release qualification.

## Failure handling

Availability reasons are closed and machine-readable. At minimum they
distinguish membership, blob authentication, policy identity, validator
identity, schema identity, candidate identity, and aggregate-shape failures.

Candidate failures are also closed and distinct from infrastructure failures.
Presentation may render human-readable explanations, but derivation consumes
only the typed records.

No partial aggregate yields an observation. Any unavailable expected work makes
that instrument aggregate unavailable. It does not poison the other instrument.

## Verification strategy

### Example and characterization tests

Pin valid, invalid, missing, malformed, empty-diagnostic, zero-output,
duplicate, extra, reordered, identity-mismatch, and policy-mismatch cases.

Characterization tests prove predicate 5 invokes the production compiled
Parser-IR validator rather than a parallel implementation. Predicate 4 tests
prove P4A authorization changes cannot change completeness for unchanged raw
bytes and policy.

### Property tests

Hegel properties live beside the relevant Rust tests and cover:

- permutation invariance of closed indexes;
- exact membership under generated omissions, extras, and duplicates;
- aggregate arithmetic against a simple independent model;
- monotonicity of failure counts when valid records become invalid;
- preservation of unavailable under composition; and
- rejection after any candidate, corpus, schema, validator, or policy identity
  mutation.

Properties use broad generators and shrink to minimal counterexamples. Exact
wire rendering and exact error messages remain example-test concerns.

### Disposable probe

A plumbing-only fault-injection probe supplies authenticated malformed
diagnostic bytes and demonstrates that the instrument emits the available
`:invalid-diagnostic-envelope` failure sentinel. The probe is not retained as a
production abstraction.

### Drift and repository checks

Each instrument has a bounded Capture -> Derive -> Drift fixture that
regenerates byte-identically. The composer has a deterministic pure fixture.
Delivery runs focused Rust and Clojure tests, schema checks, Nix checks, comment
hygiene, strict ADR governance, and `just validate-migration`.

## Sequencing

1. Characterize and deepen the shared capture protocol, then freeze the two
   work/index/aggregate schemas and independent policy shapes.
2. Deepen `ab-aat-to-parser-ir` to expose its existing validation outcome
   without adding a validator.
3. Implement diagnostic capture/derivation and its vacuity/failure protocol.
4. Implement Parser-IR conformance capture/derivation and `no_output` protocol.
5. Implement the pure composer.
6. Add property, drift, adversarial identity, and disposable fault-injection
   tests.
7. Record P4B as implemented without touching authoritative P5 evidence.

The two instruments can be implemented independently after their common record
conventions are frozen. The composer follows both.

## Non-goals

- Diagnostic recall or diagnostic-gap authorization.
- Reinterpreting P4A dispositions as completeness.
- Reusing P2's supporting Parser-IR precondition as predicate 5.
- Changing the predicate set or threshold semantics.
- Corpus-scale capture, final candidate pinning, registry admission, or ADR
  0039 promotion.
- A generic multi-parser validation framework.

## Acceptance criteria

P4B is implemented when:

1. one shared capture protocol owns blob authentication, membership closure,
   generation identity, envelope construction, and total status mapping, while
   predicates 4 and 5 retain independent closed records, indexes, aggregates,
   policies, and availability;
2. authenticated invalid candidate artifacts yield available failures while
   evidence/identity faults yield unavailable;
3. zero diagnostics pass only with explicit vacuity counts;
4. zero generated Parser-IR outputs yield `:no-parser-ir-output` failure;
5. no work can disappear from either closed expected membership set;
6. predicate 5 uses the production compiled validator and predicate 4 remains
   independent of P4A authorization;
7. semantic-closure drift rotates policy identity or fails closed;
8. the pure composer rejects scalar bypass, overwrite, and mixed identities;
9. bounded Capture -> Derive -> Drift and property tests pass; and
10. repository-wide validation exits zero without P5 capture, admission, or
    promotion.

## Architecture review record

The hammock and Hickey review produced six designed resolutions. None is marked
implemented or verified until its named characterization or contract test is
green:

1. **Capture coupling -- Blocker; resolution designed, pending
   `independent-instrument-availability-test`.** Two predicates sharing a
   candidate does not justify one lifecycle; capture and availability remain
   independent.
2. **Authenticated-invalid ambiguity -- Blocker; resolution designed, pending
   `typed-failure-sentinels-are-available-failures-test`.** Candidate contract
   violations are available failures, not infrastructure unavailability. This
   test must pin the evaluator behavior explicitly rather than rely on the
   current unavailability allowlist incidentally.
3. **Unknowable denominator -- Blocker; resolution designed, pending
   `malformed-authenticated-diagnostics-use-failure-sentinel-test`.** Malformed
   diagnostic bytes use a typed failure sentinel rather than fabricated `0.0`.
4. **Vacuity -- Blocker; resolution designed, pending
   `authenticated-empty-diagnostics-disclose-vacuity-test`.** A zero-diagnostic
   pass carries exact counts and `vacuous: true` in the observation product.
5. **Validator duplication -- Strong suggestion; resolution designed, pending
   `qualification-and-production-use-one-compiled-validator-test`.** Predicate 5
   deepens the existing compiled validator instead of implementing another, and
   the production path retains its hard-abort behavior.
6. **Semantic identity -- Strong suggestion; resolution designed, pending
   `semantic-closure-manifest-equals-derived-closure-test`.** Policy identity
   binds a mechanically guarded transitive closure, not a human version label.

The resulting modules are deep around two cohesive decisions: diagnostic wire
conformance and Parser-IR schema conformance. The composer is intentionally
small because identity coherence, not measurement, is its sole concern.

### Deepening classification

- **Shared capture protocol -- Protocol Design, then Deepen.** Observed
  evidence: `abc.tools.parser-rq-capture` already owns logical blob
  authentication and envelopes, while P1-P3 independently repeat closed-index
  and status-projection concerns. P4B adds the common invariants to that owner,
  not measurement semantics. The falsifier is any need for instrument policy
  fields in the shared interface.
- **Diagnostic instrument -- Measurement projection.** Observed evidence: the
  schema-v3 value and P4A capture already exist. The remaining cohesive concern
  is diagnostic wire conformance and vacuity arithmetic over the shared capture
  protocol. This classification is disproved if implementation needs P4A
  disposition state.
- **Parser-IR instrument -- Implementation Refactor, then Deepen.** Observed
  evidence: the production validator exists behind a fail-fast `?` boundary,
  but its generated value and structured outcome are lost at the qualification
  boundary. Characterization must pin current timing and errors before adding
  the qualification outcome path. The falsifier is any need for a second pass,
  a second validator, or weakened production failure behavior.
- **Composer -- Delete/Inline risk controlled.** It is a pure function required
  for adversarial identity tests, not a strategy, factory, service, or extension
  hierarchy. If it accumulates capture, policy, metric, or registry behavior,
  the seam has failed and those concerns return to their owners.

Trust, lifecycle, time, and identity are explicit in the closed records and
policies. No mutable shared state crosses either capture boundary. The principal
behavior-preservation risk is exposing predicate 5's existing validation result;
the implementation plan must characterize that path before refactoring it.
