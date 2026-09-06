# Redefine Fatal Failures Against Governed Expected Outcomes

## Implementation Status

Decided 2026-07-27 (the corpus-tiering design's D7); recorded 2026-07-31 by
transcription from the design's *D7* section, which is that decision's own
written reasoning. Not implemented: `allowed_dispositions` is still
level-confused and the expected-outcome model is unspecified, so the
adversarial fixture tier remains blocked on the work, not on the decision.

## Context

`parser_rq_core_attempt` computes `:fatal_failures` by counting records
whose measured disposition is `fatal_error`, with no reference to the corpus
member's `:expected_status`. `allowed_dispositions` merely permits the value
to travel through the capture protocol; it does not make an *expected* fatal
rejection pass. So a fixture that deliberately expects fatal rejection
increments the count and fails `fatal-failures <= 0` — the adversarial
population the fixture tier promises cannot be adopted under the current
predicate.

Three ways out were tabled: redefine the predicate against governed
expectations; keep malformed-input conformance in a separate negative-test
lane; or require every supported malformed input to produce a non-fatal
parsed-and-diagnosed result.

## Decision

Option 1: redefine the predicate as **`unexpected-fatal-failures <= 0`**,
comparing each observed disposition against a governed expected outcome, so
that the predicate says what it means. This rotates instrument semantics and
`predicate_set_hash`, and is sequenced with the wall-time reshape so one
identity rotation covers both.

Prerequisites, in order, before anything can compare:

1. untangle `allowed_dispositions` — it lists the record-*status* value
   `protocol_error` inside a *disposition* vocabulary, conflating the work
   schema's two levels;
2. specify the two-level expected-outcome model with closed, schema-enforced
   vocabularies, including which values are never expectable;
3. adopt the safe initial contract: expectations restricted to `parsed` and
   `fatal_error`, with `adapter_timeout`, `protocol_error`, and
   `unavailable` remaining unconditional failures — an expected timeout
   would make the timeout predicate vacuous, and an expected protocol error
   would let a harness fault masquerade as a governed outcome;
4. map corpus expectations to wire values, authenticate exact membership
   between expectations and observed records, and specify behaviour for a
   missing or unknown expectation.

## Consequences

The decision unblocks the work, not the tier: until step 4 lands, the
fixture tier still cannot carry adversarial cases. When it does land, the
predicate declaration changes, so `predicate_set_hash` rotates and prior
captured evidence is stale under the old identity.
