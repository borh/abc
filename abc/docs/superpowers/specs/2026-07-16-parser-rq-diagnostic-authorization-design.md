# Parser RQ Diagnostic Authorization and R2 Activation Design

**Status:** Approved for planning

**Scope:** The custom `ab-aozora` parser only. Third-party parsers remain
research instruments until their comprehensive report/publication is complete;
their later retirement is a separate milestone and does not block this work.

**Produces:** A P4-owned, identity-bound adapter from authenticated raw
`ab-aozora` schema-v3 diagnostics to exact authorized decoded-byte intervals,
and activation of the R2 `:silent_drops` observation over P1's existing pure
reconciler.

## Problem

P1 deliberately stopped at a pure interval operation:

```text
uncovered decoded-byte intervals + authorized intervals
  -> diagnosed intervals + silent intervals
```

That operation cannot decide whether a raw parser diagnostic is trustworthy or
whether its span denotes source loss. Treating every warning span as
authorization would complect three different facts:

1. the parser emitted a diagnostic;
2. policy recognizes the diagnostic's semantics;
3. the diagnostic authorizes particular source bytes as non-silent loss.

It would also allow new diagnostic codes, severity changes, malformed spans, or
a one-byte overlap to silently change release evidence. R2 therefore remains
`:instrument-missing` until a separate P4 boundary validates the raw capture and
produces the interval value P1 already knows how to reconcile.

## Grounded inputs

- P0 supplies capture manifests, logical blob authentication, the full
  qualification identity, observation envelopes, and drift checks.
- P1 supplies authenticated work records, decoded-source byte lengths, exact
  uncovered intervals, the R1 aggregate, and the policy-free `reconcile`
  function.
- `ab_aozora_aat::diagnostics_json_from_bytes` emits a closed
  `{schemaVersion: 3, data: [...]}` envelope from the same original bytes P1
  analyzes. Sanitizer diagnostics are emitted first, parser diagnostics second;
  both are rebased into full `DecodedSource.text` UTF-8 byte coordinates.
- Each live schema-v3 entry carries `kind`, stable kebab-case `code`, `severity`,
  `source`, and a half-open `span {start,end}`; `source-contains-pua` may also
  carry `codepoint`.
- `ab_aozora_spec::diagnostic::codes` is the implementation vocabulary
  authority. The authorization policy is an independent release-evidence
  authority and must enumerate the corresponding wire codes explicitly.

## Decisions

### 1. Raw capture, authorization, and reconciliation remain separate values

The system has three immutable artifacts:

- **Raw diagnostic capture:** what the pinned parser emitted.
- **Authorization result:** what a versioned policy accepts as exact
  source-loss intervals, or why authorization is unavailable.
- **R2 result:** the pure partition of R1 uncovered intervals into diagnosed and
  silent values.

No API accepts raw diagnostics where authorized intervals are expected. No R2
code interprets diagnostic codes or severities.

### 2. Authorization is closed, versioned, and `ab-aozora`-specific

ABC owns:

- `parser-rq-diagnostic-authorization-policy.schema.json`;
- `parser-rq-diagnostic-authorization-result.schema.json`;
- immutable `parser-rq-ab-aozora-diagnostic-authorization-v1.json`.

The policy binds:

- policy ID/version and canonical SHA-256;
- parser identity `ab-aozora`;
- raw diagnostic schema version `3`;
- coordinate system `decoded_utf8`;
- every permitted wire code;
- exact permitted `source` and severity set per code;
- disposition `authorize_exact_span`, `observe_only`, or `reject_internal`.

The policy is a complete vocabulary, not a prefix match or default rule.
Duplicate policy codes are invalid. A raw code absent from the policy makes the
entire authorization result unavailable; it is never silently ignored.

### 3. Initial v1 classification

The v1 policy is generated and drift-tested against the complete live
`ab_aozora_spec::diagnostic::codes` vocabulary, then reviewed as data. Source
diagnostics in the `aozora::lex` family that describe malformed input or an
explicit recovery receive `authorize_exact_span`. The advisory
`non-canonical-directive` lint receives `observe_only`: it is authoring guidance,
not evidence that source bytes were dropped. The four internal invariant codes
(`residual-annotation-marker`, `unregistered-sentinel`,
`registry-out-of-order`, and `registry-position-mismatch`) receive
`reject_internal`; their appearance makes authorization unavailable because it
signals a parser defect rather than source loss.

The checked-in policy contains every exact code and severity/source constraint;
the implementation plan must generate the initial document from the live
vocabulary and fail drift if either side changes. There is no runtime rule such
as “all lex codes authorize.”

### 4. Exact spans only

`authorize_exact_span` contributes precisely the diagnostic's half-open decoded
UTF-8 byte interval. Authorization never expands to a line, enclosing
construct, delimiter pair, or recovery region. Any expansion would be a new,
code-specific policy version with its own evidence.

The adapter rejects:

- absent or non-integer endpoints;
- `start >= end`;
- endpoints beyond that work's decoded-source byte length;
- a coordinate system other than the policy's implicit `decoded_utf8` capture
  contract;
- a code whose `source` or severity differs from policy;
- duplicate raw entries with the same complete semantic identity
  `(code,severity,source,start,end)`;
- any `source = internal` entry.

Overlapping authorized diagnostics are normalized only after every entry has
been validated. Normalization cannot make an invalid entry acceptable.

### 5. Unknown vocabulary makes R2 unavailable

A new or unknown code is evidence of parser/policy drift. The adapter emits an
unavailable authorization result with a stable error and no trusted interval
value. This prevents a parser release from changing qualification semantics
before the new diagnostic is reviewed and the policy version is rotated.

### 6. Empty diagnostic streams are valid and disclose vacuity

An authenticated, schema-valid empty stream produces an available authorization
result with:

```text
diagnostic_count = 0
authorizing_diagnostic_count = 0
authorized_intervals = []
vacuous = true
```

R2 remains meaningful:

- if R1 has no uncovered bytes, silent drops are zero and vacuity is disclosed;
- if R1 has uncovered bytes, every uncovered byte remains silent.

An absent, unauthenticated, malformed, or identity-mismatched stream is not an
empty stream and makes R2 unavailable.

## Protocols

### Authorization input

The adapter consumes one closed input value per work:

```text
qualification identity
work ID
decoded-source byte length
original-source logical ref
raw-diagnostic logical ref and exact bytes
authorization-policy logical ref and exact bytes
```

It authenticates both logical refs by byte length and streaming SHA-256 before
JSON decoding. The raw capture must be the diagnostic ref already bound into
the corresponding authenticated P1 work record. Qualification identity, work
ID, source identity, and decoded length must match that record. A locator is
runtime configuration, never content identity.

### Authorization result

An `ok` result contains:

- schema version and full qualification `identity_ref`;
- work ID and decoded byte length;
- raw diagnostic ref;
- policy version/hash;
- coordinate system `decoded_utf8`;
- `diagnostic_count`, `authorizing_diagnostic_count`, and
  `observe_only_diagnostic_count`;
- `vacuous`;
- normalized exact `authorized_intervals`;
- bounded diagnostic witnesses sufficient to audit each disposition;
- empty errors.

An `unavailable` result retains identities, policy/capture refs when they were
authenticated, and nonempty stable errors. It omits interval and count claims
that were not established.

### Corpus authorization index

Corpus membership is inherited from the authenticated P1 record index; it is
never inferred from a diagnostic directory. A deterministic authorization
command consumes the exact P1 index, authorizes each referenced raw diagnostic
capture, stores results by content identity, and writes a closed record index
last. Missing, extra, duplicate, or unavailable work results make the corpus
authorization aggregate unavailable.

### R2 observation

The Clojure derivation boundary authenticates:

- the P1 R1 aggregate and uncovered witnesses;
- the complete authorization aggregate/index;
- the authorization policy;
- full qualification identity and exact work set.

Only then does it pass authorized interval values to P1's pure reconciler. It
derives:

```text
diagnosed = uncovered intersect authorized
silent    = uncovered subtract authorized
```

The `:silent_drops` observation value is the number of maximal connected silent
byte intervals across work-tagged results. It is not a dropped-construct census.
The release predicate remains exact `silent_drop_count <= 0`.

The envelope also records total diagnostic count, authorizing count, diagnosed
bytes, silent bytes, and vacuity. These are derived audit fields, not alternative
pass conditions.

## Conservation invariants

For every available work result:

```text
diagnosed intersect silent = empty
diagnosed union silent = uncovered
diagnosed_bytes + silent_bytes = uncovered_bytes
authorized intervals are within [0, decoded_source_bytes)
```

Across the corpus:

```text
observed work IDs = expected work IDs
sum(work uncovered_bytes) = R1 uncovered_eligible_bytes
sum(work silent_bytes) = corpus silent_bytes
```

Integer interval values are authoritative. No rounded ratio controls R2.

## Failure behavior

The adapter or R2 derivation becomes unavailable for:

- missing, unreadable, mismatched, or malformed blobs;
- raw diagnostic schema version other than `3`;
- unknown/duplicate diagnostic fields or invalid envelope shape;
- policy hash/version drift or incomplete/duplicate policy vocabulary;
- unknown code, disallowed severity/source, invalid span, or internal diagnostic;
- P1 work identity/source/diagnostic-ref mismatch;
- missing, duplicate, or extra corpus work;
- interval or integer conservation failure;
- raw diagnostics reaching the reconciler API.

An available positive silent-drop count is an honest qualification failure, not
an instrumentation failure. It must not be patched by widening diagnostic spans
or reclassifying codes during capture.

## Implementation boundaries

### ABC

- Own closed policy/result/index/aggregate schemas and the immutable v1 policy.
- Register schemas in design-bundle validation and drift checks.
- Derive the identity-bound R2 envelope from authenticated values.
- Keep predicate 4 as diagnostic-envelope completeness; diagnostic recall
  remains a separate future ADR/workstream.

### ab-validator

- Parse the schema-v3 envelope with closed Rust types.
- Authenticate and authorize `ab-aozora` diagnostics against the exact policy.
- Produce deterministic per-work results, content-addressed storage, and corpus
  index/aggregate.
- Reuse P1 interval normalization and reconciliation; do not duplicate interval
  arithmetic.

### P5

- Pin the final parser/instrument/policy identities.
- Run authoritative full-corpus capture on
  `hinoki.hyakutake-barbel.ts.net`.
- Admit/promote only after P0-P4 evidence and governance recapture pass.

## Testing strategy

- Golden classification tests cover every v1 code exactly once and drift-test
  the policy against the live Rust vocabulary.
- Closed-schema tests reject unknown fields, missing identities, duplicate
  codes, and unavailable results carrying trusted intervals.
- Adapter tests cover unknown code, severity/source mismatch, internal code,
  malformed/out-of-bounds/empty spans, duplicate entries, overlap
  normalization, and authenticated empty streams.
- Exact-overlap examples prove a one-byte authorized diagnostic cannot excuse a
  larger uncovered interval.
- Hegel properties prove authorization ordering invariance and reconciliation
  partition conservation without generating unvalidated raw diagnostics.
- Capture -> Authorize -> Derive -> Drift fixtures regenerate byte-identically;
  mutation of capture, policy, identity, or R1 witnesses fails closed.
- A compile-fail or module-privacy test proves raw diagnostic types cannot be
  supplied to the public reconciler.
- Historical captures without this authorization evidence continue to derive
  R2 as `:instrument-missing`.
- Full validation includes root `just validate-migration`, Rust tests/clippy/fmt,
  Clojure/Kaocha, comment hygiene, schema drift, and ADR governance recapture
  when an evidence closure changes.

## Rollout and non-goals

1. Land and drift-test policy/protocols.
2. Land the authorization adapter and deterministic fixture capture.
3. Activate R2 derivation while historical evidence remains unavailable.
4. P5 pins identities and performs full-corpus capture.
5. After the third-party parser research report/publication is complete, run a
   separate retirement plan that removes those parsers from active operational
   and qualification surfaces while retaining immutable research artifacts.

This design does not generalize authorization across third-party parsers,
expand spans, redefine diagnostic completeness as recall, perform corpus-scale
capture, admit a release, promote ADR 0039, or retire third-party parser code.

## Acceptance criteria

The design is implemented when:

1. every live `ab-aozora` diagnostic code has exactly one reviewed v1 policy
   disposition and vocabulary drift fails closed;
2. raw schema-v3 capture is authenticated and cannot reach reconciliation
   without an available authorization result;
3. unknown/internal/malformed diagnostics make R2 unavailable;
4. authenticated empty streams derive available results with explicit vacuity;
5. exact-span reconciliation preserves all byte and work-set invariants;
6. Capture -> Authorize -> Derive -> Drift fixtures pass and adversarial
   mutations fail;
7. `:silent_drops` becomes an identity-bound derived observation while legacy
   evidence remains `:instrument-missing`;
8. `just validate-migration` exits zero, including governance.
