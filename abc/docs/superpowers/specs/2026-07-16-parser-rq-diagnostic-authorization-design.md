# Parser RQ Diagnostic Authorization and R2 Activation Design

**Status:** Superseded in part by
`2026-07-16-parser-rq-source-claim-ledger-design.md`

**Supersession note:** The live characterization falsified the assumption that
Parser-IR node-span gaps are principally diagnostic recovery regions. This
design remains applicable to the independent R2 partition of ledger-derived
gaps, but diagnostics no longer participate in the R1 coverage authority. Its
original P4A implementation shape is replaced by P4A1-P4A3.

**Scope:** The custom `ab-aozora` parser only. Third-party parsers remain
research instruments until their comprehensive report/publication is complete;
their later retirement is a separate milestone and does not block this work.

**Produces:** A P4-owned, identity-bound adapter from authenticated raw
`ab-aozora` schema-v3 diagnostics to exact authorized decoded-byte intervals,
and activation of the R2 `:silent_drops` observation over P1's existing pure
reconciler.

**Historical program placement:** This was P4A. The source-claim-ledger design
now decomposes that slot into P4A1-P4A3. The diagnostic trust boundary survives
as P4A3 and depends on ledger-authoritative R1 gaps from P4A2. P4B remains the
existing predicate-4/5 hardening work. None depends on admission; P5 remains the
barrier.

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
- raw diagnostic schema ID, canonical SHA-256, and version `3`;
- coordinate system `decoded_utf8`;
- every permitted wire code;
- exact permitted `source` and severity set per code;
- disposition `authorize_exact_span`, `observe_only`, or `reject_internal`.

The policy is a complete vocabulary, not a prefix match or default rule.
Duplicate policy codes are invalid. A raw code absent from the policy makes the
entire authorization result unavailable; it is never silently ignored.

The parser vocabulary may generate a **candidate** policy skeleton, but cannot
approve it. ABC's reviewed, canonical policy document is the authority. Runtime
code never regenerates policy, infers disposition from a namespace, or imports a
parser match statement as authorization. A drift test compares the independent
policy code set with the live vocabulary and fails on either addition or
removal.

### 3. Initial v1 classification requires characterization, not namespace rules

Before v1 is frozen, a disposable characterization command must exercise one
representative source for every live code and join three values without
conflating them:

```text
diagnostic entry
R1 coverage over the same decoded source
documented parser recovery/output for that code
```

Its committed small summary records, per code, severity/source, diagnostic
span, whether the span is already covered, recovery behavior, and a proposed
disposition with rationale. The disposable command is not shipped as policy and
does not write the final policy. The reviewed summary is the evidence used to
author ABC's exact table.

This gate matters because the live vocabulary already contains distinct
semantics: `accent-decomposition-applied` is documented as loss-free;
`non-canonical-directive` is advisory; `bouten-target-ambiguous` still applies a
best-effort choice; other diagnostics explicitly degrade a construct or preserve
it as unknown/raw. Sharing a `lex` namespace is not evidence that all should
excuse uncovered bytes.

The final v1 document enumerates all 21 live code identities exactly. The four
internal invariant codes
(`residual-annotation-marker`, `unregistered-sentinel`,
`registry-out-of-order`, and `registry-position-mismatch`) receive
`reject_internal`; their appearance makes authorization unavailable because it
signals a parser defect rather than source loss. Loss-free/advisory recoveries
receive `observe_only`. Only codes whose reviewed recovery contract establishes
that their exact diagnostic span is the intended accountable source-loss region
may receive `authorize_exact_span`.

If characterization cannot justify `authorize_exact_span` for any code, v1 may
contain no authorizing codes. R2 still activates correctly: uncovered bytes stay
silent. The purpose of the policy is to preserve truth, not manufacture a green
predicate.

### 4. Exact spans only

`authorize_exact_span` contributes precisely the diagnostic's half-open decoded
UTF-8 byte interval. Authorization never expands to a line, enclosing
construct, delimiter pair, or recovery region. Any expansion would be a new,
code-specific policy version with its own evidence.

The adapter rejects:

- absent or non-integer endpoints;
- `start >= end`;
- endpoints beyond that work's decoded-source byte length;
- endpoints that are not UTF-8 character boundaries in the authenticated
  decoded-source value;
- a coordinate system other than the policy's implicit `decoded_utf8` capture
  contract;
- a code whose `source` or severity differs from policy;
- a `kind` that is not the underscore form of the same stable identity as the
  kebab-case `code`;
- `codepoint` on any code other than `source-contains-pua`, or a missing/invalid
  `codepoint` for that code;
- duplicate raw entries with the same complete semantic identity
  `(code,severity,source,start,end)`;
- any `source = internal` entry.

Overlapping authorized diagnostics are normalized only after every entry has
been validated. Normalization cannot make an invalid entry acceptable.

The raw schema is a first-class pinned protocol. The adapter validates the
envelope against the canonical schema whose ID/hash is named by policy; checking
only `schemaVersion: 3` is insufficient.

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
decoded-source logical ref, exact UTF-8 bytes, and byte length
original-source logical ref
raw-diagnostic logical ref and exact bytes
authorization-policy logical ref and exact bytes
```

It authenticates both logical refs by byte length and streaming SHA-256 before
JSON decoding. The raw capture must be the diagnostic ref already bound into
the corresponding authenticated P1 work record. Qualification identity, work
ID, source identity, and decoded length must match that record. A locator is
runtime configuration, never content identity.

The decoded-source bytes are required even though the reconciler operates on
intervals: numeric bounds alone cannot prove that diagnostic endpoints lie on
UTF-8 character boundaries. The I/O boundary authenticates the decoded blob
against the P1 work record; the pure authorizer requires every endpoint to be a
valid boundary in that exact value.

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
- a content-addressed full decision ledger with one decision for every raw
  diagnostic, plus bounded report witnesses;
- empty errors.

An `unavailable` result retains identities, policy/capture refs when they were
authenticated, and nonempty stable errors. It omits interval and count claims
that were not established.

The deep module is intentionally narrow:

```text
authorize(validated capture value, validated policy value, work context)
  -> available authorization value | unavailable value
```

Blob lookup, streaming authentication, CAS publication, and corpus orchestration
remain outside this pure decision function. This is a real seam: the pure
function has independent property tests, while the I/O boundary has tamper,
locator, and atomic-publication tests. It is not an interface introduced for
hypothetical parser variation.

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
byte intervals, normalized and counted within each work and then summed. No
interval ever merges across work identities. It is not a dropped-construct census.
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
one decision-ledger entry exists for every raw diagnostic
```

Across the corpus:

```text
observed work IDs = expected work IDs
sum(work uncovered_bytes) = R1 uncovered_eligible_bytes
sum(work silent_bytes) = corpus silent_bytes
corpus vacuous = (sum(work diagnostic_count) = 0)
```

Integer interval values are authoritative. No rounded ratio controls R2.

## Failure behavior

The adapter or R2 derivation becomes unavailable for:

- missing, unreadable, mismatched, or malformed blobs;
- raw diagnostic schema version other than `3`;
- raw diagnostic schema ID/hash mismatch;
- unknown/duplicate diagnostic fields or invalid envelope shape;
- disagreement between redundant `kind` and `code` identities;
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
- Review and own the disposition rationale. Parser code can expose vocabulary
  and examples but cannot authorize itself.

### ab-validator

- Parse the schema-v3 envelope with closed Rust types.
- Authenticate and authorize `ab-aozora` diagnostics against the exact policy.
- Produce deterministic per-work results, content-addressed storage, and corpus
  index/aggregate.
- Reuse P1 interval normalization and reconciliation; do not duplicate interval
  arithmetic.
- Keep validation/authorization pure after blob authentication; keep capture,
  storage, and orchestration at the shell.

### P5

- Pin the final parser/instrument/policy identities.
- Run authoritative full-corpus capture on
  `hinoki.hyakutake-barbel.ts.net`.
- Admit/promote only after P0-P4 evidence and governance recapture pass.

## Identity, time, and rotation

Authorization is a value derived at one explicit release-candidate identity; it
is not mutable current policy. The qualification identity's
`instrument_versions` map gains exact entries for the diagnostic authorizer and
R2 derivation instrument. Capture manifests additionally bind the raw schema
hash and authorization policy hash.

Any change to the raw schema, live vocabulary, code severity/source, span
semantics, or disposition rationale requires review before capture. A compatible
implementation change may retain the policy version only when canonical policy
bytes are unchanged; otherwise the policy version/hash rotates. Old results
remain immutable and derivable against their old policy. No registry row is
rewritten in place.

The only state transition is publication of new immutable artifacts followed by
an atomic index/manifest update. Partial CAS blobs are unreferenced values, not
current evidence. Rollback is selection of the prior manifest/policy identity,
not mutation of results.

## Testing strategy

- A disposable pre-freeze characterization covers every live code and records
  recovery/coverage evidence; reviewed v1 golden tests then cover every code
  exactly once and drift-test the independent policy against the Rust
  vocabulary.
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
- Raw-schema tests pin `kind`/`code`, conditional `codepoint`, source/severity,
  and schema ID/hash coherence.
- Historical captures without this authorization evidence continue to derive
  R2 as `:instrument-missing`.
- Full validation includes root `just validate-migration`, Rust tests/clippy/fmt,
  Clojure/Kaocha, comment hygiene, schema drift, and ADR governance recapture
  when an evidence closure changes.

## Rollout and non-goals

1. Run the disposable all-code characterization and review its summary.
2. Land and drift-test independently authored policy/protocols.
3. Land the authorization adapter and deterministic fixture capture.
4. Activate R2 derivation while historical evidence remains unavailable.
5. P5 pins identities and performs full-corpus capture.
6. After the third-party parser research report/publication is complete, run a
   separate retirement plan that removes those parsers from active operational
   and qualification surfaces while retaining immutable research artifacts.

This design does not generalize authorization across third-party parsers,
expand spans, redefine diagnostic completeness as recall, perform corpus-scale
capture, admit a release, promote ADR 0039, or retire third-party parser code.

## Acceptance criteria

The design is implemented when:

1. every live `ab-aozora` diagnostic code has exactly one reviewed v1 policy
   disposition grounded in the committed characterization summary, and
   vocabulary drift fails closed;
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

## Architecture review record

The Hickey/deep-module review changed the draft in six material ways:

1. **Authority circularity — Blocker, resolved.** Parser vocabulary may produce
   a candidate skeleton but cannot approve dispositions; reviewed ABC policy is
   independent authority.
2. **Namespace authorization — Blocker, resolved.** A disposable per-code
   recovery/coverage characterization precedes policy freeze; `lex` membership
   is never an authorization rule.
3. **Protocol identity — Blocker, resolved.** Raw schema ID/hash and redundant
   `kind`/`code` identity are validated, not only `schemaVersion: 3`.
4. **Coordinate validity — Blocker, resolved.** Authorization consumes the
   authenticated decoded value and checks UTF-8 boundaries, not numeric bounds
   alone.
5. **Audit truncation — Strong suggestion, resolved.** Full per-diagnostic
   decisions live in a content-addressed ledger; bounded witnesses are only a
   report projection.
6. **Roadmap/metric ambiguity — Strong suggestion, resolved.** P4 is explicitly
   split into P4A/P4B, and R2 is defined as a per-work maximal byte-interval
   witness count rather than a construct census.

The resulting authorization module qualifies as a deep module: one cohesive
decision concern, a small pure interface, explicit trust/identity/error
semantics, and separate I/O orchestration. It is not a generic multi-parser
strategy seam.
