# Bind Final Parser Release Instruments

## Implementation Status

The five final instrument bindings, generated predicate-set identity, bounded
core-attempt analyzer, and admission/promotion transaction evidence are
implemented and registered. The authoritative corpus capture was not part of
this decision. The later P5 campaign completed that capture, accepted ADR 0040,
and promoted ADR 0039 to Accepted.

## Context

The nine release predicates were fixed before measurement, but five declarations
still named provisional commands or explicitly missing instruments. Those names
did not identify the versioned, authenticated producers implemented by P1, P4A,
P4B, and the core-attempt campaign slice. A candidate cannot be frozen while the
predicate identity points at those placeholders.

## Decision

Bind only the following instrument fields:

| Predicate | Former declaration | Final instrument |
| --- | --- | --- |
| fatal failures | `ab-check batch fatal_error result count (adapter exit 1)` | `parser-rq-core-attempt-v1` |
| source span coverage | `no committed per-work source-span-coverage instrument for ab-aozora` | `parser-rq-source-recognition-v1` |
| silent drops | `no committed silent-drop instrument (requires span-coverage vs diagnostic reconciliation)` | `parser-rq-diagnostic-authorization-v1` |
| wall time | `ab-check batch wall time via time(1); threshold from ci-smoke-corpus runtime target` | `parser-rq-core-attempt-v1` |
| timeout policy | `ab-check --per_work_timeout adapter_timeout result count` | `parser-rq-core-attempt-v1` |

Predicate IDs, dimensions, observed keys, units, comparators, and thresholds are
unchanged. The core-attempt instrument performs exactly three serial,
lock-retained repetitions and reduces each of its three observations by the
maximum. Source recognition and diagnostic authorization consume closed corpus
membership and authenticated capture values; they do not discover output files.

The binding rotates the predicate-set hash and therefore the qualification
identity. Old observations remain historical under their former identity and
cannot be relabeled. ADR 0040 and ADR 0039 remain Proposed until the fresh P5
capture satisfies their evidence-dependent acceptance criteria.

## Consequences

ADR 0041 must be Accepted before candidate freeze. Any further instrument
semantic change requires a new governed predicate identity; it cannot be folded
into the authoritative capture. This decision authorizes instrument names, not
predicate verdicts, registry admission, or ADR 0039 promotion.

## Evidence

The live predicate rotation, core-attempt, and campaign tests run in the
standing Kaocha suite
(`test/abc/tools/parser_release_qualification_test.clj`,
`test/abc/tools/parser_rq_core_attempt_test.clj`,
`test/abc/tools/parser_rq_campaign_test.clj`).
