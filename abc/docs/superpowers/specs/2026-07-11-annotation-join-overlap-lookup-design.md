# Annotation Join Overlap Lookup Design

**Date:** 2026-07-11

## Goal

Reduce annotation-join runtime without changing any join result, output shape,
classification, ordering, or validation contract.

## Scope and ownership

This change belongs entirely to ABC. It modifies the generated-view join in
`abc.tools.annotation-join`; it does not change parser output, AAT, parser-IR,
tokenizer spans, schemas, manifests, or registry admission.

Tokens remain subject to the existing precondition: ascending by input-span
start and non-overlapping. Annotations need not be sorted and may overlap,
be zero-width, or be inverted.

The optimized lookup cannot retain the exhaustive scan's accidental tolerance
of invalid token order. `join` therefore coerces its token input to a vector
and validates ascending, non-overlapping, non-degenerate spans once per call.
Violation throws `ExceptionInfo` before producing any result. The public
docstring states this failure mode. This O(tokens) validation is intentional:
it prevents a future unvalidated caller from receiving plausible but wrong
classifications, while remaining negligible beside per-annotation lookup.

## Design

Replace the exhaustive token scan performed for every annotation with:

1. A lower-bound binary search for the first token whose span end is greater
   than the annotation start.
2. A forward scan from that position while token span start is less than the
   annotation end.

For a non-degenerate annotation this returns exactly the tokens satisfying
the existing overlap predicate `token.start < annotation.end &&
annotation.start < token.end`, in their original order. Degenerate spans keep
the existing empty-cover behavior before lookup.

This changes expected work from O(annotations × tokens) to
O(annotations × log(tokens) + returned overlaps). It deliberately does not
use a stateful sorted merge because annotation ordering is not a documented
precondition, and it does not add an interval tree because token spans are
already sorted and non-overlapping.

## Verification

The current exhaustive predicate remains as an independent test oracle, not
as a production fallback. Existing exact classification tests remain intact.
A `clojure.test.check` property generates valid sorted, non-overlapping token
spans plus arbitrary annotation spans and asserts full join-result equality
between the optimized implementation and the exhaustive oracle. Generated
cases include empty token streams, gaps, boundary contact, zero-width spans,
inverted spans, and annotations outside the token range.

The performance target is grounded in the re-verification handoff, which
records that the annotation×token join dominates the approximately 7.3-hour
single-core join-stats step
(`docs/handoffs/2026-07-11-tokenizer-comparison-followups.md`). A deterministic
benchmark command compares the two lookup algorithms over at least two real
works from a retained join-stats run: the largest available token stream and a
second work selected for high annotation density. It reports token and
annotation counts, records the equality-checking exhaustive pass as its
baseline, warms the optimized implementation, and reports five optimized
elapsed measurements with a median ratio against that baseline. Repeating the
exhaustive pass on the largest retained work was rejected after a measured
attempt exceeded 17 minutes without completing the original five-repeat
protocol; multiplying that known bottleneck does not improve the go/no-go
decision. Input paths are operator arguments and are never checked into source.

The performance acceptance floor is a 10× median speedup for the isolated
overlap lookup on each real work. Failure to meet it stops the optimization
for investigation; it is not waived by passing functional tests. Benchmark
numbers are operator evidence, not a timing-sensitive automated test.

## Acceptance criteria

- Focused annotation-join tests and the property test pass.
- The existing ABC Clojure checks pass.
- The benchmark verifies identical results and records a material speedup on
  both real workloads, meeting the 10× isolated-lookup floor.
- No public output or file format changes.
- No Phase 3 parser file is modified.
