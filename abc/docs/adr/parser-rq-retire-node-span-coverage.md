# Retire Parser-IR Node-Span Coverage

## Implementation Status

Not implemented. The decision is recorded; the scoped plan is
`docs/superpowers/plans/2026-07-27-q13-node-span-coverage.md`.

## Context

`:parser_ir_node_span_coverage` was retained as supporting evidence beside the
ledger-authoritative `source_span_coverage` during the R1 source-claim migration.
It was recorded as an open question on two grounds: it is named like a ratio and
measures something else, and it costs 9.80 ms per work — 4.2% of a capture layer
that is itself 9.25× all of `ab-check` — while being authoritative for no
predicate.

Both grounds hold. Traced and then measured, the problem is larger than either.

**One coordinate name means two things in two crates.** `map_node_span`
(`ab-aat-to-parser-ir/src/convert.rs`) throws away the AAT span's byte offsets,
emits the caller's running accumulator over each node's own emitted text, and
hard-codes `coordinate_system` to `decoded_utf8`. Inside that crate the string
means the **visible-text projection**: `sentences.rs` states the invariant that
depends on it and records that using real source offsets there was the historical
bug. `analyze.rs` reads the same string as the **decoded source file**.

Neither guard can catch it. The coordinate check compares the label against a
value the emitter hard-codes, so it can never fire. Bounds validation cannot
either, because emitted text is shorter than its source, so every interval
validates — `Interval::new` checks only `start <= end <= bound` and, unlike the
recognition path, never checks `is_char_boundary`.

**Measured through the built binary**, the three governed works of 43, 58 and 76
source bytes all report `covered_eligible_bytes` of exactly 25, because all three
emit the same eight visible characters plus a newline. On `hatsukoi.txt`, slicing
the decoded source at each node's span reproduces that node's own text 7 times in
8,137. Every record returned `status: "ok"` with no errors.

The quantity is approximately visible-text bytes ÷ decoded-source bytes: a
markup-density ratio published as coverage.

## Decision

**Retire the quantity.** Remove the `:parser_ir_node_span_coverage` measurement
key, the `NodeSpans` coverage basis, the span-union computation, and the coverage
byte fields it publishes.

Renaming is rejected: a renamed field is still computed by unioning intervals
from two coordinate systems, and the published record still declares a coordinate
it does not inhabit.

Re-deriving it over genuine source offsets is rejected on its own motivation.
The emitter would have to reconcile two coordinates rather than correct one; it
is a parser-IR schema change with publication-pipeline blast radius; and no
predicate wants the result. If a real parser-IR source-coverage measure is ever
needed, the predicate that requires it should justify it.

### Scoped to the quantity, not the analyzer

The consumer enumeration changed the shape of this decision, and the distinction
is load-bearing.

**Every consumer of the coverage number is a test.**

| Consumer | Callers |
| --- | --- |
| `install-source-recognition-observation` | 1, in `parser_release_qualification_test.clj` |
| `derive-source-span-envelope` | 9, all in `parser_rq_source_accountability_test.clj` |
| `docs/reports/parser-release-qualification-measurements.edn` | key absent |

**But the analyzer has one production role that must survive.** `analyze_corpus`
also produces the membership index that `analyze_recognition_corpus`
authenticates against, and the release-authoritative recognition path depends on
it — for the closed work list, the identity reference, the coherence counts, and
`membership_ref`, the hash of the index bytes recorded into every recognition
record. It reads no coverage number from it.

So the retirement removes the span union and the coverage fields. It retains the
membership derivation and the per-work authentication that index carries:
parser-IR `schema_id` and `schema_hash` against the qualification identity, and
`derived_from` against its AAT and mapping coordinates.

An earlier statement of this decision said "drop the observation and retire the
`NodeSpans` analyzer path." That was too broad — it would have taken the
membership index and the parser-IR authentication with it.

## Consequences

**This is a versioned instrument change, not a deletion.** Changing what the
membership index contains rotates `membership_ref`, and therefore every
recognition record. Prior captured evidence becomes protocol-incompatible rather
than merely stale.

**Published artifacts are not rewritten.** Twenty-two artifacts under
`docs/reports/parser-rq/runs/` carry the `parser_ir.nodes[*].span` basis, six of
them in the currently promoted run. Published manifests are immutable. The v1
schemas must either be retained so those artifacts stay validatable, or the
artifacts must be explicitly declared protocol-incompatible. Choosing neither
would leave published evidence that nothing can check.

**The cost saving is re-measured, not assumed.** 9.80 ms per work is an estimate
attached to the computation being removed.

**It unblocks the region partition.** Retiring this path removes `analyze.rs`'s
whole-file eligibility from the set of things
[parser-rq-source-region-partition](parser-rq-source-region-partition.md) task 3
must migrate.

**One question is deliberately left open**, because it belongs to the parser-IR
emitter rather than to this instrument: a single parser-IR document carries spans
in two coordinate systems, both labelled `decoded_utf8` — nodes run contiguously
in accumulator coordinates while the `底本` `source-note` node carries a genuine
decoded-source span that slices to exactly the colophon. Retiring the coverage
quantity removes the consumer that silently unioned across both, but it does not
make the emitter's coordinate determinate. That needs its own owner.

## Evidence

No claim carries an evidence path: nothing is implemented, and the measurements
were taken through the built binary under a synthesized qualification identity
that authenticates nothing. Promotion to Accepted requires evidence for each
claim.

Plan: `docs/superpowers/plans/2026-07-27-q13-node-span-coverage.md`.
Design: `docs/superpowers/specs/2026-07-26-parser-rq-corpus-tiering-design.md`.
