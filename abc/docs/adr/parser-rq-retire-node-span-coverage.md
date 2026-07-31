# Retire Parser-IR Node-Span Coverage

## Implementation Status

**All six tasks done** (2026-07-27). The dead Clojure is deleted, the v1 schemas
are frozen, the work record is at
`abc/parser-rq-source-accountability-work/v2`, the P1 aggregate is withdrawn,
and the rotation is re-measured on the governed corpus under the promoted
identity.

Measured, not assumed: `qualification_identity_ref` is **unchanged** at
`sha256:8c1716f0…d960d366`; `membership_ref` moved from `sha256:746b9989…6d2e78`
to `sha256:93e9dbd4…39c9f18`; the per-work record went 1,524 → 1,225 bytes; and
recognition still returns `ok` on all three works across the rotation.

The re-measurement also put a number on what was being published. On the same
three works the release-authoritative recognition instrument reports 177 of 177
bytes recognized with a zero semantic gap, while the retired quantity reported
75 of 177 covered — a disagreement of more than 2×, between two instruments
whose intervals were both labelled `decoded_utf8`, that nothing ever compared.

The 9.80 ms/work cost saving was **not** re-measured and is therefore **not
claimed**. See the plan's *Task 6 result*.

Diagnosis and enumeration:
`docs/superpowers/plans/2026-07-27-q13-node-span-coverage.md`.
Task sequence: `docs/superpowers/plans/2026-07-27-q13-implementation.md`.

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

So the retirement removes the span union and the coverage fields — and, decided
2026-07-27, the record's `eligible`/`ignored` fields and the P1 aggregate with
them. Those exist to support a computation that is going away; keeping them would
preserve the shape of a measurement without the measurement, and would hand the
region partition a whole-file eligibility it must then undo. P1 becomes a
provenance-and-authentication record.

It retains the membership derivation and the per-work authentication that index
carries: parser-IR `schema_id` and `schema_hash` against the qualification
identity, and `derived_from` against its AAT and mapping coordinates.
`taxonomy_version` and `taxonomy_hash` stay too — they record which taxonomy was
in force, which is provenance rather than measurement.

An earlier statement of this decision said "drop the observation and retire the
`NodeSpans` analyzer path." That was too broad — it would have taken the
membership index and the parser-IR authentication with it.

## Consequences

**This is a versioned instrument change, not a deletion.** Changing what the
membership index contains rotates `membership_ref`, and therefore every
recognition record. Prior captured evidence becomes protocol-incompatible rather
than merely stale — measured 2026-07-27 as `sha256:746b9989…6d2e78` →
`sha256:93e9dbd4…39c9f18` on the governed corpus. The wire enums are closed with
`deny_unknown_fields`, so a v2 reader cannot read a v1 work record and a v1
reader cannot read a v2 one. There is no reinterpretation path, only re-capture.

**The qualification identity does not rotate.** Traced through the predicate set,
then observed: the same identity file yields the same `identity_ref` on both
sides of the change. No capture becomes stale *against the identity*.

**Published artifacts are not rewritten.** Counted 2026-07-27 during
implementation: **18** artifacts under `docs/reports/parser-rq/runs/` carry a v1
P1 wire version — 9 work records, 3 aggregates and 6 indexes, distributed
exactly 6 per run across three runs, one of which is the promoted
`24d61fc7…`. Of these, the **9 work records** are the ones carrying the
`parser_ir.nodes[*].span` basis. An earlier draft of this record said
"twenty-two artifacts carry the basis"; that number was not measured and is
wrong on both the count and the population it counted.

Published manifests are immutable. **Decided 2026-07-27: the v1 schemas are
retained frozen alongside v2**, so those artifacts stay validatable. Deleting
them would leave published evidence that nothing can check.

The freeze is `schemas/parser-rq-source-accountability-work-v1.schema.json` and
`-aggregate-v1.schema.json`: standalone copies, not `$ref` aliases, since an
alias would track whatever the live schema became and would not be a freeze. The
index schema is **not** frozen, because `RecordIndex` never carried a coverage
quantity and its shape does not move; the live schema keeps validating both
sides of the change.

A frozen schema has no live producer to keep it honest, which is how one quietly
stops matching what it claims to validate. Two tests hold it:
`published-v1-evidence-still-validates-against-the-frozen-schemas` validates all
18 published artifacts and asserts the 9/3/6 counts, so a vanished artifact
fails rather than shrinking the validated set to nothing; and
`the-frozen-v1-schemas-are-frozen-copies-and-not-aliases` asserts each still
requires the retired fields it was frozen to validate.

**The cost saving is not claimed.** 9.80 ms per work was an estimate attached to
the computation being removed, and it was **not** re-measured: the governed
corpus is 43–76 bytes per work, where process startup dominates, and a
meaningful figure needs a real work and therefore a regenerated parser-IR
against the pinned corpus. The retirement stands on the coordinate defect
alone; the cost argument was always secondary.

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

Implemented 2026-07-27. Claims c3 and c6 now carry evidence paths. Three claims
still do not, and the reason differs by claim:

- **c1** (what the quantity measured) rests on probe measurements taken through
  the built binary under a **synthesized** qualification identity that
  authenticates nothing. The task 6 re-measurement was taken under the real
  promoted identity, but it measures the *replacement*, so it evidences c6
  rather than c1. Re-evidencing c1 would mean rebuilding the retired instrument
  to measure it again, which is not worth doing to document a defect that no
  longer ships.
- **c2** (the coordinate collision) is a source-trace across two crates. Its
  support is the citations in this record, not an executable check.
- **c4** and **c5** are the decision and its scope.

The Rust tests that hold most of this — `tests/membership_seam.rs`,
`tests/analyze_work.rs`, `tests/corpus_index.rs`,
`tests/fixture_capture.rs` — cannot be cited as evidence paths: the governance
schema admits only `test/`, `fixtures/`, `nix/` and `docs/evidence/external/`
prefixes, and those files live under `ab-validator/crates/`. That is a gap in
what this corpus can reference, not an absence of evidence, and it is worth
naming rather than working around.

Promotion to Accepted requires evidence for **each** claim, so this record is
not promotion-ready as it stands.

Plan: `docs/superpowers/plans/2026-07-27-q13-node-span-coverage.md`.
Design: `docs/superpowers/specs/2026-07-26-parser-rq-corpus-tiering-design.md`.

## Note 2026-07-31 — disposition of the c7 emitter question

Claim c7 left the mixed-coordinate parser-IR emitter question to its own
owner. The owner's disposition, traced before deciding: the parser-IR
schema's `span.coordinate_system` is `const: "decoded_utf8"` with a
description that promises decoded-source byte offsets, so *any* truthful
relabel of the accumulator spans is a `parser_ir_schema_hash` rotation —
which moves the ADR-0023 admission tuple and the publication chain behind
it. The two point fixes that avoid the rotation are both rejected: making
the source-note arm use accumulator coordinates would delete the only spans
in the document that satisfy the schema's stated semantics to make a false
label uniform, and re-deriving every node span over genuine source offsets
was already rejected by c4 on its own motivation.

The resolution is therefore sequenced, not skipped: the coordinate
vocabulary split — a span coordinate that distinguishes emitted-text offsets
from decoded-source offsets — rides the next planned parser-IR schema
rotation rather than a point fix. Until then the divergence is contained:
Q13's retirement removed the last consumer that unioned across the two
coordinate systems, the source-note arm keeps its genuine decoded span, and
no new consumer may read `nodes[*].span` as decoded-source offsets — the
release-authoritative source measure is the classified-source ledger, not
parser-IR node spans.
