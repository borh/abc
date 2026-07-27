# Partition the Source into Body and Metadata Populations

## Implementation Status

Not implemented. The contract is decided; the task sequence is in
`docs/superpowers/plans/2026-07-27-q15-region-partition.md`. Tasks 1 and 2
(characterize the control, declare the partition) are unblocked and change no
measurement — task 2 only since the region set was closed, which an earlier draft
of this record left open while asserting conservation over it.

## Context

[parser-rq-instrument-before-threshold](parser-rq-instrument-before-threshold.md)
established that `source_span_coverage` divides a body-derived numerator by a
whole-file denominator, and that the instrument is fixed rather than the
threshold. It did not say what the fixed instrument measures.

Two answers were put to the owner. Reduce the denominator to the body projection
the parser was actually given; or extend capture so the header and colophon
produce facts in the same coordinate as body facts. Both are coherent, and they
mean different things by `source_span_coverage = 1.0`.

**Both framings were rejected.** They present the body and the packaging metadata
as competing denominators for one ratio. They are two populations. The body is
the work — prose and annotations. The header and `底本：` colophon are metadata
*about* the work: a notation legend and bibliographic provenance of the
transcription. A single ratio over their union averages parser fidelity against
packaging attribution and can mean neither.

The evidence for that is the history of the number itself. The observed 0.9640
could not distinguish "the parser missed prose" from "packaging is never lexed",
and establishing that it was entirely the latter took three attempts.

## Decision

Partition the decoded source into declared regions and measure the populations
separately, with conjunctive clearance.

| Region | Extent | Measured by |
| --- | --- | --- |
| header | `[0, body_start)` | metadata attribution |
| body | `[body_start, body_end)` | `source_span_coverage` |
| tail | `[body_end, len)` | metadata attribution |

Three regions, no fourth, no unassigned byte. Note the tail begins at `body_end`
and **not** at `aozora_body_range`'s `tail_start` — see *Consequences*.

- Region authority is `aozora_body_range` **alone**. No separator or `底本：`
  heuristic is maintained anywhere else.
- The regions are pairwise disjoint and their union is the whole decoded file.
  The instrument asserts that conservation and fails closed.
- `source_span_coverage` becomes body-projection coverage. It is not a whole-file
  measure and must not be read as one.
- Metadata attribution is a separate predicate over the header and tail in their
  own coordinate. Header and tail are recorded as distinct regions so a failure
  localizes to one end of the file; they qualify under one conjunctive predicate
  rather than one predicate each.
- **A work clears only when both clear.**
- Neither threshold is carried forward by default. Build, measure under an
  explicitly non-authoritative exploratory campaign, then predeclare.

## Consequences

**This is not the denominator reduction Q11 rejected.** No byte leaves the
accounting; the metadata bytes move to a different *accounted* region. The
conservation identity is what keeps that honest, and it is the reason the
partition is worth its cost.

**Conservation becomes assertable, and that is the point.** Every defect traced
in this area shares one property: no invariant existed that could catch it. The
node-span analyzer compares a coordinate label against a value the emitter
hard-codes, so its check can never fire. `Interval::new` bounds-checks and
nothing else. `source_span_coverage` divided across coordinates and returned a
believable 0.9640. A declared partition with a conservation check is the first
structure here that breaks loudly instead of returning a plausible number.

**The cost is both earlier options combined**, plus a partition schema and its
checks. This is not a middle path, and it should not be adopted believing it is.

**`aozora_body_range`'s return value is not itself a partition**, so the region
derivation does not use it verbatim. It returns `(body_start..body_end,
tail_start)` where `body_end` is trim-adjusted for trailing newlines and
`tail_start` is not, so whenever a tail exists at least one byte lies between
them.

The tail region is therefore `[body_end, len)`, extending backward to meet the
body and absorbing the blank lines that separate them. This is symmetric with the
header, which already absorbs its adjacent blank lines because `skip_blank_lines`
advances `body_start` past them.

Two alternatives were rejected. Widening `body_end` *forward* to `tail_start`
puts transcriber whitespace into the body population, making body coverage depend
on how many blank lines a transcriber left; metadata attribution over packaging
whitespace is trivially satisfiable by comparison, since the ledger vocabulary
already carries a `newline` disposition. Changing `aozora_body_range` itself is
rejected because `sanitized.body` is the projection handed to the parser, so
moving that boundary would move `source_span_coverage`'s numerator as a side
effect of a partition fix.

This gap was found by writing the plan rather than by running the instrument —
and an earlier draft of this record asserted whole-file conservation over three
regions while separately conceding a fourth was needed, which review caught. Both
are the argument for declaring the partition explicitly before relying on it.

**Sequencing.** Retiring the parser-IR node-span coverage path removes
`analyze.rs`, `aggregate.rs`, two schemas and four test files from this change
set, so that question is settled first. Mapping the twelve unmapped
`DirectiveKind` variants in `node_policy` is body-side work and a separate
reviewable deliverable, not blocked by the partition.

**Identity.** The predicate contract changes, so `predicate_set_hash` moves.
Prior captured evidence is stale.

## Evidence

No claim of this record carries an evidence path: nothing is implemented. The
measurements motivating it were taken under a synthesized qualification identity
that authenticates nothing. Promotion to Accepted requires evidence for each
claim, the governed three-work corpus as a control, and a real-source sample
re-measured through the built binary.

Plan: `docs/superpowers/plans/2026-07-27-q15-region-partition.md`.
Design: `docs/superpowers/specs/2026-07-26-parser-rq-corpus-tiering-design.md`.
