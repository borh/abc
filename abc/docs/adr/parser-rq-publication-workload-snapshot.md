# Bind Tier 2 to the Publication-Workload Snapshot

## Implementation Status

Decided 2026-07-27 (the corpus-tiering design's Q11); recorded 2026-07-31 by
transcription from the design's *Q11, decided* section, which is that
decision's own written reasoning. Nothing is implemented: the tier-2 corpus
does not exist yet, and every claim below describes the decided contract
rather than built behaviour, which is why the record is Proposed.

## Context

The three-tier corpus architecture needs a population for tier 2, and the
design surfaced two candidates: publication's successfully-selected source
projection, or the pre-attempt catalog-backed candidate population with
selection failures retained as qualification failures. The objection to the
first is denominator shrinkage — a `continue_on_failure` regression could
quietly shrink `selected` while every ratio predicate stays at 1.0. The
objection to the second turned out to be fatal rather than aesthetic.

## Decision

Tier 2 binds to publication's successfully-selected projection and is named
the **publication-workload snapshot**. The pre-attempt population *cannot*
supply corpus-entry identity for exactly the members that distinguish it: a
corpus entry needs `:source_sha256` and a primary-text member, both produced
by `source-bundle/inspect-zip` inside `inspect-selected-work!` — precisely
the fallible step selection wraps. A derivation-failed member has no source
hash *because it failed*, and inventing an absent-hash representation would
weaken `corpus-snapshot-hash` for every other entry.

The shrinkage objection is answered by attestation instead: the tier-2
report carries its own declared denominator — `candidates_considered`, the
`derive_failures` set, the `rejected` set with reasons, and (added
2026-07-31) `capture_refusals` for works that produce no capture record at
all. All are projections of data the pipeline already produces. The
denominator never shrinks silently; it is either visible in the report or
moved by a governed corpus-entry decision that rotates `corpus_list_hash`.

## Consequences

Q9's equality half becomes near-tautological — authenticate that the corpus
was projected from `source_selection_hash` rather than re-derived. Its
substantive half is unchanged: the accounted set difference against the
source-bundle admission universe and the conversion-audit population, where
the 17,878 / 17,880 / 17,886 spread has to be explained. Once proven, the
claim is a census of one snapshot coinciding with an operational claim about
the publication workload; it expires when `aozorabunko-src` is bumped.
