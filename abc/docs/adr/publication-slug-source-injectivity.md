# Publication Slug Source Injectivity

## Implementation Status

Accepted on 2026-07-25, after the defect was reproduced at full-corpus scale,
the fail-closed guard and index check landed with tests, and the identity
change was verified over the whole selected corpus.

## Context

The work slug is publication identity: it names the single directory
`works/<slug>/` and `publications/<slug>/` that a source materializes into.
It was derived from `(work_id, person_id, zip basename)`, discarding the
directory the ZIP lived under.

That derivation is not injective over Aozora. Aozora files one `work_id`
under several contributor card directories, and `person_id` comes from
primary-person metadata under catalog last-row-wins, so it names neither
directory — `062694_002402_…` was selected from both `cards/001085/` and
`cards/002385/`, and `002402` is neither. Catalog matching keys on the ZIP
basename alone, so one catalog row matched every copy. The directory was the
only distinguishing element, and it was thrown away.

A full-corpus run on aozorabunko `0e9ea3e586eb0aa34039fabfc85a407d2f98b165`
selected 17,602 sources and produced 17,595 output directories. **Seven
publications were silently destroyed** — the second materialization of a slug
overwrote the first's artifacts.

Only four of the seven were detectable downstream, and the reason matters for
where the check belongs. Artifact references are captured per manifest write,
so both writes appended references; but `sort-artifact-references` applies
`distinct` before the index is stored, so byte-identical duplicates collapsed
before any verifier could see them. The arithmetic confirmed it: 17,595 × 4 =
70,380 expected references, the index held 70,396, and the difference of 16 is
exactly the four content-differing collisions × 4. The remaining three
produced **no diagnostic of any kind**.

Concurrency was ruled out as the initiating cause: the failure reproduced
bit-identically across two runs with different corpus snapshot hashes.
Content-identity deduplication was ruled out as a remedy because the four
content-differing pairs have different `work_content_hash` and bundle
identities — collapsing them would discard distinct logical works. (Differing
raw ZIP bytes would have been weak evidence, since archives can differ through
packaging alone; the derived content identities are the load-bearing fact.)

## Decision

The card directory is part of publication identity. The slug is
`work_id`_`person_id`_`card_directory`_`zip_stem`, injective by construction,
and derived from one source's own coordinates alone.

Three properties are deliberate:

- **Unconditional, not per-collision.** A discriminator appended "only when a
  collision is detected" would make a work's identity a function of the entire
  selected set, so an unrelated source entering or leaving the corpus could
  rename a publication. Identity must never depend on corpus membership.
- **No governed exception registry.** The alternative — a committed list of
  colliding coordinates with assigned slugs — touches fewer identities but
  creates permanent governance: a new entry every time upstream Aozora
  introduces a collision. That cost is perpetual; the rotation cost was
  one-time and, at the moment of this decision, zero.
- **Fail closed on unslugifiable input.** A relpath naming no card directory
  raises rather than producing a slug that cannot address a unique source.

Two independent guards defend the invariant. Candidate slug uniqueness is
asserted **before any slug-addressed write**, because `inspect-selected-work!`
writes `works/<slug>/` from inside the parallel selection map — a check placed
after selection would run after the colliding writes. Separately,
snapshot-index validation rejects duplicate slug claims in
`source_selection_identity_object.sources`, the only index location that
retains that evidence for both collision classes.

The admission rule is explicit: a catalog-backed candidate claims identity
*before* archive inspection. An unreadable archive does not withdraw the claim,
so `continue_on_failure` never resolves a slug collision. This keeps identity
independent of transient archive health, and prevents publication membership
from changing when corruption is later repaired.

## Consequences

Every slug changes. This was affordable only because no work slug was
published or committed anywhere at the time of the decision: rights
publication was `:blocked-pending-assessment-migration`, so no run had ever
been admissibly published, and no committed manifest, snapshot index, or
baseline embedded a corpus slug. The immutability constraint that would
normally make this a migration was, for real slugs, vacuous.

**That window is now closed.** After a real publication ships, changing the
slug formula becomes a genuine full-corpus identity rotation against published
manifests, and the governed-registry alternative becomes the cheaper option
permanently. Any future collision class must be resolved that way, not by
amending the formula again.

The candidate uniqueness guard becomes defence in depth rather than a filter
that fires in normal operation: the slug is now injective in the relpath
alone, and distinct sources have distinct relpaths, so no distinct pair of
candidates can collide. It is retained because it converts any future
weakening of the formula into a loud failure instead of a silently overwritten
publication.

Snapshot-index validation now rejects indexes that are schema-valid under
`0.2.0` and that an earlier validator accepted, without a schema version bump.
This is integrity hardening rather than a contract break: no correct index
could ever have contained duplicate source slug claims, so nothing legitimate
is newly rejected.
