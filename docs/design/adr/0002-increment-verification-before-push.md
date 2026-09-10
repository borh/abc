# 0002: Verifying the candidate commit against the head verified alongside it

Status: accepted, 2026-09-10. Owner decision, taken pre-genesis. Amends §9
step 5 only; no wire format, schema or conformance vector changes, and nothing
a third-party verifier does changes.

The number 0002 was briefly used by a draft that proposed three open decisions
rather than recording a made one. That draft was withdrawn to the issue tracker
and deleted; only this record was ever accepted under the number.

## Context

The transaction verifies twice. Step 2 fully verifies the fetched head C, which
is how the head is established at all. Step 5 then verified the newly written
candidate C′, and the wording named the §8 primitive, which walks the chain from
C′ back to genesis. The second walk therefore re-checked a prefix the same
process had verified moments earlier, under the same pinned keys, and whose
result it was still holding.

Measured at 17,602 works over a ten-release chain, that doubled how fast
publication cost grows with chain length: 6.3 seconds per additional release
against 3.2 for a single verification. The cost compounds, because building a
chain pays it at every length. For the intended per-commit operation over the
upstream history it is the difference between roughly nine years of compute and
roughly five and a half.

## Decision

Step 5 may establish the §8 invariants for C′ alone against step 2's result,
instead of applying the primitive to C′.

The permission is narrow and the text says so. It binds to C's exact commit id
and to the head C carries; it must establish at C′ everything the primitive
would; and it is available only to a party holding its own §8 result for C. A
verifier presented with a repository holds no such result and must walk.

## Why this is not a weakening

A commit id is a content address over its whole history: a commit names its
tree, a tree names its blobs, and a parent id names everything behind it. The
prefix cannot differ without C differing, and the check is bound to C's id.
Content addressing is what makes the carried-forward result a statement about
the repository rather than about a moment in time, so the usual objection to
caching a verification, that what was verified may since have changed, does not
arise.

The two documented ways around content addressing are already closed by §8's
own view contract, which disables replacement refs and rejects alternate object
directories at construction.

## Why not leave the wording and record an interpretation

Step 5's sentence names a procedure before the colon and states a requirement
after it, so reading the requirement as controlling was available, and §5's
`not-evaluated` reading is precedent for interpreting frozen text without
changing it. It was rejected here because that precedent's record did not
survive: the ledger holding it was deleted and the interpretation had to be
recovered from Git history. A reader comparing this specification against the
implementation would find an apparent contradiction and no way to resolve it
from the repository. Amending the text costs one pre-genesis edit and leaves
nothing to recover.

## Accepted consequences

- The specification now describes two ways to satisfy step 5 rather than one,
  and an implementation may be read against either.
- The narrow form is available to exactly one party, the publisher, in exactly
  one place. Any other use is out of specification.
- Implementations must keep the two paths checking the same things. The
  reference implementation shares the per-commit check between them rather than
  restating it, after an equivalence test found the restated version rejecting a
  zero head reappearing after genesis for a different reason than the walk.

## Alternatives rejected

- **Revert to the full walk.** Specification and implementation agree with
  nothing recorded, at roughly nine years of compute for the backfill instead of
  five and a half.
- **Fold the shortcut into the §8 primitive**, so step 5 keeps its wording.
  This buries a shortcut whose soundness depends on the caller having verified
  the prefix itself inside the function third parties call to verify from
  scratch. A separately named operation, documented as not a trust primitive, is
  harder to misuse.
