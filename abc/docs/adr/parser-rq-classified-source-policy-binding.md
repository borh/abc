# Bind the Classified-Source Policy into the Qualification Identity

## Implementation Status

Implemented and covered by tests. Recorded as Proposed rather than Accepted:
acceptance is a governance act, and the record is offered for promotion rather
than asserting it.

This record amends
[parser-release-instrument-bindings](parser-release-instrument-bindings.md) in
one scope — the `source_recognition` instrument policy closure — so that the
documented authority matches the instrument's actual authority.

## Context

`instrument_policy_hashes` binds each instrument's governed policy document by
content, so that the qualification identity names what changed rather than
rotating opaquely. For `source_recognition` it bound
`data/parser-rq-ignored-regions-v1.json` — the ignored-regions taxonomy, whose
`rules` array is empty and which therefore exempts nothing.

The document that actually decides `source_span_coverage` was not bound.
`data/parser-rq-ab-aozora-classified-source-v1.json` carries the 31 rules that
decide which eligible bytes are recognized. The parallel diagnostic-gap policy
**was** bound, so this was an inconsistency within one mechanism rather than a
uniform omission.

This is worth stating precisely, because the obvious framing is wrong. **A policy
edit could not drift silently.** `parser_git_rev` is a field of the identity, the
capture binary embeds the policy with `include_bytes!` so its sha256 moves, and
`parser-rq-classified-source-authority-v1.json` pins the policy by
`raw_bytes_hash` and `identity_hash` and fails closed. Any edit rotates the
identity.

What the identity could not do was *name* what changed. It rotated on every
commit for any reason, so a recognition-policy amendment was indistinguishable
from an unrelated one. That is an attribution defect, and it matters most exactly
when a recognition-policy amendment is imminent.

## Decision

Bind the classified-source policy's canonical bytes into
`instrument_policy_hashes[:source_recognition]`, alongside the taxonomy it
already bound, and do it **before** any recognition-policy amendment.

`instrument-policy-paths` widens from member → one path to member → an ordered
vector of paths. The wire shape is unchanged:

- a single-document member retains its document hash **byte-for-byte**;
- a multi-document member folds its documents' hashes in declared order.

So `parser-rq-candidate.schema.json` is untouched, and only one member coordinate
moves.

The bound value is the policy's canonical bytes, not its authority document. The
pin and the content stay one coordinate; binding the authority document would put
a level of indirection between the identity and the rules that decide the
measurement.

## Consequences

`qualification_identity_ref` rotates. Captures made against the prior identity
are stale. **That is the intended consequence** — it is what makes the later
recognition-policy amendment attributable to the recognition policy rather than
to an unrelated commit.

Measured over the live tree, exactly one member coordinate moved; the other six
are unchanged.

Ordering it before the region-partition work was deliberate. Binding after that
amendment would have folded two semantic changes into one rotation, and the
identity would still not have named which one moved the number.

## Evidence

- `test/abc/tools/parser_rq_campaign_test.clj` — membership test asserting the
  single-document case is byte-identical and checking the multi-document fold;
  and `classified-source-policy-edits-rotate-the-qualification-identity`, which
  applies a representative policy edit and asserts that at the same commit and
  parser revision both `instrument_policy_hashes["source_recognition"]` and
  `qualification_identity_ref` move while `predicate_set_hash` does not.

Design: `docs/superpowers/specs/2026-07-26-parser-rq-corpus-tiering-design.md`.
