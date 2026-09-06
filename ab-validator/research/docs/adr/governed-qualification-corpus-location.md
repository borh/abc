# Locate the Qualification Corpus by Governance, Not Site Configuration

## Implementation Status

Accepted on 2026-07-26. Both corpus-byte captures resolve one root from the
governed corpus artifact, and the site descriptor no longer declares a corpus
locator.

## Context

ADR 0042 removed committed site identity and had runtime configuration supply
four paths: `corpus_root`, `evidence_store_root`, `scratch_root`, and
`campaign_lock_path`. Three of those are genuinely places the operator owns.
The fourth was not.

Nothing ever read corpus bytes from the descriptor's `corpus_root`. Preflight
authenticated it only as "an absolute, existing directory" — an *empty*
directory passed — while the two captures that do read corpus bytes each
restated the corpus location inline, spelled two different ways
(`ab_root / "crates/..."` in core capture, `candidate_tree /
"ab-validator/crates/..."` in resource capture). An operator following the
runbook would point `corpus_root` at the corpus they intended to qualify and
get a campaign over entirely different bytes, with no diagnostic.

This is not a naming problem. The corpus is not a runtime place at all. Which
bytes a release is qualified against is a governed decision, and it is already
recorded as one: `data/parser-release-qualification-corpus.edn` declares a
repository-relative `:corpus_root` and pins every member's path and hash. Every
entry's `:source_path` must begin with that root, and `:source_path` is one of
`corpus-entry-identity-keys`, so the corpus location is bound into
`corpus_list_hash` and through it into the candidate's qualification identity.
Making the same value operator-supplied put a governed decision behind an
unauthenticated runtime knob.

## Decision

The corpus root is resolved once, from the governed corpus artifact, below the
authenticated candidate tree. It is validated as repository-relative, free of
`..`, strictly resolvable, below that tree after resolution, and a directory;
anything else fails closed. Only operations that actually read corpus bytes
resolve it, so no other capture acquires a dependency on a corpus existing.

The site descriptor declares three runtime places and no corpus locator. A
descriptor that still names one is **rejected** by the closed contract rather
than accepted and ignored, so the misreading that motivated this record cannot
recur silently. The descriptor schema is therefore `3.0.0`.

## Consequences

Existing site descriptors stop working and must have `corpus_root` removed and
their `schema_version` raised. That break is the point: a `2.0.0` descriptor
carries an operator's belief about which corpus is qualified, and that belief
was never true. Failing loudly at preflight is preferable to honouring the
other three fields while silently discarding the first.

Full-corpus requalification remains out of reach, but the reason is now stated
honestly. It was never reachable by repointing the descriptor. It requires a
governed corpus change — new members, their pinned hashes, and the rotation of
`corpus_list_hash` and `corpus_snapshot_hash` that follows — together with the
closed-membership instrument policies that pin the same members. Those move as
one governed migration, not as configuration.

No qualification result changes. The resolved path is byte-identical to what
the two retired inline expressions produced for the governed corpus root, so
argv, corpus membership, and every predicate observation are unaffected.

## Evidence

The descriptor contract and its rejection of a corpus locator are checked in
`test/abc/tools/validate_design_bundle_test.clj`. Corpus-derived candidate
identity is checked in `test/abc/tools/parser_rq_campaign_test.clj`. Resolver
behaviour and the characterization against both retired expressions run in the
`abc` flake checks `parser-rq-campaign-site` and
`parser-rq-campaign-orchestrator`.
