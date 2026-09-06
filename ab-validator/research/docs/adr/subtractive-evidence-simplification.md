# Subtractive Evidence Simplification

## Implementation Status

Accepted on 2026-07-23 after the typed-evidence apparatus was removed, every
retained criterion was re-pointed at its ordinary executable evidence, and the
strict governance validator passed over the full corpus.

## Context

ADR 0034's typed-evidence protocol grew to roughly ten thousand lines of
governance Clojure and three hundred registry, capture, and bootstrap
artifacts. Inspection of the final registry showed the model was ceremony:
every one of its 177 expected values was boolean `true`, claim kinds and
evidence kinds were pairwise redundant in practice, operational bundles
recorded only exit codes, and the bootstrap snapshot re-implemented a git
tree. The registry re-proved what the auto-discovered test suite already
executes, while the gate evaluated rather than ran its checks.

## Decision

Accepted ADR criteria are backed by ordinary, auto-discovered executable
evidence, not by a typed evidence registry:

- Each Accepted criterion cites its evidence as a repository path under
  `test/`, `fixtures/`, `nix/`, or `docs/evidence/external/`, and
  `abc.tools.adr` checks that every cited path exists. Whether a cited test's
  assertions entail the criterion remains a human review judgment.
- Execution is enforced by running the cited suites and checks through the
  standing gates, not by storing per-claim results.
- Domain measurements are preserved as ordinary committed artifacts under
  their own schemas, beside the ordinary tests that validate them.
- Human-source evidence, such as the custom-parser ownership assessment, is
  immutable historical evidence; git records its state. Amending ADR 0038:
  the assessment's `Review after` date is a prose obligation on the project
  owner, not a machine-enforced expiry, and the committed governance epoch
  file is removed with the registry.
- Claim headers keep the `ADR-NNNN-CN — kind` syntax, but the claim-kind
  vocabulary is prose only; no closed kind set is enforced.
- The typed-evidence registry, claim/evidence compatibility matrix,
  governance epoch, capture and replay apparatus, claim-migration ledger, and
  bootstrap snapshot are deleted without replacement.

## Consequences

- Evidence citations live only in ADR prose and are path-checked; there is no
  second registry to keep synchronized.
- Deleting or moving a cited test fails governance immediately, which is the
  intended replacement for registry freshness bookkeeping.
- Historical typed-evidence artifacts remain reachable in git history; no
  promotion proof is retained in the working tree.

## Rollback

Reintroducing a typed evidence protocol requires a new ADR superseding this
one; the deleted apparatus remains available in git history.
