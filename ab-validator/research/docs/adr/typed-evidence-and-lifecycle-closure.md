# Typed Evidence and Lifecycle Closure

## Implementation Status

The typed claim/evidence vocabulary, deterministic reference date, audit-mode
reporting, explicit lifecycle dimensions, and visited-set dependency closure
are implemented. C1 and C2 are backed by focused structural captures, while C3
is backed by the immutable pre-promotion corpus snapshot. Final live strict
enforcement, including this ADR, is required by the promotion procedure.

## Context

Existing ADR acceptance verifies evidence-looking paths but does not bind each
claim to a compatible evidence kind, observed predicate, input identity, or
freshness date. Before ADR 0029's promotion, ADR 0031 also reached a
non-Accepted decision through a scoped dependency. Conflating decision
canonicity, validation scope, release authority, and evidence strength makes
acceptance appear stronger than the recorded observations warrant.

## Decision

This ADR proposes a closed typed-evidence protocol for ADR promotion. Stable
claim IDs will reference entries in `docs/adr/adr-evidence.edn`; compatibility
will be determined by the checked-in claim/evidence matrix, and verdicts will
be derived rather than stored. External-evidence review will use the committed
governance date, never the process clock.

Accepted ADRs will declare validation scope and release authority separately
from status. Their complete dependency closure will contain only Accepted
ADRs. The production validator will use deterministic ordinary Clojure
algorithms; solver formulations may independently review invariants but will
not become runtime dependencies.

## Consequences

- Acceptance claims become machine-checkable without treating all evidence as
  interchangeable.
- Advancing evidence freshness becomes a visible reviewed data change.
- Audit mode can enumerate migration debt without breaking unrelated derived
  views.
- Enforcement cannot begin until the governance ADR passes its own rules.

## Forward Recovery

If migration must stop, retain audit mode and the information-preserving
registry. Do not restore scoped dependency waivers or path existence as
sufficient evidence for semantic claims.
