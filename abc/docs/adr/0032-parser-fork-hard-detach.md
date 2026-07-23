# ADR 0032: Parser Fork Hard Detach

Status: Accepted
Validation scope: full-corpus
Release authority: development
Date: 2026-07-10
Accepted: 2026-07-10
Supersedes: none
Amended by: ADR 0038
Amends: ADR 0030
Depends on: ADR 0030
Source: `ab-validator/docs/superpowers/specs/2026-07-10-consolidated-parser-design.md`

## Implementation Status

Accepted and implemented as of 2026-07-10. The `ab-aozora-*` crates are
lifted into `ab-validator/crates/` with provenance headers recording the
upstream pin and this ADR; the parity shim in `ab-aozora-cli` reimplements
upstream's `aozora inspect` dispatch over the lifted `ab-aozora-facade`.
Gate B (`FORK_CONFORMANCE_PARITY_CONFIRMED`) and Gate A
(`FORK_PARITY_CONFIRMED`) confirm byte/semantic parity with the pinned
upstream at rev `1a4f864603970983719655aa4af4525958ac2d38`, and the perf
gate confirms no regression beyond threshold; see
`ab-validator/docs/superpowers/reports/2026-07-10-fork-parity-conformance.md`,
`ab-validator/docs/superpowers/reports/2026-07-10-fork-parity-perf.md`, and
`ab-validator/docs/superpowers/reports/2026-07-10-fork-parity-corpus.md`.
`ab-validator/docs/handoffs/2026-07-10-parser-fork-provenance.md` records
the detach-rev provenance, lifted-crate closure, and test inventory.

## Context

ADR 0030 selected `aozora-pipeline` (`P4suta/aozora`) as the consolidated
parser base with an upstream-first, fork-fallback engagement model. Design
brainstorming for the fork (2026-07-10) chose a different ownership model:
a library fork living in the `ab-validator` workspace, hard-detached at the
measured pin, with no future upstream merges.

The decision is a maintenance-economics tradeoff, not a measurement
problem (a pinned revision does not move). The two candidate models:

- **Merge relationship**: track upstream releases; every merge re-imports
  an actively developed ~90k-LOC workspace under our gates and forces a
  full re-measure cycle (the 2026-07-08 study's §7 controlled re-pin cost
  a session even for a measurement-equivalent bump). Our Level-3
  publication structures modify parse output invasively, so merges would
  conflict in exactly the code we change most.
- **Hard detach with selective porting**: no standing merge burden; an
  upstream change we want is a deliberate, reviewed patch with its own
  re-measure. Cost is forfeiting upstream fixes by default and owning all
  future parser work with current staffing (a single maintainer, working
  in bounded sessions).

Expected divergence favors detaching: the fork's roadmap (native AAT
emission, source-region model, Level-3 structures) rewrites the projection
surface upstream has no reason to accept wholesale, while the parsing core
we inherit is already corpus-clean at the pin (coverage 0.969, zero
timeouts). The porting lane stays open and cheap relative to merging.

## Decision

- The consolidated parser is a hard fork of `P4suta/aozora` at rev
  `1a4f864603970983719655aa4af4525958ac2d38` (the measured pin of the
  ADR 0030 evidence). No future merges from upstream.
- ADR 0030's upstream-first engagement model is replaced: upstream
  contributions are no longer part of the parser plan. If a later upstream
  change is wanted, it is ported as a reviewed patch with re-measurement,
  not a merge.
- Lifted crates live in `ab-validator/crates/` under `ab-aozora-*` names
  (`aozora` umbrella → `ab-aozora-facade`), inside the workspace gates.
  Attribution is preserved: the upstream `NOTICE` file travels with the
  lifted code and each lifted crate records the upstream repository and
  rev in a provenance header. Upstream license is `MIT OR Apache-2.0`,
  matching the workspace.
- **Revisit trigger:** re-open this decision by superseding ADR if either
  (a) upstream ships a capability the acceptance criteria need whose port
  is estimated at more than two focused sessions, or (b) fork-only
  maintenance (excluding planned roadmap work) exceeds roughly one session
  per month over a quarter.
- All other ADR 0030 decisions (selection evidence, candidate dispositions,
  selection-is-not-admission, reversibility via adapter/mapping
  coordinates) are unchanged.

## Consequences

- Upstream fixes and features after the pin are forfeited by default;
  porting one is a deliberate, measured act.
- The fork's identity chain starts at the pin: parser identity is carried
  by the `ab-aozora` adapter/version coordinates under ADR 0023, exactly as
  for any adapter.
- The `upstream-aozora-src` flake input remains until the legacy comparison
  lane is retired after Phase 4 admission (design: Legacy Lane Retention).

## Acceptance Criteria

- **ADR-0032-C1 — structural-invariant:** ADRs 0030, 0032, and 0038 carry reciprocal amendment links. Evidence boundary: `test/abc/tools/parser_relations_provenance_evidence_test.clj`.
- **ADR-0032-C2 — structural-invariant:** The fork-provenance handoff, lifted-crate provenance headers, upstream NOTICE, and ADR record the same detach repository, revision, and licence boundary. Evidence boundary: `test/abc/tools/parser_relations_provenance_evidence_test.clj`.

## Future Verification

Maintenance economics and maintainer capacity remain future verification
conditions for a superseding ownership decision.

## Rollback

Superseding ADR re-establishing an upstream relationship (or a different
base per ADR 0030's rollback path); no registry, manifest, or identity
change is implied by this ADR itself.
