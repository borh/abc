# ADR 0031: Uniform ADR Governance Validation

Status: Accepted
Date: 2026-07-10
Accepted: 2026-07-10
Validation scope: structural
Release authority: none
Supersedes: none
Amends: ADR 0029 [scope: ADR header and decision-graph source validation]
Amended by: ADR 0043 [scope: typed evidence registry]
Depends on: ADR 0029 [scope: generated decision graph contract]
Source: `docs/superpowers/specs/2026-07-10-adr-governance-hardening-design.md`

## Implementation Status

Accepted on 2026-07-10 after the shared ADR parser, uniform corpus migration,
Nix governance gate, per-coordinate architecture validation, and workflow-run
validation passed the Acceptance Criteria below.

## Context

The committed Mermaid files are byte-current relative to their immediate
sources, but the current gates can still accept decorated statuses, missing
acceptance dates and sections, permanent legacy evidence exceptions, stale
architecture identity metadata, unscoped dependencies on Draft decisions, and
unvalidated workflow-run input. Byte drift is therefore necessary evidence for
a derived view, but not sufficient evidence of lifecycle or semantic validity.

## Decision

ADR Markdown uses the closed header and relation grammar documented in
`docs/adr/README.md`. `abc.tools.adr` is the sole parser and policy validator;
the decision graph consumes its values. Existing ADRs are repaired before the
legacy allowlist is removed. Accepted ADRs require existing executable
evidence, while Draft and Proposed criteria remain promotion conditions.
Amendment and supersession links are reciprocal and relation scopes use
`[scope: …]`. Architecture identity ownership is recorded per schema coordinate
and checked for totality. Runtime workflow diagrams validate their JSON Schema
and semantic invariants before rendering.

## Acceptance Criteria

- **ADR-0031-C1 — structural-invariant:** The ADR parser and lifecycle,
  relation, dependency, claim-header, and section policies reject their
  enumerated invalid fixtures. Evidence boundary:
  `test/abc/tools/adr_test.clj`.
- **ADR-0031-C2 — structural-invariant:** ADR graph construction consumes the
  shared parsed value model and preserves scoped relation labels. Evidence
  boundary: `test/abc/tools/diagram/adr_graph_test.clj`.
- **ADR-0031-C3 — structural-invariant:** Manifest identity coordinates and
  declared owner references are structurally total for the current contract.
  Evidence boundary: `test/abc/tools/diagram/architecture_graph_test.clj`.
- **ADR-0031-C4 — fixture-behavior:** Schema-invalid and semantically invalid
  workflow fixtures are rejected before rendering. Evidence boundary:
  `test/abc/tools/diagram/workflow_graph_test.clj`.

## Future Verification

Complete typed claim/evidence coverage was owned by ADR 0034 until ADR 0043
superseded the typed-evidence protocol. Under ADR 0043, Accepted criteria cite
ordinary executable evidence paths and the standing gates execute them; no
per-claim coverage registry exists.

## Consequences

- ADR authoring is stricter and malformed metadata fails before diagram output.
- The permanent evidence allowlist and shell-only parser are removed.
- Adding a manifest identity coordinate also requires explicit ADR ownership.
- Proposed implementation remains distinguishable from canonical acceptance.

## Rollback

Reverting the gate requires a new ADR; generated diagrams remain non-identity
documentation views.
