# ADR 0031: Uniform ADR Governance Validation

Status: Proposed
Date: 2026-07-10
Supersedes: none
Amends: ADR 0029 [scope: ADR header and decision-graph source validation]
Depends on: ADR 0029 [scope: generated decision graph contract]
Source: `docs/superpowers/specs/2026-07-10-adr-governance-hardening-design.md`

## Implementation Status

Implementation is in progress under the approved governance-hardening plan.

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

- `test/abc/tools/adr_test.clj` covers parser and policy failures and proves the
  current repository is clean.
- `test/abc/tools/diagram/adr_graph_test.clj` proves the graph consumes shared
  ADR values and preserves scope labels.
- `test/abc/tools/diagram/architecture_graph_test.clj` proves coordinate and
  owner completeness.
- `test/abc/tools/diagram/workflow_graph_test.clj` proves invalid workflow runs
  cannot be rendered.

## Consequences

- ADR authoring is stricter and malformed metadata fails before diagram output.
- The permanent evidence allowlist and shell-only parser are removed.
- Adding a manifest identity coordinate also requires explicit ADR ownership.
- Proposed implementation remains distinguishable from canonical acceptance.

## Rollback

Reverting the gate requires a new ADR; generated diagrams remain non-identity
documentation views.
