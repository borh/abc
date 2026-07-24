# Uniform ADR Governance Validation

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
