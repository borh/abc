# Parser-RQ Evidence Retention Readiness

Status: Blocked
Owner: Repository operator

The portable parser-RQ implementation may merge, but the authoritative campaign
must not mint its sole authorization until all three records below are supplied:

- the reliable evidence-store service or operating procedure covering the
  configured `evidence_store_root`;
- the external backup procedure and accountable operator;
- the date and result of a successful restore test applicable to that store.

These records are operational prerequisites, not parser qualification evidence.
They must never be copied into candidate, readiness, authorization, capture,
evaluation, admission, or promotion identity.

This file is an operator checklist, not a machine-verifiable receipt. A parser-RQ
program reading `Status: Ready` would authenticate only the operator's label,
not the backup or restore that the label describes. The operator must stop before
candidate freeze while this record says `Status: Blocked`.
