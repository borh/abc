# Parser-RQ Evidence Retention Readiness

The portable parser-RQ implementation may merge, but the authoritative campaign
must not mint its sole authorization until all three records below are supplied:

- the reliable evidence-store service or operating procedure covering the
  configured `evidence_store_root`;
- the external backup procedure and accountable operator;
- the date and result of a successful restore test applicable to that store.

These records are operational prerequisites, not parser qualification evidence.
They must never be copied into candidate, readiness, authorization, capture,
evaluation, admission, or promotion identity.

The adjacent `parser-rq-evidence-retention.json` is the closed operational stop
consumed by the production orchestrator. It does not prove that a backup or
restore occurred; it makes the named operator's authorization explicit and
fail-closed. The orchestrator refuses production while that record is blocked.
The JSON record is the sole source of operational status and ownership; this
runbook deliberately does not duplicate those mutable values.
