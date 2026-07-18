# ADR 0042: Separate Parser-RQ Evidence Integrity from Storage Retention

Status: Proposed
Date: 2026-07-18
Depends on: ADR 0039 [scope: release qualification evidence integrity], ADR 0041 [scope: fixed parser release instruments]
Validation scope: structural
Release authority: development

## Implementation Status

The target contract is approved but not yet implemented. ADR 0039 and ADR 0040
remain Proposed, and no authoritative parser-RQ capture exists.

## Context

The execution-readiness protocol braided parser evidence authentication with a
named host, mount topology, and two-path replication. Reliable storage and
external backup already own retention; parser qualification must authenticate
referenced bytes without defining backup infrastructure.

## Decision

The target contract deletes committed site identity and application-level
replication. Runtime configuration supplies four paths. Readiness binds the
candidate, provenance, graph, corpus, and clean revisions without binding those
paths. Promotion requires exact closed membership and a streaming SHA-256 and
byte-count re-hash from one configured evidence store.

Evidence retention is owned by the repository operator through the operational
record at `docs/reports/parser-rq-evidence-retention.json` and its adjacent
runbook. Missing ownership, procedure, or restore evidence blocks production
execution but is not a parser qualification verdict or identity input.

This stop is enforced by the production orchestrator through a separate closed
operational record. The record does not prove an external restore; it makes the
named operator's authorization explicit and fail-closed without entering any
qualification identity. The orchestrator authenticates stored content, while
the named operator owns the external retention procedure and restore evidence.

## Consequences

The unprovisioned replica ceases to be a qualification blocker as a consequence,
not as the motivation. Predicate, provenance, corpus, admission, and canonical
generation semantics remain unchanged. Historical hinoki observations remain
historical facts.

## Acceptance Criteria

- **ADR-0042-C1 — structural-invariant:** Active parser-RQ contracts contain no
  machine identity, filesystem topology, site policy, or replication protocol.
- **ADR-0042-C2 — fixture-behavior:** One-store evidence verification rejects
  missing, escaping, truncated, extra, or hash-mismatched closed members.
- **ADR-0042-C3 — structural-invariant:** Authorization binds portable readiness,
  and promotion requires the self-authenticating closed evidence-integrity
  receipt while predicate, provenance, and admission contracts remain unchanged.
