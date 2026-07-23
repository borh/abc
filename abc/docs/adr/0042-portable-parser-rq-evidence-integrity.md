# ADR 0042: Keep Parser-RQ Evidence Portable and Self-Contained

Status: Accepted
Date: 2026-07-18
Accepted: 2026-07-18
Depends on: ADR 0041
Validation scope: structural
Release authority: development

## Implementation Status

Portable contracts, one-store evidence verification, promotion integration,
the active-surface guard, and bounded governance evidence are implemented. ADR
0039 and ADR 0040 remain Proposed, and no authoritative parser-RQ capture
exists.

## Context

The execution-readiness protocol braided parser evidence authentication with a
named host, mount topology, and two-path replication. Parser qualification must
instead authenticate the complete value closure needed to reconstruct its
decision without defining storage infrastructure.

## Decision

The target contract deletes committed site identity and application-level
replication. Runtime configuration supplies four paths. Readiness binds the
candidate, provenance, graph, corpus, and clean revisions without binding those
paths. Promotion requires exact closed membership and a streaming SHA-256 and
byte-count re-hash from one configured evidence store.

Evidence containment is described by
`docs/reports/parser-rq-evidence-containment.md`. Every release-relevant value
is committed, pinned, or manifest-addressed below the configured evidence
store. Promotion re-hashes that closed set. No result depends on an undeclared
path, operator workstation, or third-party storage service.

## Consequences

The unprovisioned replica ceases to be a qualification blocker as a consequence,
not as the motivation. Predicate, provenance, corpus, admission, and canonical
generation semantics remain unchanged. Historical hinoki observations remain
historical facts. Production has no separate retention declaration or storage
workflow in its protocol.

## Acceptance Criteria

- **ADR-0042-C1 — structural-invariant:** Active parser-RQ contracts contain no
  machine identity, filesystem topology, site policy, or replication protocol.
  Evidence: `test/abc/tools/parser_rq_portability_test.clj`.
- **ADR-0042-C2 — fixture-behavior:** One-store evidence verification rejects
  missing, escaping, truncated, extra, or hash-mismatched closed members.
  Evidence: `test/abc/tools/parser_rq_campaign_test.clj`.
- **ADR-0042-C3 — structural-invariant:** Authorization binds portable readiness,
  and promotion requires the self-authenticating closed evidence-integrity
  receipt while predicate, provenance, and admission contracts remain unchanged.
  Evidence: `test/abc/tools/parser_rq_campaign_test.clj`.

## Evidence

The active surface, promotion, and one-store verifier checks run together in
the standing Kaocha suite (`test/abc/tools/parser_rq_portability_test.clj`,
`test/abc/tools/parser_rq_campaign_test.clj`) and in the
`ab-validator` flake check `parser-rq-campaign-provenance-python-tests`.
