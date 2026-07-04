# ADR 0005: Operational Runtime, API, and Retention

Status: Draft
Date: 2026-04-26
Supersedes: none
Source: `docs/high-level-architecture-note.md` v0.5

## Implementation Status

Still Draft. The v0 file/CLI surface, run-summary schema, and local validation
gate exist, but the broader operational runtime choices, full-corpus storage
evaluation, and distributed publication policy remain undecided.

2026-07-04 evidence note: `docs/handoffs/measurement-probes-2026-07-04.md`
keeps bounded Nix materialization viable but does not decide storage packing,
query runtime, API/service shape, retention policy, or multi-host publication.
The split proposed in `docs/handoffs/bounded-workset-index-design.md` remains
the recommended next ADR shape: separate reproducibility materialization from
operational orchestration, storage/packing, query runtime, API/service, and
retention/archive decisions.

Follow-up query-runtime note:
`docs/handoffs/query-runtime-and-history-index.md` separates completed XTDB v1
retirement from query-runtime selection. It records a small SQLite-vs-DuckDB
query-pack probe and recommends keeping generated indexes as derived views over
canonical files until an ADR accepts a concrete runtime. TEI generation remains
part of parser-IR publication rendering, not this query-runtime replacement
track.

## Context

ABC may eventually use Rust parsers, JVM validation, RDF tooling, Nix, query
indexes, and ML workflows. v0 should avoid requiring the entire stack for the
smallest example bundle.

## Decision

The v0 operational surface is file and CLI oriented:

- Canonical artifacts are ordinary files plus JSON manifests.
- RDF, query index entries, and RO-Crate packaging are derived views.
- REST, gRPC, authenticated query services, distributed scheduling, and
  multi-host publication are deferred.
- If Jing/JVM validation is used, the v0 orchestrator must batch validation or
  use a persistent validation worker. One JVM startup per work is not
  acceptable at corpus scale.
- Multi-host workers require a future compare-and-swap or database-backed
  publication protocol; rename-based publishing is single-host only.

## Observability

Every batch run should produce a machine-readable run summary containing:

- works considered,
- works changed,
- parse failures,
- TEI validation failures,
- warnings and errors by severity/code,
- artifact counts and sizes,
- parse/render/validation durations,
- cache hit rate where available,
- tool versions.

The v0 run summary format is JSON Lines: one `run-start` event, zero or more
`work-result` events, and one `run-complete` event. The minimal v0 event
contract is `schemas/run-summary.schema.json`; future orchestrators may extend
it only by versioning that schema.

## Retention

- Hot: local development outputs may be deleted when rebuildable.
- Warm: shared caches retain common artifacts subject to storage budgets.
- Cold: recipes, manifests, and source locks are retained while outputs may be
  regenerated.
- Archived: release manifests, schemas, canonicalization fixtures, and archive
  identifiers are retained indefinitely.

## Acceptance Criteria

- The example bundle can be produced without a database service.
- CI smoke corpus target runtime is under five minutes on baseline hardware.
- Failure manifests survive even when failed outputs do not exist.
- Failure manifests are written under the same manifest output root as
  successful artifacts and are indexed by `artifact_id`, `work_content_hash`,
  and failure diagnostic code where available.
- Full-corpus storage design evaluates loose files against batched storage.
- The operational design explicitly documents that rename-based atomic
  publishing is single-host only.

## Rollback

If file-oriented orchestration becomes too brittle, promote a workflow engine
or service runtime by a new ADR, while keeping canonical manifests portable.
