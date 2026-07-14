# ADR 0029: Diagrams as Gated Derived Views

Status: Accepted
Date: 2026-07-10
Accepted: 2026-07-12
Validation scope: fixture
Release authority: none
Supersedes: none
Amended by: ADR 0031 [scope: ADR header and decision-graph source validation]
Depends on: ADR 0006
Source: `docs/superpowers/plans/2026-07-10-adr-and-system-diagrams.md`

## Implementation Status

Accepted on 2026-07-12 after the ADR, architecture, workflow, registry, and
renderer test namespaces passed together and the Nix diagram-drift gate
regenerated every committed view without differences. ADR 0031's implemented
header/parser amendment is therefore closed over an Accepted graph contract.

## Context

The ADR set (0001–0028) and schema bundle encode a real system, but there is
no diagram of it anywhere under `abc/`. Two graphs are latent in the ADRs: an
ADR→ADR decision graph (from `Supersedes`/`Amends`/`Amended by`/`Depends on`
headers) and a system-dataflow graph (artifacts/stages keyed to schemas and
identity coordinates). A runtime provenance graph is additionally available per
executed run from `workflow-run.json`. Critically, the strongest *semantic*
relationships — the Hard-Rule restatement across ADRs 0013–0021, the
schema-hash cascade 0015→0016→0017/0018→0020, the drift harness/model coupling
0021→0020 — live only in README prose today; headers cannot express them.

A hand-drawn diagram, or an untyped overload of `Depends on`, would create a
second silently-drifting source of truth — the failure mode manifest identity
exists to prevent.

## Decision

Diagrams are **generated derived views**, never authored by hand. The pure
domain of each diagram is a **graph value** (`{:direction :nodes :edges
:class-defs}`); Mermaid is one renderer over it. A single module
(`abc.tools.diagram.core`) owns rendering and drift-checking effects over a
registry of committed diagrams.

Three sources of truth, one builder each:

1. **Decision map** — ADR headers unioned with a typed `docs/adr/adr-relations.edn`
   sidecar → `docs/adr/adr-graph.mmd` (`abc.tools.diagram.adr-graph`).
2. **System architecture** — `docs/architecture-stages.edn`, cross-checked
   against `schemas/schema-contracts.json` and the ADR files →
   `docs/architecture.mmd` (`abc.tools.diagram.architecture-graph`).
3. **Runtime provenance** — any `workflow-run.json`, rendered on demand to
   stdout (`abc.tools.diagram.workflow-graph`); per-run, not committed.

Each committed `.mmd` begins with a `%% GENERATED …` header and is byte-stable.
`clojure -M:abc/diagrams --check` regenerates in memory and fails on any
difference; it runs in the flake `diagram-drift` check.

### Typed relationships and single ownership

`adr-relations.edn` is a source of truth (hand-authored, lint-validated — not
generated). To avoid a second home for edges headers already own, ownership is
partitioned by edge-type:

- **Header fields own** `{amends, supersedes, depends-on}`.
- **The sidecar owns** header-inexpressible types
  `{restates-hard-rule, schema-hash-cascade, harness-for, extends}`.

`abc.tools.diagram.adr-graph/lint*` REJECTS any sidecar edge typed as a
header-owned type, any unknown type, any malformed entry, and any `from`/`to`
that does not resolve to an existing ADR. The Tier-1 builder unions both
sources; each edge is styled by type. The sidecar is EDN validated by lint, not
a registered JSON-Schema contract — it is dev-facing documentation metadata,
deliberately kept lower-ceremony than `schemas/schema-contracts.json`.

## Hard Rule

`.mmd` files, `adr-relations.edn`, and `architecture-stages.edn` are
publication/documentation views. They MUST NOT feed `manifest_identity_object`
or change any `artifact_id`, consistent with the global invariant in
`docs/adr/README.md`.

Two ADR-header hygiene rules become binding lints:

- **Reciprocal amend links:** `Amends: ADR B` on A requires `Amended by: ADR A`
  on B.
- **Resolvable references:** every `ADR NNNN` in `Amends:`/`Depends on:` must be
  an existing ADR file.

`Supersedes:` may be scoped prose (e.g. ADR 0012's "TEI stub language in ADR
0006"); the lint does not require a bare `ADR NNNN` there.

## Consequences

- The decision map and architecture diagram cannot silently drift from the
  ADRs/schemas; a stale diagram fails the gate.
- Semantic relationships become first-class, typed, and referentially checked
  instead of prose in the README.
- Adding an ADR, stage, or relation includes regenerating the diagrams (one
  command) and, for amends, backfilling the reciprocal link.

## Acceptance Criteria

- **ADR-0029-C1 — structural-invariant:** ADR graph construction consumes
  shared parsed ADR values, preserves relation scopes, and rejects unknown,
  dangling, malformed, or header-owned sidecar edges.
- **ADR-0029-C2 — structural-invariant:** Every declared architecture-stage
  schema and ADR coordinate resolves in the current repository contract.
- **ADR-0029-C3 — fixture-behavior:** The committed passed workflow fixture
  renders deterministically with producer-to-consumer edges.
- **ADR-0029-C4 — structural-invariant:** Every registered committed diagram
  is byte-equal to a fresh render of its registry entry.
- **ADR-0029-C5 — structural-invariant:** Diagram `run!` returns `:ok? false`
  for lint problems without terminating the host process. Evidence boundary:
  `test/abc/tools/diagram/core_test.clj`.

## Rollback

Delete the generators, `.mmd` files, the two EDN sidecars, the `diagram-drift`
check, and this ADR. No manifest or artifact identity is affected.
