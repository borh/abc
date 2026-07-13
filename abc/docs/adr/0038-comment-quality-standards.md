# ADR 0038: Comment Quality Standards

Status: Accepted
Date: 2026-07-13

## Context

A comment quality audit found ~190 transient references in source
comments: handoff documents, task/phase/plan labels, dated spec refs,
internal issue tracker numbers, TODOs, speculative language, UNSTABLE
markers, and transient Tier/Phase references.

## Decision

Source comments follow these rules:

1. The only allowed external references are to code symbols and
   specific ADRs. References to upstream specifications (JIS, Unicode,
   TEI, IIIF, RDF, SHACL, JSON-LD) are permitted.
2. Internal issue tracker numbers are not referenced from source
   comments. Six numbers that name durable semantic invariants used
   as cross-file vocabulary (#78, #228, #331, #333, #384, #435) are
   retained as stable identifiers and defined in `docs/glossary.md`.
3. Project-management labels (Task N, Plan G.4, plan amendment,
   Phase labels, Plan Blocker) are deleted.
4. Dated spec/report references and handoff references are rewritten
   as self-contained prose.
5. TODOs are converted to factual limitation statements.
6. Per-item UNSTABLE markers are consolidated into a single crate- or
   module-level `# Stability` section.
7. The "Tier-A canary" invariant name is retained. Algorithm-stage
   Phase markers (`// --- Phase N: description ---`) are retained.

## Consequences

- A verification recipe (grep commands) enforces these standards.
- The former handoff/spec/report directories are deleted.
