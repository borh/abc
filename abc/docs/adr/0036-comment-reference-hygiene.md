# ADR 0036: Comment Reference Hygiene

Status: Accepted
Date: 2026-07-13

## Context

Source comments contain references to the project's own issue tracker,
handoff documents, dated specs, task labels, and planning artifacts.
These are transient: they lose meaning once the task is closed or the
document moves.

## Decision

1. The codebase does not reference its own issue tracker from source
   comments. Feature labels and one-off issue references are rewritten
   as self-contained prose.
2. Issue numbers that name **durable semantic invariants** used as
   cross-file vocabulary are retained as stable identifiers. These
   function identically to named invariants like "Tier-A canary." Six
   such numbers are recognized: #78, #228, #331, #333, #384, #435.
   Their definitions live in `docs/glossary.md`, a living reference
   document.
3. Project-management labels (Task N, Plan G.4, plan amendment, Phase
   labels, Plan Blocker) are deleted. They carry zero meaning to a
   code reader.
4. Dated spec/report references and handoff references are rewritten as
   self-contained prose; durable rationale is extracted into ADRs
   before the reference is removed.
5. TODOs are converted to factual limitation statements. Per-item
   UNSTABLE markers are consolidated into a single crate- or
   module-level `# Stability` section.
6. References to upstream/external specifications (JIS, Unicode, TEI,
   IIIF, RDF, SHACL, JSON-LD) and to specific ADRs remain permitted.

## Consequences

- The 6 stable invariant names remain greppable cross-file vocabulary
  with a single glossary mapping them to definitions.
- ~190 transient references are removed, reducing comment debt.
- `docs/handoffs/`, `docs/superpowers/specs/`, and
  `docs/superpowers/reports/` are deleted after all code references
  are cleaned up; durable rationale is extracted into ADRs first.
