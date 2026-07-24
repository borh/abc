# Data-Driven Decision Records

## Implementation Status

Accepted on 2026-07-24 after the full corpus was migrated to
`decisions.edn`, every consumer was re-pointed at the data, and the strict
governance validator, decision graph, and generated index passed over the
migrated corpus.

## Context

The decision corpus reached forty-two records whose machine-facing facts —
status, dates, five reciprocal relation fields with bracketed scopes, typed
claim headers, and backtick-scanned evidence paths — lived inside a closed
Markdown grammar. Roughly six hundred lines of regex-based parsing existed
only to recover that data from prose, a separate `adr-relations.edn` sidecar
held the relation types headers could not express, and organization depended
on a sequential numbering scheme that carried no meaning. ADR 0043 had just
demonstrated the failure mode of apparatus that re-proves what simpler
representations already state; this record applies the same subtractive
judgment to the grammar itself.

## Decision

Machine-facing decision facts live in a single authoritative
`docs/adr/decisions.edn`; Markdown files are pure narrative that no tool
parses.

- Identity is the kebab-case slug. Legacy numbers are retired into a frozen
  `:legacy-number` set for historical resolution only; new records are
  slug-only.
- Relations are a `:class`-discriminated sum: a closed lifecycle enum
  (`:supersedes`, `:amends`, `:depends-on`) with validation semantics, and
  open annotation edges with none. Edges are stated once, on the acting
  record; inverses are derived. Lifecycle graphs are irreflexive and acyclic.
- Claims are data (`:id`, `:kind`, `:statement`, `:evidence`), local to their
  record; `:kind` and `:topics` remain open descriptive vocabularies.
- `abc.tools.decisions` owns a strict EDN loading boundary, the Malli shape
  schema, and corpus semantic checks; the governance CLI contract is
  unchanged.
- `INDEX.md` and `adr-graph.mmd` are generated, byte-current derived views;
  architecture metadata cites decisions by slug.
- The Markdown header grammar, claim-header grammar, reciprocal relation
  fields, `adr-relations.edn`, tombstone convention, invalid-Markdown fixture
  corpus, and the legacy parser are deleted without replacement.

## Consequences

- One representation instead of three; a typo in a lifecycle relation is a
  schema error rather than a silently ignored annotation.
- Organization comes from queryable attributes (`:topics`, `:status`, the
  relation graph) rendered as generated views, not from sequence position.
- Historical `ADR NNNN` and `ADR-NNNN-CN` references resolve through the
  frozen legacy-number table in the generated index.
- Narrative files carry decision meaning for humans only; moving a cited
  evidence path fails governance immediately.

## Rollback

Reverting to the Markdown grammar requires a new record superseding this
one; the deleted machinery remains available in git history, and the
migration landed as one revertable unit.
