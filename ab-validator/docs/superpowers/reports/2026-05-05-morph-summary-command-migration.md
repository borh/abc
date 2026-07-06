# 2026-05-05 morph summary command migration report

## Why this report

`ab-morph-run` removed the legacy JSONL summary command family from active use:

- `summarize-compact`
- `summarize-examples`
- `summarize-differences`
- `summarize-nway`
- `summarize-nway-patterns`

This change was completed, and the remaining guidance now uses only warehouse-backed
`summarize-warehouse-*` commands for corpus triage and recurrence reporting.

## Status

- Active CLI cleanup for `ab-morph-run` is complete; legacy summary command variants are no longer present in `crates/ab-morph-run/src/main.rs`.
- Operational workflow guidance in [docs/morph-corpus-workflow.md](/home/bor/Projects/ab-validator/docs/morph-corpus-workflow.md) was updated to use warehouse commands.
- Legacy command references in historical `superpowers` artifacts were archived instead of left as broken in active docs.

## Canonical replacements

| Legacy JSONL workflow intent | Active replacement |
| --- | --- |
| Source-level compact triage (`summarize-compact`) | `summarize-warehouse-nway` or `summarize-warehouse-pairwise` |
| Bounded example inspection (`summarize-examples`) | `summarize-warehouse-regions` |
| Compact pattern aggregation (`summarize-differences`) | `summarize-warehouse-patterns` plus `summarize-warehouse-pattern-examples` |
| N-way source summary (`summarize-nway`) | `summarize-warehouse-nway` |
| N-way pattern counts (`summarize-nway-patterns`) | `summarize-warehouse-patterns` |

## Docs archived as legacy

The following files were moved to [docs/superpowers/archive](../archive) because they
embed removed JSONL-command workflows and are now historical references only:

- 2026-04-29-compact-morph-workflow-tools.md
- 2026-04-30-nway-morph-diff.md
- 2026-04-30-nway-morph-diff-design.md
- 2026-05-01-morph-results-warehouse.md
- 2026-04-29-morph-targeted-reruns.md
- 2026-04-29-morph-lf-canonicalization-impact.md
- 2026-04-30-morph-canonical-v2-triage.md
- 2026-04-30-morph-corpus-difference-triage.md
- 2026-04-30-morph-lexical-triage.md
- 2026-04-30-morph-script-category-filtering.md
- 2026-04-30-morph-whitespace-exact-counters.md
- 2026-04-30-morph-whitespace-isolation.md
- 2026-04-30-nway-morph-smoke.md
- 2026-05-01-nway-pattern-counts-split.md
- 2026-05-01-warehouse-subset-100-triage.md
- 2026-05-01-warehouse-subset-200-parallel-triage.md
- 2026-05-01-warehouse-subset-triage.md

## Remaining active `superpowers` docs status

- Current active report set has no removed-command command examples.
- Remaining plan/spec/reports files now describe active warehouse-based work (for example, `2026-05-03-morph-warehouse-sql-report.md`, `2026-05-03-aat-oracle-correctness-provenance.md`, etc.).
- No additional legacy JSONL-command plans or specs remain in active directories.

