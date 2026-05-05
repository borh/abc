# Superpowers Documentation Sweep — 2026-05-05

## Scope

- Performed global docs sweep for stale legacy planning material and archive-readiness status.
- Focused on:
  - `docs/superpowers/plans`
  - `docs/superpowers/specs`
  - `docs/superpowers/reports`
  - `docs/` (for removed `summarize-*` references)

## Findings

### 1) Legacy command references

- Legacy `summarize-*` command names are no longer present in active plan/spec content.
- Remaining references are limited to historical archive documents and the migration report where they are intentionally preserved as historical evidence.
- This is consistent with the migration boundary established on 2026-05-05.

### 2) Archive readiness check

- Ran:
  - `bash docs/superpowers/scripts/list-archive-ready-plans.sh`
  - `bash docs/superpowers/scripts/archive-completed-plans.sh --dry-run`
- Output indicates no active plan currently satisfies archive-ready criteria (checklist completion + `status: done`).
- No additional active archives were automatically identified in this sweep.

### 3) Plan status

- Active plans remain unchanged since the last report except the earlier archive moves of deprecated roadmaps.
- `docs/superpowers/PLAN-EXECUTION-ORDER.md` was already updated to include the archived roadmap entries.

### 4) Additional archival candidates checked

- No further active documents with explicit `**Superseded:**` directives were found outside the items already archived earlier.

## Follow-up

- Continue using the existing archive flow:
  - Mark superseded documents for archival in place.
  - Apply `archive-completed-plans.sh --move` only when status/readiness gates are met.
