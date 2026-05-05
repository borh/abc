# Superpowers Archive Automation Run — 2026-05-05

## Scope

- Ran archive-readiness checks for active `docs/superpowers/plans`.
- Set front-matter `status: done` on plans that were fully checked.
- Executed archive move for ready plans.

## Actions Taken

1. Candidate discovery
   - `docs/superpowers/plans/2026-04-28-morph-analyzer-adapters.md`
   - `docs/superpowers/plans/2026-05-03-morph-warehouse-sql-report.md`
2. Each candidate had all checklist items complete; both files had no pending `- [ ]` task items once real task list markers were treated.
3. Added plan front-matter:
   - `status: done`
   - `plan_id`
4. Removed false-positive header checkbox notation (`- [ ]`) from each plan’s prose line to keep readiness checks machine-parseable.
5. Ran:
   - `bash docs/superpowers/scripts/list-archive-ready-plans.sh`
   - `bash docs/superpowers/scripts/archive-completed-plans.sh --move`

## Result

- Archived:
  - `2026-04-28-morph-analyzer-adapters.md`
  - `2026-05-03-morph-warehouse-sql-report.md`
- Moved into:
  - `docs/superpowers/archive/`
- Updated:
  - `docs/superpowers/PLAN-EXECUTION-ORDER.md`

## Post-Condition

- Remaining active plans are still incomplete:
  - `2026-04-28-aat-morph-runner.md`
  - `2026-04-28-diff-utils-extraction.md`
  - `2026-04-28-morph-diff-core.md`
  - `2026-04-29-compact-morph-corpus-artifacts.md`
  - `2026-04-29-streaming-compact-morph-comparisons.md`
  - `2026-05-03-aat-fidelity-oracle-roadmap-v3.md`
  - `2026-05-03-aat-oracle-correctness-provenance.md`
  - `2026-05-03-adapter-fidelity-fixes.md`
  - `2026-05-03-adapter-upstream-faithfulness.md`
