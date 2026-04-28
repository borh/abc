# Superpowers Plan Execution Order

## Progress convention

All plan documents in `docs/superpowers/plans/` now use front-matter status fields:

- `plan_id`: filename timestamp/title identifier
- `status`: `not_started | blocked | done`
- `started`: YYYY-MM-DD when work began
- `next_update`: optional next check-in date
- `owner`: current owner (optional)
- `target_prerequisites`: dependency notes used for sequencing

## Active plans

1. `2026-04-28-aozora2html-adapter` (active)
   - Adds the official Ruby parser as a third adapter so AAT-level parity
     replaces the broken pandoc-based parity script.

## Archived plans

All seven previously queued plans landed and were moved to
`docs/superpowers/archive/`:

- `2026-04-25-parser-validation-harness`
- `2026-04-26-parser-neutral-ir`
- `2026-04-26-structured-ab-ir`
- `2026-04-26-semantic-summary-comparison`
- `2026-04-26-aozora-rs-comparison`
- `2026-04-26-aozora-rs-performance`
- `2026-04-26-aozora-syntax-coverage-increment`

## Open spec gaps

These are spec-only items that are *not* covered by an active plan; revisit
when prioritized:

1. `aozora-syntax-coverage-design.md` — `ir_projection::tei` not
   implemented; `ab-ir` lacks structured block kinds for jisage / warichu /
   figure / break; matrix rows still `status = "partial"`.
2. `parser-validation-harness-design.md` — `ab-render-diff` crate not
   built. After the AAT-parity script lands, the pressure to build it
   eases; revisit only if AAT diffs prove insufficient.
3. `aozora-rs-performance-design.md` — performance phases shipped; a
   follow-up benchmark may be useful once `aozora2html` is in the
   comparison.

## Archive rule

A plan is archive-ready when:

- every checklist item is marked `- [x]`, and
- `status: done` is explicitly set in front-matter.

## Archive process

- Place completed plans under [`docs/superpowers/archive/`](/home/bor/Projects/ab-validator/docs/superpowers/archive) using:
  - `bash docs/superpowers/scripts/archive-completed-plans.sh --move`
- Review before move with:
  - `bash docs/superpowers/scripts/archive-completed-plans.sh --dry-run`
- Run a readiness report (checklists + status) with:
  - `bash docs/superpowers/scripts/list-archive-ready-plans.sh`

Recommended workflow after any status changes:

1. Mark completed plans as `status: done`.
2. Run the readiness report.
3. Run archive move only for plans that are truly ready.

Recommended status updates for plan progress:

- Set `status: blocked` with reason notes when a prerequisite is missing.
- Set `status: done` only when archive-ready criteria are met.
