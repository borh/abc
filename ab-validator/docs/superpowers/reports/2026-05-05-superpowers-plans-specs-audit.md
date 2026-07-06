# Superpowers Plans/Specs Audit — 2026-05-05

## Scope

- Reviewed all markdown files in `docs/superpowers/plans/` and `docs/superpowers/specs/` for stale or replaced content.
- Re-ran legacy-command scan for removed JSONL summary variants.
- Did not modify implementation code.

## Audit results

### 1) Legacy command cleanup already addressed

- No active plan or spec still references removed `summarize-*` command family (`summarize-compact`, `summarize-examples`, `summarize-differences`, `summarize-nway`, `summarize-nway-patterns`).
- This is documented in `docs/superpowers/reports/2026-05-05-morph-summary-command-migration.md`.

### 2) Archive decisions

- Archived as stale/outdated:
  - `docs/superpowers/plans/2026-05-03-aat-fidelity-oracle-roadmap.md`
  - `docs/superpowers/plans/2026-05-03-aat-fidelity-oracle-roadmap-v2.md`

### Rationale

- Both files begin with an explicit `Superseded` note directing work to `docs/superpowers/plans/2026-05-03-aat-fidelity-oracle-roadmap-v3.md`.
- They duplicate the same effort with earlier assumptions that were intentionally replaced.

### 3) Remaining active plans/specs

- No additional active plans/specs were judged outdated based on explicit supersession/deprecation markers and current usage context.
- The currently active `docs/superpowers/plans/2026-05-03-aat-fidelity-oracle-roadmap-v3.md` remains the authoritative version for the AAT-oracle roadmap.
- The following spec documents remain intentionally retained for current architecture coverage and gap tracking:
  - `docs/superpowers/specs/2026-05-03-morph-warehouse-sql-report-design.md`
  - `docs/superpowers/specs/2026-04-28-aat-morph-runner-design.md`
  - `docs/superpowers/specs/2026-04-28-morph-analyzer-adapters-design.md`
  - `docs/superpowers/specs/2026-04-28-morph-diff-core-design.md`
  - `docs/superpowers/specs/2026-04-28-syntax-coverage-methodology.md`
  - `docs/superpowers/specs/2026-04-28-diff-utils-extraction-design.md`
  - `docs/superpowers/specs/2026-04-28-aozora2html-coverage-notes.md`
  - `docs/superpowers/specs/2026-04-26-gaiji-resolution-comparison-notes.md`
  - `docs/superpowers/specs/2026-04-26-aozora-syntax-coverage-design.md`
  - `docs/superpowers/specs/2026-04-26-aozora-rs-performance-design.md`
  - `docs/superpowers/specs/2026-04-25-parser-validation-harness-design.md`

## Follow-up

- If future supersession is decided for the current roadmap, apply the same rule:
  - move superseded artifacts to `docs/superpowers/archive/`
  - create a dated audit report instead of inline annotations.
