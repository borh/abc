# Source Reference Reconciliation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a reproducible report that reconciles observed source-inventory rows with official-Aozora syntax rows and P4suta notation-spec comparison features.

**Architecture:** A focused Python report builder consumes the existing source-authority summary, syntax coverage TOML, and notation-spec comparison summary. It emits Markdown and JSON evidence without changing parser, adapter, parser-IR, TEI, or plaintext behavior.

**Tech Stack:** Python 3.13 standard library (`argparse`, `json`, `tomllib`, `pathlib`), shell smoke tests, committed Markdown/JSON reports.

## Global Constraints

- Source inventory remains authoritative for observed corpus prevalence.
- Official Aozora evidence comes from syntax rows that cite `references/aozorabunko/rules/...`.
- P4suta notation-spec remains comparison evidence only.
- The report must not require network or `/db`.
- The smoke test must run with temporary fixture inputs.
- Plaintext policy is unchanged.

---

### Task 1: Report Builder And Smoke

**Files:**
- Create: `reports/source-references/reconcile-aozora-notation.py`
- Create: `tests/source-reference-reconciliation-smoke.sh`
- Create: `docs/superpowers/reports/2026-07-06-source-reference-reconciliation.md`
- Create: `docs/superpowers/reports/2026-07-06-source-reference-reconciliation.summary.json`
- Modify: `docs/superpowers/plans/2026-07-06-complete-aozora-markup-publication-mapping.md`
- Modify: `justfile`

**Interfaces:**
- Consumes:
  - syntax TOML file with `[[syntax]]` rows.
  - source summary JSON with top-level `rows`.
  - notation comparison JSON with top-level `rows`.
- Produces:
  - `build_summary(...) -> dict`
  - JSON summary with `schema_version == "aozora-source-reference-reconciliation-v1"`.
  - Markdown report headed `# Source Reference Reconciliation`.

- [ ] **Step 1: Write the failing smoke**

Create `tests/source-reference-reconciliation-smoke.sh` with fixture syntax, source, and notation files. Assert that the summary includes one documented observed row, one documented unobserved row, one observed row without syntax coverage, and one unmapped P4suta feature.

- [ ] **Step 2: Run the smoke to verify RED**

Run:

```bash
bash tests/source-reference-reconciliation-smoke.sh
```

Expected: fails because `reports/source-references/reconcile-aozora-notation.py` does not exist.

- [ ] **Step 3: Implement the report builder**

Create `reports/source-references/reconcile-aozora-notation.py` with:

- TOML loading through `tomllib`.
- source-row occurrence lookup from source summary JSON.
- official-row detection by `reference_sources` entries that contain `references/aozorabunko/rules/`.
- P4suta feature mapping through a small explicit feature-to-source-row table.
- JSON and Markdown writers.

- [ ] **Step 4: Run the smoke to verify GREEN**

Run:

```bash
bash tests/source-reference-reconciliation-smoke.sh
```

Expected: prints `source reference reconciliation smoke ok`.

- [ ] **Step 5: Generate current report**

Run:

```bash
python3 reports/source-references/reconcile-aozora-notation.py \
  --syntax-coverage data/aozora-syntax-coverage.toml \
  --source-summary docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json \
  --notation-summary docs/superpowers/reports/2026-07-05-aozora-notation-spec-comparison.summary.json \
  --manual-root /home/bor/Dependencies/aozorabunko/rules \
  --summary-json docs/superpowers/reports/2026-07-06-source-reference-reconciliation.summary.json \
  --report-md docs/superpowers/reports/2026-07-06-source-reference-reconciliation.md
```

- [ ] **Step 6: Update overarching plan and justfile**

Update the overarching plan's Workstream 5 and verification commands to cite the new report and its expected complete verdict for current evidence. Add `source-reference-reconciliation-smoke` and `source-reference-reconciliation-report` just recipes.

- [ ] **Step 7: Verify**

Run:

```bash
bash tests/source-reference-reconciliation-smoke.sh
python3 -m py_compile reports/source-references/reconcile-aozora-notation.py
jq -e '.schema_version == "aozora-source-reference-reconciliation-v1"' docs/superpowers/reports/2026-07-06-source-reference-reconciliation.summary.json
jq -e '.verdict == "SOURCE_REFERENCE_RECONCILIATION_COMPLETE"' docs/superpowers/reports/2026-07-06-source-reference-reconciliation.summary.json
git diff --check
```

- [ ] **Step 8: Commit**

```bash
git add reports/source-references/reconcile-aozora-notation.py \
  tests/source-reference-reconciliation-smoke.sh \
  docs/superpowers/specs/2026-07-06-source-reference-reconciliation-design.md \
  docs/superpowers/plans/2026-07-06-source-reference-reconciliation.md \
  docs/superpowers/plans/2026-07-06-complete-aozora-markup-publication-mapping.md \
  docs/superpowers/reports/2026-07-06-source-reference-reconciliation.md \
  docs/superpowers/reports/2026-07-06-source-reference-reconciliation.summary.json \
  justfile
git commit -m "feat(source): add reference reconciliation report"
```

## Self-Review

- Spec coverage: Task 1 implements the report builder, smoke, current report, and roadmap update.
- Placeholder scan: no TBD/TODO placeholders.
- Type consistency: report schema and verdict names match between spec, plan, smoke, and verification.
