# Morph Warehouse SQL Report Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a reproducible SQL-backed report scaffold for the morph warehouse triage run.

**Architecture:** Keep SQL templates in the repo and write report snapshots under `/db`. A shell runner substitutes `__RUN_DIR__` and `__LIMIT__`, executes DuckDB queries, and creates a Markdown index linking each query to its TSV output.

**Tech Stack:** Bash, DuckDB CLI, Parquet-backed warehouse tables, Markdown.

---

### Task 1: Smoke Test

**Files:**
- Create: `tests/morph-warehouse-report-smoke.sh`

- [x] **Step 1: Write the failing test**

Create a shell test that builds a tiny warehouse fixture under `/db/ab-validator/tmp/morph-warehouse-report-test`, runs `reports/morph-warehouse/build-report.sh`, and asserts the generated report shape.

- [x] **Step 2: Run test to verify it fails**

Run:

```bash
bash tests/morph-warehouse-report-smoke.sh
```

Expected before implementation: failure because `reports/morph-warehouse/build-report.sh` does not exist.

### Task 2: Report Runner And SQL Templates

**Files:**
- Create: `reports/morph-warehouse/build-report.sh`
- Create: `reports/morph-warehouse/README.md`
- Create: `reports/morph-warehouse/queries/001-run-summary.sql`
- Create: `reports/morph-warehouse/queries/010-source-disagreement-density.sql`
- Create: `reports/morph-warehouse/queries/020-pairwise-segmentation.sql`
- Create: `reports/morph-warehouse/queries/030-top-segmentation-patterns.sql`
- Create: `reports/morph-warehouse/queries/040-top-pos1-patterns.sql`
- Create: `reports/morph-warehouse/queries/050-historical-kana-probes.sql`
- Create: `reports/morph-warehouse/queries/060-dialogue-punctuation-probes.sql`
- Create: `reports/morph-warehouse/queries/070-largest-disagreement-regions.sql`

- [x] **Step 1: Implement the runner**

The runner accepts:

```bash
reports/morph-warehouse/build-report.sh RUN_DIR OUTPUT_DIR [LIMIT]
```

It validates `RUN_DIR`, writes temporary DuckDB spill under `OUTPUT_DIR/tmp`, writes TSV files under `OUTPUT_DIR/outputs`, copies substituted SQL snapshots under `OUTPUT_DIR/queries`, and writes `OUTPUT_DIR/index.md`.

- [x] **Step 2: Add triage-safe SQL templates**

Each SQL file must read only triage tables with `read_parquet('__RUN_DIR__/...')`. Avoid raw feature tables in this first version.

- [x] **Step 3: Run the smoke test to verify it passes**

Run:

```bash
bash tests/morph-warehouse-report-smoke.sh
```

Expected: exit 0.

### Task 3: Full Triage Report Build

**Files:**
- Output only under `/db/ab-validator/morph-warehouse/reports/triage-2026-05-03-jobs12`

- [x] **Step 1: Run the report builder on the real triage warehouse**

Run:

```bash
reports/morph-warehouse/build-report.sh \
  /db/ab-validator/morph-warehouse/runs/triage-2026-05-03-jobs12 \
  /db/ab-validator/morph-warehouse/reports/triage-2026-05-03-jobs12 \
  50
```

- [x] **Step 2: Inspect generated outputs**

Check:

```bash
find /db/ab-validator/morph-warehouse/reports/triage-2026-05-03-jobs12 -maxdepth 2 -type f | sort
sed -n '1,120p' /db/ab-validator/morph-warehouse/reports/triage-2026-05-03-jobs12/index.md
```

Expected: `index.md`, substituted queries, and TSV outputs are present.

### Task 4: Verification

**Files:**
- No new files unless verification reveals a defect.

- [x] **Step 1: Run the smoke test**

```bash
bash tests/morph-warehouse-report-smoke.sh
```

- [x] **Step 2: Verify report outputs**

```bash
grep -R "__RUN_DIR__\\|__LIMIT__" /db/ab-validator/morph-warehouse/reports/triage-2026-05-03-jobs12/queries && exit 1 || true
find /db/ab-validator/morph-warehouse/reports/triage-2026-05-03-jobs12/outputs -type f -name '*.tsv' -size +0c | wc -l
```

Expected: no unresolved placeholders and eight non-empty TSV files.
