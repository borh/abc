# Parser Run Evidence Verification Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Generate and verify the checked 20-lane parser evidence summary from external hash-verified raw runs.

**Architecture:** A focused Python module consumes a checked identity template plus an explicit logical-root resolver. It delegates raw artifact validation to the neutral executor, validates the closed study matrix and identity bindings, then emits canonical JSON for byte comparison.

**Tech Stack:** Python 3.13 standard library, pytest, existing `neutral_executor.py`.

## Global Constraints

- Store no machine-local external artifact paths in checked files.
- Never synthesize the missing immediately-before-run host capture.
- Require exactly five candidates, two modes, and two inventories.
- Verify every external per-item artifact hash before accepting a summary.

---

### Task 1: Closed generator and verifier

**Files:**
- Create: `ab-validator/reports/parser-study/freeze_run_evidence.py`
- Create: `ab-validator/reports/parser-study/tests/test_freeze_run_evidence.py`
- Modify: `ab-validator/reports/parser-study/runs/aozora-parser-neutral-comparison-2026-07/run-manifests.json`

**Interfaces:**
- Consumes: checked summary JSON and resolver JSON mapping logical roots to local directories.
- Produces: `generate(summary_path: Path, resolver_path: Path) -> bytes` and a CLI that verifies or writes canonical bytes.

- [ ] **Step 1: Write failing synthetic tests**

Create minimal two-inventory fixtures covering all 20 lanes and assert canonical regeneration, full artifact verification, exact matrix rejection, command/environment mismatch rejection, and closed unavailable host capture.

- [ ] **Step 2: Run RED**

Run: `pytest ab-validator/reports/parser-study/tests/test_freeze_run_evidence.py -q`
Expected: import failure because `freeze_run_evidence.py` does not exist.

- [ ] **Step 3: Implement minimal verifier**

Implement safe resolver loading, exact lane matrix checks, candidate/mode command binding checks, invocation of `neutral_executor.verify`, manifest/execution/count comparison, and canonical `json.dumps(..., sort_keys=True, ensure_ascii=False, separators=(",", ":")) + "\n"` output.

- [ ] **Step 4: Run GREEN**

Run the focused pytest, ruff check/format, and strict mypy; expect all pass.

### Task 2: Real evidence and repair audit

**Files:**
- Modify: `ab-validator/reports/parser-study/runs/aozora-parser-neutral-comparison-2026-07/run-manifests.json`
- Create: `ab-validator/reports/parser-study/runs/aozora-parser-neutral-comparison-2026-07/aozora2html-repair-audit.json`
- Modify: `.superpowers/sdd/task-3-report.md`

**Interfaces:**
- Consumes: all `/tmp/task3-runs` and `/tmp/task3-full-runs` lanes through an untracked resolver.
- Produces: byte-identical checked summary regeneration and a path-neutral repair audit.

- [ ] **Step 1: Add real-data identity fields and unavailable host classification**

Record exact command/environment values already hashed by each outcome, bind parser/adapter/corpus/protocol/timeout identities, and record host capture as unavailable.

- [ ] **Step 2: Record recoverable repair facts**

Record final hashes and repair command templates; mark rejected bytes and mismatched ID lists unavailable because the overwritten evidence was not preserved.

- [ ] **Step 3: Verify real external evidence**

Run the generator with an explicit `/tmp` resolver and require byte-identical checked output plus all 20 raw verifications.

- [ ] **Step 4: Run repository checks and commit**

Run focused Python checks, `git diff --check`, and comment hygiene. Commit only Task 3 files.
