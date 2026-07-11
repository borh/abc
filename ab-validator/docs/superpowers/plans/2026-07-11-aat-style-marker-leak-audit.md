# AAT Style Marker Leak Audit Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a deterministic streaming audit for complete and partial Aozora markers inside AAT style text, then check in compact evidence for the retained dump.

**Architecture:** A standalone report CLI streams sorted JSON files, recursively visits AAT blocks, attributes text to the nearest style, and builds a compact summary model. JSON serialization and Markdown rendering consume the same model; malformed input is recorded and returns exit 1, while findings on valid input return 0.

**Tech Stack:** Python 3.11 standard library, pytest, ruff, mypy.

## Global Constraints

- Do not modify parser, adapter, AAT schema, registry, or Phase 3 plan files.
- Match complete markers with shortest non-overlapping `［＃[^］]*］` semantics.
- Count unmatched `［＃` openers separately and do not reconstruct markers across text nodes.
- Stream one input file at a time; check in only compact summary JSON and Markdown.
- Exit 0 for a complete audit with or without findings, 1 for malformed input, and 2 for usage errors.

---

### Task 1: Detection and recursive attribution

**Files:**
- Create: `ab-validator/reports/aat-fidelity/style-marker-leak-audit.py`
- Create: `ab-validator/reports/aat-fidelity/tests/test_style_marker_leak_audit.py`

**Interfaces:**
- Produces: `marker_occurrences(text: str) -> tuple[list[str], int]` and recursive document auditing that attributes a text node only to its nearest containing style.

- [ ] **Step 1: Write failing marker tests.** Cover two complete markers plus a stray `］`, an unmatched `［＃`, and ordinary text.
- [ ] **Step 2: Run pytest and verify RED.** Run `python -m pytest ab-validator/reports/aat-fidelity/tests/test_style_marker_leak_audit.py -v`; expect import/file failure.
- [ ] **Step 3: Implement shortest matching.** Use a compiled `［＃[^］]*］` regex, `finditer`, and count unmatched opener offsets not covered by matches.
- [ ] **Step 4: Run marker tests and verify GREEN.** Run the focused pytest command.
- [ ] **Step 5: Write failing recursive fixtures.** Cover clean styles, prose markers outside styles, bold/emphasis/bouten, nested styles without double attribution, multiple descendants, missing/non-string text values, and invalid `blocks`.
- [ ] **Step 6: Implement recursive traversal.** Walk dictionaries/lists with JSON-pointer-like paths; on entering a nested style it becomes the nearest owner. Record malformed audited shapes without aborting remaining files.
- [ ] **Step 7: Run focused tests and verify GREEN.** Run the focused pytest command.

### Task 2: Deterministic aggregation, examples, CLI, and Markdown

**Files:**
- Modify: `ab-validator/reports/aat-fidelity/style-marker-leak-audit.py`
- Modify: `ab-validator/reports/aat-fidelity/tests/test_style_marker_leak_audit.py`

**Interfaces:**
- Consumes: `AAT_DIR --summary-json PATH --report-md PATH [--example-limit N] [--input-label LABEL]`.
- Produces: schema `soranoha.aat-style-marker-leak-audit.v1`, deterministic JSON and Markdown, maximum two examples per file selected round-robin across style types.

- [ ] **Step 1: Write failing aggregation tests.** Assert exact counts, sorted count tables, paired and quoted signatures, two-per-file cap, style round-robin diversity, excerpt bounds, and byte-identical repeat output.
- [ ] **Step 2: Implement the summary model and signature function.** Paired leading/trailing complete markers become `OPEN…CLOSE`; a single quoted marker remains unchanged; other marker sequences retain their ordered marker list signature.
- [ ] **Step 3: Implement deterministic example selection.** Sort candidates within style type by file stem/path/signature, round-robin sorted style types, skip candidates after a file has two selected examples, and stop at the global limit.
- [ ] **Step 4: Implement Markdown from the summary model.** Include limitations for unmatched and split-node markers and tables for totals, style types, signatures, malformed input, and examples.
- [ ] **Step 5: Implement argparse and atomic output writes.** Usage errors return 2 through argparse; completed valid audits return 0; recorded malformed input returns 1 after both outputs are written.
- [ ] **Step 6: Run focused pytest and verify GREEN.** Run the focused pytest command.
- [ ] **Step 7: Run `just python-quality`.** Expect ruff format/check and mypy success.

### Task 3: Pinned-dump evidence

**Files:**
- Create: `ab-validator/docs/superpowers/reports/2026-07-11-aat-style-marker-leak-audit.json`
- Create: `ab-validator/docs/superpowers/reports/2026-07-11-aat-style-marker-leak-audit.md`

**Interfaces:**
- Consumes: retained 17,886-file `aozora-full-repin-1a4f864/aat/aozora-adapter` directory.
- Produces: compact byte-stable evidence labeled `aozora-full-repin-1a4f864`, with no absolute input path.

- [ ] **Step 1: Run the audit twice to temporary outputs.** Use `--input-label aozora-full-repin-1a4f864 --example-limit 30`; require exit 0.
- [ ] **Step 2: Compare repeat outputs byte-for-byte.** Use `cmp` for JSON and Markdown; any difference blocks evidence generation.
- [ ] **Step 3: Generate the checked-in outputs.** Run the same command targeting the two report paths.
- [ ] **Step 4: Inspect evidence size and contents.** Confirm 17,886 scanned files, zero malformed inputs, bounded examples, and no `/db/` substring.
- [ ] **Step 5: Re-run focused pytest and `just python-quality`.** Expect success.

### Task 4: Final verification

**Files:** none

- [ ] **Step 1: Run `just validate-migration`.** Expect success, or report any unrelated pre-existing failure with evidence.
- [ ] **Step 2: Run `git diff --check` and inspect the final diff.** Confirm parser and Phase 3 files are untouched.

