# Melos Structural Probe Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a measurement-only probe showing whether existing Melos AATs preserve paragraph segmentation and final source attribution, and whether current parser-IR represents those values without residual divergence.

**Architecture:** Reuse the existing `ab-aat-to-parser-ir` converter as the authority for parser-IR output and divergence records. Add a small summarizer module that inspects AAT structure, converter output, and divergence bundles, then writes JSON and Markdown reports. Keep CI fixture-only; local `/db` Melos inputs are used only for the committed measurement report. The follow-up expansion consumes ABC's machine-readable `../abc/docs/handoffs/tei-eaj-aozora-workset-export.json`; this plan does not materialize those 55 rows.

**Tech Stack:** Rust (`ab-aat-to-parser-ir`), serde JSON, existing mapping/schema preflight, shell smoke, Just.

## Global Constraints

- Do not change parser-IR schema in this slice.
- Do not make parser agreement the source-authority gate; parser outputs remain supporting evidence only.
- Flake/smoke checks must not read `/db` or require network.
- The probe must reuse the current converter so measurement matches actual parser-IR emission.
- Generated full-run artifacts stay under `docs/superpowers/reports/`; fixture smoke artifacts stay in temp dirs.
- ABC's TEI-EAJ workset JSON is follow-up input only; do not scrape Markdown tables or make parser-selection claims in this slice.

---

## File Structure

- Create `crates/ab-aat-to-parser-ir/src/structural_probe.rs`: labelled AAT input parsing, AAT structure summarization, converter invocation, JSON/Markdown report rendering.
- Modify `crates/ab-aat-to-parser-ir/src/lib.rs`: expose `structural_probe`.
- Modify `crates/ab-aat-to-parser-ir/src/main.rs`: add `structural-probe` CLI subcommand.
- Modify `crates/ab-aat-to-parser-ir/tests/integration.rs`: add fixture-level probe test.
- Modify `tests/aat-to-parser-ir-cli-smoke.sh`: run the new subcommand against a synthetic final-attribution fixture.
- Modify `justfile`: add a local Melos probe recipe with overridable AAT paths.
- Create `docs/superpowers/reports/2026-07-04-melos-structural-probe.md`.
- Create `docs/superpowers/reports/2026-07-04-melos-structural-probe.summary.json`.

---

### Task 1: Probe API and Fixture Test

**Files:**
- Create: `crates/ab-aat-to-parser-ir/src/structural_probe.rs`
- Modify: `crates/ab-aat-to-parser-ir/src/lib.rs`
- Modify: `crates/ab-aat-to-parser-ir/tests/integration.rs`

**Interfaces:**
- Produces: `structural_probe::run_structural_probe(config: StructuralProbeConfig) -> anyhow::Result<StructuralProbeSummary>`
- Produces: `StructuralProbeInput { label: String, path: PathBuf }`

- [x] **Step 1: Write failing integration test**

Add a synthetic AAT with two paragraph blocks and final text
`（古伝説と、シルレルの詩から。）`. Assert that the probe counts two paragraph
blocks, detects a final attribution candidate, observes no parser-IR paragraph
node, records paragraph structural divergence, and returns `residual_free =
false`.

- [x] **Step 2: Verify the test fails**

Run:

```bash
cargo test -p ab-aat-to-parser-ir structural_probe_detects_melos_level3_gap -- --nocapture
```

Expected before implementation: compile failure because `structural_probe` does
not exist.

- [x] **Step 3: Implement the module**

Implement the data structs, visible-text extraction, final-attribution heuristic,
converter call, divergence summarization, and JSON/Markdown rendering.

- [x] **Step 4: Verify the test passes**

Run the same cargo test and expect one passing test.

### Task 2: CLI and Fixture Smoke

**Files:**
- Modify: `crates/ab-aat-to-parser-ir/src/main.rs`
- Modify: `tests/aat-to-parser-ir-cli-smoke.sh`
- Modify: `justfile`

**Interfaces:**
- Consumes: `run_structural_probe`.
- Produces CLI:

```bash
ab-aat-to-parser-ir structural-probe \
  --aat label=/path/to/input.aat.json \
  --mapping data/aat-to-parser-ir-mapping-v1.json \
  --summary-json /tmp/summary.json \
  --report-md /tmp/report.md \
  --abc-root data/abc-schemas
```

- [x] **Step 1: Add the CLI subcommand**

Parse repeated `--aat label=path` values into `StructuralProbeInput`.

- [x] **Step 2: Extend the shell smoke**

Create a second fixture AAT with final source attribution and assert:

```bash
jq -e '.totals.inputs == 1' "$out_dir/structural-summary.json"
jq -e '.inputs[0].aat.paragraph_blocks == 2' "$out_dir/structural-summary.json"
jq -e '.inputs[0].aat.final_source_attribution_candidate == true' "$out_dir/structural-summary.json"
jq -e '.inputs[0].parser_ir.paragraphs_represented == false' "$out_dir/structural-summary.json"
jq -e '.inputs[0].parser_ir.source_attribution_represented == false' "$out_dir/structural-summary.json"
jq -e '.inputs[0].verdict.residual_free == false' "$out_dir/structural-summary.json"
```

- [x] **Step 3: Add Just recipe**

Add `melos-structural-probe` with overridable `AB_MELOS_AOZORA_RS_AAT`,
`AB_MELOS_AOZORA2HTML_AAT`, and `AB_MELOS_AOZORA_EPUB3_AAT` environment
variables.

### Task 3: Local Melos Measurement Report

**Files:**
- Create: `docs/superpowers/reports/2026-07-04-melos-structural-probe.md`
- Create: `docs/superpowers/reports/2026-07-04-melos-structural-probe.summary.json`

**Interfaces:**
- Consumes the CLI from Task 2.
- Produces committed measurement evidence for ABC follow-up.

- [x] **Step 1: Run local probe**

Run:

```bash
just melos-structural-probe
```

- [x] **Step 2: Verify report content**

Check the JSON records at least aozora-rs, aozora2html, and aozora-epub3 when
their local AAT files exist, and that each successful Melos conversion reports
the paragraph/source-attribution Level 3 gap.

- [x] **Step 3: Full verification**

Run:

```bash
cargo test -p ab-aat-to-parser-ir
bash tests/aat-to-parser-ir-cli-smoke.sh
git diff --check
```

Expected: all pass.

## Self-Review

- Spec coverage: Melos paragraph evidence, source-attribution candidate, residual-free verdict, fixture-only smoke, and local report are all covered.
- Placeholder scan: no TBD/TODO placeholders.
- Type consistency: CLI, test, and module names all use `StructuralProbe*` and `structural-probe`.
