# Aozora Publication Completion Next Steps Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the next-step admission evidence needed to finish complete Aozora Bunko markup publication mapping to TEI P5, ABC extension, custom sidecar, diagnostic, or plaintext-only targets.

**Architecture:** Add a single next-work ledger report first, then use it to drive focused source-region, text-policy, adapter-fidelity, TEI P5 dossier, and parser-acceptance slices. ab-validator remains the measurement/admission repo; ABC remains the TEI/profile/custom-schema owner.

**Tech Stack:** Python report builders under `reports/`, shell smoke tests under `tests/`, Markdown/JSON reports under `docs/superpowers/reports/`, design docs under `docs/superpowers/specs/`, and synced ABC schemas/policies under `data/abc-schemas/`.

## Global Constraints

- TEI-EAJ is calibration evidence, not source authority.
- The success definition is complete Aozora markup publication accounting, not TEI Level 2/3/4 parity.
- Plaintext is visible body text only: no ruby readings, source apparatus, source notes, layout metadata, custom records, warnings, or provenance.
- All five parser lanes must stay visible in evidence when available: `aozora2html`, `aozora-epub3`, `aozora-rs`, `aozora2`, and `aozora`.
- Parser outputs are supporting evidence; source inventory remains authoritative for observed Aozora markup.
- Custom preservation requires schema, hash, owner, pointer or explicit no-pointer reason, and validation.
- Generated corpus artifacts stay out of `data/`; committed reports are summaries and contracts.
- Performance budgets are explicit; DNF is recorded as evidence.

---

## File Structure

- Create `reports/parser-ir/publication-next-work.py`
  - Reads existing report summaries and emits the next-step admission ledger.
  - Owns only report synthesis; it must not invoke parsers or ABC materializers.
- Create `tests/parser-ir-publication-next-work-smoke.sh`
  - Builds small JSON fixtures and proves ledger classification behavior.
- Create `docs/superpowers/reports/2026-07-06-aozora-publication-next-work.summary.json`
  - Machine-readable current next-step ledger.
- Create `docs/superpowers/reports/2026-07-06-aozora-publication-next-work.md`
  - Human-readable current next-step ledger.
- Modify `justfile`
  - Add `parser-ir-publication-next-work-smoke`.
  - Add `parser-ir-publication-next-work-report`.
- Modify `docs/superpowers/plans/2026-07-06-complete-aozora-markup-publication-mapping.md`
  - Point the overarching plan to the new next-work ledger as the current dashboard.
- Optional later task files:
  - `reports/parser-ir/text-policy-delta.py`
  - `tests/parser-ir-text-policy-delta-smoke.sh`
  - `reports/parser-ir/adapter-fidelity-worksets.py`
  - `tests/parser-ir-adapter-fidelity-worksets-smoke.sh`
  - `docs/superpowers/specs/tei-p5-mapping-dossiers/*.md`

## Task 1: Add The Next-Work Ledger Report

**Files:**
- Create: `reports/parser-ir/publication-next-work.py`
- Create: `tests/parser-ir-publication-next-work-smoke.sh`
- Modify: `justfile`

**Interfaces:**
- Consumes:
  - `--source-summary PATH`
  - `--coverage-summary PATH`
  - `--matrix-summary PATH`
  - `--conversion-summary PATH`
  - `--source-reference-summary PATH`
  - `--performance-report PATH`
  - `--summary-json PATH`
  - `--report-md PATH`
- Produces:
  - JSON with `schema_version: "aozora-publication-next-work-v1"`
  - `verdict`
  - `completed_gates[]`
  - `next_work_items[]`
  - `calibration_only_items[]`
  - `parser_lanes`
  - `evidence_inputs`

- [ ] **Step 1: Write the failing smoke test**

Create `tests/parser-ir-publication-next-work-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="$(mktemp -d "${TMPDIR:-/tmp}/ab-publication-next-work.XXXXXX")"
trap 'rm -rf "$out_dir"' EXIT

source_summary="$out_dir/source.json"
coverage_summary="$out_dir/coverage.json"
matrix_summary="$out_dir/matrix.json"
conversion_summary="$out_dir/conversion.json"
reference_summary="$out_dir/reference.json"
performance_md="$out_dir/performance.md"
summary_json="$out_dir/next-work.summary.json"
report_md="$out_dir/next-work.md"

cat > "$source_summary" <<'JSON'
{
  "schema_version": "aozora-source-region-coverage-v1",
  "gate_status": "SOURCE_AUTHORITY_GATE_PASS",
  "works_scanned": 2,
  "source_region_coverage": {
    "unsupported_body_markup_occurrences": 0,
    "unknown_region_occurrences": 0,
    "unknown_unreviewed_occurrences": 0,
    "terminal_provenance_occurrences": 3,
    "colophon_metadata_occurrences": 7,
    "malformed_source_occurrences": 1
  }
}
JSON

cat > "$coverage_summary" <<'JSON'
{
  "verdict": "IR_PUBLICATION_COVERAGE_COMPLETE",
  "source_region_contract": {
    "verdict": "SOURCE_REGION_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"
  },
  "publication_bundle_contract": {
    "verdict": "PUBLICATION_BUNDLE_CONTRACT_CONFIRMED_BY_ABC_VALIDATION",
    "rows_validated": 285,
    "rows_failed": 0
  },
  "closure_gaps": {
    "classified_but_not_admitted": {"count": 0},
    "true_unsupported_gaps": {"count": 0}
  }
}
JSON

cat > "$matrix_summary" <<'JSON'
{
  "totals": {
    "rows_attempted": 10,
    "materialization_failed": 0,
    "rows_skipped": 1
  },
  "paragraph_origin_buckets": {
    "aligned": 2,
    "adapter_over_segmented": 4,
    "adapter_collapsed": 3,
    "converter_paragraph_mismatch": 1
  },
  "body_text_relation_buckets": {
    "equal": 1,
    "different": 9
  },
  "adapter_paragraph_origin_buckets": {
    "aozora2html": {"adapter_over_segmented": 2},
    "aozora-epub3": {"adapter_over_segmented": 2},
    "aozora-rs": {"adapter_collapsed": 2},
    "aozora2": {"converter_paragraph_mismatch": 1},
    "aozora": {"adapter_collapsed": 1}
  }
}
JSON

cat > "$conversion_summary" <<'JSON'
{
  "totals": {"files_attempted": 10, "files_succeeded": 10, "files_failed": 0},
  "compatibility_candidates": [
    {"aat_adapter": "aozora2html", "evidence_scope": {"files_scanned": 2}},
    {"aat_adapter": "aozora-epub3", "evidence_scope": {"files_scanned": 2}},
    {"aat_adapter": "aozora-rs", "evidence_scope": {"files_scanned": 2}},
    {"aat_adapter": "aozora2", "evidence_scope": {"files_scanned": 2}},
    {"aat_adapter": "aozora", "evidence_scope": {"files_scanned": 2}}
  ]
}
JSON

cat > "$reference_summary" <<'JSON'
{
  "verdict": "SOURCE_REFERENCE_RECONCILIATION_COMPLETE",
  "totals": {
    "observed_without_syntax_row": 0,
    "p4suta_feature_unmapped": 0
  }
}
JSON

cat > "$performance_md" <<'MD'
# Parser Performance Measurement

DNF is useful signal.
MD

python3 "$repo_root/reports/parser-ir/publication-next-work.py" \
  --source-summary "$source_summary" \
  --coverage-summary "$coverage_summary" \
  --matrix-summary "$matrix_summary" \
  --conversion-summary "$conversion_summary" \
  --source-reference-summary "$reference_summary" \
  --performance-report "$performance_md" \
  --summary-json "$summary_json" \
  --report-md "$report_md"

jq -e '.schema_version == "aozora-publication-next-work-v1"' "$summary_json" >/dev/null
jq -e '.verdict == "AOZORA_PUBLICATION_NEXT_WORK_OPEN"' "$summary_json" >/dev/null
jq -e '.completed_gates[] | select(.id == "publication_bundle_contract" and .status == "complete")' "$summary_json" >/dev/null
jq -e '.completed_gates[] | select(.id == "source_authority" and .status == "complete")' "$summary_json" >/dev/null
jq -e '([.parser_lanes[].adapter] | sort) == (["aozora", "aozora-epub3", "aozora-rs", "aozora2", "aozora2html"] | sort)' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "source_region_disposition_samples" and .owner == "ab-validator+abc")' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "text_policy_calibration" and .evidence.different_rows == 9)' "$summary_json" >/dev/null
jq -e '.next_work_items[] | select(.id == "adapter_fidelity_worksets" and .evidence.adapter_distortion_rows == 10)' "$summary_json" >/dev/null
jq -e '.calibration_only_items[] | select(.id == "tei_eaj_editorial_enrichment")' "$summary_json" >/dev/null
rg -n "Aozora Publication Next Work" "$report_md" >/dev/null
```

- [ ] **Step 2: Run the smoke to verify it fails**

Run:

```bash
bash tests/parser-ir-publication-next-work-smoke.sh
```

Expected: fail because `reports/parser-ir/publication-next-work.py` does not
exist.

- [ ] **Step 3: Implement the report builder**

Create `reports/parser-ir/publication-next-work.py` with these behaviors:

- parse all input JSON files;
- read the performance Markdown as text and record whether it mentions `DNF`;
- compute completed gates:
  - `source_authority` complete when source gate passes and unknown/unsupported
    counters are zero;
  - `ir_publication_coverage` complete when coverage verdict is
    `IR_PUBLICATION_COVERAGE_COMPLETE`;
  - `publication_bundle_contract` complete when bundle rows failed is zero;
  - `five_parser_conversion` complete when all five adapters are present and
    conversion total failures are zero;
  - `source_reference_reconciliation` complete when observed/unmapped counts are
    zero.
- always emit next-work items for:
  - `source_region_disposition_samples`;
  - `text_policy_calibration`;
  - `adapter_fidelity_worksets`;
  - `tei_p5_mapping_dossiers`;
  - `parser_acceptance_criteria`.
- compute `text_policy_calibration.evidence.different_rows` from
  `body_text_relation_buckets.different`;
- compute `adapter_fidelity_worksets.evidence.adapter_distortion_rows` as the
  sum of all paragraph-origin buckets except `aligned`,
  `source_note_back_routing`, and `page_break_projection`;
- emit `calibration_only_items` containing `tei_eaj_editorial_enrichment`.

- [ ] **Step 4: Run the smoke to verify it passes**

Run:

```bash
bash tests/parser-ir-publication-next-work-smoke.sh
```

Expected: pass.

- [ ] **Step 5: Add just recipes**

Modify `justfile`:

```make
parser-ir-publication-next-work-smoke:
    bash tests/parser-ir-publication-next-work-smoke.sh

parser-ir-publication-next-work-report:
    python3 reports/parser-ir/publication-next-work.py \
      --source-summary docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json \
      --coverage-summary docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json \
      --matrix-summary docs/superpowers/reports/2026-07-04-tei-eaj-generated-matrix-comparison.summary.json \
      --conversion-summary docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json \
      --source-reference-summary docs/superpowers/reports/2026-07-06-source-reference-reconciliation.summary.json \
      --performance-report docs/superpowers/reports/2026-07-04-parser-performance-measurement.md \
      --summary-json docs/superpowers/reports/2026-07-06-aozora-publication-next-work.summary.json \
      --report-md docs/superpowers/reports/2026-07-06-aozora-publication-next-work.md
```

- [ ] **Step 6: Generate current report**

Run:

```bash
just parser-ir-publication-next-work-report
```

Expected:

- `docs/superpowers/reports/2026-07-06-aozora-publication-next-work.summary.json`
  exists;
- `docs/superpowers/reports/2026-07-06-aozora-publication-next-work.md`
  exists;
- current verdict is `AOZORA_PUBLICATION_NEXT_WORK_OPEN` because follow-up work
  remains even though current gates are green.

- [ ] **Step 7: Verify**

Run:

```bash
bash tests/parser-ir-publication-next-work-smoke.sh
python3 -m py_compile reports/parser-ir/publication-next-work.py
jq -e '.verdict == "AOZORA_PUBLICATION_NEXT_WORK_OPEN"' docs/superpowers/reports/2026-07-06-aozora-publication-next-work.summary.json
jq -e '([.parser_lanes[].adapter] | sort) == (["aozora", "aozora-epub3", "aozora-rs", "aozora2", "aozora2html"] | sort)' docs/superpowers/reports/2026-07-06-aozora-publication-next-work.summary.json
git diff --check
```

Expected: all commands pass.

- [ ] **Step 8: Commit**

```bash
git add reports/parser-ir/publication-next-work.py \
  tests/parser-ir-publication-next-work-smoke.sh \
  docs/superpowers/reports/2026-07-06-aozora-publication-next-work.summary.json \
  docs/superpowers/reports/2026-07-06-aozora-publication-next-work.md \
  justfile
git commit -m "feat(publication): add next-work admission ledger"
```

## Task 2: Add Source-Region Disposition Sample Worksets

**Files:**
- Create: `reports/source-regions/source-region-disposition-samples.py`
- Create: `tests/source-region-disposition-samples-smoke.sh`
- Create: `docs/superpowers/reports/2026-07-06-source-region-disposition-samples.summary.json`
- Create: `docs/superpowers/reports/2026-07-06-source-region-disposition-samples.md`
- Modify: `justfile`

**Interfaces:**
- Consumes:
  - `--source-summary PATH`
  - `--source-report-md PATH`
  - `--policy PATH`
  - `--summary-json PATH`
  - `--report-md PATH`
- Produces source-class sample rows for:
  - `notation_legend`
  - `notation_placeholder`
  - `body_end_boundary`
  - `terminal_provenance`
  - `colophon_metadata`
  - `malformed_source`
  - `letter_address_origin`

- [ ] **Step 1: Write a failing smoke**

Create `tests/source-region-disposition-samples-smoke.sh` with fixture JSON
that includes the six source-region classes and policy dispositions. Assert:

```bash
jq -e '.verdict == "SOURCE_REGION_DISPOSITION_SAMPLES_READY"' "$summary_json"
jq -e '.classes[] | select(.source_class == "terminal_provenance" and .plaintext_projection == "omit")' "$summary_json"
jq -e '.classes[] | select(.source_class == "colophon_metadata" and .tei_target == "teiHeader/sourceDesc")' "$summary_json"
jq -e '.classes[] | select(.source_class == "letter_address_origin" and .status == "policy_needed")' "$summary_json"
```

- [ ] **Step 2: Run the smoke to verify it fails**

Run:

```bash
bash tests/source-region-disposition-samples-smoke.sh
```

Expected: fail because the script does not exist.

- [ ] **Step 3: Implement the sampler**

Implement a report builder that:

- reads `data/abc-schemas/data/source-region-publication-policy-v0.json`;
- emits all policy classes with `target_class`, `tei_target`,
  `custom_sidecar`, `plaintext_projection`, and `measurement_status`;
- adds `letter_address_origin` as `policy_needed` until ABC/source-region
  policy explicitly admits it;
- records current measured counters from the source summary.

- [ ] **Step 4: Generate current report**

Run:

```bash
python3 reports/source-regions/source-region-disposition-samples.py \
  --source-summary docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json \
  --source-report-md docs/superpowers/reports/2026-07-04-source-authority-representability.md \
  --policy data/abc-schemas/data/source-region-publication-policy-v0.json \
  --summary-json docs/superpowers/reports/2026-07-06-source-region-disposition-samples.summary.json \
  --report-md docs/superpowers/reports/2026-07-06-source-region-disposition-samples.md
```

- [ ] **Step 5: Verify and commit**

Run:

```bash
bash tests/source-region-disposition-samples-smoke.sh
python3 -m py_compile reports/source-regions/source-region-disposition-samples.py
git diff --check
```

Commit:

```bash
git add reports/source-regions/source-region-disposition-samples.py \
  tests/source-region-disposition-samples-smoke.sh \
  docs/superpowers/reports/2026-07-06-source-region-disposition-samples.summary.json \
  docs/superpowers/reports/2026-07-06-source-region-disposition-samples.md \
  justfile
git commit -m "feat(source): report source-region disposition samples"
```

## Task 3: Split TEI-EAJ Text Difference Causes

**Files:**
- Create: `reports/parser-ir/text-policy-delta.py`
- Create: `tests/parser-ir-text-policy-delta-smoke.sh`
- Create: `docs/superpowers/reports/2026-07-06-text-policy-delta.summary.json`
- Create: `docs/superpowers/reports/2026-07-06-text-policy-delta.md`
- Modify: `justfile`

**Interfaces:**
- Consumes: `--matrix-summary PATH`
- Produces:
  - `schema_version: "parser-ir-text-policy-delta-v1"`
  - `counts_by_cause`
  - `source_markup_backed_blockers`
  - `calibration_only_rows`

- [ ] **Step 1: Write a failing smoke**

The smoke fixture should include rows for `ruby_expanded_parenless`, `different`
with front/back profile, `generated_contains_tei_eaj`, and `equal`. Assert
that only source-markup-backed causes become blockers.

- [ ] **Step 2: Implement cause classification**

Use the matrix row fields already present:

- `text.surface_relations.ruby_expanded_parenless.relation`
- `text.surface_relations.base_drop_parentheticals.relation`
- `tei_eaj.structure_profiles`
- `classification.source_note_body_excluded`
- `classification.paragraph_origin_bucket`
- `text.body_base_text_relation`

Classify rows into:

- `ruby_or_parenthetical_policy`
- `front_back_source_region_policy`
- `body_visible_layout_policy`
- `adapter_text_loss`
- `tei_eaj_editorial_or_enrichment`
- `already_equal`
- `unknown_text_delta`

- [ ] **Step 3: Generate and verify current report**

Run:

```bash
python3 reports/parser-ir/text-policy-delta.py \
  --matrix-summary docs/superpowers/reports/2026-07-04-tei-eaj-generated-matrix-comparison.summary.json \
  --summary-json docs/superpowers/reports/2026-07-06-text-policy-delta.summary.json \
  --report-md docs/superpowers/reports/2026-07-06-text-policy-delta.md
bash tests/parser-ir-text-policy-delta-smoke.sh
python3 -m py_compile reports/parser-ir/text-policy-delta.py
git diff --check
```

- [ ] **Step 4: Commit**

```bash
git add reports/parser-ir/text-policy-delta.py \
  tests/parser-ir-text-policy-delta-smoke.sh \
  docs/superpowers/reports/2026-07-06-text-policy-delta.summary.json \
  docs/superpowers/reports/2026-07-06-text-policy-delta.md \
  justfile
git commit -m "feat(parser-ir): classify text policy deltas"
```

## Task 4: Generate Adapter Fidelity Worksets

**Files:**
- Create: `reports/parser-ir/adapter-fidelity-worksets.py`
- Create: `tests/parser-ir-adapter-fidelity-worksets-smoke.sh`
- Create: `docs/superpowers/reports/2026-07-06-adapter-fidelity-worksets.summary.json`
- Create: `docs/superpowers/reports/2026-07-06-adapter-fidelity-worksets.md`
- Modify: `justfile`

**Interfaces:**
- Consumes: `--matrix-summary PATH`
- Produces worksets for:
  - `adapter_over_segmented`
  - `adapter_collapsed`
  - `adapter_under_segmented`
  - `converter_paragraph_mismatch`
  - `adapter_raw_only`

- [ ] **Step 1: Write failing smoke**

Use a fixture with one row for each paragraph-origin bucket. Assert that each
workset contains the expected adapter/work IDs and that calibration-safe buckets
such as `aligned`, `source_note_back_routing`, and `page_break_projection` are
not emitted as adapter-fidelity blockers.

- [ ] **Step 2: Implement workset generator**

Generate JSON arrays of row objects under the report summary, not `/db`. Each
row object should include:

- `work_id`
- `adapter`
- `tei_eaj_file`
- `paragraph_origin_bucket`
- `body_text_relation`
- `parser_ir_nodes`
- `paragraph_count`
- `source_note_count`

- [ ] **Step 3: Generate current report and verify**

Run:

```bash
python3 reports/parser-ir/adapter-fidelity-worksets.py \
  --matrix-summary docs/superpowers/reports/2026-07-04-tei-eaj-generated-matrix-comparison.summary.json \
  --summary-json docs/superpowers/reports/2026-07-06-adapter-fidelity-worksets.summary.json \
  --report-md docs/superpowers/reports/2026-07-06-adapter-fidelity-worksets.md
bash tests/parser-ir-adapter-fidelity-worksets-smoke.sh
python3 -m py_compile reports/parser-ir/adapter-fidelity-worksets.py
git diff --check
```

- [ ] **Step 4: Commit**

```bash
git add reports/parser-ir/adapter-fidelity-worksets.py \
  tests/parser-ir-adapter-fidelity-worksets-smoke.sh \
  docs/superpowers/reports/2026-07-06-adapter-fidelity-worksets.summary.json \
  docs/superpowers/reports/2026-07-06-adapter-fidelity-worksets.md \
  justfile
git commit -m "feat(parser-ir): generate adapter fidelity worksets"
```

## Task 5: Scaffold TEI P5 Mapping Dossiers

**Files:**
- Create directory: `docs/superpowers/specs/tei-p5-mapping-dossiers/`
- Create: `docs/superpowers/specs/tei-p5-mapping-dossiers/README.md`
- Create one dossier per initial feature family:
  - `ruby.md`
  - `gaiji.md`
  - `page-line-breaks.md`
  - `paragraphs-headings.md`
  - `layout-indentation.md`
  - `emphasis-style.md`
  - `images-figures-captions.md`
  - `warigaki.md`
  - `kunten.md`
  - `source-apparatus-front-back.md`
  - `malformed-source-diagnostics.md`

**Interfaces:**
- Each dossier uses the same sections:
  - `Source Inventory`
  - `Parser-IR Representation`
  - `TEI P5 Target`
  - `ABC Extension Or Sidecar`
  - `Plaintext Projection`
  - `Current Evidence`
  - `Open Decisions`

- [ ] **Step 1: Create dossier README**

The README must state that TEI P5 references live in
`../abc/references/TEI/P5` and that each dossier must cite exact local paths
when TEI P5 is used for a claim.

- [ ] **Step 2: Create initial dossier stubs with known evidence**

Each dossier should include current report paths and a clear status:

- `admitted`
- `policy-needed`
- `adapter-fidelity-needed`
- `schema-needed`
- `diagnostic-only`

Do not leave any dossier with an empty section. If evidence is not yet measured,
write `Evidence status: not yet measured in this dossier` and add a concrete
next action.

- [ ] **Step 3: Verify no placeholders**

Run:

```bash
rg -n "T[B]D|T[O]DO|f[i]ll in|implement lat[e]r|app[r]opriate" docs/superpowers/specs/tei-p5-mapping-dossiers
git diff --check
```

Expected: `rg` finds no matches.

- [ ] **Step 4: Commit**

```bash
git add docs/superpowers/specs/tei-p5-mapping-dossiers
git commit -m "docs(tei): scaffold Aozora feature mapping dossiers"
```

## Task 6: Define Parser Acceptance Criteria

**Files:**
- Create: `docs/superpowers/specs/2026-07-06-comprehensive-parser-acceptance-criteria.md`
- Modify: `docs/superpowers/plans/2026-07-06-complete-aozora-markup-publication-mapping.md`

**Interfaces:**
- Consumes:
  - next-work ledger report from Task 1;
  - source-reference reconciliation report;
  - full-corpus conversion report;
  - parser performance report;
  - TEI P5 dossiers from Task 5.
- Produces:
  - source-coverage acceptance criteria;
  - parser-IR emission criteria;
  - publication-bundle criteria;
  - performance/DNF criteria;
  - explicit non-goals for editorial enrichment.

- [ ] **Step 1: Write the acceptance spec**

The spec must include these minimum criteria:

- all source inventory rows observed in the corpus are parsed or diagnostically
  preserved;
- zero unsupported body markup rows;
- zero unreviewed source-region rows;
- parser-IR validates against the current schema;
- representative and full-scope bundle validation pass;
- parser performance uses bounded budgets and reports DNF;
- TEI-EAJ editorial enrichment is not required unless source markup contains the
  fact.

- [ ] **Step 2: Update overarching plan**

Add the acceptance spec path under Workstream 3 and state that new parser work
must not start as a replacement for the admission ledger.

- [ ] **Step 3: Verify**

Run:

```bash
rg -n "comprehensive-parser-acceptance" docs/superpowers/plans/2026-07-06-complete-aozora-markup-publication-mapping.md
rg -n "TEI-EAJ editorial enrichment is not required" docs/superpowers/specs/2026-07-06-comprehensive-parser-acceptance-criteria.md
git diff --check
```

- [ ] **Step 4: Commit**

```bash
git add docs/superpowers/specs/2026-07-06-comprehensive-parser-acceptance-criteria.md \
  docs/superpowers/plans/2026-07-06-complete-aozora-markup-publication-mapping.md
git commit -m "docs(parser): define comprehensive parser acceptance criteria"
```

## Task 7: Wire The Next-Work Ledger Into The Overall Gate

**Files:**
- Modify: `reports/parser-ir/publication-coverage.py`
- Modify: `tests/parser-ir-publication-coverage-smoke.sh`
- Modify: `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`
- Modify: `docs/superpowers/reports/2026-07-06-ir-publication-coverage.md`
- Modify: `docs/superpowers/plans/2026-07-06-complete-aozora-markup-publication-mapping.md`

**Interfaces:**
- Consumes: `docs/superpowers/reports/2026-07-06-aozora-publication-next-work.summary.json`
- Produces: `next_work_dashboard` block inside the IR publication coverage summary.

- [ ] **Step 1: Extend the coverage smoke fixture**

Add a fixture next-work summary with:

```json
{
  "schema_version": "aozora-publication-next-work-v1",
  "verdict": "AOZORA_PUBLICATION_NEXT_WORK_OPEN",
  "next_work_items": [{"id": "text_policy_calibration"}]
}
```

Assert that publication coverage includes:

```bash
jq -e '.next_work_dashboard.verdict == "AOZORA_PUBLICATION_NEXT_WORK_OPEN"' "$summary_json"
jq -e '.next_work_dashboard.next_work_items_count == 1' "$summary_json"
```

- [ ] **Step 2: Add `--next-work-summary` to coverage report**

Modify `reports/parser-ir/publication-coverage.py` so the argument is optional.
When present, include:

- path;
- hash;
- verdict;
- next work item count;
- item IDs.

- [ ] **Step 3: Regenerate current coverage report**

Run:

```bash
python3 reports/parser-ir/publication-coverage.py \
  --parser-ir-schema data/abc-schemas/schemas/parser-ir.schema.json \
  --mapping data/aat-to-parser-ir-mapping-v1.json \
  --source-summary docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json \
  --matrix-summary docs/superpowers/reports/2026-07-04-tei-eaj-generated-matrix-comparison.summary.json \
  --source-delta-summary docs/superpowers/reports/2026-07-05-plain-prose-source-delta.summary.json \
  --custom-contract-schema data/abc-schemas/schemas/parser-ir-publication-preservation.schema.json \
  --bundle-validation-summary docs/superpowers/reports/2026-07-06-publication-bundle-full-matrix-validation.summary.json \
  --next-work-summary docs/superpowers/reports/2026-07-06-aozora-publication-next-work.summary.json \
  --summary-json docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json \
  --report-md docs/superpowers/reports/2026-07-06-ir-publication-coverage.md
```

- [ ] **Step 4: Verify**

Run:

```bash
bash tests/parser-ir-publication-coverage-smoke.sh
python3 -m py_compile reports/parser-ir/publication-coverage.py
jq -e '.next_work_dashboard.verdict == "AOZORA_PUBLICATION_NEXT_WORK_OPEN"' docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json
git diff --check
```

- [ ] **Step 5: Commit**

```bash
git add reports/parser-ir/publication-coverage.py \
  tests/parser-ir-publication-coverage-smoke.sh \
  docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json \
  docs/superpowers/reports/2026-07-06-ir-publication-coverage.md \
  docs/superpowers/plans/2026-07-06-complete-aozora-markup-publication-mapping.md
git commit -m "feat(publication): surface next-work dashboard in coverage"
```

## Final Verification

After all tasks:

```bash
bash tests/parser-ir-publication-next-work-smoke.sh
bash tests/source-region-disposition-samples-smoke.sh
bash tests/parser-ir-text-policy-delta-smoke.sh
bash tests/parser-ir-adapter-fidelity-worksets-smoke.sh
bash tests/parser-ir-publication-coverage-smoke.sh
python3 -m py_compile \
  reports/parser-ir/publication-next-work.py \
  reports/source-regions/source-region-disposition-samples.py \
  reports/parser-ir/text-policy-delta.py \
  reports/parser-ir/adapter-fidelity-worksets.py \
  reports/parser-ir/publication-coverage.py
jq -e '.verdict == "AOZORA_PUBLICATION_NEXT_WORK_OPEN"' docs/superpowers/reports/2026-07-06-aozora-publication-next-work.summary.json
jq -e '.next_work_dashboard.verdict == "AOZORA_PUBLICATION_NEXT_WORK_OPEN"' docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json
git diff --check
```

Expected: all commands pass.

## Self-Review

- Spec coverage: The plan implements the design spec's first required artifact
  first: a next-work admission ledger. It then creates focused slices for
  source-region disposition, text-policy calibration, adapter fidelity, TEI P5
  dossiers, parser acceptance, and final gate wiring.
- Placeholder scan: no placeholder tokens are intentionally present.
- Type consistency: report keys use stable snake_case JSON names and all paths
  match existing repo conventions.
