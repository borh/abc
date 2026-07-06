# Complete Aozora Markup Publication Mapping Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement implementation slices derived from this plan. This document is the reviewable overarching plan; each implementation slice should get its own focused task plan before code changes.

**Goal:** Map all observed Aozora Bunko markup through parser-IR into TEI P5 where faithful, or into an ABC custom schema/sidecar/diagnostic contract where TEI is not exact.

**Architecture:** ab-validator remains the measurement and admission-gate repo. ABC remains the owner of TEI profile, renderer behavior, custom preservation schema, and design-bundle validation. The goal is reached by keeping source authority, parser evidence, IR coverage, TEI/profile policy, custom preservation, and plaintext policy synchronized as one publication contract.

**Tech Stack:** Rust crates in `crates/`, shell smoke tests in `tests/`, Python report builders under `reports/`, JSON schemas under `data/`, ABC schema snapshots under `data/abc-schemas/`, and ABC-owned Clojure/Nix validation in the sibling `../abc` repo.

**Genre:** This is an overarching roadmap/meta-plan. It is intentionally not a checkbox implementation plan; each workstream that changes code should get a focused sibling implementation plan using the repo's `### Task N` checkbox convention.

## Global Constraints

- TEI-EAJ is calibration evidence, not source authority.
- The success definition is Aozora markup publication accounting, not TEI Level 2/3 parity.
- Plaintext is visible body text only: no ruby readings, source apparatus, source notes, layout metadata, custom records, warnings, or provenance.
- All five parser lanes must stay visible in evidence when available: `aozora2html`, `aozora-epub3`, `aozora-rs`, `aozora2`, and `aozora`.
- Parser outputs are supporting evidence; source inventory remains authoritative for observed Aozora markup.
- Custom preservation is not a dumping ground; every custom fact must have schema, hash, owner, source pointer or reason for absence, and validation.
- Legacy source-authority counters remain compatibility aliases until ABC confirms migration to `source_region_coverage`.
- Generated corpus artifacts stay out of `data/`; committed reports are summaries and contracts.

---

## Current State

The current measured reports already satisfy the ab-validator-side coverage gate:

- `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`
  - `verdict == "IR_PUBLICATION_COVERAGE_COMPLETE"`
  - `next_work_dashboard.verdict == "AOZORA_PUBLICATION_NEXT_WORK_OPEN"`
  - `parser_evidence_coverage.verdict == "FIVE_PARSER_EVIDENCE_COMPLETE"`
  - `publication_bundle_contract.verdict == "PUBLICATION_BUNDLE_CONTRACT_CONFIRMED_BY_ABC_VALIDATION"`
  - required parsers: `aozora2html`, `aozora-epub3`, `aozora-rs`, `aozora2`, `aozora`
  - `closure_gaps.classified_but_not_admitted.count == 0`
  - `closure_gaps.true_unsupported_gaps.count == 0`
- `docs/superpowers/reports/2026-07-06-publication-bundle-full-matrix-validation.summary.json`
  - `verdict == "PUBLICATION_BUNDLE_BATCH_VALIDATION_PASSED"`
  - `scope.kind == "full-tei-eaj-matrix"`
  - `scope.rows_validated == 285`
  - `scope.rows_failed == 0`
  - `checks.plaintext_body_only == true`
  - validates the full five-parser TEI-EAJ matrix publication bundles across
    parser-IR, TEI XML, plaintext, preservation sidecar, source-region
    evidence, TEI manifest, and plaintext manifest
- `docs/superpowers/reports/2026-07-06-publication-bundle-validation.summary.json`
  - legacy ABC v0 fixture diagnostic; currently
    `verdict == "PUBLICATION_BUNDLE_VALIDATION_FAILED"` because the old fixture
    plaintext preserves the raw gaiji marker while the current body-only
    projection uses the gaiji fallback string
- `docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json`
  - `schema_version == "aozora-source-region-coverage-v1"`
  - `gate_status == "SOURCE_AUTHORITY_GATE_PASS"`
  - `works_scanned == 17894`
  - `unallowlisted_unknown_markers_total == 0`
  - `source_region_coverage.unsupported_body_markup_occurrences == 0`
  - `source_region_coverage.unknown_region_occurrences == 0`
  - `source_region_coverage.unknown_unreviewed_occurrences == 0`
  - `source_region_coverage.body_end_boundary_occurrences == 243`
  - `source_region_coverage.terminal_provenance_occurrences == 609`
  - `source_region_coverage.colophon_metadata_occurrences == 89416`
  - `source_region_coverage.letter_address_origin_occurrences == 6`
- `docs/superpowers/reports/2026-07-06-aozora-publication-next-work.summary.json`
  - current dashboard for source-region disposition samples, text-policy
    calibration, adapter-fidelity worksets, TEI P5 dossiers, and parser
    acceptance criteria

That is a measured admission state, not the end of the project. The remaining
work is durability and scale: ab-validator must keep the synced ABC contracts
current, broaden cross-artifact validation beyond the fixture bundle, and keep
adapter/parser evidence current as inputs change. A full five-parser TEI-EAJ
matrix bundle run has also been materialized and validated as diagnostic
evidence; it found plaintext policy blockers that must be closed before that
larger scope can replace the representative admission evidence.

## Workstream 1: ABC Source-Region Integration

**Purpose:** Make ABC consume source-region evidence as a first-class publication input.

**Inputs:**

- `docs/handoffs/source-region-coverage-abc-integration.md`
- `docs/superpowers/specs/2026-07-06-aozora-source-region-and-apparatus-contract.md`
- `docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json`

The canonical vocabulary for source regions and apparatus kinds is the source
region contract spec. The handoff and this roadmap are snapshots for ABC
coordination and must be reconciled with that spec if the vocabulary changes.

**Required ABC outcomes:**

- Accept `aozora-source-region-coverage-v1`.
- Require `SOURCE_AUTHORITY_GATE_PASS` for full publication admission.
- Require zero unsupported body markup, unknown region rows, and unknown unreviewed rows.
- Admit disposition policy for:
  - `notation_legend`
  - `notation_placeholder`
  - `body_end_boundary`
  - `terminal_provenance`
  - `colophon_metadata`
  - `malformed_source`
- Use explicit source-region counters for prevalence:
  `body_end_boundary_occurrences: 243`,
  `terminal_provenance_occurrences: 609`,
  `colophon_metadata_occurrences: 89416`, and
  `letter_address_origin_occurrences: 6`.
- Decide ABC disposition policy for measured `letter_address_origin` rows.
- Preserve malformed-source residues as diagnostics, not unsupported syntax.
- Keep plaintext body-only.

**ab-validator follow-up after ABC lands:**

- Done for ABC commit `95ace31 feat(parser-ir): validate source-region coverage`.
- Synced ABC snapshots:
  - `data/abc-schemas/schemas/source-region-coverage.schema.json`
  - `data/abc-schemas/schemas/manifest.schema.json`
  - `data/abc-schemas/data/source-region-publication-policy-v0.json`
- `docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json`
  now requires `source_region_contract.verdict ==
  "SOURCE_REGION_CONTRACT_CONFIRMED_BY_ABC_INTEGRATION"` for complete
  publication coverage.

**Review gate:** ab-validator reports still show `IR_PUBLICATION_COVERAGE_COMPLETE`
after syncing the ABC source-region schema, policy, and manifest sidecar role.

## Workstream 2: Cross-Artifact Publication Bundle Validation

**Purpose:** Prove parser-IR, TEI, custom preservation, source-region evidence, and plaintext are mutually consistent.

**Validation target:**

- Parser-IR validates against the pinned schema hash.
- TEI validates against ABC's TEI profile and Schematron.
- Preservation sidecar validates against ABC's custom schema.
- Every sidecar record with a TEI projection resolves to a TEI pointer.
- Every TEI `abc:*` projection resolves to a sidecar record.
- Every sidecar/source-region record resolves to either a source pointer, source-inventory row, or an explicit reason why no source pointer exists.
- Plaintext contains body-visible text only.

**Implementation shape:**

- Prefer ABC design-bundle validation as the owner of cross-file validation.
- Keep ab-validator smoke tests focused on measuring and checking the generated reports.
- Add ab-validator checks only for facts ab-validator owns: source-region counts, parser evidence completeness, mapping/coverage report shape, and synced schema hashes.

**Review gate:** A batch bundle with source apparatus, body markup, TEI
projection, sidecar records, and plaintext passes validation and is cited by
path/hash in ab-validator's report.

**Current status:** Confirmed for a freshly materialized 25-row representative
batch and promoted to the full 285-row five-parser TEI-EAJ matrix bundle
evidence. The older ABC parser-IR example fixture remains a diagnostic check;
it currently fails the stricter plaintext body-only check on gaiji raw-marker
projection and is not the admission evidence. Regenerate that diagnostic
fixture with:

```bash
just parser-ir-publication-bundle-validation
just parser-ir-publication-coverage-report
```

Regenerate the representative batch evidence with:

```bash
just parser-ir-level3-tei-eaj-generated-matrix-audit 5 /db/ab-validator/parser-ir/representative-publication-bundle-current-smoke docs/superpowers/reports/2026-07-04-tei-eaj-structural-expansion.summary.json 24
just parser-ir-publication-bundle-batch-validation /db/ab-validator/parser-ir/representative-publication-bundle-current-smoke representative docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json docs/superpowers/reports/2026-07-06-publication-bundle-batch-validation.md docs/superpowers/reports/2026-07-06-publication-bundle-batch-validation.summary.json ../abc
python3 reports/parser-ir/publication-coverage.py \
  --parser-ir-schema data/abc-schemas/schemas/parser-ir.schema.json \
  --mapping data/aat-to-parser-ir-mapping-v1.json \
  --source-summary docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json \
  --matrix-summary docs/superpowers/reports/2026-07-04-tei-eaj-generated-matrix-comparison.summary.json \
  --source-delta-summary docs/superpowers/reports/2026-07-05-plain-prose-source-delta.summary.json \
  --custom-contract-schema data/abc-schemas/schemas/parser-ir-publication-preservation.schema.json \
  --bundle-validation-summary docs/superpowers/reports/2026-07-06-publication-bundle-batch-validation.summary.json \
  --summary-json docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json \
  --report-md docs/superpowers/reports/2026-07-06-ir-publication-coverage.md
```

Existing pre-contract `/db` materialization directories are useful for
comparison, but they do not prove the current publication bundle contract if
they lack current preservation sidecars. The current publication coverage
report cites the full matrix bundle evidence:
`publication_bundle_contract.validation_scope == "batch"`,
`publication_bundle_contract.rows_validated == 285`, and
`publication_bundle_contract.rows_failed == 0`.

The current full five-parser TEI-EAJ matrix diagnostic evidence is:

- `docs/superpowers/reports/2026-07-06-publication-bundle-full-matrix-validation.summary.json`
  - `scope.kind == "full-tei-eaj-matrix"`
  - `scope.rows_validated == 285`
  - `scope.rows_failed == 0`
  - all bundle checks pass
- `docs/superpowers/reports/2026-07-06-publication-bundle-full-matrix-validation.md`

This full-matrix bundle result proves internal publication-bundle consistency:
Parser-IR, TEI, plaintext, manifests, source-region evidence, and preservation
sidecars agree for every materialized row. It does not prove TEI-EAJ admission
or complete source-region policy.

An earlier version of this diagnostic reported 10 `plaintext_body_only`
failures because the validator omitted visible `layout-span` text from its
parser-IR body projection. The corrected checker follows the ABC plaintext
policy: layout metadata is omitted, but body-visible layout text remains.

The underlying TEI-EAJ generated matrix still carries admission gaps. In
particular, letter address/origin rows such as `宛先` and `発信地` need a
source-region/publication policy decision before they can be treated as
front/header material rather than body text. TCY/page-number text such as `10`
is currently represented as body-visible `layout-span` text; changing that
requires an explicit source-region or layout-only policy, not a plaintext
renderer shortcut.

## Workstream 3: Parser Evidence And Adapter Fidelity

**Purpose:** Keep the five-parser evidence matrix useful without confusing parser consensus with source authority.

Parser acceptance criteria are defined in
`docs/superpowers/specs/2026-07-06-comprehensive-parser-acceptance-criteria.md`.
New parser work must not start as a replacement for the admission ledger; it
must consume the ledger and close named source coverage, parser-IR emission,
publication bundle, and performance criteria.

**Current parser evidence lanes:**

- `aozora2html`
- `aozora-epub3`
- `aozora-rs`
- `aozora2`
- `aozora`

**Priority order:**

1. Keep full-corpus AAT and AAT-to-parser-IR audits reproducible for all five lanes.
2. Keep performance numbers as comparison evidence, with DNF as useful signal rather than a reason to wait indefinitely.
3. Investigate adapter-specific distortions only when they affect publication accounting:
   - paragraph segmentation
   - source-note and back-matter routing
   - quote/caption association
   - layout markers such as tcy, yokogumi, keigakomi, and jisage
   - raw-source recovery and gaiji resolution
4. Do not require parsers to produce TEI-EAJ editorial enrichment such as people, places, speakers, roles, or speech acts.

**Review gate:** Missing parser evidence is explicit in the report. Parser failures are separated from IR/schema/policy gaps. Source-authority coverage does not depend on parser consensus.

## Workstream 4: TEI P5 And ABC Extension Policy

**Purpose:** Keep TEI as the primary publication surface without forcing non-TEI facts into misleading TEI elements.

**TEI-first facts:**

- text
- ruby base/reading
- gaiji where TEI `g`/`charDecl` is faithful
- page and line breaks
- paragraphs
- headings
- images and captions
- quotes where source evidence supports the target

**TEI policy projection facts:**

- source-note placement
- paragraph layout and indentation
- emphasis and style rendition
- quote/caption policy
- source apparatus routed to TEI header/front/back when ABC admits that route

**TEI plus ABC extension or sidecar facts:**

- accent source code
- warigaki split-line identity
- tcy/yokogumi/keigakomi source identity
- gaiji resolution status when TEI visible text is insufficient
- source pointers and source-inventory row IDs

**Custom-sidecar facts:**

- mapping identity and hashes
- parser/adapter identity
- divergence records
- warning/error diagnostics
- span coordinates when TEI does not preserve exact coordinate semantics
- producer metrics and provenance

These buckets are planning vocabulary. The report gate must still fold them
back into the canonical classes from the publication contract:
`tei_exact`, `tei_policy_projection`, `tei_plus_abc_extension`,
`custom_sidecar`, `body_visible_text_only`, or `unsupported_gap`.

**Review gate:** The coverage report has no `classified_but_not_admitted` rows and no true unsupported gaps after ABC profile/custom-contract hashes are synced.

## Workstream 5: Source Inventory And Specification Drift

**Purpose:** Prevent new source syntax or parser updates from silently escaping coverage.

**Maintenance triggers:**

- Aozora corpus update.
- Source scanner change.
- New parser adapter.
- AAT schema or parser-IR schema change.
- ABC TEI profile or custom preservation schema change.
- New external notation/spec source, including `P4suta/aozora-notation-spec`.

**Required action on each trigger:**

1. Regenerate source-authority report.
2. Regenerate parser evidence reports for affected adapters.
3. Regenerate IR publication coverage report.
4. Compare source-inventory rows against admitted TEI/custom classes.
5. Fail the gate on any unreviewed source marker, unknown region, unsupported body markup, or unmapped publication family.

**Review gate:** Updated reports include the new input identity and either preserve `IR_PUBLICATION_COVERAGE_COMPLETE` or identify a named blocker with owner and prevalence.

**Current status:** Source-reference reconciliation is measured for the current
source inventory, curated syntax table, and P4suta notation-spec comparison:

- `docs/superpowers/reports/2026-07-06-source-reference-reconciliation.summary.json`
  - `verdict == "SOURCE_REFERENCE_RECONCILIATION_COMPLETE"`
  - `observed_without_syntax_row == 0`
  - `p4suta_feature_unmapped == 0`
  - `p4suta_feature_comparison_only == 4`
- `docs/superpowers/reports/2026-07-06-source-reference-reconciliation.md`

Regenerate with:

```bash
just source-reference-reconciliation-report
```

## Workstream 6: Goal Definition And Non-Goals

**Goal statement to keep using:**

Complete Aozora Bunko markup publication mapping: every observed source markup and parser-IR fact is represented by TEI P5, TEI plus ABC extension, custom sidecar, plaintext body text, diagnostic preservation, or an explicit unsupported blocker.

**Do not use as the goal:**

- "TEI Level 2/3 parity"
- "TEI-EAJ exact match"
- "parser consensus"
- "all TEI P5 semantic enrichment"

**Explicit non-goals:**

- Inferring people, places, speakers, roles, or speech acts from plain text.
- Putting ruby readings or metadata into plaintext.
- Treating front/back/source apparatus as malformed source.
- Treating malformed source residue as unsupported Aozora markup.

## Immediate Next Tasks

1. Settle ABC disposition policy for measured `letter_address_origin` rows
   (`宛先` / `発信地`), which now have source evidence but remain
   `policy_needed`.
2. Keep the full-matrix materialization and
   `parser-ir-publication-bundle-batch-validation` current; only feed the
   full-matrix batch summary into `parser-ir-publication-coverage-report` while
   it has `rows_failed == 0`.
3. Keep all five parser evidence lanes current:
   `aozora2html`, `aozora-epub3`, `aozora-rs`, `aozora2`, and `aozora`.
4. Rerun `just source-reference-reconciliation-report` when the source
   inventory, Aozora manual evidence, syntax coverage table, or
   `P4suta/aozora-notation-spec` changes.

## Verification Commands

Run these in ab-validator after each contract-affecting change:

```bash
bash tests/source-representability-gate-smoke.sh
bash tests/source-inventory-smoke.sh
bash tests/source-reference-reconciliation-smoke.sh
bash tests/parser-ir-publication-bundle-smoke.sh
bash tests/parser-ir-publication-bundle-batch-smoke.sh
bash tests/parser-ir-publication-coverage-smoke.sh
jq -e '.gate_status == "SOURCE_AUTHORITY_GATE_PASS"' docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json
jq -e '.source_region_coverage.unsupported_body_markup_occurrences == 0 and .source_region_coverage.unknown_region_occurrences == 0 and .source_region_coverage.unknown_unreviewed_occurrences == 0' docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json
jq -e '.verdict == "SOURCE_REFERENCE_RECONCILIATION_COMPLETE" and .totals.observed_without_syntax_row == 0 and .totals.p4suta_feature_unmapped == 0' docs/superpowers/reports/2026-07-06-source-reference-reconciliation.summary.json
jq -e '.verdict == "PUBLICATION_BUNDLE_BATCH_VALIDATION_PASSED" and .scope.rows_validated == 25 and .scope.rows_failed == 0' docs/superpowers/reports/2026-07-06-publication-bundle-batch-validation.summary.json
jq -e '.publication_bundle_contract.verdict == "PUBLICATION_BUNDLE_CONTRACT_CONFIRMED_BY_ABC_VALIDATION"' docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json
jq -e '.publication_bundle_contract.validation_scope == "batch" and .publication_bundle_contract.rows_validated == 285 and .publication_bundle_contract.rows_failed == 0' docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json
jq -e '.next_work_dashboard.verdict == "AOZORA_PUBLICATION_NEXT_WORK_OPEN"' docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json
jq -e '.verdict == "IR_PUBLICATION_COVERAGE_COMPLETE"' docs/superpowers/reports/2026-07-06-ir-publication-coverage.summary.json
jq -e '.verdict == "PUBLICATION_BUNDLE_BATCH_VALIDATION_PASSED" and .scope.kind == "full-tei-eaj-matrix" and .scope.rows_validated == 285 and .scope.rows_failed == 0 and .checks.plaintext_body_only == true' docs/superpowers/reports/2026-07-06-publication-bundle-full-matrix-validation.summary.json
git diff --check
```

Run this when Nix checks are in scope:

```bash
system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix build ".#checks.${system}.source-representability-gate" --print-build-logs
nix build ".#checks.${system}.parser-ir-publication-bundle-batch-smoke" --print-build-logs
```

## Self-Review

- Scope: this plan covers the overall publication goal and deliberately defers per-slice implementation details to focused task plans.
- Source authority: Aozora source inventory remains the authority for source constructs.
- TEI-EAJ: comparison evidence only, not the goal.
- Plaintext: remains metadata-free visible body text.
- Parser evidence: all five active lanes are named.
- Cross-repo boundary: ABC owns disposition, renderer, profile, custom schema, and bundle validation; ab-validator owns measurement and admission reports.
- Placeholder scan: no unresolved placeholder markers.
