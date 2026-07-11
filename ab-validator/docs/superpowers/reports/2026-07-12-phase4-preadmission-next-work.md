# Aozora Publication Next Work

Verdict: `AOZORA_PUBLICATION_NEXT_WORK_COMPLETE`

## Completed Gates

- `source_authority`: `complete`
- `ir_publication_coverage`: `complete`
- `publication_bundle_contract`: `complete`
- `five_parser_conversion`: `open`
- `source_reference_reconciliation`: `complete`

## Next Work Items

- `source_region_disposition_samples` (ab-validator+abc): `complete`
  - classes_total=7, admitted=7, policy_needed=0
  - policy_needed_classes=[]
  - evidence_needed_classes=[]
- `text_policy_calibration` (ab-validator+abc): `complete`
  - different_rows=271, source_markup_backed_blockers=223, calibration_only_rows=7
  - counts_by_cause={"adapter_text_loss": 55, "body_visible_layout_policy": 51, "front_back_source_region_policy": 50, "ruby_or_parenthetical_policy": 67, "tei_eaj_editorial_or_enrichment": 48, "unknown_text_delta": 0}
  - source_markup_backed_worksets={"adapter_text_loss": "docs/superpowers/reports/2026-07-06-text-policy-worksets/adapter_text_loss.json", "body_visible_layout_policy": "docs/superpowers/reports/2026-07-06-text-policy-worksets/body_visible_layout_policy.json", "front_back_source_region_policy": "docs/superpowers/reports/2026-07-06-text-policy-worksets/front_back_source_region_policy.json", "ruby_or_parenthetical_policy": "docs/superpowers/reports/2026-07-06-text-policy-worksets/ruby_or_parenthetical_policy.json"}
  - manual_classification_worksets={}
- `adapter_fidelity_worksets` (ab-validator): `complete`
  - adapter_distortion_rows=218
  - worksets={"adapter_collapsed": 67, "adapter_over_segmented": 127, "adapter_raw_only": 2, "adapter_under_segmented": 14, "converter_paragraph_mismatch": 8}
  - workset_files={"adapter_collapsed": "docs/superpowers/reports/2026-07-06-adapter-fidelity-worksets/adapter_collapsed/all.json", "adapter_over_segmented": "docs/superpowers/reports/2026-07-06-adapter-fidelity-worksets/adapter_over_segmented/all.json", "adapter_raw_only": "docs/superpowers/reports/2026-07-06-adapter-fidelity-worksets/adapter_raw_only/all.json", "adapter_under_segmented": "docs/superpowers/reports/2026-07-06-adapter-fidelity-worksets/adapter_under_segmented/all.json", "converter_paragraph_mismatch": "docs/superpowers/reports/2026-07-06-adapter-fidelity-worksets/converter_paragraph_mismatch/all.json"}
  - excluded_counts={"aligned": 60, "page_break_projection": 3, "source_note_back_routing": 4}
- `tei_p5_mapping_dossiers` (ab-validator+abc): `complete`
  - dossier_count=11
  - status_counts={"adapter-fidelity-needed": 1, "admitted": 4, "diagnostic-only": 1, "policy-needed": 3, "schema-needed": 2}
  - complete_section_count=11, incomplete_section_dossiers=[]
  - tei_p5_reference_count=34, file_count=34, directory_count=0
  - tei_p5_reference_root=$TEI_P5_ROOT, root_exists=True, unverified=0
- `parser_acceptance_criteria` (ab-validator): `complete`
  - spec=docs/superpowers/specs/2026-07-06-comprehensive-parser-acceptance-criteria.md
  - spec_status=Accepted, required_evidence_inputs=6
  - required_evidence_paths_existing=6/6
  - missing_required_evidence_paths=[]

## Parser Lanes

- `aozora2html`: files_scanned=0
- `aozora-epub3`: files_scanned=0
- `aozora-rs`: files_scanned=0
- `aozora2`: files_scanned=0
- `aozora`: files_scanned=0

## Calibration Only

- `tei_eaj_editorial_enrichment`

## Phase 4 Pre-Admission Citation Table (ceremony step 1, Task 16)

This section is written by hand — the generator above reconfirms gate/item
status but does not itself decide CLOSED vs. CARRIED. Every 2026-07-06
`aozora-publication-next-work` item is consumed and closed here; none are
replaced. The wholesale gate (Task 20) reads this table.

### CLOSED — reconfirmed by Phase 4 evidence, no new deltas

| 2026-07-06 item | Disposition | Phase 4 evidence |
| --- | --- | --- |
| `completed_gates.source_authority` | CLOSED | `docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json` (frozen, untouched by Phase 4) reconfirmed via this run's `source_authority_gate` in `2026-07-12-phase4-preadmission-coverage.summary.json` — `SOURCE_AUTHORITY_GATE_PASS`, all three counters still 0. |
| `completed_gates.publication_bundle_contract` | CLOSED | `docs/superpowers/reports/2026-07-06-publication-bundle-full-matrix-validation.summary.json` (frozen) reconfirmed `PUBLICATION_BUNDLE_CONTRACT_CONFIRMED_BY_ABC_VALIDATION`, 285/285 rows, in `2026-07-12-phase4-preadmission-coverage.summary.json`. |
| `completed_gates.source_reference_reconciliation` | CLOSED | `docs/superpowers/reports/2026-07-12-phase4-preadmission-reconciliation.summary.json` (this task, Step 2) — fresh run, verdict `SOURCE_REFERENCE_RECONCILIATION_COMPLETE`, matching the frozen 2026-07-06 evidence. |
| `next_work_items.source_region_disposition_samples` | CLOSED | `docs/superpowers/reports/2026-07-06-source-region-disposition-samples.summary.json` (frozen; Phase 4 does not touch source-region disposition sampling) — 7/7 classes admitted, reconfirmed unchanged in this refresh. |
| `next_work_items.text_policy_calibration` | CLOSED | `docs/superpowers/reports/2026-07-06-text-policy-delta.summary.json` + worksets (frozen; the legacy v1/0.2.8 mapping pinned for this pre-activation refresh surfaces no new deltas). |
| `next_work_items.adapter_fidelity_worksets` | CLOSED | `docs/superpowers/reports/2026-07-06-adapter-fidelity-worksets.summary.json` (frozen), reconfirmed unchanged. |
| `next_work_items.tei_p5_mapping_dossiers` | CLOSED | Reconfirmed complete against a freshly `nix build`-resolved `tei-p5-reference` root in this run: 34/34 files verified, 0 unverified, 11/11 dossiers with complete sections (`docs/superpowers/specs/tei-p5-mapping-dossiers`). |
| `next_work_items.parser_acceptance_criteria` | CLOSED | `docs/superpowers/specs/2026-07-06-comprehensive-parser-acceptance-criteria.md` (frozen), 6/6 required evidence paths still existing. |

### CARRIED — expected pre-activation state, cited but not re-closed here

| Item | Disposition | Why it does not gate admission |
| --- | --- | --- |
| `completed_gates.ir_publication_coverage` | CARRIED | The frozen 2026-07-06 coverage summary (this recipe's default `--coverage-summary` input, per brief) still reads `IR_PUBLICATION_COVERAGE_COMPLETE` and is what this next-work run reconfirms. The *separately regenerated* `2026-07-12-phase4-preadmission-coverage.summary.json` (Step 1 of this task, run against the legacy v1/0.2.8 mapping per the "pre-activation legacy five" constraint) instead reads `IR_PUBLICATION_COVERAGE_BLOCKED_CLASSIFIED_GAPS`, because the ABC custom-contract schema on disk has already rotated to `0.3.0` (`CUSTOM_CONTRACT_CANDIDATE_PROVIDED`, not yet `CONFIRMED_BY_ABC_INTEGRATION`) while the mapping pinned for this refresh is still the legacy `0.2.8`. This is expected drift from Phase 4 work already in flight on the ABC side, not a regression; it is the admission ceremony's (Task 17+) job to reconcile the mapping/schema pair and re-confirm, and Task 20 re-runs coverage under activated wiring as the second verification. All five required assertions for this task (the three source-region counters, `source_region_contract.verdict`, `parser_evidence_coverage.verdict`) hold regardless. |
| `completed_gates.five_parser_conversion` | CARRIED | Reads `open` in this run because the brief substitutes `--conversion-summary` with `2026-07-12-ab-aozora-phase4-c4-conversion-audit.summary.json` (Task 15), which carries exactly one `compatibility_candidates` lane (`ab-aozora`, the C4 fork candidate) rather than the five legacy comparison adapters. The frozen five-way matrix comparison (`docs/superpowers/reports/2026-07-04-tei-eaj-generated-matrix-comparison.summary.json`) still backs `parser_evidence_coverage.verdict == FIVE_PARSER_EVIDENCE_COMPLETE`, verified directly in Step 1. This lane is re-derived once activation wiring is live (Task 20). |
| Bare-toggle marker forms (`［＃横組み］…終わり` ~3,188; bare `［＃罫囲み］` ~25) | CARRIED | Explicit Phase 4 non-goal (`docs/superpowers/specs/2026-07-11-consolidated-parser-phase4-level3-admission-activation-design.md`, Decisions §1): deferred past Phase 4, stays raw-preserved. Raw preservation counts as covered evidence, so it does not affect `unsupported_body_markup_occurrences == 0` (verified 0 in Step 1); it remains the named classifier ceiling on the ledger. |
| Keigakomi 44-marker denominator residual (673 matrix vs 717 frozen) | CARRIED | Named ledger item per `docs/superpowers/reports/2026-07-11-keigakomi-yokogumi-denominator-attribution.md`; explicit Phase 4 non-goal — "nothing in Phase 4 depends on it." Does not gate admission. |
| Warigaki/kunten vocabulary | CARRIED | Awaits its own vocabulary ADR (Phase 4 non-goal, same design doc). Both remain raw-preserved so no evidence is lost — the warigaki mapping rows already surface as `classified_but_not_admitted` closure-gap entries (e.g. rule IDs `A-07`, `A-29`, `A-32`, `A-102`, `A-108`, `A-112` in `2026-07-12-phase4-preadmission-coverage.summary.json`) rather than as true unsupported gaps. |

Totals: **8 CLOSED**, **5 CARRIED** (2 completed-gate reconfirmation caveats + 3 named Phase 4 non-goal ledger items).
