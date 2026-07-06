# Aozora Publication Next Work

Verdict: `AOZORA_PUBLICATION_NEXT_WORK_COMPLETE`

## Completed Gates

- `source_authority`: `complete`
- `ir_publication_coverage`: `complete`
- `publication_bundle_contract`: `complete`
- `five_parser_conversion`: `complete`
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
  - tei_p5_reference_root=abc/references/TEI/P5, root_exists=True, unverified=0
- `parser_acceptance_criteria` (ab-validator): `complete`
  - spec=docs/superpowers/specs/2026-07-06-comprehensive-parser-acceptance-criteria.md
  - spec_status=Accepted, required_evidence_inputs=6
  - required_evidence_paths_existing=6/6
  - missing_required_evidence_paths=[]

## Parser Lanes

- `aozora2html`: files_scanned=17689
- `aozora-epub3`: files_scanned=17844
- `aozora-rs`: files_scanned=17894
- `aozora2`: files_scanned=17856
- `aozora`: files_scanned=17886

## Calibration Only

- `tei_eaj_editorial_enrichment`
