# Profile-Aware Level 3 TEI Admission

This report classifies measured parser-IR generated TEI evidence into profile-aware Level 3 admission buckets. TEI-EAJ remains comparison evidence; source inventory remains source authority.

## Verdict

- source_authority_gate: `SOURCE_AUTHORITY_GATE_PASS`
- parser_ir_infrastructure_verdict: `LEVEL3_IR_INFRASTRUCTURE_READY`
- plain_prose_verdict: `LEVEL3_PLAIN_PROSE_BLOCKED_ADAPTER_FIDELITY_AND_TEXT_POLICY`
- plain_prose_workset_verdict: `LEVEL3_PLAIN_PROSE_WORKSET_BLOCKED`
- blocking_owners: `adapter, policy`
- mapping_hash: `sha256:feaab2d246fd17d79dc979012893400e0f5faacc0df04e260bee4f2b129299bf`

## Mapping

- mapping_id: `https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe`
- mapping_version: `0.2.3`
- mapping_hash: `sha256:feaab2d246fd17d79dc979012893400e0f5faacc0df04e260bee4f2b129299bf`
- mapping_schema_hash: `sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4`
- target_parser_ir_schema_id: `https://w3id.org/abc/schemas/parser-ir.schema.json`
- target_parser_ir_schema_hash: `sha256:a1fcd348bf396d8d4e6f30ffb928b76b3802b594ea773ed6fa9e1dac52edf712`
- generated_mapping_rules: `714`

## Plain Prose Admission

- plaintext_surface: `body_base_text`
- ruby_expanded_surfaces: `admit_exact_structural_equivalence_only`
- metadata_policy: `exclude_typed_metadata_from_plaintext`

| metric | value |
|---|---:|
| rows_total | 30 |
| rows_passed | 2 |
| rows_failed | 28 |

## Plain Prose Workset Admission

- admission_unit: `tei_eaj_work_file_with_any_passing_parser_candidate`
- verdict: `LEVEL3_PLAIN_PROSE_WORKSET_BLOCKED`
- blocking_owners: `adapter, policy`

| metric | value |
|---|---:|
| work_files_total | 6 |
| work_files_passed | 1 |
| work_files_failed | 5 |

### Passing Candidates By Adapter

| adapter | work files |
|---|---:|
| aozora-epub3 | 1 |
| aozora2html | 1 |

### Workset Failures By Owner

| owner | work files |
|---|---:|
| adapter | 5 |
| policy | 5 |

### Failures By Owner

| owner | rows |
|---|---:|
| adapter | 24 |
| policy | 26 |

### Paragraph Origin Buckets

| bucket | rows |
|---|---:|
| adapter_collapsed | 9 |
| adapter_over_segmented | 13 |
| adapter_raw_only | 1 |
| adapter_under_segmented | 1 |
| aligned | 6 |

### Text Policy Buckets

| bucket | rows |
|---|---:|
| different | 22 |
| ruby_expanded_equal | 4 |
| ruby_expanded_parenless_generated_contains_tei_eaj | 3 |
| ruby_expanded_parenless_tei_eaj_contains_generated | 1 |

## Profile Lanes

| profile | label | rows | verdict |
|---|---|---:|---|
| drama | Drama | 50 | `LANE_POLICY_REQUIRED` |
| front_back_matter | Front/back matter | 5 | `LANE_POLICY_REQUIRED` |
| lineated_text | Lineated text | 120 | `LANE_POLICY_REQUIRED` |
| lv4_enrichment | Level 4 enrichment | 35 | `LANE_OUT_OF_SCOPE_FOR_LEVEL3` |
| notes | Notes | 35 | `LANE_POLICY_REQUIRED` |
| plain_prose | Plain prose | 30 | `LANE_READY_FOR_GATE_IMPLEMENTATION` |
| verse | Verse | 10 | `LANE_POLICY_REQUIRED` |

## Evidence Gaps

- rows: 5
- no_materializable_aat: 5

## Plain Prose Failed Rows

| work_id | adapter | paragraph origin | text bucket | owners | file |
|---|---|---|---|---|---|
| 15938 | aozora2html | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/15938_tei.xml` |
| 15938 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/15938_tei.xml` |
| 15938 | aozora-rs | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/15938_tei.xml` |
| 15938 | aozora2 | adapter_collapsed | different | adapter, policy | `data/complete/tei_lib_lv3/15938_tei.xml` |
| 15938 | aozora | adapter_raw_only | different | adapter, policy | `data/complete/tei_lib_lv3/15938_tei.xml` |
| 2509 | aozora2html | aligned | different | policy | `data/complete/tei_lib_lv3/2509_tei.xml` |
| 2509 | aozora-epub3 | aligned | different | policy | `data/complete/tei_lib_lv3/2509_tei.xml` |
| 2509 | aozora-rs | adapter_collapsed | different | adapter, policy | `data/complete/tei_lib_lv3/2509_tei.xml` |
| 2509 | aozora2 | adapter_collapsed | different | adapter, policy | `data/complete/tei_lib_lv3/2509_tei.xml` |
| 2509 | aozora | adapter_collapsed | different | adapter, policy | `data/complete/tei_lib_lv3/2509_tei.xml` |
| 43077 | aozora2html | adapter_over_segmented | ruby_expanded_parenless_tei_eaj_contains_generated | adapter, policy | `data/complete/tei_lib_lv3/43077_tei.xml` |
| 43077 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/43077_tei.xml` |
| 43077 | aozora-rs | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/43077_tei.xml` |
| 43077 | aozora2 | adapter_under_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/43077_tei.xml` |
| 43077 | aozora | adapter_collapsed | different | adapter, policy | `data/complete/tei_lib_lv3/43077_tei.xml` |
| 4872 | aozora-rs | adapter_over_segmented | ruby_expanded_equal | adapter | `data/complete/tei_lib_lv3/4872_tei.xml` |
| 4872 | aozora2 | adapter_collapsed | ruby_expanded_equal | adapter | `data/complete/tei_lib_lv3/4872_tei.xml` |
| 4872 | aozora | adapter_collapsed | different | adapter, policy | `data/complete/tei_lib_lv3/4872_tei.xml` |
| 56999 | aozora2html | adapter_over_segmented | ruby_expanded_parenless_generated_contains_tei_eaj | adapter, policy | `data/complete/tei_lib_lv3/56999_tei.xml` |
| 56999 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/56999_tei.xml` |
| 56999 | aozora-rs | adapter_over_segmented | ruby_expanded_parenless_generated_contains_tei_eaj | adapter, policy | `data/complete/tei_lib_lv3/56999_tei.xml` |
| 56999 | aozora2 | adapter_collapsed | ruby_expanded_parenless_generated_contains_tei_eaj | adapter, policy | `data/complete/tei_lib_lv3/56999_tei.xml` |
| 56999 | aozora | adapter_collapsed | different | adapter, policy | `data/complete/tei_lib_lv3/56999_tei.xml` |
| 57044 | aozora2html | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/57044_tei.xml` |
| 57044 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/57044_tei.xml` |
| 57044 | aozora-rs | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/57044_tei.xml` |
| 57044 | aozora2 | aligned | different | policy | `data/complete/tei_lib_lv3/57044_tei.xml` |
| 57044 | aozora | aligned | different | policy | `data/complete/tei_lib_lv3/57044_tei.xml` |
