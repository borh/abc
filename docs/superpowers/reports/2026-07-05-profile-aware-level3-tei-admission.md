# Profile-Aware Level 3 TEI Admission

This report classifies measured parser-IR generated TEI evidence into profile-aware Level 3 admission buckets. TEI-EAJ remains comparison evidence; source inventory remains source authority.

## Verdict

- source_authority_gate: `SOURCE_AUTHORITY_GATE_PASS`
- parser_ir_infrastructure_verdict: `LEVEL3_IR_INFRASTRUCTURE_BLOCKED`
- plain_prose_verdict: `LEVEL3_PLAIN_PROSE_BLOCKED_ADAPTER_FIDELITY_AND_TEXT_POLICY_AND_ABC_RENDERER`
- blocking_owners: `adapter, policy, abc_renderer`
- mapping_hash: `sha256:21791c841557ced968464b38e42971e22830edb0efdb0be6faf285761532f770`

## Mapping

- mapping_id: `https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe`
- mapping_version: `0.2.3`
- mapping_hash: `sha256:21791c841557ced968464b38e42971e22830edb0efdb0be6faf285761532f770`
- mapping_schema_hash: `sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4`
- target_parser_ir_schema_id: `https://w3id.org/abc/schemas/parser-ir.schema.json`
- target_parser_ir_schema_hash: `sha256:8e56871965e647e40ade08fd9dd580a3516d33905be17957cc79750bd42ea64d`
- generated_mapping_rules: `127`

## Plain Prose Admission

- plaintext_surface: `body_base_text`
- ruby_expanded_surfaces: `diagnostic_only`
- metadata_policy: `exclude_typed_metadata_from_plaintext`

| metric | value |
|---|---:|
| rows_total | 108 |
| rows_passed | 1 |
| rows_failed | 107 |

### Failures By Owner

| owner | rows |
|---|---:|
| abc_renderer | 1 |
| adapter | 82 |
| policy | 103 |

### Paragraph Origin Buckets

| bucket | rows |
|---|---:|
| adapter_collapsed | 6 |
| adapter_over_segmented | 75 |
| adapter_under_segmented | 1 |
| aligned | 26 |

### Text Policy Buckets

| bucket | rows |
|---|---:|
| base_drop_parentheticals_equal | 15 |
| base_equal | 5 |
| different | 42 |
| ruby_expanded_equal | 4 |
| ruby_expanded_parenless_equal | 37 |
| ruby_expanded_parenless_generated_contains_tei_eaj | 4 |
| ruby_expanded_parenless_tei_eaj_contains_generated | 1 |

## Profile Lanes

| profile | label | rows | verdict |
|---|---|---:|---|
| drama | Drama | 34 | `LANE_POLICY_REQUIRED` |
| front_back_matter | Front/back matter | 8 | `LANE_POLICY_REQUIRED` |
| lv4_enrichment | Level 4 enrichment | 37 | `LANE_OUT_OF_SCOPE_FOR_LEVEL3` |
| notes | Notes | 27 | `LANE_POLICY_REQUIRED` |
| plain_prose | Plain prose | 108 | `LANE_READY_FOR_GATE_IMPLEMENTATION` |
| verse | Verse | 6 | `LANE_POLICY_REQUIRED` |

## Evidence Gaps

- rows: 5
- no_materializable_aat: 5

## Plain Prose Failed Rows

| work_id | adapter | paragraph origin | text bucket | owners | file |
|---|---|---|---|---|---|
| 15099 | aozora2html | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/15099_tei.xml` |
| 15099 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/15099_tei.xml` |
| 15099 | aozora-rs | aligned | different | policy | `data/complete/tei_lib_lv3/15099_tei.xml` |
| 15099 | aozora2 | aligned | different | policy | `data/complete/tei_lib_lv3/15099_tei.xml` |
| 15938 | aozora2html | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/15938_tei.xml` |
| 15938 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/15938_tei.xml` |
| 15938 | aozora-rs | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/15938_tei.xml` |
| 15938 | aozora2 | adapter_collapsed | different | abc_renderer, adapter, policy | `data/complete/tei_lib_lv3/15938_tei.xml` |
| 236 | aozora2html | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/236_tei.xml` |
| 236 | aozora-epub3 | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/236_tei.xml` |
| 236 | aozora-rs | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/236_tei.xml` |
| 236 | aozora2 | aligned | ruby_expanded_parenless_equal | policy | `data/complete/tei_lib_lv3/236_tei.xml` |
| 2509 | aozora2html | aligned | different | policy | `data/complete/tei_lib_lv3/2509_tei.xml` |
| 2509 | aozora-epub3 | aligned | different | policy | `data/complete/tei_lib_lv3/2509_tei.xml` |
| 2509 | aozora-rs | adapter_collapsed | different | adapter, policy | `data/complete/tei_lib_lv3/2509_tei.xml` |
| 2509 | aozora2 | adapter_collapsed | different | adapter, policy | `data/complete/tei_lib_lv3/2509_tei.xml` |
| 43077 | aozora2html | adapter_over_segmented | ruby_expanded_parenless_tei_eaj_contains_generated | adapter, policy | `data/complete/tei_lib_lv3/43077_tei.xml` |
| 43077 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/43077_tei.xml` |
| 43077 | aozora-rs | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/43077_tei.xml` |
| 43077 | aozora2 | adapter_under_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/43077_tei.xml` |
| 45093 | aozora2html | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/45093_tei.xml` |
| 45093 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/45093_tei.xml` |
| 45093 | aozora-rs | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/45093_tei.xml` |
| 45093 | aozora2 | adapter_collapsed | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/45093_tei.xml` |
| 4872 | aozora2html | aligned | ruby_expanded_equal | policy | `data/complete/tei_lib_lv3/4872_tei.xml` |
| 4872 | aozora-epub3 | aligned | ruby_expanded_equal | policy | `data/complete/tei_lib_lv3/4872_tei.xml` |
| 4872 | aozora-rs | adapter_over_segmented | ruby_expanded_equal | adapter, policy | `data/complete/tei_lib_lv3/4872_tei.xml` |
| 4872 | aozora2 | adapter_collapsed | ruby_expanded_equal | adapter, policy | `data/complete/tei_lib_lv3/4872_tei.xml` |
| 53386 | aozora2html | adapter_over_segmented | ruby_expanded_parenless_generated_contains_tei_eaj | adapter, policy | `data/complete/tei_lib_lv3/53386_tei.xml` |
| 53386 | aozora-epub3 | adapter_over_segmented | base_equal | adapter | `data/complete/tei_lib_lv3/53386_tei.xml` |
| 53386 | aozora-rs | adapter_over_segmented | base_equal | adapter | `data/complete/tei_lib_lv3/53386_tei.xml` |
| 53386 | aozora2 | aligned | ruby_expanded_parenless_generated_contains_tei_eaj | policy | `data/complete/tei_lib_lv3/53386_tei.xml` |
| 56996 | aozora2html | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/56996_tei.xml` |
| 56996 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/56996_tei.xml` |
| 56996 | aozora-rs | aligned | ruby_expanded_parenless_equal | policy | `data/complete/tei_lib_lv3/56996_tei.xml` |
| 56996 | aozora2 | adapter_over_segmented | base_drop_parentheticals_equal | adapter, policy | `data/complete/tei_lib_lv3/56996_tei.xml` |
| 56998 | aozora2html | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/56998_tei.xml` |
| 56998 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/56998_tei.xml` |
| 56998 | aozora-rs | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/56998_tei.xml` |
| 56998 | aozora2 | aligned | base_drop_parentheticals_equal | policy | `data/complete/tei_lib_lv3/56998_tei.xml` |
| 56999 | aozora2html | adapter_over_segmented | ruby_expanded_parenless_generated_contains_tei_eaj | adapter, policy | `data/complete/tei_lib_lv3/56999_tei.xml` |
| 56999 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/56999_tei.xml` |
| 56999 | aozora-rs | adapter_over_segmented | ruby_expanded_parenless_generated_contains_tei_eaj | adapter, policy | `data/complete/tei_lib_lv3/56999_tei.xml` |
| 56999 | aozora2 | adapter_collapsed | different | adapter, policy | `data/complete/tei_lib_lv3/56999_tei.xml` |
| 57001 | aozora2html | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57001_tei.xml` |
| 57001 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/57001_tei.xml` |
| 57001 | aozora-rs | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57001_tei.xml` |
| 57001 | aozora2 | aligned | base_drop_parentheticals_equal | policy | `data/complete/tei_lib_lv3/57001_tei.xml` |
| 57002 | aozora2html | adapter_over_segmented | base_equal | adapter | `data/complete/tei_lib_lv3/57002_tei.xml` |
| 57002 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/57002_tei.xml` |
| 57002 | aozora-rs | adapter_over_segmented | base_equal | adapter | `data/complete/tei_lib_lv3/57002_tei.xml` |
| 57003 | aozora2html | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57003_tei.xml` |
| 57003 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/57003_tei.xml` |
| 57003 | aozora-rs | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57003_tei.xml` |
| 57003 | aozora2 | aligned | base_drop_parentheticals_equal | policy | `data/complete/tei_lib_lv3/57003_tei.xml` |
| 57004 | aozora2html | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57004_tei.xml` |
| 57004 | aozora-epub3 | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57004_tei.xml` |
| 57004 | aozora-rs | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57004_tei.xml` |
| 57004 | aozora2 | aligned | base_drop_parentheticals_equal | policy | `data/complete/tei_lib_lv3/57004_tei.xml` |
| 57005 | aozora2html | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57005_tei.xml` |
| 57005 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/57005_tei.xml` |
| 57005 | aozora-rs | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57005_tei.xml` |
| 57005 | aozora2 | aligned | base_drop_parentheticals_equal | policy | `data/complete/tei_lib_lv3/57005_tei.xml` |
| 57006 | aozora2html | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57006_tei.xml` |
| 57006 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/57006_tei.xml` |
| 57006 | aozora-rs | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57006_tei.xml` |
| 57006 | aozora2 | aligned | different | policy | `data/complete/tei_lib_lv3/57006_tei.xml` |
| 57037 | aozora2html | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57037_tei.xml` |
| 57037 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/57037_tei.xml` |
| 57037 | aozora-rs | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57037_tei.xml` |
| 57037 | aozora2 | aligned | different | policy | `data/complete/tei_lib_lv3/57037_tei.xml` |
| 57038 | aozora2html | adapter_over_segmented | base_drop_parentheticals_equal | adapter, policy | `data/complete/tei_lib_lv3/57038_tei.xml` |
| 57038 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/57038_tei.xml` |
| 57038 | aozora-rs | adapter_over_segmented | base_drop_parentheticals_equal | adapter, policy | `data/complete/tei_lib_lv3/57038_tei.xml` |
| 57038 | aozora2 | aligned | base_drop_parentheticals_equal | policy | `data/complete/tei_lib_lv3/57038_tei.xml` |
| 57039 | aozora2html | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57039_tei.xml` |
| 57039 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/57039_tei.xml` |
| 57039 | aozora-rs | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57039_tei.xml` |
| 57039 | aozora2 | aligned | base_drop_parentheticals_equal | policy | `data/complete/tei_lib_lv3/57039_tei.xml` |
| 57040 | aozora2html | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57040_tei.xml` |
| 57040 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/57040_tei.xml` |
| 57040 | aozora-rs | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57040_tei.xml` |
| 57040 | aozora2 | aligned | base_drop_parentheticals_equal | policy | `data/complete/tei_lib_lv3/57040_tei.xml` |
| 57041 | aozora2html | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57041_tei.xml` |
| 57041 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/57041_tei.xml` |
| 57041 | aozora-rs | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57041_tei.xml` |
| 57041 | aozora2 | aligned | base_drop_parentheticals_equal | policy | `data/complete/tei_lib_lv3/57041_tei.xml` |
| 57042 | aozora2html | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57042_tei.xml` |
| 57042 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/57042_tei.xml` |
| 57042 | aozora-rs | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57042_tei.xml` |
| 57042 | aozora2 | aligned | base_drop_parentheticals_equal | policy | `data/complete/tei_lib_lv3/57042_tei.xml` |
| 57043 | aozora2html | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/57043_tei.xml` |
| 57043 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/57043_tei.xml` |
| 57043 | aozora-rs | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/57043_tei.xml` |
| 57043 | aozora2 | aligned | different | policy | `data/complete/tei_lib_lv3/57043_tei.xml` |
| 57044 | aozora2html | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/57044_tei.xml` |
| 57044 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/57044_tei.xml` |
| 57044 | aozora-rs | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/57044_tei.xml` |
| 57044 | aozora2 | aligned | different | policy | `data/complete/tei_lib_lv3/57044_tei.xml` |
| 57046 | aozora2html | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57046_tei.xml` |
| 57046 | aozora-epub3 | adapter_over_segmented | different | adapter, policy | `data/complete/tei_lib_lv3/57046_tei.xml` |
| 57046 | aozora-rs | aligned | ruby_expanded_parenless_equal | policy | `data/complete/tei_lib_lv3/57046_tei.xml` |
| 57046 | aozora2 | adapter_over_segmented | base_drop_parentheticals_equal | adapter, policy | `data/complete/tei_lib_lv3/57046_tei.xml` |
| 57047 | aozora2html | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57047_tei.xml` |
| 57047 | aozora-epub3 | adapter_over_segmented | ruby_expanded_parenless_equal | adapter, policy | `data/complete/tei_lib_lv3/57047_tei.xml` |
| 57047 | aozora-rs | aligned | ruby_expanded_parenless_equal | policy | `data/complete/tei_lib_lv3/57047_tei.xml` |
| 57047 | aozora2 | adapter_over_segmented | base_drop_parentheticals_equal | adapter, policy | `data/complete/tei_lib_lv3/57047_tei.xml` |
