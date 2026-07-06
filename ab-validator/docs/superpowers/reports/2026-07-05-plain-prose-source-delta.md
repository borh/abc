# Plain Prose Source Delta Probe

This report diagnoses plain-prose Level 3 blockers. It is not a Level 3 admission decision.

## Verdict

- parser_evidence_coverage: `FIVE_PARSER_EVIDENCE_COMPLETE`
- blocking_owners: `adapter, evidence, policy`
- mapping_id: `https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe`
- mapping_version: `0.2.3`

## Required Parsers

- `aozora2html`: 6 rows
- `aozora-epub3`: 6 rows
- `aozora-rs`: 6 rows
- `aozora2`: 6 rows
- `aozora`: 6 rows

## Classification Counts

| classification | rows |
|---|---:|
| adapter_paragraph_bug | 24 |
| evidence_gap | 5 |
| ruby_metadata_not_plaintext | 4 |
| source_text_policy_required | 22 |

## Rows

| work_id | adapter | classifications | owners | file |
|---|---|---|---|---|
| 15938 | aozora2html | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/15938_tei.xml` |
| 15938 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/15938_tei.xml` |
| 15938 | aozora-rs | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/15938_tei.xml` |
| 15938 | aozora2 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/15938_tei.xml` |
| 15938 | aozora | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/15938_tei.xml` |
| 2509 | aozora2html | source_text_policy_required | policy | `data/complete/tei_lib_lv3/2509_tei.xml` |
| 2509 | aozora-epub3 | source_text_policy_required | policy | `data/complete/tei_lib_lv3/2509_tei.xml` |
| 2509 | aozora-rs | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/2509_tei.xml` |
| 2509 | aozora2 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/2509_tei.xml` |
| 2509 | aozora | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/2509_tei.xml` |
| 43077 | aozora2html | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/43077_tei.xml` |
| 43077 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/43077_tei.xml` |
| 43077 | aozora-rs | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/43077_tei.xml` |
| 43077 | aozora2 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/43077_tei.xml` |
| 43077 | aozora | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/43077_tei.xml` |
| 4872 | aozora2html | none | none | `data/complete/tei_lib_lv3/4872_tei.xml` |
| 4872 | aozora-epub3 | none | none | `data/complete/tei_lib_lv3/4872_tei.xml` |
| 4872 | aozora-rs | adapter_paragraph_bug | adapter | `data/complete/tei_lib_lv3/4872_tei.xml` |
| 4872 | aozora2 | adapter_paragraph_bug | adapter | `data/complete/tei_lib_lv3/4872_tei.xml` |
| 4872 | aozora | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/4872_tei.xml` |
| 56999 | aozora2html | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/56999_tei.xml` |
| 56999 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/56999_tei.xml` |
| 56999 | aozora-rs | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/56999_tei.xml` |
| 56999 | aozora2 | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/56999_tei.xml` |
| 56999 | aozora | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/56999_tei.xml` |
| 57044 | aozora2html | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/57044_tei.xml` |
| 57044 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/57044_tei.xml` |
| 57044 | aozora-rs | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/57044_tei.xml` |
| 57044 | aozora2 | source_text_policy_required | policy | `data/complete/tei_lib_lv3/57044_tei.xml` |
| 57044 | aozora | source_text_policy_required | policy | `data/complete/tei_lib_lv3/57044_tei.xml` |
| None | missing | evidence_gap | evidence | `data/draft/tei_lib_lv2/01.xml` |
| None | missing | evidence_gap | evidence | `data/draft/tei_lib_lv2/02.xml` |
| None | missing | evidence_gap | evidence | `data/draft/tei_lib_lv2/yosano_genji_kiritsubo_ids.xml` |
| None | missing | evidence_gap | evidence | `data/etc/Curriculum vitae of Wakugawa Pēchin, Jitchaku Village.xml` |
| None | missing | evidence_gap | evidence | `data/etc/校異源氏物語_header更新版.xml` |
