# Plain Prose Source Delta Probe

This report diagnoses plain-prose Level 3 blockers. It is not a Level 3 admission decision.

## Verdict

- parser_evidence_coverage: `FIVE_PARSER_EVIDENCE_COMPLETE`
- blocking_owners: `adapter, evidence, policy`
- mapping_id: `https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe`
- mapping_version: `0.2.3`

## Required Parsers

- `aozora2html`: 27 rows
- `aozora-epub3`: 27 rows
- `aozora-rs`: 27 rows
- `aozora2`: 27 rows
- `aozora`: 27 rows

## Classification Counts

| classification | rows |
|---|---:|
| adapter_paragraph_bug | 86 |
| evidence_gap | 5 |
| ruby_metadata_not_plaintext | 64 |
| source_text_policy_required | 66 |
| tei_eaj_editorial_segmentation | 2 |

## Rows

| work_id | adapter | classifications | owners | file |
|---|---|---|---|---|
| 15099 | aozora2html | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/15099_tei.xml` |
| 15099 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/15099_tei.xml` |
| 15099 | aozora-rs | source_text_policy_required | policy | `data/complete/tei_lib_lv3/15099_tei.xml` |
| 15099 | aozora2 | source_text_policy_required | policy | `data/complete/tei_lib_lv3/15099_tei.xml` |
| 15099 | aozora | source_text_policy_required | policy | `data/complete/tei_lib_lv3/15099_tei.xml` |
| 15938 | aozora2html | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/15938_tei.xml` |
| 15938 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/15938_tei.xml` |
| 15938 | aozora-rs | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/15938_tei.xml` |
| 15938 | aozora2 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/15938_tei.xml` |
| 15938 | aozora | tei_eaj_editorial_segmentation, source_text_policy_required | policy | `data/complete/tei_lib_lv3/15938_tei.xml` |
| 236 | aozora2html | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/236_tei.xml` |
| 236 | aozora-epub3 | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/236_tei.xml` |
| 236 | aozora-rs | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/236_tei.xml` |
| 236 | aozora2 | ruby_metadata_not_plaintext | policy | `data/complete/tei_lib_lv3/236_tei.xml` |
| 236 | aozora | source_text_policy_required | policy | `data/complete/tei_lib_lv3/236_tei.xml` |
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
| 45093 | aozora2html | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/45093_tei.xml` |
| 45093 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/45093_tei.xml` |
| 45093 | aozora-rs | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/45093_tei.xml` |
| 45093 | aozora2 | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/45093_tei.xml` |
| 45093 | aozora | tei_eaj_editorial_segmentation, source_text_policy_required | policy | `data/complete/tei_lib_lv3/45093_tei.xml` |
| 4872 | aozora2html | ruby_metadata_not_plaintext | policy | `data/complete/tei_lib_lv3/4872_tei.xml` |
| 4872 | aozora-epub3 | ruby_metadata_not_plaintext | policy | `data/complete/tei_lib_lv3/4872_tei.xml` |
| 4872 | aozora-rs | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/4872_tei.xml` |
| 4872 | aozora2 | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/4872_tei.xml` |
| 4872 | aozora | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/4872_tei.xml` |
| 53386 | aozora2html | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/53386_tei.xml` |
| 53386 | aozora-epub3 | adapter_paragraph_bug | adapter | `data/complete/tei_lib_lv3/53386_tei.xml` |
| 53386 | aozora-rs | adapter_paragraph_bug | adapter | `data/complete/tei_lib_lv3/53386_tei.xml` |
| 53386 | aozora2 | ruby_metadata_not_plaintext | policy | `data/complete/tei_lib_lv3/53386_tei.xml` |
| 53386 | aozora | source_text_policy_required | policy | `data/complete/tei_lib_lv3/53386_tei.xml` |
| 56996 | aozora2html | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/56996_tei.xml` |
| 56996 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/56996_tei.xml` |
| 56996 | aozora-rs | ruby_metadata_not_plaintext | policy | `data/complete/tei_lib_lv3/56996_tei.xml` |
| 56996 | aozora2 | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/56996_tei.xml` |
| 56996 | aozora | source_text_policy_required | policy | `data/complete/tei_lib_lv3/56996_tei.xml` |
| 56998 | aozora2html | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/56998_tei.xml` |
| 56998 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/56998_tei.xml` |
| 56998 | aozora-rs | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/56998_tei.xml` |
| 56998 | aozora2 | ruby_metadata_not_plaintext | policy | `data/complete/tei_lib_lv3/56998_tei.xml` |
| 56998 | aozora | source_text_policy_required | policy | `data/complete/tei_lib_lv3/56998_tei.xml` |
| 56999 | aozora2html | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/56999_tei.xml` |
| 56999 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/56999_tei.xml` |
| 56999 | aozora-rs | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/56999_tei.xml` |
| 56999 | aozora2 | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/56999_tei.xml` |
| 56999 | aozora | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/56999_tei.xml` |
| 57001 | aozora2html | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57001_tei.xml` |
| 57001 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/57001_tei.xml` |
| 57001 | aozora-rs | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57001_tei.xml` |
| 57001 | aozora2 | ruby_metadata_not_plaintext | policy | `data/complete/tei_lib_lv3/57001_tei.xml` |
| 57001 | aozora | source_text_policy_required | policy | `data/complete/tei_lib_lv3/57001_tei.xml` |
| 57002 | aozora2html | adapter_paragraph_bug | adapter | `data/complete/tei_lib_lv3/57002_tei.xml` |
| 57002 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/57002_tei.xml` |
| 57002 | aozora-rs | adapter_paragraph_bug | adapter | `data/complete/tei_lib_lv3/57002_tei.xml` |
| 57002 | aozora2 | none | none | `data/complete/tei_lib_lv3/57002_tei.xml` |
| 57002 | aozora | source_text_policy_required | policy | `data/complete/tei_lib_lv3/57002_tei.xml` |
| 57003 | aozora2html | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57003_tei.xml` |
| 57003 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/57003_tei.xml` |
| 57003 | aozora-rs | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57003_tei.xml` |
| 57003 | aozora2 | ruby_metadata_not_plaintext | policy | `data/complete/tei_lib_lv3/57003_tei.xml` |
| 57003 | aozora | source_text_policy_required | policy | `data/complete/tei_lib_lv3/57003_tei.xml` |
| 57004 | aozora2html | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57004_tei.xml` |
| 57004 | aozora-epub3 | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57004_tei.xml` |
| 57004 | aozora-rs | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57004_tei.xml` |
| 57004 | aozora2 | ruby_metadata_not_plaintext | policy | `data/complete/tei_lib_lv3/57004_tei.xml` |
| 57004 | aozora | source_text_policy_required | policy | `data/complete/tei_lib_lv3/57004_tei.xml` |
| 57005 | aozora2html | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57005_tei.xml` |
| 57005 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/57005_tei.xml` |
| 57005 | aozora-rs | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57005_tei.xml` |
| 57005 | aozora2 | ruby_metadata_not_plaintext | policy | `data/complete/tei_lib_lv3/57005_tei.xml` |
| 57005 | aozora | source_text_policy_required | policy | `data/complete/tei_lib_lv3/57005_tei.xml` |
| 57006 | aozora2html | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57006_tei.xml` |
| 57006 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/57006_tei.xml` |
| 57006 | aozora-rs | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57006_tei.xml` |
| 57006 | aozora2 | ruby_metadata_not_plaintext | policy | `data/complete/tei_lib_lv3/57006_tei.xml` |
| 57006 | aozora | source_text_policy_required | policy | `data/complete/tei_lib_lv3/57006_tei.xml` |
| 57037 | aozora2html | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57037_tei.xml` |
| 57037 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/57037_tei.xml` |
| 57037 | aozora-rs | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57037_tei.xml` |
| 57037 | aozora2 | ruby_metadata_not_plaintext | policy | `data/complete/tei_lib_lv3/57037_tei.xml` |
| 57037 | aozora | source_text_policy_required | policy | `data/complete/tei_lib_lv3/57037_tei.xml` |
| 57038 | aozora2html | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57038_tei.xml` |
| 57038 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/57038_tei.xml` |
| 57038 | aozora-rs | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57038_tei.xml` |
| 57038 | aozora2 | ruby_metadata_not_plaintext | policy | `data/complete/tei_lib_lv3/57038_tei.xml` |
| 57038 | aozora | source_text_policy_required | policy | `data/complete/tei_lib_lv3/57038_tei.xml` |
| 57039 | aozora2html | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57039_tei.xml` |
| 57039 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/57039_tei.xml` |
| 57039 | aozora-rs | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57039_tei.xml` |
| 57039 | aozora2 | ruby_metadata_not_plaintext | policy | `data/complete/tei_lib_lv3/57039_tei.xml` |
| 57039 | aozora | source_text_policy_required | policy | `data/complete/tei_lib_lv3/57039_tei.xml` |
| 57040 | aozora2html | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57040_tei.xml` |
| 57040 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/57040_tei.xml` |
| 57040 | aozora-rs | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57040_tei.xml` |
| 57040 | aozora2 | ruby_metadata_not_plaintext | policy | `data/complete/tei_lib_lv3/57040_tei.xml` |
| 57040 | aozora | source_text_policy_required | policy | `data/complete/tei_lib_lv3/57040_tei.xml` |
| 57041 | aozora2html | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57041_tei.xml` |
| 57041 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/57041_tei.xml` |
| 57041 | aozora-rs | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57041_tei.xml` |
| 57041 | aozora2 | ruby_metadata_not_plaintext | policy | `data/complete/tei_lib_lv3/57041_tei.xml` |
| 57041 | aozora | source_text_policy_required | policy | `data/complete/tei_lib_lv3/57041_tei.xml` |
| 57042 | aozora2html | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57042_tei.xml` |
| 57042 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/57042_tei.xml` |
| 57042 | aozora-rs | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57042_tei.xml` |
| 57042 | aozora2 | ruby_metadata_not_plaintext | policy | `data/complete/tei_lib_lv3/57042_tei.xml` |
| 57042 | aozora | source_text_policy_required | policy | `data/complete/tei_lib_lv3/57042_tei.xml` |
| 57043 | aozora2html | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/57043_tei.xml` |
| 57043 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/57043_tei.xml` |
| 57043 | aozora-rs | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/57043_tei.xml` |
| 57043 | aozora2 | source_text_policy_required | policy | `data/complete/tei_lib_lv3/57043_tei.xml` |
| 57043 | aozora | source_text_policy_required | policy | `data/complete/tei_lib_lv3/57043_tei.xml` |
| 57044 | aozora2html | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/57044_tei.xml` |
| 57044 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/57044_tei.xml` |
| 57044 | aozora-rs | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/57044_tei.xml` |
| 57044 | aozora2 | source_text_policy_required | policy | `data/complete/tei_lib_lv3/57044_tei.xml` |
| 57044 | aozora | source_text_policy_required | policy | `data/complete/tei_lib_lv3/57044_tei.xml` |
| 57046 | aozora2html | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57046_tei.xml` |
| 57046 | aozora-epub3 | adapter_paragraph_bug, source_text_policy_required | adapter, policy | `data/complete/tei_lib_lv3/57046_tei.xml` |
| 57046 | aozora-rs | ruby_metadata_not_plaintext | policy | `data/complete/tei_lib_lv3/57046_tei.xml` |
| 57046 | aozora2 | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57046_tei.xml` |
| 57046 | aozora | source_text_policy_required | policy | `data/complete/tei_lib_lv3/57046_tei.xml` |
| 57047 | aozora2html | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57047_tei.xml` |
| 57047 | aozora-epub3 | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57047_tei.xml` |
| 57047 | aozora-rs | ruby_metadata_not_plaintext | policy | `data/complete/tei_lib_lv3/57047_tei.xml` |
| 57047 | aozora2 | adapter_paragraph_bug, ruby_metadata_not_plaintext | adapter, policy | `data/complete/tei_lib_lv3/57047_tei.xml` |
| 57047 | aozora | source_text_policy_required | policy | `data/complete/tei_lib_lv3/57047_tei.xml` |
| None | missing | evidence_gap | evidence | `data/draft/tei_lib_lv2/01.xml` |
| None | missing | evidence_gap | evidence | `data/draft/tei_lib_lv2/02.xml` |
| None | missing | evidence_gap | evidence | `data/draft/tei_lib_lv2/yosano_genji_kiritsubo_ids.xml` |
| None | missing | evidence_gap | evidence | `data/etc/Curriculum vitae of Wakugawa Pēchin, Jitchaku Village.xml` |
| None | missing | evidence_gap | evidence | `data/etc/校異源氏物語_header更新版.xml` |
