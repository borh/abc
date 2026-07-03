# Aozora2html Policy Samples

- run_dir: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z`
- audit: `docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.md`
- limit_per_bucket: 10

## Observed warigaki

| Work ID | Report | AAT | Node Path | Kind | Syntax | Failures | Preview |
|---|---|---|---|---|---|---|---|
| 000005_53194 | `check-reports/aozora2html-adapter/000005_53194-ebb0cbaf64b3.json` | `aat/aozora2html-adapter/000005_53194-ebb0cbaf64b3.json` | `$.blocks[249].content[1]` | warigaki |  |  | `{"kind": "warigaki"}` |
| 000034_519 | `check-reports/aozora2html-adapter/000034_519-059f2347de78.json` | `aat/aozora2html-adapter/000034_519-0975863ec5cb.json` | `$.blocks[13].children[0].content[0]` | warigaki |  |  | `{"kind": "warigaki"}` |
| 000038_42202 | `check-reports/aozora2html-adapter/000038_42202-afcc2eabc8f9.json` | `aat/aozora2html-adapter/000038_42202-afcc2eabc8f9.json` | `$.blocks[13].children[2].content[5]` | warigaki |  | visible_text_body_order | `{"kind": "warigaki"}` |
| 000057_43276 | `check-reports/aozora2html-adapter/000057_43276-14d0be58d734.json` | `aat/aozora2html-adapter/000057_43276-14d0be58d734.json` | `$.blocks[24].content[5]` | warigaki |  |  | `{"kind": "warigaki"}` |
| 000082_56329 | `check-reports/aozora2html-adapter/000082_56329-7ff54d59e532.json` | `aat/aozora2html-adapter/000082_56329-7ff54d59e532.json` | `$.blocks[0].content[13]` | warigaki |  |  | `{"kind": "warigaki"}` |
| 000083_46289 | `check-reports/aozora2html-adapter/000083_46289-16b111c5dae5.json` | `aat/aozora2html-adapter/000083_46289-16b111c5dae5.json` | `$.blocks[50].content[3]` | warigaki |  | visible_text_body_order | `{"kind": "warigaki"}` |
| 000093_525 | `check-reports/aozora2html-adapter/000093_525-a4f389b18ad3.json` | `aat/aozora2html-adapter/000093_525-dea69ff600ac.json` | `$.blocks[6].content[1]` | warigaki |  |  | `{"kind": "warigaki"}` |
| 000094_2525 | `check-reports/aozora2html-adapter/000094_2525-9c05271ce1b8.json` | `aat/aozora2html-adapter/000094_2525-9c05271ce1b8.json` | `$.blocks[93].content[1]` | warigaki |  | visible_text_body_order | `{"kind": "warigaki"}` |
| 000106_52958 | `check-reports/aozora2html-adapter/000106_52958-e1c80d087fdd.json` | `aat/aozora2html-adapter/000106_52958-e1c80d087fdd.json` | `$.blocks[942].content[5]` | warigaki |  | gaiji_resolution, visible_text_body_order | `{"kind": "warigaki"}` |
| 000106_56858 | `check-reports/aozora2html-adapter/000106_56858-b4d3a3c5ac6e.json` | `aat/aozora2html-adapter/000106_56858-b4d3a3c5ac6e.json` | `$.blocks[1].children[0].content[1].content[23]` | warigaki |  | visible_text_body_order | `{"kind": "warigaki"}` |

## warigaki adapter_timeout_or_protocol_error

| Work ID | Report | AAT | Node Path | Kind | Syntax | Failures | Preview |
|---|---|---|---|---|---|---|---|
| 000006_1869 | `check-reports/aozora2html-adapter/000006_1869-62320f0f4474.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000077_1323 | `check-reports/aozora2html-adapter/000077_1323-b51132c1dd72.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000081_47027 | `check-reports/aozora2html-adapter/000081_47027-a6a7780bf470.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000096_2093 | `check-reports/aozora2html-adapter/000096_2093-71cae78a6bed.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000106_56910 | `check-reports/aozora2html-adapter/000106_56910-e74ada484515.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000118_614 | `check-reports/aozora2html-adapter/000118_614-ee8775977dc9.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000125_1317 | `check-reports/aozora2html-adapter/000125_1317-75eca9fa8fae.json` | `` | `` |  |  | adapter_protocol_error | `{}` |
| 000179_943 | `check-reports/aozora2html-adapter/000179_943-0c3f07a107dd.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000207_24382 | `check-reports/aozora2html-adapter/000207_24382-96674b532103.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000207_42217 | `check-reports/aozora2html-adapter/000207_42217-8e421e57608e.json` | `` | `` |  |  | adapter_timeout | `{}` |

## warigaki parse_incomplete

| Work ID | Report | AAT | Node Path | Kind | Syntax | Failures | Preview |
|---|---|---|---|---|---|---|---|
| 000125_45231 | `check-reports/aozora2html-adapter/000125_45231-ef96c3f5d895.json` | `aat/aozora2html-adapter/000125_45231-ef96c3f5d895.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000284_2227 | `check-reports/aozora2html-adapter/000284_2227-a860417d25bf.json` | `aat/aozora2html-adapter/000284_2227-a860417d25bf.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000284_2665 | `check-reports/aozora2html-adapter/000284_2665-0b38a88d5fa2.json` | `aat/aozora2html-adapter/000284_2665-0b38a88d5fa2.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000603_4729 | `check-reports/aozora2html-adapter/000603_4729-2a1e8bddff5c.json` | `aat/aozora2html-adapter/000603_4729-2a1e8bddff5c.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000933_47196 | `check-reports/aozora2html-adapter/000933_47196-806bb4bcf38e.json` | `aat/aozora2html-adapter/000933_47196-806bb4bcf38e.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |

## warigaki report_failed_other_property

| Work ID | Report | AAT | Node Path | Kind | Syntax | Failures | Preview |
|---|---|---|---|---|---|---|---|
| 000082_49526 | `check-reports/aozora2html-adapter/000082_49526-0ce076982149.json` | `aat/aozora2html-adapter/000082_49526-0ce076982149.json` | `` |  |  | visible_text_body_order | `{}` |
| 000146_48313 | `check-reports/aozora2html-adapter/000146_48313-c0fb76f5a906.json` | `aat/aozora2html-adapter/000146_48313-c0fb76f5a906.json` | `` |  |  | visible_text_body_order | `{}` |
| 000157_43445 | `check-reports/aozora2html-adapter/000157_43445-78d601d2bde4.json` | `aat/aozora2html-adapter/000157_43445-78d601d2bde4.json` | `` |  |  | gaiji_resolution, ruby_completeness | `{}` |
| 000933_46949 | `check-reports/aozora2html-adapter/000933_46949-a8e448a10fbb.json` | `aat/aozora2html-adapter/000933_46949-a8e448a10fbb.json` | `` |  |  | visible_text_body_order | `{}` |
| 000933_47180 | `check-reports/aozora2html-adapter/000933_47180-a22148699c06.json` | `aat/aozora2html-adapter/000933_47180-a22148699c06.json` | `` |  |  | visible_text_body_order | `{}` |
| 000933_47182 | `check-reports/aozora2html-adapter/000933_47182-2ecdb3807268.json` | `aat/aozora2html-adapter/000933_47182-2ecdb3807268.json` | `` |  |  | visible_text_body_order | `{}` |
| 000933_47186 | `check-reports/aozora2html-adapter/000933_47186-9e15a76bb944.json` | `aat/aozora2html-adapter/000933_47186-9e15a76bb944.json` | `` |  |  | visible_text_body_order | `{}` |
| 001059_5075 | `check-reports/aozora2html-adapter/001059_5075-31637c1f95b9.json` | `aat/aozora2html-adapter/001059_5075-31637c1f95b9.json` | `` |  |  | visible_text_body_order | `{}` |
| 001123_42930 | `check-reports/aozora2html-adapter/001123_42930-2b0719322fb7.json` | `aat/aozora2html-adapter/001123_42930-2b0719322fb7.json` | `` |  |  | visible_text_body_order | `{}` |
| 001127_45250 | `check-reports/aozora2html-adapter/001127_45250-a17afcbe1bda.json` | `aat/aozora2html-adapter/001127_45250-a17afcbe1bda.json` | `` |  |  | visible_text_body_order | `{}` |

## Warigaki source feature without AAT observation

| Work ID | Report | AAT | Node Path | Kind | Syntax | Failures | Preview |
|---|---|---|---|---|---|---|---|
| 000006_1868 | `check-reports/aozora2html-adapter/000006_1868-e598e91a19ff.json` | `aat/aozora2html-adapter/000006_1868-e598e91a19ff.json` | `` |  |  |  | `{}` |
| 000075_4250 | `check-reports/aozora2html-adapter/000075_4250-7e32f80f0150.json` | `aat/aozora2html-adapter/000075_4250-7e32f80f0150.json` | `` |  |  |  | `{}` |
| 000106_57905 | `check-reports/aozora2html-adapter/000106_57905-d4c02a20ee39.json` | `aat/aozora2html-adapter/000106_57905-d4c02a20ee39.json` | `` |  |  |  | `{}` |
| 000121_45086 | `check-reports/aozora2html-adapter/000121_45086-1a5c986d414a.json` | `aat/aozora2html-adapter/000121_45086-1a5c986d414a.json` | `` |  |  |  | `{}` |
| 000125_43656 | `check-reports/aozora2html-adapter/000125_43656-72671814e76e.json` | `aat/aozora2html-adapter/000125_43656-72671814e76e.json` | `` |  |  |  | `{}` |
| 000125_45230 | `check-reports/aozora2html-adapter/000125_45230-80ae1c6a2cc4.json` | `aat/aozora2html-adapter/000125_45230-80ae1c6a2cc4.json` | `` |  |  |  | `{}` |
| 000146_47356 | `check-reports/aozora2html-adapter/000146_47356-fca1bd12d2a6.json` | `aat/aozora2html-adapter/000146_47356-fca1bd12d2a6.json` | `` |  |  |  | `{}` |
| 000146_48131 | `check-reports/aozora2html-adapter/000146_48131-e0cc23d97452.json` | `aat/aozora2html-adapter/000146_48131-e0cc23d97452.json` | `` |  |  |  | `{}` |
| 000284_2226 | `check-reports/aozora2html-adapter/000284_2226-b56fd371b06f.json` | `aat/aozora2html-adapter/000284_2226-b56fd371b06f.json` | `` |  |  |  | `{}` |
| 000284_2942 | `check-reports/aozora2html-adapter/000284_2942-2854527a09a0.json` | `aat/aozora2html-adapter/000284_2942-2854527a09a0.json` | `` |  |  |  | `{}` |

## Observed kunten

| Work ID | Report | AAT | Node Path | Kind | Syntax | Failures | Preview |
|---|---|---|---|---|---|---|---|
| 000038_1408 | `check-reports/aozora2html-adapter/000038_1408-77f192c358bc.json` | `aat/aozora2html-adapter/000038_1408-77f192c358bc.json` | `$.blocks[3].content[68]` | style | kunten.kaeriten |  | `{"kind": "style", "style_type": "kaeriten", "x-marker": "レ", "x-provenance": "parser"}` |
| 000042_1694 | `check-reports/aozora2html-adapter/000042_1694-eb8c88c4846e.json` | `aat/aozora2html-adapter/000042_1694-eb8c88c4846e.json` | `$.blocks[8].content[1].base_content[1]` | style | kunten.kaeriten |  | `{"kind": "style", "style_type": "kaeriten", "x-marker": "レ", "x-provenance": "parser"}` |
| 000050_3581 | `check-reports/aozora2html-adapter/000050_3581-405fd70ec41b.json` | `aat/aozora2html-adapter/000050_3581-405fd70ec41b.json` | `$.blocks[43].content[1].base_content[1]` | style | kunten.kaeriten |  | `{"kind": "style", "style_type": "kaeriten", "x-marker": "レ", "x-provenance": "parser"}` |
| 000051_1436 | `check-reports/aozora2html-adapter/000051_1436-8276953442b7.json` | `aat/aozora2html-adapter/000051_1436-8276953442b7.json` | `$.blocks[4].content[3].base_content[1]` | style | kunten.kaeriten | visible_text_body_order | `{"kind": "style", "style_type": "kaeriten", "x-marker": "レ", "x-provenance": "parser"}` |
| 000051_2709 | `check-reports/aozora2html-adapter/000051_2709-bef45ad83cdd.json` | `aat/aozora2html-adapter/000051_2709-bef45ad83cdd.json` | `$.blocks[33].content[49]` | style | kunten.kaeriten | visible_text_body_order | `{"kind": "style", "style_type": "kaeriten", "x-marker": "レ", "x-provenance": "parser"}` |
| 000051_3199 | `check-reports/aozora2html-adapter/000051_3199-d0530bb2dec9.json` | `aat/aozora2html-adapter/000051_3199-d0530bb2dec9.json` | `$.blocks[22].content[17]` | style | kunten.kaeriten | visible_text_body_order | `{"kind": "style", "style_type": "kaeriten", "x-marker": "レ", "x-provenance": "parser"}` |
| 000051_3556 | `check-reports/aozora2html-adapter/000051_3556-dccca57b90c6.json` | `aat/aozora2html-adapter/000051_3556-dccca57b90c6.json` | `$.blocks[1].content[9].base_content[1]` | style | kunten.kaeriten | visible_text_body_order | `{"kind": "style", "style_type": "kaeriten", "x-marker": "二", "x-provenance": "parser"}` |
| 000051_43787 | `check-reports/aozora2html-adapter/000051_43787-d0420844828e.json` | `aat/aozora2html-adapter/000051_43787-d0420844828e.json` | `$.blocks[4].content[1].base_content[1]` | style | kunten.kaeriten | gaiji_resolution, visible_text_body_order | `{"kind": "style", "style_type": "kaeriten", "x-marker": "レ", "x-provenance": "parser"}` |
| 000051_4652 | `check-reports/aozora2html-adapter/000051_4652-94398990e2c6.json` | `aat/aozora2html-adapter/000051_4652-94398990e2c6.json` | `$.blocks[3].content[20].base_content[1]` | style | kunten.kaeriten |  | `{"kind": "style", "style_type": "kaeriten", "x-marker": "二", "x-provenance": "parser"}` |
| 000052_5036 | `check-reports/aozora2html-adapter/000052_5036-a5f78fddcbe9.json` | `aat/aozora2html-adapter/000052_5036-a5f78fddcbe9.json` | `$.blocks[52].content[23].base_content[1]` | style | kunten.kaeriten |  | `{"kind": "style", "style_type": "kaeriten", "x-marker": "二", "x-provenance": "parser"}` |

## kunten adapter_timeout_or_protocol_error

| Work ID | Report | AAT | Node Path | Kind | Syntax | Failures | Preview |
|---|---|---|---|---|---|---|---|
| 000006_1869 | `check-reports/aozora2html-adapter/000006_1869-62320f0f4474.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000051_1452 | `check-reports/aozora2html-adapter/000051_1452-b93481005b4d.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000051_4331 | `check-reports/aozora2html-adapter/000051_4331-6b7b2b7c90f8.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000096_2093 | `check-reports/aozora2html-adapter/000096_2093-71cae78a6bed.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000129_2084 | `check-reports/aozora2html-adapter/000129_2084-1731cdb61106.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000148_761 | `check-reports/aozora2html-adapter/000148_761-de1bc43c7072.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000150_52090 | `check-reports/aozora2html-adapter/000150_52090-f7a25d6982c7.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000255_1403 | `check-reports/aozora2html-adapter/000255_1403-7e59069231b9.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000320_43481 | `check-reports/aozora2html-adapter/000320_43481-b0ddcc6a02ed.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000933_47550 | `check-reports/aozora2html-adapter/000933_47550-70e0b2b8bbb0.json` | `` | `` |  |  | adapter_timeout | `{}` |

## kunten parse_incomplete

| Work ID | Report | AAT | Node Path | Kind | Syntax | Failures | Preview |
|---|---|---|---|---|---|---|---|
| 000083_1051 | `check-reports/aozora2html-adapter/000083_1051-1b4568a349ce.json` | `aat/aozora2html-adapter/000083_1051-1b4568a349ce.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000158_1507 | `check-reports/aozora2html-adapter/000158_1507-8f379d289fe5.json` | `aat/aozora2html-adapter/000158_1507-8f379d289fe5.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000250_18353 | `check-reports/aozora2html-adapter/000250_18353-c161863ee2e1.json` | `aat/aozora2html-adapter/000250_18353-c161863ee2e1.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000284_2227 | `check-reports/aozora2html-adapter/000284_2227-a860417d25bf.json` | `aat/aozora2html-adapter/000284_2227-a860417d25bf.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000290_48049 | `check-reports/aozora2html-adapter/000290_48049-f0ea74eac3a1.json` | `aat/aozora2html-adapter/000290_48049-f0ea74eac3a1.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000305_1897 | `check-reports/aozora2html-adapter/000305_1897-b3ce99e6fae1.json` | `aat/aozora2html-adapter/000305_1897-b3ce99e6fae1.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000372_2587 | `check-reports/aozora2html-adapter/000372_2587-16c25b9d30c3.json` | `aat/aozora2html-adapter/000372_2587-16c25b9d30c3.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000372_3412 | `check-reports/aozora2html-adapter/000372_3412-10e2c884b1ab.json` | `aat/aozora2html-adapter/000372_3412-10e2c884b1ab.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000933_47196 | `check-reports/aozora2html-adapter/000933_47196-806bb4bcf38e.json` | `aat/aozora2html-adapter/000933_47196-806bb4bcf38e.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 001331_48284 | `check-reports/aozora2html-adapter/001331_48284-335f21f7beb6.json` | `aat/aozora2html-adapter/001331_48284-335f21f7beb6.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |

## kunten report_failed_other_property

| Work ID | Report | AAT | Node Path | Kind | Syntax | Failures | Preview |
|---|---|---|---|---|---|---|---|
| 000026_50238 | `check-reports/aozora2html-adapter/000026_50238-fc986b6ae191.json` | `aat/aozora2html-adapter/000026_50238-fc986b6ae191.json` | `` |  |  | visible_text_body_order | `{}` |
| 000026_50242 | `check-reports/aozora2html-adapter/000026_50242-c86f569f98ca.json` | `aat/aozora2html-adapter/000026_50242-c86f569f98ca.json` | `` |  |  | visible_text_body_order | `{}` |
| 000026_50259 | `check-reports/aozora2html-adapter/000026_50259-ff92a4881b03.json` | `aat/aozora2html-adapter/000026_50259-ff92a4881b03.json` | `` |  |  | visible_text_body_order | `{}` |
| 000026_51893 | `check-reports/aozora2html-adapter/000026_51893-245c3d8f22cf.json` | `aat/aozora2html-adapter/000026_51893-245c3d8f22cf.json` | `` |  |  | visible_text_body_order | `{}` |
| 000026_55774 | `check-reports/aozora2html-adapter/000026_55774-8de37823936a.json` | `aat/aozora2html-adapter/000026_55774-8de37823936a.json` | `` |  |  | visible_text_body_order | `{}` |
| 000026_55777 | `check-reports/aozora2html-adapter/000026_55777-88d859555a42.json` | `aat/aozora2html-adapter/000026_55777-88d859555a42.json` | `` |  |  | visible_text_body_order | `{}` |
| 000026_55779 | `check-reports/aozora2html-adapter/000026_55779-ee5fd274ce3c.json` | `aat/aozora2html-adapter/000026_55779-ee5fd274ce3c.json` | `` |  |  | visible_text_body_order | `{}` |
| 000026_55781 | `check-reports/aozora2html-adapter/000026_55781-9cfc66ec4c7b.json` | `aat/aozora2html-adapter/000026_55781-9cfc66ec4c7b.json` | `` |  |  | visible_text_body_order | `{}` |
| 000026_55785 | `check-reports/aozora2html-adapter/000026_55785-f803987d315a.json` | `aat/aozora2html-adapter/000026_55785-f803987d315a.json` | `` |  |  | visible_text_body_order | `{}` |
| 000050_48399 | `check-reports/aozora2html-adapter/000050_48399-2e76dc785aef.json` | `aat/aozora2html-adapter/000050_48399-2e76dc785aef.json` | `` |  |  | visible_text_body_order | `{}` |

## Kunten source feature without AAT observation

| Work ID | Report | AAT | Node Path | Kind | Syntax | Failures | Preview |
|---|---|---|---|---|---|---|---|
| 000712_52955 | `check-reports/aozora2html-adapter/000712_52955-131c917f84d3.json` | `aat/aozora2html-adapter/000712_52955-131c917f84d3.json` | `` |  |  |  | `{}` |
| 000712_52957 | `check-reports/aozora2html-adapter/000712_52957-8ce110916880.json` | `aat/aozora2html-adapter/000712_52957-8ce110916880.json` | `` |  |  |  | `{}` |
| 001383_56944 | `check-reports/aozora2html-adapter/001383_56944-1b63de07717e.json` | `aat/aozora2html-adapter/001383_56944-1b63de07717e.json` | `` |  |  |  | `{}` |
