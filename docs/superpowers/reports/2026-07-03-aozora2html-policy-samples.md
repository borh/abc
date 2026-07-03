# Aozora2html Policy Samples

- run_dir: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z`
- retry_run_dir: `/db/ab-validator/aat-corpus/aozora2html-source-feature-gap-20260703T082427Z`
- audit: `docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.md`
- limit_per_bucket: 10

## Observed warigaki

| Work ID | Run | Report | AAT | Node Path | Kind | Syntax | Failures | Preview |
|---|---|---|---|---|---|---|---|---|
| 000005_53194 | baseline | `check-reports/aozora2html-adapter/000005_53194-ebb0cbaf64b3.json` | `aat/aozora2html-adapter/000005_53194-ebb0cbaf64b3.json` | `$.blocks[249].content[1]` | warigaki |  |  | `{"kind": "warigaki"}` |
| 000006_1868 | retry | `check-reports/aozora2html-adapter/000006_1868-e598e91a19ff.json` | `aat/aozora2html-adapter/000006_1868-e598e91a19ff.json` | `$.blocks[1].content[9]` | warigaki |  |  | `{"kind": "warigaki", "x-provenance": "source-derived"}` |
| 000006_1869 | retry | `check-reports/aozora2html-adapter/000006_1869-62320f0f4474.json` | `aat/aozora2html-adapter/000006_1869-62320f0f4474.json` | `$.blocks[249].content[1]` | warigaki |  |  | `{"kind": "warigaki", "x-provenance": "source-derived"}` |
| 000034_519 | baseline | `check-reports/aozora2html-adapter/000034_519-059f2347de78.json` | `aat/aozora2html-adapter/000034_519-0975863ec5cb.json` | `$.blocks[13].children[0].content[0]` | warigaki |  |  | `{"kind": "warigaki"}` |
| 000038_42202 | baseline | `check-reports/aozora2html-adapter/000038_42202-afcc2eabc8f9.json` | `aat/aozora2html-adapter/000038_42202-afcc2eabc8f9.json` | `$.blocks[13].children[2].content[5]` | warigaki |  | visible_text_body_order | `{"kind": "warigaki"}` |
| 000057_43276 | baseline | `check-reports/aozora2html-adapter/000057_43276-14d0be58d734.json` | `aat/aozora2html-adapter/000057_43276-14d0be58d734.json` | `$.blocks[24].content[5]` | warigaki |  |  | `{"kind": "warigaki"}` |
| 000075_4250 | retry | `check-reports/aozora2html-adapter/000075_4250-7e32f80f0150.json` | `aat/aozora2html-adapter/000075_4250-7e32f80f0150.json` | `$.blocks[8].content[1]` | warigaki |  |  | `{"kind": "warigaki", "x-provenance": "source-derived"}` |
| 000081_47027 | retry | `check-reports/aozora2html-adapter/000081_47027-a6a7780bf470.json` | `aat/aozora2html-adapter/000081_47027-a6a7780bf470.json` | `$.blocks[3570].content[1]` | warigaki |  |  | `{"kind": "warigaki", "x-provenance": "source-derived"}` |
| 000082_56329 | baseline | `check-reports/aozora2html-adapter/000082_56329-7ff54d59e532.json` | `aat/aozora2html-adapter/000082_56329-7ff54d59e532.json` | `$.blocks[0].content[13]` | warigaki |  |  | `{"kind": "warigaki"}` |
| 000083_46289 | baseline | `check-reports/aozora2html-adapter/000083_46289-16b111c5dae5.json` | `aat/aozora2html-adapter/000083_46289-16b111c5dae5.json` | `$.blocks[50].content[3]` | warigaki |  | visible_text_body_order | `{"kind": "warigaki"}` |

## warigaki adapter_timeout_or_protocol_error

| Work ID | Run | Report | AAT | Node Path | Kind | Syntax | Failures | Preview |
|---|---|---|---|---|---|---|---|---|
| 000077_1323 | baseline | `check-reports/aozora2html-adapter/000077_1323-b51132c1dd72.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000096_2093 | baseline | `check-reports/aozora2html-adapter/000096_2093-71cae78a6bed.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000106_56910 | baseline | `check-reports/aozora2html-adapter/000106_56910-e74ada484515.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000118_614 | baseline | `check-reports/aozora2html-adapter/000118_614-ee8775977dc9.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000125_1317 | baseline | `check-reports/aozora2html-adapter/000125_1317-75eca9fa8fae.json` | `` | `` |  |  | adapter_protocol_error | `{}` |
| 000179_943 | baseline | `check-reports/aozora2html-adapter/000179_943-0c3f07a107dd.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000207_24382 | baseline | `check-reports/aozora2html-adapter/000207_24382-96674b532103.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000207_42217 | baseline | `check-reports/aozora2html-adapter/000207_42217-8e421e57608e.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000279_1704 | baseline | `check-reports/aozora2html-adapter/000279_1704-e64286701ace.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000311_33187 | baseline | `check-reports/aozora2html-adapter/000311_33187-503307d11b89.json` | `` | `` |  |  | adapter_timeout | `{}` |

## warigaki parse_incomplete

| Work ID | Run | Report | AAT | Node Path | Kind | Syntax | Failures | Preview |
|---|---|---|---|---|---|---|---|---|
| 000125_45231 | baseline | `check-reports/aozora2html-adapter/000125_45231-ef96c3f5d895.json` | `aat/aozora2html-adapter/000125_45231-ef96c3f5d895.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000284_2227 | baseline | `check-reports/aozora2html-adapter/000284_2227-a860417d25bf.json` | `aat/aozora2html-adapter/000284_2227-a860417d25bf.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000284_2665 | baseline | `check-reports/aozora2html-adapter/000284_2665-0b38a88d5fa2.json` | `aat/aozora2html-adapter/000284_2665-0b38a88d5fa2.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000603_4729 | baseline | `check-reports/aozora2html-adapter/000603_4729-2a1e8bddff5c.json` | `aat/aozora2html-adapter/000603_4729-2a1e8bddff5c.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000933_47196 | baseline | `check-reports/aozora2html-adapter/000933_47196-806bb4bcf38e.json` | `aat/aozora2html-adapter/000933_47196-806bb4bcf38e.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |

## warigaki report_failed_other_property

| Work ID | Run | Report | AAT | Node Path | Kind | Syntax | Failures | Preview |
|---|---|---|---|---|---|---|---|---|
| 000082_49526 | baseline | `check-reports/aozora2html-adapter/000082_49526-0ce076982149.json` | `aat/aozora2html-adapter/000082_49526-0ce076982149.json` | `` |  |  | visible_text_body_order | `{}` |
| 000146_48313 | baseline | `check-reports/aozora2html-adapter/000146_48313-c0fb76f5a906.json` | `aat/aozora2html-adapter/000146_48313-c0fb76f5a906.json` | `` |  |  | visible_text_body_order | `{}` |
| 000157_43445 | baseline | `check-reports/aozora2html-adapter/000157_43445-78d601d2bde4.json` | `aat/aozora2html-adapter/000157_43445-78d601d2bde4.json` | `` |  |  | gaiji_resolution, ruby_completeness | `{}` |
| 000933_46949 | baseline | `check-reports/aozora2html-adapter/000933_46949-a8e448a10fbb.json` | `aat/aozora2html-adapter/000933_46949-a8e448a10fbb.json` | `` |  |  | visible_text_body_order | `{}` |
| 000933_47180 | baseline | `check-reports/aozora2html-adapter/000933_47180-a22148699c06.json` | `aat/aozora2html-adapter/000933_47180-a22148699c06.json` | `` |  |  | visible_text_body_order | `{}` |
| 000933_47182 | baseline | `check-reports/aozora2html-adapter/000933_47182-2ecdb3807268.json` | `aat/aozora2html-adapter/000933_47182-2ecdb3807268.json` | `` |  |  | visible_text_body_order | `{}` |
| 000933_47186 | baseline | `check-reports/aozora2html-adapter/000933_47186-9e15a76bb944.json` | `aat/aozora2html-adapter/000933_47186-9e15a76bb944.json` | `` |  |  | visible_text_body_order | `{}` |
| 001059_5075 | baseline | `check-reports/aozora2html-adapter/001059_5075-31637c1f95b9.json` | `aat/aozora2html-adapter/001059_5075-31637c1f95b9.json` | `` |  |  | visible_text_body_order | `{}` |
| 001123_42930 | baseline | `check-reports/aozora2html-adapter/001123_42930-2b0719322fb7.json` | `aat/aozora2html-adapter/001123_42930-2b0719322fb7.json` | `` |  |  | visible_text_body_order | `{}` |
| 001127_45250 | baseline | `check-reports/aozora2html-adapter/001127_45250-a17afcbe1bda.json` | `aat/aozora2html-adapter/001127_45250-a17afcbe1bda.json` | `` |  |  | visible_text_body_order | `{}` |

## Warigaki source feature without AAT observation

| Work ID | Run | Report | AAT | Node Path | Kind | Syntax | Failures | Preview |
|---|---|---|---|---|---|---|---|---|
| 000106_57905 | baseline | `check-reports/aozora2html-adapter/000106_57905-d4c02a20ee39.json` | `aat/aozora2html-adapter/000106_57905-d4c02a20ee39.json` | `` |  |  |  | `{}` |
| 000121_45086 | baseline | `check-reports/aozora2html-adapter/000121_45086-1a5c986d414a.json` | `aat/aozora2html-adapter/000121_45086-1a5c986d414a.json` | `` |  |  |  | `{}` |
| 000296_47149 | retry | `check-reports/aozora2html-adapter/000296_47149-6902a2409f29.json` | `aat/aozora2html-adapter/000296_47149-6902a2409f29.json` | `` |  |  |  | `{}` |
| 000416_47410 | baseline | `check-reports/aozora2html-adapter/000416_47410-657b7e22f1a5.json` | `aat/aozora2html-adapter/000416_47410-657b7e22f1a5.json` | `` |  |  |  | `{}` |
| 000754_48376 | baseline | `check-reports/aozora2html-adapter/000754_48376-324688ac5877.json` | `aat/aozora2html-adapter/000754_48376-324688ac5877.json` | `` |  |  |  | `{}` |
| 000933_47549 | baseline | `check-reports/aozora2html-adapter/000933_47549-bd7f8686f785.json` | `aat/aozora2html-adapter/000933_47549-bd7f8686f785.json` | `` |  |  |  | `{}` |
| 001095_43203 | baseline | `check-reports/aozora2html-adapter/001095_43203-9deeeee6819e.json` | `aat/aozora2html-adapter/001095_43203-9deeeee6819e.json` | `` |  |  |  | `{}` |
| 001095_43204 | baseline | `check-reports/aozora2html-adapter/001095_43204-f900f5a3a78d.json` | `aat/aozora2html-adapter/001095_43204-f900f5a3a78d.json` | `` |  |  |  | `{}` |
| 001095_43205 | baseline | `check-reports/aozora2html-adapter/001095_43205-ce1539fea3a6.json` | `aat/aozora2html-adapter/001095_43205-ce1539fea3a6.json` | `` |  |  |  | `{}` |
| 001095_43206 | baseline | `check-reports/aozora2html-adapter/001095_43206-b09f54b68b1d.json` | `aat/aozora2html-adapter/001095_43206-b09f54b68b1d.json` | `` |  |  |  | `{}` |

## Observed kunten

| Work ID | Run | Report | AAT | Node Path | Kind | Syntax | Failures | Preview |
|---|---|---|---|---|---|---|---|---|
| 000006_1869 | retry | `check-reports/aozora2html-adapter/000006_1869-62320f0f4474.json` | `aat/aozora2html-adapter/000006_1869-62320f0f4474.json` | `$.blocks[866].content[11].base_content[1]` | style | kunten.kaeriten |  | `{"kind": "style", "style_type": "kaeriten", "x-marker": "レ", "x-provenance": "parser"}` |
| 000038_1408 | baseline | `check-reports/aozora2html-adapter/000038_1408-77f192c358bc.json` | `aat/aozora2html-adapter/000038_1408-77f192c358bc.json` | `$.blocks[3].content[68]` | style | kunten.kaeriten |  | `{"kind": "style", "style_type": "kaeriten", "x-marker": "レ", "x-provenance": "parser"}` |
| 000042_1694 | baseline | `check-reports/aozora2html-adapter/000042_1694-eb8c88c4846e.json` | `aat/aozora2html-adapter/000042_1694-eb8c88c4846e.json` | `$.blocks[8].content[1].base_content[1]` | style | kunten.kaeriten |  | `{"kind": "style", "style_type": "kaeriten", "x-marker": "レ", "x-provenance": "parser"}` |
| 000050_3581 | baseline | `check-reports/aozora2html-adapter/000050_3581-405fd70ec41b.json` | `aat/aozora2html-adapter/000050_3581-405fd70ec41b.json` | `$.blocks[43].content[1].base_content[1]` | style | kunten.kaeriten |  | `{"kind": "style", "style_type": "kaeriten", "x-marker": "レ", "x-provenance": "parser"}` |
| 000051_1436 | baseline | `check-reports/aozora2html-adapter/000051_1436-8276953442b7.json` | `aat/aozora2html-adapter/000051_1436-8276953442b7.json` | `$.blocks[4].content[3].base_content[1]` | style | kunten.kaeriten | visible_text_body_order | `{"kind": "style", "style_type": "kaeriten", "x-marker": "レ", "x-provenance": "parser"}` |
| 000051_2709 | baseline | `check-reports/aozora2html-adapter/000051_2709-bef45ad83cdd.json` | `aat/aozora2html-adapter/000051_2709-bef45ad83cdd.json` | `$.blocks[33].content[49]` | style | kunten.kaeriten | visible_text_body_order | `{"kind": "style", "style_type": "kaeriten", "x-marker": "レ", "x-provenance": "parser"}` |
| 000051_3199 | baseline | `check-reports/aozora2html-adapter/000051_3199-d0530bb2dec9.json` | `aat/aozora2html-adapter/000051_3199-d0530bb2dec9.json` | `$.blocks[22].content[17]` | style | kunten.kaeriten | visible_text_body_order | `{"kind": "style", "style_type": "kaeriten", "x-marker": "レ", "x-provenance": "parser"}` |
| 000051_3556 | baseline | `check-reports/aozora2html-adapter/000051_3556-dccca57b90c6.json` | `aat/aozora2html-adapter/000051_3556-dccca57b90c6.json` | `$.blocks[1].content[9].base_content[1]` | style | kunten.kaeriten | visible_text_body_order | `{"kind": "style", "style_type": "kaeriten", "x-marker": "二", "x-provenance": "parser"}` |
| 000051_43787 | baseline | `check-reports/aozora2html-adapter/000051_43787-d0420844828e.json` | `aat/aozora2html-adapter/000051_43787-d0420844828e.json` | `$.blocks[4].content[1].base_content[1]` | style | kunten.kaeriten | gaiji_resolution, visible_text_body_order | `{"kind": "style", "style_type": "kaeriten", "x-marker": "レ", "x-provenance": "parser"}` |
| 000051_4652 | baseline | `check-reports/aozora2html-adapter/000051_4652-94398990e2c6.json` | `aat/aozora2html-adapter/000051_4652-94398990e2c6.json` | `$.blocks[3].content[20].base_content[1]` | style | kunten.kaeriten |  | `{"kind": "style", "style_type": "kaeriten", "x-marker": "二", "x-provenance": "parser"}` |

## kunten adapter_timeout_or_protocol_error

| Work ID | Run | Report | AAT | Node Path | Kind | Syntax | Failures | Preview |
|---|---|---|---|---|---|---|---|---|
| 000051_1452 | baseline | `check-reports/aozora2html-adapter/000051_1452-b93481005b4d.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000051_4331 | baseline | `check-reports/aozora2html-adapter/000051_4331-6b7b2b7c90f8.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000096_2093 | baseline | `check-reports/aozora2html-adapter/000096_2093-71cae78a6bed.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000129_2084 | baseline | `check-reports/aozora2html-adapter/000129_2084-1731cdb61106.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000148_761 | baseline | `check-reports/aozora2html-adapter/000148_761-de1bc43c7072.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000150_52090 | baseline | `check-reports/aozora2html-adapter/000150_52090-f7a25d6982c7.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000255_1403 | baseline | `check-reports/aozora2html-adapter/000255_1403-7e59069231b9.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000320_43481 | baseline | `check-reports/aozora2html-adapter/000320_43481-b0ddcc6a02ed.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 000933_47550 | baseline | `check-reports/aozora2html-adapter/000933_47550-70e0b2b8bbb0.json` | `` | `` |  |  | adapter_timeout | `{}` |
| 001025_50909 | baseline | `check-reports/aozora2html-adapter/001025_50909-dabf470f1f18.json` | `` | `` |  |  | adapter_timeout | `{}` |

## kunten parse_incomplete

| Work ID | Run | Report | AAT | Node Path | Kind | Syntax | Failures | Preview |
|---|---|---|---|---|---|---|---|---|
| 000083_1051 | baseline | `check-reports/aozora2html-adapter/000083_1051-1b4568a349ce.json` | `aat/aozora2html-adapter/000083_1051-1b4568a349ce.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000158_1507 | baseline | `check-reports/aozora2html-adapter/000158_1507-8f379d289fe5.json` | `aat/aozora2html-adapter/000158_1507-8f379d289fe5.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000250_18353 | baseline | `check-reports/aozora2html-adapter/000250_18353-c161863ee2e1.json` | `aat/aozora2html-adapter/000250_18353-c161863ee2e1.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000284_2227 | baseline | `check-reports/aozora2html-adapter/000284_2227-a860417d25bf.json` | `aat/aozora2html-adapter/000284_2227-a860417d25bf.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000290_48049 | baseline | `check-reports/aozora2html-adapter/000290_48049-f0ea74eac3a1.json` | `aat/aozora2html-adapter/000290_48049-f0ea74eac3a1.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000305_1897 | baseline | `check-reports/aozora2html-adapter/000305_1897-b3ce99e6fae1.json` | `aat/aozora2html-adapter/000305_1897-b3ce99e6fae1.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000372_2587 | baseline | `check-reports/aozora2html-adapter/000372_2587-16c25b9d30c3.json` | `aat/aozora2html-adapter/000372_2587-16c25b9d30c3.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000372_3412 | baseline | `check-reports/aozora2html-adapter/000372_3412-10e2c884b1ab.json` | `aat/aozora2html-adapter/000372_3412-10e2c884b1ab.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 000933_47196 | baseline | `check-reports/aozora2html-adapter/000933_47196-806bb4bcf38e.json` | `aat/aozora2html-adapter/000933_47196-806bb4bcf38e.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |
| 001331_48284 | baseline | `check-reports/aozora2html-adapter/001331_48284-335f21f7beb6.json` | `aat/aozora2html-adapter/001331_48284-335f21f7beb6.json` | `` |  |  | gaiji_resolution, parse_completeness, ruby_completeness | `{}` |

## kunten report_failed_other_property

| Work ID | Run | Report | AAT | Node Path | Kind | Syntax | Failures | Preview |
|---|---|---|---|---|---|---|---|---|
| 000026_50238 | baseline | `check-reports/aozora2html-adapter/000026_50238-fc986b6ae191.json` | `aat/aozora2html-adapter/000026_50238-fc986b6ae191.json` | `` |  |  | visible_text_body_order | `{}` |
| 000026_50242 | baseline | `check-reports/aozora2html-adapter/000026_50242-c86f569f98ca.json` | `aat/aozora2html-adapter/000026_50242-c86f569f98ca.json` | `` |  |  | visible_text_body_order | `{}` |
| 000026_50259 | baseline | `check-reports/aozora2html-adapter/000026_50259-ff92a4881b03.json` | `aat/aozora2html-adapter/000026_50259-ff92a4881b03.json` | `` |  |  | visible_text_body_order | `{}` |
| 000026_51893 | baseline | `check-reports/aozora2html-adapter/000026_51893-245c3d8f22cf.json` | `aat/aozora2html-adapter/000026_51893-245c3d8f22cf.json` | `` |  |  | visible_text_body_order | `{}` |
| 000026_55774 | baseline | `check-reports/aozora2html-adapter/000026_55774-8de37823936a.json` | `aat/aozora2html-adapter/000026_55774-8de37823936a.json` | `` |  |  | visible_text_body_order | `{}` |
| 000026_55777 | baseline | `check-reports/aozora2html-adapter/000026_55777-88d859555a42.json` | `aat/aozora2html-adapter/000026_55777-88d859555a42.json` | `` |  |  | visible_text_body_order | `{}` |
| 000026_55779 | baseline | `check-reports/aozora2html-adapter/000026_55779-ee5fd274ce3c.json` | `aat/aozora2html-adapter/000026_55779-ee5fd274ce3c.json` | `` |  |  | visible_text_body_order | `{}` |
| 000026_55781 | baseline | `check-reports/aozora2html-adapter/000026_55781-9cfc66ec4c7b.json` | `aat/aozora2html-adapter/000026_55781-9cfc66ec4c7b.json` | `` |  |  | visible_text_body_order | `{}` |
| 000026_55785 | baseline | `check-reports/aozora2html-adapter/000026_55785-f803987d315a.json` | `aat/aozora2html-adapter/000026_55785-f803987d315a.json` | `` |  |  | visible_text_body_order | `{}` |
| 000050_48399 | baseline | `check-reports/aozora2html-adapter/000050_48399-2e76dc785aef.json` | `aat/aozora2html-adapter/000050_48399-2e76dc785aef.json` | `` |  |  | visible_text_body_order | `{}` |

## Kunten source feature without AAT observation

| Work ID | Run | Report | AAT | Node Path | Kind | Syntax | Failures | Preview |
|---|---|---|---|---|---|---|---|---|
| 000712_52955 | baseline | `check-reports/aozora2html-adapter/000712_52955-131c917f84d3.json` | `aat/aozora2html-adapter/000712_52955-131c917f84d3.json` | `` |  |  |  | `{}` |
| 000712_52957 | baseline | `check-reports/aozora2html-adapter/000712_52957-8ce110916880.json` | `aat/aozora2html-adapter/000712_52957-8ce110916880.json` | `` |  |  |  | `{}` |
| 001383_56944 | baseline | `check-reports/aozora2html-adapter/001383_56944-1b63de07717e.json` | `aat/aozora2html-adapter/001383_56944-1b63de07717e.json` | `` |  |  |  | `{}` |
