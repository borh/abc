# Aozora2html Policy Residual Triage

- run_dir: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z`
- retry_run_dir: `/db/ab-validator/aat-corpus/aozora2html-final-four-gap-20260703T091523Z`
- audit_summary: `docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.summary.json`
- residual_union_count: 264

## Residual Bucket Counts

| Family | Bucket | Works | Next action |
|---|---|---:|---|
| warigaki | adapter_timeout_or_protocol_error | 32 | adapter timeout diagnosis |
| warigaki | schema_invalid_or_no_aat | 0 | schema or AAT persistence fix |
| warigaki | parse_incomplete | 5 | adapter parse-completeness fix |
| warigaki | report_failed_other_property | 16 | adapter-oracle characterization |
| warigaki | source_feature_without_aat_observation | 29 | source detector versus adapter observation triage |
| kunten | adapter_timeout_or_protocol_error | 36 | adapter timeout diagnosis |
| kunten | schema_invalid_or_no_aat | 0 | schema or AAT persistence fix |
| kunten | parse_incomplete | 10 | adapter parse-completeness fix |
| kunten | report_failed_other_property | 151 | adapter-oracle characterization |
| kunten | source_feature_without_aat_observation | 3 | source detector versus adapter observation triage |

## Residual Evidence Summary

| Family | Bucket | Works | Reports | AAT | Clean reports | Failed properties | Evidence runs | Evidence note |
|---|---|---:|---:|---:|---:|---|---|---|
| warigaki | adapter_timeout_or_protocol_error | 32 | 32 | 0 | 0 | adapter_protocol_error:1, adapter_timeout:31 | baseline:32 | adapter runtime/protocol failure |
| warigaki | schema_invalid_or_no_aat | 0 | 0 | 0 | 0 |  |  | schema or AAT persistence failure |
| warigaki | parse_incomplete | 5 | 5 | 5 | 0 | gaiji_resolution:5, parse_completeness:5, ruby_completeness:5 | baseline:5 | parse-completeness failure |
| warigaki | report_failed_other_property | 16 | 16 | 16 | 0 | gaiji_resolution:2, ruby_completeness:1, visible_text_body_order:15 | baseline:16 | AAT present but non-policy property failures need oracle characterization |
| warigaki | source_feature_without_aat_observation | 29 | 29 | 29 | 29 |  | baseline:27, retry:2 | valid AAT without family observation; adapter/source-feature mapping candidate |
| kunten | adapter_timeout_or_protocol_error | 36 | 36 | 0 | 0 | adapter_timeout:36 | baseline:36 | adapter runtime/protocol failure |
| kunten | schema_invalid_or_no_aat | 0 | 0 | 0 | 0 |  |  | schema or AAT persistence failure |
| kunten | parse_incomplete | 10 | 10 | 10 | 0 | gaiji_resolution:10, parse_completeness:10, ruby_completeness:10 | baseline:10 | parse-completeness failure |
| kunten | report_failed_other_property | 151 | 151 | 151 | 0 | gaiji_resolution:4, ruby_completeness:1, visible_text_body_order:151 | baseline:151 | AAT present but non-policy property failures need oracle characterization |
| kunten | source_feature_without_aat_observation | 3 | 3 | 3 | 3 |  | baseline:3 | valid AAT without family observation; adapter/source-feature mapping candidate |

## Samples

### warigaki adapter_timeout_or_protocol_error

Next action: adapter timeout diagnosis

| Work ID | Run | Source Lines | Syntax Keys | Failures | Report | AAT | Next action |
|---|---|---|---|---|---|---|---|
| 000077_1323 | baseline | warigaki:39, warigaki:343, warigaki:507, warigaki:1178 |  | adapter_timeout | `check-reports/aozora2html-adapter/000077_1323-b51132c1dd72.json` | `` | adapter timeout diagnosis |
| 000096_2093 | baseline | warigaki:1713 |  | adapter_timeout | `check-reports/aozora2html-adapter/000096_2093-71cae78a6bed.json` | `` | adapter timeout diagnosis |
| 000106_56910 | baseline | warigaki:3581 |  | adapter_timeout | `check-reports/aozora2html-adapter/000106_56910-e74ada484515.json` | `` | adapter timeout diagnosis |
| 000118_614 | baseline | warigaki:474, warigaki:836, warigaki:3317 |  | adapter_timeout | `check-reports/aozora2html-adapter/000118_614-ee8775977dc9.json` | `` | adapter timeout diagnosis |
| 000125_1317 | baseline | warigaki:53, warigaki:178, warigaki:225, warigaki:245, warigaki:306, warigaki:307, warigaki:310, warigaki:328 (+92 more) |  | adapter_protocol_error | `check-reports/aozora2html-adapter/000125_1317-75eca9fa8fae.json` | `` | adapter timeout diagnosis |
| 000179_943 | baseline | warigaki:102, warigaki:103, warigaki:104, warigaki:105, warigaki:106, warigaki:107, warigaki:108 |  | adapter_timeout | `check-reports/aozora2html-adapter/000179_943-0c3f07a107dd.json` | `` | adapter timeout diagnosis |
| 000207_24382 | baseline | warigaki:405 |  | adapter_timeout | `check-reports/aozora2html-adapter/000207_24382-96674b532103.json` | `` | adapter timeout diagnosis |
| 000207_42217 | baseline | warigaki:162, warigaki:294 |  | adapter_timeout | `check-reports/aozora2html-adapter/000207_42217-8e421e57608e.json` | `` | adapter timeout diagnosis |
| 000279_1704 | baseline | warigaki:2960 |  | adapter_timeout | `check-reports/aozora2html-adapter/000279_1704-e64286701ace.json` | `` | adapter timeout diagnosis |
| 000311_33187 | baseline | warigaki:1543 |  | adapter_timeout | `check-reports/aozora2html-adapter/000311_33187-503307d11b89.json` | `` | adapter timeout diagnosis |
| 000363_42286 | baseline | warigaki:98, warigaki:100, warigaki:128, warigaki:508, warigaki:878, warigaki:920, warigaki:922, warigaki:1326 (+13 more) |  | adapter_timeout | `check-reports/aozora2html-adapter/000363_42286-e130312992b9.json` | `` | adapter timeout diagnosis |
| 000363_46886 | baseline | warigaki:40, warigaki:54, warigaki:77, warigaki:385, warigaki:401, warigaki:402, warigaki:415, warigaki:460 (+11 more) |  | adapter_timeout | `check-reports/aozora2html-adapter/000363_46886-f213d3cd1283.json` | `` | adapter timeout diagnosis |
| 000933_47550 | baseline | warigaki:89, warigaki:427, warigaki:527, warigaki:1077 |  | adapter_timeout | `check-reports/aozora2html-adapter/000933_47550-70e0b2b8bbb0.json` | `` | adapter timeout diagnosis |
| 000961_4820 | baseline | warigaki:1768 |  | adapter_timeout | `check-reports/aozora2html-adapter/000961_4820-1fa47562950a.json` | `` | adapter timeout diagnosis |
| 000989_4489 | baseline | warigaki:74 |  | adapter_timeout | `check-reports/aozora2html-adapter/000989_4489-f6442c1e3b44.json` | `` | adapter timeout diagnosis |

### warigaki parse_incomplete

Next action: adapter parse-completeness fix

| Work ID | Run | Source Lines | Syntax Keys | Failures | Report | AAT | Next action |
|---|---|---|---|---|---|---|---|
| 000125_45231 | baseline | warigaki:46, warigaki:47, warigaki:76, warigaki:124, warigaki:130, warigaki:216, warigaki:275 |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000125_45231-ef96c3f5d895.json` | `aat/aozora2html-adapter/000125_45231-ef96c3f5d895.json` | adapter parse-completeness fix |
| 000284_2227 | baseline | warigaki:115, warigaki:125, warigaki:137 |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000284_2227-a860417d25bf.json` | `aat/aozora2html-adapter/000284_2227-a860417d25bf.json` | adapter parse-completeness fix |
| 000284_2665 | baseline | warigaki:34 |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000284_2665-0b38a88d5fa2.json` | `aat/aozora2html-adapter/000284_2665-0b38a88d5fa2.json` | adapter parse-completeness fix |
| 000603_4729 | baseline | warigaki:16, warigaki:27, warigaki:34, warigaki:36 |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000603_4729-2a1e8bddff5c.json` | `aat/aozora2html-adapter/000603_4729-2a1e8bddff5c.json` | adapter parse-completeness fix |
| 000933_47196 | baseline | warigaki:31, warigaki:32, warigaki:33, warigaki:46, warigaki:48, warigaki:49, warigaki:51, warigaki:54 (+3 more) |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000933_47196-806bb4bcf38e.json` | `aat/aozora2html-adapter/000933_47196-806bb4bcf38e.json` | adapter parse-completeness fix |

### warigaki report_failed_other_property

Next action: adapter-oracle characterization

| Work ID | Run | Source Lines | Syntax Keys | Failures | Report | AAT | Next action |
|---|---|---|---|---|---|---|---|
| 000082_49526 | baseline | warigaki:920 | gaiji.marker, gaiji_ruby.inline_base, ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/000082_49526-0ce076982149.json` | `aat/aozora2html-adapter/000082_49526-0ce076982149.json` | adapter-oracle characterization |
| 000146_48313 | baseline | warigaki:408 | decoration.bousen, gaiji.marker, ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/000146_48313-c0fb76f5a906.json` | `aat/aozora2html-adapter/000146_48313-c0fb76f5a906.json` | adapter-oracle characterization |
| 000157_43445 | baseline | warigaki:49, warigaki:52, warigaki:78 | gaiji.marker, gaiji_ruby.inline_base, ruby.basic | gaiji_resolution, ruby_completeness | `check-reports/aozora2html-adapter/000157_43445-78d601d2bde4.json` | `aat/aozora2html-adapter/000157_43445-78d601d2bde4.json` | adapter-oracle characterization |
| 000933_46949 | baseline | warigaki:33, warigaki:37, warigaki:41, warigaki:45, warigaki:49, warigaki:53, warigaki:57, warigaki:61 | ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/000933_46949-a8e448a10fbb.json` | `aat/aozora2html-adapter/000933_46949-a8e448a10fbb.json` | adapter-oracle characterization |
| 000933_47180 | baseline | warigaki:123, warigaki:137, warigaki:153, warigaki:154, warigaki:334 | decoration.bousen, gaiji.marker, ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/000933_47180-a22148699c06.json` | `aat/aozora2html-adapter/000933_47180-a22148699c06.json` | adapter-oracle characterization |
| 000933_47182 | baseline | warigaki:31 | ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/000933_47182-2ecdb3807268.json` | `aat/aozora2html-adapter/000933_47182-2ecdb3807268.json` | adapter-oracle characterization |
| 000933_47186 | baseline | warigaki:40 | gaiji.marker, kunten.kaeriten, ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/000933_47186-9e15a76bb944.json` | `aat/aozora2html-adapter/000933_47186-9e15a76bb944.json` | adapter-oracle characterization |
| 001059_5075 | baseline | warigaki:24, warigaki:26, warigaki:27, warigaki:28, warigaki:29, warigaki:30, warigaki:37, warigaki:38 (+1 more) | kunten.kaeriten, ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/001059_5075-31637c1f95b9.json` | `aat/aozora2html-adapter/001059_5075-31637c1f95b9.json` | adapter-oracle characterization |
| 001123_42930 | baseline | warigaki:25, warigaki:141 | ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/001123_42930-2b0719322fb7.json` | `aat/aozora2html-adapter/001123_42930-2b0719322fb7.json` | adapter-oracle characterization |
| 001127_45250 | baseline | warigaki:39 | gaiji.marker, kunten.kaeriten, ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/001127_45250-a17afcbe1bda.json` | `aat/aozora2html-adapter/001127_45250-a17afcbe1bda.json` | adapter-oracle characterization |
| 001224_46113 | baseline | warigaki:150 | gaiji.marker, ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/001224_46113-fb944708ccc4.json` | `aat/aozora2html-adapter/001224_46113-fb944708ccc4.json` | adapter-oracle characterization |
| 001310_51839 | baseline | warigaki:4423 | decoration.bold_italic, decoration.font_size, gaiji.marker, gaiji_ruby.inline_base, ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/001310_51839-464119227edb.json` | `aat/aozora2html-adapter/001310_51839-464119227edb.json` | adapter-oracle characterization |
| 001320_47885 | baseline | warigaki:29, warigaki:49, warigaki:81, warigaki:103, warigaki:104, warigaki:106, warigaki:108, warigaki:118 | gaiji.marker, gaiji_ruby.inline_base, kunten.kaeriten, ruby.basic | gaiji_resolution, visible_text_body_order | `check-reports/aozora2html-adapter/001320_47885-c544442fa927.json` | `aat/aozora2html-adapter/001320_47885-c544442fa927.json` | adapter-oracle characterization |
| 001320_48130 | baseline | warigaki:48 | kunten.kaeriten, ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/001320_48130-b71901a7d822.json` | `aat/aozora2html-adapter/001320_48130-b71901a7d822.json` | adapter-oracle characterization |
| 001320_48226 | baseline | warigaki:41 | gaiji.marker, kunten.kaeriten, ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/001320_48226-3622fdfe2ab3.json` | `aat/aozora2html-adapter/001320_48226-3622fdfe2ab3.json` | adapter-oracle characterization |

### warigaki source_feature_without_aat_observation

Next action: source detector versus adapter observation triage

| Work ID | Run | Source Lines | Syntax Keys | Failures | Report | AAT | Next action |
|---|---|---|---|---|---|---|---|
| 000106_57905 | baseline | warigaki:279 | gaiji.marker, gaiji_ruby.inline_base, ruby.basic |  | `check-reports/aozora2html-adapter/000106_57905-d4c02a20ee39.json` | `aat/aozora2html-adapter/000106_57905-d4c02a20ee39.json` | source detector versus adapter observation triage |
| 000121_45086 | baseline | warigaki:2269 | gaiji.marker, ruby.basic |  | `check-reports/aozora2html-adapter/000121_45086-1a5c986d414a.json` | `aat/aozora2html-adapter/000121_45086-1a5c986d414a.json` | source detector versus adapter observation triage |
| 000416_47410 | baseline | warigaki:374 | ruby.basic |  | `check-reports/aozora2html-adapter/000416_47410-657b7e22f1a5.json` | `aat/aozora2html-adapter/000416_47410-657b7e22f1a5.json` | source detector versus adapter observation triage |
| 000754_48376 | baseline | warigaki:122 | gaiji.marker, gaiji_ruby.inline_base, ruby.basic |  | `check-reports/aozora2html-adapter/000754_48376-324688ac5877.json` | `aat/aozora2html-adapter/000754_48376-324688ac5877.json` | source detector versus adapter observation triage |
| 000933_47549 | baseline | warigaki:68 | gaiji.marker, kunten.kaeriten, ruby.basic |  | `check-reports/aozora2html-adapter/000933_47549-bd7f8686f785.json` | `aat/aozora2html-adapter/000933_47549-bd7f8686f785.json` | source detector versus adapter observation triage |
| 001095_43203 | baseline | warigaki:9, warigaki:23 |  |  | `check-reports/aozora2html-adapter/001095_43203-9deeeee6819e.json` | `aat/aozora2html-adapter/001095_43203-9deeeee6819e.json` | source detector versus adapter observation triage |
| 001095_43204 | baseline | warigaki:12, warigaki:380, warigaki:381 | ruby.basic |  | `check-reports/aozora2html-adapter/001095_43204-f900f5a3a78d.json` | `aat/aozora2html-adapter/001095_43204-f900f5a3a78d.json` | source detector versus adapter observation triage |
| 001095_43205 | baseline | warigaki:12, warigaki:305, warigaki:306 | ruby.basic |  | `check-reports/aozora2html-adapter/001095_43205-ce1539fea3a6.json` | `aat/aozora2html-adapter/001095_43205-ce1539fea3a6.json` | source detector versus adapter observation triage |
| 001095_43206 | baseline | warigaki:224, warigaki:225 | ruby.basic |  | `check-reports/aozora2html-adapter/001095_43206-b09f54b68b1d.json` | `aat/aozora2html-adapter/001095_43206-b09f54b68b1d.json` | source detector versus adapter observation triage |
| 001095_43207 | baseline | warigaki:15, warigaki:295, warigaki:296 | ruby.basic |  | `check-reports/aozora2html-adapter/001095_43207-e1ff696f32a9.json` | `aat/aozora2html-adapter/001095_43207-e1ff696f32a9.json` | source detector versus adapter observation triage |
| 001095_43208 | baseline | warigaki:15, warigaki:275, warigaki:276 | ruby.basic |  | `check-reports/aozora2html-adapter/001095_43208-573223c98b63.json` | `aat/aozora2html-adapter/001095_43208-573223c98b63.json` | source detector versus adapter observation triage |
| 001095_43209 | baseline | warigaki:15, warigaki:305, warigaki:306 | ruby.basic |  | `check-reports/aozora2html-adapter/001095_43209-d54cc59d4ee9.json` | `aat/aozora2html-adapter/001095_43209-d54cc59d4ee9.json` | source detector versus adapter observation triage |
| 001095_43210 | baseline | warigaki:15, warigaki:442, warigaki:443 | ruby.basic |  | `check-reports/aozora2html-adapter/001095_43210-433f96fc10f2.json` | `aat/aozora2html-adapter/001095_43210-433f96fc10f2.json` | source detector versus adapter observation triage |
| 001095_43211 | baseline | warigaki:12, warigaki:248, warigaki:249 | ruby.basic |  | `check-reports/aozora2html-adapter/001095_43211-5d30b342e8d6.json` | `aat/aozora2html-adapter/001095_43211-5d30b342e8d6.json` | source detector versus adapter observation triage |
| 001095_43212 | baseline | warigaki:15, warigaki:334, warigaki:335 | ruby.basic |  | `check-reports/aozora2html-adapter/001095_43212-0a94f7b02220.json` | `aat/aozora2html-adapter/001095_43212-0a94f7b02220.json` | source detector versus adapter observation triage |

### kunten adapter_timeout_or_protocol_error

Next action: adapter timeout diagnosis

| Work ID | Run | Source Lines | Syntax Keys | Failures | Report | AAT | Next action |
|---|---|---|---|---|---|---|---|
| 000051_1452 | baseline | kunten.kaeriten:18, kunten.kaeriten:234, kunten.kaeriten:480, kunten.kaeriten:481, kunten.kaeriten:482, kunten.kaeriten:483, kunten.kaeriten:484 |  | adapter_timeout | `check-reports/aozora2html-adapter/000051_1452-b93481005b4d.json` | `` | adapter timeout diagnosis |
| 000051_4331 | baseline | kunten.kaeriten:18, kunten.kaeriten:199, kunten.okurigana:18, kunten.okurigana:21, kunten.okurigana:199 |  | adapter_timeout | `check-reports/aozora2html-adapter/000051_4331-6b7b2b7c90f8.json` | `` | adapter timeout diagnosis |
| 000096_2093 | baseline | kunten.kaeriten:18, kunten.kaeriten:2554 |  | adapter_timeout | `check-reports/aozora2html-adapter/000096_2093-71cae78a6bed.json` | `` | adapter timeout diagnosis |
| 000129_2084 | baseline | kunten.okurigana:18, kunten.okurigana:428 |  | adapter_timeout | `check-reports/aozora2html-adapter/000129_2084-1731cdb61106.json` | `` | adapter timeout diagnosis |
| 000148_761 | baseline | kunten.kaeriten:18, kunten.kaeriten:3813 |  | adapter_timeout | `check-reports/aozora2html-adapter/000148_761-de1bc43c7072.json` | `` | adapter timeout diagnosis |
| 000150_52090 | baseline | kunten.kaeriten:39, kunten.kaeriten:40, kunten.kaeriten:42 |  | adapter_timeout | `check-reports/aozora2html-adapter/000150_52090-f7a25d6982c7.json` | `` | adapter timeout diagnosis |
| 000255_1403 | baseline | kunten.kaeriten:18, kunten.kaeriten:116, kunten.kaeriten:337, kunten.kaeriten:363, kunten.kaeriten:365, kunten.kaeriten:367, kunten.kaeriten:412, kunten.kaeriten:3350 (+1 more) |  | adapter_timeout | `check-reports/aozora2html-adapter/000255_1403-7e59069231b9.json` | `` | adapter timeout diagnosis |
| 000320_43481 | baseline | kunten.kaeriten:19, kunten.kaeriten:109, kunten.kaeriten:195, kunten.kaeriten:342 |  | adapter_timeout | `check-reports/aozora2html-adapter/000320_43481-b0ddcc6a02ed.json` | `` | adapter timeout diagnosis |
| 000933_47550 | baseline | kunten.kaeriten:18, kunten.kaeriten:36, kunten.kaeriten:96, kunten.kaeriten:114, kunten.kaeriten:138, kunten.kaeriten:208, kunten.kaeriten:304, kunten.kaeriten:314 (+11 more) |  | adapter_timeout | `check-reports/aozora2html-adapter/000933_47550-70e0b2b8bbb0.json` | `` | adapter timeout diagnosis |
| 001025_50909 | baseline | kunten.kaeriten:21, kunten.kaeriten:2469, kunten.kaeriten:5200, kunten.kaeriten:5225, kunten.kaeriten:5239, kunten.kaeriten:5241, kunten.kaeriten:5243 |  | adapter_timeout | `check-reports/aozora2html-adapter/001025_50909-dabf470f1f18.json` | `` | adapter timeout diagnosis |
| 001059_5082 | baseline | kunten.kaeriten:18, kunten.kaeriten:102, kunten.kaeriten:112, kunten.kaeriten:114, kunten.kaeriten:140, kunten.kaeriten:148, kunten.kaeriten:153, kunten.kaeriten:162 (+59 more) |  | adapter_timeout | `check-reports/aozora2html-adapter/001059_5082-9d9eb0fc29bc.json` | `` | adapter timeout diagnosis |
| 001191_55276 | baseline | kunten.kaeriten:18, kunten.kaeriten:142, kunten.kaeriten:154, kunten.kaeriten:155, kunten.kaeriten:186, kunten.kaeriten:190, kunten.kaeriten:266, kunten.kaeriten:278 (+148 more) |  | adapter_timeout | `check-reports/aozora2html-adapter/001191_55276-9cb2a5001381.json` | `` | adapter timeout diagnosis |
| 001224_46076 | baseline | kunten.okurigana:18, kunten.okurigana:3386, kunten.okurigana:3564, kunten.okurigana:3657 |  | adapter_timeout | `check-reports/aozora2html-adapter/001224_46076-386f883cb70a.json` | `` | adapter timeout diagnosis |
| 001266_46820 | baseline | kunten.kaeriten:18, kunten.kaeriten:119, kunten.kaeriten:204, kunten.kaeriten:309, kunten.kaeriten:462, kunten.kaeriten:488, kunten.kaeriten:496, kunten.kaeriten:565 (+29 more) |  | adapter_timeout | `check-reports/aozora2html-adapter/001266_46820-80ffbd27db9f.json` | `` | adapter timeout diagnosis |
| 001266_51368 | baseline | kunten.kaeriten:18, kunten.kaeriten:442, kunten.kaeriten:531, kunten.kaeriten:890, kunten.kaeriten:950, kunten.kaeriten:953, kunten.kaeriten:965, kunten.kaeriten:1133 (+11 more) |  | adapter_timeout | `check-reports/aozora2html-adapter/001266_51368-3f5151853d59.json` | `` | adapter timeout diagnosis |

### kunten parse_incomplete

Next action: adapter parse-completeness fix

| Work ID | Run | Source Lines | Syntax Keys | Failures | Report | AAT | Next action |
|---|---|---|---|---|---|---|---|
| 000083_1051 | baseline | kunten.kaeriten:18, kunten.kaeriten:680 |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000083_1051-1b4568a349ce.json` | `aat/aozora2html-adapter/000083_1051-1b4568a349ce.json` | adapter parse-completeness fix |
| 000158_1507 | baseline | kunten.kaeriten:19, kunten.kaeriten:294, kunten.kaeriten:296, kunten.kaeriten:297, kunten.kaeriten:298, kunten.kaeriten:299, kunten.kaeriten:300, kunten.kaeriten:301 (+4 more) |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000158_1507-8f379d289fe5.json` | `aat/aozora2html-adapter/000158_1507-8f379d289fe5.json` | adapter parse-completeness fix |
| 000250_18353 | baseline | kunten.kaeriten:18, kunten.kaeriten:257, kunten.kaeriten:259, kunten.kaeriten:260, kunten.kaeriten:296, kunten.kaeriten:480, kunten.kaeriten:693, kunten.okurigana:18 (+7 more) |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000250_18353-c161863ee2e1.json` | `aat/aozora2html-adapter/000250_18353-c161863ee2e1.json` | adapter parse-completeness fix |
| 000284_2227 | baseline | kunten.kaeriten:18, kunten.kaeriten:75 |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000284_2227-a860417d25bf.json` | `aat/aozora2html-adapter/000284_2227-a860417d25bf.json` | adapter parse-completeness fix |
| 000290_48049 | baseline | kunten.kaeriten:19, kunten.kaeriten:160 |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000290_48049-f0ea74eac3a1.json` | `aat/aozora2html-adapter/000290_48049-f0ea74eac3a1.json` | adapter parse-completeness fix |
| 000305_1897 | baseline | kunten.kaeriten:18, kunten.kaeriten:1217 |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000305_1897-b3ce99e6fae1.json` | `aat/aozora2html-adapter/000305_1897-b3ce99e6fae1.json` | adapter parse-completeness fix |
| 000372_2587 | baseline | kunten.kaeriten:18, kunten.kaeriten:30, kunten.kaeriten:32, kunten.kaeriten:34, kunten.kaeriten:36, kunten.kaeriten:39, kunten.kaeriten:43, kunten.kaeriten:47 (+28 more) |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000372_2587-16c25b9d30c3.json` | `aat/aozora2html-adapter/000372_2587-16c25b9d30c3.json` | adapter parse-completeness fix |
| 000372_3412 | baseline | kunten.kaeriten:18, kunten.kaeriten:29, kunten.kaeriten:41, kunten.kaeriten:42, kunten.kaeriten:47, kunten.kaeriten:49, kunten.kaeriten:57, kunten.kaeriten:60 (+11 more) |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000372_3412-10e2c884b1ab.json` | `aat/aozora2html-adapter/000372_3412-10e2c884b1ab.json` | adapter parse-completeness fix |
| 000933_47196 | baseline | kunten.kaeriten:19, kunten.kaeriten:31, kunten.kaeriten:32, kunten.kaeriten:33, kunten.kaeriten:46, kunten.kaeriten:48, kunten.kaeriten:49, kunten.kaeriten:51 (+41 more) |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000933_47196-806bb4bcf38e.json` | `aat/aozora2html-adapter/000933_47196-806bb4bcf38e.json` | adapter parse-completeness fix |
| 001331_48284 | baseline | kunten.kaeriten:20, kunten.kaeriten:28, kunten.kaeriten:34, kunten.kaeriten:42, kunten.kaeriten:50, kunten.kaeriten:62, kunten.kaeriten:70, kunten.kaeriten:76 (+92 more) |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/001331_48284-335f21f7beb6.json` | `aat/aozora2html-adapter/001331_48284-335f21f7beb6.json` | adapter parse-completeness fix |

### kunten report_failed_other_property

Next action: adapter-oracle characterization

| Work ID | Run | Source Lines | Syntax Keys | Failures | Report | AAT | Next action |
|---|---|---|---|---|---|---|---|
| 000026_50238 | baseline | kunten.okurigana:14, kunten.okurigana:47, kunten.okurigana:51, kunten.okurigana:52, kunten.okurigana:53, kunten.okurigana:54, kunten.okurigana:57, kunten.okurigana:59 (+3 more) | ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/000026_50238-fc986b6ae191.json` | `aat/aozora2html-adapter/000026_50238-fc986b6ae191.json` | adapter-oracle characterization |
| 000026_50242 | baseline | kunten.okurigana:10, kunten.okurigana:16 |  | visible_text_body_order | `check-reports/aozora2html-adapter/000026_50242-c86f569f98ca.json` | `aat/aozora2html-adapter/000026_50242-c86f569f98ca.json` | adapter-oracle characterization |
| 000026_50259 | baseline | kunten.okurigana:14, kunten.okurigana:21, kunten.okurigana:22, kunten.okurigana:23 | ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/000026_50259-ff92a4881b03.json` | `aat/aozora2html-adapter/000026_50259-ff92a4881b03.json` | adapter-oracle characterization |
| 000026_51893 | baseline | kunten.okurigana:15, kunten.okurigana:27 | ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/000026_51893-245c3d8f22cf.json` | `aat/aozora2html-adapter/000026_51893-245c3d8f22cf.json` | adapter-oracle characterization |
| 000026_55774 | baseline | kunten.okurigana:14, kunten.okurigana:18, kunten.okurigana:22 | ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/000026_55774-8de37823936a.json` | `aat/aozora2html-adapter/000026_55774-8de37823936a.json` | adapter-oracle characterization |
| 000026_55777 | baseline | kunten.okurigana:14, kunten.okurigana:23 | ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/000026_55777-88d859555a42.json` | `aat/aozora2html-adapter/000026_55777-88d859555a42.json` | adapter-oracle characterization |
| 000026_55779 | baseline | kunten.okurigana:17, kunten.okurigana:46 | ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/000026_55779-ee5fd274ce3c.json` | `aat/aozora2html-adapter/000026_55779-ee5fd274ce3c.json` | adapter-oracle characterization |
| 000026_55781 | baseline | kunten.okurigana:15, kunten.okurigana:27 | decoration.font_size, ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/000026_55781-9cfc66ec4c7b.json` | `aat/aozora2html-adapter/000026_55781-9cfc66ec4c7b.json` | adapter-oracle characterization |
| 000026_55785 | baseline | kunten.okurigana:14, kunten.okurigana:22, kunten.okurigana:24 | ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/000026_55785-f803987d315a.json` | `aat/aozora2html-adapter/000026_55785-f803987d315a.json` | adapter-oracle characterization |
| 000050_48399 | baseline | kunten.okurigana:333 | gaiji.marker, gaiji_ruby.inline_base, ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/000050_48399-2e76dc785aef.json` | `aat/aozora2html-adapter/000050_48399-2e76dc785aef.json` | adapter-oracle characterization |
| 000050_57486 | baseline | kunten.okurigana:18, kunten.okurigana:94 | gaiji.marker, gaiji_ruby.inline_base, ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/000050_57486-361b9127071d.json` | `aat/aozora2html-adapter/000050_57486-361b9127071d.json` | adapter-oracle characterization |
| 000051_1434 | baseline | kunten.okurigana:18, kunten.okurigana:24, kunten.okurigana:52, kunten.okurigana:98 | gaiji.marker, gaiji_ruby.inline_base, ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/000051_1434-356e8ea352c6.json` | `aat/aozora2html-adapter/000051_1434-356e8ea352c6.json` | adapter-oracle characterization |
| 000051_1438 | baseline | kunten.okurigana:18, kunten.okurigana:23, kunten.okurigana:28, kunten.okurigana:29, kunten.okurigana:31 | gaiji.marker, gaiji_ruby.inline_base, ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/000051_1438-1ec6c48de990.json` | `aat/aozora2html-adapter/000051_1438-1ec6c48de990.json` | adapter-oracle characterization |
| 000051_1439 | baseline | kunten.okurigana:18, kunten.okurigana:46, kunten.okurigana:67 | gaiji.marker, gaiji_ruby.inline_base, ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/000051_1439-d1b0a3c521d6.json` | `aat/aozora2html-adapter/000051_1439-d1b0a3c521d6.json` | adapter-oracle characterization |
| 000051_1440 | baseline | kunten.okurigana:15, kunten.okurigana:21 | gaiji.marker, ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/000051_1440-d6066a06cca1.json` | `aat/aozora2html-adapter/000051_1440-d6066a06cca1.json` | adapter-oracle characterization |

### kunten source_feature_without_aat_observation

Next action: source detector versus adapter observation triage

| Work ID | Run | Source Lines | Syntax Keys | Failures | Report | AAT | Next action |
|---|---|---|---|---|---|---|---|
| 000712_52955 | baseline | kunten.kaeriten:20 | gaiji.marker |  | `check-reports/aozora2html-adapter/000712_52955-131c917f84d3.json` | `aat/aozora2html-adapter/000712_52955-131c917f84d3.json` | source detector versus adapter observation triage |
| 000712_52957 | baseline | kunten.kaeriten:15, kunten.kaeriten:26 | decoration.font_size, gaiji.marker, ruby.basic |  | `check-reports/aozora2html-adapter/000712_52957-8ce110916880.json` | `aat/aozora2html-adapter/000712_52957-8ce110916880.json` | source detector versus adapter observation triage |
| 001383_56944 | baseline | kunten.kaeriten:17, kunten.kaeriten:38, kunten.okurigana:17, kunten.okurigana:20, kunten.okurigana:38 | ruby.basic |  | `check-reports/aozora2html-adapter/001383_56944-1b63de07717e.json` | `aat/aozora2html-adapter/001383_56944-1b63de07717e.json` | source detector versus adapter observation triage |
