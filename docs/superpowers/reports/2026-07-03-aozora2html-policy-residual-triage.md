# Aozora2html Policy Residual Triage

- run_dir: `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z`
- retry_run_dir: `/db/ab-validator/aat-corpus/aozora2html-policy-retry-20260703T055526Z`
- audit_summary: `docs/superpowers/reports/2026-07-03-aozora2html-measurement-trust.summary.json`
- residual_union_count: 264

## Residual Bucket Counts

| Family | Bucket | Works | Next action |
|---|---|---:|---|
| warigaki | adapter_timeout_or_protocol_error | 7 | adapter timeout diagnosis |
| warigaki | schema_invalid_or_no_aat | 0 | schema or AAT persistence fix |
| warigaki | parse_incomplete | 5 | adapter parse-completeness fix |
| warigaki | report_failed_other_property | 17 | adapter-oracle characterization |
| warigaki | source_feature_without_aat_observation | 65 | source detector versus adapter observation triage |
| kunten | adapter_timeout_or_protocol_error | 12 | adapter timeout diagnosis |
| kunten | schema_invalid_or_no_aat | 0 | schema or AAT persistence fix |
| kunten | parse_incomplete | 10 | adapter parse-completeness fix |
| kunten | report_failed_other_property | 153 | adapter-oracle characterization |
| kunten | source_feature_without_aat_observation | 3 | source detector versus adapter observation triage |

## Residual Evidence Summary

| Family | Bucket | Works | Reports | AAT | Clean reports | Failed properties | Evidence runs | Evidence note |
|---|---|---:|---:|---:|---:|---|---|---|
| warigaki | adapter_timeout_or_protocol_error | 7 | 7 | 0 | 0 | adapter_protocol_error:1, adapter_timeout:6 | retry:7 | adapter runtime/protocol failure |
| warigaki | schema_invalid_or_no_aat | 0 | 0 | 0 | 0 |  |  | schema or AAT persistence failure |
| warigaki | parse_incomplete | 5 | 5 | 5 | 0 | gaiji_resolution:5, parse_completeness:5, ruby_completeness:5 | retry:5 | parse-completeness failure |
| warigaki | report_failed_other_property | 17 | 17 | 17 | 0 | gaiji_resolution:2, ruby_completeness:1, visible_text_body_order:16 | baseline:16, retry:1 | AAT present but non-policy property failures need oracle characterization |
| warigaki | source_feature_without_aat_observation | 65 | 65 | 65 | 65 |  | baseline:63, retry:2 | valid AAT without family observation; adapter/source-feature mapping candidate |
| kunten | adapter_timeout_or_protocol_error | 12 | 12 | 0 | 0 | adapter_timeout:12 | retry:12 | adapter runtime/protocol failure |
| kunten | schema_invalid_or_no_aat | 0 | 0 | 0 | 0 |  |  | schema or AAT persistence failure |
| kunten | parse_incomplete | 10 | 10 | 10 | 0 | gaiji_resolution:10, parse_completeness:10, ruby_completeness:10 | retry:10 | parse-completeness failure |
| kunten | report_failed_other_property | 153 | 153 | 153 | 0 | gaiji_resolution:5, ruby_completeness:1, visible_text_body_order:153 | baseline:151, retry:2 | AAT present but non-policy property failures need oracle characterization |
| kunten | source_feature_without_aat_observation | 3 | 3 | 3 | 3 |  | baseline:3 | valid AAT without family observation; adapter/source-feature mapping candidate |

## Samples

### warigaki adapter_timeout_or_protocol_error

Next action: adapter timeout diagnosis

| Work ID | Run | Source Lines | Syntax Keys | Failures | Report | AAT | Next action |
|---|---|---|---|---|---|---|---|
| 000125_1317 | retry | warigaki:53, warigaki:178, warigaki:225, warigaki:245, warigaki:306, warigaki:307, warigaki:310, warigaki:328 (+92 more) |  | adapter_protocol_error | `check-reports/aozora2html-adapter/000125_1317-75eca9fa8fae.json` | `` | adapter timeout diagnosis |
| 000279_1704 | retry | warigaki:2960 |  | adapter_timeout | `check-reports/aozora2html-adapter/000279_1704-e64286701ace.json` | `` | adapter timeout diagnosis |
| 000311_33187 | retry | warigaki:1543 |  | adapter_timeout | `check-reports/aozora2html-adapter/000311_33187-503307d11b89.json` | `` | adapter timeout diagnosis |
| 000961_4820 | retry | warigaki:1768 |  | adapter_timeout | `check-reports/aozora2html-adapter/000961_4820-1fa47562950a.json` | `` | adapter timeout diagnosis |
| 001111_42789 | retry | warigaki:711, warigaki:747, warigaki:1244, warigaki:1362, warigaki:1389, warigaki:1462, warigaki:2111, warigaki:3181 |  | adapter_timeout | `check-reports/aozora2html-adapter/001111_42789-1e2257edc5d5.json` | `` | adapter timeout diagnosis |
| 001411_59071 | retry | warigaki:1199 |  | adapter_timeout | `check-reports/aozora2html-adapter/001411_59071-23ad926fa543.json` | `` | adapter timeout diagnosis |
| 001518_51731 | retry | warigaki:80, warigaki:101, warigaki:130, warigaki:131, warigaki:132, warigaki:133, warigaki:134, warigaki:155 (+54 more) |  | adapter_timeout | `check-reports/aozora2html-adapter/001518_51731-53223fbef36e.json` | `` | adapter timeout diagnosis |

### warigaki parse_incomplete

Next action: adapter parse-completeness fix

| Work ID | Run | Source Lines | Syntax Keys | Failures | Report | AAT | Next action |
|---|---|---|---|---|---|---|---|
| 000125_45231 | retry | warigaki:46, warigaki:47, warigaki:76, warigaki:124, warigaki:130, warigaki:216, warigaki:275 |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000125_45231-ef96c3f5d895.json` | `aat/aozora2html-adapter/000125_45231-ef96c3f5d895.json` | adapter parse-completeness fix |
| 000284_2227 | retry | warigaki:115, warigaki:125, warigaki:137 |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000284_2227-a860417d25bf.json` | `aat/aozora2html-adapter/000284_2227-a860417d25bf.json` | adapter parse-completeness fix |
| 000284_2665 | retry | warigaki:34 |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000284_2665-0b38a88d5fa2.json` | `aat/aozora2html-adapter/000284_2665-0b38a88d5fa2.json` | adapter parse-completeness fix |
| 000603_4729 | retry | warigaki:16, warigaki:27, warigaki:34, warigaki:36 |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000603_4729-2a1e8bddff5c.json` | `aat/aozora2html-adapter/000603_4729-2a1e8bddff5c.json` | adapter parse-completeness fix |
| 000933_47196 | retry | warigaki:31, warigaki:32, warigaki:33, warigaki:46, warigaki:48, warigaki:49, warigaki:51, warigaki:54 (+3 more) |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000933_47196-806bb4bcf38e.json` | `aat/aozora2html-adapter/000933_47196-806bb4bcf38e.json` | adapter parse-completeness fix |

### warigaki report_failed_other_property

Next action: adapter-oracle characterization

| Work ID | Run | Source Lines | Syntax Keys | Failures | Report | AAT | Next action |
|---|---|---|---|---|---|---|---|
| 000077_1323 | retry | warigaki:39, warigaki:343, warigaki:507, warigaki:1178 | decoration.bousen, gaiji.marker, gaiji_ruby.inline_base, ruby.basic | visible_text_body_order | `check-reports/aozora2html-adapter/000077_1323-b51132c1dd72.json` | `aat/aozora2html-adapter/000077_1323-b51132c1dd72.json` | adapter-oracle characterization |
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

### warigaki source_feature_without_aat_observation

Next action: source detector versus adapter observation triage

| Work ID | Run | Source Lines | Syntax Keys | Failures | Report | AAT | Next action |
|---|---|---|---|---|---|---|---|
| 000006_1868 | baseline | warigaki:22 | gaiji.marker, ruby.basic |  | `check-reports/aozora2html-adapter/000006_1868-e598e91a19ff.json` | `aat/aozora2html-adapter/000006_1868-e598e91a19ff.json` | source detector versus adapter observation triage |
| 000006_1869 | retry | warigaki:293, warigaki:730 | gaiji.marker, gaiji_ruby.inline_base, kunten.kaeriten, ruby.basic |  | `check-reports/aozora2html-adapter/000006_1869-62320f0f4474.json` | `aat/aozora2html-adapter/000006_1869-62320f0f4474.json` | source detector versus adapter observation triage |
| 000075_4250 | baseline | warigaki:522, warigaki:562, warigaki:563, warigaki:566, warigaki:567 | gaiji.marker, ruby.basic |  | `check-reports/aozora2html-adapter/000075_4250-7e32f80f0150.json` | `aat/aozora2html-adapter/000075_4250-7e32f80f0150.json` | source detector versus adapter observation triage |
| 000081_47027 | retry | warigaki:3872 | decoration.font_size, gaiji.marker, gaiji_ruby.inline_base, ruby.basic |  | `check-reports/aozora2html-adapter/000081_47027-a6a7780bf470.json` | `aat/aozora2html-adapter/000081_47027-a6a7780bf470.json` | source detector versus adapter observation triage |
| 000106_57905 | baseline | warigaki:279 | gaiji.marker, gaiji_ruby.inline_base, ruby.basic |  | `check-reports/aozora2html-adapter/000106_57905-d4c02a20ee39.json` | `aat/aozora2html-adapter/000106_57905-d4c02a20ee39.json` | source detector versus adapter observation triage |
| 000121_45086 | baseline | warigaki:2269 | gaiji.marker, ruby.basic |  | `check-reports/aozora2html-adapter/000121_45086-1a5c986d414a.json` | `aat/aozora2html-adapter/000121_45086-1a5c986d414a.json` | source detector versus adapter observation triage |
| 000125_43656 | baseline | warigaki:216, warigaki:537, warigaki:733 | decoration.bold_italic, figure.image_inline, gaiji.marker, gaiji_ruby.inline_base, ruby.basic |  | `check-reports/aozora2html-adapter/000125_43656-72671814e76e.json` | `aat/aozora2html-adapter/000125_43656-72671814e76e.json` | source detector versus adapter observation triage |
| 000125_45230 | baseline | warigaki:35 | figure.image_inline, gaiji.marker, gaiji_ruby.inline_base, ruby.basic |  | `check-reports/aozora2html-adapter/000125_45230-80ae1c6a2cc4.json` | `aat/aozora2html-adapter/000125_45230-80ae1c6a2cc4.json` | source detector versus adapter observation triage |
| 000146_47356 | baseline | warigaki:326, warigaki:328 | gaiji.marker, ruby.basic |  | `check-reports/aozora2html-adapter/000146_47356-fca1bd12d2a6.json` | `aat/aozora2html-adapter/000146_47356-fca1bd12d2a6.json` | source detector versus adapter observation triage |
| 000146_48131 | baseline | warigaki:887 | decoration.bousen, gaiji.marker, ruby.basic |  | `check-reports/aozora2html-adapter/000146_48131-e0cc23d97452.json` | `aat/aozora2html-adapter/000146_48131-e0cc23d97452.json` | source detector versus adapter observation triage |
| 000284_2226 | baseline | warigaki:12, warigaki:13, warigaki:25, warigaki:27, warigaki:32 | gaiji.marker |  | `check-reports/aozora2html-adapter/000284_2226-b56fd371b06f.json` | `aat/aozora2html-adapter/000284_2226-b56fd371b06f.json` | source detector versus adapter observation triage |
| 000284_2942 | baseline | warigaki:20 | gaiji.marker, ruby.basic |  | `check-reports/aozora2html-adapter/000284_2942-2854527a09a0.json` | `aat/aozora2html-adapter/000284_2942-2854527a09a0.json` | source detector versus adapter observation triage |
| 000296_45664 | baseline | warigaki:31, warigaki:39, warigaki:51 | gaiji.marker, gaiji_ruby.inline_base, ruby.basic |  | `check-reports/aozora2html-adapter/000296_45664-2110a4530d48.json` | `aat/aozora2html-adapter/000296_45664-2110a4530d48.json` | source detector versus adapter observation triage |
| 000296_47149 | baseline | warigaki:21, warigaki:34 | ruby.basic |  | `check-reports/aozora2html-adapter/000296_47149-6902a2409f29.json` | `aat/aozora2html-adapter/000296_47149-6902a2409f29.json` | source detector versus adapter observation triage |
| 000346_49187 | baseline | warigaki:24 | ruby.basic |  | `check-reports/aozora2html-adapter/000346_49187-b369f6e9ceaf.json` | `aat/aozora2html-adapter/000346_49187-b369f6e9ceaf.json` | source detector versus adapter observation triage |

### kunten adapter_timeout_or_protocol_error

Next action: adapter timeout diagnosis

| Work ID | Run | Source Lines | Syntax Keys | Failures | Report | AAT | Next action |
|---|---|---|---|---|---|---|---|
| 000129_2084 | retry | kunten.okurigana:18, kunten.okurigana:428 |  | adapter_timeout | `check-reports/aozora2html-adapter/000129_2084-1731cdb61106.json` | `` | adapter timeout diagnosis |
| 000148_761 | retry | kunten.kaeriten:18, kunten.kaeriten:3813 |  | adapter_timeout | `check-reports/aozora2html-adapter/000148_761-de1bc43c7072.json` | `` | adapter timeout diagnosis |
| 001025_50909 | retry | kunten.kaeriten:21, kunten.kaeriten:2469, kunten.kaeriten:5200, kunten.kaeriten:5225, kunten.kaeriten:5239, kunten.kaeriten:5241, kunten.kaeriten:5243 |  | adapter_timeout | `check-reports/aozora2html-adapter/001025_50909-dabf470f1f18.json` | `` | adapter timeout diagnosis |
| 001266_46820 | retry | kunten.kaeriten:18, kunten.kaeriten:119, kunten.kaeriten:204, kunten.kaeriten:309, kunten.kaeriten:462, kunten.kaeriten:488, kunten.kaeriten:496, kunten.kaeriten:565 (+29 more) |  | adapter_timeout | `check-reports/aozora2html-adapter/001266_46820-80ffbd27db9f.json` | `` | adapter timeout diagnosis |
| 001562_52399 | retry | kunten.okurigana:19, kunten.okurigana:127 |  | adapter_timeout | `check-reports/aozora2html-adapter/001562_52399-697e4904cd6e.json` | `` | adapter timeout diagnosis |
| 001562_52410 | retry | kunten.kaeriten:19, kunten.kaeriten:275, kunten.kaeriten:276, kunten.kaeriten:388, kunten.kaeriten:389 |  | adapter_timeout | `check-reports/aozora2html-adapter/001562_52410-3b287943927c.json` | `` | adapter timeout diagnosis |
| 001562_52414 | retry | kunten.kaeriten:19, kunten.kaeriten:3899, kunten.kaeriten:3900 |  | adapter_timeout | `check-reports/aozora2html-adapter/001562_52414-1cff9ab1fbc0.json` | `` | adapter timeout diagnosis |
| 001562_52415 | retry | kunten.kaeriten:3636, kunten.kaeriten:3637 |  | adapter_timeout | `check-reports/aozora2html-adapter/001562_52415-8885c8158a3d.json` | `` | adapter timeout diagnosis |
| 001562_52417 | retry | kunten.kaeriten:385, kunten.kaeriten:386, kunten.kaeriten:387, kunten.kaeriten:390, kunten.kaeriten:391, kunten.kaeriten:3265, kunten.kaeriten:3268 |  | adapter_timeout | `check-reports/aozora2html-adapter/001562_52417-3594c8ea4be4.json` | `` | adapter timeout diagnosis |
| 001562_52418 | retry | kunten.kaeriten:1120 |  | adapter_timeout | `check-reports/aozora2html-adapter/001562_52418-be449c4fe82f.json` | `` | adapter timeout diagnosis |
| 001562_56757 | retry | kunten.kaeriten:19, kunten.kaeriten:3372, kunten.kaeriten:3373 |  | adapter_timeout | `check-reports/aozora2html-adapter/001562_56757-7b2454d69ff7.json` | `` | adapter timeout diagnosis |
| 001764_55990 | retry | kunten.kaeriten:21, kunten.kaeriten:121, kunten.kaeriten:391, kunten.kaeriten:1018 |  | adapter_timeout | `check-reports/aozora2html-adapter/001764_55990-79961e884b28.json` | `` | adapter timeout diagnosis |

### kunten parse_incomplete

Next action: adapter parse-completeness fix

| Work ID | Run | Source Lines | Syntax Keys | Failures | Report | AAT | Next action |
|---|---|---|---|---|---|---|---|
| 000083_1051 | retry | kunten.kaeriten:18, kunten.kaeriten:680 |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000083_1051-1b4568a349ce.json` | `aat/aozora2html-adapter/000083_1051-1b4568a349ce.json` | adapter parse-completeness fix |
| 000158_1507 | retry | kunten.kaeriten:19, kunten.kaeriten:294, kunten.kaeriten:296, kunten.kaeriten:297, kunten.kaeriten:298, kunten.kaeriten:299, kunten.kaeriten:300, kunten.kaeriten:301 (+4 more) |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000158_1507-8f379d289fe5.json` | `aat/aozora2html-adapter/000158_1507-8f379d289fe5.json` | adapter parse-completeness fix |
| 000250_18353 | retry | kunten.kaeriten:18, kunten.kaeriten:257, kunten.kaeriten:259, kunten.kaeriten:260, kunten.kaeriten:296, kunten.kaeriten:480, kunten.kaeriten:693, kunten.okurigana:18 (+7 more) |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000250_18353-c161863ee2e1.json` | `aat/aozora2html-adapter/000250_18353-c161863ee2e1.json` | adapter parse-completeness fix |
| 000284_2227 | retry | kunten.kaeriten:18, kunten.kaeriten:75 |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000284_2227-a860417d25bf.json` | `aat/aozora2html-adapter/000284_2227-a860417d25bf.json` | adapter parse-completeness fix |
| 000290_48049 | retry | kunten.kaeriten:19, kunten.kaeriten:160 |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000290_48049-f0ea74eac3a1.json` | `aat/aozora2html-adapter/000290_48049-f0ea74eac3a1.json` | adapter parse-completeness fix |
| 000305_1897 | retry | kunten.kaeriten:18, kunten.kaeriten:1217 |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000305_1897-b3ce99e6fae1.json` | `aat/aozora2html-adapter/000305_1897-b3ce99e6fae1.json` | adapter parse-completeness fix |
| 000372_2587 | retry | kunten.kaeriten:18, kunten.kaeriten:30, kunten.kaeriten:32, kunten.kaeriten:34, kunten.kaeriten:36, kunten.kaeriten:39, kunten.kaeriten:43, kunten.kaeriten:47 (+28 more) |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000372_2587-16c25b9d30c3.json` | `aat/aozora2html-adapter/000372_2587-16c25b9d30c3.json` | adapter parse-completeness fix |
| 000372_3412 | retry | kunten.kaeriten:18, kunten.kaeriten:29, kunten.kaeriten:41, kunten.kaeriten:42, kunten.kaeriten:47, kunten.kaeriten:49, kunten.kaeriten:57, kunten.kaeriten:60 (+11 more) |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000372_3412-10e2c884b1ab.json` | `aat/aozora2html-adapter/000372_3412-10e2c884b1ab.json` | adapter parse-completeness fix |
| 000933_47196 | retry | kunten.kaeriten:19, kunten.kaeriten:31, kunten.kaeriten:32, kunten.kaeriten:33, kunten.kaeriten:46, kunten.kaeriten:48, kunten.kaeriten:49, kunten.kaeriten:51 (+41 more) |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/000933_47196-806bb4bcf38e.json` | `aat/aozora2html-adapter/000933_47196-806bb4bcf38e.json` | adapter parse-completeness fix |
| 001331_48284 | retry | kunten.kaeriten:20, kunten.kaeriten:28, kunten.kaeriten:34, kunten.kaeriten:42, kunten.kaeriten:50, kunten.kaeriten:62, kunten.kaeriten:70, kunten.kaeriten:76 (+92 more) |  | gaiji_resolution, parse_completeness, ruby_completeness | `check-reports/aozora2html-adapter/001331_48284-335f21f7beb6.json` | `aat/aozora2html-adapter/001331_48284-335f21f7beb6.json` | adapter parse-completeness fix |

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
