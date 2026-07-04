# AAT Parser-IR Conversion Audit

- generated_unix_seconds: `1783172545`
- mapping: `https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe` `0.2.3`
- mapping_hash: `sha256:21791c841557ced968464b38e42971e22830edb0efdb0be6faf285761532f770`
- mapping_schema_hash: `sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4`
- target_parser_ir_schema_hash: `sha256:8e56871965e647e40ade08fd9dd580a3516d33905be17957cc79750bd42ea64d`

## Totals

| files_attempted | files_succeeded | files_failed | parser_ir_nodes | divergence_records | divergence_occurrences | elapsed_seconds |
|---:|---:|---:|---:|---:|---:|---:|
| 71301 | 67415 | 3886 | 28494053 | 829660 | 46816666 | 24.818 |

## Inputs

| label | files | aat_dir |
|---|---:|---|
| aozora-rs-adapter | 17894 | `/home/bor/Projects/ab-validator/scratch/morph-full-corpus/aats/aozora-rs-adapter` |
| aozora2-adapter | 17874 | `/db/ab-validator/aat-corpus/aozora2-full-20260704T132955Z/aat/aozora2-adapter` |
| aozora2html-adapter | 17689 | `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter` |
| aozora-epub3-adapter | 17844 | `/db/ab-validator/aat-corpus/aozora-epub3-full-20260704T050652Z-300s/aat/aozora-epub3-adapter` |

## Corpus Results

| corpus | attempted | succeeded | failed | parser_ir_nodes | divergence_occurrences |
|---|---:|---:|---:|---:|---:|
| aozora-epub3-adapter | 17844 | 17844 | 0 | 10670874 | 18314414 |
| aozora-rs-adapter | 17894 | 17894 | 0 | 7821839 | 11926731 |
| aozora2-adapter | 17874 | 13988 | 3886 | 1582155 | 4329397 |
| aozora2html-adapter | 17689 | 17689 | 0 | 8419185 | 12246124 |

## Divergence Categories

| category | occurrences |
|---|---:|
| AMBIGUITY | 29335276 |
| INVENTION | 11473725 |
| LOSS | 5622535 |
| STRUCTURAL | 352862 |
| UNSUPPORTED | 32268 |

## Compatibility Candidates

| adapter | adapter_version | mapping_version | mapping_hash | files_succeeded | files_failed | rules_emitted | rules_missing | unsupported_occurrences |
|---|---|---|---|---:|---:|---:|---:|---:|
| aozora-epub3 | aozora-epub3-adapter 0.1.0 AozoraEpub3-JDK21-1.3.4-jdk21 | 0.2.3 | `sha256:21791c841557ced968464b38e42971e22830edb0efdb0be6faf285761532f770` | 17844 | 0 | 61 | 66 | 13234 |
| aozora-rs | aozora-rs-adapter 0.1.0 2b4e8d1 | 0.2.3 | `sha256:21791c841557ced968464b38e42971e22830edb0efdb0be6faf285761532f770` | 17894 | 0 | 25 | 102 | 0 |
| aozora2 | aozora2-adapter 0.1.0 aozora-core-0.7.1 | 0.2.3 | `sha256:21791c841557ced968464b38e42971e22830edb0efdb0be6faf285761532f770` | 13988 | 3886 | 89 | 38 | 4804 |
| aozora2html | aozora2html-adapter 0.1.0 gem-3.0.1 | 0.2.3 | `sha256:21791c841557ced968464b38e42971e22830edb0efdb0be6faf285761532f770` | 17689 | 0 | 112 | 15 | 14230 |

## Rule Coverage

- rules_total: `127`
- rules_emitted: `127`
- rules_missing: `0`

| rule_id | category | files | occurrences |
|---|---|---:|---:|
| A-01 | AMBIGUITY | 165 | 2567 |
| A-02 | AMBIGUITY | 38 | 112 |
| A-03 | AMBIGUITY | 18 | 20 |
| A-04 | AMBIGUITY | 18 | 20 |
| A-05 | AMBIGUITY | 7 | 18 |
| A-06 | AMBIGUITY | 143 | 566 |
| A-07 | AMBIGUITY | 579 | 2232 |
| A-08 | AMBIGUITY | 13649 | 910469 |
| A-09 | AMBIGUITY | 5096 | 77976 |
| A-10 | AMBIGUITY | 2 | 2 |
| A-11 | AMBIGUITY | 85 | 551 |
| A-12 | AMBIGUITY | 11 | 20 |
| A-13 | AMBIGUITY | 6620 | 71404 |
| A-14 | AMBIGUITY | 6656 | 72191 |
| A-15 | AMBIGUITY | 30 | 85 |
| A-16 | AMBIGUITY | 137 | 563 |
| A-17 | AMBIGUITY | 1389 | 9403 |
| A-18 | AMBIGUITY | 1041 | 3955 |
| A-19 | AMBIGUITY | 10362 | 80528 |
| A-20 | AMBIGUITY | 67114 | 27221781 |
| A-21 | AMBIGUITY | 32044 | 485189 |
| A-22 | AMBIGUITY | 3 | 4 |
| A-23 | AMBIGUITY | 221 | 1430 |
| A-24 | AMBIGUITY | 4 | 17 |
| A-25 | AMBIGUITY | 4012 | 41715 |
| A-26 | AMBIGUITY | 18836 | 285038 |
| A-27 | AMBIGUITY | 5 | 5 |
| A-28 | AMBIGUITY | 67415 | 67415 |
| I-01 | INVENTION | 67415 | 67415 |
| I-02 | INVENTION | 50249 | 10347033 |
| I-03 | INVENTION | 67415 | 67415 |
| I-04 | INVENTION | 67415 | 67415 |
| I-05 | INVENTION | 16563 | 265128 |
| I-06 | INVENTION | 16563 | 265128 |
| I-07 | INVENTION | 67415 | 67415 |
| I-08 | INVENTION | 1 | 47 |
| I-09 | INVENTION | 31 | 561 |
| I-10 | INVENTION | 579 | 2232 |
| I-11 | INVENTION | 115 | 782 |
| I-12 | INVENTION | 567 | 3945 |
| I-13 | INVENTION | 10362 | 80528 |
| I-14 | INVENTION | 15647 | 238681 |
| L-01 | LOSS | 10571 | 82760 |
| L-02 | LOSS | 5 | 20 |
| L-03 | LOSS | 1 | 47 |
| L-04 | LOSS | 1 | 47 |
| L-05 | LOSS | 1 | 47 |
| L-06 | LOSS | 20 | 76 |
| L-07 | LOSS | 1 | 1 |
| L-08 | LOSS | 1 | 1 |
| L-09 | LOSS | 1 | 1 |
| L-10 | LOSS | 18 | 20 |
| L-11 | LOSS | 3 | 31 |
| L-12 | LOSS | 8 | 41 |
| L-13 | LOSS | 2 | 2 |
| L-14 | LOSS | 41 | 217 |
| L-15 | LOSS | 2 | 2 |
| L-16 | LOSS | 146 | 1617 |
| L-17 | LOSS | 458 | 4751 |
| L-18 | LOSS | 31 | 561 |
| L-19 | LOSS | 25 | 555 |
| L-20 | LOSS | 25 | 555 |
| L-21 | LOSS | 445 | 1762 |
| L-22 | LOSS | 3924 | 134076 |
| L-23 | LOSS | 6 | 9 |
| L-24 | LOSS | 11 | 20 |
| L-25 | LOSS | 1 | 1 |
| L-26 | LOSS | 22 | 117 |
| L-27 | LOSS | 22 | 117 |
| L-28 | LOSS | 518 | 5987 |
| L-29 | LOSS | 518 | 5987 |
| L-30 | LOSS | 47 | 103 |
| L-31 | LOSS | 58 | 481 |
| L-32 | LOSS | 6620 | 71404 |
| L-33 | LOSS | 78 | 1377 |
| L-34 | LOSS | 143 | 2707 |
| L-35 | LOSS | 72 | 275 |
| L-36 | LOSS | 437 | 20691 |
| L-37 | LOSS | 94 | 202 |
| L-38 | LOSS | 1176 | 4964 |
| L-39 | LOSS | 6202 | 1046749 |
| L-40 | LOSS | 135 | 682 |
| L-41 | LOSS | 567 | 3945 |
| L-42 | LOSS | 502 | 3785 |
| L-43 | LOSS | 502 | 3785 |
| L-44 | LOSS | 3639 | 19360 |
| L-45 | LOSS | 24504 | 4034597 |
| L-46 | LOSS | 4 | 17 |
| L-47 | LOSS | 5 | 32 |
| L-48 | LOSS | 5 | 32 |
| L-49 | LOSS | 213 | 3811 |
| L-50 | LOSS | 213 | 3811 |
| L-51 | LOSS | 86 | 432 |
| L-52 | LOSS | 17 | 131 |
| L-53 | LOSS | 4012 | 41715 |
| L-54 | LOSS | 17894 | 17894 |
| L-55 | LOSS | 67415 | 67415 |
| L-56 | LOSS | 32710 | 32710 |
| S-01 | STRUCTURAL | 18 | 20 |
| S-02 | STRUCTURAL | 6620 | 71404 |
| S-03 | STRUCTURAL | 115 | 782 |
| S-04 | STRUCTURAL | 76 | 148 |
| S-05 | STRUCTURAL | 4012 | 41715 |
| S-06 | STRUCTURAL | 15647 | 238681 |
| S-07 | STRUCTURAL | 49 | 90 |
| S-08 | STRUCTURAL | 15 | 22 |
| U-01 | UNSUPPORTED | 12 | 93 |
| U-02 | UNSUPPORTED | 3 | 9 |
| U-03 | UNSUPPORTED | 2 | 3 |
| U-04 | UNSUPPORTED | 1 | 1 |
| U-05 | UNSUPPORTED | 1 | 2 |
| U-06 | UNSUPPORTED | 9 | 56 |
| U-07 | UNSUPPORTED | 433 | 7744 |
| U-08 | UNSUPPORTED | 30 | 165 |
| U-09 | UNSUPPORTED | 175 | 2889 |
| U-10 | UNSUPPORTED | 85 | 547 |
| U-11 | UNSUPPORTED | 1 | 1 |
| U-12 | UNSUPPORTED | 76 | 148 |
| U-13 | UNSUPPORTED | 8 | 61 |
| U-14 | UNSUPPORTED | 10 | 28 |
| U-15 | UNSUPPORTED | 60 | 252 |
| U-16 | UNSUPPORTED | 1016 | 7711 |
| U-17 | UNSUPPORTED | 48 | 161 |
| U-18 | UNSUPPORTED | 638 | 10871 |
| U-19 | UNSUPPORTED | 221 | 1414 |
| U-20 | UNSUPPORTED | 49 | 90 |
| U-21 | UNSUPPORTED | 15 | 22 |

## Top Errors

| count | message | samples |
|---:|---|---|
| 2990 | unsupported inline kind: raw | aozora2-adapter:000006_1869-62320f0f4474.json<br>aozora2-adapter:000006_4627-ded0d23b15b9.json<br>aozora2-adapter:000008_1083-bf767c36e951.json<br>aozora2-adapter:000008_47374-9c929d4f4d67.json<br>aozora2-adapter:000008_47386-49ed3c33666b.json |
| 128 | unsupported inline kind in source attribution projection: accent | aozora2-adapter:000020_745-bce191ee0ece.json<br>aozora2-adapter:000026_50241-af65ef658680.json<br>aozora2-adapter:000026_55717-3ba0b0630fd5.json<br>aozora2-adapter:000042_1682-4401cf92d836.json<br>aozora2-adapter:000042_2453-83567d45101e.json |
| 94 | unsupported inline kind: accent | aozora2-adapter:000026_50245-e840465144a2.json<br>aozora2-adapter:000027_523-e22ef286b8e7.json<br>aozora2-adapter:000035_2277-b04f7f121e1a.json<br>aozora2-adapter:000075_4250-7e32f80f0150.json<br>aozora2-adapter:000076_45641-e69c2d20d152.json |
| 58 | unsupported inline kind in visible projection: accent | aozora2-adapter:000026_55732-e162f2f7263b.json<br>aozora2-adapter:000026_55739-075fc7322e7b.json<br>aozora2-adapter:000042_2469-3372cea4b938.json<br>aozora2-adapter:000042_42768-2a6afe487bf8.json<br>aozora2-adapter:000065_393-58fae23a2747.json |
| 13 | AAT schema validation failed at /blocks/2: {"content":[{"content":[{"kind":"_page_break"}],"kind":"style","style_type":"jisage_line","x-indent":3}],"kind":"paragraph"} is not valid under any of the schemas listed in the 'oneOf' keyword | aozora2-adapter:000026_894-a581b59ea241.json<br>aozora2-adapter:001154_52085-360b82b3a358.json<br>aozora2-adapter:001393_55880-9b509ae4d8b4.json<br>aozora2-adapter:001506_52459-848d36385487.json<br>aozora2-adapter:001558_52689-086e5fee8352.json |
| 10 | unmeasured divergence: category=STRUCTURAL aat_pointer=blocks[].children[].yokogumi_block parser_ir_pointer=null | aozora2-adapter:000051_4331-6b7b2b7c90f8.json<br>aozora2-adapter:000146_49621-4ef3deeff66d.json<br>aozora2-adapter:000169_2582-42f9d81b9f9d.json<br>aozora2-adapter:000183_45359-2e59fb5fc00d.json<br>aozora2-adapter:000183_45375-4a04a61ec324.json |
| 9 | AAT schema validation failed at /blocks/2: {"content":[{"content":[{"kind":"_page_break"}],"kind":"style","style_type":"jisage_line","x-indent":5}],"kind":"paragraph"} is not valid under any of the schemas listed in the 'oneOf' keyword | aozora2-adapter:000035_58054-a105276ee66f.json<br>aozora2-adapter:000050_4561-0de58b79c513.json<br>aozora2-adapter:000050_50110-652f7ebc7d8c.json<br>aozora2-adapter:000888_33205-f9e1e3877a72.json<br>aozora2-adapter:001154_46989-8739dbd7f687.json |
| 9 | unsupported block kind without measured v1 divergence rule: caption_block | aozora2-adapter:000058_57440-fd9d6f0c6c51.json<br>aozora2-adapter:000448_55729-f621cbdeea07.json<br>aozora2-adapter:001492_51195-3f9f5beb37de.json<br>aozora2-adapter:001548_52232-2a72fadd0e1a.json<br>aozora2-adapter:001569_61447-b0864851f689.json |
| 8 | AAT schema validation failed at /blocks/1: {"content":[{"content":[{"kind":"_page_break"}],"kind":"style","style_type":"jisage_line","x-indent":5}],"kind":"paragraph"} is not valid under any of the schemas listed in the 'oneOf' keyword | aozora2-adapter:000050_50102-1de4c2efe644.json<br>aozora2-adapter:000067_641-a1edf2acd386.json<br>aozora2-adapter:000922_47099-0bee13490cf7.json<br>aozora2-adapter:001021_50119-245447465457.json<br>aozora2-adapter:001154_52302-68551bde3692.json |
| 8 | AAT schema validation failed at /blocks/2: {"content":[{"content":[{"kind":"_page_break"}],"kind":"style","style_type":"jisage_line","x-indent":4}],"kind":"paragraph"} is not valid under any of the schemas listed in the 'oneOf' keyword | aozora2-adapter:000074_56504-7f54f210c27e.json<br>aozora2-adapter:000082_49527-a2aee4bea75e.json<br>aozora2-adapter:000082_49544-ff9c3e9f69a9.json<br>aozora2-adapter:001154_44302-97fc55b16b97.json<br>aozora2-adapter:001157_49845-0034c188a7b3.json |
| 8 | AAT schema validation failed at /blocks/2: {"content":[{"content":[{"kind":"_page_break"}],"kind":"style","style_type":"jisage_line","x-indent":7}],"kind":"paragraph"} is not valid under any of the schemas listed in the 'oneOf' keyword | aozora2-adapter:000050_48392-bf3de88905a8.json<br>aozora2-adapter:000050_48400-bc422b70ed02.json<br>aozora2-adapter:001154_44778-0a45df29c1f8.json<br>aozora2-adapter:001154_44781-0533e4f594e2.json<br>aozora2-adapter:001154_44783-bcea154679be.json |
| 8 | unmeasured divergence: category=INVENTION aat_pointer=blocks[].content[].warigaki.upper[].gaiji.description parser_ir_pointer=gaiji.raw_marker | aozora2-adapter:001127_43836-11b94a10d99f.json<br>aozora2-adapter:001127_50702-a825bbf9a742.json<br>aozora2-adapter:001127_50703-cf0b89035b2f.json<br>aozora2-adapter:001341_54185-da3fab9bb441.json<br>aozora2-adapter:001344_50580-beb3c54d871d.json |
| 6 | AAT schema validation failed at /blocks/2: {"content":[{"content":[{"kind":"_page_break"}],"kind":"style","style_type":"jisage_line","x-indent":2}],"kind":"paragraph"} is not valid under any of the schemas listed in the 'oneOf' keyword | aozora2-adapter:000106_52354-8507b23f79f0.json<br>aozora2-adapter:000106_52370-2f5f475180bd.json<br>aozora2-adapter:000106_53493-2641a2391907.json<br>aozora2-adapter:001097_49825-a45c90aa56eb.json<br>aozora2-adapter:001154_44774-b3bb879b388f.json |
| 6 | AAT schema validation failed at /blocks/3: {"content":[{"content":[{"kind":"_page_break"}],"kind":"style","style_type":"jisage_line","x-indent":2}],"kind":"paragraph"} is not valid under any of the schemas listed in the 'oneOf' keyword | aozora2-adapter:000067_1789-aae6d58f40b1.json<br>aozora2-adapter:000106_52353-1686b97a0729.json<br>aozora2-adapter:001726_56081-ef6c814ac72c.json<br>aozora2-adapter:001726_56083-53e23043ce07.json<br>aozora2-adapter:001726_56085-92cdb1e940fb.json |
| 6 | unmeasured divergence: category=UNSUPPORTED aat_pointer=blocks[].content[].content[].content[].content[].content[].warigaki parser_ir_pointer=(emphasis.text) | aozora2-adapter:000908_51428-a7c512660dad.json<br>aozora2-adapter:000908_51431-19532976e120.json<br>aozora2-adapter:000908_51854-29c5ef4dbc51.json<br>aozora2-adapter:000908_51929-aeed627205bb.json<br>aozora2-adapter:000908_51941-e63f547d8fcd.json |
| 6 | unmeasured divergence: category=UNSUPPORTED aat_pointer=blocks[].heading.content[].warigaki parser_ir_pointer=(emphasis.text) | aozora2-adapter:001524_51918-13454376355b.json<br>aozora2-adapter:001524_51919-e5443561ac8a.json<br>aozora2-adapter:001524_51920-84deac9a013f.json<br>aozora2-adapter:001524_51921-f8d3ac01c1ac.json<br>aozora2-adapter:001524_51922-f7eb17bb77fa.json |
| 5 | unsupported inline kind in source attribution projection: yokogumi | aozora2-adapter:000026_50239-f4c2d8bb9024.json<br>aozora2-adapter:000035_312-cb6505044b26.json<br>aozora2-adapter:000035_313-586a212e52ce.json<br>aozora2-adapter:000311_2029-716e98653b00.json<br>aozora2-adapter:001166_43826-0b22f1c3dae6.json |
| 4 | AAT schema validation failed at /blocks/2: {"content":[{"content":[{"kind":"_page_break"}],"kind":"style","style_type":"jisage_line","x-indent":1}],"kind":"paragraph"} is not valid under any of the schemas listed in the 'oneOf' keyword | aozora2-adapter:000072_408-22fa0af57a9f.json<br>aozora2-adapter:000885_51307-d71bdc6627be.json<br>aozora2-adapter:001529_546-84eef7ac6431.json<br>aozora2-adapter:001848_59608-b08264acd57e.json |
| 4 | AAT schema validation failed at /blocks/3: {"content":[{"content":[{"kind":"_page_break"}],"kind":"style","style_type":"jisage_line","x-indent":7}],"kind":"paragraph"} is not valid under any of the schemas listed in the 'oneOf' keyword | aozora2-adapter:001154_51828-a65dede5c37f.json<br>aozora2-adapter:001154_51835-9d7dca0d3ea8.json<br>aozora2-adapter:001154_52089-e25eaa61c9e2.json<br>aozora2-adapter:001154_52309-c1af2773bb57.json |
| 4 | unmeasured divergence: category=UNSUPPORTED aat_pointer=blocks[].heading.content[].content[].warigaki parser_ir_pointer=(emphasis.text) | aozora2-adapter:001469_50753-073c7b7fdf12.json<br>aozora2-adapter:001511_51407-45fdbda89a25.json<br>aozora2-adapter:001835_57257-98b8083a41b2.json<br>aozora2-adapter:001995_59048-ce70ab1d8c49.json |

## Failure Samples

| corpus | path | message |
|---|---|---|
| aozora2-adapter | `000006_1869-62320f0f4474.json` | unsupported inline kind: raw |
| aozora2-adapter | `000006_4627-ded0d23b15b9.json` | unsupported inline kind: raw |
| aozora2-adapter | `000008_1083-bf767c36e951.json` | unsupported inline kind: raw |
| aozora2-adapter | `000008_47374-9c929d4f4d67.json` | unsupported inline kind: raw |
| aozora2-adapter | `000008_47386-49ed3c33666b.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_226-7771d96568b3.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_42929-f0f7ff036246.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_43028-cc484d1dc264.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_43056-80425d7b6392.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_43497-4d15dc523f93.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_43498-84246c6d4982.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_43522-19039e8f33b5.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_43523-08e4293be7fb.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_43524-08e343a3961f.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_45340-06021e5a5cc7.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_50711-f70596fd94c9.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_50712-3bd98c23f17b.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_50713-8c9159c8c915.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_50714-588859399991.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_50715-0bd4eab2b77d.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_50716-4a0d93059ac8.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_50717-0eeb5853dc3a.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_50718-3bd33a2d1f82.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_54910-42cf7cd96173.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_54911-2cb75e442e4d.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_54912-7ebf88db448d.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_54913-d76c2d5a30f2.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_54914-e26a90bf5379.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_54915-01f287655c90.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_55881-f4df11665cda.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_55882-1cadf9cd2bfd.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_57322-eb1ac8e18a2d.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_61393-46f69ce5ba94.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_61394-7bfd13580c25.json` | unsupported inline kind: raw |
| aozora2-adapter | `000009_8-5db3c281b807.json` | unsupported inline kind: raw |
| aozora2-adapter | `000011_55301-915f23ca52bf.json` | unsupported inline kind: raw |
| aozora2-adapter | `000011_889-8a691eba9355.json` | unsupported inline kind: raw |
| aozora2-adapter | `000011_899-bc8b5148d0e0.json` | AAT schema validation failed at /blocks/1: {"content":[{"content":[{"kind":"text","value":"優しき歌　"},{"description":"ローマ数字1、1-13-21","jis_code":"1-13-21","kind":"gaiji","resolved":"Ⅰ","unresolved_reason":null},{"kind":"raw","source":"BlockEnd(Midashi)"},{"base":"風信子","base_content":[{"kind":"text","value":"風信子"}],"direction":"right","kind":"ruby","reading":"ヒヤシンス","reading_content":[{"kind":"text","value":"ヒヤシンス"}]},{"kind":"text","value":"叢書　第四篇\r\n\r\n"},{"kind":"_page_break"}],"kind":"style","style_type":"jisage_line","x-indent":1}],"kind":"paragraph"} is not valid under any of the schemas listed in the 'oneOf' keyword |
| aozora2-adapter | `000012_1092-1c3a5aac467e.json` | unsupported inline kind: raw |
| aozora2-adapter | `000012_24448-32312eb2c779.json` | unsupported inline kind: raw |
| aozora2-adapter | `000012_33200-32660f9eb979.json` | unsupported inline kind: raw |
| aozora2-adapter | `000012_48628-4fde87720d42.json` | unsupported inline kind: raw |
| aozora2-adapter | `000013_11-62419a345fca.json` | AAT schema validation failed at /blocks/1: {"content":[{"content":[{"kind":"text","value":"　私は、友が無くては、耐へられぬのです。しかし、私には、ありません。この貧しい詩を、これを、読んでくださる方の胸へ捧げます。そして、私を、あなたの友にしてください。\r\n"},{"kind":"_page_break"}],"kind":"style","style_type":"jisage_line","x-indent":2}],"kind":"paragraph"} is not valid under any of the schemas listed in the 'oneOf' keyword |
| aozora2-adapter | `000014_12-e0352951f32f.json` | unsupported inline kind: raw |
| aozora2-adapter | `000014_728-af8280acaa59.json` | unsupported inline kind: raw |
| aozora2-adapter | `000019_194-5219b1146dad.json` | unsupported inline kind: raw |
| aozora2-adapter | `000019_42379-2c7b3481e08a.json` | unsupported inline kind: raw |
| aozora2-adapter | `000019_42380-e06a90ced089.json` | unsupported inline kind: raw |
| aozora2-adapter | `000019_42381-e359d7f661b1.json` | unsupported inline kind: raw |
| aozora2-adapter | `000019_42382-c81f40a971a8.json` | unsupported inline kind: raw |
