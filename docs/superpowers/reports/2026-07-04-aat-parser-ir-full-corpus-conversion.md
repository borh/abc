# Full-Corpus AAT Parser-IR Conversion Audit

- generated_unix_seconds: `1783122082`
- mapping: `https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe` `0.1.1`
- mapping_schema_hash: `sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4`
- target_parser_ir_schema_hash: `sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396`

## Totals

| files_attempted | files_succeeded | files_failed | parser_ir_nodes | divergence_records | divergence_occurrences | elapsed_seconds |
|---:|---:|---:|---:|---:|---:|---:|
| 35583 | 35123 | 460 | 15672849 | 599338 | 29225729 | 11.715 |

## Inputs

| label | files | aat_dir |
|---|---:|---|
| aozora-rs-adapter | 17894 | `scratch/morph-full-corpus/aats/aozora-rs-adapter` |
| aozora2html-adapter | 17689 | `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter` |

## Corpus Results

| corpus | attempted | succeeded | failed | parser_ir_nodes | divergence_occurrences |
|---|---:|---:|---:|---:|---:|
| aozora-rs-adapter | 17894 | 17894 | 0 | 7828615 | 13246894 |
| aozora2html-adapter | 17689 | 17229 | 460 | 7844234 | 15978835 |

## Divergence Categories

| category | occurrences |
|---|---:|
| AMBIGUITY | 19049053 |
| INVENTION | 6820640 |
| LOSS | 319530 |
| STRUCTURAL | 3025276 |
| UNSUPPORTED | 11230 |

## Rule Coverage

- rules_total: `118`
- rules_emitted: `98`
- rules_missing: `20`

| rule_id | category | files | occurrences |
|---|---|---:|---:|
| A-01 | AMBIGUITY | 58 | 293 |
| A-02 | AMBIGUITY | 18 | 57 |
| A-03 | AMBIGUITY | 4 | 4 |
| A-04 | AMBIGUITY | 60 | 236 |
| A-05 | AMBIGUITY | 6 | 17 |
| A-06 | AMBIGUITY | 113 | 438 |
| A-07 | AMBIGUITY | 410 | 1587 |
| A-08 | AMBIGUITY | 6166 | 270555 |
| A-09 | AMBIGUITY | 3833 | 57991 |
| A-10 | AMBIGUITY | 2 | 2 |
| A-11 | AMBIGUITY | 50 | 180 |
| A-12 | AMBIGUITY | 10 | 19 |
| A-13 | AMBIGUITY | 2933 | 6707 |
| A-14 | AMBIGUITY | 6194 | 144603 |
| A-15 | AMBIGUITY | 3 | 39 |
| A-16 | AMBIGUITY | 45 | 332 |
| A-17 | AMBIGUITY | 270 | 1832 |
| A-18 | AMBIGUITY | 1 | 2 |
| A-19 | AMBIGUITY | 9028 | 74104 |
| A-20 | AMBIGUITY | 35011 | 15299514 |
| A-21 | AMBIGUITY | 16605 | 265164 |
| A-22 | AMBIGUITY | 1 | 1 |
| A-23 | AMBIGUITY | 161 | 863 |
| A-24 | AMBIGUITY | 1 | 1 |
| A-25 | AMBIGUITY | 1326 | 8948 |
| A-26 | AMBIGUITY | 35018 | 2880437 |
| A-27 | AMBIGUITY | 4 | 4 |
| A-28 | AMBIGUITY | 35123 | 35123 |
| I-01 | INVENTION | 35123 | 35123 |
| I-02 | INVENTION | 27888 | 6025849 |
| I-03 | INVENTION | 35123 | 35123 |
| I-04 | INVENTION | 35123 | 35123 |
| I-05 | INVENTION | 16178 | 246413 |
| I-06 | INVENTION | 16178 | 246413 |
| I-07 | INVENTION | 35123 | 35123 |
| I-08 | INVENTION | 0 | 0 |
| I-09 | INVENTION | 0 | 0 |
| I-10 | INVENTION | 410 | 1587 |
| I-11 | INVENTION | 60 | 235 |
| I-12 | INVENTION | 0 | 0 |
| I-13 | INVENTION | 9028 | 74104 |
| I-14 | INVENTION | 6194 | 85547 |
| L-01 | LOSS | 9167 | 75691 |
| L-02 | LOSS | 4 | 18 |
| L-03 | LOSS | 0 | 0 |
| L-04 | LOSS | 0 | 0 |
| L-05 | LOSS | 0 | 0 |
| L-06 | LOSS | 0 | 0 |
| L-07 | LOSS | 4 | 4 |
| L-08 | LOSS | 0 | 0 |
| L-09 | LOSS | 2 | 7 |
| L-10 | LOSS | 0 | 0 |
| L-11 | LOSS | 23 | 47 |
| L-12 | LOSS | 1 | 1 |
| L-13 | LOSS | 120 | 685 |
| L-14 | LOSS | 325 | 2920 |
| L-15 | LOSS | 0 | 0 |
| L-16 | LOSS | 0 | 0 |
| L-17 | LOSS | 0 | 0 |
| L-18 | LOSS | 392 | 1480 |
| L-19 | LOSS | 209 | 447 |
| L-20 | LOSS | 5 | 8 |
| L-21 | LOSS | 10 | 19 |
| L-22 | LOSS | 0 | 0 |
| L-23 | LOSS | 20 | 102 |
| L-24 | LOSS | 20 | 102 |
| L-25 | LOSS | 120 | 533 |
| L-26 | LOSS | 120 | 533 |
| L-27 | LOSS | 8 | 25 |
| L-28 | LOSS | 2933 | 6707 |
| L-29 | LOSS | 0 | 0 |
| L-30 | LOSS | 7 | 17 |
| L-31 | LOSS | 14 | 128 |
| L-32 | LOSS | 48 | 267 |
| L-33 | LOSS | 6 | 7 |
| L-34 | LOSS | 68 | 910 |
| L-35 | LOSS | 943 | 37862 |
| L-36 | LOSS | 0 | 0 |
| L-37 | LOSS | 0 | 0 |
| L-38 | LOSS | 0 | 0 |
| L-39 | LOSS | 0 | 0 |
| L-40 | LOSS | 3322 | 17310 |
| L-41 | LOSS | 2842 | 9108 |
| L-42 | LOSS | 1 | 1 |
| L-43 | LOSS | 1 | 2 |
| L-44 | LOSS | 1 | 2 |
| L-45 | LOSS | 12 | 62 |
| L-46 | LOSS | 12 | 62 |
| L-47 | LOSS | 1 | 2 |
| L-48 | LOSS | 1326 | 8948 |
| L-49 | LOSS | 35123 | 35123 |
| L-50 | LOSS | 35123 | 35123 |
| L-51 | LOSS | 17894 | 17894 |
| L-52 | LOSS | 35123 | 35123 |
| L-53 | LOSS | 32250 | 32250 |
| S-01 | STRUCTURAL | 4 | 4 |
| S-02 | STRUCTURAL | 58 | 232 |
| S-03 | STRUCTURAL | 2933 | 6707 |
| S-04 | STRUCTURAL | 60 | 235 |
| S-05 | STRUCTURAL | 6166 | 137661 |
| S-06 | STRUCTURAL | 1326 | 8948 |
| S-07 | STRUCTURAL | 6194 | 85547 |
| S-08 | STRUCTURAL | 35011 | 2785942 |
| U-01 | UNSUPPORTED | 9 | 41 |
| U-02 | UNSUPPORTED | 0 | 0 |
| U-03 | UNSUPPORTED | 1 | 1 |
| U-04 | UNSUPPORTED | 1 | 2 |
| U-05 | UNSUPPORTED | 5 | 22 |
| U-06 | UNSUPPORTED | 299 | 4708 |
| U-07 | UNSUPPORTED | 24 | 141 |
| U-08 | UNSUPPORTED | 50 | 180 |
| U-09 | UNSUPPORTED | 1 | 1 |
| U-10 | UNSUPPORTED | 0 | 0 |
| U-11 | UNSUPPORTED | 2 | 2 |
| U-12 | UNSUPPORTED | 11 | 96 |
| U-13 | UNSUPPORTED | 500 | 5031 |
| U-14 | UNSUPPORTED | 40 | 142 |
| U-15 | UNSUPPORTED | 161 | 863 |

## Top Errors

| count | message | samples |
|---:|---|---|
| 460 | unsupported inline kind: figure | aozora2html-adapter:000009_226-7771d96568b3.json<br>aozora2html-adapter:000009_50711-f70596fd94c9.json<br>aozora2html-adapter:000009_50712-3bd98c23f17b.json<br>aozora2html-adapter:000009_50713-8c9159c8c915.json<br>aozora2html-adapter:000009_50714-588859399991.json |

## Failure Samples

| corpus | path | message |
|---|---|---|
| aozora2html-adapter | `000009_226-7771d96568b3.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000009_50711-f70596fd94c9.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000009_50712-3bd98c23f17b.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000009_50713-8c9159c8c915.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000009_50714-588859399991.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000009_50715-0bd4eab2b77d.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000009_50716-4a0d93059ac8.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000009_50717-0eeb5853dc3a.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000009_50718-3bd33a2d1f82.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000009_54910-42cf7cd96173.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000009_54911-2cb75e442e4d.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000009_54912-7ebf88db448d.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000009_54913-d76c2d5a30f2.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000009_54914-e26a90bf5379.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000009_54915-01f287655c90.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000009_55881-f4df11665cda.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000009_55882-1cadf9cd2bfd.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000009_57322-eb1ac8e18a2d.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000009_61393-46f69ce5ba94.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000009_61394-7bfd13580c25.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000009_8-5db3c281b807.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000014_728-af8280acaa59.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000019_194-5219b1146dad.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000019_42378-fd25eeedeb58.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000019_42379-2c7b3481e08a.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000019_42380-e06a90ced089.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000019_42381-e359d7f661b1.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000019_42382-c81f40a971a8.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000019_42383-b9c20f2eaa0d.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000019_42384-efc54318f404.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000019_42385-fe87380e7e80.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000019_42387-a4296dd29b5b.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000019_44423-c9eca98ed7aa.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000019_46318-23650845f06c.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000019_46319-90f61566f086.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000019_58861-bf697ebc4b79.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000019_62693-2ece272dce7e.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000025_1144-dc8e42a7bbde.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000025_61236-2e637e23447b.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000034_55507-30a9658433b4.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000040_47287-6819a6d8336a.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000042_1684-e38fc64b1764.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000042_24394-b2136b763458.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000042_2441-f3f17aaff217.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000042_42702-7d3c7ce67ced.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000042_42705-3d7bc3b96c41.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000051_50021-da59112a0542.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000058_57440-fd9d6f0c6c51.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000061_510-7c87649c3a47.json` | unsupported inline kind: figure |
| aozora2html-adapter | `000063_385-d4fb3dee4ba1.json` | unsupported inline kind: figure |
