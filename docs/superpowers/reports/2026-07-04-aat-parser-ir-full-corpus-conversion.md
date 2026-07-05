# AAT Parser-IR Conversion Audit

- generated_unix_seconds: `1783246241`
- mapping: `https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe` `0.2.3`
- mapping_hash: `sha256:9812e1cc0d956143e61cdd9d0a5bccaa3ec301c7984279230107c23919936e8b`
- mapping_schema_hash: `sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4`
- target_parser_ir_schema_hash: `sha256:d98eb9684e7a88f5b62693dd582e28f14834ff85011dc7297e7b41516f7be913`

## Totals

| files_attempted | files_succeeded | files_failed | parser_ir_nodes | divergence_records | divergence_occurrences | elapsed_seconds |
|---:|---:|---:|---:|---:|---:|---:|
| 89169 | 88698 | 471 | 32957092 | 1030352 | 58212289 | 32.165 |

## Inputs

| label | files | aat_dir |
|---|---:|---|
| aozora-rs-adapter | 17894 | `/home/bor/Projects/ab-validator/scratch/morph-full-corpus/aats/aozora-rs-adapter` |
| aozora2-adapter | 17856 | `/db/ab-validator/aat-corpus/aozora2-full-20260705T083650Z-layout-fix5/aat/aozora2-adapter` |
| aozora2html-adapter | 17689 | `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter` |
| aozora-epub3-adapter | 17844 | `/db/ab-validator/aat-corpus/aozora-epub3-full-20260704T050652Z-300s/aat/aozora-epub3-adapter` |
| aozora-adapter | 17886 | `/db/ab-validator/aat-corpus/aozora-full-20260705T015007Z/aat/aozora-adapter` |

## Corpus Results

| corpus | attempted | succeeded | failed | parser_ir_nodes | divergence_occurrences |
|---|---:|---:|---:|---:|---:|
| aozora-adapter | 17886 | 17886 | 0 | 988246 | 5100160 |
| aozora-epub3-adapter | 17844 | 17844 | 0 | 10600649 | 18103739 |
| aozora-rs-adapter | 17894 | 17894 | 0 | 7821839 | 11926731 |
| aozora2-adapter | 17856 | 17385 | 471 | 5215785 | 11101371 |
| aozora2html-adapter | 17689 | 17689 | 0 | 8330573 | 11980288 |

## Raw Nodes

- nodes_total: `4800218`
- files_with_raw: `21917`
- fatal_direct_failures: `0`

| corpus | nodes_total | files_with_raw | fatal_direct_failures |
|---|---:|---:|---:|
| aozora-adapter | 4727752 | 16890 | 0 |
| aozora2-adapter | 69507 | 4768 | 0 |
| aozora2html-adapter | 2959 | 259 | 0 |

| inferred_provenance | nodes |
|---|---:|
| parser-derived | 4282492 |
| source-derived | 517726 |

| source_class | nodes |
|---|---:|
| aozora-command | 1886 |
| aozora-marker | 195397 |
| editorial-note | 8591 |
| empty | 2989698 |
| html-fragment | 2960 |
| parser-token | 51093 |
| text | 1550593 |

| corpus | path | pointer | provenance | class | source_marker_kind | source_preview |
|---|---|---|---|---|---|---|
| aozora2-adapter | `000005_53194-ebb0cbaf64b3.json` | `$.blocks[0].content[0].content[0].content[0]` | source-derived | text |  | ページの左右中央 |
| aozora2-adapter | `000005_53194-ebb0cbaf64b3.json` | `$.blocks[0].content[0].content[4]` | source-derived | aozora-command |  | 改丁 |
| aozora2-adapter | `000005_53194-ebb0cbaf64b3.json` | `$.blocks[44].content[0].content[261]` | source-derived | editorial-note |  | 「……』」は底本では「……」」 |
| aozora2-adapter | `000006_1869-62320f0f4474.json` | `$.blocks[8].content[423].base_content[1]` | source-derived | text |  | 「引」は小書き右寄せ |
| aozora2-adapter | `000006_1869-62320f0f4474.json` | `$.blocks[10].content[4170]` | source-derived | aozora-command |  | 改丁 |
| aozora2-adapter | `000006_1869-62320f0f4474.json` | `$.blocks[10].content[5133]` | source-derived | editorial-note |  | 「狼狽《あわ》てて」は底本では「狼狙《あわ》てて」 |
| aozora2-adapter | `000006_1869-62320f0f4474.json` | `$.blocks[12].content[1311]` | source-derived | text |  | 「引」は小書き右寄せ |
| aozora2-adapter | `000006_1869-62320f0f4474.json` | `$.blocks[16].content[179]` | source-derived | text |  | 「引」は小書き右寄せ |
| aozora2-adapter | `000006_1869-62320f0f4474.json` | `$.blocks[16].content[182]` | source-derived | text |  | 「引」は小書き右寄せ |
| aozora2-adapter | `000006_1869-62320f0f4474.json` | `$.blocks[16].content[1403]` | source-derived | aozora-command |  | 改丁 |
| aozora2-adapter | `000006_1869-62320f0f4474.json` | `$.blocks[18].content[1275]` | source-derived | text |  | 「引」は小書き右寄せ |
| aozora2-adapter | `000006_1869-62320f0f4474.json` | `$.blocks[18].content[1277]` | source-derived | text |  | 「引」は小書き右寄せ |
| aozora2-adapter | `000006_1869-62320f0f4474.json` | `$.blocks[18].content[1279]` | source-derived | text |  | 「引」は小書き右寄せ |
| aozora2-adapter | `000006_3310-49cd8c085df4.json` | `$.blocks[2].content[0].content[434]` | source-derived | text |  | 「涕」はママ |
| aozora2-adapter | `000006_4627-ded0d23b15b9.json` | `$.blocks[0].content[7]` | source-derived | editorial-note |  | 「齷齪」は底本では「齷齦」 |
| aozora2-adapter | `000008_1083-2ea128250d0d.json` | `$.blocks[0].content[0].content[23]` | source-derived | text |  | 底本は改行天付き |
| aozora2-adapter | `000008_1083-2ea128250d0d.json` | `$.blocks[0].content[0].content[35]` | source-derived | text |  | 底本は「俵」を「依」と誤植 |
| aozora2-adapter | `000008_1083-2ea128250d0d.json` | `$.blocks[0].content[0].content[37]` | source-derived | text |  | 底本は「ただまま」を「ただま」と誤植 |
| aozora2-adapter | `000008_1083-2ea128250d0d.json` | `$.blocks[0].content[0].content[45]` | source-derived | editorial-note |  | 底本ではここのみ「莚」。他は「筵」 |
| aozora2-adapter | `000008_1083-2ea128250d0d.json` | `$.blocks[0].content[0].content[53]` | source-derived | editorial-note |  | 「仁王立ちになった」は底本では「仁王立ち」と誤植 |

## Divergence Categories

| category | occurrences |
|---|---:|
| AMBIGUITY | 32959970 |
| INVENTION | 13324689 |
| LOSS | 6892156 |
| STRUCTURAL | 221920 |
| UNSUPPORTED | 4813554 |

## Compatibility Candidates

| adapter | adapter_version | mapping_version | mapping_hash | files_succeeded | files_failed | rules_emitted | rules_missing | unsupported_occurrences |
|---|---|---|---|---:|---:|---:|---:|---:|
| aozora | aozora-adapter 0.1.0 aozora 0.4.1 | 0.2.3 | `sha256:9812e1cc0d956143e61cdd9d0a5bccaa3ec301c7984279230107c23919936e8b` | 17886 | 0 | 16 | 698 | 4727752 |
| aozora-epub3 | aozora-epub3-adapter 0.1.0 AozoraEpub3-JDK21-1.3.4-jdk21 | 0.2.3 | `sha256:9812e1cc0d956143e61cdd9d0a5bccaa3ec301c7984279230107c23919936e8b` | 17844 | 0 | 61 | 653 | 13234 |
| aozora-rs | aozora-rs-adapter 0.1.0 2b4e8d1 | 0.2.3 | `sha256:9812e1cc0d956143e61cdd9d0a5bccaa3ec301c7984279230107c23919936e8b` | 17894 | 0 | 25 | 689 | 0 |
| aozora2 | aozora2-adapter 0.1.0 aozora-core-0.7.1 | 0.2.3 | `sha256:9812e1cc0d956143e61cdd9d0a5bccaa3ec301c7984279230107c23919936e8b` | 17385 | 471 | 537 | 177 | 58338 |
| aozora2html | aozora2html-adapter 0.1.0 gem-3.0.1 | 0.2.3 | `sha256:9812e1cc0d956143e61cdd9d0a5bccaa3ec301c7984279230107c23919936e8b` | 17689 | 0 | 112 | 602 | 14230 |

## Rule Coverage

- rules_total: `714`
- rules_emitted: `565`
- rules_missing: `149`

| rule_id | category | files | occurrences |
|---|---|---:|---:|
| A-01 | AMBIGUITY | 0 | 0 |
| A-02 | AMBIGUITY | 0 | 0 |
| A-03 | AMBIGUITY | 1 | 4 |
| A-04 | AMBIGUITY | 2 | 7 |
| A-05 | AMBIGUITY | 195 | 2703 |
| A-06 | AMBIGUITY | 45 | 128 |
| A-07 | AMBIGUITY | 1 | 14 |
| A-08 | AMBIGUITY | 19 | 21 |
| A-09 | AMBIGUITY | 19 | 21 |
| A-10 | AMBIGUITY | 0 | 0 |
| A-100 | AMBIGUITY | 6 | 9 |
| A-101 | AMBIGUITY | 74 | 615 |
| A-102 | AMBIGUITY | 1 | 1 |
| A-103 | AMBIGUITY | 2080 | 10877 |
| A-104 | AMBIGUITY | 11501 | 88477 |
| A-105 | AMBIGUITY | 70437 | 28571108 |
| A-106 | AMBIGUITY | 32655 | 496847 |
| A-107 | AMBIGUITY | 3 | 4 |
| A-108 | AMBIGUITY | 0 | 0 |
| A-109 | AMBIGUITY | 10 | 13 |
| A-11 | AMBIGUITY | 0 | 0 |
| A-110 | AMBIGUITY | 14 | 20 |
| A-111 | AMBIGUITY | 326 | 2306 |
| A-112 | AMBIGUITY | 1 | 7 |
| A-113 | AMBIGUITY | 1 | 1 |
| A-114 | AMBIGUITY | 1 | 1 |
| A-115 | AMBIGUITY | 2 | 2 |
| A-116 | AMBIGUITY | 2 | 2 |
| A-117 | AMBIGUITY | 7 | 10 |
| A-118 | AMBIGUITY | 24 | 154 |
| A-119 | AMBIGUITY | 162 | 2564 |
| A-12 | AMBIGUITY | 1 | 1 |
| A-120 | AMBIGUITY | 23 | 213 |
| A-121 | AMBIGUITY | 0 | 0 |
| A-122 | AMBIGUITY | 5150 | 78174 |
| A-123 | AMBIGUITY | 11877 | 154644 |
| A-124 | AMBIGUITY | 7 | 7 |
| A-125 | AMBIGUITY | 88698 | 88698 |
| A-126 | AMBIGUITY | 0 | 0 |
| A-13 | AMBIGUITY | 0 | 0 |
| A-14 | AMBIGUITY | 0 | 0 |
| A-15 | AMBIGUITY | 0 | 0 |
| A-16 | AMBIGUITY | 0 | 0 |
| A-17 | AMBIGUITY | 1 | 1 |
| A-18 | AMBIGUITY | 1 | 1 |
| A-19 | AMBIGUITY | 1 | 1 |
| A-20 | AMBIGUITY | 1 | 1 |
| A-21 | AMBIGUITY | 12 | 35 |
| A-22 | AMBIGUITY | 3 | 3 |
| A-23 | AMBIGUITY | 151 | 595 |
| A-24 | AMBIGUITY | 3 | 3 |
| A-25 | AMBIGUITY | 92 | 2148 |
| A-26 | AMBIGUITY | 27 | 34 |
| A-27 | AMBIGUITY | 312 | 1016 |
| A-28 | AMBIGUITY | 800 | 3093 |
| A-29 | AMBIGUITY | 15327 | 1019895 |
| A-30 | AMBIGUITY | 5412 | 81168 |
| A-31 | AMBIGUITY | 2 | 2 |
| A-32 | AMBIGUITY | 5 | 9 |
| A-33 | AMBIGUITY | 5 | 10 |
| A-34 | AMBIGUITY | 134 | 933 |
| A-35 | AMBIGUITY | 2 | 5 |
| A-36 | AMBIGUITY | 11 | 20 |
| A-37 | AMBIGUITY | 6632 | 71441 |
| A-38 | AMBIGUITY | 6635 | 71461 |
| A-39 | AMBIGUITY | 0 | 0 |
| A-40 | AMBIGUITY | 0 | 0 |
| A-41 | AMBIGUITY | 1 | 4 |
| A-42 | AMBIGUITY | 1 | 1 |
| A-43 | AMBIGUITY | 1 | 1 |
| A-44 | AMBIGUITY | 1 | 1 |
| A-45 | AMBIGUITY | 1 | 2 |
| A-46 | AMBIGUITY | 1 | 4 |
| A-47 | AMBIGUITY | 1 | 1 |
| A-48 | AMBIGUITY | 1 | 1 |
| A-49 | AMBIGUITY | 1 | 5 |
| A-50 | AMBIGUITY | 1 | 1 |
| A-51 | AMBIGUITY | 1 | 1 |
| A-52 | AMBIGUITY | 0 | 0 |
| A-53 | AMBIGUITY | 1 | 2 |
| A-54 | AMBIGUITY | 1 | 1 |
| A-55 | AMBIGUITY | 1 | 1 |
| A-56 | AMBIGUITY | 1 | 1 |
| A-57 | AMBIGUITY | 1 | 1 |
| A-58 | AMBIGUITY | 0 | 0 |
| A-59 | AMBIGUITY | 0 | 0 |
| A-60 | AMBIGUITY | 0 | 0 |
| A-61 | AMBIGUITY | 1 | 1 |
| A-62 | AMBIGUITY | 0 | 0 |
| A-63 | AMBIGUITY | 1 | 8 |
| A-64 | AMBIGUITY | 1 | 1 |
| A-65 | AMBIGUITY | 1 | 2 |
| A-66 | AMBIGUITY | 2 | 7 |
| A-67 | AMBIGUITY | 2 | 3 |
| A-68 | AMBIGUITY | 0 | 0 |
| A-69 | AMBIGUITY | 0 | 0 |
| A-70 | AMBIGUITY | 0 | 0 |
| A-71 | AMBIGUITY | 2 | 2 |
| A-72 | AMBIGUITY | 4 | 6 |
| A-73 | AMBIGUITY | 3 | 4 |
| A-74 | AMBIGUITY | 2 | 5 |
| A-75 | AMBIGUITY | 2 | 3 |
| A-76 | AMBIGUITY | 3 | 9 |
| A-77 | AMBIGUITY | 1 | 2 |
| A-78 | AMBIGUITY | 1 | 1 |
| A-79 | AMBIGUITY | 4 | 8 |
| A-80 | AMBIGUITY | 5 | 10 |
| A-81 | AMBIGUITY | 2 | 4 |
| A-82 | AMBIGUITY | 7 | 26 |
| A-83 | AMBIGUITY | 8 | 20 |
| A-84 | AMBIGUITY | 7 | 15 |
| A-85 | AMBIGUITY | 5 | 11 |
| A-86 | AMBIGUITY | 11 | 24 |
| A-87 | AMBIGUITY | 13 | 24 |
| A-88 | AMBIGUITY | 24 | 44 |
| A-89 | AMBIGUITY | 43 | 129 |
| A-90 | AMBIGUITY | 0 | 0 |
| A-91 | AMBIGUITY | 63 | 175 |
| A-92 | AMBIGUITY | 218 | 1010 |
| A-93 | AMBIGUITY | 818 | 7041 |
| A-94 | AMBIGUITY | 1055 | 5531 |
| A-95 | AMBIGUITY | 888 | 7862 |
| A-96 | AMBIGUITY | 3768 | 2142992 |
| A-97 | AMBIGUITY | 2036 | 45426 |
| A-98 | AMBIGUITY | 0 | 0 |
| A-99 | AMBIGUITY | 5 | 8 |
| I-01 | INVENTION | 88698 | 88698 |
| I-02 | INVENTION | 57266 | 12029483 |
| I-03 | INVENTION | 88698 | 88698 |
| I-04 | INVENTION | 88698 | 88698 |
| I-05 | INVENTION | 33144 | 381066 |
| I-06 | INVENTION | 33144 | 381066 |
| I-07 | INVENTION | 88698 | 88698 |
| I-08 | INVENTION | 0 | 0 |
| I-09 | INVENTION | 1 | 47 |
| I-10 | INVENTION | 2 | 7 |
| I-11 | INVENTION | 0 | 0 |
| I-12 | INVENTION | 0 | 0 |
| I-13 | INVENTION | 3 | 3 |
| I-14 | INVENTION | 42 | 587 |
| I-15 | INVENTION | 800 | 3093 |
| I-16 | INVENTION | 5 | 10 |
| I-17 | INVENTION | 15 | 15 |
| I-18 | INVENTION | 0 | 0 |
| I-19 | INVENTION | 0 | 0 |
| I-20 | INVENTION | 127 | 683 |
| I-21 | INVENTION | 888 | 7862 |
| I-22 | INVENTION | 6 | 9 |
| I-23 | INVENTION | 757 | 5643 |
| I-24 | INVENTION | 11501 | 88477 |
| I-25 | INVENTION | 0 | 0 |
| I-26 | INVENTION | 14 | 20 |
| I-27 | INVENTION | 6704 | 71826 |
| L-01 | LOSS | 12328 | 99481 |
| L-02 | LOSS | 0 | 0 |
| L-03 | LOSS | 1 | 10 |
| L-04 | LOSS | 7 | 24 |
| L-05 | LOSS | 1 | 47 |
| L-06 | LOSS | 1 | 47 |
| L-07 | LOSS | 1 | 47 |
| L-08 | LOSS | 1 | 3 |
| L-09 | LOSS | 25 | 96 |
| L-10 | LOSS | 0 | 0 |
| L-100 | LOSS | 1 | 1 |
| L-101 | LOSS | 1 | 1 |
| L-102 | LOSS | 1 | 1 |
| L-103 | LOSS | 1 | 1 |
| L-104 | LOSS | 0 | 0 |
| L-105 | LOSS | 1 | 1 |
| L-106 | LOSS | 0 | 0 |
| L-107 | LOSS | 1 | 1 |
| L-108 | LOSS | 0 | 0 |
| L-109 | LOSS | 1 | 1 |
| L-11 | LOSS | 1 | 1 |
| L-110 | LOSS | 1 | 1 |
| L-111 | LOSS | 0 | 0 |
| L-112 | LOSS | 1 | 2 |
| L-113 | LOSS | 1 | 1 |
| L-114 | LOSS | 0 | 0 |
| L-115 | LOSS | 1 | 2 |
| L-116 | LOSS | 1 | 1 |
| L-117 | LOSS | 0 | 0 |
| L-118 | LOSS | 1 | 4 |
| L-119 | LOSS | 1 | 1 |
| L-12 | LOSS | 1 | 1 |
| L-120 | LOSS | 0 | 0 |
| L-121 | LOSS | 1 | 1 |
| L-122 | LOSS | 1 | 1 |
| L-123 | LOSS | 0 | 0 |
| L-124 | LOSS | 1 | 1 |
| L-125 | LOSS | 1 | 1 |
| L-126 | LOSS | 0 | 0 |
| L-127 | LOSS | 1 | 1 |
| L-128 | LOSS | 1 | 1 |
| L-129 | LOSS | 0 | 0 |
| L-13 | LOSS | 1 | 1 |
| L-130 | LOSS | 1 | 2 |
| L-131 | LOSS | 1 | 1 |
| L-132 | LOSS | 0 | 0 |
| L-133 | LOSS | 1 | 1 |
| L-134 | LOSS | 1 | 1 |
| L-135 | LOSS | 0 | 0 |
| L-136 | LOSS | 1 | 1 |
| L-137 | LOSS | 1 | 1 |
| L-138 | LOSS | 0 | 0 |
| L-139 | LOSS | 2 | 4 |
| L-14 | LOSS | 19 | 21 |
| L-140 | LOSS | 1 | 1 |
| L-141 | LOSS | 0 | 0 |
| L-142 | LOSS | 2 | 4 |
| L-143 | LOSS | 1 | 1 |
| L-144 | LOSS | 0 | 0 |
| L-145 | LOSS | 1 | 1 |
| L-146 | LOSS | 0 | 0 |
| L-147 | LOSS | 2 | 5 |
| L-148 | LOSS | 1 | 1 |
| L-149 | LOSS | 0 | 0 |
| L-15 | LOSS | 0 | 0 |
| L-150 | LOSS | 2 | 5 |
| L-151 | LOSS | 1 | 1 |
| L-152 | LOSS | 0 | 0 |
| L-153 | LOSS | 2 | 2 |
| L-154 | LOSS | 1 | 1 |
| L-155 | LOSS | 0 | 0 |
| L-156 | LOSS | 1 | 1 |
| L-157 | LOSS | 1 | 1 |
| L-158 | LOSS | 2 | 5 |
| L-159 | LOSS | 1 | 1 |
| L-16 | LOSS | 0 | 0 |
| L-160 | LOSS | 0 | 0 |
| L-161 | LOSS | 2 | 2 |
| L-162 | LOSS | 1 | 1 |
| L-163 | LOSS | 0 | 0 |
| L-164 | LOSS | 6 | 28 |
| L-165 | LOSS | 1 | 1 |
| L-166 | LOSS | 0 | 0 |
| L-167 | LOSS | 13 | 140 |
| L-168 | LOSS | 1 | 1 |
| L-169 | LOSS | 3 | 7 |
| L-17 | LOSS | 0 | 0 |
| L-170 | LOSS | 29 | 162 |
| L-171 | LOSS | 1 | 1 |
| L-172 | LOSS | 8 | 14 |
| L-173 | LOSS | 85 | 560 |
| L-174 | LOSS | 3 | 3 |
| L-175 | LOSS | 161 | 2788 |
| L-176 | LOSS | 26 | 925 |
| L-177 | LOSS | 550 | 5540 |
| L-178 | LOSS | 0 | 0 |
| L-179 | LOSS | 42 | 587 |
| L-18 | LOSS | 0 | 0 |
| L-180 | LOSS | 34 | 579 |
| L-181 | LOSS | 34 | 579 |
| L-182 | LOSS | 475 | 1857 |
| L-183 | LOSS | 4913 | 174977 |
| L-184 | LOSS | 0 | 0 |
| L-185 | LOSS | 1 | 11 |
| L-186 | LOSS | 0 | 0 |
| L-187 | LOSS | 0 | 0 |
| L-188 | LOSS | 0 | 0 |
| L-189 | LOSS | 6 | 9 |
| L-19 | LOSS | 1 | 1 |
| L-190 | LOSS | 11 | 20 |
| L-191 | LOSS | 1 | 1 |
| L-192 | LOSS | 23 | 118 |
| L-193 | LOSS | 23 | 118 |
| L-194 | LOSS | 518 | 5987 |
| L-195 | LOSS | 518 | 5987 |
| L-196 | LOSS | 47 | 103 |
| L-197 | LOSS | 58 | 481 |
| L-198 | LOSS | 6632 | 71441 |
| L-199 | LOSS | 0 | 0 |
| L-20 | LOSS | 1 | 1 |
| L-200 | LOSS | 0 | 0 |
| L-201 | LOSS | 0 | 0 |
| L-202 | LOSS | 0 | 0 |
| L-203 | LOSS | 0 | 0 |
| L-204 | LOSS | 0 | 0 |
| L-205 | LOSS | 0 | 0 |
| L-206 | LOSS | 0 | 0 |
| L-207 | LOSS | 0 | 0 |
| L-208 | LOSS | 0 | 0 |
| L-209 | LOSS | 0 | 0 |
| L-21 | LOSS | 1 | 1 |
| L-210 | LOSS | 0 | 0 |
| L-211 | LOSS | 0 | 0 |
| L-212 | LOSS | 0 | 0 |
| L-213 | LOSS | 0 | 0 |
| L-214 | LOSS | 0 | 0 |
| L-215 | LOSS | 0 | 0 |
| L-216 | LOSS | 0 | 0 |
| L-217 | LOSS | 0 | 0 |
| L-218 | LOSS | 1 | 2 |
| L-219 | LOSS | 2 | 10 |
| L-22 | LOSS | 1 | 1 |
| L-220 | LOSS | 1 | 3 |
| L-221 | LOSS | 2 | 2 |
| L-222 | LOSS | 2 | 3 |
| L-223 | LOSS | 2 | 3 |
| L-224 | LOSS | 2 | 3 |
| L-225 | LOSS | 2 | 3 |
| L-226 | LOSS | 1 | 7 |
| L-227 | LOSS | 1 | 2 |
| L-228 | LOSS | 1 | 2 |
| L-229 | LOSS | 1 | 3 |
| L-23 | LOSS | 1 | 1 |
| L-230 | LOSS | 1 | 1 |
| L-231 | LOSS | 2 | 9 |
| L-232 | LOSS | 1 | 6 |
| L-233 | LOSS | 1 | 2 |
| L-234 | LOSS | 2 | 5 |
| L-235 | LOSS | 2 | 3 |
| L-236 | LOSS | 1 | 3 |
| L-237 | LOSS | 2 | 7 |
| L-238 | LOSS | 1 | 2 |
| L-239 | LOSS | 2 | 5 |
| L-24 | LOSS | 1 | 1 |
| L-240 | LOSS | 1 | 3 |
| L-241 | LOSS | 2 | 9 |
| L-242 | LOSS | 1 | 8 |
| L-243 | LOSS | 1 | 4 |
| L-244 | LOSS | 2 | 6 |
| L-245 | LOSS | 2 | 3 |
| L-246 | LOSS | 1 | 12 |
| L-247 | LOSS | 2 | 5 |
| L-248 | LOSS | 2 | 4 |
| L-249 | LOSS | 1 | 1 |
| L-25 | LOSS | 1 | 1 |
| L-250 | LOSS | 1 | 2 |
| L-251 | LOSS | 2 | 5 |
| L-252 | LOSS | 2 | 9 |
| L-253 | LOSS | 2 | 4 |
| L-254 | LOSS | 2 | 19 |
| L-255 | LOSS | 2 | 12 |
| L-256 | LOSS | 2 | 20 |
| L-257 | LOSS | 2 | 13 |
| L-258 | LOSS | 3 | 14 |
| L-259 | LOSS | 2 | 11 |
| L-26 | LOSS | 1 | 1 |
| L-260 | LOSS | 3 | 14 |
| L-261 | LOSS | 3 | 6 |
| L-262 | LOSS | 3 | 10 |
| L-263 | LOSS | 3 | 8 |
| L-264 | LOSS | 3 | 11 |
| L-265 | LOSS | 2 | 21 |
| L-266 | LOSS | 3 | 13 |
| L-267 | LOSS | 2 | 8 |
| L-268 | LOSS | 2 | 5 |
| L-269 | LOSS | 3 | 14 |
| L-27 | LOSS | 1 | 1 |
| L-270 | LOSS | 2 | 8 |
| L-271 | LOSS | 3 | 7 |
| L-272 | LOSS | 2 | 7 |
| L-273 | LOSS | 3 | 7 |
| L-274 | LOSS | 3 | 15 |
| L-275 | LOSS | 3 | 13 |
| L-276 | LOSS | 3 | 10 |
| L-277 | LOSS | 2 | 17 |
| L-278 | LOSS | 3 | 5 |
| L-279 | LOSS | 3 | 11 |
| L-28 | LOSS | 1 | 1 |
| L-280 | LOSS | 4 | 19 |
| L-281 | LOSS | 5 | 17 |
| L-282 | LOSS | 5 | 22 |
| L-283 | LOSS | 0 | 0 |
| L-284 | LOSS | 5 | 23 |
| L-285 | LOSS | 0 | 0 |
| L-286 | LOSS | 5 | 26 |
| L-287 | LOSS | 0 | 0 |
| L-288 | LOSS | 4 | 16 |
| L-289 | LOSS | 0 | 0 |
| L-29 | LOSS | 1 | 1 |
| L-290 | LOSS | 5 | 16 |
| L-291 | LOSS | 4 | 23 |
| L-292 | LOSS | 5 | 31 |
| L-293 | LOSS | 4 | 31 |
| L-294 | LOSS | 5 | 24 |
| L-295 | LOSS | 5 | 21 |
| L-296 | LOSS | 5 | 18 |
| L-297 | LOSS | 1 | 1 |
| L-298 | LOSS | 5 | 22 |
| L-299 | LOSS | 1 | 1 |
| L-30 | LOSS | 1 | 1 |
| L-300 | LOSS | 5 | 26 |
| L-301 | LOSS | 3 | 24 |
| L-302 | LOSS | 6 | 26 |
| L-303 | LOSS | 4 | 26 |
| L-304 | LOSS | 0 | 0 |
| L-305 | LOSS | 6 | 33 |
| L-306 | LOSS | 3 | 37 |
| L-307 | LOSS | 1 | 2 |
| L-308 | LOSS | 1 | 4 |
| L-309 | LOSS | 6 | 29 |
| L-31 | LOSS | 1 | 1 |
| L-310 | LOSS | 6 | 43 |
| L-311 | LOSS | 6 | 46 |
| L-312 | LOSS | 5 | 67 |
| L-313 | LOSS | 6 | 38 |
| L-314 | LOSS | 4 | 39 |
| L-315 | LOSS | 6 | 32 |
| L-316 | LOSS | 5 | 40 |
| L-317 | LOSS | 7 | 54 |
| L-318 | LOSS | 6 | 30 |
| L-319 | LOSS | 1 | 2 |
| L-32 | LOSS | 1 | 1 |
| L-320 | LOSS | 1 | 4 |
| L-321 | LOSS | 6 | 56 |
| L-322 | LOSS | 1 | 1 |
| L-323 | LOSS | 5 | 72 |
| L-324 | LOSS | 6 | 61 |
| L-325 | LOSS | 5 | 149 |
| L-326 | LOSS | 7 | 85 |
| L-327 | LOSS | 1 | 1 |
| L-328 | LOSS | 5 | 51 |
| L-329 | LOSS | 6 | 69 |
| L-33 | LOSS | 1 | 1 |
| L-330 | LOSS | 7 | 81 |
| L-331 | LOSS | 1 | 2 |
| L-332 | LOSS | 1 | 4 |
| L-333 | LOSS | 6 | 103 |
| L-334 | LOSS | 7 | 92 |
| L-335 | LOSS | 7 | 97 |
| L-336 | LOSS | 0 | 0 |
| L-337 | LOSS | 7 | 112 |
| L-338 | LOSS | 1 | 2 |
| L-339 | LOSS | 1 | 4 |
| L-34 | LOSS | 1 | 1 |
| L-340 | LOSS | 7 | 115 |
| L-341 | LOSS | 7 | 125 |
| L-342 | LOSS | 7 | 167 |
| L-343 | LOSS | 1 | 2 |
| L-344 | LOSS | 2 | 5 |
| L-345 | LOSS | 10 | 142 |
| L-346 | LOSS | 1 | 2 |
| L-347 | LOSS | 9 | 147 |
| L-348 | LOSS | 2 | 3 |
| L-349 | LOSS | 10 | 253 |
| L-35 | LOSS | 1 | 1 |
| L-350 | LOSS | 2 | 3 |
| L-351 | LOSS | 11 | 170 |
| L-352 | LOSS | 0 | 0 |
| L-353 | LOSS | 1 | 2 |
| L-354 | LOSS | 14 | 182 |
| L-355 | LOSS | 5 | 6 |
| L-356 | LOSS | 12 | 262 |
| L-357 | LOSS | 1 | 2 |
| L-358 | LOSS | 2 | 6 |
| L-359 | LOSS | 13 | 406 |
| L-36 | LOSS | 1 | 1 |
| L-360 | LOSS | 0 | 0 |
| L-361 | LOSS | 5 | 12 |
| L-362 | LOSS | 19 | 1612 |
| L-363 | LOSS | 4 | 7 |
| L-364 | LOSS | 24 | 580 |
| L-365 | LOSS | 1 | 2 |
| L-366 | LOSS | 9 | 18 |
| L-367 | LOSS | 35 | 1036 |
| L-368 | LOSS | 5 | 11 |
| L-369 | LOSS | 48 | 995 |
| L-37 | LOSS | 1 | 1 |
| L-370 | LOSS | 2 | 2 |
| L-371 | LOSS | 7 | 55 |
| L-372 | LOSS | 56 | 1143 |
| L-373 | LOSS | 2 | 2 |
| L-374 | LOSS | 10 | 55 |
| L-375 | LOSS | 69 | 1486 |
| L-376 | LOSS | 5 | 9 |
| L-377 | LOSS | 21 | 53 |
| L-378 | LOSS | 95 | 1969 |
| L-379 | LOSS | 3 | 10 |
| L-38 | LOSS | 1 | 1 |
| L-380 | LOSS | 31 | 84 |
| L-381 | LOSS | 165 | 4331 |
| L-382 | LOSS | 0 | 0 |
| L-383 | LOSS | 10 | 26 |
| L-384 | LOSS | 54 | 242 |
| L-385 | LOSS | 265 | 6503 |
| L-386 | LOSS | 23 | 93 |
| L-387 | LOSS | 173 | 800 |
| L-388 | LOSS | 718 | 37706 |
| L-389 | LOSS | 109 | 663 |
| L-39 | LOSS | 1 | 2 |
| L-390 | LOSS | 35 | 143 |
| L-391 | LOSS | 127 | 683 |
| L-392 | LOSS | 126 | 682 |
| L-393 | LOSS | 126 | 682 |
| L-394 | LOSS | 92 | 344 |
| L-395 | LOSS | 805 | 4187 |
| L-396 | LOSS | 3100 | 996054 |
| L-397 | LOSS | 4918 | 367100 |
| L-398 | LOSS | 0 | 0 |
| L-399 | LOSS | 0 | 0 |
| L-40 | LOSS | 1 | 1 |
| L-400 | LOSS | 0 | 0 |
| L-401 | LOSS | 2 | 6 |
| L-402 | LOSS | 0 | 0 |
| L-403 | LOSS | 153 | 731 |
| L-404 | LOSS | 757 | 5643 |
| L-405 | LOSS | 668 | 5409 |
| L-406 | LOSS | 668 | 5409 |
| L-407 | LOSS | 3838 | 19995 |
| L-408 | LOSS | 27267 | 4646312 |
| L-409 | LOSS | 0 | 0 |
| L-41 | LOSS | 1 | 2 |
| L-410 | LOSS | 0 | 0 |
| L-411 | LOSS | 1 | 2 |
| L-412 | LOSS | 2 | 6 |
| L-413 | LOSS | 0 | 0 |
| L-414 | LOSS | 0 | 0 |
| L-415 | LOSS | 0 | 0 |
| L-416 | LOSS | 0 | 0 |
| L-417 | LOSS | 0 | 0 |
| L-418 | LOSS | 0 | 0 |
| L-419 | LOSS | 0 | 0 |
| L-42 | LOSS | 1 | 1 |
| L-420 | LOSS | 0 | 0 |
| L-421 | LOSS | 0 | 0 |
| L-422 | LOSS | 0 | 0 |
| L-423 | LOSS | 0 | 0 |
| L-424 | LOSS | 0 | 0 |
| L-425 | LOSS | 0 | 0 |
| L-426 | LOSS | 0 | 0 |
| L-427 | LOSS | 0 | 0 |
| L-428 | LOSS | 0 | 0 |
| L-429 | LOSS | 0 | 0 |
| L-43 | LOSS | 1 | 1 |
| L-430 | LOSS | 0 | 0 |
| L-431 | LOSS | 0 | 0 |
| L-432 | LOSS | 0 | 0 |
| L-433 | LOSS | 0 | 0 |
| L-434 | LOSS | 0 | 0 |
| L-435 | LOSS | 0 | 0 |
| L-436 | LOSS | 0 | 0 |
| L-437 | LOSS | 0 | 0 |
| L-438 | LOSS | 0 | 0 |
| L-439 | LOSS | 0 | 0 |
| L-44 | LOSS | 1 | 1 |
| L-440 | LOSS | 0 | 0 |
| L-441 | LOSS | 0 | 0 |
| L-442 | LOSS | 0 | 0 |
| L-443 | LOSS | 0 | 0 |
| L-444 | LOSS | 0 | 0 |
| L-445 | LOSS | 0 | 0 |
| L-446 | LOSS | 0 | 0 |
| L-447 | LOSS | 0 | 0 |
| L-448 | LOSS | 0 | 0 |
| L-449 | LOSS | 0 | 0 |
| L-45 | LOSS | 1 | 1 |
| L-450 | LOSS | 0 | 0 |
| L-451 | LOSS | 0 | 0 |
| L-452 | LOSS | 0 | 0 |
| L-453 | LOSS | 0 | 0 |
| L-454 | LOSS | 0 | 0 |
| L-455 | LOSS | 0 | 0 |
| L-456 | LOSS | 0 | 0 |
| L-457 | LOSS | 1 | 8 |
| L-458 | LOSS | 1 | 1 |
| L-459 | LOSS | 1 | 7 |
| L-46 | LOSS | 1 | 2 |
| L-460 | LOSS | 1 | 1 |
| L-461 | LOSS | 3 | 10 |
| L-462 | LOSS | 2 | 2 |
| L-463 | LOSS | 3 | 8 |
| L-464 | LOSS | 5 | 21 |
| L-465 | LOSS | 2 | 2 |
| L-466 | LOSS | 4 | 42 |
| L-467 | LOSS | 1 | 1 |
| L-468 | LOSS | 8 | 27 |
| L-469 | LOSS | 4 | 4 |
| L-47 | LOSS | 1 | 1 |
| L-470 | LOSS | 7 | 18 |
| L-471 | LOSS | 2 | 3 |
| L-472 | LOSS | 10 | 38 |
| L-473 | LOSS | 0 | 0 |
| L-474 | LOSS | 9 | 12 |
| L-475 | LOSS | 15 | 89 |
| L-476 | LOSS | 28 | 56 |
| L-477 | LOSS | 36 | 704 |
| L-478 | LOSS | 4 | 11 |
| L-479 | LOSS | 67 | 231 |
| L-48 | LOSS | 1 | 1 |
| L-480 | LOSS | 77 | 6700 |
| L-481 | LOSS | 31 | 144 |
| L-482 | LOSS | 346 | 6210 |
| L-483 | LOSS | 376 | 124199 |
| L-484 | LOSS | 0 | 0 |
| L-485 | LOSS | 0 | 0 |
| L-486 | LOSS | 8 | 20 |
| L-487 | LOSS | 0 | 0 |
| L-488 | LOSS | 0 | 0 |
| L-489 | LOSS | 23 | 213 |
| L-49 | LOSS | 1 | 1 |
| L-490 | LOSS | 1 | 1 |
| L-491 | LOSS | 61 | 1586 |
| L-492 | LOSS | 61 | 1586 |
| L-493 | LOSS | 394 | 17022 |
| L-494 | LOSS | 394 | 17022 |
| L-495 | LOSS | 505 | 7804 |
| L-496 | LOSS | 36 | 603 |
| L-497 | LOSS | 5150 | 78174 |
| L-498 | LOSS | 17894 | 17894 |
| L-499 | LOSS | 88698 | 88698 |
| L-50 | LOSS | 1 | 1 |
| L-500 | LOSS | 32710 | 32710 |
| L-51 | LOSS | 1 | 1 |
| L-52 | LOSS | 1 | 1 |
| L-53 | LOSS | 1 | 1 |
| L-54 | LOSS | 1 | 1 |
| L-55 | LOSS | 1 | 1 |
| L-56 | LOSS | 1 | 1 |
| L-57 | LOSS | 1 | 1 |
| L-58 | LOSS | 1 | 1 |
| L-59 | LOSS | 1 | 2 |
| L-60 | LOSS | 1 | 1 |
| L-61 | LOSS | 1 | 1 |
| L-62 | LOSS | 1 | 3 |
| L-63 | LOSS | 1 | 1 |
| L-64 | LOSS | 1 | 1 |
| L-65 | LOSS | 1 | 1 |
| L-66 | LOSS | 1 | 1 |
| L-67 | LOSS | 1 | 1 |
| L-68 | LOSS | 1 | 1 |
| L-69 | LOSS | 1 | 2 |
| L-70 | LOSS | 1 | 1 |
| L-71 | LOSS | 1 | 1 |
| L-72 | LOSS | 1 | 1 |
| L-73 | LOSS | 1 | 1 |
| L-74 | LOSS | 1 | 1 |
| L-75 | LOSS | 1 | 2 |
| L-76 | LOSS | 1 | 1 |
| L-77 | LOSS | 1 | 1 |
| L-78 | LOSS | 1 | 1 |
| L-79 | LOSS | 1 | 1 |
| L-80 | LOSS | 2 | 3 |
| L-81 | LOSS | 1 | 1 |
| L-82 | LOSS | 1 | 1 |
| L-83 | LOSS | 1 | 1 |
| L-84 | LOSS | 1 | 1 |
| L-85 | LOSS | 1 | 1 |
| L-86 | LOSS | 1 | 1 |
| L-87 | LOSS | 1 | 2 |
| L-88 | LOSS | 1 | 1 |
| L-89 | LOSS | 1 | 1 |
| L-90 | LOSS | 1 | 1 |
| L-91 | LOSS | 1 | 1 |
| L-92 | LOSS | 1 | 1 |
| L-93 | LOSS | 1 | 1 |
| L-94 | LOSS | 1 | 1 |
| L-95 | LOSS | 1 | 1 |
| L-96 | LOSS | 1 | 1 |
| L-97 | LOSS | 1 | 2 |
| L-98 | LOSS | 1 | 1 |
| L-99 | LOSS | 1 | 1 |
| S-01 | STRUCTURAL | 18 | 72 |
| S-02 | STRUCTURAL | 19 | 21 |
| S-03 | STRUCTURAL | 6632 | 71441 |
| S-04 | STRUCTURAL | 15 | 15 |
| S-05 | STRUCTURAL | 89 | 173 |
| S-06 | STRUCTURAL | 14 | 18 |
| S-07 | STRUCTURAL | 5150 | 78174 |
| S-08 | STRUCTURAL | 6704 | 71826 |
| S-09 | STRUCTURAL | 65 | 112 |
| S-10 | STRUCTURAL | 40 | 68 |
| U-01 | UNSUPPORTED | 14 | 95 |
| U-02 | UNSUPPORTED | 1 | 1 |
| U-03 | UNSUPPORTED | 3 | 9 |
| U-04 | UNSUPPORTED | 1 | 14 |
| U-05 | UNSUPPORTED | 2 | 3 |
| U-06 | UNSUPPORTED | 1 | 1 |
| U-07 | UNSUPPORTED | 3 | 4 |
| U-08 | UNSUPPORTED | 11 | 14 |
| U-09 | UNSUPPORTED | 4 | 11 |
| U-10 | UNSUPPORTED | 10 | 57 |
| U-11 | UNSUPPORTED | 596 | 9603 |
| U-12 | UNSUPPORTED | 33 | 169 |
| U-13 | UNSUPPORTED | 260 | 880 |
| U-14 | UNSUPPORTED | 214 | 3026 |
| U-15 | UNSUPPORTED | 134 | 882 |
| U-16 | UNSUPPORTED | 0 | 0 |
| U-17 | UNSUPPORTED | 1 | 1 |
| U-18 | UNSUPPORTED | 89 | 173 |
| U-19 | UNSUPPORTED | 14 | 18 |
| U-20 | UNSUPPORTED | 8 | 61 |
| U-21 | UNSUPPORTED | 1 | 1 |
| U-22 | UNSUPPORTED | 1 | 1 |
| U-23 | UNSUPPORTED | 1 | 2 |
| U-24 | UNSUPPORTED | 1 | 1 |
| U-25 | UNSUPPORTED | 2 | 6 |
| U-26 | UNSUPPORTED | 9 | 16 |
| U-27 | UNSUPPORTED | 3 | 4 |
| U-28 | UNSUPPORTED | 20 | 82 |
| U-29 | UNSUPPORTED | 93 | 570 |
| U-30 | UNSUPPORTED | 9 | 11 |
| U-31 | UNSUPPORTED | 1099 | 7903 |
| U-32 | UNSUPPORTED | 141 | 1093 |
| U-33 | UNSUPPORTED | 0 | 0 |
| U-34 | UNSUPPORTED | 124 | 806 |
| U-35 | UNSUPPORTED | 1 | 1 |
| U-36 | UNSUPPORTED | 0 | 0 |
| U-37 | UNSUPPORTED | 1309 | 13986 |
| U-38 | UNSUPPORTED | 61 | 238 |
| U-39 | UNSUPPORTED | 20039 | 4758328 |
| U-40 | UNSUPPORTED | 832 | 13066 |
| U-41 | UNSUPPORTED | 326 | 2122 |
| U-42 | UNSUPPORTED | 6 | 6 |
| U-43 | UNSUPPORTED | 0 | 0 |
| U-44 | UNSUPPORTED | 1 | 2 |
| U-45 | UNSUPPORTED | 1 | 3 |
| U-46 | UNSUPPORTED | 1 | 1 |
| U-47 | UNSUPPORTED | 5 | 7 |
| U-48 | UNSUPPORTED | 14 | 52 |
| U-49 | UNSUPPORTED | 6 | 44 |
| U-50 | UNSUPPORTED | 65 | 112 |
| U-51 | UNSUPPORTED | 40 | 68 |

## Top Errors

| count | message | samples |
|---:|---|---|
| 261 | unsupported inline kind: accent | aozora2-adapter:000026_50245-e840465144a2.json<br>aozora2-adapter:000026_894-a581b59ea241.json<br>aozora2-adapter:000027_523-e22ef286b8e7.json<br>aozora2-adapter:000034_55507-30a9658433b4.json<br>aozora2-adapter:000035_2277-b04f7f121e1a.json |
| 140 | unsupported inline kind in source attribution projection: accent | aozora2-adapter:000020_745-bce191ee0ece.json<br>aozora2-adapter:000026_50241-af65ef658680.json<br>aozora2-adapter:000026_55717-3ba0b0630fd5.json<br>aozora2-adapter:000042_1682-4401cf92d836.json<br>aozora2-adapter:000042_2453-83567d45101e.json |
| 52 | unsupported inline kind in visible projection: accent | aozora2-adapter:000026_55732-e162f2f7263b.json<br>aozora2-adapter:000026_55739-075fc7322e7b.json<br>aozora2-adapter:000042_42768-2a6afe487bf8.json<br>aozora2-adapter:000061_377-7004ce5c0169.json<br>aozora2-adapter:000065_393-58fae23a2747.json |
| 11 | unsupported inline kind: yokogumi | aozora2-adapter:000067_2843-cc347e7bd424.json<br>aozora2-adapter:000094_56515-42626c61203a.json<br>aozora2-adapter:000214_1668-baebe662c2db.json<br>aozora2-adapter:000311_4206-efc960698f44.json<br>aozora2-adapter:000311_46240-7d0812519452.json |
| 5 | unsupported inline kind in source attribution projection: yokogumi | aozora2-adapter:000026_50239-f4c2d8bb9024.json<br>aozora2-adapter:000035_312-cb6505044b26.json<br>aozora2-adapter:000035_313-586a212e52ce.json<br>aozora2-adapter:000311_2029-716e98653b00.json<br>aozora2-adapter:001166_43826-0b22f1c3dae6.json |
| 2 | unsupported inline kind in visible projection: yokogumi | aozora2-adapter:000042_1684-e38fc64b1764.json<br>aozora2-adapter:000311_46252-97497fc2f6c8.json |

## Failure Samples

| corpus | path | message |
|---|---|---|
| aozora2-adapter | `000020_745-bce191ee0ece.json` | unsupported inline kind in source attribution projection: accent |
| aozora2-adapter | `000026_50239-f4c2d8bb9024.json` | unsupported inline kind in source attribution projection: yokogumi |
| aozora2-adapter | `000026_50241-af65ef658680.json` | unsupported inline kind in source attribution projection: accent |
| aozora2-adapter | `000026_50245-e840465144a2.json` | unsupported inline kind: accent |
| aozora2-adapter | `000026_55717-3ba0b0630fd5.json` | unsupported inline kind in source attribution projection: accent |
| aozora2-adapter | `000026_55732-e162f2f7263b.json` | unsupported inline kind in visible projection: accent |
| aozora2-adapter | `000026_55739-075fc7322e7b.json` | unsupported inline kind in visible projection: accent |
| aozora2-adapter | `000026_894-a581b59ea241.json` | unsupported inline kind: accent |
| aozora2-adapter | `000027_523-e22ef286b8e7.json` | unsupported inline kind: accent |
| aozora2-adapter | `000034_55507-30a9658433b4.json` | unsupported inline kind: accent |
| aozora2-adapter | `000035_2277-b04f7f121e1a.json` | unsupported inline kind: accent |
| aozora2-adapter | `000035_312-cb6505044b26.json` | unsupported inline kind in source attribution projection: yokogumi |
| aozora2-adapter | `000035_313-586a212e52ce.json` | unsupported inline kind in source attribution projection: yokogumi |
| aozora2-adapter | `000042_1682-4401cf92d836.json` | unsupported inline kind in source attribution projection: accent |
| aozora2-adapter | `000042_1684-e38fc64b1764.json` | unsupported inline kind in visible projection: yokogumi |
| aozora2-adapter | `000042_2345-3b616fdc4dbd.json` | unsupported inline kind: accent |
| aozora2-adapter | `000042_2347-efd9d60e8bbb.json` | unsupported inline kind: accent |
| aozora2-adapter | `000042_2348-607f0940a36f.json` | unsupported inline kind: accent |
| aozora2-adapter | `000042_2453-83567d45101e.json` | unsupported inline kind in source attribution projection: accent |
| aozora2-adapter | `000042_2469-3372cea4b938.json` | unsupported inline kind: accent |
| aozora2-adapter | `000042_2471-5b8c4993357f.json` | unsupported inline kind: accent |
| aozora2-adapter | `000042_2473-16aee6fa0807.json` | unsupported inline kind in source attribution projection: accent |
| aozora2-adapter | `000042_42768-2a6afe487bf8.json` | unsupported inline kind in visible projection: accent |
| aozora2-adapter | `000042_43075-419384138fba.json` | unsupported inline kind in source attribution projection: accent |
| aozora2-adapter | `000042_43082-bffc5116ebfb.json` | unsupported inline kind in source attribution projection: accent |
| aozora2-adapter | `000042_43083-6b85760e219c.json` | unsupported inline kind: accent |
| aozora2-adapter | `000042_43252-8fbc53aead66.json` | unsupported inline kind in source attribution projection: accent |
| aozora2-adapter | `000042_43534-ac52138d9d11.json` | unsupported inline kind in source attribution projection: accent |
| aozora2-adapter | `000061_377-7004ce5c0169.json` | unsupported inline kind in visible projection: accent |
| aozora2-adapter | `000061_510-7c87649c3a47.json` | unsupported inline kind: accent |
| aozora2-adapter | `000065_393-58fae23a2747.json` | unsupported inline kind in visible projection: accent |
| aozora2-adapter | `000065_393-89dcd8df28e2.json` | unsupported inline kind: accent |
| aozora2-adapter | `000065_393-bb9331cf0b9e.json` | unsupported inline kind in visible projection: accent |
| aozora2-adapter | `000067_2843-cc347e7bd424.json` | unsupported inline kind: yokogumi |
| aozora2-adapter | `000075_4250-7e32f80f0150.json` | unsupported inline kind: accent |
| aozora2-adapter | `000075_47960-ad2f47ffff6b.json` | unsupported inline kind in source attribution projection: accent |
| aozora2-adapter | `000075_47964-2dcfbb083de1.json` | unsupported inline kind: accent |
| aozora2-adapter | `000075_47967-46eddbff13c0.json` | unsupported inline kind in source attribution projection: accent |
| aozora2-adapter | `000076_45641-e69c2d20d152.json` | unsupported inline kind: accent |
| aozora2-adapter | `000081_4467-349d5986b896.json` | unsupported inline kind in source attribution projection: accent |
| aozora2-adapter | `000081_45631-62e9b96079cd.json` | unsupported inline kind: accent |
| aozora2-adapter | `000081_47027-a6a7780bf470.json` | unsupported inline kind in visible projection: accent |
| aozora2-adapter | `000093_52936-58dc3d1ed8db.json` | unsupported inline kind in source attribution projection: accent |
| aozora2-adapter | `000093_52937-f4e1e873e23a.json` | unsupported inline kind: accent |
| aozora2-adapter | `000094_1871-1c6066e3f979.json` | unsupported inline kind: accent |
| aozora2-adapter | `000094_2076-9e2654647a23.json` | unsupported inline kind: accent |
| aozora2-adapter | `000094_2523-1126d46db94f.json` | unsupported inline kind: accent |
| aozora2-adapter | `000094_2524-adac834e2d72.json` | unsupported inline kind in source attribution projection: accent |
| aozora2-adapter | `000094_2525-9c05271ce1b8.json` | unsupported inline kind: accent |
| aozora2-adapter | `000094_42338-a56311a7fceb.json` | unsupported inline kind in visible projection: accent |
