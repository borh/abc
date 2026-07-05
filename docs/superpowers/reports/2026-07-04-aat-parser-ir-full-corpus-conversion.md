# AAT Parser-IR Conversion Audit

- generated_unix_seconds: `1783245775`
- mapping: `https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe` `0.2.3`
- mapping_hash: `sha256:9812e1cc0d956143e61cdd9d0a5bccaa3ec301c7984279230107c23919936e8b`
- mapping_schema_hash: `sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4`
- target_parser_ir_schema_hash: `sha256:d98eb9684e7a88f5b62693dd582e28f14834ff85011dc7297e7b41516f7be913`

## Totals

| files_attempted | files_succeeded | files_failed | parser_ir_nodes | divergence_records | divergence_occurrences | elapsed_seconds |
|---:|---:|---:|---:|---:|---:|---:|
| 89169 | 67906 | 21263 | 28890549 | 804793 | 46686120 | 30.261 |

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
| aozora-adapter | 17886 | 996 | 16890 | 996 | 5996 |
| aozora-epub3-adapter | 17844 | 17844 | 0 | 10600649 | 18103739 |
| aozora-rs-adapter | 17894 | 17894 | 0 | 7821839 | 11926731 |
| aozora2-adapter | 17856 | 13483 | 4373 | 2136492 | 4669366 |
| aozora2html-adapter | 17689 | 17689 | 0 | 8330573 | 11980288 |

## Raw Nodes

- nodes_total: `4800218`
- files_with_raw: `21917`
- fatal_direct_failures: `20958`

| corpus | nodes_total | files_with_raw | fatal_direct_failures |
|---|---:|---:|---:|
| aozora-adapter | 4727752 | 16890 | 16890 |
| aozora2-adapter | 69507 | 4768 | 4068 |
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
| AMBIGUITY | 29709249 |
| INVENTION | 11587710 |
| LOSS | 5185181 |
| STRUCTURAL | 172978 |
| UNSUPPORTED | 31002 |

## Compatibility Candidates

| adapter | adapter_version | mapping_version | mapping_hash | files_succeeded | files_failed | rules_emitted | rules_missing | unsupported_occurrences |
|---|---|---|---|---:|---:|---:|---:|---:|
| aozora | aozora-adapter 0.1.0 aozora 0.4.1 | 0.2.3 | `sha256:9812e1cc0d956143e61cdd9d0a5bccaa3ec301c7984279230107c23919936e8b` | 996 | 16890 | 8 | 706 | 0 |
| aozora-epub3 | aozora-epub3-adapter 0.1.0 AozoraEpub3-JDK21-1.3.4-jdk21 | 0.2.3 | `sha256:9812e1cc0d956143e61cdd9d0a5bccaa3ec301c7984279230107c23919936e8b` | 17844 | 0 | 61 | 653 | 13234 |
| aozora-rs | aozora-rs-adapter 0.1.0 2b4e8d1 | 0.2.3 | `sha256:9812e1cc0d956143e61cdd9d0a5bccaa3ec301c7984279230107c23919936e8b` | 17894 | 0 | 25 | 689 | 0 |
| aozora2 | aozora2-adapter 0.1.0 aozora-core-0.7.1 | 0.2.3 | `sha256:9812e1cc0d956143e61cdd9d0a5bccaa3ec301c7984279230107c23919936e8b` | 13483 | 4373 | 267 | 447 | 3538 |
| aozora2html | aozora2html-adapter 0.1.0 gem-3.0.1 | 0.2.3 | `sha256:9812e1cc0d956143e61cdd9d0a5bccaa3ec301c7984279230107c23919936e8b` | 17689 | 0 | 112 | 602 | 14230 |

## Rule Coverage

- rules_total: `714`
- rules_emitted: `308`
- rules_missing: `406`

| rule_id | category | files | occurrences |
|---|---|---:|---:|
| A-01 | AMBIGUITY | 0 | 0 |
| A-02 | AMBIGUITY | 0 | 0 |
| A-03 | AMBIGUITY | 0 | 0 |
| A-04 | AMBIGUITY | 0 | 0 |
| A-05 | AMBIGUITY | 165 | 2567 |
| A-06 | AMBIGUITY | 37 | 111 |
| A-07 | AMBIGUITY | 0 | 0 |
| A-08 | AMBIGUITY | 18 | 20 |
| A-09 | AMBIGUITY | 18 | 20 |
| A-10 | AMBIGUITY | 0 | 0 |
| A-100 | AMBIGUITY | 0 | 0 |
| A-101 | AMBIGUITY | 12 | 27 |
| A-102 | AMBIGUITY | 0 | 0 |
| A-103 | AMBIGUITY | 995 | 3917 |
| A-104 | AMBIGUITY | 10309 | 80489 |
| A-105 | AMBIGUITY | 66541 | 27118071 |
| A-106 | AMBIGUITY | 30460 | 459261 |
| A-107 | AMBIGUITY | 3 | 4 |
| A-108 | AMBIGUITY | 0 | 0 |
| A-109 | AMBIGUITY | 4 | 6 |
| A-11 | AMBIGUITY | 0 | 0 |
| A-110 | AMBIGUITY | 7 | 10 |
| A-111 | AMBIGUITY | 227 | 1491 |
| A-112 | AMBIGUITY | 0 | 0 |
| A-113 | AMBIGUITY | 0 | 0 |
| A-114 | AMBIGUITY | 0 | 0 |
| A-115 | AMBIGUITY | 0 | 0 |
| A-116 | AMBIGUITY | 0 | 0 |
| A-117 | AMBIGUITY | 2 | 2 |
| A-118 | AMBIGUITY | 5 | 78 |
| A-119 | AMBIGUITY | 14 | 220 |
| A-12 | AMBIGUITY | 0 | 0 |
| A-120 | AMBIGUITY | 3 | 13 |
| A-121 | AMBIGUITY | 0 | 0 |
| A-122 | AMBIGUITY | 3472 | 29508 |
| A-123 | AMBIGUITY | 10192 | 105927 |
| A-124 | AMBIGUITY | 5 | 5 |
| A-125 | AMBIGUITY | 67906 | 67906 |
| A-126 | AMBIGUITY | 0 | 0 |
| A-13 | AMBIGUITY | 0 | 0 |
| A-14 | AMBIGUITY | 0 | 0 |
| A-15 | AMBIGUITY | 0 | 0 |
| A-16 | AMBIGUITY | 0 | 0 |
| A-17 | AMBIGUITY | 0 | 0 |
| A-18 | AMBIGUITY | 0 | 0 |
| A-19 | AMBIGUITY | 0 | 0 |
| A-20 | AMBIGUITY | 0 | 0 |
| A-21 | AMBIGUITY | 7 | 18 |
| A-22 | AMBIGUITY | 1 | 1 |
| A-23 | AMBIGUITY | 138 | 559 |
| A-24 | AMBIGUITY | 1 | 1 |
| A-25 | AMBIGUITY | 22 | 58 |
| A-26 | AMBIGUITY | 3 | 3 |
| A-27 | AMBIGUITY | 102 | 256 |
| A-28 | AMBIGUITY | 565 | 2190 |
| A-29 | AMBIGUITY | 13495 | 903082 |
| A-30 | AMBIGUITY | 5025 | 77844 |
| A-31 | AMBIGUITY | 2 | 2 |
| A-32 | AMBIGUITY | 0 | 0 |
| A-33 | AMBIGUITY | 0 | 0 |
| A-34 | AMBIGUITY | 87 | 578 |
| A-35 | AMBIGUITY | 0 | 0 |
| A-36 | AMBIGUITY | 11 | 20 |
| A-37 | AMBIGUITY | 6619 | 71403 |
| A-38 | AMBIGUITY | 6622 | 71423 |
| A-39 | AMBIGUITY | 0 | 0 |
| A-40 | AMBIGUITY | 0 | 0 |
| A-41 | AMBIGUITY | 0 | 0 |
| A-42 | AMBIGUITY | 0 | 0 |
| A-43 | AMBIGUITY | 0 | 0 |
| A-44 | AMBIGUITY | 0 | 0 |
| A-45 | AMBIGUITY | 0 | 0 |
| A-46 | AMBIGUITY | 0 | 0 |
| A-47 | AMBIGUITY | 0 | 0 |
| A-48 | AMBIGUITY | 0 | 0 |
| A-49 | AMBIGUITY | 0 | 0 |
| A-50 | AMBIGUITY | 0 | 0 |
| A-51 | AMBIGUITY | 0 | 0 |
| A-52 | AMBIGUITY | 0 | 0 |
| A-53 | AMBIGUITY | 0 | 0 |
| A-54 | AMBIGUITY | 0 | 0 |
| A-55 | AMBIGUITY | 1 | 1 |
| A-56 | AMBIGUITY | 0 | 0 |
| A-57 | AMBIGUITY | 1 | 1 |
| A-58 | AMBIGUITY | 0 | 0 |
| A-59 | AMBIGUITY | 0 | 0 |
| A-60 | AMBIGUITY | 0 | 0 |
| A-61 | AMBIGUITY | 0 | 0 |
| A-62 | AMBIGUITY | 0 | 0 |
| A-63 | AMBIGUITY | 1 | 8 |
| A-64 | AMBIGUITY | 0 | 0 |
| A-65 | AMBIGUITY | 1 | 2 |
| A-66 | AMBIGUITY | 1 | 6 |
| A-67 | AMBIGUITY | 2 | 3 |
| A-68 | AMBIGUITY | 0 | 0 |
| A-69 | AMBIGUITY | 0 | 0 |
| A-70 | AMBIGUITY | 0 | 0 |
| A-71 | AMBIGUITY | 0 | 0 |
| A-72 | AMBIGUITY | 1 | 1 |
| A-73 | AMBIGUITY | 1 | 2 |
| A-74 | AMBIGUITY | 1 | 2 |
| A-75 | AMBIGUITY | 0 | 0 |
| A-76 | AMBIGUITY | 1 | 1 |
| A-77 | AMBIGUITY | 1 | 2 |
| A-78 | AMBIGUITY | 0 | 0 |
| A-79 | AMBIGUITY | 1 | 1 |
| A-80 | AMBIGUITY | 1 | 1 |
| A-81 | AMBIGUITY | 1 | 2 |
| A-82 | AMBIGUITY | 2 | 15 |
| A-83 | AMBIGUITY | 4 | 15 |
| A-84 | AMBIGUITY | 2 | 5 |
| A-85 | AMBIGUITY | 1 | 1 |
| A-86 | AMBIGUITY | 5 | 11 |
| A-87 | AMBIGUITY | 6 | 10 |
| A-88 | AMBIGUITY | 13 | 25 |
| A-89 | AMBIGUITY | 26 | 47 |
| A-90 | AMBIGUITY | 0 | 0 |
| A-91 | AMBIGUITY | 31 | 94 |
| A-92 | AMBIGUITY | 138 | 547 |
| A-93 | AMBIGUITY | 299 | 1306 |
| A-94 | AMBIGUITY | 882 | 4517 |
| A-95 | AMBIGUITY | 325 | 1444 |
| A-96 | AMBIGUITY | 2107 | 686303 |
| A-97 | AMBIGUITY | 1026 | 17785 |
| A-98 | AMBIGUITY | 0 | 0 |
| A-99 | AMBIGUITY | 0 | 0 |
| I-01 | INVENTION | 67906 | 67906 |
| I-02 | INVENTION | 50244 | 10625277 |
| I-03 | INVENTION | 67906 | 67906 |
| I-04 | INVENTION | 67906 | 67906 |
| I-05 | INVENTION | 16566 | 265138 |
| I-06 | INVENTION | 16566 | 265138 |
| I-07 | INVENTION | 67906 | 67906 |
| I-08 | INVENTION | 0 | 0 |
| I-09 | INVENTION | 1 | 47 |
| I-10 | INVENTION | 0 | 0 |
| I-11 | INVENTION | 0 | 0 |
| I-12 | INVENTION | 0 | 0 |
| I-13 | INVENTION | 1 | 1 |
| I-14 | INVENTION | 31 | 561 |
| I-15 | INVENTION | 565 | 2190 |
| I-16 | INVENTION | 0 | 0 |
| I-17 | INVENTION | 15 | 15 |
| I-18 | INVENTION | 0 | 0 |
| I-19 | INVENTION | 0 | 0 |
| I-20 | INVENTION | 32 | 55 |
| I-21 | INVENTION | 325 | 1444 |
| I-22 | INVENTION | 0 | 0 |
| I-23 | INVENTION | 566 | 3945 |
| I-24 | INVENTION | 10309 | 80489 |
| I-25 | INVENTION | 0 | 0 |
| I-26 | INVENTION | 7 | 10 |
| I-27 | INVENTION | 6671 | 71776 |
| L-01 | LOSS | 10756 | 84134 |
| L-02 | LOSS | 0 | 0 |
| L-03 | LOSS | 1 | 10 |
| L-04 | LOSS | 5 | 20 |
| L-05 | LOSS | 1 | 47 |
| L-06 | LOSS | 1 | 47 |
| L-07 | LOSS | 1 | 47 |
| L-08 | LOSS | 0 | 0 |
| L-09 | LOSS | 20 | 76 |
| L-10 | LOSS | 0 | 0 |
| L-100 | LOSS | 0 | 0 |
| L-101 | LOSS | 0 | 0 |
| L-102 | LOSS | 0 | 0 |
| L-103 | LOSS | 0 | 0 |
| L-104 | LOSS | 0 | 0 |
| L-105 | LOSS | 0 | 0 |
| L-106 | LOSS | 0 | 0 |
| L-107 | LOSS | 0 | 0 |
| L-108 | LOSS | 0 | 0 |
| L-109 | LOSS | 0 | 0 |
| L-11 | LOSS | 1 | 1 |
| L-110 | LOSS | 0 | 0 |
| L-111 | LOSS | 0 | 0 |
| L-112 | LOSS | 0 | 0 |
| L-113 | LOSS | 0 | 0 |
| L-114 | LOSS | 0 | 0 |
| L-115 | LOSS | 0 | 0 |
| L-116 | LOSS | 0 | 0 |
| L-117 | LOSS | 0 | 0 |
| L-118 | LOSS | 0 | 0 |
| L-119 | LOSS | 0 | 0 |
| L-12 | LOSS | 1 | 1 |
| L-120 | LOSS | 0 | 0 |
| L-121 | LOSS | 0 | 0 |
| L-122 | LOSS | 0 | 0 |
| L-123 | LOSS | 0 | 0 |
| L-124 | LOSS | 0 | 0 |
| L-125 | LOSS | 0 | 0 |
| L-126 | LOSS | 0 | 0 |
| L-127 | LOSS | 0 | 0 |
| L-128 | LOSS | 0 | 0 |
| L-129 | LOSS | 0 | 0 |
| L-13 | LOSS | 1 | 1 |
| L-130 | LOSS | 0 | 0 |
| L-131 | LOSS | 0 | 0 |
| L-132 | LOSS | 0 | 0 |
| L-133 | LOSS | 0 | 0 |
| L-134 | LOSS | 0 | 0 |
| L-135 | LOSS | 0 | 0 |
| L-136 | LOSS | 0 | 0 |
| L-137 | LOSS | 0 | 0 |
| L-138 | LOSS | 0 | 0 |
| L-139 | LOSS | 0 | 0 |
| L-14 | LOSS | 18 | 20 |
| L-140 | LOSS | 0 | 0 |
| L-141 | LOSS | 0 | 0 |
| L-142 | LOSS | 0 | 0 |
| L-143 | LOSS | 0 | 0 |
| L-144 | LOSS | 0 | 0 |
| L-145 | LOSS | 0 | 0 |
| L-146 | LOSS | 0 | 0 |
| L-147 | LOSS | 0 | 0 |
| L-148 | LOSS | 0 | 0 |
| L-149 | LOSS | 0 | 0 |
| L-15 | LOSS | 0 | 0 |
| L-150 | LOSS | 0 | 0 |
| L-151 | LOSS | 0 | 0 |
| L-152 | LOSS | 0 | 0 |
| L-153 | LOSS | 0 | 0 |
| L-154 | LOSS | 0 | 0 |
| L-155 | LOSS | 0 | 0 |
| L-156 | LOSS | 0 | 0 |
| L-157 | LOSS | 0 | 0 |
| L-158 | LOSS | 0 | 0 |
| L-159 | LOSS | 0 | 0 |
| L-16 | LOSS | 0 | 0 |
| L-160 | LOSS | 0 | 0 |
| L-161 | LOSS | 0 | 0 |
| L-162 | LOSS | 0 | 0 |
| L-163 | LOSS | 0 | 0 |
| L-164 | LOSS | 0 | 0 |
| L-165 | LOSS | 0 | 0 |
| L-166 | LOSS | 0 | 0 |
| L-167 | LOSS | 2 | 2 |
| L-168 | LOSS | 0 | 0 |
| L-169 | LOSS | 0 | 0 |
| L-17 | LOSS | 0 | 0 |
| L-170 | LOSS | 6 | 25 |
| L-171 | LOSS | 0 | 0 |
| L-172 | LOSS | 1 | 1 |
| L-173 | LOSS | 37 | 100 |
| L-174 | LOSS | 2 | 2 |
| L-175 | LOSS | 141 | 780 |
| L-176 | LOSS | 5 | 15 |
| L-177 | LOSS | 434 | 4103 |
| L-178 | LOSS | 0 | 0 |
| L-179 | LOSS | 31 | 561 |
| L-18 | LOSS | 0 | 0 |
| L-180 | LOSS | 25 | 555 |
| L-181 | LOSS | 25 | 555 |
| L-182 | LOSS | 443 | 1760 |
| L-183 | LOSS | 3840 | 131469 |
| L-184 | LOSS | 0 | 0 |
| L-185 | LOSS | 0 | 0 |
| L-186 | LOSS | 0 | 0 |
| L-187 | LOSS | 0 | 0 |
| L-188 | LOSS | 0 | 0 |
| L-189 | LOSS | 6 | 9 |
| L-19 | LOSS | 0 | 0 |
| L-190 | LOSS | 11 | 20 |
| L-191 | LOSS | 1 | 1 |
| L-192 | LOSS | 22 | 117 |
| L-193 | LOSS | 22 | 117 |
| L-194 | LOSS | 518 | 5987 |
| L-195 | LOSS | 518 | 5987 |
| L-196 | LOSS | 47 | 103 |
| L-197 | LOSS | 58 | 481 |
| L-198 | LOSS | 6619 | 71403 |
| L-199 | LOSS | 0 | 0 |
| L-20 | LOSS | 0 | 0 |
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
| L-21 | LOSS | 0 | 0 |
| L-210 | LOSS | 0 | 0 |
| L-211 | LOSS | 0 | 0 |
| L-212 | LOSS | 0 | 0 |
| L-213 | LOSS | 0 | 0 |
| L-214 | LOSS | 0 | 0 |
| L-215 | LOSS | 0 | 0 |
| L-216 | LOSS | 0 | 0 |
| L-217 | LOSS | 0 | 0 |
| L-218 | LOSS | 0 | 0 |
| L-219 | LOSS | 0 | 0 |
| L-22 | LOSS | 0 | 0 |
| L-220 | LOSS | 0 | 0 |
| L-221 | LOSS | 0 | 0 |
| L-222 | LOSS | 0 | 0 |
| L-223 | LOSS | 0 | 0 |
| L-224 | LOSS | 0 | 0 |
| L-225 | LOSS | 0 | 0 |
| L-226 | LOSS | 0 | 0 |
| L-227 | LOSS | 0 | 0 |
| L-228 | LOSS | 0 | 0 |
| L-229 | LOSS | 0 | 0 |
| L-23 | LOSS | 0 | 0 |
| L-230 | LOSS | 0 | 0 |
| L-231 | LOSS | 0 | 0 |
| L-232 | LOSS | 0 | 0 |
| L-233 | LOSS | 0 | 0 |
| L-234 | LOSS | 0 | 0 |
| L-235 | LOSS | 0 | 0 |
| L-236 | LOSS | 0 | 0 |
| L-237 | LOSS | 0 | 0 |
| L-238 | LOSS | 0 | 0 |
| L-239 | LOSS | 0 | 0 |
| L-24 | LOSS | 0 | 0 |
| L-240 | LOSS | 0 | 0 |
| L-241 | LOSS | 0 | 0 |
| L-242 | LOSS | 0 | 0 |
| L-243 | LOSS | 0 | 0 |
| L-244 | LOSS | 0 | 0 |
| L-245 | LOSS | 0 | 0 |
| L-246 | LOSS | 0 | 0 |
| L-247 | LOSS | 0 | 0 |
| L-248 | LOSS | 0 | 0 |
| L-249 | LOSS | 0 | 0 |
| L-25 | LOSS | 0 | 0 |
| L-250 | LOSS | 0 | 0 |
| L-251 | LOSS | 0 | 0 |
| L-252 | LOSS | 0 | 0 |
| L-253 | LOSS | 1 | 1 |
| L-254 | LOSS | 1 | 8 |
| L-255 | LOSS | 1 | 7 |
| L-256 | LOSS | 1 | 14 |
| L-257 | LOSS | 1 | 9 |
| L-258 | LOSS | 1 | 1 |
| L-259 | LOSS | 1 | 4 |
| L-26 | LOSS | 0 | 0 |
| L-260 | LOSS | 1 | 4 |
| L-261 | LOSS | 1 | 1 |
| L-262 | LOSS | 1 | 4 |
| L-263 | LOSS | 1 | 1 |
| L-264 | LOSS | 1 | 1 |
| L-265 | LOSS | 0 | 0 |
| L-266 | LOSS | 1 | 6 |
| L-267 | LOSS | 1 | 2 |
| L-268 | LOSS | 1 | 2 |
| L-269 | LOSS | 1 | 2 |
| L-27 | LOSS | 0 | 0 |
| L-270 | LOSS | 1 | 4 |
| L-271 | LOSS | 1 | 1 |
| L-272 | LOSS | 1 | 1 |
| L-273 | LOSS | 1 | 2 |
| L-274 | LOSS | 1 | 5 |
| L-275 | LOSS | 1 | 7 |
| L-276 | LOSS | 1 | 2 |
| L-277 | LOSS | 0 | 0 |
| L-278 | LOSS | 1 | 2 |
| L-279 | LOSS | 1 | 2 |
| L-28 | LOSS | 0 | 0 |
| L-280 | LOSS | 1 | 3 |
| L-281 | LOSS | 2 | 6 |
| L-282 | LOSS | 2 | 9 |
| L-283 | LOSS | 0 | 0 |
| L-284 | LOSS | 2 | 5 |
| L-285 | LOSS | 0 | 0 |
| L-286 | LOSS | 2 | 10 |
| L-287 | LOSS | 0 | 0 |
| L-288 | LOSS | 2 | 7 |
| L-289 | LOSS | 0 | 0 |
| L-29 | LOSS | 0 | 0 |
| L-290 | LOSS | 2 | 7 |
| L-291 | LOSS | 2 | 14 |
| L-292 | LOSS | 2 | 12 |
| L-293 | LOSS | 1 | 12 |
| L-294 | LOSS | 2 | 6 |
| L-295 | LOSS | 2 | 4 |
| L-296 | LOSS | 2 | 8 |
| L-297 | LOSS | 1 | 1 |
| L-298 | LOSS | 2 | 5 |
| L-299 | LOSS | 0 | 0 |
| L-30 | LOSS | 0 | 0 |
| L-300 | LOSS | 2 | 5 |
| L-301 | LOSS | 1 | 3 |
| L-302 | LOSS | 2 | 5 |
| L-303 | LOSS | 1 | 3 |
| L-304 | LOSS | 0 | 0 |
| L-305 | LOSS | 2 | 8 |
| L-306 | LOSS | 1 | 10 |
| L-307 | LOSS | 0 | 0 |
| L-308 | LOSS | 0 | 0 |
| L-309 | LOSS | 2 | 5 |
| L-31 | LOSS | 0 | 0 |
| L-310 | LOSS | 2 | 16 |
| L-311 | LOSS | 2 | 5 |
| L-312 | LOSS | 2 | 21 |
| L-313 | LOSS | 2 | 3 |
| L-314 | LOSS | 2 | 7 |
| L-315 | LOSS | 2 | 9 |
| L-316 | LOSS | 2 | 4 |
| L-317 | LOSS | 3 | 9 |
| L-318 | LOSS | 3 | 6 |
| L-319 | LOSS | 0 | 0 |
| L-32 | LOSS | 0 | 0 |
| L-320 | LOSS | 0 | 0 |
| L-321 | LOSS | 2 | 8 |
| L-322 | LOSS | 0 | 0 |
| L-323 | LOSS | 1 | 14 |
| L-324 | LOSS | 2 | 7 |
| L-325 | LOSS | 1 | 4 |
| L-326 | LOSS | 3 | 12 |
| L-327 | LOSS | 0 | 0 |
| L-328 | LOSS | 3 | 6 |
| L-329 | LOSS | 2 | 3 |
| L-33 | LOSS | 0 | 0 |
| L-330 | LOSS | 3 | 7 |
| L-331 | LOSS | 0 | 0 |
| L-332 | LOSS | 0 | 0 |
| L-333 | LOSS | 2 | 19 |
| L-334 | LOSS | 3 | 31 |
| L-335 | LOSS | 3 | 24 |
| L-336 | LOSS | 0 | 0 |
| L-337 | LOSS | 2 | 34 |
| L-338 | LOSS | 0 | 0 |
| L-339 | LOSS | 0 | 0 |
| L-34 | LOSS | 0 | 0 |
| L-340 | LOSS | 2 | 20 |
| L-341 | LOSS | 3 | 41 |
| L-342 | LOSS | 2 | 35 |
| L-343 | LOSS | 0 | 0 |
| L-344 | LOSS | 0 | 0 |
| L-345 | LOSS | 4 | 36 |
| L-346 | LOSS | 1 | 2 |
| L-347 | LOSS | 4 | 36 |
| L-348 | LOSS | 1 | 2 |
| L-349 | LOSS | 5 | 124 |
| L-35 | LOSS | 0 | 0 |
| L-350 | LOSS | 2 | 3 |
| L-351 | LOSS | 5 | 46 |
| L-352 | LOSS | 0 | 0 |
| L-353 | LOSS | 1 | 2 |
| L-354 | LOSS | 5 | 28 |
| L-355 | LOSS | 4 | 5 |
| L-356 | LOSS | 5 | 41 |
| L-357 | LOSS | 0 | 0 |
| L-358 | LOSS | 1 | 2 |
| L-359 | LOSS | 6 | 208 |
| L-36 | LOSS | 0 | 0 |
| L-360 | LOSS | 0 | 0 |
| L-361 | LOSS | 2 | 3 |
| L-362 | LOSS | 8 | 296 |
| L-363 | LOSS | 2 | 3 |
| L-364 | LOSS | 10 | 288 |
| L-365 | LOSS | 0 | 0 |
| L-366 | LOSS | 3 | 4 |
| L-367 | LOSS | 18 | 397 |
| L-368 | LOSS | 2 | 3 |
| L-369 | LOSS | 24 | 510 |
| L-37 | LOSS | 0 | 0 |
| L-370 | LOSS | 0 | 0 |
| L-371 | LOSS | 2 | 3 |
| L-372 | LOSS | 27 | 332 |
| L-373 | LOSS | 0 | 0 |
| L-374 | LOSS | 3 | 9 |
| L-375 | LOSS | 38 | 852 |
| L-376 | LOSS | 0 | 0 |
| L-377 | LOSS | 5 | 7 |
| L-378 | LOSS | 53 | 722 |
| L-379 | LOSS | 0 | 0 |
| L-38 | LOSS | 0 | 0 |
| L-380 | LOSS | 9 | 12 |
| L-381 | LOSS | 85 | 1246 |
| L-382 | LOSS | 0 | 0 |
| L-383 | LOSS | 2 | 7 |
| L-384 | LOSS | 16 | 33 |
| L-385 | LOSS | 149 | 2856 |
| L-386 | LOSS | 6 | 15 |
| L-387 | LOSS | 73 | 278 |
| L-388 | LOSS | 409 | 19330 |
| L-389 | LOSS | 46 | 90 |
| L-39 | LOSS | 0 | 0 |
| L-390 | LOSS | 13 | 29 |
| L-391 | LOSS | 32 | 55 |
| L-392 | LOSS | 31 | 54 |
| L-393 | LOSS | 31 | 54 |
| L-394 | LOSS | 22 | 72 |
| L-395 | LOSS | 585 | 3086 |
| L-396 | LOSS | 1734 | 318875 |
| L-397 | LOSS | 4270 | 314518 |
| L-398 | LOSS | 0 | 0 |
| L-399 | LOSS | 0 | 0 |
| L-40 | LOSS | 0 | 0 |
| L-400 | LOSS | 0 | 0 |
| L-401 | LOSS | 1 | 1 |
| L-402 | LOSS | 0 | 0 |
| L-403 | LOSS | 133 | 678 |
| L-404 | LOSS | 566 | 3945 |
| L-405 | LOSS | 501 | 3785 |
| L-406 | LOSS | 501 | 3785 |
| L-407 | LOSS | 3634 | 19357 |
| L-408 | LOSS | 24093 | 3996557 |
| L-409 | LOSS | 0 | 0 |
| L-41 | LOSS | 0 | 0 |
| L-410 | LOSS | 0 | 0 |
| L-411 | LOSS | 0 | 0 |
| L-412 | LOSS | 0 | 0 |
| L-413 | LOSS | 0 | 0 |
| L-414 | LOSS | 0 | 0 |
| L-415 | LOSS | 0 | 0 |
| L-416 | LOSS | 0 | 0 |
| L-417 | LOSS | 0 | 0 |
| L-418 | LOSS | 0 | 0 |
| L-419 | LOSS | 0 | 0 |
| L-42 | LOSS | 0 | 0 |
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
| L-43 | LOSS | 0 | 0 |
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
| L-44 | LOSS | 0 | 0 |
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
| L-45 | LOSS | 0 | 0 |
| L-450 | LOSS | 0 | 0 |
| L-451 | LOSS | 0 | 0 |
| L-452 | LOSS | 0 | 0 |
| L-453 | LOSS | 0 | 0 |
| L-454 | LOSS | 0 | 0 |
| L-455 | LOSS | 0 | 0 |
| L-456 | LOSS | 0 | 0 |
| L-457 | LOSS | 0 | 0 |
| L-458 | LOSS | 0 | 0 |
| L-459 | LOSS | 0 | 0 |
| L-46 | LOSS | 0 | 0 |
| L-460 | LOSS | 0 | 0 |
| L-461 | LOSS | 0 | 0 |
| L-462 | LOSS | 1 | 1 |
| L-463 | LOSS | 0 | 0 |
| L-464 | LOSS | 1 | 1 |
| L-465 | LOSS | 1 | 1 |
| L-466 | LOSS | 0 | 0 |
| L-467 | LOSS | 0 | 0 |
| L-468 | LOSS | 2 | 3 |
| L-469 | LOSS | 0 | 0 |
| L-47 | LOSS | 0 | 0 |
| L-470 | LOSS | 1 | 2 |
| L-471 | LOSS | 1 | 1 |
| L-472 | LOSS | 1 | 3 |
| L-473 | LOSS | 0 | 0 |
| L-474 | LOSS | 2 | 4 |
| L-475 | LOSS | 2 | 5 |
| L-476 | LOSS | 2 | 2 |
| L-477 | LOSS | 4 | 44 |
| L-478 | LOSS | 3 | 10 |
| L-479 | LOSS | 11 | 25 |
| L-48 | LOSS | 0 | 0 |
| L-480 | LOSS | 14 | 2976 |
| L-481 | LOSS | 5 | 13 |
| L-482 | LOSS | 38 | 354 |
| L-483 | LOSS | 41 | 25594 |
| L-484 | LOSS | 0 | 0 |
| L-485 | LOSS | 0 | 0 |
| L-486 | LOSS | 1 | 9 |
| L-487 | LOSS | 0 | 0 |
| L-488 | LOSS | 0 | 0 |
| L-489 | LOSS | 3 | 13 |
| L-49 | LOSS | 0 | 0 |
| L-490 | LOSS | 0 | 0 |
| L-491 | LOSS | 4 | 27 |
| L-492 | LOSS | 4 | 27 |
| L-493 | LOSS | 150 | 2867 |
| L-494 | LOSS | 150 | 2867 |
| L-495 | LOSS | 64 | 373 |
| L-496 | LOSS | 15 | 116 |
| L-497 | LOSS | 3472 | 29508 |
| L-498 | LOSS | 17894 | 17894 |
| L-499 | LOSS | 67906 | 67906 |
| L-50 | LOSS | 0 | 0 |
| L-500 | LOSS | 32710 | 32710 |
| L-51 | LOSS | 0 | 0 |
| L-52 | LOSS | 0 | 0 |
| L-53 | LOSS | 0 | 0 |
| L-54 | LOSS | 0 | 0 |
| L-55 | LOSS | 0 | 0 |
| L-56 | LOSS | 0 | 0 |
| L-57 | LOSS | 0 | 0 |
| L-58 | LOSS | 0 | 0 |
| L-59 | LOSS | 0 | 0 |
| L-60 | LOSS | 0 | 0 |
| L-61 | LOSS | 0 | 0 |
| L-62 | LOSS | 0 | 0 |
| L-63 | LOSS | 0 | 0 |
| L-64 | LOSS | 0 | 0 |
| L-65 | LOSS | 0 | 0 |
| L-66 | LOSS | 0 | 0 |
| L-67 | LOSS | 0 | 0 |
| L-68 | LOSS | 0 | 0 |
| L-69 | LOSS | 0 | 0 |
| L-70 | LOSS | 0 | 0 |
| L-71 | LOSS | 0 | 0 |
| L-72 | LOSS | 0 | 0 |
| L-73 | LOSS | 0 | 0 |
| L-74 | LOSS | 0 | 0 |
| L-75 | LOSS | 0 | 0 |
| L-76 | LOSS | 0 | 0 |
| L-77 | LOSS | 0 | 0 |
| L-78 | LOSS | 0 | 0 |
| L-79 | LOSS | 0 | 0 |
| L-80 | LOSS | 0 | 0 |
| L-81 | LOSS | 0 | 0 |
| L-82 | LOSS | 0 | 0 |
| L-83 | LOSS | 0 | 0 |
| L-84 | LOSS | 0 | 0 |
| L-85 | LOSS | 0 | 0 |
| L-86 | LOSS | 0 | 0 |
| L-87 | LOSS | 0 | 0 |
| L-88 | LOSS | 0 | 0 |
| L-89 | LOSS | 0 | 0 |
| L-90 | LOSS | 0 | 0 |
| L-91 | LOSS | 0 | 0 |
| L-92 | LOSS | 0 | 0 |
| L-93 | LOSS | 0 | 0 |
| L-94 | LOSS | 0 | 0 |
| L-95 | LOSS | 0 | 0 |
| L-96 | LOSS | 0 | 0 |
| L-97 | LOSS | 0 | 0 |
| L-98 | LOSS | 0 | 0 |
| L-99 | LOSS | 0 | 0 |
| S-01 | STRUCTURAL | 2 | 2 |
| S-02 | STRUCTURAL | 18 | 20 |
| S-03 | STRUCTURAL | 6619 | 71403 |
| S-04 | STRUCTURAL | 15 | 15 |
| S-05 | STRUCTURAL | 76 | 148 |
| S-06 | STRUCTURAL | 3 | 4 |
| S-07 | STRUCTURAL | 3472 | 29508 |
| S-08 | STRUCTURAL | 6671 | 71776 |
| S-09 | STRUCTURAL | 45 | 82 |
| S-10 | STRUCTURAL | 13 | 20 |
| U-01 | UNSUPPORTED | 12 | 93 |
| U-02 | UNSUPPORTED | 0 | 0 |
| U-03 | UNSUPPORTED | 3 | 9 |
| U-04 | UNSUPPORTED | 0 | 0 |
| U-05 | UNSUPPORTED | 2 | 3 |
| U-06 | UNSUPPORTED | 1 | 1 |
| U-07 | UNSUPPORTED | 1 | 2 |
| U-08 | UNSUPPORTED | 2 | 2 |
| U-09 | UNSUPPORTED | 0 | 0 |
| U-10 | UNSUPPORTED | 9 | 56 |
| U-11 | UNSUPPORTED | 426 | 6000 |
| U-12 | UNSUPPORTED | 30 | 165 |
| U-13 | UNSUPPORTED | 0 | 0 |
| U-14 | UNSUPPORTED | 172 | 2804 |
| U-15 | UNSUPPORTED | 87 | 574 |
| U-16 | UNSUPPORTED | 0 | 0 |
| U-17 | UNSUPPORTED | 1 | 1 |
| U-18 | UNSUPPORTED | 76 | 148 |
| U-19 | UNSUPPORTED | 3 | 4 |
| U-20 | UNSUPPORTED | 8 | 61 |
| U-21 | UNSUPPORTED | 0 | 0 |
| U-22 | UNSUPPORTED | 1 | 1 |
| U-23 | UNSUPPORTED | 0 | 0 |
| U-24 | UNSUPPORTED | 1 | 1 |
| U-25 | UNSUPPORTED | 1 | 4 |
| U-26 | UNSUPPORTED | 7 | 13 |
| U-27 | UNSUPPORTED | 1 | 1 |
| U-28 | UNSUPPORTED | 10 | 28 |
| U-29 | UNSUPPORTED | 29 | 106 |
| U-30 | UNSUPPORTED | 0 | 0 |
| U-31 | UNSUPPORTED | 0 | 0 |
| U-32 | UNSUPPORTED | 43 | 270 |
| U-33 | UNSUPPORTED | 0 | 0 |
| U-34 | UNSUPPORTED | 49 | 214 |
| U-35 | UNSUPPORTED | 0 | 0 |
| U-36 | UNSUPPORTED | 0 | 0 |
| U-37 | UNSUPPORTED | 993 | 7869 |
| U-38 | UNSUPPORTED | 48 | 161 |
| U-39 | UNSUPPORTED | 0 | 0 |
| U-40 | UNSUPPORTED | 635 | 10852 |
| U-41 | UNSUPPORTED | 227 | 1457 |
| U-42 | UNSUPPORTED | 0 | 0 |
| U-43 | UNSUPPORTED | 0 | 0 |
| U-44 | UNSUPPORTED | 0 | 0 |
| U-45 | UNSUPPORTED | 0 | 0 |
| U-46 | UNSUPPORTED | 0 | 0 |
| U-47 | UNSUPPORTED | 0 | 0 |
| U-48 | UNSUPPORTED | 0 | 0 |
| U-49 | UNSUPPORTED | 0 | 0 |
| U-50 | UNSUPPORTED | 45 | 82 |
| U-51 | UNSUPPORTED | 13 | 20 |

## Top Errors

| count | message | samples |
|---:|---|---|
| 20958 | unsupported inline kind: raw | aozora2-adapter:000005_53194-ebb0cbaf64b3.json<br>aozora2-adapter:000006_1869-62320f0f4474.json<br>aozora2-adapter:000006_4627-ded0d23b15b9.json<br>aozora2-adapter:000008_1083-bf767c36e951.json<br>aozora2-adapter:000008_47357-6b9c19f6420d.json |
| 138 | unsupported inline kind: accent | aozora2-adapter:000026_50245-e840465144a2.json<br>aozora2-adapter:000027_523-e22ef286b8e7.json<br>aozora2-adapter:000035_2277-b04f7f121e1a.json<br>aozora2-adapter:000042_2469-3372cea4b938.json<br>aozora2-adapter:000061_510-7c87649c3a47.json |
| 129 | unsupported inline kind in source attribution projection: accent | aozora2-adapter:000020_745-bce191ee0ece.json<br>aozora2-adapter:000026_50241-af65ef658680.json<br>aozora2-adapter:000026_55717-3ba0b0630fd5.json<br>aozora2-adapter:000042_1682-4401cf92d836.json<br>aozora2-adapter:000042_2453-83567d45101e.json |
| 32 | unsupported inline kind in visible projection: accent | aozora2-adapter:000026_55732-e162f2f7263b.json<br>aozora2-adapter:000026_55739-075fc7322e7b.json<br>aozora2-adapter:000042_42768-2a6afe487bf8.json<br>aozora2-adapter:000065_393-58fae23a2747.json<br>aozora2-adapter:000065_393-bb9331cf0b9e.json |
| 5 | unsupported inline kind in source attribution projection: yokogumi | aozora2-adapter:000026_50239-f4c2d8bb9024.json<br>aozora2-adapter:000035_312-cb6505044b26.json<br>aozora2-adapter:000035_313-586a212e52ce.json<br>aozora2-adapter:000311_2029-716e98653b00.json<br>aozora2-adapter:001166_43826-0b22f1c3dae6.json |
| 1 | unsupported inline kind: yokogumi | aozora2-adapter:001569_57279-a4f95b2004e3.json |

## Failure Samples

| corpus | path | message |
|---|---|---|
| aozora2-adapter | `000005_53194-ebb0cbaf64b3.json` | unsupported inline kind: raw |
| aozora2-adapter | `000006_1869-62320f0f4474.json` | unsupported inline kind: raw |
| aozora2-adapter | `000006_4627-ded0d23b15b9.json` | unsupported inline kind: raw |
| aozora2-adapter | `000008_1083-bf767c36e951.json` | unsupported inline kind: raw |
| aozora2-adapter | `000008_47357-6b9c19f6420d.json` | unsupported inline kind: raw |
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
| aozora2-adapter | `000011_899-bc8b5148d0e0.json` | unsupported inline kind: raw |
| aozora2-adapter | `000012_1092-1c3a5aac467e.json` | unsupported inline kind: raw |
| aozora2-adapter | `000012_24448-32312eb2c779.json` | unsupported inline kind: raw |
| aozora2-adapter | `000012_33200-32660f9eb979.json` | unsupported inline kind: raw |
| aozora2-adapter | `000012_48628-4fde87720d42.json` | unsupported inline kind: raw |
| aozora2-adapter | `000014_12-e0352951f32f.json` | unsupported inline kind: raw |
| aozora2-adapter | `000014_728-af8280acaa59.json` | unsupported inline kind: raw |
| aozora2-adapter | `000019_194-5219b1146dad.json` | unsupported inline kind: raw |
| aozora2-adapter | `000019_42379-2c7b3481e08a.json` | unsupported inline kind: raw |
| aozora2-adapter | `000019_42380-e06a90ced089.json` | unsupported inline kind: raw |
| aozora2-adapter | `000019_42381-e359d7f661b1.json` | unsupported inline kind: raw |
