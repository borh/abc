# AAT Parser-IR Conversion Audit

- generated_unix_seconds: `1783250977`
- mapping: `https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe` `0.2.3`
- mapping_hash: `sha256:13734117384aede0ee484cbda1b44b29c96007a17f238a21788f567f7da8ea06`
- mapping_schema_hash: `sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4`
- target_parser_ir_schema_hash: `sha256:da916a3a92f64d985cb98f9b2ddc7f562e660fd0c3dbe0c902392d3764b0158a`

## Totals

| files_attempted | files_succeeded | files_failed | parser_ir_nodes | divergence_records | divergence_occurrences | elapsed_seconds |
|---:|---:|---:|---:|---:|---:|---:|
| 89169 | 89169 | 0 | 33359611 | 1042024 | 59048388 | 76.956 |

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
| aozora2-adapter | 17856 | 17856 | 0 | 5618304 | 11937470 |
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
| AMBIGUITY | 33414901 |
| INVENTION | 13471065 |
| LOSS | 7094923 |
| STRUCTURAL | 231129 |
| UNSUPPORTED | 4836370 |

## Compatibility Candidates

| adapter | adapter_version | mapping_version | mapping_hash | files_succeeded | files_failed | rules_emitted | rules_missing | unsupported_occurrences |
|---|---|---|---|---:|---:|---:|---:|---:|
| aozora | aozora-adapter 0.1.0 aozora 0.4.1 | 0.2.3 | `sha256:13734117384aede0ee484cbda1b44b29c96007a17f238a21788f567f7da8ea06` | 17886 | 0 | 16 | 698 | 4727752 |
| aozora-epub3 | aozora-epub3-adapter 0.1.0 AozoraEpub3-JDK21-1.3.4-jdk21 | 0.2.3 | `sha256:13734117384aede0ee484cbda1b44b29c96007a17f238a21788f567f7da8ea06` | 17844 | 0 | 61 | 653 | 13234 |
| aozora-rs | aozora-rs-adapter 0.1.0 2b4e8d1 | 0.2.3 | `sha256:13734117384aede0ee484cbda1b44b29c96007a17f238a21788f567f7da8ea06` | 17894 | 0 | 25 | 689 | 0 |
| aozora2 | aozora2-adapter 0.1.0 aozora-core-0.7.1 | 0.2.3 | `sha256:13734117384aede0ee484cbda1b44b29c96007a17f238a21788f567f7da8ea06` | 17856 | 0 | 670 | 44 | 81154 |
| aozora2html | aozora2html-adapter 0.1.0 gem-3.0.1 | 0.2.3 | `sha256:13734117384aede0ee484cbda1b44b29c96007a17f238a21788f567f7da8ea06` | 17689 | 0 | 112 | 602 | 14230 |

## Rule Coverage

- rules_total: `714`
- rules_emitted: `692`
- rules_missing: `22`

| rule_id | category | files | occurrences |
|---|---|---:|---:|
| A-01 | AMBIGUITY | 0 | 0 |
| A-02 | AMBIGUITY | 9 | 59 |
| A-03 | AMBIGUITY | 1 | 4 |
| A-04 | AMBIGUITY | 2 | 7 |
| A-05 | AMBIGUITY | 216 | 2920 |
| A-06 | AMBIGUITY | 47 | 130 |
| A-07 | AMBIGUITY | 1 | 14 |
| A-08 | AMBIGUITY | 19 | 21 |
| A-09 | AMBIGUITY | 19 | 21 |
| A-10 | AMBIGUITY | 87 | 670 |
| A-100 | AMBIGUITY | 7 | 10 |
| A-101 | AMBIGUITY | 85 | 903 |
| A-102 | AMBIGUITY | 1 | 1 |
| A-103 | AMBIGUITY | 2263 | 13590 |
| A-104 | AMBIGUITY | 11700 | 91590 |
| A-105 | AMBIGUITY | 70902 | 28778226 |
| A-106 | AMBIGUITY | 32996 | 514947 |
| A-107 | AMBIGUITY | 3 | 4 |
| A-108 | AMBIGUITY | 1 | 4 |
| A-109 | AMBIGUITY | 11 | 14 |
| A-11 | AMBIGUITY | 3 | 76 |
| A-110 | AMBIGUITY | 15 | 21 |
| A-111 | AMBIGUITY | 347 | 2537 |
| A-112 | AMBIGUITY | 1 | 7 |
| A-113 | AMBIGUITY | 2 | 6 |
| A-114 | AMBIGUITY | 2 | 2 |
| A-115 | AMBIGUITY | 2 | 2 |
| A-116 | AMBIGUITY | 4 | 5 |
| A-117 | AMBIGUITY | 10 | 16 |
| A-118 | AMBIGUITY | 28 | 176 |
| A-119 | AMBIGUITY | 189 | 2819 |
| A-12 | AMBIGUITY | 1 | 1 |
| A-120 | AMBIGUITY | 26 | 249 |
| A-121 | AMBIGUITY | 0 | 0 |
| A-122 | AMBIGUITY | 5348 | 87024 |
| A-123 | AMBIGUITY | 12080 | 163529 |
| A-124 | AMBIGUITY | 7 | 7 |
| A-125 | AMBIGUITY | 89169 | 89169 |
| A-126 | AMBIGUITY | 0 | 0 |
| A-13 | AMBIGUITY | 1 | 2 |
| A-14 | AMBIGUITY | 1 | 2 |
| A-15 | AMBIGUITY | 1 | 2 |
| A-16 | AMBIGUITY | 1 | 2 |
| A-17 | AMBIGUITY | 1 | 1 |
| A-18 | AMBIGUITY | 1 | 1 |
| A-19 | AMBIGUITY | 2 | 3 |
| A-20 | AMBIGUITY | 1 | 1 |
| A-21 | AMBIGUITY | 13 | 37 |
| A-22 | AMBIGUITY | 5 | 27 |
| A-23 | AMBIGUITY | 158 | 633 |
| A-24 | AMBIGUITY | 5 | 27 |
| A-25 | AMBIGUITY | 108 | 2395 |
| A-26 | AMBIGUITY | 36 | 49 |
| A-27 | AMBIGUITY | 361 | 1266 |
| A-28 | AMBIGUITY | 854 | 3416 |
| A-29 | AMBIGUITY | 15602 | 1042752 |
| A-30 | AMBIGUITY | 5501 | 82608 |
| A-31 | AMBIGUITY | 2 | 2 |
| A-32 | AMBIGUITY | 5 | 9 |
| A-33 | AMBIGUITY | 5 | 10 |
| A-34 | AMBIGUITY | 142 | 1007 |
| A-35 | AMBIGUITY | 2 | 5 |
| A-36 | AMBIGUITY | 12 | 28 |
| A-37 | AMBIGUITY | 6634 | 71461 |
| A-38 | AMBIGUITY | 6637 | 71481 |
| A-39 | AMBIGUITY | 257 | 1493 |
| A-40 | AMBIGUITY | 122 | 1016 |
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
| A-52 | AMBIGUITY | 1 | 1 |
| A-53 | AMBIGUITY | 1 | 2 |
| A-54 | AMBIGUITY | 1 | 1 |
| A-55 | AMBIGUITY | 1 | 1 |
| A-56 | AMBIGUITY | 2 | 2 |
| A-57 | AMBIGUITY | 2 | 2 |
| A-58 | AMBIGUITY | 1 | 1 |
| A-59 | AMBIGUITY | 1 | 1 |
| A-60 | AMBIGUITY | 1 | 1 |
| A-61 | AMBIGUITY | 1 | 1 |
| A-62 | AMBIGUITY | 1 | 1 |
| A-63 | AMBIGUITY | 1 | 8 |
| A-64 | AMBIGUITY | 1 | 1 |
| A-65 | AMBIGUITY | 2 | 3 |
| A-66 | AMBIGUITY | 2 | 7 |
| A-67 | AMBIGUITY | 2 | 3 |
| A-68 | AMBIGUITY | 1 | 1 |
| A-69 | AMBIGUITY | 1 | 1 |
| A-70 | AMBIGUITY | 1 | 2 |
| A-71 | AMBIGUITY | 3 | 4 |
| A-72 | AMBIGUITY | 5 | 7 |
| A-73 | AMBIGUITY | 4 | 5 |
| A-74 | AMBIGUITY | 4 | 7 |
| A-75 | AMBIGUITY | 4 | 5 |
| A-76 | AMBIGUITY | 4 | 10 |
| A-77 | AMBIGUITY | 2 | 3 |
| A-78 | AMBIGUITY | 2 | 2 |
| A-79 | AMBIGUITY | 6 | 13 |
| A-80 | AMBIGUITY | 7 | 12 |
| A-81 | AMBIGUITY | 4 | 9 |
| A-82 | AMBIGUITY | 8 | 29 |
| A-83 | AMBIGUITY | 11 | 24 |
| A-84 | AMBIGUITY | 8 | 16 |
| A-85 | AMBIGUITY | 6 | 12 |
| A-86 | AMBIGUITY | 17 | 34 |
| A-87 | AMBIGUITY | 21 | 59 |
| A-88 | AMBIGUITY | 34 | 86 |
| A-89 | AMBIGUITY | 57 | 162 |
| A-90 | AMBIGUITY | 0 | 0 |
| A-91 | AMBIGUITY | 83 | 238 |
| A-92 | AMBIGUITY | 257 | 1309 |
| A-93 | AMBIGUITY | 915 | 8391 |
| A-94 | AMBIGUITY | 1121 | 5771 |
| A-95 | AMBIGUITY | 989 | 9487 |
| A-96 | AMBIGUITY | 3992 | 2305574 |
| A-97 | AMBIGUITY | 2181 | 55044 |
| A-98 | AMBIGUITY | 0 | 0 |
| A-99 | AMBIGUITY | 6 | 9 |
| I-01 | INVENTION | 89169 | 89169 |
| I-02 | INVENTION | 57632 | 12164302 |
| I-03 | INVENTION | 89169 | 89169 |
| I-04 | INVENTION | 89169 | 89169 |
| I-05 | INVENTION | 33144 | 381066 |
| I-06 | INVENTION | 33144 | 381066 |
| I-07 | INVENTION | 89169 | 89169 |
| I-08 | INVENTION | 9 | 59 |
| I-09 | INVENTION | 1 | 47 |
| I-10 | INVENTION | 2 | 7 |
| I-11 | INVENTION | 87 | 670 |
| I-12 | INVENTION | 3 | 76 |
| I-13 | INVENTION | 5 | 27 |
| I-14 | INVENTION | 47 | 599 |
| I-15 | INVENTION | 854 | 3416 |
| I-16 | INVENTION | 5 | 10 |
| I-17 | INVENTION | 15 | 15 |
| I-18 | INVENTION | 257 | 1493 |
| I-19 | INVENTION | 122 | 1016 |
| I-20 | INVENTION | 150 | 1034 |
| I-21 | INVENTION | 989 | 9487 |
| I-22 | INVENTION | 7 | 10 |
| I-23 | INVENTION | 800 | 6513 |
| I-24 | INVENTION | 11700 | 91590 |
| I-25 | INVENTION | 1 | 4 |
| I-26 | INVENTION | 15 | 21 |
| I-27 | INVENTION | 6723 | 71861 |
| L-01 | LOSS | 12569 | 104568 |
| L-02 | LOSS | 9 | 59 |
| L-03 | LOSS | 1 | 10 |
| L-04 | LOSS | 7 | 24 |
| L-05 | LOSS | 1 | 47 |
| L-06 | LOSS | 1 | 47 |
| L-07 | LOSS | 1 | 47 |
| L-08 | LOSS | 1 | 3 |
| L-09 | LOSS | 27 | 113 |
| L-10 | LOSS | 0 | 0 |
| L-100 | LOSS | 1 | 1 |
| L-101 | LOSS | 1 | 1 |
| L-102 | LOSS | 1 | 1 |
| L-103 | LOSS | 1 | 1 |
| L-104 | LOSS | 1 | 4 |
| L-105 | LOSS | 1 | 1 |
| L-106 | LOSS | 1 | 8 |
| L-107 | LOSS | 1 | 1 |
| L-108 | LOSS | 1 | 10 |
| L-109 | LOSS | 1 | 1 |
| L-11 | LOSS | 1 | 1 |
| L-110 | LOSS | 1 | 1 |
| L-111 | LOSS | 1 | 10 |
| L-112 | LOSS | 1 | 2 |
| L-113 | LOSS | 1 | 1 |
| L-114 | LOSS | 1 | 8 |
| L-115 | LOSS | 1 | 2 |
| L-116 | LOSS | 1 | 1 |
| L-117 | LOSS | 1 | 8 |
| L-118 | LOSS | 1 | 4 |
| L-119 | LOSS | 1 | 1 |
| L-12 | LOSS | 1 | 1 |
| L-120 | LOSS | 1 | 10 |
| L-121 | LOSS | 1 | 1 |
| L-122 | LOSS | 1 | 1 |
| L-123 | LOSS | 1 | 8 |
| L-124 | LOSS | 1 | 1 |
| L-125 | LOSS | 1 | 1 |
| L-126 | LOSS | 1 | 10 |
| L-127 | LOSS | 1 | 1 |
| L-128 | LOSS | 1 | 1 |
| L-129 | LOSS | 1 | 4 |
| L-13 | LOSS | 1 | 1 |
| L-130 | LOSS | 2 | 3 |
| L-131 | LOSS | 1 | 1 |
| L-132 | LOSS | 1 | 10 |
| L-133 | LOSS | 1 | 1 |
| L-134 | LOSS | 1 | 1 |
| L-135 | LOSS | 1 | 10 |
| L-136 | LOSS | 1 | 1 |
| L-137 | LOSS | 1 | 1 |
| L-138 | LOSS | 1 | 24 |
| L-139 | LOSS | 2 | 4 |
| L-14 | LOSS | 19 | 21 |
| L-140 | LOSS | 1 | 1 |
| L-141 | LOSS | 1 | 22 |
| L-142 | LOSS | 2 | 4 |
| L-143 | LOSS | 1 | 1 |
| L-144 | LOSS | 1 | 10 |
| L-145 | LOSS | 1 | 1 |
| L-146 | LOSS | 1 | 44 |
| L-147 | LOSS | 2 | 5 |
| L-148 | LOSS | 1 | 1 |
| L-149 | LOSS | 1 | 46 |
| L-15 | LOSS | 87 | 670 |
| L-150 | LOSS | 2 | 5 |
| L-151 | LOSS | 1 | 1 |
| L-152 | LOSS | 1 | 50 |
| L-153 | LOSS | 2 | 2 |
| L-154 | LOSS | 1 | 1 |
| L-155 | LOSS | 1 | 72 |
| L-156 | LOSS | 1 | 1 |
| L-157 | LOSS | 2 | 99 |
| L-158 | LOSS | 3 | 6 |
| L-159 | LOSS | 1 | 1 |
| L-16 | LOSS | 8 | 63 |
| L-160 | LOSS | 1 | 86 |
| L-161 | LOSS | 2 | 2 |
| L-162 | LOSS | 1 | 1 |
| L-163 | LOSS | 1 | 104 |
| L-164 | LOSS | 7 | 31 |
| L-165 | LOSS | 1 | 1 |
| L-166 | LOSS | 1 | 238 |
| L-167 | LOSS | 15 | 212 |
| L-168 | LOSS | 1 | 1 |
| L-169 | LOSS | 4 | 240 |
| L-17 | LOSS | 3 | 76 |
| L-170 | LOSS | 32 | 273 |
| L-171 | LOSS | 1 | 1 |
| L-172 | LOSS | 9 | 382 |
| L-173 | LOSS | 95 | 640 |
| L-174 | LOSS | 3 | 3 |
| L-175 | LOSS | 171 | 3182 |
| L-176 | LOSS | 28 | 927 |
| L-177 | LOSS | 572 | 5994 |
| L-178 | LOSS | 0 | 0 |
| L-179 | LOSS | 47 | 599 |
| L-18 | LOSS | 4 | 8 |
| L-180 | LOSS | 37 | 583 |
| L-181 | LOSS | 37 | 583 |
| L-182 | LOSS | 484 | 1874 |
| L-183 | LOSS | 5063 | 180230 |
| L-184 | LOSS | 0 | 0 |
| L-185 | LOSS | 2 | 12 |
| L-186 | LOSS | 0 | 0 |
| L-187 | LOSS | 1 | 4 |
| L-188 | LOSS | 0 | 0 |
| L-189 | LOSS | 6 | 9 |
| L-19 | LOSS | 1 | 1 |
| L-190 | LOSS | 12 | 28 |
| L-191 | LOSS | 1 | 1 |
| L-192 | LOSS | 24 | 156 |
| L-193 | LOSS | 24 | 156 |
| L-194 | LOSS | 519 | 6008 |
| L-195 | LOSS | 519 | 6008 |
| L-196 | LOSS | 47 | 103 |
| L-197 | LOSS | 59 | 490 |
| L-198 | LOSS | 6634 | 71461 |
| L-199 | LOSS | 257 | 1493 |
| L-20 | LOSS | 1 | 1 |
| L-200 | LOSS | 86 | 308 |
| L-201 | LOSS | 122 | 1016 |
| L-202 | LOSS | 27 | 373 |
| L-203 | LOSS | 5 | 36 |
| L-204 | LOSS | 7 | 12 |
| L-205 | LOSS | 1 | 1 |
| L-206 | LOSS | 4 | 12 |
| L-207 | LOSS | 2 | 3 |
| L-208 | LOSS | 3 | 14 |
| L-209 | LOSS | 1 | 1 |
| L-21 | LOSS | 1 | 1 |
| L-210 | LOSS | 1 | 1 |
| L-211 | LOSS | 1 | 1 |
| L-212 | LOSS | 1 | 3 |
| L-213 | LOSS | 1 | 2 |
| L-214 | LOSS | 1 | 1 |
| L-215 | LOSS | 1 | 1 |
| L-216 | LOSS | 1 | 2 |
| L-217 | LOSS | 1 | 1 |
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
| L-270 | LOSS | 3 | 9 |
| L-271 | LOSS | 4 | 10 |
| L-272 | LOSS | 3 | 8 |
| L-273 | LOSS | 4 | 9 |
| L-274 | LOSS | 4 | 17 |
| L-275 | LOSS | 3 | 13 |
| L-276 | LOSS | 3 | 10 |
| L-277 | LOSS | 2 | 17 |
| L-278 | LOSS | 3 | 5 |
| L-279 | LOSS | 3 | 11 |
| L-28 | LOSS | 1 | 1 |
| L-280 | LOSS | 4 | 19 |
| L-281 | LOSS | 6 | 19 |
| L-282 | LOSS | 5 | 22 |
| L-283 | LOSS | 1 | 1 |
| L-284 | LOSS | 5 | 23 |
| L-285 | LOSS | 1 | 1 |
| L-286 | LOSS | 6 | 39 |
| L-287 | LOSS | 1 | 1 |
| L-288 | LOSS | 6 | 28 |
| L-289 | LOSS | 1 | 2 |
| L-29 | LOSS | 1 | 1 |
| L-290 | LOSS | 8 | 26 |
| L-291 | LOSS | 6 | 28 |
| L-292 | LOSS | 6 | 32 |
| L-293 | LOSS | 6 | 41 |
| L-294 | LOSS | 7 | 29 |
| L-295 | LOSS | 8 | 31 |
| L-296 | LOSS | 7 | 28 |
| L-297 | LOSS | 1 | 1 |
| L-298 | LOSS | 7 | 37 |
| L-299 | LOSS | 1 | 1 |
| L-30 | LOSS | 1 | 1 |
| L-300 | LOSS | 7 | 54 |
| L-301 | LOSS | 5 | 40 |
| L-302 | LOSS | 8 | 36 |
| L-303 | LOSS | 6 | 39 |
| L-304 | LOSS | 1 | 1 |
| L-305 | LOSS | 9 | 46 |
| L-306 | LOSS | 6 | 47 |
| L-307 | LOSS | 1 | 2 |
| L-308 | LOSS | 1 | 4 |
| L-309 | LOSS | 10 | 46 |
| L-31 | LOSS | 1 | 1 |
| L-310 | LOSS | 10 | 62 |
| L-311 | LOSS | 10 | 56 |
| L-312 | LOSS | 8 | 85 |
| L-313 | LOSS | 10 | 50 |
| L-314 | LOSS | 7 | 44 |
| L-315 | LOSS | 10 | 49 |
| L-316 | LOSS | 9 | 54 |
| L-317 | LOSS | 10 | 73 |
| L-318 | LOSS | 10 | 39 |
| L-319 | LOSS | 1 | 2 |
| L-32 | LOSS | 1 | 1 |
| L-320 | LOSS | 1 | 4 |
| L-321 | LOSS | 8 | 64 |
| L-322 | LOSS | 1 | 1 |
| L-323 | LOSS | 8 | 81 |
| L-324 | LOSS | 9 | 73 |
| L-325 | LOSS | 9 | 157 |
| L-326 | LOSS | 9 | 99 |
| L-327 | LOSS | 1 | 1 |
| L-328 | LOSS | 9 | 71 |
| L-329 | LOSS | 7 | 73 |
| L-33 | LOSS | 1 | 1 |
| L-330 | LOSS | 10 | 92 |
| L-331 | LOSS | 1 | 2 |
| L-332 | LOSS | 1 | 4 |
| L-333 | LOSS | 10 | 116 |
| L-334 | LOSS | 8 | 94 |
| L-335 | LOSS | 11 | 112 |
| L-336 | LOSS | 1 | 1 |
| L-337 | LOSS | 10 | 125 |
| L-338 | LOSS | 1 | 2 |
| L-339 | LOSS | 1 | 4 |
| L-34 | LOSS | 1 | 1 |
| L-340 | LOSS | 10 | 128 |
| L-341 | LOSS | 11 | 144 |
| L-342 | LOSS | 11 | 193 |
| L-343 | LOSS | 2 | 3 |
| L-344 | LOSS | 2 | 5 |
| L-345 | LOSS | 14 | 178 |
| L-346 | LOSS | 1 | 2 |
| L-347 | LOSS | 13 | 157 |
| L-348 | LOSS | 3 | 5 |
| L-349 | LOSS | 15 | 349 |
| L-35 | LOSS | 1 | 1 |
| L-350 | LOSS | 3 | 4 |
| L-351 | LOSS | 16 | 228 |
| L-352 | LOSS | 1 | 1 |
| L-353 | LOSS | 2 | 8 |
| L-354 | LOSS | 20 | 238 |
| L-355 | LOSS | 5 | 6 |
| L-356 | LOSS | 18 | 383 |
| L-357 | LOSS | 1 | 2 |
| L-358 | LOSS | 3 | 7 |
| L-359 | LOSS | 18 | 516 |
| L-36 | LOSS | 1 | 1 |
| L-360 | LOSS | 1 | 1 |
| L-361 | LOSS | 5 | 12 |
| L-362 | LOSS | 24 | 1689 |
| L-363 | LOSS | 4 | 7 |
| L-364 | LOSS | 29 | 799 |
| L-365 | LOSS | 1 | 2 |
| L-366 | LOSS | 9 | 18 |
| L-367 | LOSS | 41 | 1241 |
| L-368 | LOSS | 7 | 17 |
| L-369 | LOSS | 55 | 1199 |
| L-37 | LOSS | 1 | 1 |
| L-370 | LOSS | 3 | 3 |
| L-371 | LOSS | 9 | 67 |
| L-372 | LOSS | 66 | 1571 |
| L-373 | LOSS | 2 | 2 |
| L-374 | LOSS | 15 | 80 |
| L-375 | LOSS | 86 | 2203 |
| L-376 | LOSS | 5 | 9 |
| L-377 | LOSS | 28 | 69 |
| L-378 | LOSS | 113 | 3048 |
| L-379 | LOSS | 3 | 10 |
| L-38 | LOSS | 1 | 1 |
| L-380 | LOSS | 40 | 113 |
| L-381 | LOSS | 192 | 5527 |
| L-382 | LOSS | 0 | 0 |
| L-383 | LOSS | 11 | 28 |
| L-384 | LOSS | 66 | 289 |
| L-385 | LOSS | 304 | 8501 |
| L-386 | LOSS | 27 | 100 |
| L-387 | LOSS | 208 | 1158 |
| L-388 | LOSS | 793 | 42513 |
| L-389 | LOSS | 115 | 690 |
| L-39 | LOSS | 1 | 2 |
| L-390 | LOSS | 39 | 164 |
| L-391 | LOSS | 150 | 1034 |
| L-392 | LOSS | 149 | 1033 |
| L-393 | LOSS | 149 | 1033 |
| L-394 | LOSS | 112 | 428 |
| L-395 | LOSS | 863 | 5207 |
| L-396 | LOSS | 3257 | 1059500 |
| L-397 | LOSS | 5069 | 376355 |
| L-398 | LOSS | 0 | 0 |
| L-399 | LOSS | 0 | 0 |
| L-40 | LOSS | 1 | 1 |
| L-400 | LOSS | 0 | 0 |
| L-401 | LOSS | 2 | 6 |
| L-402 | LOSS | 0 | 0 |
| L-403 | LOSS | 157 | 737 |
| L-404 | LOSS | 800 | 6513 |
| L-405 | LOSS | 705 | 6259 |
| L-406 | LOSS | 705 | 6259 |
| L-407 | LOSS | 3876 | 20140 |
| L-408 | LOSS | 27604 | 4712411 |
| L-409 | LOSS | 0 | 0 |
| L-41 | LOSS | 1 | 2 |
| L-410 | LOSS | 1 | 4 |
| L-411 | LOSS | 1 | 2 |
| L-412 | LOSS | 3 | 7 |
| L-413 | LOSS | 0 | 0 |
| L-414 | LOSS | 3 | 33 |
| L-415 | LOSS | 0 | 0 |
| L-416 | LOSS | 19 | 203 |
| L-417 | LOSS | 2 | 2 |
| L-418 | LOSS | 1 | 1 |
| L-419 | LOSS | 1 | 2 |
| L-42 | LOSS | 1 | 1 |
| L-420 | LOSS | 1 | 2 |
| L-421 | LOSS | 1 | 1 |
| L-422 | LOSS | 1 | 6 |
| L-423 | LOSS | 1 | 1 |
| L-424 | LOSS | 1 | 2 |
| L-425 | LOSS | 1 | 1 |
| L-426 | LOSS | 1 | 1 |
| L-427 | LOSS | 1 | 1 |
| L-428 | LOSS | 1 | 1 |
| L-429 | LOSS | 1 | 1 |
| L-43 | LOSS | 1 | 1 |
| L-430 | LOSS | 1 | 1 |
| L-431 | LOSS | 1 | 1 |
| L-432 | LOSS | 1 | 2 |
| L-433 | LOSS | 1 | 1 |
| L-434 | LOSS | 1 | 1 |
| L-435 | LOSS | 1 | 1 |
| L-436 | LOSS | 1 | 1 |
| L-437 | LOSS | 1 | 2 |
| L-438 | LOSS | 1 | 1 |
| L-439 | LOSS | 1 | 2 |
| L-44 | LOSS | 1 | 1 |
| L-440 | LOSS | 1 | 1 |
| L-441 | LOSS | 1 | 1 |
| L-442 | LOSS | 1 | 1 |
| L-443 | LOSS | 1 | 1 |
| L-444 | LOSS | 1 | 1 |
| L-445 | LOSS | 1 | 1 |
| L-446 | LOSS | 1 | 1 |
| L-447 | LOSS | 1 | 1 |
| L-448 | LOSS | 1 | 3 |
| L-449 | LOSS | 1 | 1 |
| L-45 | LOSS | 1 | 1 |
| L-450 | LOSS | 1 | 2 |
| L-451 | LOSS | 1 | 1 |
| L-452 | LOSS | 1 | 1 |
| L-453 | LOSS | 1 | 4 |
| L-454 | LOSS | 1 | 5 |
| L-455 | LOSS | 1 | 2 |
| L-456 | LOSS | 1 | 1 |
| L-457 | LOSS | 1 | 8 |
| L-458 | LOSS | 2 | 2 |
| L-459 | LOSS | 2 | 8 |
| L-46 | LOSS | 1 | 2 |
| L-460 | LOSS | 1 | 1 |
| L-461 | LOSS | 3 | 10 |
| L-462 | LOSS | 2 | 2 |
| L-463 | LOSS | 3 | 8 |
| L-464 | LOSS | 6 | 22 |
| L-465 | LOSS | 2 | 2 |
| L-466 | LOSS | 4 | 42 |
| L-467 | LOSS | 2 | 2 |
| L-468 | LOSS | 8 | 27 |
| L-469 | LOSS | 4 | 4 |
| L-47 | LOSS | 1 | 1 |
| L-470 | LOSS | 8 | 19 |
| L-471 | LOSS | 3 | 5 |
| L-472 | LOSS | 10 | 38 |
| L-473 | LOSS | 1 | 1 |
| L-474 | LOSS | 12 | 15 |
| L-475 | LOSS | 19 | 132 |
| L-476 | LOSS | 33 | 64 |
| L-477 | LOSS | 40 | 734 |
| L-478 | LOSS | 4 | 11 |
| L-479 | LOSS | 74 | 283 |
| L-48 | LOSS | 1 | 1 |
| L-480 | LOSS | 86 | 6984 |
| L-481 | LOSS | 36 | 453 |
| L-482 | LOSS | 378 | 8422 |
| L-483 | LOSS | 411 | 138302 |
| L-484 | LOSS | 0 | 0 |
| L-485 | LOSS | 0 | 0 |
| L-486 | LOSS | 10 | 32 |
| L-487 | LOSS | 0 | 0 |
| L-488 | LOSS | 2 | 2 |
| L-489 | LOSS | 26 | 249 |
| L-49 | LOSS | 1 | 1 |
| L-490 | LOSS | 1 | 1 |
| L-491 | LOSS | 71 | 1673 |
| L-492 | LOSS | 71 | 1673 |
| L-493 | LOSS | 418 | 17288 |
| L-494 | LOSS | 418 | 17288 |
| L-495 | LOSS | 559 | 9558 |
| L-496 | LOSS | 44 | 674 |
| L-497 | LOSS | 5348 | 87024 |
| L-498 | LOSS | 17894 | 17894 |
| L-499 | LOSS | 89169 | 89169 |
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
| S-01 | STRUCTURAL | 26 | 288 |
| S-02 | STRUCTURAL | 19 | 21 |
| S-03 | STRUCTURAL | 6634 | 71461 |
| S-04 | STRUCTURAL | 15 | 15 |
| S-05 | STRUCTURAL | 91 | 175 |
| S-06 | STRUCTURAL | 31 | 52 |
| S-07 | STRUCTURAL | 5348 | 87024 |
| S-08 | STRUCTURAL | 6723 | 71861 |
| S-09 | STRUCTURAL | 66 | 113 |
| S-10 | STRUCTURAL | 61 | 119 |
| U-01 | UNSUPPORTED | 15 | 96 |
| U-02 | UNSUPPORTED | 6 | 22 |
| U-03 | UNSUPPORTED | 4 | 10 |
| U-04 | UNSUPPORTED | 1 | 14 |
| U-05 | UNSUPPORTED | 2 | 3 |
| U-06 | UNSUPPORTED | 1 | 1 |
| U-07 | UNSUPPORTED | 3 | 4 |
| U-08 | UNSUPPORTED | 13 | 16 |
| U-09 | UNSUPPORTED | 5 | 14 |
| U-10 | UNSUPPORTED | 11 | 73 |
| U-11 | UNSUPPORTED | 620 | 10841 |
| U-12 | UNSUPPORTED | 34 | 178 |
| U-13 | UNSUPPORTED | 321 | 1293 |
| U-14 | UNSUPPORTED | 234 | 4064 |
| U-15 | UNSUPPORTED | 142 | 955 |
| U-16 | UNSUPPORTED | 3 | 7 |
| U-17 | UNSUPPORTED | 1 | 1 |
| U-18 | UNSUPPORTED | 91 | 175 |
| U-19 | UNSUPPORTED | 31 | 52 |
| U-20 | UNSUPPORTED | 8 | 61 |
| U-21 | UNSUPPORTED | 1 | 1 |
| U-22 | UNSUPPORTED | 1 | 1 |
| U-23 | UNSUPPORTED | 1 | 2 |
| U-24 | UNSUPPORTED | 1 | 1 |
| U-25 | UNSUPPORTED | 3 | 7 |
| U-26 | UNSUPPORTED | 10 | 17 |
| U-27 | UNSUPPORTED | 5 | 13 |
| U-28 | UNSUPPORTED | 23 | 97 |
| U-29 | UNSUPPORTED | 105 | 680 |
| U-30 | UNSUPPORTED | 9 | 11 |
| U-31 | UNSUPPORTED | 1227 | 11143 |
| U-32 | UNSUPPORTED | 186 | 2010 |
| U-33 | UNSUPPORTED | 0 | 0 |
| U-34 | UNSUPPORTED | 139 | 1134 |
| U-35 | UNSUPPORTED | 2 | 3 |
| U-36 | UNSUPPORTED | 9 | 25 |
| U-37 | UNSUPPORTED | 1350 | 16783 |
| U-38 | UNSUPPORTED | 61 | 238 |
| U-39 | UNSUPPORTED | 20284 | 4765187 |
| U-40 | UNSUPPORTED | 893 | 18367 |
| U-41 | UNSUPPORTED | 347 | 2321 |
| U-42 | UNSUPPORTED | 8 | 9 |
| U-43 | UNSUPPORTED | 11 | 58 |
| U-44 | UNSUPPORTED | 1 | 2 |
| U-45 | UNSUPPORTED | 1 | 3 |
| U-46 | UNSUPPORTED | 1 | 1 |
| U-47 | UNSUPPORTED | 5 | 7 |
| U-48 | UNSUPPORTED | 15 | 90 |
| U-49 | UNSUPPORTED | 7 | 47 |
| U-50 | UNSUPPORTED | 66 | 113 |
| U-51 | UNSUPPORTED | 61 | 119 |

## Top Errors

No conversion failures were observed.

## Failure Samples

No conversion failure samples.
