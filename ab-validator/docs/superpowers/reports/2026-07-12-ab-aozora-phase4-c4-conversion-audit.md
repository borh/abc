# AAT Parser-IR Conversion Audit

- generated_unix_seconds: `1783806167`
- mapping: `https://w3id.org/abc/mappings/aat-v2-to-parser-ir-v1/generated-probe` `0.3.0`
- mapping_hash: `sha256:7249cd727ef2da90dcd591e6009bead9235fe1c140697ee6bd70aacd9e85ee40`
- mapping_schema_hash: `sha256:e6af01115ccdb7c5cad086eee4c458230f6b6f55e0dfee7791730b48994283e2`
- target_parser_ir_schema_hash: `sha256:a1e1b5069fdec17cbb1f94eb5e9a582d1b109dd95c07257f4da7d9b76c82cfa2`

## Totals

| files_attempted | files_succeeded | files_failed | parser_ir_nodes | divergence_records | divergence_occurrences | elapsed_seconds |
|---:|---:|---:|---:|---:|---:|---:|
| 17886 | 17886 | 0 | 18804620 | 231845 | 4944131 | 10.288 |

## Inputs

| label | files | aat_dir |
|---|---:|---|
| ab-aozora | 17886 | `/db/ab-validator/aat-corpus/ab-aozora-phase4-c4-27772b1/aat/ab-aozora` |

## Corpus Results

| corpus | attempted | succeeded | failed | parser_ir_nodes | divergence_occurrences |
|---|---:|---:|---:|---:|---:|
| ab-aozora | 17886 | 17886 | 0 | 18804620 | 4944131 |

## Raw Nodes

- nodes_total: `198429`
- files_with_raw: `7591`
- fatal_direct_failures: `0`

| corpus | nodes_total | files_with_raw | fatal_direct_failures |
|---|---:|---:|---:|
| ab-aozora | 198429 | 7591 | 0 |

| inferred_provenance | nodes |
|---|---:|
| parser-derived | 185787 |
| source-derived | 12642 |

| source_class | nodes |
|---|---:|
| aozora-marker | 191188 |
| editorial-note | 5 |
| text | 7236 |

| corpus | path | pointer | provenance | class | source_marker_kind | source_preview |
|---|---|---|---|---|---|---|
| ab-aozora | `000005_53194-ebb0cbaf64b3.json` | `$.blocks[0].content[0]` | parser-derived | aozora-marker | center | ［＃ページの左右中央］ |
| ab-aozora | `000005_53194-ebb0cbaf64b3.json` | `$.blocks[0].content[2]` | parser-derived | aozora-marker | indent | ［＃４字下げ］ |
| ab-aozora | `000005_53194-ebb0cbaf64b3.json` | `$.blocks[0].content[6]` | parser-derived | aozora-marker | sectionBreak | ［＃改丁］ |
| ab-aozora | `000005_53194-ebb0cbaf64b3.json` | `$.blocks[18].content[361]` | parser-derived | aozora-marker | directive | ［＃割り注］ |
| ab-aozora | `000005_53194-ebb0cbaf64b3.json` | `$.blocks[18].content[363]` | parser-derived | aozora-marker | directive | ［＃割り注終わり］ |
| ab-aozora | `000005_53194-ebb0cbaf64b3.json` | `$.blocks[18].content[365]` | parser-derived | text | angleQuote | ≪愛さでやまぬ胸なれば≫ |
| ab-aozora | `000005_53194-ebb0cbaf64b3.json` | `$.blocks[36].content[106]` | parser-derived | aozora-marker | directive | ［＃割り注］ |
| ab-aozora | `000005_53194-ebb0cbaf64b3.json` | `$.blocks[36].content[108]` | parser-derived | aozora-marker | directive | ［＃割り注終わり］ |
| ab-aozora | `000005_53194-ebb0cbaf64b3.json` | `$.blocks[44].content[261]` | parser-derived | aozora-marker | directive | ［＃「……』」は底本では「……」」］ |
| ab-aozora | `000006_1868-e598e91a19ff.json` | `$.blocks[1].content[9]` | parser-derived | aozora-marker | containerOpen | ［＃ここから割り注］ |
| ab-aozora | `000006_1868-e598e91a19ff.json` | `$.blocks[1].content[11]` | parser-derived | aozora-marker | containerClose | ［＃ここで割り注終わり］ |
| ab-aozora | `000006_1869-62320f0f4474.json` | `$.blocks[3].content[0]` | parser-derived | aozora-marker | pageBreak | ［＃改ページ］ |
| ab-aozora | `000006_1869-62320f0f4474.json` | `$.blocks[6].content[0]` | parser-derived | aozora-marker | pageBreak | ［＃改ページ］ |
| ab-aozora | `000006_1869-62320f0f4474.json` | `$.blocks[6].content[425]` | parser-derived | aozora-marker | directive | ［＃「引」は小書き右寄せ］ |
| ab-aozora | `000006_1869-62320f0f4474.json` | `$.blocks[8].content[2656]` | parser-derived | aozora-marker | containerOpen | ［＃ここから割り注］ |
| ab-aozora | `000006_1869-62320f0f4474.json` | `$.blocks[8].content[2658]` | parser-derived | aozora-marker | containerClose | ［＃ここで割り注終わり］ |
| ab-aozora | `000006_1869-62320f0f4474.json` | `$.blocks[8].content[4178]` | parser-derived | aozora-marker | sectionBreak | ［＃改丁］ |
| ab-aozora | `000006_1869-62320f0f4474.json` | `$.blocks[8].content[4484]` | source-derived | text | unparsed-source-gap | 髷も出たおケシも出た。○○《なになに》会幹事、実は古猫の怪という、 |
| ab-aozora | `000006_1869-62320f0f4474.json` | `$.blocks[8].content[5144]` | parser-derived | aozora-marker | directive | ［＃「狼狽《あわ》てて」は底本では「狼狙《あわ》てて」］ |
| ab-aozora | `000006_1869-62320f0f4474.json` | `$.blocks[9].children[0].content[8]` | source-derived | text | unparsed-source-gap | 候ように○○《どこそこ》のお祖師さまへ |

## Sentence Projection Failures

Files failed during sentence projection: 0

| Atomic Node Type | Failures |
|---|---:|

Other sentence projection failures: 0

## Divergence Categories

| category | occurrences |
|---|---:|
| AMBIGUITY | 390896 |
| INVENTION | 3853084 |
| LOSS | 426741 |
| STRUCTURAL | 74665 |
| UNSUPPORTED | 198745 |

## Compatibility Candidates

| adapter | adapter_version | mapping_version | mapping_hash | files_succeeded | files_failed | rules_emitted | rules_missing | unsupported_occurrences |
|---|---|---|---|---:|---:|---:|---:|---:|
| ab-aozora | ab-aozora 0.5.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git 27772b1b75c9ceeb0b724095bbbb47f774f3a275) | 0.3.0 | `sha256:7249cd727ef2da90dcd591e6009bead9235fe1c140697ee6bd70aacd9e85ee40` | 17886 | 0 | 54 | 633 | 198745 |

## Rule Coverage

- rules_total: `687`
- rules_emitted: `54`
- rules_missing: `633`

| rule_id | category | files | occurrences |
|---|---|---:|---:|
| A-01 | AMBIGUITY | 0 | 0 |
| A-02 | AMBIGUITY | 0 | 0 |
| A-03 | AMBIGUITY | 0 | 0 |
| A-04 | AMBIGUITY | 0 | 0 |
| A-05 | AMBIGUITY | 0 | 0 |
| A-06 | AMBIGUITY | 0 | 0 |
| A-07 | AMBIGUITY | 0 | 0 |
| A-08 | AMBIGUITY | 0 | 0 |
| A-09 | AMBIGUITY | 0 | 0 |
| A-10 | AMBIGUITY | 0 | 0 |
| A-100 | AMBIGUITY | 0 | 0 |
| A-101 | AMBIGUITY | 0 | 0 |
| A-102 | AMBIGUITY | 0 | 0 |
| A-103 | AMBIGUITY | 0 | 0 |
| A-104 | AMBIGUITY | 4244 | 37253 |
| A-105 | AMBIGUITY | 4245 | 37336 |
| A-106 | AMBIGUITY | 0 | 0 |
| A-107 | AMBIGUITY | 6600 | 163473 |
| A-108 | AMBIGUITY | 0 | 0 |
| A-109 | AMBIGUITY | 0 | 0 |
| A-11 | AMBIGUITY | 0 | 0 |
| A-110 | AMBIGUITY | 0 | 0 |
| A-111 | AMBIGUITY | 0 | 0 |
| A-112 | AMBIGUITY | 0 | 0 |
| A-113 | AMBIGUITY | 0 | 0 |
| A-114 | AMBIGUITY | 0 | 0 |
| A-115 | AMBIGUITY | 0 | 0 |
| A-116 | AMBIGUITY | 0 | 0 |
| A-117 | AMBIGUITY | 0 | 0 |
| A-118 | AMBIGUITY | 0 | 0 |
| A-119 | AMBIGUITY | 0 | 0 |
| A-12 | AMBIGUITY | 0 | 0 |
| A-120 | AMBIGUITY | 0 | 0 |
| A-121 | AMBIGUITY | 0 | 0 |
| A-122 | AMBIGUITY | 0 | 0 |
| A-123 | AMBIGUITY | 0 | 0 |
| A-124 | AMBIGUITY | 0 | 0 |
| A-125 | AMBIGUITY | 0 | 0 |
| A-126 | AMBIGUITY | 0 | 0 |
| A-127 | AMBIGUITY | 3296 | 56376 |
| A-128 | AMBIGUITY | 3297 | 56461 |
| A-129 | AMBIGUITY | 1 | 1 |
| A-13 | AMBIGUITY | 0 | 0 |
| A-130 | AMBIGUITY | 17886 | 17886 |
| A-14 | AMBIGUITY | 0 | 0 |
| A-15 | AMBIGUITY | 0 | 0 |
| A-16 | AMBIGUITY | 0 | 0 |
| A-17 | AMBIGUITY | 0 | 0 |
| A-18 | AMBIGUITY | 0 | 0 |
| A-19 | AMBIGUITY | 0 | 0 |
| A-20 | AMBIGUITY | 12 | 156 |
| A-21 | AMBIGUITY | 0 | 0 |
| A-22 | AMBIGUITY | 12 | 156 |
| A-23 | AMBIGUITY | 0 | 0 |
| A-24 | AMBIGUITY | 11 | 68 |
| A-25 | AMBIGUITY | 501 | 1950 |
| A-26 | AMBIGUITY | 501 | 1961 |
| A-27 | AMBIGUITY | 0 | 0 |
| A-28 | AMBIGUITY | 584 | 8118 |
| A-29 | AMBIGUITY | 0 | 0 |
| A-30 | AMBIGUITY | 0 | 0 |
| A-31 | AMBIGUITY | 0 | 0 |
| A-32 | AMBIGUITY | 0 | 0 |
| A-33 | AMBIGUITY | 0 | 0 |
| A-34 | AMBIGUITY | 0 | 0 |
| A-35 | AMBIGUITY | 0 | 0 |
| A-36 | AMBIGUITY | 0 | 0 |
| A-37 | AMBIGUITY | 0 | 0 |
| A-38 | AMBIGUITY | 22 | 151 |
| A-39 | AMBIGUITY | 22 | 151 |
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
| A-55 | AMBIGUITY | 0 | 0 |
| A-56 | AMBIGUITY | 0 | 0 |
| A-57 | AMBIGUITY | 0 | 0 |
| A-58 | AMBIGUITY | 0 | 0 |
| A-59 | AMBIGUITY | 0 | 0 |
| A-60 | AMBIGUITY | 0 | 0 |
| A-61 | AMBIGUITY | 0 | 0 |
| A-62 | AMBIGUITY | 0 | 0 |
| A-63 | AMBIGUITY | 0 | 0 |
| A-64 | AMBIGUITY | 0 | 0 |
| A-65 | AMBIGUITY | 0 | 0 |
| A-66 | AMBIGUITY | 0 | 0 |
| A-67 | AMBIGUITY | 0 | 0 |
| A-68 | AMBIGUITY | 0 | 0 |
| A-69 | AMBIGUITY | 0 | 0 |
| A-70 | AMBIGUITY | 0 | 0 |
| A-71 | AMBIGUITY | 0 | 0 |
| A-72 | AMBIGUITY | 0 | 0 |
| A-73 | AMBIGUITY | 0 | 0 |
| A-74 | AMBIGUITY | 0 | 0 |
| A-75 | AMBIGUITY | 0 | 0 |
| A-76 | AMBIGUITY | 0 | 0 |
| A-77 | AMBIGUITY | 0 | 0 |
| A-78 | AMBIGUITY | 0 | 0 |
| A-79 | AMBIGUITY | 0 | 0 |
| A-80 | AMBIGUITY | 0 | 0 |
| A-81 | AMBIGUITY | 0 | 0 |
| A-82 | AMBIGUITY | 0 | 0 |
| A-83 | AMBIGUITY | 0 | 0 |
| A-84 | AMBIGUITY | 0 | 0 |
| A-85 | AMBIGUITY | 0 | 0 |
| A-86 | AMBIGUITY | 0 | 0 |
| A-87 | AMBIGUITY | 0 | 0 |
| A-88 | AMBIGUITY | 0 | 0 |
| A-89 | AMBIGUITY | 0 | 0 |
| A-90 | AMBIGUITY | 0 | 0 |
| A-91 | AMBIGUITY | 0 | 0 |
| A-92 | AMBIGUITY | 0 | 0 |
| A-93 | AMBIGUITY | 0 | 0 |
| A-94 | AMBIGUITY | 318 | 2098 |
| A-95 | AMBIGUITY | 0 | 0 |
| A-96 | AMBIGUITY | 318 | 2098 |
| A-97 | AMBIGUITY | 0 | 0 |
| A-98 | AMBIGUITY | 507 | 5203 |
| A-99 | AMBIGUITY | 0 | 0 |
| I-01 | INVENTION | 17886 | 17886 |
| I-02 | INVENTION | 14303 | 3556724 |
| I-03 | INVENTION | 17886 | 17886 |
| I-04 | INVENTION | 17886 | 17886 |
| I-05 | INVENTION | 5088 | 91590 |
| I-06 | INVENTION | 5088 | 91590 |
| I-07 | INVENTION | 17886 | 17886 |
| I-08 | INVENTION | 0 | 0 |
| I-09 | INVENTION | 0 | 0 |
| I-10 | INVENTION | 0 | 0 |
| I-11 | INVENTION | 0 | 0 |
| I-12 | INVENTION | 0 | 0 |
| I-13 | INVENTION | 12 | 156 |
| I-14 | INVENTION | 0 | 0 |
| I-15 | INVENTION | 501 | 1961 |
| I-16 | INVENTION | 0 | 0 |
| I-17 | INVENTION | 0 | 0 |
| I-18 | INVENTION | 0 | 0 |
| I-19 | INVENTION | 0 | 0 |
| I-20 | INVENTION | 0 | 0 |
| I-21 | INVENTION | 0 | 0 |
| I-22 | INVENTION | 0 | 0 |
| I-23 | INVENTION | 318 | 2098 |
| I-24 | INVENTION | 0 | 0 |
| I-25 | INVENTION | 0 | 0 |
| I-26 | INVENTION | 4245 | 37336 |
| I-27 | INVENTION | 0 | 0 |
| I-28 | INVENTION | 0 | 0 |
| I-29 | INVENTION | 0 | 0 |
| I-30 | INVENTION | 0 | 0 |
| I-31 | INVENTION | 0 | 0 |
| I-32 | INVENTION | 12 | 85 |
| I-33 | INVENTION | 0 | 0 |
| L-01 | LOSS | 512 | 1796 |
| L-02 | LOSS | 0 | 0 |
| L-03 | LOSS | 0 | 0 |
| L-04 | LOSS | 0 | 0 |
| L-05 | LOSS | 0 | 0 |
| L-06 | LOSS | 0 | 0 |
| L-07 | LOSS | 0 | 0 |
| L-08 | LOSS | 0 | 0 |
| L-09 | LOSS | 0 | 0 |
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
| L-11 | LOSS | 0 | 0 |
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
| L-12 | LOSS | 0 | 0 |
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
| L-13 | LOSS | 0 | 0 |
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
| L-14 | LOSS | 0 | 0 |
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
| L-158 | LOSS | 48 | 133 |
| L-159 | LOSS | 0 | 0 |
| L-16 | LOSS | 0 | 0 |
| L-160 | LOSS | 0 | 0 |
| L-161 | LOSS | 0 | 0 |
| L-162 | LOSS | 0 | 0 |
| L-163 | LOSS | 0 | 0 |
| L-164 | LOSS | 0 | 0 |
| L-165 | LOSS | 0 | 0 |
| L-166 | LOSS | 0 | 0 |
| L-167 | LOSS | 0 | 0 |
| L-168 | LOSS | 0 | 0 |
| L-169 | LOSS | 0 | 0 |
| L-17 | LOSS | 0 | 0 |
| L-170 | LOSS | 22 | 151 |
| L-171 | LOSS | 0 | 0 |
| L-172 | LOSS | 0 | 0 |
| L-173 | LOSS | 0 | 0 |
| L-174 | LOSS | 0 | 0 |
| L-175 | LOSS | 0 | 0 |
| L-176 | LOSS | 0 | 0 |
| L-177 | LOSS | 0 | 0 |
| L-178 | LOSS | 0 | 0 |
| L-179 | LOSS | 0 | 0 |
| L-18 | LOSS | 0 | 0 |
| L-180 | LOSS | 0 | 0 |
| L-181 | LOSS | 0 | 0 |
| L-182 | LOSS | 0 | 0 |
| L-183 | LOSS | 0 | 0 |
| L-184 | LOSS | 0 | 0 |
| L-185 | LOSS | 0 | 0 |
| L-186 | LOSS | 0 | 0 |
| L-187 | LOSS | 0 | 0 |
| L-188 | LOSS | 0 | 0 |
| L-189 | LOSS | 0 | 0 |
| L-19 | LOSS | 0 | 0 |
| L-190 | LOSS | 0 | 0 |
| L-191 | LOSS | 0 | 0 |
| L-192 | LOSS | 0 | 0 |
| L-193 | LOSS | 0 | 0 |
| L-194 | LOSS | 0 | 0 |
| L-195 | LOSS | 0 | 0 |
| L-196 | LOSS | 0 | 0 |
| L-197 | LOSS | 0 | 0 |
| L-198 | LOSS | 0 | 0 |
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
| L-253 | LOSS | 0 | 0 |
| L-254 | LOSS | 0 | 0 |
| L-255 | LOSS | 0 | 0 |
| L-256 | LOSS | 0 | 0 |
| L-257 | LOSS | 0 | 0 |
| L-258 | LOSS | 0 | 0 |
| L-259 | LOSS | 0 | 0 |
| L-26 | LOSS | 0 | 0 |
| L-260 | LOSS | 0 | 0 |
| L-261 | LOSS | 0 | 0 |
| L-262 | LOSS | 0 | 0 |
| L-263 | LOSS | 0 | 0 |
| L-264 | LOSS | 0 | 0 |
| L-265 | LOSS | 0 | 0 |
| L-266 | LOSS | 0 | 0 |
| L-267 | LOSS | 0 | 0 |
| L-268 | LOSS | 0 | 0 |
| L-269 | LOSS | 0 | 0 |
| L-27 | LOSS | 0 | 0 |
| L-270 | LOSS | 0 | 0 |
| L-271 | LOSS | 0 | 0 |
| L-272 | LOSS | 0 | 0 |
| L-273 | LOSS | 0 | 0 |
| L-274 | LOSS | 0 | 0 |
| L-275 | LOSS | 0 | 0 |
| L-276 | LOSS | 0 | 0 |
| L-277 | LOSS | 0 | 0 |
| L-278 | LOSS | 0 | 0 |
| L-279 | LOSS | 0 | 0 |
| L-28 | LOSS | 0 | 0 |
| L-280 | LOSS | 0 | 0 |
| L-281 | LOSS | 0 | 0 |
| L-282 | LOSS | 0 | 0 |
| L-283 | LOSS | 0 | 0 |
| L-284 | LOSS | 0 | 0 |
| L-285 | LOSS | 0 | 0 |
| L-286 | LOSS | 0 | 0 |
| L-287 | LOSS | 0 | 0 |
| L-288 | LOSS | 0 | 0 |
| L-289 | LOSS | 0 | 0 |
| L-29 | LOSS | 0 | 0 |
| L-290 | LOSS | 0 | 0 |
| L-291 | LOSS | 0 | 0 |
| L-292 | LOSS | 0 | 0 |
| L-293 | LOSS | 0 | 0 |
| L-294 | LOSS | 0 | 0 |
| L-295 | LOSS | 0 | 0 |
| L-296 | LOSS | 0 | 0 |
| L-297 | LOSS | 0 | 0 |
| L-298 | LOSS | 0 | 0 |
| L-299 | LOSS | 0 | 0 |
| L-30 | LOSS | 0 | 0 |
| L-300 | LOSS | 0 | 0 |
| L-301 | LOSS | 0 | 0 |
| L-302 | LOSS | 0 | 0 |
| L-303 | LOSS | 0 | 0 |
| L-304 | LOSS | 0 | 0 |
| L-305 | LOSS | 0 | 0 |
| L-306 | LOSS | 0 | 0 |
| L-307 | LOSS | 0 | 0 |
| L-308 | LOSS | 0 | 0 |
| L-309 | LOSS | 0 | 0 |
| L-31 | LOSS | 0 | 0 |
| L-310 | LOSS | 0 | 0 |
| L-311 | LOSS | 0 | 0 |
| L-312 | LOSS | 0 | 0 |
| L-313 | LOSS | 0 | 0 |
| L-314 | LOSS | 0 | 0 |
| L-315 | LOSS | 0 | 0 |
| L-316 | LOSS | 0 | 0 |
| L-317 | LOSS | 0 | 0 |
| L-318 | LOSS | 0 | 0 |
| L-319 | LOSS | 0 | 0 |
| L-32 | LOSS | 0 | 0 |
| L-320 | LOSS | 0 | 0 |
| L-321 | LOSS | 0 | 0 |
| L-322 | LOSS | 0 | 0 |
| L-323 | LOSS | 0 | 0 |
| L-324 | LOSS | 0 | 0 |
| L-325 | LOSS | 0 | 0 |
| L-326 | LOSS | 0 | 0 |
| L-327 | LOSS | 0 | 0 |
| L-328 | LOSS | 0 | 0 |
| L-329 | LOSS | 0 | 0 |
| L-33 | LOSS | 0 | 0 |
| L-330 | LOSS | 0 | 0 |
| L-331 | LOSS | 0 | 0 |
| L-332 | LOSS | 0 | 0 |
| L-333 | LOSS | 0 | 0 |
| L-334 | LOSS | 0 | 0 |
| L-335 | LOSS | 0 | 0 |
| L-336 | LOSS | 0 | 0 |
| L-337 | LOSS | 0 | 0 |
| L-338 | LOSS | 0 | 0 |
| L-339 | LOSS | 0 | 0 |
| L-34 | LOSS | 0 | 0 |
| L-340 | LOSS | 0 | 0 |
| L-341 | LOSS | 0 | 0 |
| L-342 | LOSS | 0 | 0 |
| L-343 | LOSS | 0 | 0 |
| L-344 | LOSS | 0 | 0 |
| L-345 | LOSS | 0 | 0 |
| L-346 | LOSS | 0 | 0 |
| L-347 | LOSS | 0 | 0 |
| L-348 | LOSS | 0 | 0 |
| L-349 | LOSS | 0 | 0 |
| L-35 | LOSS | 0 | 0 |
| L-350 | LOSS | 0 | 0 |
| L-351 | LOSS | 0 | 0 |
| L-352 | LOSS | 0 | 0 |
| L-353 | LOSS | 0 | 0 |
| L-354 | LOSS | 0 | 0 |
| L-355 | LOSS | 0 | 0 |
| L-356 | LOSS | 0 | 0 |
| L-357 | LOSS | 0 | 0 |
| L-358 | LOSS | 0 | 0 |
| L-359 | LOSS | 0 | 0 |
| L-36 | LOSS | 0 | 0 |
| L-360 | LOSS | 0 | 0 |
| L-361 | LOSS | 0 | 0 |
| L-362 | LOSS | 0 | 0 |
| L-363 | LOSS | 0 | 0 |
| L-364 | LOSS | 0 | 0 |
| L-365 | LOSS | 0 | 0 |
| L-366 | LOSS | 41 | 122 |
| L-367 | LOSS | 0 | 0 |
| L-368 | LOSS | 0 | 0 |
| L-369 | LOSS | 0 | 0 |
| L-37 | LOSS | 0 | 0 |
| L-370 | LOSS | 0 | 0 |
| L-371 | LOSS | 0 | 0 |
| L-372 | LOSS | 0 | 0 |
| L-373 | LOSS | 0 | 0 |
| L-374 | LOSS | 0 | 0 |
| L-375 | LOSS | 0 | 0 |
| L-376 | LOSS | 0 | 0 |
| L-377 | LOSS | 0 | 0 |
| L-378 | LOSS | 0 | 0 |
| L-379 | LOSS | 460 | 1490 |
| L-38 | LOSS | 0 | 0 |
| L-380 | LOSS | 0 | 0 |
| L-381 | LOSS | 0 | 0 |
| L-382 | LOSS | 0 | 0 |
| L-383 | LOSS | 0 | 0 |
| L-384 | LOSS | 0 | 0 |
| L-385 | LOSS | 0 | 0 |
| L-386 | LOSS | 0 | 0 |
| L-387 | LOSS | 0 | 0 |
| L-388 | LOSS | 0 | 0 |
| L-389 | LOSS | 0 | 0 |
| L-39 | LOSS | 0 | 0 |
| L-390 | LOSS | 0 | 0 |
| L-391 | LOSS | 0 | 0 |
| L-392 | LOSS | 0 | 0 |
| L-393 | LOSS | 0 | 0 |
| L-394 | LOSS | 0 | 0 |
| L-395 | LOSS | 0 | 0 |
| L-396 | LOSS | 0 | 0 |
| L-397 | LOSS | 0 | 0 |
| L-398 | LOSS | 0 | 0 |
| L-399 | LOSS | 0 | 0 |
| L-40 | LOSS | 0 | 0 |
| L-400 | LOSS | 0 | 0 |
| L-401 | LOSS | 0 | 0 |
| L-402 | LOSS | 0 | 0 |
| L-403 | LOSS | 0 | 0 |
| L-404 | LOSS | 0 | 0 |
| L-405 | LOSS | 0 | 0 |
| L-406 | LOSS | 0 | 0 |
| L-407 | LOSS | 0 | 0 |
| L-408 | LOSS | 0 | 0 |
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
| L-462 | LOSS | 0 | 0 |
| L-463 | LOSS | 0 | 0 |
| L-464 | LOSS | 3296 | 56376 |
| L-465 | LOSS | 0 | 0 |
| L-466 | LOSS | 17886 | 17886 |
| L-467 | LOSS | 0 | 0 |
| L-468 | LOSS | 3220 | 56280 |
| L-469 | LOSS | 17735 | 17737 |
| L-47 | LOSS | 0 | 0 |
| L-470 | LOSS | 5088 | 91590 |
| L-471 | LOSS | 5088 | 91590 |
| L-472 | LOSS | 5088 | 91590 |
| L-48 | LOSS | 0 | 0 |
| L-49 | LOSS | 0 | 0 |
| L-50 | LOSS | 0 | 0 |
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
| S-01 | STRUCTURAL | 0 | 0 |
| S-02 | STRUCTURAL | 0 | 0 |
| S-03 | STRUCTURAL | 22 | 151 |
| S-04 | STRUCTURAL | 0 | 0 |
| S-05 | STRUCTURAL | 0 | 0 |
| S-06 | STRUCTURAL | 0 | 0 |
| S-07 | STRUCTURAL | 3296 | 56376 |
| S-08 | STRUCTURAL | 12 | 85 |
| S-09 | STRUCTURAL | 89 | 159 |
| S-10 | STRUCTURAL | 75 | 157 |
| S-11 | STRUCTURAL | 0 | 0 |
| S-12 | STRUCTURAL | 17735 | 17737 |
| U-01 | UNSUPPORTED | 0 | 0 |
| U-02 | UNSUPPORTED | 0 | 0 |
| U-03 | UNSUPPORTED | 0 | 0 |
| U-04 | UNSUPPORTED | 0 | 0 |
| U-05 | UNSUPPORTED | 0 | 0 |
| U-06 | UNSUPPORTED | 1 | 2 |
| U-07 | UNSUPPORTED | 0 | 0 |
| U-08 | UNSUPPORTED | 764 | 19360 |
| U-09 | UNSUPPORTED | 0 | 0 |
| U-10 | UNSUPPORTED | 0 | 0 |
| U-11 | UNSUPPORTED | 0 | 0 |
| U-12 | UNSUPPORTED | 0 | 0 |
| U-13 | UNSUPPORTED | 0 | 0 |
| U-14 | UNSUPPORTED | 0 | 0 |
| U-15 | UNSUPPORTED | 0 | 0 |
| U-16 | UNSUPPORTED | 0 | 0 |
| U-17 | UNSUPPORTED | 0 | 0 |
| U-18 | UNSUPPORTED | 0 | 0 |
| U-19 | UNSUPPORTED | 0 | 0 |
| U-20 | UNSUPPORTED | 0 | 0 |
| U-21 | UNSUPPORTED | 0 | 0 |
| U-22 | UNSUPPORTED | 0 | 0 |
| U-23 | UNSUPPORTED | 0 | 0 |
| U-24 | UNSUPPORTED | 274 | 3858 |
| U-25 | UNSUPPORTED | 0 | 0 |
| U-26 | UNSUPPORTED | 0 | 0 |
| U-27 | UNSUPPORTED | 0 | 0 |
| U-28 | UNSUPPORTED | 7441 | 175209 |
| U-29 | UNSUPPORTED | 0 | 0 |
| U-30 | UNSUPPORTED | 0 | 0 |
| U-31 | UNSUPPORTED | 0 | 0 |
| U-32 | UNSUPPORTED | 0 | 0 |
| U-33 | UNSUPPORTED | 0 | 0 |
| U-34 | UNSUPPORTED | 0 | 0 |
| U-35 | UNSUPPORTED | 0 | 0 |
| U-36 | UNSUPPORTED | 0 | 0 |
| U-37 | UNSUPPORTED | 0 | 0 |
| U-38 | UNSUPPORTED | 0 | 0 |
| U-39 | UNSUPPORTED | 89 | 159 |
| U-40 | UNSUPPORTED | 75 | 157 |

## Top Errors

No conversion failures were observed.

## Failure Samples

No conversion failure samples.
