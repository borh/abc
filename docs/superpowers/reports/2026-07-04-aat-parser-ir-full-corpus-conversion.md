# Full-Corpus AAT Parser-IR Conversion Audit

- generated_unix_seconds: `1783144145`
- mapping: `https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe` `0.2.1`
- mapping_hash: `sha256:b508665af72c237fc60f00b720f80db2b16148aa64b5d1cc723a2948ee576390`
- mapping_schema_hash: `sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4`
- target_parser_ir_schema_hash: `sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396`

## Totals

| files_attempted | files_succeeded | files_failed | parser_ir_nodes | divergence_records | divergence_occurrences | elapsed_seconds |
|---:|---:|---:|---:|---:|---:|---:|
| 53427 | 53427 | 0 | 26914048 | 784485 | 55027637 | 16.785 |

## Inputs

| label | files | aat_dir |
|---|---:|---|
| aozora-rs-adapter | 17894 | `/home/bor/Projects/ab-validator/scratch/morph-full-corpus/aats/aozora-rs-adapter` |
| aozora2html-adapter | 17689 | `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter` |
| aozora-epub3-adapter | 17844 | `/db/ab-validator/aat-corpus/aozora-epub3-full-20260704T050652Z-300s/aat/aozora-epub3-adapter` |

## Corpus Results

| corpus | attempted | succeeded | failed | parser_ir_nodes | divergence_occurrences |
|---|---:|---:|---:|---:|---:|
| aozora-epub3-adapter | 17844 | 17844 | 0 | 10670874 | 24679754 |
| aozora-rs-adapter | 17894 | 17894 | 0 | 7828615 | 13211106 |
| aozora2html-adapter | 17689 | 17689 | 0 | 8414559 | 17136777 |

## Divergence Categories

| category | occurrences |
|---|---:|
| AMBIGUITY | 33909733 |
| INVENTION | 10691975 |
| LOSS | 3815460 |
| STRUCTURAL | 6583005 |
| UNSUPPORTED | 27464 |

## Compatibility Candidates

| adapter | adapter_version | mapping_version | mapping_hash | files_succeeded | files_failed | rules_emitted | rules_missing | unsupported_occurrences |
|---|---|---|---|---:|---:|---:|---:|---:|
| aozora-epub3 | aozora-epub3-adapter 0.1.0 AozoraEpub3-JDK21-1.3.4-jdk21 | 0.2.1 | `sha256:b508665af72c237fc60f00b720f80db2b16148aa64b5d1cc723a2948ee576390` | 17844 | 0 | 64 | 66 | 13234 |
| aozora-rs | aozora-rs-adapter 0.1.0 2b4e8d1 | 0.2.1 | `sha256:b508665af72c237fc60f00b720f80db2b16148aa64b5d1cc723a2948ee576390` | 17894 | 0 | 26 | 104 | 0 |
| aozora2html | aozora2html-adapter 0.1.0 gem-3.0.1 | 0.2.1 | `sha256:b508665af72c237fc60f00b720f80db2b16148aa64b5d1cc723a2948ee576390` | 17689 | 0 | 115 | 15 | 14230 |

## Rule Coverage

- rules_total: `130`
- rules_emitted: `130`
- rules_missing: `0`

| rule_id | category | files | occurrences |
|---|---|---:|---:|
| A-01 | AMBIGUITY | 154 | 2531 |
| A-02 | AMBIGUITY | 32 | 100 |
| A-03 | AMBIGUITY | 18 | 20 |
| A-04 | AMBIGUITY | 168 | 2265 |
| A-05 | AMBIGUITY | 6 | 17 |
| A-06 | AMBIGUITY | 135 | 554 |
| A-07 | AMBIGUITY | 453 | 1887 |
| A-08 | AMBIGUITY | 11604 | 870800 |
| A-09 | AMBIGUITY | 4804 | 77062 |
| A-10 | AMBIGUITY | 2 | 2 |
| A-11 | AMBIGUITY | 66 | 411 |
| A-12 | AMBIGUITY | 11 | 20 |
| A-13 | AMBIGUITY | 6613 | 71397 |
| A-14 | AMBIGUITY | 13604 | 490531 |
| A-15 | AMBIGUITY | 3 | 39 |
| A-16 | AMBIGUITY | 49 | 345 |
| A-17 | AMBIGUITY | 306 | 2194 |
| A-18 | AMBIGUITY | 1 | 2 |
| A-19 | AMBIGUITY | 9192 | 76049 |
| A-20 | AMBIGUITY | 53139 | 25723554 |
| A-21 | AMBIGUITY | 23401 | 431767 |
| A-22 | AMBIGUITY | 3 | 4 |
| A-23 | AMBIGUITY | 180 | 1328 |
| A-24 | AMBIGUITY | 2 | 10 |
| A-25 | AMBIGUITY | 1651 | 13394 |
| A-26 | AMBIGUITY | 53293 | 6090018 |
| A-27 | AMBIGUITY | 5 | 5 |
| A-28 | AMBIGUITY | 53427 | 53427 |
| I-01 | INVENTION | 53427 | 53427 |
| I-02 | INVENTION | 42482 | 9635075 |
| I-03 | INVENTION | 53427 | 53427 |
| I-04 | INVENTION | 53427 | 53427 |
| I-05 | INVENTION | 16563 | 265128 |
| I-06 | INVENTION | 16563 | 265128 |
| I-07 | INVENTION | 53427 | 53427 |
| I-08 | INVENTION | 1 | 47 |
| I-09 | INVENTION | 27 | 556 |
| I-10 | INVENTION | 453 | 1887 |
| I-11 | INVENTION | 110 | 777 |
| I-12 | INVENTION | 484 | 3790 |
| I-13 | INVENTION | 9192 | 76049 |
| I-14 | INVENTION | 13616 | 229830 |
| L-01 | LOSS | 9339 | 77936 |
| L-02 | LOSS | 5 | 20 |
| L-03 | LOSS | 1 | 47 |
| L-04 | LOSS | 1 | 47 |
| L-05 | LOSS | 1 | 47 |
| L-06 | LOSS | 19 | 74 |
| L-07 | LOSS | 1 | 1 |
| L-08 | LOSS | 1 | 1 |
| L-09 | LOSS | 1 | 1 |
| L-10 | LOSS | 18 | 20 |
| L-11 | LOSS | 1 | 1 |
| L-12 | LOSS | 3 | 10 |
| L-13 | LOSS | 1 | 1 |
| L-14 | LOSS | 27 | 53 |
| L-15 | LOSS | 2 | 2 |
| L-16 | LOSS | 136 | 772 |
| L-17 | LOSS | 390 | 3687 |
| L-18 | LOSS | 27 | 556 |
| L-19 | LOSS | 22 | 551 |
| L-20 | LOSS | 22 | 551 |
| L-21 | LOSS | 430 | 1738 |
| L-22 | LOSS | 2859 | 119277 |
| L-23 | LOSS | 6 | 9 |
| L-24 | LOSS | 11 | 20 |
| L-25 | LOSS | 1 | 1 |
| L-26 | LOSS | 22 | 117 |
| L-27 | LOSS | 22 | 117 |
| L-28 | LOSS | 518 | 5987 |
| L-29 | LOSS | 518 | 5987 |
| L-30 | LOSS | 47 | 103 |
| L-31 | LOSS | 58 | 481 |
| L-32 | LOSS | 6613 | 71397 |
| L-33 | LOSS | 1 | 8 |
| L-34 | LOSS | 8 | 20 |
| L-35 | LOSS | 15 | 130 |
| L-36 | LOSS | 58 | 375 |
| L-37 | LOSS | 8 | 21 |
| L-38 | LOSS | 103 | 1934 |
| L-39 | LOSS | 1054 | 40442 |
| L-40 | LOSS | 112 | 618 |
| L-41 | LOSS | 484 | 3790 |
| L-42 | LOSS | 437 | 3662 |
| L-43 | LOSS | 437 | 3662 |
| L-44 | LOSS | 3482 | 18927 |
| L-45 | LOSS | 16819 | 3334020 |
| L-46 | LOSS | 2 | 10 |
| L-47 | LOSS | 2 | 5 |
| L-48 | LOSS | 2 | 5 |
| L-49 | LOSS | 60 | 352 |
| L-50 | LOSS | 60 | 352 |
| L-51 | LOSS | 9 | 17 |
| L-52 | LOSS | 8 | 73 |
| L-53 | LOSS | 1651 | 13394 |
| L-54 | LOSS | 17894 | 17894 |
| L-55 | LOSS | 53427 | 53427 |
| L-56 | LOSS | 32710 | 32710 |
| S-01 | STRUCTURAL | 18 | 20 |
| S-02 | STRUCTURAL | 154 | 2245 |
| S-03 | STRUCTURAL | 6613 | 71397 |
| S-04 | STRUCTURAL | 110 | 777 |
| S-05 | STRUCTURAL | 70 | 132 |
| S-06 | STRUCTURAL | 11604 | 418357 |
| S-07 | STRUCTURAL | 1651 | 13394 |
| S-08 | STRUCTURAL | 13616 | 229830 |
| S-09 | STRUCTURAL | 34 | 58 |
| S-10 | STRUCTURAL | 53139 | 5846794 |
| S-11 | STRUCTURAL | 1 | 1 |
| U-01 | UNSUPPORTED | 12 | 93 |
| U-02 | UNSUPPORTED | 3 | 9 |
| U-03 | UNSUPPORTED | 2 | 3 |
| U-04 | UNSUPPORTED | 1 | 1 |
| U-05 | UNSUPPORTED | 1 | 2 |
| U-06 | UNSUPPORTED | 8 | 54 |
| U-07 | UNSUPPORTED | 346 | 5828 |
| U-08 | UNSUPPORTED | 30 | 165 |
| U-09 | UNSUPPORTED | 156 | 2723 |
| U-10 | UNSUPPORTED | 66 | 411 |
| U-11 | UNSUPPORTED | 1 | 1 |
| U-12 | UNSUPPORTED | 70 | 132 |
| U-13 | UNSUPPORTED | 8 | 61 |
| U-14 | UNSUPPORTED | 2 | 2 |
| U-15 | UNSUPPORTED | 15 | 139 |
| U-16 | UNSUPPORTED | 557 | 5990 |
| U-17 | UNSUPPORTED | 47 | 152 |
| U-18 | UNSUPPORTED | 448 | 10311 |
| U-19 | UNSUPPORTED | 180 | 1328 |
| U-20 | UNSUPPORTED | 34 | 58 |
| U-21 | UNSUPPORTED | 1 | 1 |

## Top Errors

No conversion failures were observed.

## Failure Samples

No conversion failure samples.
