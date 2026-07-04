# Full-Corpus AAT Parser-IR Conversion Audit

- generated_unix_seconds: `1783127952`
- mapping: `https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe` `0.2.0`
- mapping_hash: `sha256:68b0868b25f3b072a47d781099178bf2a31e4b16c561814f5e13e3801714d089`
- mapping_schema_hash: `sha256:38ec7f0e5affb10329b550a091cd3a6fb5a25e26fd469dfe9f8249970cf9adb4`
- target_parser_ir_schema_hash: `sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396`

## Totals

| files_attempted | files_succeeded | files_failed | parser_ir_nodes | divergence_records | divergence_occurrences | elapsed_seconds |
|---:|---:|---:|---:|---:|---:|---:|
| 35583 | 35583 | 0 | 16243174 | 541167 | 30347883 | 10.660 |

## Inputs

| label | files | aat_dir |
|---|---:|---|
| aozora-rs-adapter | 17894 | `/home/bor/Projects/ab-validator/.worktrees/parser-ir-identity-compat-hardening/scratch/morph-full-corpus/aats/aozora-rs-adapter` |
| aozora2html-adapter | 17689 | `/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter` |

## Corpus Results

| corpus | attempted | succeeded | failed | parser_ir_nodes | divergence_occurrences |
|---|---:|---:|---:|---:|---:|
| aozora-rs-adapter | 17894 | 17894 | 0 | 7828615 | 13211106 |
| aozora2html-adapter | 17689 | 17689 | 0 | 8414559 | 17136777 |

## Divergence Categories

| category | occurrences |
|---|---:|
| AMBIGUITY | 19819114 |
| INVENTION | 7043185 |
| LOSS | 273503 |
| STRUCTURAL | 3197851 |
| UNSUPPORTED | 14230 |

## Compatibility Candidates

| adapter | adapter_version | mapping_version | mapping_hash | files_succeeded | files_failed | rules_emitted | rules_missing | unsupported_occurrences |
|---|---|---|---|---:|---:|---:|---:|---:|
| aozora-rs | aozora-rs-adapter 0.1.0 2b4e8d1 | 0.2.0 | `sha256:68b0868b25f3b072a47d781099178bf2a31e4b16c561814f5e13e3801714d089` | 17894 | 0 | 26 | 90 | 0 |
| aozora2html | aozora2html-adapter 0.1.0 gem-3.0.1 | 0.2.0 | `sha256:68b0868b25f3b072a47d781099178bf2a31e4b16c561814f5e13e3801714d089` | 17689 | 0 | 115 | 1 | 14230 |

## Rule Coverage

- rules_total: `116`
- rules_emitted: `116`
- rules_missing: `0`

| rule_id | category | files | occurrences |
|---|---|---:|---:|
| A-01 | AMBIGUITY | 69 | 465 |
| A-02 | AMBIGUITY | 20 | 59 |
| A-03 | AMBIGUITY | 4 | 4 |
| A-04 | AMBIGUITY | 71 | 402 |
| A-05 | AMBIGUITY | 6 | 17 |
| A-06 | AMBIGUITY | 135 | 554 |
| A-07 | AMBIGUITY | 453 | 1887 |
| A-08 | AMBIGUITY | 6501 | 321722 |
| A-09 | AMBIGUITY | 4064 | 67542 |
| A-10 | AMBIGUITY | 2 | 2 |
| A-11 | AMBIGUITY | 66 | 411 |
| A-12 | AMBIGUITY | 11 | 20 |
| A-13 | AMBIGUITY | 3104 | 7564 |
| A-14 | AMBIGUITY | 6531 | 163689 |
| A-15 | AMBIGUITY | 3 | 39 |
| A-16 | AMBIGUITY | 49 | 345 |
| A-17 | AMBIGUITY | 306 | 2194 |
| A-18 | AMBIGUITY | 1 | 2 |
| A-19 | AMBIGUITY | 9192 | 76049 |
| A-20 | AMBIGUITY | 35471 | 15806117 |
| A-21 | AMBIGUITY | 17005 | 290131 |
| A-22 | AMBIGUITY | 3 | 4 |
| A-23 | AMBIGUITY | 180 | 1328 |
| A-24 | AMBIGUITY | 2 | 10 |
| A-25 | AMBIGUITY | 1379 | 9210 |
| A-26 | AMBIGUITY | 35478 | 3033760 |
| A-27 | AMBIGUITY | 4 | 4 |
| A-28 | AMBIGUITY | 35583 | 35583 |
| I-01 | INVENTION | 35583 | 35583 |
| I-02 | INVENTION | 28285 | 6192284 |
| I-03 | INVENTION | 35583 | 35583 |
| I-04 | INVENTION | 35583 | 35583 |
| I-05 | INVENTION | 16534 | 265099 |
| I-06 | INVENTION | 16534 | 265099 |
| I-07 | INVENTION | 35583 | 35583 |
| I-08 | INVENTION | 1 | 47 |
| I-09 | INVENTION | 27 | 556 |
| I-10 | INVENTION | 453 | 1887 |
| I-11 | INVENTION | 71 | 401 |
| I-12 | INVENTION | 447 | 3485 |
| I-13 | INVENTION | 9192 | 76049 |
| I-14 | INVENTION | 6531 | 95946 |
| L-01 | LOSS | 9339 | 77936 |
| L-02 | LOSS | 5 | 20 |
| L-03 | LOSS | 1 | 47 |
| L-04 | LOSS | 1 | 47 |
| L-05 | LOSS | 1 | 47 |
| L-06 | LOSS | 1 | 1 |
| L-07 | LOSS | 4 | 4 |
| L-08 | LOSS | 1 | 1 |
| L-09 | LOSS | 3 | 10 |
| L-10 | LOSS | 1 | 1 |
| L-11 | LOSS | 27 | 53 |
| L-12 | LOSS | 2 | 2 |
| L-13 | LOSS | 136 | 772 |
| L-14 | LOSS | 372 | 3612 |
| L-15 | LOSS | 27 | 556 |
| L-16 | LOSS | 22 | 551 |
| L-17 | LOSS | 22 | 551 |
| L-18 | LOSS | 430 | 1738 |
| L-19 | LOSS | 228 | 575 |
| L-20 | LOSS | 6 | 9 |
| L-21 | LOSS | 11 | 20 |
| L-22 | LOSS | 1 | 1 |
| L-23 | LOSS | 22 | 117 |
| L-24 | LOSS | 22 | 117 |
| L-25 | LOSS | 130 | 615 |
| L-26 | LOSS | 130 | 615 |
| L-27 | LOSS | 10 | 37 |
| L-28 | LOSS | 3104 | 7564 |
| L-29 | LOSS | 1 | 8 |
| L-30 | LOSS | 8 | 20 |
| L-31 | LOSS | 15 | 130 |
| L-32 | LOSS | 58 | 375 |
| L-33 | LOSS | 8 | 21 |
| L-34 | LOSS | 103 | 1934 |
| L-35 | LOSS | 1014 | 40104 |
| L-36 | LOSS | 112 | 618 |
| L-37 | LOSS | 447 | 3485 |
| L-38 | LOSS | 400 | 3357 |
| L-39 | LOSS | 400 | 3357 |
| L-40 | LOSS | 3482 | 18927 |
| L-41 | LOSS | 2956 | 10004 |
| L-42 | LOSS | 2 | 10 |
| L-43 | LOSS | 2 | 5 |
| L-44 | LOSS | 2 | 5 |
| L-45 | LOSS | 12 | 62 |
| L-46 | LOSS | 12 | 62 |
| L-47 | LOSS | 2 | 3 |
| L-48 | LOSS | 1379 | 9210 |
| L-49 | LOSS | 17894 | 17894 |
| L-50 | LOSS | 35583 | 35583 |
| L-51 | LOSS | 32710 | 32710 |
| S-01 | STRUCTURAL | 4 | 4 |
| S-02 | STRUCTURAL | 69 | 398 |
| S-03 | STRUCTURAL | 3104 | 7564 |
| S-04 | STRUCTURAL | 71 | 401 |
| S-05 | STRUCTURAL | 6501 | 155724 |
| S-06 | STRUCTURAL | 1379 | 9210 |
| S-07 | STRUCTURAL | 6531 | 95946 |
| S-08 | STRUCTURAL | 35471 | 2928604 |
| U-01 | UNSUPPORTED | 12 | 93 |
| U-02 | UNSUPPORTED | 2 | 3 |
| U-03 | UNSUPPORTED | 1 | 1 |
| U-04 | UNSUPPORTED | 1 | 2 |
| U-05 | UNSUPPORTED | 8 | 54 |
| U-06 | UNSUPPORTED | 346 | 5828 |
| U-07 | UNSUPPORTED | 30 | 165 |
| U-08 | UNSUPPORTED | 66 | 411 |
| U-09 | UNSUPPORTED | 1 | 1 |
| U-10 | UNSUPPORTED | 8 | 61 |
| U-11 | UNSUPPORTED | 2 | 2 |
| U-12 | UNSUPPORTED | 15 | 139 |
| U-13 | UNSUPPORTED | 557 | 5990 |
| U-14 | UNSUPPORTED | 47 | 152 |
| U-15 | UNSUPPORTED | 180 | 1328 |

## Top Errors

No conversion failures were observed.

## Failure Samples

No conversion failure samples.
