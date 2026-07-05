# Generated Parser-IR TEI vs TEI-EAJ Workset Audit

This report materializes ABC TEI from parser-IR, then compares the generated TEI structure against pinned TEI-EAJ XML. It measures structural deltas; it does not treat TEI-EAJ Level 4 enrichment as parser-IR admission scope.

## Totals

| metric | value |
|---|---:|
| rows_attempted | 285 |
| tei_eaj_rows_attempted | 57 |
| materialization_succeeded | 285 |
| materialization_failed | 0 |
| rows_skipped | 5 |

## Paragraph Delta Buckets

| bucket | rows |
|---|---:|
| collapsed | 67 |
| exact | 60 |
| over_split | 134 |
| under_split | 24 |

## Paragraph Origin Buckets

| bucket | rows |
|---|---:|
| adapter_collapsed | 67 |
| adapter_over_segmented | 127 |
| adapter_under_segmented | 14 |
| aligned | 60 |
| converter_paragraph_mismatch | 10 |
| page_break_projection | 3 |
| source_note_back_routing | 4 |

## Body Text Relation Buckets

| relation | rows |
|---|---:|
| different | 271 |
| equal | 7 |
| generated_contains_tei_eaj | 6 |
| tei_eaj_contains_generated | 1 |

## Body Text Match Buckets

| bucket | rows |
|---|---:|
| base_drop_parentheticals_equal | 4 |
| base_equal | 7 |
| different | 187 |
| ruby_expanded_equal | 4 |
| ruby_expanded_parenless_equal | 65 |
| ruby_expanded_parenless_generated_contains_tei_eaj | 14 |
| ruby_expanded_parenless_tei_eaj_contains_generated | 4 |

## TEI-EAJ Structure Profile Buckets

| profile | rows |
|---|---:|
| drama | 50 |
| front_back_matter | 10 |
| lv4_enrichment | 45 |
| notes | 35 |
| plain_prose | 135 |
| verse | 10 |

## Paragraph Origin by TEI-EAJ Profile

| profile | origin | rows |
|---|---|---:|
| drama | adapter_collapsed | 21 |
| drama | adapter_over_segmented | 12 |
| drama | adapter_under_segmented | 7 |
| drama | aligned | 1 |
| drama | converter_paragraph_mismatch | 7 |
| drama | page_break_projection | 2 |
| front_back_matter | adapter_collapsed | 2 |
| front_back_matter | adapter_over_segmented | 5 |
| front_back_matter | aligned | 3 |
| lv4_enrichment | adapter_collapsed | 15 |
| lv4_enrichment | adapter_over_segmented | 18 |
| lv4_enrichment | adapter_under_segmented | 1 |
| lv4_enrichment | aligned | 7 |
| lv4_enrichment | source_note_back_routing | 4 |
| notes | adapter_collapsed | 15 |
| notes | adapter_over_segmented | 14 |
| notes | adapter_under_segmented | 4 |
| notes | aligned | 2 |
| plain_prose | adapter_collapsed | 10 |
| plain_prose | adapter_over_segmented | 75 |
| plain_prose | adapter_under_segmented | 1 |
| plain_prose | aligned | 47 |
| plain_prose | converter_paragraph_mismatch | 2 |
| verse | adapter_collapsed | 4 |
| verse | adapter_over_segmented | 3 |
| verse | adapter_under_segmented | 1 |
| verse | converter_paragraph_mismatch | 1 |
| verse | page_break_projection | 1 |

## Adapter Paragraph Delta Buckets

| adapter | bucket | rows |
|---|---|---:|
| aozora | collapsed | 32 |
| aozora | exact | 23 |
| aozora | under_split | 2 |
| aozora-epub3 | exact | 7 |
| aozora-epub3 | over_split | 48 |
| aozora-epub3 | under_split | 2 |
| aozora-rs | collapsed | 16 |
| aozora-rs | exact | 6 |
| aozora-rs | over_split | 35 |
| aozora2 | collapsed | 19 |
| aozora2 | exact | 19 |
| aozora2 | over_split | 6 |
| aozora2 | under_split | 13 |
| aozora2html | exact | 5 |
| aozora2html | over_split | 45 |
| aozora2html | under_split | 7 |

## Adapter Paragraph Origin Buckets

| adapter | bucket | rows |
|---|---|---:|
| aozora | adapter_collapsed | 32 |
| aozora | aligned | 23 |
| aozora | converter_paragraph_mismatch | 2 |
| aozora-epub3 | adapter_over_segmented | 46 |
| aozora-epub3 | adapter_under_segmented | 2 |
| aozora-epub3 | aligned | 7 |
| aozora-epub3 | source_note_back_routing | 2 |
| aozora-rs | adapter_collapsed | 16 |
| aozora-rs | adapter_over_segmented | 35 |
| aozora-rs | aligned | 6 |
| aozora2 | adapter_collapsed | 19 |
| aozora2 | adapter_over_segmented | 4 |
| aozora2 | adapter_under_segmented | 7 |
| aozora2 | aligned | 19 |
| aozora2 | converter_paragraph_mismatch | 8 |
| aozora2html | adapter_over_segmented | 42 |
| aozora2html | adapter_under_segmented | 5 |
| aozora2html | aligned | 5 |
| aozora2html | page_break_projection | 3 |
| aozora2html | source_note_back_routing | 2 |

## Adapter Body Text Match Buckets

| adapter | bucket | rows |
|---|---|---:|
| aozora | different | 56 |
| aozora | ruby_expanded_parenless_tei_eaj_contains_generated | 1 |
| aozora-epub3 | base_equal | 1 |
| aozora-epub3 | different | 50 |
| aozora-epub3 | ruby_expanded_equal | 1 |
| aozora-epub3 | ruby_expanded_parenless_equal | 4 |
| aozora-epub3 | ruby_expanded_parenless_generated_contains_tei_eaj | 1 |
| aozora-rs | base_drop_parentheticals_equal | 2 |
| aozora-rs | base_equal | 2 |
| aozora-rs | different | 28 |
| aozora-rs | ruby_expanded_equal | 1 |
| aozora-rs | ruby_expanded_parenless_equal | 21 |
| aozora-rs | ruby_expanded_parenless_generated_contains_tei_eaj | 3 |
| aozora2 | base_drop_parentheticals_equal | 2 |
| aozora2 | base_equal | 1 |
| aozora2 | different | 29 |
| aozora2 | ruby_expanded_equal | 1 |
| aozora2 | ruby_expanded_parenless_equal | 20 |
| aozora2 | ruby_expanded_parenless_generated_contains_tei_eaj | 4 |
| aozora2html | base_equal | 3 |
| aozora2html | different | 24 |
| aozora2html | ruby_expanded_equal | 1 |
| aozora2html | ruby_expanded_parenless_equal | 20 |
| aozora2html | ruby_expanded_parenless_generated_contains_tei_eaj | 6 |
| aozora2html | ruby_expanded_parenless_tei_eaj_contains_generated | 3 |

## Rows

| work_id | TEI-EAJ file | adapter | AAT p | parser-IR p | generated body p | TEI-EAJ body p | delta | bucket | origin | base text | best text match | source note |
|---|---|---|---:|---:|---:|---:|---:|---|---|---|---|---|
| 1126 | `data/complete/tei_lib_lv3/1126_tei.xml` | aozora2html | 30 | 30 | 30 | 169 | -139 | under_split | adapter_under_segmented | different | different | - |
| 1126 | `data/complete/tei_lib_lv3/1126_tei.xml` | aozora-epub3 | 154 | 154 | 154 | 169 | -15 | under_split | adapter_under_segmented | different | different | - |
| 1126 | `data/complete/tei_lib_lv3/1126_tei.xml` | aozora-rs | 1 | 1 | 1 | 169 | -168 | collapsed | adapter_collapsed | different | different | - |
| 1126 | `data/complete/tei_lib_lv3/1126_tei.xml` | aozora2 | 52 | 28 | 28 | 169 | -141 | under_split | converter_paragraph_mismatch | different | different | - |
| 1126 | `data/complete/tei_lib_lv3/1126_tei.xml` | aozora | 1 | 1 | 1 | 169 | -168 | collapsed | adapter_collapsed | different | different | - |
| 15099 | `data/complete/tei_lib_lv3/15099_tei.xml` | aozora2html | 62 | 62 | 62 | 1 | 61 | over_split | adapter_over_segmented | different | different | - |
| 15099 | `data/complete/tei_lib_lv3/15099_tei.xml` | aozora-epub3 | 62 | 62 | 62 | 1 | 61 | over_split | adapter_over_segmented | different | different | - |
| 15099 | `data/complete/tei_lib_lv3/15099_tei.xml` | aozora-rs | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 15099 | `data/complete/tei_lib_lv3/15099_tei.xml` | aozora2 | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 15099 | `data/complete/tei_lib_lv3/15099_tei.xml` | aozora | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 15938 | `data/complete/tei_lib_lv3/15938_tei.xml` | aozora2html | 8 | 8 | 8 | 6 | 2 | over_split | adapter_over_segmented | different | different | - |
| 15938 | `data/complete/tei_lib_lv3/15938_tei.xml` | aozora-epub3 | 8 | 8 | 8 | 6 | 2 | over_split | adapter_over_segmented | different | different | - |
| 15938 | `data/complete/tei_lib_lv3/15938_tei.xml` | aozora-rs | 15 | 15 | 15 | 6 | 9 | over_split | adapter_over_segmented | different | different | - |
| 15938 | `data/complete/tei_lib_lv3/15938_tei.xml` | aozora2 | 1 | 1 | 1 | 6 | -5 | collapsed | adapter_collapsed | different | different | - |
| 15938 | `data/complete/tei_lib_lv3/15938_tei.xml` | aozora | 1 | 0 | 0 | 6 | -6 | under_split | converter_paragraph_mismatch | different | different | - |
| 236 | `data/complete/tei_lib_lv3/236_tei.xml` | aozora2html | 29 | 29 | 29 | 1 | 28 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 236 | `data/complete/tei_lib_lv3/236_tei.xml` | aozora-epub3 | 29 | 29 | 29 | 1 | 28 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 236 | `data/complete/tei_lib_lv3/236_tei.xml` | aozora-rs | 33 | 33 | 33 | 1 | 32 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 236 | `data/complete/tei_lib_lv3/236_tei.xml` | aozora2 | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | ruby_expanded_parenless_equal | - |
| 236 | `data/complete/tei_lib_lv3/236_tei.xml` | aozora | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 2509 | `data/complete/tei_lib_lv3/2509_tei.xml` | aozora2html | 28 | 28 | 28 | 28 | 0 | exact | aligned | different | different | - |
| 2509 | `data/complete/tei_lib_lv3/2509_tei.xml` | aozora-epub3 | 28 | 28 | 28 | 28 | 0 | exact | aligned | different | different | - |
| 2509 | `data/complete/tei_lib_lv3/2509_tei.xml` | aozora-rs | 1 | 1 | 1 | 28 | -27 | collapsed | adapter_collapsed | different | different | - |
| 2509 | `data/complete/tei_lib_lv3/2509_tei.xml` | aozora2 | 1 | 1 | 1 | 28 | -27 | collapsed | adapter_collapsed | different | different | - |
| 2509 | `data/complete/tei_lib_lv3/2509_tei.xml` | aozora | 1 | 1 | 1 | 28 | -27 | collapsed | adapter_collapsed | different | different | - |
| 42320 | `data/complete/tei_lib_lv3/42320_tei.xml` | aozora2html | 4 | 4 | 4 | 3 | 1 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_generated_contains_tei_eaj | - |
| 42320 | `data/complete/tei_lib_lv3/42320_tei.xml` | aozora-epub3 | 4 | 4 | 4 | 3 | 1 | over_split | adapter_over_segmented | different | different | - |
| 42320 | `data/complete/tei_lib_lv3/42320_tei.xml` | aozora-rs | 8 | 8 | 8 | 3 | 5 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_generated_contains_tei_eaj | - |
| 42320 | `data/complete/tei_lib_lv3/42320_tei.xml` | aozora2 | 1 | 1 | 1 | 3 | -2 | collapsed | adapter_collapsed | different | ruby_expanded_parenless_generated_contains_tei_eaj | - |
| 42320 | `data/complete/tei_lib_lv3/42320_tei.xml` | aozora | 1 | 1 | 1 | 3 | -2 | collapsed | adapter_collapsed | different | different | - |
| 43077 | `data/complete/tei_lib_lv3/43077_tei.xml` | aozora2html | 32 | 32 | 32 | 30 | 2 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_tei_eaj_contains_generated | - |
| 43077 | `data/complete/tei_lib_lv3/43077_tei.xml` | aozora-epub3 | 33 | 33 | 33 | 30 | 3 | over_split | adapter_over_segmented | different | different | - |
| 43077 | `data/complete/tei_lib_lv3/43077_tei.xml` | aozora-rs | 40 | 40 | 40 | 30 | 10 | over_split | adapter_over_segmented | different | different | - |
| 43077 | `data/complete/tei_lib_lv3/43077_tei.xml` | aozora2 | 4 | 4 | 4 | 30 | -26 | under_split | adapter_under_segmented | different | different | - |
| 43077 | `data/complete/tei_lib_lv3/43077_tei.xml` | aozora | 1 | 1 | 1 | 30 | -29 | collapsed | adapter_collapsed | different | different | - |
| 43563 | `data/complete/tei_lib_lv3/43563_tei.xml` | aozora2html | 186 | 186 | 186 | 26 | 160 | over_split | adapter_over_segmented | different | different | - |
| 43563 | `data/complete/tei_lib_lv3/43563_tei.xml` | aozora-epub3 | 186 | 186 | 186 | 26 | 160 | over_split | adapter_over_segmented | different | different | - |
| 43563 | `data/complete/tei_lib_lv3/43563_tei.xml` | aozora-rs | 202 | 202 | 202 | 26 | 176 | over_split | adapter_over_segmented | different | different | - |
| 43563 | `data/complete/tei_lib_lv3/43563_tei.xml` | aozora2 | 1 | 1 | 1 | 26 | -25 | collapsed | adapter_collapsed | different | different | - |
| 43563 | `data/complete/tei_lib_lv3/43563_tei.xml` | aozora | 1 | 1 | 1 | 26 | -25 | collapsed | adapter_collapsed | different | different | - |
| 4411 | `data/complete/tei_lib_lv3/4411_tei.xml` | aozora2html | 6 | 6 | 6 | 2 | 4 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 4411 | `data/complete/tei_lib_lv3/4411_tei.xml` | aozora-epub3 | 12 | 12 | 12 | 2 | 10 | over_split | adapter_over_segmented | different | different | - |
| 4411 | `data/complete/tei_lib_lv3/4411_tei.xml` | aozora-rs | 12 | 12 | 12 | 2 | 10 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 4411 | `data/complete/tei_lib_lv3/4411_tei.xml` | aozora2 | 1 | 1 | 1 | 2 | -1 | collapsed | adapter_collapsed | different | ruby_expanded_parenless_equal | - |
| 4411 | `data/complete/tei_lib_lv3/4411_tei.xml` | aozora | 1 | 1 | 1 | 2 | -1 | collapsed | adapter_collapsed | tei_eaj_contains_generated | ruby_expanded_parenless_tei_eaj_contains_generated | - |
| 4464 | `data/complete/tei_lib_lv3/4464_tei.xml` | aozora2html | 26 | 26 | 26 | 17 | 9 | over_split | adapter_over_segmented | different | different | - |
| 4464 | `data/complete/tei_lib_lv3/4464_tei.xml` | aozora-epub3 | 26 | 26 | 26 | 17 | 9 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 4464 | `data/complete/tei_lib_lv3/4464_tei.xml` | aozora-rs | 1 | 1 | 1 | 17 | -16 | collapsed | adapter_collapsed | different | ruby_expanded_parenless_equal | - |
| 4464 | `data/complete/tei_lib_lv3/4464_tei.xml` | aozora2 | 3 | 3 | 3 | 17 | -14 | under_split | adapter_under_segmented | different | ruby_expanded_parenless_equal | - |
| 4464 | `data/complete/tei_lib_lv3/4464_tei.xml` | aozora | 1 | 1 | 1 | 17 | -16 | collapsed | adapter_collapsed | different | different | - |
| 45093 | `data/complete/tei_lib_lv3/45093_tei.xml` | aozora2html | 18 | 18 | 18 | 6 | 12 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 45093 | `data/complete/tei_lib_lv3/45093_tei.xml` | aozora-epub3 | 18 | 18 | 18 | 6 | 12 | over_split | adapter_over_segmented | different | different | - |
| 45093 | `data/complete/tei_lib_lv3/45093_tei.xml` | aozora-rs | 27 | 27 | 27 | 6 | 21 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 45093 | `data/complete/tei_lib_lv3/45093_tei.xml` | aozora2 | 1 | 1 | 1 | 6 | -5 | collapsed | adapter_collapsed | different | ruby_expanded_parenless_equal | - |
| 45093 | `data/complete/tei_lib_lv3/45093_tei.xml` | aozora | 1 | 0 | 0 | 6 | -6 | under_split | converter_paragraph_mismatch | different | different | - |
| 4872 | `data/complete/tei_lib_lv3/4872_tei.xml` | aozora2html | 5 | 5 | 5 | 5 | 0 | exact | aligned | different | ruby_expanded_equal | - |
| 4872 | `data/complete/tei_lib_lv3/4872_tei.xml` | aozora-epub3 | 5 | 5 | 5 | 5 | 0 | exact | aligned | different | ruby_expanded_equal | - |
| 4872 | `data/complete/tei_lib_lv3/4872_tei.xml` | aozora-rs | 9 | 9 | 9 | 5 | 4 | over_split | adapter_over_segmented | different | ruby_expanded_equal | - |
| 4872 | `data/complete/tei_lib_lv3/4872_tei.xml` | aozora2 | 1 | 1 | 1 | 5 | -4 | collapsed | adapter_collapsed | different | ruby_expanded_equal | - |
| 4872 | `data/complete/tei_lib_lv3/4872_tei.xml` | aozora | 1 | 1 | 1 | 5 | -4 | collapsed | adapter_collapsed | different | different | - |
| 50502 | `data/complete/tei_lib_lv3/50502_tei.xml` | aozora2html | 8 | 8 | 8 | 8 | 0 | exact | aligned | different | different | - |
| 50502 | `data/complete/tei_lib_lv3/50502_tei.xml` | aozora-epub3 | 8 | 8 | 8 | 8 | 0 | exact | aligned | different | different | - |
| 50502 | `data/complete/tei_lib_lv3/50502_tei.xml` | aozora-rs | 1 | 1 | 1 | 8 | -7 | collapsed | adapter_collapsed | different | different | - |
| 50502 | `data/complete/tei_lib_lv3/50502_tei.xml` | aozora2 | 1 | 1 | 1 | 8 | -7 | collapsed | adapter_collapsed | different | different | - |
| 50502 | `data/complete/tei_lib_lv3/50502_tei.xml` | aozora | 1 | 1 | 1 | 8 | -7 | collapsed | adapter_collapsed | different | different | - |
| 51307 | `data/complete/tei_lib_lv3/51307_tei.xml` | aozora2html | 412 | 406 | 406 | 7 | 399 | over_split | page_break_projection | different | different | - |
| 51307 | `data/complete/tei_lib_lv3/51307_tei.xml` | aozora-epub3 | 400 | 400 | 400 | 7 | 393 | over_split | adapter_over_segmented | different | different | - |
| 51307 | `data/complete/tei_lib_lv3/51307_tei.xml` | aozora-rs | 1 | 1 | 1 | 7 | -6 | collapsed | adapter_collapsed | different | different | - |
| 51307 | `data/complete/tei_lib_lv3/51307_tei.xml` | aozora2 | 14 | 8 | 8 | 7 | 1 | over_split | converter_paragraph_mismatch | different | different | - |
| 51307 | `data/complete/tei_lib_lv3/51307_tei.xml` | aozora | 1 | 1 | 1 | 7 | -6 | collapsed | adapter_collapsed | different | different | - |
| 51520 | `data/complete/tei_lib_lv3/51520_tei.xml` | aozora2html | 76 | 76 | 76 | 76 | 0 | exact | aligned | different | ruby_expanded_parenless_equal | - |
| 51520 | `data/complete/tei_lib_lv3/51520_tei.xml` | aozora-epub3 | 76 | 76 | 76 | 76 | 0 | exact | aligned | different | different | - |
| 51520 | `data/complete/tei_lib_lv3/51520_tei.xml` | aozora-rs | 80 | 80 | 80 | 76 | 4 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 51520 | `data/complete/tei_lib_lv3/51520_tei.xml` | aozora2 | 1 | 1 | 1 | 76 | -75 | collapsed | adapter_collapsed | different | ruby_expanded_parenless_equal | - |
| 51520 | `data/complete/tei_lib_lv3/51520_tei.xml` | aozora | 1 | 1 | 1 | 76 | -75 | collapsed | adapter_collapsed | different | different | - |
| 53386 | `data/complete/tei_lib_lv3/53386_tei.xml` | aozora2html | 5 | 5 | 5 | 1 | 4 | over_split | adapter_over_segmented | generated_contains_tei_eaj | ruby_expanded_parenless_generated_contains_tei_eaj | - |
| 53386 | `data/complete/tei_lib_lv3/53386_tei.xml` | aozora-epub3 | 3 | 3 | 3 | 1 | 2 | over_split | adapter_over_segmented | equal | base_equal | - |
| 53386 | `data/complete/tei_lib_lv3/53386_tei.xml` | aozora-rs | 7 | 7 | 7 | 1 | 6 | over_split | adapter_over_segmented | equal | base_equal | - |
| 53386 | `data/complete/tei_lib_lv3/53386_tei.xml` | aozora2 | 1 | 1 | 1 | 1 | 0 | exact | aligned | generated_contains_tei_eaj | ruby_expanded_parenless_generated_contains_tei_eaj | - |
| 53386 | `data/complete/tei_lib_lv3/53386_tei.xml` | aozora | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 53617 | `data/complete/tei_lib_lv3/53617_tei.xml` | aozora2html | 20 | 20 | 20 | 1 | 19 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_generated_contains_tei_eaj | - |
| 53617 | `data/complete/tei_lib_lv3/53617_tei.xml` | aozora-epub3 | 20 | 20 | 20 | 1 | 19 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_generated_contains_tei_eaj | - |
| 53617 | `data/complete/tei_lib_lv3/53617_tei.xml` | aozora-rs | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | ruby_expanded_parenless_generated_contains_tei_eaj | - |
| 53617 | `data/complete/tei_lib_lv3/53617_tei.xml` | aozora2 | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | ruby_expanded_parenless_generated_contains_tei_eaj | - |
| 53617 | `data/complete/tei_lib_lv3/53617_tei.xml` | aozora | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 55783 | `data/complete/tei_lib_lv3/55783_tei.xml` | aozora2html | 16 | 16 | 16 | 126 | -110 | under_split | adapter_under_segmented | different | different | - |
| 55783 | `data/complete/tei_lib_lv3/55783_tei.xml` | aozora-epub3 | 127 | 127 | 127 | 126 | 1 | over_split | adapter_over_segmented | different | different | - |
| 55783 | `data/complete/tei_lib_lv3/55783_tei.xml` | aozora-rs | 1 | 1 | 1 | 126 | -125 | collapsed | adapter_collapsed | different | different | - |
| 55783 | `data/complete/tei_lib_lv3/55783_tei.xml` | aozora2 | 10 | 7 | 7 | 126 | -119 | under_split | converter_paragraph_mismatch | different | different | - |
| 55783 | `data/complete/tei_lib_lv3/55783_tei.xml` | aozora | 1 | 1 | 1 | 126 | -125 | collapsed | adapter_collapsed | different | different | - |
| 56996 | `data/complete/tei_lib_lv3/56996_tei.xml` | aozora2html | 8 | 8 | 8 | 1 | 7 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 56996 | `data/complete/tei_lib_lv3/56996_tei.xml` | aozora-epub3 | 9 | 9 | 9 | 1 | 8 | over_split | adapter_over_segmented | different | different | - |
| 56996 | `data/complete/tei_lib_lv3/56996_tei.xml` | aozora-rs | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | ruby_expanded_parenless_equal | - |
| 56996 | `data/complete/tei_lib_lv3/56996_tei.xml` | aozora2 | 3 | 3 | 3 | 1 | 2 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 56996 | `data/complete/tei_lib_lv3/56996_tei.xml` | aozora | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 56998 | `data/complete/tei_lib_lv3/56998_tei.xml` | aozora2html | 9 | 9 | 9 | 1 | 8 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 56998 | `data/complete/tei_lib_lv3/56998_tei.xml` | aozora-epub3 | 10 | 10 | 10 | 1 | 9 | over_split | adapter_over_segmented | different | different | - |
| 56998 | `data/complete/tei_lib_lv3/56998_tei.xml` | aozora-rs | 16 | 16 | 16 | 1 | 15 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 56998 | `data/complete/tei_lib_lv3/56998_tei.xml` | aozora2 | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | ruby_expanded_parenless_equal | - |
| 56998 | `data/complete/tei_lib_lv3/56998_tei.xml` | aozora | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 56999 | `data/complete/tei_lib_lv3/56999_tei.xml` | aozora2html | 9 | 9 | 9 | 8 | 1 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_generated_contains_tei_eaj | - |
| 56999 | `data/complete/tei_lib_lv3/56999_tei.xml` | aozora-epub3 | 10 | 10 | 10 | 8 | 2 | over_split | adapter_over_segmented | different | different | - |
| 56999 | `data/complete/tei_lib_lv3/56999_tei.xml` | aozora-rs | 17 | 17 | 17 | 8 | 9 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_generated_contains_tei_eaj | - |
| 56999 | `data/complete/tei_lib_lv3/56999_tei.xml` | aozora2 | 1 | 1 | 1 | 8 | -7 | collapsed | adapter_collapsed | different | ruby_expanded_parenless_generated_contains_tei_eaj | - |
| 56999 | `data/complete/tei_lib_lv3/56999_tei.xml` | aozora | 1 | 1 | 1 | 8 | -7 | collapsed | adapter_collapsed | different | different | - |
| 57001 | `data/complete/tei_lib_lv3/57001_tei.xml` | aozora2html | 13 | 13 | 13 | 1 | 12 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57001 | `data/complete/tei_lib_lv3/57001_tei.xml` | aozora-epub3 | 14 | 14 | 14 | 1 | 13 | over_split | adapter_over_segmented | different | different | - |
| 57001 | `data/complete/tei_lib_lv3/57001_tei.xml` | aozora-rs | 20 | 20 | 20 | 1 | 19 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57001 | `data/complete/tei_lib_lv3/57001_tei.xml` | aozora2 | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | ruby_expanded_parenless_equal | - |
| 57001 | `data/complete/tei_lib_lv3/57001_tei.xml` | aozora | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 57002 | `data/complete/tei_lib_lv3/57002_tei.xml` | aozora2html | 7 | 7 | 7 | 1 | 6 | over_split | adapter_over_segmented | equal | base_equal | - |
| 57002 | `data/complete/tei_lib_lv3/57002_tei.xml` | aozora-epub3 | 8 | 8 | 8 | 1 | 7 | over_split | adapter_over_segmented | different | different | - |
| 57002 | `data/complete/tei_lib_lv3/57002_tei.xml` | aozora-rs | 14 | 14 | 14 | 1 | 13 | over_split | adapter_over_segmented | equal | base_equal | - |
| 57002 | `data/complete/tei_lib_lv3/57002_tei.xml` | aozora2 | 1 | 1 | 1 | 1 | 0 | exact | aligned | equal | base_equal | - |
| 57002 | `data/complete/tei_lib_lv3/57002_tei.xml` | aozora | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 57003 | `data/complete/tei_lib_lv3/57003_tei.xml` | aozora2html | 5 | 5 | 5 | 1 | 4 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57003 | `data/complete/tei_lib_lv3/57003_tei.xml` | aozora-epub3 | 6 | 6 | 6 | 1 | 5 | over_split | adapter_over_segmented | different | different | - |
| 57003 | `data/complete/tei_lib_lv3/57003_tei.xml` | aozora-rs | 12 | 12 | 12 | 1 | 11 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57003 | `data/complete/tei_lib_lv3/57003_tei.xml` | aozora2 | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | ruby_expanded_parenless_equal | - |
| 57003 | `data/complete/tei_lib_lv3/57003_tei.xml` | aozora | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 57004 | `data/complete/tei_lib_lv3/57004_tei.xml` | aozora2html | 11 | 11 | 11 | 1 | 10 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57004 | `data/complete/tei_lib_lv3/57004_tei.xml` | aozora-epub3 | 12 | 12 | 12 | 1 | 11 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57004 | `data/complete/tei_lib_lv3/57004_tei.xml` | aozora-rs | 18 | 18 | 18 | 1 | 17 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57004 | `data/complete/tei_lib_lv3/57004_tei.xml` | aozora2 | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | ruby_expanded_parenless_equal | - |
| 57004 | `data/complete/tei_lib_lv3/57004_tei.xml` | aozora | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 57005 | `data/complete/tei_lib_lv3/57005_tei.xml` | aozora2html | 13 | 13 | 13 | 1 | 12 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57005 | `data/complete/tei_lib_lv3/57005_tei.xml` | aozora-epub3 | 14 | 14 | 14 | 1 | 13 | over_split | adapter_over_segmented | different | different | - |
| 57005 | `data/complete/tei_lib_lv3/57005_tei.xml` | aozora-rs | 20 | 20 | 20 | 1 | 19 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57005 | `data/complete/tei_lib_lv3/57005_tei.xml` | aozora2 | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | ruby_expanded_parenless_equal | - |
| 57005 | `data/complete/tei_lib_lv3/57005_tei.xml` | aozora | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 57006 | `data/complete/tei_lib_lv3/57006_tei.xml` | aozora2html | 10 | 10 | 10 | 1 | 9 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57006 | `data/complete/tei_lib_lv3/57006_tei.xml` | aozora-epub3 | 11 | 11 | 11 | 1 | 10 | over_split | adapter_over_segmented | different | different | - |
| 57006 | `data/complete/tei_lib_lv3/57006_tei.xml` | aozora-rs | 17 | 17 | 17 | 1 | 16 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57006 | `data/complete/tei_lib_lv3/57006_tei.xml` | aozora2 | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | ruby_expanded_parenless_equal | - |
| 57006 | `data/complete/tei_lib_lv3/57006_tei.xml` | aozora | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 57037 | `data/complete/tei_lib_lv3/57037_tei.xml` | aozora2html | 10 | 10 | 10 | 1 | 9 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57037 | `data/complete/tei_lib_lv3/57037_tei.xml` | aozora-epub3 | 11 | 11 | 11 | 1 | 10 | over_split | adapter_over_segmented | different | different | - |
| 57037 | `data/complete/tei_lib_lv3/57037_tei.xml` | aozora-rs | 17 | 17 | 17 | 1 | 16 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57037 | `data/complete/tei_lib_lv3/57037_tei.xml` | aozora2 | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | ruby_expanded_parenless_equal | - |
| 57037 | `data/complete/tei_lib_lv3/57037_tei.xml` | aozora | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 57038 | `data/complete/tei_lib_lv3/57038_tei.xml` | aozora2html | 6 | 6 | 6 | 1 | 5 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57038 | `data/complete/tei_lib_lv3/57038_tei.xml` | aozora-epub3 | 10 | 10 | 10 | 1 | 9 | over_split | adapter_over_segmented | different | different | - |
| 57038 | `data/complete/tei_lib_lv3/57038_tei.xml` | aozora-rs | 13 | 13 | 13 | 1 | 12 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57038 | `data/complete/tei_lib_lv3/57038_tei.xml` | aozora2 | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | ruby_expanded_parenless_equal | - |
| 57038 | `data/complete/tei_lib_lv3/57038_tei.xml` | aozora | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 57039 | `data/complete/tei_lib_lv3/57039_tei.xml` | aozora2html | 7 | 7 | 7 | 1 | 6 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57039 | `data/complete/tei_lib_lv3/57039_tei.xml` | aozora-epub3 | 8 | 8 | 8 | 1 | 7 | over_split | adapter_over_segmented | different | different | - |
| 57039 | `data/complete/tei_lib_lv3/57039_tei.xml` | aozora-rs | 14 | 14 | 14 | 1 | 13 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57039 | `data/complete/tei_lib_lv3/57039_tei.xml` | aozora2 | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | ruby_expanded_parenless_equal | - |
| 57039 | `data/complete/tei_lib_lv3/57039_tei.xml` | aozora | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 57040 | `data/complete/tei_lib_lv3/57040_tei.xml` | aozora2html | 11 | 11 | 11 | 1 | 10 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57040 | `data/complete/tei_lib_lv3/57040_tei.xml` | aozora-epub3 | 12 | 12 | 12 | 1 | 11 | over_split | adapter_over_segmented | different | different | - |
| 57040 | `data/complete/tei_lib_lv3/57040_tei.xml` | aozora-rs | 18 | 18 | 18 | 1 | 17 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57040 | `data/complete/tei_lib_lv3/57040_tei.xml` | aozora2 | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | ruby_expanded_parenless_equal | - |
| 57040 | `data/complete/tei_lib_lv3/57040_tei.xml` | aozora | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 57041 | `data/complete/tei_lib_lv3/57041_tei.xml` | aozora2html | 6 | 6 | 6 | 1 | 5 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57041 | `data/complete/tei_lib_lv3/57041_tei.xml` | aozora-epub3 | 7 | 7 | 7 | 1 | 6 | over_split | adapter_over_segmented | different | different | - |
| 57041 | `data/complete/tei_lib_lv3/57041_tei.xml` | aozora-rs | 13 | 13 | 13 | 1 | 12 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57041 | `data/complete/tei_lib_lv3/57041_tei.xml` | aozora2 | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | ruby_expanded_parenless_equal | - |
| 57041 | `data/complete/tei_lib_lv3/57041_tei.xml` | aozora | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 57042 | `data/complete/tei_lib_lv3/57042_tei.xml` | aozora2html | 10 | 10 | 10 | 1 | 9 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57042 | `data/complete/tei_lib_lv3/57042_tei.xml` | aozora-epub3 | 11 | 11 | 11 | 1 | 10 | over_split | adapter_over_segmented | different | different | - |
| 57042 | `data/complete/tei_lib_lv3/57042_tei.xml` | aozora-rs | 17 | 17 | 17 | 1 | 16 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57042 | `data/complete/tei_lib_lv3/57042_tei.xml` | aozora2 | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | ruby_expanded_parenless_equal | - |
| 57042 | `data/complete/tei_lib_lv3/57042_tei.xml` | aozora | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 57043 | `data/complete/tei_lib_lv3/57043_tei.xml` | aozora2html | 9 | 9 | 9 | 1 | 8 | over_split | adapter_over_segmented | different | different | - |
| 57043 | `data/complete/tei_lib_lv3/57043_tei.xml` | aozora-epub3 | 10 | 10 | 10 | 1 | 9 | over_split | adapter_over_segmented | different | different | - |
| 57043 | `data/complete/tei_lib_lv3/57043_tei.xml` | aozora-rs | 16 | 16 | 16 | 1 | 15 | over_split | adapter_over_segmented | different | different | - |
| 57043 | `data/complete/tei_lib_lv3/57043_tei.xml` | aozora2 | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 57043 | `data/complete/tei_lib_lv3/57043_tei.xml` | aozora | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 57044 | `data/complete/tei_lib_lv3/57044_tei.xml` | aozora2html | 12 | 12 | 12 | 1 | 11 | over_split | adapter_over_segmented | different | different | - |
| 57044 | `data/complete/tei_lib_lv3/57044_tei.xml` | aozora-epub3 | 14 | 14 | 14 | 1 | 13 | over_split | adapter_over_segmented | different | different | - |
| 57044 | `data/complete/tei_lib_lv3/57044_tei.xml` | aozora-rs | 20 | 20 | 20 | 1 | 19 | over_split | adapter_over_segmented | different | different | - |
| 57044 | `data/complete/tei_lib_lv3/57044_tei.xml` | aozora2 | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 57044 | `data/complete/tei_lib_lv3/57044_tei.xml` | aozora | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 57046 | `data/complete/tei_lib_lv3/57046_tei.xml` | aozora2html | 14 | 14 | 14 | 1 | 13 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57046 | `data/complete/tei_lib_lv3/57046_tei.xml` | aozora-epub3 | 16 | 16 | 16 | 1 | 15 | over_split | adapter_over_segmented | different | different | - |
| 57046 | `data/complete/tei_lib_lv3/57046_tei.xml` | aozora-rs | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | ruby_expanded_parenless_equal | - |
| 57046 | `data/complete/tei_lib_lv3/57046_tei.xml` | aozora2 | 2 | 2 | 2 | 1 | 1 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57046 | `data/complete/tei_lib_lv3/57046_tei.xml` | aozora | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 57047 | `data/complete/tei_lib_lv3/57047_tei.xml` | aozora2html | 8 | 8 | 8 | 1 | 7 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57047 | `data/complete/tei_lib_lv3/57047_tei.xml` | aozora-epub3 | 9 | 9 | 9 | 1 | 8 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57047 | `data/complete/tei_lib_lv3/57047_tei.xml` | aozora-rs | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | ruby_expanded_parenless_equal | - |
| 57047 | `data/complete/tei_lib_lv3/57047_tei.xml` | aozora2 | 2 | 2 | 2 | 1 | 1 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 57047 | `data/complete/tei_lib_lv3/57047_tei.xml` | aozora | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 57048 | `data/complete/tei_lib_lv3/57048_tei.xml` | aozora2html | 4 | 4 | 4 | 1 | 3 | over_split | adapter_over_segmented | different | different | - |
| 57048 | `data/complete/tei_lib_lv3/57048_tei.xml` | aozora-epub3 | 5 | 5 | 5 | 1 | 4 | over_split | adapter_over_segmented | different | different | - |
| 57048 | `data/complete/tei_lib_lv3/57048_tei.xml` | aozora-rs | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 57048 | `data/complete/tei_lib_lv3/57048_tei.xml` | aozora2 | 2 | 2 | 2 | 1 | 1 | over_split | adapter_over_segmented | different | different | - |
| 57048 | `data/complete/tei_lib_lv3/57048_tei.xml` | aozora | 1 | 1 | 1 | 1 | 0 | exact | aligned | different | different | - |
| 86 | `data/complete/tei_lib_lv3/86_tei.xml` | aozora2html | 134 | 134 | 134 | 136 | -2 | under_split | adapter_under_segmented | different | ruby_expanded_parenless_generated_contains_tei_eaj | - |
| 86 | `data/complete/tei_lib_lv3/86_tei.xml` | aozora-epub3 | 134 | 134 | 134 | 136 | -2 | under_split | adapter_under_segmented | different | different | - |
| 86 | `data/complete/tei_lib_lv3/86_tei.xml` | aozora-rs | 145 | 145 | 145 | 136 | 9 | over_split | adapter_over_segmented | different | different | - |
| 86 | `data/complete/tei_lib_lv3/86_tei.xml` | aozora2 | 1 | 1 | 1 | 136 | -135 | collapsed | adapter_collapsed | different | different | - |
| 86 | `data/complete/tei_lib_lv3/86_tei.xml` | aozora | 1 | 1 | 1 | 136 | -135 | collapsed | adapter_collapsed | different | different | - |
| 15099 | `data/complete/tei_lib_lv4/104_15099.xml` | aozora2html | 62 | 62 | 62 | 57 | 5 | over_split | adapter_over_segmented | different | different | - |
| 15099 | `data/complete/tei_lib_lv4/104_15099.xml` | aozora-epub3 | 62 | 62 | 62 | 57 | 5 | over_split | adapter_over_segmented | different | different | - |
| 15099 | `data/complete/tei_lib_lv4/104_15099.xml` | aozora-rs | 1 | 1 | 1 | 57 | -56 | collapsed | adapter_collapsed | different | different | - |
| 15099 | `data/complete/tei_lib_lv4/104_15099.xml` | aozora2 | 1 | 1 | 1 | 57 | -56 | collapsed | adapter_collapsed | different | different | - |
| 15099 | `data/complete/tei_lib_lv4/104_15099.xml` | aozora | 1 | 1 | 1 | 57 | -56 | collapsed | adapter_collapsed | different | different | - |
| 1567 | `data/complete/tei_lib_lv4/1567_header_updated.xml` | aozora2html | 75 | 75 | 74 | 17 | 57 | over_split | source_note_back_routing | equal | base_equal | back |
| 1567 | `data/complete/tei_lib_lv4/1567_header_updated.xml` | aozora-epub3 | 75 | 75 | 74 | 17 | 57 | over_split | source_note_back_routing | different | different | back |
| 1567 | `data/complete/tei_lib_lv4/1567_header_updated.xml` | aozora | 1 | 1 | 1 | 17 | -16 | collapsed | adapter_collapsed | different | different | - |
| 1567 | `data/complete/tei_lib_lv4/1567_header_updated.xml` | aozora-rs | 79 | 79 | 79 | 17 | 62 | over_split | adapter_over_segmented | generated_contains_tei_eaj | base_drop_parentheticals_equal | - |
| 1567 | `data/complete/tei_lib_lv4/1567_header_updated.xml` | aozora2 | 1 | 1 | 1 | 17 | -16 | collapsed | adapter_collapsed | generated_contains_tei_eaj | base_drop_parentheticals_equal | - |
| 1567 | `data/complete/tei_lib_lv4/1567_tei.xml` | aozora2html | 75 | 75 | 74 | 17 | 57 | over_split | source_note_back_routing | equal | base_equal | back |
| 1567 | `data/complete/tei_lib_lv4/1567_tei.xml` | aozora-epub3 | 75 | 75 | 74 | 17 | 57 | over_split | source_note_back_routing | different | different | back |
| 1567 | `data/complete/tei_lib_lv4/1567_tei.xml` | aozora | 1 | 1 | 1 | 17 | -16 | collapsed | adapter_collapsed | different | different | - |
| 1567 | `data/complete/tei_lib_lv4/1567_tei.xml` | aozora-rs | 79 | 79 | 79 | 17 | 62 | over_split | adapter_over_segmented | generated_contains_tei_eaj | base_drop_parentheticals_equal | - |
| 1567 | `data/complete/tei_lib_lv4/1567_tei.xml` | aozora2 | 1 | 1 | 1 | 17 | -16 | collapsed | adapter_collapsed | generated_contains_tei_eaj | base_drop_parentheticals_equal | - |
| 45245 | `data/complete/tei_lib_lv4/45245_tei.xml` | aozora2html | 39 | 39 | 39 | 4 | 35 | over_split | adapter_over_segmented | different | different | - |
| 45245 | `data/complete/tei_lib_lv4/45245_tei.xml` | aozora-epub3 | 39 | 39 | 39 | 4 | 35 | over_split | adapter_over_segmented | different | different | - |
| 45245 | `data/complete/tei_lib_lv4/45245_tei.xml` | aozora-rs | 43 | 43 | 43 | 4 | 39 | over_split | adapter_over_segmented | different | different | - |
| 45245 | `data/complete/tei_lib_lv4/45245_tei.xml` | aozora2 | 1 | 1 | 1 | 4 | -3 | collapsed | adapter_collapsed | different | different | - |
| 45245 | `data/complete/tei_lib_lv4/45245_tei.xml` | aozora | 1 | 1 | 1 | 4 | -3 | collapsed | adapter_collapsed | different | different | - |
| 50362 | `data/complete/tei_lib_lv4/50362_tei.xml` | aozora2html | 15 | 15 | 15 | 12 | 3 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_tei_eaj_contains_generated | - |
| 50362 | `data/complete/tei_lib_lv4/50362_tei.xml` | aozora-epub3 | 12 | 12 | 12 | 12 | 0 | exact | aligned | different | different | - |
| 50362 | `data/complete/tei_lib_lv4/50362_tei.xml` | aozora-rs | 23 | 23 | 23 | 12 | 11 | over_split | adapter_over_segmented | different | different | - |
| 50362 | `data/complete/tei_lib_lv4/50362_tei.xml` | aozora2 | 4 | 4 | 4 | 12 | -8 | under_split | adapter_under_segmented | different | different | - |
| 50362 | `data/complete/tei_lib_lv4/50362_tei.xml` | aozora | 1 | 1 | 1 | 12 | -11 | collapsed | adapter_collapsed | different | different | - |
| 7928 | `data/complete/tei_lib_lv4/7928_tei.xml` | aozora2html | 61 | 61 | 61 | 110 | -49 | under_split | adapter_under_segmented | different | different | - |
| 7928 | `data/complete/tei_lib_lv4/7928_tei.xml` | aozora-epub3 | 148 | 148 | 148 | 110 | 38 | over_split | adapter_over_segmented | different | different | - |
| 7928 | `data/complete/tei_lib_lv4/7928_tei.xml` | aozora-rs | 1 | 1 | 1 | 110 | -109 | collapsed | adapter_collapsed | different | different | - |
| 7928 | `data/complete/tei_lib_lv4/7928_tei.xml` | aozora2 | 39 | 28 | 28 | 110 | -82 | under_split | converter_paragraph_mismatch | different | different | - |
| 7928 | `data/complete/tei_lib_lv4/7928_tei.xml` | aozora | 1 | 1 | 1 | 110 | -109 | collapsed | adapter_collapsed | different | different | - |
| 1126 | `data/draft/tei_lib_lv3/1126_tei.xml` | aozora2html | 30 | 30 | 30 | 154 | -124 | under_split | adapter_under_segmented | different | ruby_expanded_parenless_tei_eaj_contains_generated | - |
| 1126 | `data/draft/tei_lib_lv3/1126_tei.xml` | aozora-epub3 | 154 | 154 | 154 | 154 | 0 | exact | aligned | different | different | - |
| 1126 | `data/draft/tei_lib_lv3/1126_tei.xml` | aozora-rs | 1 | 1 | 1 | 154 | -153 | collapsed | adapter_collapsed | different | ruby_expanded_parenless_equal | - |
| 1126 | `data/draft/tei_lib_lv3/1126_tei.xml` | aozora2 | 52 | 28 | 28 | 154 | -126 | under_split | converter_paragraph_mismatch | different | different | - |
| 1126 | `data/draft/tei_lib_lv3/1126_tei.xml` | aozora | 1 | 1 | 1 | 154 | -153 | collapsed | adapter_collapsed | different | different | - |
| 1576 | `data/draft/tei_lib_lv3/1576_tei.xml` | aozora2html | 467 | 466 | 466 | 496 | -30 | under_split | page_break_projection | different | different | - |
| 1576 | `data/draft/tei_lib_lv3/1576_tei.xml` | aozora-epub3 | 529 | 529 | 529 | 496 | 33 | over_split | adapter_over_segmented | different | different | - |
| 1576 | `data/draft/tei_lib_lv3/1576_tei.xml` | aozora-rs | 1 | 1 | 1 | 496 | -495 | collapsed | adapter_collapsed | different | different | - |
| 1576 | `data/draft/tei_lib_lv3/1576_tei.xml` | aozora2 | 7 | 6 | 6 | 496 | -490 | under_split | converter_paragraph_mismatch | different | different | - |
| 1576 | `data/draft/tei_lib_lv3/1576_tei.xml` | aozora | 1 | 1 | 1 | 496 | -495 | collapsed | adapter_collapsed | different | different | - |
| 43563 | `data/draft/tei_lib_lv3/43563_tei.xml` | aozora2html | 186 | 186 | 186 | 26 | 160 | over_split | adapter_over_segmented | different | different | - |
| 43563 | `data/draft/tei_lib_lv3/43563_tei.xml` | aozora-epub3 | 186 | 186 | 186 | 26 | 160 | over_split | adapter_over_segmented | different | different | - |
| 43563 | `data/draft/tei_lib_lv3/43563_tei.xml` | aozora-rs | 202 | 202 | 202 | 26 | 176 | over_split | adapter_over_segmented | different | different | - |
| 43563 | `data/draft/tei_lib_lv3/43563_tei.xml` | aozora2 | 1 | 1 | 1 | 26 | -25 | collapsed | adapter_collapsed | different | different | - |
| 43563 | `data/draft/tei_lib_lv3/43563_tei.xml` | aozora | 1 | 1 | 1 | 26 | -25 | collapsed | adapter_collapsed | different | different | - |
| 52208 | `data/draft/tei_lib_lv3/52208_tei.xml` | aozora2html | 27 | 27 | 27 | 18 | 9 | over_split | adapter_over_segmented | different | different | - |
| 52208 | `data/draft/tei_lib_lv3/52208_tei.xml` | aozora-epub3 | 27 | 27 | 27 | 18 | 9 | over_split | adapter_over_segmented | different | different | - |
| 52208 | `data/draft/tei_lib_lv3/52208_tei.xml` | aozora-rs | 1 | 1 | 1 | 18 | -17 | collapsed | adapter_collapsed | different | different | - |
| 52208 | `data/draft/tei_lib_lv3/52208_tei.xml` | aozora2 | 5 | 5 | 5 | 18 | -13 | under_split | adapter_under_segmented | different | different | - |
| 52208 | `data/draft/tei_lib_lv3/52208_tei.xml` | aozora | 1 | 1 | 1 | 18 | -17 | collapsed | adapter_collapsed | different | different | - |
| 86 | `data/draft/tei_lib_lv3/86_tei.xml` | aozora2html | 134 | 134 | 134 | 123 | 11 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_equal | - |
| 86 | `data/draft/tei_lib_lv3/86_tei.xml` | aozora-epub3 | 134 | 134 | 134 | 123 | 11 | over_split | adapter_over_segmented | different | different | - |
| 86 | `data/draft/tei_lib_lv3/86_tei.xml` | aozora-rs | 145 | 145 | 145 | 123 | 22 | over_split | adapter_over_segmented | different | different | - |
| 86 | `data/draft/tei_lib_lv3/86_tei.xml` | aozora2 | 1 | 1 | 1 | 123 | -122 | collapsed | adapter_collapsed | different | different | - |
| 86 | `data/draft/tei_lib_lv3/86_tei.xml` | aozora | 1 | 1 | 1 | 123 | -122 | collapsed | adapter_collapsed | different | different | - |
| 1805 | `data/draft/tei_lib_lv4/1805_tei.xml` | aozora2html | 450 | 450 | 450 | 29 | 421 | over_split | adapter_over_segmented | different | ruby_expanded_parenless_generated_contains_tei_eaj | - |
| 1805 | `data/draft/tei_lib_lv4/1805_tei.xml` | aozora-epub3 | 880 | 880 | 880 | 29 | 851 | over_split | adapter_over_segmented | different | different | - |
| 1805 | `data/draft/tei_lib_lv4/1805_tei.xml` | aozora-rs | 1 | 1 | 1 | 29 | -28 | collapsed | adapter_collapsed | different | different | - |
| 1805 | `data/draft/tei_lib_lv4/1805_tei.xml` | aozora2 | 336 | 331 | 331 | 29 | 302 | over_split | converter_paragraph_mismatch | different | different | - |
| 1805 | `data/draft/tei_lib_lv4/1805_tei.xml` | aozora | 1 | 1 | 1 | 29 | -28 | collapsed | adapter_collapsed | different | different | - |
| 2571 | `data/draft/tei_lib_lv4/2571_tei.xml` | aozora2html | 77 | 77 | 77 | 3 | 74 | over_split | adapter_over_segmented | different | different | - |
| 2571 | `data/draft/tei_lib_lv4/2571_tei.xml` | aozora-epub3 | 77 | 77 | 77 | 3 | 74 | over_split | adapter_over_segmented | different | different | - |
| 2571 | `data/draft/tei_lib_lv4/2571_tei.xml` | aozora-rs | 82 | 82 | 82 | 3 | 79 | over_split | adapter_over_segmented | different | different | - |
| 2571 | `data/draft/tei_lib_lv4/2571_tei.xml` | aozora2 | 1 | 1 | 1 | 3 | -2 | collapsed | adapter_collapsed | different | different | - |
| 2571 | `data/draft/tei_lib_lv4/2571_tei.xml` | aozora | 1 | 1 | 1 | 3 | -2 | collapsed | adapter_collapsed | different | different | - |
| 4244 | `data/draft/tei_lib_lv4/4244-1_tei.xml` | aozora2html | 74 | 74 | 74 | 24 | 50 | over_split | adapter_over_segmented | different | different | - |
| 4244 | `data/draft/tei_lib_lv4/4244-1_tei.xml` | aozora-epub3 | 74 | 74 | 74 | 24 | 50 | over_split | adapter_over_segmented | different | different | - |
| 4244 | `data/draft/tei_lib_lv4/4244-1_tei.xml` | aozora-rs | 1 | 1 | 1 | 24 | -23 | collapsed | adapter_collapsed | different | different | - |
| 4244 | `data/draft/tei_lib_lv4/4244-1_tei.xml` | aozora2 | 6 | 6 | 6 | 24 | -18 | under_split | adapter_under_segmented | different | different | - |
| 4244 | `data/draft/tei_lib_lv4/4244-1_tei.xml` | aozora | 1 | 1 | 1 | 24 | -23 | collapsed | adapter_collapsed | different | different | - |
| 4244 | `data/draft/tei_lib_lv4/4244-3_tei.xml` | aozora2html | 74 | 74 | 74 | 13 | 61 | over_split | adapter_over_segmented | different | different | - |
| 4244 | `data/draft/tei_lib_lv4/4244-3_tei.xml` | aozora-epub3 | 74 | 74 | 74 | 13 | 61 | over_split | adapter_over_segmented | different | different | - |
| 4244 | `data/draft/tei_lib_lv4/4244-3_tei.xml` | aozora-rs | 1 | 1 | 1 | 13 | -12 | collapsed | adapter_collapsed | different | different | - |
| 4244 | `data/draft/tei_lib_lv4/4244-3_tei.xml` | aozora2 | 6 | 6 | 6 | 13 | -7 | under_split | adapter_under_segmented | different | different | - |
| 4244 | `data/draft/tei_lib_lv4/4244-3_tei.xml` | aozora | 1 | 1 | 1 | 13 | -12 | collapsed | adapter_collapsed | different | different | - |
| 4244 | `data/draft/tei_lib_lv4/4244-4_tei.xml` | aozora2html | 74 | 74 | 74 | 8 | 66 | over_split | adapter_over_segmented | different | different | - |
| 4244 | `data/draft/tei_lib_lv4/4244-4_tei.xml` | aozora-epub3 | 74 | 74 | 74 | 8 | 66 | over_split | adapter_over_segmented | different | different | - |
| 4244 | `data/draft/tei_lib_lv4/4244-4_tei.xml` | aozora-rs | 1 | 1 | 1 | 8 | -7 | collapsed | adapter_collapsed | different | different | - |
| 4244 | `data/draft/tei_lib_lv4/4244-4_tei.xml` | aozora2 | 6 | 6 | 6 | 8 | -2 | under_split | adapter_under_segmented | different | different | - |
| 4244 | `data/draft/tei_lib_lv4/4244-4_tei.xml` | aozora | 1 | 1 | 1 | 8 | -7 | collapsed | adapter_collapsed | different | different | - |
| 46453 | `data/draft/tei_lib_lv4/46453_tei.xml` | aozora2html | 74 | 72 | 72 | 89 | -17 | under_split | page_break_projection | different | different | - |
| 46453 | `data/draft/tei_lib_lv4/46453_tei.xml` | aozora-epub3 | 145 | 145 | 145 | 89 | 56 | over_split | adapter_over_segmented | different | different | - |
| 46453 | `data/draft/tei_lib_lv4/46453_tei.xml` | aozora-rs | 1 | 1 | 1 | 89 | -88 | collapsed | adapter_collapsed | different | different | - |
| 46453 | `data/draft/tei_lib_lv4/46453_tei.xml` | aozora2 | 65 | 43 | 43 | 89 | -46 | under_split | converter_paragraph_mismatch | different | different | - |
| 46453 | `data/draft/tei_lib_lv4/46453_tei.xml` | aozora | 1 | 1 | 1 | 89 | -88 | collapsed | adapter_collapsed | different | different | - |
| 54457 | `data/draft/tei_lib_lv4/54457_tei.xml` | aozora2html | 15 | 15 | 15 | 15 | 0 | exact | aligned | different | different | - |
| 54457 | `data/draft/tei_lib_lv4/54457_tei.xml` | aozora-epub3 | 15 | 15 | 15 | 15 | 0 | exact | aligned | different | different | - |
| 54457 | `data/draft/tei_lib_lv4/54457_tei.xml` | aozora-rs | 19 | 19 | 19 | 15 | 4 | over_split | adapter_over_segmented | different | different | - |
| 54457 | `data/draft/tei_lib_lv4/54457_tei.xml` | aozora2 | 1 | 1 | 1 | 15 | -14 | collapsed | adapter_collapsed | different | different | - |
| 54457 | `data/draft/tei_lib_lv4/54457_tei.xml` | aozora | 1 | 1 | 1 | 15 | -14 | collapsed | adapter_collapsed | different | different | - |
