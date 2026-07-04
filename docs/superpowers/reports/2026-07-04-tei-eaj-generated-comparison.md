# Generated Parser-IR TEI vs TEI-EAJ Workset Audit

This report materializes ABC TEI from parser-IR, then compares the generated TEI structure against pinned TEI-EAJ XML. It measures structural deltas; it does not treat TEI-EAJ Level 4 enrichment as parser-IR admission scope.

## Totals

| metric | value |
|---|---:|
| rows_attempted | 57 |
| materialization_succeeded | 57 |
| materialization_failed | 0 |
| rows_skipped | 5 |

## Paragraph Delta Buckets

| bucket | rows |
|---|---:|
| exact | 5 |
| over_split | 45 |
| under_split | 7 |

## Body Text Relation Buckets

| relation | rows |
|---|---:|
| different | 53 |
| equal | 3 |
| generated_contains_tei_eaj | 1 |

## Body Text Match Buckets

| bucket | rows |
|---|---:|
| base_drop_parentheticals_equal | 1 |
| base_equal | 3 |
| different | 26 |
| ruby_expanded_equal | 1 |
| ruby_expanded_parenless_equal | 19 |
| ruby_expanded_parenless_generated_contains_tei_eaj | 5 |
| ruby_expanded_parenless_tei_eaj_contains_generated | 2 |

## Rows

| work_id | TEI-EAJ file | adapter | generated body p | TEI-EAJ body p | delta | bucket | base text | best text match | source note |
|---|---|---|---:|---:|---:|---|---|---|---|
| 1126 | `data/complete/tei_lib_lv3/1126_tei.xml` | aozora2html | 30 | 169 | -139 | under_split | different | different | - |
| 15099 | `data/complete/tei_lib_lv3/15099_tei.xml` | aozora2html | 62 | 1 | 61 | over_split | different | different | - |
| 15938 | `data/complete/tei_lib_lv3/15938_tei.xml` | aozora2html | 8 | 6 | 2 | over_split | different | different | - |
| 236 | `data/complete/tei_lib_lv3/236_tei.xml` | aozora2html | 29 | 1 | 28 | over_split | different | ruby_expanded_parenless_equal | - |
| 2509 | `data/complete/tei_lib_lv3/2509_tei.xml` | aozora2html | 28 | 28 | 0 | exact | different | different | - |
| 42320 | `data/complete/tei_lib_lv3/42320_tei.xml` | aozora2html | 4 | 3 | 1 | over_split | different | ruby_expanded_parenless_generated_contains_tei_eaj | - |
| 43077 | `data/complete/tei_lib_lv3/43077_tei.xml` | aozora2html | 32 | 30 | 2 | over_split | different | ruby_expanded_parenless_tei_eaj_contains_generated | - |
| 43563 | `data/complete/tei_lib_lv3/43563_tei.xml` | aozora2html | 186 | 26 | 160 | over_split | different | different | - |
| 4411 | `data/complete/tei_lib_lv3/4411_tei.xml` | aozora2html | 6 | 2 | 4 | over_split | different | ruby_expanded_parenless_equal | - |
| 4464 | `data/complete/tei_lib_lv3/4464_tei.xml` | aozora2html | 26 | 17 | 9 | over_split | different | different | - |
| 45093 | `data/complete/tei_lib_lv3/45093_tei.xml` | aozora2html | 18 | 6 | 12 | over_split | different | ruby_expanded_parenless_equal | - |
| 4872 | `data/complete/tei_lib_lv3/4872_tei.xml` | aozora2html | 5 | 5 | 0 | exact | different | ruby_expanded_equal | - |
| 50502 | `data/complete/tei_lib_lv3/50502_tei.xml` | aozora2html | 8 | 8 | 0 | exact | different | different | - |
| 51307 | `data/complete/tei_lib_lv3/51307_tei.xml` | aozora2html | 406 | 7 | 399 | over_split | different | different | - |
| 51520 | `data/complete/tei_lib_lv3/51520_tei.xml` | aozora2html | 76 | 76 | 0 | exact | different | ruby_expanded_parenless_equal | - |
| 53386 | `data/complete/tei_lib_lv3/53386_tei.xml` | aozora2html | 5 | 1 | 4 | over_split | generated_contains_tei_eaj | ruby_expanded_parenless_generated_contains_tei_eaj | - |
| 53617 | `data/complete/tei_lib_lv3/53617_tei.xml` | aozora2html | 20 | 1 | 19 | over_split | different | ruby_expanded_parenless_generated_contains_tei_eaj | - |
| 55783 | `data/complete/tei_lib_lv3/55783_tei.xml` | aozora2html | 16 | 126 | -110 | under_split | different | different | - |
| 56996 | `data/complete/tei_lib_lv3/56996_tei.xml` | aozora2html | 8 | 1 | 7 | over_split | different | ruby_expanded_parenless_equal | - |
| 56998 | `data/complete/tei_lib_lv3/56998_tei.xml` | aozora2html | 9 | 1 | 8 | over_split | different | ruby_expanded_parenless_equal | - |
| 56999 | `data/complete/tei_lib_lv3/56999_tei.xml` | aozora2html | 9 | 8 | 1 | over_split | different | ruby_expanded_parenless_generated_contains_tei_eaj | - |
| 57001 | `data/complete/tei_lib_lv3/57001_tei.xml` | aozora2html | 13 | 1 | 12 | over_split | different | ruby_expanded_parenless_equal | - |
| 57002 | `data/complete/tei_lib_lv3/57002_tei.xml` | aozora2html | 7 | 1 | 6 | over_split | equal | base_equal | - |
| 57003 | `data/complete/tei_lib_lv3/57003_tei.xml` | aozora2html | 5 | 1 | 4 | over_split | different | ruby_expanded_parenless_equal | - |
| 57004 | `data/complete/tei_lib_lv3/57004_tei.xml` | aozora2html | 11 | 1 | 10 | over_split | different | ruby_expanded_parenless_equal | - |
| 57005 | `data/complete/tei_lib_lv3/57005_tei.xml` | aozora2html | 13 | 1 | 12 | over_split | different | ruby_expanded_parenless_equal | - |
| 57006 | `data/complete/tei_lib_lv3/57006_tei.xml` | aozora2html | 10 | 1 | 9 | over_split | different | ruby_expanded_parenless_equal | - |
| 57037 | `data/complete/tei_lib_lv3/57037_tei.xml` | aozora2html | 10 | 1 | 9 | over_split | different | ruby_expanded_parenless_equal | - |
| 57038 | `data/complete/tei_lib_lv3/57038_tei.xml` | aozora2html | 6 | 1 | 5 | over_split | different | base_drop_parentheticals_equal | - |
| 57039 | `data/complete/tei_lib_lv3/57039_tei.xml` | aozora2html | 7 | 1 | 6 | over_split | different | ruby_expanded_parenless_equal | - |
| 57040 | `data/complete/tei_lib_lv3/57040_tei.xml` | aozora2html | 11 | 1 | 10 | over_split | different | ruby_expanded_parenless_equal | - |
| 57041 | `data/complete/tei_lib_lv3/57041_tei.xml` | aozora2html | 6 | 1 | 5 | over_split | different | ruby_expanded_parenless_equal | - |
| 57042 | `data/complete/tei_lib_lv3/57042_tei.xml` | aozora2html | 10 | 1 | 9 | over_split | different | ruby_expanded_parenless_equal | - |
| 57043 | `data/complete/tei_lib_lv3/57043_tei.xml` | aozora2html | 9 | 1 | 8 | over_split | different | different | - |
| 57044 | `data/complete/tei_lib_lv3/57044_tei.xml` | aozora2html | 12 | 1 | 11 | over_split | different | different | - |
| 57046 | `data/complete/tei_lib_lv3/57046_tei.xml` | aozora2html | 14 | 1 | 13 | over_split | different | ruby_expanded_parenless_equal | - |
| 57047 | `data/complete/tei_lib_lv3/57047_tei.xml` | aozora2html | 8 | 1 | 7 | over_split | different | ruby_expanded_parenless_equal | - |
| 57048 | `data/complete/tei_lib_lv3/57048_tei.xml` | aozora2html | 4 | 1 | 3 | over_split | different | different | - |
| 86 | `data/complete/tei_lib_lv3/86_tei.xml` | aozora2html | 134 | 136 | -2 | under_split | different | ruby_expanded_parenless_generated_contains_tei_eaj | - |
| 15099 | `data/complete/tei_lib_lv4/104_15099.xml` | aozora2html | 62 | 57 | 5 | over_split | different | different | - |
| 1567 | `data/complete/tei_lib_lv4/1567_header_updated.xml` | aozora2html | 74 | 17 | 57 | over_split | equal | base_equal | back |
| 1567 | `data/complete/tei_lib_lv4/1567_tei.xml` | aozora2html | 74 | 17 | 57 | over_split | equal | base_equal | back |
| 45245 | `data/complete/tei_lib_lv4/45245_tei.xml` | aozora2html | 39 | 4 | 35 | over_split | different | different | - |
| 50362 | `data/complete/tei_lib_lv4/50362_tei.xml` | aozora2html | 15 | 12 | 3 | over_split | different | ruby_expanded_parenless_tei_eaj_contains_generated | - |
| 7928 | `data/complete/tei_lib_lv4/7928_tei.xml` | aozora2html | 61 | 110 | -49 | under_split | different | different | - |
| 1126 | `data/draft/tei_lib_lv3/1126_tei.xml` | aozora2html | 30 | 154 | -124 | under_split | different | different | - |
| 1576 | `data/draft/tei_lib_lv3/1576_tei.xml` | aozora2html | 466 | 496 | -30 | under_split | different | different | - |
| 43563 | `data/draft/tei_lib_lv3/43563_tei.xml` | aozora2html | 186 | 26 | 160 | over_split | different | different | - |
| 52208 | `data/draft/tei_lib_lv3/52208_tei.xml` | aozora2html | 27 | 18 | 9 | over_split | different | different | - |
| 86 | `data/draft/tei_lib_lv3/86_tei.xml` | aozora2html | 134 | 123 | 11 | over_split | different | ruby_expanded_parenless_equal | - |
| 1805 | `data/draft/tei_lib_lv4/1805_tei.xml` | aozora2html | 450 | 29 | 421 | over_split | different | different | - |
| 2571 | `data/draft/tei_lib_lv4/2571_tei.xml` | aozora2html | 77 | 3 | 74 | over_split | different | different | - |
| 4244 | `data/draft/tei_lib_lv4/4244-1_tei.xml` | aozora2html | 74 | 24 | 50 | over_split | different | different | - |
| 4244 | `data/draft/tei_lib_lv4/4244-3_tei.xml` | aozora2html | 74 | 13 | 61 | over_split | different | different | - |
| 4244 | `data/draft/tei_lib_lv4/4244-4_tei.xml` | aozora2html | 74 | 8 | 66 | over_split | different | different | - |
| 46453 | `data/draft/tei_lib_lv4/46453_tei.xml` | aozora2html | 72 | 89 | -17 | under_split | different | different | - |
| 54457 | `data/draft/tei_lib_lv4/54457_tei.xml` | aozora2html | 15 | 15 | 0 | exact | different | different | - |
