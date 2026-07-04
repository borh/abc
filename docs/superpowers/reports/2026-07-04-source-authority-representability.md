# Source Authority Representability Inventory

## Operator Run

- command: `RUST_BACKTRACE=0 just source-inventory-full 24 scratch/ab-index.json /home/bor/Dependencies/aozorabunko`
- index: `scratch/ab-index.json`
- corpus: `/home/bor/Dependencies/aozorabunko`
- index entries scanned: 17,894
- generated unknown workset: `/db/ab-validator/source-inventory/unknown-workset.json`

## Summary

- works_scanned: 17894
- works_failed: 0
- markers_total: 4335091
- unknown_markers_total: 39629
- unallowlisted_unknown_markers_total: 39629
- allowlisted_unknown_markers_total: 0

## Representability

- typed_occurrences: 3745779
- raw_preserved_occurrences: 0
- out_of_body_occurrences: 0
- unsupported_occurrences: 0
- needs_research_occurrences: 299935

## Rows

| row | works | occurrences | samples |
|---|---:|---:|---|
| annotation.bouki | 12 | 127 | 000031_2846, 000037_2848, 000156_2699, 000219_2932, 000287_3061 |
| annotation.chuuki | 9 | 12 | 000146_49258, 000301_1872, 000311_33189, 000311_46249, 000908_51431 |
| break.line_explicit | 47 | 161 | 000075_4250, 000081_47027, 000083_46289, 000106_56858, 000106_57905 |
| break.page_line | 841 | 8046 | 000005_53194, 000006_1869, 000009_55881, 000011_889, 000011_899 |
| caption.block | 29 | 620 | 000058_57440, 000226_1150, 000255_47055, 000304_46461, 000320_43481 |
| caption.inline | 136 | 1040 | 000014_728, 000067_1768, 000067_1788, 000067_1789, 000093_1916 |
| decoration.bold_italic | 56 | 218 | 000025_1144, 000026_55916, 000035_52380, 000072_54444, 000075_47964 |
| decoration.boten | 7 | 42 | 000048_45476, 000048_48803, 000067_2249, 000305_43619, 000933_47202 |
| decoration.bousen | 317 | 17829 | 000006_1869, 000013_11, 000019_4376, 000034_55507, 000038_1408 |
| decoration.direction_override | 6 | 65 | 000096_935, 000866_3039, 001094_42603, 001242_46444, 001467_50733 |
| decoration.keigakomi | 106 | 200 | 000067_1789, 000072_408, 000096_2093, 000096_2100, 000096_2117 |
| editor_note.unmapped | 14068 | 689586 | 000005_5, 000005_53194, 000006_1868, 000006_1869, 000006_3310 |
| emphasis.basic | 6576 | 157495 | 000005_53194, 000006_1869, 000006_382, 000006_383, 000006_58819 |
| figure.image_caption | 103 | 1777 | 000019_42378, 000019_42379, 000019_42380, 000019_42381, 000019_42382 |
| figure.image_inline | 504 | 5812 | 000009_226, 000009_50711, 000009_50712, 000009_50713, 000009_50714 |
| gaiji.jis_code | 5772 | 55331 | 000005_5, 000006_1869, 000006_3310, 000006_383, 000006_384 |
| gaiji.marker | 5930 | 62372 | 000005_5, 000006_1868, 000006_1869, 000006_3310, 000006_383 |
| gaiji.un_embed | 38 | 144 | 000019_4376, 000025_kantou, 000038_323, 000040_1326, 000040_737 |
| gaiji.unicode_codepoint | 631 | 3719 | 000008_47357, 000008_47386, 000020_55103, 000022_42254, 000023_1698 |
| gaiji_ruby.inline_base | 332 | 737 | 000006_1869, 000019_4376, 000022_42254, 000025_201, 000027_1463 |
| heading.basic | 3684 | 69096 | 000005_53194, 000006_58819, 000008_1083, 000008_47357, 000008_47386 |
| heading.dogyo | 142 | 11839 | 000011_899, 000058_59060, 000067_1790, 000067_4869, 000081_1058 |
| heading.mado | 6 | 1680 | 000255_47342, 000296_1864, 000961_4820, 001402_49946, 001404_49966 |
| indentation.basic | 8169 | 134394 | 000005_5, 000005_53194, 000006_1868, 000006_1869, 000006_3310 |
| indentation.burasage | 722 | 5243 | 000008_47386, 000009_55881, 000019_4376, 000025_2943, 000031_863 |
| indentation.chitsuki | 2187 | 7952 | 000006_1868, 000006_382, 000006_383, 000006_384, 000006_901 |
| indentation.jisage_block | 4095 | 44881 | 000005_5, 000005_53194, 000006_1868, 000006_1869, 000006_3310 |
| indentation.jisage_oneline | 1 | 2 | 000284_2227 |
| iteration.kunoji | 1125 | 10700 | 000006_58810, 000006_58819, 000008_47357, 000012_1092, 000012_24448 |
| kunten.kaeriten | 488 | 28082 | 000006_1869, 000038_1408, 000042_1694, 000050_3581, 000051_1436 |
| kunten.okurigana | 256 | 6562 | 000026_50238, 000026_50242, 000026_50259, 000026_51893, 000026_55774 |
| layout.tcy | 1 | 1 | 000150_46616 |
| layout.yokogumi | 87 | 182 | 000034_55507, 000038_42202, 000051_4331, 000076_45641, 000076_46943 |
| reference.frontref | 682 | 3920 | 000006_46659, 000012_2585, 000012_4316, 000022_197, 000026_46578 |
| ruby.basic | 14321 | 3608407 | 000005_5, 000005_53194, 000005_55215, 000005_55216, 000005_55217 |
| ruby.placement_directional | 31 | 312 | 000034_1213, 000050_3581, 000129_694, 000146_49258, 000146_50202 |
| warichu.basic | 361 | 3390 | 000005_53194, 000006_1868, 000006_1869, 000034_519, 000038_42202 |
| warigaki.parenthetical | 1 | 1 | 000034_519 |

## Unknown Source Markers

| work_id | line | kind | raw | body |
|---|---:|---|---|---|
| 000005_5 | 11 | MalformedRuby | ｜ | ｜ |
| 000005_5 | 14 | CommandFullwidth | ［＃］ |  |
| 000005_53194 | 11 | MalformedRuby | ｜ | ｜ |
| 000005_53194 | 14 | CommandFullwidth | ［＃］ |  |
| 000006_1868 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_1868 | 13 | CommandFullwidth | ［＃］ |  |
| 000006_1869 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_1869 | 13 | CommandFullwidth | ［＃］ |  |
| 000006_3310 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_3310 | 13 | CommandFullwidth | ［＃］ |  |
| 000006_3311 | 10 | CommandFullwidth | ［＃］ |  |
| 000006_382 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_382 | 13 | CommandFullwidth | ［＃］ |  |
| 000006_383 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_383 | 13 | CommandFullwidth | ［＃］ |  |
| 000006_384 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_384 | 13 | CommandFullwidth | ［＃］ |  |
| 000006_4627 | 11 | CommandFullwidth | ［＃］ |  |
| 000006_4627 | 37 | AccentNotation | 〔はない〕 | はない |
| 000006_46659 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_46659 | 13 | CommandFullwidth | ［＃］ |  |
| 000006_47064 | 48 | AccentNotation | 〔日本語〕 | 日本語 |
| 000006_58819 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_58819 | 13 | CommandFullwidth | ［＃］ |  |
| 000006_901 | 10 | CommandFullwidth | ［＃］ |  |
| 000008_1083 | 10 | CommandFullwidth | ［＃］ |  |
| 000008_1083 | 10 | CommandFullwidth | ［＃］ |  |
| 000008_18327 | 10 | MalformedRuby | ｜ | ｜ |
| 000008_18327 | 13 | CommandFullwidth | ［＃］ |  |
| 000008_2688 | 10 | MalformedRuby | ｜ | ｜ |
| 000008_2688 | 13 | CommandFullwidth | ［＃］ |  |
| 000008_407 | 10 | MalformedRuby | ｜ | ｜ |
| 000008_407 | 13 | CommandFullwidth | ［＃］ |  |
| 000008_47357 | 10 | MalformedRuby | ｜ | ｜ |
| 000008_47357 | 13 | CommandFullwidth | ［＃］ |  |
| 000008_47361 | 10 | MalformedRuby | ｜ | ｜ |
| 000008_47361 | 13 | CommandFullwidth | ［＃］ |  |
| 000008_47374 | 10 | CommandFullwidth | ［＃］ |  |
| 000008_47382 | 10 | CommandFullwidth | ［＃］ |  |
| 000008_47383 | 10 | CommandFullwidth | ［＃］ |  |
| 000008_47384 | 10 | MalformedRuby | ｜ | ｜ |
| 000008_47384 | 13 | CommandFullwidth | ［＃］ |  |
| 000008_47386 | 10 | MalformedRuby | ｜ | ｜ |
| 000008_47386 | 13 | CommandFullwidth | ［＃］ |  |
| 000008_58922 | 7 | CommandFullwidth | ［＃］ |  |
| 000008_58922 | 20 | MalformedRuby | ｜ | ｜ |
| 000008_58922 | 20 | MalformedRuby | ｜ | ｜ |
| 000008_697 | 10 | MalformedRuby | ｜ | ｜ |
| 000008_697 | 13 | CommandFullwidth | ［＃］ |  |
| 000008_7 | 10 | CommandFullwidth | ［＃］ |  |
| 000009_226 | 12 | CommandFullwidth | ［＃］ |  |
| 000009_42929 | 12 | MalformedRuby | ｜ | ｜ |
| 000009_42929 | 15 | CommandFullwidth | ［＃］ |  |
| 000009_43028 | 12 | MalformedRuby | ｜ | ｜ |
| 000009_43028 | 15 | CommandFullwidth | ［＃］ |  |
| 000009_43028 | 99 | MalformedRuby | ｜ | ｜ |
| 000009_43056 | 11 | MalformedRuby | ｜ | ｜ |
| 000009_43056 | 14 | CommandFullwidth | ［＃］ |  |
| 000009_43471 | 12 | MalformedRuby | ｜ | ｜ |
| 000009_43471 | 15 | CommandFullwidth | ［＃］ |  |
| 000009_43497 | 11 | MalformedRuby | ｜ | ｜ |
| 000009_43497 | 14 | CommandFullwidth | ［＃］ |  |
| 000009_43498 | 11 | MalformedRuby | ｜ | ｜ |
| 000009_43498 | 14 | CommandFullwidth | ［＃］ |  |
| 000009_43522 | 11 | MalformedRuby | ｜ | ｜ |
| 000009_43522 | 14 | CommandFullwidth | ［＃］ |  |
| 000009_43523 | 11 | MalformedRuby | ｜ | ｜ |
| 000009_43523 | 14 | CommandFullwidth | ［＃］ |  |
| 000009_43524 | 11 | MalformedRuby | ｜ | ｜ |
| 000009_43524 | 14 | CommandFullwidth | ［＃］ |  |
| 000009_45340 | 11 | MalformedRuby | ｜ | ｜ |
| 000009_45340 | 14 | CommandFullwidth | ［＃］ |  |
| 000009_50711 | 12 | CommandFullwidth | ［＃］ |  |
| 000009_50712 | 12 | MalformedRuby | ｜ | ｜ |
| 000009_50712 | 15 | CommandFullwidth | ［＃］ |  |
| 000009_50713 | 13 | CommandFullwidth | ［＃］ |  |
| 000009_50714 | 13 | CommandFullwidth | ［＃］ |  |
| 000009_50715 | 13 | CommandFullwidth | ［＃］ |  |
| 000009_50716 | 13 | CommandFullwidth | ［＃］ |  |
| 000009_50717 | 13 | CommandFullwidth | ［＃］ |  |
| 000009_50718 | 13 | CommandFullwidth | ［＃］ |  |
| 000009_54910 | 13 | CommandFullwidth | ［＃］ |  |
| 000009_54911 | 13 | CommandFullwidth | ［＃］ |  |
| 000009_54912 | 13 | CommandFullwidth | ［＃］ |  |
| 000009_54913 | 13 | CommandFullwidth | ［＃］ |  |
| 000009_54914 | 13 | CommandFullwidth | ［＃］ |  |
| 000009_54915 | 12 | CommandFullwidth | ［＃］ |  |
| 000009_55881 | 12 | CommandFullwidth | ［＃］ |  |
| 000009_55882 | 12 | CommandFullwidth | ［＃］ |  |
| 000009_57322 | 13 | CommandFullwidth | ［＃］ |  |
| 000009_61393 | 13 | CommandFullwidth | ［＃］ |  |
| 000009_61394 | 12 | CommandFullwidth | ［＃］ |  |
| 000009_8 | 12 | CommandFullwidth | ［＃］ |  |
| 000011_55300 | 10 | CommandFullwidth | ［＃］ |  |
| 000011_55301 | 7 | CommandFullwidth | ［＃］ |  |
| 000011_889 | 10 | CommandFullwidth | ［＃］ |  |
| 000011_899 | 10 | CommandFullwidth | ［＃］ |  |
| 000011_9 | 10 | CommandFullwidth | ［＃］ |  |
| 000012_10 | 10 | MalformedRuby | ｜ | ｜ |
| 000012_10 | 13 | CommandFullwidth | ［＃］ |  |

## Decode Failures

None.

## Inputs

- matrix: `/home/bor/Projects/ab-validator/data/aozora-syntax-coverage.toml`
- index: `scratch/ab-index.json`
- corpus: `/home/bor/Dependencies/aozorabunko`
- allowlist: `/home/bor/Projects/ab-validator/data/aozora-source-inventory-allowlist.toml`
