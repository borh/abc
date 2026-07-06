# Source Authority Representability Inventory

## Verdict

- source_authority_gate: `SOURCE_AUTHORITY_GATE_PASS`
- strict_errors: none

## Scope

This is a source-markup authority gate: every reached explicit Aozora Bunko marker must have a reviewed representation and, for represented rows, a TEI P5 projection target. Semantic TEI enrichment such as named-entity, speech, role, or place annotation is outside this gate and remains a downstream editorial layer.

## Summary

- works_scanned: 17894
- works_failed: 0
- markers_total: 4323915
- unknown_markers_total: 14886
- unallowlisted_unknown_markers_total: 0
- allowlisted_unknown_markers_total: 14886

## Representability

- typed_occurrences: 4570071
- raw_preserved_occurrences: 46382
- out_of_body_occurrences: 950
- malformed_noise_occurrences: 13936
- unsupported_occurrences: 0
- needs_research_occurrences: 0

## Source Region Coverage

- schema_version: `aozora-source-region-coverage-v1`
- body_typed_occurrences: 4570071
- body_raw_preserved_occurrences: 46382
- source_apparatus_occurrences: 13920
- front_matter_occurrences: 14627
- back_matter_occurrences: 243
- malformed_source_occurrences: 16
- unsupported_body_markup_occurrences: 0
- unknown_region_occurrences: 0
- unknown_unreviewed_occurrences: 0

## Rows

| row | works | occurrences | samples |
|---|---:|---:|---|
| accent.dotted_letter | 3 | 93 | 001096_42686, 001096_43554, 001096_43672 |
| annotation.bouki | 12 | 127 | 000031_2846, 000037_2848, 000156_2699, 000219_2932, 000287_3061 |
| annotation.chuuki | 5513 | 35371 | 000005_53194, 000006_1869, 000006_3310, 000006_4627, 000006_46659 |
| annotation.layout_note | 8 | 19 | 000072_864, 000083_3329, 000259_3554, 000305_1896, 000311_2018 |
| break.line_explicit | 47 | 161 | 000075_4250, 000081_47027, 000083_46289, 000106_56858, 000106_57905 |
| break.page_line | 943 | 9229 | 000005_53194, 000006_1869, 000009_55881, 000011_889, 000011_899 |
| caption.block | 48 | 1726 | 000058_57440, 000091_50354, 000125_1321, 000165_49567, 000226_1150 |
| caption.inline | 143 | 2397 | 000014_728, 000058_57440, 000067_1768, 000067_1788, 000067_1789 |
| decoration.bold_italic | 319 | 8830 | 000020_4487, 000020_46404, 000025_1144, 000025_56503, 000026_50241 |
| decoration.boten | 6236 | 129086 | 000005_53194, 000006_382, 000006_383, 000006_58819, 000008_1083 |
| decoration.bousen | 327 | 18127 | 000006_1869, 000013_11, 000019_4376, 000034_55507, 000038_1408 |
| decoration.direction_override | 6 | 65 | 000096_935, 000866_3039, 001094_42603, 001242_46444, 001467_50733 |
| decoration.font_size | 1223 | 51508 | 000006_1869, 000008_58922, 000011_899, 000019_4376, 000019_58861 |
| decoration.keigakomi | 157 | 717 | 000043_341, 000063_385, 000067_1789, 000072_408, 000072_864 |
| decoration.typeface | 3 | 6 | 000212_4839, 000311_33191, 001917_61172 |
| emphasis.basic | 6576 | 157494 | 000005_53194, 000006_1869, 000006_382, 000006_383, 000006_58819 |
| figure.image_caption | 103 | 1777 | 000019_42378, 000019_42379, 000019_42380, 000019_42381, 000019_42382 |
| figure.image_inline | 531 | 5877 | 000009_226, 000009_45340, 000009_50711, 000009_50712, 000009_50713 |
| gaiji.jis_code | 5772 | 55322 | 000005_5, 000006_1869, 000006_3310, 000006_383, 000006_384 |
| gaiji.marker | 5931 | 62355 | 000005_5, 000006_1868, 000006_1869, 000006_3310, 000006_383 |
| gaiji.un_embed | 38 | 144 | 000019_4376, 000025_kantou, 000038_323, 000040_1326, 000040_737 |
| gaiji.unicode_codepoint | 631 | 3714 | 000008_47357, 000008_47386, 000020_55103, 000022_42254, 000023_1698 |
| gaiji_ruby.inline_base | 2799 | 13450 | 000005_5, 000006_1869, 000006_3310, 000008_1083, 000008_47357 |
| glyph.variant_note | 1954 | 7634 | 000006_1869, 000008_1083, 000008_47386, 000011_55301, 000011_889 |
| heading.basic | 3696 | 80592 | 000005_53194, 000006_58819, 000008_1083, 000008_47357, 000008_47386 |
| heading.dogyo | 142 | 11839 | 000011_899, 000058_59060, 000067_1790, 000067_4869, 000081_1058 |
| heading.mado | 6 | 1680 | 000255_47342, 000296_1864, 000961_4820, 001402_49946, 001404_49966 |
| indentation.basic | 8169 | 134393 | 000005_5, 000005_53194, 000006_1868, 000006_1869, 000006_3310 |
| indentation.burasage | 1217 | 13278 | 000006_3311, 000006_58819, 000008_47386, 000009_55881, 000019_4376 |
| indentation.chitsuki | 6276 | 20358 | 000006_1868, 000006_1869, 000006_3310, 000006_3311, 000006_382 |
| indentation.jisage_block | 4607 | 94993 | 000005_5, 000005_53194, 000006_1868, 000006_1869, 000006_3310 |
| indentation.jisage_oneline | 122 | 279 | 000025_202, 000025_216, 000035_235, 000035_266, 000038_42207 |
| indentation.jizume | 242 | 3239 | 000026_55781, 000034_55507, 000040_47289, 000050_48400, 000055_56499 |
| iteration.kunoji | 1125 | 10701 | 000006_58810, 000006_58819, 000008_47357, 000012_1092, 000012_24448 |
| kunten.kaeriten | 490 | 27987 | 000006_1869, 000038_1408, 000042_1694, 000050_3581, 000051_1436 |
| kunten.okurigana | 256 | 6562 | 000026_50238, 000026_50242, 000026_50259, 000026_51893, 000026_55774 |
| layout.center_page | 263 | 885 | 000005_53194, 000009_55881, 000011_899, 000025_47220, 000026_219 |
| layout.multicolumn | 26 | 73 | 000035_1566, 000042_2471, 000061_377, 000081_45631, 000082_1309 |
| layout.tcy | 725 | 19794 | 000014_728, 000020_2223, 000020_46404, 000023_55306, 000023_55324 |
| layout.yokogumi | 423 | 3690 | 000019_59374, 000025_1144, 000026_50239, 000026_55717, 000026_55732 |
| reference.frontref | 683 | 3921 | 000006_46659, 000012_2585, 000012_4316, 000022_197, 000026_46578 |
| ruby.basic | 14321 | 3607926 | 000005_5, 000005_53194, 000005_55215, 000005_55216, 000005_55217 |
| ruby.placement_directional | 31 | 318 | 000034_1213, 000050_3581, 000129_694, 000146_49258, 000146_50202 |
| source.note_label | 531 | 1381 | 000008_1083, 000012_198, 000013_542, 000019_4376, 000026_50239 |
| source.page_reference | 6 | 58 | 000160_875, 000989_351, 000989_352, 000989_42687, 001518_51731 |
| source.reviewed_residual_command | 189 | 603 | 000008_1083, 000026_55776, 000048_358, 000051_1441, 000051_1442 |
| structure.quote_block | 6 | 21 | 000034_233, 000035_296, 000137_733, 000280_1706, 000989_351 |
| structure.table | 7 | 46 | 000042_2345, 000042_2348, 000042_2449, 000096_2100, 000311_2745 |
| warichu.basic | 362 | 6605 | 000005_53194, 000006_1868, 000006_1869, 000034_519, 000038_42202 |
| warigaki.parenthetical | 1 | 2 | 000034_519 |

## Unknown Source Markers

None.

## Unknown Source Marker Classes

Showing 8 report rows of 8 total classes. JSON carries 8 top classes. truncated: false

| kind | raw | occurrences | unallowlisted | allowlisted | samples |
|---|---|---:|---:|---:|---|
| CommandFullwidth | ［＃］ | 13920 | 0 | 13920 | 000005_5, 000005_53194, 000006_1868, 000006_1869, 000006_3310 |
| CommandFullwidth | ［＃…］ | 465 | 0 | 465 | 000038_1408, 000042_1694, 000050_3581, 000051_1436, 000051_1452 |
| CommandFullwidth | ［＃本文終わり］ | 243 | 0 | 243 | 000009_226, 000009_50711, 000009_50712, 000009_50713, 000009_50714 |
| CommandFullwidth | ［＃（…）］ | 242 | 0 | 242 | 000026_50238, 000026_50242, 000026_50259, 000026_51893, 000026_55774 |
| MalformedCommand | ［＃ | 8 | 0 | 8 | 000081_4461, 000106_2415, 000121_1754, 000124_662, 000148_798 |
| MalformedAccentNotation | 〔 | 6 | 0 | 6 | 000091_522, 000311_16002, 001341_60380, 001341_60385, 001404_49966 |
| MalformedGaiji | ※［＃ | 1 | 0 | 1 | 000271_1556 |
| MalformedImplicitRuby | 《 | 1 | 0 | 1 | JISTABLE |

## Decode Failures

None.

## Inputs

- matrix: `/home/bor/Projects/ab-validator/data/aozora-syntax-coverage.toml`
- index: `scratch/ab-index.json`
- corpus: `references/aozorabunko`
- allowlist: `/home/bor/Projects/ab-validator/data/aozora-source-inventory-allowlist.toml`
