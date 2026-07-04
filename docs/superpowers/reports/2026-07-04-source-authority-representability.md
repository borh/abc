# Source Authority Representability Inventory

## Verdict

- source_authority_gate: `SOURCE_AUTHORITY_GATE_FAILING_REVIEW_REQUIRED`
- note: this is not a passing representability gate; durable representability claims remain blocked until strict_errors is empty.
- strict_errors:
  - source inventory row annotation.bouki has occurrences but no representability table
  - source inventory row annotation.chuuki has occurrences but no representability table
  - source inventory row break.line_explicit has occurrences but no representability table
  - source inventory row break.page_line has representability.status = needs_research
  - source inventory row caption.block has occurrences but no representability table
  - source inventory row caption.inline has occurrences but no representability table
  - source inventory row decoration.bold_italic has occurrences but no representability table
  - source inventory row decoration.boten has occurrences but no representability table
  - source inventory row decoration.bousen has occurrences but no representability table
  - source inventory row decoration.direction_override has occurrences but no representability table
  - source inventory row decoration.keigakomi has occurrences but no representability table
  - source inventory row emphasis.basic has representability.status = needs_research
  - source inventory row figure.image_inline has occurrences but no representability table
  - source inventory row gaiji.jis_code has occurrences but no representability table
  - source inventory row gaiji.un_embed has occurrences but no representability table
  - source inventory row gaiji.unicode_codepoint has occurrences but no representability table
  - source inventory row heading.dogyo has occurrences but no representability table
  - source inventory row heading.mado has occurrences but no representability table
  - source inventory row indentation.basic has representability.status = needs_research
  - source inventory row indentation.burasage has occurrences but no representability table
  - source inventory row indentation.chitsuki has occurrences but no representability table
  - source inventory row indentation.jisage_block has occurrences but no representability table
  - source inventory row indentation.jisage_oneline has occurrences but no representability table
  - source inventory row iteration.kunoji has occurrences but no representability table
  - source inventory row kunten.kaeriten has occurrences but no representability table
  - source inventory row kunten.okurigana has occurrences but no representability table
  - source inventory row layout.tcy has occurrences but no representability table
  - source inventory row layout.yokogumi has occurrences but no representability table
  - source inventory row reference.frontref has occurrences but no representability table
  - source inventory row ruby.placement_directional has occurrences but no representability table
  - source inventory row warigaki.parenthetical has occurrences but no representability table
  - 224028 unallowlisted source markers

## Summary

- works_scanned: 17894
- works_failed: 0
- markers_total: 4335091
- unknown_markers_total: 224028
- unallowlisted_unknown_markers_total: 224028
- allowlisted_unknown_markers_total: 0

## Representability

- typed_occurrences: 3758493
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
| emphasis.basic | 6576 | 157495 | 000005_53194, 000006_1869, 000006_382, 000006_383, 000006_58819 |
| figure.image_caption | 103 | 1777 | 000019_42378, 000019_42379, 000019_42380, 000019_42381, 000019_42382 |
| figure.image_inline | 504 | 5812 | 000009_226, 000009_50711, 000009_50712, 000009_50713, 000009_50714 |
| gaiji.jis_code | 5772 | 55331 | 000005_5, 000006_1869, 000006_3310, 000006_383, 000006_384 |
| gaiji.marker | 5930 | 62372 | 000005_5, 000006_1868, 000006_1869, 000006_3310, 000006_383 |
| gaiji.un_embed | 38 | 144 | 000019_4376, 000025_kantou, 000038_323, 000040_1326, 000040_737 |
| gaiji.unicode_codepoint | 631 | 3719 | 000008_47357, 000008_47386, 000020_55103, 000022_42254, 000023_1698 |
| gaiji_ruby.inline_base | 2799 | 13451 | 000005_5, 000006_1869, 000006_3310, 000008_1083, 000008_47357 |
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
| 000005_5 | 21 | CommandFullwidth | ［＃ここで字下げ終わり］ | ここで字下げ終わり |
| 000005_53194 | 11 | MalformedRuby | ｜ | ｜ |
| 000005_53194 | 14 | CommandFullwidth | ［＃］ |  |
| 000005_53194 | 24 | CommandFullwidth | ［＃改丁］ | 改丁 |
| 000005_53194 | 290 | CommandFullwidth | ［＃割り注終わり］ | 割り注終わり |
| 000005_53194 | 297 | CommandFullwidth | ［＃ここで字下げ終わり］ | ここで字下げ終わり |
| 000005_53194 | 549 | CommandFullwidth | ［＃割り注終わり］ | 割り注終わり |
| 000005_53194 | 652 | CommandFullwidth | ［＃「……』」は底本では「……」」］ | 「……』」は底本では「……」」 |
| 000005_53194 | 676 | CommandFullwidth | ［＃ここで字下げ終わり］ | ここで字下げ終わり |
| 000006_1868 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_1868 | 13 | CommandFullwidth | ［＃］ |  |
| 000006_1868 | 20 | CommandFullwidth | ［＃ここで字下げ終わり］ | ここで字下げ終わり |
| 000006_1868 | 22 | CommandFullwidth | ［＃ここで割り注終わり］ | ここで割り注終わり |
| 000006_1869 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_1869 | 13 | CommandFullwidth | ［＃］ |  |
| 000006_1869 | 24 | CommandFullwidth | ［＃ここで字下げ終わり］ | ここで字下げ終わり |
| 000006_1869 | 25 | CommandFullwidth | ［＃地から２字上げ］ | 地から２字上げ |
| 000006_1869 | 34 | CommandFullwidth | ［＃ここで字下げ終わり］ | ここで字下げ終わり |
| 000006_1869 | 35 | CommandFullwidth | ［＃地から２字上げ］ | 地から２字上げ |
| 000006_1869 | 79 | CommandFullwidth | ［＃ここで字下げ終わり］ | ここで字下げ終わり |
| 000006_1869 | 293 | CommandFullwidth | ［＃ここで割り注終わり］ | ここで割り注終わり |
| 000006_1869 | 481 | CommandFullwidth | ［＃改丁］ | 改丁 |
| 000006_1869 | 688 | CommandFullwidth | ［＃ここで字下げ終わり］ | ここで字下げ終わり |
| 000006_1869 | 730 | CommandFullwidth | ［＃ここで割り注終わり］ | ここで割り注終わり |
| 000006_1869 | 917 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_1869 | 927 | CommandFullwidth | ［＃ここで字下げ終わり］ | ここで字下げ終わり |
| 000006_1869 | 933 | CommandFullwidth | ［＃ここで字下げ終わり］ | ここで字下げ終わり |
| 000006_1869 | 993 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_1869 | 993 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_1869 | 1319 | CommandFullwidth | ［＃改丁］ | 改丁 |
| 000006_1869 | 1328 | CommandFullwidth | ［＃ここで字下げ終わり］ | ここで字下げ終わり |
| 000006_1869 | 1558 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_1869 | 1558 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_1869 | 1558 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_1869 | 1559 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_1869 | 1559 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_3310 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_3310 | 13 | CommandFullwidth | ［＃］ |  |
| 000006_3310 | 283 | CommandFullwidth | ［＃ここで字下げ終わり］ | ここで字下げ終わり |
| 000006_3310 | 316 | CommandFullwidth | ［＃「涕」はママ］ | 「涕」はママ |
| 000006_3310 | 827 | CommandFullwidth | ［＃地から２字上げ］ | 地から２字上げ |
| 000006_3310 | 831 | CommandFullwidth | ［＃ここで字下げ終わり］ | ここで字下げ終わり |
| 000006_3311 | 10 | CommandFullwidth | ［＃］ |  |
| 000006_3311 | 11 | CommandFullwidth | ［＃ここから改行天付き、折り返して１字下げ］ | ここから改行天付き、折り返して１字下げ |
| 000006_3311 | 16 | CommandFullwidth | ［＃ここから改行天付き、折り返して１字下げ］ | ここから改行天付き、折り返して１字下げ |
| 000006_3311 | 20 | CommandFullwidth | ［＃ここで字下げ終わり］ | ここで字下げ終わり |
| 000006_3311 | 24 | CommandFullwidth | ［＃ここで字下げ終わり］ | ここで字下げ終わり |
| 000006_3311 | 25 | CommandFullwidth | ［＃地から２字上げ］ | 地から２字上げ |
| 000006_3311 | 29 | CommandFullwidth | ［＃ここで字下げ終わり］ | ここで字下げ終わり |
| 000006_3311 | 35 | CommandFullwidth | ［＃ここから改行天付き、折り返して１字下げ］ | ここから改行天付き、折り返して１字下げ |
| 000006_3311 | 43 | CommandFullwidth | ［＃ここで字下げ終わり］ | ここで字下げ終わり |
| 000006_3311 | 46 | CommandFullwidth | ［＃ここで字下げ終わり］ | ここで字下げ終わり |
| 000006_382 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_382 | 13 | CommandFullwidth | ［＃］ |  |
| 000006_383 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_383 | 13 | CommandFullwidth | ［＃］ |  |
| 000006_384 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_384 | 13 | CommandFullwidth | ［＃］ |  |
| 000006_4627 | 11 | CommandFullwidth | ［＃］ |  |
| 000006_4627 | 37 | AccentNotation | 〔はない〕 | はない |
| 000006_4627 | 37 | CommandFullwidth | ［＃「齷齪」は底本では「齷齦」］ | 「齷齪」は底本では「齷齦」 |
| 000006_46659 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_46659 | 13 | CommandFullwidth | ［＃］ |  |
| 000006_46659 | 25 | CommandFullwidth | ［＃地から２字上げ］ | 地から２字上げ |
| 000006_47064 | 48 | AccentNotation | 〔日本語〕 | 日本語 |
| 000006_58810 | 23 | CommandFullwidth | ［＃地から１字上げ］ | 地から１字上げ |
| 000006_58819 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_58819 | 13 | CommandFullwidth | ［＃］ |  |
| 000006_58819 | 29 | CommandFullwidth | ［＃ここで字下げ終わり］ | ここで字下げ終わり |
| 000006_58819 | 35 | CommandFullwidth | ［＃ここで字下げ終わり］ | ここで字下げ終わり |
| 000006_58819 | 37 | CommandFullwidth | ［＃ここから改行天付き、折り返して１字下げ］ | ここから改行天付き、折り返して１字下げ |
| 000006_58819 | 41 | CommandFullwidth | ［＃ここで字下げ終わり］ | ここで字下げ終わり |
| 000006_58819 | 57 | CommandFullwidth | ［＃地から２字上げ］ | 地から２字上げ |
| 000006_901 | 10 | CommandFullwidth | ［＃］ |  |
| 000008_1083 | 10 | CommandFullwidth | ［＃］ |  |
| 000008_1083 | 11 | CommandFullwidth | ［＃底本は「俵」を「依」と誤植］ | 底本は「俵」を「依」と誤植 |
| 000008_1083 | 66 | CommandFullwidth | ［＃底本は改行天付き］ | 底本は改行天付き |
| 000008_1083 | 92 | CommandFullwidth | ［＃底本は「俵」を「依」と誤植］ | 底本は「俵」を「依」と誤植 |
| 000008_1083 | 92 | CommandFullwidth | ［＃底本は「ただまま」を「ただま」と誤植］ | 底本は「ただまま」を「ただま」と誤植 |
| 000008_1083 | 103 | CommandFullwidth | ［＃底本ではここのみ「莚」。他は「筵」］ | 底本ではここのみ「莚」。他は「筵」 |
| 000008_1083 | 112 | CommandFullwidth | ［＃「仁王立ちになった」は底本では「仁王立ち」と誤植］ | 「仁王立ちになった」は底本では「仁王立ち」と誤植 |
| 000008_1083 | 204 | CommandFullwidth | ［＃底本は「咤」を「口+它」と誤植］ | 底本は「咤」を「口+它」と誤植 |
| 000008_1083 | 209 | CommandFullwidth | ［＃「ヤンに傍点］ | 「ヤンに傍点 |
| 000008_1083 | 248 | CommandFullwidth | ［＃２０字下げて、地より１字あきで］ | ２０字下げて、地より１字あきで |
| 000008_1083 | 256 | CommandFullwidth | ［＃初出時「………………………行ぐ奴からさかしまに……………やるまでよ！」］ | 初出時「………………………行ぐ奴からさかしまに……………やるまでよ！」 |
| 000008_1083 | 257 | CommandFullwidth | ［＃初出時「……よ、……に目がつかんかい、地主に。」］ | 初出時「……よ、……に目がつかんかい、地主に。」 |
| 000008_1083 | 258 | CommandFullwidth | ［＃初出時「貧乏人同士みんなして……………………。」］ | 初出時「貧乏人同士みんなして……………………。」 |
| 000008_1083 | 259 | CommandFullwidth | ［＃初出時「…………………………………………………工夫するこった。」］ | 初出時「…………………………………………………工夫するこった。」 |
| 000008_1083 | 260 | CommandFullwidth | ［＃初出時「「………………行く奴に金を返せって法があるかい、」］ | 初出時「「………………行く奴に金を返せって法があるかい、」 |
| 000008_1083 | 261 | CommandFullwidth | ［＃初出時「そんなことで……に勝てっかい！」］ | 初出時「そんなことで……に勝てっかい！」 |
| 000008_1083 | 262 | CommandFullwidth | ［＃初出時「世のなかの………がちゃんとそういうふうにできているんだ。」］ | 初出時「世のなかの………がちゃんとそういうふうにできているんだ。」 |
| 000008_1083 | 263 | CommandFullwidth | ［＃初出時「つまり二重に……………………。」］ | 初出時「つまり二重に……………………。」 |
| 000008_1083 | 264 | CommandFullwidth | ［＃初出時「三重にも……………………………………。」］ | 初出時「三重にも……………………………………。」 |
| 000008_1083 | 265 | CommandFullwidth | ［＃初出時「こないだみてえに折角かたまって………も」］ | 初出時「こないだみてえに折角かたまって………も」 |
| 000008_1083 | 267 | CommandFullwidth | ［＃創元社版は「小船頭」］ | 創元社版は「小船頭」 |
| 000008_1083 | 268 | CommandFullwidth | ［＃創元社版では「破損」］ | 創元社版では「破損」 |
| 000008_1083 | 10 | CommandFullwidth | ［＃］ |  |
| 000008_1083 | 67 | CommandFullwidth | ［＃「　がやがやと」は底本では「がやがやと」］ | 「　がやがやと」は底本では「がやがやと」 |

## Unknown Source Marker Classes

Showing 50 report rows of 29995 total classes. JSON carries 1000 top classes. truncated: true

| kind | raw | occurrences | unallowlisted | allowlisted | samples |
|---|---|---:|---:|---:|---|
| CommandFullwidth | ［＃ここで字下げ終わり］ | 44055 | 44055 | 0 | 000005_5, 000005_53194, 000006_1868, 000006_1869, 000006_3310 |
| CommandFullwidth | ［＃］ | 13920 | 13920 | 0 | 000005_5, 000005_53194, 000006_1868, 000006_1869, 000006_3310 |
| MalformedRuby | ｜ | 9658 | 9658 | 0 | 000005_5, 000005_53194, 000006_1868, 000006_1869, 000006_3310 |
| CommandFullwidth | ［＃小さな文字終わり］ | 8908 | 8908 | 0 | 000011_899, 000019_58861, 000023_55325, 000023_55384, 000023_55390 |
| CommandFullwidth | ［＃１段階小さな文字］ | 8807 | 8807 | 0 | 000011_899, 000019_58861, 000023_55325, 000023_55384, 000023_55390 |
| CommandFullwidth | ［＃中見出し終わり］ | 7436 | 7436 | 0 | 000011_55301, 000011_899, 000014_728, 000019_59261, 000026_55730 |
| CommandFullwidth | ［＃ここから改行天付き、折り返して１字下げ］ | 6212 | 6212 | 0 | 000006_3311, 000006_58819, 000020_46404, 000025_216, 000025_47220 |
| CommandFullwidth | ［＃ここで小さな文字終わり］ | 5767 | 5767 | 0 | 000011_899, 000026_55781, 000034_55507, 000035_58054, 000042_43083 |
| CommandFullwidth | ［＃ここから１段階小さな文字］ | 5759 | 5759 | 0 | 000011_899, 000026_55781, 000034_55507, 000035_58054, 000042_43083 |
| CommandFullwidth | ［＃地から１字上げ］ | 5205 | 5205 | 0 | 000006_58810, 000008_1083, 000008_47361, 000008_47384, 000008_47386 |
| CommandFullwidth | ［＃行右小書き終わり］ | 4161 | 4161 | 0 | 000094_2523, 000094_2524, 000094_2525, 000094_2526, 000094_42338 |
| CommandFullwidth | ［＃行右小書き］ | 4161 | 4161 | 0 | 000094_2523, 000094_2524, 000094_2525, 000094_2526, 000094_42338 |
| CommandFullwidth | ［＃太字終わり］ | 3791 | 3791 | 0 | 000026_50241, 000035_52380, 000067_1789, 000083_57848, 000096_2100 |
| CommandFullwidth | ［＃地から２字上げ］ | 3668 | 3668 | 0 | 000006_1869, 000006_3310, 000006_3311, 000006_46659, 000006_58819 |
| CommandFullwidth | ［＃割り注終わり］ | 2938 | 2938 | 0 | 000005_53194, 000034_519, 000038_42202, 000057_43276, 000082_49526 |
| CommandFullwidth | ［＃小見出し終わり］ | 1803 | 1803 | 0 | 000011_899, 000026_894, 000067_1768, 000067_47566, 000106_52301 |
| CommandFullwidth | ［＃地から３字上げ］ | 1727 | 1727 | 0 | 000012_198, 000012_435, 000023_2951, 000025_1146, 000025_201 |
| CommandFullwidth | ［＃「＊」は行右小書き］ | 1615 | 1615 | 0 | 000019_58861, 000042_43074, 000055_59489, 000067_2843, 000075_47964 |
| CommandFullwidth | ［＃横組み］ | 1599 | 1599 | 0 | 000019_59374, 000026_55717, 000026_55732, 000026_55738, 000034_519 |
| CommandFullwidth | ［＃横組み終わり］ | 1589 | 1589 | 0 | 000019_59374, 000026_55717, 000026_55732, 000026_55738, 000034_519 |
| CommandFullwidth | ［＃「（c）」は縦中横］ | 1583 | 1583 | 0 | 001848_59469, 001848_59607, 001848_59608, 001849_59466, 001849_59467 |
| CommandFullwidth | ［＃ここで字詰め終わり］ | 1373 | 1373 | 0 | 000026_55781, 000034_55507, 000040_47289, 000050_48400, 000055_56499 |
| CommandFullwidth | ［＃傍点終わり］ | 1334 | 1334 | 0 | 000034_56908, 000042_61014, 000042_61015, 000042_61019, 000063_385 |
| CommandFullwidth | ［＃「（b）」は縦中横］ | 1297 | 1297 | 0 | 001848_59469, 001848_59607, 001848_59608, 001849_59466, 001849_59467 |
| CommandFullwidth | ［＃改丁］ | 1233 | 1233 | 0 | 000005_53194, 000006_1869, 000009_55881, 000011_889, 000011_899 |
| CommandFullwidth | ［＃「（a）」は縦中横］ | 1165 | 1165 | 0 | 001848_59469, 001848_59607, 001849_59466, 001849_59467 |
| CommandFullwidth | ［＃キャプション終わり］ | 1102 | 1102 | 0 | 000091_50354, 000125_1321, 000165_49567, 000226_1150, 000279_1704 |
| CommandFullwidth | ［＃大見出し終わり］ | 1011 | 1011 | 0 | 000011_899, 000034_519, 000034_56908, 000035_2282, 000050_43467 |
| CommandFullwidth | ［＃斜体終わり］ | 880 | 880 | 0 | 000067_1790, 000075_47967, 000094_2525, 000094_56535, 000218_46213 |
| AccentNotation | 〔〕 | 770 | 770 | 0 | 000020_745, 000026_50241, 000026_50245, 000026_50255, 000026_51891 |
| CommandFullwidth | ［＃「１）」は縦中横］ | 715 | 715 | 0 | 001149_43550, 001149_43551, 001149_45455, 001149_46188, 001569_52468 |
| CommandFullwidth | ［＃「b」は下付き小文字］ | 708 | 708 | 0 | 001185_45210 |
| CommandFullwidth | ［＃「a」は下付き小文字］ | 666 | 666 | 0 | 001185_45210 |
| CommandFullwidth | ［＃ここから改行天付き、折り返して２字下げ］ | 650 | 650 | 0 | 000035_1580, 000035_1581, 000061_377, 000063_385, 000067_859 |
| AccentNotation | 〔欄外に〕 | 586 | 586 | 0 | 000311_15981, 000311_15982, 000311_15995, 000311_16000, 000311_16002 |
| CommandFullwidth | ［＃ここから２６字詰め］ | 575 | 575 | 0 | 000082_49526, 000094_42338, 000106_2415, 000106_55787, 000118_613 |
| CommandFullwidth | ［＃「10」は縦中横］ | 552 | 552 | 0 | 000020_2223, 000020_46404, 000023_55390, 000026_50239, 000034_56908 |
| CommandFullwidth | ［＃改段］ | 506 | 506 | 0 | 000040_732, 000067_859, 000082_1304, 000083_46289, 000096_46708 |
| CommandFullwidth | ［＃「１）」は縦中横、行右小書き］ | 493 | 493 | 0 | 001149_43550, 001149_43551, 001149_45455 |
| CommandFullwidth | ［＃地から４字上げ］ | 482 | 482 | 0 | 000026_51309, 000026_51310, 000026_51311, 000026_51314, 000026_51315 |
| CommandFullwidth | ［＃…］ | 465 | 465 | 0 | 000038_1408, 000042_1694, 000050_3581, 000051_1436, 000051_1452 |
| CommandFullwidth | ［＃「大字」は１段階小さな文字］ | 462 | 462 | 0 | 001566_58320, 001566_58447, 001566_58544, 001566_58777 |
| CommandFullwidth | ［＃地から５字上げ］ | 411 | 411 | 0 | 000009_55882, 000035_1577, 000050_33223, 000050_33225, 000050_33226 |
| CommandFullwidth | ［＃ここから中見出し］ | 397 | 397 | 0 | 000026_219, 000026_894, 000037_45454, 000055_365, 000055_698 |
| CommandFullwidth | ［＃ここで中見出し終わり］ | 396 | 396 | 0 | 000026_219, 000026_894, 000037_45454, 000055_365, 000055_698 |
| CommandFullwidth | ［＃「字」は１段階小さな文字］ | 388 | 388 | 0 | 001566_58447, 001566_58544 |
| CommandFullwidth | ［＃「＊＊」は行右小書き］ | 358 | 358 | 0 | 000055_59489, 000067_2843, 000075_47964, 000218_46213, 000218_46226 |
| CommandFullwidth | ［＃「d」は下付き小文字］ | 336 | 336 | 0 | 001185_45210 |
| CommandFullwidth | ［＃ここから改行天付き、折り返して３字下げ］ | 321 | 321 | 0 | 000009_55881, 000035_1582, 000081_42346, 000082_49527, 000082_49544 |
| CommandFullwidth | ［＃ここから２５字詰め］ | 319 | 319 | 0 | 000106_50900, 000106_52390, 000106_52391, 000106_52958, 000106_53049 |

## Decode Failures

None.

## Inputs

- matrix: `/home/bor/Projects/ab-validator/data/aozora-syntax-coverage.toml`
- index: `scratch/ab-index.json`
- corpus: `/home/bor/Dependencies/aozorabunko`
- allowlist: `/home/bor/Projects/ab-validator/data/aozora-source-inventory-allowlist.toml`
