# Source Authority Representability Inventory

## Verdict

- source_authority_gate: `SOURCE_AUTHORITY_GATE_FAILING_REVIEW_REQUIRED`
- note: this is not a passing representability gate; durable representability claims remain blocked until strict_errors is empty.
- strict_errors:
  - source inventory row heading.dogyo has occurrences but no representability table
  - source inventory row heading.mado has occurrences but no representability table
  - source inventory row iteration.kunoji has occurrences but no representability table
  - source inventory row kunten.kaeriten has occurrences but no representability table
  - source inventory row kunten.okurigana has occurrences but no representability table
  - source inventory row layout.yokogumi has occurrences but no representability table
  - source inventory row reference.frontref has occurrences but no representability table
  - source inventory row ruby.placement_directional has occurrences but no representability table
  - source inventory row warigaki.parenthetical has occurrences but no representability table
  - 12973 unallowlisted source markers

## Scope

This is a source-markup authority gate: every reached explicit Aozora Bunko marker must have a reviewed representation and, for represented rows, a TEI P5 projection target. Semantic TEI enrichment such as named-entity, speech, role, or place annotation is outside this gate and remains a downstream editorial layer.

## Summary

- works_scanned: 17894
- works_failed: 0
- markers_total: 4325244
- unknown_markers_total: 26893
- unallowlisted_unknown_markers_total: 12973
- allowlisted_unknown_markers_total: 13920

## Representability

- typed_occurrences: 4364879
- raw_preserved_occurrences: 25072
- out_of_body_occurrences: 0
- unsupported_occurrences: 13920
- needs_research_occurrences: 0

## Rows

| row | works | occurrences | samples |
|---|---:|---:|---|
| annotation.bouki | 12 | 127 | 000031_2846, 000037_2848, 000156_2699, 000219_2932, 000287_3061 |
| annotation.chuuki | 3932 | 24880 | 000006_1869, 000006_4627, 000006_47064, 000008_1083, 000008_47357 |
| break.line_explicit | 47 | 161 | 000075_4250, 000081_47027, 000083_46289, 000106_56858, 000106_57905 |
| break.page_line | 943 | 10028 | 000005_53194, 000006_1869, 000009_55881, 000011_889, 000011_899 |
| caption.block | 47 | 1722 | 000058_57440, 000091_50354, 000125_1321, 000165_49567, 000226_1150 |
| caption.inline | 136 | 1040 | 000014_728, 000067_1768, 000067_1788, 000067_1789, 000093_1916 |
| decoration.bold_italic | 141 | 5105 | 000025_1144, 000026_50241, 000026_55916, 000035_52380, 000067_1789 |
| decoration.boten | 79 | 1376 | 000034_56908, 000042_61014, 000042_61015, 000042_61019, 000048_45476 |
| decoration.bousen | 317 | 18067 | 000006_1869, 000013_11, 000019_4376, 000034_55507, 000038_1408 |
| decoration.direction_override | 6 | 65 | 000096_935, 000866_3039, 001094_42603, 001242_46444, 001467_50733 |
| decoration.font_size | 1166 | 51097 | 000008_58922, 000011_899, 000019_58861, 000019_59261, 000019_59375 |
| decoration.keigakomi | 112 | 444 | 000063_385, 000067_1789, 000072_408, 000096_2093, 000096_2100 |
| emphasis.basic | 6576 | 157495 | 000005_53194, 000006_1869, 000006_382, 000006_383, 000006_58819 |
| figure.image_caption | 103 | 1777 | 000019_42378, 000019_42379, 000019_42380, 000019_42381, 000019_42382 |
| figure.image_inline | 504 | 5812 | 000009_226, 000009_50711, 000009_50712, 000009_50713, 000009_50714 |
| gaiji.jis_code | 5772 | 55331 | 000005_5, 000006_1869, 000006_3310, 000006_383, 000006_384 |
| gaiji.marker | 5930 | 62372 | 000005_5, 000006_1868, 000006_1869, 000006_3310, 000006_383 |
| gaiji.un_embed | 38 | 144 | 000019_4376, 000025_kantou, 000038_323, 000040_1326, 000040_737 |
| gaiji.unicode_codepoint | 631 | 3719 | 000008_47357, 000008_47386, 000020_55103, 000022_42254, 000023_1698 |
| gaiji_ruby.inline_base | 2799 | 13451 | 000005_5, 000006_1869, 000006_3310, 000008_1083, 000008_47357 |
| heading.basic | 3693 | 80336 | 000005_53194, 000006_58819, 000008_1083, 000008_47357, 000008_47386 |
| heading.dogyo | 142 | 11839 | 000011_899, 000058_59060, 000067_1790, 000067_4869, 000081_1058 |
| heading.mado | 6 | 1680 | 000255_47342, 000296_1864, 000961_4820, 001402_49946, 001404_49966 |
| indentation.basic | 8169 | 134394 | 000005_5, 000005_53194, 000006_1868, 000006_1869, 000006_3310 |
| indentation.burasage | 1203 | 13236 | 000006_3311, 000006_58819, 000008_47386, 000009_55881, 000019_4376 |
| indentation.chitsuki | 6171 | 20073 | 000006_1868, 000006_1869, 000006_3310, 000006_3311, 000006_382 |
| indentation.jisage_block | 4599 | 89428 | 000005_5, 000005_53194, 000006_1868, 000006_1869, 000006_3310 |
| indentation.jisage_oneline | 121 | 278 | 000025_202, 000025_216, 000035_235, 000035_266, 000038_42207 |
| indentation.jizume | 242 | 3239 | 000026_55781, 000034_55507, 000040_47289, 000050_48400, 000055_56499 |
| iteration.kunoji | 1125 | 10700 | 000006_58810, 000006_58819, 000008_47357, 000012_1092, 000012_24448 |
| kunten.kaeriten | 488 | 28082 | 000006_1869, 000038_1408, 000042_1694, 000050_3581, 000051_1436 |
| kunten.okurigana | 256 | 6562 | 000026_50238, 000026_50242, 000026_50259, 000026_51893, 000026_55774 |
| layout.tcy | 715 | 19746 | 000014_728, 000020_2223, 000020_46404, 000023_55306, 000023_55324 |
| layout.yokogumi | 403 | 3549 | 000019_59374, 000026_55717, 000026_55732, 000026_55738, 000034_519 |
| reference.frontref | 682 | 3920 | 000006_46659, 000012_2585, 000012_4316, 000022_197, 000026_46578 |
| ruby.basic | 14321 | 3608407 | 000005_5, 000005_53194, 000005_55215, 000005_55216, 000005_55217 |
| ruby.placement_directional | 31 | 312 | 000034_1213, 000050_3581, 000129_694, 000146_49258, 000146_50202 |
| warichu.basic | 361 | 6601 | 000005_53194, 000006_1868, 000006_1869, 000034_519, 000038_42202 |
| warigaki.parenthetical | 1 | 1 | 000034_519 |

## Unknown Source Markers

| work_id | line | kind | raw | body |
|---|---:|---|---|---|
| 000005_53194 | 652 | CommandFullwidth | ［＃「……』」は底本では「……」」］ | 「……』」は底本では「……」」 |
| 000006_1869 | 917 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_1869 | 993 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_1869 | 993 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_1869 | 1558 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_1869 | 1558 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_1869 | 1558 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_1869 | 1559 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_1869 | 1559 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_3310 | 316 | CommandFullwidth | ［＃「涕」はママ］ | 「涕」はママ |
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
| 000008_1083 | 260 | CommandFullwidth | ［＃初出時「………………………行ぐ奴からさかしまに……………やるまでよ！」］ | 初出時「………………………行ぐ奴からさかしまに……………やるまでよ！」 |
| 000008_1083 | 261 | CommandFullwidth | ［＃初出時「……よ、……に目がつかんかい、地主に。」］ | 初出時「……よ、……に目がつかんかい、地主に。」 |
| 000008_1083 | 262 | CommandFullwidth | ［＃初出時「貧乏人同士みんなして……………………。」］ | 初出時「貧乏人同士みんなして……………………。」 |
| 000008_1083 | 263 | CommandFullwidth | ［＃初出時「…………………………………………………工夫するこった。」］ | 初出時「…………………………………………………工夫するこった。」 |
| 000008_1083 | 264 | CommandFullwidth | ［＃初出時「「………………行く奴に金を返せって法があるかい、」］ | 初出時「「………………行く奴に金を返せって法があるかい、」 |
| 000008_1083 | 265 | CommandFullwidth | ［＃初出時「そんなことで……に勝てっかい！」］ | 初出時「そんなことで……に勝てっかい！」 |
| 000008_1083 | 266 | CommandFullwidth | ［＃初出時「世のなかの………がちゃんとそういうふうにできているんだ。」］ | 初出時「世のなかの………がちゃんとそういうふうにできているんだ。」 |
| 000008_1083 | 267 | CommandFullwidth | ［＃初出時「つまり二重に……………………。」］ | 初出時「つまり二重に……………………。」 |
| 000008_1083 | 268 | CommandFullwidth | ［＃初出時「三重にも……………………………………。」］ | 初出時「三重にも……………………………………。」 |
| 000008_1083 | 269 | CommandFullwidth | ［＃初出時「こないだみてえに折角かたまって………も」］ | 初出時「こないだみてえに折角かたまって………も」 |
| 000008_1083 | 271 | CommandFullwidth | ［＃創元社版は「小船頭」］ | 創元社版は「小船頭」 |
| 000008_1083 | 272 | CommandFullwidth | ［＃創元社版では「破損」］ | 創元社版では「破損」 |
| 000008_47357 | 367 | CommandFullwidth | ［＃「』」は、底本では「」」］ | 「』」は、底本では「」」 |
| 000008_47357 | 467 | CommandFullwidth | ［＃「コップ」はママ］ | 「コップ」はママ |
| 000008_47357 | 1138 | CommandFullwidth | ［＃「」とあげて」は底本では「」　とあげて」］ | 「」とあげて」は底本では「」　とあげて」 |
| 000008_47386 | 253 | CommandFullwidth | ［＃ルビの「いつとき」は底本では「ひつとき」］ | ルビの「いつとき」は底本では「ひつとき」 |
| 000008_47386 | 448 | CommandFullwidth | ［＃「伊貝にしたって」はママ］ | 「伊貝にしたって」はママ |
| 000008_58922 | 20 | MalformedRuby | ｜ | ｜ |
| 000008_58922 | 20 | MalformedRuby | ｜ | ｜ |
| 000009_226 | 314 | CommandFullwidth | ［＃本文終わり］ | 本文終わり |
| 000009_42929 | 16 | CommandFullwidth | ［＃「」」は底本では欠落］ | 「」」は底本では欠落 |
| 000009_42929 | 53 | CommandFullwidth | ［＃「」」は底本では欠落］ | 「」」は底本では欠落 |
| 000009_42929 | 93 | CommandFullwidth | ［＃「「」は底本では一文後にある］ | 「「」は底本では一文後にある |
| 000009_42929 | 114 | CommandFullwidth | ［＃空白は底本では「「」］ | 空白は底本では「「」 |
| 000009_42929 | 114 | CommandFullwidth | ［＃「。」は底本では「」」］ | 「。」は底本では「」」 |
| 000009_42929 | 174 | CommandFullwidth | ［＃「、」は底本では欠落］ | 「、」は底本では欠落 |
| 000009_43028 | 99 | MalformedRuby | ｜ | ｜ |
| 000009_43497 | 166 | CommandFullwidth | ［＃「小さいな」はママ］ | 「小さいな」はママ |
| 000009_43522 | 71 | CommandFullwidth | ［＃「弟」は誤訳で本当は「兄」］ | 「弟」は誤訳で本当は「兄」 |
| 000009_43522 | 100 | CommandFullwidth | ［＃「・」は底本では欠落］ | 「・」は底本では欠落 |
| 000009_43523 | 100 | CommandFullwidth | ［＃「。」は底本では欠落］ | 「。」は底本では欠落 |
| 000009_43523 | 209 | CommandFullwidth | ［＃「」」は底本では欠落］ | 「」」は底本では欠落 |
| 000009_45340 | 58 | CommandFullwidth | ［＃図１入る］ | 図１入る |
| 000009_45340 | 95 | CommandFullwidth | ［＃図２入る］ | 図２入る |
| 000009_45340 | 99 | CommandFullwidth | ［＃図３入る］ | 図３入る |
| 000009_45340 | 108 | CommandFullwidth | ［＃図４入る］ | 図４入る |
| 000009_45340 | 127 | CommandFullwidth | ［＃図５入る］ | 図５入る |
| 000009_45340 | 246 | CommandFullwidth | ［＃図６入る］ | 図６入る |
| 000009_45340 | 246 | CommandFullwidth | ［＃図６入る］ | 図６入る |
| 000009_45340 | 248 | CommandFullwidth | ［＃図７入る］ | 図７入る |
| 000009_45340 | 337 | CommandFullwidth | ［＃図８入る］ | 図８入る |
| 000009_50711 | 252 | CommandFullwidth | ［＃本文終わり］ | 本文終わり |
| 000009_50712 | 261 | CommandFullwidth | ［＃本文終わり］ | 本文終わり |
| 000009_50713 | 253 | CommandFullwidth | ［＃本文終わり］ | 本文終わり |
| 000009_50714 | 14 | CommandFullwidth | ［＃ここから２字下げ、地から３字下げ］ | ここから２字下げ、地から３字下げ |
| 000009_50714 | 17 | CommandFullwidth | ［＃ここから２字下げ、地から３字下げ］ | ここから２字下げ、地から３字下げ |
| 000009_50714 | 188 | CommandFullwidth | ［＃本文終わり］ | 本文終わり |
| 000009_50715 | 210 | CommandFullwidth | ［＃本文終わり］ | 本文終わり |
| 000009_50716 | 184 | CommandFullwidth | ［＃本文終わり］ | 本文終わり |
| 000009_50717 | 287 | CommandFullwidth | ［＃本文終わり］ | 本文終わり |
| 000009_50718 | 209 | CommandFullwidth | ［＃本文終わり］ | 本文終わり |
| 000009_54910 | 162 | CommandFullwidth | ［＃本文終わり］ | 本文終わり |
| 000009_54911 | 154 | CommandFullwidth | ［＃本文終わり］ | 本文終わり |
| 000009_54912 | 227 | CommandFullwidth | ［＃本文終わり］ | 本文終わり |
| 000009_54913 | 217 | CommandFullwidth | ［＃本文終わり］ | 本文終わり |
| 000009_54914 | 187 | CommandFullwidth | ［＃本文終わり］ | 本文終わり |
| 000009_54915 | 238 | CommandFullwidth | ［＃本文終わり］ | 本文終わり |
| 000009_55881 | 189 | CommandFullwidth | ［＃ここで字下げおわり］ | ここで字下げおわり |
| 000009_55881 | 951 | CommandFullwidth | ［＃本文終わり］ | 本文終わり |
| 000009_55882 | 236 | CommandFullwidth | ［＃本文終わり］ | 本文終わり |
| 000009_57322 | 217 | CommandFullwidth | ［＃本文終わり］ | 本文終わり |
| 000009_61393 | 273 | CommandFullwidth | ［＃本文終わり］ | 本文終わり |
| 000009_61394 | 173 | CommandFullwidth | ［＃本文終わり］ | 本文終わり |
| 000009_8 | 259 | CommandFullwidth | ［＃本文終わり］ | 本文終わり |
| 000011_55301 | 80 | CommandFullwidth | ［＃「そうなつたかは」はママ］ | 「そうなつたかは」はママ |
| 000011_889 | 11 | CommandFullwidth | ［＃ここから２字下げ、横書き］ | ここから２字下げ、横書き |
| 000011_889 | 14 | CommandFullwidth | ［＃ここから２字下げ、横書き］ | ここから２字下げ、横書き |
| 000011_889 | 175 | CommandFullwidth | ［＃ここから２字下げ、横書き］ | ここから２字下げ、横書き |
| 000012_10 | 32 | CommandFullwidth | ［＃一字下げ忘れか？200-14］ | 一字下げ忘れか？200-14 |

## Unknown Source Marker Classes

Showing 50 report rows of 8545 total classes. JSON carries 1000 top classes. truncated: true

| kind | raw | occurrences | unallowlisted | allowlisted | samples |
|---|---|---:|---:|---:|---|
| MalformedRuby | ｜ | 581 | 581 | 0 | 000008_58922, 000009_43028, 000034_519, 000040_47288, 000067_395 |
| CommandFullwidth | ［＃…］ | 465 | 465 | 0 | 000038_1408, 000042_1694, 000050_3581, 000051_1436, 000051_1452 |
| CommandFullwidth | ［＃本文終わり］ | 243 | 243 | 0 | 000009_226, 000009_50711, 000009_50712, 000009_50713, 000009_50714 |
| CommandFullwidth | ［＃（…）］ | 242 | 242 | 0 | 000026_50238, 000026_50242, 000026_50259, 000026_51893, 000026_55774 |
| MalformedAccentNotation | 〔 | 56 | 56 | 0 | 000026_219, 000034_55507, 000081_4416, 000081_50764, 000091_522 |
| CommandFullwidth | ［＃ここから２字下げ、小さい活字］ | 48 | 48 | 0 | 000051_4620 |
| CommandFullwidth | ［＃ここで字下げ終わり、小さい活字も終わり］ | 48 | 48 | 0 | 000051_4620 |
| CommandFullwidth | ［＃ここで地付き終わり］ | 42 | 42 | 0 | 000067_859, 000072_54444, 000082_43042, 000082_43050, 000106_59473 |
| CommandFullwidth | ［＃「ん」は小書き］ | 40 | 40 | 0 | 000081_1940, 000081_4415, 000081_4416, 000081_4424, 000081_4441 |
| CommandFullwidth | ［＃ここで字上げ終わり］ | 33 | 33 | 0 | 000035_1586, 000081_1935, 000082_49526, 000083_1090, 000096_2093 |
| CommandFullwidth | ［＃この行はゴシック体］ | 29 | 29 | 0 | 000281_1710, 000311_4211, 000311_4232 |
| CommandFullwidth | ［＃ここから天付き、折り返して１字下げ］ | 26 | 26 | 0 | 001154_44776, 001471_55564, 001471_55567, 001471_55575 |
| CommandFullwidth | ［＃「天皇制」に×傍点］ | 25 | 25 | 0 | 000311_3149 |
| CommandFullwidth | ［＃「（訳注）」は行左小書き］ | 23 | 23 | 0 | 002265_62680, 002265_62681, 002265_62687, 002265_62688 |
| CommandFullwidth | ［＃ここから地から２字上げ］ | 21 | 21 | 0 | 000035_1586, 000096_2093, 000124_1315, 000158_1504, 000448_46417 |
| CommandFullwidth | ［＃白三角傍点終わり］ | 21 | 21 | 0 | 000034_56908 |
| CommandFullwidth | ［＃白三角傍点］ | 21 | 21 | 0 | 000034_56908 |
| CommandFullwidth | ［＃ここから２字下げ、ゴシック体］ | 20 | 20 | 0 | 000035_307 |
| CommandFullwidth | ［＃ゴシック体］ | 20 | 20 | 0 | 000096_1115, 000096_2100, 000311_2018, 000311_2023, 000311_2024 |
| CommandFullwidth | ［＃二重傍線終わり］ | 20 | 20 | 0 | 000279_1704, 001257_60357, 001509_51405 |
| CommandFullwidth | ［＃「〃」は横組み］ | 18 | 18 | 0 | 000281_3595, 000281_3598 |
| CommandFullwidth | ［＃中見出終わり］ | 18 | 18 | 0 | 000296_58605, 000989_353, 001799_59015 |
| CommandFullwidth | ［＃大文字］ | 18 | 18 | 0 | 000096_2100 |
| CommandFullwidth | ［＃「。」は底本では欠落］ | 17 | 17 | 0 | 000009_43523, 000019_42382, 000294_1858, 001048_45381, 001090_42307 |
| CommandFullwidth | ［＃「？！」は横一列］ | 17 | 17 | 0 | 000125_665 |
| CommandFullwidth | ［＃ここから２段組み］ | 17 | 17 | 0 | 000061_377, 000311_46235, 000885_2557, 000908_51734, 001021_50117 |
| CommandFullwidth | ［＃「ツァー」に×傍点］ | 16 | 16 | 0 | 000311_3149 |
| CommandFullwidth | ［＃「革命」にばつ傍点］ | 16 | 16 | 0 | 001311_53951, 001422_50296, 001471_55570, 001618_54002, 001627_54064 |
| CommandFullwidth | ［＃「独裁」に×傍点］ | 15 | 15 | 0 | 000311_3149 |
| CommandFullwidth | ［＃sは下ドット付き］ | 14 | 14 | 0 | 001096_42686, 001096_43672 |
| CommandFullwidth | ［＃「!?」は横一列］ | 14 | 14 | 0 | 000111_566, 000111_567, 000111_568 |
| CommandFullwidth | ［＃「。」は底本では脱落］ | 14 | 14 | 0 | 000320_2562, 000885_2549, 000885_2550, 000885_2551, 000885_2554 |
| CommandFullwidth | ［＃「〃　〃」は横組み］ | 14 | 14 | 0 | 000281_3598 |
| CommandFullwidth | ［＃「な」は小書き］ | 14 | 14 | 0 | 000081_4424 |
| CommandFullwidth | ［＃「十三人」に白三角傍点］ | 14 | 14 | 0 | 000183_52746 |
| CommandFullwidth | ［＃「詫び」は底本では「詑び」と誤植］ | 14 | 14 | 0 | 000111_1479, 000111_565, 000111_566 |
| CommandFullwidth | ［＃ここで段組み終わり］ | 14 | 14 | 0 | 000061_377, 000096_2100, 000311_46235, 000885_2557, 000908_51734 |
| CommandFullwidth | ［＃「「」は底本では欠落］ | 13 | 13 | 0 | 000019_42380, 000019_42385, 000022_4873, 001090_42307, 001123_42940 |
| CommandFullwidth | ［＃「ガ」は小書き］ | 13 | 13 | 0 | 000311_2013, 000311_3141, 000311_3155, 000311_3845, 000311_3850 |
| CommandFullwidth | ［＃「天皇」に×傍点］ | 13 | 13 | 0 | 000311_2850, 000311_3149 |
| CommandFullwidth | ［＃中文字］ | 13 | 13 | 0 | 000096_2100 |
| CommandFullwidth | ［＃地より１字上げ］ | 13 | 13 | 0 | 000035_1578, 000216_2518, 000284_2609, 000286_1758, 000885_2165 |
| CommandFullwidth | ［＃大文字、太字］ | 13 | 13 | 0 | 000096_2100 |
| CommandFullwidth | ［＃mは上ドット付き］ | 12 | 12 | 0 | 001096_43672 |
| CommandFullwidth | ［＃nは上ドット付き］ | 12 | 12 | 0 | 001096_42686, 001096_43554 |
| CommandFullwidth | ［＃「　　」は罫囲み］ | 12 | 12 | 0 | 000125_1317, 000146_50413, 000908_51960, 001344_54437, 001344_54856 |
| CommandFullwidth | ［＃「、」は底本では欠落］ | 11 | 11 | 0 | 000009_42929, 000294_1858, 001090_42307, 001123_43496 |
| CommandFullwidth | ［＃「ン」は小書き］ | 11 | 11 | 0 | 000081_1940, 000082_1306 |
| CommandFullwidth | ［＃「革命」に×傍点］ | 11 | 11 | 0 | 000311_3149, 000311_3150, 000311_3893 |
| CommandFullwidth | ［＃ここからページの左右中央］ | 11 | 11 | 0 | 000885_2557 |

## Decode Failures

None.

## Inputs

- matrix: `/home/bor/Projects/ab-validator/data/aozora-syntax-coverage.toml`
- index: `scratch/ab-index.json`
- corpus: `/home/bor/Dependencies/aozorabunko`
- allowlist: `/home/bor/Projects/ab-validator/data/aozora-source-inventory-allowlist.toml`
