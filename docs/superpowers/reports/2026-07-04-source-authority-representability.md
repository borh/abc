# Source Authority Representability Inventory

## Verdict

- source_authority_gate: `SOURCE_AUTHORITY_GATE_FAILING_REVIEW_REQUIRED`
- note: this is not a passing representability gate; durable representability claims remain blocked until strict_errors is empty.
- strict_errors:
  - source inventory row annotation.bouki has occurrences but no representability table
  - source inventory row annotation.chuuki has occurrences but no representability table
  - source inventory row caption.block has occurrences but no representability table
  - source inventory row caption.inline has occurrences but no representability table
  - source inventory row decoration.bold_italic has occurrences but no representability table
  - source inventory row decoration.boten has occurrences but no representability table
  - source inventory row decoration.bousen has occurrences but no representability table
  - source inventory row decoration.direction_override has occurrences but no representability table
  - source inventory row decoration.font_size has occurrences but no representability table
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
  - source inventory row indentation.jizume has occurrences but no representability table
  - source inventory row iteration.kunoji has occurrences but no representability table
  - source inventory row kunten.kaeriten has occurrences but no representability table
  - source inventory row kunten.okurigana has occurrences but no representability table
  - source inventory row layout.tcy has occurrences but no representability table
  - source inventory row layout.yokogumi has occurrences but no representability table
  - source inventory row reference.frontref has occurrences but no representability table
  - source inventory row ruby.placement_directional has occurrences but no representability table
  - source inventory row warigaki.parenthetical has occurrences but no representability table
  - 52368 unallowlisted source markers

## Scope

This is a source-markup authority gate: every reached explicit Aozora Bunko marker must have a reviewed representation and, for represented rows, a TEI P5 projection target. Semantic TEI enrichment such as named-entity, speech, role, or place annotation is outside this gate and remains a downstream editorial layer.

## Summary

- works_scanned: 17894
- works_failed: 0
- markers_total: 4335091
- unknown_markers_total: 52368
- unallowlisted_unknown_markers_total: 52368
- allowlisted_unknown_markers_total: 0

## Representability

- typed_occurrences: 3783133
- raw_preserved_occurrences: 0
- out_of_body_occurrences: 0
- unsupported_occurrences: 0
- needs_research_occurrences: 291889

## Rows

| row | works | occurrences | samples |
|---|---:|---:|---|
| annotation.bouki | 12 | 127 | 000031_2846, 000037_2848, 000156_2699, 000219_2932, 000287_3061 |
| annotation.chuuki | 2265 | 17608 | 000006_4627, 000006_47064, 000019_4376, 000020_745, 000026_219 |
| break.line_explicit | 47 | 161 | 000075_4250, 000081_47027, 000083_46289, 000106_56858, 000106_57905 |
| break.page_line | 943 | 10028 | 000005_53194, 000006_1869, 000009_55881, 000011_889, 000011_899 |
| caption.block | 47 | 1722 | 000058_57440, 000091_50354, 000125_1321, 000165_49567, 000226_1150 |
| caption.inline | 136 | 1040 | 000014_728, 000067_1768, 000067_1788, 000067_1789, 000093_1916 |
| decoration.bold_italic | 141 | 4889 | 000025_1144, 000026_50241, 000026_55916, 000035_52380, 000067_1789 |
| decoration.boten | 79 | 1376 | 000034_56908, 000042_61014, 000042_61015, 000042_61019, 000048_45476 |
| decoration.bousen | 317 | 18067 | 000006_1869, 000013_11, 000019_4376, 000034_55507, 000038_1408 |
| decoration.direction_override | 6 | 65 | 000096_935, 000866_3039, 001094_42603, 001242_46444, 001467_50733 |
| decoration.font_size | 997 | 43905 | 000011_899, 000019_58861, 000019_59261, 000019_59375, 000023_55325 |
| decoration.keigakomi | 106 | 200 | 000067_1789, 000072_408, 000096_2093, 000096_2100, 000096_2117 |
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
| indentation.burasage | 1200 | 13048 | 000006_3311, 000006_58819, 000008_47386, 000009_55881, 000019_4376 |
| indentation.chitsuki | 6171 | 20073 | 000006_1868, 000006_1869, 000006_3310, 000006_3311, 000006_382 |
| indentation.jisage_block | 4599 | 88936 | 000005_5, 000005_53194, 000006_1868, 000006_1869, 000006_3310 |
| indentation.jisage_oneline | 1 | 2 | 000284_2227 |
| indentation.jizume | 231 | 2747 | 000026_55781, 000034_55507, 000040_47289, 000050_48400, 000055_56499 |
| iteration.kunoji | 1125 | 10700 | 000006_58810, 000006_58819, 000008_47357, 000012_1092, 000012_24448 |
| kunten.kaeriten | 488 | 28082 | 000006_1869, 000038_1408, 000042_1694, 000050_3581, 000051_1436 |
| kunten.okurigana | 256 | 6562 | 000026_50238, 000026_50242, 000026_50259, 000026_51893, 000026_55774 |
| layout.tcy | 704 | 19416 | 000014_728, 000020_2223, 000020_46404, 000023_55306, 000023_55324 |
| layout.yokogumi | 403 | 3549 | 000019_59374, 000026_55717, 000026_55732, 000026_55738, 000034_519 |
| reference.frontref | 682 | 3920 | 000006_46659, 000012_2585, 000012_4316, 000022_197, 000026_46578 |
| ruby.basic | 14321 | 3608407 | 000005_5, 000005_53194, 000005_55215, 000005_55216, 000005_55217 |
| ruby.placement_directional | 31 | 312 | 000034_1213, 000050_3581, 000129_694, 000146_49258, 000146_50202 |
| warichu.basic | 361 | 6601 | 000005_53194, 000006_1868, 000006_1869, 000034_519, 000038_42202 |
| warigaki.parenthetical | 1 | 1 | 000034_519 |

## Unknown Source Markers

| work_id | line | kind | raw | body |
|---|---:|---|---|---|
| 000005_5 | 11 | MalformedRuby | ｜ | ｜ |
| 000005_5 | 14 | CommandFullwidth | ［＃］ |  |
| 000005_53194 | 11 | MalformedRuby | ｜ | ｜ |
| 000005_53194 | 14 | CommandFullwidth | ［＃］ |  |
| 000005_53194 | 652 | CommandFullwidth | ［＃「……』」は底本では「……」」］ | 「……』」は底本では「……」」 |
| 000006_1868 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_1868 | 13 | CommandFullwidth | ［＃］ |  |
| 000006_1869 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_1869 | 13 | CommandFullwidth | ［＃］ |  |
| 000006_1869 | 917 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_1869 | 993 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_1869 | 993 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_1869 | 1558 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_1869 | 1558 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_1869 | 1558 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_1869 | 1559 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_1869 | 1559 | CommandFullwidth | ［＃「引」は小書き右寄せ］ | 「引」は小書き右寄せ |
| 000006_3310 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_3310 | 13 | CommandFullwidth | ［＃］ |  |
| 000006_3310 | 316 | CommandFullwidth | ［＃「涕」はママ］ | 「涕」はママ |
| 000006_3311 | 10 | CommandFullwidth | ［＃］ |  |
| 000006_382 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_382 | 13 | CommandFullwidth | ［＃］ |  |
| 000006_383 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_383 | 13 | CommandFullwidth | ［＃］ |  |
| 000006_384 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_384 | 13 | CommandFullwidth | ［＃］ |  |
| 000006_4627 | 11 | CommandFullwidth | ［＃］ |  |
| 000006_4627 | 37 | CommandFullwidth | ［＃「齷齪」は底本では「齷齦」］ | 「齷齪」は底本では「齷齦」 |
| 000006_46659 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_46659 | 13 | CommandFullwidth | ［＃］ |  |
| 000006_58819 | 10 | MalformedRuby | ｜ | ｜ |
| 000006_58819 | 13 | CommandFullwidth | ［＃］ |  |
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
| 000008_1083 | 93 | CommandFullwidth | ［＃「四俵も」は底本では「四依も」］ | 「四俵も」は底本では「四依も」 |
| 000008_1083 | 93 | CommandFullwidth | ［＃「ままくらってる」は底本では「まくらってる」］ | 「ままくらってる」は底本では「まくらってる」 |
| 000008_1083 | 113 | CommandFullwidth | ［＃「舟の上に仁王立ちになった船頭は」は底本では「舟の上に仁王立ち船頭は」］ | 「舟の上に仁王立ちになった船頭は」は底本では「舟の上に仁王立ち船頭は」 |
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
| 000008_18327 | 10 | MalformedRuby | ｜ | ｜ |
| 000008_18327 | 13 | CommandFullwidth | ［＃］ |  |
| 000008_2688 | 10 | MalformedRuby | ｜ | ｜ |
| 000008_2688 | 13 | CommandFullwidth | ［＃］ |  |
| 000008_407 | 10 | MalformedRuby | ｜ | ｜ |
| 000008_407 | 13 | CommandFullwidth | ［＃］ |  |
| 000008_47357 | 10 | MalformedRuby | ｜ | ｜ |
| 000008_47357 | 13 | CommandFullwidth | ［＃］ |  |
| 000008_47357 | 68 | CommandFullwidth | ［＃「そのうち」は底本では「そのち」］ | 「そのうち」は底本では「そのち」 |
| 000008_47357 | 237 | CommandFullwidth | ［＃「知らなかつた。」は底本では「知らなかた。」］ | 「知らなかつた。」は底本では「知らなかた。」 |
| 000008_47357 | 367 | CommandFullwidth | ［＃「』」は、底本では「」」］ | 「』」は、底本では「」」 |
| 000008_47357 | 467 | CommandFullwidth | ［＃「コップ」はママ］ | 「コップ」はママ |
| 000008_47357 | 522 | CommandFullwidth | ［＃「あるのです」は底本では「あるのでず」］ | 「あるのです」は底本では「あるのでず」 |
| 000008_47357 | 567 | CommandFullwidth | ［＃「耕作する」は底本では「耕かるこ作」］ | 「耕作する」は底本では「耕かるこ作」 |
| 000008_47357 | 567 | CommandFullwidth | ［＃「かかることが」は底本では「かことが」］ | 「かかることが」は底本では「かことが」 |
| 000008_47357 | 1121 | CommandFullwidth | ［＃「待たれた」は底本では「待た。れた」］ | 「待たれた」は底本では「待た。れた」 |
| 000008_47357 | 1138 | CommandFullwidth | ［＃「」とあげて」は底本では「」　とあげて」］ | 「」とあげて」は底本では「」　とあげて」 |
| 000008_47357 | 1221 | CommandFullwidth | ［＃「やがて、」は底本では「やがて　」］ | 「やがて、」は底本では「やがて　」 |
| 000008_47357 | 1268 | CommandFullwidth | ［＃「あらうか？　さうして人々は」は底本では「あらうか？さうして人々は」］ | 「あらうか？　さうして人々は」は底本では「あらうか？さうして人々は」 |
| 000008_47357 | 1376 | CommandFullwidth | ［＃「そして、」は底本では「そして。」］ | 「そして、」は底本では「そして。」 |
| 000008_47361 | 10 | MalformedRuby | ｜ | ｜ |
| 000008_47361 | 13 | CommandFullwidth | ［＃］ |  |
| 000008_47361 | 101 | CommandFullwidth | ［＃「無理矢理に」は底本では「無理失理に」］ | 「無理矢理に」は底本では「無理失理に」 |
| 000008_47361 | 158 | CommandFullwidth | ［＃「ときから」は底本では「とから」］ | 「ときから」は底本では「とから」 |
| 000008_47374 | 10 | CommandFullwidth | ［＃］ |  |
| 000008_47374 | 73 | CommandFullwidth | ［＃「奎吾に」は底本では「奎吉に」］ | 「奎吾に」は底本では「奎吉に」 |
| 000008_47382 | 10 | CommandFullwidth | ［＃］ |  |

## Unknown Source Marker Classes

Showing 50 report rows of 15925 total classes. JSON carries 1000 top classes. truncated: true

| kind | raw | occurrences | unallowlisted | allowlisted | samples |
|---|---|---:|---:|---:|---|
| CommandFullwidth | ［＃］ | 13920 | 13920 | 0 | 000005_5, 000005_53194, 000006_1868, 000006_1869, 000006_3310 |
| MalformedRuby | ｜ | 9658 | 9658 | 0 | 000005_5, 000005_53194, 000006_1868, 000006_1869, 000006_3310 |
| AccentNotation | 〔〕 | 770 | 770 | 0 | 000020_745, 000026_50241, 000026_50245, 000026_50255, 000026_51891 |
| CommandFullwidth | ［＃「b」は下付き小文字］ | 708 | 708 | 0 | 001185_45210 |
| CommandFullwidth | ［＃「a」は下付き小文字］ | 666 | 666 | 0 | 001185_45210 |
| CommandFullwidth | ［＃…］ | 465 | 465 | 0 | 000038_1408, 000042_1694, 000050_3581, 000051_1436, 000051_1452 |
| CommandFullwidth | ［＃「大字」は１段階小さな文字］ | 462 | 462 | 0 | 001566_58320, 001566_58447, 001566_58544, 001566_58777 |
| CommandFullwidth | ［＃「字」は１段階小さな文字］ | 388 | 388 | 0 | 001566_58447, 001566_58544 |
| CommandFullwidth | ［＃「d」は下付き小文字］ | 336 | 336 | 0 | 001185_45210 |
| CommandFullwidth | ［＃「p」は下付き小文字］ | 286 | 286 | 0 | 001185_45210 |
| CommandFullwidth | ［＃「c」は下付き小文字］ | 273 | 273 | 0 | 001185_45210 |
| CommandFullwidth | ［＃本文終わり］ | 243 | 243 | 0 | 000009_226, 000009_50711, 000009_50712, 000009_50713, 000009_50714 |
| CommandFullwidth | ［＃「t」は下付き小文字］ | 242 | 242 | 0 | 001185_45210 |
| CommandFullwidth | ［＃（…）］ | 242 | 242 | 0 | 000026_50238, 000026_50242, 000026_50259, 000026_51893, 000026_55774 |
| CommandFullwidth | ［＃「k」は下付き小文字］ | 199 | 199 | 0 | 001185_45210 |
| CommandFullwidth | ［＃ここで罫囲み終わり］ | 194 | 194 | 0 | 000067_1789, 000072_408, 000096_2093, 000096_2100, 000096_2117 |
| CommandFullwidth | ［＃改行天付き、折り返して１字下げ］ | 187 | 187 | 0 | 000272_1805, 001154_51829, 001726_56089 |
| CommandFullwidth | ［＃ここで太字終わり］ | 184 | 184 | 0 | 000026_55916, 000035_52380, 000072_54444, 000096_2117, 000113_4325 |
| CommandFullwidth | ［＃「1」は下付き小文字］ | 181 | 181 | 0 | 000042_2345, 000148_789, 000281_3598, 000281_43266, 001185_45210 |
| CommandFullwidth | ［＃ここから３字下げ、１行２０字組みで］ | 178 | 178 | 0 | 000321_2169 |
| CommandFullwidth | ［＃ここで字下げ、２０字組み終わり］ | 178 | 178 | 0 | 000321_2169 |
| CommandFullwidth | ［＃縦中横終わり］ | 165 | 165 | 0 | 000061_377, 000081_47029, 000081_60685, 000094_2525, 000226_1150 |
| CommandFullwidth | ［＃縦中横］ | 165 | 165 | 0 | 000061_377, 000081_47029, 000081_60685, 000094_2525, 000226_1150 |
| CommandFullwidth | ［＃大きな文字終わり］ | 158 | 158 | 0 | 000019_59261, 000034_55507, 000050_4561, 000096_2093, 000096_2100 |
| CommandFullwidth | ［＃「a,1」は下付き小文字］ | 148 | 148 | 0 | 001185_45210 |
| CommandFullwidth | ［＃「b,1」は下付き小文字］ | 117 | 117 | 0 | 001185_45210 |
| CommandFullwidth | ［＃天から２字下げ］ | 116 | 116 | 0 | 000051_3330, 000067_2843, 000074_3565, 000076_448, 000082_43090 |
| CommandFullwidth | ［＃天から３字下げ］ | 89 | 89 | 0 | 000025_202, 000025_216, 000074_3565, 000081_1935, 000082_49529 |
| CommandFullwidth | ［＃「2」は下付き小文字］ | 83 | 83 | 0 | 000081_47029, 000106_49617, 000106_53493, 000148_789, 000226_1150 |
| CommandFullwidth | ［＃ここから２字下げ、２２字詰め］ | 73 | 73 | 0 | 000885_2560 |
| CommandFullwidth | ［＃「d,1」は下付き小文字］ | 64 | 64 | 0 | 001185_45210 |
| MalformedAccentNotation | 〔 | 56 | 56 | 0 | 000026_219, 000034_55507, 000081_4416, 000081_50764, 000091_522 |
| CommandFullwidth | ［＃「2」は上付き小文字］ | 55 | 55 | 0 | 000094_42338, 000106_2415, 000129_45224, 000226_1150, 000281_3598 |
| CommandFullwidth | ［＃「3」は下付き小文字］ | 54 | 54 | 0 | 000106_49617, 000148_789, 000281_3598, 001185_45210, 001569_57451 |
| CommandFullwidth | ［＃「a,b」は下付き小文字］ | 53 | 53 | 0 | 001185_45210 |
| CommandFullwidth | ［＃「、」は底本では「。」］ | 48 | 48 | 0 | 000019_42380, 000019_42382, 000019_42383, 000019_42384, 000019_42385 |
| CommandFullwidth | ［＃ここから２字下げ、小さい活字］ | 48 | 48 | 0 | 000051_4620 |
| CommandFullwidth | ［＃ここで字下げ終わり、小さい活字も終わり］ | 48 | 48 | 0 | 000051_4620 |
| CommandFullwidth | ［＃「r,1」は下付き小文字］ | 47 | 47 | 0 | 001185_45210 |
| CommandFullwidth | ［＃「c,b」は下付き小文字］ | 46 | 46 | 0 | 001185_45210 |
| CommandFullwidth | ［＃「c,1」は下付き小文字］ | 44 | 44 | 0 | 001185_45210 |
| CommandFullwidth | ［＃ここで地付き終わり］ | 42 | 42 | 0 | 000067_859, 000072_54444, 000082_43042, 000082_43050, 000106_59473 |
| CommandFullwidth | ［＃「子」は１段階小さな文字］ | 41 | 41 | 0 | 000165_3622 |
| CommandFullwidth | ［＃「b,a」は下付き小文字］ | 40 | 40 | 0 | 001185_45210 |
| CommandFullwidth | ［＃「ん」は小書き］ | 40 | 40 | 0 | 000081_1940, 000081_4415, 000081_4416, 000081_4424, 000081_4441 |
| CommandFullwidth | ［＃天から４字下げ］ | 37 | 37 | 0 | 000050_45755, 000051_47086, 000081_46600, 000081_48221, 000083_1090 |
| CommandFullwidth | ［＃「a,2」は下付き小文字］ | 34 | 34 | 0 | 001185_45210 |
| CommandFullwidth | ［＃ルビは「悪魔の尿溜」にかかる］ | 34 | 34 | 0 | 000125_1320 |
| CommandFullwidth | ［＃ここで字上げ終わり］ | 33 | 33 | 0 | 000035_1586, 000081_1935, 000082_49526, 000083_1090, 000096_2093 |
| CommandFullwidth | ［＃「a,3」は下付き小文字］ | 32 | 32 | 0 | 001185_45210 |

## Decode Failures

None.

## Inputs

- matrix: `/home/bor/Projects/ab-validator/data/aozora-syntax-coverage.toml`
- index: `scratch/ab-index.json`
- corpus: `/home/bor/Dependencies/aozorabunko`
- allowlist: `/home/bor/Projects/ab-validator/data/aozora-source-inventory-allowlist.toml`
