# Source Authority Representability Inventory

## Verdict

- source_authority_gate: `SOURCE_AUTHORITY_GATE_FAILING_REVIEW_REQUIRED`
- note: this is not a passing representability gate; durable representability claims remain blocked until strict_errors is empty.
- strict_errors:
  - 178 unallowlisted source markers

## Scope

This is a source-markup authority gate: every reached explicit Aozora Bunko marker must have a reviewed representation and, for represented rows, a TEI P5 projection target. Semantic TEI enrichment such as named-entity, speech, role, or place annotation is outside this gate and remains a downstream editorial layer.

## Summary

- works_scanned: 17894
- works_failed: 0
- markers_total: 4323915
- unknown_markers_total: 15062
- unallowlisted_unknown_markers_total: 178
- allowlisted_unknown_markers_total: 14884

## Representability

- typed_occurrences: 4570068
- raw_preserved_occurrences: 46091
- out_of_body_occurrences: 950
- unsupported_occurrences: 13934
- needs_research_occurrences: 0

## Rows

| row | works | occurrences | samples |
|---|---:|---:|---|
| accent.dotted_letter | 3 | 93 | 001096_42686, 001096_43554, 001096_43672 |
| annotation.bouki | 12 | 127 | 000031_2846, 000037_2848, 000156_2699, 000219_2932, 000287_3061 |
| annotation.chuuki | 5513 | 35373 | 000005_53194, 000006_1869, 000006_3310, 000006_4627, 000006_46659 |
| annotation.layout_note | 8 | 19 | 000072_864, 000083_3329, 000259_3554, 000305_1896, 000311_2018 |
| break.line_explicit | 47 | 161 | 000075_4250, 000081_47027, 000083_46289, 000106_56858, 000106_57905 |
| break.page_line | 943 | 9229 | 000005_53194, 000006_1869, 000009_55881, 000011_889, 000011_899 |
| caption.block | 48 | 1726 | 000058_57440, 000091_50354, 000125_1321, 000165_49567, 000226_1150 |
| caption.inline | 143 | 2397 | 000014_728, 000058_57440, 000067_1768, 000067_1788, 000067_1789 |
| decoration.bold_italic | 319 | 8829 | 000020_4487, 000020_46404, 000025_1144, 000025_56503, 000026_50241 |
| decoration.boten | 6236 | 129086 | 000005_53194, 000006_382, 000006_383, 000006_58819, 000008_1083 |
| decoration.bousen | 327 | 18127 | 000006_1869, 000013_11, 000019_4376, 000034_55507, 000038_1408 |
| decoration.direction_override | 6 | 65 | 000096_935, 000866_3039, 001094_42603, 001242_46444, 001467_50733 |
| decoration.font_size | 1223 | 51508 | 000006_1869, 000008_58922, 000011_899, 000019_4376, 000019_58861 |
| decoration.keigakomi | 157 | 717 | 000043_341, 000063_385, 000067_1789, 000072_408, 000072_864 |
| decoration.typeface | 3 | 6 | 000212_4839, 000311_33191, 001917_61172 |
| emphasis.basic | 6576 | 157493 | 000005_53194, 000006_1869, 000006_382, 000006_383, 000006_58819 |
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
| reference.frontref | 682 | 3920 | 000006_46659, 000012_2585, 000012_4316, 000022_197, 000026_46578 |
| ruby.basic | 14321 | 3607926 | 000005_5, 000005_53194, 000005_55215, 000005_55216, 000005_55217 |
| ruby.placement_directional | 31 | 318 | 000034_1213, 000050_3581, 000129_694, 000146_49258, 000146_50202 |
| source.note_label | 531 | 1381 | 000008_1083, 000012_198, 000013_542, 000019_4376, 000026_50239 |
| source.page_reference | 6 | 58 | 000160_875, 000989_351, 000989_352, 000989_42687, 001518_51731 |
| source.reviewed_residual_command | 92 | 310 | 000048_358, 000051_1441, 000051_1442, 000051_1448, 000051_3538 |
| structure.quote_block | 6 | 21 | 000034_233, 000035_296, 000137_733, 000280_1706, 000989_351 |
| structure.table | 7 | 46 | 000042_2345, 000042_2348, 000042_2449, 000096_2100, 000311_2745 |
| warichu.basic | 362 | 6605 | 000005_53194, 000006_1868, 000006_1869, 000034_519, 000038_42202 |
| warigaki.parenthetical | 1 | 2 | 000034_519 |

## Unknown Source Markers

| work_id | line | kind | raw | body |
|---|---:|---|---|---|
| 000008_1083 | 209 | CommandFullwidth | ［＃「ヤンに傍点］ | 「ヤンに傍点 |
| 000083_1362 | 61 | CommandFullwidth | ［＃ここで字下げ終わり」］ | ここで字下げ終わり」 |
| 000096_2117 | 35 | CommandFullwidth | ［＃波罫線］ | 波罫線 |
| 000124_651 | 664 | CommandFullwidth | ［＃「七夕」「真つすぐな街」は自由律俳句］ | 「七夕」「真つすぐな街」は自由律俳句 |
| 000148_1104 | 100 | CommandFullwidth | ［＃「郷－即のへん」、232-1］ | 「郷－即のへん」、232-1 |
| 000160_1255 | 45 | CommandFullwidth | ［＃萩原喜一郎、隣家］ | 萩原喜一郎、隣家 |
| 000160_1255 | 62 | CommandFullwidth | ［＃灯火管制。夜間、敵機の来襲に備えて、灯りを遮ったり落としたりすこと］ | 灯火管制。夜間、敵機の来襲に備えて、灯りを遮ったり落としたりすこと |
| 000160_1255 | 104 | CommandFullwidth | ［＃第八十六通常議会］ | 第八十六通常議会 |
| 000160_1255 | 260 | CommandFullwidth | ［＃娘婿の永田徹郎海軍大尉］ | 娘婿の永田徹郎海軍大尉 |
| 000160_1255 | 383 | CommandFullwidth | ［＃横須賀鎮守府。鎮守府は、海軍の根拠地に置かれた機関］ | 横須賀鎮守府。鎮守府は、海軍の根拠地に置かれた機関 |
| 000160_1255 | 454 | CommandFullwidth | ［＃晴彦］ | 晴彦 |
| 000160_1255 | 513 | CommandFullwidth | ［＃天皇の諮問機関、枢密院の異称］ | 天皇の諮問機関、枢密院の異称 |
| 000160_1255 | 519 | CommandFullwidth | ［＃戦時下食糧統制の一環として配給された、外食券を利用する食堂。現金があっても、券がなければ食べられなかった］ | 戦時下食糧統制の一環として配給された、外食券を利用する食堂。現金があっても、券がなければ食べられなかった |
| 000160_1255 | 755 | CommandFullwidth | ［＃用もないのに廊下をうろつき回ること］ | 用もないのに廊下をうろつき回ること |
| 000160_1255 | 868 | CommandFullwidth | ［＃朝永］ | 朝永 |
| 000160_1255 | 892 | CommandFullwidth | ［＃朝永良太］ | 朝永良太 |
| 000160_1255 | 1085 | CommandFullwidth | ［＃引用、終わり］ | 引用、終わり |
| 000160_1255 | 1104 | CommandFullwidth | ［＃戦争終結の詔勅を放送］ | 戦争終結の詔勅を放送 |
| 000160_1255 | 1132 | CommandFullwidth | ［＃東久邇宮稔彦首相］ | 東久邇宮稔彦首相 |
| 000160_1255 | 1150 | CommandFullwidth | ［＃宣伝ビラ］ | 宣伝ビラ |
| 000160_1255 | 1151 | CommandFullwidth | ［＃口語自由詩で、民衆の現実を描こうとした、「民衆派」の詩人］ | 口語自由詩で、民衆の現実を描こうとした、「民衆派」の詩人 |
| 000160_1255 | 1158 | CommandFullwidth | ［＃降伏文書の調印式場として使われた］ | 降伏文書の調印式場として使われた |
| 000160_1255 | 1164 | CommandFullwidth | ［＃正男］ | 正男 |
| 000160_1255 | 1164 | CommandFullwidth | ［＃移動演劇隊桜隊。広島滞在中、原爆に遭う］ | 移動演劇隊桜隊。広島滞在中、原爆に遭う |
| 000160_1255 | 1173 | CommandFullwidth | ［＃葵］ | 葵 |
| 000160_1255 | 1173 | CommandFullwidth | ［＃美治郎］ | 美治郎 |
| 000160_1255 | 1203 | CommandFullwidth | ［＃喜重郎］ | 喜重郎 |
| 000160_1255 | 1379 | CommandFullwidth | ［＃公職追放］ | 公職追放 |
| 000160_1255 | 1379 | CommandFullwidth | ［＃超国家主義団体］ | 超国家主義団体 |
| 000160_1255 | 1396 | CommandFullwidth | ［＃準］ | 準 |
| 000160_1255 | 1397 | CommandFullwidth | ［＃宇陀児］ | 宇陀児 |
| 000160_1255 | 1441 | CommandFullwidth | ［＃金融緊急措置令。新円発行、旧円預金は封鎖］ | 金融緊急措置令。新円発行、旧円預金は封鎖 |
| 000160_1255 | 1479 | CommandFullwidth | ［＃海野の別ペンネーム］ | 海野の別ペンネーム |
| 000160_1255 | 1567 | CommandFullwidth | ［＃謙］ | 謙 |
| 000160_1255 | 1628 | CommandFullwidth | ［＃高太郎］ | 高太郎 |
| 000160_1255 | 1678 | CommandFullwidth | ［＃新興宗教、璽宇教教祖璽光尊、幹部の元横綱双葉山、棋士呉清源ら、食糧管理法違犯により二十一日に逮捕］ | 新興宗教、璽宇教教祖璽光尊、幹部の元横綱双葉山、棋士呉清源ら、食糧管理法違犯により二十一日に逮捕 |
| 000160_875 | 1396 | CommandFullwidth | ［＃以降の「――」で始まる通信文の2行目以降は2字下げ］ | 以降の「――」で始まる通信文の2行目以降は2字下げ |
| 000182_946 | 24 | CommandFullwidth | ［＃１９字下げて］ | １９字下げて |
| 000243_1328 | 112 | CommandFullwidth | ［＃括弧内は「染付」と「赤繪」の二行になっている］ | 括弧内は「染付」と「赤繪」の二行になっている |
| 000250_18353 | 232 | CommandFullwidth | ［＃「諸国における富の分配」の図表のこと］ | 「諸国における富の分配」の図表のこと |
| 000250_4644 | 281 | CommandFullwidth | ［＃原文は括弧「〔〕」を使うが、他の所と一致させるため改める］ | 原文は括弧「〔〕」を使うが、他の所と一致させるため改める |
| 000256_2590 | 384 | CommandFullwidth | ［＃（）内の文字全てに傍点、ただし読点をのぞく］ | （）内の文字全てに傍点、ただし読点をのぞく |
| 000301_1872 | 836 | CommandFullwidth | ［＃ここに「下ニ詳ナリ」という注意書きが入る］ | ここに「下ニ詳ナリ」という注意書きが入る |
| 000305_3608 | 26 | CommandFullwidth | ［＃罫線の部分は、「｛」「｝」で括る］ | 罫線の部分は、「｛」「｝」で括る |
| 000305_43618 | 15 | CommandFullwidth | ［＃闇汁の図］ | 闇汁の図 |
| 000311_33188 | 815 | CommandFullwidth | ［＃便箋右上に花飾り付きのページ数］ | 便箋右上に花飾り付きのページ数 |
| 000311_3434 | 14 | CommandFullwidth | ［＃ここで字下げ、横書き終わり］ | ここで字下げ、横書き終わり |
| 000311_4212 | 106 | CommandFullwidth | ［＃３つの「｛」は１つに繋がる］ | ３つの「｛」は１つに繋がる |
| 000311_46236 | 303 | CommandFullwidth | ［＃寿江］ | 寿江 |
| 000311_46236 | 1384 | CommandFullwidth | ［＃本田道之］ | 本田道之 |
| 000311_46236 | 1474 | CommandFullwidth | ［＃黒田鵬心］ | 黒田鵬心 |
| 000311_46236 | 1734 | CommandFullwidth | ［＃小杉放庵］ | 小杉放庵 |
| 000311_46239 | 34 | CommandFullwidth | ［＃武者小路実篤のペンネーム］ | 武者小路実篤のペンネーム |
| 000311_46239 | 141 | CommandFullwidth | ［＃大瀧菊子］ | 大瀧菊子 |
| 000311_46239 | 264 | CommandFullwidth | ［＃百合子の実家］ | 百合子の実家 |
| 000311_46241 | 1029 | CommandFullwidth | ［＃福地源一郎］ | 福地源一郎 |
| 000311_46241 | 1226 | CommandFullwidth | ［＃加藤シヅエ］ | 加藤シヅエ |
| 000311_46242 | 192 | CommandFullwidth | ［＃神近市子］ | 神近市子 |
| 000311_46242 | 192 | CommandFullwidth | ［＃大杉栄］ | 大杉栄 |
| 000311_46242 | 325 | CommandFullwidth | ［＃堺利彦］ | 堺利彦 |
| 000311_46242 | 737 | CommandFullwidth | ［＃野上豊一郎］ | 野上豊一郎 |
| 000311_46242 | 883 | CommandFullwidth | ［＃貞の末息子］ | 貞の末息子 |
| 000311_46243 | 524 | CommandFullwidth | ［＃湯浅芳子］ | 湯浅芳子 |
| 000311_46243 | 577 | CommandFullwidth | ［＃江井、中條家の運転手］ | 江井、中條家の運転手 |
| 000311_46243 | 821 | CommandFullwidth | ［＃網野菊］ | 網野菊 |
| 000311_46244 | 73 | CommandFullwidth | ［＃呉昌碩］ | 呉昌碩 |
| 000311_46244 | 81 | CommandFullwidth | ［＃長谷川如是閑］ | 長谷川如是閑 |
| 000311_46244 | 124 | CommandFullwidth | ［＃宮部金吾］ | 宮部金吾 |
| 000311_46244 | 327 | CommandFullwidth | ［＃原阿佐緒］ | 原阿佐緒 |
| 000311_46244 | 358 | CommandFullwidth | ［＃青山杉作］ | 青山杉作 |
| 000311_46244 | 373 | CommandFullwidth | ［＃洋装店］ | 洋装店 |
| 000311_46245 | 1121 | CommandFullwidth | ［＃「検察官」］ | 「検察官」 |
| 000311_46245 | 1151 | CommandFullwidth | ［＃映画館名］ | 映画館名 |
| 000311_46246 | 22 | CommandFullwidth | ［＃十月革命］ | 十月革命 |
| 000311_46246 | 81 | CommandFullwidth | ［＃土曜集会］ | 土曜集会 |
| 000311_46246 | 516 | CommandFullwidth | ［＃ロマン・キム］ | ロマン・キム |
| 000311_46246 | 550 | CommandFullwidth | ［＃青鞜］ | 青鞜 |
| 000311_46246 | 573 | CommandFullwidth | ［＃少年団員］ | 少年団員 |
| 000311_46246 | 710 | CommandFullwidth | ［＃同志］ | 同志 |
| 000311_46246 | 793 | CommandFullwidth | ［＃宮本百合子の作品「赤い貨車」のナースチャのモデル］ | 宮本百合子の作品「赤い貨車」のナースチャのモデル |
| 000311_46246 | 957 | CommandFullwidth | ［＃映画］ | 映画 |
| 000311_46246 | 1042 | CommandFullwidth | ［＃共産党員］ | 共産党員 |
| 000311_46246 | 1048 | CommandFullwidth | ［＃演説、報告］ | 演説、報告 |
| 000311_46246 | 1054 | CommandFullwidth | ［＃赤いけし］ | 赤いけし |
| 000311_46246 | 1091 | CommandFullwidth | ［＃河原崎長十郎］ | 河原崎長十郎 |
| 000311_46246 | 1167 | CommandFullwidth | ［＃通りの名］ | 通りの名 |
| 000311_46246 | 1301 | CommandFullwidth | ［＃洗面器］ | 洗面器 |
| 000311_46246 | 1353 | CommandFullwidth | ［＃共産主義者（女）］ | 共産主義者（女） |
| 000311_46246 | 1385 | CommandFullwidth | ［＃映画館名、「穂」］ | 映画館名、「穂」 |
| 000311_46246 | 1454 | CommandFullwidth | ［＃戯曲「赤藍色の島」］ | 戯曲「赤藍色の島」 |
| 000311_46246 | 1461 | CommandFullwidth | ［＃殺虫剤名］ | 殺虫剤名 |
| 000311_46246 | 1498 | CommandFullwidth | ［＃「ココ」は手描きの切符の下部の線に結ばれている］ | 「ココ」は手描きの切符の下部の線に結ばれている |
| 000311_46247 | 138 | CommandFullwidth | ［＃覚え書き、ノート］ | 覚え書き、ノート |
| 000311_46247 | 544 | CommandFullwidth | ［＃〇・四一キログラム］ | 〇・四一キログラム |
| 000311_46247 | 566 | CommandFullwidth | ［＃反宗教］ | 反宗教 |
| 000311_46247 | 573 | CommandFullwidth | ［＃休息の家］ | 休息の家 |
| 000311_46247 | 573 | CommandFullwidth | ［＃食後の休息時間］ | 食後の休息時間 |
| 000311_46247 | 600 | CommandFullwidth | ［＃旅行案内書］ | 旅行案内書 |
| 000311_46247 | 662 | CommandFullwidth | ［＃根津嘉一郎］ | 根津嘉一郎 |
| 000311_46247 | 1221 | CommandFullwidth | ［＃木村毅］ | 木村毅 |

## Unknown Source Marker Classes

Showing 50 report rows of 185 total classes. JSON carries 185 top classes. truncated: false

| kind | raw | occurrences | unallowlisted | allowlisted | samples |
|---|---|---:|---:|---:|---|
| CommandFullwidth | ［＃（ルヽ）］ | 2 | 2 | 0 | 001341_50287 |
| CommandFullwidth | ［＃〇・四一キログラム］ | 1 | 1 | 0 | 000311_46247 |
| CommandFullwidth | ［＃「ココ」は手描きの切符の下部の線に結ばれている］ | 1 | 1 | 0 | 000311_46246 |
| CommandFullwidth | ［＃「ヤンに傍点］ | 1 | 1 | 0 | 000008_1083 |
| CommandFullwidth | ［＃「七夕」「真つすぐな街」は自由律俳句］ | 1 | 1 | 0 | 000124_651 |
| CommandFullwidth | ［＃「嘘の効用」］ | 1 | 1 | 0 | 000922_47099 |
| CommandFullwidth | ［＃「検察官」］ | 1 | 1 | 0 | 000311_46245 |
| CommandFullwidth | ［＃「諸国における富の分配」の図表のこと］ | 1 | 1 | 0 | 000250_18353 |
| CommandFullwidth | ［＃「郷－即のへん」、232-1］ | 1 | 1 | 0 | 000148_1104 |
| CommandFullwidth | ［＃「［途中略］ | 1 | 1 | 0 | 001657_54333 |
| CommandFullwidth | ［＃「［Ａ］ | 1 | 1 | 0 | 001402_49946 |
| CommandFullwidth | ［＃「｝一八〇」はこの後の５行にわたる］ | 1 | 1 | 0 | 001164_43670 |
| CommandFullwidth | ［＃ここで字下げ、横書き終わり］ | 1 | 1 | 0 | 000311_3434 |
| CommandFullwidth | ［＃ここで字下げ終わり　］ | 1 | 1 | 0 | 000874_3397 |
| CommandFullwidth | ［＃ここで字下げ終わり」］ | 1 | 1 | 0 | 000083_1362 |
| CommandFullwidth | ［＃ここに「下ニ詳ナリ」という注意書きが入る］ | 1 | 1 | 0 | 000301_1872 |
| CommandFullwidth | ［＃この読点不適当］ | 1 | 1 | 0 | 000603_4729 |
| CommandFullwidth | ［＃スフ］ | 1 | 1 | 0 | 000311_46253 |
| CommandFullwidth | ［＃トーキー］ | 1 | 1 | 0 | 000311_46248 |
| CommandFullwidth | ［＃レールが鳴り響く］ | 1 | 1 | 0 | 000311_4834 |
| CommandFullwidth | ［＃ロマン・キム］ | 1 | 1 | 0 | 000311_46246 |
| CommandFullwidth | ［＃上部欄外に「じうもんじカ」］ | 1 | 1 | 0 | 000908_51396 |
| CommandFullwidth | ［＃中野鈴子］ | 1 | 1 | 0 | 000311_46251 |
| CommandFullwidth | ［＃以降の「――」で始まる通信文の2行目以降は2字下げ］ | 1 | 1 | 0 | 000160_875 |
| CommandFullwidth | ［＃休息の家］ | 1 | 1 | 0 | 000311_46247 |
| CommandFullwidth | ［＃便箋右上に花飾り付きのページ数］ | 1 | 1 | 0 | 000311_33188 |
| CommandFullwidth | ［＃党員証］ | 1 | 1 | 0 | 000311_46248 |
| CommandFullwidth | ［＃公職追放］ | 1 | 1 | 0 | 000160_1255 |
| CommandFullwidth | ［＃共同印刷］ | 1 | 1 | 0 | 000311_46252 |
| CommandFullwidth | ［＃共産主義者（女）］ | 1 | 1 | 0 | 000311_46246 |
| CommandFullwidth | ［＃共産党員］ | 1 | 1 | 0 | 000311_46246 |
| CommandFullwidth | ［＃共産党的］ | 1 | 1 | 0 | 000311_46248 |
| CommandFullwidth | ［＃前衛］ | 1 | 1 | 0 | 000311_46248 |
| CommandFullwidth | ［＃加藤シヅエ］ | 1 | 1 | 0 | 000311_46241 |
| CommandFullwidth | ［＃十月革命］ | 1 | 1 | 0 | 000311_46246 |
| CommandFullwidth | ［＃南京虫］ | 1 | 1 | 0 | 000311_46248 |
| CommandFullwidth | ［＃原文は括弧「〔〕」を使うが、他の所と一致させるため改める］ | 1 | 1 | 0 | 000250_4644 |
| CommandFullwidth | ［＃原阿佐緒］ | 1 | 1 | 0 | 000311_46244 |
| CommandFullwidth | ［＃厳寒］ | 1 | 1 | 0 | 000311_46253 |
| CommandFullwidth | ［＃反宗教］ | 1 | 1 | 0 | 000311_46247 |
| CommandFullwidth | ［＃口語自由詩で、民衆の現実を描こうとした、「民衆派」の詩人］ | 1 | 1 | 0 | 000160_1255 |
| CommandFullwidth | ［＃古本屋名］ | 1 | 1 | 0 | 000311_46248 |
| CommandFullwidth | ［＃同志］ | 1 | 1 | 0 | 000311_46246 |
| CommandFullwidth | ［＃呉昌碩］ | 1 | 1 | 0 | 000311_46244 |
| CommandFullwidth | ［＃喜重郎］ | 1 | 1 | 0 | 000160_1255 |
| CommandFullwidth | ［＃国際婦人デー］ | 1 | 1 | 0 | 000311_46248 |
| CommandFullwidth | ［＃土曜集会］ | 1 | 1 | 0 | 000311_46246 |
| CommandFullwidth | ［＃埋橋久子の友人、壺井栄ではない］ | 1 | 1 | 0 | 000311_46254 |
| CommandFullwidth | ［＃堺利彦］ | 1 | 1 | 0 | 000311_46242 |
| CommandFullwidth | ［＃変わり者］ | 1 | 1 | 0 | 000311_46248 |

## Decode Failures

None.

## Inputs

- matrix: `/home/bor/Projects/ab-validator/data/aozora-syntax-coverage.toml`
- index: `scratch/ab-index.json`
- corpus: `/home/bor/Dependencies/aozorabunko`
- allowlist: `/home/bor/Projects/ab-validator/data/aozora-source-inventory-allowlist.toml`
