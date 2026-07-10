use std::path::PathBuf;

use ab_coverage::matrix::RepresentabilityStatus;
use ab_coverage::source_inventory::{inventory_document, patterns_from_rows};
use ab_coverage::{
    AdapterCell, CorpusPrevalence, CoverageBasis, CoverageMatrix, ParserCell, Recognition,
    RowAatFidelity, RowError, SchemaValidator, ValidationOptions,
};

fn matrix_path() -> PathBuf {
    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    manifest
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("data/aozora-syntax-coverage.toml")
}

#[test]
fn matrix_schema_valid() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let errors = SchemaValidator::validate(&matrix, ValidationOptions::lenient());
    assert!(
        errors.is_empty(),
        "matrix has {} schema errors:\n{}",
        errors.len(),
        join_errors(&errors)
    );
}

#[test]
fn matrix_required_keys_present() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let opts = ValidationOptions {
        allow_unknown: true,
        required_keys: &["aozora2", "aozora-rs", "aozora2html"],
    };
    let errors = SchemaValidator::validate(&matrix, opts);
    assert!(
        errors.is_empty(),
        "every row must declare cells for the three known parser/adapter ids:\n{}",
        join_errors(&errors)
    );
}

#[test]
fn matrix_corpus_prevalence_present_for_every_row() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    for row in matrix.rows() {
        assert!(
            row.corpus_prevalence.is_some(),
            "row {} is missing [syntax.corpus_prevalence]",
            row.id
        );
    }
}

#[test]
fn oracle_case_syntax_rows_exist_and_link_back() {
    #[derive(serde::Deserialize)]
    struct OracleCases {
        case: Vec<OracleCase>,
    }

    #[derive(serde::Deserialize)]
    struct OracleCase {
        id: String,
        syntax_row_ids: Vec<String>,
    }

    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let rows = matrix
        .rows()
        .iter()
        .map(|row| (row.id.as_str(), row))
        .collect::<std::collections::BTreeMap<_, _>>();
    let oracle_path = matrix_path()
        .parent()
        .unwrap()
        .join("aat-oracle-cases.toml");
    let oracle: OracleCases =
        toml::from_str(&std::fs::read_to_string(oracle_path).unwrap()).unwrap();

    for case in oracle.case {
        for row_id in &case.syntax_row_ids {
            let row = rows.get(row_id.as_str()).unwrap_or_else(|| {
                panic!("oracle case {} references missing row {}", case.id, row_id)
            });
            assert!(
                row.oracle_cases.iter().any(|linked| linked == &case.id),
                "row {} must link back to oracle case {}",
                row.id,
                case.id
            );
        }
    }
}

#[test]
fn kunten_rows_detect_real_fixture_spellings() {
    use ab_coverage::detectors::{DetectorContext, DetectorRegistry};

    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let registry = DetectorRegistry::from_matrix(matrix.rows());
    let fixture = matrix_path()
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("tests/fixtures/kunten-source-excerpt.txt");
    let source = std::fs::read_to_string(&fixture).expect("read kunten fixture");
    let empty_aat = serde_json::json!({
        "version": 1,
        "work_id": "kunten-fixture",
        "blocks": [],
        "meta": {"adapter": "fixture", "adapter_version": "fixture"}
    });
    let ctx = DetectorContext {
        aat: &empty_aat,
        source: &source,
    };

    assert!(
        registry.detect("kunten.kaeriten", &ctx) > 0,
        "kunten.kaeriten must not false-zero on compact real fixture markers"
    );
    assert!(
        registry.detect("kunten.okurigana", &ctx) > 0,
        "kunten.okurigana must not false-zero on compact real fixture markers"
    );
}

#[test]
fn shared_detector_traversal_matches_per_row_detection() {
    use ab_coverage::detectors::{DetectorContext, DetectorRegistry};

    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let registry = DetectorRegistry::from_matrix(matrix.rows());
    let aat = serde_json::json!({
        "version": 1,
        "work_id": "detector-equivalence",
        "blocks": [
            {"kind": "heading", "content": [{"kind": "text", "value": "見出し"}]},
            {"kind": "paragraph", "content": [
                {"kind": "ruby", "base": "山", "reading": "やま", "direction": "left"},
                {"kind": "gaiji", "description": "U+4E00、第1水準", "jis_code": "1-16-01"},
                {"kind": "style", "style_type": "boten", "content": []},
                {"kind": "tcy", "content": []},
                {"kind": "warichu", "content": []}
            ]},
            {"kind": "figure", "caption": "図", "content": []},
            {"kind": "caption_block", "content": [{"kind": "caption"}]}
        ],
        "meta": {"adapter": "fixture", "adapter_version": "fixture"}
    });
    let source = "｜山《やま》※［＃U+4E00］［＃傍点］［＃改ページ］";
    let ctx = DetectorContext { aat: &aat, source };
    let all = registry.detect_all(&ctx);

    for row_id in registry.rows() {
        assert_eq!(
            registry.detect(row_id, &ctx),
            all[row_id],
            "shared traversal diverged for {row_id}"
        );
    }
}

#[test]
fn source_inventory_classifies_kunten_source_note_variants() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃「レ」は返り点］",
        "［＃「」内の「レ」は返り点］",
        "［＃「」内の「一二」は返り点］",
        "［＃以下の「」内の「レ一二」は返り点］",
        "［＃「」内の「レ一二」は返り点、以下同じ］",
        "［＃「」内の一二は返り点］",
        "［＃「一言」の「一」をのぞいて「レ一二三」は返り点］",
        "［＃ここで字下げ終わり、「レ」は返り点］",
        "［＃以下、「レ一二」は返り点］",
        "［＃以下「レ一二」は返り点］",
        "［＃以下の「」内の、「レ一二」は返り点］",
        "［＃返り点の「上」あり］",
        "［＃返り点の「下」あり］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known return-point source note variants should not remain unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("kunten.kaeriten")
            .map(|count| count.occurrences),
        Some(13)
    );
}

#[test]
fn source_inventory_classifies_common_corpus_command_variants() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃ここで字下げ終わり］",
        "［＃地から１字上げ］",
        "［＃ここから改行天付き、折り返して１字下げ］",
        "［＃小さな文字終わり］",
        "［＃１段階小さな文字］",
        "［＃中見出し終わり］",
        "［＃太字終わり］",
        "［＃割り注終わり］",
        "［＃横組み終わり］",
        "［＃「（c）」は縦中横］",
        "［＃改丁］",
        "［＃改段］",
        "［＃キャプション終わり］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known corpus command variants should not remain unknown"
    );
    for row_id in [
        "indentation.jisage_block",
        "indentation.chitsuki",
        "indentation.burasage",
        "decoration.font_size",
        "heading.basic",
        "decoration.bold_italic",
        "warichu.basic",
        "layout.yokogumi",
        "layout.tcy",
        "break.page_line",
        "caption.block",
    ] {
        assert!(
            summary.row_counts.contains_key(row_id),
            "expected source inventory row {row_id}"
        );
    }
}

#[test]
fn source_inventory_classifies_next_high_volume_command_variants() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃行右小書き］",
        "［＃行右小書き終わり］",
        "［＃「＊」は行右小書き］",
        "［＃横組み］",
        "［＃ここで横組み終わり］",
        "［＃ここから２６字詰め］",
        "［＃ここで字詰め終わり］",
        "［＃傍点終わり］",
        "［＃傍線終わり］",
        "〔欄外に〕",
        "〔訳註〕",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known high-volume corpus marker variants should not remain unknown"
    );
    for row_id in [
        "decoration.font_size",
        "layout.yokogumi",
        "indentation.jizume",
        "decoration.boten",
        "decoration.bousen",
        "annotation.chuuki",
    ] {
        assert!(
            summary.row_counts.contains_key(row_id),
            "expected source inventory row {row_id}"
        );
    }
}

#[test]
fn source_inventory_classifies_source_authority_tail_style_layout_variants() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃ここから２字下げ、小さい活字］",
        "［＃ここで字下げ終わり、小さい活字も終わり］",
        "［＃ここで地付き終わり］",
        "［＃ここで字上げ終わり］",
        "［＃地より１字上げ］",
        "［＃ここから地から２字上げ］",
        "［＃「ん」は小書き］",
        "［＃大文字］",
        "［＃中文字］",
        "［＃大文字、太字］",
        "［＃この行はゴシック体］",
        "［＃ゴシック体］",
        "［＃「井伏鱒二」はゴチック］",
        "［＃「一千円」は大文字、太字］",
        "［＃白三角傍点］",
        "［＃白三角傍点終わり］",
        "［＃二重傍線終わり］",
        "［＃「〃」は横組み］",
        "［＃「？！」は横一列］",
        "［＃２０字下げて、地より１字あきで］",
        "［＃天より３１字下げ、地より２字上げで］",
        "［＃以下地付き］",
        "［＃以下、地付き］",
        "［＃２文字目の「i」は下付き小文字、４文字目の「i」は上付き小文字］",
        "［＃以下の２つの英文はすべてイタリック文字、横書き］",
        "［＃（Ｆ・Ｏ）は下揃え］",
        "［＃この歌、二行前の歌に頭揃え。］",
        "［＃で各歌の頭は全て揃っている。］",
        "［＃次の３項目は２行目以降１字下げ］",
        "［＃「頓首　敬白」は地付き、地より３字アキ］",
        "［＃「（裏面欧文番組略）」は地付き、地より１字アキ］",
        "［＃ゴシック体、地付き、地より２字あげ］",
        "［＃『江馬兆策識」』は地付き］",
        "［＃ここから１０字下げ折り返して１７字下げ］",
        "［＃６字下がる］",
        "［＃改見開き右］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known tail style/layout corpus variants should not remain unknown"
    );
    for row_id in [
        "decoration.boten",
        "decoration.bold_italic",
        "decoration.bousen",
        "decoration.font_size",
        "indentation.chitsuki",
        "indentation.jisage_block",
        "layout.tcy",
        "layout.yokogumi",
    ] {
        assert!(
            summary.row_counts.contains_key(row_id),
            "expected source inventory row {row_id}"
        );
    }
}

#[test]
fn source_inventory_classifies_boten_corpus_variants() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃「天皇制」に×傍点］",
        "［＃「革命」にばつ傍点］",
        "［＃「十三人」に白三角傍点］",
        "［＃「自然の諸事物を」～「表象である」に傍点］",
        "［＃「革命」に×傍点、伏字を起こした文字］",
        "［＃「ほ」に傍点、罫囲み］",
        "［＃「けし」の傍点］",
        "［＃「しぶしぶ」は傍点］",
        "［＃「じぶんで」は傍点］",
        "［＃「た」と「ふ」の間に白三角傍点］",
        "［＃「は」と「う」の間に白三角傍点］",
        "［＃「クリティカル・エッセイ」の「・」を除く部分に傍点］",
        "［＃「ア」に点］",
        "［＃『独立とは「独り立つ」といふことなり』に傍点］",
        "［＃「右手の袖口を」から「ズボンを穿いて」まで傍点］",
        "［＃「私の生き方」に白四角傍点］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known corpus boten variants should not remain unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("decoration.boten")
            .map(|count| count.occurrences),
        Some(16)
    );
}

#[test]
fn source_inventory_classifies_caption_inline_corpus_variants() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃「第一図」はキャプション］",
        "［＃「第二図」はキャプション］",
        "［＃「第２図」はキャプション］",
        "［＃「●ブリュームリスアルプ」はキャプション］",
        "［＃「乗馬陥泥の難」はキャプション］",
        "［＃「丹後地震に伴へる郷村断層」はキャプション］",
        "［＃「「歌留多」の函」はキャプション］",
        "［＃「〈「それこそ、ひどい仕事だよ。」〉」はキャプション］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known corpus inline-caption command variants should not remain unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("caption.inline")
            .map(|count| count.occurrences),
        Some(8)
    );
}

#[test]
fn source_inventory_classifies_heading_and_keigakomi_corpus_variants() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃中見出終わり］",
        "［＃「希い――「原爆の図」によせて――」は大見出し］",
        "［＃「インターネット図書館「青空文庫」の特色」は中見出し］",
        "［＃「　　」は罫囲み］",
        "［＃「花」は罫囲み］",
        "［＃「ＧＯＴＯ」は罫囲み］",
        "［＃小見出し文字］",
        "［＃見出し文字］",
        "［＃ここで字下げ、枠囲み終わり］",
        "［＃ここで字下げ、罫囲み終わり］",
        "［＃「住友　第一」は枠囲み］",
        "［＃「」は中見出し］",
        "［＃「おぼつかぐら」は太字、罫囲み］",
        "［＃「グレコの絵との連想」に枠囲み］",
        "［＃「ソヴェト同盟ヲ守レ！」に枠線］",
        "［＃「一」は「□」囲み］",
        "［＃「拝観料」は罫で囲む］",
        "［＃以下の「残怨白紅花盛　余多人切支丹寺」は罫で囲む］",
        "［＃次の段落には、天地左右にオモテケイ囲み］",
        "［＃この行全体はミシン罫囲み］",
        "［＃「小沼農場」に大見出し］",
        "［＃以下、「次ぎの…」から「…困ります。」までは罫線囲み］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known corpus heading and keigakomi variants should not remain unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("heading.basic")
            .map(|count| count.occurrences),
        Some(7)
    );
    assert_eq!(
        summary
            .row_counts
            .get("decoration.keigakomi")
            .map(|count| count.occurrences),
        Some(15)
    );
}

#[test]
fn source_inventory_classifies_table_and_multicolumn_corpus_variants() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃ここから２段組み］",
        "［＃ここで段組み終わり］",
        "［＃ここから２段組］",
        "［＃ここで段組終わり］",
        "［＃段組み適用外］",
        "［＃ここから表］",
        "［＃ここで表終わり］",
        "［＃ここから表組］",
        "［＃ここで表組終わり］",
        "［＃ここから表罫囲み］",
        "［＃ここで表罫囲み終わり］",
        "［＃「うどん」と「きそば」は２列に並ぶ］",
        "［＃「ただ」と「咲」は２列に並ぶ］",
        "［＃「赤江米子氏」と「母の或部分」は２列に並ぶ］",
        "［＃「權次」と「權六」は横並びになっている］",
        "［＃１段目］",
        "［＃２段目］",
        "［＃「岩波日本」と「文学講座」が１行内で２行に分けられている］",
        "［＃「午後一時開演」「同　五時終了」は２行組み、地付き］",
        "［＃「新来朝」「五国聯合」は２行組み、ゴシック体。「バード・ストーン一座大曲馬」は特大文字、ゴシック体］",
        "［＃ここから２段組み、段間に罫］",
        "［＃ここで２段組み、罫囲み終わり］",
        "［＃１行目］",
        "［＃２行目］",
        "［＃３行目］",
        "［＃ここに表組入る、別ファイル（densyanokonzatsu_table.txt）参照］",
        "［＃ここで字下げ、表罫囲み終わり］",
        "［＃ここからプログラム、表罫囲み］",
        "［＃ここでプログラム（表罫囲み）終わり］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known corpus table and multi-column markers should not remain unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("layout.multicolumn")
            .map(|count| count.occurrences),
        Some(16)
    );
    assert_eq!(
        summary
            .row_counts
            .get("structure.table")
            .map(|count| count.occurrences),
        Some(13)
    );
}

#[test]
fn source_inventory_classifies_warigaki_corpus_variants() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = ["［＃割書］", "［＃割書終わり］", "「注」の割書"].join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known corpus warigaki markers should not remain unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("warigaki.parenthetical")
            .map(|count| count.occurrences),
        Some(2)
    );
}

#[test]
fn source_inventory_classifies_annotation_editor_notes() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃「、」は底本では「。」］",
        "［＃ママ］",
        "［＃ルビは「悪魔の尿溜」にかかる］",
        "［＃入力者注(5)］",
        "［ルビの「おもて」は底本では「うら」］",
        "［＃「。」は底本では欠落］",
        "［＃「詫び」は底本では「詑び」と誤植］",
        "［＃底本のまま］",
        "［＃「。」はママ］",
        "［＃ルビ抜けはママ］",
        "［＃「い」に「ママ」注記］",
        "［＃「註」略］",
        "［＃「Ｂ圖」省略］",
        "［＃本文中、伏せ字は「＊」で表した。］",
        "［＃底本２字伏字］",
        "［＃図が入るが省略。底本44ページ］",
        "［＃ルビの「しふ」は初出では「しう」］",
        "［＃「み」余分か、それとも「見」か？］",
        "［＃「言」は、『谷崎潤一郎全集　第十九巻』（中央公論新社2015年6月10日初版発行）と『谷崎潤一郎全集　第十五卷』（中央公論社1968年1月25日発行）では「云」］",
        "［＃創元社版では「破損」］",
        "［＃岩波文庫の注は「翌三年十二月の誤り」とする］",
        "［＃改行を挿入］",
        "［＃原文まま］",
        "［＃初出時「………………………行ぐ奴からさかしまに……………やるまでよ！」］",
        "［＃初出の『四季』第四號・昭和十年二月號では「孤獨を愛する人にとつて」となっている］",
        "［＃「弟」は誤訳で本当は「兄」］",
        "［＃「奥深く」は筑摩版では「奥深く広く」］",
        "［＃岩波文庫版では「殺され」］",
        "［＃旺文社文庫版「なんぼ土産にするとかって」］",
        "［＃句点が抜けていると考えられる］",
        "［＃この作品は表題と副題のみで、本文はありません。］",
        "［＃「雲隠れ」の帖は冒頭の晶子詞のみで本文はありません。］",
        "［＃ルビは「弄び物」に付く］",
        "［＃「起上り」にルビ］",
        "［＃「【例題五】」は定本では「【例題六】］",
        "［＃「お伽話」のルビ］",
        "［＃「あめとう」に欄外に校注、「アメリカ唐桟の略」］",
        "［＃「どんどん」に欄外に校注、「三橋の側にあった不忍池の水の落口」］",
        "［＃「お前のような不孝者は」か？］",
        "［＃「てまえ」あるいは「てめえ」か］",
        "［＃「ジップ」は桃源社版では「ジッブ」］",
        "［＃「色々に盛装して」または「色々な盛装をして」と思われる］",
        "［＃「今の天皇」は「大正天皇」］",
        "［＃「佐土布都の神」は本文の書き下し文では「佐士布都の神」］",
        "［＃「ベネズェラ」は本文では「ベネズエラ」］",
        "［＃「マリニヨリの」は本文では「マリニョリの」］",
        "［＃「ムツセメルリ」は本文では「ムッセメルリ」］",
        "［＃「把握するのである。」は初刊本「人生論ノート」創元社、昭和16年8月11日発行では「把握するのである。しかしながら愛するといふことは如何に困難であるか。」］",
        "［＃「本誌」は「改造」］",
        "［＃「な」は判読困難につき推定、コマ25-左-3］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known editorial annotation markers should not remain unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("annotation.chuuki")
            .map(|count| count.occurrences),
        Some(50)
    );
}

#[test]
fn source_inventory_classifies_source_page_reference_notes() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃「一六八頁」は「安康天皇」の「市の邊の押齒の王」］",
        "［＃「一三九頁」は「應神天皇」の「天の日矛」］",
        "［＃「二七頁」は「伊耶那岐の命と伊耶那美の命」の「身禊」］",
        "［＃欄外に「続千載集巻四、秋上、太政大臣。」の校注あり］",
        "［＃、87-上段-7］",
        "［＃、94-下段-19］",
        "［＃「三六六ページ」は「清寧天皇・顯宗天皇・仁賢天皇」の「シジムの新築祝い」］",
        "［＃「二三〇頁」は「大國主の神」］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known source page/cross-reference notes should not remain unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("source.page_reference")
            .map(|count| count.occurrences),
        Some(8)
    );
}

#[test]
fn table_and_multicolumn_rows_have_raw_preserved_representability() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    for (row_id, expected_projection) in
        [("layout.multicolumn", "div"), ("structure.table", "table")]
    {
        let row = matrix
            .rows()
            .iter()
            .find(|row| row.id == row_id)
            .unwrap_or_else(|| panic!("{row_id} row"));
        let representability = row
            .representability
            .as_ref()
            .unwrap_or_else(|| panic!("{row_id} needs a reviewed representability cell"));

        assert_eq!(
            representability.status,
            RepresentabilityStatus::RawPreserved
        );
        assert!(representability.raw_fallback);
        assert!(
            row.tei_projection.contains(expected_projection),
            "{row_id} needs a TEI projection preserving {expected_projection}"
        );
    }
}

#[test]
fn annotation_rows_have_reviewed_representability() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    for row_id in ["annotation.chuuki", "annotation.bouki"] {
        let row = matrix
            .rows()
            .iter()
            .find(|row| row.id == row_id)
            .unwrap_or_else(|| panic!("{row_id} row"));
        let representability = row
            .representability
            .as_ref()
            .unwrap_or_else(|| panic!("{row_id} needs a reviewed representability cell"));

        assert_eq!(
            representability.status,
            RepresentabilityStatus::RawPreserved
        );
        assert!(representability.raw_fallback);
        assert!(
            row.tei_projection.contains("note"),
            "{row_id} needs a TEI note projection"
        );
    }
}

#[test]
fn gaiji_subform_rows_have_typed_representability() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    for row_id in [
        "gaiji.unicode_codepoint",
        "gaiji.jis_code",
        "gaiji.un_embed",
    ] {
        let row = matrix
            .rows()
            .iter()
            .find(|row| row.id == row_id)
            .unwrap_or_else(|| panic!("{row_id} row"));
        let representability = row
            .representability
            .as_ref()
            .unwrap_or_else(|| panic!("{row_id} needs a reviewed representability cell"));

        assert_eq!(representability.status, RepresentabilityStatus::Typed);
        assert!(representability.raw_fallback);
        assert!(
            representability
                .aat_nodes
                .iter()
                .any(|candidate| candidate == "gaiji"),
            "{row_id} should preserve a typed gaiji node when adapters/parser expose it"
        );
        assert!(
            row.tei_projection.contains("g"),
            "{row_id} needs a TEI g projection"
        );
    }
}

#[test]
fn media_rows_have_typed_representability() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    for (row_id, expected_node, expected_projection) in [
        ("caption.inline", "caption", "head"),
        ("caption.block", "caption_block", "figure"),
        ("figure.image_inline", "figure", "graphic"),
    ] {
        let row = matrix
            .rows()
            .iter()
            .find(|row| row.id == row_id)
            .unwrap_or_else(|| panic!("{row_id} row"));
        let representability = row
            .representability
            .as_ref()
            .unwrap_or_else(|| panic!("{row_id} needs a reviewed representability cell"));

        assert_eq!(representability.status, RepresentabilityStatus::Typed);
        assert!(representability.raw_fallback);
        assert!(
            representability
                .aat_nodes
                .iter()
                .any(|candidate| candidate == expected_node),
            "{row_id} should preserve {expected_node} when adapters/parser expose it"
        );
        assert!(
            row.tei_projection.contains(expected_projection),
            "{row_id} needs a TEI projection preserving {expected_projection}"
        );
    }
}

#[test]
fn emphasis_and_decoration_rows_have_reviewed_representability() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    for (row_id, expected_status, expected_node, expected_projection) in [
        (
            "emphasis.basic",
            RepresentabilityStatus::Typed,
            "style",
            "hi",
        ),
        (
            "decoration.boten",
            RepresentabilityStatus::Typed,
            "style",
            "boten",
        ),
        (
            "decoration.bousen",
            RepresentabilityStatus::Typed,
            "style",
            "bousen",
        ),
        (
            "decoration.direction_override",
            RepresentabilityStatus::RawPreserved,
            "style",
            "place",
        ),
    ] {
        let row = matrix
            .rows()
            .iter()
            .find(|row| row.id == row_id)
            .unwrap_or_else(|| panic!("{row_id} row"));
        let representability = row
            .representability
            .as_ref()
            .unwrap_or_else(|| panic!("{row_id} needs a reviewed representability cell"));

        assert_eq!(representability.status, expected_status);
        assert!(representability.raw_fallback);
        assert!(
            representability
                .aat_nodes
                .iter()
                .any(|candidate| candidate == expected_node),
            "{row_id} should preserve {expected_node} when adapters/parser expose it"
        );
        assert!(
            row.tei_projection.contains(expected_projection),
            "{row_id} needs a TEI projection preserving {expected_projection}"
        );
    }
}

#[test]
fn structural_inline_rows_have_typed_representability() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    for (row_id, expected_node, expected_projection) in [
        ("heading.dogyo", "heading", "rend"),
        ("heading.mado", "heading", "rend"),
        ("layout.yokogumi", "yokogumi", "yokogumi"),
        ("ruby.placement_directional", "ruby", "place"),
        ("warigaki.parenthetical", "warigaki", "warigaki"),
    ] {
        let row = matrix
            .rows()
            .iter()
            .find(|row| row.id == row_id)
            .unwrap_or_else(|| panic!("{row_id} row"));
        let representability = row
            .representability
            .as_ref()
            .unwrap_or_else(|| panic!("{row_id} needs a reviewed representability cell"));

        assert_eq!(representability.status, RepresentabilityStatus::Typed);
        assert!(representability.raw_fallback);
        assert!(
            representability
                .aat_nodes
                .iter()
                .any(|candidate| candidate == expected_node),
            "{row_id} should preserve {expected_node} when adapters/parser expose it"
        );
        assert!(
            row.tei_projection.contains(expected_projection),
            "{row_id} needs a TEI projection preserving {expected_projection}"
        );
    }
}

#[test]
fn source_inventory_classifies_shorthand_directional_ruby() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = "［＃左にルビ付き］";
    let summary = inventory_document("fixture", source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "shorthand left-ruby source marker should not remain unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("ruby.placement_directional")
            .map(|count| count.occurrences),
        Some(1)
    );
}

#[test]
fn kanbun_and_reference_rows_have_typed_representability() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    for (row_id, expected_node, expected_projection) in [
        ("iteration.kunoji", "gaiji", "g"),
        ("kunten.kaeriten", "style", "kaeriten"),
        ("kunten.okurigana", "ruby", "okurigana"),
        ("reference.frontref", "style", "ref"),
    ] {
        let row = matrix
            .rows()
            .iter()
            .find(|row| row.id == row_id)
            .unwrap_or_else(|| panic!("{row_id} row"));
        let representability = row
            .representability
            .as_ref()
            .unwrap_or_else(|| panic!("{row_id} needs a reviewed representability cell"));

        assert_eq!(representability.status, RepresentabilityStatus::Typed);
        assert!(representability.raw_fallback);
        assert!(
            representability
                .aat_nodes
                .iter()
                .any(|candidate| candidate == expected_node),
            "{row_id} should preserve {expected_node} when adapters/parser expose it"
        );
        assert!(
            row.tei_projection.contains(expected_projection),
            "{row_id} needs a TEI projection preserving {expected_projection}"
        );
    }
}

#[test]
fn source_inventory_classifies_indentation_corpus_variants() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃改行天付き、折り返して１字下げ］",
        "［＃ここから３字下げ、１行２０字組みで］",
        "［＃ここで字下げ、２０字組み終わり］",
        "［＃天から２字下げ］",
        "［＃ここから２字下げ、２２字詰め］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known indentation corpus variants should not remain unknown"
    );
    for row_id in [
        "indentation.burasage",
        "indentation.jisage_block",
        "indentation.jisage_oneline",
        "indentation.jizume",
    ] {
        assert!(
            summary.row_counts.contains_key(row_id),
            "expected source inventory row {row_id}"
        );
    }
}

#[test]
fn source_inventory_classifies_jisage_wording_variants() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃字下げ終わり］",
        "［＃1字下げ終わり］",
        "［＃１字下げここまで］",
        "［＃ここで字下げおわり］",
        "［＃ここで字下げ終り］",
        "［＃ここより１字下げ］",
        "［＃以下３字下げ］",
        "［＃ここから一字下げ］",
        "［＃改行ごとに二字下げ］",
        "［＃二字下げ終わり］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known jisage wording variants should not remain unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("indentation.jisage_block")
            .map(|count| count.occurrences),
        Some(10)
    );
}

#[test]
fn source_inventory_classifies_chitsuki_alignment_corpus_variants() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃下げて、地より１字あきで］",
        "［＃下げて地より２字あきで］",
        "［＃２１字下げ、地より２字あきで］",
        "［＃地付き、地より３字アキ］",
        "［＃地付きで］",
        "［＃こ地付き］",
        "［＃以下の文章は地付き］",
        "［＃この日付は行末に記す］",
        "［＃この行はポイントを下げて、地より２字上げ］",
        "［＃「地付き］",
        "［＃右寄せ］",
        "［＃地より２字上がり］",
        "［＃この行は行末より１字上がり］",
        "［＃下げて地付きで］",
        "［＃地寄せ］",
        "［＃文末より１字上げ揃え］",
        "［＃「訳者」は文末より１字上げ揃え］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known chitsuki/right-alignment corpus variants should not remain unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("indentation.chitsuki")
            .map(|count| count.occurrences),
        Some(17)
    );
}

#[test]
fn indentation_rows_have_typed_representability() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    for (row_id, node, projection) in [
        ("indentation.basic", "jisage_block", "rend"),
        ("indentation.jisage_block", "jisage_block", "jisage"),
        ("indentation.jisage_oneline", "style", "jisage"),
        ("indentation.chitsuki", "style", "chitsuki"),
        ("indentation.jizume", "style", "jizume"),
        ("indentation.burasage", "style", "burasage"),
    ] {
        let row = matrix
            .rows()
            .iter()
            .find(|row| row.id == row_id)
            .unwrap_or_else(|| panic!("{row_id} row"));
        let representability = row
            .representability
            .as_ref()
            .unwrap_or_else(|| panic!("{row_id} needs a reviewed representability cell"));

        assert_eq!(representability.status, RepresentabilityStatus::Typed);
        assert!(representability.raw_fallback);
        assert!(
            representability
                .aat_nodes
                .iter()
                .any(|candidate| candidate == node),
            "{row_id} should preserve {node} when adapters/parser expose it"
        );
        assert!(
            row.tei_projection.contains(projection),
            "{row_id} needs a TEI projection preserving {projection}"
        );
    }
}

#[test]
fn source_inventory_classifies_font_size_subscript_variants() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃「b」は下付き小文字］",
        "［＃「a,1」は下付き小文字］",
        "［＃「”」は下付き］",
        "［＃「1」はすべて下付き小文字］",
        "［＃アラビア数字はすべて下付き小文字］",
        "［＃「大字」は１段階小さな文字］",
        "［＃「阿」は一段階小さな文字］",
        "［＃「〃」は上部に出ている］",
        "［＃「競吟」は上部に出ている］",
        "［＃大きな文字終わり］",
        "［＃ここで１段階小さな文字終わり］",
        "［＃「（「ギヨオテ傳」）」は１段階小さな文字］",
        "［＃「Ａｎ」はそれぞれ縦中横、数字は上付き小書き］",
        "［＃１回り大きな文字］",
        "［＃２回り大きな文字］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known font-size and subscript corpus variants should not remain unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("decoration.font_size")
            .map(|count| count.occurrences),
        Some(15)
    );
}

#[test]
fn source_inventory_classifies_dotted_letter_variants() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "Sam［＃mは上ドット付き］",
        "Sas［＃sは下ドット付き］",
        "Sisa［＃２つめのsは下ドット付き］",
        "Visnu［＃snはともに下ドット付き］",
        "Samsa［＃mは上ドット付き。２つめのsは下ドット付き］",
        "Konkana［＃前のnは上ドット付き、後のnは下ドット付き］",
        "Ta［＃Tは下ドット付き］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known dotted-letter corpus variants should not remain unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("accent.dotted_letter")
            .map(|count| count.occurrences),
        Some(7)
    );
}

#[test]
fn source_inventory_classifies_glyph_variant_notes() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃「ル」は上に「⌒」付き］",
        "［＃一つ目の「e」は「´」付き］",
        "［＃「e」はアクサン（´）付き］",
        "［＃ηに帯気、ωに曲アクセント］",
        "［＃最初のαに平息、３文字目のαに鋭アクセント、σはファイナルシグマ］",
        "［＃「?!」は一字］",
        "［＃「2」は指数］",
        "［＃「1/4」は分数］",
        "［＃「√」の中に「５」］",
        "［＃「♂」は矢印が下向き］",
        "［＃「Ｏ」は覆面の英字です。］",
        "［＃返り点の「二」の右横に縦棒あり］",
        "［＃「ちへ」の右に「）」］",
        "［＃「エ」は小さい「ヱ」］",
        "［＃「IV」はローマ数字の４］",
        "［＃「井」は○付き文字］",
        "［＃「印」は○付き文字］",
        "［＃「焔」の火へんを炎にしたうえで、へんとつくりをいれかえた字、焔の正字と同字］",
        "［＃「３」は「√」の記号の中に入っている］",
        "［＃「※」は「たけかんむり＋隻」、17-8］",
        "［＃「※」は「つつみがまえ（勹）」＋「夕」で、読みは「そうそう」67-6］",
        "［＃「！？」は１マスに横並び］",
        "［＃「A」は accent grave（｀）付き］",
        "［＃「e」はマクロン付き（-）E小文字］",
        "［＃「mao」の「a」に長音記号］",
        "［＃「prthu」のrは下ドット付き］",
        "［＃「u」の上に「^」がつく］",
        "［＃「ο」はアキュートアクセント付き］",
        "［＃「Novae」「discendae」「docendae」および「Jurisprudentiae」のそれぞれの末尾「ae」は、「a」と「e」の合字］",
        "［＃「□□」は２倍の長方形］",
        "［＃「え」は「江」のくずし字］",
        "［＃「t」は下点付き、182-6］",
        "［＃「　」は欠字］",
        "［＃「かしく」は崩し字］",
        "［＃「シ」の右上に小さな四角あり］",
        "［＃「!!!」は一文字、111-18］",
        "［＃simhaのmは上ドット付き］",
        "［＃右下の部分は「蝎」の右下部と同形］",
        "［＃やまいだれの中は「間」］",
        "［＃「楫」に「ほこづくり」を加える、55-12］",
        "［＃「漱」の「欠」に代えて「攵」］",
        "［＃「入場料」から「七円」まで大文字、ゴシック体。「＝＝」は二倍二重ダーシ］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known glyph-variant source notes should not remain unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("glyph.variant_note")
            .map(|count| count.occurrences),
        Some(42)
    );
}

#[test]
fn source_inventory_classifies_source_note_labels() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃劇場名］",
        "［＃ホテル名］",
        "［＃お手伝いさん］",
        "［＃夫人］",
        "［＃長男］",
        "［＃暢彦、次男］",
        "［＃中條華、中條家三女。百合子が長女、次女は千鶴（生後四ヵ月で死亡）］",
        "［＃探偵小説家、生理学者。本名は、林髞］",
        "［＃スカーフ］",
        "［＃未完］",
        "［＃「（１）」は注釈番号］",
        "［＃中條太郎］",
        "［＃倉知誠夫、倉知貞の夫］",
        "［＃英男の家庭教師］",
        "［＃母］",
        "［＃甥］",
        "［＃ゴーリキー］",
        "［＃トゥルビン家のありし日］",
        "［＃並木道］",
        "［＃昼食］",
        "［＃国男］",
        "［＃宮本顕治］",
        "［＃村田安］",
        "［＃村田敏子、荒木茂の姪］",
        "［＃松平正次］",
        "［＃労農赤色海軍］",
        "［＃協同組合住宅］",
        "［＃吼えろ、支那］",
        "［＃西村茂樹］",
        "［＃辻馬車］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "source label notes should be reviewed source-authority markers"
    );
    assert_eq!(
        summary
            .row_counts
            .get("source.note_label")
            .map(|count| count.occurrences),
        Some(30)
    );
}

#[test]
fn source_inventory_classifies_residual_source_label_gloss_notes() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃АОМС、モスクワ・ソビエトの行政部］",
        "［＃ВОКС、全ソ対外文化連絡協会］",
        "［＃Госиздат］",
        "［＃МХАТ、モスクワ芸術座］",
        "［＃моя＝私の愛する人（呼びかけ）、湯浅芳子のこと］",
        "［＃обед］",
        "［＃お茶の水附属高等女学校同窓会］",
        "［＃七たび生まれ変わって、国に報いるの意］",
        "［＃終電の別称］",
        "［＃解剖学者。随筆家］",
        "［＃理論物理学者。科学思想家］",
        "［＃チェレパーノワ＝ヨー子］",
        "［＃家族全員で死ぬこと］",
        "［＃市川男女蔵＝市川左団次］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "residual glossary/source-label notes should be reviewed source-authority markers"
    );
    assert_eq!(
        summary
            .row_counts
            .get("source.note_label")
            .map(|count| count.occurrences),
        Some(14)
    );
}

#[test]
fn source_inventory_classifies_quote_and_letter_block_markers() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃これより手紙文、１字下げ］",
        "［＃ここから引用文、３字下げ］",
        "［＃ここから引用文、３字下げ、３行アキ］",
        "［＃引用文終わり］",
        "［＃ここで引用文終り］",
        "［＃ここより手紙文、１字下げ］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known quote and letter block markers should not remain unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("structure.quote_block")
            .map(|count| count.occurrences),
        Some(6)
    );
}

#[test]
fn source_inventory_classifies_residual_layout_media_and_decoration_notes() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃「著　者」は天より４０字下げ、地より２字上げ］",
        "［＃「複」の文字の下から２字下げ、横組み右揃えで］",
        "［＃ここから天付き折り返して１字下げ］",
        "［＃ここから字下げ］",
        "［＃ここから小文字、２字下げ。冒頭のみ１字下げ］",
        "［＃ここから引用文、３字下げ、はじめの「一」のみ２字下げ］",
        "［＃ここから２字下げ　］",
        "［＃ここから３　字下げ］",
        "［＃ここから４字下げ。鍵括弧のついた台詞のみ、３字下げ。］",
        "［＃地から五字上げ］",
        "［＃字下げ、地付きここまで］",
        "［＃図「ソヴェト選挙系統」入る、P516］",
        "［＃図１～図３は、右から続く一葉］",
        "［＃子供が描いた地図入る：星形の都市を川が横断し、鉄道が縦断、中央に運動グラウンドとレーニン記念像、西側と北側に住宅・労働者クラブ、東側に天文学校・小学校（四年制、七年制、九年制）・職業学校・託児所・子供の遊び場・ピオニェールのクラブ、都市の周囲にはソヴェト農場「ピオニェール」、川沿いに都市に近い側から皮革工場・織物工場・染工場・紡績工場・発電所・ピオニェール野営所がある。ピオニェール＝開拓者（パイオニア）、旧ソ連の少年団］",
        "［＃巻頭に梅津只圓翁の写真と合わせて３枚の写真あり］",
        "［＃扉の挿絵（fig49192_01png、横356×縦292）入る］",
        "［＃昭和新山の出来た経過を示す図入る］",
        "［＃楽譜入る］",
        "［＃この行はポイントを下げ、「昔の武蔵野今は東京府下」は地より１１字上げ］",
        "［＃ここのみ拗音が小さい字「っ」になっている］",
        "［＃次３行は、文字はゴシック体、罫線は全て波線］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "residual layout, media, and decoration source notes should not remain unknown"
    );
    for row_id in [
        "indentation.jisage_block",
        "indentation.burasage",
        "indentation.chitsuki",
        "structure.quote_block",
        "layout.yokogumi",
        "figure.image_inline",
        "decoration.font_size",
        "decoration.bold_italic",
        "decoration.bousen",
    ] {
        assert!(
            summary.row_counts.contains_key(row_id),
            "expected source inventory row {row_id}"
        );
    }
}

#[test]
fn source_inventory_classifies_residual_layout_and_typeface_scope_markers() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃ここからページ上部横組み］",
        "［＃ページ上部横組み終わり］",
        "［＃ここから本文外に横書き］",
        "［＃ここで本文外横書き終わり］",
        "［＃ここから横書き］",
        "［＃ここで横書き終わり］",
        "［＃ここから横組みの表］",
        "［＃ここで横組みの表終わり］",
        "［＃ここから２段組、上段］",
        "［＃ここで２段組、上段終わり］",
        "［＃ここから２段組、下段］",
        "［＃ここで２段組、下段終わり］",
        "［＃ここから手書き文字］",
        "［＃ここで手書き文字終わり］",
        "［＃ここから教科書体］",
        "［＃ここで教科書体終わり］",
        "［＃「百合子」は手書き文字］",
        "［＃「顯治様」は手書き文字］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "residual layout and typeface source markers should not remain unknown"
    );
    for row_id in [
        "layout.yokogumi",
        "structure.table",
        "layout.multicolumn",
        "decoration.typeface",
    ] {
        assert!(
            summary.row_counts.contains_key(row_id),
            "expected source inventory row {row_id}"
        );
    }
}

#[test]
fn source_inventory_classifies_residual_layout_annotation_notes() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃「どこやらに」と「稻妻に」の句の上には、この二つの句を括る波括弧あり］",
        "［＃「堀割になれて」と「堀割に風の」の句の上には、この二つの句を括る波括弧あり］",
        "［＃「｝に等しく、」は前の５行にわたる］",
        "［＃「｝であろう、」は前の６行にわたる」］",
        "［＃以下「Ａ」と「Ｂ」は「油」の下で二行に分かれ、「Ａ」「Ｂ」の下に上向きのくくり記号］",
        "［＃「直木」と「菊池」の中間に「手直り表」］",
        "［＃「独立性をもたせたのは」と「はっきりさせたのは、」は２行］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "residual source layout annotations should be reviewed source-authority markers"
    );
    assert_eq!(
        summary
            .row_counts
            .get("annotation.layout_note")
            .map(|count| count.occurrences),
        Some(7)
    );
}

#[test]
fn source_inventory_classifies_residual_source_context_notes() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃この日、海野がしたためた遺書を、以下に引く］",
        "［＃ここには室生犀星の詩が引用されている］",
        "［＃ここに土田杏村の「跋」入る］",
        "［＃以上、宮原晃一郎による解説］",
        "［＃以下、新聞の切抜き］",
        "［＃ドイツの開発した、有翼のロケット爆弾機］",
        "［＃マリアナ基地からのＢ29、東京を初偵察］",
        "［＃天皇、神格化否定の詔勅。いわゆる人間宣言］",
        "［＃実際は五月十日付が最終のたより］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "residual source context notes should be reviewed source-authority markers"
    );
    assert_eq!(
        summary
            .row_counts
            .get("source.note_label")
            .map(|count| count.occurrences),
        Some(9)
    );
}

#[test]
fn source_inventory_classifies_residual_inline_layout_and_style_notes() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃「３字下げ」］",
        "［＃場面設定の表題、及び「Ｔ」で始まる最初の行以外は、１文字下げた位置で頭を揃える］",
        "［＃「幕。」は地付き］",
        "［＃「幕。」は地付け］",
        "［＃「江」はポイント小さく右寄せ］",
        "［＃「約」は小さめの文字］",
        "［＃「細字の部分」は割り注で処理］",
        "［＃ここで小文字、字下げ終わり］",
        "［＃下げて、地付きで］",
        "［＃天から２８字下げて］",
        "［＃本文の台詞部分は２行目から、その台詞の最後まで天より１字下げ。ト書き部分は天より４字下げ（ト書きの段落の１行目は４字下げてある）］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "residual inline layout/style source markers should not remain unknown"
    );
    for row_id in [
        "indentation.jisage_block",
        "indentation.chitsuki",
        "decoration.font_size",
        "warichu.basic",
        "annotation.layout_note",
    ] {
        assert!(
            summary.row_counts.contains_key(row_id),
            "expected source inventory row {row_id}"
        );
    }
}

#[test]
fn source_inventory_classifies_residual_source_label_name_notes() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃わが息子］",
        "［＃われらが青春］",
        "［＃アコーデオン］",
        "［＃カバンを持った男］",
        "［＃カルタ遊び］",
        "［＃ケーキ］",
        "［＃サマータイム］",
        "［＃バチェラー八重子、アイヌ］",
        "［＃ペレール、両親が滞在していたアパート］",
        "［＃メリー・ツィン］",
        "［＃モスクワの官営売店］",
        "［＃モスクワプロレタリア作家協会］",
        "［＃ルイバコフの妻］",
        "［＃ロシアプロレタリア作家同盟］",
        "［＃中山正直、本田道之の弟］",
        "［＃中川八十勝、電気試験所時代の同僚］",
        "［＃中村吉右衛門、尾上菊五郎］",
        "［＃中村吉蔵］",
        "［＃久米正雄］",
        "［＃五ヵ年計画］",
        "［＃伊藤白蓮］",
        "［＃全日本無産者芸術団体協議会］",
        "［＃八十勝］",
        "［＃共産主義青年同盟］",
        "［＃宮本トミ］",
        "［＃宮本友子］",
        "［＃宮本捨吉］",
        "［＃宮本顕治の生家］",
        "［＃富樫はつ、中條家書生］",
        "［＃小林房次郎、中條家の書生］",
        "［＃山尾市次郎、中條家小作人］",
        "［＃岡東浩。海野の神戸一中時代の友人。三菱商事勤務。麻布に居住］",
        "［＃弁護士、政治家。戦後、公職追放処分を受けるが、東京裁判では東条英機の主任弁護士となる］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "residual source label/name notes should be reviewed source-authority markers"
    );
    assert_eq!(
        summary
            .row_counts
            .get("source.note_label")
            .map(|count| count.occurrences),
        Some(33)
    );
}

#[test]
fn source_inventory_classifies_source_authority_remaining_command_batch() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃ここからページ下部縦組み］",
        "［＃ページ下部縦組み終わり］",
        "［＃ここから罫仕切り、----で挾まれた部分が一つの仕切り内］",
        "［＃ここで罫仕切り終わり］",
        "［＃ここから紙幣の文字の訳文］",
        "［＃ここで訳文終わり］",
        "［＃この行は下に横組みで］",
        "［＃この行は枠の上に横書き］",
        "［＃この行は枠囲み］",
        "［＃「才助」は枠囲い］",
        "［＃「＋」は点線丸囲み］",
        "［＃「第二十章　必死の努力」は中中見出し］",
        "［＃中文字、ゴシック体］",
        "［＃前の行とは0.5行アキ、「犯人の第二告白」はゴシック体］",
        "［＃列項目名は２段組、１段目］",
        "［＃列項目名２段目は１段目をそれぞれ２分割］",
        "［＃右図の解説文終わり］",
        "［＃左図の解説文終わり］",
        "［＃図「ロシア社会主義連邦ソヴェト共和国中央及地方機関ノ交互関係」P524、「我国之国家機構図」P525］",
        "［＃次行は三字下げ、九字空き地付きで］",
        "［＃行末から１字上で地付き］",
        "［＃「私の二十五日」全体にかかるルビ］",
        "［＃「（一）」は自注］",
        "［＃「＊」は注釈記号。欄外に「釜の製造元」の注］",
        "［＃印刷不鮮明、87-14］",
        "［＃読みは「つね」］",
        "［＃現代語訳「さびしい林の中の草の庵にひとり坐して暁をむかえると、折から仏・法・僧の三宝を唱える一羽の鳥の声を聞いた。」］",
        "［＃一九四二（昭和十七）年一月から五月にかけて、海野は海軍報道班文学挺身隊員として従軍］",
        "［＃朝日新聞社カメラマン。一九四二（昭和十七）年に海野が海軍報道班員として従軍した際、共にラバウルに］",
        "［＃満蒙開拓移民の指導などに当たった、明治―昭和期の農本主義者］",
        "［＃神奈川県国府津の海岸に中條家の別荘があった］",
        "［＃東京都世田谷区若林町］",
        "［＃ＪＲとなった国電の旧称］",
        "［＃項目名］",
        "［＃食料品店名］",
        "［＃船名「香取丸」］",
        "［＃（一）は自注］",
        "［＃１字アキか改行か判然せず］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "remaining reviewed command families should not stay outside the source-authority gate"
    );
    assert_eq!(
        summary
            .row_counts
            .get("source.reviewed_residual_command")
            .map(|count| count.occurrences),
        Some(38)
    );
}

#[test]
fn source_inventory_classifies_residual_source_label_provenance_batch() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃中野鈴子］",
        "［＃党員証］",
        "［＃公職追放］",
        "［＃共同印刷］",
        "［＃共産主義者（女）］",
        "［＃共産党員］",
        "［＃共産党的］",
        "［＃前衛］",
        "［＃加藤シヅエ］",
        "［＃十月革命］",
        "［＃南京虫］",
        "［＃原阿佐緒］",
        "［＃反宗教］",
        "［＃国際婦人デー］",
        "［＃土曜集会］",
        "［＃堺利彦］",
        "［＃大杉栄］",
        "［＃大森咲江］",
        "［＃天皇の諮問機関、枢密院の異称］",
        "［＃宮本百合子の作品「赤い貨車」のナースチャのモデル］",
        "［＃宮本美代］",
        "［＃宮部金吾］",
        "［＃小売店］",
        "［＃少年団員］",
        "［＃少年団］",
        "［＃帝国主義］",
        "［＃戦争終結の詔勅を放送］",
        "［＃戦時下食糧統制の一環として配給された、外食券を利用する食堂。現金があっても、券がなければ食べられなかった］",
        "［＃新興宗教、璽宇教教祖璽光尊、幹部の元横綱双葉山、棋士呉清源ら、食糧管理法違犯により二十一日に逮捕］",
        "［＃日本プロレタリア・エスペランチスト同盟］",
        "［＃日本プロレタリア美術家同盟］",
        "［＃映画館名］",
        "［＃映画］",
        "［＃東久邇宮稔彦首相］",
        "［＃東京高等師範学校］",
        "［＃横須賀鎮守府。鎮守府は、海軍の根拠地に置かれた機関］",
        "［＃武者小路実篤のペンネーム］",
        "［＃河原崎長十郎］",
        "［＃海野の別ペンネーム］",
        "［＃社会民主党の右翼少数派、ボルシェビキ（左翼多数派）と対立］",
        "［＃第一次世界大戦の休戦記念日］",
        "［＃第八十六通常議会］",
        "［＃統一労働総同盟］",
        "［＃金融緊急措置令。新円発行、旧円預金は封鎖］",
        "［＃降伏文書の調印式場として使われた］",
        "［＃青鞜］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "residual source-label and provenance notes should remain raw-preserved, not unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("source.reviewed_residual_command")
            .map(|count| count.occurrences),
        Some(46)
    );
}

#[test]
fn source_inventory_classifies_residual_source_label_name_batch() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃休息の家］",
        "［＃古本屋名］",
        "［＃同志］",
        "［＃呉昌碩］",
        "［＃喜重郎］",
        "［＃埋橋久子の友人、壺井栄ではない］",
        "［＃変わり者］",
        "［＃夕食］",
        "［＃大瀧菊子］",
        "［＃大熊信行］",
        "［＃娘婿の永田徹郎海軍大尉］",
        "［＃宇陀児］",
        "［＃官僚主義］",
        "［＃宣伝ビラ］",
        "［＃寿江］",
        "［＃射撃］",
        "［＃小杉放庵］",
        "［＃岡田シヅ］",
        "［＃庭園］",
        "［＃感情の陰謀］",
        "［＃憤怒］",
        "［＃戯曲「赤藍色の島」］",
        "［＃手塚英孝、てっちゃん］",
        "［＃手塚英孝］",
        "［＃改訂］",
        "［＃旅行案内書］",
        "［＃映画館名、「穂」］",
        "［＃晴彦］",
        "［＃朝永良太］",
        "［＃朝永］",
        "［＃木村毅］",
        "［＃本田道之］",
        "［＃本間久子］",
        "［＃根津嘉一郎］",
        "［＃正男］",
        "［＃殺虫剤名］",
        "［＃江井、中條家の運転手］",
        "［＃洋装店］",
        "［＃洗面器］",
        "［＃流血］",
        "［＃湯浅アサ］",
        "［＃湯浅善吉］",
        "［＃湯浅芳子］",
        "［＃湯浅誠三郎］",
        "［＃湯浅貞雄］",
        "［＃湿布］",
        "［＃準］",
        "［＃演説、報告］",
        "［＃熱狂的］",
        "［＃現代詩への夕べ］",
        "［＃田村俊子］",
        "［＃百合子の実家］",
        "［＃百合子の愛称］",
        "［＃神沢フミ］",
        "［＃神近市子］",
        "［＃福地源一郎］",
        "［＃窪川稲子の家のお手伝い］",
        "［＃竹内栖鳳］",
        "［＃竹村書房］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "residual source-label/name notes should remain raw-preserved, not unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("source.reviewed_residual_command")
            .map(|count| count.occurrences),
        Some(59)
    );
}

#[test]
fn source_inventory_classifies_final_reviewed_residual_command_batch() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃「ヤンに傍点］",
        "［＃ここで字下げ終わり」］",
        "［＃波罫線］",
        "［＃「七夕」「真つすぐな街」は自由律俳句］",
        "［＃「郷－即のへん」、232-1］",
        "［＃萩原喜一郎、隣家］",
        "［＃灯火管制。夜間、敵機の来襲に備えて、灯りを遮ったり落としたりすこと］",
        "［＃用もないのに廊下をうろつき回ること］",
        "［＃引用、終わり］",
        "［＃口語自由詩で、民衆の現実を描こうとした、「民衆派」の詩人］",
        "［＃移動演劇隊桜隊。広島滞在中、原爆に遭う］",
        "［＃葵］",
        "［＃美治郎］",
        "［＃超国家主義団体］",
        "［＃謙］",
        "［＃高太郎］",
        "［＃以降の「――」で始まる通信文の2行目以降は2字下げ］",
        "［＃１９字下げて］",
        "［＃括弧内は「染付」と「赤繪」の二行になっている］",
        "［＃「諸国における富の分配」の図表のこと］",
        "［＃原文は括弧「〔〕」を使うが、他の所と一致させるため改める］",
        "［＃（）内の文字全てに傍点、ただし読点をのぞく］",
        "［＃ここに「下ニ詳ナリ」という注意書きが入る］",
        "［＃罫線の部分は、「｛」「｝」で括る］",
        "［＃闇汁の図］",
        "［＃便箋右上に花飾り付きのページ数］",
        "［＃ここで字下げ、横書き終わり］",
        "［＃３つの「｛」は１つに繋がる］",
        "［＃黒田鵬心］",
        "［＃野上豊一郎］",
        "［＃貞の末息子］",
        "［＃網野菊］",
        "［＃長谷川如是閑］",
        "［＃青山杉作］",
        "［＃「検察官」］",
        "［＃ロマン・キム］",
        "［＃赤いけし］",
        "［＃通りの名］",
        "［＃「ココ」は手描きの切符の下部の線に結ばれている］",
        "［＃覚え書き、ノート］",
        "［＃〇・四一キログラム］",
        "［＃食後の休息時間］",
        "［＃正しくは「All Quiet on the Western Front」］",
        "［＃血の日曜日］",
        "［＃集団農場］",
        "［＃風呂］",
        "［＃風俗、生活様式］",
        "［＃蓄音機］",
        "［＃トーキー］",
        "［＃野上彌生子］",
        "［＃貴志（康一）］",
        "［＃スフ］",
        "［＃厳寒］",
        "［＃里見勝蔵］",
        "［＃レールが鳴り響く］",
        "［＃この読点不適当］",
        "［＃ここで字下げ終わり　］",
        "［＃上部欄外に「じうもんじカ」］",
        "［＃「嘘の効用」］",
        "［＃（ハヾ）］",
        "［＃（止（波））］",
        "［＃「｝一八〇」はこの後の５行にわたる］",
        "［＃第九章冒頭部分（五六）のこと］",
        "［＃第六章第九段落目以降のこと］",
        "［＃第二章後ろから数えて三段落目のこと］",
        "［＃第二章（二五）の最後の段落のこと］",
        "［＃第二章（二四）のこと］",
        "［＃第一章第五節（二〇）のこと］",
        "［＃（ルヽ）］",
        "［＃（ルヽ）］",
        "［＃「［Ａ］のようにも」は底本では「［Ａ］ようにも」］",
        "［＃金馬と小金馬の対談がここにはいる。］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "final reviewed residual commands should remain raw-preserved, not unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("source.reviewed_residual_command")
            .map(|count| count.occurrences),
        Some(72)
    );
}

#[test]
fn source_inventory_classifies_tail_positioning_and_caption_variants() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃以下の括弧内割注］",
        "［＃天より３２字下げて地より３字上げで］",
        "［＃「序にかえて」全体、天より２字下げ］",
        "［＃ここから最後まで１字下げ］",
        "［＃ここで１字下げ終わり］",
        "［＃ここで文字下げ終わり］",
        "［＃ここから図表下部解説文］",
        "［＃ここで図表下部解説文終わり］",
        "［＃ここで見出し終わり］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known tail positioning and caption variants should not remain unknown"
    );
    for row_id in [
        "warichu.basic",
        "indentation.jisage_block",
        "caption.block",
        "heading.basic",
    ] {
        assert!(
            summary.row_counts.contains_key(row_id),
            "expected source inventory row {row_id}"
        );
    }
}

#[test]
fn source_inventory_classifies_dialogue_indent_and_figure_variants() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃ここから改行１字下げ、折り返して２字下げ］",
        "［＃台詞はすべて、折り返し２行目から、天より１字下げ］",
        "［＃本文の台詞部分は２行目から、その台詞の最後まで天より１字下げ。］",
        "［＃数字は１字下げ、説明文は３字下げ］",
        "［＃図形　□（四角）に内接する◆］",
        "［＃図４、花の絵］",
        "［＃図６入る］",
        "［＃ここに挿し絵入る］",
        "［＃ここに花園の挿絵あり］",
        "［＃ひめだるまの写真（fig45338_01png、横441×縦233）入る］",
        "［＃カット「手書きの図」入る。44-上段］",
        "［＃「ねませ和子よの譜」の表題付きの楽譜入る（略）］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known dialogue-indent and figure source variants should not remain unknown"
    );
    for row_id in ["indentation.burasage", "figure.image_inline"] {
        assert!(
            summary.row_counts.contains_key(row_id),
            "expected source inventory row {row_id}"
        );
    }
}

#[test]
fn source_inventory_classifies_page_center_layout_variants() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃ページの左右中央］",
        "［＃ページの左右中央に］",
        "［＃ここからページの左右中央］",
        "［＃左右中央］",
        "［＃中央寄せ］",
        "［＃直線は中央に配置］",
        "［＃改ページ、ページの左右中央に］",
        "［＃横組みで、ページの上部、左右中央に］",
        "［＃「｝（同時に）」は前２行の中央、下に］",
        "［＃「黄泉の使！　黄泉の使！」は２行の中央、括弧は２行にわたる波括弧］",
        "［＃上記の詞書は、ポイントを下げて中央やや下がり目に］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known page-center layout variants should not remain unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("layout.center_page")
            .map(|count| count.occurrences),
        Some(11)
    );
    assert_eq!(
        summary
            .row_counts
            .get("break.page_line")
            .map(|count| count.occurrences),
        Some(1),
        "combined page-break plus center marker should still count the break"
    );
}

#[test]
fn source_inventory_classifies_layout_and_inline_style_corpus_variants() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃ここから天付き、折り返して１字下げ］",
        "［＃ここから天付き、折り返して２字下げ］",
        "［＃「（訳注）」は行左小書き］",
        "［＃「一」は行左小書き］",
        "［＃「引」は小書き右寄せ］",
        "［＃左に傍線］",
        "［＃左に傍線終わり］",
        "［＃（一）は縦中横］",
        "［＃「Ａｎ」はそれぞれ縦中横、数字は上付き小書き］",
        "［＃「x2」、「y2」、「x2」はそれぞれ縦中横、すべての「2」は上付き小書き］",
        "［＃（十一）は縦中横、「十一」は縦組み］",
        "［＃ここから左から右への横組み］",
        "［＃ここで左から右への横組み終わり］",
        "［＃「田島校長＝０」は横書き］",
        "［＃「Ａ＋Ａ×Ｂ：Ｂ＋Ｂ×Ａ」は横書き］",
        "［＃横書き、「誰」はアクセント（∨）付き］",
        "［＃「近頃流行の、「文学と政治」のことに一寸言及するならば、」は太字］",
        "［＃「Nothing from nothing ever yet was born」の部分はイタリック体］",
        "［＃二つ目、三つ目の「？」は太字］",
        "［＃「-λt」は「e」の上付き］",
        "［＃「一ノ戸」の「ノ」は小書き］",
        "［＃「ココア入リ」は本文より小さいサイズの文字］",
        "［＃「Miss　B. A. Bae.」は斜体字］",
        "［＃「for the reason」はイタリック体］",
        "［＃「P = 0.07693694」は上線（￣）付き］",
        "［＃「v」は下線（_）付き、181-表組2行目］",
        "［＃「est」に下線］",
        "［＃「San」は３０度位右上がり］",
        "［＃「show」は３０度位右上がり］",
        "［＃「一五四・六」の両側に傍線］",
        "［＃「一体だ」に波線］",
        "［＃「月二」は１８０度回転］",
        "［＃「ココノトコロハ三拝九拝シテアル部分」は２日～10日の下に縦中横］",
        "［＃「うらみ思ひ」は、「刈萱の穗にあらはれぬ」と「かな」の間に挟まれるような形でポイントを下げて２行で］",
        "［＃「文字結　青」はポイントを下げる］",
        "［＃「曉臺ノ句　風早し二つにわれてむら千鳥」は「帆柱や二つにわれてむら千鳥」の下にポイントを下げて２行で］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known layout and inline-style corpus variants should not remain unknown"
    );
    for (row_id, expected) in [
        ("indentation.burasage", 2),
        ("decoration.font_size", 12),
        ("decoration.bold_italic", 5),
        ("decoration.bousen", 7),
        ("layout.tcy", 6),
        ("layout.yokogumi", 5),
    ] {
        assert_eq!(
            summary
                .row_counts
                .get(row_id)
                .map(|count| count.occurrences),
            Some(expected),
            "expected source inventory row {row_id}"
        );
    }
}

#[test]
fn source_inventory_classifies_inline_style_variant_spellings() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃「先祖と共に」に傍点◎］",
        "［＃「五月四日」に傍点（白丸）］",
        "［＃四字傍点（白丸）］",
        "［＃「松島」に蛇の目傍点］",
        "［＃「しん」傍点］",
        "［＃「ワット」「ステブンソン」「ヱヂソン」に傍線］",
        "［＃「ヱ」は小文字］",
        "［＃「ヱ」の小文字］",
        "［＃「のうえ」は小さい文字］",
        "［＃「九」ゴシック体］",
        "［＃「受賞図書」「著者」「出版元」はゴシック体］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known inline style spelling variants should not remain unknown"
    );
    for (row_id, expected) in [
        ("decoration.boten", 5),
        ("decoration.bousen", 1),
        ("decoration.font_size", 3),
        ("decoration.bold_italic", 2),
    ] {
        assert_eq!(
            summary
                .row_counts
                .get(row_id)
                .map(|count| count.occurrences),
            Some(expected),
            "expected source inventory row {row_id}"
        );
    }
}

#[test]
fn decoration_font_size_has_typed_representability() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let row = matrix
        .rows()
        .iter()
        .find(|row| row.id == "decoration.font_size")
        .expect("decoration.font_size row");
    let representability = row
        .representability
        .as_ref()
        .expect("decoration.font_size needs a reviewed representability cell");

    assert_eq!(representability.status, RepresentabilityStatus::Typed);
    assert!(representability.raw_fallback);
    assert!(
        representability
            .aat_nodes
            .iter()
            .any(|node| node == "font_size")
    );
    assert!(
        row.tei_projection.contains("hi"),
        "font-size source markers need a TEI P5 hi projection"
    );
}

#[test]
fn source_inventory_classifies_bold_italic_corpus_closing_markers() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = ["［＃ここで太字終わり］", "［＃ここで斜体終わり］"].join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known bold/italic corpus closing markers should not remain unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("decoration.bold_italic")
            .map(|count| count.occurrences),
        Some(2)
    );
}

#[test]
fn decoration_bold_italic_has_typed_representability() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let row = matrix
        .rows()
        .iter()
        .find(|row| row.id == "decoration.bold_italic")
        .expect("decoration.bold_italic row");
    let representability = row
        .representability
        .as_ref()
        .expect("decoration.bold_italic needs a reviewed representability cell");

    assert_eq!(representability.status, RepresentabilityStatus::Typed);
    assert!(representability.raw_fallback);
    assert!(
        representability
            .aat_nodes
            .iter()
            .any(|node| node == "style")
    );
    assert!(
        row.tei_projection.contains("bold") && row.tei_projection.contains("italic"),
        "bold/italic source markers need TEI P5 hi rend projections"
    );
}

#[test]
fn source_inventory_classifies_keigakomi_block_markers() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃罫囲み］",
        "［＃罫囲み終わり］",
        "［＃ここから罫囲み］",
        "［＃ここで罫囲み終わり］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known keigakomi corpus markers should not remain unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("decoration.keigakomi")
            .map(|count| count.occurrences),
        Some(4)
    );
}

#[test]
fn decoration_keigakomi_has_typed_representability() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let row = matrix
        .rows()
        .iter()
        .find(|row| row.id == "decoration.keigakomi")
        .expect("decoration.keigakomi row");
    let representability = row
        .representability
        .as_ref()
        .expect("decoration.keigakomi needs a reviewed representability cell");

    assert_eq!(representability.status, RepresentabilityStatus::Typed);
    assert!(representability.raw_fallback);
    assert!(
        representability
            .aat_nodes
            .iter()
            .any(|node| node == "keigakomi")
    );
    assert!(
        row.tei_projection.contains("keigakomi"),
        "keigakomi source markers need a TEI projection preserving the ruled-box intent"
    );
}

#[test]
fn source_inventory_classifies_tcy_block_markers() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃縦中横］",
        "［＃縦中横終わり］",
        "［＃ここで縦中横終わり］",
    ]
    .join("\n");
    let summary = inventory_document("fixture", &source, &patterns);

    assert_eq!(
        summary.unknown_examples,
        [],
        "known tcy block corpus variants should not remain unknown"
    );
    assert_eq!(
        summary
            .row_counts
            .get("layout.tcy")
            .map(|count| count.occurrences),
        Some(3)
    );
}

#[test]
fn layout_tcy_has_typed_representability() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let row = matrix
        .rows()
        .iter()
        .find(|row| row.id == "layout.tcy")
        .expect("layout.tcy row");
    let representability = row
        .representability
        .as_ref()
        .expect("layout.tcy needs a reviewed representability cell");

    assert_eq!(representability.status, RepresentabilityStatus::Typed);
    assert!(representability.raw_fallback);
    assert!(representability.aat_nodes.iter().any(|node| node == "tcy"));
    assert!(
        row.tei_projection.contains("tcy"),
        "tcy source markers need a TEI projection preserving the tcy layout intent"
    );
}

#[test]
fn forbidden_combinations_rejected() {
    use std::collections::BTreeMap;
    let mut parsers = BTreeMap::new();
    parsers.insert(
        "fake".to_string(),
        ParserCell {
            recognition: Recognition::Aborts,
            evidence: String::new(),
            notes: String::new(),
        },
    );
    let mut adapters = BTreeMap::new();
    adapters.insert(
        "fake".to_string(),
        AdapterCell {
            aat_fidelity: RowAatFidelity::Preserved,
            evidence: String::new(),
            notes: String::new(),
        },
    );
    let row = ab_coverage::matrix::Row {
        id: "test.row".into(),
        priority: 1,
        category: "test".into(),
        feature_keys: vec![],
        reference_sources: vec![],
        source_examples: vec![],
        source_patterns: vec![],
        ir_nodes: vec![],
        aat_nodes: vec![],
        tei_projection: "fixture-tei".to_owned(),
        plaintext_projection: String::new(),
        comparison_projection: String::new(),
        validation_properties: vec![],
        adapter_expectations: vec![],
        oracle_cases: vec![],
        status: ab_coverage::RowStatus::NeedsResearch,
        status_reason: String::new(),
        parsers,
        adapters,
        representability: None,
        corpus_prevalence: Some(CorpusPrevalence {
            works_with_feature: 0,
            total_occurrences: 0,
            detector_id: String::new(),
            coverage_basis: CoverageBasis::NotRun,
            sample_works: vec![],
        }),
    };
    let dummy = make_matrix(vec![row]);
    let opts = ValidationOptions {
        allow_unknown: false,
        required_keys: &[],
    };
    let errors = SchemaValidator::validate(&dummy, opts);
    assert!(
        errors
            .iter()
            .any(|e| e.message.contains("forbidden combination")),
        "expected forbidden-combination error, got: {:?}",
        errors
    );
}

#[test]
fn schema_matrix_parses_representability_cell() {
    let matrix = matrix_from_toml_str(
        "representability-parse",
        r#"
[[syntax]]
id = "fixture.typed"
priority = 1
category = "fixture"
feature_keys = []
reference_sources = []
source_examples = []
source_patterns = []
ir_nodes = []
aat_nodes = ["ruby"]
tei_projection = "ruby/rb/rt"
plaintext_projection = ""
comparison_projection = ""
validation_properties = []
adapter_expectations = []
status = "needs_research"
status_reason = "fixture"

[syntax.representability]
source_inventory_row = "fixture.typed"
status = "typed"
aat_nodes = ["ruby"]
raw_fallback = true
evidence = "fixture"
notes = "fixture"
"#,
    );

    assert_eq!(matrix.rows()[0].id, "fixture.typed");
    let representability = matrix.rows()[0]
        .representability
        .as_ref()
        .expect("representability cell");
    assert_eq!(representability.source_inventory_row, "fixture.typed");
    assert_eq!(representability.status, RepresentabilityStatus::Typed);
    assert_eq!(representability.aat_nodes, ["ruby"]);
    assert!(representability.raw_fallback);
    assert_eq!(representability.evidence, "fixture");
    assert_eq!(representability.notes, "fixture");
}

#[test]
fn schema_matrix_rejects_represented_source_row_without_tei_projection() {
    let matrix = matrix_from_toml_str(
        "representability-without-tei-projection",
        r#"
[[syntax]]
id = "fixture.typed_without_tei"
priority = 1
category = "fixture"
feature_keys = []
reference_sources = []
source_examples = []
source_patterns = []
ir_nodes = []
aat_nodes = ["ruby"]
tei_projection = ""
plaintext_projection = ""
comparison_projection = ""
validation_properties = []
adapter_expectations = []
status = "needs_research"
status_reason = "fixture"

[syntax.representability]
source_inventory_row = "fixture.typed_without_tei"
status = "typed"
aat_nodes = ["ruby"]
raw_fallback = true
"#,
    );
    let errors = SchemaValidator::validate(&matrix, ValidationOptions::lenient());

    assert!(
        errors.iter().any(|e| e.message.contains("tei_projection")),
        "expected tei_projection error, got: {:?}",
        errors
    );
}

#[test]
fn schema_matrix_rejects_typed_representability_without_aat_nodes() {
    let matrix = matrix_from_toml_str(
        "representability-typed-without-aat",
        r#"
[[syntax]]
id = "fixture.typed_without_aat"
priority = 1
category = "fixture"
feature_keys = []
reference_sources = []
source_examples = []
source_patterns = []
ir_nodes = []
aat_nodes = []
tei_projection = ""
plaintext_projection = ""
comparison_projection = ""
validation_properties = []
adapter_expectations = []
status = "needs_research"
status_reason = "fixture"

[syntax.representability]
source_inventory_row = "fixture.typed_without_aat"
status = "typed"
aat_nodes = []
raw_fallback = true
"#,
    );
    let errors = SchemaValidator::validate(&matrix, ValidationOptions::lenient());

    assert!(
        errors
            .iter()
            .any(|e| e.message.contains("typed") && e.message.contains("aat_nodes")),
        "expected typed/aat_nodes error, got: {:?}",
        errors
    );
}

#[test]
fn schema_matrix_rejects_unsupported_representability_with_raw_fallback() {
    let matrix = matrix_from_toml_str(
        "representability-unsupported-raw-fallback",
        r#"
[[syntax]]
id = "fixture.unsupported"
priority = 1
category = "fixture"
feature_keys = []
reference_sources = []
source_examples = []
source_patterns = []
ir_nodes = []
aat_nodes = []
tei_projection = ""
plaintext_projection = ""
comparison_projection = ""
validation_properties = []
adapter_expectations = []
status = "needs_research"
status_reason = "fixture"

[syntax.representability]
source_inventory_row = "fixture.unsupported"
status = "unsupported"
raw_fallback = true
"#,
    );
    let errors = SchemaValidator::validate(&matrix, ValidationOptions::lenient());

    assert!(
        errors.iter().any(|e| e.message.contains("raw_fallback")),
        "expected unsupported/raw_fallback error, got: {:?}",
        errors
    );
}

#[test]
fn schema_matrix_rejects_empty_source_inventory_row() {
    let matrix = matrix_from_toml_str(
        "representability-empty-source-row",
        r#"
[[syntax]]
id = "fixture.empty_source_row"
priority = 1
category = "fixture"
feature_keys = []
reference_sources = []
source_examples = []
source_patterns = []
ir_nodes = []
aat_nodes = []
tei_projection = ""
plaintext_projection = ""
comparison_projection = ""
validation_properties = []
adapter_expectations = []
status = "needs_research"
status_reason = "fixture"

[syntax.representability]
source_inventory_row = ""
status = "needs_research"
raw_fallback = true
"#,
    );
    let errors = SchemaValidator::validate(&matrix, ValidationOptions::lenient());

    assert!(
        errors
            .iter()
            .any(|e| e.message.contains("source_inventory_row")),
        "expected source_inventory_row error, got: {:?}",
        errors
    );
}

#[test]
fn schema_matrix_rejects_unknown_source_inventory_row() {
    let matrix = matrix_from_toml_str(
        "representability-unknown-source-row",
        r#"
[[syntax]]
id = "fixture.unknown_source_row"
priority = 1
category = "fixture"
feature_keys = []
reference_sources = []
source_examples = []
source_patterns = []
ir_nodes = []
aat_nodes = []
tei_projection = ""
plaintext_projection = ""
comparison_projection = ""
validation_properties = []
adapter_expectations = []
status = "needs_research"
status_reason = "fixture"

[syntax.representability]
source_inventory_row = "fixture.missing"
status = "needs_research"
raw_fallback = true
"#,
    );
    let errors = SchemaValidator::validate(&matrix, ValidationOptions::lenient());

    assert!(
        errors.iter().any(|e| e.message.contains("fixture.missing")),
        "expected missing source row error, got: {:?}",
        errors
    );
}

#[test]
fn schema_matrix_does_not_infer_representability_from_corpus_prevalence() {
    let matrix = matrix_from_toml_str(
        "representability-no-prevalence-inference",
        r#"
[[syntax]]
id = "fixture.prevalent_without_representability"
priority = 1
category = "fixture"
feature_keys = []
reference_sources = []
source_examples = []
source_patterns = []
ir_nodes = []
aat_nodes = []
tei_projection = ""
plaintext_projection = ""
comparison_projection = ""
validation_properties = []
adapter_expectations = []
status = "needs_research"
status_reason = "fixture"

[syntax.corpus_prevalence]
works_with_feature = 1
total_occurrences = 42
detector_id = "fixture_prevalent_without_representability"
coverage_basis = "full_corpus"
sample_works = ["work"]
"#,
    );
    let errors = SchemaValidator::validate(&matrix, ValidationOptions::lenient());

    assert!(
        errors
            .iter()
            .all(|e| !e.message.contains("representability")),
        "corpus_prevalence must not imply representability requirements, got: {:?}",
        errors
    );
}

fn make_matrix(rows: Vec<ab_coverage::matrix::Row>) -> CoverageMatrix {
    // Round-trip through TOML to avoid exposing private constructors.
    let mut buf = String::new();
    for row in rows {
        buf.push_str(&serialize_row(&row));
    }
    let path = std::env::temp_dir().join(format!("ab-coverage-test-{}.toml", std::process::id()));
    std::fs::write(&path, buf).unwrap();
    let matrix = CoverageMatrix::from_toml(&path).unwrap();
    let _ = std::fs::remove_file(&path);
    matrix
}

fn matrix_from_toml_str(name: &str, toml: &str) -> CoverageMatrix {
    let path = std::env::temp_dir().join(format!(
        "ab-coverage-test-{}-{name}.toml",
        std::process::id()
    ));
    std::fs::write(&path, toml).unwrap();
    let matrix = CoverageMatrix::from_toml(&path).unwrap();
    let _ = std::fs::remove_file(&path);
    matrix
}

fn serialize_row(row: &ab_coverage::matrix::Row) -> String {
    let mut out = String::from("[[syntax]]\n");
    out.push_str(&format!("id = {:?}\n", row.id));
    out.push_str(&format!("priority = {}\n", row.priority));
    out.push_str(&format!("category = {:?}\n", row.category));
    out.push_str("feature_keys = []\n");
    out.push_str("reference_sources = []\n");
    out.push_str("source_examples = []\n");
    out.push_str("source_patterns = []\n");
    out.push_str("ir_nodes = []\n");
    out.push_str("aat_nodes = []\n");
    out.push_str("tei_projection = \"\"\n");
    out.push_str("plaintext_projection = \"\"\n");
    out.push_str("comparison_projection = \"\"\n");
    out.push_str("validation_properties = []\n");
    out.push_str("adapter_expectations = []\n");
    out.push_str("oracle_cases = []\n");
    let status = match row.status {
        ab_coverage::RowStatus::Covered => "covered",
        ab_coverage::RowStatus::Partial => "partial",
        ab_coverage::RowStatus::NotModeled => "not_modeled",
        ab_coverage::RowStatus::NeedsResearch => "needs_research",
    };
    out.push_str(&format!("status = \"{status}\"\n"));
    out.push_str(&format!("status_reason = {:?}\n", row.status_reason));
    for (id, cell) in &row.parsers {
        out.push_str(&format!("\n[syntax.parsers.\"{id}\"]\n"));
        out.push_str(&format!(
            "recognition = \"{}\"\n",
            cell.recognition.as_str()
        ));
        out.push_str(&format!("evidence = {:?}\n", cell.evidence));
        out.push_str(&format!("notes = {:?}\n", cell.notes));
    }
    for (id, cell) in &row.adapters {
        out.push_str(&format!("\n[syntax.adapters.\"{id}\"]\n"));
        out.push_str(&format!(
            "aat_fidelity = \"{}\"\n",
            cell.aat_fidelity.as_str()
        ));
        out.push_str(&format!("evidence = {:?}\n", cell.evidence));
        out.push_str(&format!("notes = {:?}\n", cell.notes));
    }
    if let Some(cell) = &row.representability {
        out.push_str("\n[syntax.representability]\n");
        out.push_str(&format!(
            "source_inventory_row = {:?}\n",
            cell.source_inventory_row
        ));
        out.push_str(&format!("status = {:?}\n", cell.status.as_str()));
        out.push_str(&format!("aat_nodes = {:?}\n", cell.aat_nodes));
        out.push_str(&format!("raw_fallback = {}\n", cell.raw_fallback));
        out.push_str(&format!("evidence = {:?}\n", cell.evidence));
        out.push_str(&format!("notes = {:?}\n", cell.notes));
    }
    if let Some(prev) = &row.corpus_prevalence {
        out.push_str("\n[syntax.corpus_prevalence]\n");
        out.push_str(&format!(
            "works_with_feature = {}\n",
            prev.works_with_feature
        ));
        out.push_str(&format!("total_occurrences = {}\n", prev.total_occurrences));
        out.push_str(&format!("detector_id = {:?}\n", prev.detector_id));
        let basis = match prev.coverage_basis {
            CoverageBasis::FullCorpus => "full_corpus",
            CoverageBasis::StratifiedSample => "stratified_sample",
            CoverageBasis::NotRun => "not_run",
        };
        out.push_str(&format!("coverage_basis = \"{basis}\"\n"));
        out.push_str("sample_works = []\n");
    }
    out
}

fn join_errors(errors: &[RowError]) -> String {
    errors
        .iter()
        .map(|e| format!("  - {e}"))
        .collect::<Vec<_>>()
        .join("\n")
}
