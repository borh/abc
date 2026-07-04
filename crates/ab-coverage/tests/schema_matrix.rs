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
fn source_inventory_classifies_kunten_source_note_variants() {
    let matrix = CoverageMatrix::from_toml(&matrix_path()).expect("load matrix");
    let patterns = patterns_from_rows(matrix.rows());
    let source = [
        "［＃「レ」は返り点］",
        "［＃「」内の「レ」は返り点］",
        "［＃「」内の「一二」は返り点］",
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
        Some(3)
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
        "［＃白三角傍点］",
        "［＃白三角傍点終わり］",
        "［＃二重傍線終わり］",
        "［＃「〃」は横組み］",
        "［＃「？！」は横一列］",
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
        Some(6)
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
        Some(5)
    );
    assert_eq!(
        summary
            .row_counts
            .get("decoration.keigakomi")
            .map(|count| count.occurrences),
        Some(6)
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
        Some(9)
    );
    assert_eq!(
        summary
            .row_counts
            .get("structure.table")
            .map(|count| count.occurrences),
        Some(6)
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
        Some(16)
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
        Some(13)
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
        Some(13)
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
        Some(19)
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
        Some(11)
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
        Some(8)
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
        "［＃「田島校長＝０」は横書き］",
        "［＃「Ａ＋Ａ×Ｂ：Ｂ＋Ｂ×Ａ」は横書き］",
        "［＃横書き、「誰」はアクセント（∨）付き］",
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
        ("decoration.font_size", 4),
        ("decoration.bousen", 2),
        ("layout.tcy", 2),
        ("layout.yokogumi", 3),
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
