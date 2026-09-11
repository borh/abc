//! Supplied section locators annotate printed page labels without replacing them.

use std::path::Path;

use ab_aat_to_parser_ir::{ConversionOptions, ConversionRequest, MappingDocument, SchemaSet};
use serde_json::Value;

fn convert(body: &str) -> Value {
    let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: serde_json::from_slice(&ab_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
            .unwrap(),
        mapping: MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v2.json"))
            .unwrap(),
        schemas: SchemaSet::for_aat_version(2).unwrap(),
        options: ConversionOptions::default(),
    })
    .unwrap()
    .parser_ir
}

#[test]
fn source_page_reference_keeps_target_and_supplied_locator() {
    for (page, locator) in [
        ("三五頁", "「須佐の男の神」の「穀物の種」"),
        ("二三〇頁", "「大國主の神」"),
        (
            "三六六ページ",
            "「清寧天皇・顯宗天皇・仁賢天皇」の「シジムの新築祝い」",
        ),
    ] {
        let marker = format!("［＃「{page}」は{locator}］");
        let ir = convert(&format!("前{page}{marker}にある。"));
        let node = ir["nodes"]
            .as_array()
            .unwrap()
            .iter()
            .find(|n| n["type"] == "annotated-text")
            .expect("target-associated reference");
        assert_eq!(node["text"], page);
        assert_eq!(node["note_kind"], "cross-reference");
        assert_eq!(node["annotation_children"][0]["text"], locator);
        assert_eq!(
            node["annotation_children"][0]["span"]["coordinate_system"],
            "annotation_utf8"
        );
        assert_eq!(
            node["source_span"]["start"],
            "題\n作者\n\n前".len() + page.len()
        );
        assert_eq!(ir["interpretation_problems"], serde_json::json!([]));
    }
}

#[test]
fn missing_page_target_and_nonreference_suffix_remain_explicit() {
    for body in [
        "本文［＃「三五頁」は「章」］",
        "三五頁［＃「三五頁」は「章」、未知の指定］",
        "字［＃「字」は「章」］",
    ] {
        let ir = convert(body);
        assert!(!ir["interpretation_problems"].as_array().unwrap().is_empty());
        assert!(
            !ir["nodes"]
                .as_array()
                .unwrap()
                .iter()
                .any(|n| n["note_kind"] == "cross-reference")
        );
    }
}
