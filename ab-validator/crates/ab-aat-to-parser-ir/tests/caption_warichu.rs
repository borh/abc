//! Caption and interlinear-note roles do not imply a figure relationship.
use std::path::Path;

use ab_aat_to_parser_ir::{ConversionOptions, ConversionRequest, MappingDocument, SchemaSet};
use serde_json::Value;

fn convert(body: &str) -> Value {
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
    let aat =
        serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
            .unwrap();
    ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping: MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v2.json"))
            .unwrap(),
        schemas: SchemaSet::for_aat_version(&repo, None, 2).unwrap(),
        options: ConversionOptions::default(),
    })
    .unwrap()
    .parser_ir
}

#[test]
fn inline_caption_retains_ruby_without_inventing_a_figure() {
    let ir = convert("前［＃キャプション］漢字《かんじ》［＃キャプション終わり］後。");
    let caption = ir["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|n| n["type"] == "caption")
        .expect("typed caption");
    assert_eq!(caption["text"], "漢字");
    assert_eq!(caption["inline_children"][0]["type"], "ruby");
    assert!(caption.get("target").is_none());
    assert_eq!(ir["interpretation_problems"], serde_json::json!([]));
}

#[test]
fn generic_warichu_owns_its_exact_native_scope() {
    let ir = convert("前［＃ここから割り注］訳者注、漢字《かんじ》［＃ここで割り注終わり］後。");
    let note = ir["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|n| n["type"] == "warichu")
        .expect("typed warichu");
    assert_eq!(note["text"], "訳者注、漢字");
    assert_eq!(ir["interpretation_problems"], serde_json::json!([]));
    assert_eq!(
        ir["interpretation_facts"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|f| f["kind"] == "warichu")
            .count(),
        2
    );
}

#[test]
fn multiline_roles_preserve_two_source_paragraphs() {
    for (open, close, role) in [
        (
            "［＃ここからキャプション］",
            "［＃ここでキャプション終わり］",
            "caption",
        ),
        (
            "［＃ここから割り注］",
            "［＃ここで割り注終わり］",
            "warichu",
        ),
    ] {
        let ir = convert(&format!("前。\n{open}\n第一。\n第二。\n{close}\n後。"));
        let scopes = ir["layout_blocks"].as_array().unwrap();
        let scope = scopes
            .iter()
            .find(|s| s["role"] == role)
            .expect("typed block role");
        assert_eq!(
            scope["paragraph_range"]["end"].as_u64().unwrap()
                - scope["paragraph_range"]["start"].as_u64().unwrap(),
            2
        );
        assert_eq!(ir["interpretation_problems"], serde_json::json!([]));
    }
}

#[test]
fn retrospective_caption_retains_whole_rich_target() {
    let ir = convert("前｜漢字《かんじ》［＃「漢字」はキャプション］後。");
    let caption = ir["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|n| n["type"] == "caption")
        .expect("typed caption");
    assert_eq!(caption["text"], "漢字");
    assert_eq!(caption["inline_children"][0]["type"], "ruby");
}
