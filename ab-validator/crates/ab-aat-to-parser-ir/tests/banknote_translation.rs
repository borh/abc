//! Source-positioned translations preserve their content independently of omitted images.
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
fn banknote_translation_preserves_seven_lines_and_independent_omission() {
    let body = "前。\n［＃王家の紙幣の図、図省略］\n［＃ここから紙幣の文字の訳文］\n　　国王の名において\n十リーヴル兌換券\n　軍需品代として交付す\n　平和確立とともに償還す\n第三部　第一〇三九〇号\n　　　ストフレー\n　　正教王党軍（欄外に）\n［＃ここで訳文終わり］\n後。";
    let ir = convert(body);
    let scope = &ir["layout_blocks"][0];
    assert_eq!(scope["role"], "translation");
    assert_eq!(scope["source_kind"], "banknote-text");
    assert!(scope.get("relative_placement").is_none());
    let start = scope["node_range"]["start"].as_u64().unwrap();
    let end = scope["node_range"]["end"].as_u64().unwrap();
    let paragraphs: Vec<_> = ir["paragraphs"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|p| {
            p["node_range"]["start"].as_u64().unwrap() >= start
                && p["node_range"]["end"].as_u64().unwrap() <= end
        })
        .collect();
    assert_eq!(paragraphs.len(), 7);
    assert_eq!(
        ir["interpretation_facts"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|f| f["kind"] == "translation")
            .count(),
        2
    );
    assert!(
        ir["nodes"]
            .as_array()
            .unwrap()
            .iter()
            .any(|n| n["type"] == "editor-note" && n["text"] == "王家の紙幣の図、図省略")
    );
    assert_eq!(ir["interpretation_problems"], serde_json::json!([]));
}

#[test]
fn translation_scope_cannot_claim_an_unmatched_boundary() {
    for body in [
        "［＃ここから紙幣の文字の訳文］\n訳文\n［＃ここでキャプション終わり］",
        "［＃ここから別の訳文］\n訳文\n［＃ここで訳文終わり］",
    ] {
        let ir = convert(body);
        assert!(
            !ir["layout_blocks"]
                .as_array()
                .unwrap()
                .iter()
                .any(|s| s["role"] == "translation")
        );
        assert!(
            !ir["interpretation_facts"]
                .as_array()
                .unwrap()
                .iter()
                .any(|f| f["kind"] == "translation")
        );
        assert!(!ir["interpretation_problems"].as_array().unwrap().is_empty());
    }
}
