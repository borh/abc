//! Native paired layout scopes retain typography and source paragraph boundaries.

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
        schemas: SchemaSet::for_aat_version(&repo, None, 2).unwrap(),
        options: ConversionOptions::default(),
    })
    .unwrap()
    .parser_ir
}

#[test]
fn paired_frame_and_horizontal_preserve_layout_across_paragraphs() {
    for (open, close, kind) in [
        ("罫囲み", "罫囲み", "keigakomi"),
        ("横組み", "横組み", "yokogumi"),
    ] {
        let ir = convert(&format!(
            "前［＃ここから{open}］甲\n乙［＃ここで{close}終わり］後"
        ));
        assert_eq!(ir["layout_blocks"][0]["typography"]["kind"], kind);
        assert_eq!(
            ir["paragraphs"]
                .as_array()
                .unwrap()
                .iter()
                .filter(|p| p["role"] == "body")
                .count(),
            2
        );
        assert_eq!(ir["interpretation_problems"], serde_json::json!([]));
        assert_eq!(
            ir["interpretation_facts"]
                .as_array()
                .unwrap()
                .iter()
                .filter(|f| f["kind"] == "layout")
                .count(),
            2
        );
    }
}

#[test]
fn unclosed_frame_retains_marker_and_uncertainty() {
    let ir = convert("［＃ここから罫囲み］\n本文");
    assert!(ir["layout_blocks"].as_array().unwrap().is_empty());
    assert!(!ir["interpretation_problems"].as_array().unwrap().is_empty());
}
