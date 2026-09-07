//! Native typography extents preserve independent source paragraphs.
use ab_aat_to_parser_ir::{ConversionOptions, ConversionRequest, MappingDocument, SchemaSet};
use serde_json::Value;
use std::path::Path;

#[test]
fn typography_scope_retains_paragraphs_and_native_marker_ownership() {
    let source = "題\n作者\n\n前。\n［＃ここから１段階小さな文字］\n第一。\n第二。\n［＃ここで小さな文字終わり］\n後。\n\n底本：本\n";
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let aat =
        serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
            .unwrap();
    let ir = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping: MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v2.json"))
            .unwrap(),
        schemas: SchemaSet::for_aat_version(&repo, None, 2).unwrap(),
        options: ConversionOptions::default(),
    })
    .unwrap()
    .parser_ir;
    let scopes: Vec<&Value> = ir["layout_blocks"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|v| v.get("typography").is_some())
        .collect();
    assert_eq!(scopes.len(), 1, "{ir}");
    assert_eq!(scopes[0]["typography"]["kind"], "font-size");
    assert_eq!(scopes[0]["typography"]["size_type"], "small");
    assert_eq!(scopes[0]["typography"]["level"], 1);
    assert_eq!(
        scopes[0]["paragraph_range"]["end"].as_u64().unwrap()
            - scopes[0]["paragraph_range"]["start"].as_u64().unwrap(),
        2
    );
    assert_eq!(ir["interpretation_problems"], serde_json::json!([]));
}
