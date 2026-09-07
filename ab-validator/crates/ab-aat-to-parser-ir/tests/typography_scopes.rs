//! Native typography extents preserve independent source paragraphs.
use ab_aat_to_parser_ir::{ConversionOptions, ConversionRequest, MappingDocument, SchemaSet};
use serde_json::Value;
use std::path::Path;

fn convert(source: &str) -> Value {
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
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
fn typography_scope_retains_paragraphs_and_native_marker_ownership() {
    let ir = convert(
        "題\n作者\n\n前。\n［＃ここから１段階小さな文字］\n第一。\n第二。\n［＃ここで小さな文字終わり］\n後。\n\n底本：本\n",
    );
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
        scopes[0]["node_range"],
        serde_json::json!({"start":ir["paragraphs"][1]["node_range"]["start"],
                          "end":ir["paragraphs"][2]["node_range"]["end"]})
    );
    assert_eq!(ir["interpretation_problems"], serde_json::json!([]));
}

#[test]
fn source_heading_remains_inside_typography_node_extent() {
    let ir = convert(
        "題\n作者\n\n前\n［＃ここから１段階小さな文字］\n第一\n［＃中見出し］章《しょう》［＃中見出し終わり］\n第二\n［＃ここで小さな文字終わり］\n後\n\n底本：本\n",
    );
    let nodes = ir["nodes"].as_array().unwrap();
    let heading = nodes
        .iter()
        .position(|node| node["type"] == "heading")
        .unwrap();
    let range = &ir["layout_blocks"][0]["node_range"];
    let start = usize::try_from(range["start"].as_u64().unwrap()).unwrap();
    let end = usize::try_from(range["end"].as_u64().unwrap()).unwrap();
    assert!(start < heading && heading < end);
    assert_eq!(nodes[heading]["style"], "normal");
    assert_eq!(nodes[heading]["inline_children"][0]["type"], "ruby");
    assert_eq!(ir["interpretation_problems"], serde_json::json!([]));
}
