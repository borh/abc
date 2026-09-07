//! Source paragraphs survive independently scoped typography.

use std::path::Path;

use ab_aat_to_parser_ir::{ConversionOptions, ConversionRequest, MappingDocument, SchemaSet};
use serde_json::Value;

fn convert(body: &str) -> Value {
    let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: serde_json::from_slice(
            &ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap(),
        )
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
fn scope_ends_inside_the_second_source_paragraph() {
    let ir =
        convert("　［＃ここから斜体］First speech.\n\n　Second speech.［＃ここで斜体終わり］＊");
    let body: Vec<_> = ir["paragraphs"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|p| p["role"] == "body")
        .collect();
    assert_eq!(body.len(), 2, "{ir}");
    let scope = &ir["layout_blocks"][0];
    assert_eq!(ir["layout_blocks"].as_array().unwrap().len(), 1);
    assert!(scope["source_span"]["start"].is_u64());
    assert!(
        scope["node_range"]["end"].as_u64().unwrap()
            < body[1]["node_range"]["end"].as_u64().unwrap()
    );
    assert_eq!(ir["interpretation_problems"], serde_json::json!([]));
}

#[test]
fn a_heading_prevents_merging_same_line_body_fragments() {
    let ir = convert(
        "前［＃ここから斜体］甲\n［＃中見出し］章［＃中見出し終わり］後\n乙［＃ここで斜体終わり］外",
    );
    assert_eq!(ir["layout_blocks"].as_array().unwrap().len(), 1);
    assert_eq!(
        ir["nodes"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|node| node["type"] == "heading")
            .count(),
        1
    );
    let heading_index = ir["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .position(|node| node["type"] == "heading")
        .unwrap();
    for paragraph in ir["paragraphs"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|p| p["role"] == "body")
    {
        let start = usize::try_from(paragraph["node_range"]["start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(paragraph["node_range"]["end"].as_u64().unwrap()).unwrap();
        assert!(!(start <= heading_index && heading_index < end));
    }
    assert_eq!(ir["interpretation_problems"], serde_json::json!([]));
}

#[test]
fn stripped_marker_line_does_not_supply_the_next_paragraph_identity() {
    let ir = convert("　［＃ここから斜体］\n本文\n［＃ここで斜体終わり］後");
    let scope = &ir["layout_blocks"][0];
    let start = scope["node_range"]["start"].as_u64().unwrap();
    let paragraph = ir["paragraphs"]
        .as_array()
        .unwrap()
        .iter()
        .find(|p| p["node_range"]["start"].as_u64() == Some(start))
        .unwrap();
    assert_eq!(paragraph["source_span"]["line"], 5);
}
