//! Supplied line geometry remains independent of its enclosed text.

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
fn supplied_geometry_retains_independent_dimensions_and_body_text() {
    let ir = convert(
        "［＃ここから６字下げ、折り返して７字下げ、２１字詰め］\n本文\n［＃ここで字下げ終わり］",
    );
    let scope = &ir["layout_blocks"][0];
    assert_eq!(scope["indent"], 6);
    assert_eq!(scope["continuation_indent"], 7);
    assert_eq!(scope["width"], 21);
    assert!(
        ir["nodes"]
            .as_array()
            .unwrap()
            .iter()
            .any(|node| node["text"] == "本文")
    );
    assert!(ir["interpretation_problems"].as_array().unwrap().is_empty());
}

#[test]
fn page_placement_and_nested_indentation_are_distinct_scopes() {
    let ir = convert(
        "［＃ここからページの左右中央］\n［＃ここから３字下げ］\n扉\n［＃ここで字下げ終わり］\n［＃改丁］\n次頁",
    );
    let scopes = ir["layout_blocks"].as_array().unwrap();
    assert_eq!(scopes.len(), 2);
    assert_eq!(scopes[0]["indent"], 3);
    assert_eq!(scopes[1]["page_placement"], "center");
    assert!(scopes[1].get("align").is_none());
    assert!(scopes[1].get("indent").is_none());
    assert!(
        scopes[1]["node_range"]["end"].as_u64().unwrap()
            < ir["nodes"].as_array().unwrap().len() as u64
    );
}
