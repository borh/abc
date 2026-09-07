//! Source lines, explicit breaks and inline headings have independent boundaries.

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
fn inline_heading_does_not_terminate_line_indentation() {
    let ir = convert("［＃１字下げ］［＃同行大見出し］優しき歌［＃同行大見出し終わり］叢書");
    assert_eq!(ir["layout_blocks"][0]["node_range"],ir["paragraphs"][0]["node_range"]);
}

#[test]
fn explicit_break_inside_warichu_does_not_terminate_line_alignment() {
    let ir = convert("［＃地から１字上げ］［＃ここから割り注］甲［＃改行］乙［＃ここで割り注終わり］作者");
    assert_eq!(ir["layout_blocks"][0]["node_range"],ir["paragraphs"][0]["node_range"]);
    assert_eq!(ir["interpretation_problems"],serde_json::json!([]));
}

#[test]
fn removed_boundary_newline_does_not_merge_following_blank_line_into_scope() {
    let ir = convert("［＃ここから２字下げ］\n本文［＃ここで字下げ終わり］\n\n外");
    assert_eq!(ir["layout_blocks"][0]["node_range"],ir["paragraphs"][0]["node_range"]);
}

#[test]
fn explicit_breaks_at_scope_edges_are_not_boundary_whitespace() {
    let ir = convert("［＃ここから２字下げ］［＃改行］甲［＃改行］［＃ここで字下げ終わり］");
    assert_eq!(ir["nodes"].as_array().unwrap().iter().filter(|n| n["type"] == "line-break").count(), 2);
    assert_eq!(ir["nodes"].as_array().unwrap().iter().filter(|n| n["type"] == "text").filter_map(|n| n["text"].as_str()).collect::<String>(), "甲");
}
