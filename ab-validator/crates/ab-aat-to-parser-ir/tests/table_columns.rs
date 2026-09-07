//! Supplied table lines, column counts and explicit milestones remain independent.

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
fn table_retains_source_lines_without_invented_cells() {
    let ir = convert(
        "［＃ここから表］\n人口の表\n年次／出生／死亡\n一七五七年／八一八七八／六九〇五四\n［＃ここで表終わり］",
    );
    assert_eq!(ir["layout_blocks"][0]["role"], "table");
    let text = ir["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .filter_map(|n| n["text"].as_str())
        .collect::<String>();
    assert!(text.contains("年次／出生／死亡"));
    assert_eq!(
        ir["paragraphs"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|p| p["role"] == "body")
            .count(),
        3
    );
    assert_eq!(ir["interpretation_problems"], serde_json::json!([]));
    assert_eq!(
        ir["interpretation_facts"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|f| f["kind"] == "table")
            .count(),
        2
    );
}

#[test]
fn column_count_does_not_invent_breaks_and_explicit_break_stays_inside_paragraph() {
    for (body, expected) in [("甲乙", 0), ("甲［＃改段］乙", 1)] {
        let ir = convert(&format!(
            "［＃ここから２段組み］\n{body}\n［＃ここで段組み終わり］"
        ));
        assert_eq!(ir["layout_blocks"][0]["column_count"], 2);
        assert_eq!(
            ir["nodes"]
                .as_array()
                .unwrap()
                .iter()
                .filter(|n| n["type"] == "column-break")
                .count(),
            expected
        );
        assert_eq!(
            ir["paragraphs"]
                .as_array()
                .unwrap()
                .iter()
                .filter(|p| p["role"] == "body")
                .count(),
            1
        );
        assert_eq!(ir["interpretation_problems"], serde_json::json!([]));
        assert_eq!(
            ir["interpretation_facts"]
                .as_array()
                .unwrap()
                .iter()
                .filter(|f| f["kind"] == "layout-break")
                .count(),
            expected
        );
    }
}

#[test]
fn distinct_source_page_markers_retain_their_own_kind() {
    for (source, marker) in [
        ("改ページ", "page"),
        ("改丁", "kaicho"),
        ("改見開き", "kaimihiraki"),
    ] {
        let ir = convert(&format!("前［＃{source}］後"));
        let node = ir["nodes"]
            .as_array()
            .unwrap()
            .iter()
            .find(|n| n["type"] == "page-break")
            .unwrap();
        assert_eq!(node["marker"], marker);
        assert!(node["source_span"]["start"].is_u64());
        assert_eq!(ir["interpretation_problems"], serde_json::json!([]));
    }
}

#[test]
fn explicit_mismatched_column_count_is_not_certified() {
    let ir = convert("［＃ここから２段組み］\n本文\n［＃ここで３段組み終わり］");
    assert!(ir["layout_blocks"].as_array().unwrap().is_empty());
    assert!(!ir["interpretation_problems"].as_array().unwrap().is_empty());
    assert!(
        ir["interpretation_facts"]
            .as_array()
            .unwrap()
            .iter()
            .all(|f| f["kind"] != "line-layout")
    );
}
