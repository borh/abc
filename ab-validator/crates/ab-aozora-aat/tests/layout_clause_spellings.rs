//! Source-supplied layout spellings share established native geometry.

use ab_aozora_aat::aat_json_from_bytes;
use serde_json::{Value, json};

#[test]
fn supplied_closer_aliases_retain_exact_scope_markers() {
    for close in ["［＃字下げ終わり］", "［＃ここで字下げおわり］"] {
        let source = format!("［＃ここから３字下げ］\n本文\n{close}\n後");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        assert_eq!(aat["meta"]["parse_complete"], true, "{aat}");
        let block = &aat["blocks"][0];
        assert_eq!(block["kind"], "layout_block", "{aat}");
        let span = &block["interpretation_marker_spans"][1];
        let start = usize::try_from(span["byte_start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(span["byte_end"].as_u64().unwrap()).unwrap();
        assert_eq!(&source[start..end], close);
        assert_eq!(aat["blocks"][1]["content"][0]["value"], "後");
    }
}

#[test]
fn independent_clause_aliases_use_existing_axes() {
    for (clause, fields) in [
        ("文章は横組み", json!({"direction":"horizontal"})),
        ("横組みで", json!({"direction":"horizontal"})),
        ("１行７字", json!({"width":7})),
    ] {
        let source = format!("［＃ここから３字下げ、{clause}］\n本文\n［＃ここで字下げ終わり］");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let block = &aat["blocks"][0];
        assert_eq!(block["indent"], 3, "{aat}");
        for (key, expected) in fields.as_object().unwrap() {
            assert_eq!(&block[key], expected, "{aat}");
        }
        assert!(block.get("line_count").is_none(), "{aat}");
        assert!(
            !block.to_string().contains("interpretation_problem"),
            "{aat}"
        );
    }
}
