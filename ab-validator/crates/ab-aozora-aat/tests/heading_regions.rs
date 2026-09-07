//! Heading ranges preserve native presentation and exact marker ownership.

use ab_aozora_aat::aat_json_from_bytes;
use serde_json::Value;

fn document(body: &str) -> Value {
    let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
    serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap()
}

#[test]
fn source_heading_styles_preserve_rich_content_and_marker_facts() {
    for (prefix, style) in [("", "normal"), ("同行", "dogyo"), ("窓", "mado")] {
        let aat = document(&format!(
            "前［＃{prefix}中見出し］漢字《かんじ》［＃{prefix}中見出し終わり］後"
        ));
        let mut pending = vec![&aat["blocks"]];
        let mut headings = Vec::new();
        while let Some(node) = pending.pop() {
            if node["kind"] == "heading" {
                headings.push(node);
            }
            match node {
                Value::Object(map) => pending.extend(map.values()),
                Value::Array(array) => pending.extend(array),
                _ => {}
            }
        }
        assert_eq!(headings.len(), 1, "{aat}");
        let heading = headings[0];
        assert_eq!(heading["style"], style);
        assert_eq!(heading["level"], 2);
        assert_eq!(heading["content"][0]["kind"], "ruby");
        assert_eq!(
            heading["interpretation_marker_spans"]
                .as_array()
                .unwrap()
                .len(),
            2
        );
        assert_eq!(
            aat["meta"]["interpretation_facts"]
                .as_array()
                .unwrap()
                .iter()
                .filter(|fact| fact["kind"] == "heading")
                .count(),
            2
        );
        if style != "normal" {
            assert_eq!(aat["blocks"][0]["kind"], "paragraph");
            assert_eq!(aat["blocks"][0]["content"][1]["kind"], "heading");
        }
    }
}

#[test]
fn here_from_heading_keeps_size_indentation_and_internal_line_break() {
    for (size, level) in [("大", 1), ("中", 2), ("小", 3)] {
        let aat = document(&format!(
            "［＃３字下げ］［＃ここから{size}見出し］\n一\n二\n［＃ここで見出し終わり］"
        ));
        let heading = &aat["blocks"][0];
        assert_eq!(heading["kind"], "heading");
        assert_eq!(heading["level"], level);
        assert_eq!(heading["indent"], 3);
        let text: String = heading["content"]
            .as_array()
            .unwrap()
            .iter()
            .filter_map(|node| node["value"].as_str())
            .collect();
        assert_eq!(text, "一\n二");
    }
}

#[test]
fn unmatched_heading_remains_source_located_and_uncertain() {
    let aat = document("［＃大見出し］題［＃中見出し終わり］");
    assert!(
        !aat["meta"]["interpretation_facts"]
            .as_array()
            .into_iter()
            .flatten()
            .any(|fact| fact["kind"] == "heading")
    );
    let raw: Vec<_> = aat["blocks"][0]["content"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|node| node["kind"] == "raw")
        .collect();
    assert_eq!(raw.len(), 2);
    for node in raw {
        assert_eq!(
            node["interpretation_problem"]["kind"],
            "uninterpreted-notation"
        );
        assert!(node["span"]["byte_start"].is_u64());
    }
}
