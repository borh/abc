//! A compound indentation closer must match its supplied presentation axes.

use ab_aozora_aat::aat_json_from_bytes;
use serde_json::Value;

#[test]
fn supplied_style_closers_end_the_matching_indentation_scope() {
    for (style, field, expected) in [
        ("横組み", "direction", "horizontal"),
        ("罫囲み", "formatting", "rule"),
        ("枠囲み", "formatting", "unspecified"),
        ("破線枠囲み", "formatting", "dashed-rule"),
    ] {
        let source = format!(
            "［＃ここから３字下げ、{style}］\n本文\n［＃ここで字下げ、{style}終わり］\n外側"
        );
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let block = &aat["blocks"][0];
        assert_eq!(block["kind"], "layout_block", "{aat}");
        let actual = if field == "formatting" {
            &block[field]["border"]
        } else {
            &block[field]
        };
        assert_eq!(actual, expected, "{aat}");
        assert!(!block.to_string().contains("外側"), "{aat}");
        assert!(!aat.to_string().contains("interpretation_problem"), "{aat}");
        let document = ab_aozora_facade::Document::new(source.as_str());
        let tree = document.parse();
        let canonical = tree.to_source();
        let reparsed = ab_aozora_facade::Document::new(canonical.as_str());
        assert_eq!(tree.to_html(), reparsed.parse().to_html());
        assert_eq!(canonical, reparsed.parse().to_source());
    }
}

#[test]
fn style_closer_cannot_end_an_indentation_with_a_different_style() {
    for (open, close) in [("横組み", "罫囲み"), ("罫囲み", "横組み")] {
        let source =
            format!("［＃ここから３字下げ、{open}］\n本文\n［＃ここで字下げ、{close}終わり］");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        assert!(aat.to_string().contains("interpretation_problem"), "{aat}");
        assert!(
            !aat.to_string().contains("\"kind\":\"layout_block\""),
            "{aat}"
        );
    }
}
