//! Parenthesized TCY aliases preserve exact source targets and owned markers.

use ab_aozora_aat::aat_json_from_bytes;
use ab_aozora_facade::Document;
use serde_json::Value;
use std::fmt::Write;

#[test]
fn parenthesized_tcy_uses_the_existing_target_and_marker_contract() {
    for target in ["（一）", "（二）", "（イ）", "（十一）", "（１）"] {
        let marker = format!("［＃{target}は縦中横］");
        let source = format!("前{target}{marker}後");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let facts = aat["meta"]["interpretation_facts"].as_array().unwrap();
        assert_eq!(facts.len(), 1, "{aat}");
        let span = &facts[0]["source_span"];
        let start = usize::try_from(span["start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(span["end"].as_u64().unwrap()).unwrap();
        assert_eq!(&source[start..end], marker);
        assert!(!aat.to_string().contains("interpretation_problem"));
        let document = Document::new(source);
        let html = document.parse().to_html();
        assert!(
            html.contains(&format!("class=\"aozora-combine-upright\">{target}</span>")),
            "{html}"
        );
        let canonical = document.parse().to_source();
        assert_eq!(Document::new(canonical).parse().to_html(), html);
    }
}

#[test]
fn parenthesized_tcy_declines_missing_or_malformed_target_claims() {
    for source in [
        "（二）［＃（一）は縦中横］",
        "前［＃（一）は縦中横］",
        "（）［＃（）は縦中横］",
        "（一［＃（一は縦中横］",
        "（一）（二）［＃（一）（二）は縦中横］",
        "（一）［＃（一）は縦中横、未知の指定］",
    ] {
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        assert!(
            aat["meta"]["interpretation_facts"]
                .as_array()
                .is_none_or(Vec::is_empty),
            "{source}: {aat}"
        );
        assert!(aat.to_string().contains("unknown-notation"), "{aat}");
        assert_eq!(Document::new(source).parse().to_source(), source);
    }
}

#[test]
fn quoted_target_cache_cannot_rule_out_unquoted_source_operands() {
    let mut prefix = String::new();
    for n in 0..70 {
        write!(prefix, "「語{n}」").unwrap();
    }
    let source = format!("{prefix}（１）［＃（１）は縦中横］");
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    assert_eq!(
        aat["meta"]["interpretation_facts"]
            .as_array()
            .unwrap()
            .len(),
        1
    );
    assert!(!aat.to_string().contains("unknown-notation"));
}
