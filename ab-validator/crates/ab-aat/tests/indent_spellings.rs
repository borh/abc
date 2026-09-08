//! Source spelling variants retain indentation geometry and closer constraints.

use ab_aat::aat_json_from_bytes;
use serde_json::Value;

#[test]
fn supplied_indent_spellings_share_the_same_geometry() {
    for (opener, closer, amount, continuation) in [
        ("ここから一字下げ", "ここで字下げ終わり", 1, None),
        ("ここから二字下げ", "ここで字下げ終り", 2, None),
        ("ここより１字下げ", "ここで文字下げ終わり", 1, None),
        ("ここから３　字下げ", "ここで３字下げ終わり", 3, None),
        (
            "ここから改行１字下げ、折り返して２字下げ",
            "ここで字下げ終わり",
            1,
            Some(2),
        ),
        (
            "ここから天付き折り返して１字下げ",
            "ここで字下げ終わり",
            0,
            Some(1),
        ),
        (
            "ここから１０字下げ折り返して１７字下げ",
            "ここで字下げ終わり",
            10,
            Some(17),
        ),
    ] {
        let source = format!("［＃{opener}］\n本文\n［＃{closer}］");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let block = &aat["blocks"][0];
        assert_eq!(block["kind"], "layout_block", "{aat}");
        assert_eq!(block["indent"], amount, "{aat}");
        assert_eq!(block["continuation_indent"].as_u64(), continuation, "{aat}");
        assert!(!aat.to_string().contains("interpretation_problem"), "{aat}");
    }
}

#[test]
fn explicitly_numbered_closer_cannot_terminate_a_different_indent() {
    let source = "［＃ここから２字下げ］\n本文\n［＃ここで１字下げ終わり］";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    assert!(aat.to_string().contains("interpretation_problem"), "{aat}");
    assert!(
        aat["meta"]["interpretation_facts"]
            .as_array()
            .is_none_or(Vec::is_empty),
        "{aat}"
    );
}

#[test]
fn canonical_source_preserves_a_supplied_closing_magnitude() {
    let source = "［＃ここから３字下げ］\n本文\n［＃ここで３字下げ終わり］";
    let document = ab_aozora_facade::Document::new(source);
    let tree = document.parse();
    let canonical = tree.to_source();
    assert!(
        canonical.contains("［＃ここで3字下げ終わり］"),
        "{canonical}"
    );
    let reparsed = ab_aozora_facade::Document::new(canonical.as_str());
    assert_eq!(reparsed.parse().to_html(), tree.to_html());
    assert_eq!(reparsed.parse().to_source(), canonical);
}

#[test]
fn unrelated_or_conflicting_remainder_is_not_an_indent_alias() {
    for opener in [
        "ここから一二字下げ",
        "ここから改行改行天付き、折り返して２字下げ",
        "ここから300字下げ",
        "ここから改行１字下げらしい",
        "ここより１字下げという説明",
    ] {
        let source = format!("［＃{opener}］\n本文\n［＃ここで字下げ終わり］");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        assert_ne!(aat["blocks"][0]["kind"], "layout_block", "{aat}");
        assert!(aat.to_string().contains("interpretation_problem"), "{aat}");
    }
}
