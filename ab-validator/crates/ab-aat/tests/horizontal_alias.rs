//! Supplied horizontal writing aliases share canonical target and scope semantics.

use ab_aat::aat_json_from_bytes;
use ab_aozora_facade::Document;
use serde_json::Value;

#[test]
fn horizontal_aliases_preserve_targets_scopes_and_round_trip() {
    for (source, expected_facts) in [
        ("前しらおか［＃「しらおか」は横書き］後", 1),
        ("［＃ここから横書き］\n甲\n乙\n［＃ここで横書き終わり］", 2),
    ] {
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        assert_eq!(
            aat["meta"]["interpretation_facts"]
                .as_array()
                .unwrap()
                .len(),
            expected_facts,
            "{aat}"
        );
        assert!(!aat.to_string().contains("interpretation_problem"));
        let document = Document::new(source);
        let tree = document.parse();
        assert_eq!(
            tree.to_html(),
            Document::new(tree.to_source()).parse().to_html()
        );
    }
}

#[test]
fn unsupported_horizontal_alias_scopes_remain_explicit() {
    for source in [
        "前［＃ここから横書き］甲",
        "前［＃ここで横書き終わり］",
        "前［＃「不在」は横書き、未知の指定］",
        "前［＃「不在」は横書き］",
        "前［＃「不在」は横組み］",
    ] {
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        assert!(aat.to_string().contains("interpretation_problem"), "{aat}");
        assert!(
            aat["meta"]["interpretation_facts"]
                .as_array()
                .is_none_or(Vec::is_empty)
        );
    }
}
