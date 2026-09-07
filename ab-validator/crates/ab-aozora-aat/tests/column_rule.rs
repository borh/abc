//! A supplied rule between columns is independent of an enclosing frame.

use ab_aozora_aat::aat_json_from_bytes;
use ab_aozora_facade::Document;
use serde_json::Value;

#[test]
fn supplied_column_rule_survives_source_serialization_and_aat() {
    let source = "［＃ここから２段組み、段間に罫］\n甲［＃改段］乙\n［＃ここで段組み終わり］";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let scope = &aat["blocks"][0];
    assert_eq!(scope["column_count"], 2);
    assert_eq!(scope["column_rule"], true);
    assert_eq!(
        scope["interpretation_marker_spans"]
            .as_array()
            .unwrap()
            .len(),
        2
    );
    assert!(!scope.to_string().contains("interpretation_problem"));
    let document = Document::new(source);
    let serialized = document.parse().to_source();
    assert!(serialized.contains("段間に罫"));
    let reparsed: Value =
        serde_json::from_slice(&aat_json_from_bytes(serialized.as_bytes()).unwrap()).unwrap();
    let restored = reparsed["blocks"]
        .as_array()
        .unwrap()
        .iter()
        .find(|block| block["kind"] == "layout_block")
        .unwrap();
    assert_eq!(restored["column_rule"], true);
    assert!(
        document
            .parse()
            .to_html()
            .contains("data-column-rule=\"true\"")
    );
}
