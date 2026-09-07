//! Supplied omission and incompleteness statements remain source apparatus.

use ab_aozora_aat::aat_json_from_bytes;
use ab_aozora_facade::Document;
use serde_json::Value;

#[test]
fn source_statements_preserve_assertion_kind_and_exact_marker() {
    for (statement, kind) in [("「註」略", "omission"), ("未完", "incompleteness")] {
        let marker = format!("［＃{statement}］");
        let source = format!("前{marker}後");
        assert_eq!(Document::new(source.as_str()).parse().to_source(), source);
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let content = aat["blocks"][0]["content"].as_array().unwrap();
        let note = content
            .iter()
            .find(|node| node["kind"] == "editorial_note")
            .unwrap();
        assert_eq!(note["note_kind"], kind);
        assert_eq!(note["text"], statement);
        assert_eq!(note["span"]["byte_start"], "前".len());
        assert_eq!(note["span"]["byte_end"], "前".len() + marker.len());
        assert_eq!(content.first().unwrap()["value"], "前");
        assert_eq!(content.last().unwrap()["value"], "後");
        let facts = aat["meta"]["interpretation_facts"].as_array().unwrap();
        assert_eq!(
            facts
                .iter()
                .filter(|fact| fact["kind"] == "editorial-note")
                .count(),
            1
        );
    }
}

#[test]
fn statement_like_prose_and_unknown_qualifiers_remain_distinct() {
    for source in ["未完", "［＃未完のため省略］", "［＃「註」一部略］"] {
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        assert!(
            aat["blocks"][0]["content"]
                .as_array()
                .unwrap()
                .iter()
                .all(|node| node["kind"] != "editorial_note")
        );
    }
}
