//! Retrospective kunten declarations own the supplied target and exact marker.

use ab_aat::aat_json_from_bytes;
use serde_json::Value;

#[test]
fn exact_marker_facts_survive_plain_and_rich_ruby_targets() {
    for source in [
        "前レ［＃「レ」は返り点］後",
        "｜漢レ［＃「レ」は返り点］字《かんじ》",
        "前一レ［＃「一レ」は返り点］後",
    ] {
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let facts = aat["meta"]["interpretation_facts"].as_array().unwrap();
        let fact = facts
            .iter()
            .find(|fact| fact["kind"] == "kunten")
            .unwrap_or_else(|| panic!("{source}: {aat}"));
        let start = usize::try_from(fact["source_span"]["start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(fact["source_span"]["end"].as_u64().unwrap()).unwrap();
        assert_eq!(
            &source[start..end],
            &source[source.find('［').unwrap()..source.find('］').unwrap() + "］".len()],
            "{aat}"
        );
        let mut pending = vec![&aat["blocks"]];
        let mut marks = Vec::new();
        while let Some(value) = pending.pop() {
            match value {
                Value::Object(fields) => {
                    if value["kind"] == "kunten" {
                        marks.push(value);
                    }
                    pending.extend(fields.values());
                }
                Value::Array(values) => pending.extend(values),
                _ => {}
            }
        }
        assert_eq!(marks.len(), 1, "{aat}");
        let mark = marks[0];
        assert_eq!(mark["interpretation_marker_spans"][0]["byte_start"], start);
        assert_eq!(mark["span"]["byte_end"], end);
        assert!(mark["span"]["byte_start"].as_u64().unwrap() < start as u64);
    }
}

#[test]
fn unestablished_reading_target_remains_located_source_notation() {
    let source = "｜漢字《かんレ［＃「レ」は返り点］じ》";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let reading = &aat["blocks"][0]["content"][0]["reading_content"];
    assert_eq!(reading[0]["value"], "かんレ");
    assert_eq!(reading[1]["source"], "［＃「レ」は返り点］");
    assert_eq!(
        reading[1]["interpretation_problem"]["kind"],
        "unknown-notation"
    );
    assert!(
        !aat["meta"]["interpretation_facts"]
            .as_array()
            .unwrap()
            .iter()
            .any(|fact| fact["kind"] == "kunten")
    );
}
