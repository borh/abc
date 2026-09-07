//! Ruby readings retain supplied kunten as located native annotations.

use ab_aozora_aat::aat_json_from_bytes;
use serde_json::Value;

#[test]
fn ruby_reading_retains_typed_kunten_and_exact_source_span() {
    let source = "漢《かん［＃（ノ）］じ》";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let ruby = &aat["blocks"][0]["content"][0];
    assert_eq!(ruby["kind"], "ruby");
    let mark = &ruby["reading_content"][1];
    assert_eq!(mark["kind"], "kunten", "{aat}");
    assert_eq!(mark["kunten_kind"], "okurigana", "{mark}");
    let start = usize::try_from(mark["span"]["byte_start"].as_u64().unwrap()).unwrap();
    let end = usize::try_from(mark["span"]["byte_end"].as_u64().unwrap()).unwrap();
    assert_eq!(&source[start..end], "［＃（ノ）］");
}

#[test]
fn unknown_reading_annotation_stays_raw_beside_kunten() {
    let source = "漢《か［＃（ノ）］ん［＃未知の注］じ》";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let reading = aat["blocks"][0]["content"][0]["reading_content"]
        .as_array()
        .unwrap();
    let unknown = reading.iter().find(|node| node["kind"] == "raw").unwrap();
    assert_eq!(unknown["source"], "［＃未知の注］");
    assert_eq!(
        unknown["interpretation_problem"]["code"],
        "unknown-notation"
    );
    let start = usize::try_from(unknown["span"]["byte_start"].as_u64().unwrap()).unwrap();
    let end = usize::try_from(unknown["span"]["byte_end"].as_u64().unwrap()).unwrap();
    assert_eq!(&source[start..end], unknown["source"]);
    assert!(reading.iter().any(|node| node["kind"] == "kunten"));
    assert!(
        !reading
            .iter()
            .filter_map(|node| node["value"].as_str())
            .any(|text| text.contains("［＃"))
    );
}

#[test]
fn quote_and_newline_mapping_preserve_nested_marker_coordinates() {
    for source in ["前\r\n漢《か［＃（ノ）］ん》", "≪漢《か［＃（ノ）］ん》≫"]
    {
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let mut pending = vec![&aat];
        let mut found = false;
        while let Some(node) = pending.pop() {
            match node {
                Value::Array(values) => pending.extend(values),
                Value::Object(fields) => {
                    if node["kind"] == "kunten" && node.get("span").is_some() {
                        let start =
                            usize::try_from(node["span"]["byte_start"].as_u64().unwrap()).unwrap();
                        let end =
                            usize::try_from(node["span"]["byte_end"].as_u64().unwrap()).unwrap();
                        assert_eq!(&source[start..end], "［＃（ノ）］");
                        found = true;
                    }
                    pending.extend(fields.values());
                }
                _ => {}
            }
        }
        assert!(found, "{aat}");
    }
}

#[test]
fn explicit_base_contains_native_annotations_until_reading() {
    for (source, expected) in [
        ("｜漢［＃レ］字《かん［＃（ノ）］じ》", "漢字"),
        (
            "｜a［＃レ］ b※［＃「えんにょう＋囘」、第４水準2-12-11］字《よみ》",
            "a b𢌞字",
        ),
        (
            "｜遊［＃二］松島［＃一］記《まつしまにあそぶき》",
            "遊松島記",
        ),
    ] {
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let nodes = aat["blocks"][0]["content"].as_array().unwrap();
        assert_eq!(nodes.len(), 1, "{aat}");
        assert_eq!(nodes[0]["kind"], "ruby");
        assert_eq!(nodes[0]["base"], expected);
        assert!(
            nodes[0]["base_content"]
                .as_array()
                .unwrap()
                .iter()
                .any(|node| node["kind"] == "kunten")
        );
    }
}
