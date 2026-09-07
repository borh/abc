//! Separately printed notes become source-associated apparatus only after complete adoption.
use ab_aozora_aat::aat_json_from_bytes;
use serde_json::Value;

fn collect<'a>(value: &'a Value, kind: &str, found: &mut Vec<&'a Value>) {
    match value {
        Value::Object(object) => {
            if value["kind"] == kind {
                found.push(value);
            }
            for value in object.values() {
                collect(value, kind, found);
            }
        }
        Value::Array(array) => {
            for value in array {
                collect(value, kind, found);
            }
        }
        _ => {}
    }
}

fn principal(value: &Value) -> String {
    if value["kind"] == "text" {
        return value["value"].as_str().unwrap().to_owned();
    }
    for key in ["blocks", "children", "content"] {
        if let Some(children) = value[key].as_array() {
            return children.iter().map(principal).collect();
        }
    }
    String::new()
}

#[test]
fn notes_above_and_below_one_target_are_not_principal_text() {
    let marker =
        "［＃「なごりイ」は「檐端哉」の右側に、「り檐の花イ」は左側に、注記するような形で］";
    let source =
        format!("　なごりイ\n飯蛸の手をひろげたる檐端哉\n　り檐の花イ\n{marker}\n次の句\n");
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let mut notes = Vec::new();
    collect(&aat, "annotated_text", &mut notes);
    assert_eq!(notes.len(), 2, "{aat}");
    assert_eq!(
        principal(&aat).replace('\n', ""),
        "飯蛸の手をひろげたる檐端哉次の句"
    );
    let mut sides = Vec::new();
    for note in notes {
        let annotation = &note["annotation_content"][0];
        let start = usize::try_from(annotation["span"]["byte_start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(annotation["span"]["byte_end"].as_u64().unwrap()).unwrap();
        assert_eq!(&source[start..end], annotation["value"].as_str().unwrap());
        assert!(end < source.find(marker).unwrap());
        sides.push((
            note["position"].as_str().unwrap(),
            annotation["value"].as_str().unwrap(),
        ));
        assert_eq!(note["span"]["byte_start"], source.find(marker).unwrap());
    }
    sides.sort_unstable();
    assert_eq!(sides, vec![("left", "り檐の花イ"), ("right", "なごりイ")]);
    let document = ab_aozora_facade::Document::new(source.as_str());
    assert_eq!(document.parse().to_source(), source);
}

#[test]
fn disjoint_targets_keep_note_order_and_original_crlf_provenance() {
    let marker = "［＃「よそイ」は「どこ」の左側に、「やイ」は「ぞ」の左側に注記するような形で］";
    let source = format!("家根の上にどこの哀れぞ揚燈籠\r\n　よそイ　やイ\r\n{marker}\r\n次\r\n");
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let mut notes = Vec::new();
    collect(&aat, "annotated_text", &mut notes);
    assert_eq!(notes.len(), 2, "{aat}");
    assert_eq!(
        principal(&aat).replace(['\n', '\r'], ""),
        "家根の上にどこの哀れぞ揚燈籠次"
    );
    for note in notes {
        let annotation = &note["annotation_content"][0];
        let start = usize::try_from(annotation["span"]["byte_start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(annotation["span"]["byte_end"].as_u64().unwrap()).unwrap();
        assert_eq!(&source[start..end], annotation["value"].as_str().unwrap());
    }
}

#[test]
fn a_target_spelling_inside_source_markup_does_not_justify_removal() {
    let source = "繩\n※［＃「綱」、未解決外字］\n［＃「繩」は「綱」の右側に注記するような形で］\n";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let mut notes = Vec::new();
    collect(&aat, "annotated_text", &mut notes);
    assert!(notes.is_empty(), "{aat}");
    assert!(principal(&aat).contains('繩'));
    assert!(aat.to_string().contains("unresolved-transcribed-note"));
}
