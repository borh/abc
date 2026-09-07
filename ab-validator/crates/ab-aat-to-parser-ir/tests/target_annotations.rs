use std::path::Path;

use ab_aat_to_parser_ir::{ConversionOptions, ConversionRequest, MappingDocument, SchemaSet};
use serde_json::{Value, json};

fn convert(body: &str) -> Value {
    let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let mapping =
        MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v2.json")).unwrap();
    let schemas = SchemaSet::for_aat_version(&repo, None, 2).unwrap();
    ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: serde_json::from_slice(
            &ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap(),
        )
        .unwrap(),
        mapping,
        schemas,
        options: ConversionOptions::default(),
    })
    .unwrap()
    .parser_ir
}

fn nodes(value: &Value) -> Vec<&Value> {
    let mut result = Vec::new();
    let mut pending = vec![value];
    while let Some(value) = pending.pop() {
        match value {
            Value::Object(map) => {
                result.push(value);
                pending.extend(map.values());
            }
            Value::Array(values) => pending.extend(values),
            _ => {}
        }
    }
    result
}

#[test]
fn target_notes_preserve_principal_and_annotation_axes_with_only_supplied_side() {
    for (body, position) in [
        ("刺［＃「刺」に「テフダ」の注記］後", None),
        ("刺［＃「刺」の左に「テフダ」の注記］後", Some("left")),
        ("刺［＃「刺」の右に「テフダ」の注記］後", Some("right")),
        ("刺［＃「刺」に「ママ」注記］後", None),
    ] {
        let ir = convert(body);
        let all = nodes(&ir);
        let annotation = all
            .iter()
            .find(|node| node["type"] == "annotated-text")
            .unwrap();
        assert_eq!(annotation["text"], "刺");
        assert_eq!(annotation["position"].as_str(), position);
        assert_eq!(
            annotation["annotation_children"][0]["span"]["coordinate_system"],
            "annotation_utf8"
        );
        assert_eq!(annotation["annotation_children"][0]["span"]["start"], 0);
        assert_eq!(
            annotation["inline_children"][0]["span"]["coordinate_system"],
            "parser_text_utf8"
        );
        assert_eq!(ir["interpretation_problems"], json!([]));
        let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
        let note_child = &annotation["annotation_children"][0];
        let start = usize::try_from(note_child["source_span"]["start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(note_child["source_span"]["end"].as_u64().unwrap()).unwrap();
        assert_eq!(&source[start..end], note_child["text"].as_str().unwrap());
        let fact = ir["interpretation_facts"]
            .as_array()
            .unwrap()
            .iter()
            .find(|fact| fact["kind"] == "annotated-text")
            .unwrap();
        assert_eq!(fact["source_span"], annotation["source_span"]);
    }
}

#[test]
fn annotations_reuse_rich_source_fragments_without_turning_ruby_into_principal_text() {
    let ir = convert("漢《かん》［＃「漢」に「字《じ》」の注記］後");
    let all = nodes(&ir);
    let annotation = all
        .iter()
        .find(|node| node["type"] == "annotated-text")
        .unwrap();
    assert_eq!(annotation["text"], "漢");
    assert_eq!(annotation["inline_children"][0]["type"], "ruby");
    assert_eq!(annotation["inline_children"][0]["ruby"]["reading"], "かん");
    assert_eq!(annotation["annotation_children"][0]["type"], "ruby");
    assert_eq!(
        annotation["annotation_children"][0]["ruby"]["reading"],
        "じ"
    );
    assert_eq!(
        annotation["annotation_children"][0]["span"]["coordinate_system"],
        "annotation_utf8"
    );
    assert_eq!(ir["interpretation_problems"], json!([]));
}

#[test]
fn unsupported_annotation_content_retains_principal_text_and_located_uncertainty() {
    let ir = convert("刺［＃「刺」に「字［＃未知の注記］」の注記］後");
    let all = nodes(&ir);
    assert!(!all.iter().any(|node| node["type"] == "annotated-text"));
    assert!(all.iter().any(|node| node["text"] == "刺"));
    assert!(!ir["interpretation_problems"].as_array().unwrap().is_empty());
}

#[test]
fn native_interior_target_preserves_surrounding_okurigana() {
    let ir = convert("土を堀り［＃「堀」に「ママ」注記］かへす");
    let annotation = nodes(&ir)
        .into_iter()
        .find(|node| node["type"] == "annotated-text")
        .unwrap();
    assert_eq!(annotation["text"], "堀");
    assert_eq!(annotation["annotation_children"][0]["text"], "ママ");
    let text: String = ir["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|node| node["type"] != "source-note")
        .filter_map(|node| node["text"].as_str())
        .collect();
    assert_eq!(text, "土を堀りかへす");
    assert_eq!(ir["interpretation_problems"], json!([]));
}

#[test]
fn mismatched_annotation_target_does_not_invent_principal_content() {
    let ir = convert("刺［＃「別」に「ママ」注記］後");
    assert!(
        !nodes(&ir)
            .iter()
            .any(|node| node["type"] == "annotated-text")
    );
    let text: String = ir["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|node| node["type"] != "source-note")
        .filter_map(|node| node["text"].as_str())
        .collect();
    assert_eq!(text, "刺後");
    assert!(!ir["interpretation_problems"].as_array().unwrap().is_empty());
}
