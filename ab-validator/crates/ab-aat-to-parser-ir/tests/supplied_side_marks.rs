//! Supplied side marks retain position without an inferred linguistic role.
use std::path::Path;

use ab_aat_to_parser_ir::{ConversionOptions, ConversionRequest, MappingDocument, SchemaSet};
use serde_json::Value;

fn convert(body: &str) -> Value {
    let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let mapping =
        MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v2.json")).unwrap();
    let schemas = SchemaSet::for_aat_version(&repo, None, 2).unwrap();
    ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: serde_json::from_slice(&ab_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
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
fn supplied_right_parenthesis_has_separate_target_and_annotation_axes() {
    let ir = convert("あまぐれ　おろちへ［＃「ちへ」の右に「）」］");
    let all = nodes(&ir);
    let note = all
        .iter()
        .find(|n| n["note_kind"] == "supplied-mark")
        .unwrap();
    assert_eq!(note["text"], "ちへ");
    assert_eq!(note["position"], "right");
    assert_eq!(note["annotation_children"][0]["text"], "）");
    assert_eq!(
        note["inline_children"][0]["source_span"]["end"],
        note["source_span"]["start"]
    );
    assert_eq!(
        note["annotation_children"][0]["span"]["coordinate_system"],
        "annotation_utf8"
    );
    assert!(ir["interpretation_problems"].as_array().unwrap().is_empty());
    let fact = ir["interpretation_facts"]
        .as_array()
        .unwrap()
        .iter()
        .find(|f| f["source_span"] == note["source_span"])
        .unwrap();
    assert_eq!(fact["kind"], "supplied-mark");
    assert_eq!(fact["aspects"], serde_json::json!(["structure", "layout"]));
}

#[test]
fn source_mark_requires_the_supplied_adjacent_target() {
    for body in [
        "前［＃「ちへ」の右に「）」］",
        "ちえ［＃「ちへ」の右に「）」］",
        "ちへ前［＃「ちへ」の右に「）」］",
        "ちへ前ちへ後［＃「ちへ」の右に「）」］",
        "ちへ［＃「ちへ」の右に「）の注記］",
        "ちへ［＃「ちへ」の右に「）」か］",
    ] {
        let ir = convert(body);
        assert!(
            !nodes(&ir).iter().any(|n| n["note_kind"] == "supplied-mark"),
            "{body}"
        );
        assert!(
            !ir["interpretation_problems"].as_array().unwrap().is_empty(),
            "{body}"
        );
    }
}
