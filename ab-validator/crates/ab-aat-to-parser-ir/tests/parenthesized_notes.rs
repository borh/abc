//! Parenthesized notes retain the supplied principal spelling and literal note.
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
fn supplied_parentheses_are_note_content_not_a_principal_correction() {
    let ir = convert("病殺［＃「殺」に（死）の注記］とするも可。");
    let all = nodes(&ir);
    let note = all.iter().find(|n| n["type"] == "annotated-text").unwrap();
    assert_eq!(note["text"], "殺");
    assert_eq!(note["note_kind"], "gloss");
    assert_eq!(note["annotation_children"][0]["text"], "（死）");
    assert!(note.get("position").is_none());
    assert_eq!(
        note["inline_children"][0]["source_span"]["end"],
        note["source_span"]["start"]
    );
    assert!(ir["interpretation_problems"].as_array().unwrap().is_empty());
    assert!(
        ir["interpretation_facts"]
            .as_array()
            .unwrap()
            .iter()
            .any(|f| f["kind"] == "annotated-text" && f["source_span"] == note["source_span"])
    );
}

#[test]
fn malformed_payloads_and_unowned_targets_stay_explicit() {
    for body in [
        "病［＃「殺」に（死）の注記］",
        "殺前殺後［＃「殺」に（死）の注記］",
        "病殺［＃「殺」に（）の注記］",
        "病殺［＃「殺」に（死の注記］",
        "病殺［＃「殺」に（死）余の注記］",
        "病殺［＃「殺」に（死）の注記か］",
    ] {
        let ir = convert(body);
        assert!(
            !nodes(&ir).iter().any(|n| n["type"] == "annotated-text"),
            "{body}"
        );
        assert!(
            !ir["interpretation_problems"].as_array().unwrap().is_empty(),
            "{body}"
        );
    }
}
