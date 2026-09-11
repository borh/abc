//! Typeface ranges preserve source paragraphs and independent embedded styling.
use std::path::Path;

use ab_aat_to_parser_ir::{ConversionOptions, ConversionRequest, MappingDocument, SchemaSet};
use serde_json::Value;

fn convert(body: &str) -> Value {
    let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let mapping =
        MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v2.json")).unwrap();
    let schemas = SchemaSet::for_aat_version(2).unwrap();
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
fn textbook_scope_preserves_embedded_bold_and_mid_paragraph_end() {
    let ir = convert(
        "前\n［＃ここから教科書体］\n本文\n人［＃「人」は太字］　話［＃ここで教科書体終わり］後",
    );
    let scope = ir["layout_blocks"]
        .as_array()
        .unwrap()
        .iter()
        .find(|s| s["typography"]["style"] == "textbook")
        .unwrap();
    let end = usize::try_from(scope["node_range"]["end"].as_u64().unwrap()).unwrap();
    assert!(ir["nodes"][end]["text"].as_str().unwrap().starts_with('後'));
    assert!(ir["paragraphs"].as_array().unwrap().iter().any(|p| {
        p["node_range"]["start"].as_u64().unwrap() < u64::try_from(end).unwrap()
            && p["node_range"]["end"].as_u64().unwrap() > u64::try_from(end).unwrap()
    }));
    assert!(
        nodes(&ir)
            .iter()
            .any(|n| n["type"] == "emphasis" && n["style"] == "bold" && n["text"] == "人")
    );
    assert!(ir["interpretation_problems"].as_array().unwrap().is_empty());
    assert_eq!(
        ir["interpretation_facts"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|f| f["kind"] == "emphasis")
            .count(),
        3
    );
}

#[test]
fn a_missing_textbook_close_does_not_certify_the_scope() {
    let ir = convert("［＃ここから教科書体］\n本文");
    assert!(!nodes(&ir).iter().any(|n| n["style"] == "textbook"));
    assert!(!ir["interpretation_problems"].as_array().unwrap().is_empty());
}
