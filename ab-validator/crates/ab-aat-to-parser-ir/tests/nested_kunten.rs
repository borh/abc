//! Nested kunten and unknown annotations retain their ruby reading context.
use ab_aat_to_parser_ir::{ConversionOptions, ConversionRequest, MappingDocument, SchemaSet};
use serde_json::Value;
use std::path::Path;

#[test]
fn ruby_reading_keeps_supplied_marks_and_explicit_unknown_source() {
    let source = "題\n作者\n\n漢《か［＃（ノ）］ん［＃未知の注］じ》\n\n底本：本\n";
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let ir = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: serde_json::from_slice(
            &ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap(),
        )
        .unwrap(),
        mapping: MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v2.json"))
            .unwrap(),
        schemas: SchemaSet::for_aat_version(&repo, None, 2).unwrap(),
        options: ConversionOptions::default(),
    })
    .unwrap()
    .parser_ir;
    let ruby = ir["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|node| node["type"] == "ruby")
        .unwrap();
    let text = ruby.to_string();
    assert!(text.contains("kunten"), "{ruby}");
    let problem = ir["interpretation_problems"]
        .as_array()
        .unwrap()
        .iter()
        .find(|node| node["code"] == "unknown-notation")
        .unwrap();
    assert_eq!(problem["raw"], "［＃未知の注］");
    let mut pending: Vec<&Value> = vec![ruby];
    let mut found = false;
    while let Some(node) = pending.pop() {
        match node {
            Value::Array(values) => pending.extend(values),
            Value::Object(fields) => {
                if node["type"] == "kunten" {
                    assert_eq!(node["text"], "ノ");
                    let start =
                        usize::try_from(node["source_span"]["start"].as_u64().unwrap()).unwrap();
                    let end =
                        usize::try_from(node["source_span"]["end"].as_u64().unwrap()).unwrap();
                    assert_eq!(&source[start..end], "［＃（ノ）］");
                    found = true;
                }
                pending.extend(fields.values());
            }
            _ => {}
        }
    }
    assert!(found);
}
