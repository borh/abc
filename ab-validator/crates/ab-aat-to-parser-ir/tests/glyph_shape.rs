//! Shape assertions own only their exact source target and supplied statement.
use std::path::Path;

use ab_aat_to_parser_ir::{ConversionOptions, ConversionRequest, MappingDocument, SchemaSet};
use serde_json::{Value, json};

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
fn adjacent_shape_assertions_preserve_literal_principal_and_separate_provenance() {
    for (target, statement) in [("ル", "上に「⌒」付き"), ("♂", "矢印が下向き")] {
        let marker = format!("［＃「{target}」は{statement}］");
        let ir = convert(&format!("前{target}{marker}後"));
        let all = nodes(&ir);
        let note = all
            .iter()
            .find(|n| n["note_kind"] == "glyph-shape")
            .unwrap();
        assert_eq!(note["text"], target);
        assert_eq!(note["annotation_children"][0]["text"], statement);
        assert!(note.get("position").is_none());
        let principal = &note["inline_children"][0]["source_span"];
        let annotation = &note["source_span"];
        assert_eq!(principal["end"], annotation["start"]);
        assert_eq!(
            principal["end"].as_u64().unwrap() - principal["start"].as_u64().unwrap(),
            3
        );
        assert!(ir["interpretation_problems"].as_array().unwrap().is_empty());
        let fact = ir["interpretation_facts"]
            .as_array()
            .unwrap()
            .iter()
            .find(|f| f["kind"] == "glyph-shape-assertion")
            .unwrap();
        assert_eq!(fact["source_span"], *annotation);
        assert_eq!(fact["aspects"], json!(["structure"]));
        assert!(
            all.iter()
                .all(|n| n["type"] != "gaiji" && n["type"] != "supplied-diacritic")
        );
    }
}

#[test]
fn absent_wrong_nonadjacent_and_quoted_lookalike_targets_remain_explicit() {
    let marker = "［＃「ル」は上に「⌒」付き］";
    for body in [
        format!("前{marker}後"),
        format!("リ{marker}"),
        format!("ル前{marker}"),
        format!("ル前ル後{marker}"),
        format!("「ル」{marker}"),
    ] {
        let ir = convert(&body);
        assert!(
            nodes(&ir).iter().all(|n| n["note_kind"] != "glyph-shape"),
            "{body}"
        );
        assert!(
            ir["interpretation_problems"]
                .as_array()
                .unwrap()
                .iter()
                .any(|p| p["raw"] == marker),
            "{body}"
        );
    }
}

#[test]
fn repeated_scalar_qualifies_only_the_immediately_preceding_source_occurrence() {
    let ir = convert("ルル［＃「ル」は上に「⌒」付き］後");
    let all = nodes(&ir);
    let note = all
        .iter()
        .find(|n| n["note_kind"] == "glyph-shape")
        .unwrap();
    assert_eq!(note["text"], "ル");
    assert_eq!(
        note["source_span"]["start"].as_u64().unwrap()
            - note["inline_children"][0]["source_span"]["start"]
                .as_u64()
                .unwrap(),
        3
    );
}
