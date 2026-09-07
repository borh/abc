//! External table references retain their source slot independently of principal text.

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
fn external_table_reference_preserves_filename_and_located_missing_content() {
    let marker = "［＃ここに表組入る、別ファイル（densyanokonzatsu_table.txt）参照］";
    let ir = convert(&format!("前{marker}後"));
    let all = nodes(&ir);
    let note = all
        .iter()
        .find(|n| n["note_kind"] == "external-table-reference")
        .unwrap();
    assert_eq!(note["source_filename"], "densyanokonzatsu_table.txt");
    let problem = &ir["interpretation_problems"][0];
    assert_eq!(ir["interpretation_problems"].as_array().unwrap().len(), 1);
    assert_eq!(problem["raw"], marker);
    assert_eq!(problem["kind"], "content-outside-primary-input");
    assert_eq!(problem["aspects"], json!(["content"]));
    assert_eq!(problem["influence"], json!({"kind":"source-location"}));
    assert_eq!(problem["source_span"], note["source_span"]);
    let fact = ir["interpretation_facts"]
        .as_array()
        .unwrap()
        .iter()
        .find(|f| f["kind"] == "external-table-reference")
        .unwrap();
    assert_eq!(fact["source_span"], problem["source_span"]);
    assert_eq!(fact["aspects"], json!(["structure"]));
    assert!(
        all.iter()
            .all(|n| n["type"] != "table" && n["type"] != "illustration")
    );
}

#[test]
fn malformed_or_other_external_reference_remains_uninterpreted() {
    for marker in [
        "［＃ここに表組入る、別ファイル（../table.txt）参照］",
        "［＃ここに図入る、別ファイル（table.txt）参照］",
    ] {
        let ir = convert(&format!("前{marker}後"));
        assert!(
            nodes(&ir)
                .iter()
                .all(|n| n["note_kind"] != "external-table-reference")
        );
        assert!(
            ir["interpretation_facts"]
                .as_array()
                .unwrap()
                .iter()
                .all(|f| f["kind"] != "external-table-reference")
        );
        assert!(
            ir["interpretation_problems"]
                .as_array()
                .unwrap()
                .iter()
                .any(|p| p["raw"] == marker && p["influence"]["kind"] == "document")
        );
    }
}
