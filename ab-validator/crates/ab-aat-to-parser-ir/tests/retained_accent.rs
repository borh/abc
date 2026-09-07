use std::path::Path;

use ab_aat_to_parser_ir::{ConversionOptions, ConversionRequest, MappingDocument, SchemaSet};
use serde_json::Value;

fn convert(source: &str) -> Value {
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let aat =
        serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
            .unwrap();
    ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping: MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v2.json"))
            .unwrap(),
        schemas: SchemaSet::for_aat_version(&repo, None, 2).unwrap(),
        options: ConversionOptions::default(),
    })
    .unwrap()
    .parser_ir
}

#[test]
fn retained_multiline_accents_preserve_text_and_located_uncertainty() {
    let raw = "〔ae&\r\nordinary〕";
    let source = format!("\u{feff}題\r\n作者\r\n\r\n前{raw}後。\r\n\r\n底本：本\r\n");
    let ir = convert(&source);
    let problems = ir["interpretation_problems"].as_array().unwrap();
    assert_eq!(problems.len(), 1, "{problems:?}");
    let problem = &problems[0];
    assert_eq!(problem["kind"], "uninterpreted-notation");
    assert_eq!(problem["raw"], raw);
    assert_eq!(problem["source_span"]["coordinate_system"], "decoded_utf8");
    let start = usize::try_from(problem["source_span"]["start"].as_u64().unwrap()).unwrap();
    let end = usize::try_from(problem["source_span"]["end"].as_u64().unwrap()).unwrap();
    let decoded = source.strip_prefix('\u{feff}').unwrap();
    assert_eq!(&decoded[start..end], raw);
    assert!(
        problem["aspects"]
            .as_array()
            .unwrap()
            .iter()
            .any(|aspect| aspect == "content")
    );
    let text: String = ir["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .filter_map(|node| node["text"].as_str())
        .collect();
    assert!(text.contains("前〔ae&\nordinary〕後。"), "{text:?}");
}

#[test]
fn ordinary_multiline_brackets_do_not_claim_accent_uncertainty() {
    let ir = convert("題\n作者\n\n〔ordinary\ntext〕\n\n底本：本\n");
    assert!(ir["interpretation_problems"].as_array().unwrap().is_empty());
}
