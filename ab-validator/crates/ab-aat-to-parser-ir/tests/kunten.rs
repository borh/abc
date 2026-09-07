//! Supplied kunten remains separate from principal text with exact source positions.
use ab_aat_to_parser_ir::{ConversionOptions, ConversionRequest, MappingDocument, SchemaSet};
use serde_json::Value;
use std::path::Path;

#[test]
fn supplied_kunten_preserves_category_text_and_source() {
    let source = "題\n作者\n\n漢［＃レ］字［＃（レ）］給［＃（弖）］［＃一レ］。\n\n底本：本\n";
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let aat =
        serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
            .unwrap();
    let ir = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat,
        mapping: MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v2.json"))
            .unwrap(),
        schemas: SchemaSet::for_aat_version(&repo, None, 2).unwrap(),
        options: ConversionOptions::default(),
    })
    .unwrap()
    .parser_ir;
    let marks: Vec<&Value> = ir["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|n| n["type"] == "kunten")
        .collect();
    assert_eq!(marks.len(), 4, "{ir}");
    for (node, (kind, text, raw)) in marks.iter().zip([
        ("return-mark", "レ", "［＃レ］"),
        ("okurigana", "レ", "［＃（レ）］"),
        ("okurigana", "弖", "［＃（弖）］"),
        ("return-mark", "一レ", "［＃一レ］"),
    ]) {
        assert_eq!(node["kunten_kind"], kind);
        assert_eq!(node["text"], text);
        let span = &node["source_span"];
        let start = usize::try_from(span["start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(span["end"].as_u64().unwrap()).unwrap();
        assert_eq!(&source[start..end], raw);
    }
    assert_eq!(ir["interpretation_problems"], serde_json::json!([]));
}
