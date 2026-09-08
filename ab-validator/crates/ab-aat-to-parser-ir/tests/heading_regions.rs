//! Source headings retain presentation and rich content through the IR boundary.

use std::path::Path;

use ab_aat_to_parser_ir::{ConversionOptions, ConversionRequest, MappingDocument, SchemaSet};
use serde_json::Value;

#[test]
fn source_heading_regions_survive_validated_conversion() {
    for prefix in ["", "同行", "窓"] {
        let source = format!(
            "題\n作者\n\n前［＃{prefix}中見出し］漢字《かんじ》［＃{prefix}中見出し終わり］後\n\n底本：本\n"
        );
        let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let result = ab_aat_to_parser_ir::convert(ConversionRequest {
            aat: serde_json::from_slice(&ab_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
                .unwrap(),
            mapping: MappingDocument::from_path(
                &repo.join("data/aat-to-parser-ir-mapping-v2.json"),
            )
            .unwrap(),
            schemas: SchemaSet::for_aat_version(&repo, None, 2).unwrap(),
            options: ConversionOptions::default(),
        })
        .unwrap();
        let nodes = result.parser_ir["nodes"].as_array().unwrap();
        let heading = nodes.iter().find(|node| node["type"] == "heading").unwrap();
        assert_eq!(heading["text"], "漢字");
        assert_eq!(heading["inline_children"][0]["type"], "ruby");
        assert_eq!(heading["source_span"]["coordinate_system"], "decoded_utf8");
        assert!(matches!(
            heading["style"].as_str(),
            Some("normal" | "dogyo" | "mado")
        ));
        let facts: &Vec<Value> = result.parser_ir["interpretation_facts"].as_array().unwrap();
        assert_eq!(
            facts
                .iter()
                .filter(|fact| fact["kind"] == "heading")
                .count(),
            2
        );
    }
}
