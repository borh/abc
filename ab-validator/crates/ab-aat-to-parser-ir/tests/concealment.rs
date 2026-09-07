//! Concealment assertions retain supplied placeholders and never reconstruct text.

use std::path::Path;

use ab_aat_to_parser_ir::{ConversionOptions, ConversionRequest, MappingDocument, SchemaSet};
use serde_json::Value;

fn convert(body: &str) -> Value {
    let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    ab_aat_to_parser_ir::convert(ConversionRequest {
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
    .parser_ir
}

#[test]
fn source_quantity_is_attached_to_exact_visible_placeholders() {
    for (marks, count) in [("□", 1), ("□□", 2), ("□□□□", 4)] {
        let ir = convert(&format!("前{marks}［＃底本{count}字伏字］後"));
        let node = ir["nodes"]
            .as_array()
            .unwrap()
            .iter()
            .find(|n| n["type"] == "annotated-text")
            .unwrap();
        assert_eq!(node["text"], marks);
        assert_eq!(node["note_kind"], "base-edition");
        let marker_start = "題\n作者\n\n前".len() + marks.len();
        assert_eq!(node["source_span"]["start"], marker_start);
        assert_eq!(
            node["source_span"]["end"],
            marker_start + format!("［＃底本{count}字伏字］").len()
        );
        assert_eq!(node["annotation_children"][0]["type"], "gap");
        assert_eq!(node["annotation_children"][0]["quantity"], count);
        assert_eq!(
            node["annotation_children"][0]["span"]["coordinate_system"],
            "annotation_utf8"
        );
        assert_eq!(ir["interpretation_problems"], serde_json::json!([]));
    }
}

#[test]
fn conflicting_quantity_or_missing_target_is_not_repaired() {
    for body in [
        "□□□［＃底本２字伏字］",
        "本文［＃底本２字伏字］",
        "□□ ［＃底本２字伏字］",
    ] {
        let ir = convert(body);
        assert!(!ir["interpretation_problems"].as_array().unwrap().is_empty());
        assert!(
            ir["nodes"]
                .as_array()
                .unwrap()
                .iter()
                .all(|n| n["type"] != "annotated-text")
        );
    }
}

#[test]
fn explicit_placeholder_does_not_invent_quantity_or_edition() {
    let ir = convert("前＊［＃「＊」は伏せ字］後");
    let node = ir["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .find(|n| n["type"] == "annotated-text")
        .unwrap();
    assert_eq!(node["text"], "＊");
    assert_eq!(node["note_kind"], "source-concealment");
    let gap = &node["annotation_children"][0];
    assert_eq!(gap["extent"], "unknown");
    assert!(gap.get("quantity").is_none());
    assert!(gap.get("unit").is_none());
    assert_eq!(ir["interpretation_problems"], serde_json::json!([]));
}

#[test]
fn gap_is_apparatus_and_has_one_supplied_extent() {
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let schemas = SchemaSet::for_aat_version(&repo, None, 2).unwrap();
    let validators = ab_aat_to_parser_ir::schema::SchemaValidators::compile(&schemas).unwrap();
    let ir = convert("□□［＃底本２字伏字］");
    let index = ir["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .position(|n| n["type"] == "annotated-text")
        .unwrap();
    let gap = ir["nodes"][index]["annotation_children"][0].clone();
    let mut body_gap = ir.clone();
    body_gap["nodes"][index] = gap.clone();
    assert!(!validators.parser_ir.is_valid(&body_gap));
    let mut principal_gap = ir.clone();
    principal_gap["nodes"][index]["inline_children"][0] = gap;
    assert!(!validators.parser_ir.is_valid(&principal_gap));
    let mut contradictory_extent = ir;
    contradictory_extent["nodes"][index]["annotation_children"][0]["extent"] =
        serde_json::json!("unknown");
    assert!(!validators.parser_ir.is_valid(&contradictory_extent));
}
