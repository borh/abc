//! Source-supplied rendition remains distinct from guessed mathematical content.

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
fn supplied_rendition_keeps_target_and_exact_layout_fact() {
    for (body, kind, text) in [
        ("花［＃「花」は罫囲み］", "keigakomi", "花"),
        ("обед［＃「обед」は横組み］", "yokogumi", "обед"),
        ("1/4πDt［＃「1/4πDt」は分数］", "fraction", "1/4πDt"),
    ] {
        let ir = convert(body);
        let layout = &ir["nodes"][0];
        assert_eq!(layout["type"], "layout-span");
        assert_eq!(layout["layout"]["kind"], kind);
        assert_eq!(layout["text"], text);
        assert_eq!(ir["interpretation_problems"], serde_json::json!([]));
        let facts = ir["interpretation_facts"].as_array().unwrap();
        assert_eq!(facts.len(), 1);
        assert_eq!(facts[0]["kind"], "layout");
        assert_eq!(facts[0]["aspects"], serde_json::json!(["layout"]));
    }
}

#[test]
fn absent_target_is_retained_as_uncertain_source() {
    let ir = convert("花［＃「月」は罫囲み］");
    assert!(ir["interpretation_facts"].as_array().unwrap().is_empty());
    assert!(!ir["interpretation_problems"].as_array().unwrap().is_empty());
    assert!(
        ir["nodes"]
            .as_array()
            .unwrap()
            .iter()
            .any(|node| node["note"]["raw"] == "［＃「月」は罫囲み］")
    );
}

#[test]
fn native_enclosure_kinds_do_not_collapse_to_one_border() {
    for (notation, expected) in [
        ("罫囲み", "rule"),
        ("「□」囲み", "box"),
        ("○付き文字", "circle"),
        ("点線丸囲み", "dotted-circle"),
        ("二重罫囲み", "double-rule"),
    ] {
        let ir = convert(&format!("花［＃「花」は{notation}］"));
        assert_eq!(
            ir["nodes"][0]["layout"]["border"], expected,
            "{notation}: {ir}"
        );
        assert_eq!(ir["nodes"][0]["text"], "花");
    }
}
