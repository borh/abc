//! Observed spellings reuse the same semantic constructs as their canonical forms.
use ab_aat_to_parser_ir::{ConversionOptions, ConversionRequest, MappingDocument, SchemaSet};
use serde_json::Value;
use std::path::Path;

fn convert(body: &str) -> Value {
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
    let aat =
        serde_json::from_slice(&ab_aat::aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
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
fn observed_typeface_and_size_spellings_have_typed_meaning() {
    for (body, kind, field, expected) in [
        (
            "井伏鱒二［＃「井伏鱒二」はゴチック］",
            "emphasis",
            "/style",
            "gothic",
        ),
        (
            "阿［＃「阿」は一段階小さな文字］",
            "layout-span",
            "/layout/size_type",
            "small",
        ),
        (
            "な［＃「な」は小書き］",
            "layout-span",
            "/layout/size",
            "small",
        ),
        (
            "引［＃「引」は小書き右寄せ］",
            "layout-span",
            "/layout/position",
            "right",
        ),
    ] {
        let ir = convert(body);
        assert_eq!(ir["nodes"][0]["type"], kind, "{body}");
        assert_eq!(ir["nodes"][0].pointer(field).unwrap(), expected, "{body}");
        assert_eq!(
            ir["interpretation_problems"],
            serde_json::json!([]),
            "{body}"
        );
    }
}

#[test]
fn heading_close_with_elided_okurigana_closes_the_original_scope() {
    let ir = convert("［＃中見出し］章《しょう》［＃中見出終わり］\n本文。");
    assert_eq!(ir["nodes"][0]["type"], "heading");
    assert_eq!(ir["nodes"][0]["text"], "章");
    assert_eq!(ir["interpretation_problems"], serde_json::json!([]));
}
