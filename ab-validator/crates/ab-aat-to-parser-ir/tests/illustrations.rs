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
fn illustration_payload_is_metadata_and_does_not_consume_principal_offsets() {
    let ir = convert("前［＃お神籤の図（fig50556_01.png、横320×縦322）入る］後");
    let image = nodes(&ir)
        .into_iter()
        .find(|n| n["type"] == "image")
        .unwrap();
    assert_eq!(image["src"], "fig50556_01.png");
    assert_eq!(image["alt"], "お神籤の図");
    assert_eq!(image["width"], 320);
    assert_eq!(image["height"], 322);
    assert_eq!(image["span"]["start"], image["span"]["end"]);
    assert!(
        ir["interpretation_facts"]
            .as_array()
            .unwrap()
            .iter()
            .any(|f| f["kind"] == "illustration" && f["source_span"] == image["source_span"])
    );
    assert_eq!(ir["interpretation_problems"], json!([]));
}

#[test]
fn quoted_caption_reference_reuses_rich_metadata_axis_without_a_second_body_caption() {
    let ir = convert(
        "［＃「漢《かん》の図」のキャプション付きの図（fig1.png、横10×縦20）入る］\n漢《かん》の図［＃「漢の図」はキャプション］",
    );
    let image = nodes(&ir)
        .into_iter()
        .find(|n| n["type"] == "image")
        .unwrap();
    assert_eq!(image["src"], "fig1.png");
    assert_eq!(image["width"], 10);
    assert_eq!(image["height"], 20);
    let ruby = &image["caption_reference_children"][0];
    assert_eq!(ruby["type"], "ruby");
    assert_eq!(ruby["ruby"]["reading"], "かん");
    assert_eq!(ruby["span"]["coordinate_system"], "annotation_utf8");
    assert_eq!(ir["interpretation_problems"], json!([]));
}

#[test]
fn unsupported_dimensions_keep_the_source_claim_without_guessing_geometry() {
    let ir = convert("前［＃図（fig1.png、横不明×縦20）入る］後");
    let image = nodes(&ir)
        .into_iter()
        .find(|n| n["type"] == "image")
        .unwrap();
    assert_eq!(image["dimensions_source"], "横不明×縦20");
    assert!(image.get("width").is_none());
    assert_eq!(ir["interpretation_problems"].as_array().unwrap().len(), 1);
}

#[test]
fn general_description_and_trailing_reference_keep_native_fragment_provenance() {
    for (body, key) in [
        (
            "前［＃漢《かん》の図（fig1.png）入る］後",
            "description_children",
        ),
        (
            "前［＃挿絵（fig1.png）「漢《かん》」入る］後",
            "caption_reference_children",
        ),
    ] {
        let ir = convert(body);
        let image = nodes(&ir)
            .into_iter()
            .find(|n| n["type"] == "image")
            .unwrap();
        assert_eq!(image[key][0]["type"], "ruby");
        assert_eq!(image[key][0]["ruby"]["reading"], "かん");
        assert_eq!(
            image[key][0]["span"]["coordinate_system"],
            "annotation_utf8"
        );
        assert!(image[key][0].get("source_span").is_some());
        assert_eq!(ir["interpretation_problems"], json!([]));
    }
}

#[test]
fn unknown_image_metadata_retains_all_uncertain_aspects() {
    let ir = convert("前［＃漢［＃未知の説明］の図（fig1.png、横不明×縦20）入る］後");
    let image = nodes(&ir)
        .into_iter()
        .find(|n| n["type"] == "image")
        .unwrap();
    assert_eq!(image["description_source"], "漢［＃未知の説明］の図");
    assert_eq!(image["alt"], "");
    assert_eq!(
        ir["interpretation_problems"][0]["aspects"],
        json!(["structure", "layout"])
    );
    assert!(
        !ir["interpretation_facts"]
            .as_array()
            .unwrap()
            .iter()
            .any(|fact| fact["kind"] == "illustration")
    );
}

#[test]
fn image_lettering_edition_statement_stays_on_the_annotation_axis() {
    let marker = "［＃「脳髄の大きさの比較」の図（fig18353_07.png）入る。「（実物の二分の一大）」とあるのは底本では「（実物の五分の二大）」］";
    let ir = convert(&format!("前{marker}後"));
    let image = nodes(&ir)
        .into_iter()
        .find(|n| n["type"] == "image")
        .unwrap();
    assert_eq!(image["src"], "fig18353_07.png");
    assert_eq!(image["alt"], "「脳髄の大きさの比較」の図");
    let note = &image["annotation_children"][0];
    assert_eq!(note["type"], "editor-note");
    assert_eq!(note["note_kind"], "base-edition");
    assert_eq!(
        note["text"],
        "「（実物の二分の一大）」とあるのは底本では「（実物の五分の二大）」"
    );
    assert_eq!(note["span"]["coordinate_system"], "annotation_utf8");
    assert_eq!(note["span"]["start"], note["span"]["end"]);
    assert_eq!(note["source_span"], image["source_span"]);
    assert!(ir["interpretation_problems"].as_array().unwrap().is_empty());
}

#[test]
fn asset_clause_shapes_are_distinguished_from_each_other() {
    // The four shapes the archive actually contains. Only the last two are
    // defective, and each is defective in its own clause: 018371 writes the
    // dimensions clause and leaves it unfilled, and 045338 writes a stem where
    // the filename belongs. Neither can be repaired without inventing the
    // value the source failed to supply.
    let well_formed = convert("［＃挿絵（fig_ok.png、横441×縦233）入る］");
    let image = nodes(&well_formed)
        .into_iter()
        .find(|n| n["type"] == "image")
        .unwrap();
    assert_eq!(image["src"], "fig_ok.png");
    assert_eq!(image["width"], 441);
    assert!(image.get("src_source").is_none());
    assert!(image.get("dimensions_source").is_none());
    assert!(
        well_formed["interpretation_problems"]
            .as_array()
            .unwrap()
            .is_empty()
    );

    // A dimensions clause the source simply does not write is absent, not
    // defective, and says nothing about the work.
    let absent = convert("［＃挿絵（fig_ok.png）入る］");
    let image = nodes(&absent)
        .into_iter()
        .find(|n| n["type"] == "image")
        .unwrap();
    assert_eq!(image["src"], "fig_ok.png");
    assert!(image.get("width").is_none());
    assert!(image.get("dimensions_source").is_none());
    assert!(
        absent["interpretation_problems"]
            .as_array()
            .unwrap()
            .is_empty()
    );

    // Declared and left empty: the raw clause is kept and the layout aspect
    // is uncertain, but the filename still resolves.
    let empty_dimensions = convert(
        "［＃「朝鮮慶州金冠塚發見の王冠」のキャプション付きの図（fig18371_01.png、横×縦）入る］",
    );
    let image = nodes(&empty_dimensions)
        .into_iter()
        .find(|n| n["type"] == "image")
        .unwrap();
    assert_eq!(image["src"], "fig18371_01.png");
    assert!(image.get("width").is_none());
    assert_eq!(image["dimensions_source"], "横×縦");
    assert!(image.get("src_source").is_none());
    assert_eq!(
        empty_dimensions["interpretation_problems"][0]["aspects"],
        json!(["layout"])
    );

    // A stem with no extension separator names no file, so the asset identity
    // is the uncertain one and no `src` is asserted.
    let stem_only = convert("［＃ひめだるまの写真（fig45338_01png、横441×縦233）入る］");
    let image = nodes(&stem_only)
        .into_iter()
        .find(|n| n["type"] == "image")
        .unwrap();
    assert_eq!(image["src"], Value::Null);
    assert_eq!(image["src_source"], "fig45338_01png");
    assert_eq!(image["width"], 441);
    assert_eq!(image["height"], 233);
    assert_eq!(
        stem_only["interpretation_problems"][0]["aspects"],
        json!(["structure"])
    );
}

#[test]
fn both_clauses_can_be_defective_in_one_marker() {
    // 018371 does this twice: an unfilled dimensions clause and a stem-only
    // filename in the same marker. The aspects are reported together, in the
    // schema's own order.
    let ir =
        convert("［＃「第五十圖　支那古錢」のキャプション付きの図（fig18371_48png、横×縦）入る］");
    let image = nodes(&ir)
        .into_iter()
        .find(|n| n["type"] == "image")
        .unwrap();
    assert_eq!(image["src"], Value::Null);
    assert_eq!(image["src_source"], "fig18371_48png");
    assert_eq!(image["dimensions_source"], "横×縦");
    assert!(image.get("width").is_none());
    assert_eq!(
        ir["interpretation_problems"][0]["aspects"],
        json!(["structure", "layout"])
    );
}
