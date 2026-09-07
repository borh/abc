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
fn witness_alternative_retains_a_whole_styled_target() {
    let ir = convert(
        "ΩIV［＃「IV」は上付き小文字］［＃「IV」は底本では「VI」］k［＃「k」は下付き小文字］",
    );
    let all = nodes(&ir);
    let app = all
        .iter()
        .find(|node| node["type"] == "base-text-variant")
        .unwrap();
    assert_eq!(app["text"], "IV");
    assert_eq!(app["variant"]["base_text"], "VI");
    assert_eq!(app["inline_children"][0]["style"], "superscript");
    assert_eq!(ir["interpretation_problems"], json!([]));
}

#[test]
fn explicit_witness_absence_retains_principal_text() {
    let ir = convert("出来ない。［＃「。」は底本では欠落］後。");
    let all = nodes(&ir);
    let app = all
        .iter()
        .find(|node| node["type"] == "base-text-variant")
        .unwrap();
    assert_eq!(app["text"], "。");
    assert_eq!(app["variant"]["base_text"], "");
    assert_eq!(ir["interpretation_problems"], json!([]));
}

#[test]
fn typed_witness_apparatus_does_not_hide_an_independent_target() {
    let ir = convert("覆われた２）［＃「)」は底本では欠落］［＃「２）」は縦中横、行右小書き］。");
    let all = nodes(&ir);
    let compound = all
        .iter()
        .find(|node| node["type"] == "layout-span" && node["layout"].is_array())
        .unwrap();
    assert_eq!(compound["text"], "２）");
    let note = compound["inline_children"]
        .as_array()
        .unwrap()
        .last()
        .unwrap();
    assert_eq!(note["type"], "editor-note");
    assert_eq!(note["note"]["raw"], "［＃「)」は底本では欠落］");
    let problems = ir["interpretation_problems"].as_array().unwrap();
    assert_eq!(problems.len(), 1);
    assert_eq!(problems[0]["raw"], "［＃「)」は底本では欠落］");
}

#[test]
fn unknown_apparatus_and_partial_rich_targets_remain_unresolved() {
    for body in [
        "字［＃「字」は未知の状態］［＃「字」は底本では「別」］",
        "AB［＃「AB」は上付き小文字］［＃「B」は底本では「C」］",
    ] {
        let ir = convert(body);
        assert!(
            !nodes(&ir)
                .iter()
                .any(|node| node["type"] == "base-text-variant")
        );
        assert!(!ir["interpretation_problems"].as_array().unwrap().is_empty());
    }
}

#[test]
fn ruby_variant_uses_the_nearest_reading_axis_without_consuming_okurigana() {
    for body in [
        "人を過《あや》め［＃ルビの「あや」は底本では「なや」］後。",
        "｜くだらないこと《ウンジン》を［＃ルビの「ウンジン」は底本では「ウンジイ」］！",
    ] {
        let ir = convert(body);
        let all = nodes(&ir);
        let ruby = all.iter().find(|node| node["type"] == "ruby").unwrap();
        assert_eq!(
            ruby["reading_children"][0]["type"], "base-text-variant",
            "{ir}"
        );
        assert_eq!(
            ruby["reading_children"][0]["span"]["coordinate_system"],
            "reading_utf8"
        );
        assert_eq!(ir["interpretation_problems"], json!([]));
    }
}

#[test]
fn reading_target_does_not_cross_a_mismatch_line_or_rich_boundary() {
    for body in [
        "過《あや》字《じ》［＃ルビの「あや」は底本では「なや」］",
        "過《あや》\nめ［＃ルビの「あや」は底本では「なや」］",
        "弾正《だんじょうだいひつ》［＃ルビの「だんじょう」は底本では「だんじゅう」］",
    ] {
        let ir = convert(body);
        assert!(
            !nodes(&ir)
                .iter()
                .any(|node| node["type"] == "base-text-variant")
        );
        assert!(!ir["interpretation_problems"].as_array().unwrap().is_empty());
    }
}

#[test]
fn rich_witness_reuses_ruby_structure_on_an_independent_axis() {
    let body = "「何を狼狽《あわ》てて［＃「狼狽《あわ》てて」は底本では「狼狙《あわ》てて」］";
    let ir = convert(body);
    let all = nodes(&ir);
    let app = all
        .iter()
        .find(|node| node["type"] == "base-text-variant")
        .unwrap();
    assert_eq!(app["text"], "狼狽てて");
    assert_eq!(app["variant"]["base_text"], "狼狙てて");
    let witness = &app["variant"]["base_children"];
    assert_eq!(witness[0]["type"], "ruby");
    assert_eq!(witness[0]["ruby"]["base"], "狼狙");
    assert_eq!(witness[0]["ruby"]["reading"], "あわ");
    assert_eq!(
        witness[0]["span"],
        json!({"start":0,"end":6,"coordinate_system":"witness_utf8"})
    );
    assert_eq!(witness[1]["span"]["start"], 6);
    assert_eq!(
        app["inline_children"][0]["span"]["coordinate_system"],
        "parser_text_utf8"
    );
    let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
    let start = source.find("狼狙《あわ》").unwrap();
    assert_eq!(witness[0]["source_span"]["start"], start);
    assert_eq!(
        witness[0]["source_span"]["end"],
        start + "狼狙《あわ》".len()
    );
    assert_eq!(ir["interpretation_problems"], json!([]));
    let fact = ir["interpretation_facts"]
        .as_array()
        .unwrap()
        .iter()
        .find(|fact| fact["kind"] == "text-variant")
        .unwrap();
    assert_eq!(fact["source_span"]["start"], source.find("［＃").unwrap());
    assert_eq!(
        fact["source_span"]["end"],
        source.find('］').unwrap() + '］'.len_utf8()
    );
}

#[test]
fn quoted_ruby_identity_must_match_the_actual_reading() {
    for body in [
        "狼狽《ろうばい》てて［＃「狼狽《あわ》てて」は底本では「狼狙《あわ》てて」］",
        "狼狽てて［＃「狼狽《あわ》てて」は底本では「狼狙《あわ》てて」］",
    ] {
        let ir = convert(body);
        assert!(
            !nodes(&ir)
                .iter()
                .any(|node| node["type"] == "base-text-variant")
        );
        assert_eq!(
            ir["interpretation_problems"][0]["kind"],
            "unresolved-variant"
        );
        assert!(
            !ir["interpretation_facts"]
                .as_array()
                .unwrap()
                .iter()
                .any(|fact| fact["kind"] == "text-variant")
        );
    }
}

#[test]
fn plain_target_can_have_a_rich_witness_without_changing_principal_text() {
    let ir = convert("居た［＃「居た」は底本では「居《ゐ》つた」］後。 ");
    let all = nodes(&ir);
    let app = all
        .iter()
        .find(|node| node["type"] == "base-text-variant")
        .unwrap();
    assert_eq!(app["text"], "居た");
    assert_eq!(app["variant"]["base_text"], "居つた");
    assert_eq!(app["variant"]["base_children"][0]["ruby"]["reading"], "ゐ");
    assert_eq!(ir["interpretation_problems"], json!([]));
}
