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
fn native_typeface_and_script_values_are_not_generic_emphasis() {
    for (source_kind, style) in [
        ("ゴシック体", "gothic"),
        ("斜体", "italic"),
        ("上付き小文字", "superscript"),
        ("下付き小文字", "subscript"),
    ] {
        let ir = convert(&format!("字［＃「字」は{source_kind}］"));
        assert!(
            nodes(&ir)
                .iter()
                .any(|n| n["type"] == "emphasis" && n["style"] == style && n["text"] == "字"),
            "{source_kind}: {ir}"
        );
        assert_eq!(ir["interpretation_problems"], json!([]));
    }
}

#[test]
fn relative_absolute_font_size_and_source_side_have_exclusive_payloads() {
    for (source_kind, expected) in [
        (
            "２段階小さな文字",
            json!({"kind":"font-size", "source":"aat-inline", "size_type":"small", "level":2}),
        ),
        (
            "１段階大きな文字",
            json!({"kind":"font-size", "source":"aat-inline", "size_type":"large", "level":1}),
        ),
        (
            "小文字",
            json!({"kind":"font-size", "source":"aat-inline", "size_type":"absolute", "size":"small"}),
        ),
        (
            "行右小書き",
            json!({"kind":"small-script", "source":"aat-inline", "position":"right"}),
        ),
        (
            "行左小書き",
            json!({"kind":"small-script", "source":"aat-inline", "position":"left"}),
        ),
    ] {
        let ir = convert(&format!("字［＃「字」は{source_kind}］"));
        assert!(
            nodes(&ir).iter().any(|n| n["type"] == "layout-span"
                && n["layout"] == expected
                && n["text"] == "字"),
            "{source_kind}: {ir}"
        );
        assert_eq!(ir["interpretation_problems"], json!([]));
    }
}

#[test]
fn paired_rich_regions_preserve_children_and_unmatched_scopes_remain_explicit() {
    for (open, close, kind, field, expected) in [
        ("斜体", "斜体終わり", "emphasis", "style", json!("italic")),
        (
            "行右小書き",
            "行右小書き終わり",
            "layout-span",
            "layout",
            json!({"kind":"small-script", "source":"aat-inline", "position":"right"}),
        ),
        (
            "縦中横",
            "縦中横終わり",
            "layout-span",
            "layout",
            json!({"kind":"tcy", "source":"aat-inline", "marker":null}),
        ),
    ] {
        let ir = convert(&format!(
            "前［＃{open}］｜※［＃歌記号、1-3-28］字《じ》［＃{close}］後"
        ));
        let all = nodes(&ir);
        let wrapper = all
            .iter()
            .find(|n| n["type"] == kind && n[field] == expected)
            .expect("typed rich wrapper");
        assert_eq!(wrapper["text"], "〽字", "{ir}");
        assert!(nodes(wrapper).iter().any(|n| n["type"] == "ruby"));
        assert!(nodes(wrapper).iter().any(|n| n["type"] == "gaiji"));
        assert_eq!(ir["interpretation_problems"], json!([]), "{ir}");
        let unclosed = convert(&format!("前［＃{open}］｜※［＃歌記号、1-3-28］字《じ》後"));
        assert!(
            !unclosed["interpretation_problems"]
                .as_array()
                .unwrap()
                .is_empty()
        );
    }
}

#[test]
fn unmatched_caption_target_keeps_source_and_never_gets_an_established_fact() {
    let ir = convert("図［＃「別」はキャプション］");
    assert_eq!(
        ir["interpretation_problems"][0]["raw"],
        "［＃「別」はキャプション］"
    );
    assert_eq!(
        ir["interpretation_problems"][0]["aspects"],
        json!(["structure", "layout"])
    );
    assert!(
        !ir["interpretation_facts"]
            .as_array()
            .unwrap()
            .iter()
            .any(|f| f["kind"] == "caption")
    );
}

#[test]
fn font_size_schema_rejects_mixed_absolute_and_relative_payloads() {
    fn alter(value: &mut Value, field: &str, payload: &Value) {
        match value {
            Value::Object(map) => {
                if map.get("kind").and_then(Value::as_str) == Some("font-size") {
                    map.insert(field.into(), payload.clone());
                } else {
                    for child in map.values_mut() {
                        alter(child, field, payload);
                    }
                }
            }
            Value::Array(values) => {
                for child in values {
                    alter(child, field, payload);
                }
            }
            _ => {}
        }
    }
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let schemas = SchemaSet::for_aat_version(&repo, None, 2).unwrap();
    for (source, field, value) in [
        ("字［＃「字」は小文字］", "level", json!(1)),
        ("字［＃「字」は２段階小さな文字］", "size", json!("small")),
    ] {
        let mut ir = convert(source);
        alter(&mut ir, field, &value);
        assert!(
            ab_aat_to_parser_ir::schema::validate_value(
                &schemas.parser_ir_schema,
                &ir,
                "parser-ir"
            )
            .is_err()
        );
    }
}

#[test]
fn inline_tcy_does_not_terminate_enclosing_indentation() {
    for (open, layout) in [
        (
            "ここから１字下げ",
            json!({"kind":"jisage", "source":"aat-block", "indent":1}),
        ),
        (
            "ここから１字下げ、折り返して２字下げ",
            json!({"kind":"burasage", "source":"aat-style", "first_line_indent":1, "continuation_indent":2}),
        ),
    ] {
        let ir = convert(&format!(
            "［＃{open}］\nビタミン［＃縦中横］B1［＃「1」は下付き小文字］［＃縦中横終わり］　二ミリグラム\n次の行。\n［＃ここで字下げ終わり］"
        ));
        let tcy_index = ir["nodes"]
            .as_array()
            .unwrap()
            .iter()
            .position(|node| node["layout"]["kind"] == "tcy")
            .unwrap();
        let paragraph = ir["paragraphs"]
            .as_array()
            .unwrap()
            .iter()
            .find(|paragraph| {
                paragraph["node_range"]["start"].as_u64().unwrap() <= tcy_index as u64
                    && paragraph["node_range"]["end"].as_u64().unwrap() > tcy_index as u64
            })
            .unwrap();
        assert_eq!(paragraph["layout"], layout, "{ir}");
        let start = usize::try_from(paragraph["node_range"]["start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(paragraph["node_range"]["end"].as_u64().unwrap()).unwrap();
        let text: String = ir["nodes"].as_array().unwrap()[start..end]
            .iter()
            .filter_map(|node| node["text"].as_str())
            .collect();
        assert_eq!(text.trim_matches('\n'), "ビタミンB1　二ミリグラム");
        let next = ir["paragraphs"]
            .as_array()
            .unwrap()
            .iter()
            .find(|p| p["node_range"]["start"].as_u64() == Some(end as u64))
            .unwrap();
        assert_eq!(next["layout"], layout);
        let next_end = usize::try_from(next["node_range"]["end"].as_u64().unwrap()).unwrap();
        let next_text: String = ir["nodes"].as_array().unwrap()[end..next_end]
            .iter()
            .filter_map(|node| node["text"].as_str())
            .collect();
        assert_eq!(next_text.trim_matches('\n'), "次の行。");
        assert_eq!(ir["interpretation_problems"], json!([]));
    }
}

#[test]
fn paired_font_size_preserves_leading_space_and_enclosing_layout() {
    let ir = convert(
        "［＃ここから１字下げ、折り返して２字下げ］\n前の行。\n　［＃１段階小さな文字］〔中略〕［＃小さな文字終わり］続き。\n［＃ここで字下げ終わり］",
    );
    let nodes = ir["nodes"].as_array().unwrap();
    let index = nodes
        .iter()
        .position(|n| n["layout"]["kind"] == "font-size")
        .unwrap();
    let paragraph = ir["paragraphs"]
        .as_array()
        .unwrap()
        .iter()
        .find(|p| {
            p["node_range"]["start"].as_u64().unwrap() <= index as u64
                && p["node_range"]["end"].as_u64().unwrap() > index as u64
        })
        .unwrap();
    assert_eq!(paragraph["layout"]["kind"], "burasage");
    assert_eq!(paragraph["layout"]["first_line_indent"], 1);
    assert_eq!(paragraph["layout"]["continuation_indent"], 2);
    let start = usize::try_from(paragraph["node_range"]["start"].as_u64().unwrap()).unwrap();
    let end = usize::try_from(paragraph["node_range"]["end"].as_u64().unwrap()).unwrap();
    let text: String = nodes[start..end]
        .iter()
        .filter_map(|n| n["text"].as_str())
        .collect();
    assert_eq!(text.trim_matches('\n'), "　〔中略〕続き。");
}

#[test]
fn compound_attributes_share_one_target_and_exact_marker() {
    let marker = "［＃「１）」は縦中横、行右小書き］";
    let ir = convert(&format!("前１）{marker}後。"));
    let all = nodes(&ir);
    let compound = all
        .iter()
        .filter(|node| node["type"] == "layout-span" && node["layout"].is_array())
        .collect::<Vec<_>>();
    assert_eq!(compound.len(), 1, "{ir}");
    assert_eq!(compound[0]["text"], "１）");
    assert_eq!(
        compound[0]["layout"],
        json!([
            {"kind":"tcy","source":"aat-inline","marker":null},
            {"kind":"small-script","source":"aat-inline","position":"right"}
        ])
    );
    assert_eq!(ir["interpretation_problems"], json!([]));
    assert!(
        ir["interpretation_facts"]
            .as_array()
            .unwrap()
            .iter()
            .any(|fact| {
                let source = format!("題\n作者\n\n前１）{marker}後。\n\n底本：本\n");
                let span = &fact["source_span"];
                &source[span["start"].as_u64().unwrap() as usize
                    ..span["end"].as_u64().unwrap() as usize]
                    == marker
            })
    );
}

#[test]
fn unsupported_compound_clauses_and_missing_targets_remain_explicit() {
    for body in [
        "１）［＃「１）」は縦中横、未知の書式］",
        "前［＃「１）」は縦中横、行右小書き］",
    ] {
        let ir = convert(body);
        assert!(
            !ir["interpretation_problems"].as_array().unwrap().is_empty(),
            "{ir}"
        );
        assert!(
            !nodes(&ir)
                .iter()
                .any(|node| node["type"] == "layout-span" && node["layout"].is_array())
        );
    }
}
