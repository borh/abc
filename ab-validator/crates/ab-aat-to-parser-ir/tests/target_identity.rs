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
fn unknown_apparatus_and_ambiguous_rich_targets_remain_unresolved() {
    for body in [
        "字［＃「字」は未知の状態］［＃「字」は底本では「別」］",
        "ABAB［＃「ABAB」は上付き小文字］［＃「B」は底本では「C」］",
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
fn reading_target_does_not_cross_a_mismatch_line_or_ambiguous_reading() {
    for body in [
        "過《あや》字《じ》［＃ルビの「あや」は底本では「なや」］",
        "過《あや》\nめ［＃ルビの「あや」は底本では「なや」］",
        "弾正《だんじょうだんじょう》［＃ルビの「だんじょう」は底本では「だんじゅう」］",
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

#[test]
fn literal_quote_targets_and_absence_wording_resolve_without_balancing_text_quotes() {
    for (body, current, witness) in [
        ("終り……』［＃「……』」は底本では「……」」］", "……』", "……」"),
        ("』［＃「』」は、底本では「」」］", "』", "」"),
        ("「［＃「「」は底本では脱落］", "「", ""),
    ] {
        let ir = convert(body);
        let all = nodes(&ir);
        let app = all
            .iter()
            .find(|node| node["type"] == "base-text-variant")
            .unwrap_or_else(|| panic!("{body}: {ir}"));
        assert_eq!(app["text"], current);
        assert_eq!(app["variant"]["base_text"], witness);
        assert_eq!(ir["interpretation_problems"], json!([]));
    }
}

#[test]
fn base_edition_statements_are_apparatus_not_principal_layout() {
    let ir = convert("本文［＃底本では４字下げ］続き。");
    let all = nodes(&ir);
    let note = all
        .iter()
        .find(|node| node["note_kind"] == "base-edition")
        .unwrap();
    assert_eq!(note["type"], "editor-note");
    assert_eq!(note["text"], "底本では４字下げ");
    assert_eq!(note["span"]["start"], note["span"]["end"]);
    assert!(!all.iter().any(|node| node["type"] == "layout-span"));
    assert_eq!(ir["interpretation_problems"], json!([]));
    assert!(
        ir["interpretation_facts"]
            .as_array()
            .unwrap()
            .iter()
            .any(|fact| fact["kind"] == "editorial-note"
                && fact["source_span"] == note["source_span"])
    );
}

#[test]
fn source_error_assertion_survives_beside_the_structured_alternative() {
    let ir = convert("あっちイ［＃「あっちイ」は底本では「あつちイ」と誤記］後。");
    let all = nodes(&ir);
    let app = all
        .iter()
        .find(|node| node["type"] == "base-text-variant")
        .unwrap();
    let note = all
        .iter()
        .find(|node| node["note_kind"] == "base-edition")
        .unwrap();
    assert_eq!(app["text"], "あっちイ");
    assert_eq!(app["variant"]["base_text"], "あつちイ");
    assert_eq!(note["text"], "「あっちイ」は底本では「あつちイ」と誤記");
    assert_eq!(app["source_span"], note["source_span"]);
    assert_eq!(ir["interpretation_problems"], json!([]));
}

#[test]
fn mixed_current_formatting_and_malformed_variants_are_not_discharged_as_prose() {
    for body in [
        "１）［＃「１）」は縦中横、未知の属性、「１」が底本では欠落］",
        "字［＃「字」は底本では「他］",
    ] {
        let ir = convert(body);
        assert!(
            !nodes(&ir)
                .iter()
                .any(|node| node["note_kind"] == "base-edition")
        );
        assert!(!ir["interpretation_problems"].as_array().unwrap().is_empty());
    }
}

#[test]
fn unique_principal_subrange_preserves_the_whole_ruby_reading() {
    for (body, base, reading, target, witness) in [
        (
            "渡辺崋山《わたなべかざん》［＃「崋山」は底本では「華山」］も",
            "渡辺崋山",
            "わたなべかざん",
            "崋山",
            "華山",
        ),
        (
            "｜溌剌《はつらつ》［＃「剌」は底本では「刺」］たる",
            "溌剌",
            "はつらつ",
            "剌",
            "刺",
        ),
        (
            "甲乙丙《こうおつへい》［＃「乙」は底本では「オツ」］",
            "甲乙丙",
            "こうおつへい",
            "乙",
            "オツ",
        ),
    ] {
        let ir = convert(body);
        let all = nodes(&ir);
        let rubies = all
            .iter()
            .filter(|node| node["type"] == "ruby")
            .collect::<Vec<_>>();
        assert_eq!(rubies.len(), 1);
        assert_eq!(rubies[0]["ruby"]["base"], base);
        assert_eq!(rubies[0]["ruby"]["reading"], reading);
        let app = rubies[0]["inline_children"]
            .as_array()
            .unwrap()
            .iter()
            .find(|node| node["type"] == "base-text-variant")
            .unwrap();
        assert_eq!(app["text"], target);
        assert_eq!(app["variant"]["base_text"], witness);
        let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
        let position = source.find(target).unwrap();
        assert_eq!(app["inline_children"][0]["source_span"]["start"], position);
        assert_eq!(
            app["inline_children"][0]["source_span"]["end"],
            position + target.len()
        );
        assert_eq!(ir["interpretation_problems"], json!([]));
    }
}

#[test]
fn repeated_principal_target_or_different_reading_cannot_establish_a_subrange() {
    for body in [
        "人人《ひとびと》［＃「人」は底本では「者」］",
        "甲乙丙《こうおつへい》［＃「乙《おつ》」は底本では「オツ」］",
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
    }
}

#[test]
fn edition_statements_preserve_attribution_and_never_apply_base_only_formatting() {
    for (body, kind) in [
        ("b［＃「b」は底本では上付き小文字］", "base-edition"),
        ("行。［＃この行は底本では天付き］", "base-edition"),
        (
            "かづきぬ［＃「かづきぬ」は初出では「かつぎぬ」］",
            "first-publication",
        ),
        (
            "臙脂色《えんじいろ》［＃ルビの「えんじいろ」は初出では「ゑんじいろ」］",
            "first-publication",
        ),
    ] {
        let ir = convert(body);
        let all = nodes(&ir);
        let note = all.iter().find(|node| node["note_kind"] == kind).unwrap();
        assert_eq!(note["span"]["start"], note["span"]["end"]);
        assert_eq!(
            note["text"],
            body.split_once("［＃")
                .unwrap()
                .1
                .strip_suffix('］')
                .unwrap()
        );
        assert!(
            !all.iter()
                .any(|node| node["type"] == "layout-span" || node["type"] == "base-text-variant")
        );
        assert_eq!(ir["interpretation_problems"], json!([]));
    }
}

#[test]
fn quoted_ruby_delimiters_address_the_existing_reading_axis() {
    let ir = convert("躓《つまず》［＃「《つまず》」は底本では「《つまづ》」］き");
    let all = nodes(&ir);
    let ruby = all.iter().find(|node| node["type"] == "ruby").unwrap();
    let app = &ruby["reading_children"][0];
    assert_eq!(app["type"], "base-text-variant");
    assert_eq!(app["text"], "つまず");
    assert_eq!(app["variant"]["base_text"], "つまづ");
    assert_eq!(app["span"]["coordinate_system"], "reading_utf8");
    assert_eq!(ir["interpretation_problems"], json!([]));
}

#[test]
fn target_identity_retains_terminal_kunten_without_giving_it_principal_width() {
    for (body, text, witness_mark) in [
        (
            "厩中［＃一］［＃「厩中［＃一］」は底本では「厩中［＃二］」］",
            "厩中",
            "二",
        ),
        (
            "顆一［＃一］［＃「顆一［＃一］」は底本では「顆一」］",
            "顆一",
            "",
        ),
        (
            "魂気帰［＃二］於天［＃一］［＃「魂気帰［＃二］於天［＃一］」は底本では「魂気帰［＃レ］於天［＃一］」］",
            "魂気帰於天",
            "レ一",
        ),
    ] {
        let ir = convert(body);
        let all = nodes(&ir);
        let app = all
            .iter()
            .find(|node| node["type"] == "base-text-variant")
            .unwrap();
        assert_eq!(app["text"], text);
        assert_eq!(app["variant"]["base_text"], text);
        let witness = app["variant"]["base_children"].as_array().unwrap();
        let marks = witness
            .iter()
            .filter(|node| node["type"] == "kunten")
            .collect::<Vec<_>>();
        assert_eq!(
            marks
                .iter()
                .map(|node| node["text"].as_str().unwrap())
                .collect::<String>(),
            witness_mark
        );
        assert!(
            marks
                .iter()
                .all(|node| node["span"]["start"] == node["span"]["end"]
                    && node["span"]["coordinate_system"] == "witness_utf8")
        );
        assert_eq!(ir["interpretation_problems"], json!([]));
    }
}

#[test]
fn qualified_and_bibliographic_statements_do_not_fabricate_adjacent_targets() {
    for statement in [
        "第三段目「□４□」は底本では「□５□」",
        "八段目九段目左端「１」は底本では「□」",
        "注記の「く」は底本では欠落",
        "ルビの「まへ」の「へ」は底本では左に九十度傾いている",
        "この行「｛｝」に挟まれ「／」で区切られた要素は、底本では真横に並ぶ",
        "「麗艶」は底本では「艶麗」。以下の本では「麗艶」。『鏡花全集　卷五』（岩波書店）",
    ] {
        let ir = convert(&format!("無関係な本文。［＃{statement}］後。"));
        let all = nodes(&ir);
        let note = all
            .iter()
            .find(|node| node["note_kind"] == "base-edition")
            .unwrap();
        assert_eq!(note["text"], statement);
        assert_eq!(note["span"]["start"], note["span"]["end"]);
        assert!(!all.iter().any(|node| node["type"] == "base-text-variant"));
        assert_eq!(ir["interpretation_problems"], json!([]));
    }
    for statement in [
        "ルビの「違う」は底本では「別」",
        "ここから２字下げ、底本では一行目は１字下げ",
        "「１）」は縦中横、行右小書き、「１」が底本では欠落",
        "「字」は底本では「別",
    ] {
        let ir = convert(&format!("無関係な本文。［＃{statement}］後。"));
        assert!(
            !nodes(&ir)
                .iter()
                .any(|node| node["note_kind"] == "base-edition")
        );
    }
}

#[test]
fn mixed_tcy_clauses_preserve_formatting_and_whole_or_partial_witnesses() {
    for (body, current, witness) in [
        (
            "１）［＃「１）」は縦中横、行右小書き、底本では欠落］",
            "１）",
            "",
        ),
        (
            "２）［＃「２）」は縦中横、行右小書き、底本では「１）」］",
            "２）",
            "１）",
        ),
        (
            "１）［＃「１）」は縦中横、行右小書き、「１」が底本では欠落］",
            "１",
            "",
        ),
        (
            "３）［＃「３）」は縦中横、行右小書き、「）」が底本では欠落］",
            "）",
            "",
        ),
        (
            "１）［＃「１）」は縦中横、行右小書き、「１」が底本では「２」］",
            "１",
            "２",
        ),
    ] {
        let ir = convert(body);
        let all = nodes(&ir);
        let formatting = all
            .iter()
            .find(|node| node["type"] == "layout-span")
            .unwrap();
        assert_eq!(formatting["layout"][0]["kind"], "tcy");
        assert_eq!(formatting["layout"][1]["kind"], "small-script");
        assert_eq!(formatting["layout"][1]["position"], "right");
        let app = nodes(formatting)
            .into_iter()
            .find(|node| node["type"] == "base-text-variant")
            .unwrap();
        assert_eq!(app["text"], current);
        assert_eq!(app["variant"]["base_text"], witness);
        assert_eq!(ir["interpretation_problems"], json!([]));
    }
}

#[test]
fn mixed_edition_targets_require_unique_literal_source_evidence() {
    for body in [
        "１）［＃「１）」は縦中横、行右小書き、「９」が底本では欠落］",
        "１１）［＃「１１）」は縦中横、行右小書き、「１」が底本では欠落］",
    ] {
        let ir = convert(body);
        let all = nodes(&ir);
        assert!(all.iter().any(|node| node["type"] == "layout-span"));
        assert!(!all.iter().any(|node| node["type"] == "base-text-variant"));
        assert_eq!(
            ir["interpretation_problems"][0]["kind"],
            "unresolved-variant"
        );
    }
}

#[test]
fn unqualified_variant_can_address_the_immediately_adjacent_whole_reading() {
    for body in [
        "下根岸《しもねぎし》［＃「しもねぎし」は底本では「しもねがし」と誤記］",
        "腕車《くるま》［＃「くるま」は底本では「くまる」と誤記］",
    ] {
        let ir = convert(body);
        let all = nodes(&ir);
        let ruby = all.iter().find(|node| node["type"] == "ruby").unwrap();
        let reading = nodes(&ruby["reading_children"]);
        assert!(
            reading
                .iter()
                .any(|node| node["type"] == "base-text-variant")
        );
        assert!(reading.iter().any(|node| node["type"] == "editor-note"));
        assert_eq!(ir["interpretation_problems"], json!([]));
    }
    for body in [
        "下根岸《しもねぎし》へ［＃「しもねぎし」は底本では「しもねがし」］",
        "下根岸《しもねぎし》腕車《くるま》［＃「しもねぎし」は底本では「しもねがし」］",
        "下根岸《しもねぎし》［＃「ねぎし」は底本では「ねがし」］",
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
    }
}

#[test]
fn attributed_variants_preserve_qualified_editorial_wording() {
    let ir = convert("特殊化［＃「特殊化」は底本では「殊特化」となっている。誤記か］");
    let all = nodes(&ir);
    let app = all
        .iter()
        .find(|node| node["type"] == "base-text-variant")
        .unwrap();
    assert_eq!(app["variant"]["base_text"], "殊特化");
    let note = all
        .iter()
        .find(|node| node["type"] == "editor-note")
        .unwrap();
    assert_eq!(
        note["text"],
        "「特殊化」は底本では「殊特化」となっている。誤記か"
    );
    assert_eq!(ir["interpretation_problems"], json!([]));

    let ir = convert("ＡとＢとは［＃「ＡとＢとは」底本では「ＡととＢとは」］");
    let all = nodes(&ir);
    let app = all
        .iter()
        .find(|node| node["type"] == "base-text-variant")
        .unwrap();
    assert_eq!(app["text"], "ＡとＢとは");
    assert_eq!(app["variant"]["base_text"], "ＡととＢとは");
    assert_eq!(ir["interpretation_problems"], json!([]));
}

#[test]
fn a_witness_can_retain_an_unmapped_glyph_without_inventing_its_character() {
    let body = "貳朱《にしゅ》を［＃「貳朱を」は底本では「※［＃「弋＋頁」、74-10］朱を」］";
    let ir = convert(body);
    let all = nodes(&ir);
    let app = all
        .iter()
        .find(|node| node["type"] == "base-text-variant")
        .unwrap();
    assert_eq!(app["text"], "貳朱を");
    assert_eq!(app["variant"]["base_text"], "\u{fffc}朱を");
    let glyph = &app["variant"]["base_children"][0];
    assert_eq!(glyph["type"], "gaiji");
    assert!(glyph["gaiji"]["unicode"].is_null());
    assert_eq!(glyph["gaiji"]["raw_marker"], "弋＋頁");
    assert_eq!(glyph["span"]["coordinate_system"], "witness_utf8");
    assert_eq!(glyph["span"]["end"], 3);
    let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
    let start = usize::try_from(glyph["source_span"]["start"].as_u64().unwrap()).unwrap();
    let end = usize::try_from(glyph["source_span"]["end"].as_u64().unwrap()).unwrap();
    assert_eq!(&source[start..end], "※［＃「弋＋頁」、74-10］");
    assert_eq!(ir["interpretation_problems"], json!([]));
}

#[test]
fn component_substitution_glyphs_remain_structured_witnesses() {
    for description in [
        "「闃」の「目」に代えて「自」",
        "「贏」の「貝」に代えて「果」、（二）-27-3",
    ] {
        let marker = format!("※［＃{description}］");
        let body = format!("字［＃「字」は底本では「{marker}」］");
        let ir = convert(&body);
        let all = nodes(&ir);
        let app = all
            .iter()
            .find(|node| node["type"] == "base-text-variant")
            .unwrap();
        assert_eq!(app["text"], "字");
        assert_eq!(app["variant"]["base_text"], "\u{fffc}");
        let glyph = &app["variant"]["base_children"][0];
        assert_eq!(glyph["type"], "gaiji");
        assert!(glyph["gaiji"]["unicode"].is_null());
        assert_eq!(glyph["span"]["coordinate_system"], "witness_utf8");
        let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
        let start = usize::try_from(glyph["source_span"]["start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(glyph["source_span"]["end"].as_u64().unwrap()).unwrap();
        assert_eq!(&source[start..end], marker);
        assert_eq!(ir["interpretation_problems"], json!([]));
    }
}

#[test]
fn unknown_witness_directives_and_unknown_target_glyphs_are_not_inferred() {
    for body in [
        "字［＃「字」は底本では「字［＃未知の意味］」］",
        "※［＃「弋＋頁」、74-10］［＃「※［＃「木＋貝」、74-10］」は底本では「字」］",
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
fn source_variants_preserve_structured_warichu_in_both_readings() {
    let ir = convert(
        "（［＃割り注］「前篇」の「五　インド征服」［＃割り注終わり］）［＃「（［＃割り注］「前篇」の「五　インド征服」［＃割り注終わり］）」は底本では「（［＃割り注］五一頁参照［＃割り注終わり］）」］",
    );
    let all = nodes(&ir);
    let app = all
        .iter()
        .find(|node| node["type"] == "base-text-variant")
        .unwrap();
    assert_eq!(app["text"], "（「前篇」の「五　インド征服」）");
    assert_eq!(app["variant"]["base_text"], "（五一頁参照）");
    for children in [&app["inline_children"], &app["variant"]["base_children"]] {
        assert!(nodes(children).iter().any(|node| node["type"] == "warichu"));
    }
    assert_eq!(ir["interpretation_problems"], json!([]));
}

#[test]
fn normalized_accent_targets_keep_their_exact_original_source_extent() {
    for (body, expected_source) in [
        (
            "前〔Annette von Droste=Hu:lshoff［＃「Hu:lshoff」は底本では「Hu:lshoffs」］〕後",
            "Hu:lshoff",
        ),
        (
            "前 〔UN COUP D'OE&IL〕［＃「〔UN COUP D'OE&IL〕」は底本では「〔UN COUP D,OE&IL〕」］後",
            "UN COUP D'OE&IL",
        ),
    ] {
        let ir = convert(body);
        let all = nodes(&ir);
        let app = all
            .iter()
            .find(|node| node["type"] == "base-text-variant")
            .unwrap();
        let child = &app["inline_children"][0];
        let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
        let start = usize::try_from(child["source_span"]["start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(child["source_span"]["end"].as_u64().unwrap()).unwrap();
        let raw = &source[start..end];
        assert_eq!(raw, expected_source);
        assert!(!app["text"].as_str().unwrap().contains([':', '&']));
        assert_eq!(ir["interpretation_problems"], json!([]));
    }
    let ir = convert("前Hu:lshoff［＃「〔Hu:lshoff〕」は底本では「〔Hu:lshoffs〕」］");
    assert!(
        !nodes(&ir)
            .iter()
            .any(|node| node["type"] == "base-text-variant")
    );
    assert_eq!(
        ir["interpretation_problems"][0]["kind"],
        "unresolved-variant"
    );
}

#[test]
fn unresolved_principal_glyph_occupies_one_placeholder_and_retains_its_description() {
    let ir = convert("前※［＃「弋＋頁」、74-10］後");
    let all = nodes(&ir);
    let glyph = all.iter().find(|node| node["type"] == "gaiji").unwrap();
    assert_eq!(glyph["span"]["start"], 3);
    assert_eq!(glyph["span"]["end"], 6);
    assert_eq!(glyph["gaiji"]["raw_marker"], "弋＋頁");
    let after = all
        .iter()
        .find(|node| node["type"] == "text" && node["text"] == "後")
        .unwrap();
    assert_eq!(after["span"]["start"], 6);
}

#[test]
fn unique_variant_subranges_preserve_their_immediate_container() {
    for (body, current, base) in [
        (
            "非買同盟は不可能である［＃「非買同盟は不可能である」に傍点］［＃「非買同盟は」は底本では「非賣同盟は」］",
            "非買同盟は",
            "非賣同盟は",
        ),
        (
            "AB［＃「AB」は上付き小文字］［＃「B」は底本では「C」］",
            "B",
            "C",
        ),
    ] {
        let ir = convert(body);
        let all = nodes(&ir);
        let app = all
            .iter()
            .find(|node| node["type"] == "base-text-variant")
            .unwrap();
        assert_eq!(app["text"], current);
        assert_eq!(app["variant"]["base_text"], base);
        assert_eq!(ir["interpretation_problems"], json!([]));
        if body.contains("傍点") || body.contains("上付き") {
            let style = all.iter().find(|node| node["type"] == "emphasis").unwrap();
            assert!(
                nodes(style)
                    .iter()
                    .any(|node| node["type"] == "base-text-variant")
            );
        }
    }
    for body in [
        "名刺と名刺を［＃「名刺」は底本では「名剌」］",
        "名刺\n紙入を［＃「名刺」は底本では「名剌」］",
        "名刺［＃未知の対象］紙入を［＃「名刺」は底本では「名剌」］",
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
fn explicit_reading_subrange_keeps_the_complete_ruby_association() {
    let body = "蠣崎波響《かきざきはきやう》［＃ルビの「かきざき」は底本では「かきさき」］";
    let ir = convert(body);
    let all = nodes(&ir);
    let ruby = all.iter().find(|node| node["type"] == "ruby").unwrap();
    assert_eq!(ruby["ruby"]["base"], "蠣崎波響");
    assert_eq!(ruby["ruby"]["reading"], "かきざきはきやう");
    let reading = nodes(&ruby["reading_children"]);
    let app = reading
        .iter()
        .find(|node| node["type"] == "base-text-variant")
        .unwrap();
    assert_eq!(app["text"], "かきざき");
    assert_eq!(app["variant"]["base_text"], "かきさき");
    assert_eq!(app["span"]["coordinate_system"], "reading_utf8");
    assert_eq!(app["span"]["end"], "かきざき".len());
    let current = &app["inline_children"][0];
    let start = usize::try_from(current["source_span"]["start"].as_u64().unwrap()).unwrap();
    let end = usize::try_from(current["source_span"]["end"].as_u64().unwrap()).unwrap();
    let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
    assert_eq!(&source[start..end], "かきざき");
    assert_eq!(ir["interpretation_problems"], json!([]));
}
