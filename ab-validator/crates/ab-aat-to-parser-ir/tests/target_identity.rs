use std::path::Path;

use ab_aat_to_parser_ir::{ConversionOptions, ConversionRequest, MappingDocument, SchemaSet};
use serde_json::{Value, json};

#[test]
fn shared_literal_continuation_remains_outside_the_witness_quote() {
    for (body, current, witness, continuation) in [
        (
            "時藏《ときぞう》は［＃「時藏《ときぞう》は」は底本では「由藏《よしぞう》」は］",
            "時藏は",
            "由藏は",
            "は",
        ),
        (
            "二三歩｜後退《あとしざ》つた［＃「二三歩｜後退《あとしざ》つた」は底本では「二三｜歩退《あとしざ》」つた］",
            "二三歩後退つた",
            "二三歩退つた",
            "つた",
        ),
        (
            "一般與論の［＃「一般與論の」は底本では「一般輿論」の］",
            "一般與論の",
            "一般輿論の",
            "の",
        ),
    ] {
        let ir = convert(body);
        let all = nodes(&ir);
        let app = all
            .iter()
            .find(|node| node["type"] == "base-text-variant")
            .unwrap();
        assert_eq!(app["text"], current);
        assert_eq!(app["variant"]["base_text"], witness);
        let children = app["variant"]["base_children"].as_array().unwrap();
        let tail = children.last().unwrap();
        assert_eq!(tail["text"], continuation);
        let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
        let start = usize::try_from(tail["source_span"]["start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(tail["source_span"]["end"].as_u64().unwrap()).unwrap();
        assert_eq!(&source[start..end], continuation);
        assert!(source[..start].ends_with('」'));
        assert_eq!(ir["interpretation_problems"], json!([]));
    }
    for body in [
        "一般與論の［＃「一般與論の」は底本では「一般輿論」は］",
        "一般與論の［＃「一般與論の」は底本では「一般輿論」のとある］",
        "一般與論の後［＃「一般與論の」は底本では「一般輿論」の］",
        "一般與論の［＃「一般與論の」は底本では「一般輿論」「別」の］",
        "時藏《ときぞう》は［＃「時藏《じぞう》は」は底本では「由藏《よしぞう》」は］",
    ] {
        let ir = convert(body);
        assert!(
            !nodes(&ir)
                .iter()
                .any(|node| node["type"] == "base-text-variant"),
            "{body}"
        );
        assert!(!ir["interpretation_problems"].as_array().unwrap().is_empty());
    }
}

fn convert(body: &str) -> Value {
    let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let mapping =
        MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v2.json")).unwrap();
    let schemas = SchemaSet::for_aat_version(2).unwrap();
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
    let partial = convert("字［＃「字」は底本では「字［＃未知の意味］」］");
    assert!(
        nodes(&partial)
            .iter()
            .any(|node| node["type"] == "base-text-variant")
    );
    assert!(
        nodes(&partial)
            .iter()
            .any(|node| node["note"]["raw"] == "［＃未知の意味］")
    );
    assert!(
        !partial["interpretation_facts"]
            .as_array()
            .unwrap()
            .iter()
            .any(|fact| fact["kind"] == "text-variant")
    );
    assert!(
        !partial["interpretation_problems"]
            .as_array()
            .unwrap()
            .is_empty()
    );
    let mismatch =
        convert("※［＃「弋＋頁」、74-10］［＃「※［＃「木＋貝」、74-10］」は底本では「字」］");
    assert!(
        !nodes(&mismatch)
            .iter()
            .any(|node| node["type"] == "base-text-variant")
    );
    assert!(
        !mismatch["interpretation_problems"]
            .as_array()
            .unwrap()
            .is_empty()
    );
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

#[test]
fn variant_citations_and_assertions_stay_separate_from_the_witness() {
    for (current, witness, tail) in [
        ("藤井", "蔵井", "、412-13"),
        ("山", "出", "と誤植、25-上-1"),
        ("平和", "価格", "、正誤表による訂正"),
        ("如何《どう》", "如何《どう》う", "と「う」が重複"),
    ] {
        let statement = format!("「{current}」は底本では「{witness}」{tail}");
        let ir = convert(&format!("{current}［＃{statement}］"));
        let all = nodes(&ir);
        let app = all
            .iter()
            .find(|node| node["type"] == "base-text-variant")
            .unwrap();
        assert_eq!(
            app["variant"]["base_text"],
            if witness == "如何《どう》う" {
                "如何う"
            } else {
                witness
            }
        );
        let note = all
            .iter()
            .find(|node| node["type"] == "editor-note")
            .unwrap();
        assert_eq!(note["note_kind"], "base-edition");
        assert_eq!(note["text"], statement);
        assert_eq!(ir["interpretation_problems"], json!([]));
    }
}

#[test]
fn direct_glyph_witnesses_reuse_native_reference_interpretation() {
    let marker = "※［＃「飮のへん＋稻のつくり」、第4水準2-92-68］";
    let ir = convert(&format!("餡［＃「餡」は底本では{marker}］"));
    let all = nodes(&ir);
    let app = all
        .iter()
        .find(|node| node["type"] == "base-text-variant")
        .unwrap();
    assert_eq!(app["text"], "餡");
    assert_eq!(app["variant"]["base_children"][0]["type"], "gaiji");
    assert_eq!(ir["interpretation_problems"], json!([]));
    let ir = convert("牝牛［＃「牝牛」では底本では「牡牛」］");
    let all = nodes(&ir);
    let app = all
        .iter()
        .find(|node| node["type"] == "base-text-variant")
        .unwrap();
    assert_eq!(app["variant"]["base_text"], "牡牛");
    assert_eq!(ir["interpretation_problems"], json!([]));
}

#[test]
fn base_edition_geometry_does_not_format_or_retarget_principal_text() {
    for statement in [
        "左図の解説文、底本では横組み",
        "「differentiation」の左から２番目のtは底本では上下逆",
        "「5」の傍線は底本では欠落",
        "「士は、」の後は、底本では改行１字下げ",
        "「％」は底本では「・」の右横に付く",
        "ルビの「ゲトウ」は、底本では「ゲ」が左に90度回転",
    ] {
        let ir = convert(&format!("本文［＃{statement}］"));
        let all = nodes(&ir);
        let note = all
            .iter()
            .find(|node| node["note_kind"] == "base-edition")
            .unwrap();
        assert_eq!(note["text"], statement);
        assert!(
            !all.iter()
                .any(|node| node["type"] == "base-text-variant" || node["type"] == "emphasis")
        );
        assert_eq!(ir["interpretation_problems"], json!([]));
    }
    let ir = convert("別文［＃「字」は底本では「他字」］");
    assert!(
        !nodes(&ir)
            .iter()
            .any(|node| node["note_kind"] == "base-edition")
    );
    assert!(!ir["interpretation_problems"].as_array().unwrap().is_empty());
}

#[test]
fn editorial_explanations_keep_variants_and_standalone_source_statements() {
    let statement = "「１７５９２」は底本では「１７３９２」。【例題一】と同一と考えられるため、【例題一】に合わせました。";
    let ir = convert(&format!("１７５９２［＃{statement}］"));
    let all = nodes(&ir);
    let app = all
        .iter()
        .find(|node| node["type"] == "base-text-variant")
        .unwrap();
    assert_eq!(app["variant"]["base_text"], "１７３９２");
    assert!(
        all.iter()
            .any(|node| node["note_kind"] == "base-edition" && node["text"] == statement)
    );
    assert_eq!(ir["interpretation_problems"], json!([]));
    for statement in [
        "底本で第二十四頁にあるのは、例題七（底本では例題八）です。",
        "次の手紙は「遺書」として書かれ投函されなかった。底本では第十九巻の巻末に収録",
        "入力者註：底本では「中国」「支那」が共に使われているが、「中国」に統一した。",
    ] {
        let ir = convert(&format!("本文［＃{statement}］"));
        assert!(
            nodes(&ir)
                .iter()
                .any(|node| node["note_kind"] == "base-edition" && node["text"] == statement)
        );
        assert_eq!(ir["interpretation_problems"], json!([]));
    }
}

#[test]
fn edition_targets_remain_inside_their_normal_heading() {
    let ir = convert(
        "［＃１字下げ］欧洲婦人の髪（晶子）［＃「欧洲婦人の髪（晶子）」は大見出し］［＃「欧洲婦人の髪（晶子）」は底本では「欧洲婦人の髪」］\n本文",
    );
    let all = nodes(&ir);
    let heading = all.iter().find(|node| node["type"] == "heading").unwrap();
    assert_eq!(heading["style"], "normal");
    assert_eq!(heading["level"], 1);
    assert_eq!(heading["text"], "欧洲婦人の髪（晶子）");
    let app = &heading["inline_children"][0];
    assert_eq!(app["type"], "base-text-variant", "{ir}");
    assert_eq!(app["variant"]["base_text"], "欧洲婦人の髪");
    assert_eq!(ir["interpretation_problems"], json!([]));

    let ir = convert(
        "［＃１字下げ］ロダン翁《をう》［＃「ロダン翁」は大見出し］［＃ルビの「をう」は底本では「おう」］\n本文",
    );
    let all = nodes(&ir);
    let heading = all.iter().find(|node| node["type"] == "heading").unwrap();
    assert_eq!(heading["text"], "ロダン翁");
    let ruby = all.iter().find(|node| node["type"] == "ruby").unwrap();
    assert_eq!(ruby["reading_children"][0]["type"], "base-text-variant");
    assert_eq!(ir["interpretation_problems"], json!([]));
    for middle in ["［＃未知の意味］", "\n", "\n別の本文"] {
        let ir = convert(&format!(
            "見出し［＃「見出し」は大見出し］{middle}［＃「見出し」は底本では「別題」］"
        ));
        assert!(
            !nodes(&ir)
                .iter()
                .any(|node| node["type"] == "base-text-variant")
        );
        assert!(!ir["interpretation_problems"].as_array().unwrap().is_empty());
    }
}

#[test]
fn unresolved_glyph_targets_require_the_same_structured_identity() {
    let glyph = "※［＃濁点付き井、379-1］";
    let current = format!("ダ・{glyph}ンチ等の");
    let ir = convert(&format!(
        "{current}［＃「{current}」は底本では「ダ{glyph}ンチ等の」］"
    ));
    let all = nodes(&ir);
    let app = all
        .iter()
        .find(|node| node["type"] == "base-text-variant")
        .expect("exact described glyph target");
    assert_eq!(app["text"], "ダ・\u{fffc}ンチ等の");
    assert_eq!(app["variant"]["base_text"], "ダ\u{fffc}ンチ等の");
    assert_eq!(ir["interpretation_problems"], json!([]));
    assert!(
        nodes(app)
            .iter()
            .any(|node| node["type"] == "gaiji" && node["resolved"].is_null())
    );

    for (actual, quoted) in [
        (glyph, "※［＃濁点付き中、379-1］"),
        (glyph, "※［＃濁点付き井、380-1］"),
        (glyph, "\u{fffc}"),
        ("\u{fffc}", glyph),
    ] {
        let ir = convert(&format!(
            "甲{actual}乙［＃「甲{quoted}乙」は底本では「別」］"
        ));
        assert!(
            !nodes(&ir)
                .iter()
                .any(|node| node["type"] == "base-text-variant"),
            "{actual} / {quoted}"
        );
        assert!(!ir["interpretation_problems"].as_array().unwrap().is_empty());
    }
    for actual in [glyph, "※［＃濁点付き中、379-1］", "\u{fffc}"] {
        let ir = convert(&format!(
            "｜甲{actual}《こう》［＃「｜甲{glyph}《こう》」は底本では「別」］"
        ));
        assert_eq!(
            nodes(&ir)
                .iter()
                .any(|node| node["type"] == "base-text-variant"),
            actual == glyph
        );
    }
    for suffix in ["別", "\n", "［＃未知の意味］"] {
        let ir = convert(&format!(
            "{current}{suffix}［＃「{current}」は底本では「別」］"
        ));
        assert!(
            !nodes(&ir)
                .iter()
                .any(|node| node["type"] == "base-text-variant")
        );
    }
}

#[test]
fn rich_heading_quotes_constrain_only_the_ruby_they_supply() {
    let text = "一〇、失せ物は巽《たつみ》の方の栗《マロニエ》の根元を探すべし。";
    let quote = "一〇、失せ物は巽《たつみ》の方の栗の根元を探すべし。";
    let ir = convert(&format!("{text}［＃「{quote}」は同行中見出し］後。"));
    let all = nodes(&ir);
    let heading = all
        .iter()
        .find(|node| node["type"] == "heading")
        .expect("source-owned rich heading");
    assert_eq!(
        heading["text"],
        "一〇、失せ物は巽の方の栗の根元を探すべし。"
    );
    assert_eq!(heading["style"], "dogyo");
    assert_eq!(
        nodes(heading)
            .iter()
            .filter(|node| node["type"] == "ruby")
            .count(),
        2
    );
    assert!(ir["interpretation_problems"].as_array().unwrap().is_empty());
    for body in [
        "ギリシャの医師たち［＃「ギリシヤの医師たち」は同行小見出し］",
        "キリスト教は愛他主義の第一要因［＃「キリスト教は愛他主義の第一要員」は同行小見出し］",
        "巽《たつみ》［＃「巽《ちがう》」は中見出し］",
        "前［＃「不在」は大見出し］後",
    ] {
        let ir = convert(body);
        assert!(!nodes(&ir).iter().any(|node| node["type"] == "heading"));
        assert!(
            !ir["interpretation_problems"].as_array().unwrap().is_empty(),
            "{body}"
        );
        assert!(
            ir["interpretation_facts"]
                .as_array()
                .unwrap()
                .iter()
                .all(|fact| fact["kind"] != "heading")
        );
    }
}

#[test]
fn edition_ranges_preserve_documentary_targets_without_changing_body_layout() {
    let ir = convert(
        "［＃ここから２字下げ］\n［＃ここから底本では上段］\n北海道の羆《ひぐま》。\n［＃ここまで底本では上段］\n［＃ここから底本では下段］\n下の文。\n［＃ここまで底本では下段］\n［＃ここで字下げ終わり］",
    );
    let all = nodes(&ir);
    let notes: Vec<_> = all
        .iter()
        .filter(|node| node["type"] == "editor-note" && node.get("target_source_spans").is_some())
        .collect();
    assert_eq!(notes.len(), 2);
    for note in notes {
        assert_eq!(note["note_kind"], "base-edition");
        let target = &note["target_source_spans"][0];
        assert_eq!(target["start"], note["source_span"]["end"]);
        assert_eq!(target["end"], note["closing_source_span"]["start"]);
        assert_eq!(target["coordinate_system"], "decoded_utf8");
    }
    assert_eq!(all.iter().filter(|node| node["type"] == "ruby").count(), 1);
    assert_eq!(ir["layout_blocks"][0]["indent"], 2);
    assert_eq!(
        ir["interpretation_facts"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|fact| fact["kind"] == "editorial-note")
            .count(),
        4
    );
    assert_eq!(ir["interpretation_problems"], json!([]));
    for body in [
        "［＃ここから底本では上段］本文［＃ここまで底本では下段］",
        "［＃ここから底本では上段］本文",
        "本文［＃ここまで底本では上段］",
        "［＃ここから底本では上段］甲［＃ここから底本では下段］乙［＃ここまで底本では上段］丙［＃ここまで底本では下段］",
        "［＃ここから底本では上段］甲［＃ここから底本では下段］乙［＃ここまで底本では上段］丙［＃ここまで底本では上段］",
    ] {
        let ir = convert(body);
        assert!(
            !nodes(&ir)
                .iter()
                .any(|node| node.get("target_source_spans").is_some())
        );
        assert!(!ir["interpretation_problems"].as_array().unwrap().is_empty());
        assert!(
            ir["interpretation_facts"]
                .as_array()
                .unwrap()
                .iter()
                .all(|fact| fact["kind"] != "editorial-note")
        );
    }
}

#[test]
fn editorial_source_targets_reject_overlapping_or_reversed_segments() {
    let source =
        "題\n作者\n\n［＃ここから底本では上段］本文［＃ここまで底本では上段］\n\n底本：本\n";
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    for spans in [[(10, 20), (19, 30)], [(20, 30), (10, 20)]] {
        let mut aat: Value =
            serde_json::from_slice(&ab_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
                .unwrap();
        let note = &mut aat["blocks"][0]["content"][0];
        assert_eq!(note["kind"], "editorial_note");
        let original = note["target_source_spans"][0].clone();
        note["target_source_spans"] = json!(spans.map(|(start, end)| {
            let mut span = original.clone();
            span["byte_start"] = json!(start);
            span["byte_end"] = json!(end);
            span
        }));
        let error = ab_aat_to_parser_ir::convert(ConversionRequest {
            aat,
            mapping: MappingDocument::from_path(
                &repo.join("data/aat-to-parser-ir-mapping-v2.json"),
            )
            .unwrap(),
            schemas: SchemaSet::for_aat_version(2).unwrap(),
            options: ConversionOptions::default(),
        })
        .unwrap_err();
        assert!(
            format!("{error:#}").contains("overlap or are out of order"),
            "{error:#}"
        );
    }
}

#[test]
fn physical_break_variant_preserves_discontiguous_source_targets() {
    let marker = "［＃「白よ、［＃改行］［＃改行］」は底本では「白よ、［＃改行］」］";
    for breaks in ["\n\n", "\r\n\r\n"] {
        let ir = convert(&format!("指の白よ、{marker}{breaks}次。"));
        let all = nodes(&ir);
        let note = all
            .iter()
            .find(|node| node["type"] == "editor-note" && node.get("annotation_children").is_some())
            .unwrap();
        assert_eq!(note["target_source_spans"].as_array().unwrap().len(), 2);
        let targets = &note["target_source_spans"];
        assert_eq!(targets[0]["end"], note["source_span"]["start"]);
        assert_eq!(targets[1]["start"], note["source_span"]["end"]);
        assert_eq!(
            targets[1]["end"].as_u64().unwrap() - targets[1]["start"].as_u64().unwrap(),
            u64::try_from(breaks.len()).unwrap()
        );
        let app = &note["annotation_children"][0];
        assert_eq!(app["text"], "白よ、\n\n");
        assert_eq!(app["variant"]["base_text"], "白よ、\n");
        assert_eq!(app["span"]["coordinate_system"], "annotation_utf8");
        assert_eq!(ir["interpretation_problems"], json!([]));
    }
    for body in [
        format!("白よ、{marker}\n次。"),
        format!("白よ、{marker}別\n\n次。"),
        format!("白よ、別{marker}\n\n次。"),
        format!("白よ、\n{marker}\n\n次。"),
    ] {
        let ir = convert(&body);
        assert!(
            !ir["interpretation_problems"].as_array().unwrap().is_empty(),
            "{body}"
        );
        assert!(
            !nodes(&ir)
                .iter()
                .any(|node| node.get("annotation_children").is_some()),
            "{body}"
        );
    }
}

#[test]
fn gaiji_keeps_its_separate_edition_assertion() {
    let statement = "底本はこの字を「さんずい＋「仰」のつくり」と作字上の誤り";
    let body =
        format!("田口※［＃「※」は「さんずい＋卯」、第4水準2-78-35、17-上-9、{statement}］三郎");
    let ir = convert(&body);
    let all = nodes(&ir);
    let annotated = all
        .iter()
        .find(|node| node["type"] == "annotated-text")
        .unwrap();
    assert_eq!(annotated["text"], "泖");
    assert_eq!(annotated["note_kind"], "base-edition");
    let fact = ir["interpretation_facts"]
        .as_array()
        .unwrap()
        .iter()
        .find(|fact| {
            fact["kind"] == "editorial-note" && fact["source_span"] == annotated["source_span"]
        })
        .unwrap();
    assert_eq!(fact["aspects"], json!(["content", "structure"]));
    assert_eq!(annotated["inline_children"][0]["gaiji"]["unicode"], "泖");
    assert_eq!(annotated["annotation_children"][0]["text"], statement);
    assert_eq!(
        annotated["annotation_children"][0]["span"]["coordinate_system"],
        "annotation_utf8"
    );
    assert_eq!(ir["interpretation_problems"], json!([]));
}

#[test]
fn base_edition_concealment_claim_does_not_assert_layout() {
    let ir = convert("□□［＃底本２字伏字］");
    let all = nodes(&ir);
    let annotated = all
        .iter()
        .find(|node| node["type"] == "annotated-text")
        .unwrap();
    assert_eq!(annotated["text"], "□□");
    assert_eq!(annotated["annotation_children"][0]["type"], "gap");
    assert_eq!(annotated["annotation_children"][0]["quantity"], 2);
    let fact = ir["interpretation_facts"]
        .as_array()
        .unwrap()
        .iter()
        .find(|fact| {
            fact["kind"] == "editorial-note" && fact["source_span"] == annotated["source_span"]
        })
        .unwrap();
    assert_eq!(fact["aspects"], json!(["content", "structure"]));
    assert_eq!(ir["interpretation_problems"], json!([]));
}

#[test]
fn supplied_note_roles_reject_invented_placement_and_missing_targets() {
    for (body, role) in [
        ("（１）［＃「（１）」は注釈番号］", "annotation-number"),
        ("（一）［＃（一）は自注］", "author-note"),
    ] {
        let ir = convert(body);
        let annotation = nodes(&ir)
            .into_iter()
            .find(|node| node["type"] == "annotated-text")
            .unwrap();
        assert_eq!(annotation["note_kind"], role);
        assert!(annotation.get("position").is_none());
        assert_eq!(ir["interpretation_problems"], json!([]));
        let schemas = SchemaSet::for_aat_version(2).unwrap();
        let mut invalid = ir.clone();
        fn inject(value: &mut Value) {
            match value {
                Value::Object(map) => {
                    if map.get("type").and_then(Value::as_str) == Some("annotated-text") {
                        map.insert("position".into(), json!("left"));
                    }
                    for child in map.values_mut() {
                        inject(child);
                    }
                }
                Value::Array(values) => values.iter_mut().for_each(inject),
                _ => {}
            }
        }
        inject(&mut invalid);
        assert!(
            ab_aat_to_parser_ir::schema::validate_value(&schemas.parser_ir_schema, &invalid, "IR")
                .is_err()
        );
    }
    for body in [
        "（二）［＃（一）は自注］",
        "（一）別［＃（一）は自注］",
        "（１）別［＃「（１）」は注釈番号］",
    ] {
        let ir = convert(body);
        assert!(
            !nodes(&ir)
                .iter()
                .any(|node| node["type"] == "annotated-text")
        );
        assert!(!ir["interpretation_problems"].as_array().unwrap().is_empty());
    }
}

#[test]
fn rich_sic_statements_compose_without_weakening_quoted_identity() {
    let ir = convert(
        "小突《こづか》かれるので［＃「小突《こづか》かれるので」はママ］［＃「小突《こづか》かれるので［＃「小突《こづか》かれるので」はママ］」は底本では「かれるので小突《こづか》［＃「かれるので小突《こづか》」はママ］」］",
    );
    let all = nodes(&ir);
    let variant = all
        .iter()
        .find(|node| node["type"] == "base-text-variant")
        .expect("exact rich source variant");
    assert_eq!(variant["text"], "小突かれるので");
    let notes = all
        .iter()
        .filter(|node| node["type"] == "editor-note" && node["note_kind"] == "sic")
        .collect::<Vec<_>>();
    assert_eq!(notes.len(), 2);
    for note in notes {
        assert!(nodes(note).iter().any(|node| node["type"] == "ruby"));
        assert_eq!(
            note["annotation_children"][0]["span"]["coordinate_system"],
            "annotation_utf8"
        );
    }
    assert_eq!(ir["interpretation_problems"], json!([]));
    let note_spans = ir["interpretation_facts"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|fact| fact["kind"] == "editorial-note")
        .map(|fact| fact["source_span"].clone())
        .collect::<Vec<_>>();
    assert_eq!(
        note_spans.len(),
        3,
        "principal, checked current quotation, witness"
    );
    assert!(
        note_spans
            .windows(2)
            .all(|pair| pair[0]["end"].as_u64() < pair[1]["start"].as_u64())
    );
    for body in [
        "甲［＃「甲」はママ］別［＃「甲［＃「甲」はママ］」は底本では「乙」］",
        "甲［＃「甲」はママ］［＃「甲［＃底本のまま］」は底本では「乙」］",
        "甲［＃未解釈指示］［＃「甲［＃未解釈指示］」は底本では「乙」］",
    ] {
        let ir = convert(body);
        assert!(
            !nodes(&ir)
                .iter()
                .any(|node| node["type"] == "base-text-variant")
        );
        assert!(!ir["interpretation_problems"].as_array().unwrap().is_empty());
        let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
        let outer_start = source.find("［＃「甲［＃").unwrap();
        assert!(
            ir["interpretation_facts"]
                .as_array()
                .unwrap()
                .iter()
                .all(|fact| fact["source_span"]["start"].as_u64().unwrap()
                    < u64::try_from(outer_start).unwrap()),
            "an unchecked quoted current or witness receives no positive facts"
        );
    }
}

#[test]
fn a_reading_variant_selects_only_its_exact_structured_suffix() {
    let body = "懲々《こり／″＼》［＃ルビの「／″＼」は底本では「こり／＼」］";
    let ir = convert(body);
    let all = nodes(&ir);
    let ruby = all.iter().find(|node| node["type"] == "ruby").unwrap();
    assert_eq!(ruby["ruby"]["reading"], "こり〲");
    let variant = all
        .iter()
        .find(|node| node["type"] == "base-text-variant")
        .expect("exact reading suffix");
    assert_eq!(variant["text"], "〲");
    assert_eq!(variant["variant"]["base_text"], "こり〱");
    assert_eq!(
        variant["span"],
        json!({"start":6,"end":9,"coordinate_system":"reading_utf8"})
    );
    assert_eq!(variant["inline_children"][0]["type"], "iteration-mark");
    assert_eq!(ir["interpretation_problems"], json!([]));
    for body in [
        "懲々《こり／＼》［＃ルビの「／″＼」は底本では「こり／＼」］",
        "懲々《こり／″＼》別《べつ》［＃ルビの「／″＼」は底本では「こり／＼」］",
        "懲々《／″＼こり》［＃ルビの「／″＼」は底本では「こり／＼」］",
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
fn edition_targets_retain_the_immediately_preceding_accent_scope() {
    for (body, current, witness, spelling) in [
        (
            "９．〔Der Mu:s&iggang wird von unserem Lehrer verdammt.〕［＃「〔Mu:s&iggang〕」は底本では「〔Mu:s&igang〕」］",
            "Müßiggang",
            "Müßigang",
            "Mu:s&iggang",
        ),
        (
            "ROLLER:［＃「ROLLER:」は太字］ 〔Wis&t ihr auch, das& man uns auskundschaftet?〕［＃「auch」は底本では「acuh」］",
            "auch",
            "acuh",
            "auch",
        ),
        (
            "〔a: auch danach〕［＃「auch」は底本では「｜別《べつ》」］",
            "auch",
            "別",
            "auch",
        ),
    ] {
        let ir = convert(body);
        let all = nodes(&ir);
        let app = all
            .iter()
            .find(|node| node["type"] == "base-text-variant")
            .unwrap();
        assert_eq!(app["text"], current);
        assert_eq!(app["variant"]["base_text"], witness);
        let span = &app["inline_children"][0]["source_span"];
        let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
        let start = usize::try_from(span["start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(span["end"].as_u64().unwrap()).unwrap();
        assert_eq!(&source[start..end], spelling);
        assert_eq!(ir["interpretation_problems"], json!([]));
    }
    for body in [
        "〔auch auch danach〕［＃「auch」は底本では「acuh」］",
        "auch danach［＃「auch」は底本では「acuh」］",
        "〔auch danach〕別［＃「auch」は底本では「acuh」］",
        "〔auch danach〕\n［＃「auch」は底本では「acuh」］",
        "〔auch danach〕〔anders〕［＃「auch」は底本では「acuh」］",
        "〔Der Mu:s&iggang wird〕［＃「Musiggang」は底本では「Musigang」］",
    ] {
        let ir = convert(body);
        assert!(
            !nodes(&ir)
                .iter()
                .any(|node| node["type"] == "base-text-variant"),
            "{body}"
        );
        assert!(
            !ir["interpretation_problems"].as_array().unwrap().is_empty(),
            "{body}"
        );
    }
}

#[test]
fn a_quoted_source_spelling_inherits_only_its_exact_target_accent_context() {
    let body = "〔Ich danke dir für dein Ha:tte.“〕［＃「Ha:tte.“」は底本では「Ha:tte“.」］";
    let ir = convert(body);
    let all = nodes(&ir);
    let app = all
        .iter()
        .find(|node| node["type"] == "base-text-variant")
        .unwrap();
    assert_eq!(app["text"], "Hätte.“");
    assert_eq!(app["variant"]["base_text"], "Hätte“.");
    let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
    for (node, spelling) in [
        (&app["inline_children"][0], "Ha:tte.“"),
        (&app["variant"]["base_children"][0], "Ha:tte“."),
    ] {
        let span = &node["source_span"];
        let start = usize::try_from(span["start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(span["end"].as_u64().unwrap()).unwrap();
        assert_eq!(&source[start..end], spelling);
    }
    assert_eq!(ir["interpretation_problems"], json!([]));
    let facts = ir["interpretation_facts"].as_array().unwrap();
    assert_eq!(facts.len(), 1);
    assert_eq!(facts[0]["kind"], "text-variant");
    assert_eq!(
        facts[0]["source_span"]["start"],
        source.find("［＃").unwrap()
    );
    let ligature = convert("〔ae&〕［＃「ae&」は底本では「oe&」］");
    let ligature_nodes = nodes(&ligature);
    let ligature_app = ligature_nodes
        .iter()
        .find(|node| node["type"] == "base-text-variant")
        .unwrap();
    assert_eq!(ligature_app["text"], "æ");
    assert_eq!(ligature_app["variant"]["base_text"], "œ");
    let span = &ligature_app["inline_children"][0]["source_span"];
    assert_eq!(
        span["end"].as_u64().unwrap() - span["start"].as_u64().unwrap(),
        3
    );
    for body in [
        "〔ae&〕［＃「e&」は底本では「o&」］",
        "Hätte.“［＃「Ha:tte.“」は底本では「Ha:tte“.」］",
        "〔cafe' Hätte.“〕［＃「Ha:tte.“」は底本では「Ha:tte“.」］",
        "〔Ha:tte.“〕別［＃「Ha:tte.“」は底本では「Ha:tte“.」］",
        "〔Ha:tte.“〕\n［＃「Ha:tte.“」は底本では「Ha:tte“.」］",
        "〔Ha:tte.“ danach〕［＃「Ha:tte.“」は底本では「Ha:tte“.」］",
        "〔Ha:tte.“〕〔cafe'〕［＃「Ha:tte.“」は底本では「Ha:tte“.」］",
        "〔Ha:tte.“〕［＃「Ha:tte.“」は底本では「Ha:tte［＃未知］“.」］",
    ] {
        let ir = convert(body);
        assert!(
            !nodes(&ir)
                .iter()
                .any(|node| node["type"] == "base-text-variant"),
            "{body}"
        );
        assert!(
            !ir["interpretation_problems"].as_array().unwrap().is_empty(),
            "{body}"
        );
    }
}

#[test]
fn an_edition_target_can_own_the_entire_adjacent_quotation_interior() {
    for (body, target) in [
        (
            "「露西亞車」［＃「露西亞車」は底本では「靈西亞車」］",
            "露西亞車",
        ),
        (
            "「五・一五事件」［＃「五・一五事件」は底本では「五一・五事件」］",
            "五・一五事件",
        ),
        (
            "「何かしら」［＃「何かしら」は底本では「何かいら」］",
            "何かしら",
        ),
        (
            "「漢字《かんじ》」［＃「漢字《かんじ》」は底本では「文字」］",
            "漢字",
        ),
    ] {
        let ir = convert(body);
        let all = nodes(&ir);
        let app = all
            .iter()
            .find(|node| node["type"] == "base-text-variant")
            .unwrap();
        assert_eq!(app["text"], target);
        assert_eq!(ir["interpretation_problems"], json!([]));
        let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
        let children = app["inline_children"].as_array().unwrap();
        let start = usize::try_from(
            children.first().unwrap()["source_span"]["start"]
                .as_u64()
                .unwrap(),
        )
        .unwrap();
        let end = usize::try_from(
            children.last().unwrap()["source_span"]["end"]
                .as_u64()
                .unwrap(),
        )
        .unwrap();
        assert_eq!(&source[start - 3..start], "「");
        assert_eq!(&source[end..end + 3], "」");
    }
    for body in [
        "漢字」［＃「漢字」は底本では「文字」］",
        "「漢字」別［＃「漢字」は底本では「文字」］",
        "「漢字」\n［＃「漢字」は底本では「文字」］",
        "「漢字」［＃「字」は底本では「文」］",
        "「漢字」「別」［＃「漢字」は底本では「文字」］",
        "「漢字《かんじ》」［＃「漢字《かんし》」は底本では「文字」］",
    ] {
        let ir = convert(body);
        assert!(
            !nodes(&ir)
                .iter()
                .any(|node| node["type"] == "base-text-variant"),
            "{body}"
        );
        assert!(
            !ir["interpretation_problems"].as_array().unwrap().is_empty(),
            "{body}"
        );
    }
}

#[test]
fn a_reading_variant_can_supply_its_shared_principal_continuation() {
    let body = "零《こぼ》す［＃ルビの「こぼ（す）」は底本では「にぼ（す）」］";
    let ir = convert(body);
    let all = nodes(&ir);
    let app = all
        .iter()
        .find(|node| node["type"] == "base-text-variant")
        .unwrap();
    assert_eq!(app["text"], "こぼ");
    assert_eq!(app["variant"]["base_text"], "にぼ");
    assert_eq!(ir["interpretation_problems"], json!([]));
    let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
    for (node, expected) in [
        (&app["inline_children"][0], "こぼ"),
        (&app["variant"]["base_children"][0], "にぼ"),
    ] {
        let span = &node["source_span"];
        let start = usize::try_from(span["start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(span["end"].as_u64().unwrap()).unwrap();
        assert_eq!(&source[start..end], expected);
    }
    let literal = convert("零《こぼ（す）》［＃ルビの「こぼ（す）」は底本では「にぼ（す）」］");
    let literal_nodes = nodes(&literal);
    let literal_app = literal_nodes
        .iter()
        .find(|node| node["type"] == "base-text-variant")
        .unwrap();
    assert_eq!(literal_app["text"], "こぼ（す）");
    assert_eq!(literal_app["variant"]["base_text"], "にぼ（す）");
    for body in [
        "零《こぼ》［＃ルビの「こぼ（す）」は底本では「にぼ（す）」］",
        "零《こぼ》した［＃ルビの「こぼ（す）」は底本では「にぼ（す）」］",
        "零《こぼ》す［＃ルビの「こぼ（す）」は底本では「にぼ（した）」］",
        "零《こぼ》す［＃ルビの「こぼ（）」は底本では「にぼ（）」］",
        "零《こぼ》す\n［＃ルビの「こぼ（す）」は底本では「にぼ（す）」］",
        "零《こぼ》す［＃底本のまま］［＃ルビの「こぼ（す）」は底本では「にぼ（す）」］",
        "零《こぼ》す別《べつ》す［＃ルビの「こぼ（す）」は底本では「にぼ（す）」］",
        "零《こぼ》す［＃ルビの「こぽ（す）」は底本では「にぼ（す）」］",
        "零《こぼ》す［＃「こぼ（す）」は底本では「にぼ（す）」］",
    ] {
        let ir = convert(body);
        assert!(
            !nodes(&ir)
                .iter()
                .any(|node| node["type"] == "base-text-variant"),
            "{body}"
        );
        assert!(
            !ir["interpretation_problems"].as_array().unwrap().is_empty(),
            "{body}"
        );
    }
}

#[test]
fn exact_targets_retain_uncertain_witness_components_without_claiming_them_complete() {
    for body in [
        "萬一《まんいち》［＃「萬一《まんいち》」は底本では「萬　《まん　　》」］",
        "。棚《たな》に［＃「。棚《たな》に」は底本では「。《たな》棚に」］",
    ] {
        let ir = convert(body);
        let all = nodes(&ir);
        let app = all
            .iter()
            .find(|node| node["type"] == "base-text-variant")
            .unwrap();
        let witness = app["variant"]["base_children"].as_array().unwrap();
        assert!(witness.iter().any(
            |node| node["type"] == "editor-note" && node["note"]["resolution"] == "unresolved"
        ));
        let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
        for note in witness.iter().filter(|node| node["type"] == "editor-note") {
            let span = &note["source_span"];
            let start = usize::try_from(span["start"].as_u64().unwrap()).unwrap();
            let end = usize::try_from(span["end"].as_u64().unwrap()).unwrap();
            assert_eq!(&source[start..end], note["note"]["raw"].as_str().unwrap());
            assert_eq!(note["span"]["coordinate_system"], "witness_utf8");
            assert!(
                ir["interpretation_problems"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .any(|problem| problem["source_span"] == *span)
            );
        }
        assert!(!witness.iter().any(|node| node["type"] == "ruby"));
        assert!(
            !ir["interpretation_facts"]
                .as_array()
                .unwrap()
                .iter()
                .any(|fact| fact["kind"] == "text-variant")
        );
        assert!(
            ir["interpretation_problems"]
                .as_array()
                .unwrap()
                .iter()
                .any(|problem| problem["code"] == "unparsed-source-gap")
        );
    }
    let mismatch =
        convert("萬二《まんに》［＃「萬一《まんいち》」は底本では「萬　《まん　　》」］");
    assert!(
        !nodes(&mismatch)
            .iter()
            .any(|node| node["type"] == "base-text-variant")
    );
    let complete =
        convert("萬一《まんいち》［＃「萬一《まんいち》」は底本では「萬二《まんに》」］");
    assert!(
        complete["interpretation_facts"]
            .as_array()
            .unwrap()
            .iter()
            .any(|fact| fact["kind"] == "text-variant")
    );
    assert_eq!(complete["interpretation_problems"], json!([]));
}
