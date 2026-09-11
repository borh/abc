//! A quotation or letter role written ahead of the measure that scopes it
//! reaches the same scope as the measure, and neither displaces the other.

use std::path::Path;

use ab_aat_to_parser_ir::{ConversionOptions, ConversionRequest, MappingDocument, SchemaSet};
use serde_json::Value;

fn convert(body: &str) -> Value {
    let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
    let repo = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: serde_json::from_slice(&ab_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
            .unwrap(),
        mapping: MappingDocument::from_path(&repo.join("data/aat-to-parser-ir-mapping-v2.json"))
            .unwrap(),
        schemas: SchemaSet::for_aat_version(2).unwrap(),
        options: ConversionOptions::default(),
    })
    .unwrap()
    .parser_ir
}

fn fact_kinds(ir: &Value) -> Vec<String> {
    ir["interpretation_facts"]
        .as_array()
        .unwrap()
        .iter()
        .map(|fact| fact["kind"].as_str().unwrap().to_owned())
        .collect()
}

#[test]
fn a_role_written_before_the_measure_keeps_both_the_role_and_the_measure() {
    for (body, indent) in [
        (
            "［＃ここから引用文、３字下げ］\n引いた文\n［＃引用文終わり］",
            3,
        ),
        (
            "［＃ここから引用文、一字下げ］\n引いた文\n［＃ここで引用文終り］",
            1,
        ),
        (
            "［＃ここから引用文、８字下げ］\n引いた文\n［＃ここで引用文終わり］",
            8,
        ),
    ] {
        let ir = convert(body);
        let scope = &ir["layout_blocks"][0];
        assert_eq!(scope["role"], "quotation", "{body}");
        assert_eq!(scope["indent"], indent, "{body}");
        assert_eq!(
            ir["interpretation_problems"],
            serde_json::json!([]),
            "{body}"
        );
    }
}

#[test]
fn a_letter_is_not_folded_onto_the_quotation_role() {
    for opener in ["これより手紙文、１字下げ", "ここより手紙文、１字下げ"] {
        let ir = convert(&format!("［＃{opener}］\n拝啓\n［＃ここで字下げ終わり］"));
        let scope = &ir["layout_blocks"][0];
        assert_eq!(scope["role"], "letter", "{opener}");
        assert_eq!(scope["indent"], 1, "{opener}");
        assert_eq!(
            ir["interpretation_problems"],
            serde_json::json!([]),
            "{opener}"
        );
    }
}

#[test]
fn a_named_role_is_claimed_alongside_the_geometry_it_is_supplied_with() {
    let ir = convert("［＃ここから引用文、３字下げ］\n引いた文\n［＃引用文終わり］");
    let kinds = fact_kinds(&ir);
    assert!(kinds.iter().any(|kind| kind == "quotation"), "{kinds:?}");
    assert!(kinds.iter().any(|kind| kind == "line-layout"), "{kinds:?}");

    let letter = convert("［＃これより手紙文、１字下げ］\n拝啓\n［＃ここで字下げ終わり］");
    let kinds = fact_kinds(&letter);
    assert!(kinds.iter().any(|kind| kind == "letter"), "{kinds:?}");
    assert!(!kinds.iter().any(|kind| kind == "quotation"), "{kinds:?}");
}

#[test]
fn a_supplied_vertical_gap_stays_a_retained_clause() {
    // `３行アキ` states a gap the enclosed lines do not carry; executing it
    // would insert blank lines the source does not have, so it is retained.
    let ir = convert("［＃ここから引用文、３字下げ、３行アキ］\n引いた文\n［＃引用文終わり］");
    let scope = &ir["layout_blocks"][0];
    assert_eq!(scope["role"], "quotation");
    assert_eq!(scope["indent"], 3);
    let problems = ir["interpretation_problems"].as_array().unwrap();
    assert!(!problems.is_empty(), "the gap clause is retained");
}

#[test]
fn a_line_subset_exception_stays_a_retained_clause() {
    // `はじめの「一」のみ２字下げ` names a subset of the enclosed lines. The
    // scope holds no line identity to resolve it against, so the clause is
    // retained rather than applied to a guessed line.
    let ir = convert(
        "［＃ここから引用文、３字下げ、はじめの「一」のみ２字下げ］\n一　引いた文\n［＃引用文終わり］",
    );
    let scope = &ir["layout_blocks"][0];
    assert_eq!(scope["role"], "quotation");
    assert_eq!(scope["indent"], 3);
    assert!(
        !ir["interpretation_problems"].as_array().unwrap().is_empty(),
        "the exception clause is retained"
    );
}

#[test]
fn a_quotation_closer_does_not_close_a_scope_that_names_no_role() {
    // The closer names the role it ends. Letting it close a plain indentation
    // scope would make the source say the enclosed lines were quoted.
    let ir = convert("［＃ここから３字下げ］\n地の文\n［＃引用文終わり］");
    assert_eq!(ir["layout_blocks"], serde_json::json!([]));
    assert!(
        !ir["interpretation_problems"].as_array().unwrap().is_empty(),
        "the unmatched pair is recorded, not silently paired"
    );
}

#[test]
fn a_close_that_names_no_scope_keyword_stays_unresolved() {
    // `［＃引用、終わり］` names neither the indentation family nor the
    // 引用文 role, and the one work carrying it opens no quotation scope for it
    // to end. Reading it as a close would assert a scope the source never
    // opened, so it stays retained source.
    let ir = convert("引いた文\n［＃引用、終わり］");
    assert!(
        !ir["interpretation_problems"].as_array().unwrap().is_empty(),
        "the marker is retained as unresolved"
    );
    let text = serde_json::to_string(&ir["nodes"]).unwrap();
    assert!(text.contains("引用、終わり"), "source kept: {text}");
}
