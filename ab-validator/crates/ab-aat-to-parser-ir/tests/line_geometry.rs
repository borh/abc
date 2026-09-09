//! Supplied line geometry remains independent of its enclosed text.

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
        schemas: SchemaSet::for_aat_version(&repo, None, 2).unwrap(),
        options: ConversionOptions::default(),
    })
    .unwrap()
    .parser_ir
}

#[test]
fn supplied_geometry_retains_independent_dimensions_and_body_text() {
    let ir = convert(
        "［＃ここから６字下げ、折り返して７字下げ、２１字詰め］\n本文\n［＃ここで字下げ終わり］",
    );
    let scope = &ir["layout_blocks"][0];
    assert_eq!(scope["indent"], 6);
    assert_eq!(scope["continuation_indent"], 7);
    assert_eq!(scope["width"], 21);
    assert!(
        ir["nodes"]
            .as_array()
            .unwrap()
            .iter()
            .any(|node| node["text"] == "本文")
    );
    assert!(ir["interpretation_problems"].as_array().unwrap().is_empty());
}

#[test]
fn page_placement_and_nested_indentation_are_distinct_scopes() {
    let ir = convert(
        "［＃ここからページの左右中央］\n［＃ここから３字下げ］\n扉\n［＃ここで字下げ終わり］\n［＃改丁］\n次頁",
    );
    let scopes = ir["layout_blocks"].as_array().unwrap();
    assert_eq!(scopes.len(), 2);
    assert_eq!(scopes[0]["indent"], 3);
    assert_eq!(scopes[1]["page_placement"], "horizontal-center");
    assert!(scopes[1].get("align").is_none());
    assert!(scopes[1].get("indent").is_none());
    assert!(
        scopes[1]["node_range"]["end"].as_u64().unwrap()
            < ir["nodes"].as_array().unwrap().len() as u64
    );
}

#[test]
fn compound_frames_reuse_typed_formatting_without_losing_coapplied_attributes() {
    for (clause, border) in [("罫囲み", "rule"), ("破線枠囲み", "dashed-rule")] {
        let ir = convert(&format!(
            "［＃ここから３字下げ、ゴシック体、{clause}］\n本文\n［＃ここで字下げ終わり］"
        ));
        let block = &ir["layout_blocks"][0];
        assert!(block.get("border").is_none(), "{ir}");
        let attributes = block["typography"]
            .as_array()
            .expect("independent attributes");
        assert!(
            attributes
                .iter()
                .any(|value| value["kind"] == "emphasis" && value["style"] == "gothic"),
            "{ir}"
        );
        assert!(
            attributes
                .iter()
                .any(|value| value["kind"] == "keigakomi" && value["border"] == border),
            "{ir}"
        );
        assert!(
            attributes
                .iter()
                .all(|value| value["source"] == "aat-block"),
            "{ir}"
        );
        assert!(
            ir["interpretation_problems"].as_array().unwrap().is_empty(),
            "{ir}"
        );
    }
}

#[test]
fn conflicting_frame_kinds_preserve_indentation_and_uncertainty() {
    for clauses in ["罫囲み、破線枠囲み", "破線枠囲み、罫囲み"] {
        let ir = convert(&format!(
            "［＃ここから３字下げ、{clauses}］\n本文\n［＃ここで字下げ終わり］"
        ));
        let scope = &ir["layout_blocks"][0];
        assert_eq!(scope["indent"], 3, "{ir}");
        assert!(scope.get("typography").is_none(), "{ir}");
        assert!(
            ir["interpretation_problems"]
                .as_array()
                .unwrap()
                .iter()
                .any(|problem| problem["raw"] == clauses),
            "{ir}"
        );
    }
}

#[test]
fn generic_frame_closer_preserves_the_openers_supplied_rule_pattern() {
    let ir = convert("［＃ここから４字下げ、破線枠囲み］\n本文\n［＃ここで字下げ、枠囲み終わり］");
    assert_eq!(
        ir["layout_blocks"][0]["typography"]["border"],
        "dashed-rule"
    );
    assert!(ir["interpretation_problems"].as_array().unwrap().is_empty());
    let ir = convert("［＃ここから５字下げ、枠囲み］\n本文\n［＃ここで字下げ終わり］");
    assert_eq!(
        ir["layout_blocks"][0]["typography"]["border"],
        "unspecified"
    );
}

#[test]
fn a_line_measure_keeps_its_dimensions_whichever_order_the_source_writes_them() {
    // `{W}字組み` with no line count states the measure `{W}字詰め` states,
    // and `{W}字詰×{L}行` states the pair `{L}行{W}字組み` states.
    for (clause, width, lines) in [
        ("２０字組み", 20, None),
        ("２０字詰め", 20, None),
        ("１５字詰×５行", 15, Some(5)),
        ("５行１５字組みで", 15, Some(5)),
    ] {
        let ir = convert(&format!(
            "［＃ここから３字下げ、{clause}］\n本文\n［＃ここで字下げ終わり］"
        ));
        let scope = &ir["layout_blocks"][0];
        assert_eq!(scope["indent"], 3, "{clause}");
        assert_eq!(scope["width"], width, "{clause}");
        match lines {
            Some(count) => assert_eq!(scope["line_count"], count, "{clause}"),
            None => assert!(scope.get("line_count").is_none(), "{clause}"),
        }
        assert_eq!(
            ir["interpretation_problems"],
            serde_json::json!([]),
            "{clause}"
        );
    }
}

#[test]
fn a_foot_margin_stated_of_the_block_reads_as_the_margin_it_names() {
    for clause in ["地は本文より２字上", "地より２字上げ", "地から２字上げ"]
    {
        let ir = convert(&format!(
            "［＃ここから４字下げ、{clause}］\n本文\n［＃ここで字下げ終わり］"
        ));
        let scope = &ir["layout_blocks"][0];
        assert_eq!(scope["indent"], 4, "{clause}");
        assert_eq!(scope["offset_from_end"], 2, "{clause}");
        assert_eq!(
            ir["interpretation_problems"],
            serde_json::json!([]),
            "{clause}"
        );
    }
}

#[test]
fn a_margin_clause_whose_verb_contradicts_its_edge_stays_unresolved() {
    // `地から３字下げ` names the foot edge and then moves away from it. Reading
    // it as a foot margin would choose an intent the spelling does not state,
    // so the indentation is kept and the clause is retained as it stands.
    let ir = convert("［＃ここから２字下げ、地から３字下げ］\n本文\n［＃ここで字下げ終わり］");
    let scope = &ir["layout_blocks"][0];
    assert_eq!(scope["indent"], 2);
    assert!(scope.get("offset_from_end").is_none());
    assert!(
        ir["interpretation_problems"]
            .as_array()
            .unwrap()
            .iter()
            .any(|problem| problem["raw"] == "地から３字下げ"),
        "{ir}"
    );
}

#[test]
fn an_exception_naming_the_lines_it_applies_to_stays_unresolved() {
    // The source names a subset of the enclosed lines and gives it a different
    // indentation. Which lines those are is not recoverable without reading the
    // text, so the supplied scope indentation stands and the clause is kept.
    for clause in [
        "ただし冒頭の歌記号のみは２字下げ",
        "ただし改行行頭の「・」のみ１字下げ",
        "一つの行が複数行に渡る場合は２行目から２字下げ",
    ] {
        let ir = convert(&format!(
            "［＃ここから３字下げ、{clause}］\n本文\n［＃ここで字下げ終わり］"
        ));
        assert_eq!(ir["layout_blocks"][0]["indent"], 3, "{clause}");
        assert!(
            ir["interpretation_problems"]
                .as_array()
                .unwrap()
                .iter()
                .any(|problem| problem["raw"] == clause),
            "{clause}"
        );
    }
}

#[test]
fn a_close_that_restates_its_measure_ends_the_scope_that_measure_opened() {
    for closer in ["１字下げここまで", "1字下げ終わり", "１字下げ終り"] {
        let ir = convert(&format!(
            "［＃これより手紙文、１字下げ］\n拝啓\n［＃{closer}］\n地の文"
        ));
        let scope = &ir["layout_blocks"][0];
        assert_eq!(scope["indent"], 1, "{closer}");
        assert_eq!(scope["role"], "letter", "{closer}");
        assert_eq!(
            ir["interpretation_problems"],
            serde_json::json!([]),
            "{closer}"
        );
        // The close is where the source put it: the following line is outside.
        let end = scope["node_range"]["end"].as_u64().unwrap() as usize;
        let inside = serde_json::to_string(&ir["nodes"].as_array().unwrap()[..end]).unwrap();
        assert!(!inside.contains("地の文"), "{closer}: {inside}");
    }
}

#[test]
fn a_restated_measure_that_disagrees_with_the_open_does_not_close_it() {
    // The measure on the close is the source's own statement of which scope it
    // ends. Closing a scope it does not name would move the boundary to a place
    // the source did not put it.
    let ir = convert("［＃ここから１字下げ］\n本文\n［＃３字下げここまで］");
    assert_eq!(ir["layout_blocks"], serde_json::json!([]));
    assert!(!ir["interpretation_problems"].as_array().unwrap().is_empty());
}

#[test]
fn a_measured_close_reads_its_measure_in_either_numeral_spelling() {
    for (open, close, indent) in [
        ("ここから１字下げ", "ここで１字下げ終わり", 1),
        ("ここから二字下げ", "ここで二字下げ終わり", 2),
        ("ここから３字下げ", "ここで３字下げ終わり", 3),
    ] {
        let ir = convert(&format!("［＃{open}］\n本文\n［＃{close}］"));
        assert_eq!(ir["layout_blocks"][0]["indent"], indent, "{close}");
        assert_eq!(
            ir["interpretation_problems"],
            serde_json::json!([]),
            "{close}"
        );
    }
}

#[test]
fn an_indent_applied_at_every_break_stays_unresolved() {
    // `改行ごとに二字下げ` indents at each line break. Whether the scope's own
    // first line counts as following a break is what the source leaves open,
    // and a layout scope cannot carry a continuation indent without also
    // stating the indent of the first line. Choosing either reading would put a
    // measure on a line the source did not measure, so both markers are kept.
    let ir = convert("［＃改行ごとに二字下げ］前略我等両人［＃二字下げ終わり］");
    assert_eq!(ir["layout_blocks"], serde_json::json!([]));
    let problems = ir["interpretation_problems"].as_array().unwrap();
    assert_eq!(problems.len(), 2, "{problems:?}");
    let text = serde_json::to_string(&ir["nodes"]).unwrap();
    assert!(
        text.contains("前略我等両人"),
        "the enclosed text is kept: {text}"
    );
}

#[test]
fn a_close_naming_scopes_no_block_opened_stays_unresolved() {
    // `次行は…で` measures one named line and opens no scope. The close that
    // follows names an indentation and a foot alignment that were never opened
    // as blocks, so reading it as a close would end scopes that do not exist.
    let ir = convert(
        "［＃次行は三字下げ、九字空き地付きで］\n一金一百円也\n［＃字下げ、地付きここまで］",
    );
    assert!(
        ir["interpretation_problems"]
            .as_array()
            .unwrap()
            .iter()
            .any(|problem| problem["raw"] == "［＃字下げ、地付きここまで］"),
        "{:?}",
        ir["interpretation_problems"]
    );
}
