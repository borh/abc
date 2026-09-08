//! Page placement, line alignment and end margins are independent supplied axes.

use ab_aat::aat_json_from_bytes;
use serde_json::{Value, json};

#[test]
fn repeated_column_opener_keeps_indentation_and_hanging_width() {
    let opener = "［＃ここから一字下げ、折り返して二字下げ、ここから二段組］";
    let closer = "［＃ここで字下げ終わり、ここで段組終わり］";
    let source = format!("{opener}\n一、本文。\n二、続き。\n{closer}\n外。\n");
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let scope = &aat["blocks"][0];
    assert_eq!(scope["indent"], 1, "{aat}");
    assert_eq!(scope["continuation_indent"], 2, "{aat}");
    assert_eq!(scope["column_count"], 2, "{aat}");
    assert_eq!(
        scope["span"]["byte_end"],
        source.find(closer).unwrap() + closer.len(),
        "{aat}"
    );
    assert!(
        !scope.to_string().contains("interpretation_problem"),
        "{aat}"
    );
    assert!(!scope.to_string().contains("外。"), "{aat}");
    let document = ab_aozora_facade::Document::new(source.as_str());
    let serialized = document.parse().to_source();
    let reparsed: Value =
        serde_json::from_slice(&aat_json_from_bytes(serialized.as_bytes()).unwrap()).unwrap();
    let restored = reparsed["blocks"]
        .as_array()
        .unwrap()
        .iter()
        .find(|block| block["kind"] == "layout_block")
        .unwrap();
    assert_eq!(restored["column_count"], 2, "{reparsed}");
    assert_eq!(restored["continuation_indent"], 2, "{reparsed}");
    assert!(!serialized.contains("改段"), "{serialized}");
}

#[test]
fn conflicting_column_counts_preserve_only_independent_geometry() {
    for clause in [
        "ここから二段組、三段組",
        "三段組、ここから二段組",
        "ここから０段組",
    ] {
        let source = format!(
            "［＃ここから一字下げ、折り返して二字下げ、{clause}］\n本文。\n［＃ここで字下げ終わり］"
        );
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let scope = &aat["blocks"][0];
        assert_eq!(scope["indent"], 1, "{aat}");
        assert_eq!(scope["continuation_indent"], 2, "{aat}");
        assert!(scope.get("column_count").is_none(), "{aat}");
        assert!(
            scope.to_string().contains("interpretation_problem"),
            "{aat}"
        );
        assert!(
            !aat["meta"]["interpretation_facts"]
                .as_array()
                .unwrap()
                .iter()
                .any(|fact| fact["source_span"]["start"] == 0),
            "{aat}"
        );
    }
}

#[test]
fn supplied_geometry_does_not_collapse_independent_axes() {
    for (clauses, fields) in [
        (
            "３０字詰め、ページ左右中央",
            json!({"width":30,"page_placement":"horizontal-center"}),
        ),
        (
            "ページの左右中央、中央揃え",
            json!({"page_placement":"horizontal-center","align":"center"}),
        ),
        (
            "横組み右揃えで",
            json!({"direction":"horizontal","align":"right"}),
        ),
        ("地より１字上げ", json!({"offset_from_end":1})),
    ] {
        let source = format!("［＃ここから３字下げ、{clauses}］\n本文\n［＃ここで字下げ終わり］");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let block = &aat["blocks"][0];
        assert_eq!(block["indent"], 3, "{aat}");
        for (key, expected) in fields.as_object().unwrap() {
            assert_eq!(&block[key], expected, "{aat}");
        }
        assert!(
            !block.to_string().contains("interpretation_problem"),
            "{aat}"
        );
    }
}

#[test]
fn conflicting_line_alignment_does_not_erase_page_placement() {
    for alignments in ["中央揃え、右揃え", "右揃え、中央揃え"] {
        let source = format!(
            "［＃ここから３字下げ、ページの左右中央、{alignments}］\n本文\n［＃ここで字下げ終わり］"
        );
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let block = &aat["blocks"][0];
        assert_eq!(block["page_placement"], "horizontal-center", "{aat}");
        assert!(block.get("align").is_none(), "{aat}");
        assert!(block.to_string().contains(alignments), "{aat}");
        let facts = aat["meta"]["interpretation_facts"].as_array().unwrap();
        let close = "［＃ここで字下げ終わり］";
        assert_eq!(facts.len(), 1, "{aat}");
        assert_eq!(facts[0]["kind"], "line-layout");
        assert_eq!(
            facts[0]["source_span"]["start"],
            source.rfind(close).unwrap()
        );
        assert_eq!(facts[0]["source_span"]["end"], source.len());
    }
}

#[test]
fn canonical_source_and_native_html_keep_placement_separate() {
    let source = "［＃ここから３字下げ、ページ左右中央、右揃え、地より１字上げ］\n本文\n［＃ここで字下げ終わり］";
    let document = ab_aozora_facade::Document::new(source);
    let tree = document.parse();
    let html = tree.to_html();
    assert!(
        html.contains("aozora-container-page-horizontal-center"),
        "{html}"
    );
    assert!(html.contains("aozora-container-align-right"), "{html}");
    assert!(!html.contains("aozora-container-center"), "{html}");
    assert!(html.contains("padding-inline-end: 1em"), "{html}");
    let canonical = tree.to_source();
    let reparsed = ab_aozora_facade::Document::new(canonical.as_str());
    assert_eq!(reparsed.parse().to_html(), html);
    assert_eq!(reparsed.parse().to_source(), canonical);
}

#[test]
fn compound_scope_claims_each_established_presentation_family() {
    for (clause, close, content) in [
        ("横組み右揃えで", "字下げ、横組み終わり", "本文"),
        ("罫囲み", "字下げ、罫囲み終わり", "本文"),
        ("枠囲み", "字下げ、枠囲み終わり", "本文"),
        (
            "横組み右揃えで",
            "字下げ、横組み終わり",
            "本文\n［＃横組みの下に、左右中央縦組みで］\n縦の説明",
        ),
    ] {
        let opener = format!("［＃ここから２字下げ、{clause}］");
        let closer = format!("［＃ここで{close}］");
        let source = format!("{opener}\n{content}\n{closer}");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let facts = aat["meta"]["interpretation_facts"].as_array().unwrap();
        for start in [0, source.find(&closer).unwrap()] {
            for kind in ["line-layout", "layout"] {
                assert!(
                    facts
                        .iter()
                        .any(|fact| fact["kind"] == kind && fact["source_span"]["start"] == start),
                    "{aat}"
                );
            }
        }
        assert!(!aat.to_string().contains("interpretation_problem"), "{aat}");
    }
}
