//! Supplied formula purpose shares source boundaries with independent indentation.
use ab_aozora_aat::aat_json_from_bytes;
use serde_json::Value;

fn parse(source: &str) -> Value {
    serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap()
}

#[test]
fn compound_formula_scope_preserves_role_geometry_and_rich_content() {
    let source = "［＃ここから５字下げ、ここから数式］\nW(r,t) = x2［＃「2」は上付き小文字］\n［＃ここで字下げ終わり、ここで数式終わり］";
    let aat = parse(source);
    let block = &aat["blocks"][0];
    assert_eq!(block["kind"], "layout_block", "{aat}");
    assert_eq!(block["role"], "formula", "{aat}");
    assert_eq!(block["children"][0]["indent"], 5);
    assert!(!aat.to_string().contains("interpretation_problem"), "{aat}");
    assert!(block.to_string().contains("W(r,t) = x"), "{aat}");
    let document = ab_aozora_facade::Document::new(source);
    let tree = document.parse();
    assert_eq!(
        tree.source_nodes()
            .iter()
            .filter(|node| node.source_span.start == 0)
            .count(),
        1
    );
    let canonical = tree.to_source();
    assert!(canonical.contains("ここから数式"), "{canonical}");
    assert!(canonical.contains("ここで数式終わり"), "{canonical}");
    let reparsed = ab_aozora_facade::Document::new(canonical.as_str());
    assert_eq!(reparsed.parse().to_html(), tree.to_html());
    assert_eq!(reparsed.parse().to_source(), canonical);
}

#[test]
fn supplied_formula_closer_does_not_certify_a_plain_indent_scope() {
    let source = "［＃ここから５字下げ］\nx = 1\n［＃ここで字下げ終わり、ここで数式終わり］";
    let aat = parse(source);
    assert!(aat.to_string().contains("interpretation_problem"), "{aat}");
}

#[test]
fn formula_purpose_does_not_hide_missing_scope_end_or_unknown_clauses() {
    for source in [
        "［＃ここから５字下げ、ここから数式］\nx = 1",
        "［＃ここから５字下げ、ここから数式、未対応の指定］\nx = 1\n［＃ここで字下げ終わり、ここで数式終わり］",
    ] {
        let aat = parse(source);
        assert!(aat.to_string().contains("interpretation_problem"), "{aat}");
        assert!(
            aat["meta"]["interpretation_facts"]
                .as_array()
                .into_iter()
                .flatten()
                .all(|fact| fact["source_span"]["start"] != 0),
            "{aat}"
        );
    }
}

#[test]
fn formula_scope_survives_independent_indentation_replacement() {
    let source = "［＃ここから５字下げ、ここから数式］\na = 1\n［＃ここから３字下げ］\nb = 2\n［＃ここから５字下げ］\nc = 3\n［＃ここで字下げ終わり、ここで数式終わり］";
    let aat = parse(source);
    let formula = &aat["blocks"][0];
    assert_eq!(formula["role"], "formula", "{aat}");
    let children = formula["children"].as_array().unwrap();
    assert_eq!(children.len(), 3, "{aat}");
    assert_eq!(
        children
            .iter()
            .map(|child| child["indent"].as_u64().unwrap())
            .collect::<Vec<_>>(),
        [5, 3, 5]
    );
    assert!(!aat.to_string().contains("interpretation_problem"), "{aat}");
    let facts = aat["meta"]["interpretation_facts"].as_array().unwrap();
    assert_eq!(
        facts
            .iter()
            .filter(|fact| fact["kind"] == "formula")
            .count(),
        2
    );
}
