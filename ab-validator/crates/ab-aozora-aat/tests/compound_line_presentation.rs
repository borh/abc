//! Co-applied line typography and end spacing retain one supplied target.
use ab_aozora_aat::aat_json_from_bytes;
use ab_aozora_facade::Document;
use serde_json::Value;

fn parse(source: &str) -> Value {
    serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap()
}

#[test]
fn gothic_end_spacing_preserves_the_line_and_enclosing_indent() {
    let marker = "［＃ゴシック体、地付き、地より２字あげ］";
    let source = format!(
        "［＃ここから１字下げ］\n前の行。\n段原興行王　談{marker}\n次の行。\n［＃ここで字下げ終わり］\n"
    );
    let aat = parse(&source);
    let outer = &aat["blocks"][0];
    assert_eq!(outer["indent"], 1, "{aat}");
    let children = outer["children"].as_array().unwrap();
    assert_eq!(children.len(), 3, "{aat}");
    let aligned = &children[1];
    assert_eq!(aligned["kind"], "layout_block", "{aat}");
    assert_eq!(aligned["align"], "right", "{aat}");
    assert_eq!(aligned["offset_from_end"], 2, "{aat}");
    assert_eq!(aligned["formatting"]["style_type"], "gothic", "{aat}");
    assert!(aligned.get("indent").is_none(), "{aat}");
    assert_eq!(
        aligned["children"][0]["content"][0]["value"], "段原興行王　談",
        "{aat}"
    );
    let start = source.find(marker).unwrap();
    let facts = aat["meta"]["interpretation_facts"].as_array().unwrap();
    for kind in ["line-layout", "emphasis"] {
        assert!(
            facts.iter().any(|fact| fact["kind"] == kind
                && fact["source_span"]["start"] == start
                && fact["source_span"]["end"] == start + marker.len()),
            "{aat}"
        );
    }
}

#[test]
fn compound_line_source_serialization_keeps_both_axes() {
    let source = "段原興行王　談［＃ゴシック体、地付き、地より２字あげ］\n";
    let document = Document::new(source);
    let tree = document.parse();
    let html = tree.to_html();
    assert!(html.contains("aozora-line-goshikku"), "{html}");
    assert!(html.contains("aozora-align-end-2"), "{html}");
    let canonical = tree.to_source();
    let reparsed = Document::new(canonical.as_str());
    assert_eq!(reparsed.parse().to_html(), html);
    assert_eq!(reparsed.parse().to_source(), canonical);
}

#[test]
fn compound_line_does_not_ignore_conflicting_or_unknown_clauses() {
    for body in [
        "ゴシック体、地付き、地より２字あげ、地より３字あげ",
        "ゴシック体、地付き、地より２字あげ、未知の指定",
        "ゴシック体、地付き、地より字あげ",
    ] {
        let source = format!("段原興行王　談［＃{body}］\n");
        let aat = parse(&source);
        assert!(aat.to_string().contains("interpretation_problem"), "{aat}");
        assert!(
            !aat.to_string().contains("\"style_type\":\"gothic\""),
            "{aat}"
        );
    }
}
