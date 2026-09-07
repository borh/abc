//! Page placement, line alignment and end margins are independent supplied axes.

use ab_aozora_aat::aat_json_from_bytes;
use serde_json::{Value, json};

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
        assert!(
            aat["meta"]["interpretation_facts"]
                .as_array()
                .is_none_or(|facts| facts.iter().all(|fact| fact["source_span"]["start"] != 0)),
            "{aat}"
        );
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
