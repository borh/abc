//! Equivalent end-offset spellings preserve the supplied line and its source markers.
use ab_aat::aat_json_from_bytes;
use serde_json::Value;

fn parse(source: &str) -> Value {
    serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap()
}

#[test]
fn supplied_end_spacing_and_kanji_counts_use_the_existing_offset() {
    for (marker, offset, prefix) in [
        ("地から一字上げ", 1, true),
        ("地から五字上げ", 5, true),
        ("地付き、地より１字アキ", 1, false),
        ("地付き、地より２字あき", 2, false),
        ("地付き、地より８字アキ", 8, false),
    ] {
        let source = if prefix {
            format!("前の行\n［＃{marker}］署名\n次の行\n")
        } else {
            format!("前の行\n署名［＃{marker}］\n次の行\n")
        };
        let aat = parse(&source);
        let layout = &aat["blocks"][1];
        assert_eq!(layout["align"], "right", "{source}: {aat}");
        assert_eq!(layout["offset_from_end"], offset, "{source}: {aat}");
        assert_eq!(layout["children"][0]["content"][0]["value"], "署名");
        assert!(!aat.to_string().contains("interpretation_problem"), "{aat}");
        let document = ab_aozora_facade::Document::new(source.as_str());
        let tree = document.parse();
        let canonical = tree.to_source();
        let reparsed = ab_aozora_facade::Document::new(canonical.as_str());
        assert_eq!(reparsed.parse().to_html(), tree.to_html());
        assert_eq!(reparsed.parse().to_source(), canonical);
    }
}

#[test]
fn repeated_offset_on_the_same_complete_line_does_not_accumulate_margin() {
    let source = "前\n［＃地から８字上げ］美作守内［＃地付き、地より８字アキ］\n後\n";
    let aat = parse(source);
    let layout = &aat["blocks"][1];
    assert_eq!(layout["offset_from_end"], 8, "{aat}");
    assert_eq!(layout["children"][0]["kind"], "paragraph", "{aat}");
    assert_eq!(
        layout["interpretation_marker_spans"]
            .as_array()
            .unwrap()
            .len(),
        2
    );
    assert!(!aat.to_string().contains("interpretation_problem"), "{aat}");
}

#[test]
fn contradictory_line_end_offsets_preserve_both_assertions_without_a_chosen_margin() {
    let source = "［＃地から１字上げ］署名［＃地付き、地より２字あき］\n";
    let aat = parse(source);
    assert_eq!(aat["blocks"][0]["kind"], "paragraph", "{aat}");
    let problems = aat["blocks"][0]["content"].as_array().unwrap();
    assert_eq!(
        problems
            .iter()
            .filter(|node| node.get("interpretation_problem").is_some())
            .count(),
        2,
        "{aat}"
    );
    assert!(
        !aat["meta"]["interpretation_facts"]
            .to_string()
            .contains("line-layout"),
        "{aat}"
    );
}

#[test]
fn incomplete_or_unbounded_spacing_suffixes_remain_unknown() {
    for marker in [
        "地付き、地より２字",
        "地付き、地より300字アキ",
        "地付き、地より２字あきか？",
        "地から一五字上げ",
    ] {
        let aat = parse(&format!("署名［＃{marker}］\n"));
        assert!(aat.to_string().contains("interpretation_problem"), "{aat}");
    }
}

#[test]
fn an_enclosing_offset_scope_is_not_a_repeated_line_assertion() {
    let aat = parse(
        "［＃ここから地から８字上げ］\n署名［＃地付き、地より８字アキ］\n［＃ここで字上げ終わり］\n",
    );
    let outer = &aat["blocks"][0];
    assert_eq!(outer["offset_from_end"], 8, "{aat}");
    assert_eq!(outer["children"][0]["offset_from_end"], 8, "{aat}");
}
