//! End alignment owns the supplied target without swallowing preceding prose.
use ab_aat::aat_json_from_bytes;
use serde_json::Value;

fn parse(source: &str) -> Value {
    serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap()
}

#[test]
fn quoted_end_offsets_preserve_the_exact_adjacent_target() {
    for (target, offset) in [("頓首　敬白", 3), ("（裏面欧文番組略）", 1)] {
        let source = format!("前文。{target}［＃「{target}」は地付き、地より{offset}字アキ］\n");
        let aat = parse(&source);
        let content = aat["blocks"][0]["content"].as_array().unwrap();
        assert_eq!(content[0]["value"], "前文。", "{aat}");
        let aligned = &content[1];
        assert_eq!(aligned["kind"], "chitsuki", "{aat}");
        assert_eq!(aligned["offset_from_end"], offset, "{aat}");
        assert_eq!(aligned["content"][0]["value"], target, "{aat}");
        assert!(!aat.to_string().contains("interpretation_problem"), "{aat}");
    }
}

#[test]
fn inline_prefix_aligns_only_following_text_on_the_same_source_line() {
    for marker in ["地付きで", "地付き", "地から２字上げ"] {
        let aat = parse(&format!("前文。［＃{marker}］（完）\n次の行\n"));
        assert_eq!(aat["blocks"][0]["kind"], "paragraph", "{aat}");
        assert_eq!(aat["blocks"][0]["content"][0]["value"], "前文。", "{aat}");
        let aligned = &aat["blocks"][1];
        assert_eq!(aligned["kind"], "layout_block", "{aat}");
        assert_eq!(
            aligned["children"][0]["content"][0]["value"], "（完）",
            "{aat}"
        );
        assert!(!aat.to_string().contains("interpretation_problem"), "{aat}");
    }
}

#[test]
fn quoted_alignment_does_not_guess_a_missing_target() {
    for body in ["別の文", "署。名"] {
        let aat = parse(&format!("{body}［＃「署名」は地付き、地より３字アキ］\n"));
        assert!(aat.to_string().contains("interpretation_problem"), "{aat}");
        assert!(
            !aat["meta"]["interpretation_facts"]
                .to_string()
                .contains("line-layout"),
            "{aat}"
        );
    }
}

#[test]
fn a_native_owned_nonadjacent_target_does_not_include_intervening_prose() {
    let aat = parse("署名。続き［＃「署名」は地付き、地より３字アキ］\n");
    let content = &aat["blocks"][0]["content"];
    assert_eq!(content[0]["kind"], "chitsuki", "{aat}");
    assert_eq!(content[0]["content"][0]["value"], "署名", "{aat}");
    assert_eq!(content[1]["value"], "。続き", "{aat}");
}

#[test]
fn rich_quoted_target_retains_ruby_structure() {
    let aat = parse("前。署名《しょめい》［＃「署名」は地付き、地より３字アキ］\n");
    let aligned = &aat["blocks"][0]["content"][1];
    assert_eq!(aligned["kind"], "chitsuki", "{aat}");
    assert_eq!(aligned["content"][0]["kind"], "ruby", "{aat}");
    assert_eq!(aligned["content"][0]["reading"], "しょめい", "{aat}");
}

#[test]
fn rich_quoted_target_retains_the_supplied_gaiji_node() {
    let target = "※［＃濁点付き片仮名ヱ、1-7-84］";
    let aat = parse(&format!(
        "前。{target}［＃「{target}」は地付き、地より３字アキ］\n"
    ));
    let aligned = &aat["blocks"][0]["content"][1];
    assert_eq!(aligned["kind"], "chitsuki", "{aat}");
    assert_eq!(aligned["content"][0]["kind"], "gaiji", "{aat}");
    assert_eq!(aligned["content"].as_array().unwrap().len(), 1, "{aat}");
}

#[test]
fn region_anchor_before_line_alignment_does_not_create_an_empty_paragraph() {
    for (opener, closer) in [
        ("ここから３字下げ", "ここで字下げ終わり"),
        ("ここから１段階小さな文字", "ここで小さな文字終わり"),
    ] {
        let source = format!("［＃{opener}］\n［＃地から３字上げ］宛先\n［＃{closer}］\n");
        let aat = parse(&source);
        let children = aat["blocks"][0]["children"].as_array().unwrap();
        assert_eq!(children.len(), 1, "{aat}");
        let aligned = &children[0];
        assert_eq!(aligned["kind"], "layout_block", "{aat}");
        let paragraph = &aligned["children"][0];
        let content = paragraph["content"].as_array().unwrap();
        assert_eq!(content[0]["value"], "", "{aat}");
        assert_eq!(
            content[0]["span"]["byte_start"],
            source.find("［＃地から").unwrap(),
            "{aat}"
        );
        assert_eq!(
            content[0]["span"]["byte_start"], content[0]["span"]["byte_end"],
            "{aat}"
        );
        assert!(content.iter().any(|node| node["value"] == "宛先"), "{aat}");
        assert_eq!(aligned["children"].as_array().unwrap().len(), 1, "{aat}");
    }
}
