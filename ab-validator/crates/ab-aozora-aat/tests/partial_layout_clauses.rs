//! Independent geometry survives source-located unresolved clauses.
use ab_aozora_aat::aat_json_from_bytes;
use serde_json::Value;

#[test]
fn unknown_clause_keeps_independent_geometry_and_exact_uncertainty() {
    let marker = "［＃ここから３字下げ、未対応指定、２０字詰め］";
    let source = format!("{marker}\n本文\n［＃ここで字下げ終わり］");
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let layout = &aat["blocks"][0];
    assert_eq!(layout["kind"], "layout_block", "{aat}");
    assert_eq!(layout["indent"], 3);
    assert_eq!(layout["width"], 20);
    let retained = &layout["children"][0]["content"][0];
    assert_eq!(retained["source"], "未対応指定");
    let start = usize::try_from(retained["span"]["byte_start"].as_u64().unwrap()).unwrap();
    let end = usize::try_from(retained["span"]["byte_end"].as_u64().unwrap()).unwrap();
    assert_eq!(&source[start..end], "未対応指定");
    let document = ab_aozora_facade::Document::new(source.as_str());
    assert!(document.parse().to_source().contains(marker));
    assert!(
        aat["meta"]["interpretation_facts"]
            .as_array()
            .is_none_or(Vec::is_empty)
    );
    assert!(layout.to_string().contains("未対応指定"));
    assert!(layout.to_string().contains("本文"));
}

#[test]
fn conflicting_width_is_not_selected_by_clause_order() {
    for widths in ["２０字詰め、２１字詰め", "２１字詰め、２０字詰め"] {
        let source = format!("［＃ここから３字下げ、{widths}］\n本文\n［＃ここで字下げ終わり］");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        assert_eq!(aat["blocks"][0]["indent"], 3, "{aat}");
        assert!(aat["blocks"][0].get("width").is_none());
        assert_eq!(
            aat["blocks"][0]["children"][0]["content"][0]["source"],
            widths
        );
        assert!(
            aat["meta"]["interpretation_facts"]
                .as_array()
                .is_none_or(Vec::is_empty)
        );
    }
}

#[test]
fn quoted_examples_do_not_become_layout_clauses() {
    let source = "［＃ここから３字下げ、未対応「例、２０字詰め、続」、ゴシック体］\n本文\n［＃ここで字下げ終わり］";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let layout = &aat["blocks"][0];
    assert_eq!(layout["indent"], 3, "{aat}");
    assert!(layout.get("width").is_none());
    assert_eq!(layout["formatting"]["style_type"], "gothic");
    assert_eq!(
        layout["children"][0]["content"][0]["source"],
        "未対応「例、２０字詰め、続」"
    );
}

#[test]
fn partial_clause_spans_remain_on_original_decoded_source() {
    let clause = "未対応〔cafe'〕指定";
    let source = format!(
        "\u{feff}題\r\n作者\r\n\r\n［＃ここから３字下げ、{clause}、２０字詰め］\r\n本文\r\n［＃ここで字下げ終わり］\r\n\r\n底本：本\r\n"
    );
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let layout = &aat["blocks"][0];
    assert_eq!(layout["indent"], 3, "{aat}");
    assert_eq!(layout["width"], 20);
    let retained = &layout["children"][0]["content"][0];
    assert_eq!(retained["source"], clause);
    let start = usize::try_from(retained["span"]["byte_start"].as_u64().unwrap()).unwrap();
    let end = usize::try_from(retained["span"]["byte_end"].as_u64().unwrap()).unwrap();
    assert_eq!(&source.trim_start_matches('\u{feff}')[start..end], clause);
}

#[test]
fn supplied_unknown_clauses_do_not_erase_indentation() {
    for (amount, clause) in [
        (3, "「甲」は返り点"),
        (5, "ここから数式"),
        (2, "横組み右揃えで"),
        (5, "本文よりひとまわり大きい太ゴシック体"),
        (4, "破線枠囲み"),
    ] {
        let source =
            format!("［＃ここから{amount}字下げ、{clause}］\n本文\n［＃ここで字下げ終わり］");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let layout = &aat["blocks"][0];
        assert_eq!(layout["indent"], amount, "{aat}");
        assert_eq!(layout["children"][0]["content"][0]["source"], clause);
        assert!(
            aat["meta"]["interpretation_facts"]
                .as_array()
                .is_none_or(Vec::is_empty)
        );
    }
}

#[test]
fn edition_layout_statements_do_not_change_principal_geometry() {
    for statement in [
        "底本では一行目は１字下げ",
        "底本では３字下げ",
        "底本では表組み",
    ] {
        let marker = format!("［＃ここから２字下げ、{statement}］");
        let source = format!("{marker}\n本文\n［＃ここで字下げ終わり］");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let layout = &aat["blocks"][0];
        assert_eq!(layout["indent"], 2, "{aat}");
        let note = &layout["children"][0]["content"][0];
        assert_eq!(note["kind"], "editorial_note");
        assert_eq!(note["note_kind"], "base-edition");
        assert_eq!(note["text"], statement);
        let start = usize::try_from(note["span"]["byte_start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(note["span"]["byte_end"].as_u64().unwrap()).unwrap();
        assert_eq!(&source[start..end], statement);
        assert_eq!(
            layout["children"][1]["content"]
                .as_array()
                .unwrap()
                .iter()
                .filter_map(|node| node["value"].as_str())
                .collect::<String>(),
            "本文"
        );
        assert_eq!(
            layout["interpretation_marker_spans"]
                .as_array()
                .unwrap()
                .len(),
            2
        );
        assert!(
            ab_aozora_facade::Document::new(source.as_str())
                .parse()
                .to_source()
                .contains(&marker)
        );
    }
}

#[test]
fn edition_statement_does_not_absorb_a_separate_unknown_clause() {
    let source =
        "［＃ここから２字下げ、底本では３字下げ、未知の指定］\n本文\n［＃ここで字下げ終わり］";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let layout = &aat["blocks"][0];
    let annotations = layout["children"][0]["content"].as_array().unwrap();
    assert_eq!(annotations[0]["kind"], "editorial_note");
    assert_eq!(annotations[0]["text"], "底本では３字下げ");
    assert_eq!(annotations[1]["kind"], "raw");
    assert_eq!(annotations[1]["source"], "未知の指定");
    assert!(
        layout["interpretation_marker_spans"]
            .as_array()
            .unwrap()
            .is_empty()
    );
}

#[test]
fn columns_compose_horizontal_transcription_and_edition_apparatus() {
    let marker = "［＃ここから２段組み、横書き、底本では前後の文とは改行しない］";
    let source = format!("{marker}\n甲《こう》乙\n［＃ここで段組み終わり］\n次");
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let layout = &aat["blocks"][0];
    assert_eq!(layout["column_count"], 2, "{aat}");
    assert_eq!(layout["direction"], "horizontal");
    assert!(layout.get("indent").is_none());
    assert_eq!(
        layout["children"][0]["content"][0]["text"],
        "底本では前後の文とは改行しない"
    );
    assert_eq!(
        layout["interpretation_marker_spans"]
            .as_array()
            .unwrap()
            .len(),
        2
    );
    let document = ab_aozora_facade::Document::new(source.as_str());
    let parsed = document.parse();
    assert!(parsed.to_source().contains(marker));
    let html = parsed.to_html();
    assert!(
        html.contains("aozora-container-columns aozora-container-yokogumi"),
        "{html}"
    );
    assert!(html.contains("data-columns=\"2\""));
}

#[test]
fn columns_count_mismatch_does_not_claim_a_complete_scope() {
    let source = "［＃ここから２段組み、横書き］\n本文\n［＃ここで３段組み終わり］";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    assert!(
        aat["meta"]["interpretation_facts"]
            .as_array()
            .is_none_or(Vec::is_empty),
        "{aat}"
    );
    assert!(aat.to_string().contains("本文"));
}

#[test]
fn bouten_keeps_current_target_and_base_edition_description_separate() {
    let marker = "［＃「あとさし」に傍点、底本では「あとさ」に傍点］";
    let source = format!("あとさし{marker}後");
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let content = aat["blocks"][0]["content"].as_array().unwrap();
    let style = content
        .iter()
        .find(|node| node["style_type"] == "bouten")
        .unwrap_or_else(|| panic!("{aat}"));
    assert_eq!(style["content"][0]["value"], "あとさし", "{aat}");
    let note = content
        .iter()
        .find(|node| node["kind"] == "editorial_note")
        .unwrap();
    assert_eq!(note["text"], "底本では「あとさ」に傍点");
    assert_eq!(note["note_kind"], "base-edition");
    let start = usize::try_from(note["span"]["byte_start"].as_u64().unwrap()).unwrap();
    let end = usize::try_from(note["span"]["byte_end"].as_u64().unwrap()).unwrap();
    assert_eq!(&source[start..end], marker);
    assert!(
        ab_aozora_facade::Document::new(source.as_str())
            .parse()
            .to_source()
            .contains(marker)
    );
}
