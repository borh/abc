//! Source-selected annotation targets retain rich transcription independently of spelling.

use ab_aozora_aat::aat_json_from_bytes;
use serde_json::Value;

#[test]
fn adjacent_source_targets_retain_rich_principal_content() {
    for (target, child_kind) in [
        ("どふ／＼", "iteration-mark"),
        ("※［＃濁点付き片仮名ヱ、1-7-84］", "gaiji"),
        ("※［＃「飮のへん＋旨」、341-5］", "gaiji"),
        ("白《タク》衾", "ruby"),
    ] {
        let marker = format!("［＃「{target}」に「マヽ」の注記］");
        let source = format!("前、{target}{marker}後");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let content = aat["blocks"][0]["content"].as_array().unwrap();
        let note = content
            .iter()
            .find(|node| node["kind"] == "annotated_text")
            .unwrap_or_else(|| panic!("source target was not attached: {source}"));
        assert!(
            note["content"]
                .as_array()
                .unwrap()
                .iter()
                .any(|node| node["kind"] == child_kind)
        );
        assert_eq!(note["span"]["byte_start"], "前、".len() + target.len());
        assert_eq!(
            note["span"]["byte_end"],
            "前、".len() + target.len() + marker.len()
        );
        assert_eq!(content.first().unwrap()["value"], "前、");
        assert_eq!(content.last().unwrap()["value"], "後");
    }
}

#[test]
fn different_source_target_is_not_replaced_by_a_visible_match() {
    let source = "前剌［＃「刺」の左に「テフダ」の注記］後";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let content = aat["blocks"][0]["content"].as_array().unwrap();
    assert!(content.iter().all(|node| node["kind"] != "annotated_text"));
    assert!(content.iter().any(
        |node| node["kind"] == "raw" && node["source"] == "［＃「刺」の左に「テフダ」の注記］"
    ));
}

#[test]
fn annotations_inside_ruby_bases_keep_the_full_reading_association() {
    for (base, target, reading) in [
        ("菌毒", "菌", "きんどく"),
        ("其駁雑", "駁雑", "そのはくざつ"),
        ("安然", "然", "あんぜん"),
    ] {
        let source = format!("前、{base}《{reading}》［＃「{target}」の左に「注」の注記］後");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let content = aat["blocks"][0]["content"].as_array().unwrap();
        let ruby = content.iter().find(|node| node["kind"] == "ruby").unwrap();
        assert_eq!(ruby["base"], base);
        assert_eq!(ruby["reading"], reading);
        let note = ruby["base_content"]
            .as_array()
            .unwrap()
            .iter()
            .find(|node| node["kind"] == "annotated_text")
            .unwrap();
        assert_eq!(note["content"][0]["value"], target);
        assert_eq!(note["position"], "left");
        assert!(content.iter().all(|node| node["kind"] != "raw"));
    }
}

#[test]
fn ambiguous_ruby_base_target_stays_unresolved() {
    let source = "菌菌《きんきん》［＃「菌」の左に「注」の注記］";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let content = aat["blocks"][0]["content"].as_array().unwrap();
    assert!(content.iter().any(|node| node["kind"] == "raw"));
    assert!(content.iter().all(|node| node["kind"] != "annotated_text"));
}
