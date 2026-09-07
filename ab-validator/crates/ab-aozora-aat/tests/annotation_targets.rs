//! Source-selected annotation targets retain rich transcription independently of spelling.

use ab_aozora_aat::aat_json_from_bytes;
use serde_json::Value;

#[test]
fn editorial_gloss_keeps_edition_provenance_and_original_ruby() {
    let statement =
        "校注、「枕橋の架してある堀の奥のところ」、ただし底本では校注が脱落、底本の親本にて確認";
    let source = format!("本所｜〆切《しめきり》［＃「〆切」に{statement}］後");
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let content = aat["blocks"][0]["content"].as_array().unwrap();
    let note = content
        .iter()
        .find(|node| node["kind"] == "annotated_text")
        .unwrap();
    let ruby = &note["content"][0];
    assert_eq!(ruby["kind"], "ruby");
    assert_eq!(ruby["base"], "〆切");
    assert_eq!(ruby["reading"], "しめきり");
    assert_eq!(note["note_kind"], "gloss");
    assert_eq!(note["annotation_content"][0]["value"], statement);
    assert!(note.get("position").is_none());
    assert!(content.iter().all(|node| node["kind"] != "raw"));
    let document = ab_aozora_facade::Document::new(source.as_str());
    let tree = document.parse();
    assert_eq!(tree.source(), source);
    let canonical = tree.to_source();
    assert_eq!(
        tree.to_html(),
        ab_aozora_facade::Document::new(canonical.as_str())
            .parse()
            .to_html()
    );
}

#[test]
fn editorial_gloss_does_not_guess_target_or_provenance() {
    for (base, statement) in [
        (
            "締切",
            "校注、「説明」、ただし底本では校注が脱落、底本の親本にて確認",
        ),
        ("〆切", "校注、「説明」、ただし底本の状態は不明"),
        (
            "〆切",
            "校注、「」、ただし底本では校注が脱落、底本の親本にて確認",
        ),
    ] {
        let source = format!("{base}《しめきり》［＃「〆切」に{statement}］");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        assert!(
            aat["blocks"][0]["content"]
                .as_array()
                .unwrap()
                .iter()
                .any(|node| node["kind"] == "raw")
        );
    }
}

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

#[test]
fn supplied_note_roles_keep_each_operand_occurrence_independent() {
    for (target, marker, role, word) in [
        (
            "（１）",
            "［＃「（１）」は注釈番号］",
            "annotation-number",
            "注釈番号",
        ),
        ("（一）", "［＃（一）は自注］", "author-note", "自注"),
        ("（一）", "［＃「（一）」は自注］", "author-note", "自注"),
    ] {
        let source = format!("前{target}{marker}中{target}{marker}後");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let content = aat["blocks"][0]["content"].as_array().unwrap();
        let notes = content
            .iter()
            .filter(|node| node["kind"] == "annotated_text")
            .collect::<Vec<_>>();
        assert_eq!(notes.len(), 2, "{source}");
        for note in &notes {
            assert_eq!(note["note_kind"], role);
            assert_eq!(note["content"][0]["value"], target);
            assert_eq!(note["annotation_content"][0]["value"], word);
            assert!(note.get("position").is_none());
            let span = &note["span"];
            let start = usize::try_from(span["byte_start"].as_u64().unwrap()).unwrap();
            let end = usize::try_from(span["byte_end"].as_u64().unwrap()).unwrap();
            assert_eq!(&source[start..end], marker);
        }
        assert_ne!(notes[0]["span"], notes[1]["span"]);
    }
}
