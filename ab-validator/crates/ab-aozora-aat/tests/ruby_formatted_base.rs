//! Explicit ruby bases retain nested supplied formatting and exact source extent.

use ab_aozora_aat::aat_json_from_bytes;
use serde_json::Value;

#[test]
fn explicit_base_keeps_formatting_and_following_base_text() {
    for (source, base, reading, formatted) in [
        (
            "｜宜引［＃「引」は小書き右寄せ］縞《いいしま》",
            "宜引縞",
            "いいしま",
            "引",
        ),
        (
            "｜羯阿［＃「阿」は一段階小さな文字］迦《ぎゃあぎあ》",
            "羯阿迦",
            "ぎゃあぎあ",
            "阿",
        ),
        (
            "｜咳［＃「咳」は罫囲み］声《しわぶき》",
            "咳声",
            "しわぶき",
            "咳",
        ),
    ] {
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let nodes = aat["blocks"][0]["content"].as_array().unwrap();
        assert_eq!(nodes.len(), 1, "{aat}");
        let ruby = &nodes[0];
        assert_eq!(ruby["kind"], "ruby");
        assert_eq!(ruby["base"], base);
        assert_eq!(ruby["reading"], reading);
        let style = ruby["base_content"]
            .as_array()
            .unwrap()
            .iter()
            .find(|node| node.get("content").is_some())
            .unwrap();
        assert_eq!(style["content"][0]["value"], formatted);
        assert_eq!(ruby["interpretation_marker_spans"][0]["byte_start"], 0);
        assert_eq!(
            ruby["interpretation_marker_spans"][0]["byte_end"],
            source.len()
        );
    }
}

#[test]
fn independent_styles_share_one_explicit_base_without_changing_their_targets() {
    let source = "｜甲［＃「甲」は太字］乙［＃「乙」は斜体］丙《よみ》";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let ruby = &aat["blocks"][0]["content"][0];
    assert_eq!(ruby["base"], "甲乙丙");
    let children = ruby["base_content"].as_array().unwrap();
    assert_eq!(children[0]["content"][0]["value"], "甲");
    assert_eq!(children[1]["content"][0]["value"], "乙");
    assert_eq!(children[2]["value"], "丙");
    let document = ab_aozora_facade::Document::new(source);
    let tree = document.parse();
    assert_eq!(tree.to_source().trim_end(), source);
    let html = tree.to_html();
    assert!(
        html.contains("甲") && html.contains("乙") && html.contains("丙") && html.contains("よみ")
    );
}

#[test]
fn missing_or_empty_reading_replays_source_held_for_explicit_base() {
    for source in ["｜甲［＃「甲」は太字］乙", "｜甲［＃「甲」は太字］乙《》"]
    {
        let document = ab_aozora_facade::Document::new(source);
        let tree = document.parse();
        assert_eq!(tree.to_source().trim_end(), source);
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        assert!(
            aat["blocks"][0]["content"]
                .as_array()
                .unwrap()
                .iter()
                .all(|node| node["kind"] != "ruby")
        );
    }
}
