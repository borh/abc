//! Plain and structured inline scopes retain both source marker identities.

use ab_aat::aat_json_from_bytes;
use serde_json::Value;

#[test]
fn plain_scope_claims_both_markers() {
    for (open, close) in [
        ("［＃斜体］", "［＃斜体終わり］"),
        ("［＃太字］", "［＃太字終わり］"),
    ] {
        let source = format!("前{open}本文{close}後");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        assert_eq!(
            aat["blocks"][0]["content"][1]["content"][0]["value"],
            "本文"
        );
        assert_eq!(aat["blocks"][0]["content"][0]["value"], "前");
        assert_eq!(aat["blocks"][0]["content"][2]["value"], "後");
        let spans: Vec<_> = aat["meta"]["interpretation_facts"]
            .as_array()
            .unwrap()
            .iter()
            .map(|fact| {
                let span = &fact["source_span"];
                let start = usize::try_from(span["start"].as_u64().unwrap()).unwrap();
                let end = usize::try_from(span["end"].as_u64().unwrap()).unwrap();
                &source[start..end]
            })
            .collect();
        assert_eq!(spans, [open, close], "{aat}");
    }
}

#[test]
fn paired_decoration_preserves_its_kind_and_target() {
    let source = "前［＃傍点］刻下の急務［＃傍点終わり］後";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let style = &aat["blocks"][0]["content"][1];
    assert_eq!(style["kind"], "style");
    assert_eq!(style["style_type"], "bouten");
    assert_eq!(style["decoration"]["kind"], "傍点");
    assert_eq!(style["decoration"]["position"], "right");
    assert_eq!(style["content"][0]["value"], "刻下の急務");
    assert_eq!(
        style["interpretation_marker_spans"]
            .as_array()
            .unwrap()
            .len(),
        2
    );
}

#[test]
fn different_decoration_closes_do_not_claim_a_pair() {
    let source = "前［＃傍点］刻下の急務［＃傍線終わり］後";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    assert!(aat["meta"].get("interpretation_facts").is_none());
    let content = aat["blocks"][0]["content"].as_array().unwrap();
    assert!(content.iter().any(|node| node["value"] == "刻下の急務"));
    assert!(content.iter().any(|node| node["source"] == "［＃傍点］"));
    assert!(
        content
            .iter()
            .any(|node| node["source"] == "［＃傍線終わり］")
    );
}
