//! Qualitative font instructions preserve supplied degree without numbered stages.

use ab_aozora_aat::aat_json_from_bytes;
use serde_json::Value;

#[test]
fn qualitative_size_retains_direction_and_supplied_qualifier() {
    for (clause, direction, qualifier) in [
        ("小さい活字", "smaller", None),
        (
            "字のポイントはやや小さくしてある。",
            "smaller",
            Some("やや"),
        ),
        (
            "本文よりひとまわり大きい太ゴシック体",
            "larger",
            Some("ひとまわり"),
        ),
    ] {
        let source = format!("［＃ここから２字下げ、{clause}］\n本文\n［＃ここで字下げ終わり］");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let formatting = &aat["blocks"][0]["formatting"];
        let attributes = formatting
            .as_array()
            .map_or_else(|| vec![formatting], |values| values.iter().collect());
        let font = attributes
            .iter()
            .find(|value| value["kind"] == "font_size")
            .unwrap();
        assert!(
            aat["meta"]["interpretation_facts"]
                .as_array()
                .unwrap()
                .iter()
                .any(|fact| fact["kind"] == "emphasis" && fact["source_span"]["start"] == 0),
            "{aat}"
        );
        assert_eq!(font["size_type"], "qualitative", "{aat}");
        assert_eq!(font["direction"], direction, "{aat}");
        assert_eq!(font["qualifier"].as_str(), qualifier, "{aat}");
        assert!(font.get("level").is_none(), "{aat}");
        assert!(!aat.to_string().contains("interpretation_problem"), "{aat}");
        if qualifier == Some("ひとまわり") {
            for style in ["bold", "gothic"] {
                assert!(
                    attributes.iter().any(|value| value["style_type"] == style),
                    "{aat}"
                );
            }
        }
    }
}

#[test]
fn canonical_serialization_preserves_degree_and_coapplied_weight() {
    for clause in [
        "小さい活字",
        "字のポイントはやや小さくしてある。",
        "本文よりひとまわり大きい太ゴシック体",
    ] {
        let source = format!("［＃ここから２字下げ、{clause}］\n本文\n［＃ここで字下げ終わり］");
        let document = ab_aozora_facade::Document::new(source.as_str());
        let tree = document.parse();
        let canonical = tree.to_source();
        let before: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let after: Value =
            serde_json::from_slice(&aat_json_from_bytes(canonical.as_bytes()).unwrap()).unwrap();
        assert_eq!(
            before["blocks"][0]["formatting"],
            after["blocks"]
                .as_array()
                .unwrap()
                .iter()
                .find(|block| block["kind"] == "layout_block")
                .unwrap()["formatting"],
            "{canonical}\n{after}"
        );
        let html = tree.to_html();
        assert!(!html.contains("data-steps"));
        if clause.contains("太ゴシック体") {
            assert!(html.contains("aozora-container-futoji"), "{html}");
            assert!(html.contains("aozora-container-goshikku"), "{html}");
        }
        assert!(!after.to_string().contains("interpretation_problem"));
    }
}

#[test]
fn conflicting_qualitative_sizes_keep_weight_without_selecting_a_size() {
    for clauses in [
        "小さい活字、本文よりひとまわり大きい太ゴシック体",
        "本文よりひとまわり大きい太ゴシック体、小さい活字",
    ] {
        let source = format!("［＃ここから２字下げ、{clauses}］\n本文\n［＃ここで字下げ終わり］");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let block = &aat["blocks"][0];
        assert_eq!(block["indent"], 2);
        let styles = block["formatting"].as_array().unwrap();
        assert!(
            styles.iter().all(|style| style["kind"] != "font_size"),
            "{aat}"
        );
        assert!(
            styles.iter().any(|style| style["style_type"] == "bold"),
            "{aat}"
        );
        assert!(aat.to_string().contains("interpretation_problem"), "{aat}");
        assert!(
            aat["meta"]["interpretation_facts"]
                .as_array()
                .unwrap()
                .iter()
                .all(|fact| fact["source_span"]["start"] != 0),
            "{aat}"
        );
    }
}
