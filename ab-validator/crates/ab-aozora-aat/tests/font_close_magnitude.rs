//! Supplied font-size closer magnitude constrains source scope ownership.
use ab_aozora_aat::aat_json_from_bytes;
use serde_json::Value;

#[test]
fn matching_and_omitted_closer_magnitudes_preserve_the_scope() {
    for separator in ["", "\n"] {
        for closer in ["ここで1段階小さな文字終わり", "ここで小さな文字終わり"]
        {
            let source = format!(
                "［＃ここから1段階小さな文字］{separator}甲《こう》乙{separator}［＃{closer}］\n後"
            );
            let aat: Value =
                serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
            let facts = aat["meta"]["interpretation_facts"].as_array().unwrap();
            assert_eq!(
                facts.iter().filter(|f| f["kind"] == "emphasis").count(),
                2,
                "{aat}"
            );
            assert!(
                ab_aozora_facade::Document::new(source.as_str())
                    .parse()
                    .to_source()
                    .contains(closer)
            );
        }
    }
}

#[test]
fn contradictory_closer_attributes_never_establish_a_scope() {
    for separator in ["", "\n"] {
        for closer in ["ここで2段階小さな文字終わり", "ここで1段階大きな文字終わり"]
        {
            let source =
                format!("［＃ここから1段階小さな文字］{separator}甲{separator}［＃{closer}］");
            let aat: Value =
                serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
            assert!(
                !aat["meta"]["interpretation_facts"]
                    .as_array()
                    .into_iter()
                    .flatten()
                    .any(|f| f["kind"] == "emphasis"),
                "{aat}"
            );
            assert!(aat.to_string().contains(closer), "{aat}");
        }
    }
}
