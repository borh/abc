use ab_aozora_aat::aat_json_from_bytes;
use serde_json::Value;

#[test]
fn accent_scope_delimiters_are_syntax_but_literal_brackets_are_text() {
    for (source, expected) in [
        ("〔a` la Huysmans〕", "à la Huysmans"),
        ("〔本全集〕", "〔本全集〕"),
        ("前〔a`〕中〔本全集〕後", "前à中〔本全集〕後"),
    ] {
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let actual: String = aat["blocks"][0]["content"]
            .as_array()
            .unwrap()
            .iter()
            .map(|node| node["value"].as_str().expect("visible text node"))
            .collect();
        assert_eq!(actual, expected, "{source}");
    }
}
