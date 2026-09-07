//! Ruby association survives realization of source-encoded Latin letters.
use ab_aozora_aat::aat_json_from_bytes;
use ab_aozora_facade::{Document, syntax::accent::ACCENT_TABLE};
use serde_json::Value;

#[test]
fn source_accents_preserve_the_whole_adjacent_latin_base() {
    for (source_base, expected) in [
        ("〔e'galite'〕", "égalité"),
        ("〔Charite'〕", "Charité"),
        ("〔Ske^ne^〕", "Skênê"),
        ("〔Orche^stra〕", "Orchêstra"),
        ("〔Proske^nion〕", "Proskênion"),
    ] {
        let source = format!("前{source_base}《よみ》後");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let nodes = aat["blocks"][0]["content"].as_array().unwrap();
        let ruby = nodes
            .iter()
            .find(|n| n["kind"] == "ruby")
            .unwrap_or_else(|| panic!("{aat}"));
        assert_eq!(ruby["base"], expected);
        assert_eq!(ruby["reading"], "よみ");
        assert!(nodes.iter().all(|n| n["kind"] != "raw"), "{aat}");
    }
}

#[test]
fn accent_letters_share_the_latin_class_without_crossing_other_boundaries() {
    for (source, expected) in [
        ("前〔e'galite'〕《よみ》", "égalité"),
        ("〔cafe'〕｜〔e'〕《よみ》", "é"),
        ("〔e'〕Я《よみ》", "Я"),
        ("Я〔e'〕《よみ》", "é"),
    ] {
        let canonical = Document::new(source).parse().to_source();
        for spelling in [source, canonical.as_str()] {
            let aat: Value =
                serde_json::from_slice(&aat_json_from_bytes(spelling.as_bytes()).unwrap()).unwrap();
            let nodes = aat["blocks"][0]["content"].as_array().unwrap();
            let ruby = nodes.iter().find(|n| n["kind"] == "ruby").unwrap();
            assert_eq!(ruby["base"], expected, "{spelling}");
        }
    }
    for source in [
        "〔e'〕 《よみ》",
        "〔e'〕.《よみ》",
        "×《よみ》",
        "÷《よみ》",
    ] {
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        assert!(
            aat["blocks"][0]["content"]
                .as_array()
                .unwrap()
                .iter()
                .all(|n| n["kind"] != "ruby"),
            "{source}"
        );
    }
}

#[test]
fn all_composed_accent_letters_survive_ruby_association() {
    let mut source = String::new();
    for (_, letter) in ACCENT_TABLE {
        source.push('x');
        source.push(*letter);
        source.push_str("《よみ》 ");
    }
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let bases: Vec<_> = aat["blocks"][0]["content"]
        .as_array()
        .unwrap()
        .iter()
        .filter(|n| n["kind"] == "ruby")
        .map(|n| n["base"].as_str().unwrap())
        .collect();
    let expected: Vec<_> = ACCENT_TABLE
        .iter()
        .map(|(_, letter)| format!("x{letter}"))
        .collect();
    assert_eq!(bases, expected);
}
