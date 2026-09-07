//! Implicit ruby uses one adjacent script class and preserves explicit boundaries.

use ab_aozora_aat::aat_json_from_bytes;
use ab_aozora_facade::Document;
use serde_json::Value;

fn nodes(source: &str) -> Vec<Value> {
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    aat["blocks"][0]["content"].as_array().unwrap().clone()
}

#[test]
fn cyrillic_and_decimal_runs_keep_their_supplied_readings() {
    for (source, base, reading) in [
        ("現代のСССР《エスエスエスエル》", "СССР", "エスエスエスエル"),
        ("М《ム》", "М", "ム"),
        ("Ёё《よ》", "Ёё", "よ"),
        ("８《エイト》", "８", "エイト"),
        ("12３《すうじ》", "12３", "すうじ"),
    ] {
        let content = nodes(source);
        let ruby = content.iter().find(|node| node["kind"] == "ruby").unwrap();
        assert_eq!(ruby["base"], base);
        assert_eq!(ruby["reading"], reading);
        let canonical = Document::new(source).parse().to_source();
        let reparsed = nodes(&canonical);
        let reparsed_ruby = reparsed.iter().find(|node| node["kind"] == "ruby").unwrap();
        assert_eq!(reparsed_ruby["base"], base);
        assert_eq!(reparsed_ruby["reading"], reading);
    }
}

#[test]
fn classes_and_explicit_bars_bound_the_base() {
    for (source, base) in [
        ("ABCЯ《ヤ》", "Я"),
        ("Яa《エー》", "a"),
        ("９｜８《はち》", "８"),
        ("｜Я８《やはち》", "Я８"),
    ] {
        let content = nodes(source);
        let ruby = content.iter().find(|node| node["kind"] == "ruby").unwrap();
        assert_eq!(ruby["base"], base);
        let canonical = Document::new(source).parse().to_source();
        let reparsed = nodes(&canonical);
        assert_eq!(
            reparsed.iter().find(|node| node["kind"] == "ruby").unwrap()["base"],
            base
        );
    }
    for source in ["Я 《や》", "８.《はち》"] {
        assert!(nodes(source).iter().all(|node| node["kind"] != "ruby"));
    }
}
