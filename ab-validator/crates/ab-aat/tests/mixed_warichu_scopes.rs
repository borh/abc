//! Supplied warichu delimiter aliases retain exact scope ownership.

use ab_aat::aat_json_from_bytes;
use serde_json::Value;

#[test]
fn generic_opener_accepts_bare_closer_without_losing_neighbors() {
    for open in ["［＃ここから割り注］", "［＃割り注］"] {
        let close = "［＃割り注終わり］";
        let source = format!("前{open}隱五年{close}後");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let content = &aat["blocks"][0]["content"];
        assert_eq!(content[0]["value"], "前", "{aat}");
        assert_eq!(content[1]["kind"], "warichu", "{aat}");
        assert_eq!(content[1]["content"][0]["value"], "隱五年");
        assert_eq!(content[2]["value"], "後");
        for (span, expected) in content[1]["interpretation_marker_spans"]
            .as_array()
            .unwrap()
            .iter()
            .zip([open, close])
        {
            let start = usize::try_from(span["byte_start"].as_u64().unwrap()).unwrap();
            let end = usize::try_from(span["byte_end"].as_u64().unwrap()).unwrap();
            assert_eq!(&source[start..end], expected);
        }
    }
}

fn warichu_count(value: &Value) -> usize {
    match value {
        Value::Object(_) => {
            usize::from(value["kind"] == "warichu")
                + warichu_count(&value["content"])
                + warichu_count(&value["children"])
        }
        Value::Array(values) => values.iter().map(warichu_count).sum(),
        _ => 0,
    }
}

#[test]
fn nested_bare_and_generic_scopes_close_the_nearest_opener() {
    for (outer, inner) in [
        ("［＃ここから割り注］", "［＃割り注］"),
        ("［＃割り注］", "［＃ここから割り注］"),
        ("［＃ここから割り注］", "［＃ここから割り注］"),
    ] {
        let source = format!("前{outer}一{inner}二［＃割り注終わり］三［＃割り注終わり］後");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        assert_eq!(warichu_count(&aat["blocks"]), 2, "{aat}");
    }
}

#[test]
fn absent_or_crossed_opener_does_not_authorize_bare_closer() {
    for source in [
        "前［＃割り注終わり］後",
        "前［＃ここから割り注］中",
        "前［＃ここから割り注］中［＃ここから太字］字［＃割り注終わり］後",
    ] {
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        assert_eq!(warichu_count(&aat["blocks"]), 0, "{aat}");
        assert!(aat.to_string().contains("raw"), "{aat}");
    }
}

#[test]
fn nested_scope_aliases_retain_warichu_end_ownership() {
    let source =
        "前［＃ここから割り注］一［＃ここから太字］二［＃太字終わり］三［＃割り注終わり］後";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    assert_eq!(aat["meta"]["parse_complete"], true, "{aat}");
    assert_eq!(warichu_count(&aat["blocks"]), 1, "{aat}");
    let span = &aat["blocks"][0]["content"][1]["interpretation_marker_spans"][1];
    let start = usize::try_from(span["byte_start"].as_u64().unwrap()).unwrap();
    let end = usize::try_from(span["byte_end"].as_u64().unwrap()).unwrap();
    assert_eq!(&source[start..end], "［＃割り注終わり］");
}
