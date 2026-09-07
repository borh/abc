//! Source-region ownership comes from native matched marker extents.

use ab_aozora_aat::aat_json_from_bytes;
use ab_aozora_facade::Document;
use serde_json::Value;

#[test]
fn native_pairs_retain_both_original_marker_spans() {
    let source = "前［＃中見出し］漢字《かんじ》［＃中見出し終わり］後";
    let document = Document::new(source);
    let tree = document.parse();
    let pair = &tree.container_pairs()[0];
    assert_eq!(pair.source_open.slice(source), "［＃中見出し］");
    assert_eq!(pair.source_close.slice(source), "［＃中見出し終わり］");
}

#[test]
fn native_scope_matching_checks_explicit_heading_and_font_attributes() {
    for source in [
        "［＃大見出し］題［＃中見出し終わり］",
        "［＃同行中見出し］題［＃中見出し終わり］",
        "［＃ここから１段階大きな文字］本文［＃ここで小さな文字終わり］",
    ] {
        let document = Document::new(source);
        let tree = document.parse();
        assert!(tree.container_pairs().is_empty(), "{source}");
        assert!(
            tree.diagnostics()
                .iter()
                .any(|d| d.code().ends_with("::mismatched_container_close")),
            "{source}"
        );
        assert_eq!(tree.source(), source);
    }
    let document = Document::new("［＃ここから大見出し］題［＃ここで見出し終わり］");
    assert_eq!(document.parse().container_pairs().len(), 1);
}

#[test]
fn aat_native_close_metadata_uses_original_decoded_coordinates() {
    let source = "\u{feff}題\r\n作者\r\n\r\n〔ae&〕［＃ここから１段階小さな文字］\r\n本文\r\n［＃ここで小さな文字終わり］\r\n\r\n底本：本\r\n";
    let decoded = source.strip_prefix('\u{feff}').unwrap();
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let mut pending = vec![&aat];
    let mut closes = Vec::new();
    while let Some(node) = pending.pop() {
        if let Some(span) = node.get("x-native-close-span") {
            let start = usize::try_from(span["byte_start"].as_u64().unwrap()).unwrap();
            let end = usize::try_from(span["byte_end"].as_u64().unwrap()).unwrap();
            closes.push(&decoded[start..end]);
        }
        match node {
            Value::Object(map) => pending.extend(map.values()),
            Value::Array(array) => pending.extend(array),
            _ => {}
        }
    }
    assert_eq!(closes, ["［＃ここで小さな文字終わり］"]);
}
