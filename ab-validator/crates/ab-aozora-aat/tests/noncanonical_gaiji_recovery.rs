//! Unresolved annotation boundaries must not consume surrounding source prose.

use ab_aozora_aat::aat_json_from_bytes;
use serde_json::Value;

#[test]
fn missing_annotation_hash_keeps_prose_outside_the_failed_marker() {
    let marker = "※［「纏」の「广」に代えて「厂」、54-14］";
    for body in [format!("前{marker}後。"), format!("「前{marker}後。」")] {
        let source = format!("題\n作者\n\n{body}\n\n底本：本\n");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let content = aat["blocks"][0]["content"].as_array().unwrap();
        let raw: Vec<_> = content
            .iter()
            .filter(|node| node["kind"] == "raw")
            .collect();
        assert_eq!(raw.len(), 1, "{aat}");
        assert_eq!(raw[0]["source"], marker, "{body}: {content:?}");
        let text: String = content
            .iter()
            .filter_map(|node| node["value"].as_str())
            .collect();
        assert_eq!(text, body.replace(marker, ""));
    }
}
