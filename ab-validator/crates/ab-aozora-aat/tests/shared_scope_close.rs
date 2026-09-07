//! A single source closure can establish the ends of adjacent nested regions.

use ab_aozora_aat::aat_json_from_bytes;
use serde_json::Value;

#[test]
fn nested_columns_and_frame_share_the_actual_close_marker() {
    for suffix in ["", "、段間に罫"] {
        let close = "［＃ここで２段組み、罫囲み終わり］";
        let source = format!("［＃ここから罫囲み］\n［＃ここから２段組み{suffix}］\n本文\n{close}");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let frame = &aat["blocks"][0];
        assert_eq!(frame["kind"], "typography_block", "{aat}");
        let columns = frame["children"]
            .as_array()
            .unwrap()
            .iter()
            .find(|child| child["kind"] == "layout_block")
            .unwrap();
        assert_eq!(columns["kind"], "layout_block", "{aat}");
        assert_eq!(columns["column_count"], 2);
        assert_eq!(frame["span"]["byte_end"], source.len());
        assert_eq!(columns["span"]["byte_end"], source.len());
        if suffix.is_empty() {
            assert_eq!(
                frame["interpretation_marker_spans"][1],
                columns["interpretation_marker_spans"][1]
            );
            assert!(!aat["blocks"].to_string().contains("interpretation_problem"));
        } else {
            assert!(columns.to_string().contains("段間に罫"));
            assert_eq!(
                columns["interpretation_marker_spans"]
                    .as_array()
                    .unwrap()
                    .len(),
                1
            );
            assert_eq!(
                columns["interpretation_marker_spans"][0],
                frame["interpretation_marker_spans"][1]
            );
        }
    }
}

#[test]
fn unmatched_shared_boundary_retains_one_uncertain_source_occurrence() {
    let close = "［＃ここで２段組み、罫囲み終わり］";
    let source = format!("［＃ここから罫囲み］\n［＃ここから３段組］\n本文\n{close}");
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let mut pending = vec![&aat["blocks"]];
    let mut markers = Vec::new();
    while let Some(node) = pending.pop() {
        if node["kind"] == "raw" && node["source"] == close {
            markers.push(node);
        }
        match node {
            Value::Array(values) => pending.extend(values),
            Value::Object(values) => pending.extend(values.values()),
            _ => {}
        }
    }
    assert_eq!(markers.len(), 1);
    assert!(markers[0].get("interpretation_problem").is_some());
    let close_start = source.len() - close.len();
    assert!(
        aat["meta"]["interpretation_facts"]
            .as_array()
            .into_iter()
            .flatten()
            .all(|fact| fact["source_span"]["start"] != close_start)
    );
}
