//! Baseline placement and mathematical roles do not supply a font size.

use ab_aozora_aat::aat_json_from_bytes;
use serde_json::Value;

fn find_kind<'a>(value: &'a Value, kind: &str) -> Option<&'a Value> {
    if value["kind"] == kind {
        return Some(value);
    }
    match value {
        Value::Object(fields) => fields.values().find_map(|child| find_kind(child, kind)),
        Value::Array(children) => children.iter().find_map(|child| find_kind(child, kind)),
        _ => None,
    }
}

#[test]
fn supplied_position_and_exponent_role_own_only_their_target() {
    for (source, kind, expected) in [
        (
            "“［＃「“」は下付き］MON”［＃「”」は下付き］",
            "baseline_position",
            "“",
        ),
        ("A2［＃「2」は指数］－B2［＃「2」は指数］", "exponent", "2"),
    ] {
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let node = find_kind(&aat, kind).unwrap_or_else(|| panic!("{aat}"));
        assert_eq!(node["content"][0]["value"], expected, "{aat}");
        assert!(node.get("size").is_none());
        assert!(node.get("level").is_none());
        assert!(node.get("base").is_none());
        assert!(!aat.to_string().contains("interpretation_problem"), "{aat}");
    }
}

#[test]
fn canonical_source_preserves_role_and_exact_annotation_target() {
    for (source, class_name) in [
        ("“［＃「“」は下付き］", "aozora-lowered"),
        ("A2［＃「2」は指数］", "aozora-exponent"),
    ] {
        let document = ab_aozora_facade::Document::new(source);
        let tree = document.parse();
        let canonical = tree.to_source();
        let reparsed = ab_aozora_facade::Document::new(canonical.as_str());
        assert_eq!(reparsed.parse().to_html(), tree.to_html());
        assert!(tree.to_html().contains(class_name));
        assert!(!tree.to_html().contains("<small"));
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let kind = if class_name == "aozora-lowered" {
            "baseline_position"
        } else {
            "exponent"
        };
        let node = find_kind(&aat, kind).unwrap();
        let target = &node["content"][0];
        let start = usize::try_from(target["span"]["byte_start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(target["span"]["byte_end"].as_u64().unwrap()).unwrap();
        assert_eq!(&source[start..end], target["value"].as_str().unwrap());
        let marker = &node["interpretation_marker_spans"][0];
        let start = usize::try_from(marker["byte_start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(marker["byte_end"].as_u64().unwrap()).unwrap();
        assert_eq!(&source[start..end], &source[source.find('［').unwrap()..]);
    }
}

#[test]
fn absent_targets_remain_unresolved() {
    for source in [
        "［＃「2」は指数］",
        "別字［＃「2」は指数］",
        "別字［＃「“」は下付き］",
    ] {
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        assert!(find_kind(&aat, "exponent").is_none(), "{aat}");
        assert!(find_kind(&aat, "baseline_position").is_none(), "{aat}");
        assert!(aat.to_string().contains("interpretation_problem"), "{aat}");
    }
}
