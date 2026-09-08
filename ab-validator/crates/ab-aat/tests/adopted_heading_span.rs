//! Adopted headings retain target and marker coordinates for enclosing layouts.
use ab_aat::aat_json_from_bytes;
use serde_json::Value;

fn find<'a>(value: &'a Value, kind: &str) -> Option<&'a Value> {
    match value {
        Value::Object(map) => {
            if value["kind"] == kind {
                return Some(value);
            }
            map.values().find_map(|child| find(child, kind))
        }
        Value::Array(items) => items.iter().find_map(|child| find(child, kind)),
        _ => None,
    }
}

#[test]
fn heading_adoption_preserves_exact_target_and_marker_extent() {
    for title in ["茶話", "茶話《ちゃばなし》"] {
        let target = "茶話";
        let heading = format!("{title}［＃「{target}」は同行中見出し］");
        let source = format!("題\n作者\n\n［＃１字下げ］{heading}［＃地付き］２・27\n\n底本：本\n");
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let adopted = find(&aat, "heading").unwrap();
        let start = usize::try_from(adopted["span"]["byte_start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(adopted["span"]["byte_end"].as_u64().unwrap()).unwrap();
        assert_eq!(&source[start..end], heading);
        let markers = adopted["interpretation_marker_spans"].as_array().unwrap();
        assert_eq!(markers.len(), 1);
        assert!(markers[0]["byte_start"].as_u64().unwrap() > u64::try_from(start).unwrap());
        let layout = find(&aat, "layout_block").unwrap();
        assert!(layout["span"]["byte_start"].is_u64());
        assert_eq!(layout["children"][0]["kind"], "paragraph");
        let aligned = &layout["children"][1];
        assert_eq!(aligned["align"], "right");
        assert_eq!(aligned["span"]["byte_start"], end);
        assert_eq!(aligned["children"][0]["content"][0]["value"], "２・27");
    }
}
