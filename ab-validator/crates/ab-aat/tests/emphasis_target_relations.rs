//! Source selectors decorate only their established target ranges.
use serde_json::Value;

fn visible(value: &Value) -> String {
    if value["kind"] == "text" {
        value["value"].as_str().unwrap().to_owned()
    } else {
        value["content"]
            .as_array()
            .map(|children| children.iter().map(visible).collect())
            .unwrap_or_default()
    }
}

fn styles<'a>(value: &'a Value, selected: &mut Vec<&'a Value>) {
    match value {
        Value::Object(object) => {
            if value["kind"] == "style" && value["style_type"] == "bouten" {
                selected.push(value);
            }
            for child in object.values() {
                styles(child, selected);
            }
        }
        Value::Array(children) => {
            for child in children {
                styles(child, selected);
            }
        }
        _ => {}
    }
}

#[test]
fn source_relations_keep_unselected_text_and_exact_marker_facts() {
    for (body, expected, plain) in [
        (
            "地面の価格やのれんのねうち［＃「のれん」と「ねうち」に傍点］後",
            vec!["のれん", "ねうち"],
            "地面の価格やのれんのねうち後",
        ),
        (
            "法文・法律［＃「法律」の「法」に傍点］後",
            vec!["法"],
            "法文・法律後",
        ),
        (
            "生、息、行、意気［＃「生」「息」「行」「意気」に傍点］後",
            vec!["生", "息", "行", "意気"],
            "生、息、行、意気後",
        ),
        // The operand names what to leave out, and it leaves out every
        // occurrence of itself rather than the first alone.
        (
            "行・実践［＃「行・実践」の「・」を除く部分に傍点］後",
            vec!["行", "実践"],
            "行・実践後",
        ),
    ] {
        let source = format!("題\n著者\n\n{body}\n\n底本：本\n");
        let aat: Value =
            serde_json::from_slice(&ab_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
                .unwrap();
        let mut selected = Vec::new();
        styles(&aat["blocks"], &mut selected);
        assert_eq!(
            selected
                .iter()
                .map(|node| visible(node))
                .collect::<Vec<_>>(),
            expected,
            "{aat:#}"
        );
        let paragraphs = aat["blocks"].as_array().unwrap();
        assert!(
            paragraphs.iter().any(|p| visible(p).trim() == plain),
            "{aat:#}"
        );
        let marker_start = source.find("［＃").unwrap();
        let marker_end =
            source[marker_start..].find('］').unwrap() + marker_start + '］'.len_utf8();
        for node in selected {
            let markers = node["interpretation_marker_spans"].as_array().unwrap();
            assert_eq!(markers.len(), 1);
            assert_eq!(markers[0]["byte_start"], marker_start);
            assert_eq!(markers[0]["byte_end"], marker_end);
        }
    }
}
