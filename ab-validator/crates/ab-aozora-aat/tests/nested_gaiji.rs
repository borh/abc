//! Unresolved outer glyphs keep their source extent and supplied ruby association.
use serde_json::Value;

#[test]
fn unresolved_glyph_targets_compare_source_identity_instead_of_placeholders() {
    fn count_emphasis(value: &Value) -> usize {
        match value {
            Value::Object(object) => {
                usize::from(value["kind"] == "style" && value["style_type"] == "bouten")
                    + object.values().map(count_emphasis).sum::<usize>()
            }
            Value::Array(children) => children.iter().map(count_emphasis).sum(),
            _ => 0,
        }
    }
    let first = "※［＃「女＋※［＃「臣」の「コ」に代えて「口」、第4水準2-85-54］」、49-15］";
    let different = "※［＃「姉」の正字、「女＋※［＃第3水準1-85-57］のつくり」、253-上-7］";
    for (principal, target, expected) in [(first, first, 1), (different, first, 0)] {
        let source =
            format!("題\n著者\n\n前{principal}［＃「{target}」に白丸傍点］後\n\n底本：本\n");
        let aat: Value =
            serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
                .unwrap();
        assert_eq!(count_emphasis(&aat["blocks"]), expected, "{source}");
    }
}

#[test]
fn nested_component_does_not_replace_the_outer_glyph_or_its_surroundings() {
    fn visit<'a>(value: &'a Value, glyphs: &mut Vec<&'a Value>, text: &mut String) {
        match value {
            Value::Object(object) => {
                if value["kind"] == "gaiji" {
                    glyphs.push(value);
                }
                if value["kind"] == "text" {
                    text.push_str(value["value"].as_str().unwrap());
                }
                for child in object.values() {
                    visit(child, glyphs, text);
                }
            }
            Value::Array(children) => {
                for child in children {
                    visit(child, glyphs, text);
                }
            }
            _ => {}
        }
    }
    let marker = "※［＃「姉」の正字、「女＋※［＃第3水準1-85-57］のつくり」、253-上-7］";
    let source = format!("題\n著者\n\n前「{marker}《ね》えさん」後\n\n底本：本\n");
    let aat: Value =
        serde_json::from_slice(&ab_aozora_aat::aat_json_from_bytes(source.as_bytes()).unwrap())
            .unwrap();
    let mut glyphs = Vec::new();
    let mut text = String::new();
    visit(&aat["blocks"], &mut glyphs, &mut text);
    assert_eq!(glyphs.len(), 1);
    let glyph = glyphs[0];
    assert!(glyph["resolved"].is_null());
    assert!(glyph["x-codepoint"].is_null());
    assert_eq!(glyph["jis_code"], "253-上-7");
    let start = usize::try_from(glyph["span"]["byte_start"].as_u64().unwrap()).unwrap();
    let end = usize::try_from(glyph["span"]["byte_end"].as_u64().unwrap()).unwrap();
    assert_eq!(&source[start..end], marker);
    assert!(text.contains("前「"));
    assert!(text.contains("えさん」後"));
}
