//! A stray opening bracket in an annotation body does not consume the text after it.

use ab_aat::aat_json_from_bytes;
use serde_json::Value;

/// Collect every `raw` node's source alongside the reading text, in order.
fn walk(node: &Value, raws: &mut Vec<String>, text: &mut String) {
    match node {
        Value::Array(items) => {
            for item in items {
                walk(item, raws, text);
            }
        }
        Value::Object(fields) => {
            match fields.get("kind").and_then(Value::as_str) {
                Some("raw") => {
                    raws.push(fields["source"].as_str().unwrap().to_owned());
                    return;
                }
                Some("text") => {
                    text.push_str(fields["value"].as_str().unwrap());
                    return;
                }
                _ => {}
            }
            for value in fields.values() {
                walk(value, raws, text);
            }
        }
        _ => {}
    }
}

/// Collect every `raw` node whole.
fn raw_nodes<'a>(node: &'a Value, found: &mut Vec<&'a Value>) {
    match node {
        Value::Array(items) => items.iter().for_each(|item| raw_nodes(item, found)),
        Value::Object(fields) => {
            if fields.get("kind").and_then(Value::as_str) == Some("raw") {
                found.push(node);
            }
            fields.values().for_each(|value| raw_nodes(value, found));
        }
        _ => {}
    }
}

/// Work 000662 writes `［愛」` inside an annotation body where it meant `「愛」`.
/// The stray opener has no partner of its own, so reading it as structure hands
/// the annotation's `］` to it and the annotation runs to the end of the line,
/// taking the rest of the poem with it. Those words are reading text, and they
/// have to survive a defect in the apparatus that annotates them.
#[test]
fn a_stray_opening_bracket_keeps_the_reading_text_that_follows_it() {
    let source = "　最初の人生を野原に寝て味はつた可愛［＃［愛」に「ママ」の注記］さうなサクラ子\n";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();

    let mut raws = Vec::new();
    let mut text = String::new();
    walk(&aat["blocks"], &mut raws, &mut text);

    assert_eq!(raws, ["［＃［愛」に「ママ」の注記］"], "{aat}");
    assert_eq!(text, "　最初の人生を野原に寝て味はつた可愛さうなサクラ子\n");
}

/// The marker itself stays uninterpreted, because the source is malformed there
/// and any reading of it would be invented. What changes is that the parser now
/// reports an unrecognised directive rather than a stretch of source it could
/// not account for: it knows where the annotation begins and ends, and only its
/// body is beyond reading.
#[test]
fn the_malformed_marker_is_reported_as_a_directive_the_parser_cannot_read() {
    let source = "　最初の人生を野原に寝て味はつた可愛［＃［愛」に「ママ」の注記］さうなサクラ子\n";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();

    let mut found = Vec::new();
    raw_nodes(&aat["blocks"], &mut found);

    assert_eq!(found.len(), 1, "{aat}");
    let marker = &found[0];
    assert_eq!(marker["x-source-marker-kind"], "directive");
    assert_eq!(marker["interpretation_problem"]["kind"], "unknown-notation");
}
