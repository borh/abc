//! Accent notation is decoded without removing literal bracket text.

use ab_aat::aat_json_from_bytes;
use proptest::prelude::*;
use serde_json::Value;

fn source_notes(value: &Value) -> Vec<&Value> {
    let mut pending = vec![value];
    let mut raw = Vec::new();
    while let Some(value) = pending.pop() {
        match value {
            Value::Object(fields) => {
                if value.get("source").is_some() {
                    raw.push(value);
                }
                pending.extend(fields.values());
            }
            Value::Array(values) => pending.extend(values),
            _ => {}
        }
    }
    raw
}

#[test]
fn nested_accent_scopes_do_not_shift_later_source_notes() {
    let note = "［＃「平和」は底本では「価格」、正誤表による訂正］";
    for source in [
        format!("〔a'〔e'〕o'〕\n平和{note}"),
        format!("≪〔a'〔e'〕o'〕平和{note}≫"),
    ] {
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let raw = source_notes(&aat);
        assert!(raw.iter().any(|node| node["source"] == note), "{aat}");
        for node in raw {
            let start = usize::try_from(node["span"]["byte_start"].as_u64().unwrap()).unwrap();
            let end = usize::try_from(node["span"]["byte_end"].as_u64().unwrap()).unwrap();
            assert_eq!(node["source"], &source[start..end], "{node}");
        }
    }
}

#[test]
fn resolved_variant_spelling_preserves_original_accent_notation() {
    let source = "〔sorgfa:ltig［＃「sorgfa:ltig」は底本では「sorgfa:tig」］〕";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let raw = source_notes(&aat);
    let note = raw
        .iter()
        .find(|node| node["kind"] == "text-variant")
        .unwrap();
    assert_eq!(note["content"][0]["value"], "sorgfältig");
    assert_eq!(note["base_text"], "sorgfätig");
    assert_eq!(
        note["source"],
        "［＃「sorgfa:ltig」は底本では「sorgfa:tig」］"
    );
    let start = usize::try_from(note["span"]["byte_start"].as_u64().unwrap()).unwrap();
    let end = usize::try_from(note["span"]["byte_end"].as_u64().unwrap()).unwrap();
    assert_eq!(note["source"], &source[start..end]);
}

#[test]
fn unparsed_source_gaps_do_not_own_the_visible_accent_prefix() {
    let source = "前〔cafe'〕［＃tail\r\n";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let raw = source_notes(&aat);
    assert_eq!(raw.len(), 1);
    assert_eq!(raw[0]["source"], "［＃tail");
    let start = usize::try_from(raw[0]["span"]["byte_start"].as_u64().unwrap()).unwrap();
    let end = usize::try_from(raw[0]["span"]["byte_end"].as_u64().unwrap()).unwrap();
    assert_eq!(&source[start..end], "［＃tail");
}

proptest! {
    #[test]
    fn retained_raw_notes_match_their_decoded_source_slices(
        prefix in "[a-zあ-ん]{0,30}",
        accents in prop::collection::vec(prop::sample::select(vec!["〔a'〕", "〔a'〔e'〕o'〕", "〔本全集〕"]), 0..5),
        quote in any::<bool>(),
        crlf in any::<bool>(),
    ) {
        let note = "［＃「平和」は底本では「価格」、正誤表による訂正］";
        let body = format!("{prefix}{}{}平和{note}", accents.join(""), if crlf { "\r\n" } else { "\n" });
        let source = if quote { format!("≪{body}≫") } else { body };
        let aat: Value = serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let raw = source_notes(&aat);
        prop_assert!(raw.iter().any(|node| node["source"] == note));
        for node in raw {
            let start = usize::try_from(node["span"]["byte_start"].as_u64().unwrap()).unwrap();
            let end = usize::try_from(node["span"]["byte_end"].as_u64().unwrap()).unwrap();
            prop_assert_eq!(node["source"].as_str(), source.get(start..end));
        }
    }
}

#[test]
fn accent_scope_delimiters_are_syntax_but_literal_brackets_are_text() {
    for (source, expected) in [
        ("〔a` la Huysmans〕", "à la Huysmans"),
        ("〔本全集〕", "〔本全集〕"),
        ("前〔a`〕中〔本全集〕後", "前à中〔本全集〕後"),
    ] {
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let actual: String = aat["blocks"][0]["content"]
            .as_array()
            .unwrap()
            .iter()
            .map(|node| node["value"].as_str().expect("visible text node"))
            .collect();
        assert_eq!(actual, expected, "{source}");
    }
}
