//! Edition witness delimiter glyphs stay inside their source annotation.
use ab_aozora_aat::aat_json_from_bytes;
use serde_json::Value;

#[test]
fn quoted_closing_bracket_does_not_leak_annotation_tail_into_prose() {
    let marker = "［＃「〕」は底本では「］」］";
    let source = format!("前〕{marker}後");
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    assert_eq!(aat["blocks"][0]["content"][2]["value"], "後", "{aat}");
    let variant = &aat["blocks"][0]["content"][1];
    assert_eq!(variant["kind"], "text-variant", "{aat}");
    assert_eq!(variant["source"], marker);
    assert_eq!(variant["base_text"], "］");
    assert_eq!(variant["content"][0]["value"], "〕");
    assert_eq!(
        variant["span"]["byte_end"],
        source.len() - "後".len(),
        "{aat}"
    );
}
