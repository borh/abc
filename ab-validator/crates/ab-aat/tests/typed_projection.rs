//! AAT interpretation follows typed parser attributes, independently of target spelling.

use ab_aat::aat_json_from_bytes;
use serde_json::Value;

#[test]
fn emphasis_meaning_does_not_depend_on_words_in_its_target() {
    let aat: Value = serde_json::from_slice(
        &aat_json_from_bytes("太字［＃「太字」は斜体］".as_bytes()).unwrap(),
    )
    .unwrap();
    assert_eq!(aat["blocks"][0]["content"][0]["style_type"], "italic");
    assert_eq!(
        aat["blocks"][0]["content"][0]["content"][0]["value"],
        "太字"
    );
}
