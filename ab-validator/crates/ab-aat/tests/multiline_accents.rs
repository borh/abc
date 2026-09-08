//! Source-paired accent stanzas preserve physical lines and later annotation extents.

use ab_aat::aat_json_from_bytes;
use encoding_rs::SHIFT_JIS;
use serde_json::Value;

#[test]
fn encoded_stanza_preserves_paragraphs_and_following_source_marker() {
    let marker = "［＃未対応の注記］";
    let source = format!(
        "題\r\n作者\r\n\r\n〔Pardonnez a` mon bavardage\r\nJ'en suis a` mon premier voyage.〕\r\n{marker}\r\n\r\n底本：本\r\n"
    );
    let encoded = SHIFT_JIS.encode(&source).0.into_owned();
    for bytes in [source.as_bytes(), encoded.as_slice()] {
        let aat: Value = serde_json::from_slice(&aat_json_from_bytes(bytes).unwrap()).unwrap();
        assert_eq!(
            aat["blocks"][0]["content"][0]["value"],
            "Pardonnez à mon bavardage\n"
        );
        assert_eq!(
            aat["blocks"][1]["content"][0]["value"],
            "J'en suis à mon premier voyage.\n"
        );
        let retained = &aat["blocks"][2]["content"][0];
        assert_eq!(retained["source"], marker);
        let start = usize::try_from(retained["span"]["byte_start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(retained["span"]["byte_end"].as_u64().unwrap()).unwrap();
        assert_eq!(&source[start..end], marker);
    }
}
