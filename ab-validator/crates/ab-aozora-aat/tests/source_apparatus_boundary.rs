//! Explicit body boundaries retain source apparatus outside the reading text.

use ab_aozora_aat::aat_json_from_bytes;
use serde_json::Value;

#[test]
fn explicit_body_end_keeps_translation_apparatus_and_source_spans() {
    for newline in ["\n", "\r\n"] {
        let source = format!(
            "作品{newline}作者{newline}{newline}本文{newline}{newline}［＃本文終わり］{newline}翻訳の底本：原書{newline}※利用条件{newline}翻訳者：訳者{newline}"
        );
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let blocks = aat["blocks"].as_array().unwrap();
        let notes: Vec<_> = blocks
            .iter()
            .filter(|node| node["kind"] == "source_note")
            .collect();
        let tail: String = notes
            .iter()
            .flat_map(|note| note["content"].as_array().unwrap())
            .map(|node| node["value"].as_str().unwrap())
            .collect();
        assert_eq!(
            tail,
            source[source.find("［＃本文終わり］").unwrap()..].replace("\r\n", "\n")
        );
        assert!(notes.iter().any(|note| {
            note["region_class"] == "terminal_provenance"
                && note["content"][0]["value"]
                    .as_str()
                    .unwrap()
                    .starts_with("翻訳の底本：")
        }));
        for note in notes {
            assert_eq!(note["placement"], "back");
            for node in note["content"].as_array().unwrap() {
                let start = usize::try_from(node["span"]["byte_start"].as_u64().unwrap()).unwrap();
                let end = usize::try_from(node["span"]["byte_end"].as_u64().unwrap()).unwrap();
                assert_eq!(node["value"], source[start..end].replace("\r\n", "\n"));
            }
        }
        let body: String = blocks
            .iter()
            .filter(|node| node["kind"] != "source_note")
            .flat_map(|node| node["content"].as_array().into_iter().flatten())
            .filter_map(|node| node["value"].as_str())
            .collect();
        assert_eq!(body, "本文");
        assert!(
            !aat["meta"]["warnings"]
                .as_array()
                .unwrap()
                .iter()
                .any(|warning| warning["code"] == "tail-line-unclassified")
        );
    }
}
