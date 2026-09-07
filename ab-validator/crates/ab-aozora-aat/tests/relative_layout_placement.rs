//! Relative placement retains a source-owned anchor without inventing page coordinates.

use ab_aozora_aat::aat_json_from_bytes;
use serde_json::Value;

fn placements<'a>(node: &'a Value, result: &mut Vec<&'a Value>) {
    if node["kind"] == "layout_block" && node.get("relative_placement").is_some() {
        result.push(node);
    }
    match node {
        Value::Object(fields) => fields.values().for_each(|child| placements(child, result)),
        Value::Array(children) => children.iter().for_each(|child| placements(child, result)),
        _ => {}
    }
}

#[test]
fn relative_text_anchor_preserves_its_offset_without_absolute_indent() {
    let source = "　複\n\n［＃「複」の文字の下から２字下げ、横組み右揃えで］\n1500\n7×2\n［＃ここで横組み終わり］\n外側";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let mut blocks = Vec::new();
    placements(&aat, &mut blocks);
    assert_eq!(blocks.len(), 1, "{aat}");
    let block = blocks[0];
    assert_eq!(block["direction"], "horizontal");
    assert_eq!(block["align"], "right");
    assert!(block.get("indent").is_none());
    assert_eq!(block["relative_placement"]["offset_chars"], 2);
    assert_eq!(block["relative_placement"]["anchor_span"]["start"], 3);
    assert_eq!(block["relative_placement"]["anchor_span"]["end"], 6);
    assert!(!block.to_string().contains("外側"));
    assert!(!aat.to_string().contains("interpretation_problem"));
}

#[test]
fn missing_or_multiple_literal_anchors_remain_unresolved() {
    for prefix in ["別", "複と複"] {
        let source = format!(
            "{prefix}\n［＃「複」の文字の下から２字下げ、横組み右揃えで］\n1500\n［＃ここで横組み終わり］"
        );
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let mut blocks = Vec::new();
        placements(&aat, &mut blocks);
        assert!(blocks.is_empty(), "{aat}");
        assert!(aat.to_string().contains("interpretation_problem"));
    }
}

#[test]
fn vertical_label_replaces_horizontal_presentation_within_remaining_indent() {
    let source = "［＃ここから２字下げ、横組み右揃えで］\n2000K\n500km\n［＃横組みの下に、左右中央縦組みで］\n逆カモメ型Ｗ\n［＃ここで字下げ、横組み終わり］\n外側";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let mut blocks = Vec::new();
    placements(&aat, &mut blocks);
    assert_eq!(blocks.len(), 1, "{aat}");
    let block = blocks[0];
    assert_eq!(block["direction"], "vertical");
    assert_eq!(block["align"], "center");
    assert_eq!(
        block["relative_placement"]["anchor_kind"],
        "horizontal-block"
    );
    let anchor = &block["relative_placement"]["anchor_span"];
    assert_eq!(
        &source[usize::try_from(anchor["start"].as_u64().unwrap()).unwrap()
            ..usize::try_from(anchor["end"].as_u64().unwrap()).unwrap()],
        "2000K\n500km"
    );
    assert!(!block.to_string().contains("外側"));
    assert!(!aat.to_string().contains("interpretation_problem"), "{aat}");
}

#[test]
fn relative_anchor_uses_original_decoded_bom_and_crlf_coordinates() {
    let source = "\u{feff}複\r\n\r\n［＃「複」の文字の下から２字下げ、横組み右揃えで］\r\n1500\r\n［＃ここで横組み終わり］";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let mut blocks = Vec::new();
    placements(&aat, &mut blocks);
    let span = &blocks[0]["relative_placement"]["anchor_span"];
    let decoded = ab_aozora_aat::decode_source_bytes(source.as_bytes()).unwrap();
    assert_eq!(
        &decoded.text[usize::try_from(span["start"].as_u64().unwrap()).unwrap()
            ..usize::try_from(span["end"].as_u64().unwrap()).unwrap()],
        "複"
    );
    assert_eq!(
        blocks[0]["span"]["byte_start"],
        decoded.text.find("［＃").unwrap()
    );
}

#[test]
fn relative_placement_serialization_preserves_source_semantics_and_one_marker() {
    for source in [
        "複\n［＃「複」の文字の下から２字下げ、横組み右揃えで］\n1500\n［＃ここで横組み終わり］",
        "［＃ここから２字下げ、横組み右揃えで］\n2000K\n［＃横組みの下に、左右中央縦組みで］\n逆カモメ型Ｗ\n［＃ここで字下げ、横組み終わり］",
    ] {
        let document = ab_aozora_facade::Document::new(source);
        let tree = document.parse();
        let serialized = tree.to_source();
        let reparsed = ab_aozora_facade::Document::new(serialized.as_str());
        assert_eq!(tree.to_html(), reparsed.parse().to_html());
        assert_eq!(serialized, reparsed.parse().to_source());
        assert_eq!(
            serialized.matches("［＃").count(),
            source.matches("［＃").count()
        );
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(serialized.as_bytes()).unwrap()).unwrap();
        let mut blocks = Vec::new();
        placements(&aat, &mut blocks);
        assert_eq!(blocks.len(), 1);
        assert!(!aat.to_string().contains("interpretation_problem"));
    }
}

#[test]
fn a_completed_horizontal_block_is_not_guessed_as_an_active_anchor() {
    let source = "［＃ここから横組み］\nABC\n［＃ここで横組み終わり］\n［＃横組みの下に、左右中央縦組みで］\n説明\n［＃ここで横組み終わり］";
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let mut blocks = Vec::new();
    placements(&aat, &mut blocks);
    assert!(blocks.is_empty());
    assert!(aat.to_string().contains("interpretation_problem"));
}
