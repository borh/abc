//! Supplied line geometry retains its source scope and independent axes.
use ab_aozora_aat::aat_json_from_bytes;
use serde_json::Value;

#[test]
fn native_width_and_compound_geometry_are_preserved() {
    for (source, indent, continuation, width, lines) in [
        (
            "［＃ここから２６字詰め］\n本文\n［＃ここで字詰め終わり］",
            None,
            None,
            Some(26),
            None,
        ),
        (
            "［＃ここから３字下げ、１行２０字組みで］\n本文\n［＃ここで字下げ、２０字組み終わり］",
            Some(3),
            None,
            Some(20),
            Some(1),
        ),
        (
            "［＃ここから６字下げ、折り返して７字下げ、２１字詰め］\n本文\n［＃ここで字下げ終わり］",
            Some(6),
            Some(7),
            Some(21),
            None,
        ),
    ] {
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let block = &aat["blocks"][0];
        assert_eq!(block["kind"], "layout_block", "{aat}");
        assert_eq!(block["indent"].as_u64(), indent);
        assert_eq!(block["continuation_indent"].as_u64(), continuation);
        assert_eq!(block["width"].as_u64(), width);
        assert_eq!(block["line_count"].as_u64(), lines);
        assert_eq!(
            block["interpretation_marker_spans"]
                .as_array()
                .unwrap()
                .len(),
            2
        );
    }
}

#[test]
fn next_indent_opener_is_a_replacement_not_a_closing_marker() {
    use ab_aozora_facade::{Document, syntax::ast::ContainerEnd};
    let source = "［＃ここから２字下げ］\n前\n［＃ここから改行天付き、折り返して１字下げ］\n後\n［＃ここで字下げ終わり］";
    let document = Document::new(source);
    let tree = document.parse();
    assert_eq!(tree.container_pairs().len(), 2);
    let ContainerEnd::IndentReplacement(replacement) = tree.container_pairs()[0].source_end else {
        panic!("replacement boundary expected")
    };
    assert_eq!(
        replacement.slice(source),
        "［＃ここから改行天付き、折り返して１字下げ］"
    );
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    assert_eq!(aat["blocks"].as_array().unwrap().len(), 2, "{aat}");
    assert_eq!(aat["blocks"][0]["indent"], 2);
    assert_eq!(aat["blocks"][0]["source_end"]["kind"], "next-indent-opener");
    assert_eq!(
        aat["blocks"][0]["interpretation_marker_spans"]
            .as_array()
            .unwrap()
            .len(),
        1
    );
    assert_eq!(aat["blocks"][1]["indent"], 0);
    assert_eq!(aat["blocks"][1]["continuation_indent"], 1);
}

#[test]
fn page_placement_ends_before_the_real_page_break() {
    for opener in ["［＃ページの左右中央］", "［＃ここからページの左右中央］"]
    {
        let source = format!(
            "{opener}\n［＃ここから３字下げ］\n扉\n［＃ここで字下げ終わり］\n［＃改丁］\n次頁"
        );
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let block = &aat["blocks"][0];
        assert_eq!(block["kind"], "layout_block", "{aat}");
        assert_eq!(block["page_placement"], "horizontal-center");
        assert!(block.get("align").is_none());
        assert!(block.get("indent").is_none());
        assert!(
            block["children"]
                .as_array()
                .unwrap()
                .iter()
                .any(|child| child["indent"] == 3)
        );
        let end = &block["source_end"];
        assert_eq!(end["kind"], "page-break");
        let start = usize::try_from(end["span"]["byte_start"].as_u64().unwrap()).unwrap();
        let finish = usize::try_from(end["span"]["byte_end"].as_u64().unwrap()).unwrap();
        assert_eq!(&source[start..finish], "［＃改丁］");
        assert_eq!(aat["blocks"][1]["content"][0]["kind"], "layout_break");
        assert_eq!(aat["blocks"][1]["content"][0]["break_kind"], "kaicho");
        assert!(aat["blocks"][2].to_string().contains("次頁"));
    }
}
