//! Native ruby readings preserve kunten rendering and source transcription.
use ab_aozora_facade::Document;

#[test]
fn ruby_reading_kunten_roundtrips_and_uses_annotation_layout() {
    for (source, expected) in [
        (
            "漢《か［＃レ］ん》",
            "<sub class=\"aozora-kaeriten\">レ</sub>",
        ),
        (
            "漢《か［＃（ノ）］ん》",
            "<sup class=\"aozora-okurigana\">ノ</sup>",
        ),
    ] {
        let document = Document::new(source);
        let tree = document.parse();
        assert_eq!(tree.source(), source);
        assert_eq!(tree.to_source(), format!("｜{source}"));
        assert!(tree.to_html().contains(expected), "{}", tree.to_html());
    }
}

#[test]
fn explicit_mixed_base_replays_source_when_no_reading_adopts_it() {
    for source in [
        "前｜漢［＃レ］字後",
        "前｜漢［＃レ］字《》後",
        "前｜漢［＃レ］字\n次《つぎ》",
        "前｜漢［＃レ］字《かんじ》後",
        "｜遊［＃二］松島［＃一］記《まつしまにあそぶき》",
        "｜a［＃レ］ b《よみ》",
        "｜漢［＃未知の注］字《よみ》",
        "前｜漢［＃レ］字［＃「字」に傍点］後",
    ] {
        let document = Document::new(source);
        let tree = document.parse();
        assert_eq!(tree.source(), source);
        let canonical = source.replace("\n次《つぎ》", "\n｜次《つぎ》");
        assert_eq!(tree.to_source(), canonical, "{}", tree.to_html());
    }
}
