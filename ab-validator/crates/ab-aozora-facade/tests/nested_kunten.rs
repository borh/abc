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
        assert_eq!(tree.to_source(), source);
        assert!(tree.to_html().contains(expected), "{}", tree.to_html());
    }
}
