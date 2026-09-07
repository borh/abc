//! Kunten categories determine layout while source serialization retains notation.
use ab_aozora_facade::Document;

#[test]
fn kunten_category_controls_rendering_and_roundtrips() {
    for (source, html) in [
        ("漢［＃レ］", "<sub class=\"aozora-kaeriten\">レ</sub>"),
        ("漢［＃（レ）］", "<sup class=\"aozora-okurigana\">レ</sup>"),
        ("給［＃（弖）］", "<sup class=\"aozora-okurigana\">弖</sup>"),
        ("所［＃一レ］", "<sub class=\"aozora-kaeriten\">一レ</sub>"),
    ] {
        let document = Document::new(source);
        let tree = document.parse();
        assert!(tree.to_html().contains(html), "{}", tree.to_html());
        assert_eq!(tree.to_source(), source);
    }
}
