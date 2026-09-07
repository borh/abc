//! Supplied iteration notation renders as its mark and roundtrips as source.
use ab_aozora_facade::Document;

#[test]
fn iteration_marks_preserve_spelling_and_visible_character() {
    for (source, visible) in [
        ("フゴ／＼していた。", "フゴ〱していた。"),
        ("とき／″＼", "とき〲"),
        ("「とき／″＼」", "「とき〲」"),
        ("時々《とき／″＼》", "とき〲"),
        ("｜とき／″＼《時々》", "とき〲"),
    ] {
        let document = Document::new(source);
        let tree = document.parse();
        assert!(tree.to_html().contains(visible), "{}", tree.to_html());
        assert_eq!(tree.to_source(), source);
    }
}

#[test]
fn malformed_notation_and_encoded_halves_are_literal() {
    for source in ["／", "＼", "／″", "／゛＼", "〳〵", "〴〵", "〱〲"] {
        let document = Document::new(source);
        let tree = document.parse();
        assert!(tree.to_html().contains(source));
        assert_eq!(tree.to_source(), source);
    }
}
