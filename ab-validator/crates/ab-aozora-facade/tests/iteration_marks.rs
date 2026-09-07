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
