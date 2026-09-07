//! Scoped TCY source spelling and inline layout through the public native parser.

use ab_aozora_facade::Document;

#[test]
fn scoped_tcy_round_trips_plain_and_rich_children() {
    for body in ["29", "（※［＃ローマ数字1、1-13-21］）", "漢字《かんじ》"] {
        let source = format!("前［＃縦中横］{body}［＃縦中横終わり］後");
        let document = Document::new(source.clone());
        let tree = document.parse();
        assert_eq!(tree.source(), source);
        let child = Document::new(body).parse().to_source();
        assert_eq!(
            tree.to_source(),
            format!("前［＃縦中横］{child}［＃縦中横終わり］後")
        );
        let html = tree.to_html();
        assert!(
            html.contains("<span class=\"aozora-combine-upright\">"),
            "{html}"
        );
        assert!(!html.contains("aozora-container"), "{html}");
    }
}
