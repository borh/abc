//! Formatting quotations name source targets; they do not insert principal text.
use ab_aozora_facade::Document;

#[test]
fn absent_formatting_targets_never_become_visible_text() {
    for marker in [
        "［＃「不在」は太字］",
        "［＃「不在」に傍点］",
        "［＃「不在」は「□」囲み］",
        "［＃「不在」は大見出し］",
    ] {
        let source = format!("前{marker}後");
        let document = Document::new(source.clone());
        let tree = document.parse();
        let html = tree.to_html();
        assert!(!html.contains(">不在<"), "{source}: {html}");
        assert_eq!(tree.to_source(), source);
    }
}

#[test]
fn styled_ruby_base_keeps_its_source_text_and_decoration() {
    let document = Document::new("漢字《かんじ》［＃「漢字」は太字］");
    let tree = document.parse();
    let html = tree.to_html();
    assert!(html.contains("<ruby>"), "{html}");
    assert!(
        html.contains(r#"<b class="aozora-futoji">漢字</b>"#),
        "{html}"
    );
    let restored = Document::new(tree.to_source());
    assert_eq!(html, restored.parse().to_html());
}

#[test]
fn absent_reference_inside_explicit_ruby_does_not_enlarge_its_base() {
    let source = "｜甲［＃「不在」は太字］《よみ》";
    let document = Document::new(source);
    let tree = document.parse();
    let html = tree.to_html();
    assert!(html.contains("<ruby>甲<rp>"), "{html}");
    assert!(!html.contains("不在"), "{html}");
    assert_eq!(tree.to_source(), source);
    assert_eq!(html, Document::new(tree.to_source()).parse().to_html());
}
