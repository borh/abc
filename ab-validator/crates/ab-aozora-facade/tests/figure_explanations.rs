//! Figure explanations retain source scope and placement without figure identity.

use ab_aozora_facade::Document;

#[test]
fn figure_explanation_scope_preserves_paragraphs_and_both_boundaries() {
    let open = "［＃ここから図表下部解説文］";
    let close = "［＃ここで図表下部解説文終わり］";
    let source = format!("前\n{open}\n説明一。\n説明二。\n{close}\n後");
    let document = Document::new(source.clone());
    let tree = document.parse();
    assert_eq!(tree.source(), source);
    assert_eq!(tree.container_pairs().len(), 1);
    let pair = &tree.container_pairs()[0];
    assert_eq!(
        &source[pair.source_open.start as usize..pair.source_open.end as usize],
        open
    );
    assert!(tree.diagnostics().is_empty());
    let serialized = tree.to_source();
    assert_eq!(serialized.matches(open).count(), 1);
    assert_eq!(serialized.matches(close).count(), 1);
    let roundtrip = Document::new(serialized);
    assert_eq!(roundtrip.parse().to_html(), tree.to_html());
    assert!(
        tree.to_html()
            .contains("data-purpose=\"figure-explanation\"")
    );
    assert!(tree.to_html().contains("data-placement=\"below\""));
}

#[test]
fn ordinary_caption_closers_do_not_establish_explanation_boundaries() {
    for source in [
        "［＃ここから図表下部解説文］\n説明\n［＃ここでキャプション終わり］",
        "［＃ここからキャプション］\n説明\n［＃ここで図表下部解説文終わり］",
        "［＃ここから図表下部解説文］\n説明",
    ] {
        let document = Document::new(source);
        let tree = document.parse();
        assert_eq!(tree.source(), source);
        assert!(tree.container_pairs().is_empty());
        assert!(!tree.diagnostics().is_empty());
    }
}
