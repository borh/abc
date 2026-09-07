//! Supplied translation boundaries own content independently of an omitted image.
use ab_aozora_facade::Document;

#[test]
fn banknote_translation_retains_both_supplied_boundaries() {
    let open = "［＃ここから紙幣の文字の訳文］";
    let close = "［＃ここで訳文終わり］";
    let source = format!("前\n{open}\n　　国王の名において\n十リーヴル兌換券\n{close}\n後");
    let document = Document::new(source.as_str());
    let tree = document.parse();
    assert_eq!(tree.source(), source);
    assert_eq!(tree.container_pairs().len(), 1);
    assert!(tree.diagnostics().is_empty());
    let canonical = tree.to_source();
    assert_eq!(canonical.matches(open).count(), 1);
    assert_eq!(canonical.matches(close).count(), 1);
    assert_eq!(
        Document::new(canonical.as_str()).parse().to_html(),
        tree.to_html()
    );
    assert!(
        tree.to_html()
            .contains("data-source-kind=\"banknote-text\"")
    );
}

#[test]
fn missing_or_different_translation_openers_remain_unresolved() {
    for source in [
        "［＃ここから紙幣の文字の訳文］\n訳文\n［＃ここでキャプション終わり］",
        "［＃ここから別の訳文］\n訳文\n［＃ここで訳文終わり］",
        "［＃ここから紙幣の文字の訳文］\n訳文",
    ] {
        let document = Document::new(source);
        let tree = document.parse();
        assert_eq!(tree.source(), source);
        assert!(tree.container_pairs().is_empty());
        assert!(!tree.diagnostics().is_empty());
    }
}
