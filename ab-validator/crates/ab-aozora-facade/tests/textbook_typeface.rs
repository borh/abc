//! Textbook typeface scope preserves its supplied boundaries and rich contents.
use ab_aozora_facade::Document;

#[test]
fn textbook_scope_closes_before_the_rest_of_the_same_source_line() {
    let open = "［＃ここから教科書体］";
    let close = "［＃ここで教科書体終わり］";
    let source = format!("前\n{open}\n本文\n人［＃「人」は太字］　話{close}後");
    let document = Document::new(source.clone());
    let tree = document.parse();
    assert_eq!(tree.container_pairs().len(), 1);
    let pair = &tree.container_pairs()[0];
    assert_eq!(
        &source[pair.source_open.start as usize..pair.source_open.end as usize],
        open
    );
    assert_eq!(pair.kind.as_json_tag(), "textbook");
    let canonical = tree.to_source();
    assert_eq!(canonical.matches(open).count(), 1);
    assert_eq!(canonical.matches(close).count(), 1);
    assert!(tree.to_html().contains("data-typeface=\"教科書体\""));
    let reparse = Document::new(canonical.as_str());
    assert_eq!(tree.to_html(), reparse.parse().to_html());
}

#[test]
fn textbook_scope_does_not_accept_an_unrelated_or_missing_closer() {
    for source in [
        "［＃ここから教科書体］\n本文",
        "［＃ここから教科書体］\n本文［＃ここで斜体終わり］",
    ] {
        let document = Document::new(source);
        let tree = document.parse();
        assert!(tree.container_pairs().is_empty());
        assert!(!tree.diagnostics().is_empty());
        assert_eq!(tree.source(), source);
    }
}
