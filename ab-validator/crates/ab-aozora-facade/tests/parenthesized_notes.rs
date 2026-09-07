//! Parenthesized note payloads survive canonical source serialization.
use ab_aozora_facade::Document;

#[test]
fn canonical_quoted_note_keeps_the_original_parentheses() {
    let source = "病殺［＃「殺」に（死）の注記］とするも可。";
    let document = Document::new(source);
    let parsed = document.parse();
    let canonical = parsed.to_source();
    assert_eq!(canonical, "病殺［＃「殺」に「（死）」の注記］とするも可。");
    let reparsed_document = Document::new(canonical.as_str());
    let reparsed = reparsed_document.parse();
    assert_eq!(parsed.to_html(), reparsed.to_html());
    assert_eq!(reparsed.to_source(), canonical);
}
