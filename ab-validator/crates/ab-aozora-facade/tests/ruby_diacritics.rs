//! Adjacent supplied dots decorate the ruby base without altering source ownership.

use ab_aozora_facade::Document;

#[test]
fn ruby_diacritic_preserves_source_and_reading() {
    for source in [
        "Samgha《サングハ》［＃mは上ドット付き］",
        "｜Samgha《サングハ》［＃mは上ドット付き］",
    ] {
        let document = Document::new(source);
        let tree = document.parse();
        assert_eq!(
            tree.to_source(),
            "｜Samgha《サングハ》［＃mは上ドット付き］"
        );
        let html = tree.to_html();
        assert!(html.contains("Saṁgha"), "{html}");
        assert!(html.contains("サングハ"), "{html}");
    }
}

#[test]
fn canonical_source_retains_normalized_scope_target_semantics() {
    let document = Document::new("〔samgha_disesa.v〕［＃mは上ドット付き］");
    let tree = document.parse();
    assert!(tree.to_html().contains("saṁghādisesa.v"));
    let serialized = tree.to_source();
    let reparsed_document = Document::new(serialized.as_str());
    let reparsed = reparsed_document.parse();
    assert!(
        reparsed.to_html().contains("saṁghādisesa.v"),
        "{serialized}: {}",
        reparsed.to_html()
    );
    assert_eq!(reparsed.to_source(), serialized);
}
