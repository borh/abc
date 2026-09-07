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
