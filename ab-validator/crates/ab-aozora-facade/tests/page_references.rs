//! Page reference notes retain their source locator and visible page label.
use ab_aozora_facade::Document;

#[test]
fn page_reference_roundtrips_without_resolving_a_link() {
    let source = "三五頁［＃「三五頁」は「須佐の男の神」の「穀物の種」］にある。";
    let document = Document::new(source);
    let tree = document.parse();
    assert_eq!(tree.to_source(), source);
    assert!(!tree.to_html().contains("aozora-directive"));
}
