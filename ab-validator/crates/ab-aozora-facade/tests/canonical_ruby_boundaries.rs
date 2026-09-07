//! Canonical source keeps each ruby base explicit, independent of preceding text.
use ab_aozora_facade::Document;

#[test]
fn explicit_canonical_ruby_preserves_plain_rich_and_gaiji_bases() {
    for source in [
        "漢字《かんじ》",
        "前｜漢字《かんじ》後",
        "abc《エービーシー》",
        "※［＃ローマ数字1、1-13-21］《いち》",
        "※［＃ローマ数字1、1-13-21］※［＃ローマ数字2、1-13-22］《いちに》",
        "｜漢※［＃ローマ数字1、1-13-21］《よみ》",
        "｜前［＃未知］｜漢字《かんじ》",
        "｜前\n漢字《かんじ》",
    ] {
        let original = Document::new(source);
        let tree = original.parse();
        let canonical = tree.to_source();
        let restored = Document::new(canonical.clone());
        assert_eq!(
            tree.to_html(),
            restored.parse().to_html(),
            "{source}: {canonical}"
        );
        assert_eq!(canonical, restored.parse().to_source(), "{source}");
    }
}
