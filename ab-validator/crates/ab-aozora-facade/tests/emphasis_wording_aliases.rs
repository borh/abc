//! Equivalent emphasis wording keeps existing target and mark ownership.
use ab_aozora_facade::Document;

#[test]
fn supplied_emphasis_aliases_match_canonical_source_semantics() {
    for (alias, canonical) in [
        ("は傍点", "に傍点"),
        ("傍点", "に傍点"),
        ("に黒丸傍点", "に丸傍点"),
        ("の左に黒丸傍点", "の左に丸傍点"),
        ("の両側に黒丸傍点", "の両側に丸傍点"),
    ] {
        let source = format!("前語［＃「語」{alias}］後");
        let expected = format!("前語［＃「語」{canonical}］後");
        let document = Document::new(source.as_str());
        let tree = document.parse();
        let canonical_document = Document::new(expected.as_str());
        assert_eq!(
            tree.to_html(),
            canonical_document.parse().to_html(),
            "{alias}"
        );
        let serialized = tree.to_source();
        let reparsed = Document::new(serialized.as_str());
        assert_eq!(tree.to_html(), reparsed.parse().to_html());
    }
    for position in ["", "左に"] {
        let source = format!("［＃{position}黒丸傍点］語［＃{position}黒丸傍点終わり］");
        let canonical = source.replace("黒丸傍点", "丸傍点");
        let document = Document::new(source.as_str());
        let expected = Document::new(canonical.as_str());
        assert!(document.parse().to_html().contains("aozora-bouten-maru"));
        assert_eq!(document.parse().to_html(), expected.parse().to_html());
    }
}

#[test]
fn emphasis_aliases_do_not_select_missing_or_annotation_only_targets() {
    for source in [
        "前［＃「不在」は傍点］後",
        "前［＃「不在」傍点］後",
        "前［＃「不在」に黒丸傍点］後",
        "前［＃「語」は未定］［＃「語」傍点］後",
        "前語［＃「語」は傍点追加］後",
        "前語［＃「語」傍点追加］後",
        "前語［＃「語」に黒丸傍点追加］後",
    ] {
        let document = Document::new(source);
        let tree = document.parse();
        assert!(!tree.to_html().contains("<em"), "{source}");
        assert_eq!(tree.source(), source);
        let serialized = tree.to_source();
        let reparsed = Document::new(serialized.as_str());
        assert_eq!(tree.to_html(), reparsed.parse().to_html());
    }
}
