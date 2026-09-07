//! Whole glyph descriptions preserve nested component references as source data.
use ab_aozora_facade::Document;

#[test]
fn nested_description_is_one_unresolved_source_glyph() {
    for marker in [
        "※［＃「姉」の正字、「女＋※［＃第3水準1-85-57］のつくり」、252-下-27］",
        "※［＃「姉」の正字、「※［＃「柿」の正字、第3水準1-85-57］」の「木」に代えて「女」、749-12］",
        "※［＃「金＋※［＃「插」でつくりの縦棒が下に突き抜けている、第4水準2-13-28］のつくり」、161-下-29］",
    ] {
        let source = format!("前{marker}後");
        let document = Document::new(source.as_str());
        let tree = document.parse();
        let glyphs: Vec<_> = tree
            .source_nodes()
            .iter()
            .filter(|node| node.node.kind().as_json_tag() == "gaiji")
            .collect();
        assert_eq!(glyphs.len(), 1, "{marker}");
        let span = glyphs[0].source_span;
        assert_eq!(&source[span.start as usize..span.end as usize], marker);
        let serialized = tree.to_source();
        assert_eq!(serialized, source);
        let reparsed = Document::new(serialized.as_str());
        assert_eq!(reparsed.parse().to_html(), tree.to_html());
    }
}
