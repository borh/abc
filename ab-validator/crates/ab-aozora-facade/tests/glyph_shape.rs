//! Glyph-shape instructions qualify existing source scalars without realizing a new glyph.
use ab_aozora_facade::{Document, Node, NodeRef};

#[test]
fn glyph_shape_assertions_keep_the_adjacent_scalar_and_source_spelling() {
    for source in [
        "Yule（ユール［＃「ル」は上に「⌒」付き］）",
        "♂［＃「♂」は矢印が下向き］例",
    ] {
        let document = Document::new(source);
        let tree = document.parse();
        assert!(
            tree.source_nodes()
                .iter()
                .any(|node| matches!(node.node, NodeRef::Inline(Node::MarginNote(_))))
        );
        assert_eq!(tree.to_source(), source);
        let html = tree.to_html();
        assert!(!html.contains("<ruby>"));
        assert!(html.contains("data-shape-assertion="));
        assert!(html.contains(">ル</span>") || html.contains(">♂</span>"));
    }
}
