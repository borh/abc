//! Emphasis selectors retain their exact disjoint or contextual source targets.
use ab_aozora_facade::{Document, ForwardOrigin, Node, NodeRef};

#[test]
fn selected_targets_have_separate_owned_spans_and_one_original_marker() {
    for (source, expected) in [
        (
            "地面の価格やのれんのねうち［＃「のれん」と「ねうち」に傍点］後",
            vec!["のれん", "ねうち"],
        ),
        ("法文・法律［＃「法律」の「法」に傍点］後", vec!["法"]),
        (
            "生、息、行、意気［＃「生」「息」「行」「意気」に傍点］後",
            vec!["生", "息", "行", "意気"],
        ),
    ] {
        let document = Document::new(source);
        let tree = document.parse();
        let selected: Vec<_> = tree
            .source_nodes()
            .iter()
            .filter_map(|node| {
                let NodeRef::Inline(Node::Format(format)) = node.node else {
                    return None;
                };
                (format.origin == ForwardOrigin::Detached).then(|| {
                    &source[node.source_span.start as usize..node.source_span.end as usize]
                })
            })
            .collect();
        assert_eq!(selected, expected, "{source}");
        assert_eq!(tree.to_source(), source);
        let serialized = tree.to_source();
        let reparsed = Document::new(serialized.as_str());
        assert_eq!(tree.to_html(), reparsed.parse().to_html());
    }
}

#[test]
fn unresolved_selectors_do_not_style_a_partial_or_different_target() {
    for source in [
        "のれんのみ［＃「のれん」と「ねうち」に傍点］",
        "のれんとのれんのねうち［＃「のれん」と「ねうち」に傍点］",
        "法律後［＃「法律」の「法」に傍点］",
        "法法［＃「法法」の「法」に傍点］",
        "法法法［＃「法法法」の「法法」に傍点］",
        "｜のれん《のれん》のねうち［＃「のれん」と「ねうち」に傍点］",
        "法律［＃「法律」の「律法」に傍点］",
        "法律［＃「法律」の「法」を除く部分に傍点］",
    ] {
        let document = Document::new(source);
        let tree = document.parse();
        assert!(
            !tree.source_nodes().iter().any(|node| matches!(node.node,
            NodeRef::Inline(Node::Format(format)) if format.origin == ForwardOrigin::Detached)),
            "{source}"
        );
        assert_eq!(tree.to_source(), source);
    }
}

#[test]
fn owned_context_selection_does_not_retarget_an_earlier_ruby() {
    let source = "｜法《ほう》と法律［＃「法律」の「法」に傍点］";
    let document = Document::new(source);
    let tree = document.parse();
    for node in tree.source_nodes() {
        if let NodeRef::Inline(Node::Ruby(ruby)) = node.node {
            assert!(ruby.base_emphasis.is_none());
        }
    }
    assert_eq!(tree.to_source(), source);
}
