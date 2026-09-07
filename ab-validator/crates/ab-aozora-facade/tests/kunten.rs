//! Kunten categories determine layout while source serialization retains notation.
use ab_aozora_facade::Document;

#[test]
fn kunten_category_controls_rendering_and_roundtrips() {
    for (source, html) in [
        ("漢［＃レ］", "<sub class=\"aozora-kaeriten\">レ</sub>"),
        ("漢［＃（レ）］", "<sup class=\"aozora-okurigana\">レ</sup>"),
        ("給［＃（弖）］", "<sup class=\"aozora-okurigana\">弖</sup>"),
        ("所［＃一レ］", "<sub class=\"aozora-kaeriten\">一レ</sub>"),
    ] {
        let document = Document::new(source);
        let tree = document.parse();
        assert!(tree.to_html().contains(html), "{}", tree.to_html());
        assert_eq!(tree.to_source(), source);
    }
}

#[test]
fn retrospective_return_marks_reclassify_only_the_supplied_literal() {
    use ab_aozora_facade::syntax::ast::{KuntenKind, Node, NodeRef};
    let source = "飾レ［＃「レ」は返り点］中舍レ［＃「レ」は返り点］執二［＃「二」は返り点］其禮事一［＃「一」は返り点］";
    let document = Document::new(source);
    let tree = document.parse();
    let marks: Vec<_> = tree
        .source_nodes()
        .iter()
        .filter_map(|node| {
            if let NodeRef::Inline(Node::Kunten(mark)) = node.node {
                assert_eq!(mark.kind, KuntenKind::ReturnMark);
                Some(&source[node.source_span.start as usize..node.source_span.end as usize])
            } else {
                None
            }
        })
        .collect();
    assert_eq!(
        marks,
        [
            "レ［＃「レ」は返り点］",
            "レ［＃「レ」は返り点］",
            "二［＃「二」は返り点］",
            "一［＃「一」は返り点］"
        ]
    );
    assert_eq!(
        tree.to_source(),
        "飾［＃レ］中舍［＃レ］執［＃二］其禮事［＃一］"
    );
    assert_eq!(
        tree.to_html(),
        Document::new(tree.to_source()).parse().to_html()
    );
}

#[test]
fn unresolved_return_mark_operands_do_not_reclaim_other_source_text() {
    use ab_aozora_facade::syntax::ast::{Node, NodeRef};
    for source in [
        "漢［＃「レ」は返り点］",
        "漢レ別［＃「レ」は返り点］",
        "漢レ［＃「一」は返り点］",
        "漢レ［＃「レ」は返り点らしい］",
        "漢字［＃「字」は返り点］",
    ] {
        let document = Document::new(source);
        let tree = document.parse();
        assert!(
            !tree
                .source_nodes()
                .iter()
                .any(|node| matches!(node.node, NodeRef::Inline(Node::Kunten(_)))),
            "{source}"
        );
        assert_eq!(tree.to_source(), source);
    }
}
