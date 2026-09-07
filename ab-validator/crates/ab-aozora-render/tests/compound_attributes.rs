//! Co-applied layout properties retain one visible target and one directive.

use ab_aozora_pipeline::lex;
use ab_aozora_render::{render_html, serialize};
use ab_aozora_syntax::ast::{Node, NodeRef};
use ab_aozora_syntax::{BoutenPosition, ForwardAttr};

#[test]
fn compound_attributes_render_once_and_serialize_one_directive() {
    let source = "前１）［＃「１）」は縦中横、行右小書き］後。";
    let parsed = lex(source);
    let formats = parsed
        .source_nodes
        .iter()
        .filter_map(|entry| match entry.node {
            NodeRef::Inline(Node::Format(format)) => Some(format),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(formats.len(), 1);
    assert_eq!(
        parsed.store.resolve_forward_attrs(&formats[0].attrs),
        &[
            ForwardAttr::CombineUpright,
            ForwardAttr::SmallScript(BoutenPosition::Right)
        ]
    );
    let html = render_html(&parsed);
    assert_eq!(html.matches("１）").count(), 1, "{html}");
    assert_eq!(html.matches("<span").count(), 1, "{html}");
    assert!(html.contains("aozora-combine-upright"), "{html}");
    let small = ab_aozora_spec::roman_slug("行右小書き").unwrap();
    assert!(html.contains(&format!("aozora-{small}")), "{html}");
    assert_eq!(serialize(&parsed), source);
}
