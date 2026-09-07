//! Scoped TCY retains its native region and all enclosed source constructs.

use ab_aozora_pipeline::lex;
use ab_aozora_syntax::{RegionClose, RegionFormat, ast::NodeRef};

#[test]
fn scoped_tcy_retains_markers_around_plain_and_gaiji_content() {
    for content in ["29", "（※［＃ローマ数字1、1-13-21］）"] {
        let source = format!("前［＃縦中横］{content}［＃縦中横終わり］後");
        let parsed = lex(&source);
        let regions: Vec<_> = parsed
            .source_nodes
            .iter()
            .filter(|node| matches!(node.node, NodeRef::BlockOpen(_) | NodeRef::BlockClose(_)))
            .collect();
        assert_eq!(regions.len(), 2);
        assert_eq!(
            regions[0].node,
            NodeRef::BlockOpen(RegionFormat::CombineUpright)
        );
        assert_eq!(
            regions[1].node,
            NodeRef::BlockClose(RegionClose::CombineUpright)
        );
        for (node, marker) in regions.iter().zip(["［＃縦中横］", "［＃縦中横終わり］"])
        {
            assert_eq!(
                &source[node.source_span.start as usize..node.source_span.end as usize],
                marker
            );
        }
        assert!(parsed.diagnostics.is_empty(), "{:?}", parsed.diagnostics);
    }
}

#[test]
fn tcy_prefixes_do_not_consume_unrecognized_compounds() {
    for source in ["［＃縦中横で行右小書き］", "［＃縦中横終わりの注記］"] {
        let parsed = lex(source);
        assert!(!parsed.source_nodes.iter().any(|node| matches!(
            node.node,
            NodeRef::BlockOpen(RegionFormat::CombineUpright)
                | NodeRef::BlockClose(RegionClose::CombineUpright)
        )));
    }
}

#[test]
fn unmatched_tcy_retains_the_actual_marker_without_synthetic_close() {
    let parsed = lex("前［＃縦中横］29後");
    assert!(
        parsed
            .source_nodes
            .iter()
            .any(|node| matches!(node.node, NodeRef::BlockOpen(RegionFormat::CombineUpright)))
    );
    assert!(
        !parsed
            .source_nodes
            .iter()
            .any(|node| matches!(node.node, NodeRef::BlockClose(_)))
    );
}
