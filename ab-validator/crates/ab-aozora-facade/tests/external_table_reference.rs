//! External table instructions are source references rather than supplied table content.
use ab_aozora_facade::{DirectiveKind, Document, Node, NodeRef};

#[test]
fn supplied_table_instruction_is_known_without_inserting_content() {
    let source = "前［＃ここに表組入る、別ファイル（densyanokonzatsu_table.txt）参照］後";
    let document = Document::new(source);
    let tree = document.parse();
    assert!(tree.diagnostics().is_empty());
    assert!(!tree.source_nodes().iter().any(
        |node| matches!(node.node, NodeRef::Inline(Node::Directive(d)) | NodeRef::BlockLeaf(Node::Directive(d)) if d.kind == DirectiveKind::Unknown)
    ));
    assert_eq!(tree.to_source(), source);
    let html = tree.to_html();
    assert!(!html.contains("<table"));
    assert!(!html.contains("<img"));
}
