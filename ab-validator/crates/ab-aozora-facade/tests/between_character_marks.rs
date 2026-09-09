//! A mark the source places at a junction belongs to neither run it sits between.
use ab_aozora_facade::syntax::{MarginNoteKind, MarkAnchor};
use ab_aozora_facade::{Content, Document, Node, NodeRef};

/// Each junction mark as `(left run, right run, mark)`.
///
/// The two runs are read separately because the point of the marker is that it
/// sits between them: a single joined string would still pass if the parser
/// had quietly adopted the pair as one target.
fn between_marks(source: &str) -> Vec<(String, String, String)> {
    let document = Document::new(source);
    let tree = document.parse();
    let store = &tree.lex_output().store;
    let found = tree
        .source_nodes()
        .iter()
        .filter_map(|node| {
            let NodeRef::Inline(Node::MarginNote(note)) = node.node else {
                return None;
            };
            let MarkAnchor::Between = note.anchor? else {
                return None;
            };
            assert_eq!(note.kind, MarginNoteKind::SuppliedMark, "{source}");
            let [Content::Plain(left), Content::Plain(right)] =
                store.resolve_content_range(note.base)
            else {
                return None;
            };
            Some((
                store.resolve_str(*left).to_owned(),
                store.resolve_str(*right).to_owned(),
                store.content_range_as_plain(note.note)?.to_owned(),
            ))
        })
        .collect();
    // A marker the source wrote as a pair must come back as the pair it was
    // written as, not as a note on the run the two make together.
    assert_eq!(tree.to_source(), source, "{source}");
    found
}

#[test]
fn a_junction_mark_keeps_both_runs_it_sits_between() {
    assert_eq!(
        between_marks("自然［＃「自」と「然」の間に白三角傍点］"),
        [("自".to_owned(), "然".to_owned(), "白三角傍点".to_owned())]
    );
}

#[test]
fn consecutive_junction_marks_each_take_the_run_before_them() {
    // The corpus line that motivates this reads three of them in a row, so
    // each one has to claim only the pair it names.
    assert_eq!(
        between_marks(
            "自然［＃「自」と「然」の間に白三角傍点］はう［＃「は」と「う」の間に白三角傍点］たふ［＃「た」と「ふ」の間に白三角傍点］"
        ),
        [
            ("自".to_owned(), "然".to_owned(), "白三角傍点".to_owned()),
            ("は".to_owned(), "う".to_owned(), "白三角傍点".to_owned()),
            ("た".to_owned(), "ふ".to_owned(), "白三角傍点".to_owned()),
        ]
    );
}

#[test]
fn runs_that_do_not_meet_locate_no_junction() {
    for source in [
        // Something stands between them, so there is no one place to put the
        // mark and no reason to prefer either gap.
        "自X然［＃「自」と「然」の間に白三角傍点］",
        // The pair is not the run the marker follows.
        "自然他［＃「自」と「然」の間に白三角傍点］",
        // Named in an order the text does not have.
        "然自［＃「自」と「然」の間に白三角傍点］",
        // Three runs have two junctions, and the marker names neither.
        "自然は［＃「自」と「然」と「は」の間に白三角傍点］",
        // Not a mark this vocabulary knows.
        "自然［＃「自」と「然」の間に何か］",
    ] {
        assert!(between_marks(source).is_empty(), "{source}");
    }
}
