//! Container recovery stays observable without manufacturing source pairs.

use ab_aozora_facade::{Document, Severity};

#[test]
fn missing_closes_report_the_exact_opening_marker() {
    for marker in [
        "［＃縦中横］",
        "［＃ここから2字下げ］",
        "［＃ここから太字］",
    ] {
        let source = format!("前{marker}漢字《かんじ》後");
        let document = Document::new(source.clone());
        let tree = document.parse();
        assert_eq!(tree.source(), source);
        assert_eq!(tree.to_source().replace('\n', ""), source);
        let diagnostics = tree.diagnostics();
        let diagnostic = diagnostics
            .iter()
            .find(|d| d.code().ends_with("::unclosed_container"))
            .expect("missing recovery diagnostic");
        assert_eq!(diagnostic.severity(), Severity::Error);
        let span = diagnostic.span();
        assert_eq!(&source[span.start as usize..span.end as usize], marker);
        assert!(tree.container_pairs().is_empty());
        assert!(tree.to_html().contains("後"));
    }
}

#[test]
fn orphan_closes_and_crossing_families_never_establish_pairs() {
    for (source, expected) in [
        ("前［＃縦中横終わり］後", "unmatched_container_close"),
        (
            "［＃縦中横］前［＃ここで字下げ終わり］",
            "mismatched_container_close",
        ),
        (
            "［＃ここから2字下げ］［＃縦中横］前［＃ここで字下げ終わり］後［＃縦中横終わり］",
            "mismatched_container_close",
        ),
    ] {
        let document = Document::new(source);
        let tree = document.parse();
        assert_eq!(tree.source(), source);
        assert_eq!(tree.to_source().replace('\n', ""), source);
        assert!(
            tree.diagnostics()
                .iter()
                .any(|d| d.code().ends_with(expected)),
            "{:?}",
            tree.diagnostics()
        );
        assert!(
            tree.container_pairs().is_empty(),
            "{:?}",
            tree.container_pairs()
        );
    }
}

#[test]
fn correctly_nested_scopes_keep_their_established_pairs() {
    let source = "［＃ここから2字下げ］［＃縦中横］前［＃縦中横終わり］後［＃ここで字下げ終わり］";
    let document = Document::new(source);
    let tree = document.parse();
    assert!(tree.diagnostics().is_empty(), "{:?}", tree.diagnostics());
    assert_eq!(tree.container_pairs().len(), 2);
    assert_eq!(tree.to_source().replace('\n', ""), source);
}

#[test]
fn missing_outer_close_does_not_invalidate_a_matched_inner_scope() {
    let source = "［＃ここから2字下げ］［＃縦中横］29［＃縦中横終わり］";
    let document = Document::new(source);
    let tree = document.parse();
    assert_eq!(tree.container_pairs().len(), 1);
    assert_eq!(tree.diagnostics().len(), 1);
    assert!(
        tree.diagnostics()[0]
            .code()
            .ends_with("::unclosed_container")
    );
    let span = tree.diagnostics()[0].span();
    assert_eq!(
        &source[span.start as usize..span.end as usize],
        "［＃ここから2字下げ］"
    );
    assert_eq!(tree.source(), source);
}
