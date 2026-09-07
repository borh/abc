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

#[test]
fn one_supplied_closer_can_end_the_named_nested_scopes() {
    for close in [
        "［＃ここで２段組み、罫囲み終わり］",
        "［＃ここで段組、罫囲み終わり］",
    ] {
        let source = format!("［＃ここから罫囲み］\n［＃ここから２段組］\n本文\n{close}");
        let document = Document::new(source.clone());
        let tree = document.parse();
        assert!(tree.diagnostics().is_empty(), "{:?}", tree.diagnostics());
        assert_eq!(tree.container_pairs().len(), 2);
        assert_eq!(tree.source(), source);
        let serialized = tree.to_source();
        assert_eq!(serialized.matches(close).count(), 1);
        let reparsed_document = Document::new(serialized);
        let reparsed = reparsed_document.parse();
        assert!(reparsed.diagnostics().is_empty());
        assert_eq!(reparsed.container_pairs().len(), 2);
        assert_eq!(
            tree.container_pairs()[0].source_end,
            tree.container_pairs()[1].source_end
        );
    }
}

#[test]
fn shared_closer_never_partially_consumes_a_mismatched_stack() {
    let close = "［＃ここで２段組み、罫囲み終わり］";
    for openers in [
        "［＃ここから罫囲み］［＃ここから３段組み］",
        "［＃ここから２段組み］",
        "［＃ここから罫囲み］［＃ここから太字］［＃ここから２段組み］",
        "［＃ここから２段組み］［＃ここから罫囲み］",
    ] {
        let source = format!("{openers}\n本文\n{close}");
        let document = Document::new(source.as_str());
        let tree = document.parse();
        assert!(tree.container_pairs().is_empty());
        assert!(!tree.diagnostics().is_empty());
        assert_eq!(tree.to_source().matches(close).count(), 1);
        assert_eq!(tree.source(), source);
    }
    let source = format!(
        "［＃ここから罫囲み］［＃ここから３段組み］\n本文\n{close}\n［＃ここで３段組み終わり］［＃罫囲み終わり］"
    );
    let document = Document::new(source.as_str());
    let tree = document.parse();
    assert_eq!(tree.container_pairs().len(), 2);
    assert_eq!(tree.to_source().matches(close).count(), 1);
}
