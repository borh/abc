//! Classified-source fact projection and canonicalization contracts.

use ab_aozora_pipeline::{
    ClassifiedSourceDisposition as Disposition, ClassifiedSourceEvidenceClass as EvidenceClass,
    ClassifiedSourceRole as Role, ConstructId, canonicalize_classified_source_facts, lex,
};

fn projection(source: &str) -> Vec<(u32, u32, ConstructId, Role, Disposition, EvidenceClass)> {
    lex(source)
        .classified_source_facts
        .into_iter()
        .map(|fact| {
            (
                fact.source_span.start,
                fact.source_span.end,
                fact.construct_id,
                fact.source_role,
                fact.disposition,
                fact.evidence_class,
            )
        })
        .collect()
}

#[test]
fn projects_plain_recovery_newline_and_unknown_directive() {
    assert_eq!(
        projection("plain"),
        vec![(
            0,
            5,
            ConstructId::PlainText,
            Role::VisibleText,
            Disposition::EmittedSemanticValue,
            EvidenceClass::AcceptedText,
        )]
    );
    assert_eq!(
        projection("｜"),
        vec![(
            0,
            3,
            ConstructId::RecoveredVerbatim,
            Role::UnrecognizedSourceForm,
            Disposition::PreservedOpaque,
            EvidenceClass::RecoveredVerbatim,
        )]
    );
    assert_eq!(
        projection("a\r\nb"),
        vec![
            (
                0,
                1,
                ConstructId::PlainText,
                Role::VisibleText,
                Disposition::EmittedSemanticValue,
                EvidenceClass::AcceptedText
            ),
            (
                1,
                2,
                ConstructId::Newline,
                Role::StructuralNewline,
                Disposition::StructuralControl,
                EvidenceClass::StructuralToken
            ),
            (
                2,
                3,
                ConstructId::PlainText,
                Role::VisibleText,
                Disposition::EmittedSemanticValue,
                EvidenceClass::AcceptedText
            ),
        ]
    );
    assert_eq!(
        projection("［＃まったく未知の注記です］"),
        vec![(
            0,
            42,
            ConstructId::UnknownDirective,
            Role::UnrecognizedSourceForm,
            Disposition::PreservedOpaque,
            EvidenceClass::UnknownDirective,
        )]
    );
}

#[test]
fn projects_typed_nodes_and_container_markers() {
    let ruby = projection("｜青梅《おうめ》");
    assert_eq!(ruby[0].2, ConstructId::Ruby);
    assert_eq!(ruby[0].3, Role::Ruby);
    assert_eq!(ruby[0].4, Disposition::EmittedSemanticValue);
    assert_eq!(ruby[0].5, EvidenceClass::TypedNode);

    assert_eq!(
        projection("［＃ここから2字下げ］\n本文\n［＃ここで字下げ終わり］")
            .into_iter()
            .map(|fact| fact.2)
            .collect::<Vec<_>>(),
        vec![
            ConstructId::ContainerOpen,
            ConstructId::Newline,
            ConstructId::PlainText,
            ConstructId::Newline,
            ConstructId::ContainerClose,
        ]
    );
}

#[test]
fn overlapping_lowering_emits_complete_canonical_value_order() {
    let out = lex("題\n［＃「題」は大見出し］");
    assert_eq!(
        out.classified_source_facts
            .first()
            .unwrap()
            .source_span
            .start,
        0
    );
    assert_eq!(
        out.classified_source_facts.last().unwrap().source_span.end,
        out.sanitized_len
    );
    assert!(out.classified_source_facts.windows(2).all(|pair| {
        (pair[0].source_span.start, pair[0].source_span.end)
            <= (pair[1].source_span.start, pair[1].source_span.end)
    }));

    let mut reversed = out.classified_source_facts.clone();
    reversed.reverse();
    assert_eq!(
        canonicalize_classified_source_facts(reversed),
        canonicalize_classified_source_facts(out.classified_source_facts)
    );
}

#[test]
fn canonicalization_preserves_duplicates_for_fail_closed_validation() {
    let fact = lex("text").classified_source_facts[0];
    assert_eq!(
        canonicalize_classified_source_facts(vec![fact, fact]),
        vec![fact, fact]
    );
}

#[hegel::test(test_cases = 100)]
fn permutation_canonicalizes_identically(tc: hegel::TestCase) {
    use hegel::generators;

    let parts = tc.draw(generators::vecs(generators::booleans()).max_size(64));
    let source: String = parts
        .iter()
        .map(|semantic| if *semantic { "文" } else { "｜" })
        .collect();
    let facts = lex(&source).classified_source_facts;
    let mut reversed = facts.clone();
    reversed.reverse();
    assert_eq!(
        canonicalize_classified_source_facts(facts),
        canonicalize_classified_source_facts(reversed)
    );
}

#[hegel::test(test_cases = 100)]
fn semantic_intervals_are_a_subset_of_accounted(tc: hegel::TestCase) {
    use hegel::generators;

    let source = tc.draw(generators::text().max_size(96));
    let facts = lex(&source).classified_source_facts;
    for semantic in facts.iter().filter(|fact| fact.is_semantic()) {
        assert!(facts.iter().any(|accounted| {
            accounted.is_accounted()
                && accounted.source_span.start <= semantic.source_span.start
                && accounted.source_span.end >= semantic.source_span.end
        }));
    }
}

#[hegel::test(test_cases = 100)]
fn opaque_facts_are_never_semantic(tc: hegel::TestCase) {
    use hegel::generators;

    let source = tc.draw(generators::text().max_size(96));
    for fact in lex(&source).classified_source_facts {
        if fact.disposition == Disposition::PreservedOpaque {
            assert!(!fact.is_semantic());
            assert!(fact.is_accounted());
        }
    }
}
