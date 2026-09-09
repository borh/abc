//! The normalizer keeps text where the source put it.
//!
//! `lex` replaces each recognized construct with a PUA sentinel and copies
//! everything else through verbatim, so the plain remainder of the normalized
//! text is a subsequence of the sanitized source, in source order. Nothing
//! else in the pipeline restores that order: what the normalizer emits is what
//! the parser IR, the TEI and the plaintext all carry, so a run emitted out of
//! order is a work published with its text rearranged.

use ab_aozora_pipeline::{NodeRef, lex};
use ab_aozora_render::serialize;
use ab_aozora_syntax::ast::Node;
use ab_notation_strategies::config::default_config;
use ab_notation_strategies::generators::{aozora_fragment, nested_pairs, pathological_aozora};
use proptest::prelude::*;

/// Every character the normalizer copied through, in the order it copied
/// them, must still be findable going forward through the sanitized source.
fn plain_runs_keep_source_order(source: &str) -> Result<(), String> {
    let out = lex(source);
    let sanitized = out.sanitized.as_str();
    let mut cursor = 0usize;
    for ch in out.normalized.as_str().chars() {
        // Sentinels stand for constructs whose source text is elsewhere, and
        // the blank lines around a block sentinel are the normalizer's own.
        if ('\u{E001}'..='\u{E004}').contains(&ch) || ch == '\n' {
            continue;
        }
        match sanitized[cursor..].find(ch) {
            Some(at) => cursor += at + ch.len_utf8(),
            None => {
                return Err(format!(
                    "{ch:?} is not reachable from byte {cursor} of the sanitized source"
                ));
            }
        }
    }
    Ok(())
}

fn directive_count(source: &str) -> usize {
    lex(source)
        .registry
        .iter_sorted()
        .filter(|(_, entry)| matches!(entry, NodeRef::Inline(Node::Directive(_))))
        .count()
}

/// A `｜` with no `《》` after it opens a base that never closes. A bracket
/// after the bar is held open as a possible continuation, because that is what
/// `｜漢字［＃「漢字」に傍点］` is; a literal bracket ends the possibility. The
/// base's own text was emitted after the run that follows it, which moved the
/// directive out of the base and let the next parse read it as bracket
/// content: one directive became none.
#[test]
fn a_literal_bracket_after_an_unterminated_ruby_base_keeps_the_base_in_place() {
    let source = "｜［＃］漢字［《≫》";
    assert_eq!(Ok(()), plain_runs_keep_source_order(source));
    assert_eq!(
        directive_count(source),
        directive_count(&serialize(&lex(source))),
        "directive count drifted across a serialize round trip"
    );
}

proptest! {
    #![proptest_config(default_config())]

    #[test]
    fn aozora_fragment_keeps_source_order(s in aozora_fragment(120)) {
        prop_assert_eq!(Ok(()), plain_runs_keep_source_order(&s));
    }

    #[test]
    fn pathological_input_keeps_source_order(s in pathological_aozora(120)) {
        prop_assert_eq!(Ok(()), plain_runs_keep_source_order(&s));
    }

    #[test]
    fn nested_pairs_keep_source_order(s in nested_pairs(24)) {
        prop_assert_eq!(Ok(()), plain_runs_keep_source_order(&s));
    }
}
