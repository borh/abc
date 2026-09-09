//! Shapes where `lex` -> `serialize` -> `lex` is known not to be a fixed
//! point. Each test here is an ignored, executable statement of a defect: it
//! runs on demand with `cargo test -- --ignored`, and it starts passing when
//! the defect is fixed, which is the signal to un-ignore it.

use ab_aozora_pipeline::{NodeRef, lex};
use ab_aozora_render::serialize;
use ab_aozora_syntax::ast::Node;

fn directive_count(out: &ab_aozora_pipeline::LexOutput) -> usize {
    out.registry
        .iter_sorted()
        .filter(|(_, entry)| matches!(entry, NodeRef::Inline(Node::Directive(_))))
        .count()
}

/// The serializer pads a block directive with blank lines. On input whose
/// brackets do not balance, those newlines expire the open brackets that the
/// first parse still had on its stack, so a later `］` that closed a real
/// bracket in the first parse is an unmatched close in the second. An
/// unmatched close does not pop, by design, so the quote frames it would have
/// force-resolved stay open and bury the directive that follows them inside a
/// body: the first parse sees one directive and the second sees none.
///
/// Both parses are locally consistent with their own input. Fixing this means
/// deciding whether serializing malformed input may introduce newlines that
/// change its bracket structure, which is a question about the serializer's
/// padding rule rather than about the pair stage, so the test records the
/// disagreement instead of asserting either answer.
#[test]
#[ignore = "known defect: block-directive padding changes bracket structure on malformed input"]
fn directive_after_unmatched_closes_survives_block_directive_padding() {
    let source = "［［》》［＃ここから字下げ］《《≪≪］］［＃改］］";
    let first = lex(source);
    let second = lex(&serialize(&first));
    assert_eq!(
        directive_count(&first),
        directive_count(&second),
        "directive count drifted across a serialize round trip"
    );
}
