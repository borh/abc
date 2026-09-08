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

/// An unpaired `［` before a block directive suppresses a later directive in
/// the first parse. The serializer then pads the block directive with blank
/// lines, which separates the stray bracket from what follows it, so the
/// second parse sees the later directive after all.
///
/// Fixing this means deciding which of the two readings is correct on
/// malformed input, not just making the counts agree, which is why the test
/// records the disagreement instead of asserting either answer.
#[test]
#[ignore = "known defect: serializing an unpaired bracket changes what the next parse sees"]
fn unpaired_bracket_before_a_block_directive_round_trips() {
    let source = "［［＃ここから字下げ］［＃\n］";
    let first = lex(source);
    let second = lex(&serialize(&first));
    assert_eq!(
        directive_count(&first),
        directive_count(&second),
        "directive count drifted across a serialize round trip"
    );
}
