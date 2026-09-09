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

/// A `｜` with no `《》` after it opens a ruby base that never ends. The
/// serializer emits the run it collected first and the unterminated `｜`
/// after it, which moves the text across the marker: `｜［＃］漢字` comes
/// back as `漢字｜［＃］`. In the reordered form the `［＃］` no longer sits
/// inside the ruby base, so the following `［` opens a bracket that swallows
/// it and the directive the first parse registered is gone from the second.
///
/// Both parses are locally consistent with their own input. Fixing this means
/// deciding what an unterminated `｜` base owns when it is written back out,
/// which is a question about the ruby emitter rather than about the pair
/// stage, so the test records the disagreement instead of asserting either
/// answer.
#[test]
#[ignore = "known defect: an unterminated ruby base reorders its text on serialize"]
fn directive_inside_an_unterminated_ruby_base_keeps_its_place() {
    let source = "｜［＃］漢字［《≫》";
    let first = lex(source);
    let second = lex(&serialize(&first));
    assert_eq!(
        directive_count(&first),
        directive_count(&second),
        "directive count drifted across a serialize round trip"
    );
}
