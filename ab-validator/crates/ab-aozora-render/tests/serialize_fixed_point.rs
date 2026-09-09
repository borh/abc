//! Pin the Aozora-source serializer's "fixed point after one pass"
//! invariant.
//!
//! Contract: for every input that the lexer can ingest, running
//! `serialize ∘ parse` once produces a string `s`. A second pass —
//! `serialize ∘ parse(s)` — must produce the same `s` byte-for-byte.
//!
//! Some inputs are pinned to the stronger statement that the first pass
//! already reproduces the source: the normalizer's blank-line padding around
//! a block sentinel is internal, and adding it to the output changed what the
//! next parse saw.

use ab_aozora_pipeline::lex;
use ab_aozora_render::serialize;
use proptest::prelude::*;

fn round_trip(src: &str) -> String {
    let out = lex(src);
    serialize(&out)
}

fn fixed_point(src: &str) -> bool {
    let one = round_trip(src);
    let two = round_trip(&one);
    one == two
}

/// The normalizer writes a blank line either side of a block sentinel so the
/// line-oriented recognizers have a boundary to work with. Those newlines are
/// not source: writing them out inserts blank lines the author never wrote.
#[test]
fn a_block_directive_keeps_the_lines_the_source_gave_it() {
    for source in [
        "前\n［＃改ページ］\n後",
        "前\n\n［＃改ページ］\n\n後",
        "前\n\n\n\n［＃改ページ］\n\n\n\n後",
        "［＃ここから2字下げ］\n本文\n［＃ここで字下げ終わり］",
    ] {
        assert_eq!(
            round_trip(source),
            source,
            "serialize changed the blank lines"
        );
    }
}

/// On input whose brackets do not balance, the padding was parse-relevant:
/// the pair stage expires an open bracket at a newline, so a bracket the
/// first parse still held was gone by the time the next parse reached the `］`
/// that closed it, and the trailing directive ended up buried in a body
/// instead of at top level.
#[test]
fn a_block_directive_in_unbalanced_brackets_keeps_the_lines_the_source_gave_it() {
    let source = "［［》》［＃ここから字下げ］《《≪≪］］［＃改］］";
    assert_eq!(round_trip(source), source, "serialize introduced newlines");
}

#[test]
fn fixed_point_on_empty_input() {
    assert!(fixed_point(""));
}

#[test]
fn fixed_point_on_pure_ascii() {
    assert!(fixed_point("hello, world"));
}

#[test]
fn fixed_point_on_pure_japanese_prose() {
    assert!(fixed_point("青空文庫の本文。"));
}

#[test]
fn fixed_point_on_explicit_ruby() {
    assert!(fixed_point("｜青梅《おうめ》"));
}

#[test]
fn fixed_point_on_inline_ruby() {
    assert!(fixed_point("青梅《おうめ》"));
}

#[test]
fn fixed_point_on_page_break() {
    assert!(fixed_point("前\n\n［＃改ページ］\n\n後"));
}

#[test]
fn fixed_point_on_paragraph_breaks() {
    assert!(fixed_point("a\n\nb\n\nc"));
}

#[test]
fn fixed_point_on_html_unsafe_chars_in_source() {
    // Source contains literal `<`, `>`, `&`, `"`, `'`. Serialization
    // must NOT escape them — only HTML rendering does.
    let src = "a<b>&\"'";
    let out = round_trip(src);
    assert_eq!(out, src, "serializer must not escape HTML metachars");
    assert!(fixed_point(src));
}

#[test]
fn fixed_point_on_gaiji_reference() {
    // Real Aozora source: 「※［＃「木＋吶のつくり」、第3水準1-85-54］」
    let src = "※［＃「木＋吶のつくり」、第3水準1-85-54］";
    assert!(fixed_point(src));
}

#[test]
fn fixed_point_on_standalone_gaiji() {
    // No-`※` standalone external-character notes must round-trip
    // *without* gaining a `※` — the `standalone` flag suppresses it.
    for src in [
        "［＃「※」は「祿－示」、第3水準1-84-27、144-上-9］",
        "［＃「比」の「ヒ」に代えて「く」、第4水準2-1-23］",
    ] {
        assert_eq!(round_trip(src), src, "standalone gaiji must not gain a ※");
        assert!(fixed_point(src));
    }
}

#[test]
fn fixed_point_on_kaeriten() {
    let src = "学［＃二、レ点］而時習之";
    assert!(fixed_point(src));
}

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 256,
        .. ProptestConfig::default()
    })]

    /// Plain mixed text — letters, digits, hiragana, kanji, newlines,
    /// HTML-unsafe ASCII — must round-trip to a fixed point. The
    /// regex avoids generating Aozora markup characters that the
    /// lexer would split on; that subset is exercised in the
    /// hand-written tests above (and in the lex-pipeline crate).
    #[test]
    fn arbitrary_plain_text_is_a_fixed_point(
        s in "[A-Za-z0-9 \n<>&\"'\u{3042}-\u{3093}\u{4E00}-\u{4E20}]{0,80}",
    ) {
        prop_assert!(fixed_point(&s), "non-fixed point on input {:?}", s);
    }

    /// Length monotonicity: serialization can grow the input (PUA
    /// sentinels expand back to multi-char Aozora markers) but for
    /// pure-text input with no markup it must produce the same
    /// length.
    #[test]
    fn pure_text_length_preserved(
        s in "[A-Za-z0-9 \u{3042}-\u{3093}]{0,80}",
    ) {
        let out = round_trip(&s);
        prop_assert_eq!(
            out.len(), s.len(),
            "pure text input grew on serialize: {:?} -> {:?}", s, out,
        );
    }
}

#[test]
fn supplied_component_substitutions_keep_their_source_spelling() {
    for source in [
        "※［＃「闃」の「目」に代えて「自」］",
        "※［＃「贏」の「貝」に代えて「果」、（二）-27-3］",
    ] {
        assert_eq!(round_trip(source), source);
        assert!(fixed_point(source));
    }
}
