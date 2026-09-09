//! Input strategies for the workspace's property suites.
//!
//! The strategies are stratified rather than monolithic: each targets one
//! shape of input that exercises a particular class of invariant. A test opts
//! into the shapes it needs, so a shrinker narrows on the offending shape
//! instead of wandering through unrelated regions of the input space.
//!
//! | Strategy | Shape | What it is for |
//! |---|---|---|
//! | [`aozora_fragment`] | Trigger glyphs, prose filler, decorative rule rows | The workhorse: parse is total, no sentinel leaks |
//! | [`pathological_aozora`] | Unbalanced and stacked delimiters | Malformed input never panics |
//! | [`unicode_adversarial`] | Combining marks, overrides, PUA, astral planes | The parser never decodes its way into a panic |
//! | [`xss_payload`] | Script-injection vectors | Nothing executable survives HTML rendering |
//! | [`nested_pairs`] | Delimiters nested to depth | The pair stage's frame stack under depth |
//!
//! The Aozora vocabulary comes from `ab_aozora_spec`: [`trigger_glyphs`]
//! reads the parser's own trigger set, and [`unicode_adversarial`] reads the
//! reserved sentinel characters. Restating either as a literal list here
//! would let the generators drift from the parser they test.

use std::str;

use ab_aozora_spec::{ALL_SENTINELS, trigger::ALL_TRIGGER_TRIGRAMS};
use proptest::prelude::*;
use proptest::sample::select;

/// Every Aozora trigger character, as the parser defines them.
///
/// Each trigger is a 3-byte BMP codepoint, so the spec's trigram table is
/// also the list of trigger characters once decoded.
///
/// # Panics
///
/// Panics if a trigram in the spec is not valid UTF-8. Every entry is a BMP
/// codepoint's encoding, so this cannot fire without a corrupt spec table.
#[must_use]
pub fn trigger_glyphs() -> Vec<String> {
    ALL_TRIGGER_TRIGRAMS
        .iter()
        .map(|trigram| {
            str::from_utf8(trigram)
                .expect("every trigger trigram is a BMP codepoint's UTF-8 encoding")
                .to_owned()
        })
        .collect()
}

/// Prose and layout filler: what sits between the triggers in real source.
const FILLER: &[&str] = &[
    "漢字",
    "かんじ",
    "改ページ",
    "改丁",
    "ABC",
    "1234",
    "、",
    "。",
    " ",
    "\n",
    "\n\n",
];

/// Runs of the three characters `CommonMark` reads as a setext underline or a
/// thematic break.
///
/// These patterns exercise the sanitize stage's decorative-rule isolation. A
/// 底本 that draws a divider as a row of hyphens must remain a paragraph
/// followed by a rule, rather than turning the preceding paragraph into a
/// heading.
///
/// The widths bracket the isolator's minimum, `DECORATIVE_RULE_MIN_LEN`, which
/// is 10. Three and nine are below it: `CommonMark` still reads them as a
/// thematic break or a setext underline, but the isolator leaves them alone,
/// so they are the side of the boundary where a disagreement between the two
/// would show. Twelve and thirty-five are above it, and differ from each other
/// so a failure shrinks to the shorter row.
fn decorative_rule_rows() -> Vec<String> {
    ["-", "=", "_"]
        .into_iter()
        .flat_map(|mark| {
            [3, 9, 12, 35]
                .into_iter()
                .map(move |width| mark.repeat(width))
        })
        .collect()
}

/// Mixed-shape Aozora source: 0 to `max_atoms` atoms drawn from the trigger
/// set, prose filler, and decorative rule rows, concatenated.
///
/// This is the primary strategy for "parse is total" and "no sentinel
/// escapes into output" properties. It emits *unbalanced* delimiter shapes on
/// purpose, so shrinking can surface a malformed-input panic; a property that
/// needs well-formed input gates on the lexer's diagnostics instead of
/// expecting this generator to supply it.
pub fn aozora_fragment(max_atoms: usize) -> impl Strategy<Value = String> {
    let mut atoms = trigger_glyphs();
    atoms.extend(FILLER.iter().map(|filler| (*filler).to_owned()));
    atoms.extend(decorative_rule_rows());
    prop::collection::vec(select(atoms), 0..=max_atoms).prop_map(|pieces| pieces.concat())
}

/// Directive shapes that are well-formed as text but arrive without their
/// partner, or with their partner in the wrong order.
const UNPAIRED_DIRECTIVES: &[&str] = &[
    "［＃ここから字下げ］",
    "［＃ここで字下げ終わり］",
    "［＃ここから罫囲み］",
    "［＃ここで罫囲み終わり］",
    "［＃「」は大見出し］",
    "［＃「X」に傍点］",
    "［＃改",
    "※［＃",
];

/// Adversarial Aozora source: delimiters that stack up without closing, close
/// without opening, or pair across the wrong kind.
///
/// No atom can balance on its own: each is a doubled delimiter, a truncated
/// directive, or a complete container directive without its partner. Joining
/// a handful therefore produces an imbalance reliably, which is what drives
/// the pair stage's stack into its error paths. `max_depth` caps how many
/// atoms are joined; 4 to 8 keeps the shrinker fast. Well past that the draws
/// are almost all malformed, and shrinking spends its budget inside the
/// malformed regime.
pub fn pathological_aozora(max_depth: usize) -> impl Strategy<Value = String> {
    let mut atoms: Vec<String> = doubled_trigger_glyphs();
    atoms.extend(UNPAIRED_DIRECTIVES.iter().map(|shape| (*shape).to_owned()));
    atoms.extend(["｜ABC《".to_owned(), "》DEF".to_owned(), "\n\n".to_owned()]);
    prop::collection::vec(select(atoms), 0..=max_depth).prop_map(|pieces| pieces.concat())
}

/// Every trigger character doubled.
///
/// Doubling is what makes an atom reliably unbalanced: two of a delimiter
/// cannot both find a partner in a fragment assembled from other doubled
/// atoms.
fn doubled_trigger_glyphs() -> Vec<String> {
    ALL_TRIGGER_TRIGRAMS
        .iter()
        .map(|trigram| {
            str::from_utf8(trigram)
                .expect("every trigram is a BMP codepoint's UTF-8 encoding")
                .repeat(2)
        })
        .collect()
}

/// Unicode edge cases: sequences that are valid UTF-8 but hostile to naive
/// index arithmetic, plus the codepoints the lexer reserves for itself.
///
/// The reserved sentinels come from `ab_aozora_spec::ALL_SENTINELS` rather
/// than a literal list. Source text that already contains one must produce a
/// diagnostic and must not panic, and that property is only as good as this
/// generator's agreement with what the lexer actually reserves.
pub fn unicode_adversarial() -> impl Strategy<Value = String> {
    let mut cases: Vec<String> = vec![
        // Combining marks stacked on ASCII bases: char count and byte length
        // diverge, and so do grapheme boundaries.
        "a\u{0301}\u{0302}\u{0303}e\u{0304}".to_owned(),
        // Right-to-left override opened and closed around Latin text.
        "\u{202E}abc\u{202C}def".to_owned(),
        // A byte order mark away from offset zero, where the sanitize stage
        // does not strip it.
        "pre\u{FEFF}post".to_owned(),
        // Private use beyond the reserved sentinels.
        "\u{E5FF}\u{F8FF}text".to_owned(),
        // Zero-width joiner and non-joiner.
        "a\u{200C}b\u{200D}c".to_owned(),
        // Variation selector following a CJK base.
        "字\u{FE0F}".to_owned(),
        // Fullwidth forms of ASCII.
        "ＡＢＣ１２３".to_owned(),
        // Astral plane: 4-byte encodings, where a 3-byte trigram assumption
        // would misalign.
        "😀🎌🗾".to_owned(),
        // Fullwidth brackets around fullwidth content.
        "［＃ｂｒａｃｋｅｔ］".to_owned(),
    ];
    // Each reserved sentinel, embedded between two ordinary characters so a
    // failure names the sentinel rather than a bare one-character input.
    cases.extend(ALL_SENTINELS.iter().map(|sentinel| format!("X{sentinel}Y")));
    select(cases)
}

/// Script-injection vectors, one per way a renderer can leak executable
/// content.
///
/// Grouped by the escaping mistake each one catches: raw tags, event handler
/// attributes, `javascript:` and `data:` URLs, comment and CDATA smuggling,
/// percent-encoding that must stay literal, and fullwidth lookalikes that
/// must not be folded to ASCII. The last two put Aozora markup on either side
/// of the payload, since the interesting bugs live where the two escaping
/// paths meet.
pub fn xss_payload() -> impl Strategy<Value = String> {
    select(vec![
        // Raw script elements, including case and attribute variation.
        "<script>alert(1)</script>".to_owned(),
        "<SCRIPT>alert(1)</SCRIPT>".to_owned(),
        "<script src=x>".to_owned(),
        "<script xmlns='http://www.w3.org/1999/xhtml'>bad()</script>".to_owned(),
        "<<script>alert(1)</script>".to_owned(),
        // Event handler attributes on elements that fire without interaction.
        "<img src=x onerror=alert(1)>".to_owned(),
        "<svg onload=alert(1)>".to_owned(),
        "<body onload=alert(1)>".to_owned(),
        "<div onmouseover=alert(1)>hover</div>".to_owned(),
        "<div class=\"\" onclick=\"alert(1)\">".to_owned(),
        // Script-bearing URLs in both quoting styles, and in a frame.
        "<a href=\"javascript:alert(1)\">go</a>".to_owned(),
        "<a href='javascript:alert(1)'>go</a>".to_owned(),
        "<iframe src=javascript:alert(1)>".to_owned(),
        "<a href=\"data:text/html,<script>x()</script>\">go</a>".to_owned(),
        // Smuggling through constructs a passthrough might not descend into.
        "<!--<script>x()</script>-->".to_owned(),
        "<![CDATA[<script>bad()</script>]]>".to_owned(),
        // Percent-encoded: must survive as literal text, never be decoded.
        "%3Cscript%3Ealert(1)%3C%2Fscript%3E".to_owned(),
        // Fullwidth angle brackets: must not be normalised to ASCII, even
        // though the parser does handle fullwidth ［ as markup.
        "＜script＞alert(1)＜/script＞".to_owned(),
        // Payload adjacent to Aozora markup on both sides.
        "｜漢字《かんじ》<script>x()</script>".to_owned(),
        "［＃「<script>」は大見出し］".to_owned(),
    ])
}

/// Delimiters nested to a drawn depth with no content between them: `depth`
/// opens followed by `depth` closes.
///
/// The workhorse [`aozora_fragment`] spreads its atoms thin, so it rarely
/// reaches the depth that would corrupt the pair stage's frame stack. This
/// strategy goes straight there. Depth is drawn from `0..=max_depth` rather
/// than fixed at the maximum so a failure shrinks to the shallowest depth
/// that still reproduces it.
pub fn nested_pairs(max_depth: usize) -> impl Strategy<Value = String> {
    let pairs = [("《", "》"), ("［＃", "］"), ("｜", "》"), ("〔", "〕")];
    (0_usize..=max_depth, select(pairs.to_vec())).prop_map(|(depth, (open, close))| {
        let mut nested = String::with_capacity((open.len() + close.len()) * depth);
        for _ in 0..depth {
            nested.push_str(open);
        }
        for _ in 0..depth {
            nested.push_str(close);
        }
        nested
    })
}

#[cfg(test)]
mod tests {
    use super::{ALL_TRIGGER_TRIGRAMS, trigger_glyphs};

    #[test]
    fn every_trigger_glyph_decodes_to_one_character() {
        let glyphs = trigger_glyphs();
        assert_eq!(glyphs.len(), ALL_TRIGGER_TRIGRAMS.len());
        for glyph in glyphs {
            assert_eq!(
                glyph.chars().count(),
                1,
                "a trigger is one character, got {glyph:?}"
            );
            assert_eq!(glyph.len(), 3, "a trigger is 3 UTF-8 bytes, got {glyph:?}");
        }
    }
}
