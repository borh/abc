//! Trigger scan: byte offsets of every Aozora notation marker in the source.
//!
//! Aozora's 13 notation triggers (`｜《》≪≫［］＃※〔〕「」`) are each a
//! 3-byte BMP UTF-8 codepoint. The tokenize stage needs their positions
//! before it can cut the buffer into text runs and delimiters, so this is
//! the first pass over the source.
//!
//! Matching fingerprints all three bytes of each trigram rather than
//! filtering on the leading byte. Six of the triggers lead with `0xE3`, and
//! so does every kana codepoint, so a leading-byte filter would nominate
//! most of a Japanese text as a candidate. The patterns come from
//! [`ALL_TRIGGER_TRIGRAMS`], the same constant
//! [`classify_trigger_bytes`] is pinned against, so the scanner and the
//! classifier cannot disagree about what counts as a trigger.
//!
//! [`naive_scan_offsets`] classifies the window at every byte position
//! instead. It is the independent reference the automaton is differentially
//! tested against below, not a production path.

use std::sync::OnceLock;

use ab_aozora_spec::classify_trigger_bytes;
use ab_aozora_spec::trigger::ALL_TRIGGER_TRIGRAMS;
use aho_corasick::{AhoCorasick, MatchKind};

/// The process-wide trigger automaton, built once.
///
/// Construction, and `aho-corasick`'s one-time runtime CPU-feature detection
/// for its packed backend, are amortised across every parse in the process.
fn automaton() -> &'static AhoCorasick {
    static AUTOMATON: OnceLock<AhoCorasick> = OnceLock::new();
    AUTOMATON.get_or_init(|| {
        AhoCorasick::builder()
            .match_kind(MatchKind::Standard)
            .build(ALL_TRIGGER_TRIGRAMS)
            .expect("the 13 fixed trigger trigrams always compile")
    })
}

/// Build the trigger automaton now rather than inside the first scan.
/// Idempotent and sub-microsecond after the first call.
pub(crate) fn prewarm() {
    let _ = automaton();
}

/// Byte offsets of every trigger character in `source`, ascending.
///
/// Non-overlapping matching is equivalent to classifying every byte position
/// here, which is what makes the automaton and [`naive_scan_offsets`]
/// interchangeable: all 13 patterns are three bytes long and lead with
/// `0xE2`, `0xE3` or `0xEF`, while their continuation bytes are all in
/// `0x80..=0xBF`. No trigram can therefore begin inside another, so no match
/// can mask a second one.
///
/// # Panics
///
/// Panics if `source` is longer than [`u32::MAX`] bytes. The tokenize stage
/// asserts that bound on construction, before it reaches this function.
pub(crate) fn scan_offsets(source: &str) -> Vec<u32> {
    automaton()
        .find_iter(source)
        .map(|m| u32::try_from(m.start()).expect("source longer than u32::MAX is unsupported"))
        .collect()
}

/// Brute-force reference scan: classify the 3-byte window at every byte
/// position, keeping the positions that classify as a trigger.
///
/// This is the independent oracle for [`scan_offsets`], not a production
/// path: the two share only the trigger set, so a fault in the automaton's
/// construction or in its match handling shows up as a disagreement. It is
/// `pub` (though hidden) because the pipeline-level scan properties in
/// `tests/property_scan_equiv.rs` need the same oracle from outside the
/// crate.
///
/// # Panics
///
/// Panics if `source` is longer than [`u32::MAX`] bytes.
#[doc(hidden)]
#[must_use]
pub fn naive_scan_offsets(source: &str) -> Vec<u32> {
    let bytes = source.as_bytes();
    let mut offsets = Vec::new();
    if bytes.len() < 3 {
        return offsets;
    }
    for start in 0..=bytes.len() - 3 {
        let window = [bytes[start], bytes[start + 1], bytes[start + 2]];
        if classify_trigger_bytes(window).is_some() {
            offsets.push(u32::try_from(start).expect("source longer than u32::MAX is unsupported"));
        }
    }
    offsets
}

#[cfg(test)]
mod tests {
    use ab_notation_strategies::config::default_config;
    use ab_notation_strategies::generators::{
        aozora_fragment, pathological_aozora, unicode_adversarial,
    };
    use proptest::prelude::*;

    use super::{naive_scan_offsets, scan_offsets};

    /// The automaton and the brute-force reference must produce identical
    /// offset vectors for any input.
    fn assert_scan_matches_naive(source: &str) {
        assert_eq!(
            scan_offsets(source),
            naive_scan_offsets(source),
            "scan_offsets diverged from the naive reference for input {source:?}",
        );
    }

    // Hand-curated regression anchors: one per trigger kind plus the double
    // variants. These catch the obvious regressions even when the proptests
    // below run with a reduced case count.

    #[test]
    fn empty_input_yields_no_offsets() {
        assert_scan_matches_naive("");
    }

    #[test]
    fn each_trigger_glyph() {
        for source in [
            "｜", "《", "》", "［", "］", "＃", "※", "〔", "〕", "「", "」",
        ] {
            assert_scan_matches_naive(source);
        }
    }

    #[test]
    fn double_glyph_sequences() {
        assert_scan_matches_naive("《《");
        assert_scan_matches_naive("》》");
        assert_scan_matches_naive("《《重要》》");
        // Double-angle quotation triggers (U+226A/U+226B).
        assert_scan_matches_naive("≪");
        assert_scan_matches_naive("≫");
        assert_scan_matches_naive("≪重要≫");
    }

    #[test]
    fn ascii_only_has_zero_triggers() {
        assert_scan_matches_naive(&"a".repeat(4096));
    }

    #[test]
    fn kana_sharing_e3_lead_byte_yields_zero_triggers() {
        // Every hiragana and katakana codepoint leads with 0xE2 or 0xE3, as
        // the 《》「」〔〕≪≫ triggers do, but none is a trigger. Both
        // scanners must skip them.
        assert_scan_matches_naive("あいうえおカキクケコこんにちは漢字");
        assert!(scan_offsets("あいうえおこんにちは").is_empty());
    }

    #[test]
    fn finds_triggers_amid_japanese_text() {
        // 漢《かん》字: 《 at byte 3, 》 at byte 12.
        assert_eq!(scan_offsets("漢《かん》字"), vec![3, 12]);
    }

    #[test]
    fn mixed_sample_finds_every_trigger() {
        let source = "漢《かん》字、※［＃ここまで］「終わり」";
        assert_scan_matches_naive(source);
        assert_eq!(scan_offsets(source).len(), 8, "sample has 8 triggers");
    }

    proptest! {
        #![proptest_config(default_config())]

        /// Agreement on the workhorse `aozora_fragment` distribution.
        #[test]
        fn scan_matches_naive_on_aozora_fragment(s in aozora_fragment(120)) {
            assert_scan_matches_naive(&s);
        }

        /// Pathological and unbalanced Aozora: the same agreement property
        /// over inputs the lex pipeline rejects. The scanner does not care
        /// about well-formedness, only about trigger-byte positions.
        #[test]
        fn scan_matches_naive_on_pathological_aozora(s in pathological_aozora(120)) {
            assert_scan_matches_naive(&s);
        }

        /// Unicode adversarial: combining marks, RTL overrides, PUA bytes.
        /// Neither scanner decodes; both walk the byte buffer.
        #[test]
        fn scan_matches_naive_on_unicode_adversarial(s in unicode_adversarial()) {
            assert_scan_matches_naive(&s);
        }
    }
}
