//! Streaming stages for Aozora Bunko notation.
//!
//! Sanitization normalizes line endings and notation; tokenization locates
//! triggers; pairing establishes nested delimiter extents; classification
//! creates nodes in the owned store. [`crate::lex`] drives these stages and
//! records classified spans in the normalized-text registry and source tables.
//!
//! Use [`crate::lex`] for complete parsing. Individual stages support pipeline
//! control and instrumentation.

pub use ab_aozora_spec::{
    BLOCK_CLOSE_SENTINEL, BLOCK_LEAF_SENTINEL, BLOCK_OPEN_SENTINEL, INLINE_SENTINEL, SLUGS,
    SlugEntry, SlugFamily, canonicalise_slug,
};

pub mod classify;
#[cfg(feature = "classify-instrument")]
pub mod instrumentation;
pub mod offset;
pub mod pair;
#[doc(hidden)]
pub mod sanitize;
pub mod token;
mod tokenize;
#[doc(hidden)]
pub mod trigger_scan;

pub use classify::{
    ClassifiedSpan, ClassifyStream, PlainProvenance, PlainSpan, SpanKind, classify,
};
pub use offset::{OffsetMap, offset_map};
pub use pair::{PairEvent, PairKind, PairStream, pair};
pub use sanitize::{SanitizeOutput, sanitize};
#[doc(hidden)]
pub use sanitize::{
    has_long_rule_line, isolate_decorative_rules, normalize_line_endings, rewrite_accent_spans,
};
pub use token::{Token, TriggerKind};
pub use tokenize::{Tokenizer, tokenize};

#[cfg(test)]
mod tests {
    //! Sentinel-constant invariants. Other crate-public surface is
    //! covered by per-stage tests and the lex-path tests in
    //! `aozora-pipeline`; this block keeps the structural invariants that
    //! every downstream consumer relies on (PUA range membership +
    //! pairwise distinctness) co-located with the re-exports.
    use super::*;

    #[test]
    fn sentinel_constants_are_in_pua_range() {
        for &c in &[
            INLINE_SENTINEL,
            BLOCK_LEAF_SENTINEL,
            BLOCK_OPEN_SENTINEL,
            BLOCK_CLOSE_SENTINEL,
        ] {
            let code = u32::from(c);
            assert!(
                (0xE000..=0xF8FF).contains(&code),
                "{c:?} ({code:#06X}) must lie in Unicode PUA"
            );
        }
    }

    #[test]
    fn sentinel_constants_are_distinct() {
        let sentinels = [
            INLINE_SENTINEL,
            BLOCK_LEAF_SENTINEL,
            BLOCK_OPEN_SENTINEL,
            BLOCK_CLOSE_SENTINEL,
        ];
        for (i, a) in sentinels.iter().enumerate() {
            for b in &sentinels[i + 1..] {
                assert_ne!(a, b, "sentinels must be pairwise distinct");
            }
        }
    }
}
