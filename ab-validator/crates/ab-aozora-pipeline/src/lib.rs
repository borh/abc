//! Forked from <https://github.com/P4suta/aozora>
//! at rev 1a4f864603970983719655aa4af4525958ac2d38 (independent fork).
//! Upstream crate: aozora-pipeline. License: MIT OR Apache-2.0 (see NOTICE).

//! Aozora notation lex pipeline: owned-AST front door.
//!
//! Both the orchestrator and the per-stage pipeline impl live in
//! this single crate:
//!
//! - The orchestrator (the [`Pipeline`] state machine plus the [`lex`]
//!   entry in `fold`) drives the pipeline through its stages
//!   (sanitize → tokenize → pair → classify). The single public entry
//!   [`lex`] runs the whole thing and returns the result as an owned,
//!   lifetime-free [`LexOutput`] (`Send + Sync`): the classify
//!   stage builds the owned nodes directly into an
//!   `ab_aozora_syntax::ast::NodeStore` (a string interner plus flat
//!   content / segment pools addressed by `u32` handles); there is no
//!   arena.
//! - The stage implementations live under [`lexer`] (`lexer::sanitize`
//!   through `lexer::classify`). External consumers should reach for
//!   [`lex`] or the [`Pipeline`] state machine; the
//!   per-stage functions are exposed for benchmarks and the
//!   instrumentation feature.
//!
//! The trigger scan that opens the tokenize stage lives in
//! [`lexer::trigger_scan`]; `lexer::tokenize` is its only caller.
//!
//! # Observable equivalence
//!
//! [`lex`] is a pure function from source text to
//! [`LexOutput`] *as observed externally*, even though the
//! internal pipeline runs SIMD trigger scans over scratch buffers.
//! The determinism + sentinel-alignment proptests in
//! `tests/property_owned_output.rs` pin the contract.

#![forbid(unsafe_code)]

mod fold;
pub mod lexer;
pub mod pipeline;
pub mod text_variant;
mod transcribed_notes;

// Re-export the owned lex output + its source-node / node-ref surface so
// `lex`'s return type is nameable at the crate root (keeps intra-doc links
// resolvable under `-D warnings`) and downstream crates that depend only on
// `aozora-pipeline` (e.g. `aozora-cst`) can name the owned node types.
pub use ab_aozora_syntax::ast::{
    ClassifiedSourceDisposition, ClassifiedSourceEvidenceClass, ClassifiedSourceFact,
    ClassifiedSourceRole, ConstructId, LexOutput, NodeRef, SourceNode,
    canonicalize_classified_source_facts,
};
pub use fold::lex;
pub use pipeline::{Paired, Pipeline, Sanitized, Source, Tokenized};

/// Eagerly initialise every lazily-built parser table.
///
/// Forces the tokenize-stage trigger automaton and the classify-stage
/// annotation-classifier Aho-Corasick DFA, so the
/// first [`lex`] does not pay the one-time build cost on its
/// critical path. Idempotent and cheap to call repeatedly.
///
/// Lexing stays lazy by default; this is opt-in for latency-sensitive
/// front ends: the umbrella `ab_aozora_facade::prewarm` is the public entry point.
pub fn prewarm() {
    lexer::trigger_scan::prewarm();
    lexer::classify::prewarm();
}

/// Re-exports of the sanitize-stage decorative-rule isolator, surfaced
/// so downstream `aozora-render::serialize` can run the same idempotent
/// blank-line-injection pass on its output and converge to a parser
/// fixed point in one cycle. The helpers are otherwise pipeline
/// internals: keep the public surface narrow.
pub use lexer::sanitize::{has_long_rule_line, isolate_decorative_rules};

pub use ab_aozora_spec::{
    ALL_SENTINELS, BLOCK_CLOSE_SENTINEL, BLOCK_LEAF_SENTINEL, BLOCK_OPEN_SENTINEL, Diagnostic,
    INLINE_SENTINEL, PairKind, PairLink, SLUGS, Sentinel, SlugEntry, SlugFamily, Span, TriggerKind,
    canonicalise_slug, classify_trigger_bytes,
};

#[cfg(test)]
mod tests {
    use super::*;

    /// The trigger scan MUST yield the exact same byte offsets that the
    /// tokenize-stage tokeniser uses for its trigger positions. We
    /// cross-check at the [`LexOutput`] level: every PUA sentinel in
    /// `normalized` must correspond to a consumed source trigger.
    #[test]
    fn lex_produces_normalized_with_pua_sentinels_for_trigger_inputs() {
        let out = lex("｜青梅《おうめ》");
        // Exactly one inline sentinel for the ruby span.
        let inline_count = out
            .normalized
            .chars()
            .filter(|c| *c == INLINE_SENTINEL)
            .count();
        assert_eq!(inline_count, 1, "normalized: {:?}", out.normalized);
        assert_eq!(out.registry.count_kind(Sentinel::Inline), 1);
    }

    #[test]
    fn lex_passes_through_plain_text_unchanged() {
        let out = lex("hello, world");
        assert_eq!(out.normalized, "hello, world");
        assert!(out.registry.is_empty());
        assert!(out.diagnostics.is_empty());
    }

    #[test]
    fn lex_re_exports_sentinel_constants() {
        assert_eq!(INLINE_SENTINEL, '\u{E001}');
        assert_eq!(BLOCK_LEAF_SENTINEL, '\u{E002}');
        assert_eq!(BLOCK_OPEN_SENTINEL, '\u{E003}');
        assert_eq!(BLOCK_CLOSE_SENTINEL, '\u{E004}');
    }

    #[test]
    fn lex_handles_empty_input() {
        let out = lex("");
        assert!(out.normalized.is_empty());
        assert!(out.registry.is_empty());
        assert!(out.diagnostics.is_empty());
    }

    #[test]
    fn lex_emits_diagnostics_for_accent_decomposition() {
        let out = lex("abc〔cafe'〕def");
        assert!(
            out.diagnostics
                .iter()
                .any(|d| matches!(d, Diagnostic::AccentDecompositionApplied { .. })),
            "expected AccentDecompositionApplied, got {:?}",
            out.diagnostics
        );
    }

    #[test]
    fn lex_preserves_sanitized_len_for_segment_merge() {
        let out = lex("plain text");
        assert_eq!(usize::try_from(out.sanitized_len), Ok("plain text".len()));
    }
}
