//! Forked from <https://github.com/P4suta/aozora>
//! at rev 1a4f864603970983719655aa4af4525958ac2d38 (independent fork).
//! Upstream crate: aozora. License: MIT OR Apache-2.0 (see NOTICE).

//! `aozora` — the public meta crate.
//!
//! Single front door for parsing Aozora Bunko notation. Downstream
//! consumers should depend on this crate alone; everything they need
//! is re-exported through this surface or accessed via [`Document`]
//! and [`Tree`].
//!
//! ```
//! use ab_aozora_facade::Document;
//!
//! let doc = Document::new("｜青空《あおぞら》文庫");
//! let tree = doc.parse();
//! let html = tree.to_html();
//! assert!(html.contains("青空")); // the ruby base survives into the HTML
//! ```
//!
//! Tunable parses go through the builder chain:
//!
//! ```
//! use ab_aozora_facade::{Document, DiagnosticPolicy};
//!
//! let doc = Document::options()
//!     .diagnostic_policy(DiagnosticPolicy::DropInternal)
//!     .build("｜青梅《おうめ》");
//! let tree = doc.parse();
//! assert!(!tree.to_source().is_empty());
//! ```
//!
//! # Architecture
//!
//! [`Document`] owns the source buffer plus a `Copy` diagnostic
//! policy. [`Document::parse`] returns a [`Tree`] whose `&self`
//! lifetime tracks only that source borrow — the AST data itself is
//! owned, lifetime-free, and `Send + Sync` (an `LexOutput` backed
//! by a flat `NodeStore`: a string interner plus content / segment
//! pools addressed by `u32` handles). The interner deduplicates
//! repeated string content; dropping the tree frees the store in one
//! step, with no per-node `Drop`.
//!
//! The component crates are available through [`pipeline`], [`syntax`],
//! [`render`], and [`encoding`]. Wire projections require the corresponding
//! feature flags.
//!
//! The following README examples also run as doctests.
#![doc = include_str!("../README.md")]
#![forbid(unsafe_code)]
// Emit "Available on crate feature `…`" badges on docs.rs (and the
// GitHub-Pages build when RUSTDOCFLAGS carries `--cfg docsrs`). Inert on
// stable — `docsrs` is unset, so this never trips `feature(doc_cfg)`.
#![cfg_attr(docsrs, feature(doc_cfg))]

use core::ops::Range;

/// Crate version, re-exported for identity strings (ab-aat's
/// `adapter_version` embeds it as the `facade` coordinate).
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

pub use ab_aozora_pipeline::{LexOutput, NodeRef, SourceNode, lex};
pub use ab_aozora_spec::{
    ALL_SENTINELS, BLOCK_CLOSE_SENTINEL, BLOCK_LEAF_SENTINEL, BLOCK_OPEN_SENTINEL, Diagnostic,
    DiagnosticInfo, DiagnosticSource, INLINE_SENTINEL, InternalCheckCode, NormalizedOffset,
    PairKind, PairLink, RENDER_SLUGS, RenderSlug, SLUGS, Sentinel, Severity, SlugEntry, SlugFamily,
    SourceOffset, Span, TriggerKind, canonicalise_slug, codes, roman_slug,
};
/// Owned-AST node types editor surfaces match against (LSP inlay hints, hover,
/// completion, code actions, semantic tokens). Re-exported so external
/// consumers don't have to depend on `aozora-syntax` directly — `aozora` is the
/// single editor-facing front door.
pub use ab_aozora_syntax::{
    BlockStyles, BoutenKind, BoutenPosition, CaptionScope, ColumnBlock, ColumnCount, DirectiveKind,
    EnclosureKind, FontShift, Format, ForwardAttr, ForwardOrigin, HeadingKind, HeadingStyle,
    IndentBlock, IndentLayout, Kumi, LineFormat, LineWidth, NodeKind, RegionClose, RegionFormat,
    RubySide, SectionKind,
    ast::{Content, Node, NodeStore},
};

mod diagnostics_text;
mod document;
mod incremental;
mod splice;

#[cfg(feature = "entries")]
#[cfg_attr(docsrs, doc(cfg(feature = "entries")))]
pub mod json;

/// Plain-text diagnostic rendering (`miette`-free, every target).
pub use diagnostics_text::diagnostics_text;
pub use document::{DiagnosticPolicy, Document, ParseOptions, Tree};
/// Source-region ownership and minimal-diff source splicing.
pub use splice::{CoupledKind, Coupling, Region, RegionRole, SpliceError, SpliceSafety};

pub use incremental::{DiagBaseRef, DiagSplice, PieceSeq, SanitizedSrc};

/// Incrementally reparses diagnostics without rebuilding the full tree.
///
/// This is the LSP's per-keystroke hot path. It splices the maintained
/// [`PieceSeq`] (the next edit's
/// region-find base, from which the LSP flattens this edit's diagnostics) from
/// the store-free [`DiagBaseRef`] of the prior parse, **without building an
/// [`LexOutput`]** — no normalized/sanitized string rebuild, no store
/// clone/graft, no registry or container-pairs rebuild, and no whole-table
/// re-materialization. Cost is `O(region + #pieces)`: the maintained sequence is
/// spliced (prefix/suffix pieces shared by `Arc`), not rebuilt, versus a full
/// parse's `O(doc)`.
///
/// Returns [`DiagSplice`], or `None` for any edit whose locality it cannot prove
/// from the cached tables (the caller then full-parses, trivially correct); that
/// the edit truly changes only bytes inside `edit_old` is a caller precondition,
/// checked in debug rather than gated at runtime. Its diagnostics are pinned
/// byte-identical to a full parse by the `corpus_incremental_merge` differential
/// gate. The full tree is materialised lazily, only when a structural request
/// (rename) needs it.
///
/// This is exposed for the in-workspace LSP consumer only; its shape may change
/// without a major version bump until v0.5.0.
#[must_use]
#[allow(
    clippy::needless_pass_by_value,
    reason = "the lightweight DiagBaseRef (a sanitized source plus one PieceSeq borrow) is taken by value so the in-workspace LSP caller passes its temporary `DiagBaseRef { .. }` literal unchanged; it is forwarded by reference to the generic engine"
)]
pub fn reparse_incremental_diagnostics_only(
    base: DiagBaseRef<'_>,
    new_sanitized: &str,
    edit_old: Range<usize>,
) -> Option<DiagSplice> {
    incremental::reparse_incremental_diagnostics_only(&base, &new_sanitized, edit_old)
}

/// Runs the diagnostics-only incremental path over a generic byte source.
///
/// This variant of [`reparse_incremental_diagnostics_only`] accepts any
/// [`SanitizedSrc`] byte source `S`
/// rather than the `&str`-backed [`DiagBaseRef`]. This is the entry the
/// in-workspace LSP routes a `ropey`-backed sanitized buffer through, so it can
/// splice the rope incrementally instead of flattening it to a `String` per
/// edit; the `&str` [`reparse_incremental_diagnostics_only`] is retained for the
/// `corpus_incremental_merge` differential gate and the existing callers.
///
/// Same contract and fallbacks as [`reparse_incremental_diagnostics_only`]:
/// returns [`DiagSplice`], or `None` for any edit whose locality it cannot prove
/// from the cached tables (the caller then full-parses, trivially correct); that
/// the edit truly changes only bytes inside `edit_old` is a caller precondition,
/// checked in debug rather than gated at runtime. Like that function it is
/// exposed for the in-workspace LSP consumer only; its shape may change without a
/// major version bump until v0.5.0.
///
/// The base is taken by reference (not by value like the `&str` entry) because a
/// rope source holds a cursor and is not `Copy`; callers pass `&DiagBaseRef { .. }`.
#[must_use]
pub fn reparse_incremental_diagnostics_only_in<S: SanitizedSrc>(
    base: &DiagBaseRef<'_, S>,
    new_sanitized: &S,
    edit_old: Range<usize>,
) -> Option<DiagSplice> {
    incremental::reparse_incremental_diagnostics_only(base, new_sanitized, edit_old)
}

/// Eagerly initialise the parser's process-global lazy tables.
///
/// The *first* [`Document::parse`] then does not pay the one-time build
/// cost on its critical path.
///
/// Parsing is lazy by default: a consumer that never parses — or parses
/// no annotations — pays nothing. `prewarm` is **opt-in**. Call it once,
/// early, from a latency-sensitive front end (e.g. a WASM editor warming
/// the parser before the first keystroke). It is idempotent and
/// thread-safe; redundant calls are effectively free.
///
/// It warms the SIMD trigger-scan backend selection (tokenize stage) and
/// the annotation-classifier Aho-Corasick DFA (classify stage) — the latter is the
/// bulk of the cost (~150 microseconds; the `aozora-pipeline` `boot`
/// bench measures it).
///
/// ```
/// ab_aozora_facade::prewarm();
/// let doc = ab_aozora_facade::Document::new("｜青梅《おうめ》");
/// let _ = doc.parse().to_html();
/// ```
pub fn prewarm() {
    ab_aozora_pipeline::prewarm();
}

/// Re-export of [`ab_aozora_pipeline`] under a stable name.
///
/// Editor integrations that want per-phase access
/// (`pipeline::lexer::*` for the phase functions, `pipeline::Pipeline`
/// for the type-state machine) reach through this module so the
/// wider workspace can keep `aozora` as the single front door. The
/// `aozora-pipeline` crate is published in its own right, but is
/// normally reached through this re-export.
pub mod pipeline {
    pub use ab_aozora_pipeline::*;
}

/// Re-export of [`ab_aozora_syntax`] — owned AST node types, the
/// `NodeStore`, and the string interner.
///
/// External callers normally reach through [`Document`] /
/// [`Tree`] for the parsed-AST surface; this module exposes
/// the underlying types when they need to construct nodes directly
/// (custom renderers, owned-tree transforms).
pub mod syntax {
    pub use ab_aozora_syntax::*;
}

/// Re-export of [`ab_aozora_render`] — owned-AST HTML / source emitters.
///
/// `Tree::to_html` / `Tree::to_source` cover the common cases; custom
/// downstream renderers (EPUB, plain text, LaTeX, …) walk the owned
/// `LexOutput` (its `source_nodes` + `NodeStore`) and can reuse the
/// shared byte-spelling helpers re-exported through this module.
pub mod render {
    pub use ab_aozora_render::*;
}

/// Re-export of [`ab_aozora_encoding`] — `Shift_JIS` decoding and gaiji
/// resolution.
///
/// The sanitize stage of the lex pipeline runs encoding detection
/// first; callers that want to drive encoding without parsing can reach
/// through this module.
pub mod encoding {
    pub use ab_aozora_encoding::*;
}

/// Aozora-shaped `proptest` strategies.
///
/// Downstream renderer / visitor authors writing their own property
/// tests reach through this module instead of pulling
/// `aozora-proptest` directly. Enabled by the `proptest` Cargo
/// feature on the `aozora` crate; both `ab_aozora_facade::proptest::*` and
/// the `proptest` crate itself are then in scope for the consumer.
///
/// The generators here cover the same shapes the workspace's
/// `tests/property_*` suites rely on, so any regression noticed
/// inside the parser also surfaces inside the consumer's test
/// harness.
#[cfg(feature = "proptest")]
#[cfg_attr(docsrs, doc(cfg(feature = "proptest")))]
pub mod proptest {
    pub use ab_aozora_proptest::*;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn document_parse_returns_a_tree() {
        let doc = Document::new("hello, world");
        let tree = doc.parse();
        // Plain text round-trips intact.
        assert_eq!(tree.to_source(), "hello, world");
    }

    #[test]
    fn document_parse_handles_ruby() {
        let doc = Document::new("｜青梅《おうめ》");
        let tree = doc.parse();
        // Canonical right-side ruby is the bare form — the redundant `｜`
        // (all-kanji base at line start) is dropped;
        // `sanitized` preserves the author's `｜`.
        assert_eq!(tree.to_source(), "｜青梅《おうめ》");
        assert_eq!(tree.sanitized(), "｜青梅《おうめ》");
    }

    #[test]
    fn document_to_html_renders_plain_text() {
        let doc = Document::new("hello");
        let tree = doc.parse();
        let html = tree.to_html();
        assert!(html.contains("hello"), "html: {html}");
    }
}
