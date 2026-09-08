//! Forked from <https://github.com/P4suta/aozora>
//! at rev 1a4f864603970983719655aa4af4525958ac2d38 (independent fork).
//! Upstream crate: aozora-spec. License: MIT OR Apache-2.0 (see NOTICE).

//! Aozora Bunko notation specification crate.
//!
//! This crate is the source of truth for types shared across crates:
//!
//! - **PUA sentinel codepoints**: the four `U+E001..U+E004` markers the
//!   lexer injects into normalized text (see [`sentinels`]).
//! - **[`Span`]**: `(u32, u32)` byte-range over a UTF-8 source.
//! - **[`TriggerKind`]**: the set of Aozora notation marker characters
//!   (`｜《》［］＃※〔〕「」`) plus the const-PHF byte-sequence lookup
//!   table that maps a UTF-8 trigger byte sequence to its kind.
//! - **[`PairKind`]**: categories of balanced open/close delimiters.
//! - **[`Diagnostic`]**: non-fatal observations emitted by pipeline stages.
//!
//! ## Why a separate crate
//!
//! Pre-0.2 these types lived scattered between `aozora-syntax` (`Span`)
//! and `aozora-pipeline` (`Diagnostic`, sentinels, `TriggerKind`,
//! `PairKind`). The result: any crate that wanted a [`Diagnostic`] had
//! to depend on the full lexer, which transitively dragged in the
//! whole pipeline (sanitize → tokenize → pair → classify). The new
//! layered architecture requires `aozora-syntax`, `aozora-scan`,
//! `aozora-pipeline`, `aozora-render`, and `aozora` itself to all
//! reference these shared types without depending on the engine.
//!
//! Concretely, this crate has **no internal dependency** on any other
//! `aozora-*` crate, only on `miette`/`thiserror`. Every other crate
//! may depend on `aozora-spec` and re-export from it.

#![forbid(unsafe_code)]

pub mod diagnostic;
pub mod offset;
pub mod pair;
pub mod sentinels;
pub mod slugs;
pub mod span;
pub mod trigger;

pub use diagnostic::{
    Diagnostic, DiagnosticInfo, DiagnosticSource, InternalCheckCode, Severity, codes,
};
pub use offset::{NormalizedOffset, SourceOffset};
pub use pair::{PairKind, PairLink};
pub use sentinels::{
    ALL_SENTINELS, BLOCK_CLOSE_SENTINEL, BLOCK_LEAF_SENTINEL, BLOCK_OPEN_SENTINEL, INLINE_SENTINEL,
    Sentinel,
};
pub use slugs::{
    RENDER_SLUGS, RenderSlug, SLUGS, SlugEntry, SlugFamily, canonicalise_slug, roman_slug,
};
pub use span::Span;
pub use trigger::{TriggerKind, classify_trigger_bytes};
