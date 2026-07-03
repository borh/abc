//! Source-byte decoder re-exported from the shared `ab-encoding` crate.
//!
//! Historically `ab-check` carried its own copy of the UTF-8-BOM / Windows-31J
//! decoder with SHA-256; that impl was duplicated byte-for-byte in `ab-index`,
//! and `ab-coverage` had drifted (see `docs/handoffs/crates-optimization-audit.md`
//! §3.3). The canonical impl now lives in `ab-encoding`; this module re-exports
//! it so existing `ab_check::encoding::` call sites are unchanged.
pub use ab_encoding::{DecodedSource, decode_source_bytes, hex_sha256};
