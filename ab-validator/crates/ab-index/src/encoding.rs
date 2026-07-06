//! Source-byte decoder re-exported from the shared `ab-encoding` crate.
//!
//! `ab-index` persists `DecodedSource` into parquet rows, so it enables the
//! `serde` feature on the shared type. See `ab-encoding` for the canonical
//! UTF-8-BOM / Windows-31J + SHA-256 decode pipeline.
pub use ab_encoding::{DecodedSource, decode_source_bytes, hex_sha256};
