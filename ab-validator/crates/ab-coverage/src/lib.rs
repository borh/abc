//! Coverage matrix loader and schema validator for
//! `data/aozora-syntax-coverage.toml`. The canonical schema lives at
//! `data/aozora-syntax-coverage.schema.json` and is documented in
//! `docs/superpowers/specs/2026-04-28-syntax-coverage-methodology.md`.

pub mod adapter;
pub mod cache;
pub mod detectors;
pub mod matrix;
pub mod merge;
pub mod prevalence;
pub mod schema;
pub mod source_corpus;
pub mod source_inventory;

pub use matrix::{
    AdapterCell, CorpusPrevalence, CoverageBasis, CoverageMatrix, ParserCell, Recognition, Row,
    RowAatFidelity, RowStatus,
};
pub use schema::{RowError, SchemaValidator, ValidationOptions};
