//! Hydrates a `summarize-warehouse-interesting` ranking artifact into a
//! self-contained example bundle (examples.md + examples.json). Pure
//! post-processor: reads the artifact, the warehouse run dir, the AAT files
//! referenced by `sources.aat_path`, and (optionally) an ABC catalog export;
//! never modifies any input. Spec:
//! docs/superpowers/specs/2026-07-10-hydrate-interesting-examples-design.md

use std::path::PathBuf;

pub mod metadata;
pub mod source_context;
pub mod tables;

/// Options for [`run_hydrate_interesting`].
#[derive(Debug, Clone)]
pub struct HydrateOptions {
    /// Ranking artifact (JSON format from `summarize-warehouse-interesting`).
    pub interesting: PathBuf,
    /// Warehouse run directory (sources/morphemes/… parquet).
    pub run_dir: PathBuf,
    /// Output directory; receives `examples.md` and `examples.json`.
    pub output_dir: PathBuf,
    /// ABC catalog export with `works/` and `persons/` (author names).
    pub abc_catalog: Option<PathBuf>,
    /// Chars of context on each side of the region (default 40).
    pub context_chars: usize,
    /// Hydrate only the first N ranked rows (anomalies are always hydrated).
    pub limit: Option<usize>,
    /// Overwrite existing outputs.
    pub force: bool,
    /// Provenance timestamp, filled by the caller (CLI: now; tests: constant).
    pub built_at_utc: String,
}
