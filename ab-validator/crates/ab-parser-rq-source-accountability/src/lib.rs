#![forbid(unsafe_code)]

//! The production analyzer owns diagnostic capture; callers cannot inject it.
//!
//! ```compile_fail
//! use ab_parser_rq_source_accountability::analyze_work_with_diagnostics;
//! ```

mod analyze;
mod index;
pub mod interval;
mod model;
mod recognition;
mod recognition_aggregate;
mod recognition_corpus;
mod reconcile;

pub use analyze::analyze_work;
pub use index::{analyze_corpus, canonical_json, qualification_identity_ref};
pub use interval::Interval;
pub use model::*;
pub use recognition::{
    RecognitionAnalysis, RecognitionBlobRef, RecognitionInput, RecognitionInterval,
    RecognitionRegions, RecognitionStatus, RecognitionWorkRecord, analyze_recognition,
};
pub use recognition_aggregate::{
    RecognitionAggregate, RecognitionWorkCompleteness, RecognitionWorkInterval,
    aggregate_recognition,
};
pub use recognition_corpus::{
    RecognitionCorpusInput, RecognitionGenerationEntry, RecognitionGenerationIndex,
    RecognitionIndex, RecognitionRecordEntry, analyze_recognition_corpus,
    rfc8785_safe_integer_json,
};
pub use reconcile::{Reconciliation, reconcile};

pub const fn hegel_probe_value(value: u16) -> u16 {
    value
}
