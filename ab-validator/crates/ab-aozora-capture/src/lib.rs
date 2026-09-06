//! Authenticated classified-source capture. The release parser and converter
//! binaries do not link the retained research authority documents.
//! Only the rq adapters that actually run capture
//! (`ab-parser-rq-source-accountability`,
//! `ab-parser-rq-diagnostic-authorization`) depend on it.

pub mod classified_source;

pub use classified_source::{
    CaptureGeneration, PublishedCaptureGeneration, capture_generation_from_bytes,
    capture_generation_from_bytes_for_identity,
    capture_generation_from_bytes_for_identity_and_work, classified_source_ledger_from_bytes,
    verify_capture_generation,
};
