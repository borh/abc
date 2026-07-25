//! Authenticated classified-source capture, extracted verbatim from
//! `ab-aozora-aat` so the release-gated `ab-aozora` / `ab-aat-to-parser-ir`
//! binaries never link this module's four `abc/…` `include_bytes!` embeds.
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
