#![forbid(unsafe_code)]

//! The production analyzer owns diagnostic capture; callers cannot inject it.
//!
//! ```compile_fail
//! use ab_parser_rq_source_accountability::analyze_work_with_diagnostics;
//! ```

mod aggregate;
mod analyze;
mod index;
pub mod interval;
mod model;
mod reconcile;

pub use aggregate::aggregate;
pub use analyze::analyze_work;
pub use index::{analyze_corpus, canonical_json, qualification_identity_ref};
pub use interval::Interval;
pub use model::*;
pub use reconcile::{Reconciliation, reconcile};

pub const fn hegel_probe_value(value: u16) -> u16 {
    value
}
