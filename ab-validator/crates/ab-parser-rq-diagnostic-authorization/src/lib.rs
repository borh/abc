#![forbid(unsafe_code)]

//! Authenticates the raw diagnostic trust boundary and converts the closed
//! ABC policy into exact decoded-source intervals.

mod aggregate;
mod authorize;
mod model;

pub use aggregate::{aggregate_gap_partitions, derive_gap_partition};
pub use authorize::{
    authorize, authorize_boundary, validate_diagnostic_capture, validate_gap_policy,
    validate_work_context,
};
pub use model::*;
