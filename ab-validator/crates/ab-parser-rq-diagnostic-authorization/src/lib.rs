#![forbid(unsafe_code)]

//! Authenticates the raw diagnostic trust boundary and converts the closed
//! ABC policy into exact decoded-source intervals.

mod authorize;
mod model;

pub use authorize::{
    authorize, authorize_boundary, validate_diagnostic_capture, validate_gap_policy,
    validate_work_context,
};
pub use model::*;
