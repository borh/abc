#![forbid(unsafe_code)]

mod analyze;
pub mod interval;
mod model;

pub use analyze::analyze_work;
pub use model::*;

pub const fn hegel_probe_value(value: u16) -> u16 {
    value
}
