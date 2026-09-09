//! Forked from <https://github.com/P4suta/aozora>
//! at rev 1a4f864603970983719655aa4af4525958ac2d38 (independent fork).
//! Upstream crate: aozora-proptest. License: MIT OR Apache-2.0 (see NOTICE).
//!
//! Shared property-test inputs for the Aozora parser workspace.
//!
//! Two things live here: [`config::default_config`], the proptest settings
//! every property suite in the workspace runs under, and [`generators`], the
//! input strategies those suites draw from. Nothing here is production code.
//! The crate is `publish = false` and is reached only through
//! `[dev-dependencies]`, plus the optional `proptest` feature on
//! `ab-aozora-facade` that re-exports it for renderer authors writing
//! property tests against their own visitor implementations.
//!
//! Generators derive their Aozora vocabulary from `ab_aozora_spec` constants
//! rather than restating it. A trigger character added to the parser's
//! trigger set therefore starts appearing in generated input without anyone
//! remembering to update this crate, which is the failure this indirection
//! exists to prevent.

#![forbid(unsafe_code)]

pub mod config;
pub mod generators;
