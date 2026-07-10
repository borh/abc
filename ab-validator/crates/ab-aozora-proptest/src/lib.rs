//! Forked from <https://github.com/P4suta/aozora>
//! at rev 1a4f864603970983719655aa4af4525958ac2d38 (hard detach; ADR 0032).
//! Upstream crate: aozora-proptest. License: MIT OR Apache-2.0 (see NOTICE).

#![forbid(unsafe_code)]

//! Shared test utilities for the aozora workspace.
//!
//! This crate collects proptest [`Strategy`]s and [`ProptestConfig`]
//! defaults shared across the workspace's integration tests. It is
//! **not published** and is consumed only via `[dev-dependencies]` —
//! production code must not pull it in.
//!
//! [`Strategy`]: proptest::prelude::Strategy
//! [`ProptestConfig`]: proptest::prelude::ProptestConfig

pub mod config;
pub mod generators;
