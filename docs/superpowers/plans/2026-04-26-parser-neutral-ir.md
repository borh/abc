# Parser-Neutral IR Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a parser-neutral IR crate and port `aozora-rs-adapter` AAT projection to it without changing adapter output.

**Architecture:** `ab-ir` owns parser-neutral document/block/inline values plus AAT and visible-text projection. Parser adapters map native parser output into `ab-ir`; comparison remains artifact/report-based and can later consume IR summaries. Regex-backed compatibility logic stays isolated in the adapter for this increment.

**Tech Stack:** Rust 2024, serde_json, existing adapter crates, cargo workspace.

---

## File Structure

- Create: `crates/ab-ir/Cargo.toml` - workspace crate manifest.
- Create: `crates/ab-ir/src/lib.rs` - IR types, AAT JSON projection, visible-text projection.
- Modify: `Cargo.toml` - add `crates/ab-ir` workspace member and dependency entry.
- Modify: `adapters/aozora-rs/Cargo.toml` - depend on `ab-ir` by path.
- Modify: `adapters/aozora-rs/src/aat.rs` - use `ab_ir::{Block, Inline, ProjectedText}` and remove local parser-neutral types/projection.

## Task 1: Add `ab-ir` Crate

- [ ] Write failing tests in `crates/ab-ir/src/lib.rs` for AAT projection and visible projection.
- [ ] Run `cargo test -p ab-ir` and confirm it fails because the crate is incomplete.
- [ ] Implement `Block`, `Inline`, `ProjectedText`, `blocks_to_aat_json`, and `visible_projection`.
- [ ] Run `cargo test -p ab-ir` and confirm pass.
- [ ] Commit `feat: add parser-neutral IR crate`.

## Task 2: Port `aozora-rs` Adapter To `ab-ir`

- [ ] Write/adjust adapter tests to continue asserting current AAT output shape.
- [ ] Replace local `AatBlock`, `AatInline`, `ProjectedText`, and `blocks_to_json` definitions with `ab-ir` types/functions.
- [ ] Keep regex fallback/supplement helpers private to `aozora-rs/src/aat.rs`.
- [ ] Run `cargo test --manifest-path adapters/aozora-rs/Cargo.toml` and confirm pass.
- [ ] Commit `refactor: project aozora-rs AAT through ab-ir`.

## Task 3: Verification

- [ ] Run `cargo fmt --check`.
- [ ] Run `cargo test --workspace`.
- [ ] Run `cargo test --manifest-path adapters/aozora-rs/Cargo.toml`.
- [ ] Run `cargo clippy --workspace --all-targets -- -D warnings`.
- [ ] Run `cargo clippy --manifest-path adapters/aozora-rs/Cargo.toml --all-targets -- -D warnings`.
- [ ] Commit verification fixes if needed.
