---
plan_id: "2026-04-26-parser-neutral-ir"
status: done
started: 2026-04-27
next_update: 2026-05-04
owner: unassigned
target_prerequisites: []
---

# Parser-Neutral IR Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [x]`) syntax for tracking.

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

- [x] Write failing tests in `crates/ab-ir/src/lib.rs` for AAT projection and visible projection.
- [x] Run `cargo test -p ab-ir` and confirm it fails because the crate is incomplete.
- [x] Implement `Block`, `Inline`, `ProjectedText`, `blocks_to_aat_json`, and `visible_projection`.
- [x] Run `cargo test -p ab-ir` and confirm pass.
- [x] Commit `feat: add parser-neutral IR crate`.

## Task 2: Port `aozora-rs` Adapter To `ab-ir`

- [x] Write/adjust adapter tests to continue asserting current AAT output shape.
- [x] Replace local `AatBlock`, `AatInline`, `ProjectedText`, and `blocks_to_json` definitions with `ab-ir` types/functions.
- [x] Keep regex fallback/supplement helpers private to `aozora-rs/src/aat.rs`.
- [x] Run `cargo test --manifest-path adapters/aozora-rs/Cargo.toml` and confirm pass.
- [x] Commit `refactor: project aozora-rs AAT through ab-ir`.

## Task 3: Verification

- [x] Run `cargo fmt --check`.
- [x] Run `cargo test --workspace`.
- [x] Run `cargo test --manifest-path adapters/aozora-rs/Cargo.toml`.
- [x] Run `cargo clippy --workspace --all-targets -- -D warnings`.
- [x] Run `cargo clippy --manifest-path adapters/aozora-rs/Cargo.toml --all-targets -- -D warnings`.
- [x] Commit verification fixes if needed.
