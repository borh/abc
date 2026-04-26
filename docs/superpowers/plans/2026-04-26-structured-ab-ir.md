# Structured ab-ir Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Migrate `ab-ir` to structured ruby bases and typed gaiji references while preserving current AAT JSON output for representable cases.

**Architecture:** Keep the migration contained to `crates/ab-ir/src/lib.rs`. Public convenience constructors keep their existing names where possible, but `Inline::Ruby` changes to `base: Vec<Inline>` and `Inline::Gaiji` is replaced by `Inline::GaijiRef`. AAT projection flattens structured bases into the current AAT `ruby.base: string` where possible and exposes projection warnings for cases AAT cannot represent.

**Tech Stack:** Rust 2024, `serde_json`, existing Cargo workspace.

---

## Files

- Modify: `crates/ab-ir/src/lib.rs`

## Task 1: Add Typed Gaiji Alongside Current Behavior

- [ ] **Step 1: Write failing tests**

Add tests in `crates/ab-ir/src/lib.rs` for `GaijiKind`, `DakutenMark`, `GaijiRef`, `Inline::gaiji_ref`, and `Inline::gaiji` compatibility.

- [ ] **Step 2: Verify red**

Run:

```bash
cargo test -p ab-ir gaiji_ref
```

Expected: fail because the typed gaiji structs do not exist.

- [ ] **Step 3: Implement typed gaiji**

Add `GaijiKind`, `DakutenMark`, `GaijiRef`, and `Inline::GaijiRef`. Keep `Inline::gaiji(...)` as a compatibility constructor returning `Inline::GaijiRef` with `GaijiKind::Unknown`.

- [ ] **Step 4: Verify green and commit**

Run:

```bash
cargo test -p ab-ir gaiji_ref
git add crates/ab-ir/src/lib.rs
git commit -m "feat: add typed gaiji references to ab-ir"
```

## Task 2: Migrate Ruby Base To Structured Inline Nodes

- [ ] **Step 1: Write failing tests**

Add tests that construct ruby with a structured gaiji base and assert visible projection uses resolved gaiji text. Keep the existing `Inline::ruby("吾輩", "わがはい")` AAT JSON test unchanged.

- [ ] **Step 2: Verify red**

Run:

```bash
cargo test -p ab-ir structured_ruby
```

Expected: fail because `ruby_with_base` and `RubyPlacement` do not exist.

- [ ] **Step 3: Implement structured ruby**

Change `Inline::Ruby` to `base: Vec<Inline>, reading: String, placement: RubyPlacement, provenance: Provenance`. Add `RubyPlacement::{Right, Left}`, `Inline::ruby_with_base`, and keep `Inline::ruby` as a text-base compatibility constructor.

- [ ] **Step 4: Verify green and commit**

Run:

```bash
cargo test -p ab-ir
git add crates/ab-ir/src/lib.rs
git commit -m "feat: support structured ruby bases in ab-ir"
```

## Task 3: Preserve AAT Compatibility And Warn On Loss

- [ ] **Step 1: Write failing tests**

Add tests for:

- simple ruby still projects to `{ "kind": "ruby", "base": "吾輩", "reading": "わがはい" }`
- resolved gaiji ruby base projects to `ruby.base` equal to the resolved character
- unresolved gaiji ruby base produces a gaiji node instead of an orphan ruby and returns a projection warning with syntax ID `gaiji_ruby.unresolved_base`

- [ ] **Step 2: Verify red**

Run:

```bash
cargo test -p ab-ir aat_projection
```

Expected: fail because warning-aware projection does not exist.

- [ ] **Step 3: Implement warning-aware AAT projection**

Add `AatProjection` and `ProjectionWarning`. Make `blocks_to_aat_projection` return both projected blocks and warnings. Keep `blocks_to_aat_json` as a compatibility wrapper returning only blocks.

- [ ] **Step 4: Verify green and commit**

Run:

```bash
cargo test -p ab-ir
git add crates/ab-ir/src/lib.rs
git commit -m "feat: expose AAT projection warnings"
```

## Task 4: Final Verification

- [ ] **Step 1: Run workspace checks**

Run:

```bash
cargo test -p ab-ir -p ab-index -p ab-check
cargo fmt --all -- --check
git status --short
```

Expected: all tests and formatter pass, worktree clean after commits.
