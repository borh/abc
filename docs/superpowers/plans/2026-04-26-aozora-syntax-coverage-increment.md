# Aozora Syntax Coverage Increment Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add the first executable syntax coverage matrix increment and rename the current source visible-text logic as an explicitly lossy source projection.

**Architecture:** Keep this increment small: `ab-index` parses and validates the syntax matrix against existing feature keys, while `ab-check` exposes the current source-side visible text behavior as `source_projection::comparison_lossy_body`. The existing validation report property IDs remain stable.

**Tech Stack:** Rust 2024, `serde`, `toml`, `regex`, existing Cargo workspace.

---

## Files

- Create: `data/aozora-syntax-coverage.toml`
- Create: `crates/ab-index/src/syntax_coverage.rs`
- Modify: `crates/ab-index/src/lib.rs`
- Modify: `crates/ab-index/tests/integration.rs`
- Create: `crates/ab-check/src/source_projection.rs`
- Modify: `crates/ab-check/src/lib.rs`
- Modify: `crates/ab-check/src/properties.rs`
- Modify: `crates/ab-check/benches/check_properties.rs`

## Task 1: Syntax Coverage Matrix Artifact

- [ ] **Step 1: Write the failing matrix parser tests**

Add tests in `crates/ab-index/tests/integration.rs` that load `data/aozora-syntax-coverage.toml`, assert the priority 1 syntax IDs exist, and assert all declared `feature_keys` exist in `data/feature-patterns.toml`.

- [ ] **Step 2: Run test to verify it fails**

Run:

```bash
cargo test -p ab-index syntax_coverage
```

Expected: fail because `ab_index::syntax_coverage` and the matrix file do not exist.

- [ ] **Step 3: Add `crates/ab-index/src/syntax_coverage.rs`**

Implement `SyntaxCoverage::from_toml`, `SyntaxRow`, `SyntaxStatus`, and helpers to expose priority rows and feature keys.

- [ ] **Step 4: Add `data/aozora-syntax-coverage.toml`**

Add priority 1 rows for ruby, gaiji, gaiji+ruby, headings, emphasis, indentation, warichu, image/caption, and page/line breaks. Include `feature_keys` mapping to existing `data/feature-patterns.toml` keys.

- [ ] **Step 5: Run test to verify it passes**

Run:

```bash
cargo test -p ab-index syntax_coverage
```

Expected: pass.

- [ ] **Step 6: Commit**

```bash
git add data/aozora-syntax-coverage.toml crates/ab-index/src/syntax_coverage.rs crates/ab-index/src/lib.rs crates/ab-index/tests/integration.rs
git commit -m "feat: add Aozora syntax coverage matrix"
```

## Task 2: Explicit Lossy Source Projection

- [ ] **Step 1: Write failing source projection tests**

Add tests for `source_projection::comparison_lossy_body` preserving current behavior:

```rust
assert_eq!(comparison_lossy_body("二二※［＃小書き片仮名ン、237-11］が四"), "二二が四");
assert_eq!(comparison_lossy_body("――『｜あのひとにとって、わたし《ルビ》はなんだろう？」"), "――『あのひとにとって、わたしはなんだろう？」");
assert_eq!(comparison_lossy_body("ことを、※［＃「口＋愛」、第3水準1-15-23］《おくび》にも"), "ことを、にも");
```

- [ ] **Step 2: Run test to verify it fails**

Run:

```bash
cargo test -p ab-check source_projection
```

Expected: fail because `ab_check::source_projection` does not exist.

- [ ] **Step 3: Implement `source_projection::comparison_lossy_body`**

Move the current `source_visible_text` regex pipeline into `crates/ab-check/src/source_projection.rs`. Keep `properties::source_visible_text` as a compatibility wrapper marked deprecated in prose comments, not with a Rust attribute yet, because the benchmark still imports it.

- [ ] **Step 4: Update callers**

Use `source_projection::comparison_lossy_body` in `VisibleTextBodyOrder` and the benchmark. Keep report property ID `visible_text_body_order`.

- [ ] **Step 5: Run test to verify it passes**

Run:

```bash
cargo test -p ab-check source_projection
```

Expected: pass.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-check/src/source_projection.rs crates/ab-check/src/lib.rs crates/ab-check/src/properties.rs crates/ab-check/benches/check_properties.rs
git commit -m "refactor: name lossy source projection"
```

## Task 3: Verification

- [ ] **Step 1: Run focused workspace tests**

Run:

```bash
cargo test -p ab-index -p ab-check
```

Expected: pass.

- [ ] **Step 2: Run formatting and status checks**

Run:

```bash
cargo fmt --all -- --check
git status --short
```

Expected: formatter passes and worktree is clean after commits.
