# Semantic Summary Comparison Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add parser-neutral semantic summaries for structured `ab-ir` and teach `ab-compare` to diff those summaries when AAT artifacts carry them.

**Architecture:** `ab-ir` owns semantic summary construction from typed IR blocks and projection warnings. `ab-compare` remains AAT-file oriented and reads optional `meta.semantic_summary`; when present, it hashes and diffs those summaries alongside existing AAT-derived semantic hashes. Existing AAT comparison behavior remains intact for artifacts without summaries.

**Tech Stack:** Rust 2024, `serde`, `serde_json`, existing Cargo workspace.

---

## Files

- Modify: `crates/ab-ir/Cargo.toml`
- Modify: `crates/ab-ir/src/lib.rs`
- Modify: `crates/ab-compare/src/aat_diff.rs`
- Modify: `crates/ab-compare/tests/integration.rs`

## Task 1: Build Semantic Summary From ab-ir

- [ ] **Step 1: Write failing tests**

Add `ab-ir` tests proving `semantic_summary(&blocks, &warnings)` produces matrix-keyed entries for:

- `ruby.basic`
- `gaiji.marker`
- `gaiji_ruby.inline_base`
- `projection.warning`

- [ ] **Step 2: Verify red**

Run:

```bash
cargo test -p ab-ir semantic_summary
```

Expected: fail because `semantic_summary` does not exist.

- [ ] **Step 3: Implement typed summary**

Add `SemanticSummary`, `SemanticSummaryNode`, and `semantic_summary`. Use source spans as optional fields for future adapters; current synthetic IR tests may leave spans absent. Serialize values with `serde`.

- [ ] **Step 4: Verify and commit**

Run:

```bash
cargo test -p ab-ir semantic_summary
git add crates/ab-ir/Cargo.toml crates/ab-ir/src/lib.rs
git commit -m "feat: add ab-ir semantic summaries"
```

## Task 2: Diff Semantic Summaries In ab-compare

- [ ] **Step 1: Write failing tests**

Add an `ab-compare` integration test with two AAT files that have identical blocks but different `meta.semantic_summary.syntax["ruby.basic"]` readings. Assert the AAT comparison reports one semantic summary hash difference.

- [ ] **Step 2: Verify red**

Run:

```bash
cargo test -p ab-compare semantic_summary
```

Expected: fail because `aat_diff` ignores `meta.semantic_summary`.

- [ ] **Step 3: Implement summary hashing**

Extend `AatRoot` to read optional `meta.semantic_summary`. Add `semantic_summary_hashes` to `AatSummary`, compare them in `semantic_hash_differences`, and expose counts under keys prefixed with `summary:`.

- [ ] **Step 4: Verify and commit**

Run:

```bash
cargo test -p ab-compare semantic_summary
git add crates/ab-compare/src/aat_diff.rs crates/ab-compare/tests/integration.rs
git commit -m "feat: diff parser semantic summaries"
```

## Task 3: Final Verification

- [ ] **Step 1: Run checks**

Run:

```bash
cargo test -p ab-ir -p ab-compare -p ab-index -p ab-check
cargo fmt --all -- --check
git status --short
```

Expected: all tests and formatter pass, worktree clean after commits.
