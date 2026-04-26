# Semantic Summary Comparison Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add parser-neutral semantic summaries for structured `ab-ir` and teach `ab-compare` to diff those summaries when AAT artifacts carry them.

**Architecture:** `ab-ir` owns semantic summary construction from typed IR blocks and projection warnings. `ab-compare` remains AAT-file oriented and reads optional `meta.semantic_summary` as `serde_json::Value`, not as `ab-ir` types; this avoids a dependency from `ab-compare` to `ab-ir` while keeping the JSON contract explicit. Existing AAT comparison behavior remains intact for artifacts without summaries, and CLI JSON output grows only additive fields.

**Tech Stack:** Rust 2024, `serde`, `serde_json`, existing Cargo workspace.

---

## Files

- Modify: `crates/ab-ir/Cargo.toml`
- Modify: `crates/ab-ir/src/lib.rs`
- Create: `crates/ab-ir/src/semantic_summary.rs`
- Modify: `crates/ab-compare/src/aat_diff.rs`
- Modify: `crates/ab-compare/tests/integration.rs`

## Task 1: Build Semantic Summary From ab-ir

- [ ] **Step 1: Write failing tests**

Add `ab-ir` tests proving `semantic_summary(&blocks, &warnings)` produces matrix-keyed entries for:

- `ruby.basic`
- `gaiji.marker`
- `gaiji_ruby.inline_base`
- `projection.warning`

The summary JSON contract is:

```rust
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SemanticSummary {
    pub syntax: BTreeMap<String, Vec<SemanticSummaryNode>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SemanticSummaryNode {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source_span: Option<SourceSpan>,
    pub kind: String,
    pub value: serde_json::Value,
    pub provenance: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceSpan {
    pub start: usize,
    pub end: usize,
}
```

`SourceSpan` uses byte offsets into the decoded UTF-8 source text. Spans are
optional because current synthetic IR values do not carry source locations.
`provenance` is mandatory for a uniform JSON contract, but some rows are not
parser-originated; projection warnings should use `"projection"`.

`projection.warning` entries use `kind: "projection_warning"` and store the original `ProjectionWarning.syntax_id` inside `value.syntax_id`. This keeps the high-level bucket stable while preserving the specific warning ID.

- [ ] **Step 2: Verify red**

Run:

```bash
cargo test -p ab-ir semantic_summary
```

Expected: fail because `semantic_summary` does not exist.

- [ ] **Step 3: Implement typed summary**

Add `serde.workspace = true` to `crates/ab-ir/Cargo.toml`. Create `crates/ab-ir/src/semantic_summary.rs`, re-export it from `lib.rs`, and add rustdoc comments for the public summary types. Implement `semantic_summary(blocks: &[Block], warnings: &[ProjectionWarning]) -> SemanticSummary`. Current synthetic IR tests may leave `source_span` absent.

- [ ] **Step 4: Verify and commit**

Run:

```bash
cargo test -p ab-ir semantic_summary
git add crates/ab-ir/Cargo.toml crates/ab-ir/src/lib.rs crates/ab-ir/src/semantic_summary.rs
git commit -m "feat: add ab-ir semantic summaries"
```

## Task 2: Diff Semantic Summaries In ab-compare

- [ ] **Step 1: Write failing tests**

Add `ab-compare` integration tests for:

- two AAT files with identical blocks but different `meta.semantic_summary.syntax["ruby.basic"]` readings
- two AAT files without `meta.semantic_summary` still compare as they did before
- malformed `meta.semantic_summary` is ignored as absent, not fatal, because semantic summaries are an optional enhancement

- [ ] **Step 2: Verify red**

Run:

```bash
cargo test -p ab-compare semantic_summary
```

Expected: fail because `aat_diff` ignores `meta.semantic_summary`.

- [ ] **Step 3: Implement summary hashing**

Extend `AatRoot` to read `meta: Option<Value>`. Do not add an `ab-ir` dependency to `ab-compare`. Extract `meta.semantic_summary.syntax` as a JSON object if present; if it is absent or not an object, treat it as absent.

Hashing strategy:

- For each key under `meta.semantic_summary.syntax`, serialize that key's JSON value with `serde_json::to_vec`.
- Hash each value independently.
- Store hashes under `summary:{syntax_id}`, for example `summary:ruby.basic`.
- Do not merge summary-derived hashes into existing AAT-derived `semantic_hashes` or bucket logic. `semantic_hash_difference_counts` and `normalized_visible_difference_buckets` keep their current meaning and remain based on AAT-derived hashes only.

`serde_json::to_vec` is deterministic for a `serde_json::Value` in memory, but it is not a full semantic canonicalization layer. Spurious differences from null-vs-absent or semantically equivalent shapes are accepted in this increment; canonical summary normalization is future work.

Additive output fields:

```rust
pub struct AatCompareSummary {
    // existing fields remain unchanged
    pub semantic_summary_hash_difference_counts: BTreeMap<String, usize>,
}

pub struct AatStructuralDifference {
    // existing fields remain unchanged
    pub a_semantic_summary_hashes: BTreeMap<String, String>,
    pub b_semantic_summary_hashes: BTreeMap<String, String>,
    pub semantic_summary_hashes_differ: BTreeMap<String, bool>,
}
```

The `ab-compare` binary serializes `AatCompareSummary` directly, so these fields are an additive CLI JSON output change. Existing fields and bucket names remain unchanged.

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
cargo check -p ab-ir -p ab-compare
cargo test -p ab-ir -p ab-compare
cargo test -p ab-index -p ab-check
cargo fmt --all -- --check
git status --short
```

Expected: affected crate checks pass, regression tests for unchanged adjacent crates pass, formatter passes, and worktree is clean after commits.
