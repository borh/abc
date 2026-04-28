# Diff Utilities Extraction Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extract `FrequencyTable`, `FirstDifference`, and hashing helpers from `ab-compare` into a new `ab-diff-utils` crate; add coverage-mismatch tracking to `ab-compare`.

**Architecture:** New leaf-level crate `ab-diff-utils` with three modules (`hashing`, `first_diff`, `frequency`). `ab-compare` gains a dependency on it, replaces private helpers with re-imports, adds `Option<bool>` coverage fields to `AatSummary`, a `coverage_only_difference_count` counter, and a `CoverageMismatchTriage` section in the triage report.

**Tech Stack:** Rust edition 2024, serde, serde_json, sha2 (all already in workspace).

---

## File Structure

```
crates/ab-diff-utils/            (NEW)
├── Cargo.toml
└── src/
    ├── lib.rs
    ├── hashing.rs
    ├── first_diff.rs
    └── frequency.rs

crates/ab-compare/               (MODIFIED)
├── Cargo.toml                   (add ab-diff-utils dep)
├── src/
│   ├── lib.rs                   (unchanged)
│   ├── main.rs                  (unchanged)
│   ├── metrics.rs               (unchanged)
│   ├── aat_diff.rs              (remove hashing/first_diff, extend AatSummary, add CoverageDelta)
│   └── triage.rs                (replace TriageBucket/push_bucket, add CoverageMismatchTriage)
└── tests/
    └── integration.rs           (add 3 coverage tests)

Cargo.toml                       (workspace: add member + dep)
```

---

### Task 1: Create `ab-diff-utils` Crate Scaffolding

**Files:**
- Create: `crates/ab-diff-utils/Cargo.toml`
- Create: `crates/ab-diff-utils/src/lib.rs`

- [ ] **Step 1: Write `Cargo.toml`**

```toml
[package]
name = "ab-diff-utils"
version.workspace = true
edition.workspace = true
license.workspace = true

[dependencies]
anyhow.workspace = true
serde.workspace = true
serde_json.workspace = true
sha2.workspace = true
```

- [ ] **Step 2: Write `lib.rs`**

```rust
pub mod first_diff;
pub mod frequency;
pub mod hashing;

pub use first_diff::{FirstDifference, first_difference};
pub use frequency::{FrequencyEntry, FrequencyTable, DEFAULT_MAX_EXAMPLES};
pub use hashing::{hash_bytes, hash_json, hash_string_sequence};

- [ ] **Step 3: Commit**

```bash
git add crates/ab-diff-utils/Cargo.toml crates/ab-diff-utils/src/lib.rs
git commit -m "feat(ab-diff-utils): create crate scaffolding"
```

---

### Task 2: Implement `hashing` Module

**Files:**
- Create: `crates/ab-diff-utils/src/hashing.rs`

- [ ] **Step 1: Write `hashing.rs` with tests**

```rust
use serde_json::Value;
use sha2::{Digest, Sha256};

/// Returns `"sha256:{hex}"` for the given bytes.
pub fn hash_bytes(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    format!("sha256:{:x}", hasher.finalize())
}

/// Serializes `value` to canonical JSON and returns its SHA256 hash.
pub fn hash_json(value: &Value) -> anyhow::Result<String> {
    let bytes = serde_json::to_vec(value)?;
    Ok(hash_bytes(&bytes))
}

/// Hashes a sequence of strings using length-prefixing.
/// Each string is prefixed with its 4-byte little-endian length (u32) before
/// hashing, so `["a", "b"]` ≠ `["ab"]` and strings containing null bytes
/// are unambiguous.
pub fn hash_string_sequence(values: &[String]) -> String {
    let mut hasher = Sha256::new();
    for value in values {
        let len = value.len() as u32;
        hasher.update(&len.to_le_bytes());
        hasher.update(value.as_bytes());
    }
    format!("sha256:{:x}", hasher.finalize())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hash_bytes_is_deterministic() {
        assert_eq!(hash_bytes(b"hello"), hash_bytes(b"hello"));
        assert_ne!(hash_bytes(b"hello"), hash_bytes(b"world"));
    }

    #[test]
    fn hash_json_is_deterministic() {
        let v = serde_json::json!({"a": 1});
        assert_eq!(hash_json(&v).unwrap(), hash_json(&v).unwrap());
    }

    #[test]
    fn hash_string_sequence_uses_length_prefix_not_null_delimiter() {
        let a = hash_string_sequence(&["a".into(), "b".into()]);
        let b = hash_string_sequence(&["ab".into()]);
        assert_ne!(a, b);
        // strings containing null bytes are unambiguous with length-prefixing
        let c = hash_string_sequence(&["a\0b".into()]);
        let d = hash_string_sequence(&["a".into(), "b".into()]);
        assert_ne!(c, d);
        let e = hash_string_sequence(&["a".into(), "b".into()]);
        assert_eq!(a, e);
    }
}
```

- [ ] **Step 2: Verify tests compile and pass**

```bash
cargo test --manifest-path crates/ab-diff-utils/Cargo.toml -- hashing
```
Expected: 3 tests pass

- [ ] **Step 3: Commit**

```bash
git add crates/ab-diff-utils/src/hashing.rs
git commit -m "feat(ab-diff-utils): add hashing helpers"
```

---

### Task 3: Implement `first_diff` Module

**Files:**
- Create: `crates/ab-diff-utils/src/first_diff.rs`

- [ ] **Step 1: Write `first_diff.rs` with tests**

```rust
/// Result of finding the first character-level difference between two strings.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
pub struct FirstDifference {
    pub char_index: usize,
    pub left_snippet: String,
    pub right_snippet: String,
}

/// Finds the first character index where `left` and `right` diverge.
/// Returns `None` if the strings are identical.
pub fn first_difference(left: &str, right: &str) -> Option<FirstDifference> {
    let left_chars: Vec<char> = left.chars().collect();
    let right_chars: Vec<char> = right.chars().collect();
    let max_common = left_chars.len().min(right_chars.len());
    let char_index = (0..max_common)
        .find(|idx| left_chars[*idx] != right_chars[*idx])
        .or_else(|| (left_chars.len() != right_chars.len()).then_some(max_common))?;
    Some(FirstDifference {
        char_index,
        left_snippet: snippet(&left_chars, char_index),
        right_snippet: snippet(&right_chars, char_index),
    })
}

fn snippet(chars: &[char], center: usize) -> String {
    let start = center.saturating_sub(24);
    let end = chars.len().min(center + 24);
    chars[start..end].iter().collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identical_strings_return_none() {
        assert!(first_difference("abc", "abc").is_none());
    }

    #[test]
    fn single_char_difference() {
        let diff = first_difference("abc", "axc").unwrap();
        assert_eq!(diff.char_index, 1);
    }

    #[test]
    fn length_difference() {
        let diff = first_difference("abc", "abcd").unwrap();
        assert_eq!(diff.char_index, 3);
        assert!(diff.left_snippet.is_empty() || diff.left_snippet.len() <= 48);
    }

    #[test]
    fn context_window_bounded() {
        let left: String = std::iter::repeat('a').take(100).collect();
        let right: String = std::iter::repeat('a').take(99).chain(std::iter::once('b')).collect();
        let diff = first_difference(&left, &right).unwrap();
        assert!(diff.left_snippet.len() <= 48);
        assert!(diff.right_snippet.len() <= 48);
    }

    #[test]
    fn multi_byte_chars_use_char_index_not_byte() {
        // "今日" is 2 chars (6 bytes UTF-8), "今" is 1 char (3 bytes)
        let diff = first_difference("今日", "今").unwrap();
        assert_eq!(diff.char_index, 1);
    }
}
```

- [ ] **Step 2: Run tests**

```bash
cargo test --manifest-path crates/ab-diff-utils/Cargo.toml -- first_diff
```
Expected: 5 tests pass

- [ ] **Step 3: Commit**

```bash
git add crates/ab-diff-utils/src/first_diff.rs
git commit -m "feat(ab-diff-utils): add first_difference helper"
```

---

### Task 4: Implement `frequency` Module

**Files:**
- Create: `crates/ab-diff-utils/src/frequency.rs`

- [ ] **Step 1: Write `frequency.rs` with tests**

```rust
use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

/// Default max examples per key in a FrequencyTable.
pub const DEFAULT_MAX_EXAMPLES: usize = 10;

/// A frequency table keyed by `K` with bounded example lists of type `E`.
/// Examples are deduplicated by equality; each unique example contributes
/// at most once to the example list regardless of how many times it is
/// recorded. The count always increments on every `record` call.
///
/// Serializes as a flat `{"key": {"count": N, "examples": [...]}, ...}`
/// object (the `max_examples` field is not serialized).
#[derive(Debug, Clone)]
pub struct FrequencyTable<K, E> {
    #[serde(skip)]
    max_examples: usize,
    entries: BTreeMap<K, FrequencyEntry<E>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FrequencyEntry<E> {
    pub count: usize,
    pub examples: Vec<E>,
}

impl<K: Ord, E: Eq> FrequencyTable<K, E> {
    pub fn new(max_examples: usize) -> Self {
        Self {
            max_examples,
            entries: BTreeMap::new(),
        }
    }

    /// Records an occurrence of `key` with the given example.
    /// Increments the count. Pushes the example into the bounded list
    /// if it is not already present and the list is below `max_examples`.
    pub fn record(&mut self, key: K, example: E) {
        let entry = self.entries.entry(key).or_insert_with(|| FrequencyEntry {
            count: 0,
            examples: Vec::new(),
        });
        entry.count += 1;
        if entry.examples.len() < self.max_examples
            && !entry.examples.iter().any(|existing| existing == &example)
        {
            entry.examples.push(example);
        }
    }

    pub fn get(&self, key: &K) -> Option<&FrequencyEntry<E>> {
        self.entries.get(key)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&K, &FrequencyEntry<E>)> {
        self.entries.iter()
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

/// Custom Serialize: emit a flat object `{"key": {"count": N, "examples": [...]}, ...}`
/// by serializing only `entries`, skipping `max_examples`.
impl<K: Serialize, E: Serialize> Serialize for FrequencyTable<K, E> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.entries.serialize(serializer)
    }
}

/// Custom Deserialize: read a flat `{"key": {"count": N, "examples": [...]}, ...}`
/// object directly into `entries`, using DEFAULT_MAX_EXAMPLES for `max_examples`.
impl<'de, K: Deserialize<'de> + Ord, E: Deserialize<'de> + Eq> Deserialize<'de> for FrequencyTable<K, E> {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let entries = BTreeMap::<K, FrequencyEntry<E>>::deserialize(deserializer)?;
        Ok(FrequencyTable {
            max_examples: DEFAULT_MAX_EXAMPLES,
            entries,
        })
    }
}

impl<K: Ord, E: Eq> Default for FrequencyTable<K, E> {
    fn default() -> Self {
        Self::new(DEFAULT_MAX_EXAMPLES)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_table() {
        let table: FrequencyTable<String, String> = FrequencyTable::new(5);
        assert!(table.is_empty());
        assert_eq!(table.len(), 0);
    }

    #[test]
    fn single_key_multi_example() {
        let mut table = FrequencyTable::new(10);
        table.record("ruby".into(), "work1".into());
        table.record("ruby".into(), "work2".into());
        let entry = table.get(&"ruby".into()).unwrap();
        assert_eq!(entry.count, 2);
        assert_eq!(entry.examples.len(), 2);
    }

    #[test]
    fn deduplicates_examples() {
        let mut table = FrequencyTable::new(10);
        table.record("ruby".into(), "work1".into());
        table.record("ruby".into(), "work1".into());
        table.record("ruby".into(), "work2".into());
        let entry = table.get(&"ruby".into()).unwrap();
        assert_eq!(entry.count, 3);
        assert_eq!(entry.examples.len(), 2);
    }

    #[test]
    fn enforces_max_examples() {
        let mut table = FrequencyTable::new(2);
        table.record("ruby".into(), "a".into());
        table.record("ruby".into(), "b".into());
        table.record("ruby".into(), "c".into());
        let entry = table.get(&"ruby".into()).unwrap();
        assert_eq!(entry.count, 3);
        assert_eq!(entry.examples.len(), 2);
    }

    #[test]
    fn iteration_order_is_deterministic() {
        let mut table = FrequencyTable::new(10);
        table.record("b".into(), "x".into());
        table.record("a".into(), "y".into());
        let keys: Vec<&String> = table.iter().map(|(k, _)| k).collect();
        assert_eq!(keys, vec!["a", "b"]);
    }

    #[test]
    fn serialization_round_trip() {
        let mut table = FrequencyTable::new(10);
        table.record("ruby".into(), "work1".into());
        table.record("ruby".into(), "work2".into());
        let json = serde_json::to_value(&table).unwrap();
        let restored: FrequencyTable<String, String> = serde_json::from_value(json).unwrap();
        let entry = restored.get(&"ruby".into()).unwrap();
        assert_eq!(entry.count, 2);
        assert_eq!(entry.examples, vec!["work1", "work2"]);
    }
}
```

- [ ] **Step 2: Run tests**

```bash
cargo test --manifest-path crates/ab-diff-utils/Cargo.toml -- frequency
```
Expected: 6 tests pass

- [ ] **Step 3: Commit**

```bash
git add crates/ab-diff-utils/src/frequency.rs
git commit -m "feat(ab-diff-utils): add FrequencyTable"
```

---

### Task 5: Register `ab-diff-utils` in Workspace

**Files:**
- Modify: `Cargo.toml` (workspace root)

- [ ] **Step 1: Add to `members` and `[workspace.dependencies]`**

In `Cargo.toml`, add `"crates/ab-diff-utils"` to the `members` list:

```toml
members = [
    "crates/ab-source-syntax",
    "crates/ab-ir",
    "crates/ab-index",
    "crates/ab-check",
    "crates/ab-compare",
    "crates/ab-coverage",
    "crates/ab-diff-utils",
]
```

Add to `[workspace.dependencies]`:

```toml
ab-diff-utils = { path = "crates/ab-diff-utils" }
```

- [ ] **Step 2: Verify workspace still builds**

```bash
cargo check --workspace
```
Expected: compiles without errors

- [ ] **Step 3: Commit**

```bash
git add Cargo.toml
git commit -m "feat(workspace): add ab-diff-utils crate"
```

---

### Task 6: Add `ab-diff-utils` Dependency to `ab-compare`

**Files:**
- Modify: `crates/ab-compare/Cargo.toml`

- [ ] **Step 1: Add dependency**

```toml
[dependencies]
ab-diff-utils.workspace = true
anyhow.workspace = true
clap.workspace = true
serde.workspace = true
serde_json.workspace = true
sha2.workspace = true
walkdir.workspace = true
```

- [ ] **Step 2: Verify compilation**

```bash
cargo check -p ab-compare
```
Expected: compiles (no usage yet)

- [ ] **Step 3: Commit**

```bash
git add crates/ab-compare/Cargo.toml
git commit -m "feat(ab-compare): add ab-diff-utils dependency"
```

---

### Task 7: Migrate `ab-compare/aat_diff.rs` Hashing and FirstDiff to `ab-diff-utils`

**Files:**
- Modify: `crates/ab-compare/src/aat_diff.rs`
- Modify: `crates/ab-compare/Cargo.toml`

- [ ] **Step 1: Remove private hashing and first-diff helpers, replace with imports, drop sha2 dep**

In `crates/ab-compare/src/aat_diff.rs`:

**Remove** these functions and their code entirely:
- `fn hash_bytes`
- `fn hash_json`
- `fn hash_string_sequence`
- `fn first_visible_difference`
- `fn snippet`

**Remove** this import:
```rust
use sha2::{Digest, Sha256};
```

**Add** import at top:
```rust
use ab_diff_utils::{hash_bytes, hash_json, hash_string_sequence, first_difference};
```

**In `crates/ab-compare/Cargo.toml`**, remove `sha2` from `[dependencies]` (no longer used after extraction):

```toml
[dependencies]
ab-diff-utils.workspace = true
anyhow.workspace = true
clap.workspace = true
serde.workspace = true
serde_json.workspace = true
walkdir.workspace = true
```

**Replace** the call site of `first_visible_difference` (in `compare_aat_dirs_with_limit`):

Add `From` impl for `VisibleTextDifference` (using re-exported `FirstDifference`):

```rust
impl From<ab_diff_utils::FirstDifference> for VisibleTextDifference {
    fn from(diff: ab_diff_utils::FirstDifference) -> Self {
        VisibleTextDifference {
            char_index: diff.char_index,
            a_snippet: diff.left_snippet,
            b_snippet: diff.right_snippet,
        }
    }
}
```

Then the call site simplifies to:

```rust
            let normalized_visible_first_difference =
                normalized_visible_difference_bucket.as_ref().and_then(|_| {
                    first_difference(&left.normalized_visible, &right.normalized_visible)
                        .map(VisibleTextDifference::from)
                });
```

- [ ] **Step 2: Build and run existing tests**

```bash
cargo test -p ab-compare
```
Expected: All existing tests pass (behavior-preserving extraction)

- [ ] **Step 3: Commit**

```bash
git add crates/ab-compare/src/aat_diff.rs
git commit -m "refactor(ab-compare): migrate hashing and first_diff to ab-diff-utils"
```

---

### Task 8: Extend `AatSummary` with Coverage Fields

**Files:**
- Modify: `crates/ab-compare/src/aat_diff.rs`

- [ ] **Step 1: Add coverage fields to `AatSummary`**

In `crates/ab-compare/src/aat_diff.rs`, add three fields to `struct AatSummary`:

```rust
#[derive(Debug)]
struct AatSummary {
    work_id: String,
    structure_hash: String,
    visible_hash: String,
    normalized_visible_hash: String,
    normalized_visible: String,
    block_kinds: BTreeMap<String, usize>,
    inline_kinds: BTreeMap<String, usize>,
    semantic_totals: BTreeMap<String, usize>,
    semantic_counts: BTreeMap<String, usize>,
    semantic_hashes: BTreeMap<String, String>,
    semantic_summary_hashes: BTreeMap<String, String>,
    // NEW: coverage fields
    /// None when meta.metrics is absent from the AAT JSON.
    /// Some(false) when metrics are present and the parser succeeded without fallback.
    /// Some(true) when the parser used a fallback.
    fallback_used: Option<bool>,
    fallback_reason: Option<String>,
    source_bytes: Option<usize>,
}
```

- [ ] **Step 2: Extract coverage fields in `summarize()`**

In `fn summarize(root: AatRoot) -> Result<AatSummary>`, add extraction before the `Ok(AatSummary { ... })`:

```rust
fn summarize(root: AatRoot) -> Result<AatSummary> {
    let structure_hash = hash_json(&root.blocks)?;
    // ... existing block/inline/visible collection ...

    // NEW: extract coverage metrics from meta
    let (fallback_used, fallback_reason, source_bytes) = root
        .meta
        .as_ref()
        .and_then(|meta| meta.get("metrics"))
        .map(|metrics| {
            (
                Some(metrics.get("fallback_used").and_then(Value::as_bool).unwrap_or(false)),
                metrics.get("fallback_reason").and_then(Value::as_str).map(str::to_owned),
                metrics.get("source_bytes").and_then(Value::as_u64).map(|n| n as usize),
            )
        })
        .unwrap_or((None, None, None));

    Ok(AatSummary {
        work_id: root.work_id,
        structure_hash,
        // ... existing fields ...
        fallback_used,
        fallback_reason,
        source_bytes,
    })
}
```

- [ ] **Step 3: Build and verify**

```bash
cargo test -p ab-compare
```
Expected: all existing tests pass (new fields are unused so far)

- [ ] **Step 4: Commit**

```bash
git add crates/ab-compare/src/aat_diff.rs
git commit -m "feat(ab-compare): extend AatSummary with coverage fields from meta.metrics"
```

---

### Task 9: Add `CoverageDelta` and Expand Diff Condition

**Files:**
- Modify: `crates/ab-compare/src/aat_diff.rs`

- [ ] **Step 1: Add `CoverageDelta` struct and `coverage_only_difference_count`**

Add after `AatStructuralDifference`:

```rust
#[derive(Debug, Serialize)]
pub struct CoverageDelta {
    pub a_had_fallback: bool,
    pub b_had_fallback: bool,
    /// None when a_had_fallback is false; present when true.
    pub a_fallback_reason: Option<String>,
    /// None when b_had_fallback is false; present when true.
    pub b_fallback_reason: Option<String>,
    pub a_source_bytes: Option<usize>,
    pub b_source_bytes: Option<usize>,
}
```

Add `coverage_mismatch` field to `AatStructuralDifference`:

```rust
    pub normalized_visible_difference_bucket: Option<String>,
    pub normalized_visible_first_difference: Option<VisibleTextDifference>,
    // NEW:
    pub coverage_mismatch: Option<CoverageDelta>,
}
```

Add `coverage_only_difference_count` field **at the end** of `AatCompareSummary` (after `structural_differences`). Also add `coverage_metrics_missing` to track how many work-pairs had missing metrics on one or both sides (preventing coverage comparison):

```rust
    pub structural_differences: Vec<AatStructuralDifference>,
    // NEW:
    pub coverage_only_difference_count: usize,
    pub coverage_metrics_missing: usize,
}

- [ ] **Step 2: Add coverage-only condition and population in `compare_aat_dirs_with_limit`**

Add initialization after existing counters:
```rust
    let mut same_visible_structural_difference_count = 0usize;
    // NEW:
    let mut coverage_only_difference_count = 0usize;
    let mut coverage_differences = Vec::new();
    let mut coverage_metrics_missing = 0usize;
```

Inside the `for key in &common` loop, **before** the existing structural-difference condition, check for coverage difference. Both `coverage_only_difference_count` and `structural_difference_count` are "total observed" counters (they increment before the limit check, matching existing behavior):

```rust
        let coverage_mismatch = if left.fallback_used.is_some()
            && right.fallback_used.is_some()
            && left.fallback_used != right.fallback_used
        {
            Some(CoverageDelta {
                a_had_fallback: left.fallback_used.unwrap_or(false),
                b_had_fallback: right.fallback_used.unwrap_or(false),
                a_fallback_reason: left.fallback_reason.clone(),
                b_fallback_reason: right.fallback_reason.clone(),
                a_source_bytes: left.source_bytes,
                b_source_bytes: right.source_bytes,
            })
        } else {
            if left.fallback_used.is_none() || right.fallback_used.is_none() {
                coverage_metrics_missing += 1;
            }
            None
        };
```

Then replace the existing structural-difference condition `if left.structure_hash != right.structure_hash ...` with:

```rust
        let has_hash_difference = left.structure_hash != right.structure_hash
            || left.visible_hash != right.visible_hash
            || semantic_summary_hashes_differ
                .values()
                .any(|differs| *differs);

        if has_hash_difference {
            // ... existing structural-difference population (unchanged) ...
            // Add coverage_mismatch inside the AatStructuralDifference construction:
            structural_differences.push(AatStructuralDifference {
                // ... all existing fields ...
                normalized_visible_first_difference,
                coverage_mismatch,  // NEW field
            });
        } else if coverage_mismatch.is_some() {
            // Coverage-only difference: hashes match, but fallback differs.
            // Stored in a separate coverage_differences vector, not mixed into
            // structural_differences, so structural_difference_count == 0 is honest.
            coverage_only_difference_count += 1;
            if difference_limit.is_some_and(|limit| coverage_differences.len() >= limit) {
                continue;
            }
            coverage_differences.push(AatStructuralDifference {
                work_id: left.work_id.clone(),
                visible_text_differs: false,
                normalized_visible_text_differs: false,
                a_structure_hash: left.structure_hash.clone(),
                b_structure_hash: right.structure_hash.clone(),
                a_visible_hash: left.visible_hash.clone(),
                b_visible_hash: right.visible_hash.clone(),
                a_normalized_visible_hash: left.normalized_visible_hash.clone(),
                b_normalized_visible_hash: right.normalized_visible_hash.clone(),
                a_block_kinds: left.block_kinds.clone(),
                b_block_kinds: right.block_kinds.clone(),
                a_inline_kinds: left.inline_kinds.clone(),
                b_inline_kinds: right.inline_kinds.clone(),
                a_semantic_counts: left.semantic_counts.clone(),
                b_semantic_counts: right.semantic_counts.clone(),
                a_semantic_hashes: left.semantic_hashes.clone(),
                b_semantic_hashes: right.semantic_hashes.clone(),
                semantic_hashes_differ: semantic_hashes_differ.clone(),
                a_semantic_summary_hashes: left.semantic_summary_hashes.clone(),
                b_semantic_summary_hashes: right.semantic_summary_hashes.clone(),
                semantic_summary_hashes_differ: semantic_summary_hashes_differ.clone(),
                normalized_visible_difference_bucket: None,
                normalized_visible_first_difference: None,
                coverage_mismatch,
            });
        }
```

Add `coverage_only_difference_count`, `coverage_differences`, and `coverage_metrics_missing` to the `Ok(AatCompareSummary { ... })`:

```rust
    Ok(AatCompareSummary {
        // ... existing fields ...
        structural_differences,
        coverage_only_difference_count,
        coverage_differences,
        coverage_metrics_missing,
    })
```

- [ ] **Step 3: Update existing test `triage_report_buckets_differences_by_feature_and_metrics`**

In `tests/integration.rs`, the test `triage_report_buckets_differences_by_feature_and_metrics` constructs an `AatCompareSummary` inline. Add the new fields:

```rust
        same_visible_structural_difference_count: 1,
        structural_differences: Vec::new(),
        coverage_only_difference_count: 0,
        coverage_differences: Vec::new(),
        coverage_metrics_missing: 0,
```

- [ ] **Step 4: Build and run tests**

```bash
cargo test -p ab-compare
```
Expected: all existing tests still pass (coverage tests not written yet)

- [ ] **Step 5: Commit**

```bash
git add crates/ab-compare/src/aat_diff.rs crates/ab-compare/tests/integration.rs
git commit -m "feat(ab-compare): add CoverageDelta and coverage_only_difference_count"
```

---

### Task 10: Migrate `triage.rs` to `FrequencyTable`

**Files:**
- Modify: `crates/ab-compare/src/triage.rs`

- [ ] **Step 1: Replace `TriageBucket` and `push_bucket` with `FrequencyTable`**

Remove:
```rust
#[derive(Debug, Serialize)]
pub struct TriageBucket {
    pub count: usize,
    pub work_ids: Vec<String>,
}
```

And remove `fn push_bucket`.

Add import:
```rust
use ab_diff_utils::frequency::FrequencyTable;
```

Change `ResultDifferenceTriage` fields:
```rust
#[derive(Debug, Serialize)]
pub struct ResultDifferenceTriage {
    pub total: usize,
    pub by_property: FrequencyTable<String, String>,
    pub by_feature: FrequencyTable<String, String>,
    pub by_property_and_feature: FrequencyTable<String, String>,
}
```

- [ ] **Step 2: Replace `push_bucket` call sites in `build_triage_report`**

Replace `BTreeMap::new()` + `push_bucket` with `FrequencyTable::new(10)`:
```rust
    let mut by_property = FrequencyTable::new(10);
    let mut by_feature = FrequencyTable::new(10);
    let mut by_property_and_feature = FrequencyTable::new(10);

    for difference in &comparison.result_differences {
        by_property.record(difference.property.clone(), difference.work_id.clone());
        let work_features = features
            .get(&difference.work_id)
            .cloned()
            .unwrap_or_else(|| BTreeSet::from(["unindexed".to_owned()]));
        for feature in work_features {
            by_feature.record(feature.clone(), difference.work_id.clone());
            by_property_and_feature.record(
                format!("{}:{feature}", difference.property),
                difference.work_id.clone(),
            );
        }
    }
```

- [ ] **Step 3: Update existing test assertions**

In `tests/integration.rs`, the test `triage_report_buckets_differences_by_feature_and_metrics` accesses `by_property["visible_text_body_order"].count`. Change to use `get()` (the `.to_owned()` allocation on `&str` keys is test-only and negligible; triage code uses pre-owned `String` keys so no allocation occurs there):
```rust
    assert_eq!(
        report.result_differences.by_property.get(&"visible_text_body_order".to_owned()).unwrap().count,
        1
    );
    assert_eq!(report.result_differences.by_feature.get(&"gaiji".to_owned()).unwrap().count, 1);
    assert_eq!(report.result_differences.by_feature.get(&"ruby".to_owned()).unwrap().count, 1);
    assert_eq!(
        report.result_differences.by_feature.get(&"jisage_block".to_owned()).unwrap().count,
        1
    );
```

- [ ] **Step 4: Run tests**

```bash
cargo test -p ab-compare
```
Expected: all tests pass (wire format change: `work_ids` → `examples` in JSON output for `by_property`/`by_feature`/`by_property_and_feature`)

- [ ] **Step 5: Commit**

```bash
git add crates/ab-compare/src/triage.rs crates/ab-compare/tests/integration.rs
git commit -m "refactor(ab-compare): replace TriageBucket/push_bucket with FrequencyTable"
```

---

### Task 11: Add Coverage Mismatch Triage Section

**Files:**
- Modify: `crates/ab-compare/src/triage.rs`

- [ ] **Step 1: Add `CoverageMismatchTriage` struct and include in `TriageReport`**

Add struct:
```rust
#[derive(Debug, Serialize)]
pub struct CoverageMismatchTriage {
    pub count: usize,
    pub by_reason: FrequencyTable<String, String>,
}
```

Add field to `TriageReport`:
```rust
    pub source_supplements: Option<SourceSupplementTriage>,
    // NEW:
    pub coverage_mismatches: Option<CoverageMismatchTriage>,
    pub recommended_next_targets: Vec<String>,
```

- [ ] **Step 2: Populate coverage_mismatches in `build_triage_report`**

After `source_supplements` construction, add:

```rust
    let coverage_mismatches = aat.and_then(|summary| {
        let mut by_reason = FrequencyTable::new(20);
        let mut count = 0usize;
        // Collect from both structural_differences (hash-diff entries that also
        // carry a coverage delta) and coverage_differences (coverage-only entries).
        for diff in summary.structural_differences.iter().chain(summary.coverage_differences.iter()) {
            if let Some(coverage) = &diff.coverage_mismatch {
                count += 1;
                // left.fallback_used != right.fallback_used guarantees one is true
                // and the other false, so the (true, true) and (false, false)
                // branches below are unreachable; they're present for exhaustiveness.
                let reason = match (coverage.a_had_fallback, coverage.b_had_fallback) {
                    (true, false) => coverage.a_fallback_reason.clone().unwrap_or_else(|| "unknown".into()),
                    (false, true) => coverage.b_fallback_reason.clone().unwrap_or_else(|| "unknown".into()),
                    (true, true) => format!(
                        "a={}, b={}",
                        coverage.a_fallback_reason.as_deref().unwrap_or("unknown"),
                        coverage.b_fallback_reason.as_deref().unwrap_or("unknown"),
                    ),
                    (false, false) => "unknown".into(),
                };
                by_reason.record(reason, diff.work_id.clone());
            }
        }
        if count > 0 {
            Some(CoverageMismatchTriage { count, by_reason })
        } else {
            None
        }
    });
```

Add to `TriageReport` construction:
```rust
    TriageReport {
        // ... existing fields ...
        source_supplements,
        coverage_mismatches,  // NEW
        recommended_next_targets: recommended_next_targets(aat, metrics),
    }
```

- [ ] **Step 3: Update `recommended_next_targets`**

Add `coverage_mismatches` check. Check both `coverage_only_difference_count > 0` and whether any structural diffs carry coverage:

```rust
    let has_coverage_issues = aat.is_some_and(|summary| {
        summary.coverage_only_difference_count > 0
            || summary.structural_differences.iter().any(|d| d.coverage_mismatch.is_some())
    });
    if has_coverage_issues {
        targets.push("inspect_coverage_mismatches".to_owned());
    }
```

Place this before the existing `semantic_summary_hash_difference_counts` check.

- [ ] **Step 4: Run tests**

```bash
cargo test -p ab-compare
```
Expected: existing tests pass. The `triage_report_buckets` test's `structual_differences` is empty, so `coverage_mismatches` will be `None` — no assertion needed yet.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-compare/src/triage.rs
git commit -m "feat(ab-compare): add coverage mismatch triage section"
```

---

### Task 12: Add Coverage Integration Tests

**Files:**
- Modify: `crates/ab-compare/tests/integration.rs`

- [ ] **Step 1: Write test 1 — Coverage present in structural diff when hashes differ**

```rust
#[test]
fn coverage_delta_recorded_when_fallback_differs_and_hashes_differ() {
    let temp = tempfile::tempdir().unwrap();
    let a = temp.path().join("a");
    let b = temp.path().join("b");
    std::fs::create_dir_all(&a).unwrap();
    std::fs::create_dir_all(&b).unwrap();

    // Side A: fallback_used=true, different text
    std::fs::write(
        a.join("one.json"),
        r#"{
          "work_id": "one",
          "blocks": [
            {"kind": "paragraph", "content": [{"kind": "text", "value": "左"}]}
          ],
          "meta": {"adapter": "a", "metrics": {"fallback_used": true, "fallback_reason": "aborted", "source_bytes": 100}}
        }"#,
    )
    .unwrap();
    // Side B: fallback_used=false, different text
    std::fs::write(
        b.join("one.json"),
        r#"{
          "work_id": "one",
          "blocks": [
            {"kind": "paragraph", "content": [{"kind": "text", "value": "右"}]}
          ],
          "meta": {"adapter": "b", "metrics": {"fallback_used": false, "source_bytes": 200}}
        }"#,
    )
    .unwrap();

    let summary = ab_compare::aat_diff::compare_aat_dirs(&a, &b).unwrap();

    assert_eq!(summary.structural_difference_count, 1);
    assert_eq!(summary.coverage_only_difference_count, 0);
    let diff = &summary.structural_differences[0];
    assert!(diff.visible_text_differs);
    let coverage = diff.coverage_mismatch.as_ref().unwrap();
    assert!(coverage.a_had_fallback);
    assert!(!coverage.b_had_fallback);
    assert_eq!(coverage.a_fallback_reason.as_deref(), Some("aborted"));
    assert_eq!(coverage.b_fallback_reason, None);
    assert_eq!(coverage.a_source_bytes, Some(100));
    assert_eq!(coverage.b_source_bytes, Some(200));
}
```

- [ ] **Step 2: Write test 2 — Coverage-only difference tracked separately**

```rust
#[test]
fn coverage_only_difference_count_incremented_when_hashes_match() {
    let temp = tempfile::tempdir().unwrap();
    let a = temp.path().join("a");
    let b = temp.path().join("b");
    std::fs::create_dir_all(&a).unwrap();
    std::fs::create_dir_all(&b).unwrap();

    // Identical blocks; only fallback_used differs. Use serde_json::json! to avoid
    // format-string brace-escaping foot-guns.
    let a_json = serde_json::json!({
        "meta": {
            "adapter": "a",
            "metrics": {"fallback_used": true, "fallback_reason": "aborted"}
        },
        "work_id": "one",
        "blocks": [
            {"kind": "paragraph", "content": [{"kind": "text", "value": "本文"}]}
        ]
    });
    let b_json = serde_json::json!({
        "meta": {
            "adapter": "b",
            "metrics": {"fallback_used": false}
        },
        "work_id": "one",
        "blocks": [
            {"kind": "paragraph", "content": [{"kind": "text", "value": "本文"}]}
        ]
    });

    std::fs::write(a.join("one.json"), serde_json::to_string(&a_json).unwrap()).unwrap();
    std::fs::write(b.join("one.json"), serde_json::to_string(&b_json).unwrap()).unwrap();

    let summary = ab_compare::aat_diff::compare_aat_dirs(&a, &b).unwrap();

    assert_eq!(summary.structural_difference_count, 0);
    assert_eq!(summary.visible_text_difference_count, 0);
    assert_eq!(summary.same_visible_structural_difference_count, 0);
    assert_eq!(summary.coverage_only_difference_count, 1);
    let diff = &summary.coverage_differences[0];
    assert!(!diff.visible_text_differs);
    assert_eq!(diff.work_id, "one");
    assert!(diff.coverage_mismatch.is_some());
    assert!(diff.coverage_mismatch.as_ref().unwrap().a_had_fallback);
    assert!(!diff.coverage_mismatch.as_ref().unwrap().b_had_fallback);
}
```

- [ ] **Step 3: Write test 3 — Missing metrics skips coverage comparison**

```rust
#[test]
fn missing_metrics_skips_coverage_comparison() {
    let temp = tempfile::tempdir().unwrap();
    let a = temp.path().join("a");
    let b = temp.path().join("b");
    std::fs::create_dir_all(&a).unwrap();
    std::fs::create_dir_all(&b).unwrap();

    // Side A: no meta.metrics at all
    std::fs::write(
        a.join("one.json"),
        r#"{
          "work_id": "one",
          "blocks": [
            {"kind": "paragraph", "content": [{"kind": "text", "value": "本文"}]}
          ],
          "meta": {"adapter": "a"}
        }"#,
    )
    .unwrap();
    // Side B: fallback_used=true, same blocks
    std::fs::write(
        b.join("one.json"),
        r#"{
          "work_id": "one",
          "blocks": [
            {"kind": "paragraph", "content": [{"kind": "text", "value": "本文"}]}
          ],
          "meta": {"adapter": "b", "metrics": {"fallback_used": true, "fallback_reason": "aborted"}}
        }"#,
    )
    .unwrap();

    let summary = ab_compare::aat_diff::compare_aat_dirs(&a, &b).unwrap();

    assert_eq!(summary.structural_difference_count, 0);
    assert_eq!(summary.coverage_only_difference_count, 0);
    assert!(summary.structural_differences.is_empty());
}
```

- [ ] **Step 4: Run all tests**

```bash
cargo test -p ab-compare
```
Expected: all tests pass including the 3 new ones

- [ ] **Step 5: Commit**

```bash
git add crates/ab-compare/tests/integration.rs
git commit -m "test(ab-compare): add coverage mismatch integration tests"
```

---

### Task 13: Final Verification

- [ ] **Step 1: Format**

```bash
cargo fmt --all -- --check
```
Expected: no changes needed

- [ ] **Step 2: Clippy**

```bash
cargo clippy --workspace --all-targets -- -D warnings
```
Expected: no warnings

- [ ] **Step 3: Full workspace test**

```bash
cargo test --workspace
```
Expected: all tests pass across all crates

- [ ] **Step 4: Doc check**

```bash
cargo doc --no-deps --workspace
```
Expected: no broken intra-doc links, no warnings

- [ ] **Step 5: Commit if any formatting/lint fixes were needed**

```bash
git add -u
git commit -m "chore: fmt and clippy fixes"
```

---

## Plan Queue Maintenance

After all tasks are complete:
1. Update `docs/superpowers/PLAN-EXECUTION-ORDER.md` — mark this plan as archived
2. Open spec gap: the morphological diff crate (phase two) now has `ab-diff-utils` available as a dependency with `FrequencyTable`, `FirstDifference`, and hashing helpers ready

## Cache Migration Note

`ab-compare` has no parser cache of its own — it reads AAT output files produced by the adapters. Pre-existing AAT files that lack `meta.metrics` will produce `fallback_used: None` after this change, which correctly skips coverage comparison (no false positives). The `coverage_metrics_missing` counter on `AatCompareSummary` tracks how many work-pairs had missing metrics, so operators can audit. No cache purge is required.
