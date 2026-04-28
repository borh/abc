# Diff Utilities Extraction Design

Status: approved
Date: 2026-04-28

Scope: extract shared comparison primitives from `ab-compare` into a new
`ab-diff-utils` crate and add coverage-mismatch tracking to `ab-compare`.
This is the first phase of a two-phase effort; the second phase builds a
morphological diff engine (see `docs/morpheme-diff-algorithm-spec.md`) that
will also consume `ab-diff-utils`.

## 1. Motivation

`ab-compare` currently compares AAT structures via content hashing. The
morpheme-diff-algorithm-spec requires a different core algorithm
(span-based region alignment), but several primitives and patterns are
shared:

- Frequency tables with bounded example lists (triage buckets ↔
  segmentation transformation tables, confusion matrices, delta inventories).
- Character-level first-difference detection with context snippets.
- SHA256 content hashing for deterministic output and content-addressed
  caching.
- Region classification (OneToOne / Segmentation / CoverageMismatch) that
  maps cleanly to the spec's data model.

Extracting these into `ab-diff-utils` avoids duplication between
`ab-compare` and the future morphological diff crate.

Additionally, `ab-compare` currently conflates coverage failures with
structural differences. Adding explicit `CoverageDelta` tracking improves
triage quality and aligns the data model with the spec's
`CoverageMismatch` region type.

## 2. New Crate: `ab-diff-utils`

Location: `crates/ab-diff-utils/`

Leaf-level workspace crate with no dependency on any other workspace
crate. Depends on `serde`, `serde_json`, `sha2` (workspace deps).

### 2.1 `FrequencyTable<K, E>`

Generic frequency counter with bounded example lists. Replaces
`ab-compare::triage::push_bucket`.

```rust
pub struct FrequencyTable<K: Ord, E: Eq> {
    max_examples: usize,
    entries: BTreeMap<K, FrequencyEntry<E>>,
}

pub struct FrequencyEntry<E> {
    pub count: usize,
    pub examples: Vec<E>,
}
```

**Public API:**

- `new(max_examples: usize) -> Self`
- `record(&mut self, key: K, example: E)` — increments count; pushes
  example if below `max_examples` and the example is not already present
  in the entry's list. Deduplication is by equality (`E: Eq`): each
  unique example contributes at most once to the example list, regardless
  of how many times it is recorded. The count still increments on every
  call.
- `get(&self, key: &K) -> Option<&FrequencyEntry<E>>`
- `iter(&self) -> impl Iterator<Item = (&K, &FrequencyEntry<E>)>`
- `len(&self) -> usize`

Derives `Serialize, Deserialize` (requires `K: Serialize, E: Serialize`).
`FrequencyEntry<E>` also derives `Serialize, Deserialize`.

**Consumers:**
- `ab-compare` triage: `by_property`, `by_feature`,
  `by_property_and_feature` (currently `BTreeMap<String, TriageBucket>`)
- Morph spec §6.2: segmentation transformation table
- Morph spec §6.4: feature confusion matrices
- Morph spec §6.5: feature delta inventory
- Morph spec §6.7: concordance examples

### 2.2 `FirstDifference`

Character-level diff with context snippet. Extracted from
`ab-compare::aat_diff::first_visible_difference` and `snippet`.

```rust
pub struct FirstDifference {
    pub char_index: usize,
    pub left_snippet: String,
    pub right_snippet: String,
}

pub fn first_difference(left: &str, right: &str) -> Option<FirstDifference>
```

Context window: 48 characters (±24 around divergence). If one string
is longer than the other and all common chars match, `char_index` is
the shorter length and snippets show the end of the shorter string and
the start of the longer string's tail.

**Consumers:**
- `ab-compare`: `normalized_visible_first_difference` field
- Morph spec §4.2: character-diff diagnostic tool

### 2.3 Hashing Helpers

Three free functions for SHA256 hashing in module `hashing`. Extracted from
`ab-compare::aat_diff` private helpers.

```rust
pub fn hash_bytes(bytes: &[u8]) -> String
pub fn hash_json(value: &serde_json::Value) -> Result<String>
pub fn hash_string_sequence(values: &[String]) -> String
```

`hash_bytes` produces `"sha256:{hex}"`. `hash_string_sequence` joins
values with null-byte delimiters before hashing (so `["a", "b"]` ≠
`["ab"]`).

**Consumers:**
- `ab-compare`: AAT structure/visible/semantic hashing
- Morph crate: content-addressed analyzer-output cache

### 2.4 File Layout

```
crates/ab-diff-utils/
├── Cargo.toml
└── src/
    ├── lib.rs          // re-exports all public items
    ├── frequency.rs    // FrequencyTable, FrequencyEntry, tests
    ├── first_diff.rs   // FirstDifference, first_difference(), tests
    └── hashing.rs      // hash_bytes, hash_json, hash_string_sequence, tests
```

`DiffRegion` (OneToOne / Segmentation / CoverageMismatch) is deferred to
the morphological diff phase. The per-work coverage delta used by
`ab-compare` (§3.3) doesn't carry source-text spans and doesn't fit the
region-with-span model. Building `DiffRegion` without a consumer would
produce a type whose only user can't populate it honestly.

## 3. `ab-compare` Changes

### 3.1 Dependency

Add `ab-diff-utils = { path = "../ab-diff-utils" }` to `Cargo.toml`.

### 3.2 `aat_diff.rs` — Extend AatSummary with Coverage Fields

`AatSummary` (the internal struct built by `summarize()`) gains three
fields to carry coverage data from the AAT JSON `meta.metrics`:

```rust
struct AatSummary {
    // ... existing fields: work_id, structure_hash, visible_hash, etc. ...
    /// None when meta.metrics is absent from the AAT JSON (pre-metrics file
    /// or writer bug). Some(false) when metrics are present and the parser
    /// succeeded without fallback. Some(true) when the parser used a fallback.
    fallback_used: Option<bool>,
    fallback_reason: Option<String>,
    source_bytes: Option<usize>,
}
```

These are extracted in `summarize()` from `root.meta` (the `AatRoot.meta`
field, typed as `Option<Value>`, already deserialized by `read_aat_summaries`).
The extraction pattern:

```rust
fn summarize(root: AatRoot) -> Result<AatSummary> {
    // ... existing structure/visible hashing ...
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
    Ok(AatSummary { /* ... existing ... */ fallback_used, fallback_reason, source_bytes })
}
```

### 3.3 `aat_diff.rs` — Add CoverageDelta and Expand Difference Condition

New struct:

```rust
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

Added as optional field on `AatStructuralDifference`:

```rust
pub coverage_mismatch: Option<CoverageDelta>,
```

**Condition change:** The existing condition for creating an
`AatStructuralDifference` is:

```rust
if left.structure_hash != right.structure_hash
    || left.visible_hash != right.visible_hash
    || semantic_summary_hashes_differ.values().any(|differs| *differs)
```

This expands to also trigger when **coverage differs without hash
differences**. Coverage comparison is only performed when both sides have
metrics present (`fallback_used` is `Some` on both); if either side is
missing metrics the coverage check is skipped. The new condition adds:

```rust
    || (left.fallback_used.is_some()
        && right.fallback_used.is_some()
        && left.fallback_used != right.fallback_used)
```

**Metric impact:** Coverage-only differences do NOT increment
`structural_difference_count`, `visible_text_difference_count`, or
`same_visible_structural_difference_count`. Instead they increment a new
counter `coverage_only_difference_count` on `AatCompareSummary`. This keeps
existing counter semantics stable so downstream dashboards and regression
budgets are unaffected.

```rust
pub coverage_only_difference_count: usize,
```

### 3.4 `aat_diff.rs` — Migrate to `ab-diff-utils`

Remove and re-import:
- `first_visible_difference` → use `ab_diff_utils::first_difference`
- `snippet` → removed (internal to `first_difference` now)
- `hash_bytes`, `hash_json`, `hash_string_sequence` →
  use `ab_diff_utils::{hash_bytes, hash_json, hash_string_sequence}`

### 3.5 `triage.rs` — Migrate to `FrequencyTable`

Replace `push_bucket` free function and `TriageBucket` struct with
`FrequencyTable<String, String>` (keyed by property/feature name,
valued by work_id).

`ResultDifferenceTriage` fields change from:
```rust
pub by_property: BTreeMap<String, TriageBucket>,
```
to:
```rust
pub by_property: FrequencyTable<String, String>,
```

`push_bucket(buckets, key, work_id)` call sites become
`buckets.record(key, work_id)`.

### 3.6 `triage.rs` — Add Coverage Mismatch Section

New struct:

```rust
pub struct CoverageMismatchTriage {
    pub count: usize,
    pub by_reason: FrequencyTable<String, String>,
}
```

Added to `TriageReport`:

```rust
pub coverage_mismatches: Option<CoverageMismatchTriage>,
```

Populated in `build_triage_report` from `aat_summary`'s
`structural_differences` entries that have `coverage_mismatch: Some`.

`recommended_next_targets` gains: `"inspect_coverage_mismatches"` when
`coverage_mismatches` has entries.

**Wire-format change:** `ResultDifferenceTriage` fields change from
`BTreeMap<String, TriageBucket>` to `FrequencyTable<String, String>`,
which serializes to a different JSON shape. `FrequencyTable` serializes as
`{"key": {"count": N, "examples": [...]}, ...}` — a flat object with the
same keys as before but a different value shape (was `{count, work_ids}`,
now `{count, examples}`). This is a breaking change to the `--triage-output`
JSON; consumers that parse these fields must update their field name from
`work_ids` to `examples`.

### 3.7 `aat_diff.rs` — Remaining Internal Changes

Types and functions not listed in §3.2–§3.6 (`SemanticSequences`,
`normalize_visible`, `kind`, `count_both`, `increment`,
`semantic_hash_differences`, `semantic_summary_hash_differences`,
`semantic_totals`, `normalized_visible_difference_bucket`,
`collect_blocks`, `collect_inline_containers`, `collect_inline_node`,
`read_aat_summaries`, `aat_key`, `compare_aat_dirs`) are unchanged.
`AatSummary` and `summarize` change per §3.2; `compare_aat_dirs_with_limit`
changes per §3.3 and §3.4.

### 3.8 Unchanged Files

- `ab-compare/src/lib.rs` — re-exports unchanged
- `ab-compare/src/main.rs` — CLI unchanged; coverage data flows through
  existing `--triage-output` path
- `ab-compare/src/metrics.rs` — already reads fallback fields;
  no changes needed
- No changes to any other workspace crate

## 4. Workspace Changes

`Cargo.toml`:
- Add `"crates/ab-diff-utils"` to `members`
- Add `ab-diff-utils = { path = "crates/ab-diff-utils" }` to
  `[workspace.dependencies]`

## 5. Testing

### 5.1 `ab-diff-utils` Unit Tests

- `frequency.rs`: test empty table, single key multi-example, example
  dedup, max_examples enforcement, iteration order, serialization round-trip
- `first_diff.rs`: test identical strings (None), single-char diff,
  length-difference diff, context window bounds, and multi-byte
  character handling (e.g., `"今日"` vs `"今"` — char index 1, not
  byte index 2)
- `hashing.rs`: test deterministic output, null-delimiter separation

### 5.2 `ab-compare` Integration Tests

New tests in `tests/integration.rs`:

1. **Coverage present in structural diff:** two AAT fixture files where one
   has `fallback_used: true` with a known reason and the structure hashes
   differ. Run `compare_aat_dirs`, assert
   `structural_differences[0].coverage_mismatch` is `Some` with the expected
   reason. Also assert `structural_difference_count >= 1` and
   `coverage_only_difference_count == 0`.

2. **Coverage-only difference tracked separately:** two AAT fixture files
   with identical structure hash, visible hash, and semantic summary hashes,
   but `fallback_used` differs (left: `true, "aborted"`, right: `false`).
   Run `compare_aat_dirs`, assert `coverage_only_difference_count == 1`,
   `structural_difference_count == 0`,
   `visible_text_difference_count == 0`,
   `same_visible_structural_difference_count == 0`. The entry has
   `coverage_mismatch: Some` and `visible_text_differs: false`.

3. **Missing metrics skips coverage:** two AAT fixture files where one has no
   `meta.metrics` at all and the other has `fallback_used: true`. Both have
   identical structure/visible/semantic hashes. Run `compare_aat_dirs`,
   assert `coverage_only_difference_count == 0` and no structural difference
   is emitted.

Existing integration tests continue to pass — `FrequencyTable` and
`first_difference` are behavior-preserving extractions.

### 5.3 Migration

AAT fixtures and test data that predate `meta.metrics` will have
`fallback_used: None` after this change — coverage comparison is skipped, no
false positives. The migration policy is: accept the default-to-None behavior.
No regeneration of cached AAT is required.

## 6. Non-Goals

- No `Comparison` trait. Avoids premature abstraction when only one
  consumer exists. Reevaluate after morphological diff crate is built.
- No changes to `ab-compare`'s hash-based core algorithm.
- No morphological analysis types or logic.
- No multiway comparison support.

## 7. Relationship to Morphological Diff Spec

This extraction provides shared primitives for the recommended pipeline
in `docs/morpheme-diff-algorithm-spec.md`:

```
parser adapter → validated spans → region alignment → feature comparison → artifacts → reports
```

`ab-diff-utils` provides:
- `FrequencyTable` — backing data structure for transformation tables,
  confusion matrices, delta inventories, and concordance examples
- `FirstDifference` — the character-diff diagnostic tool
- Hashing helpers — content-addressed caching for analyzer output

The morphological diff crate (phase two) will use all three. The
`DiffRegion` enum (OneToOne / Segmentation / CoverageMismatch) is
deferred to that phase since its span-keyed model doesn't fit
`ab-compare`'s per-work coverage delta.

This extraction ensures the two comparison crates share vocabulary and
data structures without sharing a core algorithm.
