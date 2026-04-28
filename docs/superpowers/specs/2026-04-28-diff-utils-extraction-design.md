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
pub struct FrequencyTable<K: Ord, E: Eq + Hash> {
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
  example if below `max_examples` and not already present
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

### 2.3 `ContentHasher`

Three free functions for SHA256 hashing. Extracted from
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

### 2.4 `DiffRegion`

Region classification enums aligned with the morpheme-diff spec §5.1.
Generalized beyond morphemes so `ab-compare` can use the
`CoverageMismatch` variant without depending on morphological concepts.

```rust
pub enum DiffRegion {
    OneToOne { span: Range<usize> },
    Segmentation { span: Range<usize>, kind: SegmentationKind },
    CoverageMismatch { span: Range<usize>, kind: CoverageKind },
}

pub enum SegmentationKind {
    Split,
    Merge,
    Resegment,
}

pub enum CoverageKind {
    LeftOnly,
    RightOnly,
    Overlap,
    OutOfOrder,
    Unknown,
}
```

**Consumers:**
- Morph crate: all variants
- `ab-compare`: `CoverageMismatch` only (Segmentation variants are
  available but unused — acceptable since they're `pub` in a library)

### 2.5 File Layout

```
crates/ab-diff-utils/
├── Cargo.toml
└── src/
    ├── lib.rs          // re-exports all public items
    ├── frequency.rs    // FrequencyTable, FrequencyEntry, tests
    ├── first_diff.rs   // FirstDifference, first_difference(), tests
    ├── hashing.rs      // hash_bytes, hash_json, hash_string_sequence, tests
    └── diff_region.rs  // DiffRegion, SegmentationKind, CoverageKind
```

## 3. `ab-compare` Changes

### 3.1 Dependency

Add `ab-diff-utils = { path = "../ab-diff-utils" }` to `Cargo.toml`.

### 3.2 `aat_diff.rs` — Add Coverage Delta

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

Populated in `compare_aat_dirs_with_limit` when either side's
`meta.metrics` has `fallback_used: true`. The metrics are already
read from AAT JSON — no new parsing.

### 3.3 `aat_diff.rs` — Migrate to `ab-diff-utils`

Remove and re-import:
- `first_visible_difference` → use `ab_diff_utils::first_difference`
- `snippet` → removed (internal to `first_difference` now)
- `hash_bytes`, `hash_json`, `hash_string_sequence` →
  use `ab_diff_utils::{hash_bytes, hash_json, hash_string_sequence}`

### 3.4 `triage.rs` — Migrate to `FrequencyTable`

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

### 3.5 `triage.rs` — Add Coverage Mismatch Section

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

### 3.6 `aat_diff.rs` — Internal Changes Only

The `SemanticSequences` type, `AatSummary`, `normalize_visible`,
`kind`, `count_both`, `increment`, `semantic_hash_differences`,
`semantic_summary_hash_differences`, `semantic_totals`,
`normalized_visible_difference_bucket`, `collect_blocks`,
`collect_inline_containers`, `collect_inline_node`, `summarize`,
`read_aat_summaries`, `aat_key`, `compare_aat_dirs`,
`compare_aat_dirs_with_limit` all remain unchanged except for the
coverage population and import migrations listed above.

### 3.7 Unchanged Files

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
  dedup, max_examples enforcement, iteration order
- `first_diff.rs`: test identical strings (None), single-char diff,
  length-difference diff, context window bounds
- `hashing.rs`: test deterministic output, null-delimiter separation
- `diff_region.rs`: enum equality and debug formatting

### 5.2 `ab-compare` Integration Tests

New test in `tests/integration.rs`: two AAT fixture files where one
has `fallback_used: true` with a known reason. Run
`compare_aat_dirs`, assert `structural_differences[0].coverage_mismatch`
is `Some` with the expected reason.

Existing integration tests continue to pass — `FrequencyTable` and
`first_difference` are behavior-preserving extractions.

## 6. Non-Goals

- No `Comparison` trait. Avoids premature abstraction when only one
  consumer exists. Reevaluate after morphological diff crate is built.
- No changes to `ab-compare`'s hash-based core algorithm.
- No morphological analysis types or logic.
- No multiway comparison support.

## 7. Relationship to Morphological Diff Spec

This extraction directly prefigures the spec's recommended pipeline
(§10):

```
parser adapter → validated spans → region alignment → feature comparison → artifacts → reports
```

`ab-diff-utils` provides:
- `DiffRegion` — the region classification vocabulary (§5.1)
- `FrequencyTable` — the artifact data structure (§6.2–6.7)
- `FirstDifference` — the character-diff diagnostic (§4.2)
- `ContentHasher` — content-addressed caching for analyzer output

The morphological diff crate (phase two) will use all four. This
extraction ensures the two comparison crates share a vocabulary and
data structures without sharing a core algorithm.
