# Morpheme Diff Core Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build `ab-morph-diff`, a Rust workspace crate that validates morpheme spans, aligns two analyses into deterministic regions, emits feature differences only for one-to-one regions, and derives comparison stats.

**Architecture:** The crate is deliberately analyzer-independent. `model.rs` owns serializable public types, `validate.rs` checks one analysis, `align.rs` builds regions, `features.rs` emits one-to-one feature diffs, `stats.rs` derives counts and boundary metrics, `error.rs` owns typed errors, and `lib.rs` re-exports the stable API.

**Tech Stack:** Rust edition 2024, `serde`, standard-library `BTreeMap`/`BTreeSet`, fixture-driven unit tests, no Vibrato/Sudachi/CLI/aggregation dependencies in this phase.

---

## File Structure

```
crates/ab-morph-diff/            (NEW)
├── Cargo.toml
└── src/
    ├── lib.rs                   // public API and compare_pair orchestration
    ├── model.rs                 // data model + serde derives
    ├── error.rs                 // MorphDiffError
    ├── validate.rs              // validate_analysis
    ├── align.rs                 // region alignment
    ├── features.rs              // one-to-one feature comparison
    └── stats.rs                 // ComparisonStats derivation

Cargo.toml                       (MODIFIED: workspace member + dependency alias)
Cargo.lock                       (MODIFIED by cargo)
```

No adapters, CLI, aggregation implementation, or analyzer dependencies are added in this plan.

---

### Task 1: Create `ab-morph-diff` Crate Scaffolding

**Files:**
- Modify: `Cargo.toml`
- Create: `crates/ab-morph-diff/Cargo.toml`
- Create: `crates/ab-morph-diff/src/lib.rs`

- [ ] **Step 1: Register the crate in the workspace**

In root `Cargo.toml`, add `"crates/ab-morph-diff"` to `[workspace].members` after `"crates/ab-diff-utils"`, and add the workspace dependency entry:

```toml
ab-morph-diff = { path = "crates/ab-morph-diff" }
```

Expected relevant workspace dependency block:

```toml
[workspace.dependencies]
anyhow = "1.0"
ab-diff-utils = { path = "crates/ab-diff-utils" }
ab-morph-diff = { path = "crates/ab-morph-diff" }
ab-ir = { path = "crates/ab-ir" }
```

- [ ] **Step 2: Create `crates/ab-morph-diff/Cargo.toml`**

```toml
[package]
name = "ab-morph-diff"
version.workspace = true
edition.workspace = true
license.workspace = true

[dependencies]
serde.workspace = true
```

Do not add `anyhow`, `thiserror`, `ab-diff-utils`, Vibrato, or Sudachi.

- [ ] **Step 3: Create initial `lib.rs`**

```rust
pub mod align;
pub mod error;
pub mod features;
pub mod model;
pub mod stats;
pub mod validate;

pub use error::MorphDiffError;
pub use model::{
    AlignedMorpheme, Analysis, AnalyzerId, ChangedValue, Comparison, ComparisonStats,
    CoverageMismatch, CoverageMismatchKind, FeatureDiff, FeatureKey, FeatureMap, Morpheme,
    Region, SegmentationDiff, SegmentationKind, TextId,
};
pub use validate::validate_analysis;
```

This does not expose `compare_pair` yet; it is added after the pieces compile.

- [ ] **Step 4: Verify package is discoverable**

Run:

```bash
cargo check -p ab-morph-diff
```

Expected: compile fails only because referenced modules do not exist yet, or succeeds after placeholder modules are added in Task 2. If Cargo says package not found, workspace registration is wrong.

- [ ] **Step 5: Commit**

```bash
git add Cargo.toml crates/ab-morph-diff/Cargo.toml crates/ab-morph-diff/src/lib.rs Cargo.lock
git commit -m "feat(ab-morph-diff): create core crate"
```

---

### Task 2: Add Public Model and Typed Errors

**Files:**
- Create: `crates/ab-morph-diff/src/model.rs`
- Create: `crates/ab-morph-diff/src/error.rs`
- Create placeholders: `align.rs`, `features.rs`, `stats.rs`, `validate.rs`
- Modify: `crates/ab-morph-diff/src/lib.rs`

- [ ] **Step 1: Write `model.rs`**

```rust
use std::collections::BTreeMap;
use std::ops::Range;

use serde::Serialize;

pub type AnalyzerId = String;
pub type TextId = String;
pub type FeatureKey = String;
pub type FeatureMap = BTreeMap<FeatureKey, Option<String>>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Morpheme {
    pub surface: String,
    pub byte_span: Range<usize>,
    pub char_span: Range<usize>,
    pub features: FeatureMap,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Analysis {
    pub analyzer: AnalyzerId,
    pub text_id: TextId,
    pub source_text: String,
    pub morphemes: Vec<Morpheme>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub enum Region {
    OneToOne(AlignedMorpheme),
    Segmentation(SegmentationDiff),
    CoverageMismatch(CoverageMismatch),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct AlignedMorpheme {
    pub text_span: Range<usize>,
    pub from_index: usize,
    pub to_index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SegmentationDiff {
    pub text_span: Range<usize>,
    pub from_indices: Range<usize>,
    pub to_indices: Range<usize>,
    pub from_surfaces: Vec<String>,
    pub to_surfaces: Vec<String>,
    pub kind: SegmentationKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub enum SegmentationKind {
    Split,
    Merge,
    Resegment,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct CoverageMismatch {
    pub text_span: Range<usize>,
    pub from_indices: Range<usize>,
    pub to_indices: Range<usize>,
    pub reason: CoverageMismatchKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub enum CoverageMismatchKind {
    MissingFrom,
    MissingTo,
    UnequalCoverage,
    InvalidInput,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct FeatureDiff {
    pub region_index: usize,
    pub text_span: Range<usize>,
    pub surface: String,
    pub from_index: usize,
    pub to_index: usize,
    pub changed: BTreeMap<FeatureKey, ChangedValue>,
    pub same_context: BTreeMap<FeatureKey, Option<String>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ChangedValue {
    pub from: Option<String>,
    pub to: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Comparison {
    pub from_analyzer: AnalyzerId,
    pub to_analyzer: AnalyzerId,
    pub text_id: TextId,
    pub regions: Vec<Region>,
    pub feature_diffs: Vec<FeatureDiff>,
    pub stats: ComparisonStats,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ComparisonStats {
    pub from_morphemes: usize,
    pub to_morphemes: usize,
    pub one_to_one_regions: usize,
    pub one_to_one_with_feature_differences: usize,
    pub segmentation_regions: usize,
    pub coverage_mismatch_regions: usize,
    pub split_regions: usize,
    pub merge_regions: usize,
    pub resegment_regions: usize,
    pub from_morphemes_in_segmentation: usize,
    pub to_morphemes_in_segmentation: usize,
    pub boundary_precision: Option<f64>,
    pub boundary_recall: Option<f64>,
    pub boundary_f1: Option<f64>,
}
```

- [ ] **Step 2: Write `error.rs`**

```rust
use std::error::Error;
use std::fmt;

use crate::model::{AnalyzerId, TextId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MorphDiffError {
    TextIdMismatch { from: TextId, to: TextId },
    SourceTextMismatch { text_id: TextId },
    OutOfOrderSpan { analyzer: AnalyzerId, text_id: TextId, index: usize },
    OverlappingSpan { analyzer: AnalyzerId, text_id: TextId, previous: usize, current: usize },
    InvalidByteSpan { analyzer: AnalyzerId, text_id: TextId, index: usize },
    CharSpanMismatch { analyzer: AnalyzerId, text_id: TextId, index: usize },
    SurfaceMismatch { analyzer: AnalyzerId, text_id: TextId, index: usize },
}

impl fmt::Display for MorphDiffError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MorphDiffError::TextIdMismatch { from, to } => {
                write!(f, "text id mismatch: from={from}, to={to}")
            }
            MorphDiffError::SourceTextMismatch { text_id } => {
                write!(f, "source text mismatch for text_id={text_id}")
            }
            MorphDiffError::OutOfOrderSpan { analyzer, text_id, index } => {
                write!(f, "out-of-order span: analyzer={analyzer}, text_id={text_id}, index={index}")
            }
            MorphDiffError::OverlappingSpan { analyzer, text_id, previous, current } => {
                write!(f, "overlapping spans: analyzer={analyzer}, text_id={text_id}, previous={previous}, current={current}")
            }
            MorphDiffError::InvalidByteSpan { analyzer, text_id, index } => {
                write!(f, "invalid byte span: analyzer={analyzer}, text_id={text_id}, index={index}")
            }
            MorphDiffError::CharSpanMismatch { analyzer, text_id, index } => {
                write!(f, "char span mismatch: analyzer={analyzer}, text_id={text_id}, index={index}")
            }
            MorphDiffError::SurfaceMismatch { analyzer, text_id, index } => {
                write!(f, "surface/source mismatch: analyzer={analyzer}, text_id={text_id}, index={index}")
            }
        }
    }
}

impl Error for MorphDiffError {}
```

- [ ] **Step 3: Add placeholder modules**

Create `validate.rs`:

```rust
use crate::{Analysis, MorphDiffError};

pub fn validate_analysis(_analysis: &Analysis) -> Result<(), MorphDiffError> {
    Ok(())
}
```

Create `align.rs`:

```rust
use crate::{Analysis, MorphDiffError, Region};

pub fn align_regions(_from: &Analysis, _to: &Analysis) -> Result<Vec<Region>, MorphDiffError> {
    Ok(Vec::new())
}
```

Create `features.rs`:

```rust
use crate::{Analysis, FeatureDiff, FeatureKey, Region};

pub fn compare_feature_diffs(
    _from: &Analysis,
    _to: &Analysis,
    _regions: &[Region],
    _context_keys: &[FeatureKey],
) -> Vec<FeatureDiff> {
    Vec::new()
}
```

Create `stats.rs`:

```rust
use crate::{Analysis, ComparisonStats, FeatureDiff, Region};

pub fn derive_stats(
    from: &Analysis,
    to: &Analysis,
    _regions: &[Region],
    _feature_diffs: &[FeatureDiff],
) -> ComparisonStats {
    ComparisonStats {
        from_morphemes: from.morphemes.len(),
        to_morphemes: to.morphemes.len(),
        one_to_one_regions: 0,
        one_to_one_with_feature_differences: 0,
        segmentation_regions: 0,
        coverage_mismatch_regions: 0,
        split_regions: 0,
        merge_regions: 0,
        resegment_regions: 0,
        from_morphemes_in_segmentation: 0,
        to_morphemes_in_segmentation: 0,
        boundary_precision: None,
        boundary_recall: None,
        boundary_f1: None,
    }
}
```

- [ ] **Step 4: Run model compile check**

Run:

```bash
cargo check -p ab-morph-diff
```

Expected: compiles.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-diff/src Cargo.toml Cargo.lock
git commit -m "feat(ab-morph-diff): add core model types"
```

---

### Task 3: Implement Analysis Validation

**Files:**
- Modify: `crates/ab-morph-diff/src/validate.rs`

- [ ] **Step 1: Replace `validate.rs` with validation logic and tests**

```rust
use crate::{Analysis, MorphDiffError};

pub fn validate_analysis(analysis: &Analysis) -> Result<(), MorphDiffError> {
    let mut previous_end = 0usize;
    for (index, morpheme) in analysis.morphemes.iter().enumerate() {
        if morpheme.byte_span.start > morpheme.byte_span.end
            || morpheme.byte_span.end > analysis.source_text.len()
            || !analysis.source_text.is_char_boundary(morpheme.byte_span.start)
            || !analysis.source_text.is_char_boundary(morpheme.byte_span.end)
        {
            return Err(MorphDiffError::InvalidByteSpan {
                analyzer: analysis.analyzer.clone(),
                text_id: analysis.text_id.clone(),
                index,
            });
        }
        if index > 0 && morpheme.byte_span.start < previous_end {
            return Err(MorphDiffError::OverlappingSpan {
                analyzer: analysis.analyzer.clone(),
                text_id: analysis.text_id.clone(),
                previous: index - 1,
                current: index,
            });
        }
        if index > 0 && morpheme.byte_span.start < analysis.morphemes[index - 1].byte_span.start {
            return Err(MorphDiffError::OutOfOrderSpan {
                analyzer: analysis.analyzer.clone(),
                text_id: analysis.text_id.clone(),
                index,
            });
        }
        let expected_char_span = byte_span_to_char_span(&analysis.source_text, morpheme.byte_span.clone())
            .ok_or_else(|| MorphDiffError::InvalidByteSpan {
                analyzer: analysis.analyzer.clone(),
                text_id: analysis.text_id.clone(),
                index,
            })?;
        if expected_char_span != morpheme.char_span {
            return Err(MorphDiffError::CharSpanMismatch {
                analyzer: analysis.analyzer.clone(),
                text_id: analysis.text_id.clone(),
                index,
            });
        }
        if &analysis.source_text[morpheme.byte_span.clone()] != morpheme.surface.as_str() {
            return Err(MorphDiffError::SurfaceMismatch {
                analyzer: analysis.analyzer.clone(),
                text_id: analysis.text_id.clone(),
                index,
            });
        }
        previous_end = morpheme.byte_span.end;
    }
    Ok(())
}

fn byte_span_to_char_span(source: &str, byte_span: std::ops::Range<usize>) -> Option<std::ops::Range<usize>> {
    if byte_span.start > byte_span.end
        || byte_span.end > source.len()
        || !source.is_char_boundary(byte_span.start)
        || !source.is_char_boundary(byte_span.end)
    {
        return None;
    }
    let start = source[..byte_span.start].chars().count();
    let end = source[..byte_span.end].chars().count();
    Some(start..end)
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::{Analysis, Morpheme, MorphDiffError};

    use super::validate_analysis;

    fn m(source: &str, surface: &str, byte_start: usize, byte_end: usize) -> Morpheme {
        let char_start = source[..byte_start].chars().count();
        let char_end = source[..byte_end].chars().count();
        Morpheme {
            surface: surface.to_owned(),
            byte_span: byte_start..byte_end,
            char_span: char_start..char_end,
            features: BTreeMap::new(),
        }
    }

    fn analysis(morphemes: Vec<Morpheme>) -> Analysis {
        Analysis {
            analyzer: "a".to_owned(),
            text_id: "t".to_owned(),
            source_text: "今日はabc".to_owned(),
            morphemes,
        }
    }

    #[test]
    fn accepts_touching_spans() {
        let a = analysis(vec![
            m("今日はabc", "今日", 0, 6),
            m("今日はabc", "は", 6, 9),
            m("今日はabc", "abc", 9, 12),
        ]);
        assert!(validate_analysis(&a).is_ok());
    }

    #[test]
    fn rejects_overlapping_spans() {
        let a = analysis(vec![
            m("今日はabc", "今日", 0, 6),
            Morpheme { surface: "日は".to_owned(), byte_span: 3..9, char_span: 1..3, features: BTreeMap::new() },
        ]);
        assert!(matches!(validate_analysis(&a), Err(MorphDiffError::OverlappingSpan { .. })));
    }

    #[test]
    fn rejects_non_char_boundary_byte_span() {
        let a = analysis(vec![Morpheme {
            surface: "今".to_owned(),
            byte_span: 0..1,
            char_span: 0..1,
            features: BTreeMap::new(),
        }]);
        assert!(matches!(validate_analysis(&a), Err(MorphDiffError::InvalidByteSpan { .. })));
    }

    #[test]
    fn rejects_char_span_mismatch() {
        let a = analysis(vec![Morpheme {
            surface: "今日".to_owned(),
            byte_span: 0..6,
            char_span: 0..1,
            features: BTreeMap::new(),
        }]);
        assert!(matches!(validate_analysis(&a), Err(MorphDiffError::CharSpanMismatch { .. })));
    }

    #[test]
    fn rejects_surface_source_mismatch() {
        let a = analysis(vec![Morpheme {
            surface: "明日".to_owned(),
            byte_span: 0..6,
            char_span: 0..2,
            features: BTreeMap::new(),
        }]);
        assert!(matches!(validate_analysis(&a), Err(MorphDiffError::SurfaceMismatch { .. })));
    }
}
```

- [ ] **Step 2: Run validation tests**

```bash
cargo test -p ab-morph-diff validate
```

Expected: validation tests pass.

- [ ] **Step 3: Commit**

```bash
git add crates/ab-morph-diff/src/validate.rs
git commit -m "feat(ab-morph-diff): validate analysis spans"
```

---

### Task 4: Implement Region Alignment

**Files:**
- Modify: `crates/ab-morph-diff/src/align.rs`

- [ ] **Step 1: Replace `align.rs` with span alignment implementation and tests**

Use this implementation skeleton. It intentionally prefers clear region growth over clever diffing.

```rust
use std::ops::Range;

use crate::{
    AlignedMorpheme, Analysis, CoverageMismatch, CoverageMismatchKind, MorphDiffError, Region,
    SegmentationDiff, SegmentationKind,
};

pub fn align_regions(from: &Analysis, to: &Analysis) -> Result<Vec<Region>, MorphDiffError> {
    let mut regions = Vec::new();
    let mut i = 0usize;
    let mut j = 0usize;
    let source_len = from.source_text.chars().count();

    while i < from.morphemes.len() || j < to.morphemes.len() {
        if i < from.morphemes.len() && j < to.morphemes.len() {
            let left = &from.morphemes[i];
            let right = &to.morphemes[j];
            if left.char_span == right.char_span {
                regions.push(Region::OneToOne(AlignedMorpheme {
                    text_span: left.char_span.clone(),
                    from_index: i,
                    to_index: j,
                }));
                i += 1;
                j += 1;
                continue;
            }
        }

        let region_start = next_start(from, to, i, j, source_len);
        let mut region_end = next_end(from, to, i, j, source_len);
        let from_start = i;
        let to_start = j;

        loop {
            let old_i = i;
            let old_j = j;
            while i < from.morphemes.len() && from.morphemes[i].char_span.start < region_end {
                region_end = region_end.max(from.morphemes[i].char_span.end);
                i += 1;
            }
            while j < to.morphemes.len() && to.morphemes[j].char_span.start < region_end {
                region_end = region_end.max(to.morphemes[j].char_span.end);
                j += 1;
            }
            if old_i == i && old_j == j {
                break;
            }
        }

        if from_start == i && to_start < j {
            regions.push(Region::CoverageMismatch(CoverageMismatch {
                text_span: region_start..region_end,
                from_indices: from_start..from_start,
                to_indices: to_start..j,
                reason: CoverageMismatchKind::MissingFrom,
            }));
        } else if to_start == j && from_start < i {
            regions.push(Region::CoverageMismatch(CoverageMismatch {
                text_span: region_start..region_end,
                from_indices: from_start..i,
                to_indices: to_start..to_start,
                reason: CoverageMismatchKind::MissingTo,
            }));
        } else if covers_exactly(from, from_start..i, region_start..region_end)
            && covers_exactly(to, to_start..j, region_start..region_end)
        {
            regions.push(Region::Segmentation(SegmentationDiff {
                text_span: region_start..region_end,
                from_indices: from_start..i,
                to_indices: to_start..j,
                from_surfaces: from.morphemes[from_start..i]
                    .iter()
                    .map(|m| m.surface.clone())
                    .collect(),
                to_surfaces: to.morphemes[to_start..j]
                    .iter()
                    .map(|m| m.surface.clone())
                    .collect(),
                kind: segmentation_kind(i - from_start, j - to_start),
            }));
        } else {
            regions.push(Region::CoverageMismatch(CoverageMismatch {
                text_span: region_start..region_end,
                from_indices: from_start..i,
                to_indices: to_start..j,
                reason: CoverageMismatchKind::UnequalCoverage,
            }));
        }
    }

    Ok(regions)
}

fn next_start(from: &Analysis, to: &Analysis, i: usize, j: usize, source_len: usize) -> usize {
    match (from.morphemes.get(i), to.morphemes.get(j)) {
        (Some(a), Some(b)) => a.char_span.start.min(b.char_span.start),
        (Some(a), None) => a.char_span.start,
        (None, Some(b)) => b.char_span.start,
        (None, None) => source_len,
    }
}

fn next_end(from: &Analysis, to: &Analysis, i: usize, j: usize, source_len: usize) -> usize {
    match (from.morphemes.get(i), to.morphemes.get(j)) {
        (Some(a), Some(b)) => a.char_span.end.min(b.char_span.end).max(next_start(from, to, i, j, source_len) + 1),
        (Some(a), None) => a.char_span.end,
        (None, Some(b)) => b.char_span.end,
        (None, None) => source_len,
    }
}

fn covers_exactly(analysis: &Analysis, indices: Range<usize>, span: Range<usize>) -> bool {
    if indices.is_empty() {
        return false;
    }
    let first = &analysis.morphemes[indices.start];
    let last = &analysis.morphemes[indices.end - 1];
    first.char_span.start == span.start && last.char_span.end == span.end
}

fn segmentation_kind(from_count: usize, to_count: usize) -> SegmentationKind {
    match (from_count, to_count) {
        (1, n) if n > 1 => SegmentationKind::Split,
        (n, 1) if n > 1 => SegmentationKind::Merge,
        _ => SegmentationKind::Resegment,
    }
}
```

Add tests in the same module with helper constructors for:

```rust
#[test]
fn split_region_has_no_feature_diff_shape() { /* 今日 vs 今+日 */ }

#[test]
fn merge_region_is_classified() { /* で+は vs では */ }

#[test]
fn resegment_region_is_classified() { /* abc+def vs ab+cdef */ }

#[test]
fn repeated_surfaces_align_by_span() { /* は は with different spans */ }

#[test]
fn one_side_zero_morphemes_for_non_empty_text_is_missing_from_or_to() { /* empty vs 本文 */ }

#[test]
fn multiple_segmentation_regions_are_separate() { /* 今日Xでは */ }
```

Use exact assertions on `Region` values, not only counts.

- [ ] **Step 2: Run alignment tests**

```bash
cargo test -p ab-morph-diff align
```

Expected: alignment tests pass.

- [ ] **Step 3: Commit**

```bash
git add crates/ab-morph-diff/src/align.rs
git commit -m "feat(ab-morph-diff): align morpheme regions"
```

---

### Task 5: Implement Feature Diffing

**Files:**
- Modify: `crates/ab-morph-diff/src/features.rs`

- [ ] **Step 1: Replace `features.rs` with feature comparison implementation and tests**

```rust
use std::collections::{BTreeMap, BTreeSet};

use crate::{Analysis, ChangedValue, FeatureDiff, FeatureKey, Region};

pub fn compare_feature_diffs(
    from: &Analysis,
    to: &Analysis,
    regions: &[Region],
    context_keys: &[FeatureKey],
) -> Vec<FeatureDiff> {
    let context_keys = context_keys.iter().cloned().collect::<BTreeSet<_>>();
    let mut diffs = Vec::new();

    for (region_index, region) in regions.iter().enumerate() {
        let Region::OneToOne(aligned) = region else {
            continue;
        };
        let from_m = &from.morphemes[aligned.from_index];
        let to_m = &to.morphemes[aligned.to_index];
        let keys = from_m
            .features
            .keys()
            .chain(to_m.features.keys())
            .cloned()
            .collect::<BTreeSet<_>>();
        let mut changed = BTreeMap::new();
        let mut same_context = BTreeMap::new();
        for key in keys {
            let from_value = from_m.features.get(&key).cloned().unwrap_or(None);
            let to_value = to_m.features.get(&key).cloned().unwrap_or(None);
            if from_value != to_value {
                changed.insert(key, ChangedValue { from: from_value, to: to_value });
            } else if context_keys.contains(&key) {
                same_context.insert(key, from_value);
            }
        }
        if !changed.is_empty() {
            diffs.push(FeatureDiff {
                region_index,
                text_span: aligned.text_span.clone(),
                surface: from_m.surface.clone(),
                from_index: aligned.from_index,
                to_index: aligned.to_index,
                changed,
                same_context,
            });
        }
    }

    diffs
}
```

Add tests for:

```rust
#[test]
fn identical_features_emit_no_diff() { /* same POS/lemma */ }

#[test]
fn changed_pos_emits_feature_diff_with_region_index() { /* pos noun -> verb */ }

#[test]
fn missing_feature_is_explicit_none() { /* lemma Some -> None */ }

#[test]
fn context_keys_are_deduplicated_and_sorted() { /* duplicate context key input */ }

#[test]
fn segmentation_regions_do_not_emit_feature_diffs() { /* Region::Segmentation input */ }
```

- [ ] **Step 2: Run feature tests**

```bash
cargo test -p ab-morph-diff features
```

Expected: feature tests pass.

- [ ] **Step 3: Commit**

```bash
git add crates/ab-morph-diff/src/features.rs
git commit -m "feat(ab-morph-diff): compare aligned morpheme features"
```

---

### Task 6: Implement Stats and Boundary Metrics

**Files:**
- Modify: `crates/ab-morph-diff/src/stats.rs`

- [ ] **Step 1: Replace `stats.rs` with stats implementation and tests**

Implementation requirements:

- Count region kinds from `regions`.
- Count `one_to_one_with_feature_differences` by unique `FeatureDiff.region_index`.
- Count split/merge/resegment from segmentation regions.
- Count morphemes in segmentation from each side.
- Compute internal boundary sets from morpheme char spans, excluding `0` and source length.
- Remove boundaries whose offset lies inside any `CoverageMismatch.text_span`.
- Return `None` for precision/recall/F1 when the relevant denominator is zero.

Use this denominator behavior:

```rust
fn ratio(numerator: usize, denominator: usize) -> Option<f64> {
    (denominator != 0).then_some(numerator as f64 / denominator as f64)
}
```

F1 is `None` if either precision or recall is `None`, or if `precision + recall == 0.0`.

Add tests for:

```rust
#[test]
fn counts_region_kinds_and_segmentation_subtypes() { /* one split, one merge, one resegment */ }

#[test]
fn counts_unique_one_to_one_regions_with_feature_diffs() { /* two diffs same region count once */ }

#[test]
fn boundary_metrics_exclude_outer_boundaries() { /* 今日 は vs 今日 は */ }

#[test]
fn boundary_metrics_ignore_coverage_mismatch_spans() { /* mismatch region contains extra boundary */ }

#[test]
fn boundary_metrics_are_none_when_no_comparable_internal_boundaries() { /* single token each */ }
```

- [ ] **Step 2: Run stats tests**

```bash
cargo test -p ab-morph-diff stats
```

Expected: stats tests pass.

- [ ] **Step 3: Commit**

```bash
git add crates/ab-morph-diff/src/stats.rs
git commit -m "feat(ab-morph-diff): derive comparison stats"
```

---

### Task 7: Add `compare_pair` Orchestration

**Files:**
- Modify: `crates/ab-morph-diff/src/lib.rs`

- [ ] **Step 1: Add public `compare_pair`**

Append to `lib.rs`:

```rust
pub use align::align_regions;
pub use features::compare_feature_diffs;
pub use stats::derive_stats;

pub fn compare_pair(
    from: &Analysis,
    to: &Analysis,
    feature_context_keys: &[FeatureKey],
) -> Result<Comparison, MorphDiffError> {
    validate_analysis(from)?;
    validate_analysis(to)?;
    if from.text_id != to.text_id {
        return Err(MorphDiffError::TextIdMismatch {
            from: from.text_id.clone(),
            to: to.text_id.clone(),
        });
    }
    if from.source_text != to.source_text {
        return Err(MorphDiffError::SourceTextMismatch {
            text_id: from.text_id.clone(),
        });
    }

    let regions = align_regions(from, to)?;
    let feature_diffs = compare_feature_diffs(from, to, &regions, feature_context_keys);
    let stats = derive_stats(from, to, &regions, &feature_diffs);
    Ok(Comparison {
        from_analyzer: from.analyzer.clone(),
        to_analyzer: to.analyzer.clone(),
        text_id: from.text_id.clone(),
        regions,
        feature_diffs,
        stats,
    })
}
```

- [ ] **Step 2: Add integration-style unit tests in `lib.rs`**

Add a `#[cfg(test)] mod tests` with helpers and tests for:

```rust
#[test]
fn compare_pair_rejects_text_id_mismatch() { /* t1 vs t2 */ }

#[test]
fn compare_pair_rejects_source_text_mismatch() { /* same id, different source */ }

#[test]
fn compare_pair_produces_feature_diff_adjacent_to_segmentation_region() { /* 今日は */ }

#[test]
fn compare_pair_handles_empty_input() { /* source_text empty, no morphemes */ }

#[test]
fn compare_pair_handles_punctuation_only_input() { /* 。 */ }
```

- [ ] **Step 3: Run full crate tests**

```bash
cargo test -p ab-morph-diff
```

Expected: all `ab-morph-diff` tests pass.

- [ ] **Step 4: Commit**

```bash
git add crates/ab-morph-diff/src/lib.rs
git commit -m "feat(ab-morph-diff): add pairwise comparison API"
```

---

### Task 8: Add Alignment Property Tests

**Files:**
- Modify: `crates/ab-morph-diff/Cargo.toml`
- Create: `crates/ab-morph-diff/tests/alignment_properties.rs`

- [ ] **Step 1: Add dev dependency**

In `crates/ab-morph-diff/Cargo.toml`:

```toml
[dev-dependencies]
proptest = "1.5"
```

Use a direct crate version here only if `proptest` is not already in workspace dependencies. If it is added to workspace later, convert this to `proptest.workspace = true`.

- [ ] **Step 2: Add property tests**

Create `tests/alignment_properties.rs`:

```rust
use std::collections::BTreeMap;

use ab_morph_diff::{compare_pair, Analysis, Morpheme, Region};
use proptest::prelude::*;

fn analysis(analyzer: &str, text: &str, cuts: &[usize]) -> Analysis {
    let mut morphemes = Vec::new();
    for pair in cuts.windows(2) {
        let start_char = pair[0];
        let end_char = pair[1];
        let start_byte = text.char_indices().nth(start_char).map(|(i, _)| i).unwrap_or(text.len());
        let end_byte = text.char_indices().nth(end_char).map(|(i, _)| i).unwrap_or(text.len());
        morphemes.push(Morpheme {
            surface: text[start_byte..end_byte].to_owned(),
            byte_span: start_byte..end_byte,
            char_span: start_char..end_char,
            features: BTreeMap::new(),
        });
    }
    Analysis {
        analyzer: analyzer.to_owned(),
        text_id: "t".to_owned(),
        source_text: text.to_owned(),
        morphemes,
    }
}

fn cuts_from_mask(len: usize, mask: u16) -> Vec<usize> {
    let mut cuts = vec![0];
    for i in 1..len {
        if (mask & (1 << (i - 1))) != 0 {
            cuts.push(i);
        }
    }
    cuts.push(len);
    cuts
}

proptest! {
    #[test]
    fn regions_are_sorted_and_non_overlapping(a_mask in 0u16..256, b_mask in 0u16..256) {
        let text = "abcdefgh";
        let a = analysis("a", text, &cuts_from_mask(8, a_mask));
        let b = analysis("b", text, &cuts_from_mask(8, b_mask));
        let comparison = compare_pair(&a, &b, &[]).unwrap();
        let mut previous_end = 0usize;
        for region in &comparison.regions {
            let span = match region {
                Region::OneToOne(r) => r.text_span.clone(),
                Region::Segmentation(r) => r.text_span.clone(),
                Region::CoverageMismatch(r) => r.text_span.clone(),
            };
            prop_assert!(span.start >= previous_end);
            prop_assert!(span.start <= span.end);
            previous_end = span.end;
        }
    }

    #[test]
    fn feature_diffs_only_reference_one_to_one_regions(a_mask in 0u16..256, b_mask in 0u16..256) {
        let text = "abcdefgh";
        let a = analysis("a", text, &cuts_from_mask(8, a_mask));
        let b = analysis("b", text, &cuts_from_mask(8, b_mask));
        let comparison = compare_pair(&a, &b, &[]).unwrap();
        for diff in &comparison.feature_diffs {
            prop_assert!(matches!(comparison.regions.get(diff.region_index), Some(Region::OneToOne(_))));
        }
    }
}
```

- [ ] **Step 3: Run property tests**

```bash
cargo test -p ab-morph-diff --test alignment_properties
```

Expected: property tests pass.

- [ ] **Step 4: Commit**

```bash
git add crates/ab-morph-diff/Cargo.toml crates/ab-morph-diff/tests/alignment_properties.rs Cargo.lock
git commit -m "test(ab-morph-diff): add alignment property tests"
```

---

### Task 9: Final Verification

- [ ] **Step 1: Format check**

```bash
cargo fmt --all -- --check
```

Expected: no formatting changes needed.

- [ ] **Step 2: Clippy**

```bash
cargo clippy --workspace --all-targets -- -D warnings
```

Expected: no warnings.

- [ ] **Step 3: Full workspace test**

```bash
cargo test --workspace
```

Expected: all workspace tests pass.

- [ ] **Step 4: Documentation build**

```bash
cargo doc --no-deps --workspace
```

Expected: command exits 0. Existing rustdoc warnings outside `ab-morph-diff` may appear; do not fix unrelated warnings in this plan unless the user explicitly asks.

- [ ] **Step 5: Commit verification-only fixes if needed**

If formatting or lint changes were required:

```bash
git add -u
git commit -m "chore: finalize morph diff core"
```

---

## Plan Self-Review

Spec coverage:

- Core data model: Task 2.
- Typed errors and no `anyhow`: Task 2.
- Validation: Task 3.
- Region alignment and coverage mismatch anchors: Task 4.
- Feature diffs with `region_index`: Task 5.
- Stats and boundary metrics under coverage mismatch: Task 6.
- Public `compare_pair` preconditions: Task 7.
- Property tests for alignment invariants: Task 8.
- No adapters, CLI, or aggregation implementation: maintained by file structure and dependencies.

Placeholder scan:

- No `TODO`/`TBD` placeholders are required for implementation.
- Test bullets in Tasks 4-7 describe exact cases; implementation agents should write exact assertions based on model types.

Type consistency:

- `MorphDiffError`, `Comparison`, `FeatureDiff.region_index`, and all region types match the design spec.
- Aggregation types are intentionally absent from phase-1 implementation tasks.
