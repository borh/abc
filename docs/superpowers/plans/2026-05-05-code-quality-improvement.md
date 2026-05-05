# Code Quality Improvement Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Four independent workstreams addressing structural quality issues in ab-validator: (1) ab-ir visitor pattern, (2) ab-morph-run god crate split, (3) interning + robustness, (4) clippy sweep.

**Architecture:** Streams 1–3 are independent with minimal file overlap. Stream 4 runs last as a polish pass. Each stream produces a working, testable state before the next begins.

**Tech Stack:** Rust edition 2024, cargo workspace, serde, arrow/parquet, anyhow, clap.

---

## File Structure

### Stream 1: ab-ir visitor + cleanup
```
crates/ab-ir/src/lib.rs                    [modify] — visitor trait, walk_inline fn, Break cleanup, delete old walks
crates/ab-ir/src/semantic_summary.rs       [modify] — use visitor instead of duplicate walks
crates/ab-morph-diff/src/model.rs          [modify] — remove Index<&str> for FeatureMap
crates/ab-morph-analyzers/src/features.rs  [modify] — features["key"] → features.get("key") in tests
```

### Stream 2: god crate split
```
crates/ab-morph-run/src/
  lib.rs              [modify]   — slim re-exports
  pipeline.rs         [create]   — run_analyze_aat, serial/parallel dispatch
  options.rs          [create]   — SerialRunOptions, WarehouseRunOptions
  summary/
    mod.rs            [create]   — re-exports
    types.rs          [create]   — all 22 public enums/structs from top of summary.rs
    compact.rs        [create]   — summarize_compact_*
    nway.rs           [create]   — summarize_nway*
    warehouse.rs      [create]   — summarize_warehouse_*
    patterns.rs       [create]   — pattern types + helpers
    write.rs          [create]   — write_*_duckdb_tsv
  main.rs             [modify]   — simplify CLI arg → library conversions
crates/ab-warehouse/  [create]   — new crate
  Cargo.toml
  src/lib.rs
  src/schema.rs       [move from warehouse/schema.rs]
  src/rows.rs         [move from warehouse/rows.rs]
  src/writer.rs       [move from warehouse/writer.rs]
  src/sql.rs          [move from warehouse/sql.rs]
Cargo.toml            [modify]   — add ab-warehouse member
```

### Stream 3: interning removal + robustness
```
crates/ab-morph-diff/src/model.rs          [modify] — remove InternedString, use Arc<str>
crates/ab-morph-diff/src/                  [modify] — update as_str() → &*val callers
crates/ab-morph-run/src/                   [modify] — update callers, warnings → structured
crates/ab-morph-analyzers/src/vibrato.rs   [modify] — warnings → structured, panic → Result
crates/ab-morph-analyzers/src/vaporetto.rs [modify] — panic → Result
crates/ab-morph-analyzers/src/sudachi.rs   [modify] — warnings → structured
crates/ab-morph-diff/src/model.rs          [modify] — add warnings field to Analysis struct (no trait change)
crates/ab-coverage/src/adapter.rs          [modify] — panic → Result
```

### Stream 4: clippy sweep
```
crates/*/Cargo.toml     [modify] — add description, repository, keywords, categories
crates/ab-ir/src/       [modify] — #[must_use] on constructors, # Errors docs
crates/ab-morph-diff/   [modify] — #[must_use], # Errors docs
crates/ab-morph-run/    [modify] — #[must_use], # Errors docs
crates/ab-index/src/features.rs [modify] — needless_raw_string_hashes
crates/ab-diff-utils/   [modify] — unnest or-patterns
crates/ab-coverage/     [modify] — unnest or-patterns
```

---

## Stream 1: ab-ir Visitor + Design Debt Cleanup

### Task 1.1: Add InlineVisitor trait and walk_inline function

**Files:**
- Modify: `crates/ab-ir/src/lib.rs` (~after Inline enum, before existing walk functions)

- [x] **Step 1: Add the InlineVisitor trait after the Inline enum definition**

Insert after the closing `}` of `pub enum Inline { ... }` (before `#[derive(Debug, Clone, PartialEq, Eq)] pub struct StyleAttr`):

```rust
/// Visitor for walking the Inline tree. Every method has a default no-op
/// implementation so visitors only override the variants they care about.
pub trait InlineVisitor {
    // Leaf nodes — enter only
    fn enter_text(&mut self, _value: &str, _provenance: &Provenance) {}
    fn enter_text_meta(&mut self, _value: &str, _attrs: &[StyleAttr], _provenance: &Provenance) {}
    fn enter_gaiji_ref(&mut self, _gaiji: &GaijiRef) {}
    fn enter_accent(&mut self, _resolved: &str, _provenance: &Provenance) {}
    fn enter_editor_note(&mut self, _note: &str, _provenance: &Provenance) {}
    fn enter_raw(&mut self, _source: &str, _attrs: &[StyleAttr], _provenance: &Provenance) {}

    // Container nodes — enter/leave pair
    fn enter_ruby(&mut self, _base: &[Inline], _reading: &str, _placement: &RubyPlacement, _provenance: &Provenance) {}
    fn leave_ruby(&mut self) {}
    fn enter_style(&mut self, _style_type: &str, _content: &[Inline], _attrs: &[StyleAttr], _provenance: &Provenance) {}
    fn leave_style(&mut self) {}
    fn enter_scope(&mut self, _kind: &str, _content: &[Inline], _provenance: &Provenance) {}
    fn leave_scope(&mut self) {}
    fn enter_font_size(&mut self, _size_type: &str, _level: u8, _content: &[Inline], _provenance: &Provenance) {}
    fn leave_font_size(&mut self) {}
    fn enter_warigaki(&mut self, _upper: &[Inline], _lower: &[Inline], _provenance: &Provenance) {}
    fn leave_warigaki(&mut self) {}
    fn enter_figure_ref(&mut self, _source: &str, _caption: &[Inline], _provenance: &Provenance) {}
    fn leave_figure_ref(&mut self) {}
}

/// Walk an Inline tree with a visitor. Monomorphized per concrete V.
pub fn walk_inline<V: InlineVisitor>(visitor: &mut V, node: &Inline) {
    match node {
        Inline::Text { value, provenance } => {
            visitor.enter_text(value, provenance);
        }
        Inline::TextMeta { value, attrs, provenance } => {
            visitor.enter_text_meta(value, &attrs, provenance);
        }
        Inline::GaijiRef(gaiji) => {
            visitor.enter_gaiji_ref(gaiji);
        }
        Inline::Ruby { base, reading, placement, provenance, .. } => {
            visitor.enter_ruby(base, reading, placement, provenance);
            for child in base {
                walk_inline(visitor, child);
            }
            visitor.leave_ruby();
        }
        Inline::Style { style_type, content, attrs, provenance, .. } => {
            visitor.enter_style(style_type, content, &attrs, provenance);
            for child in content {
                walk_inline(visitor, child);
            }
            visitor.leave_style();
        }
        Inline::Scope { kind, content, provenance, .. } => {
            visitor.enter_scope(kind, content, provenance);
            for child in content {
                walk_inline(visitor, child);
            }
            visitor.leave_scope();
        }
        Inline::FontSize { size_type, level, content, provenance, .. } => {
            visitor.enter_font_size(size_type, *level, content, provenance);
            for child in content {
                walk_inline(visitor, child);
            }
            visitor.leave_font_size();
        }
        Inline::Warigaki { upper, lower, provenance, .. } => {
            visitor.enter_warigaki(upper, lower, provenance);
            for child in upper.iter().chain(lower.iter()) {
                walk_inline(visitor, child);
            }
            visitor.leave_warigaki();
        }
        Inline::FigureRef { source, caption, provenance, .. } => {
            visitor.enter_figure_ref(source, caption, provenance);
            for child in caption {
                walk_inline(visitor, child);
            }
            visitor.leave_figure_ref();
        }
        Inline::Accent { resolved, provenance, .. } => {
            visitor.enter_accent(resolved, provenance);
        }
        Inline::EditorNote { note, provenance } => {
            visitor.enter_editor_note(note, provenance);
        }
        Inline::Raw { source, attrs, provenance } => {
            visitor.enter_raw(source, &attrs, provenance);
        }
    }
}
```

- [x] **Step 2: Build and verify compilation**

Run: `cargo build -p ab-ir`
Expected: compiles cleanly (trait + fn added, nothing broken)

- [x] **Step 3: Commit**

```bash
git add crates/ab-ir/src/lib.rs
git commit -m "feat(ab-ir): add InlineVisitor trait and walk_inline function"
```

---

### Task 1.2: Replace collect_visible in lib.rs with VisibleCollector visitor

**Files:**
- Modify: `crates/ab-ir/src/lib.rs` (~lines 814–845 for collect_visible, ~905–911 for inline_visible_text)

- [x] **Step 1: Add VisibleCollector visitor struct**

Place after the trait+fn added in Task 1.1:

```rust
/// Collects visible text from an Inline tree.
struct VisibleCollector {
    pub out: String,
}

impl InlineVisitor for VisibleCollector {
    fn enter_text(&mut self, value: &str, _: &Provenance) {
        self.out.push_str(value);
    }
    fn enter_text_meta(&mut self, value: &str, _: &[StyleAttr], _: &Provenance) {
        self.out.push_str(value);
    }
    fn enter_gaiji_ref(&mut self, gaiji: &GaijiRef) {
        if let Some(resolved) = &gaiji.resolved {
            self.out.push_str(resolved);
        }
    }
    fn enter_accent(&mut self, resolved: &str, _: &Provenance) {
        self.out.push_str(resolved);
    }
}
```

- [x] **Step 2: Replace the old `fn collect_visible` with a delegating version**

Find the existing `fn collect_visible(value: &Inline, out: &mut String)` at ~line 814. Replace the entire function body:

```rust
fn collect_visible(value: &Inline, out: &mut String) {
    struct Vis<'a>(&'a mut String);
    impl InlineVisitor for Vis<'_> {
        fn enter_text(&mut self, v: &str, _: &Provenance) { self.0.push_str(v); }
        fn enter_text_meta(&mut self, v: &str, _: &[StyleAttr], _: &Provenance) { self.0.push_str(v); }
        fn enter_gaiji_ref(&mut self, g: &GaijiRef) {
            if let Some(r) = &g.resolved { self.0.push_str(r); }
        }
        fn enter_accent(&mut self, r: &str, _: &Provenance) { self.0.push_str(r); }
    }
    walk_inline(&mut Vis(out), value);
}
```

This preserves the function signature for `inline_visible_text` and `visible_projection` which still call it. No allocation overhead — `&mut String` is borrowed directly, avoiding `std::mem::take` (which would clear accumulated text on every call, causing data loss in loops like `visible_projection`).

- [x] **Step 3: Run tests**

Run: `cargo test -p ab-ir`
Expected: all tests pass (visible_projection tests exercise collect_visible)

- [x] **Step 4: Commit**

```bash
git add crates/ab-ir/src/lib.rs
git commit -m "refactor(ab-ir): replace collect_visible body with VisibleCollector visitor"
```

---

### Task 1.3: Replace contains_unresolved_gaiji with GaijiChecker visitor

**Files:**
- Modify: `crates/ab-ir/src/lib.rs` (~lines 795–811)

- [x] **Step 1: Add GaijiChecker visitor**

```rust
struct GaijiChecker {
    unresolved_only: bool,
    pub found: bool,
}

impl InlineVisitor for GaijiChecker {
    fn enter_gaiji_ref(&mut self, gaiji: &GaijiRef) {
        if !self.unresolved_only || gaiji.resolved.is_none() {
            self.found = true;
        }
    }
}
```

- [x] **Step 2: Replace `fn contains_unresolved_gaiji` body**

```rust
fn contains_unresolved_gaiji(content: &[Inline]) -> bool {
    let mut checker = GaijiChecker {
        unresolved_only: true,
        found: false,
    };
    for node in content {
        walk_inline(&mut checker, node);
        if checker.found {
            return true;
        }
    }
    false
}
```

- [x] **Step 3: Run tests**

Run: `cargo test -p ab-ir`
Expected: all tests pass

- [x] **Step 4: Commit**

```bash
git add crates/ab-ir/src/lib.rs
git commit -m "refactor(ab-ir): replace contains_unresolved_gaiji with GaijiChecker visitor"
```

---

### Task 1.4: Replace collect_provenance + inline_provenance with ProvenanceCollector visitor

**Files:**
- Modify: `crates/ab-ir/src/lib.rs` (~lines 849–891)

- [x] **Step 1: Add ProvenanceCounts::increment helper**

```rust
impl ProvenanceCounts {
    fn increment(&mut self, provenance: &Provenance) {
        match provenance {
            Provenance::Parser => self.parser += 1,
            Provenance::ParserNormalized => self.parser_normalized += 1,
            Provenance::SourceSupplement => self.source_supplement += 1,
            Provenance::SourceFallback => self.source_fallback += 1,
        }
    }
}
```

- [x] **Step 2: Replace `fn collect_provenance` body**

Same borrow-based approach as `collect_visible` — no `std::mem::take` (which would zero accumulated counts on every call in the loop).

```rust
fn collect_provenance(value: &Inline, counts: &mut ProvenanceCounts) {
    struct Vis<'a>(&'a mut ProvenanceCounts);
    impl InlineVisitor for Vis<'_> {
        fn enter_text(&mut self, _: &str, p: &Provenance) { self.0.increment(p); }
        fn enter_text_meta(&mut self, _: &str, _: &[StyleAttr], p: &Provenance) { self.0.increment(p); }
        fn enter_gaiji_ref(&mut self, g: &GaijiRef) { self.0.increment(&g.provenance); }
        fn enter_accent(&mut self, _: &str, p: &Provenance) { self.0.increment(p); }
        fn enter_editor_note(&mut self, _: &str, p: &Provenance) { self.0.increment(p); }
        fn enter_raw(&mut self, _: &str, _: &[StyleAttr], p: &Provenance) { self.0.increment(p); }
        fn enter_ruby(&mut self, _: &[Inline], _: &str, _: &RubyPlacement, p: &Provenance) { self.0.increment(p); }
        fn enter_style(&mut self, _: &str, _: &[Inline], _: &[StyleAttr], p: &Provenance) { self.0.increment(p); }
        fn enter_scope(&mut self, _: &str, _: &[Inline], p: &Provenance) { self.0.increment(p); }
        fn enter_font_size(&mut self, _: &str, _: u8, _: &[Inline], p: &Provenance) { self.0.increment(p); }
        fn enter_warigaki(&mut self, _: &[Inline], _: &[Inline], p: &Provenance) { self.0.increment(p); }
        fn enter_figure_ref(&mut self, _: &str, _: &[Inline], p: &Provenance) { self.0.increment(p); }
    }
    walk_inline(&mut Vis(counts), value);
}
```

The inline `Vis` struct borrows `&mut ProvenanceCounts` directly — no allocation, no `std::mem::take`, no separate `ProvenanceCollector` struct needed.

- [x] **Step 3: Remove the old `fn inline_provenance`**

Delete the function entirely — it's only called by `collect_provenance` (now dead).

- [x] **Step 4: Run tests**

Run: `cargo test -p ab-ir`
Expected: all tests pass

- [x] **Step 5: Commit**

```bash
git add crates/ab-ir/src/lib.rs
git commit -m "refactor(ab-ir): replace collect_provenance with ProvenanceCollector visitor"
```

---

### Task 1.5: Remove Break { content } and update block_content

**Files:**
- Modify: `crates/ab-ir/src/lib.rs`

- [x] **Step 1: Remove content from Break variant**

Find `pub enum Block {` and change:

```rust
// Before:
Break {
    kind: BreakKind,
    content: Vec<Inline>,
},

// After:
Break {
    kind: BreakKind,
},
```

Also update the doc comment above Break (remove "content is always empty; carried so block_content keeps a uniform signature").

- [x] **Step 2: Update constructors that pass empty vecs**

Find `Block::page_break()` and `Block::line_break()` constructors. Remove the `content: Vec::new()` field:

```rust
// Before:
pub fn page_break() -> Self {
    Self::Break {
        kind: BreakKind::Page,
        content: Vec::new(),
    }
}

// After:
pub fn page_break() -> Self {
    Self::Break {
        kind: BreakKind::Page,
    }
}
```

Do the same for `line_break()`.

- [x] **Step 3: Update block_content to handle Break explicitly**

```rust
pub fn block_content(block: &Block) -> &[Inline] {
    match block {
        Block::Paragraph { content, .. }
        | Block::Heading { content, .. }
        | Block::Jisage { content, .. }
        | Block::CaptionBlock { content, .. }
        | Block::Warichu { content, .. }
        | Block::Figure { content, .. } => content,
        Block::Break { .. } => &[],
    }
}

```

- [x] **Step 4: Delete `pub fn block_content_mut` entirely**

**Verified: zero callers** — `rg "block_content_mut" crates/` returns only the definition at lib.rs:952, no external call sites. Delete the entire function (~lines 952-962). No `unreachable!()` needed — just remove dead code.

- [x] **Step 5: Update all consumers matching `Block::Break { content, .. }`**

Search the codebase for patterns matching `Block::Break` and remove any `content` binding:

In `lib.rs` — `inline_to_aat_json` (likely uses `..` already, verify).
Check `semantic_summary.rs` for `collect_block` — it matches `Block::Break { .. }` or `_ => {}`. Verify it still compiles.

Run: `cargo build -p ab-ir 2>&1 | head -30`
Fix any compile errors from removed `content` field.

- [x] **Step 6: Run tests**

Run: `cargo test -p ab-ir`
Expected: all tests pass

- [x] **Step 7: Commit**

```bash
git add crates/ab-ir/src/lib.rs
git commit -m "refactor(ab-ir): remove always-empty content from Block::Break, delete block_content_mut"
```

---

### Task 1.6: Replace duplicate walks in semantic_summary.rs

**Files:**
- Modify: `crates/ab-ir/src/semantic_summary.rs`

- [x] **Step 1: Add ContainsGaiji visitor and replace contains_gaiji**

Add at the top of `semantic_summary.rs`:

```rust
use crate::{walk_inline, InlineVisitor};

struct ContainsGaiji {
    pub found: bool,
}

impl InlineVisitor for ContainsGaiji {
    fn enter_gaiji_ref(&mut self, _gaiji: &GaijiRef) {
        self.found = true;
    }
}
```

Replace `fn contains_gaiji(content: &[Inline]) -> bool`:

```rust
fn contains_gaiji(content: &[Inline]) -> bool {
    let mut visitor = ContainsGaiji { found: false };
    for node in content {
        walk_inline(&mut visitor, node);
        if visitor.found {
            return true;
        }
    }
    false
}
```

- [x] **Step 2: Replace inline_visible_text to use the VisibleCollector**

Remove the local `fn inline_visible_text` and `fn collect_visible` from `semantic_summary.rs`. Import the one from lib.rs (or use the same `VisibleCollector` + `walk_inline` pattern):

```rust
fn inline_visible_text(content: &[Inline]) -> String {
    let mut collector = VisibleCollector { out: String::new() };
    for node in content {
        walk_inline(&mut collector, node);
    }
    collector.out
}
```

Make sure `VisibleCollector` is imported (or re-define it locally if it's `pub(crate)`).

- [x] **Step 3: Run tests**

Run: `cargo test -p ab-ir`
Expected: all tests pass, especially `semantic_summary_records_ruby_gaiji_gaiji_ruby_and_projection_warnings`

- [x] **Step 4: Commit**

```bash
git add crates/ab-ir/src/semantic_summary.rs
git commit -m "refactor(ab-ir): replace duplicate walks in semantic_summary with visitor"
```

---

### Task 1.7: Remove FeatureMap Index impl and update callers

**Files:**
- Modify: `crates/ab-morph-diff/src/model.rs`
- Modify: `crates/ab-morph-analyzers/src/features.rs`

- [x] **Step 1: Remove the Index<&str> impl from FeatureMap**

In `model.rs`, delete lines ~213–220:

```rust
// DELETE this entire block:
impl Index<&str> for FeatureMap {
    type Output = Option<FeatureValue>;

    fn index(&self, index: &str) -> &Self::Output {
        self.get(index)
            .unwrap_or_else(|| panic!("feature key not found: {index}"))
    }
}
```

Also remove `use std::ops::Index;` from the top of the file if it's only used for this impl.

- [x] **Step 2: Update callers that use bracket syntax**

In `crates/ab-morph-analyzers/src/features.rs`, change `features["pos1"]` to `features.get("pos1").expect("pos1")`:

```rust
// Before:
assert_eq!(features["pos1"], Some("名詞".into()));
assert_eq!(features["pos2"], Some("普通名詞".into()));
assert_eq!(features["pos3"], None);
assert_eq!(features["pos4"], None);
assert_eq!(features["ctype"], Some("extra".into()));
assert_eq!(features["field_28"], Some("tail".into()));

// After:
assert_eq!(features.get("pos1"), Some(&Some("名詞".into())));
assert_eq!(features.get("pos2"), Some(&Some("普通名詞".into())));
assert_eq!(features.get("pos3"), Some(&None));
assert_eq!(features.get("pos4"), Some(&None));
assert_eq!(features.get("ctype"), Some(&Some("extra".into())));
assert_eq!(features.get("field_28"), Some(&Some("tail".into())));
```

Note: `FeatureMap::get()` returns `Option<&Option<FeatureValue>>`, so the correct comparison is `Some(&Some(...))`. This tests both key existence and value match in one `assert_eq!`. Apply the same change to the second test block at ~line 139.

- [x] **Step 3: Search for any other callers**

Run: `cargo check --workspace 2>&1 | grep -i 'index.*featuremap\|no method.*index'`
If any other crates use `features["..."]` or `map["..."]` on FeatureMap, fix them the same way.

- [x] **Step 4: Run tests**

Run: `cargo test -p ab-morph-diff -p ab-morph-analyzers`
Expected: all tests pass

- [x] **Step 5: Commit**

```bash
git add crates/ab-morph-diff/src/model.rs crates/ab-morph-analyzers/src/features.rs
git commit -m "refactor: remove panicking FeatureMap Index impl, use get() instead"
```

---

### Task 1.8: Verify full workspace after Stream 1

- [x] **Step 1: Build workspace**

Run: `cargo build --workspace`
Expected: compiles cleanly

- [x] **Step 2: Run all tests**

Run: `cargo test --workspace`
Expected: all 375 tests pass (excluding 4 ab-coverage failures from missing data)

- [x] **Step 3: Commit if any final touch-ups needed**

---

## Stream 2: ab-morph-run God Crate Split

### Task 2.1: Create summary sub-module directory and types.rs

**Files:**
- Create: `crates/ab-morph-run/src/summary/mod.rs`
- Create: `crates/ab-morph-run/src/summary/types.rs`

**Pattern for all Tasks 2.1–2.7:** Each task copies content from `summary.rs` to a new file, then deletes that content from `summary.rs`. After each task, both the new sub-module AND the remainder of `summary.rs` must compile. This ensures `summary.rs` shrinks incrementally with every commit.

- [x] **Step 1: Create the summary directory**

```bash
mkdir -p crates/ab-morph-run/src/summary
```

- [x] **Step 2: Create mod.rs with re-exports placeholder**

File: `crates/ab-morph-run/src/summary/mod.rs`
```rust
mod compact;
mod nway;
mod patterns;
mod types;
mod warehouse;
mod write;

pub use compact::*;
pub use nway::*;
pub use patterns::*;
pub use types::*;
pub use warehouse::*;
pub use write::*;
```

- [x] **Step 3: Move type definitions to types.rs**

Extract all public enums, structs, and const from `summary.rs` (lines ~1–130) into `types.rs`. These are:
- `WAREHOUSE_CORE_FEATURE_KEYS` const
- `CompactSummaryGroupBy`
- `CompactSummarySort`
- `CompactExampleFilter`
- `CompactExampleSummarySort`
- `CompactDifferenceKindFilter`
- `NwaySummarySort`
- `NwayPatternKind`
- `WarehouseFeatureProfile`
- `SummaryExclusions` struct + impl
- `CompactSummaryOptions`
- `CompactExampleSummaryOptions`
- `CompactDifferenceSummaryOptions`
- `NwaySummaryOptions`
- `NwayPatternOptions`
- `WarehousePatternOptions`
- `WarehousePatternExampleOptions`
- `WarehouseRegionKind`
- `WarehouseTextFilter`
- `WarehouseRegionOptions`
- `WarehouseErrorGroupBy`
- `WarehouseErrorSummaryOptions`
- `WarehousePairwiseSort`
- `WarehousePairwiseSummaryOptions`
- All row types: `CompactSummaryRow`, `CompactExampleSummaryRow`, `CompactDifferenceSummaryRow`, `NwaySummaryRow`, `NwayPatternRow`, `WarehouseRegionExampleRow`, `WarehouseRegionAnalyzerExampleRow`, `WarehouseFeatureDiffExampleRow`, `WarehouseErrorSummaryRow`, `WarehousePairwiseSummaryRow`

Add `use` imports at top for any dependencies (crate types, serde, arrow, etc.).

- [x] **Step 4: Add `pub mod summary;` to lib.rs and verify compilation**

In `crates/ab-morph-run/src/lib.rs`, add `pub mod summary;` if not already present.
Delete the original type definitions from `summary.rs` (but keep the `use` imports and function bodies for now).

Run: `cargo build -p ab-morph-run`
Expected: compiles. Fix any missing imports.

- [x] **Step 5: Commit**

```bash
git add crates/ab-morph-run/src/summary/
git commit -m "refactor(ab-morph-run): extract summary types into summary/types.rs"
```

---

### Task 2.2: Move compact summary functions to summary/compact.rs

**Files:**
- Create: `crates/ab-morph-run/src/summary/compact.rs`
- Modify: `crates/ab-morph-run/src/summary/mod.rs`

- [x] **Step 1: Create compact.rs with the three compact summarize functions**

Extract from `summary.rs`:
- `summarize_compact_comparisons` (~line 615)
- `summarize_compact_examples` (~line 648)
- `summarize_compact_differences` (~line 693)
- Any private helper functions they call

Use: `cargo build -p ab-morph-run 2>&1 | grep "cannot find"` to find missing imports and add them.

- [x] **Step 2: Verify compilation**

Run: `cargo build -p ab-morph-run`
Expected: compiles cleanly

- [x] **Step 3: Commit**

```bash
git add crates/ab-morph-run/src/summary/compact.rs
git commit -m "refactor(ab-morph-run): move compact summary to summary/compact.rs"
```

---

### Task 2.3: Move nway summary functions to summary/nway.rs

**Files:**
- Create: `crates/ab-morph-run/src/summary/nway.rs`

- [x] **Step 1: Create nway.rs**

Move from `summary.rs`:
- `summarize_nway` (~line 797)
- `summarize_nway_patterns` (~line 829)
- `summarize_nway_pattern_counts` (~line 923)
- Any private helpers

- [x] **Step 2: Verify compilation and commit**

Run: `cargo build -p ab-morph-run`
```bash
git add crates/ab-morph-run/src/summary/nway.rs
git commit -m "refactor(ab-morph-run): move nway summary to summary/nway.rs"
```

---

### Task 2.4: Move warehouse summary functions to summary/warehouse.rs

**Files:**
- Create: `crates/ab-morph-run/src/summary/warehouse.rs`

- [x] **Step 1: Create warehouse.rs**

Move from `summary.rs`:
- `summarize_warehouse_nway` (~line 956)
- `summarize_warehouse_regions` (~line 1005)
- `summarize_warehouse_errors` (~line 1101)
- `summarize_warehouse_pairwise` (~line 1141)
- `summarize_warehouse_nway_patterns` (~line 1289)
- `summarize_warehouse_pattern_examples` (~line 2999)
- `materialize_warehouse_core_feature_pattern_counts` (~line 1452)

- [x] **Step 2: Verify compilation and commit**

Run: `cargo build -p ab-morph-run`
```bash
git add crates/ab-morph-run/src/summary/warehouse.rs
git commit -m "refactor(ab-morph-run): move warehouse summary to summary/warehouse.rs"
```

---

### Task 2.5: Move pattern types and helpers to summary/patterns.rs

**Files:**
- Create: `crates/ab-morph-run/src/summary/patterns.rs`

- [x] **Step 1: Move pattern-related types and helpers**

All `NwayPattern*` and `WarehousePattern*` types were already moved to `types.rs`.
Move remaining pattern helper functions from `summary.rs`.

- [x] **Step 2: Verify and commit**

Run: `cargo build -p ab-morph-run`
```bash
git add crates/ab-morph-run/src/summary/patterns.rs
git commit -m "refactor(ab-morph-run): move pattern helpers to summary/patterns.rs"
```

---

### Task 2.6: Move DuckDB TSV write functions to summary/write.rs

**Files:**
- Create: `crates/ab-morph-run/src/summary/write.rs`

- [x] **Step 1: Create write.rs**

Move from `summary.rs`:
- `write_warehouse_nway_patterns_duckdb_tsv` (~line 1407)
- `write_warehouse_regions_duckdb_tsv` (~line 2017)
- `write_warehouse_pattern_examples_duckdb_tsv` (~line 2026)

- [x] **Step 2: Verify and commit**

```bash
git add crates/ab-morph-run/src/summary/write.rs
git commit -m "refactor(ab-morph-run): move DuckDB TSV writers to summary/write.rs"
```

---

### Task 2.7: Clean up original summary.rs — it should now be empty

**Files:**
- Modify: `crates/ab-morph-run/src/summary.rs`

- [x] **Step 1: Replace summary.rs with re-exports shim**

Delete all content from `summary.rs` and replace with:

```rust
// Moved to summary/ sub-module. Keep re-exports for backward compatibility.
mod summary_mod;
pub use summary_mod::*;
```

**Important:** Cargo resolves `mod summary;` as follows: if both `summary.rs` and `summary/mod.rs` exist, `summary.rs` wins. The directory module **will not** be compiled until `summary.rs` is deleted. So the order is: create `summary/` directory, copy content, **then** delete `summary.rs`.

```bash
rm crates/ab-morph-run/src/summary.rs
```

- [x] **Step 2: Verify the workspace compiles**

Run: `cargo build --workspace`
Expected: compiles cleanly. If it doesn't, cargo may still be picking up the old `summary.rs` — verify it's deleted with `ls crates/ab-morph-run/src/summary.rs`.

- [x] **Step 3: Commit**

```bash
git rm crates/ab-morph-run/src/summary.rs
git add crates/ab-morph-run/src/summary/
git commit -m "refactor(ab-morph-run): remove summary.rs, now split across summary/ sub-modules"
```

---

### Task 2.8: Create new `ab-warehouse` crate (schema + writer + sql only)

**Files:**
- Create: `crates/ab-warehouse/Cargo.toml`
- Create: `crates/ab-warehouse/src/lib.rs`
- Create: `crates/ab-warehouse/src/schema.rs` (row type definitions + WarehouseTable + WarehousePaths)
- Create: `crates/ab-warehouse/src/writer.rs` (parquet writes from row structs)
- Create: `crates/ab-warehouse/src/sql.rs` (DuckDB templates)
- Modify: `Cargo.toml` (workspace members)

**Important:** `rows.rs` stays in `ab-morph-run` — it deeply depends on `ab_morph_diff::Analysis`, `NwayRegion`, `NwayFeatureScope`, etc. (confirmed: all 483 lines of construction functions take `&Analysis`). Only the pure-I/O modules move to `ab-warehouse`.

- [x] **Step 1: Create crate directory and Cargo.toml**

```bash
mkdir -p crates/ab-warehouse/src
```

File: `crates/ab-warehouse/Cargo.toml`
```toml
[package]
name = "ab-warehouse"
version.workspace = true
edition.workspace = true
license.workspace = true

[dependencies]
anyhow.workspace = true
arrow-array.workspace = true
arrow-schema.workspace = true
parquet.workspace = true
serde.workspace = true
serde_json.workspace = true
```

File: `crates/ab-warehouse/src/lib.rs`
```rust
pub mod schema;
pub mod sql;
pub mod writer;
```

- [x] **Step 2: Move only the pure-I/O files**

```bash
cp crates/ab-morph-run/src/warehouse/schema.rs crates/ab-warehouse/src/schema.rs
cp crates/ab-morph-run/src/warehouse/writer.rs crates/ab-warehouse/src/writer.rs
cp crates/ab-morph-run/src/warehouse/sql.rs crates/ab-warehouse/src/sql.rs
```

**Do NOT move rows.rs** — it stays in `ab-morph-run`.

- [x] **Step 3: Update imports in the moved files**

In `writer.rs`: change `use super::schema::...` to `use crate::schema::...`. In `schema.rs`: row type definitions (AnalysisRow, MorphemeRow, etc.) already use plain `String`/`u64`/`Vec<String>` — no `ab_morph_diff` dependency. Verify with `rg "ab_morph_diff" crates/ab-warehouse/src/` — should return nothing.

Add `serde.workspace = true` if any row types derive Serialize/Deserialize.

Run: `cargo build -p ab-warehouse`
Fix compile errors iteratively.

- [x] **Step 4: Add ab-warehouse to workspace Cargo.toml**

In `/Cargo.toml`, add to `members`:
```toml
"crates/ab-warehouse",
```

Also add workspace dependency:
```toml
ab-warehouse = { path = "crates/ab-warehouse" }
```

- [x] **Step 5: Commit**

```bash
git add crates/ab-warehouse/ Cargo.toml
git commit -m "feat: create ab-warehouse crate with schema, writer, sql modules"
```

---

### Task 2.9: Update ab-morph-run to use ab-warehouse, keep rows.rs locally

**Files:**
- Modify: `crates/ab-morph-run/Cargo.toml`
- Modify: `crates/ab-morph-run/src/lib.rs`
- Modify: `crates/ab-morph-run/src/warehouse/` (replace moved files, keep rows.rs + mod.rs)

- [x] **Step 1: Add ab-warehouse dependency**

In `crates/ab-morph-run/Cargo.toml`:
```toml
ab-warehouse.workspace = true
```

- [x] **Step 2: Restructure warehouse/ directory**

Remove the files that moved to ab-warehouse:
```bash
rm crates/ab-morph-run/src/warehouse/schema.rs
rm crates/ab-morph-run/src/warehouse/writer.rs
rm crates/ab-morph-run/src/warehouse/sql.rs
```

Keep `rows.rs` and `mod.rs` (or rename mod.rs to contain only the rows module).

Update `warehouse/mod.rs` to re-export from ab-warehouse:
```rust
mod rows;
pub(crate) use rows::*;

pub use ab_warehouse::schema;
pub use ab_warehouse::writer;
pub use ab_warehouse::sql;
```

- [x] **Step 3: Update imports in warehouse/rows.rs**

Change `use super::schema::...` to `use ab_warehouse::schema::...`. The row construction functions continue to use `ab_morph_diff::Analysis` — no changes there.

- [x] **Step 4: Update lib.rs imports**

The `use warehouse::schema::...` and `use warehouse::writer::...` imports in `lib.rs` continue to work because `warehouse/mod.rs` re-exports from `ab_warehouse`. Verify with `cargo build -p ab-morph-run`.

- [x] **Step 5: Build and fix errors**

Run: `cargo build -p ab-morph-run`
Fix any import errors.

- [x] **Step 6: Commit**

```bash
git add crates/ab-morph-run/
git commit -m "refactor(ab-morph-run): use ab-warehouse for schema/writer/sql, keep rows.rs local"
```

---

### Task 2.10: Extract pipeline.rs from lib.rs

**Files:**
- Create: `crates/ab-morph-run/src/pipeline.rs`
- Modify: `crates/ab-morph-run/src/lib.rs`

- [x] **Step 1: Identify pipeline content in lib.rs**

The pipeline content includes:
- `run_analyze_aat` function
- Serial/parallel dispatch logic
- `LARGE_INPUT_THRESHOLD_BYTES` constant
- `WAREHOUSE_MORPHEME_ROW_BATCH_SIZE` etc. (move to pipeline or keep shared)
- Private helper functions for pipeline orchestration

- [x] **Step 2: Create pipeline.rs**

Move the pipeline functions into `pipeline.rs`. Keep `lib.rs` as the public API surface with re-exports:

```rust
// lib.rs
pub mod pipeline;

pub use pipeline::run_analyze_aat;
// ... other re-exports
```

- [x] **Step 3: Build and fix**

Run: `cargo build -p ab-morph-run`

- [x] **Step 4: Commit**

```bash
git add crates/ab-morph-run/src/pipeline.rs crates/ab-morph-run/src/lib.rs
git commit -m "refactor(ab-morph-run): extract pipeline orchestration to pipeline.rs"
```

---

### Task 2.11: Create options.rs for run option structs

**Files:**
- Create: `crates/ab-morph-run/src/options.rs`
- Modify: `crates/ab-morph-run/src/lib.rs`

- [x] **Step 1: Move SerialRunOptions and related types to options.rs**

Extract:
- `SerialRunOptions` struct
- `WarehouseProfile` (if not already in another module)
- `OutputProfile` (if not already in another module)
- Any other run-configuration types

Mark all as `pub(crate)` unless they constitute public API. If public, add doc comments.

- [x] **Step 2: Add pub mod options to lib.rs, update imports**

```rust
pub mod options; // or pub(crate) mod options;
```

- [x] **Step 3: Build and commit**

Run: `cargo build -p ab-morph-run`
```bash
git add crates/ab-morph-run/src/options.rs
git commit -m "refactor(ab-morph-run): extract options types to options.rs"
```

---

### Task 2.12: Simplify CLI adapter boilerplate in main.rs

**Files:**
- Modify: `crates/ab-morph-run/src/main.rs`

- [x] **Step 1: Identify all *Arg enum types**

Find all enum types like `SummaryGroupByArg`, `NwaySummarySortArg`, `CompactSummarySortArg`, etc. that have hand-written `into_library()` methods.

- [x] **Step 2: Replace with direct ValueEnum on library types where possible**

For types defined in `ab-morph-run`, derive `clap::ValueEnum` directly:

```rust
// Before: separate Arg enum + conversion
#[derive(clap::ValueEnum)]
enum CompactSummarySortArg { BoundaryF1, SegmentationRegions, ... }
impl CompactSummarySortArg {
    fn into_library(self) -> CompactSummarySort { ... }
}

// After: derive on library type directly
#[derive(Debug, Clone, Copy, PartialEq, Eq, clap::ValueEnum, Serialize, Deserialize)]
pub enum CompactSummarySort { ... }
```

For types in other crates where you can't modify the type, use a macro:

```rust
macro_rules! arg_enum {
    ($arg_name:ident, $lib_type:ty, { $($variant:ident),+ $(,)? }) => {
        #[derive(Debug, Clone, Copy, clap::ValueEnum)]
        enum $arg_name { $($variant),+ }
        impl From<$arg_name> for $lib_type {
            fn from(arg: $arg_name) -> Self {
                match arg { $( $arg_name::$variant => <$lib_type>::$variant ),+ }
            }
        }
    };
}
```

- [x] **Step 3: Update CLI match arms**

Replace `arg.into_library()` calls with `arg.into()`.

- [x] **Step 4: Build and verify CLI works**

Run: `cargo build -p ab-morph-run`
Run: `cargo run -p ab-morph-run -- --help`
Expected: help output renders correctly

- [x] **Step 5: Commit**

```bash
git add crates/ab-morph-run/src/main.rs
git commit -m "refactor(ab-morph-run): simplify CLI adapter enum conversions"
```

---

### Task 2.13: Verify full workspace after Stream 2

- [x] **Step 1: Build workspace**

Run: `cargo build --workspace`
Expected: compiles cleanly

- [x] **Step 2: Run all tests**

Run: `cargo test --workspace`
Expected: all 375 tests pass

- [x] **Step 3: Commit any final fixes**

---

## Stream 3: Interning Removal + Robustness Fixes

### Task 3.1: Remove InternedString, replace with Arc<str>

**Files:**
- Modify: `crates/ab-morph-diff/src/model.rs`

- [x] **Step 1: Replace InternedString definition and impls**

In `model.rs`, delete the entire `InternedString` struct and all its impls (lines ~1–100 covering the struct, `intern()`, `Debug`, `Display`, `From<&str>`, `From<String>`, `AsRef`, `Borrow`, `PartialEq`, `Hash`, `Serialize`).

Replace the type aliases:

```rust
// Before
pub type FeatureKey = InternedString;
pub type FeatureValue = InternedString;

// After
pub type FeatureKey = Arc<str>;
pub type FeatureValue = Arc<str>;
```

Remove the `thread_local!` interner block and associated imports (`RefCell`, `HashSet` that were only used for interning).

- [x] **Step 2: Update callers of as_str()**

Search: `rg "as_str\(\)" crates/ab-morph-diff/src/ crates/ab-morph-run/src/ crates/ab-morph-analyzers/src/`

Replace `val.as_str()` with `&*val` or `val.as_ref()`. For `Arc<str>`, these are equivalent.

In `model.rs` itself, update:
- `FeatureMap::insert` — line ~153: `existing.as_str().cmp(key.as_str())` → `existing.as_ref().cmp(key.as_ref())`
- `FeatureMap::get` — line ~168: `existing.as_str().cmp(key)` → `existing.as_ref().cmp(key)`

- [x] **Step 3: Update From<&str> conversions**

Where code calls `InternedString::from("foo")` or `"foo".into()`, these automatically become `Arc<str>::from("foo")` since the type alias changed. `Arc<str>` implements `From<&str>`. Verify compilation handles this.

- [x] **Step 4: Build and fix**

Run: `cargo build -p ab-morph-diff 2>&1 | head -40`
Fix all compile errors iteratively.

- [x] **Step 5: Run tests**

Run: `cargo test -p ab-morph-diff`
Expected: all tests pass

- [x] **Step 6: Commit**

```bash
git add crates/ab-morph-diff/src/model.rs
git commit -m "refactor(ab-morph-diff): remove InternedString, replace with Arc<str>"
```

---

### Task 3.2: Update InternedString callers across the workspace

**Files:**
- `crates/ab-morph-run/src/` — various files
- `crates/ab-oracle/src/` — if using FeatureKey/FeatureValue
- `crates/ab-check/src/` — if using FeatureKey/FeatureValue

- [x] **Step 1: Find all callers**

Run: `rg -l "InternedString|FeatureKey|FeatureValue" crates/ -- crates/ab-morph-diff/`

- [x] **Step 2: Build workspace and fix errors**

Run: `cargo build --workspace 2>&1 | grep "error\["`
Fix each error. Common patterns:
- `val.as_str()` → `val.as_ref()` or `&*val`
- `From<&str> for InternedString` no longer exists but `Arc<str>::from("...")` works
- `Serialize` for `InternedString` is now `Arc<str>`'s built-in serde support

- [x] **Step 3: Run all tests**

Run: `cargo test --workspace`
Expected: all tests pass

- [x] **Step 4: Commit**

```bash
git add -u
git commit -m "refactor: update all InternedString callers to use Arc<str>"
```

---

### Task 3.3: Fix panic in ab-coverage adapter.rs

**Files:**
- Modify: `crates/ab-coverage/src/adapter.rs`

- [x] **Step 1: Change AdapterBinary::for_parser to return Result**

```rust
// Before:
pub fn for_parser(repo_root: &Path, parser_id: &str) -> Self {
    let binary = match parser_id {
        "aozora2" => repo_root.join("adapters/aozora2/target/release/aozora2-adapter"),
        "aozora-rs" => repo_root.join("adapters/aozora-rs/target/release/aozora-rs-adapter"),
        "aozora2html" => repo_root.join("adapters/aozora2html/aozora2html-adapter"),
        other => panic!("unknown parser id: {other}"),
    };
    Self { parser_id: parser_id.to_string(), binary }
}

// After:
pub fn for_parser(repo_root: &Path, parser_id: &str) -> Result<Self> {
    let binary = match parser_id {
        "aozora2" => repo_root.join("adapters/aozora2/target/release/aozora2-adapter"),
        "aozora-rs" => repo_root.join("adapters/aozora-rs/target/release/aozora-rs-adapter"),
        "aozora2html" => repo_root.join("adapters/aozora2html/aozora2html-adapter"),
        other => bail!("unknown parser id: {other}"),
    };
    Ok(Self { parser_id: parser_id.to_string(), binary })
}
```

- [x] **Step 2: Update all callers of AdapterBinary::for_parser**

Run: `cargo build -p ab-coverage 2>&1 | grep error`
Change `.for_parser(...)` to `.for_parser(...)?` at each call site.

- [x] **Step 3: Run coverage tests**

Run: `cargo test -p ab-coverage`
Expected: non-data-dependent tests pass

- [x] **Step 4: Commit**

```bash
git add crates/ab-coverage/src/adapter.rs
git commit -m "fix(ab-coverage): return Result instead of panicking on unknown parser id"
```

---

### Task 3.4: Fix panics in VibratoAnalyzer and VaporettoAnalyzer internal call chains

**Files:**
- Modify: `crates/ab-morph-analyzers/src/vibrato.rs`
- Modify: `crates/ab-morph-analyzers/src/vaporetto.rs`

**Note:** `unidic_cwj_default()` already returns `Result<Self, AnalyzerError>`. The panic is in the private `resolve_default_dictionary()` helper, via `.unwrap_or_else(|err| panic!("{err}"))`. Only the internal call chain needs fixing — the public API stays the same.

- [x] **Step 1: Fix VibratoAnalyzer internal chain**

In vibrato.rs, change `resolve_default_dictionary` (~line 90):

```rust
// Before:
fn resolve_default_dictionary() -> PathBuf {
    resolve_dictionary_path(DEFAULT_VIBRATO_DICTIONARY).unwrap_or_else(|err| panic!("{err}"))
}

// After:
fn resolve_default_dictionary() -> Result<PathBuf, AnalyzerError> {
    resolve_dictionary_path(DEFAULT_VIBRATO_DICTIONARY)
}
```

Update `default_dictionary_path` and `default_dictionary_path_from_env` to return `Result<PathBuf, AnalyzerError>` and propagate with `?`. Since `unidic_cwj_default()` already calls these and returns `Result`, it naturally propagates.

- [x] **Step 2: Same for VaporettoAnalyzer**

Apply the same pattern to `vaporetto.rs` (~line 163). The `resolve_default_dictionary` → `Result<PathBuf, AnalyzerError>` chain is structurally identical to Vibrato.

- [x] **Step 3: Verify callers compile**

Run: `cargo build --workspace 2>&1 | grep error`
Expected: no new errors (callers already `.unwrap()` the public `Result` constructors).

- [x] **Step 4: Run analyzer tests**

Run: `cargo test -p ab-morph-analyzers`
Expected: all tests pass (non-dictionary-dependent ones)

- [x] **Step 5: Commit**

```bash
git add crates/ab-morph-analyzers/src/vibrato.rs crates/ab-morph-analyzers/src/vaporetto.rs
git commit -m "fix(ab-morph-analyzers): propagate Result in resolve_default_dictionary instead of panicking"
```

---

### Task 3.5: Add warnings to Analysis struct and convert eprintln! in analyzers

**Files:**
- Modify: `crates/ab-morph-diff/src/model.rs` — add `warnings` field to `Analysis`
- Modify: `crates/ab-morph-analyzers/src/lib.rs` — define `AnalyzerWarning` type
- Modify: `crates/ab-morph-analyzers/src/vibrato.rs` — populate warnings during analyze()
- Modify: `crates/ab-morph-analyzers/src/sudachi.rs` — populate warnings during analyze()

**Key design decision:** Warnings go on the `Analysis` struct, NOT on the `MorphAnalyzer` trait. This avoids all issues with `&self` mutability (analyzers build warnings during `analyze()` as local `Vec` and attach them to the returned `Analysis`). The trait signature stays unchanged — zero breaking change. Pattern matches `ProjectionWarning` in `ab-ir`.

- [x] **Step 1: Define AnalyzerWarning and add to Analysis**

In `crates/ab-morph-analyzers/src/lib.rs`, add:

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnalyzerWarning {
    pub analyzer_id: String,
    pub text_id: String,
    pub stage: String,
    pub message: String,
    pub count: usize,
    pub first_byte_offset: usize,
    pub hard_limit_bytes: usize,
}
```

In `crates/ab-morph-diff/src/model.rs`, add `warnings` field to `Analysis`:

```rust
pub struct Analysis {
    pub analyzer: AnalyzerId,
    pub text_id: TextId,
    pub source_text: String,
    pub morphemes: Vec<Morpheme>,
    pub warnings: Vec<AnalyzerWarning>,  // NEW
}
```

Build to confirm: `cargo build -p ab-morph-diff`. Note: to avoid a circular dependency (`ab-morph-analyzers` depends on `ab-morph-diff`), define `AnalyzerWarning` in `ab-morph-diff/src/model.rs` alongside `Analysis`, and re-export from `ab-morph-analyzers`.

- [x] **Step 2: Update VibratoAnalyzer to collect warnings during analyze()**

In vibrato.rs, replace the `eprintln!` block (~line 215) with warnings accumulated in a local `Vec` and attached to the `Analysis` at return:

```rust
// Before:
if hard_split_count > 0 {
    eprintln!("ab-morph-analyzers: ...", ...);
}

// After: collect warnings in a local vec, attach to Analysis before returning
let mut warnings = Vec::new();
if hard_split_count > 0 {
    warnings.push(AnalyzerWarning {
        analyzer_id: self.analyzer_id.clone(),
        text_id: document.text_id.clone(),
        stage: "vibrato_chunk".into(),
        message: "hard_split_without_sentence_boundary".into(),
        count: hard_split_count,
        first_byte_offset: first_hard_split_offset.unwrap_or(0),
        hard_limit_bytes: VIBRATO_CHUNK_BYTES,
    });
}
// ... later, when building the Analysis:
Analysis { warnings, ..analysis }
```

- [x] **Step 3: Same for SudachiAnalyzer**

Replace the `eprintln!` at sudachi.rs:121 with structured warning collection, attached to the returned `Analysis.warnings`.

- [x] **Step 4: Update ab-morph-run to surface warnings**

In `ab-morph-run`, after calling `analyzer.analyze()`, the returned `Analysis.warnings` are available. Log them or surface them in the output at the caller's discretion.

- [x] **Step 5: Fix all Analysis construction sites**

Run: `cargo build --workspace 2>&1 | grep "missing field"`
Every place that constructs `Analysis { .. }` must now include `warnings: Vec::new()` (for sites that don't collect warnings) or actual warnings.

- [x] **Step 6: Run tests**

Run: `cargo test -p ab-morph-diff -p ab-morph-analyzers -p ab-morph-run`
Expected: all non-dictionary tests pass

- [x] **Step 7: Commit**

```bash
git add crates/ab-morph-diff/src/model.rs crates/ab-morph-analyzers/
git commit -m "refactor: add AnalyzerWarning to Analysis, convert eprintln! to structured warnings"
```

---

### Task 3.6: Convert eprintln! in ab-morph-run to structured output

**Files:**
- Modify: `crates/ab-morph-run/src/lib.rs`

- [x] **Step 1: Replace eprintln! calls**

At lines 543, 955, 962 — replace with attaching warnings to the result struct (same pattern as analyzers). The caller (main.rs) decides whether to print.

- [x] **Step 2: Verify and commit**

Run: `cargo build -p ab-morph-run`
```bash
git add crates/ab-morph-run/src/lib.rs
git commit -m "refactor(ab-morph-run): convert eprintln! to structured warnings"
```

---

### Task 3.7: Fix test-only eprintln! in Sudachi tests

**Files:**
- Modify: `crates/ab-morph-analyzers/src/sudachi.rs` (test module)

- [x] **Step 1: Change eprintln! to println!**

In sudachi.rs test module, lines 345 and 366, `eprintln!("skipping Sudachi smoke test...")` → `println!("skipping Sudachi smoke test...")`.

- [x] **Step 2: Commit**

```bash
git add crates/ab-morph-analyzers/src/sudachi.rs
git commit -m "fix(ab-morph-analyzers): use println! instead of eprintln! in Sudachi tests"
```

---

### Task 3.8: Verify full workspace after Stream 3

- [x] **Step 1: Build workspace**

Run: `cargo build --workspace`
Expected: compiles

- [x] **Step 2: Run all tests**

Run: `cargo test --workspace`
Expected: all tests pass

- [x] **Step 3: Commit any final fixes**

---

## Stream 4: Clippy Sweep

### Task 4.1: Add Cargo.toml metadata to all workspace crates

**Files:**
- Modify: `crates/*/Cargo.toml` (all 12 crates + ab-warehouse)

- [x] **Step 1: Add metadata fields to each Cargo.toml**

For each crate, add:

```toml
description = "Japanese text processing — [one-line description of this crate's role]"
repository = "https://github.com/your-org/ab-validator"  <!-- FIXME: replace with actual repo URL before committing -->
keywords = ["japanese", "text-processing"]
categories = ["text-processing"]
```

Tailor the description per crate:
- `ab-ir`: "Parser-neutral intermediate representation for Aozora Bunko texts"
- `ab-morph-diff`: "Morphological analysis comparison and diff engine"
- `ab-morph-run`: "Pipeline runner for morphological analysis and comparison"
- `ab-morph-analyzers`: "Adapters for Vibrato, Sudachi, and Vaporetto morphological analyzers"
- `ab-check`: "Schema validation for AAT documents"
- `ab-compare`: "Comparison and triage for analyzer differences"
- `ab-coverage`: "Syntax coverage analysis for Japanese text parsers"
- `ab-diff-utils`: "Diff and alignment utility functions"
- `ab-index`: "Text indexing and feature extraction"
- `ab-plaintext`: "Plain text normalization and canonicalization"
- `ab-source-syntax`: "Aozora Bunko source syntax parser"
- `ab-oracle`: "Test oracle and evaluation framework"
- `ab-warehouse`: "Parquet warehouse I/O for morphological analysis data"

- [x] **Step 2: Verify cargo metadata**

Run: `cargo metadata --no-deps --format-version 1 | jq '.packages[] | select(.name | startswith("ab-")) | {name, description, repository, keywords, categories}'`
Expected: each package shows non-null description, repository, etc.

- [x] **Step 3: Commit**

```bash
git add crates/*/Cargo.toml
git commit -m "chore: add crate metadata (description, repository, keywords) to all crates"
```

---

### Task 4.2: Fix needless_raw_string_hashes

**Files:**
- Modify: `crates/ab-index/src/features.rs`

- [x] **Step 1: Fix the raw string hash**

At line 101, change `r#"..."#` → `r"..."`:

```rust
// Before:
r#"..."#

// After:
r"..."
```

- [x] **Step 2: Verify**

Run: `cargo build -p ab-index`

- [x] **Step 3: Commit**

```bash
git add crates/ab-index/src/features.rs
git commit -m "fix(ab-index): remove needless raw string hashes"
```

---

### Task 4.3: Fix or-patterns throughout codebase

**Files:**
- Modify: `crates/ab-diff-utils/src/first_diff.rs`
- Modify: `crates/ab-coverage/src/adapter.rs`
- Modify: any other files with clippy `unnested_or_patterns`

- [x] **Step 1: Find all or-pattern warnings**

Run: `cargo clippy --workspace 2>&1 | grep "unnested_or_patterns"`

- [x] **Step 2: Fix each location**

Common fixes:
- `(Some(_), Some(_)) | (Some(_), None) | (None, Some(_))` → `(Some(_) | None, Some(_)) | (Some(_), None)`
- `Some(0) | Some(2)` → `Some(0 | 2)`

- [x] **Step 3: Verify clippy passes**

Run: `cargo clippy --workspace -- -W clippy::unnested_or_patterns`
Expected: no warnings

- [x] **Step 4: Commit**

```bash
git add -u
git commit -m "fix: unnest or-patterns throughout codebase"
```

---

### Task 4.4: Add #[must_use] to pure getter/constructor functions

**Files:**
- Modify: `crates/ab-ir/src/lib.rs` — constructors
- Modify: `crates/ab-morph-diff/src/` — public API functions
- Modify: `crates/ab-morph-run/src/` — public API functions

- [x] **Step 1: Identify functions needing must_use**

Run: `cargo clippy --workspace -- -W clippy::pedantic 2>&1 | grep "must_use_candidate" | head -20`

- [x] **Step 2: Add #[must_use] to the 20 most-impactful functions**

Focus on public API: constructors (`new()`, `from_*()`, `with_*()`), pure getters, and functions with no side effects.

- [x] **Step 3: Verify no new clippy warnings**

Run: `cargo clippy --workspace -- -W clippy::must_use_candidate 2>&1 | grep -c must_use`
Expected: count is lower than before

- [x] **Step 4: Commit**

```bash
git add crates/
git commit -m "chore: add #[must_use] to pure getter/constructor functions"
```

---

### Task 4.5: Add # Errors docs to Result-returning public functions

**Files:**
- Modify: public API files across workspace

- [x] **Step 1: Find functions needing docs**

Run: `cargo clippy --workspace -- -W clippy::pedantic 2>&1 | grep "missing_errors_doc" | head -20`

- [x] **Step 2: Add # Errors sections**

For each function, add a doc comment like:

```rust
/// Does something.
///
/// # Errors
///
/// Returns an error if the input is invalid or I/O fails.
pub fn process(&self) -> Result<(), Error> { ... }
```

Prioritize the 20 most-flagged functions.

- [x] **Step 3: Commit**

```bash
git add crates/
git commit -m "docs: add # Errors sections to Result-returning functions"
```

---

### Task 4.6: Fix similar_names warnings

**Files:**
- `crates/ab-morph-diff/src/model.rs` — `interner`/`interned` (now removed with interner)
- Any remaining similar_names from clippy

- [x] **Step 1: Find remaining similar_names warnings**

Run: `cargo clippy --workspace -- -W clippy::pedantic 2>&1 | grep "similar_names"`

- [x] **Step 2: Rename variables to be more distinct**

- [x] **Step 3: Commit**

```bash
git add -u
git commit -m "chore: fix similar_names clippy warnings"
```

---

### Task 4.7: Final verification

- [x] **Step 1: Build workspace**

Run: `cargo build --workspace`
Expected: clean

- [x] **Step 2: Run all tests**

Run: `cargo test --workspace`
Expected: all tests pass (excluding 4 ab-coverage integration tests that require missing data files, and dictionary-dependent tests marked `#[ignore]`)

- [x] **Step 3: Run clippy**

Run: `cargo clippy --workspace --all-targets -- -W clippy::pedantic -W clippy::nursery -W clippy::cargo 2>&1 | grep -c "^warning:"`
Expected: prints a number measurably lower than the original ~751

- [x] **Step 4: Commit any remaining fixes**

---

## Execution Notes

- **Order:** Streams 1, 2, 3 can run in parallel if multiple workers are available. Stream 4 runs last.
- **After each stream:** Run `cargo test --workspace` and fix any failures before proceeding.
- **Commit frequency:** After every task (not every step). Each commit should compile and pass tests for its crate.
- **Known exclusions:** 4 failing ab-coverage integration tests (missing data files) are not in scope. Dictionary-dependent tests (`#[ignore = "requires dictionary symlink"]`) are also not in scope.
