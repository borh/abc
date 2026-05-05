# Code Quality Improvement Design

## Overview

Four independent workstreams addressing structural quality issues identified in code review of the ab-validator Rust workspace (12 crates, Japanese text processing/analysis). Each stream produces a standalone PR. Streams 1 and 2 are independent; stream 3 touches adjacent crates but has no code overlap with 1 or 2; stream 4 can run last or interleaved.

---

## Stream 1: ab-ir — Visitor Macro + Design Debt Cleanup

### Problem

Seven functions across `lib.rs` and `semantic_summary.rs` perform structurally identical recursive walks over the `Inline` AST:

| Function | File | Purpose |
|---|---|---|
| `collect_visible` | `lib.rs:814` | Collect visible text from Inline tree |
| `collect_visible` | `semantic_summary.rs:254` | Identical copy |
| `contains_unresolved_gaiji` | `lib.rs:795` | Check for any unresolved gaiji reference |
| `contains_gaiji` | `semantic_summary.rs:229` | Same structure, different predicate (any gaiji, resolved or not) |
| `collect_provenance` | `lib.rs:859` | Count provenance type frequencies |
| `inline_visible_text` | `lib.rs:905` | 4-line wrapper calling collect_visible |
| `inline_visible_text` | `semantic_summary.rs:246` | Identical wrapper |

Adding a new `Inline` variant requires finding and updating all 7 functions with no compiler assistance. The `Break { content: Vec<Inline> }` variant carries an always-empty field solely so `block_content()` can use a uniform accessor pattern—a structural convenience that binds empty allocations to every `Break` value.

Additionally, `FeatureMap` in `ab-morph-diff/src/model.rs` implements `Index<&str>` with a panic on missing key, violating the standard Rust convention for map types.

### Design

**Core: `macro_rules!` walker with zero-cost static dispatch.**

A declarative macro generates `walk_inline` functions for concrete visitor types. Each visitor struct implements an `InlineVisitor` trait with default no-op methods. Override only the variants you care about.

```rust
/// Visitor for walking the Inline tree. Every method has a default no-op
/// implementation so visitors only override what they need.
pub trait InlineVisitor {
    // Leaf nodes — enter only
    fn enter_text(&mut self, _value: &str, _provenance: &Provenance) {}
    fn enter_text_meta(&mut self, _value: &str, _attrs: &[StyleAttr], _provenance: &Provenance) {}
    fn enter_gaiji_ref(&mut self, _gaiji: &GaijiRef) {}
    fn enter_accent(&mut self, _resolved: &str, _provenance: &Provenance) {}
    fn enter_editor_note(&mut self, _text: &str) {}
    fn enter_raw(&mut self, _text: &str) {}

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

/// Walk an Inline tree with a visitor. Called internally by the macro.
/// Generated once and monomorphized per visitor type (zero runtime overhead).
macro_rules! walk_inline {
    ($visitor:expr, $node:expr) => {
        match $node {
            Inline::Text { value, provenance } => {
                $visitor.enter_text(value, provenance);
            }
            Inline::TextMeta { value, attrs, provenance } => {
                $visitor.enter_text_meta(value, &attrs, provenance);
            }
            Inline::GaijiRef(gaiji) => {
                $visitor.enter_gaiji_ref(gaiji);
            }
            Inline::Ruby { base, reading, placement, provenance, .. } => {
                $visitor.enter_ruby(base, reading, placement, provenance);
                for child in base {
                    walk_inline!($visitor, child);
                }
                $visitor.leave_ruby();
            }
            Inline::Style { style_type, content, attrs, provenance, .. } => {
                $visitor.enter_style(style_type, content, &attrs, provenance);
                for child in content {
                    walk_inline!($visitor, child);
                }
                $visitor.leave_style();
            }
            Inline::Scope { kind, content, provenance, .. } => {
                $visitor.enter_scope(kind, content, provenance);
                for child in content {
                    walk_inline!($visitor, child);
                }
                $visitor.leave_scope();
            }
            Inline::FontSize { size_type, level, content, provenance, .. } => {
                $visitor.enter_font_size(size_type, *level, content, provenance);
                for child in content {
                    walk_inline!($visitor, child);
                }
                $visitor.leave_font_size();
            }
            Inline::Warigaki { upper, lower, provenance, .. } => {
                $visitor.enter_warigaki(upper, lower, provenance);
                for child in upper.iter().chain(lower.iter()) {
                    walk_inline!($visitor, child);
                }
                $visitor.leave_warigaki();
            }
            Inline::FigureRef { source, caption, provenance, .. } => {
                $visitor.enter_figure_ref(source, caption, provenance);
                for child in caption {
                    walk_inline!($visitor, child);
                }
                $visitor.leave_figure_ref();
            }
            Inline::Accent { resolved, provenance, .. } => {
                $visitor.enter_accent(resolved, provenance);
            }
            Inline::EditorNote { .. } => {
                $visitor.enter_editor_note("");
            }
            Inline::Raw { .. } => {
                $visitor.enter_raw("");
            }
        }
    };
}
```

**Current functions replaced by concrete visitors:**

```rust
// was: fn collect_visible(value: &Inline, out: &mut String)
// was: fn inline_visible_text(content: &[Inline]) -> String
struct VisibleCollector { out: String }
impl InlineVisitor for VisibleCollector {
    fn enter_text(&mut self, value: &str, _: &Provenance) { self.out.push_str(value); }
    fn enter_text_meta(&mut self, value: &str, _: &[StyleAttr], _: &Provenance) { self.out.push_str(value); }
    fn enter_gaiji_ref(&mut self, gaiji: &GaijiRef) {
        if let Some(resolved) = &gaiji.resolved { self.out.push_str(resolved); }
    }
    fn enter_accent(&mut self, resolved: &str, _: &Provenance) { self.out.push_str(resolved); }
}

// was: fn contains_unresolved_gaiji(content: &[Inline]) -> bool
// was: fn contains_gaiji(content: &[Inline]) -> bool
struct GaijiChecker { unresolved_only: bool, found: bool }
impl InlineVisitor for GaijiChecker {
    fn enter_gaiji_ref(&mut self, gaiji: &GaijiRef) {
        if !self.unresolved_only || gaiji.resolved.is_none() { self.found = true; }
    }
}

// was: fn collect_provenance(value: &Inline, counts: &mut ProvenanceCounts)
// was: fn inline_provenance(value: &Inline) -> Provenance
struct ProvenanceCollector { counts: ProvenanceCounts }
impl InlineVisitor for ProvenanceCollector {
    fn enter_text(&mut self, _: &str, p: &Provenance) { self.counts.increment(p); }
    fn enter_text_meta(&mut self, _: &str, _: &[StyleAttr], p: &Provenance) { self.counts.increment(p); }
    fn enter_gaiji_ref(&mut self, gaiji: &GaijiRef) { self.counts.increment(&gaiji.provenance); }
    fn enter_ruby(&mut self, _: &[Inline], _: &str, _: &RubyPlacement, p: &Provenance) { self.counts.increment(p); }
    // ... one increment per variant with provenance
}
```

**Break variant cleanup:**

```rust
// Before:
Block::Break { kind: BreakKind, content: Vec<Inline> }  // content always empty

// After:
Block::Break { kind: BreakKind }
```

`block_content()` and `block_content_mut()` are updated to handle `Break` explicitly (return empty slice). Block-level traversal functions (`provenance_counts`, `visible_projection`, `semantic_summary`) match on `Break` directly.

**FeatureMap Index removal:**

Remove the `impl Index<&str> for FeatureMap`. Convert all call sites from `map["key"]` to `map.get("key").context("feature key not found")` or equivalent `.expect()` with a descriptive message.

### Files

| Action | Path |
|---|---|
| Modify | `crates/ab-ir/src/lib.rs` — add visitor trait, macro, concrete visitors; remove old walk functions; fix Break variant; remove redundant helpers |
| Modify | `crates/ab-ir/src/semantic_summary.rs` — replace `collect_gaiji`, `contains_gaiji`, `collect_visible`, `inline_visible_text` with visitors |
| Modify | `crates/ab-morph-diff/src/model.rs` — remove `Index<&str>` impl |
| Modify | Callers of `FeatureMap["..."]` syntax — convert to `.get()` |
| Modify | Consumers matching `Block::Break { content, .. }` — update patterns |

### Risk

Low. Visitor macro produces identical code to hand-written matches. Existing tests for `visible_projection`, `semantic_summary`, and gaiji detection serve as regression gates. The only observable change is removal of `content` from `Block::Break`.

---

## Stream 2: ab-morph-run God Crate Split

### Problem

`ab-morph-run` is 15,377 lines across 8 source files. `summary.rs` alone is 6,228 lines with 165 functions and 22 public types. The crate has absorbed at least 5 distinct responsibilities:
- Pipeline orchestration (serial/parallel/warehouse)
- Output formatting (JSONL, Parquet, compact/full profiles)
- Summarization (17 different `summarize_*` public functions)
- Parquet warehouse I/O (schema, rows, writer, SQL)
- CLI argument adapter layer (10+ `*Arg` enum types with hand-written `into_library()` methods)

10 `#[allow(clippy::too_many_arguments)]` suppressions signal functions begging for builders.

### Design

**New module structure:**

```
crates/ab-morph-run/src/
  lib.rs              (~200 lines — public API re-exports, run entry points)
  pipeline.rs         (~600 lines — run_analyze_aat, serial/parallel dispatch)
  options.rs          (~150 lines — SerialRunOptions, WarehouseRunOptions, compact/nway options moved from lib.rs)
  output.rs           (unchanged — JSONL/ZST I/O)
  script.rs           (unchanged — script classification)
  select.rs           (unchanged — source ID → AAT path resolution)
  compact.rs          (unchanged — compact comparison types)
  nway.rs             (unchanged — nway comparison types)
  summary/
    mod.rs            (re-exports only)
    types.rs          (all 22 public enums/structs: CompactSummaryGroupBy, CompactSummarySort, NwaySummarySort, WarehouseFeatureProfile, SummaryExclusions, etc.)
    compact.rs        (summarize_compact_comparisons, summarize_compact_examples, summarize_compact_differences)
    nway.rs           (summarize_nway, summarize_nway_patterns, summarize_nway_pattern_counts)
    warehouse.rs      (summarize_warehouse_errors, summarize_warehouse_nway, summarize_warehouse_nway_patterns,
                       summarize_warehouse_pairwise, summarize_warehouse_pattern_examples,
                       summarize_warehouse_regions, materialize_warehouse_core_feature_pattern_counts)
    patterns.rs       (NwayPatternKind, NwayPatternOptions, NwayPatternRow, WarehousePatternOptions,
                       WarehousePatternExampleOptions, related helper functions)
    write.rs          (write_warehouse_nway_patterns_duckdb_tsv, write_warehouse_pattern_examples_duckdb_tsv,
                       write_warehouse_regions_duckdb_tsv)
  main.rs             (~800 lines — CLI with reduced boilerplate)
```

**New crate: `ab-warehouse`**

```
crates/ab-warehouse/
  Cargo.toml
  src/
    lib.rs            (re-exports)
    schema.rs         (WarehouseTable, WarehousePaths, row type definitions)
    rows.rs           (row struct implementations, conversions)
    writer.rs         (WarehouseWriter, staged parquet writes)
    sql.rs            (DuckDB SQL report templates)
```

Dependencies: `arrow-array`, `arrow-schema`, `parquet`, `serde`, `serde_json`, `anyhow`. No dependency on `ab-morph-run` or any crate in the workspace beyond data dependencies (ab-morph-diff types for row definitions, if needed—or keep row types generic enough to avoid the dependency).

**Pipeline module:** The `run_analyze_aat` function currently mixes pipeline orchestration with output format selection in a 200+ line function with deep nesting for the `jobs == 1` serial path versus the parallel path. Extract to `pipeline.rs` with the serial and parallel paths as separate functions, sharing a common preparation phase.

**CLI cleanup:** `main.rs` has 10+ enum types like `SummaryGroupByArg`, `NwaySummarySortArg`, etc., each with hand-written `into_library()` conversions. Solution: where possible, derive `clap::ValueEnum` directly on library types. Where not possible (types in other crates), use a simple declarative macro that generates the conversion.

### Files

| Action | Path |
|---|---|
| Split | `crates/ab-morph-run/src/summary.rs` → `summary/{types,compact,nway,warehouse,patterns,write}.rs` |
| Extract | `crates/ab-morph-run/src/lib.rs` → `pipeline.rs` + `options.rs` + reduced `lib.rs` |
| Move | `crates/ab-morph-run/src/warehouse/` → `crates/ab-warehouse/src/` |
| Create | `crates/ab-warehouse/Cargo.toml` |
| Modify | `crates/ab-morph-run/Cargo.toml` — depend on ab-warehouse |
| Modify | `Cargo.toml` — add ab-warehouse workspace member |
| Modify | `crates/ab-morph-run/src/main.rs` — simplify CLI conversions |
| Modify | Consumers of warehouse types — update imports |

### Risk

Medium. This is the largest change. The split must preserve all `pub` visibility, re-exports, and module paths. Strategy: use `pub use` re-exports in the old locations as a migration shim, then remove them once consumers are updated. The existing 375 tests will catch import and visibility issues immediately.

---

## Stream 3: Interning Removal + Robustness Fixes

### Problem

1. **Thread-local interner leaks memory:** `FEATURE_STRING_INTERNER` in `ab-morph-diff/src/model.rs` is never cleared, grows unbounded across texts processed in a run. Each rayon worker thread gets its own copy (no cross-thread sharing).

2. **`eprintln!` in library code:** Chunking warnings and dictionary-path diagnostics write to stderr directly, bypassing structured error handling. Consumers can't capture, test, or redirect them.

3. **`panic!` in production code:** Three locations crash on runtime conditions:
   - `ab-coverage/src/adapter.rs:29` — unknown parser ID
   - `ab-morph-analyzers/src/vibrato.rs:91` — missing dictionary path
   - `ab-morph-analyzers/src/vaporetto.rs:163` — missing dictionary path

### Design

**Interning removal:**

Replace `InternedString` newtype with plain `Arc<str>`. Remove the thread-local interner and its supporting code (`intern()`, `ptr_eq()`, `PartialEq`/`Hash` impls). The type aliases `FeatureKey` and `FeatureValue` remain:

```rust
// Before
pub type FeatureKey = InternedString;
pub type FeatureValue = InternedString;

// After
pub type FeatureKey = Arc<str>;
pub type FeatureValue = Arc<str>;
```

Callers using `InternedString::as_str()` switch to `Arc::as_ref()`. `From<&str>` for `InternedString` becomes `Arc::from`. The `Display`, `Debug`, `Serialize` impls are replaced by `Arc<str>`'s standard implementations.

**eprintln! → structured warnings:**

| Location | Fix |
|---|---|
| `ab-morph-run/src/lib.rs:543,955,962` — chunking/progress warnings | Add `warnings: Vec<ChunkWarning>` field to `AnalysisResult`. The caller (main.rs or test) decides whether to print them. |
| `ab-morph-analyzers/src/vibrato.rs:215` — hard-split warning | Return `HardSplitWarning` in the analyzer's output struct. |
| `ab-morph-analyzers/src/sudachi.rs:121` — hard-split warning | Same pattern as vibrato. |
| `ab-morph-analyzers/src/sudachi.rs:345,366` — skipped smoke tests | These are test-only `eprintln!` → switch to `println!` (acceptable in tests). |

Following the existing `ProjectionWarning` pattern from `ab-ir` for consistency.

**panic! → Result:**

| Location | Fix |
|---|---|
| `ab-coverage/src/adapter.rs:29` | Return `Err(anyhow!("unknown parser id: {other}"))` from the function |
| `ab-morph-analyzers/src/vibrato.rs:91` | `VibratoAnalyzer::new()` returns `Result<Self>` instead of panicking |
| `ab-morph-analyzers/src/vaporetto.rs:163` | `VaporettoAnalyzer::new()` returns `Result<Self>` instead of panicking |

### Files

| Action | Path |
|---|---|
| Modify | `crates/ab-morph-diff/src/model.rs` — remove interner, InternedString → Arc<str> |
| Modify | All callers of `InternedString` — update imports and usage |
| Modify | `crates/ab-morph-run/src/lib.rs` — warnings → structured |
| Modify | `crates/ab-morph-analyzers/src/vibrato.rs` — warnings → structured, panic → Result |
| Modify | `crates/ab-morph-analyzers/src/vaporetto.rs` — panic → Result |
| Modify | `crates/ab-morph-analyzers/src/sudachi.rs` — warnings → structured |
| Modify | `crates/ab-coverage/src/adapter.rs` — panic → Result |

### Risk

Low to medium. Removing the interner is a pure simplification with no behavioral change — `Arc<str>` already provides `Eq`/`Hash` by content. The panic→Result changes are breaking for callers that `.unwrap()` the previous infallible constructors; these callers (tests, main.rs) need `.context()` or `?` added. The `eprintln!`→structured changes are additive — old behavior is preserved if the caller chooses to print.

---

## Stream 4: Clippy Sweep

Add `must_use` attributes and `# Errors` documentation, fix or-patterns, add crate metadata.

### Cargo.toml metadata for all 12 crates

```toml
[package]
description = "..."              # from existing crate docs or README context
repository = "https://github.com/..."
readme = "README.md"              # or omit if none exists
keywords = ["japanese", "text-processing", ...]
categories = ["text-processing", ...]
```

### Fixes

- **or-patterns:** `Some(0) | Some(2)` → `Some(0 | 2)` in ~5-10 locations
- **similar_names:** Rename `interner`/`interned` → more distinct names
- **single_char_names:** Expand test binding names
- **must_use_candidate:** Add `#[must_use]` to pure getter/constructor functions
- **missing_errors_doc:** Add `# Errors` doc sections to Result-returning `pub fn`

### Risk

Near-zero. All are additive (attributes, docs) or mechanical (pattern syntax).

---

## Dependency Graph

```
Stream 1 (ab-ir visitor) ── independent ──┐
                                           ├── Stream 4 (clippy) runs last
Stream 2 (god crate split) ─ independent ─┘
                                           │
Stream 3 (interner/robustness) ───┘
```

Streams 1 and 2 have zero code overlap. Streams 1 and 3 both touch `ab-morph-diff/src/model.rs` — Stream 1 removes the `Index<&str>` impl (~line 213), Stream 3 removes the `InternedString` newtype (~lines 1-100). These are non-overlapping regions; merge conflicts are unlikely. Stream 4 touches everything but only adds attributes/documentation. All four can be worked on in any order.

---

## Verification

After each stream:
- `cargo build --workspace` — must compile cleanly
- `cargo test --workspace` — all 375 tests pass
- `cargo clippy --workspace -- -W clippy::pedantic -W clippy::nursery -W clippy::cargo` — warning count decreases, no new warnings

Specific to each stream:
- Stream 1: `visible_projection`, `semantic_summary`, and gaiji resolution tests pass identically
- Stream 2: All existing tests pass, `ab-warehouse` compiles and its tests pass
- Stream 3: New unit tests for `Result` error paths pass; existing feature-map tests pass with `Arc<str>`
- Stream 4: Clippy warning count is measurably lower

---

## Non-Goals

The following issues from the code reviews are explicitly excluded from this plan:
- Dictionary resolution duplication between Vibrato/Vaporetto (follow-up plan)
- `ab-diff-utils/src/frequency.rs` O(n²) example dedup (low priority)
- `ab-source-syntax` fullwidth/ASCII marker deduplication (independent refactor)
- `ab-coverage` 4 failing integration tests (missing data files — infrastructure/config issue)
- `ab-plaintext` `canonicalize_line_endings` byte-level optimization (micro-optimization)
- Large options struct builder patterns (addressed partially by options.rs extraction in Stream 2)
