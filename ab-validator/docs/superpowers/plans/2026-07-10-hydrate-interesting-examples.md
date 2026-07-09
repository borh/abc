# Hydrated Examples for Interestingness Reports — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A `ab-morph-run hydrate-interesting` subcommand that turns the span references in a `summarize-warehouse-interesting` JSON artifact into a self-contained `examples.md` + `examples.json` bundle: projected-text snippets, per-analyzer token tables, AAT-reconstructed Aozora-markup slices, AAT node context, and work metadata with author names.

**Architecture:** A new `hydrate` module in `crates/ab-morph-run`, sibling to `calibration` (both are pure post-processors of ranking artifacts). Per-source resolution loads each AAT file once and re-projects with `ab_plaintext::visible_text_projection_with_spans` (offset-identical to the pipeline by construction). Warehouse parquet is read **streaming** (batch-by-batch, filtered to the needed source ids) so full-corpus `morphemes.parquet` never lives in memory. Every layer degrades independently into a per-example `errors[]`; the command exits non-zero only on structural failure.

**Tech Stack:** Rust (existing `ab-morph-run` crate), `arrow-array`/`parquet` (already deps), `ab-plaintext` (projection), `ab-encoding` (sha256 helper — new workspace-internal dep), `serde`/`serde_json`, `chrono`, `clap`. Tests: `tempfile` fixtures + the `ab-warehouse` `WarehouseWriter`.

**Spec:** `ab-validator/docs/superpowers/specs/2026-07-10-hydrate-interesting-examples-design.md` (read it first; this plan implements it exactly).

## Global Constraints

- All commands run from `/home/bor/Projects/soranoha/ab-validator/` (the cargo workspace root).
- Test command: `cargo test -p ab-morph-run hydrate` (module-scoped) and `cargo test -p ab-morph-run` before the final commit. Lint: `cargo clippy -p ab-morph-run --all-targets -- -D warnings` and `cargo fmt --check`.
- Determinism: output is a pure function of declared inputs; the ONLY wall-clock value is the `built_at_utc` string **passed in via `HydrateOptions`** (the CLI fills it; tests pass a constant). Never call `Utc::now()` inside the `hydrate` module.
- All iteration orders must be explicit: ranked rows in input order, examples by `(source_id, region_index)` input order, analyzers sorted lexicographically, maps are `BTreeMap`.
- Error vocabulary (exact strings, used as prefixes in `errors[]`): `aat-missing`, `projection-mismatch`, `markup-unreconstructable`, `work-record-missing`, `person-record-missing`, `works-sidecar-missing`.
- Region marker chars: `【` and `】`. Ellipsis for markup gaps: `…`.
- No new external dependencies beyond workspace members (`ab-encoding`) — no zip, no encoding_rs, no aozora-pipeline.

---

### Task 1: Module scaffold, options, and the snippet window primitive

**Files:**
- Create: `crates/ab-morph-run/src/hydrate/mod.rs`
- Create: `crates/ab-morph-run/src/hydrate/source_context.rs`
- Modify: `crates/ab-morph-run/src/lib.rs` (add `mod hydrate;` after `mod compact;` alphabetically — line ~3)

**Interfaces:**
- Produces: `hydrate::HydrateOptions` (all later tasks), `source_context::snippet_window(text: &str, char_start: u64, char_end: u64, context: usize) -> Result<Snippet>`, `Snippet { before: String, region: String, after: String }`, `Snippet::marked(&self) -> String` (returns `…before【region】after…` without TSV scrubbing).
- Consumes: nothing from other tasks.

- [ ] **Step 1: Write the failing test**

In `crates/ab-morph-run/src/hydrate/source_context.rs`:

```rust
//! Per-source resolution: AAT loading, re-projection, snippet windows,
//! and Aozora-markup reconstruction (spec Layers 1, 3, 4).

use anyhow::{Result, bail};
use serde::Serialize;

/// A snippet window around a region, parts kept separate so JSON consumers
/// can re-mark (spec §Layer 1).
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Snippet {
    pub before: String,
    pub region: String,
    pub after: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn snippet_window_slices_by_char_index_with_context() {
        let text = "abc吾輩は猫であるxyz";
        let s = snippet_window(text, 6, 10, 2).unwrap();
        assert_eq!(s.before, "輩は");
        assert_eq!(s.region, "猫である");
        assert_eq!(s.after, "xy");
        assert_eq!(s.marked(), "…輩は【猫である】xy…");
    }

    #[test]
    fn snippet_window_clips_at_document_bounds() {
        let s = snippet_window("猫である", 0, 2, 40).unwrap();
        assert_eq!(s.before, "");
        assert_eq!(s.region, "猫で");
        assert_eq!(s.after, "ある");
        // No leading ellipsis when the window reaches the document start,
        // no trailing ellipsis when it reaches the end.
        assert_eq!(s.marked(), "【猫で】ある");
    }

    #[test]
    fn snippet_window_rejects_out_of_range_span() {
        let err = snippet_window("abc", 0, 10, 0).unwrap_err();
        assert!(err.to_string().contains("out of range"));
    }
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p ab-morph-run hydrate::source_context -- --nocapture`
Expected: FAIL to compile with "cannot find function `snippet_window`" (and, first, "unresolved module" until Step 3 adds the module wiring — that is the same failure class; proceed).

- [ ] **Step 3: Write minimal implementation**

Append to `source_context.rs`:

```rust
impl Snippet {
    /// `…before【region】after…` — the Markdown display form. Ellipses appear
    /// only where the window was clipped short of the document bounds, which
    /// the constructor encodes by leaving `before`/`after` at full context
    /// length; callers never re-check bounds.
    #[must_use]
    pub fn marked(&self) -> String {
        format!(
            "{}{}【{}】{}{}",
            self.leading_ellipsis, self.before, self.region, self.after, self.trailing_ellipsis
        )
    }
}
```

Wait — `marked()` needs to know whether the window was clipped. Give `Snippet` two private bools instead of string fields (kept out of JSON):

```rust
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Snippet {
    pub before: String,
    pub region: String,
    pub after: String,
    #[serde(skip)]
    at_doc_start: bool,
    #[serde(skip)]
    at_doc_end: bool,
}

impl Snippet {
    #[must_use]
    pub fn marked(&self) -> String {
        let lead = if self.at_doc_start { "" } else { "…" };
        let trail = if self.at_doc_end { "" } else { "…" };
        format!("{lead}{}【{}】{}{trail}", self.before, self.region, self.after)
    }
}

/// Slices `text` by char index (never byte index) into a window of up to
/// `context` chars on each side of `[char_start, char_end)`. Errors when the
/// span is inverted or exceeds the text's char count.
pub fn snippet_window(text: &str, char_start: u64, char_end: u64, context: usize) -> Result<Snippet> {
    let chars: Vec<char> = text.chars().collect();
    let total = chars.len() as u64;
    if char_start > char_end || char_end > total {
        bail!("span [{char_start}, {char_end}) is out of range for a text of {total} chars");
    }
    let start = char_start as usize;
    let end = char_end as usize;
    let context_start = start.saturating_sub(context);
    let context_end = (end + context).min(chars.len());
    Ok(Snippet {
        before: chars[context_start..start].iter().collect(),
        region: chars[start..end].iter().collect(),
        after: chars[end..context_end].iter().collect(),
        at_doc_start: context_start == 0,
        at_doc_end: context_end == chars.len(),
    })
}
```

(Note: `calibration::label_export::snippet` stays untouched — its TSV scrubbing and always-ellipsis format serve the blind-labeling contract; this window keeps parts separate per the spec's JSON shape.)

Create `crates/ab-morph-run/src/hydrate/mod.rs`:

```rust
//! Hydrates a `summarize-warehouse-interesting` ranking artifact into a
//! self-contained example bundle (examples.md + examples.json). Pure
//! post-processor: reads the artifact, the warehouse run dir, the AAT files
//! referenced by `sources.aat_path`, and (optionally) an ABC catalog export;
//! never modifies any input. Spec:
//! docs/superpowers/specs/2026-07-10-hydrate-interesting-examples-design.md

use std::path::PathBuf;

pub mod source_context;

/// Options for [`run_hydrate_interesting`].
#[derive(Debug, Clone)]
pub struct HydrateOptions {
    /// Ranking artifact (JSON format from `summarize-warehouse-interesting`).
    pub interesting: PathBuf,
    /// Warehouse run directory (sources/morphemes/… parquet).
    pub run_dir: PathBuf,
    /// Output directory; receives `examples.md` and `examples.json`.
    pub output_dir: PathBuf,
    /// ABC catalog export with `works/` and `persons/` (author names).
    pub abc_catalog: Option<PathBuf>,
    /// Chars of context on each side of the region (default 40).
    pub context_chars: usize,
    /// Hydrate only the first N ranked rows (anomalies are always hydrated).
    pub limit: Option<usize>,
    /// Overwrite existing outputs.
    pub force: bool,
    /// Provenance timestamp, filled by the caller (CLI: now; tests: constant).
    pub built_at_utc: String,
}
```

In `lib.rs`, after `mod compact;`:

```rust
mod hydrate;
```

and extend the crate's public re-exports (near `pub use import_aozora::…`):

```rust
pub use hydrate::HydrateOptions;
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test -p ab-morph-run hydrate::source_context`
Expected: 3 passed.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-run/src/hydrate crates/ab-morph-run/src/lib.rs
git commit -m "feat(hydrate): module scaffold, options, snippet window primitive"
```

---

### Task 2: Aozora-markup reconstruction from typed AAT nodes

**Files:**
- Modify: `crates/ab-morph-run/src/hydrate/source_context.rs`

**Interfaces:**
- Consumes: `ab_plaintext::ProjectionSpan` (fields: `projected_char_start`, `projected_char_end`, `aat_pointer: String` — RFC 6901 like `/blocks/0/content/3`, `inline_kind`, `is_ruby_base`, `is_gaiji`, `is_note`).
- Produces:
  - `AatNodeRef { pointer: String, inline_kind: String, is_ruby_base: bool, is_gaiji: bool }` (Serialize)
  - `AozoraMarkup { text: String, byte_start: u64, byte_end: u64, approximate_pointers: Vec<String> }` (Serialize)
  - `reconstruct_markup(aat: &serde_json::Value, spans: &[ab_plaintext::ProjectionSpan], char_start: u64, char_end: u64) -> Result<(AozoraMarkup, Vec<AatNodeRef>)>` — the error is the `markup-unreconstructable` case; callers convert it to an `errors[]` entry.

- [ ] **Step 1: Write the failing tests**

Append to `source_context.rs` tests (the fixture mirrors the typed AAT observed in `aozora-full-repin-1a4f864`; byte spans index the adapter's sanitized source, so the fixture invents self-consistent spans):

```rust
    use serde_json::json;

    /// Sanitized-source layout the spans below describe (byte offsets):
    ///   0..15  text  "このあいびきは"  — wait, keep it byte-countable:
    /// Use ASCII-measurable pieces: "AB" (2b) + "｜仏蘭西《フランス》" (30b)
    /// + "CD" (2b) + "端物《はもの》" (21b) + gaiji marker (20b) + "EF" (2b).
    fn typed_aat_fixture() -> serde_json::Value {
        json!({
            "version": 1,
            "work_id": "src-a",
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "AB",
                     "span": {"byte_start": 0, "byte_end": 2, "line_start": 1, "line_end": 1}},
                    {"kind": "ruby", "base": "仏蘭西", "reading": "フランス", "direction": "right",
                     "span": {"byte_start": 2, "byte_end": 32, "line_start": 1, "line_end": 1}},
                    {"kind": "text", "value": "CD",
                     "span": {"byte_start": 32, "byte_end": 34, "line_start": 1, "line_end": 1}},
                    {"kind": "ruby", "base": "端物", "reading": "はもの", "direction": "right",
                     "span": {"byte_start": 34, "byte_end": 55, "line_start": 1, "line_end": 1}},
                    {"kind": "gaiji", "description": "小書き片仮名ン", "resolved": "ン",
                     "jis_code": "237-11", "unresolved_reason": null,
                     "span": {"byte_start": 55, "byte_end": 75, "line_start": 1, "line_end": 1}},
                    {"kind": "text", "value": "EF",
                     "span": {"byte_start": 75, "byte_end": 77, "line_start": 1, "line_end": 1}}
                ]
            }],
            "meta": {"adapter": "aozora", "adapter_version": "fixture",
                     "source_encoding": "windows-31j", "parse_complete": true,
                     "source_hash": "sha256:00", "warnings": []}
        })
    }
    // Projected text: "AB" + "仏蘭西" + "CD" + "端物" + "ン" + "EF"
    //  char offsets:   0-2    2-5      5-7    7-9     9-10   10-12

    fn fixture_spans(aat: &serde_json::Value) -> (String, Vec<ab_plaintext::ProjectionSpan>) {
        ab_plaintext::visible_text_projection_with_spans(aat)
    }

    #[test]
    fn projection_of_fixture_matches_expected_offsets() {
        let aat = typed_aat_fixture();
        let (text, spans) = fixture_spans(&aat);
        assert_eq!(text, "AB仏蘭西CD端物ンEF");
        assert!(spans.iter().any(|s| s.is_ruby_base));
        assert!(spans.iter().any(|s| s.is_gaiji));
    }

    #[test]
    fn markup_reconstruction_is_verbatim_for_text_and_ruby_with_pipe() {
        let aat = typed_aat_fixture();
        let (_, spans) = fixture_spans(&aat);
        // Region = the first ruby base, chars [2,5) ("仏蘭西").
        let (markup, nodes) = reconstruct_markup(&aat, &spans, 2, 5).unwrap();
        // Rendered "仏蘭西《フランス》" is 27 bytes; the span is 30 bytes,
        // difference exactly 3 ⇒ ｜ prefix restored, node byte-verified.
        assert_eq!(markup.text, "｜仏蘭西《フランス》");
        assert_eq!(markup.byte_start, 2);
        assert_eq!(markup.byte_end, 32);
        assert!(markup.approximate_pointers.is_empty());
        assert_eq!(nodes.len(), 1);
        assert_eq!(nodes[0].pointer, "/blocks/0/content/1");
        assert!(nodes[0].is_ruby_base);
    }

    #[test]
    fn markup_reconstruction_flags_gaiji_as_approximate() {
        let aat = typed_aat_fixture();
        let (_, spans) = fixture_spans(&aat);
        // Region = chars [7,10) ("端物ン"): second ruby (21b, no pipe:
        // rendered "端物《はもの》" is exactly 21 bytes) + gaiji.
        let (markup, nodes) = reconstruct_markup(&aat, &spans, 7, 10).unwrap();
        assert_eq!(markup.text, "端物《はもの》※［＃小書き片仮名ン］");
        assert_eq!(markup.approximate_pointers, vec!["/blocks/0/content/4".to_owned()]);
        assert_eq!(nodes.len(), 2);
    }

    #[test]
    fn markup_reconstruction_fails_on_empty_legacy_raw_node() {
        let aat = json!({
            "version": 1, "work_id": "src-legacy",
            "blocks": [{"kind": "paragraph", "content": [
                {"kind": "raw", "source": "",
                 "span": {"byte_start": 0, "byte_end": 10, "line_start": 1, "line_end": 1},
                 "x-provenance": "parser-derived", "x-source-marker-kind": "ruby"}
            ]}],
            "meta": {"adapter": "aozora", "adapter_version": "legacy",
                     "source_encoding": "windows-31j", "parse_complete": true,
                     "source_hash": "sha256:00", "warnings": []}
        });
        // A raw node with empty source projects no chars, so build a span
        // pointing at it manually (legacy corpora can produce such rows).
        let span = ab_plaintext::ProjectionSpan {
            projected_char_start: 0, projected_char_end: 1,
            aat_pointer: "/blocks/0/content/0".to_owned(),
            inline_kind: "raw".to_owned(),
            is_ruby_base: false, is_gaiji: false, is_note: false,
        };
        let err = reconstruct_markup(&aat, &[span], 0, 1).unwrap_err();
        assert!(err.to_string().contains("markup-unreconstructable"));
    }
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p ab-morph-run hydrate::source_context`
Expected: FAIL to compile — `reconstruct_markup`, `AozoraMarkup`, `AatNodeRef` not found.

- [ ] **Step 3: Write the implementation**

Append to `source_context.rs` (full implementation; the helpers follow the main entry point):

```rust
/// One contributing AAT node's identity + flags (spec §Layer 4).
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct AatNodeRef {
    pub pointer: String,
    pub inline_kind: String,
    pub is_ruby_base: bool,
    pub is_gaiji: bool,
}

/// The reconstructed markup slice (spec §Layer 3). `approximate_pointers`
/// lists nodes rendered semantically rather than byte-verified; empty means
/// the slice is verbatim sanitized-source markup.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct AozoraMarkup {
    pub text: String,
    pub byte_start: u64,
    pub byte_end: u64,
    pub approximate_pointers: Vec<String>,
}

/// Reconstructs the Aozora-markup slice covering projected chars
/// `[char_start, char_end)` from the typed AAT nodes the projection spans
/// point at. Errors (`markup-unreconstructable`) when a contributing node
/// has no renderable content.
pub fn reconstruct_markup(
    aat: &serde_json::Value,
    spans: &[ab_plaintext::ProjectionSpan],
    char_start: u64,
    char_end: u64,
) -> Result<(AozoraMarkup, Vec<AatNodeRef>)> {
    // Contributing spans, deduped by pointer, in document order. Spans are
    // sorted by projected offset already (projection contract).
    let mut pointers_seen = std::collections::BTreeSet::new();
    let mut contributing = Vec::new();
    for span in spans {
        if span.projected_char_end <= char_start || span.projected_char_start >= char_end {
            continue;
        }
        if pointers_seen.insert(span.aat_pointer.clone()) {
            contributing.push(span);
        }
    }
    if contributing.is_empty() {
        bail!("markup-unreconstructable: no projection spans cover chars [{char_start}, {char_end})");
    }

    let mut rendered = String::new();
    let mut approximate = Vec::new();
    let mut nodes = Vec::new();
    let mut byte_start = u64::MAX;
    let mut byte_end = 0u64;
    let mut prev_byte_end: Option<u64> = None;

    for span in &contributing {
        let node = resolve_spanned_node(aat, &span.aat_pointer).ok_or_else(|| {
            anyhow::anyhow!("markup-unreconstructable: pointer {} has no spanned node", span.aat_pointer)
        })?;
        let (node_start, node_end) = node_byte_span(node).ok_or_else(|| {
            anyhow::anyhow!("markup-unreconstructable: node {} has no byte span", span.aat_pointer)
        })?;
        // Coverage check: a gap between consecutive contributing nodes means
        // a non-projecting marker sits inside the region — render "…" and
        // flag the slice approximate (spec §Layer 3 step 3).
        if let Some(prev) = prev_byte_end {
            if node_start > prev {
                rendered.push('…');
                approximate.push(span.aat_pointer.clone());
            }
        }
        let piece = render_node(node, node_end - node_start).ok_or_else(|| {
            anyhow::anyhow!("markup-unreconstructable: node {} ({}) has no renderable content",
                span.aat_pointer, span.inline_kind)
        })?;
        match piece {
            Rendered::Verbatim(text) => rendered.push_str(&text),
            Rendered::Approximate(text) => {
                rendered.push_str(&text);
                approximate.push(span.aat_pointer.clone());
            }
        }
        byte_start = byte_start.min(node_start);
        byte_end = byte_end.max(node_end);
        prev_byte_end = Some(node_end);
        nodes.push(AatNodeRef {
            pointer: span.aat_pointer.clone(),
            inline_kind: span.inline_kind.clone(),
            is_ruby_base: span.is_ruby_base,
            is_gaiji: span.is_gaiji,
        });
    }
    approximate.sort();
    approximate.dedup();
    Ok((AozoraMarkup { text: rendered, byte_start, byte_end, approximate_pointers: approximate }, nodes))
}

enum Rendered {
    Verbatim(String),
    Approximate(String),
}

/// Resolves an RFC 6901 pointer; when the pointed node lacks a `span` field
/// (e.g. the inner text of a `style` node), walks up truncated pointer
/// prefixes to the nearest spanned ancestor (spec §Layer 3 step 1).
fn resolve_spanned_node<'a>(aat: &'a serde_json::Value, pointer: &str) -> Option<&'a serde_json::Value> {
    let mut path = pointer.to_owned();
    loop {
        if let Some(node) = aat.pointer(&path) {
            if node_byte_span(node).is_some() {
                return Some(node);
            }
        }
        match path.rfind('/') {
            None | Some(0) => return None,
            Some(idx) => path.truncate(idx),
        }
    }
}

fn node_byte_span(node: &serde_json::Value) -> Option<(u64, u64)> {
    let span = node.get("span")?;
    Some((span.get("byte_start")?.as_u64()?, span.get("byte_end")?.as_u64()?))
}

/// Renders one typed AAT node back to Aozora markup. `span_len` is the
/// node's sanitized-source byte length, used for byte-length verification
/// (spec §Layer 3 step 2).
fn render_node(node: &serde_json::Value, span_len: u64) -> Option<Rendered> {
    let kind = node.get("kind")?.as_str()?;
    match kind {
        "text" => {
            let value = node.get("value")?.as_str()?.to_owned();
            Some(Rendered::Verbatim(value))
        }
        "raw" => {
            let source = node.get("source")?.as_str()?;
            if source.is_empty() {
                return None; // legacy corpora: unrenderable
            }
            Some(Rendered::Verbatim(source.to_owned()))
        }
        "ruby" => {
            let base = node.get("base")?.as_str()?;
            let reading = node.get("reading")?.as_str()?;
            let plain = format!("{base}《{reading}》");
            let piped = format!("｜{plain}");
            if plain.len() as u64 == span_len {
                Some(Rendered::Verbatim(plain))
            } else if piped.len() as u64 == span_len {
                Some(Rendered::Verbatim(piped))
            } else {
                Some(Rendered::Approximate(plain))
            }
        }
        "gaiji" => {
            let description = node.get("description").and_then(serde_json::Value::as_str).unwrap_or("");
            if description.is_empty() {
                return None;
            }
            Some(Rendered::Approximate(format!("※［＃{description}］")))
        }
        // style/tcy: render inner text content, always approximate (the
        // surrounding marker form is not recoverable byte-exactly).
        "style" | "tcy" => {
            let inner: String = node
                .get("content")?
                .as_array()?
                .iter()
                .filter_map(|child| child.get("value").and_then(serde_json::Value::as_str))
                .collect();
            if inner.is_empty() { None } else { Some(Rendered::Approximate(inner)) }
        }
        _ => None,
    }
}
```

Add `use anyhow::anyhow;` if needed (or fully qualify as shown).

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p ab-morph-run hydrate::source_context`
Expected: 7 passed (3 from Task 1 + 4 new). If `markup_reconstruction_is_verbatim_for_text_and_ruby_with_pipe` fails on the pointer value, print the actual spans (`dbg!(&spans)`) — the projection walker may emit a different pointer for paragraph content; adjust the fixture's expected pointer to the actual walker output (the walker is the source of truth), NOT the reconstruction logic.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-run/src/hydrate/source_context.rs
git commit -m "feat(hydrate): reconstruct aozora markup from typed AAT nodes"
```

---

### Task 3: Streaming warehouse table readers

**Files:**
- Create: `crates/ab-morph-run/src/hydrate/tables.rs`
- Modify: `crates/ab-morph-run/src/hydrate/mod.rs` (add `pub mod tables;` — keep module list alphabetical: `pub mod source_context; pub mod tables;`)
- Modify: `crates/ab-morph-run/Cargo.toml` — no change needed here (arrow/parquet already deps)

**Interfaces:**
- Consumes: `ab_warehouse::schema::WarehouseTable` (variants `Sources`, `Morphemes`, `MorphemeFeatures`, `NwayRegionAnalyzers`; `file_name()` gives e.g. `"morphemes.parquet"`). NOTE: `crate::summary::read_warehouse_table` exists but collects whole tables into memory — do NOT use it for morphemes; this task's streaming reader is the point.
- Produces (all keyed with `BTreeMap`, deterministic):
  - `for_each_table_batch(run_dir: &Path, table: WarehouseTable, f: impl FnMut(&RecordBatch) -> Result<()>) -> Result<()>` — handles both a single `<table>.parquet` file and a directory of part files (sorted), mirroring `summary::read_warehouse_table`'s dir logic but streaming.
  - `SourceInfo { text_id: String, aat_path: String, source_chars: u64 }`; `read_sources_for(run_dir, source_ids: &BTreeSet<String>) -> Result<BTreeMap<String, SourceInfo>>` (key: source_id)
  - `RegionAnalyzerRow { analyzer_id: String, covers_exactly: bool, morpheme_start: u64, morpheme_end: u64, surfaces: Vec<String> }`; `read_region_analyzers_for(run_dir, wanted: &BTreeSet<(String, u64)>) -> Result<BTreeMap<(String, u64), Vec<RegionAnalyzerRow>>>` (key: (source_id, region_index); rows sorted by analyzer_id)
  - `Token { surface: String, char_start: u64, char_end: u64, features: BTreeMap<String, String> }`; `read_tokens_for(run_dir, ranges: &BTreeMap<(String, String), Vec<(u64, u64)>>) -> Result<BTreeMap<(String, String, u64), Token>>` — key `(source_id, analyzer_id, morpheme_index)`; `ranges` maps `(source_id, analyzer_id)` to wanted `[morpheme_start, morpheme_end)` intervals; reads `Morphemes` then `MorphemeFeatures`, keeping only rows inside a wanted interval.
  - `WorkRow { work_id: String, title: String, author_person_id: Option<String>, publication_year: Option<i32>, orthographic_style: Option<String> }`; `read_works_for(run_dir, source_ids: &BTreeSet<String>) -> Result<Option<BTreeMap<String, WorkRow>>>` — `Ok(None)` when `aozora_works.parquet` is absent (the `works-sidecar-missing` case).

- [ ] **Step 1: Write the failing test**

In `tables.rs` `#[cfg(test)] mod tests`, write a fixture warehouse using the same writer pattern as `summary/interesting.rs` tests (see `write_fixture` there, `interesting.rs:1791`):

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::warehouse::schema::{
        MorphemeFeatureRow, MorphemeRow, NwayRegionAnalyzerRow, RunRow, SCHEMA_VERSION,
        SourceRow, WarehousePaths,
    };
    use crate::warehouse::writer::WarehouseWriter;
    use std::sync::Arc;

    const RUN: &str = "run-h";

    fn arc(s: &str) -> Arc<str> { Arc::from(s) }

    fn write_fixture(root: &std::path::Path) -> std::path::PathBuf {
        let paths = WarehousePaths::new(root, RUN);
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer.append_runs(&[RunRow {
            schema_version: SCHEMA_VERSION,
            run_id: RUN.to_owned(),
            created_at_utc: "2026-07-10T00:00:00Z".to_owned(),
            input_mode: "aat_dir".to_owned(),
            input_path: "scratch/aats".to_owned(),
            source_count: 1,
            analyzer_count: 2,
            error_count: 0,
            ortho_detect_mode: "off".to_owned(),
            input_normalization_detector_id: None,
            input_normalization_policy_hash: "sha256:identity".to_owned(),
        }]).unwrap();
        writer.append_sources(&[SourceRow {
            run_id: RUN.to_owned(),
            source_id: "src-a".to_owned(),
            text_id: "txt-a".to_owned(),
            aat_path: root.join("src-a.json").display().to_string(),
            source_bytes: 36,
            source_chars: 12,
        }]).unwrap();
        writer.append_nway_region_analyzers(&[
            NwayRegionAnalyzerRow {
                run_id: arc(RUN), source_id: arc("src-a"), text_id: arc("txt-a"),
                region_index: 2, analyzer_id: arc("vibrato:unidic-cwj-202512"),
                covers_exactly: true, morpheme_start: 1, morpheme_end: 2,
                surfaces: vec!["猫である".to_owned()],
            },
            NwayRegionAnalyzerRow {
                run_id: arc(RUN), source_id: arc("src-a"), text_id: arc("txt-a"),
                region_index: 2, analyzer_id: arc("sudachi-a"),
                covers_exactly: true, morpheme_start: 1, morpheme_end: 3,
                surfaces: vec!["猫".to_owned(), "である".to_owned()],
            },
        ]).unwrap();
        writer.append_morphemes(&[
            MorphemeRow { run_id: arc(RUN), source_id: arc("src-a"), text_id: arc("txt-a"),
                analyzer_id: arc("vibrato:unidic-cwj-202512"), morpheme_index: 1,
                byte_start: 0, byte_end: 0, char_start: 6, char_end: 10,
                surface: "猫である".to_owned() },
            MorphemeRow { run_id: arc(RUN), source_id: arc("src-a"), text_id: arc("txt-a"),
                analyzer_id: arc("sudachi-a"), morpheme_index: 1,
                byte_start: 0, byte_end: 0, char_start: 6, char_end: 7,
                surface: "猫".to_owned() },
            MorphemeRow { run_id: arc(RUN), source_id: arc("src-a"), text_id: arc("txt-a"),
                analyzer_id: arc("sudachi-a"), morpheme_index: 2,
                byte_start: 0, byte_end: 0, char_start: 7, char_end: 10,
                surface: "である".to_owned() },
            // A morpheme OUTSIDE every wanted range — must not be returned.
            MorphemeRow { run_id: arc(RUN), source_id: arc("src-a"), text_id: arc("txt-a"),
                analyzer_id: arc("sudachi-a"), morpheme_index: 9,
                byte_start: 0, byte_end: 0, char_start: 0, char_end: 1,
                surface: "外".to_owned() },
        ]).unwrap();
        writer.append_morpheme_features(&[
            MorphemeFeatureRow { run_id: arc(RUN), source_id: arc("src-a"), text_id: arc("txt-a"),
                analyzer_id: arc("sudachi-a"), morpheme_index: 1,
                feature_key: arc("pos1"), feature_value: Some(arc("名詞")) },
            MorphemeFeatureRow { run_id: arc(RUN), source_id: arc("src-a"), text_id: arc("txt-a"),
                analyzer_id: arc("vibrato:unidic-cwj-202512"), morpheme_index: 1,
                feature_key: arc("pos1"), feature_value: Some(arc("動詞")) },
        ]).unwrap();
        writer.finalize().unwrap();
        paths.run_dir()
    }

    #[test]
    fn readers_filter_to_wanted_keys() {
        let dir = tempfile::tempdir().unwrap();
        let run_dir = write_fixture(dir.path());

        let sources = read_sources_for(&run_dir, &["src-a".to_owned()].into()).unwrap();
        assert_eq!(sources["src-a"].text_id, "txt-a");
        assert_eq!(sources["src-a"].source_chars, 12);

        let wanted: std::collections::BTreeSet<(String, u64)> =
            [("src-a".to_owned(), 2)].into();
        let regions = read_region_analyzers_for(&run_dir, &wanted).unwrap();
        let rows = &regions[&("src-a".to_owned(), 2)];
        assert_eq!(rows.len(), 2);
        assert_eq!(rows[0].analyzer_id, "sudachi-a"); // sorted by analyzer_id

        let mut ranges = std::collections::BTreeMap::new();
        for row in rows {
            ranges.entry(("src-a".to_owned(), row.analyzer_id.clone()))
                .or_insert_with(Vec::new)
                .push((row.morpheme_start, row.morpheme_end));
        }
        let tokens = read_tokens_for(&run_dir, &ranges).unwrap();
        assert_eq!(tokens.len(), 3); // index 9 filtered out
        let t = &tokens[&("src-a".to_owned(), "sudachi-a".to_owned(), 1)];
        assert_eq!(t.surface, "猫");
        assert_eq!(t.features["pos1"], "名詞");

        // No aozora_works.parquet in the fixture ⇒ Ok(None).
        assert!(read_works_for(&run_dir, &["src-a".to_owned()].into()).unwrap().is_none());
    }
}
```

If `WarehousePaths::new(root, RUN)` or `paths.run_dir()` do not exist with these exact signatures, mirror whatever `summary/interesting.rs::write_fixture` (`interesting.rs:1791-1793`) actually calls — that test is the authoritative usage example. Same for `append_morpheme_features`: if the writer only offers `append_morpheme_feature_columns(&mut MorphemeFeaturesColumns)` (see `writer.rs:370`), build a `MorphemeFeaturesColumns`, `push_row(run_id, source_id, text_id, analyzer_id, morpheme_index, feature_key, feature_value)` per row, and pass it.

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p ab-morph-run hydrate::tables`
Expected: FAIL to compile — reader functions not found.

- [ ] **Step 3: Write the implementation**

`tables.rs` core (column lookup by name, like `read_optional_work_map` at `interesting.rs:894`):

```rust
//! Streaming, key-filtered readers over warehouse parquet. Full-corpus
//! morphemes.parquet is tens of GB; every reader here visits one
//! RecordBatch at a time and retains only rows matching the wanted keys.

use std::collections::{BTreeMap, BTreeSet};
use std::fs::{self, File};
use std::path::Path;

use anyhow::{Context, Result};
use arrow_array::{Array, BooleanArray, Int32Array, ListArray, RecordBatch, StringArray, UInt64Array};
use parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder;

use crate::warehouse::schema::WarehouseTable;

pub fn for_each_table_batch(
    run_dir: &Path,
    table: WarehouseTable,
    mut f: impl FnMut(&RecordBatch) -> Result<()>,
) -> Result<()> {
    let path = run_dir.join(table.file_name());
    if path.is_dir() {
        let mut parts: Vec<_> = fs::read_dir(&path)
            .with_context(|| format!("failed to read {}", path.display()))?
            .map(|entry| entry.map(|entry| entry.path()))
            .collect::<std::result::Result<Vec<_>, _>>()?;
        parts.retain(|part| part.extension().is_some_and(|ext| ext == "parquet"));
        parts.sort();
        for part in parts {
            for_each_file_batch(&part, &mut f)?;
        }
        return Ok(());
    }
    for_each_file_batch(&path, &mut f)
}

fn for_each_file_batch(path: &Path, f: &mut impl FnMut(&RecordBatch) -> Result<()>) -> Result<()> {
    let file = File::open(path).with_context(|| format!("failed to open {}", path.display()))?;
    let reader = ParquetRecordBatchReaderBuilder::try_new(file)
        .with_context(|| format!("failed to read parquet metadata from {}", path.display()))?
        .build()
        .with_context(|| format!("failed to build parquet reader for {}", path.display()))?;
    for batch in reader {
        let batch = batch.with_context(|| format!("failed to read {}", path.display()))?;
        f(&batch)?;
    }
    Ok(())
}

fn str_col<'a>(batch: &'a RecordBatch, name: &str) -> Result<&'a StringArray> {
    let index = batch.schema().index_of(name).with_context(|| format!("missing column {name}"))?;
    batch.column(index).as_any().downcast_ref::<StringArray>()
        .with_context(|| format!("column {name} is not a StringArray"))
}

fn u64_col<'a>(batch: &'a RecordBatch, name: &str) -> Result<&'a UInt64Array> {
    let index = batch.schema().index_of(name).with_context(|| format!("missing column {name}"))?;
    batch.column(index).as_any().downcast_ref::<UInt64Array>()
        .with_context(|| format!("column {name} is not a UInt64Array"))
}
```

Then the four readers, each a `for_each_table_batch` loop retaining wanted rows. Representative (`read_tokens_for`; the others follow the same shape):

```rust
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
pub struct Token {
    pub surface: String,
    pub char_start: u64,
    pub char_end: u64,
    pub features: BTreeMap<String, String>,
}

pub fn read_tokens_for(
    run_dir: &Path,
    ranges: &BTreeMap<(String, String), Vec<(u64, u64)>>,
) -> Result<BTreeMap<(String, String, u64), Token>> {
    let wanted = |source_id: &str, analyzer_id: &str, morpheme_index: u64| -> bool {
        ranges
            .get(&(source_id.to_owned(), analyzer_id.to_owned()))
            .is_some_and(|intervals| intervals.iter().any(|&(s, e)| morpheme_index >= s && morpheme_index < e))
    };
    let mut tokens = BTreeMap::new();
    for_each_table_batch(run_dir, WarehouseTable::Morphemes, |batch| {
        let source_ids = str_col(batch, "source_id")?;
        let analyzer_ids = str_col(batch, "analyzer_id")?;
        let indices = u64_col(batch, "morpheme_index")?;
        let char_starts = u64_col(batch, "char_start")?;
        let char_ends = u64_col(batch, "char_end")?;
        let surfaces = str_col(batch, "surface")?;
        for row in 0..batch.num_rows() {
            if !wanted(source_ids.value(row), analyzer_ids.value(row), indices.value(row)) {
                continue;
            }
            tokens.insert(
                (source_ids.value(row).to_owned(), analyzer_ids.value(row).to_owned(), indices.value(row)),
                Token {
                    surface: surfaces.value(row).to_owned(),
                    char_start: char_starts.value(row),
                    char_end: char_ends.value(row),
                    features: BTreeMap::new(),
                },
            );
        }
        Ok(())
    })?;
    for_each_table_batch(run_dir, WarehouseTable::MorphemeFeatures, |batch| {
        let source_ids = str_col(batch, "source_id")?;
        let analyzer_ids = str_col(batch, "analyzer_id")?;
        let indices = u64_col(batch, "morpheme_index")?;
        let keys = str_col(batch, "feature_key")?;
        let values = str_col(batch, "feature_value")?;
        for row in 0..batch.num_rows() {
            let key = (source_ids.value(row).to_owned(), analyzer_ids.value(row).to_owned(), indices.value(row));
            if let Some(token) = tokens.get_mut(&key) {
                if !values.is_null(row) {
                    token.features.insert(keys.value(row).to_owned(), values.value(row).to_owned());
                }
            }
        }
        Ok(())
    })?;
    Ok(tokens)
}
```

`read_region_analyzers_for` additionally reads `surfaces` as a `ListArray` of Utf8 (see `list_string_column` in `summary_body.rs` for the downcast pattern) and `covers_exactly` as `BooleanArray`. `read_works_for` returns `Ok(None)` when `run_dir.join("aozora_works.parquet")` does not exist, reads `publication_year` as nullable `Int32Array` and `orthographic_style`/`author_person_id` as nullable Utf8 — check `is_null(row)` before `.value(row)`; if the optional columns are absent from the schema entirely (older importer), treat them as all-null rather than erroring.

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test -p ab-morph-run hydrate::tables`
Expected: 1 passed.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-run/src/hydrate
git commit -m "feat(hydrate): streaming key-filtered warehouse table readers"
```

---

### Task 4: Per-source context assembly (Layers 1, 3, 4 with degradation)

**Files:**
- Modify: `crates/ab-morph-run/src/hydrate/source_context.rs`

**Interfaces:**
- Consumes: Task 1 `snippet_window`/`Snippet`, Task 2 `reconstruct_markup`/`AozoraMarkup`/`AatNodeRef`, Task 3 `SourceInfo`.
- Produces:
  - `SourceContext` — one loaded source: `{ text: String, spans: Vec<ab_plaintext::ProjectionSpan> }` plus a private `serde_json::Value` for pointer resolution.
  - `SourceContext::load(aat_path: &str, expected_chars: u64) -> Result<SourceContext, SourceLoadError>` where `SourceLoadError { code: &'static str, detail: String }` with `code` ∈ {`aat-missing`, `projection-mismatch`}.
  - `SourceContext::hydrate_region(&self, char_start: u64, char_end: u64, context_chars: usize) -> RegionLayers` where `RegionLayers { snippet: Option<Snippet>, aozora_markup: Option<AozoraMarkup>, aat_nodes: Vec<AatNodeRef>, errors: Vec<String> }` — snippet failure or markup failure each degrade into `errors`, never panic/propagate.

- [ ] **Step 1: Write the failing test**

```rust
    #[test]
    fn source_context_load_rejects_char_count_mismatch() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("src-a.json");
        std::fs::write(&path, serde_json::to_vec(&typed_aat_fixture()).unwrap()).unwrap();
        // Projected text is 12 chars; claim 99 ⇒ projection-mismatch.
        let err = SourceContext::load(path.to_str().unwrap(), 99).unwrap_err();
        assert_eq!(err.code, "projection-mismatch");
        let err = SourceContext::load(dir.path().join("absent.json").to_str().unwrap(), 12).unwrap_err();
        assert_eq!(err.code, "aat-missing");
    }

    #[test]
    fn hydrate_region_degrades_markup_but_keeps_snippet() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("src-a.json");
        std::fs::write(&path, serde_json::to_vec(&typed_aat_fixture()).unwrap()).unwrap();
        let ctx = SourceContext::load(path.to_str().unwrap(), 12).unwrap();

        let layers = ctx.hydrate_region(2, 5, 2);
        assert!(layers.errors.is_empty());
        assert_eq!(layers.snippet.as_ref().unwrap().region, "仏蘭西");
        assert_eq!(layers.aozora_markup.as_ref().unwrap().text, "｜仏蘭西《フランス》");

        // Out-of-range region: snippet errors, markup errors, both recorded.
        let layers = ctx.hydrate_region(0, 999, 2);
        assert!(layers.snippet.is_none());
        assert!(layers.aozora_markup.is_none());
        assert!(!layers.errors.is_empty());
    }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p ab-morph-run hydrate::source_context`
Expected: FAIL to compile — `SourceContext` not found.

- [ ] **Step 3: Write the implementation**

```rust
/// A load failure with its spec error-vocabulary code.
#[derive(Debug)]
pub struct SourceLoadError {
    pub code: &'static str,
    pub detail: String,
}

/// One source's AAT, loaded once: re-projected text + spans + the raw JSON
/// for pointer resolution. Offset agreement with the warehouse is enforced
/// at load (spec §Layer 1 offset-safety invariant).
pub struct SourceContext {
    pub text: String,
    pub spans: Vec<ab_plaintext::ProjectionSpan>,
    aat: serde_json::Value,
}

/// Layers 1/3/4 for one region, each independently degradable.
#[derive(Debug)]
pub struct RegionLayers {
    pub snippet: Option<Snippet>,
    pub aozora_markup: Option<AozoraMarkup>,
    pub aat_nodes: Vec<AatNodeRef>,
    pub errors: Vec<String>,
}

impl SourceContext {
    pub fn load(aat_path: &str, expected_chars: u64) -> Result<Self, SourceLoadError> {
        let bytes = std::fs::read(aat_path).map_err(|err| SourceLoadError {
            code: "aat-missing",
            detail: format!("{aat_path}: {err}"),
        })?;
        let aat: serde_json::Value = serde_json::from_slice(&bytes).map_err(|err| SourceLoadError {
            code: "aat-missing",
            detail: format!("{aat_path}: not parseable as AAT JSON: {err}"),
        })?;
        let (text, spans) = ab_plaintext::visible_text_projection_with_spans(&aat);
        let projected_chars = text.chars().count() as u64;
        if projected_chars != expected_chars {
            return Err(SourceLoadError {
                code: "projection-mismatch",
                detail: format!(
                    "{aat_path}: projected {projected_chars} chars, warehouse sources.source_chars says {expected_chars}"
                ),
            });
        }
        Ok(Self { text, spans, aat })
    }

    pub fn hydrate_region(&self, char_start: u64, char_end: u64, context_chars: usize) -> RegionLayers {
        let mut errors = Vec::new();
        let snippet = match snippet_window(&self.text, char_start, char_end, context_chars) {
            Ok(snippet) => Some(snippet),
            Err(err) => {
                errors.push(format!("snippet: {err}"));
                None
            }
        };
        let (aozora_markup, aat_nodes) =
            match reconstruct_markup(&self.aat, &self.spans, char_start, char_end) {
                Ok((markup, nodes)) => (Some(markup), nodes),
                Err(err) => {
                    errors.push(err.to_string()); // already "markup-unreconstructable: …"
                    (None, Vec::new())
                }
            };
        RegionLayers { snippet, aozora_markup, aat_nodes, errors }
    }
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test -p ab-morph-run hydrate::source_context`
Expected: all pass (9 tests).

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-run/src/hydrate/source_context.rs
git commit -m "feat(hydrate): per-source context with load gates and layer degradation"
```

---

### Task 5: Work-metadata resolution (aozora_works ⋈ ABC catalog)

**Files:**
- Create: `crates/ab-morph-run/src/hydrate/metadata.rs`
- Modify: `crates/ab-morph-run/src/hydrate/mod.rs` (`pub mod metadata;`)

**Interfaces:**
- Consumes: Task 3 `WorkRow`.
- Produces (Serialize; all fields `Option` except `contributors`/`errors`):
  - `Contributor { person_id: String, role: String, family_name: Option<String>, given_name: Option<String> }`
  - `WorkMeta { work_id, title, publication_year: Option<i32>, orthographic_style, first_published, ndc, card_url: Option<String>, contributors: Vec<Contributor> }`
  - `resolve_work_meta(work_row: Option<&WorkRow>, abc_catalog: Option<&Path>) -> (Option<WorkMeta>, Vec<String>)` — the `Vec<String>` carries `works-sidecar-missing` / `work-record-missing` / `person-record-missing` entries.
  - `WorkMeta::display_author(&self) -> String` — joined `family_name given_name` of every `role == "著者"` contributor (`、`-separated); falls back to `person:<id>` when names are unresolved; empty string when no author contributor.

ABC record shapes (verified against `/db/ab-validator/abc-corpus/aozora-catalog-0e9ea3e5` and `abc/schemas/person-record.schema.json`):
- `works/<work_id>.json`: `{ "work": { "title", "subtitle", "first_published", "ndc", "card_url", "orthographic_style", … }, "contributors": [ { "person_id", "relation_to_work" } ] }`
- `persons/<person_id>.json`: `{ "person_id", "family_name", "given_name", … }`

- [ ] **Step 1: Write the failing test**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::hydrate::tables::WorkRow;

    fn write_catalog(dir: &std::path::Path) {
        std::fs::create_dir_all(dir.join("works")).unwrap();
        std::fs::create_dir_all(dir.join("persons")).unwrap();
        std::fs::write(dir.join("works/000080.json"), serde_json::json!({
            "work": {"work_id": "000080", "title": "煙管", "first_published": "1916",
                     "ndc": "NDC 913", "card_url": "https://www.aozora.gr.jp/cards/000879/card80.html",
                     "orthographic_style": "新字新仮名"},
            "contributors": [{"person_id": "000879", "relation_to_work": "著者"}]
        }).to_string()).unwrap();
        std::fs::write(dir.join("persons/000879.json"), serde_json::json!({
            "person_id": "000879", "family_name": "芥川", "given_name": "竜之介"
        }).to_string()).unwrap();
    }

    fn work_row() -> WorkRow {
        WorkRow {
            work_id: "000080".to_owned(),
            title: "煙管".to_owned(),
            author_person_id: Some("000879".to_owned()),
            publication_year: Some(1916),
            orthographic_style: Some("新字新仮名".to_owned()),
        }
    }

    #[test]
    fn resolves_names_and_work_fields_from_catalog() {
        let dir = tempfile::tempdir().unwrap();
        write_catalog(dir.path());
        let (meta, errors) = resolve_work_meta(Some(&work_row()), Some(dir.path()));
        assert!(errors.is_empty());
        let meta = meta.unwrap();
        assert_eq!(meta.title.as_deref(), Some("煙管"));
        assert_eq!(meta.card_url.as_deref(), Some("https://www.aozora.gr.jp/cards/000879/card80.html"));
        assert_eq!(meta.contributors[0].family_name.as_deref(), Some("芥川"));
        assert_eq!(meta.display_author(), "芥川竜之介");
    }

    #[test]
    fn degrades_without_catalog_and_without_sidecar() {
        let (meta, errors) = resolve_work_meta(Some(&work_row()), None);
        let meta = meta.unwrap();
        // Warehouse-only: title/year/style survive; author is the bare id.
        assert_eq!(meta.display_author(), "person:000879");
        assert!(errors.is_empty()); // no catalog configured is not an error

        let (meta, errors) = resolve_work_meta(None, None);
        assert!(meta.is_none());
        assert_eq!(errors, vec!["works-sidecar-missing".to_owned()]);
    }

    #[test]
    fn missing_records_are_reported_but_nonfatal() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("works")).unwrap();
        std::fs::create_dir_all(dir.path().join("persons")).unwrap();
        let (meta, errors) = resolve_work_meta(Some(&work_row()), Some(dir.path()));
        assert!(meta.is_some());
        assert!(errors.iter().any(|e| e.starts_with("work-record-missing")));
        assert!(errors.iter().any(|e| e.starts_with("person-record-missing")));
    }
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p ab-morph-run hydrate::metadata`
Expected: FAIL to compile.

- [ ] **Step 3: Write the implementation**

Straightforward serde_json field extraction. Key behaviors: start `WorkMeta` from the `WorkRow` (title/year/style/work_id always present from the sidecar); overlay ABC work fields when the catalog + record exist; contributors come from the ABC work record (each resolved against `persons/`), falling back to a single synthetic `Contributor { person_id: author_person_id, role: "著者", family_name: None, given_name: None }` when the catalog or record is unavailable but `author_person_id` is set. `display_author()`: for each `role == "著者"` contributor, `family_name.unwrap_or_default() + given_name.unwrap_or_default()`, or `person:<id>` if both names are `None`; join with `、`. Errors carry detail after a colon, e.g. `work-record-missing: works/000080.json`.

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test -p ab-morph-run hydrate::metadata`
Expected: 3 passed.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-run/src/hydrate
git commit -m "feat(hydrate): work metadata resolution with ABC catalog joins"
```

---

### Task 6: Analyzer-analysis assembly, grouping, and id shortening

**Files:**
- Create: `crates/ab-morph-run/src/hydrate/analyses.rs`
- Modify: `crates/ab-morph-run/src/hydrate/mod.rs` (`pub mod analyses;`)

**Interfaces:**
- Consumes: Task 3 `RegionAnalyzerRow`, `Token`.
- Produces:
  - `AnalyzerAnalysis { analyzer_ids: Vec<String>, covers_exactly: bool, tokens: Vec<Token> }` (Serialize; `analyzer_ids` sorted)
  - `group_analyses(rows: &[RegionAnalyzerRow], tokens: &BTreeMap<(String, String, u64), Token>, source_id: &str) -> Vec<AnalyzerAnalysis>` — one entry per distinct `(covers_exactly, tokens)` value, analyzers grouped like pattern strings; groups ordered by first (sorted) analyzer id.
  - `short_analyzer_ids(full_ids: &BTreeSet<String>) -> BTreeMap<String, String>` (full → short): strip leading `vibrato:unidic-` and a trailing `-<digits>` run; if two full ids collide on the same short form, both keep their full id. (`vibrato:unidic-csj-202512` → `csj`; `sudachi-a` → `sudachi-a`.)

- [ ] **Step 1: Write the failing test**

```rust
    #[test]
    fn groups_identical_analyses_and_orders_deterministically() {
        use crate::hydrate::tables::{RegionAnalyzerRow, Token};
        use std::collections::BTreeMap;

        let row = |analyzer_id: &str, start: u64, end: u64| RegionAnalyzerRow {
            analyzer_id: analyzer_id.to_owned(),
            covers_exactly: true,
            morpheme_start: start,
            morpheme_end: end,
            surfaces: vec![],
        };
        let token = |surface: &str, cs: u64, ce: u64, pos1: &str| Token {
            surface: surface.to_owned(),
            char_start: cs,
            char_end: ce,
            features: [("pos1".to_owned(), pos1.to_owned())].into(),
        };
        // vibrato and sudachi-c agree (one token); sudachi-a splits in two.
        let rows = vec![
            row("sudachi-a", 1, 3),
            row("sudachi-c", 1, 2),
            row("vibrato:unidic-cwj-202512", 1, 2),
        ];
        let mut tokens: BTreeMap<(String, String, u64), Token> = BTreeMap::new();
        tokens.insert(("src-a".into(), "sudachi-a".into(), 1), token("猫", 6, 7, "名詞"));
        tokens.insert(("src-a".into(), "sudachi-a".into(), 2), token("である", 7, 10, "助動詞"));
        tokens.insert(("src-a".into(), "sudachi-c".into(), 1), token("猫である", 6, 10, "名詞"));
        tokens.insert(("src-a".into(), "vibrato:unidic-cwj-202512".into(), 1), token("猫である", 6, 10, "名詞"));

        let groups = group_analyses(&rows, &tokens, "src-a");
        assert_eq!(groups.len(), 2);
        assert_eq!(groups[0].analyzer_ids, vec!["sudachi-a"]);
        assert_eq!(groups[0].tokens.len(), 2);
        assert_eq!(
            groups[1].analyzer_ids,
            vec!["sudachi-c", "vibrato:unidic-cwj-202512"]
        );
        assert_eq!(groups[1].tokens[0].surface, "猫である");
    }

    #[test]
    fn short_ids_strip_vibrato_prefix_and_date_suffix() {
        let ids: std::collections::BTreeSet<String> = [
            "vibrato:unidic-csj-202512", "vibrato:unidic-kinsei-bungo-202512", "sudachi-a",
        ].map(str::to_owned).into();
        let map = short_analyzer_ids(&ids);
        assert_eq!(map["vibrato:unidic-csj-202512"], "csj");
        assert_eq!(map["vibrato:unidic-kinsei-bungo-202512"], "kinsei-bungo");
        assert_eq!(map["sudachi-a"], "sudachi-a");
    }

    #[test]
    fn short_ids_keep_full_form_on_collision() {
        let ids: std::collections::BTreeSet<String> =
            ["vibrato:unidic-csj-202512", "vibrato:unidic-csj-202601"].map(str::to_owned).into();
        let map = short_analyzer_ids(&ids);
        assert_eq!(map["vibrato:unidic-csj-202512"], "vibrato:unidic-csj-202512");
        assert_eq!(map["vibrato:unidic-csj-202601"], "vibrato:unidic-csj-202601");
    }
```

Fill in the first test's fixture concretely (complete code, not a comment) when writing it — three `RegionAnalyzerRow`s with `morpheme_start/end` pointing into a `tokens` map built with `Token { surface: "猫である", char_start: 6, char_end: 10, features: [("pos1","動詞")] }` for the agreeing pair and a two-token split for `sudachi-a`.

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p ab-morph-run hydrate::analyses` — FAIL to compile.

- [ ] **Step 3: Write the implementation**

```rust
pub fn group_analyses(
    rows: &[RegionAnalyzerRow],
    tokens: &BTreeMap<(String, String, u64), Token>,
    source_id: &str,
) -> Vec<AnalyzerAnalysis> {
    // rows arrive sorted by analyzer_id (tables.rs contract).
    let mut groups: Vec<AnalyzerAnalysis> = Vec::new();
    for row in rows {
        let row_tokens: Vec<Token> = (row.morpheme_start..row.morpheme_end)
            .filter_map(|index| tokens.get(&(source_id.to_owned(), row.analyzer_id.clone(), index)).cloned())
            .collect();
        match groups.iter_mut().find(|group| {
            group.covers_exactly == row.covers_exactly && group.tokens == row_tokens
        }) {
            Some(group) => group.analyzer_ids.push(row.analyzer_id.clone()),
            None => groups.push(AnalyzerAnalysis {
                analyzer_ids: vec![row.analyzer_id.clone()],
                covers_exactly: row.covers_exactly,
                tokens: row_tokens,
            }),
        }
    }
    groups
}

pub fn short_analyzer_ids(full_ids: &BTreeSet<String>) -> BTreeMap<String, String> {
    let candidate = |id: &str| -> String {
        let stripped = id.strip_prefix("vibrato:unidic-").unwrap_or(id);
        match stripped.rfind('-') {
            Some(idx) if stripped[idx + 1..].chars().all(|c| c.is_ascii_digit())
                && !stripped[idx + 1..].is_empty() => stripped[..idx].to_owned(),
            _ => stripped.to_owned(),
        }
    };
    let mut counts: BTreeMap<String, usize> = BTreeMap::new();
    for id in full_ids {
        *counts.entry(candidate(id)).or_default() += 1;
    }
    full_ids.iter().map(|id| {
        let short = candidate(id);
        let value = if counts[&short] > 1 { id.clone() } else { short };
        (id.clone(), value)
    }).collect()
}
```

(Note the group-ordering property in the test: since rows are pre-sorted by analyzer_id and groups are created on first sight, group order follows first-member order and `analyzer_ids` within a group stay sorted.)

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test -p ab-morph-run hydrate::analyses` — 3 passed.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-run/src/hydrate
git commit -m "feat(hydrate): analyzer analysis grouping and id shortening"
```

---

### Task 7: Bundle orchestration + examples.json

**Files:**
- Modify: `crates/ab-morph-run/src/hydrate/mod.rs`
- Modify: `crates/ab-morph-run/Cargo.toml` — add `ab-encoding.workspace = true` to `[dependencies]`
- Modify: `crates/ab-morph-run/src/lib.rs` — extend the `pub use hydrate::…` line to `pub use hydrate::{HydrateOptions, HydrateRunSummary, run_hydrate_interesting};`

**Interfaces:**
- Consumes: everything above, plus `crate::calibration::read_ranking(path) -> Result<InterestingSummary>` (crate-visible), `crate::summary::{InterestingRow, AnomalyRow, InterestingSummary}`, `ab_encoding::hex_sha256`.
- Produces:
  - `HydratedExample { source_id, text_id, region_index, char_start, char_end, snippet: Option<Snippet>, analyzer_analyses: Vec<AnalyzerAnalysis>, aozora_markup: Option<AozoraMarkup>, aat_nodes: Vec<AatNodeRef>, work: Option<WorkMeta>, errors: Vec<String> }` (Serialize)
  - `HydratedRow { #[serde(flatten)] row: InterestingRow, hydrated_examples: Vec<HydratedExample> }`
  - `HydratedAnomaly { #[serde(flatten)] row: AnomalyRow, hydrated: HydratedExample }`
  - `Provenance { run_id, interesting_path: String, interesting_sha256: String, abc_catalog: Option<String>, context_chars: usize, limit: Option<usize>, built_at_utc: String, analyzer_legend: BTreeMap<String, String> /* short → full */ }`
  - `HydratedBundle { provenance: Provenance, score_version: crate::summary::ScoreVersionBlock, rows: Vec<HydratedRow>, anomalies: Vec<HydratedAnomaly> }`
  - `pub fn run_hydrate_interesting(opts: &HydrateOptions) -> Result<HydrateRunSummary>` — writes `examples.json` (+ Task 8 adds `examples.md`); `HydrateRunSummary { examples_full: usize, examples_partial: usize, examples_failed: usize, error_counts: BTreeMap<String, usize> }` (Serialize, for the CLI status line).

Orchestration order inside `run_hydrate_interesting`:
1. Refuse existing `examples.json`/`examples.md` unless `force`; `fs::create_dir_all(output_dir)`.
2. Read + hash the artifact bytes (`fs::read` then `ab_encoding::hex_sha256`), parse via `serde_json::from_slice::<InterestingSummary>`.
3. Apply `limit` to `rows` (`.truncate`). Collect every example key: `(source_id, region_index, char_start, char_end)` from `rows[].region_examples` and `anomalies[]`.
4. `read_sources_for` (source-id set) → `read_region_analyzers_for` (region keys) → build `ranges` → `read_tokens_for` → `read_works_for`.
5. Group examples by `source_id`; `SourceContext::load` each once (errors map to per-example `errors[]` with all layers `None`); `hydrate_region` per example; `group_analyses` per region; `resolve_work_meta` per source (memoize per work_id in a `BTreeMap`).
6. Analyzer legend: `short_analyzer_ids` over every analyzer id seen in region-analyzer rows; store inverted (short → full) in provenance.
7. Serialize `HydratedBundle` with `serde_json::to_writer_pretty` + trailing newline, matching the `SummarizeWarehouseInteresting` handler's JSON convention (`main.rs:771-773`).
8. Tally the run summary: an example is `full` when `errors.is_empty()`, `failed` when snippet AND markup AND analyses are all empty/None, else `partial`.

- [ ] **Step 1: Write the failing test**

An end-to-end fixture combining Task 3's warehouse fixture, Task 2's typed AAT (written to the path `SourceRow.aat_path` points at), a Task 5 catalog dir, and a minimal ranking JSON. Copy the `fixture_summary`/`fixture_row`/`region_example` serde_json builders **verbatim** from `calibration/label_export.rs` tests (`label_export.rs:406-456`) into this module's tests — the plan intentionally duplicates them rather than exporting test helpers across modules. The complete fixture assembly:

```rust
    /// One tempdir holding: warehouse run (Task 3's write_fixture, with
    /// SourceRow.aat_path pointing at src-a.json and source_chars matching
    /// the typed AAT fixture's 12 projected chars), the typed AAT file,
    /// a Task 5 ABC catalog, an aozora_works.parquet mapping src-a→000080,
    /// and a ranking JSON whose single row exemplifies chars [2,5) region 2.
    fn write_e2e_fixture() -> (tempfile::TempDir, HydrateOptions) {
        let dir = tempfile::tempdir().unwrap();
        let run_dir = crate::hydrate::tables::tests::write_fixture(dir.path());
        // (make tables::tests::write_fixture pub(crate) within #[cfg(test)]
        // — or re-declare it here; either is acceptable, pick one and note it)
        std::fs::write(
            dir.path().join("src-a.json"),
            serde_json::to_vec(&crate::hydrate::source_context::tests::typed_aat_fixture()).unwrap(),
        )
        .unwrap();
        let catalog = dir.path().join("catalog");
        // write works/000080.json + persons/000879.json exactly as in the
        // Task 5 test's write_catalog
        write_catalog_fixture(&catalog);
        write_aozora_works_parquet(&run_dir); // work_id 000080, source_id src-a, title 煙管, author 000879
        let interesting = dir.path().join("interesting.json");
        let ranking = fixture_summary(
            "run-h",
            vec![fixture_row(
                "p-ruby",
                vec![region_example_at("src-a", "txt-a", 2, 2, 5)],
            )],
        );
        std::fs::write(&interesting, serde_json::to_vec(&ranking).unwrap()).unwrap();
        let output_dir = dir.path().join("bundle");
        let opts = HydrateOptions {
            interesting,
            run_dir,
            output_dir,
            abc_catalog: Some(catalog),
            context_chars: 2,
            limit: None,
            force: false,
            built_at_utc: "2026-07-10T00:00:00Z".to_owned(),
        };
        (dir, opts)
    }
```

`region_example_at` is `label_export.rs`'s `region_example` extended with explicit `text_id` and `region_index` arguments; `write_aozora_works_parquet` appends one `AozoraWorkRow`-shaped record through whatever writer `import_aozora.rs` uses (look up its parquet-writing function and reuse it, or write the batch directly with `arrow`/`parquet` matching the column names in `read_works_for`). Assertions:

```rust
    #[test]
    fn hydrate_end_to_end_writes_json_bundle() {
        let (_dir, opts) = write_e2e_fixture();
        let summary = run_hydrate_interesting(&opts).unwrap();
        assert_eq!(summary.examples_failed, 0);
        let bundle: serde_json::Value = serde_json::from_str(
            &std::fs::read_to_string(opts.output_dir.join("examples.json")).unwrap()).unwrap();
        assert_eq!(bundle["provenance"]["run_id"], "run-h");
        assert!(bundle["provenance"]["interesting_sha256"].as_str().unwrap().len() == 64);
        let example = &bundle["rows"][0]["hydrated_examples"][0];
        assert_eq!(example["snippet"]["region"], "仏蘭西");
        assert_eq!(example["aozora_markup"]["text"], "｜仏蘭西《フランス》");
        assert_eq!(example["work"]["title"], "煙管");
        assert!(example["errors"].as_array().unwrap().is_empty());
        // flattened InterestingRow fields present alongside hydration:
        assert!(bundle["rows"][0]["pattern_id"].is_string());
    }

    #[test]
    fn hydrate_is_deterministic_byte_identical() {
        let (_dir, opts) = write_e2e_fixture();
        let mut opts_a = opts.clone();
        opts_a.output_dir = opts.output_dir.with_file_name("bundle-a");
        let mut opts_b = opts.clone();
        opts_b.output_dir = opts.output_dir.with_file_name("bundle-b");
        run_hydrate_interesting(&opts_a).unwrap();
        run_hydrate_interesting(&opts_b).unwrap();
        for name in ["examples.json", "examples.md"] {
            assert_eq!(
                std::fs::read(opts_a.output_dir.join(name)).unwrap(),
                std::fs::read(opts_b.output_dir.join(name)).unwrap(),
                "{name} differs between identical runs"
            );
        }
    }

    #[test]
    fn hydrate_refuses_overwrite_without_force() {
        let (_dir, opts) = write_e2e_fixture();
        run_hydrate_interesting(&opts).unwrap();
        let err = run_hydrate_interesting(&opts).unwrap_err();
        assert!(err.to_string().contains("--force"));
        let mut forced = opts.clone();
        forced.force = true;
        run_hydrate_interesting(&forced).unwrap();
    }
```

(The determinism test asserts on `examples.md` too; it passes trivially before Task 8 adds the file — guard the loop with `.filter(|name| opts_a.output_dir.join(name).exists())` until Task 8, then drop the guard.)

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p ab-morph-run hydrate::` — FAIL to compile (`run_hydrate_interesting` missing).

- [ ] **Step 3: Implement** per the orchestration order above. Wire `ab-encoding` into `Cargo.toml` `[dependencies]` (alphabetical: after `ab-diff-utils`).

- [ ] **Step 4: Run tests**

Run: `cargo test -p ab-morph-run hydrate` — all pass.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-run/src/hydrate crates/ab-morph-run/src/lib.rs crates/ab-morph-run/Cargo.toml
git commit -m "feat(hydrate): bundle orchestration and examples.json emission"
```

---

### Task 8: Markdown renderer (examples.md)

**Files:**
- Create: `crates/ab-morph-run/src/hydrate/render.rs`
- Modify: `crates/ab-morph-run/src/hydrate/mod.rs` (`pub mod render;` + call `render::write_markdown` from `run_hydrate_interesting` after the JSON write)

**Interfaces:**
- Consumes: Task 7 `HydratedBundle` (all fields).
- Produces: `write_markdown(bundle: &HydratedBundle, out: &mut impl std::io::Write) -> Result<()>`.

Layout (spec §Output; escape `|` in table cells as `\|`):

```markdown
# Hydrated examples — {run_id}

- Ranking: `{interesting_path}` (sha256:{interesting_sha256})
- Built: {built_at_utc} · context: {context_chars} chars · limit: {limit|"all"}
- ABC catalog: {path or "absent — authors shown as person ids"}

Analyzer legend: **csj** = vibrato:unidic-csj-202512 · **sudachi-a** = sudachi-a · …

## #{rank} {kind} — {pattern}
rrf {rrf_score:.6} · {examples} examples · pattern `{pattern_id}`

### 『{title}』 {display_author}（{publication_year}・{orthographic_style}）— {text_id}, region {region_index}, chars {char_start}–{char_end}
[card]({card_url})

> {snippet.marked()}

| analyzers | segmentation | pos |
| --- | --- | --- |
| {short ids, comma-joined} | {token surfaces joined with ｜} | {pos1..pos4 joined with -, per token joined with ｜} |

Aozora markup{" (approximate)" if approximate_pointers nonempty}:
```
{aozora_markup.text}
```
AAT: {pointer (inline_kind), comma-joined}

## Anomalies
### {n}. 『{title}』 …（anomaly {anomaly_score:.2}）— same example layers
```

Degradation rendering: a missing layer prints one line — `_snippet unavailable: {error}_` etc.; a missing work heading falls back to `### {text_id}, region {region_index}, chars {a}–{b}`.

- [ ] **Step 1: Write the failing test**

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn markdown_renders_all_layers() {
        let (_dir, opts) = crate::hydrate::tests::write_e2e_fixture();
        crate::hydrate::run_hydrate_interesting(&opts).unwrap();
        // Re-render from the bundle the run just produced: parse it back and
        // call write_markdown directly so this test pins the renderer, not
        // the orchestrator. (Make write_e2e_fixture pub(crate) in
        // hydrate::tests, mirroring the Task 7 note about shared fixtures.)
        let md = std::fs::read_to_string(opts.output_dir.join("examples.md")).unwrap();
        assert!(md.starts_with("# Hydrated examples — run-h"));
        assert!(md.contains("Analyzer legend:"));
        assert!(md.contains("## #1 feature — pattern-p-ruby"));
        assert!(md.contains("『煙管』 芥川竜之介"));
        assert!(md.contains("【仏蘭西】"));
        assert!(md.contains("| analyzers | segmentation | pos |"));
        assert!(md.contains("｜仏蘭西《フランス》"));
        assert!(md.contains("AAT: /blocks/0/content/1"));
        assert!(!md.contains('\t'), "markdown must not contain raw tabs");
    }
}
```
- [ ] **Step 2: Run** `cargo test -p ab-morph-run hydrate::render` — FAIL.
- [ ] **Step 3: Implement** — plain `writeln!` sequence; keep every ordering derived from bundle order (already deterministic).
- [ ] **Step 4: Run** `cargo test -p ab-morph-run hydrate` — all pass, including the Task 7 determinism test now covering both files.
- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-run/src/hydrate
git commit -m "feat(hydrate): markdown renderer for example bundles"
```

---

### Task 9: CLI subcommand

**Files:**
- Modify: `crates/ab-morph-run/src/main.rs` — add a `HydrateInteresting` variant to the `Command` enum (place it right after `SummarizeWarehouseInteresting`, ~line 291) and a match arm (after the `SummarizeWarehouseInteresting` arm, ~line 780)

**Interfaces:**
- Consumes: `ab_morph_run::{HydrateOptions, run_hydrate_interesting}`.

- [ ] **Step 1: Add the variant**

```rust
    /// Hydrates a summarize-warehouse-interesting JSON artifact into a
    /// self-contained example bundle (examples.md + examples.json) with
    /// text snippets, per-analyzer tables, reconstructed aozora markup,
    /// and work metadata. Spec:
    /// docs/superpowers/specs/2026-07-10-hydrate-interesting-examples-design.md
    HydrateInteresting {
        #[arg(long)]
        interesting: PathBuf,
        #[arg(long)]
        run_dir: PathBuf,
        #[arg(long)]
        output_dir: PathBuf,
        #[arg(long)]
        abc_catalog: Option<PathBuf>,
        #[arg(long, default_value_t = 40)]
        context_chars: usize,
        #[arg(long)]
        limit: Option<usize>,
        #[arg(long)]
        force: bool,
    },
```

- [ ] **Step 2: Add the match arm**

```rust
        Command::HydrateInteresting {
            interesting,
            run_dir,
            output_dir,
            abc_catalog,
            context_chars,
            limit,
            force,
        } => {
            let summary = ab_morph_run::run_hydrate_interesting(&ab_morph_run::HydrateOptions {
                interesting,
                run_dir,
                output_dir: output_dir.clone(),
                abc_catalog,
                context_chars,
                limit,
                force,
                built_at_utc: chrono::Utc::now().format("%Y-%m-%dT%H:%M:%SZ").to_string(),
            })?;
            eprintln!(
                "hydrated {} examples fully, {} partially, {} failed → {}",
                summary.examples_full,
                summary.examples_partial,
                summary.examples_failed,
                output_dir.display()
            );
            for (code, count) in &summary.error_counts {
                eprintln!("  {code}: {count}");
            }
            Ok(())
        }
```

- [ ] **Step 3: Verify it builds and self-documents**

Run: `cargo run -p ab-morph-run -- hydrate-interesting --help`
Expected: the help text above, exit 0. Then run the full suite: `cargo test -p ab-morph-run` — all green.

- [ ] **Step 4: Lint**

Run: `cargo clippy -p ab-morph-run --all-targets -- -D warnings && cargo fmt --check`
Expected: clean. Fix anything reported.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-run/src/main.rs
git commit -m "feat(hydrate): hydrate-interesting CLI subcommand"
```

---

### Task 10: Justfile recipe, README docs, smoke run

**Files:**
- Modify: `ab-validator/justfile` — add recipe after `morph-warehouse-run-lane-b`
- Modify: `ab-validator/reports/morph-warehouse/README.md` — new section

- [ ] **Step 1: Add the recipe** (matches the file's nix-built-engine convention, see `morph-warehouse-run-lane-b`):

```just
# Hydrate an interestingness ranking artifact into a self-contained example
# bundle (examples.md + examples.json): snippets, per-analyzer tables,
# reconstructed aozora markup, work metadata. Run on the machine holding the
# warehouse run dir + AAT corpus (hinoki for full-corpus runs).
morph-warehouse-hydrate-interesting interesting run_dir output_dir abc_catalog="" context_chars="40" limit="" force="":
	@engine_bin="$(nix build .#ab-morph-run --no-link --print-out-paths)/bin/ab-morph-run"; \
	args=(); \
	if [ -n "{{abc_catalog}}" ]; then args+=(--abc-catalog "{{abc_catalog}}"); fi; \
	if [ -n "{{limit}}" ]; then args+=(--limit "{{limit}}"); fi; \
	if [ -n "{{force}}" ]; then args+=(--force); fi; \
	"$engine_bin" hydrate-interesting \
		--interesting "{{interesting}}" \
		--run-dir "{{run_dir}}" \
		--output-dir "{{output_dir}}" \
		--context-chars "{{context_chars}}" \
		"${args[@]}"
```

- [ ] **Step 2: README section** (after the "Interactive marimo notebook" section):

```markdown
## Hydrated example bundles

`ab-morph-run hydrate-interesting` turns an interestingness ranking JSON into
a self-contained `examples.md` + `examples.json` bundle: projected-text
snippets with the disagreement region marked 【…】, per-analyzer
segmentation/POS tables, aozora markup reconstructed from the AAT, and work
metadata (title, author, year — author names resolved from an ABC catalog
export's `persons/`). Run it where the warehouse run dir and AAT corpus live
(hinoki for full-corpus runs — use the Tailscale FQDN
`hinoki.hyakutake-barbel.ts.net`; the bare `hinoki` ssh alias resolves to a
different host), then copy the bundle next to the existing reports:

```bash
just morph-warehouse-hydrate-interesting \
  /db/ab-validator/morph-warehouse/reports/dict-cmp-m2-full-2026-07-09/interesting-dictcmp-m2-full.json \
  /db/ab-validator/morph-warehouse/runs/dict-cmp-m2-full-2026-07-09 \
  /db/ab-validator/morph-warehouse/reports/dict-cmp-m2-full-2026-07-09/examples \
  /db/ab-validator/abc-corpus/aozora-catalog-0e9ea3e5
```

Every layer degrades independently (missing catalog → author ids; changed
AAT → `projection-mismatch` instead of a wrong quote); the bundle records
per-example `errors[]` and the CLI prints a full/partial/failed tally.
```

- [ ] **Step 3: Smoke run against a real triage warehouse** (farspark has `calib-triage-1000` locally). First produce a small ranking artifact, then hydrate it:

```bash
engine="$(nix build .#ab-morph-run --no-link --print-out-paths)/bin/ab-morph-run"
"$engine" summarize-warehouse-interesting \
  --run-dir /db/ab-validator/morph-warehouse/runs/calib-triage-1000 \
  --format json --limit 5 --anomalies 3 \
  --output /tmp-scratch/interesting-smoke.json --force   # use the session scratchpad dir
just morph-warehouse-hydrate-interesting \
  /tmp-scratch/interesting-smoke.json \
  /db/ab-validator/morph-warehouse/runs/calib-triage-1000 \
  /tmp-scratch/examples-smoke
```

Expected: non-empty `examples.md` whose snippets contain 【】-marked regions and whose markup lines show 《》 ruby for at least one example; the tally line reports 0 failed (partial is acceptable: the triage run predates `aozora_works.parquet` enrichment on some snapshots — if so, `works-sidecar-missing` appears and headings fall back to text ids, which is correct degradation, not failure). Read the first two examples by eye and sanity-check the snippet text against the pattern.

- [ ] **Step 4: Full verification**

Run: `cargo test -p ab-morph-run && cargo clippy -p ab-morph-run --all-targets -- -D warnings && cargo fmt --check`
Expected: all green.

- [ ] **Step 5: Commit**

```bash
git add justfile reports/morph-warehouse/README.md
git commit -m "feat(hydrate): justfile recipe and report docs for example bundles"
```

---

## Self-review checklist (run after writing, fix inline)

1. **Spec coverage:** Layers 1–5 → Tasks 1/2 (snippet, markup, AAT nodes), 6 (analyzer table), 5 (metadata); outputs → Tasks 7/8; error vocabulary → Tasks 3/4/5/7; CLI → Task 9; integration/README → Task 10; determinism → Task 7 test + `built_at_utc` injection. Non-goals respected (no TEI, no re-rank, no explorer).
2. **Type consistency:** `Snippet`/`AozoraMarkup`/`AatNodeRef` (Tasks 1–2) are consumed by `RegionLayers` (Task 4) and `HydratedExample` (Task 7); `Token`/`RegionAnalyzerRow`/`WorkRow` (Task 3) feed Tasks 5–7 under those exact names.
3. **Fixture honesty:** Task 2's expected pointer values defer to the actual `ab-plaintext` walker output (Step 4 note); Task 3 defers writer-API details to the authoritative `interesting.rs` fixture when signatures drift.
