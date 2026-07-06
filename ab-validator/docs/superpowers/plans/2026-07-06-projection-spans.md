# Phase 3 `projection_spans` + Warehouse Memory Reduction Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** The Full-profile analysis pass writes `projection_spans.parquet` (projected-char-offset → AAT-inline-node bridge, unlocking the Phase 4 ruby oracle), the warehouse `SCHEMA_VERSION` bumps 1→2 with the generalized reader rule, and three behavior-preserving memory fixes (P1–P3) shrink the large-document transient stack that caused the earlyoom restarts.

**Architecture:** One shared tree walker in `ab-plaintext` grows a span-emitting sink alongside the existing text-only sink (byte-identical projection guaranteed by construction and pinned by tests). `ab-warehouse` gains a 12th table wired through the existing writer/staging/merge machinery. The pipeline emits span rows right after `sources`, drops the AAT DOM after projection (P1), stops cloning the document text on the ortho-off path (P2), and `ab-morph-diff` replaces BTreeSet boundary sets with sorted vectors (P3). Spec: `docs/superpowers/specs/2026-07-06-projection-spans-design.md`.

**Tech Stack:** Rust workspace (serde_json, arrow/parquet, proptest); DuckDB view templates; justfile recipes.

## Global Constraints

- Working directory for all commands: `/home/bor/Projects/soranoha/ab-validator` (the monorepo component root — `cargo` commands run there).
- Test command is ALWAYS `cargo test -p ab-morph-run --features test-analyzer` for `ab-morph-run` (bare invocation false-fails 2 bin tests); plain `cargo test -p <crate>` for other crates.
- `cargo clippy -p <crate> --all-targets` (plus `--features test-analyzer` for ab-morph-run) must stay clean after every task.
- **Projection identity is load-bearing:** `visible_text_projection_with_spans(aat).0` must be byte-identical to `visible_text_projection(aat)` for every input. Any text drift invalidates warehouse row-count parity.
- Spec-exact tokens: table file `projection_spans.parquet`; columns `run_id, source_id, text_id, projected_char_start, projected_char_end, aat_pointer, inline_kind, is_ruby_base, is_gaiji, is_note`; `SCHEMA_VERSION = 2`; `READER_MAX_SCHEMA_VERSION = 2`; policy line "Readers must reject runs.schema_version values greater than the reader's supported maximum (2)."
- `is_note` is structurally false in v1 emission (notes are excluded from projection); the column ships anyway per the governing spec's column table.
- Full profile only: the triage table lists in `crates/ab-morph-run/src/options.rs` must NOT gain the new table.
- Task 7 runs real dictionaries via the monorepo's own flake outputs: `ab-validator/dictionary` (a dangling split-repo symlink at plan time) is replaced by a real local directory with `compiled/` + `optimized/` subdirs; `just dictionary-build-all` populates it from `nix build .#vibrato-dict-* --print-out-paths` symlinks. Owner-decided 2026-07-06; record as an intentional monorepo delta in `docs/migration-status.md`.
- Owner-decided 2026-07-06: implementation lands in the soranoha monorepo on branch `feat/projection-spans`; the `parity-audit` gate is deferred (drift vs the split repos is pre-existing: monorepo commit 62e49db plus abc-side changes) — do NOT run `just validate-migration` as a task gate; run `just root-flake-check-no-build` instead.
- Commits end with the trailer: `🤖 Generated with [Claude Code](https://claude.com/claude-code)`

---

### Task 1: Span-emitting projection in `ab-plaintext`

**Files:**
- Modify: `crates/ab-plaintext/src/aat.rs` (replace `collect_block`/`collect_inline`/`push_string_field` with a sink-generic walker; add `ProjectionSpan`, `visible_text_projection_with_spans`, `from_aat_value_with_spans`)
- Modify: `crates/ab-plaintext/src/lib.rs:9` (re-export the new items)
- Modify: `crates/ab-plaintext/Cargo.toml` (add `[dev-dependencies] proptest.workspace = true`)
- Test: new tests inside `crates/ab-plaintext/src/aat.rs` `mod tests`

**Interfaces:**
- Consumes: nothing from other tasks.
- Produces (used by Tasks 4):
  ```rust
  pub struct ProjectionSpan {
      pub projected_char_start: u64,
      pub projected_char_end: u64,
      pub aat_pointer: String,   // RFC 6901, e.g. "/blocks/0/content/3"
      pub inline_kind: String,   // "text" | "ruby" | "gaiji" | "accent" | "raw"
      pub is_ruby_base: bool,
      pub is_gaiji: bool,
      pub is_note: bool,
  }
  pub fn visible_text_projection_with_spans(aat: &Value) -> (String, Vec<ProjectionSpan>);
  pub fn from_aat_value_with_spans(aat: &Value)
      -> Result<(PlainTextDocument, Vec<ProjectionSpan>), PlainTextError>;
  ```
  `visible_text_projection` and `from_aat_value` keep their exact signatures and behavior.

- [ ] **Step 1: Write the failing tests**

Append to the existing `mod tests` in `crates/ab-plaintext/src/aat.rs` (the existing tests stay untouched and must keep passing — they pin the identity of the text-only path):

```rust
    fn nested_fixture() -> serde_json::Value {
        json!({
            "work_id": "w2",
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "A"},
                    {"kind": "ruby", "base": "B", "reading": "ビー"},
                    {"kind": "gaiji", "description": "desc", "resolved": "C"},
                    {"kind": "gaiji", "description": "empty", "resolved": ""},
                    {"kind": "gaiji", "description": "D"},
                    {"kind": "accent", "code": "1-09-63", "name": "e acute", "resolved": "é"},
                    {"kind": "accent", "code": "missing", "name": "fallback", "resolved": ""},
                    {"kind": "raw", "source": "E"},
                    {
                        "kind": "warigaki",
                        "upper": [{"kind": "text", "value": "F"}],
                        "lower": [{"kind": "text", "value": "G"}]
                    },
                    {"kind": "style", "content": [{"kind": "text", "value": "H"}]}
                ]
            }]
        })
    }

    fn span_tuple(span: &ProjectionSpan) -> (u64, u64, &str, &str, bool, bool, bool) {
        (
            span.projected_char_start,
            span.projected_char_end,
            span.aat_pointer.as_str(),
            span.inline_kind.as_str(),
            span.is_ruby_base,
            span.is_gaiji,
            span.is_note,
        )
    }

    #[test]
    fn spans_projection_text_is_identical_to_plain_projection() {
        for aat in [
            nested_fixture(),
            json!({"work_id": "w", "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "figure", "caption": [{"kind": "text", "value": "猫の図"}]},
                    {"kind": "style", "style_type": "notes",
                     "content": [{"kind": "text", "value": "［＃改丁］"}]},
                    {"kind": "text", "value": "A\r\nB\rC\nD"},
                    {"kind": "gaiji", "description": "x", "resolved": null,
                     "unresolved_reason": "image_fallback"}
                ]
            }]}),
            json!({"work_id": "w", "blocks": []}),
        ] {
            let (text, _) = visible_text_projection_with_spans(&aat);
            assert_eq!(text, visible_text_projection(&aat));
        }
    }

    #[test]
    fn spans_cover_projected_text_exactly_with_golden_pointers() {
        let aat = nested_fixture();
        let (text, spans) = visible_text_projection_with_spans(&aat);
        assert_eq!(text, "ABCDéfallbackEFGH");

        let expected = vec![
            (0, 1, "/blocks/0/content/0", "text", false, false, false),
            (1, 2, "/blocks/0/content/1", "ruby", true, false, false),
            (2, 3, "/blocks/0/content/2", "gaiji", false, true, false),
            (3, 4, "/blocks/0/content/4", "gaiji", false, true, false),
            (4, 5, "/blocks/0/content/5", "accent", false, false, false),
            (5, 13, "/blocks/0/content/6", "accent", false, false, false),
            (13, 14, "/blocks/0/content/7", "raw", false, false, false),
            (14, 15, "/blocks/0/content/8/upper/0", "text", false, false, false),
            (15, 16, "/blocks/0/content/8/lower/0", "text", false, false, false),
            (16, 17, "/blocks/0/content/9/content/0", "text", false, false, false),
        ];
        assert_eq!(spans.iter().map(span_tuple).collect::<Vec<_>>(), expected);

        // Coverage invariant: concatenated span slices reconstruct the text.
        let chars: Vec<char> = text.chars().collect();
        let rebuilt: String = spans
            .iter()
            .flat_map(|span| {
                chars[span.projected_char_start as usize..span.projected_char_end as usize]
                    .iter()
                    .copied()
            })
            .collect();
        assert_eq!(rebuilt, text);
    }

    #[test]
    fn figure_caption_and_block_children_get_pointer_spans() {
        let aat = json!({
            "work_id": "w",
            "blocks": [{
                "kind": "paragraph",
                "content": [{"kind": "figure", "caption": [{"kind": "text", "value": "猫"}]}],
                "children": [{
                    "kind": "paragraph",
                    "content": [{"kind": "text", "value": "子"}]
                }]
            }]
        });
        let (text, spans) = visible_text_projection_with_spans(&aat);
        assert_eq!(text, "猫子");
        assert_eq!(
            spans.iter().map(span_tuple).collect::<Vec<_>>(),
            vec![
                (0, 1, "/blocks/0/content/0/caption/0", "text", false, false, false),
                (1, 2, "/blocks/0/children/0/content/0", "text", false, false, false),
            ]
        );
    }

    #[test]
    fn crlf_canonicalization_remaps_span_offsets() {
        // Single node containing CRLFs: "A\r\nB\rC\nD" → "A\nB\nC\nD" (7 chars).
        let aat = json!({
            "work_id": "w",
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "A\r\nB\rC\nD"},
                    {"kind": "text", "value": "E"}
                ]
            }]
        });
        let (text, spans) = visible_text_projection_with_spans(&aat);
        assert_eq!(text, "A\nB\nC\nDE");
        assert_eq!(
            spans.iter().map(span_tuple).collect::<Vec<_>>(),
            vec![
                (0, 7, "/blocks/0/content/0", "text", false, false, false),
                (7, 8, "/blocks/0/content/1", "text", false, false, false),
            ]
        );
    }

    #[test]
    fn crlf_split_across_nodes_attributes_newline_to_the_cr_node() {
        // Node 1 ends with '\r', node 2 begins with '\n': whole-string
        // canonicalization produces ONE '\n', owned by the '\r' node; the
        // '\n' node loses a char (and is dropped if it becomes empty).
        let aat = json!({
            "work_id": "w",
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "X\r"},
                    {"kind": "text", "value": "\nY"},
                    {"kind": "text", "value": "\r"},
                    {"kind": "text", "value": "\n"}
                ]
            }]
        });
        let (text, spans) = visible_text_projection_with_spans(&aat);
        assert_eq!(text, visible_text_projection(&aat));
        assert_eq!(text, "X\nY\n");
        assert_eq!(
            spans.iter().map(span_tuple).collect::<Vec<_>>(),
            vec![
                (0, 2, "/blocks/0/content/0", "text", false, false, false),
                (2, 3, "/blocks/0/content/1", "text", false, false, false),
                (3, 4, "/blocks/0/content/2", "text", false, false, false),
                // content/3's lone '\n' was consumed by content/2's '\r' → span dropped.
            ]
        );
    }

    #[test]
    fn from_aat_value_with_spans_returns_document_and_spans() {
        let aat = json!({
            "work_id": "w1",
            "blocks": [{"kind": "paragraph", "content": [{"kind": "text", "value": "本文"}]}]
        });
        let (doc, spans) = from_aat_value_with_spans(&aat).unwrap();
        assert_eq!(doc.text_id, "w1");
        assert_eq!(doc.text, "本文");
        assert_eq!(spans.len(), 1);
        assert_eq!(spans[0].projected_char_end, 2);

        assert_eq!(
            from_aat_value_with_spans(&json!({"blocks": []})),
            Err(PlainTextError::MissingAatWorkId)
        );
    }

    mod span_properties {
        use proptest::prelude::*;

        use super::*;

        // Small AAT-shaped generator: paragraphs of text/ruby/gaiji nodes whose
        // strings include '\r' and '\n' so the remap path is exercised.
        fn arb_inline() -> impl Strategy<Value = serde_json::Value> {
            let arb_text = proptest::string::string_regex("[ab\r\nあ]{0,6}").unwrap();
            prop_oneof![
                arb_text.clone().prop_map(|v| json!({"kind": "text", "value": v})),
                arb_text
                    .clone()
                    .prop_map(|v| json!({"kind": "ruby", "base": v, "reading": "よみ"})),
                arb_text.prop_map(|v| json!({"kind": "gaiji", "description": v})),
            ]
        }

        fn arb_aat() -> impl Strategy<Value = serde_json::Value> {
            proptest::collection::vec(proptest::collection::vec(arb_inline(), 0..5), 0..4)
                .prop_map(|blocks| {
                    json!({
                        "work_id": "w",
                        "blocks": blocks
                            .into_iter()
                            .map(|content| json!({"kind": "paragraph", "content": content}))
                            .collect::<Vec<_>>()
                    })
                })
        }

        proptest! {
            #[test]
            fn spans_are_ordered_disjoint_and_reconstruct_the_text(aat in arb_aat()) {
                let (text, spans) = visible_text_projection_with_spans(&aat);
                prop_assert_eq!(&text, &visible_text_projection(&aat));
                let char_len = text.chars().count() as u64;
                let mut cursor = 0u64;
                for span in &spans {
                    prop_assert!(span.projected_char_start >= cursor);
                    prop_assert!(span.projected_char_start < span.projected_char_end);
                    prop_assert!(span.projected_char_end <= char_len);
                    cursor = span.projected_char_end;
                }
                let chars: Vec<char> = text.chars().collect();
                let rebuilt: String = spans
                    .iter()
                    .flat_map(|span| {
                        chars[span.projected_char_start as usize
                            ..span.projected_char_end as usize]
                            .iter()
                            .copied()
                    })
                    .collect();
                prop_assert_eq!(rebuilt, text);
            }
        }
    }
```

Note the coverage property uses `>=` (not `==`) for `projected_char_start` vs `cursor`, but the reconstruct assertion still requires full equality — both hold because a removed span's chars were exactly the canonicalization-dropped chars, which are absent from the output text too.

Also add `proptest` to `crates/ab-plaintext/Cargo.toml`:

```toml
[dev-dependencies]
proptest.workspace = true
```

(If the file already has a `[dev-dependencies]` section, add the single line to it.)

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p ab-plaintext 2>&1 | tail -5`
Expected: COMPILE FAIL — `ProjectionSpan`, `visible_text_projection_with_spans`, `from_aat_value_with_spans` not found.

- [ ] **Step 3: Implement the sink-generic walker**

Replace the body of `crates/ab-plaintext/src/aat.rs` above `mod tests` with (keep the existing `from_aat_value` doc comments):

```rust
use serde_json::Value;

use crate::{PlainTextDocument, PlainTextError, SourceFormat, canonicalize_line_endings};

/// Creates a plain-text document from an AAT JSON value.
///
/// # Errors
///
/// Returns an error when the required `work_id` field is missing or not a string.
#[must_use = "construct a PlainTextDocument from an AAT JSON value"]
pub fn from_aat_value(aat: &Value) -> Result<PlainTextDocument, PlainTextError> {
    Ok(PlainTextDocument {
        text_id: aat_text_id(aat)?,
        source_format: SourceFormat::AatVisibleText,
        text: visible_text_projection(aat),
    })
}

/// Like [`from_aat_value`], additionally returning the projected-char-offset →
/// AAT-inline-node span mapping (see [`ProjectionSpan`]).
///
/// # Errors
///
/// Returns an error when the required `work_id` field is missing or not a string.
pub fn from_aat_value_with_spans(
    aat: &Value,
) -> Result<(PlainTextDocument, Vec<ProjectionSpan>), PlainTextError> {
    let text_id = aat_text_id(aat)?;
    let (text, spans) = visible_text_projection_with_spans(aat);
    Ok((
        PlainTextDocument {
            text_id,
            source_format: SourceFormat::AatVisibleText,
            text,
        },
        spans,
    ))
}

fn aat_text_id(aat: &Value) -> Result<String, PlainTextError> {
    aat.get("work_id")
        .and_then(Value::as_str)
        .map(str::to_owned)
        .ok_or(PlainTextError::MissingAatWorkId)
}

pub fn visible_text_projection(aat: &Value) -> String {
    let mut sink = TextOnlySink(String::new());
    walk_blocks(aat, &mut Vec::new(), &mut sink);
    canonicalize_line_endings(sink.0)
}

/// Projects the visible text AND records, per contributing AAT inline node,
/// the projected char span it produced. The returned text is byte-identical
/// to [`visible_text_projection`]; spans are in document order, disjoint, and
/// jointly reconstruct the text. Only nodes contributing ≥1 char emit a span
/// (`is_note` is therefore structurally false today: note nodes are excluded
/// from projection).
pub fn visible_text_projection_with_spans(aat: &Value) -> (String, Vec<ProjectionSpan>) {
    let mut sink = SpanSink::default();
    walk_blocks(aat, &mut Vec::new(), &mut sink);
    finalize_spans(sink.text, sink.spans)
}

/// One AAT inline node's contribution to the projected plaintext.
/// Offsets are char offsets into the canonicalized projected text; end exclusive.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectionSpan {
    pub projected_char_start: u64,
    pub projected_char_end: u64,
    /// RFC 6901 JSON pointer to the contributing node, e.g. "/blocks/0/content/3".
    pub aat_pointer: String,
    pub inline_kind: String,
    pub is_ruby_base: bool,
    pub is_gaiji: bool,
    pub is_note: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SpanKind {
    Text,
    RubyBase,
    Gaiji,
    Accent,
    Raw,
}

impl SpanKind {
    fn as_str(self) -> &'static str {
        match self {
            Self::Text => "text",
            Self::RubyBase => "ruby",
            Self::Gaiji => "gaiji",
            Self::Accent => "accent",
            Self::Raw => "raw",
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum PathSeg {
    Key(&'static str),
    Index(usize),
}

/// Receives each contributing node's text. The text-only sink ignores the
/// span context entirely (and never pays for pointer construction), so the
/// plain projection keeps its current cost while sharing the single walker.
trait ProjectionSink {
    fn push(&mut self, value: &str, kind: SpanKind, path: &[PathSeg]);
}

struct TextOnlySink(String);

impl ProjectionSink for TextOnlySink {
    fn push(&mut self, value: &str, _kind: SpanKind, _path: &[PathSeg]) {
        self.0.push_str(value);
    }
}

#[derive(Default)]
struct SpanSink {
    text: String,
    char_len: usize,
    spans: Vec<ProjectionSpan>,
}

impl ProjectionSink for SpanSink {
    fn push(&mut self, value: &str, kind: SpanKind, path: &[PathSeg]) {
        if value.is_empty() {
            return;
        }
        let start = self.char_len;
        self.text.push_str(value);
        self.char_len += value.chars().count();
        self.spans.push(ProjectionSpan {
            projected_char_start: start as u64,
            projected_char_end: self.char_len as u64,
            aat_pointer: json_pointer(path),
            inline_kind: kind.as_str().to_owned(),
            is_ruby_base: kind == SpanKind::RubyBase,
            is_gaiji: kind == SpanKind::Gaiji,
            is_note: false,
        });
    }
}

fn json_pointer(path: &[PathSeg]) -> String {
    use std::fmt::Write as _;
    let mut out = String::new();
    for seg in path {
        match seg {
            PathSeg::Key(key) => {
                let _ = write!(out, "/{key}");
            }
            PathSeg::Index(index) => {
                let _ = write!(out, "/{index}");
            }
        }
    }
    out
}

fn walk_blocks(aat: &Value, path: &mut Vec<PathSeg>, sink: &mut impl ProjectionSink) {
    if let Some(blocks) = aat.get("blocks").and_then(Value::as_array) {
        path.push(PathSeg::Key("blocks"));
        for (index, block) in blocks.iter().enumerate() {
            path.push(PathSeg::Index(index));
            walk_block(block, path, sink);
            path.pop();
        }
        path.pop();
    }
}

fn walk_block(node: &Value, path: &mut Vec<PathSeg>, sink: &mut impl ProjectionSink) {
    walk_inline_children(node, "content", path, sink);
    if let Some(children) = node.get("children").and_then(Value::as_array) {
        path.push(PathSeg::Key("children"));
        for (index, child) in children.iter().enumerate() {
            path.push(PathSeg::Index(index));
            walk_block(child, path, sink);
            path.pop();
        }
        path.pop();
    }
}

fn walk_inline_children(
    node: &Value,
    key: &'static str,
    path: &mut Vec<PathSeg>,
    sink: &mut impl ProjectionSink,
) {
    if let Some(items) = node.get(key).and_then(Value::as_array) {
        path.push(PathSeg::Key(key));
        for (index, inline) in items.iter().enumerate() {
            path.push(PathSeg::Index(index));
            walk_inline(inline, path, sink);
            path.pop();
        }
        path.pop();
    }
}

fn walk_inline(node: &Value, path: &mut Vec<PathSeg>, sink: &mut impl ProjectionSink) {
    match node.get("kind").and_then(Value::as_str).unwrap_or("") {
        "text" => push_field(node, "value", SpanKind::Text, path, sink),
        "ruby" => push_field(node, "base", SpanKind::RubyBase, path, sink),
        "gaiji" => {
            if let Some(resolved) = node.get("resolved").and_then(Value::as_str) {
                sink.push(resolved, SpanKind::Gaiji, path);
            } else if node
                .get("unresolved_reason")
                .is_some_and(|value| !value.is_null())
            {
            } else {
                push_field(node, "description", SpanKind::Gaiji, path, sink);
            }
        }
        "accent" => {
            if let Some(resolved) = node
                .get("resolved")
                .and_then(Value::as_str)
                .filter(|value| !value.is_empty())
            {
                sink.push(resolved, SpanKind::Accent, path);
            } else {
                push_field(node, "name", SpanKind::Accent, path, sink);
            }
        }
        "raw" => push_field(node, "source", SpanKind::Raw, path, sink),
        "warigaki" => {
            for key in ["upper", "lower"] {
                walk_inline_children(node, key, path, sink);
            }
        }
        "figure" => walk_inline_children(node, "caption", path, sink),
        "style" if node.get("style_type").and_then(Value::as_str) == Some("notes") => {}
        _ => walk_inline_children(node, "content", path, sink),
    }
}

fn push_field(
    node: &Value,
    key: &'static str,
    kind: SpanKind,
    path: &[PathSeg],
    sink: &mut impl ProjectionSink,
) {
    if let Some(value) = node.get(key).and_then(Value::as_str) {
        sink.push(value, kind, path);
    }
}

/// Canonicalizes line endings and remaps span offsets. Canonicalization drops
/// exactly the '\n' chars directly preceded by '\r' (a lone '\r' becomes '\n'
/// 1:1), so each boundary shifts left by the number of dropped chars before
/// it. Span boundaries are non-decreasing, so one merge pass suffices. Spans
/// that become empty (their only char was a dropped '\n') are removed.
fn finalize_spans(text: String, mut spans: Vec<ProjectionSpan>) -> (String, Vec<ProjectionSpan>) {
    if !text.as_bytes().contains(&b'\r') {
        return (text, spans);
    }
    let mut dropped = Vec::new();
    let mut previous = None;
    for (char_index, ch) in text.chars().enumerate() {
        if ch == '\n' && previous == Some('\r') {
            dropped.push(char_index as u64);
        }
        previous = Some(ch);
    }
    let mut drop_cursor = 0usize;
    for span in &mut spans {
        while drop_cursor < dropped.len() && dropped[drop_cursor] < span.projected_char_start {
            drop_cursor += 1;
        }
        span.projected_char_start -= drop_cursor as u64;
        let mut end_drops = drop_cursor;
        while end_drops < dropped.len() && dropped[end_drops] < span.projected_char_end {
            end_drops += 1;
        }
        span.projected_char_end -= end_drops as u64;
        drop_cursor = end_drops;
    }
    spans.retain(|span| span.projected_char_start < span.projected_char_end);
    (canonicalize_line_endings(text), spans)
}
```

In `crates/ab-plaintext/src/lib.rs:9` change the re-export to:

```rust
pub use aat::{
    ProjectionSpan, from_aat_value, from_aat_value_with_spans, visible_text_projection,
    visible_text_projection_with_spans,
};
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p ab-plaintext 2>&1 | tail -5`
Expected: PASS — every pre-existing test (they pin the text-only identity) plus the new span tests and the proptest.

- [ ] **Step 5: Clippy + commit**

```bash
cargo clippy -p ab-plaintext --all-targets 2>&1 | tail -1
git add crates/ab-plaintext
git commit -m "feat(plaintext): span-emitting AAT projection (projection_spans producer)"
```

---

### Task 2: `ProjectionSpans` warehouse table + `SCHEMA_VERSION` 2 (`ab-warehouse`)

**Files:**
- Modify: `crates/ab-warehouse/src/schema.rs` (`SCHEMA_VERSION`, enum variant, `ALL`, `MERGED_DATA`, `file_name`, `column_names`, `ProjectionSpanRow`)
- Modify: `crates/ab-warehouse/src/writer.rs` (writer field + arms + `append_projection_spans` + schema fn)
- Modify: `crates/ab-warehouse/sql/schema.sql` (header/policy comments + `CREATE TABLE projection_spans`)
- Modify: `crates/ab-warehouse/sql/morph_views.sql` (runs-view filter + marked `warehouse_projection_spans` view)
- Modify: `crates/ab-warehouse/src/sql.rs` (conditional section strip + tests)
- Test: existing test modules in those files

**Interfaces:**
- Consumes: nothing from Task 1.
- Produces (used by Tasks 3–4):
  ```rust
  pub const SCHEMA_VERSION: u32 = 2;
  WarehouseTable::ProjectionSpans  // file_name() == "projection_spans.parquet"
  pub struct ProjectionSpanRow {
      pub run_id: Arc<str>, pub source_id: Arc<str>, pub text_id: Arc<str>,
      pub projected_char_start: u64, pub projected_char_end: u64,
      pub aat_pointer: String, pub inline_kind: String,
      pub is_ruby_base: bool, pub is_gaiji: bool, pub is_note: bool,
  }
  impl WarehouseWriter { pub fn append_projection_spans(&mut self, rows: &[ProjectionSpanRow]) -> Result<()>; }
  ```

- [ ] **Step 1: Write the failing tests**

In `crates/ab-warehouse/src/schema.rs` tests, update `every_table_has_a_parquet_file_name`:

```rust
        assert_eq!(names.len(), 12);
        assert!(names.contains(&"projection_spans.parquet"));
```

(keep the existing asserts). In `crates/ab-warehouse/src/writer.rs` tests add:

```rust
    #[test]
    fn writes_projection_spans_rows() {
        let root = temp_dir("projection-spans");
        let paths = WarehousePaths::new(&root, "run-a");
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer
            .append_projection_spans(&[ProjectionSpanRow {
                run_id: Arc::from("run-a"),
                source_id: Arc::from("source-a"),
                text_id: Arc::from("work-a"),
                projected_char_start: 0,
                projected_char_end: 3,
                aat_pointer: "/blocks/0/content/1".to_owned(),
                inline_kind: "ruby".to_owned(),
                is_ruby_base: true,
                is_gaiji: false,
                is_note: false,
            }])
            .unwrap();
        writer.finalize().unwrap();

        assert_eq!(
            parquet_table_row_count(&paths.final_dir, WarehouseTable::ProjectionSpans).unwrap(),
            1
        );
        let _ = fs::remove_dir_all(root);
    }
```

(add `ProjectionSpanRow` to the `crate::schema::{...}` import in writer.rs). In `crates/ab-warehouse/src/sql.rs` tests, update the policy assertion and add the views assertions:

```rust
        assert!(SCHEMA_SQL.contains(
            "Readers must reject runs.schema_version values greater than the reader's supported maximum (2)."
        ));
        assert!(MORPH_VIEWS_SQL_TEMPLATE.contains(&format!(
            "WHERE schema_version <= {}",
            crate::schema::SCHEMA_VERSION
        )));
        assert!(MORPH_VIEWS_SQL_TEMPLATE.contains("__RUN_DIR__/projection_spans.parquet"));
```

(replace the old `"Readers must reject runs.schema_version values other than 1"` assertion) and add a strip test mirroring the feature-diffs marker behavior:

```rust
    #[test]
    fn views_sql_drops_projection_spans_section_when_table_absent() {
        let dir = std::env::temp_dir().join(format!("views-spans-{}", std::process::id()));
        fs::create_dir_all(&dir).unwrap();
        write_run_views_sql(&dir, &dir).unwrap();
        let views = fs::read_to_string(dir.join("views.sql")).unwrap();
        assert!(!views.contains("warehouse_projection_spans"));
        assert!(!views.contains("__PROJECTION_SPANS_BEGIN__"));

        fs::write(
            dir.join(WarehouseTable::ProjectionSpans.file_name()),
            b"stub",
        )
        .unwrap();
        write_run_views_sql(&dir, &dir).unwrap();
        let views = fs::read_to_string(dir.join("views.sql")).unwrap();
        assert!(views.contains("warehouse_projection_spans"));
        assert!(!views.contains("__PROJECTION_SPANS_BEGIN__"));
        let _ = fs::remove_dir_all(dir);
    }
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p ab-warehouse 2>&1 | tail -5`
Expected: COMPILE FAIL (`ProjectionSpans` variant, `ProjectionSpanRow` not found).

- [ ] **Step 3: Implement schema.rs**

- `pub const SCHEMA_VERSION: u32 = 2;`
- Add `ProjectionSpans` to the enum, `ALL`, and `MERGED_DATA`, in each case directly after `Sources`.
- `file_name`: `Self::ProjectionSpans => "projection_spans.parquet",`
- `column_names`:

```rust
            Self::ProjectionSpans => &[
                "run_id",
                "source_id",
                "text_id",
                "projected_char_start",
                "projected_char_end",
                "aat_pointer",
                "inline_kind",
                "is_ruby_base",
                "is_gaiji",
                "is_note",
            ],
```

- Row struct (place after `SourceRow`):

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectionSpanRow {
    pub run_id: Arc<str>,
    pub source_id: Arc<str>,
    pub text_id: Arc<str>,
    pub projected_char_start: u64,
    pub projected_char_end: u64,
    pub aat_pointer: String,
    pub inline_kind: String,
    pub is_ruby_base: bool,
    pub is_gaiji: bool,
    pub is_note: bool,
}
```

- [ ] **Step 4: Implement writer.rs**

Add the field `projection_spans: Option<ArrowWriter<File>>` after `sources` in the struct; in `create_for_tables` add (after the `sources` entry):

```rust
            projection_spans: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::ProjectionSpans,
                projection_spans_schema(),
            )?,
```

`writes_table` arm: `WarehouseTable::ProjectionSpans => self.projection_spans.is_some(),`

Append method (after `append_sources`; let-else because triage runs don't open it):

```rust
    pub fn append_projection_spans(&mut self, rows: &[ProjectionSpanRow]) -> Result<()> {
        if rows.is_empty() {
            return Ok(());
        }
        let Some(writer) = self.projection_spans.as_mut() else {
            return Ok(());
        };
        write_batch(
            writer,
            projection_spans_schema(),
            vec![
                string_array(rows.iter().map(|row| row.run_id.as_ref())),
                string_array(rows.iter().map(|row| row.source_id.as_ref())),
                string_array(rows.iter().map(|row| row.text_id.as_ref())),
                u64_array(rows.iter().map(|row| row.projected_char_start)),
                u64_array(rows.iter().map(|row| row.projected_char_end)),
                string_array(rows.iter().map(|row| row.aat_pointer.as_str())),
                string_array(rows.iter().map(|row| row.inline_kind.as_str())),
                bool_array(rows.iter().map(|row| row.is_ruby_base)),
                bool_array(rows.iter().map(|row| row.is_gaiji)),
                bool_array(rows.iter().map(|row| row.is_note)),
            ],
        )
    }
```

`append_record_batch` arm (after Sources):

```rust
            WarehouseTable::ProjectionSpans => self
                .projection_spans
                .as_mut()
                .expect("projection_spans writer open")
                .write(&batch)?,
```

`finalize`: add `close_writer(self.projection_spans.take())?;` after the sources line. Schema fn (after `sources_schema`):

```rust
fn projection_spans_schema() -> Arc<Schema> {
    schema(vec![
        utf8("run_id", false),
        utf8("source_id", false),
        utf8("text_id", false),
        u64_field("projected_char_start", false),
        u64_field("projected_char_end", false),
        utf8("aat_pointer", false),
        utf8("inline_kind", false),
        bool_field("is_ruby_base"),
        bool_field("is_gaiji"),
        bool_field("is_note"),
    ])
}
```

Add `ProjectionSpanRow` to the `crate::schema::{...}` import at the top.

- [ ] **Step 5: Update the SQL templates**

`crates/ab-warehouse/sql/schema.sql` — replace lines 1–2 with:

```sql
-- Morph warehouse schema version 2.
-- Readers must reject runs.schema_version values greater than the reader's supported maximum (2).
```

and add, directly after the `sources` table and before `analyses`:

```sql
-- projection_spans (schema v2 sidecar): projected-plaintext char offsets → AAT inline nodes.
-- One row per node contributing ≥1 projected char; spans are disjoint and jointly cover the text.
-- is_note is structurally FALSE in v1 emission (note nodes are excluded from projection);
-- the column is forward infrastructure per the interestingness-ranking design.
CREATE TABLE projection_spans (
  run_id VARCHAR,
  source_id VARCHAR,
  text_id VARCHAR,
  projected_char_start UBIGINT,
  projected_char_end UBIGINT,
  aat_pointer VARCHAR,
  inline_kind VARCHAR,
  is_ruby_base BOOLEAN,
  is_gaiji BOOLEAN,
  is_note BOOLEAN
);
```

`crates/ab-warehouse/sql/morph_views.sql` — change `WHERE schema_version = 1;` to `WHERE schema_version <= 2;` and add after the `warehouse_nway_region_analyzers` view:

```sql
-- __PROJECTION_SPANS_BEGIN__
CREATE OR REPLACE VIEW warehouse_projection_spans AS
SELECT * FROM read_parquet('__RUN_DIR__/projection_spans.parquet');
-- __PROJECTION_SPANS_END__
```

- [ ] **Step 6: Conditional strip in sql.rs**

In `write_run_views_sql` (`crates/ab-warehouse/src/sql.rs:26`), after the existing `NwayFeatureDiffs` strip block, add:

```rust
    if !output_run_dir
        .join(WarehouseTable::ProjectionSpans.file_name())
        .exists()
    {
        views = remove_marked_sql_sections(
            &views,
            "-- __PROJECTION_SPANS_BEGIN__",
            "-- __PROJECTION_SPANS_END__",
        );
    }
```

and extend the marker-cleanup chain:

```rust
    views = views
        .replace("-- __RAW_FEATURE_DIFFS_BEGIN__\n", "")
        .replace("-- __RAW_FEATURE_DIFFS_END__\n", "")
        .replace("-- __PROJECTION_SPANS_BEGIN__\n", "")
        .replace("-- __PROJECTION_SPANS_END__\n", "");
```

- [ ] **Step 7: Run tests to verify they pass**

Run: `cargo test -p ab-warehouse 2>&1 | tail -5`
Expected: PASS, including the pre-existing `schema_sql_columns_match_documented_parquet_columns` (it iterates `ALL` and now checks the new table) and `empty_parquet_schemas_match_documented_columns`.

- [ ] **Step 8: Clippy + commit**

```bash
cargo clippy -p ab-warehouse --all-targets 2>&1 | tail -1
git add crates/ab-warehouse
git commit -m "feat(warehouse): projection_spans table; SCHEMA_VERSION 2 with reader-max policy"
```

---

### Task 3: Reader max schema version → 2 (`ab-morph-run` summarizer)

**Files:**
- Modify: `crates/ab-morph-run/src/summary/interesting.rs:51` (constant) and the `future_schema_version_is_rejected` test (~line 2187)

**Interfaces:**
- Consumes: Task 2's `SCHEMA_VERSION = 2` (run rows produced by the writer carry it; summarizer tests that construct `RunRow { schema_version: crate::warehouse::schema::SCHEMA_VERSION, .. }` pick it up automatically).
- Produces: `pub(crate) const READER_MAX_SCHEMA_VERSION: u32 = 2;` — the summarizer accepts v1 and v2 runs, rejects ≥3.

- [ ] **Step 1: Update the rejection test to expect max 2**

In `crates/ab-morph-run/src/summary/interesting.rs` (~line 2187), change the fixture's future version from `2` to `3` and the expected message to `"schema_version 3 exceeds this reader's supported maximum 2"`. Add, alongside it, an acceptance case for a version-1 run if the test builds run rows directly (mirror the existing `run_row(...)` helper usage: `run_row(1, ...)` must NOT error).

- [ ] **Step 2: Run to verify the test fails**

Run: `cargo test -p ab-morph-run --features test-analyzer future_schema_version -- --nocapture 2>&1 | tail -5`
Expected: FAIL — reader still rejects 2 / message says maximum 1.

- [ ] **Step 3: Bump the constant**

`crates/ab-morph-run/src/summary/interesting.rs:51`:

```rust
pub(crate) const READER_MAX_SCHEMA_VERSION: u32 = 2;
```

- [ ] **Step 4: Full crate test run**

Run: `cargo test -p ab-morph-run --features test-analyzer 2>&1 | tail -5`
Expected: PASS. (Task 2 already changed what the writer stamps; if any summarizer test fixture hard-codes `schema_version: 1` and asserts acceptance, it must still pass — v1 stays readable. Fixtures asserting the old "other than 1" message are updated to the new message.)

- [ ] **Step 5: Clippy + commit**

```bash
cargo clippy -p ab-morph-run --features test-analyzer --all-targets 2>&1 | tail -1
git add crates/ab-morph-run
git commit -m "feat(morph-run): accept warehouse schema_version 2 (reader max 2)"
```

---

### Task 4: Pipeline emission of projection spans + P1 (drop the AAT DOM)

**Files:**
- Modify: `crates/ab-morph-run/src/lib.rs:27` (import), plus new test in its test module
- Modify: `crates/ab-morph-run/src/warehouse/rows.rs` (row helper + unit test)
- Modify: `crates/ab-morph-run/src/pipeline.rs` (`run_analyze_aat_serial`: projection call, `drop(aat)`, span-row emission)

**Interfaces:**
- Consumes: Task 1's `from_aat_value_with_spans` / `ProjectionSpan`; Task 2's `ProjectionSpanRow`, `WarehouseTable::ProjectionSpans`, `append_projection_spans`.
- Produces: `warehouse::rows::projection_span_rows(run_id: &str, source_id: &str, text_id: &str, spans: &[ab_plaintext::ProjectionSpan]) -> Vec<ProjectionSpanRow>`; Full-profile warehouse runs contain `projection_spans.parquet`.

- [ ] **Step 1: Write the failing tests**

In `crates/ab-morph-run/src/warehouse/rows.rs` tests:

```rust
    #[test]
    fn maps_projection_spans_to_rows_with_shared_ids() {
        let spans = vec![ab_plaintext::ProjectionSpan {
            projected_char_start: 0,
            projected_char_end: 2,
            aat_pointer: "/blocks/0/content/0".to_owned(),
            inline_kind: "ruby".to_owned(),
            is_ruby_base: true,
            is_gaiji: false,
            is_note: false,
        }];
        let rows = projection_span_rows("run-a", "source-a", "work-a", &spans);
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].run_id.as_ref(), "run-a");
        assert_eq!(rows[0].text_id.as_ref(), "work-a");
        assert_eq!(rows[0].projected_char_end, 2);
        assert!(rows[0].is_ruby_base);
    }
```

In `crates/ab-morph-run/src/lib.rs` tests (model: `warehouse_mode_writes_sealed_parquet_without_jsonl_outputs` at ~line 1474 and `warehouse_triage_profile_omits_raw_feature_tables` at ~line 1514):

```rust
    #[test]
    fn warehouse_full_profile_writes_projection_spans() {
        let dir = temp_dir("warehouse-projection-spans");
        let aat_dir = dir.join("aats");
        let warehouse_dir = dir.join("warehouse");
        fs::create_dir_all(&aat_dir).unwrap();
        fs::write(
            aat_dir.join("source-a.json"),
            tiny_aat("work-a").replace("吾輩は猫である。", "今日"),
        )
        .unwrap();

        run_analyze_aat_warehouse(
            None,
            Some(&aat_dir),
            &["test:single".to_owned(), "test:split".to_owned()],
            &warehouse_dir,
            "run-a",
            1,
            WarehouseProfile::Full,
        )
        .unwrap();

        let run_dir = warehouse_dir.join("runs").join("run-a");
        assert_eq!(
            warehouse::writer::parquet_table_row_count(
                &run_dir,
                WarehouseTable::ProjectionSpans
            )
            .unwrap(),
            1,
            "TINY_AAT has exactly one contributing text node"
        );
        let views_sql = fs::read_to_string(run_dir.join("views.sql")).unwrap();
        assert!(views_sql.contains("warehouse_projection_spans"));
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn warehouse_triage_profile_omits_projection_spans() {
        let dir = temp_dir("warehouse-triage-spans");
        let aat_dir = dir.join("aats");
        let warehouse_dir = dir.join("warehouse");
        fs::create_dir_all(&aat_dir).unwrap();
        fs::write(
            aat_dir.join("source-a.json"),
            tiny_aat("work-a").replace("吾輩は猫である。", "今日"),
        )
        .unwrap();

        run_analyze_aat_warehouse(
            None,
            Some(&aat_dir),
            &["test:single".to_owned(), "test:split".to_owned()],
            &warehouse_dir,
            "run-a",
            1,
            WarehouseProfile::Triage,
        )
        .unwrap();

        let run_dir = warehouse_dir.join("runs").join("run-a");
        assert!(!run_dir.join("projection_spans.parquet").exists());
        let views_sql = fs::read_to_string(run_dir.join("views.sql")).unwrap();
        assert!(!views_sql.contains("warehouse_projection_spans"));
        let _ = fs::remove_dir_all(dir);
    }
```

(If `parquet_table_row_count` is not visible from the lib tests under that path, use the same access pattern the nearby merge tests at ~line 2030 use — follow the existing import, do not re-export anything new.)

Also extend the existing parallel-mode test's expectations only if it asserts an exhaustive file list (it doesn't as of `lib.rs:1568`; leave it alone).

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p ab-morph-run --features test-analyzer projection_spans -- --nocapture 2>&1 | tail -5`
Expected: COMPILE FAIL (`projection_span_rows` not found).

- [ ] **Step 3: Implement the row helper**

In `crates/ab-morph-run/src/warehouse/rows.rs` (add `ProjectionSpanRow` to the `ab_warehouse::schema::{...}` import):

```rust
pub(crate) fn projection_span_rows(
    run_id: &str,
    source_id: &str,
    text_id: &str,
    spans: &[ab_plaintext::ProjectionSpan],
) -> Vec<ProjectionSpanRow> {
    let run_id = std::sync::Arc::<str>::from(run_id);
    let source_id = std::sync::Arc::<str>::from(source_id);
    let text_id = std::sync::Arc::<str>::from(text_id);
    spans
        .iter()
        .map(|span| ProjectionSpanRow {
            run_id: std::sync::Arc::clone(&run_id),
            source_id: std::sync::Arc::clone(&source_id),
            text_id: std::sync::Arc::clone(&text_id),
            projected_char_start: span.projected_char_start,
            projected_char_end: span.projected_char_end,
            aat_pointer: span.aat_pointer.clone(),
            inline_kind: span.inline_kind.clone(),
            is_ruby_base: span.is_ruby_base,
            is_gaiji: span.is_gaiji,
            is_note: span.is_note,
        })
        .collect()
}
```

- [ ] **Step 4: Wire the pipeline**

`crates/ab-morph-run/src/lib.rs:27` becomes:

```rust
use ab_plaintext::{PlainTextDocument, from_aat_value, from_aat_value_with_spans};
```

In `crates/ab-morph-run/src/pipeline.rs`, `run_analyze_aat_serial`, replace the document-projection block (`let document = match from_aat_value(&aat) { ... };`, ~lines 609–647) with:

```rust
        let collect_projection_spans = warehouse_writer
            .as_ref()
            .is_some_and(|writer| writer.writes_table(WarehouseTable::ProjectionSpans));
        let projected = if collect_projection_spans {
            from_aat_value_with_spans(&aat).map(|(document, spans)| (document, Some(spans)))
        } else {
            from_aat_value(&aat).map(|document| (document, None))
        };
        let (document, projection_spans) = match projected {
            Ok(value) => value,
            Err(error) => {
                if let Some(writer) = &mut warehouse_writer {
                    warehouse_error_count += 1;
                    writer.append_errors(&[warehouse_error_row(
                        options
                            .warehouse
                            .as_ref()
                            .expect("warehouse options")
                            .paths
                            .run_id
                            .as_str(),
                        Some(source_id.clone()),
                        None,
                        None,
                        "project_aat",
                        "project_aat_failed",
                        &error.to_string(),
                    )])?;
                    continue;
                }
                if let Some(writer) = &mut errors_writer {
                    write_error_row(
                        &mut **writer,
                        &RunErrorRow {
                            input_path,
                            source_id: Some(source_id),
                            text_id: None,
                            analyzer: None,
                            stage: "project_aat".to_owned(),
                            error: error.to_string(),
                        },
                    )?;
                    continue;
                }
                return Err(error.into());
            }
        };
        // P1: the parsed AAT DOM is unused past projection; drop it now so the
        // per-document peak excludes it (several× file size on the large tail).
        drop(aat);
```

(The error arms are byte-for-byte the current ones; only the `Ok` binding changes.)

Then, inside the existing `if let Some(writer) = &mut warehouse_writer && let Some(first_analysis) = analyses.first()` block, directly after `writer.append_sources(&[source])?;` (~line 788), add:

```rust
            if let Some(spans) = &projection_spans {
                for chunk in spans.chunks(WAREHOUSE_MORPHEME_ROW_BATCH_SIZE) {
                    let rows = warehouse::rows::projection_span_rows(
                        run_id,
                        &source_id,
                        &document.text_id,
                        chunk,
                    );
                    writer.append_projection_spans(&rows)?;
                }
            }
```

- [ ] **Step 5: Run tests to verify they pass**

Run: `cargo test -p ab-morph-run --features test-analyzer 2>&1 | tail -5`
Expected: PASS — the two new tests plus the whole existing suite (the shard-merge path needs no change: `MERGED_DATA` already contains the new table from Task 2, and the merge staging iterates `merged_data_tables()`).

- [ ] **Step 6: Clippy + commit**

```bash
cargo clippy -p ab-morph-run --features test-analyzer --all-targets 2>&1 | tail -1
git add crates/ab-morph-run
git commit -m "feat(morph-run): write projection_spans in Full warehouse runs; drop AAT DOM after projection"
```

---

### Task 5: P2 — eliminate the ortho-off full-text clone

**Files:**
- Modify: `crates/ab-morph-run/src/pipeline.rs` (~lines 651–679: the `normalized_text` / `norm_doc` block)

**Interfaces:**
- Consumes: the Task 4 state of `run_analyze_aat_serial`.
- Produces: no API change; behavior-identical output. `norm_doc: &PlainTextDocument` (borrow) replaces the owned `norm_doc`.

- [ ] **Step 1: Restructure to borrow when no normalization fired**

Replace the block from `let (normalized_text, offset_map_opt, annotations_opt): (...)` down to and including the `let shared_normalized: Arc<str> = ...` line with:

```rust
        // Orthographic normalization (katakana→hiragana) for pre-war text.
        // `None` means "no normalization applied" — analyze the document text
        // directly with NO extra copy (P2; warehouse runs always take this path).
        let (normalized_text_opt, offset_map_opt, annotations_opt): (
            Option<String>,
            Option<ab_ortho_detect::OffsetMap>,
            Option<Vec<ab_ortho_detect::OrthoAnnotation>>,
        ) = if let Some(ref det) = detector {
            let sentences = ab_plaintext::sentence_split(&document.text);
            let annotations = det.detect(&sentences);
            if annotations.is_empty() {
                (None, None, None)
            } else {
                let (norm_text, map) =
                    ab_ortho_detect::ortho_normalize(&document.text, &annotations);
                (Some(norm_text), Some(map), Some(annotations))
            }
        } else {
            (None, None, None)
        };

        let normalized_doc_storage;
        let norm_doc: &ab_plaintext::PlainTextDocument = match normalized_text_opt {
            Some(text) => {
                normalized_doc_storage = ab_plaintext::PlainTextDocument {
                    text_id: document.text_id.clone(),
                    source_format: document.source_format,
                    text,
                };
                &normalized_doc_storage
            }
            None => &document,
        };
        // One allocation per document, shared by every per-analyzer Analysis.
        let shared_normalized: Arc<str> = Arc::from(norm_doc.text.as_str());
```

Everything downstream already reads `norm_doc` by reference (`analyzer.analyze(&norm_doc)` becomes `analyzer.analyze(norm_doc)`) and `document.text` stays untouched for the n-way/source paths. The `shared_original` line just below is unchanged (it already keys off `offset_map_opt.is_some()`).

- [ ] **Step 2: Run tests to verify identical behavior**

Run: `cargo test -p ab-morph-run --features test-analyzer 2>&1 | tail -5`
Expected: PASS — full suite, no output change anywhere (this is a pure allocation removal).

- [ ] **Step 3: Clippy + commit**

```bash
cargo clippy -p ab-morph-run --features test-analyzer --all-targets 2>&1 | tail -1
git add crates/ab-morph-run/src/pipeline.rs
git commit -m "perf(morph-run): analyze without cloning document text when ortho is off"
```

---

### Task 6: P3 — boundary sets as sorted vectors (`ab-morph-diff`)

**Files:**
- Modify: `crates/ab-morph-diff/src/nway.rs:463-485` (`boundary_counts`)

**Interfaces:**
- Consumes: nothing from other tasks (independent; can run any time after Task 0).
- Produces: same signature `fn boundary_counts(analyses: &[&Analysis], source_len: usize) -> (usize, usize)`, identical return values, flat memory instead of BTree nodes.

- [ ] **Step 1: Confirm existing coverage pins the behavior**

Run: `cargo test -p ab-morph-diff 2>&1 | tail -3` — note the passing count. `boundary_counts` feeds `NwayStats::unanimous_boundary_count`/`variable_boundary_count`, asserted by the existing nway/stats tests. No new test needed if a boundary-count assertion already exists; verify with:

```bash
grep -rn "unanimous_boundary_count\|variable_boundary_count" crates/ab-morph-diff/src | grep -c "assert\|=="
```

If NOTHING asserts the counts (grep shows only definitions), first add this test to `nway.rs`'s test module and watch it pass against the OLD code (characterization before refactor):

```rust
    #[test]
    fn boundary_counts_split_unanimous_and_variable() {
        // "今日は" (3 chars): analyzer A splits 今日|は (boundary {2});
        // analyzer B splits 今|日|は (boundaries {1, 2}).
        // Unanimous: {2}; variable: {1}.
        let a = test_analysis("w", "a", &[(0, 2), (2, 3)]);
        let b = test_analysis("w", "b", &[(0, 1), (1, 2), (2, 3)]);
        let (unanimous, variable) = boundary_counts(&[&a, &b], 3);
        assert_eq!((unanimous, variable), (1, 1));
    }
```

(build `test_analysis` from the module's existing analysis/morpheme fixture helpers — match whatever constructor pattern the surrounding tests use for `Analysis` with char spans; only `char_span` matters to `boundary_counts`.)

- [ ] **Step 2: Rewrite `boundary_counts`**

```rust
fn boundary_counts(analyses: &[&Analysis], source_len: usize) -> (usize, usize) {
    // Sorted deduped Vecs instead of BTreeSets: morpheme boundaries number in
    // the millions on large documents, and flat storage avoids the per-node
    // BTree overhead that showed up in warehouse-run RSS profiles.
    let boundary_sets = analyses
        .iter()
        .map(|analysis| {
            let mut offsets = analysis
                .morphemes
                .iter()
                .flat_map(|morpheme| [morpheme.char_span.start, morpheme.char_span.end])
                .filter(|offset| *offset != 0 && *offset != source_len)
                .collect::<Vec<_>>();
            offsets.sort_unstable();
            offsets.dedup();
            offsets
        })
        .collect::<Vec<_>>();
    let mut all_boundaries = boundary_sets
        .iter()
        .flatten()
        .copied()
        .collect::<Vec<_>>();
    all_boundaries.sort_unstable();
    all_boundaries.dedup();
    let unanimous_boundary_count = all_boundaries
        .iter()
        .filter(|boundary| {
            boundary_sets
                .iter()
                .all(|set| set.binary_search(boundary).is_ok())
        })
        .count();
    let variable_boundary_count = all_boundaries.len() - unanimous_boundary_count;
    (unanimous_boundary_count, variable_boundary_count)
}
```

If nothing else in the file still uses `BTreeSet`, trim the `use std::collections::{BTreeMap, BTreeSet};` import accordingly (line 251's `BTreeSet<FeatureKey>` return type still needs it — check before removing).

- [ ] **Step 3: Run tests to verify identical results**

Run: `cargo test -p ab-morph-diff && cargo test -p ab-morph-run --features test-analyzer 2>&1 | tail -3`
Expected: PASS both — same counts, downstream warehouse rows unchanged.

- [ ] **Step 4: Clippy + commit**

```bash
cargo clippy -p ab-morph-diff --all-targets 2>&1 | tail -1
git add crates/ab-morph-diff
git commit -m "perf(morph-diff): flat sorted boundary vectors in n-way stats"
```

---

### Task 7: Full-corpus regeneration, parity gate, canonical swap, P4 measurement

**Files:** none committed except doc updates (this plan's results section; governing spec status notes)

**Interfaces:**
- Consumes: Tasks 1–6 merged; real dictionaries (run from the main checkout; see Global Constraints for the worktree symlink gotcha).
- Produces: the new canonical warehouse run with `projection_spans.parquet`; measured wall/RSS record.

- [ ] **Step 1: Workspace green + validation gates before the heavy run**

```bash
cargo test -p ab-plaintext && cargo test -p ab-warehouse && cargo test -p ab-morph-diff \
  && cargo test -p ab-morph-run --features test-analyzer \
  && cargo clippy --workspace --all-targets --features ab-morph-run/test-analyzer 2>&1 | tail -1
cd /home/bor/Projects/soranoha && just root-flake-check-no-build && cd ab-validator
```

Expected: all suites pass, clippy clean, root flake evaluates. (`parity-audit` is intentionally NOT a gate this cycle — see Global Constraints.)

Also make the monorepo dictionary dir flake-native (once, before the run):

```bash
rm ab-validator/dictionary   # dangling symlink to ../vibrato-pipe/dictionary
mkdir -p ab-validator/dictionary/compiled ab-validator/dictionary/optimized
touch ab-validator/dictionary/compiled/.gitkeep ab-validator/dictionary/optimized/.gitkeep
# record the delta in docs/migration-status.md §Intentional Deltas, then:
git add -A ab-validator/dictionary docs/migration-status.md
git commit -m "chore(monorepo): flake-native dictionary dir replaces split-repo symlink"
just -f ab-validator/justfile dictionary-build-all   # populates compiled/ from flake outputs
```

- [ ] **Step 2: Full-corpus run under time -v (jobs=8, Full profile)**

```bash
/usr/bin/time -v just morph-warehouse-run 2>&1 | tee /tmp/phase3-run.log
grep -E "Maximum resident|Elapsed|auto-jobs" /tmp/phase3-run.log
```

(no args: the recipe defaults are profile=full, the canonical `aat_dir`, generated run_id `full-<timestamp>-jobs8`, jobs=8 — do NOT pass `""` positionals, they would override the defaults with empty strings). Record wall + peak RSS. Baselines for comparison: jobs=19 → 59:43 / 65.6 GB; jobs=10 pre-Arc → 42 min. Expected: completion without earlyoom (`journalctl -u earlyoom --since "2 hours ago"` quiet) and peak RSS visibly below the 65.6 GB baseline.

- [ ] **Step 3: Row-count parity gate vs canonical**

```bash
OLD=/db/ab-validator/morph-warehouse/runs/full-2026-07-05_164518-jobs0
NEW=/db/ab-validator/morph-warehouse/runs/<new run id>
for t in sources analyses morphemes morpheme_features nway_regions nway_region_analyzers nway_feature_diffs feature_pattern_counts errors; do
  "$AB_DUCKDB_BIN" -c "SELECT '$t', (SELECT count(*) FROM read_parquet('$OLD/$t.parquet')) AS old,
                       (SELECT count(*) FROM read_parquet('$NEW/$t.parquet')) AS new;"
done
# If a table is staged as a part DIRECTORY rather than a single file (large
# tables skip merge compaction), use read_parquet('<run>/<t>.parquet/*.parquet')
# for that table instead — same counting either way.
"$AB_DUCKDB_BIN" -c "SELECT analyzer_id, count(*) FROM read_parquet('$NEW/analyses.parquet') GROUP BY 1 ORDER BY 1;"
"$AB_DUCKDB_BIN" -c "SELECT analyzer_id, count(*) FROM read_parquet('$OLD/analyses.parquet') GROUP BY 1 ORDER BY 1;"
"$AB_DUCKDB_BIN" -c "SELECT count(*), count(DISTINCT source_id), min(projected_char_start), max(projected_char_end) FROM read_parquet('$NEW/projection_spans.parquet');"
"$AB_DUCKDB_BIN" -c "SELECT schema_version, run_id FROM read_parquet('$NEW/runs.parquet');"
```

(the `*` glob covers tables staged as part directories). Expected: the 9 merged v1 data tables + per-analyzer counts EXACTLY equal; `schema_version = 2`; `projection_spans` has ~10–30M rows with `count(DISTINCT source_id)` = 17,885 (every source contributes ≥1 span; if a handful are missing, check they are zero-text sources before accepting).

Also verify the summarizer reads the v2 run end to end:

```bash
cargo run --release -p ab-morph-run -- summarize-warehouse-interesting \
  --run-dir "$NEW" --limit 5 --format json | head -20
```

Expected: JSON output, no schema_version error.

- [ ] **Step 4: Canonical swap — ONLY on exact match**

If every count matches: `rm -rf "$OLD"` and note the new run id as canonical. If ANY count differs: **do not delete anything**; diagnose (the projection identity invariant from Task 1 is the first suspect).

- [ ] **Step 5: Record outcomes + spec status updates**

- Append a "Validation record" section to THIS plan file: run id, auto-jobs/jobs line, wall, peak RSS, parity verdict, projection_spans row count, earlyoom check result, and the wall/RSS deltas vs both baselines.
- In `docs/superpowers/specs/2026-07-05-interestingness-ranking-design.md`: mark Implementation Phases §Phase 3 as implemented (status note with date + run id, matching the Phase 2 status-note style), flip Decision 3's status from Proposed to Accepted, and mark Open Question 2 resolved (analysis pass).
- In `docs/superpowers/specs/2026-07-06-projection-spans-design.md`: no changes expected; if implementation deviated, record the deviation inline with a dated note.
- Update the next-session handoff (`docs/superpowers/plans/2026-07-06-next-session-handoff.md`): item 3 done; carry forward the deferred perf items (dictionary mmap spike, §3.15, batch sweep, auto-jobs re-fit with the new P4 numbers).

```bash
git add docs/superpowers && git commit -m "docs: record Phase 3 projection_spans validation results and spec statuses"
```

---

## Self-Review Notes

- **Spec coverage:** §Part 1 API/emission/remap/ortho → Task 1; §Part 2 schema/writer/SQL/version-bump/profile/emission-point → Tasks 2–4; §Part 3 P1 → Task 4, P2 → Task 5, P3 → Task 6, P4 → Task 7; §Error Behavior version gate → Task 3, absent-sidecar probe → Task 2 strip test + Task 4 triage test; §Test Plan items 1–3 → Task 1, 4 → Tasks 2/4, 5 → Task 3, 6 → Tasks 5/6, 7 → Task 4; §Validation → Task 7; §Deferred → Task 7 Step 5 handoff note.
- **Ordering:** Tasks 2→3 must precede 4 (writer/table + reader constant before wiring); Task 1 precedes 4; Tasks 5 and 6 are independent of each other; Task 7 last.
- **Type consistency:** `ProjectionSpan` (u64 offsets, String pointer/kind, three bools) matches `ProjectionSpanRow` field-for-field; `projection_span_rows(run_id, source_id, text_id, spans)` matches its Task 4 call site; `append_projection_spans(&[ProjectionSpanRow])` matches Tasks 2/4; `boundary_counts` signature unchanged.
- **Deliberate scope notes:** no CLI changes; triage lists untouched; `import-aozora-metadata`, calibration code, and `score_version` untouched; the non-warehouse JSONL parallel path untouched.
