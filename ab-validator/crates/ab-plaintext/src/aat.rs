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
}

impl SpanKind {
    fn as_str(self) -> &'static str {
        match self {
            Self::Text => "text",
            Self::RubyBase => "ruby",
            Self::Gaiji => "gaiji",
            Self::Accent => "accent",
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

/// RFC 6901 escaping (`~0`/`~1`) is intentionally omitted: keys are a closed
/// set of static literals (`blocks`, `content`, `children`, `upper`, `lower`,
/// `caption`) that contain no `/` or `~`. Escape before adding dynamic keys.
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
    // Back-matter `source_note` blocks carry the Aozora colophon and
    // attribution lines (底本, 入力／校正, 青空文庫作成ファイル). They are
    // provenance about the text, not the text: projecting them appends a
    // near-identical modern-kana/ASCII tail to every work, which shows up as
    // boilerplate "disagreement" in tokenizer comparisons and breaks the
    // historical-kana remap. The honbun projection cuts at 底本： for the
    // same reason.
    if node.get("kind").and_then(Value::as_str) == Some("source_note") {
        return;
    }
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
        // Raw nodes are never body text: the adapter emits them only for
        // layout markers (［＃…字下げ］, 改ページ, kaeriten, …), parser
        // residue, and unparsed markup fragments. Projecting their source
        // leaks markup into the tokenized text stream; the parser-IR
        // converter's sibling projections drop raw for the same reason.
        "raw" => {}
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

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    #[test]
    fn builds_document_from_work_id_and_visible_text() {
        let aat = json!({
            "work_id": "w1",
            "blocks": [{
                "kind": "paragraph",
                "content": [{"kind": "text", "value": "本文"}]
            }]
        });

        let doc = from_aat_value(&aat).unwrap();
        assert_eq!(doc.text_id, "w1");
        assert_eq!(doc.source_format, SourceFormat::AatVisibleText);
        assert_eq!(doc.text, "本文");
    }

    #[test]
    fn projects_nested_visible_text() {
        let aat = json!({
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
        });

        // the raw node ("E") is a layout/markup marker and must not project
        assert_eq!(visible_text_projection(&aat), "ABCDéfallbackFGH");
    }

    #[test]
    fn projects_figure_caption_text() {
        let aat = json!({
            "work_id": "w-figure",
            "blocks": [{
                "kind": "paragraph",
                "content": [{
                    "kind": "figure",
                    "filename": "fig00001_01.png",
                    "alt": "猫の図",
                    "css_class": "",
                    "width": 321,
                    "height": 123,
                    "caption": [{"kind": "text", "value": "猫の図"}]
                }]
            }]
        });

        assert_eq!(visible_text_projection(&aat), "猫の図");
    }

    #[test]
    fn projection_excludes_aozora_input_notes() {
        let aat = json!({
            "work_id": "w-notes",
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "前"},
                    {
                        "kind": "style",
                        "style_type": "notes",
                        "content": [{"kind": "text", "value": "［＃改丁］"}]
                    },
                    {"kind": "text", "value": "後"}
                ]
            }]
        });

        assert_eq!(visible_text_projection(&aat), "前後");
    }

    #[test]
    fn projection_excludes_back_matter_source_notes() {
        // The colophon and attribution tail the adapter emits as
        // `source_note` blocks is provenance, not body text.
        let aat = json!({
            "work_id": "w-colophon",
            "blocks": [
                {"kind": "paragraph", "content": [{"kind": "text", "value": "本文。\n"}]},
                {
                    "kind": "source_note",
                    "placement": "back",
                    "region_class": "colophon_metadata",
                    "content": [{"kind": "text", "value": "底本：「全集」\n"}]
                },
                {
                    "kind": "source_note",
                    "placement": "back",
                    "region_class": "terminal_provenance",
                    "content": [{"kind": "text", "value": "このファイルは、インターネットの図書館、青空文庫で作られました。\n"}]
                }
            ]
        });

        assert_eq!(visible_text_projection(&aat), "本文。\n");
        let (text, spans) = visible_text_projection_with_spans(&aat);
        assert_eq!(text, "本文。\n");
        assert!(
            spans
                .iter()
                .all(|span| span.aat_pointer.starts_with("/blocks/0/"))
        );
    }

    #[test]
    fn projection_excludes_raw_layout_markers() {
        // The corpus-scale leakage class: adapter-parsed layout notes
        // (字下げ etc.) arrive as raw nodes and must not reach the
        // tokenized text stream.
        let aat = json!({
            "work_id": "w-raw-markers",
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "前"},
                    {
                        "kind": "raw",
                        "source": "［＃２字下げ］",
                        "x-provenance": "parser-derived",
                        "x-source-marker-kind": "indent"
                    },
                    {"kind": "text", "value": "後"}
                ]
            }]
        });

        assert_eq!(visible_text_projection(&aat), "前後");
    }

    #[test]
    fn projection_canonicalizes_line_endings() {
        let aat = json!({
            "work_id": "w-line-endings",
            "blocks": [{
                "kind": "paragraph",
                "content": [{"kind": "text", "value": "A\r\nB\rC\nD"}]
            }]
        });

        assert_eq!(visible_text_projection(&aat), "A\nB\nC\nD");
    }

    #[test]
    fn empty_resolved_gaiji_is_empty_and_missing_resolved_falls_back() {
        let aat = json!({
            "work_id": "w",
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "gaiji", "description": "desc", "resolved": ""},
                    {"kind": "gaiji", "description": "fallback"}
                ]
            }]
        });

        assert_eq!(visible_text_projection(&aat), "fallback");
    }

    #[test]
    fn null_resolved_unresolved_gaiji_is_empty() {
        let aat = json!({
            "work_id": "w-null-gaiji",
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "前"},
                    {
                        "kind": "gaiji",
                        "description": "※(「てへん＋僉」、第3水準1-84-94)",
                        "resolved": null,
                        "unresolved_reason": "image_fallback"
                    },
                    {"kind": "text", "value": "後"}
                ]
            }]
        });

        assert_eq!(visible_text_projection(&aat), "前後");
    }

    #[test]
    fn missing_work_id_is_error() {
        let aat = json!({"blocks": []});
        assert_eq!(from_aat_value(&aat), Err(PlainTextError::MissingAatWorkId));
    }

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
        assert_eq!(text, "ABCDéfallbackFGH");

        let expected = vec![
            (0, 1, "/blocks/0/content/0", "text", false, false, false),
            (1, 2, "/blocks/0/content/1", "ruby", true, false, false),
            (2, 3, "/blocks/0/content/2", "gaiji", false, true, false),
            (3, 4, "/blocks/0/content/4", "gaiji", false, true, false),
            (4, 5, "/blocks/0/content/5", "accent", false, false, false),
            (5, 13, "/blocks/0/content/6", "accent", false, false, false),
            // /blocks/0/content/7 is a raw layout marker: no projection span
            (
                13,
                14,
                "/blocks/0/content/8/upper/0",
                "text",
                false,
                false,
                false,
            ),
            (
                14,
                15,
                "/blocks/0/content/8/lower/0",
                "text",
                false,
                false,
                false,
            ),
            (
                15,
                16,
                "/blocks/0/content/9/content/0",
                "text",
                false,
                false,
                false,
            ),
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
                (
                    0,
                    1,
                    "/blocks/0/content/0/caption/0",
                    "text",
                    false,
                    false,
                    false
                ),
                (
                    1,
                    2,
                    "/blocks/0/children/0/content/0",
                    "text",
                    false,
                    false,
                    false
                ),
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
            prop_oneof![
                proptest::string::string_regex("[ab\r\nあ𩸽]{0,6}")
                    .unwrap()
                    .prop_map(|v| json!({"kind": "text", "value": v})),
                proptest::string::string_regex("[ab\r\nあ𩸽]{0,6}")
                    .unwrap()
                    .prop_map(|v| json!({"kind": "ruby", "base": v, "reading": "よみ"})),
                proptest::string::string_regex("[ab\r\nあ𩸽]{0,6}")
                    .unwrap()
                    .prop_map(|v| json!({"kind": "gaiji", "description": v})),
            ]
        }

        fn arb_aat() -> impl Strategy<Value = serde_json::Value> {
            proptest::collection::vec(proptest::collection::vec(arb_inline(), 0..5), 0..4).prop_map(
                |blocks| {
                    json!({
                        "work_id": "w",
                        "blocks": blocks
                            .into_iter()
                            .map(|content| json!({"kind": "paragraph", "content": content}))
                            .collect::<Vec<_>>()
                    })
                },
            )
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
}
