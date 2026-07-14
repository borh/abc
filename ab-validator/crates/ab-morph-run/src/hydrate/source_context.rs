//! Per-source resolution: AAT loading, re-projection, snippet windows,
//! and Aozora-markup reconstruction (spec Layers 1, 3, 4).

use anyhow::{Result, bail};
use serde::Serialize;

/// A load failure with its spec error-vocabulary code.
#[derive(Debug)]
pub struct SourceLoadError {
    pub code: &'static str,
    pub detail: String,
}

/// One source's AAT, loaded once: re-projected text + spans + the raw JSON
/// for pointer resolution. Offset agreement with the warehouse is enforced
/// at load (spec §Layer 1 offset-safety invariant).
#[derive(Debug)]
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
        let aat: serde_json::Value =
            serde_json::from_slice(&bytes).map_err(|err| SourceLoadError {
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

    pub fn hydrate_region(
        &self,
        char_start: u64,
        char_end: u64,
        context_chars: usize,
    ) -> RegionLayers {
        let mut errors = Vec::new();
        let total_chars = self.text.chars().count() as u64;

        let snippet = match snippet_window(&self.text, char_start, char_end, context_chars) {
            Ok(snippet) => Some(snippet),
            Err(err) => {
                // An example span the projected text cannot contain is a
                // projection disagreement (closed vocabulary) — same code
                // as the source-level char-count gate.
                errors.push(format!("projection-mismatch: {err}"));
                None
            }
        };
        // reconstruct_markup's overlap filter would happily succeed on an
        // over-long range (every span overlaps), so an out-of-range region
        // must be rejected here to avoid quoting markup for a span the
        // warehouse never defined. The AAT node layer has no such
        // correctness concern — it is just node identities — so it is still
        // computed below with char_end clamped to document bounds.
        let aozora_markup = if char_end > total_chars {
            errors.push(format!("markup-unreconstructable: span [{char_start}, {char_end}) exceeds document bounds ({total_chars} chars)"));
            None
        } else {
            match reconstruct_markup(&self.aat, &self.spans, char_start, char_end) {
                Ok((markup, _nodes)) => Some(markup),
                Err(err) => {
                    errors.push(err.to_string()); // already "markup-unreconstructable: …"
                    None
                }
            }
        };
        // Layer 4 (AAT node context) survives markup-layer failure (spec
        // §Layer 4: "useful for debugging projection artifacts" — most
        // valuable exactly when markup reconstruction fails). Clamp
        // char_end so an out-of-range region still yields the spans that
        // overlap the in-bounds portion.
        let aat_nodes = contributing_node_refs(&self.spans, char_start, char_end.min(total_chars));
        RegionLayers {
            snippet,
            aozora_markup,
            aat_nodes,
            errors,
        }
    }
}

/// A snippet window around a region, parts kept separate so JSON consumers
/// can re-mark (spec §Layer 1).
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
    /// `…before【region】after…` — the Markdown display form. Ellipses appear
    /// only where the window was clipped short of the document bounds, which
    /// the constructor encodes by leaving `before`/`after` at full context
    /// length; callers never re-check bounds.
    #[must_use]
    pub fn marked(&self) -> String {
        let lead = if self.at_doc_start { "" } else { "…" };
        let trail = if self.at_doc_end { "" } else { "…" };
        format!(
            "{lead}{}【{}】{}{trail}",
            self.before, self.region, self.after
        )
    }
}

/// Slices `text` by char index (never byte index) into a window of up to
/// `context` chars on each side of `[char_start, char_end)`. Errors when the
/// span is inverted or exceeds the text's char count.
pub fn snippet_window(
    text: &str,
    char_start: u64,
    char_end: u64,
    context: usize,
) -> Result<Snippet> {
    let chars: Vec<char> = text.chars().collect();
    let total = chars.len() as u64;
    if char_start > char_end || char_end > total {
        bail!("span [{char_start}, {char_end}) is out of range for a text of {total} chars");
    }
    let start = char_start as usize;
    let end = char_end as usize;
    let context_start = start.saturating_sub(context);
    let context_end = end.saturating_add(context).min(chars.len());
    Ok(Snippet {
        before: chars[context_start..start].iter().collect(),
        region: chars[start..end].iter().collect(),
        after: chars[end..context_end].iter().collect(),
        at_doc_start: context_start == 0,
        at_doc_end: context_end == chars.len(),
    })
}

/// One contributing AAT node's identity + flags (spec §Layer 4).
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct AatNodeRef {
    pub pointer: String,
    pub inline_kind: String,
    pub is_ruby_base: bool,
    pub is_gaiji: bool,
}

/// A byte range inside the covering span that no contributing node
/// rendered (a non-projecting marker sits there); shown as `…` in the
/// slice text.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct ByteGap {
    pub byte_start: u64,
    pub byte_end: u64,
}

/// The reconstructed markup slice (spec §Layer 3). `gaps` records unrendered
/// byte ranges (non-projecting markers inside the region); `approximate_pointers`
/// lists nodes rendered semantically rather than byte-verified; empty lists
/// mean the slice is verbatim sanitized-source markup.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct AozoraMarkup {
    pub text: String,
    pub byte_start: u64,
    pub byte_end: u64,
    pub approximate_pointers: Vec<String>,
    pub gaps: Vec<ByteGap>,
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
    // Contributing spans, deduped by pointer, in document order (spec
    // §Layer 4; the same filter is reused by `contributing_node_refs` for
    // the AAT-node layer, which survives markup failure).
    let contributing = contributing_node_refs(spans, char_start, char_end);
    if contributing.is_empty() {
        bail!(
            "markup-unreconstructable: no projection spans cover chars [{char_start}, {char_end})"
        );
    }

    let mut rendered = String::new();
    let mut approximate = Vec::new();
    let mut gaps = Vec::new();
    let mut byte_start = u64::MAX;
    let mut byte_end = 0u64;
    let mut prev_byte_end: Option<u64> = None;

    for node_ref in &contributing {
        let node = resolve_spanned_node(aat, &node_ref.pointer).ok_or_else(|| {
            anyhow::anyhow!(
                "markup-unreconstructable: pointer {} has no spanned node",
                node_ref.pointer
            )
        })?;
        let (node_start, node_end) = node_byte_span(node).ok_or_else(|| {
            anyhow::anyhow!(
                "markup-unreconstructable: node {} has no byte span",
                node_ref.pointer
            )
        })?;
        // Coverage check: a gap between consecutive contributing nodes means
        // a non-projecting marker sits inside the region — render "…" and
        // record as a byte range (spec §Layer 3 step 3).
        if let Some(prev) = prev_byte_end
            && node_start > prev
        {
            rendered.push('…');
            gaps.push(ByteGap {
                byte_start: prev,
                byte_end: node_start,
            });
        }
        let piece = render_node(node, node_end - node_start).ok_or_else(|| {
            anyhow::anyhow!(
                "markup-unreconstructable: node {} ({}) has no renderable content",
                node_ref.pointer,
                node_ref.inline_kind
            )
        })?;
        match piece {
            Rendered::Verbatim(text) => rendered.push_str(&text),
            Rendered::Approximate(text) => {
                rendered.push_str(&text);
                approximate.push(node_ref.pointer.clone());
            }
        }
        byte_start = byte_start.min(node_start);
        byte_end = byte_end.max(node_end);
        prev_byte_end = Some(node_end);
    }
    approximate.sort();
    approximate.dedup();
    Ok((
        AozoraMarkup {
            text: rendered,
            byte_start,
            byte_end,
            approximate_pointers: approximate,
            gaps,
        },
        contributing,
    ))
}

/// The contributing spans' node identities for `[char_start, char_end)`,
/// deduped by pointer, in document order — computable even when markup
/// rendering fails (spec §Layer 4: node context survives markup omission).
/// Spans are sorted by projected offset already (projection contract).
fn contributing_node_refs(
    spans: &[ab_plaintext::ProjectionSpan],
    char_start: u64,
    char_end: u64,
) -> Vec<AatNodeRef> {
    let mut pointers_seen = std::collections::BTreeSet::new();
    let mut refs = Vec::new();
    for span in spans {
        if span.projected_char_end <= char_start || span.projected_char_start >= char_end {
            continue;
        }
        if pointers_seen.insert(span.aat_pointer.clone()) {
            refs.push(AatNodeRef {
                pointer: span.aat_pointer.clone(),
                inline_kind: span.inline_kind.clone(),
                is_ruby_base: span.is_ruby_base,
                is_gaiji: span.is_gaiji,
            });
        }
    }
    refs
}

enum Rendered {
    Verbatim(String),
    Approximate(String),
}

/// Resolves an RFC 6901 pointer; when the pointed node lacks a `span` field
/// (e.g. the inner text of a `style` node), walks up truncated pointer
/// prefixes to the nearest spanned ancestor (spec §Layer 3 step 1).
fn resolve_spanned_node<'a>(
    aat: &'a serde_json::Value,
    pointer: &str,
) -> Option<&'a serde_json::Value> {
    let mut path = pointer.to_owned();
    loop {
        if let Some(node) = aat.pointer(&path)
            && node_byte_span(node).is_some()
        {
            return Some(node);
        }
        match path.rfind('/') {
            None | Some(0) => return None,
            Some(idx) => path.truncate(idx),
        }
    }
}

fn node_byte_span(node: &serde_json::Value) -> Option<(u64, u64)> {
    let span = node.get("span")?;
    Some((
        span.get("byte_start")?.as_u64()?,
        span.get("byte_end")?.as_u64()?,
    ))
}

/// The Aozora marker form of a gaiji node — `※［＃description］` when a
/// description is present, else the resolved character. Both callers use
/// this so gaiji renders identically at top level and nested inside
/// style/tcy; byte-length verification (top level only) decides
/// verbatim-vs-approximate.
fn gaiji_markup(node: &serde_json::Value) -> Option<String> {
    if let Some(description) = node
        .get("description")
        .and_then(serde_json::Value::as_str)
        .filter(|description| !description.is_empty())
    {
        return Some(format!("※［＃{description}］"));
    }
    node.get("resolved")
        .and_then(serde_json::Value::as_str)
        .filter(|resolved| !resolved.is_empty())
        .map(str::to_owned)
}

/// Extracts text content from a node, handling nested typed children
/// (e.g. ruby, gaiji) when recursing into style/tcy. Returns None if the
/// node has no renderable text.
fn inline_child_text(node: &serde_json::Value) -> Option<String> {
    match node.get("kind")?.as_str()? {
        "text" => node.get("value")?.as_str().map(str::to_owned),
        "ruby" => {
            let base = node.get("base")?.as_str()?;
            let reading = node.get("reading")?.as_str()?;
            Some(format!("{base}《{reading}》"))
        }
        "gaiji" => gaiji_markup(node),
        "style" | "tcy" => {
            let inner: String = node
                .get("content")?
                .as_array()?
                .iter()
                .filter_map(inline_child_text)
                .collect();
            if inner.is_empty() { None } else { Some(inner) }
        }
        _ => None,
    }
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
            let rendered = gaiji_markup(node)?;
            // Same byte-length rule as ruby: a marker that tiles its span
            // exactly is verbatim sanitized-source markup; anything else
            // (elided code suffix, resolved-character fallback) is a
            // semantic approximation.
            if rendered.len() as u64 == span_len {
                Some(Rendered::Verbatim(rendered))
            } else {
                Some(Rendered::Approximate(rendered))
            }
        }
        // style/tcy: render inner text content, including nested typed nodes
        // (ruby, gaiji, etc.), always approximate (the surrounding marker form
        // is not recoverable byte-exactly).
        "style" | "tcy" => {
            let inner: String = node
                .get("content")?
                .as_array()?
                .iter()
                .filter_map(inline_child_text)
                .collect();
            if inner.is_empty() {
                None
            } else {
                Some(Rendered::Approximate(inner))
            }
        }
        _ => None,
    }
}

#[cfg(test)]
pub(crate) mod tests {
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

    use serde_json::json;

    /// Also used by `hydrate::tests::write_e2e_fixture`
    /// end-to-end orchestration fixture); `pub(crate)` for that cross-module
    /// `#[cfg(test)]` reuse.
    ///
    /// Sanitized-source layout the spans below describe (byte offsets):
    ///   0..15  text  "このあいびきは"  — wait, keep it byte-countable:
    /// Use ASCII-measurable pieces: "AB" (2b) + "｜仏蘭西《フランス》" (30b)
    /// + "CD" (2b) + "端物《はもの》" (21b) + gaiji marker (20b) + "EF" (2b).
    pub(crate) fn typed_aat_fixture() -> serde_json::Value {
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
        assert!(markup.gaps.is_empty());
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
        assert_eq!(
            markup.approximate_pointers,
            vec!["/blocks/0/content/4".to_owned()]
        );
        assert!(markup.gaps.is_empty());
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
            projected_char_start: 0,
            projected_char_end: 1,
            aat_pointer: "/blocks/0/content/0".to_owned(),
            inline_kind: "raw".to_owned(),
            is_ruby_base: false,
            is_gaiji: false,
            is_note: false,
        };
        let err = reconstruct_markup(&aat, std::slice::from_ref(&span), 0, 1).unwrap_err();
        assert!(err.to_string().contains("markup-unreconstructable"));

        // The AAT node layer is built purely from projection spans (spec
        // §Layer 4), so it still yields the raw node's ref even though
        // markup reconstruction failed on it above.
        let nodes = contributing_node_refs(std::slice::from_ref(&span), 0, 1);
        assert_eq!(nodes.len(), 1);
        assert_eq!(nodes[0].pointer, "/blocks/0/content/0");
        assert_eq!(nodes[0].inline_kind, "raw");
    }

    #[test]
    fn markup_reconstruction_renders_style_node_with_nested_ruby() {
        // AAT with a style (bouten) node whose content contains a ruby child.
        // Sanitized-source layout: "▲仏蘭西▲" (the ▲ are bouten markers, bytes
        // 0-3, 33-36) with ruby "仏蘭西《フランス》" inside (bytes 3-33).
        let aat = json!({
            "version": 1, "work_id": "src-style-ruby",
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "style", "class": "bouten", "content": [
                        {"kind": "ruby", "base": "仏蘭西", "reading": "フランス", "direction": "right"}
                    ],
                     "span": {"byte_start": 0, "byte_end": 36, "line_start": 1, "line_end": 1}}
                ]
            }],
            "meta": {"adapter": "aozora", "adapter_version": "fixture",
                     "source_encoding": "windows-31j", "parse_complete": true,
                     "source_hash": "sha256:00", "warnings": []}
        });
        // Build a span pointing at the style node. The style node renders the
        // ruby base+reading without byte-exact recovery (no marker form).
        let span = ab_plaintext::ProjectionSpan {
            projected_char_start: 0,
            projected_char_end: 3, // "仏蘭西" is 3 chars
            aat_pointer: "/blocks/0/content/0".to_owned(),
            inline_kind: "style".to_owned(),
            is_ruby_base: false,
            is_gaiji: false,
            is_note: false,
        };
        let (markup, nodes) = reconstruct_markup(&aat, &[span], 0, 3).unwrap();
        // The rendered text should include the ruby base and reading.
        assert_eq!(markup.text, "仏蘭西《フランス》");
        // The style node should be flagged as approximate (marker not recovered).
        assert_eq!(
            markup.approximate_pointers,
            vec!["/blocks/0/content/0".to_owned()]
        );
        assert!(markup.gaps.is_empty());
        assert_eq!(nodes.len(), 1);
        assert_eq!(nodes[0].pointer, "/blocks/0/content/0");
        assert_eq!(nodes[0].inline_kind, "style");
    }

    #[test]
    fn source_context_load_rejects_char_count_mismatch() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("src-a.json");
        std::fs::write(&path, serde_json::to_vec(&typed_aat_fixture()).unwrap()).unwrap();
        // Projected text is 12 chars; claim 99 ⇒ projection-mismatch.
        let err = SourceContext::load(path.to_str().unwrap(), 99).unwrap_err();
        assert_eq!(err.code, "projection-mismatch");
        let err =
            SourceContext::load(dir.path().join("absent.json").to_str().unwrap(), 12).unwrap_err();
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
        assert_eq!(
            layers.aozora_markup.as_ref().unwrap().text,
            "｜仏蘭西《フランス》"
        );

        // Out-of-range region: snippet errors, markup errors, both recorded.
        // The AAT node layer survives (spec §Layer 4: node context is
        // computable even when markup rendering fails), clamped to the
        // in-bounds portion of the requested range.
        let layers = ctx.hydrate_region(0, 999, 2);
        assert!(layers.snippet.is_none());
        assert!(layers.aozora_markup.is_none());
        assert!(!layers.errors.is_empty());
        assert!(!layers.aat_nodes.is_empty());

        // Every error uses the closed vocabulary — the snippet failure is a
        // projection disagreement, not its own ad-hoc code.
        for error in &layers.errors {
            let code = error.split(':').next().unwrap();
            assert!(
                ["projection-mismatch", "markup-unreconstructable"].contains(&code),
                "unexpected error code in {error:?}"
            );
        }
    }

    #[test]
    fn gaiji_renders_marker_form_at_every_nesting_level_and_byte_verifies() {
        // Top level, span length exactly equal to the rendered marker
        // (`※［＃小書き片仮名ン］` = 4 marker chars + 7 description chars,
        // all 3-byte UTF-8 = 33 bytes) ⇒ Verbatim, not approximate.
        let aat = json!({
            "version": 1, "work_id": "src-gaiji",
            "blocks": [{"kind": "paragraph", "content": [
                {"kind": "gaiji", "description": "小書き片仮名ン", "resolved": "ン",
                 "span": {"byte_start": 0, "byte_end": 33, "line_start": 1, "line_end": 1}}
            ]}],
            "meta": {"adapter": "aozora", "adapter_version": "fixture",
                     "source_encoding": "windows-31j", "parse_complete": true,
                     "source_hash": "sha256:00", "warnings": []}
        });
        let (text, spans) = ab_plaintext::visible_text_projection_with_spans(&aat);
        assert_eq!(text, "ン");
        let (markup, _) = reconstruct_markup(&aat, &spans, 0, 1).unwrap();
        assert_eq!(markup.text, "※［＃小書き片仮名ン］");
        assert!(
            markup.approximate_pointers.is_empty(),
            "byte-exact gaiji marker must be verbatim"
        );
        assert!(markup.gaps.is_empty());

        // Nested inside a style node: same marker form (previously the
        // nested arm preferred `resolved`, diverging from the top level).
        let aat = json!({
            "version": 1, "work_id": "src-style-gaiji",
            "blocks": [{"kind": "paragraph", "content": [
                {"kind": "style", "class": "bouten", "content": [
                    {"kind": "gaiji", "description": "小書き片仮名ン", "resolved": "ン"}
                ],
                 "span": {"byte_start": 0, "byte_end": 39, "line_start": 1, "line_end": 1}}
            ]}],
            "meta": {"adapter": "aozora", "adapter_version": "fixture",
                     "source_encoding": "windows-31j", "parse_complete": true,
                     "source_hash": "sha256:00", "warnings": []}
        });
        let span = ab_plaintext::ProjectionSpan {
            projected_char_start: 0,
            projected_char_end: 1,
            aat_pointer: "/blocks/0/content/0".to_owned(),
            inline_kind: "style".to_owned(),
            is_ruby_base: false,
            is_gaiji: false,
            is_note: false,
        };
        let (markup, _) = reconstruct_markup(&aat, &[span], 0, 1).unwrap();
        assert_eq!(markup.text, "※［＃小書き片仮名ン］");
        assert!(markup.gaps.is_empty());
    }

    #[test]
    fn gaiji_without_description_falls_back_to_resolved_as_approximate() {
        let aat = json!({
            "version": 1, "work_id": "src-gaiji-resolved",
            "blocks": [{"kind": "paragraph", "content": [
                {"kind": "gaiji", "description": "", "resolved": "ン",
                 "span": {"byte_start": 0, "byte_end": 20, "line_start": 1, "line_end": 1}}
            ]}],
            "meta": {"adapter": "aozora", "adapter_version": "fixture",
                     "source_encoding": "windows-31j", "parse_complete": true,
                     "source_hash": "sha256:00", "warnings": []}
        });
        let (_, spans) = ab_plaintext::visible_text_projection_with_spans(&aat);
        let (markup, _) = reconstruct_markup(&aat, &spans, 0, 1).unwrap();
        assert_eq!(markup.text, "ン");
        assert_eq!(
            markup.approximate_pointers,
            vec!["/blocks/0/content/0".to_owned()]
        );
        assert!(markup.gaps.is_empty());
    }

    #[test]
    fn markup_gap_is_recorded_as_byte_range_not_pointer() {
        // Two text nodes with a 10-byte hole (a non-projecting marker)
        // between them.
        let aat = json!({
            "version": 1, "work_id": "src-gap",
            "blocks": [{"kind": "paragraph", "content": [
                {"kind": "text", "value": "AB",
                 "span": {"byte_start": 0, "byte_end": 2, "line_start": 1, "line_end": 1}},
                {"kind": "text", "value": "CD",
                 "span": {"byte_start": 12, "byte_end": 14, "line_start": 1, "line_end": 1}}
            ]}],
            "meta": {"adapter": "aozora", "adapter_version": "fixture",
                     "source_encoding": "windows-31j", "parse_complete": true,
                     "source_hash": "sha256:00", "warnings": []}
        });
        let (text, spans) = ab_plaintext::visible_text_projection_with_spans(&aat);
        assert_eq!(text, "ABCD");
        let (markup, _) = reconstruct_markup(&aat, &spans, 0, 4).unwrap();
        assert_eq!(markup.text, "AB…CD");
        assert_eq!(
            markup.gaps,
            vec![ByteGap {
                byte_start: 2,
                byte_end: 12
            }]
        );
        // The node after the gap rendered verbatim — it must NOT be listed
        // as approximate just because a gap precedes it.
        assert!(markup.approximate_pointers.is_empty());
    }
}
