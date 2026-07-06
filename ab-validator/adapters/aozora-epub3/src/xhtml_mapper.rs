//! AozoraEpub3-JDK21 XHTML → AAT mapping.
//!
//! Consumes rendered body XHTML produced by the upstream `AozoraEpub3.jar` and
//! projects it into AAT `blocks`. The DOM is parsed with `roxmltree` after
//! stripping the `<!DOCTYPE>` block (roxmltree rejects DTDs). Source-derived
//! recovery runs as a separate pass (see `source_derived`).

use anyhow::{Context, Result};
use roxmltree::{Document, Node};
use serde_json::{Value, json};

use crate::decode::decode_source_bytes;
use crate::model::{MappingInput, XhtmlDocumentKind};

/// Top-level entry: map a `MappingInput` (source bytes + body/colophon XHTML
/// documents + parser-failed flag) to an AAT envelope (`serde_json::Value`).
pub fn map_to_aat(input: &MappingInput) -> Result<Value> {
    let decoded = decode_source_bytes(&input.source_bytes)?;

    if input.parser_failed {
        let message = input.parser_error_message.clone().unwrap_or_default();
        return Ok(crate::model::parse_failure_envelope(
            decoded.encoding,
            &decoded.source_hash,
            if message.is_empty() {
                "AozoraEpub3 parser aborted".to_string()
            } else {
                format!("AozoraEpub3 parser aborted: {message}")
            },
        ));
    }

    let mut warnings: Vec<Value> = Vec::new();
    let mut blocks: Vec<Value> = Vec::new();

    for doc in input
        .xhtml_documents
        .iter()
        .filter(|d| d.kind == XhtmlDocumentKind::BodySection)
    {
        let mut doc_blocks = map_blocks_from_xhtml(&doc.bytes, &mut warnings)?;
        blocks.append(&mut doc_blocks);
    }

    crate::source_derived::apply_source_derived_recovery(&mut blocks, &decoded.text);

    Ok(json!({
        "version": 1,
        "work_id": "stdin",
        "blocks": blocks,
        "meta": {
            "adapter": crate::model::ADAPTER_NAME,
            "adapter_version": crate::model::ADAPTER_VERSION,
            "source_encoding": decoded.encoding,
            "source_hash": decoded.source_hash,
            "parse_complete": true,
            "warnings": warnings,
        },
    }))
}

/// `--mode html`: concatenate body and (appended) colophon XHTML sections with
/// a section-boundary comment for debugging.
pub fn map_to_html(input: &MappingInput) -> Result<String> {
    let mut out = String::new();
    let mut idx = 0;
    let mut push = |bytes: &[u8]| -> Result<()> {
        if idx > 0 {
            out.push_str("\n<!-- aozora-epub3 section boundary -->\n");
        }
        out.push_str(std::str::from_utf8(bytes).context("non-utf8 xhtml in html mode")?);
        idx += 1;
        Ok(())
    };
    for doc in &input.xhtml_documents {
        if doc.kind == XhtmlDocumentKind::BodySection {
            push(&doc.bytes)?;
        }
    }
    for doc in &input.xhtml_documents {
        if doc.kind == XhtmlDocumentKind::Colophon {
            push(&doc.bytes)?;
        }
    }
    Ok(out)
}

// ---------------------------------------------------------------------------
// XHTML plumbing
// ---------------------------------------------------------------------------

/// roxmltree rejects `<!DOCTYPE>`; strip it (matches the aozora2html adapter).
fn strip_dtd_block(xml: &str) -> String {
    let Some(start) = xml.find("<!DOCTYPE") else {
        return xml.to_string();
    };
    let tail = &xml[start..];
    let end = tail
        .find("]>")
        .map(|i| start + i + 2)
        .or_else(|| tail.find('>').map(|i| start + i + 1));
    match end {
        Some(end) => {
            let mut out = String::with_capacity(xml.len() - (end - start));
            out.push_str(&xml[..start]);
            out.push_str(&xml[end..]);
            out
        }
        None => xml.to_string(),
    }
}

fn node_name<'a>(node: Node<'a, 'a>) -> &'a str {
    node.tag_name().name()
}

fn node_class<'a>(node: Node<'a, 'a>) -> &'a str {
    node.attribute("class").unwrap_or("")
}

fn class_with_prefix<'a>(node: Node<'a, 'a>, prefix: &str) -> Option<&'a str> {
    node_class(node)
        .split_whitespace()
        .find(|c| c.starts_with(prefix))
}

fn text_only<'a>(node: Node<'a, 'a>) -> String {
    let mut out = String::new();
    for d in node.descendants() {
        if d.is_text()
            && let Some(t) = d.text()
        {
            out.push_str(t);
        }
    }
    out
}

fn paragraph_has_content(nodes: &[Value]) -> bool {
    nodes.iter().any(|n| {
        if n.get("kind").and_then(Value::as_str) == Some("text") {
            !n.get("value")
                .and_then(Value::as_str)
                .unwrap_or_default()
                .trim()
                .is_empty()
        } else {
            true
        }
    })
}

// ---------------------------------------------------------------------------
// Inline mapping
// ---------------------------------------------------------------------------

/// Walk the inline children of `node`, emitting AAT inline nodes. Captures both
/// direct text and nested element inlines.
fn walk_inline_children<'a>(node: Node<'a, 'a>) -> Vec<Value> {
    let mut out = Vec::new();
    if let Some(text) = node.text() {
        let v = text.to_string();
        if !v.is_empty() {
            out.push(json!({"kind": "text", "value": v}));
        }
    }
    for child in node.children().filter(|c| c.is_element()) {
        out.extend(map_inline(child));
        if let Some(tail) = child.tail()
            && !tail.is_empty()
        {
            out.push(json!({"kind": "text", "value": tail}));
        }
    }
    out
}

fn map_inline<'a>(node: Node<'a, 'a>) -> Vec<Value> {
    if node.is_text() {
        let v = node.text().map(str::to_string).unwrap_or_default();
        if v.is_empty() {
            return Vec::new();
        }
        return vec![json!({"kind": "text", "value": v})];
    }
    if !node.is_element() {
        return Vec::new();
    }
    match node_name(node) {
        "br" => Vec::new(),
        "ruby" => vec![map_ruby(node)],
        "img" => vec![map_img(node)],
        "span" => map_span(node),
        "a" | "rb" => walk_inline_children(node),
        // Unknown inline element: recurse transparently.
        _ => walk_inline_children(node),
    }
}

fn map_ruby<'a>(node: Node<'a, 'a>) -> Value {
    let mut reading = String::new();
    let mut direction = "right";
    let mut rt_node_count = 0;
    for child in node.children().filter(|c| c.is_element()) {
        if node_name(child) == "rt" {
            rt_node_count += 1;
            if reading.is_empty() {
                reading = text_only(child);
                if node_class(child).split_whitespace().any(|t| t == "left") {
                    direction = "left";
                }
            }
        }
    }
    if rt_node_count == 0 {
        // Not a real ruby; treat as text.
        return json!({"kind": "text", "value": text_only(node)});
    }

    let mut base = text_only(node);
    if base.ends_with(&reading) && !reading.is_empty() {
        base.truncate(base.len() - reading.len());
    }
    base = base.trim().to_string();

    let mut out = json!({
        "kind": "ruby",
        "base": base,
        "reading": reading,
        "direction": direction,
    });
    if let Some(obj) = out.as_object_mut() {
        obj.insert(
            "base_content".to_string(),
            Value::Array(walk_inline_children_but_ruby(node)),
        );
    }
    out
}

fn walk_inline_children_but_ruby<'a>(node: Node<'a, 'a>) -> Vec<Value> {
    // base_content excludes the <rt> text.
    let mut out = Vec::new();
    if let Some(text) = node.text()
        && !text.is_empty()
    {
        out.push(json!({"kind": "text", "value": text}));
    }
    for child in node.children().filter(|c| c.is_element()) {
        if node_name(child) == "rt" {
            continue;
        }
        out.extend(map_inline(child));
        if let Some(tail) = child.tail()
            && !tail.is_empty()
        {
            out.push(json!({"kind": "text", "value": tail}));
        }
    }
    out
}

fn map_img<'a>(node: Node<'a, 'a>) -> Value {
    let src = node.attribute("src").unwrap_or("");
    let alt = node.attribute("alt").unwrap_or("");
    if src.contains("gaiji") {
        return json!({
            "kind": "gaiji",
            "description": if alt.is_empty() { "unknown" } else { alt },
            "resolved": Value::Null,
            "jis_code": Value::Null,
            "unresolved_reason": "image_fallback",
            "x-provenance": "parser",
        });
    }
    let filename = src
        .rsplit('/')
        .next()
        .filter(|s| !s.is_empty())
        .unwrap_or(src);
    let width = node.attribute("width").and_then(|s| s.parse::<i64>().ok());
    let height = node.attribute("height").and_then(|s| s.parse::<i64>().ok());
    let mut figure = json!({
        "kind": "figure",
        "filename": filename,
        "alt": alt,
        "css_class": node_class(node),
        "caption": Value::Null,
    });
    if let Some(obj) = figure.as_object_mut() {
        if let Some(w) = width {
            obj.insert("width".to_string(), Value::from(w));
        }
        if let Some(h) = height {
            obj.insert("height".to_string(), Value::from(h));
        }
    }
    figure
}

fn map_span<'a>(node: Node<'a, 'a>) -> Vec<Value> {
    let content = walk_inline_children(node);
    let cls = node_class(node);
    let tokens: Vec<&str> = cls.split_whitespace().collect();

    if tokens.contains(&"b") {
        return vec![style_node("bold", content)];
    }
    if tokens.contains(&"i") {
        return vec![style_node("italic", content)];
    }
    if let Some(boten) = tokens.iter().find(|t| {
        matches!(
            **t,
            "sesame"
                | "dot"
                | "open_sesame"
                | "open_dot"
                | "double_open_sesame"
                | "double_open_dot"
        )
    }) {
        let mut node = style_node("boten", content);
        if let Some(obj) = node.as_object_mut() {
            obj.insert(
                "x-boten-kind".to_string(),
                Value::String((*boten).to_string()),
            );
        }
        return vec![node];
    }
    if let Some(line) = tokens
        .iter()
        .find(|t| matches!(**t, "underline" | "double_underline"))
    {
        let kind = if *line == "double_underline" {
            "double"
        } else {
            "single"
        };
        let mut node = style_node("bousen", content);
        if let Some(obj) = node.as_object_mut() {
            obj.insert("x-line-kind".to_string(), Value::String(kind.to_string()));
        }
        return vec![node];
    }
    if tokens.contains(&"tcy") {
        return vec![json!({"kind": "tcy", "content": content})];
    }
    if tokens.contains(&"wrc") {
        // AozoraEpub3 `wrc` is the warichu wrapper span. Schema `warigaki`
        // requires upper/lower which cannot be reconstructed reliably from
        // rendered split spans here; emit as a style marker so the round-trip
        // is schema-valid. Source-derived warichu recovery is a no-op (see
        // source_derived.rs) for the same reason.
        return vec![style_node("warichu", content)];
    }
    content
}

fn style_node(style_type: &str, content: Vec<Value>) -> Value {
    json!({"kind": "style", "style_type": style_type, "content": content})
}

// ---------------------------------------------------------------------------
// Block mapping
// ---------------------------------------------------------------------------

fn map_blocks_from_xhtml(bytes: &[u8], warnings: &mut Vec<Value>) -> Result<Vec<Value>> {
    let decoded = decode_source_bytes(bytes)?;
    let stripped = strip_dtd_block(&decoded.text);
    let normalized = stripped.replace("&nbsp;", " ");
    let doc = match Document::parse(&normalized) {
        Ok(d) => d,
        Err(err) => {
            warnings.push(json!({"message": format!("xhtml parse failed: {err}")}));
            return Err(anyhow::anyhow!("xhtml parse failed: {err}"));
        }
    };
    let Some(body) = doc
        .root_element()
        .descendants()
        .find(|n| n.is_element() && n.tag_name().name() == "body")
    else {
        warnings.push(json!({"message": "no <body> element in XHTML"}));
        return Ok(Vec::new());
    };
    Ok(map_body_children(body))
}

fn map_body_children<'a>(body: Node<'a, 'a>) -> Vec<Value> {
    let mut blocks = Vec::new();
    // Direct text/trailing text under <body> is uncommon; wrap stray inlines.
    let mut pending: Vec<Value> = Vec::new();
    let flush = |blocks: &mut Vec<Value>, pending: &mut Vec<Value>| {
        if paragraph_has_content(pending) {
            blocks.push(json!({"kind": "paragraph", "content": pending.clone()}));
        }
        pending.clear();
    };
    if let Some(text) = body.text()
        && !text.trim().is_empty()
    {
        pending.push(json!({"kind": "text", "value": text}));
    }
    for child in body.children().filter(|c| c.is_element()) {
        let produced = map_block(child);
        if produced.is_empty() {
            continue;
        }
        // `map_block` returns fully-formed block(s); flush any pending inline
        // text first so order is preserved.
        flush(&mut blocks, &mut pending);
        blocks.extend(produced);
        if let Some(tail) = child.tail()
            && !tail.trim().is_empty()
        {
            pending.push(json!({"kind": "text", "value": tail}));
        }
    }
    flush(&mut blocks, &mut pending);
    blocks
}

fn map_block<'a>(node: Node<'a, 'a>) -> Vec<Value> {
    let name = node_name(node);

    if name == "p" {
        if let Some(heading) = try_map_inline_heading(node) {
            return vec![heading];
        }
        let content = walk_inline_children(node);
        if !paragraph_has_content(&content) {
            return Vec::new();
        }
        return vec![json!({"kind": "paragraph", "content": content})];
    }

    if matches!(name, "h1" | "h2" | "h3") {
        let level: i64 = name[1..].parse().unwrap_or(1);
        return vec![json!({
            "kind": "heading",
            "level": level,
            "style": if node_class(node).is_empty() { "normal" } else { node_class(node) },
            "content": walk_inline_children(node),
        })];
    }

    if name == "div" {
        let cls = node_class(node);
        let tokens: Vec<&str> = cls.split_whitespace().collect();

        if tokens.contains(&"chap1") {
            return vec![heading(1, "normal", node)];
        }
        if tokens.contains(&"chap2") {
            return vec![heading(2, "normal", node)];
        }
        if tokens.contains(&"chap3") {
            return vec![heading(3, "normal", node)];
        }
        if let Some(c) = class_with_prefix(node, "pt")
            && let Some(indent) = c.strip_prefix("pt").and_then(|s| s.parse::<i64>().ok())
        {
            return vec![json!({
                "kind": "jisage_block",
                "children": map_body_children(node),
                "x-indent": indent,
            })];
        }
        if tokens.contains(&"border") {
            return vec![json!({
                "kind": "keigakomi_block",
                "children": map_body_children(node),
                "x-border-kind": "solid",
            })];
        }
        if tokens.contains(&"dashed_border") {
            return vec![json!({
                "kind": "keigakomi_block",
                "children": map_body_children(node),
                "x-border-kind": "dashed",
            })];
        }
        if tokens.contains(&"yoko") {
            return vec![json!({
                "kind": "yokogumi_block",
                "children": map_body_children(node),
            })];
        }

        // Unknown div: if it has block children, recurse as a transparent
        // container; otherwise emit a paragraph of its inline content so we
        // never drop text. Avoid emitting empty paragraphs.
        let inner = map_body_children(node);
        if !inner.is_empty() {
            return inner;
        }
        let inline = walk_inline_children(node);
        if paragraph_has_content(&inline) {
            return vec![json!({"kind": "paragraph", "content": inline})];
        }
        return Vec::new();
    }

    if name == "br" || name == "hr" {
        return Vec::new();
    }

    // Anything else at block scope: project as a paragraph of its inline
    // content (fallback to keep output schema-valid and non-empty where text
    // exists) or recurse if it nests block-level elements.
    let inner = map_body_children(node);
    if !inner.is_empty() {
        return inner;
    }
    let inline = walk_inline_children(node);
    if paragraph_has_content(&inline) {
        return vec![json!({"kind": "paragraph", "content": inline})];
    }
    Vec::new()
}

fn heading<'a>(level: i64, style: &str, node: Node<'a, 'a>) -> Value {
    json!({
        "kind": "heading",
        "level": level,
        "style": style,
        "content": walk_inline_children(node),
    })
}

/// A `<p>` whose only element child is a single `<span>` with class `font5`,
/// `font3`, or `font1` (and no other non-whitespace text) becomes a dogyo
/// heading. Mixed-content paragraphs keep the span as a normal style span.
fn try_map_inline_heading<'a>(p: Node<'a, 'a>) -> Option<Value> {
    let element_children: Vec<Node> = p.children().filter(|c| c.is_element()).collect();
    if element_children.len() != 1 {
        return None;
    }
    let span = element_children[0];
    if node_name(span) != "span" {
        return None;
    }
    let cls = node_class(span);
    let level = if cls.split_whitespace().any(|t| t == "font5") {
        1
    } else if cls.split_whitespace().any(|t| t == "font3") {
        2
    } else if cls.split_whitespace().any(|t| t == "font1") {
        3
    } else {
        return None;
    };
    // Ensure no stray non-whitespace text outside the span.
    let mut stray = String::new();
    if let Some(t) = p.text() {
        stray.push_str(t);
    }
    for c in p.children() {
        if let Some(tail) = c.tail() {
            stray.push_str(tail);
        }
    }
    if !stray.trim().is_empty() {
        return None;
    }
    Some(json!({
        "kind": "heading",
        "level": level,
        "style": "dogyo",
        "x-heading-kind": "dogyo",
        "content": walk_inline_children(span),
    }))
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{MappingInput, XhtmlDocument, XhtmlDocumentKind};

    fn input_from_fixture(name: &str) -> MappingInput {
        let path = format!("tests/fixtures/{name}");
        let bytes = std::fs::read(&path).unwrap_or_else(|e| panic!("read {path}: {e}"));
        MappingInput {
            source_bytes: b"test".to_vec(),
            xhtml_documents: vec![XhtmlDocument {
                bytes,
                kind: XhtmlDocumentKind::BodySection,
            }],
            parser_failed: false,
            parser_error_message: None,
        }
    }

    fn blocks(name: &str) -> Value {
        let input = input_from_fixture(name);
        let aat = map_to_aat(&input).unwrap();
        aat["blocks"].clone()
    }

    #[test]
    fn maps_paragraph() {
        assert_eq!(
            blocks("paragraph.xhtml"),
            json!([{"kind": "paragraph", "content": [{"kind": "text", "value": "一文字。"}]}])
        );
    }

    #[test]
    fn maps_heading_chap1() {
        assert_eq!(
            blocks("heading_chap1.xhtml"),
            json!([{"kind": "heading", "level": 1, "style": "normal", "content": [{"kind": "text", "value": "第一章"}]}])
        );
    }

    #[test]
    fn maps_heading_chap2() {
        assert_eq!(blocks("heading_chap2.xhtml")[0]["level"], json!(2));
    }

    #[test]
    fn maps_heading_chap3() {
        assert_eq!(blocks("heading_chap3.xhtml")[0]["level"], json!(3));
    }

    #[test]
    fn maps_jisage() {
        assert_eq!(
            blocks("jisage.xhtml"),
            json!([{
                "kind": "jisage_block",
                "children": [{"kind": "paragraph", "content": [{"kind": "text", "value": "二字下げ"}]}],
                "x-indent": 2
            }])
        );
    }

    #[test]
    fn maps_keigakomi() {
        assert_eq!(
            blocks("keigakomi.xhtml"),
            json!([{
                "kind": "keigakomi_block",
                "children": [{"kind": "paragraph", "content": [{"kind": "text", "value": "囲み"}]}],
                "x-border-kind": "solid"
            }])
        );
    }

    #[test]
    fn maps_keigakomi_dashed() {
        assert_eq!(
            blocks("keigakomi_dashed.xhtml")[0]["x-border-kind"],
            json!("dashed")
        );
    }

    #[test]
    fn maps_yoko() {
        assert_eq!(
            blocks("yoko.xhtml"),
            json!([{
                "kind": "yokogumi_block",
                "children": [{"kind": "paragraph", "content": [{"kind": "text", "value": "横組"}]}]
            }])
        );
    }

    #[test]
    fn maps_ruby_basic() {
        assert_eq!(
            blocks("ruby_basic.xhtml"),
            json!([{
                "kind": "paragraph",
                "content": [
                    {"kind": "ruby", "base": "吾輩", "reading": "わがはい", "direction": "right",
                     "base_content": [{"kind": "text", "value": "吾輩"}]},
                    {"kind": "text", "value": "は猫である。"}
                ]
            }])
        );
    }

    #[test]
    fn maps_bold_basic() {
        assert_eq!(
            blocks("bold_basic.xhtml"),
            json!([{
                "kind": "paragraph",
                "content": [
                    {"kind": "style", "style_type": "bold", "content": [{"kind": "text", "value": "太字"}]},
                    {"kind": "text", "value": "の例"}
                ]
            }])
        );
    }

    #[test]
    fn maps_italic_basic() {
        assert_eq!(
            blocks("italic_basic.xhtml"),
            json!([{
                "kind": "paragraph",
                "content": [
                    {"kind": "style", "style_type": "italic", "content": [{"kind": "text", "value": "斜体"}]}
                ]
            }])
        );
    }

    #[test]
    fn maps_boten_sesame() {
        assert_eq!(
            blocks("boten_sesame.xhtml"),
            json!([{
                "kind": "paragraph",
                "content": [{
                    "kind": "style", "style_type": "boten",
                    "content": [{"kind": "text", "value": "傍点"}],
                    "x-boten-kind": "sesame"
                }]
            }])
        );
    }

    #[test]
    fn maps_bousen_underline() {
        assert_eq!(
            blocks("bousen_underline.xhtml"),
            json!([{
                "kind": "paragraph",
                "content": [{
                    "kind": "style", "style_type": "bousen",
                    "content": [{"kind": "text", "value": "傍線"}],
                    "x-line-kind": "single"
                }]
            }])
        );
    }

    #[test]
    fn maps_tcy() {
        assert_eq!(
            blocks("tcy.xhtml"),
            json!([{
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "昭和"},
                    {"kind": "tcy", "content": [{"kind": "text", "value": "5"}]},
                    {"kind": "text", "value": "年"}
                ]
            }])
        );
    }

    #[test]
    fn maps_warigaki_as_warichu_style() {
        // AozoraEpub3 `wrc` span -> style marker (schema-valid; warigaki needs
        // upper/lower which rendered XHTML cannot reconstruct here).
        assert_eq!(
            blocks("warigaki.xhtml"),
            json!([{
                "kind": "paragraph",
                "content": [{
                    "kind": "style", "style_type": "warichu",
                    "content": [{"kind": "text", "value": "割注"}]
                }]
            }])
        );
    }

    #[test]
    fn maps_figure_img() {
        assert_eq!(
            blocks("figure_img.xhtml"),
            json!([{
                "kind": "paragraph",
                "content": [{
                    "kind": "figure", "filename": "ch01_001.png", "alt": "挿絵",
                    "css_class": "", "caption": Value::Null,
                    "width": 100, "height": 50
                }]
            }])
        );
    }

    #[test]
    fn maps_heading_dogyo() {
        assert_eq!(
            blocks("heading_dogyo.xhtml"),
            json!([{
                "kind": "heading", "level": 2, "style": "dogyo",
                "x-heading-kind": "dogyo",
                "content": [{"kind": "text", "value": "見出し"}]
            }])
        );
    }

    #[test]
    fn maps_compound_class_bold_takes_precedence() {
        assert_eq!(
            blocks("compound_class.xhtml"),
            json!([{
                "kind": "paragraph",
                "content": [{
                    "kind": "style", "style_type": "bold",
                    "content": [{"kind": "text", "value": "太字斜体"}]
                }]
            }])
        );
    }

    #[test]
    fn maps_gaiji_resolved_text_passes_through() {
        assert_eq!(
            blocks("gaiji_text.xhtml"),
            json!([{"kind": "paragraph", "content": [{"kind": "text", "value": "呭"}]}])
        );
    }

    #[test]
    fn blank_spacer_paragraphs_are_filtered() {
        assert_eq!(
            blocks("blank_spacer.xhtml"),
            json!([{"kind": "paragraph", "content": [{"kind": "text", "value": "残る"}]}])
        );
    }

    #[test]
    fn parser_failed_produces_incomplete_envelope() {
        let input = MappingInput {
            source_bytes: b"x".to_vec(),
            xhtml_documents: vec![],
            parser_failed: true,
            parser_error_message: Some("boom".to_string()),
        };
        let aat = map_to_aat(&input).unwrap();
        assert_eq!(aat["meta"]["parse_complete"], json!(false));
        assert_eq!(aat["meta"]["adapter"], json!("aozora-epub3"));
        assert!(aat["blocks"].as_array().unwrap().is_empty());
        assert!(
            aat["meta"]["warnings"][0]["message"]
                .as_str()
                .unwrap()
                .contains("boom")
        );
    }

    #[test]
    fn map_to_html_concatenates_sections() {
        let input = MappingInput {
            source_bytes: vec![],
            xhtml_documents: vec![
                XhtmlDocument {
                    bytes: b"<body>A</body>".to_vec(),
                    kind: XhtmlDocumentKind::BodySection,
                },
                XhtmlDocument {
                    bytes: b"<body>B</body>".to_vec(),
                    kind: XhtmlDocumentKind::Colophon,
                },
            ],
            parser_failed: false,
            parser_error_message: None,
        };
        let html = map_to_html(&input).unwrap();
        assert!(html.contains("<body>A</body>"));
        assert!(html.contains("section boundary"));
        assert!(html.contains("<body>B</body>"));
    }
}
