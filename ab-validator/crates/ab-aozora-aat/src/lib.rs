use std::{collections::BTreeMap, sync::LazyLock};

use anyhow::Result;
use aozora_pipeline::lexer::sanitize as sanitize_aozora_source;
use encoding_rs::SHIFT_JIS;
use regex::Regex;
use serde::Deserialize;
use serde_json::{Value, json};
use sha2::{Digest, Sha256};

static RUBY_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^｜?(?P<base>.+?)《(?P<reading>[^》]+)》$").unwrap());

#[derive(Debug)]
pub struct DecodedSource {
    pub text: String,
    pub span_text: String,
    pub encoding: &'static str,
    pub source_hash: String,
}

#[derive(Debug, Deserialize, Clone, Copy)]
struct Span {
    start: usize,
    end: usize,
}

#[derive(Debug, Deserialize, Clone)]
struct AozoraNode {
    kind: String,
    span: Span,
}

#[derive(Debug, Deserialize, Clone)]
struct AozoraDiagnostic {
    kind: Option<String>,
    severity: Option<String>,
    span: Option<Span>,
}

#[derive(Debug, Deserialize, Clone)]
struct AozoraGaiji {
    span: Span,
    description: String,
    #[serde(default)]
    mencode: Option<String>,
    #[serde(default)]
    codepoint: Option<Value>,
    #[serde(default)]
    resolved: Option<String>,
}

pub fn decode_source_bytes(bytes: &[u8]) -> Result<DecodedSource> {
    let source_hash = format!("sha256:{}", hex_sha256(bytes));
    if bytes.starts_with(&[0xef, 0xbb, 0xbf]) {
        let text = std::str::from_utf8(&bytes[3..])?.to_owned();
        let span_text = sanitize_for_aat(&text);
        return Ok(DecodedSource {
            text,
            span_text,
            encoding: "utf-8-bom",
            source_hash,
        });
    }
    if let Ok(text) = std::str::from_utf8(bytes) {
        let text = text.to_owned();
        let span_text = sanitize_for_aat(&text);
        return Ok(DecodedSource {
            text,
            span_text,
            encoding: "utf-8",
            source_hash,
        });
    }
    let (cow, _, had_errors) = SHIFT_JIS.decode(bytes);
    let text = cow.into_owned();
    let span_text = sanitize_for_aat(&text);
    Ok(DecodedSource {
        text,
        span_text,
        encoding: if had_errors {
            "windows-31j-lossy"
        } else {
            "windows-31j"
        },
        source_hash,
    })
}

fn sanitize_for_aat(text: &str) -> String {
    let sanitized = sanitize_aozora_source(text).text.into_owned();
    aozora_body_text(&sanitized).to_owned()
}

fn aozora_body_text(source: &str) -> &str {
    let mut separators = Vec::new();
    let mut start = 0_usize;
    for line in source.split_inclusive('\n') {
        let end = start + line.len();
        if is_aozora_separator(line) {
            separators.push((start, end));
        }
        start = end;
    }

    let mut body_start = 0_usize;
    if separators.len() >= 2 {
        let legend = &source[separators[0].1..separators[1].0];
        if legend.contains("テキスト中に現れる記号について") || legend.contains("《》：ルビ")
        {
            body_start = skip_blank_lines(source, separators[1].1);
        }
    }

    let mut body_end = source.len();
    for (line_start, line) in lines_from(source, body_start) {
        if line.trim_start().starts_with("底本：") {
            body_end = trim_trailing_blank_lines(source, line_start);
            break;
        }
    }

    &source[body_start..body_end]
}

fn is_aozora_separator(line: &str) -> bool {
    let trimmed = line.trim();
    trimmed.len() >= 10 && trimmed.chars().all(|ch| ch == '-')
}

fn skip_blank_lines(source: &str, mut offset: usize) -> usize {
    while let Some(line) = source[offset..].split_inclusive('\n').next() {
        if !line.trim().is_empty() {
            break;
        }
        offset += line.len();
        if offset >= source.len() {
            break;
        }
    }
    offset
}

fn trim_trailing_blank_lines(source: &str, offset: usize) -> usize {
    source[..offset].trim_end_matches(['\n', '\r']).len()
}

fn lines_from(source: &str, offset: usize) -> impl Iterator<Item = (usize, &str)> {
    let mut cursor = offset;
    source[offset..].split_inclusive('\n').map(move |line| {
        let start = cursor;
        cursor += line.len();
        (start, line)
    })
}

use ab_aozora_facade::{Document, json};

fn projections(
    span_text: &str,
) -> Result<(Vec<AozoraNode>, Vec<AozoraDiagnostic>, Vec<AozoraGaiji>)> {
    // Mirrors the upstream binary's own stdin handling: each `aozora
    // inspect` subprocess ran decode_auto over the bytes the adapter piped
    // in (already-valid UTF-8 passes through unchanged).
    let source = ab_aozora_facade::encoding::decode_auto(span_text.as_bytes())
        .map_err(|err| anyhow::anyhow!("decode_auto: {err:?}"))?;
    let doc = Document::new(source.clone());
    let tree = doc.parse();
    let nodes = from_entries(json::node_entries(&tree))?;
    let diagnostics = from_entries(json::diagnostic_entries(tree.diagnostics()))?;
    let gaiji = from_entries(json::gaiji_entries(&source))?;
    Ok((nodes, diagnostics, gaiji))
}

/// Same data path as the deleted wire hop: the facade's Serialize impls
/// (which produced the inspect JSON) feed the adapter's Deserialize types.
/// Deserialization is key-order-independent, so no preserve_order needed.
fn from_entries<S: serde::Serialize, T: serde::de::DeserializeOwned>(
    entries: Vec<S>,
) -> Result<Vec<T>> {
    Ok(serde_json::from_value(serde_json::to_value(entries)?)?)
}

// The wire envelope's schemaVersion check becomes a compile-time pin: the
// from_entries round-trip is only valid against the wire shape this port
// was written for.
const _: () = assert!(json::SCHEMA_VERSION == 2);

pub fn aat_json_from_bytes(bytes: &[u8]) -> Result<Vec<u8>> {
    let decoded = decode_source_bytes(bytes)?;
    let (nodes, diagnostics, gaiji) = projections(&decoded.span_text)?;
    let aat = build_aat(&decoded, &nodes, &diagnostics, &gaiji);
    let mut out = Vec::new();
    serde_json::to_writer(&mut out, &aat)?;
    out.push(b'\n');
    Ok(out)
}

fn build_aat(
    decoded: &DecodedSource,
    nodes: &[AozoraNode],
    diagnostics: &[AozoraDiagnostic],
    gaiji: &[AozoraGaiji],
) -> Value {
    let gaiji_by_start = gaiji
        .iter()
        .map(|entry| (entry.span.start, entry.clone()))
        .collect::<BTreeMap<_, _>>();
    let blocks = blocks_from_inline_content(inline_content(decoded, nodes, &gaiji_by_start));
    let mut warnings = diagnostics
        .iter()
        .map(diagnostic_warning)
        .collect::<Vec<_>>();
    if !nodes.is_empty() {
        warnings.push(json!({
            "message": "aozora upstream spans are sanitized-source byte offsets; line_start and line_end are synthesized as 1",
            "line": 1
        }));
    }
    json!({
        "version": 1,
        "work_id": "stdin",
        "blocks": blocks,
        "meta": {
            "adapter": "ab-aozora",
            "adapter_version": adapter_version(),
            "source_encoding": decoded.encoding,
            "source_hash": decoded.source_hash,
            "parse_complete": diagnostics.iter().all(|d| d.severity.as_deref() != Some("error")),
            "warnings": warnings
        }
    })
}

fn blocks_from_inline_content(content: Vec<Value>) -> Vec<Value> {
    let mut blocks = Vec::new();
    let mut paragraph = Vec::new();
    let mut strip_next_leading_newline = false;
    let mut index = 0;

    while index < content.len() {
        let mut node = content[index].clone();
        if strip_next_leading_newline {
            strip_leading_newline(&mut node);
            strip_next_leading_newline = false;
            if node.get("kind").and_then(Value::as_str) == Some("text")
                && node
                    .get("value")
                    .and_then(Value::as_str)
                    .unwrap_or("")
                    .is_empty()
            {
                index += 1;
                continue;
            }
        }

        if let Some(offset) = align_end_offset(&node) {
            let boundary = find_next_raw_boundary(&content, index + 1);
            if boundary > index + 1 {
                push_paragraph_if_not_empty(&mut blocks, std::mem::take(&mut paragraph));
                let inner = content[index + 1..boundary].to_vec();
                push_chitsuki_paragraph(&mut blocks, offset, inner);
                index = boundary;
                continue;
            }
        }

        if let Some((first, rest)) = burasage_container_indent(&node) {
            if let Some(close_index) = find_matching_jisage_close(&content, index + 1) {
                push_paragraph_if_not_empty(&mut blocks, std::mem::take(&mut paragraph));
                let mut inner = content[index + 1..close_index].to_vec();
                strip_boundary_newlines(&mut inner);
                push_burasage_paragraph(&mut blocks, first, rest, inner);
                strip_next_leading_newline = true;
                index = close_index + 1;
                continue;
            }
            let boundary = find_next_container_boundary(&content, index + 1);
            if boundary > index + 1 {
                push_paragraph_if_not_empty(&mut blocks, std::mem::take(&mut paragraph));
                let mut inner = content[index + 1..boundary].to_vec();
                strip_boundary_newlines(&mut inner);
                push_burasage_paragraph(&mut blocks, first, rest, inner);
                index = boundary;
                continue;
            }
        } else if let Some(indent) = jisage_container_indent(&node) {
            if let Some(close_index) = find_matching_jisage_close(&content, index + 1) {
                push_paragraph_if_not_empty(&mut blocks, std::mem::take(&mut paragraph));
                let mut inner = content[index + 1..close_index].to_vec();
                strip_boundary_newlines(&mut inner);
                blocks.push(json!({
                    "kind": "jisage_block",
                    "x-indent": indent,
                    "children": blocks_from_inline_content(inner)
                }));
                strip_next_leading_newline = true;
                index = close_index + 1;
                continue;
            }
            let boundary = find_next_container_boundary(&content, index + 1);
            if boundary > index + 1 {
                push_paragraph_if_not_empty(&mut blocks, std::mem::take(&mut paragraph));
                let mut inner = content[index + 1..boundary].to_vec();
                strip_boundary_newlines(&mut inner);
                blocks.push(json!({
                    "kind": "jisage_block",
                    "x-indent": indent,
                    "children": blocks_from_inline_content(inner)
                }));
                index = boundary;
                continue;
            }
        }

        if is_heading_hint_raw(&node)
            && let Some(heading) = heading_block_from_hint(&mut paragraph, &node)
        {
            push_paragraph_if_not_empty(&mut blocks, std::mem::take(&mut paragraph));
            blocks.push(heading);
            strip_next_leading_newline = true;
            index += 1;
            continue;
        }
        paragraph.push(node);
        index += 1;
    }

    push_paragraph_if_not_empty(&mut blocks, paragraph);
    if blocks.is_empty() {
        blocks.push(json!({"kind": "paragraph", "content": []}));
    }
    blocks
}

fn push_paragraph_if_not_empty(blocks: &mut Vec<Value>, content: Vec<Value>) {
    if content.is_empty() {
        return;
    }
    blocks.push(json!({
        "kind": "paragraph",
        "content": content
    }));
}

fn push_chitsuki_paragraph(blocks: &mut Vec<Value>, offset: u64, content: Vec<Value>) {
    blocks.push(json!({
        "kind": "paragraph",
        "content": [{
            "kind": "style",
            "style_type": "chitsuki",
            "content": content,
            "x-align": "right",
            "x-offset": offset,
            "x-provenance": "source-derived"
        }]
    }));
}

fn align_end_offset(node: &Value) -> Option<u64> {
    if node.get("kind").and_then(Value::as_str) != Some("raw")
        || node.get("x-source-marker-kind").and_then(Value::as_str) != Some("alignEnd")
    {
        return None;
    }
    let source = node.get("source").and_then(Value::as_str)?;
    if source.contains("地付き") {
        return Some(0);
    }
    parse_aozora_number_before(source, "字上げ")
}

fn push_burasage_paragraph(blocks: &mut Vec<Value>, first: u64, rest: u64, content: Vec<Value>) {
    blocks.push(json!({
        "kind": "paragraph",
        "content": [{
            "kind": "style",
            "style_type": "burasage",
            "content": content,
            "x-indent-first": first,
            "x-indent-rest": rest,
            "x-provenance": "source-derived"
        }]
    }));
}

fn burasage_container_indent(node: &Value) -> Option<(u64, u64)> {
    if node.get("kind").and_then(Value::as_str) != Some("raw")
        || node.get("x-source-marker-kind").and_then(Value::as_str) != Some("containerOpen")
    {
        return None;
    }
    let source = node.get("source").and_then(Value::as_str)?;
    burasage_open_indent(source)
}

fn burasage_open_indent(source: &str) -> Option<(u64, u64)> {
    let marker = source.trim();
    if !marker.starts_with("［＃ここから") || !marker.ends_with('］') {
        return None;
    }
    let (first_part, rest_part) = marker.split_once("折り返して")?;
    let rest = parse_aozora_number_before(rest_part, "字下げ")?;
    let first = if first_part.contains("天付き") {
        0
    } else {
        parse_aozora_number_before(first_part, "字下げ").unwrap_or(0)
    };
    Some((first, rest))
}

fn jisage_container_indent(node: &Value) -> Option<u64> {
    if node.get("kind").and_then(Value::as_str) != Some("raw")
        || node.get("x-source-marker-kind").and_then(Value::as_str) != Some("containerOpen")
    {
        return None;
    }
    let source = node.get("source").and_then(Value::as_str)?;
    simple_jisage_open_indent(source)
}

fn simple_jisage_open_indent(source: &str) -> Option<u64> {
    let marker = source.trim();
    if !marker.starts_with("［＃ここから") || !marker.ends_with('］') {
        return None;
    }
    if marker.contains('、') || marker.contains("改行") || marker.contains("折り返して") {
        return None;
    }
    let (_, after_indent) = marker.split_once("字下げ")?;
    if after_indent != "］" {
        return None;
    }
    Some(parse_aozora_number_before(marker, "字下げ").unwrap_or(1))
}

fn find_matching_jisage_close(content: &[Value], start: usize) -> Option<usize> {
    for (offset, node) in content[start..].iter().enumerate() {
        if is_container_open_raw(node) {
            return None;
        }
        if is_jisage_container_close(node) {
            return Some(start + offset);
        }
    }
    None
}

fn find_next_container_boundary(content: &[Value], start: usize) -> usize {
    content[start..]
        .iter()
        .position(is_container_marker_raw)
        .map(|offset| start + offset)
        .unwrap_or(content.len())
}

fn find_next_raw_boundary(content: &[Value], start: usize) -> usize {
    content[start..]
        .iter()
        .position(|node| node.get("kind").and_then(Value::as_str) == Some("raw"))
        .map(|offset| start + offset)
        .unwrap_or(content.len())
}

fn is_container_marker_raw(node: &Value) -> bool {
    is_container_open_raw(node)
        || node.get("kind").and_then(Value::as_str) == Some("raw")
            && node
                .get("x-source-marker-kind")
                .and_then(Value::as_str)
                .is_some_and(|kind| kind == "containerClose")
}

fn is_container_open_raw(node: &Value) -> bool {
    node.get("kind").and_then(Value::as_str) == Some("raw")
        && node.get("x-source-marker-kind").and_then(Value::as_str) == Some("containerOpen")
}

fn is_jisage_container_close(node: &Value) -> bool {
    node.get("kind").and_then(Value::as_str) == Some("raw")
        && node.get("x-source-marker-kind").and_then(Value::as_str) == Some("containerClose")
        && node
            .get("source")
            .and_then(Value::as_str)
            .is_some_and(|source| source.contains("字下げ"))
}

fn parse_aozora_number_before(source: &str, needle: &str) -> Option<u64> {
    let prefix = source.split_once(needle)?.0;
    let mut digits = String::new();
    for ch in prefix.chars().rev() {
        if let Some(digit) = aozora_digit(ch) {
            digits.insert(0, digit);
        } else if !digits.is_empty() {
            break;
        }
    }
    if !digits.is_empty() {
        return digits.parse().ok();
    }
    parse_kanji_number_before(prefix)
}

fn aozora_digit(ch: char) -> Option<char> {
    match ch {
        '0'..='9' => Some(ch),
        '０'..='９' => char::from_digit(ch as u32 - '０' as u32, 10),
        _ => None,
    }
}

fn parse_kanji_number_before(prefix: &str) -> Option<u64> {
    let mut run = prefix
        .chars()
        .rev()
        .take_while(|ch| kanji_digit_value(*ch).is_some() || *ch == '十')
        .collect::<Vec<_>>();
    if run.is_empty() {
        return None;
    }
    run.reverse();
    let run = run.into_iter().collect::<String>();
    if let Some((tens, ones)) = run.split_once('十') {
        let tens = if tens.is_empty() {
            1
        } else {
            tens.chars().next().and_then(kanji_digit_value)?
        };
        let ones = if ones.is_empty() {
            0
        } else {
            ones.chars().next().and_then(kanji_digit_value)?
        };
        Some(tens * 10 + ones)
    } else {
        run.chars().next().and_then(kanji_digit_value)
    }
}

fn kanji_digit_value(ch: char) -> Option<u64> {
    match ch {
        '一' => Some(1),
        '二' => Some(2),
        '三' => Some(3),
        '四' => Some(4),
        '五' => Some(5),
        '六' => Some(6),
        '七' => Some(7),
        '八' => Some(8),
        '九' => Some(9),
        _ => None,
    }
}

fn strip_boundary_newlines(nodes: &mut [Value]) {
    if let Some(first) = nodes.first_mut() {
        strip_leading_newline(first);
    }
    if let Some(last) = nodes.last_mut() {
        strip_trailing_newline(last);
    }
}

fn is_heading_hint_raw(node: &Value) -> bool {
    node.get("kind").and_then(Value::as_str) == Some("raw")
        && node.get("x-source-marker-kind").and_then(Value::as_str) == Some("headingHint")
}

fn heading_block_from_hint(paragraph: &mut Vec<Value>, node: &Value) -> Option<Value> {
    let source = node.get("source").and_then(Value::as_str)?;
    let target = marker_target(source)?;
    let level = heading_level(source);
    let style = heading_style(source);
    let text = paragraph.last()?;
    if text.get("kind").and_then(Value::as_str) != Some("text")
        || text.get("value").and_then(Value::as_str)? != target
    {
        return None;
    }
    let heading_text = paragraph.pop()?;
    let indent = paragraph.last().and_then(heading_indent_marker);
    if indent.is_some() {
        paragraph.pop();
    }
    let mut heading = json!({
        "kind": "heading",
        "level": level,
        "style": style,
        "content": [heading_text],
        "x-provenance": "source-derived",
    });
    if let Some(indent) = indent {
        heading["x-indent"] = json!(indent);
    }
    Some(heading)
}

fn heading_indent_marker(node: &Value) -> Option<u64> {
    if node.get("kind").and_then(Value::as_str) != Some("raw")
        || node.get("x-source-marker-kind").and_then(Value::as_str) != Some("indent")
    {
        return None;
    }
    let source = node.get("source").and_then(Value::as_str)?;
    if !source.contains("字下げ") {
        return None;
    }
    parse_aozora_number_before(source, "字下げ")
}

fn heading_level(source: &str) -> u64 {
    if source.contains('大') {
        1
    } else if source.contains('中') {
        2
    } else {
        3
    }
}

fn heading_style(source: &str) -> &'static str {
    if source.contains("同行") {
        "dogyo"
    } else if source.contains('窓') {
        "mado"
    } else {
        "normal"
    }
}

fn strip_leading_newline(node: &mut Value) {
    if node.get("kind").and_then(Value::as_str) != Some("text") {
        return;
    }
    let Some(value) = node.get("value").and_then(Value::as_str) else {
        return;
    };
    let stripped = value.strip_prefix('\n').unwrap_or(value).to_owned();
    if let Some(obj) = node.as_object_mut() {
        obj.insert("value".to_owned(), json!(stripped));
    }
}

fn strip_trailing_newline(node: &mut Value) {
    if node.get("kind").and_then(Value::as_str) != Some("text") {
        return;
    }
    let Some(value) = node.get("value").and_then(Value::as_str) else {
        return;
    };
    let stripped = value.strip_suffix('\n').unwrap_or(value).to_owned();
    if let Some(obj) = node.as_object_mut() {
        obj.insert("value".to_owned(), json!(stripped));
    }
}

fn inline_content(
    decoded: &DecodedSource,
    nodes: &[AozoraNode],
    gaiji_by_start: &BTreeMap<usize, AozoraGaiji>,
) -> Vec<Value> {
    let mut content = Vec::new();
    let mut ordered = nodes.iter().collect::<Vec<_>>();
    ordered.sort_by_key(|node| (node.span.start, node.span.end));

    let mut cursor = 0_usize;
    for node in ordered {
        if node.span.start > cursor {
            push_source_gap(&mut content, decoded, cursor, node.span.start);
        }
        match node.kind.as_str() {
            "ruby" => content.push(ruby_node(decoded, node)),
            "gaiji" => content.push(gaiji_node(decoded, node, gaiji_by_start)),
            "bouten" => content.push(style_node(decoded, node, "bouten")),
            "emphasis" => content.push(style_node(
                decoded,
                node,
                emphasis_style_type(decoded, node),
            )),
            "combineUpright" => content.push(tcy_node(decoded, node)),
            "kaeriten" => content.push(raw_node(decoded, node, "kaeriten")),
            "directive" if source_slice(&decoded.span_text, &node.span).contains("返り点") => {
                content.push(raw_node(decoded, node, "kaeriten"));
            }
            "pageBreak" => content.push(json!({
                "kind": "raw",
                "source": source_slice(&decoded.span_text, &node.span),
                "x-provenance": "parser-derived",
                "x-source-marker-kind": "pageBreak",
                "x-break-kind": "page",
                "span": span_json(&node.span)
            })),
            _ => content.push(raw_node(decoded, node, node.kind.as_str())),
        }
        cursor = cursor.max(node.span.end);
    }
    if cursor < decoded.span_text.len() {
        push_source_gap(&mut content, decoded, cursor, decoded.span_text.len());
    }
    if decoded.span_text.contains("［＃改ページ］")
        && !content
            .iter()
            .any(|node| node.get("x-break-kind").and_then(Value::as_str) == Some("page"))
    {
        content.push(json!({
            "kind": "raw",
            "source": "［＃改ページ］",
            "x-provenance": "source-derived",
            "x-source-marker-kind": "pageBreak",
            "x-break-kind": "page"
        }));
    }
    content
}

fn push_source_gap(content: &mut Vec<Value>, decoded: &DecodedSource, start: usize, end: usize) {
    let Some(source) = decoded.span_text.get(start..end) else {
        return;
    };
    if source.is_empty() || source == "｜" {
        return;
    }
    let span = Span { start, end };
    if contains_aozora_markup(source) {
        content.push(json!({
            "kind": "raw",
            "source": source,
            "x-provenance": "source-derived",
            "x-source-marker-kind": "unparsed-source-gap",
            "span": span_json(&span)
        }));
    } else {
        content.push(json!({
            "kind": "text",
            "value": source,
            "span": span_json(&span)
        }));
    }
}

fn contains_aozora_markup(source: &str) -> bool {
    source.contains('※')
        || source.contains("［＃")
        || source.contains("[#")
        || source.contains('《')
        || source.contains('》')
        || source.contains('〔')
        || source.contains('〕')
}

fn ruby_node(decoded: &DecodedSource, node: &AozoraNode) -> Value {
    let source = source_slice(&decoded.span_text, &node.span);
    if let Some(caps) = RUBY_RE.captures(source) {
        json!({
            "kind": "ruby",
            "base": caps.name("base").unwrap().as_str(),
            "reading": caps.name("reading").unwrap().as_str(),
            "direction": "right",
            "span": span_json(&node.span)
        })
    } else {
        raw_node(decoded, node, "ruby")
    }
}

fn gaiji_node(
    decoded: &DecodedSource,
    node: &AozoraNode,
    gaiji_by_start: &BTreeMap<usize, AozoraGaiji>,
) -> Value {
    let Some(gaiji) = gaiji_by_start.get(&node.span.start) else {
        return raw_node(decoded, node, "gaiji");
    };
    json!({
        "kind": "gaiji",
        "description": gaiji.description,
        "resolved": gaiji.resolved,
        "jis_code": gaiji.mencode,
        "unresolved_reason": if gaiji.resolved.is_some() { None::<String> } else { Some("unresolved".to_owned()) },
        "x-codepoint": gaiji.codepoint,
        "span": span_json(&node.span)
    })
}

fn style_node(decoded: &DecodedSource, node: &AozoraNode, style_type: &str) -> Value {
    let source = source_slice(&decoded.span_text, &node.span);
    let text = marker_target(source).unwrap_or(source);
    json!({
        "kind": "style",
        "style_type": style_type,
        "content": [{"kind": "text", "value": text}],
        "span": span_json(&node.span)
    })
}

fn tcy_node(decoded: &DecodedSource, node: &AozoraNode) -> Value {
    let source = source_slice(&decoded.span_text, &node.span);
    let text = marker_target(source).unwrap_or(source);
    json!({
        "kind": "tcy",
        "content": [{"kind": "text", "value": text}],
        "span": span_json(&node.span)
    })
}

fn emphasis_style_type(decoded: &DecodedSource, node: &AozoraNode) -> &'static str {
    let source = source_slice(&decoded.span_text, &node.span);
    if source.contains("太字") {
        "bold"
    } else {
        "emphasis"
    }
}

fn marker_target(source: &str) -> Option<&str> {
    let start = source.find("［＃「")? + "［＃「".len();
    let rest = &source[start..];
    let end = rest.find('」')?;
    Some(&rest[..end])
}

fn raw_node(decoded: &DecodedSource, node: &AozoraNode, marker_kind: &str) -> Value {
    json!({
        "kind": "raw",
        "source": source_slice(&decoded.span_text, &node.span),
        "x-provenance": "parser-derived",
        "x-source-marker-kind": marker_kind,
        "span": span_json(&node.span)
    })
}

fn diagnostic_warning(diagnostic: &AozoraDiagnostic) -> Value {
    let mut warning = json!({
        "message": diagnostic.kind.clone().unwrap_or_else(|| "aozora diagnostic".to_owned())
    });
    if let Some(line) = diagnostic.span.as_ref().map(|_| 1_u64) {
        warning["line"] = json!(line);
    }
    warning
}

fn span_json(span: &Span) -> Value {
    json!({
        "line_start": 1,
        "line_end": 1,
        "byte_start": span.start,
        "byte_end": span.end
    })
}

fn source_slice<'a>(source: &'a str, span: &Span) -> &'a str {
    source.get(span.start..span.end).unwrap_or("")
}

/// Identity fields per the executable-boundary contract: adapter id,
/// adapter version, AAT schema version, build identity. The git rev is
/// injected by build.rs from AB_AOZORA_GIT_REV and is part of the
/// registry's exact-match coordinate — a "git unknown" build must never
/// become gate evidence or a registry row.
pub fn adapter_version() -> String {
    format!(
        "ab-aozora {} aat-schema 1 facade {} wire-schema {} (git {})",
        env!("CARGO_PKG_VERSION"),
        ab_aozora_facade_version(),
        ab_aozora_facade::json::SCHEMA_VERSION,
        env!("AB_AOZORA_GIT_REV"),
    )
}

fn ab_aozora_facade_version() -> &'static str {
    // The facade crate version stands in for the deleted wire
    // schemaVersion check as the compatibility coordinate.
    "0.1.0"
}

fn hex_sha256(bytes: &[u8]) -> String {
    let digest = Sha256::digest(bytes);
    let mut out = String::with_capacity(digest.len() * 2);
    for byte in digest {
        use std::fmt::Write as _;
        let _ = write!(out, "{byte:02x}");
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(serde::Deserialize)]
    struct SourceDecodingVector {
        name: String,
        bytes: Vec<u8>,
        text: String,
        encoding: String,
        sha256: String,
    }

    #[test]
    fn source_decoding_contract() {
        let vectors: Vec<SourceDecodingVector> = serde_json::from_str(include_str!(
            "../../../data/fixtures/source-decoding-contract.json"
        ))
        .unwrap();
        for vector in vectors {
            let decoded = decode_source_bytes(&vector.bytes).unwrap();
            assert_eq!(decoded.text, vector.text, "{} text", vector.name);
            assert_eq!(
                decoded.encoding, vector.encoding,
                "{} encoding",
                vector.name
            );
            assert_eq!(decoded.source_hash, vector.sha256, "{} hash", vector.name);
        }
    }
}
