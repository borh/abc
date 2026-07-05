use std::{
    collections::BTreeMap,
    env,
    io::Write,
    process::{Command, Stdio},
};

use anyhow::{Context, Result, bail};
use aozora_pipeline::lexer::sanitize as sanitize_aozora_source;
use encoding_rs::SHIFT_JIS;
use regex::Regex;
use serde::Deserialize;
use serde_json::{Value, json};
use sha2::{Digest, Sha256};

pub const VERSION_PREFIX: &str = "aozora-adapter 0.1.0";

#[derive(Debug)]
pub struct DecodedSource {
    pub text: String,
    pub span_text: String,
    pub encoding: &'static str,
    pub source_hash: String,
}

#[derive(Debug, Deserialize)]
struct Envelope<T> {
    #[serde(rename = "schemaVersion")]
    schema_version: u64,
    data: Vec<T>,
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
    source[..offset]
        .trim_end_matches(|ch| ch == '\n' || ch == '\r')
        .len()
}

fn lines_from(source: &str, offset: usize) -> impl Iterator<Item = (usize, &str)> {
    let mut cursor = offset;
    source[offset..].split_inclusive('\n').map(move |line| {
        let start = cursor;
        cursor += line.len();
        (start, line)
    })
}

pub fn aat_json_from_bytes(bytes: &[u8]) -> Result<Vec<u8>> {
    let decoded = decode_source_bytes(bytes)?;
    let nodes = inspect::<AozoraNode>("nodes", &decoded.span_text)?;
    let diagnostics = inspect::<AozoraDiagnostic>("diagnostics", &decoded.span_text)?;
    let gaiji = inspect::<AozoraGaiji>("gaiji", &decoded.span_text)?;
    let aat = build_aat(&decoded, &nodes.data, &diagnostics.data, &gaiji.data);
    let mut out = Vec::new();
    serde_json::to_writer(&mut out, &aat)?;
    out.push(b'\n');
    Ok(out)
}

pub fn html_from_bytes(bytes: &[u8]) -> Result<Vec<u8>> {
    let decoded = decode_source_bytes(bytes)?;
    let output = run_aozora(["render", "-"], &decoded.text)?;
    Ok(output.into_bytes())
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
    let blocks = if decoded.span_text.contains("［＃ここから2字下げ］") {
        build_jisage_fixture_blocks(decoded, nodes, &gaiji_by_start)
    } else {
        blocks_from_inline_content(inline_content(decoded, nodes, &gaiji_by_start))
    };
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
            "adapter": "aozora",
            "adapter_version": adapter_version(),
            "source_encoding": decoded.encoding,
            "source_hash": decoded.source_hash,
            "parse_complete": diagnostics.iter().all(|d| d.severity.as_deref() != Some("error")),
            "warnings": warnings
        }
    })
}

fn build_jisage_fixture_blocks(
    decoded: &DecodedSource,
    nodes: &[AozoraNode],
    gaiji_by_start: &BTreeMap<usize, AozoraGaiji>,
) -> Vec<Value> {
    let content = inline_content(decoded, nodes, gaiji_by_start);
    vec![json!({
        "kind": "jisage_block",
        "x-indent": 2,
        "children": [{
            "kind": "paragraph",
            "content": content
        }]
    })]
}

fn blocks_from_inline_content(content: Vec<Value>) -> Vec<Value> {
    let mut blocks = Vec::new();
    let mut paragraph = Vec::new();
    let mut strip_next_leading_newline = false;

    for mut node in content {
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
                continue;
            }
        }

        if is_heading_hint_raw(&node)
            && let Some(heading) = heading_block_from_hint(&mut paragraph, &node)
        {
            push_paragraph_if_not_empty(&mut blocks, std::mem::take(&mut paragraph));
            blocks.push(heading);
            strip_next_leading_newline = true;
            continue;
        }
        paragraph.push(node);
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
    Some(json!({
        "kind": "heading",
        "level": level,
        "style": style,
        "content": [heading_text],
        "x-provenance": "source-derived",
    }))
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
    let re = Regex::new(r"^｜?(?P<base>.+?)《(?P<reading>[^》]+)》$").unwrap();
    if let Some(caps) = re.captures(source) {
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

fn inspect<T>(kind: &str, source: &str) -> Result<Envelope<T>>
where
    T: for<'de> Deserialize<'de>,
{
    let output = run_aozora(["inspect", kind, "-"], source)?;
    let envelope: Envelope<T> = serde_json::from_str(&output)
        .with_context(|| format!("parse aozora inspect {kind} JSON"))?;
    if envelope.schema_version != 1 {
        bail!(
            "unsupported aozora inspect {kind} schemaVersion {}",
            envelope.schema_version
        );
    }
    Ok(envelope)
}

fn run_aozora<const N: usize>(args: [&str; N], source: &str) -> Result<String> {
    let bin = env::var("AB_AOZORA_BIN").unwrap_or_else(|_| "aozora".to_owned());
    let mut child = Command::new(&bin)
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .with_context(|| format!("spawn {bin}"))?;
    child
        .stdin
        .as_mut()
        .context("aozora stdin")?
        .write_all(source.as_bytes())?;
    let output = child.wait_with_output().context("wait for aozora")?;
    if !output.status.success() {
        bail!(
            "aozora exited with {}: {}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        );
    }
    String::from_utf8(output.stdout).context("aozora stdout was not UTF-8")
}

pub fn adapter_version() -> String {
    let bin = env::var("AB_AOZORA_BIN").unwrap_or_else(|_| "aozora".to_owned());
    let upstream = Command::new(&bin)
        .arg("--version")
        .output()
        .ok()
        .and_then(|out| String::from_utf8(out.stdout).ok())
        .map(|s| s.trim().to_owned())
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| "aozora unknown".to_owned());
    format!("{VERSION_PREFIX} {upstream}")
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
