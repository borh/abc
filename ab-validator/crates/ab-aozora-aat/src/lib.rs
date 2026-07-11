//! AAT (Aozora AST Transform) adapter ported from the frozen aozora adapter.

use std::{collections::BTreeMap, fmt::Write as _, mem, ops::Range, str, sync::LazyLock};

use anyhow::Result;
use ab_aozora_pipeline::lexer::sanitize::{SanitizeMaps, sanitize_mapped};
use encoding_rs::SHIFT_JIS;
use regex::Regex;
use serde::{Deserialize, Serialize};
use serde::de::DeserializeOwned;
use serde_json::{Value, json};
use sha2::{Digest, Sha256};

use ab_aozora_facade::{self, Diagnostic, Document, encoding, json as aozora_json};

/// The sanitize stage's `Diagnostic` type is the exact same
/// `ab_aozora_spec::Diagnostic` the facade re-exports as `Diagnostic` (and
/// the same type `Tree::diagnostics()` returns).
///
/// Confirmed via `crates/ab-aozora-pipeline/src/lexer/sanitize.rs`'s
/// `use ab_aozora_spec::Diagnostic;` and `crates/ab-aozora-facade/src/lib.rs`'s
/// `pub use ab_aozora_spec::{..., Diagnostic, ...};`. One alias, one
/// `aozora_json::diagnostic_entries` call serves both diagnostic families.
pub type AozoraSanitizeDiagnostic = Diagnostic;

/// v1-parity fallback for ruby nodes with no resolvable `ruby_entries`
/// entry (gaiji-base ruby, `※［＃…］《reading》`, whose base is
/// `Content::Segments` and so has no plain-text range to resolve — see
/// `ruby_node`'s fallback branch). This is the original v1 regex, restored
/// verbatim so a gaiji-base ruby's typed emission stays byte-identical to
/// the pre-`ruby_entries` adapter output (Task 8's delta-audit ruby class
/// requires baseline typed ruby to be byte-identical in the candidate;
/// silently downgrading these to `raw` would break that).
static RUBY_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^｜?(?P<base>.+?)《(?P<reading>[^》]+)》$").unwrap());

/// Wire-shaped source decoding output ported from the frozen adapter.
#[derive(Debug)]
pub struct DecodedSource {
    /// Decoded text.
    pub text: String,
    /// Decoded text with sanitization for span alignment.
    pub span_text: String,
    /// The encoding that was successfully decoded (utf-8, utf-8-bom, windows-31j, or lossy variant).
    pub encoding: &'static str,
    /// Hex-encoded SHA256 hash of the input bytes.
    pub source_hash: String,
    /// Sanitize-stage diagnostics (PUA collisions, accent notes) — born
    /// BEFORE the parse; the parse of neutralized text cannot rediscover
    /// them. Spans are full-sanitized-text byte offsets.
    pub sanitize_diagnostics: Vec<AozoraSanitizeDiagnostic>,
    /// Sanitize offset maps + body offset + line index for span rebasing
    /// (ADR 0024: emitted spans are offsets against the full decoded
    /// source `text`, with real 1-based line numbers).
    span_ctx: SpanContext,
}

/// Composition chain for translating a parser/sanitize-stage byte offset
/// into `DecodedSource.text` coordinates plus a real line number.
///
/// Two offset systems feed span emission: the parser (and AAT node
/// construction) work in `span_text` (sanitized-body-relative) offsets;
/// `sanitize_diagnostics` carry full-sanitized-text offsets. Both compose
/// through `maps` (sanitized → decoded `text`); the former additionally
/// needs `body_offset` added first (body-relative → full-sanitized).
#[derive(Debug)]
struct SpanContext {
    maps: SanitizeMaps,
    /// Byte offset of the body slice within the SANITIZED text.
    body_offset: usize,
    /// Byte offsets of line starts in the DECODED text (`text`).
    line_starts: Vec<usize>,
}

impl SpanContext {
    fn to_decoded(&self, body_offset: usize) -> usize {
        self.maps.to_source_offset(body_offset + self.body_offset)
    }

    fn to_decoded_end(&self, body_end: usize) -> usize {
        self.maps.to_source_end(body_end + self.body_offset)
    }

    fn line_of(&self, decoded_offset: usize) -> u64 {
        (self.line_starts.partition_point(|&s| s <= decoded_offset)) as u64
    }
}

/// Byte offsets of line starts in `text`, treating every terminator the
/// decoded source can actually carry as a boundary: `\n`, `\r\n` (ONE
/// boundary, after the pair), and bare `\r`.
///
/// The decoded source keeps its original terminators — sanitize's CR/LF
/// normalization only rewrites the text the PARSER sees. A `\n`-only
/// index therefore synthesizes line 1 for every span of a classic-Mac
/// bare-CR source (zero `\n` in the whole file), violating ADR 0024's
/// real-line requirement. Counting `\r\n` as one boundary keeps CRLF
/// sources' boundary set byte-identical to a `\n`-only index (the
/// boundary sits after the pair, exactly where `\n`+1 put it), and
/// treating lone `\r` as a terminator mirrors sanitize's lone-`\r`→`\n`
/// rewrite, so decoded-source lines and sanitized-text lines agree in
/// count for every input.
fn line_starts(text: &str) -> Vec<usize> {
    let bytes = text.as_bytes();
    let mut starts = vec![0];
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b'\n' => {
                i += 1;
                starts.push(i);
            }
            b'\r' => {
                i += if bytes.get(i + 1) == Some(&b'\n') { 2 } else { 1 };
                starts.push(i);
            }
            _ => i += 1,
        }
    }
    starts
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
    /// Stable kebab-case diagnostic code from the façade wire entry
    /// (`ab_aozora_facade::json::Diagnostic::code`, always
    /// `kind.replace('_', "-")`). `Option` only because this struct is
    /// deserialized generically from any diagnostic-entries JSON; the
    /// façade always populates it.
    #[serde(default)]
    code: Option<String>,
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

/// The lossy-local counterpart of `ab_aozora_facade::json::RubyEntry` — same
/// deserialize-the-serialized-entries pattern `AozoraDiagnostic` /
/// `AozoraGaiji` already use.
#[derive(Debug, Deserialize, Clone)]
struct AozoraRubyEntry {
    span: Span,
    base: String,
    reading: String,
    side: String,
}

/// Decode source bytes to a normalized text with encoding detection.
///
/// # Errors
///
/// Returns an error if UTF-8 decoding with BOM fails unexpectedly.
pub fn decode_source_bytes(bytes: &[u8]) -> Result<DecodedSource> {
    let source_hash = format!("sha256:{}", hex_sha256(bytes));
    if bytes.starts_with(&[0xef, 0xbb, 0xbf]) {
        let text = str::from_utf8(&bytes[3..])?.to_owned();
        let (span_text, sanitize_diagnostics, maps, body_offset) = sanitize_for_aat(&text);
        let span_ctx = SpanContext { maps, body_offset, line_starts: line_starts(&text) };
        return Ok(DecodedSource {
            text,
            span_text,
            encoding: "utf-8-bom",
            source_hash,
            sanitize_diagnostics,
            span_ctx,
        });
    }
    if let Ok(text) = str::from_utf8(bytes) {
        let text = text.to_owned();
        let (span_text, sanitize_diagnostics, maps, body_offset) = sanitize_for_aat(&text);
        let span_ctx = SpanContext { maps, body_offset, line_starts: line_starts(&text) };
        return Ok(DecodedSource {
            text,
            span_text,
            encoding: "utf-8",
            source_hash,
            sanitize_diagnostics,
            span_ctx,
        });
    }
    let (cow, _, had_errors) = SHIFT_JIS.decode(bytes);
    let text = cow.into_owned();
    let (span_text, sanitize_diagnostics, maps, body_offset) = sanitize_for_aat(&text);
    let span_ctx = SpanContext { maps, body_offset, line_starts: line_starts(&text) };
    Ok(DecodedSource {
        text,
        span_text,
        encoding: if had_errors {
            "windows-31j-lossy"
        } else {
            "windows-31j"
        },
        source_hash,
        sanitize_diagnostics,
        span_ctx,
    })
}

fn sanitize_for_aat(
    text: &str,
) -> (String, Vec<AozoraSanitizeDiagnostic>, SanitizeMaps, usize) {
    let mapped = sanitize_mapped(text);
    let sanitize_diagnostics = mapped.diagnostics;
    let sanitized = mapped.text.into_owned();
    let body = aozora_body_range(&sanitized);
    (
        sanitized[body.clone()].to_owned(),
        sanitize_diagnostics,
        mapped.maps,
        body.start,
    )
}

fn aozora_body_range(source: &str) -> Range<usize> {
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

    body_start..body_end
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

#[allow(clippy::type_complexity, reason = "one tuple per projected wire channel; a named struct would only restate the field set")]
fn projections(
    span_text: &str,
) -> Result<(
    Vec<AozoraNode>,
    Vec<AozoraDiagnostic>,
    Vec<AozoraGaiji>,
    Vec<AozoraRubyEntry>,
)> {
    // Mirrors the upstream binary's own stdin handling: each `aozora
    // inspect` subprocess ran decode_auto over the bytes the adapter piped
    // in (already-valid UTF-8 passes through unchanged).
    let source = encoding::decode_auto(span_text.as_bytes())
        .map_err(|err| anyhow::anyhow!("decode_auto: {err:?}"))?;
    let doc = Document::new(source.clone());
    let tree = doc.parse();
    let nodes = from_entries(aozora_json::node_entries(&tree))?;
    let diagnostics = from_entries(aozora_json::diagnostic_entries(tree.diagnostics()))?;
    let gaiji = from_entries(aozora_json::gaiji_entries(&source))?;
    let ruby = from_entries(aozora_json::ruby_entries(&tree))?;
    Ok((nodes, diagnostics, gaiji, ruby))
}

/// Same data path as the deleted wire hop: the facade's Serialize impls
/// (which produced the inspect JSON) feed the adapter's Deserialize types.
/// Deserialization is key-order-independent, so no `preserve_order` needed.
fn from_entries<S: Serialize, T: DeserializeOwned>(
    entries: Vec<S>,
) -> Result<Vec<T>> {
    Ok(serde_json::from_value(serde_json::to_value(entries)?)?)
}

// The wire envelope's schemaVersion check becomes a compile-time pin: the
// from_entries round-trip is only valid against the wire shape this port
// was written for.
const _: () = assert!(aozora_json::SCHEMA_VERSION == 3, "incompatible wire schema version");

/// Transform Aozora source bytes into AAT JSON output.
///
/// # Errors
///
/// Returns an error if source decoding, projection parsing, or JSON serialization fails.
pub fn aat_json_from_bytes(bytes: &[u8]) -> Result<Vec<u8>> {
    let decoded = decode_source_bytes(bytes)?;
    let (nodes, diagnostics, gaiji, ruby) = projections(&decoded.span_text)?;
    let aat = build_aat(&decoded, &nodes, &diagnostics, &gaiji, &ruby);
    let mut out = Vec::new();
    serde_json::to_writer(&mut out, &aat)?;
    out.push(b'\n');
    Ok(out)
}

/// Rewrite every `entry["span"]["start"/"end"]` in a diagnostic-entries
/// array in place via the given translation functions. Entries lacking a
/// numeric span (or lacking a `span` object entirely) pass through
/// untouched.
fn rebase_spans(
    data: &mut Value,
    translate: impl Fn(usize) -> usize,
    translate_end: impl Fn(usize) -> usize,
) {
    if let Some(items) = data.as_array_mut() {
        for entry in items {
            let (Some(start), Some(end)) = (
                entry["span"]["start"]
                    .as_u64()
                    .and_then(|v| usize::try_from(v).ok()),
                entry["span"]["end"]
                    .as_u64()
                    .and_then(|v| usize::try_from(v).ok()),
            ) else {
                continue;
            };
            entry["span"]["start"] = json!(translate(start));
            entry["span"]["end"] = json!(translate_end(end));
        }
    }
}

/// One wire diagnostics envelope (`{"data": […], "schemaVersion": 3}`)
/// per input — the `--mode diagnostics` payload.
///
/// Single owner of the diagnostics path (Phase 3 design spec): decoding,
/// sanitization, and body selection are the EXACT same
/// `decode_source_bytes` path as `aat_json_from_bytes`; the parse mirrors
/// `projections()`. Entry order: sanitize-stage diagnostics, then parser
/// diagnostics. Duplicates are impossible by construction (the inner
/// re-sanitize sees already-neutralized, already-rewritten text) — the
/// merge-order test pins this.
///
/// # Errors
///
/// Returns an error if source decoding, projection parsing, or JSON
/// serialization fails.
pub fn diagnostics_json_from_bytes(bytes: &[u8]) -> Result<Vec<u8>> {
    let decoded = decode_source_bytes(bytes)?;
    let source = encoding::decode_auto(decoded.span_text.as_bytes())
        .map_err(|err| anyhow::anyhow!("decode_auto: {err:?}"))?;
    let doc = Document::new(source);
    let tree = doc.parse();
    let mut data =
        serde_json::to_value(aozora_json::diagnostic_entries(&decoded.sanitize_diagnostics))?;
    let mut parser_entries =
        serde_json::to_value(aozora_json::diagnostic_entries(tree.diagnostics()))?;
    // Sanitize-stage entries carry full-sanitized-text offsets → through
    // the maps directly (no body offset). Parser entries carry
    // body-relative offsets → body offset + maps. Both land in decoded-
    // source (`decoded.text`) coordinates, matching `span_json`'s output.
    rebase_spans(
        &mut data,
        |o| decoded.span_ctx.maps.to_source_offset(o),
        |o| decoded.span_ctx.maps.to_source_end(o),
    );
    rebase_spans(
        &mut parser_entries,
        |o| decoded.span_ctx.to_decoded(o),
        |o| decoded.span_ctx.to_decoded_end(o),
    );
    if let (Some(items), Some(more)) = (data.as_array_mut(), parser_entries.as_array()) {
        items.extend(more.iter().cloned());
    }
    let envelope = json!({
        "schemaVersion": aozora_json::SCHEMA_VERSION,
        "data": data,
    });
    let mut out = Vec::new();
    serde_json::to_writer(&mut out, &envelope)?;
    out.push(b'\n');
    Ok(out)
}

fn build_aat(
    decoded: &DecodedSource,
    nodes: &[AozoraNode],
    diagnostics: &[AozoraDiagnostic],
    gaiji: &[AozoraGaiji],
    ruby: &[AozoraRubyEntry],
) -> Value {
    let gaiji_by_start = gaiji
        .iter()
        .map(|entry| (entry.span.start, entry.clone()))
        .collect::<BTreeMap<_, _>>();
    let ruby_by_span = ruby
        .iter()
        .map(|entry| ((entry.span.start, entry.span.end), entry.clone()))
        .collect::<BTreeMap<_, _>>();
    let blocks = blocks_from_inline_content(inline_content(
        decoded,
        nodes,
        &gaiji_by_start,
        &ruby_by_span,
    ));
    let warnings = diagnostics
        .iter()
        .map(|diagnostic| diagnostic_warning(diagnostic, &decoded.span_ctx))
        .collect::<Vec<_>>();
    json!({
        "version": 2,
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

#[allow(
    clippy::needless_pass_by_value,
    reason = "Vec<Value> signature locked by frozen-adapter port discipline"
)]
#[allow(
    clippy::too_many_lines,
    reason = "single linear classifier pass ported from the frozen adapter; splitting would obscure the branch order contract"
)]
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
                push_paragraph_if_not_empty(&mut blocks, mem::take(&mut paragraph));
                let inner = content[index + 1..boundary].to_vec();
                push_chitsuki_paragraph(&mut blocks, offset, inner);
                index = boundary;
                continue;
            }
        }

        if let Some((first, rest)) = burasage_container_indent(&node) {
            // Compound container: 字下げ/burasage classification is unchanged;
            // if the SAME marker also carries a 字詰め clause (jizume compound
            // form), the burasage output nests inside a jizume_block instead
            // of landing directly in `blocks`.
            let compound_jizume_width = node
                .get("source")
                .and_then(Value::as_str)
                .and_then(jizume_open_chars);
            if let Some(close_index) = find_matching_jisage_close(&content, index + 1) {
                push_paragraph_if_not_empty(&mut blocks, mem::take(&mut paragraph));
                let mut inner = content[index + 1..close_index].to_vec();
                strip_boundary_newlines(&mut inner);
                push_burasage_paragraph_maybe_jizume(
                    &mut blocks,
                    compound_jizume_width,
                    first,
                    rest,
                    inner,
                );
                strip_next_leading_newline = true;
                index = close_index + 1;
                continue;
            }
            let boundary = find_next_container_boundary(&content, index + 1);
            if boundary > index + 1 {
                push_paragraph_if_not_empty(&mut blocks, mem::take(&mut paragraph));
                let mut inner = content[index + 1..boundary].to_vec();
                strip_boundary_newlines(&mut inner);
                push_burasage_paragraph_maybe_jizume(
                    &mut blocks,
                    compound_jizume_width,
                    first,
                    rest,
                    inner,
                );
                index = boundary;
                continue;
            }
        } else if let Some(width) = pure_jizume_open_width(&node) {
            if let Some(close_index) = find_matching_container_close(&content, index + 1, "字詰め")
            {
                push_paragraph_if_not_empty(&mut blocks, mem::take(&mut paragraph));
                let mut inner = content[index + 1..close_index].to_vec();
                strip_boundary_newlines(&mut inner);
                blocks.push(json!({
                    "kind": "jizume_block",
                    "width": width,
                    "children": blocks_from_inline_content(inner)
                }));
                strip_next_leading_newline = true;
                index = close_index + 1;
                continue;
            }
        } else if let Some(indent) = jisage_container_indent(&node) {
            if let Some(close_index) = find_matching_jisage_close(&content, index + 1) {
                push_paragraph_if_not_empty(&mut blocks, mem::take(&mut paragraph));
                let mut inner = content[index + 1..close_index].to_vec();
                strip_boundary_newlines(&mut inner);
                blocks.push(json!({
                    "kind": "jisage_block",
                    "indent": indent,
                    "children": blocks_from_inline_content(inner)
                }));
                strip_next_leading_newline = true;
                index = close_index + 1;
                continue;
            }
            let boundary = find_next_container_boundary(&content, index + 1);
            if boundary > index + 1 {
                push_paragraph_if_not_empty(&mut blocks, mem::take(&mut paragraph));
                let mut inner = content[index + 1..boundary].to_vec();
                strip_boundary_newlines(&mut inner);
                blocks.push(json!({
                    "kind": "jisage_block",
                    "indent": indent,
                    "children": blocks_from_inline_content(inner)
                }));
                index = boundary;
                continue;
            }
        } else if block_container_open(&node, "［＃ここから罫囲み］") {
            if let Some(close_index) = find_matching_container_close(&content, index + 1, "罫囲み")
            {
                push_paragraph_if_not_empty(&mut blocks, mem::take(&mut paragraph));
                let mut inner = content[index + 1..close_index].to_vec();
                strip_boundary_newlines(&mut inner);
                blocks.push(json!({
                    "kind": "keigakomi_block",
                    "children": blocks_from_inline_content(inner)
                }));
                strip_next_leading_newline = true;
                index = close_index + 1;
                continue;
            }
        } else if block_container_open(&node, "［＃ここから横組み］")
            && let Some(close_index) = find_matching_container_close(&content, index + 1, "横組み")
        {
            push_paragraph_if_not_empty(&mut blocks, mem::take(&mut paragraph));
            let mut inner = content[index + 1..close_index].to_vec();
            strip_boundary_newlines(&mut inner);
            blocks.push(json!({
                "kind": "yokogumi_block",
                "children": blocks_from_inline_content(inner)
            }));
            strip_next_leading_newline = true;
            index = close_index + 1;
            continue;
        }

        if is_heading_hint_raw(&node)
            && let Some(heading) = heading_block_from_hint(&mut paragraph, &node)
        {
            push_paragraph_if_not_empty(&mut blocks, mem::take(&mut paragraph));
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

#[allow(
    clippy::needless_pass_by_value,
    reason = "Vec<Value> signature locked by frozen-adapter port discipline"
)]
fn push_paragraph_if_not_empty(blocks: &mut Vec<Value>, content: Vec<Value>) {
    if content.is_empty() {
        return;
    }
    blocks.push(json!({
        "kind": "paragraph",
        "content": content
    }));
}

#[allow(
    clippy::needless_pass_by_value,
    reason = "Vec<Value> signature locked by frozen-adapter port discipline"
)]
fn push_chitsuki_paragraph(blocks: &mut Vec<Value>, offset: u64, content: Vec<Value>) {
    blocks.push(json!({
        "kind": "paragraph",
        "content": [{
            "kind": "style",
            "style_type": "chitsuki",
            "content": content,
            "align": "right",
            "offset_from_end": offset,
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

#[allow(
    clippy::needless_pass_by_value,
    reason = "Vec<Value> signature locked by frozen-adapter port discipline"
)]
fn push_burasage_paragraph(blocks: &mut Vec<Value>, first: u64, rest: u64, content: Vec<Value>) {
    blocks.push(json!({
        "kind": "paragraph",
        "content": [{
            "kind": "style",
            "style_type": "burasage",
            "content": content,
            "indent_first": first,
            "indent_rest": rest,
            "x-provenance": "source-derived"
        }]
    }));
}

/// As [`push_burasage_paragraph`], but if `jizume_width` is present (the
/// compound container's marker also carried a `字詰め` clause) the burasage
/// paragraph nests inside a `jizume_block { width }` instead of landing
/// directly in `blocks` — the compound close-matching logic that got us
/// here is untouched; only the destination of the classified output moves.
#[allow(
    clippy::needless_pass_by_value,
    reason = "Vec<Value> signature locked by frozen-adapter port discipline"
)]
fn push_burasage_paragraph_maybe_jizume(
    blocks: &mut Vec<Value>,
    jizume_width: Option<u64>,
    first: u64,
    rest: u64,
    content: Vec<Value>,
) {
    match jizume_width {
        Some(width) => {
            let mut children = Vec::new();
            push_burasage_paragraph(&mut children, first, rest, content);
            blocks.push(json!({
                "kind": "jizume_block",
                "width": width,
                "children": children
            }));
        }
        None => push_burasage_paragraph(blocks, first, rest, content),
    }
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

/// Recognize a jizume (字詰め) container-open marker and extract the
/// chars-per-line count — standalone (`［＃ここからN字詰め］`) or as the
/// FINAL clause of a compound container
/// (`［＃ここから６字下げ、折り返して７字下げ、２１字詰め］`).
///
/// Consumed by the block classifier to emit `jizume_block` (standalone) or
/// to wrap the existing burasage classification of a compound container
/// (Phase 4, this recognizer's emission wave).
#[must_use]
pub fn jizume_open_chars(source: &str) -> Option<u64> {
    let marker = source.trim();
    if !marker.starts_with("［＃ここから") || !marker.ends_with('］') {
        return None;
    }
    let (_, after) = marker.split_once("字詰め")?;
    if after != "］" {
        return None;
    }
    parse_aozora_number_before(marker, "字詰め")
}

/// Recognize the jizume container-close marker (`［＃ここで字詰め終わり］`).
#[must_use]
pub fn is_jizume_close(source: &str) -> bool {
    source.trim() == "［＃ここで字詰め終わり］"
}

/// Recognize a *pure* jizume container-open — one with no `字下げ` segment —
/// so this arm never shadows (or is shadowed by) the jisage/burasage
/// compound handling, which always carries `字下げ`.
fn pure_jizume_open_width(node: &Value) -> Option<u64> {
    if node.get("kind").and_then(Value::as_str) != Some("raw")
        || node.get("x-source-marker-kind").and_then(Value::as_str) != Some("containerOpen")
    {
        return None;
    }
    let source = node.get("source").and_then(Value::as_str)?;
    if source.contains("字下げ") {
        return None;
    }
    jizume_open_chars(source)
}

fn find_matching_jisage_close(content: &[Value], start: usize) -> Option<usize> {
    find_matching_container_close(content, start, "字下げ")
}

fn find_matching_container_close(content: &[Value], start: usize, needle: &str) -> Option<usize> {
    for (offset, node) in content[start..].iter().enumerate() {
        if is_container_open_raw(node) {
            return None;
        }
        if is_container_close_with(node, needle) {
            return Some(start + offset);
        }
    }
    None
}

fn is_container_close_with(node: &Value, needle: &str) -> bool {
    node.get("kind").and_then(Value::as_str) == Some("raw")
        && node.get("x-source-marker-kind").and_then(Value::as_str) == Some("containerClose")
        && node
            .get("source")
            .and_then(Value::as_str)
            .is_some_and(|source| source.contains(needle))
}

fn block_container_open(node: &Value, marker: &str) -> bool {
    is_container_open_raw(node)
        && node
            .get("source")
            .and_then(Value::as_str)
            .is_some_and(|source| source.trim() == marker)
}

fn find_next_container_boundary(content: &[Value], start: usize) -> usize {
    content[start..]
        .iter()
        .position(is_container_marker_raw)
        .map_or(content.len(), |offset| start + offset)
}

fn find_next_raw_boundary(content: &[Value], start: usize) -> usize {
    content[start..]
        .iter()
        .position(|node| node.get("kind").and_then(Value::as_str) == Some("raw"))
        .map_or(content.len(), |offset| start + offset)
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
        heading["indent"] = json!(indent);
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
    ruby_by_span: &BTreeMap<(usize, usize), AozoraRubyEntry>,
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
            "ruby" => content.push(ruby_node(decoded, node, ruby_by_span)),
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
                "span": span_json(&node.span, &decoded.span_ctx)
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
            "span": span_json(&span, &decoded.span_ctx)
        }));
    } else {
        content.push(json!({
            "kind": "text",
            "value": source,
            "span": span_json(&span, &decoded.span_ctx)
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

#[allow(
    clippy::option_if_let_else,
    reason = "if/else form preserved from frozen adapter; lambda restructure not permitted"
)]
fn ruby_node(
    decoded: &DecodedSource,
    node: &AozoraNode,
    ruby_by_span: &BTreeMap<(usize, usize), AozoraRubyEntry>,
) -> Value {
    if let Some(entry) = ruby_by_span.get(&(node.span.start, node.span.end)) {
        return json!({
            "kind": "ruby",
            "base": entry.base,
            "reading": entry.reading,
            "direction": entry.side,
            "span": span_json(&node.span, &decoded.span_ctx)
        });
    }
    // No resolvable structured entry (e.g. gaiji-base ruby, whose base is
    // `Content::Segments` and so has no plain-text range for
    // `ruby_entries` to resolve — see the `RUBY_RE` doc comment). Fall
    // back to the original v1 regex reparse so this stays byte-identical
    // to v1's typed emission rather than silently downgrading to `raw`.
    let source = source_slice(&decoded.span_text, &node.span);
    if let Some(caps) = RUBY_RE.captures(source) {
        json!({
            "kind": "ruby",
            "base": caps.name("base").unwrap().as_str(),
            "reading": caps.name("reading").unwrap().as_str(),
            "direction": "right",
            "span": span_json(&node.span, &decoded.span_ctx)
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
        "span": span_json(&node.span, &decoded.span_ctx)
    })
}

fn style_node(decoded: &DecodedSource, node: &AozoraNode, style_type: &str) -> Value {
    let source = source_slice(&decoded.span_text, &node.span);
    let text = marker_target(source).unwrap_or(source);
    json!({
        "kind": "style",
        "style_type": style_type,
        "content": [{"kind": "text", "value": text}],
        "span": span_json(&node.span, &decoded.span_ctx)
    })
}

fn tcy_node(decoded: &DecodedSource, node: &AozoraNode) -> Value {
    let source = source_slice(&decoded.span_text, &node.span);
    let text = marker_target(source).unwrap_or(source);
    json!({
        "kind": "tcy",
        "content": [{"kind": "text", "value": text}],
        "span": span_json(&node.span, &decoded.span_ctx)
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
        "span": span_json(&node.span, &decoded.span_ctx)
    })
}

/// Builds a schema-v2 `meta.warnings[]` entry (`{code, severity, message,
/// span?}`) from a façade-diagnostic-derived `AozoraDiagnostic`. The single
/// call site (`build_aat`) only ever passes parser diagnostics sourced from
/// `aozora_json::diagnostic_entries` — every warning is façade-passthrough;
/// there are no adapter-origin warning sites.
fn diagnostic_warning(diagnostic: &AozoraDiagnostic, ctx: &SpanContext) -> Value {
    let message = diagnostic
        .kind
        .clone()
        .unwrap_or_else(|| "aozora diagnostic".to_owned());
    let code = diagnostic
        .code
        .clone()
        .unwrap_or_else(|| message.replace('_', "-"));
    // severity_str's non-exhaustive default arm is "error"; mirror that
    // here so an absent severity surfaces loudly rather than passing as
    // benign.
    let severity = diagnostic
        .severity
        .clone()
        .unwrap_or_else(|| "error".to_owned());
    let mut warning = json!({ "code": code, "severity": severity, "message": message });
    if let Some(span) = diagnostic.span.as_ref() {
        warning["span"] = span_json(span, ctx);
    }
    warning
}

fn span_json(span: &Span, ctx: &SpanContext) -> Value {
    let byte_start = ctx.to_decoded(span.start);
    let byte_end = ctx.to_decoded_end(span.end);
    let line_start = ctx.line_of(byte_start);
    let line_end = ctx.line_of(if byte_end > byte_start {
        byte_end - 1
    } else {
        byte_start
    });
    json!({
        "line_start": line_start,
        "line_end": line_end,
        "byte_start": byte_start,
        "byte_end": byte_end
    })
}

fn source_slice<'a>(source: &'a str, span: &Span) -> &'a str {
    source.get(span.start..span.end).unwrap_or("")
}

/// Identity fields per the executable-boundary contract.
///
/// Returns a version string with adapter id, version, AAT schema version, and build identity.
/// The git rev is injected by build.rs from `AB_AOZORA_GIT_REV` and is part of the registry's
/// exact-match coordinate — a "git unknown" build must never become gate evidence or a registry row.
#[must_use]
pub fn adapter_version() -> String {
    format!(
        "ab-aozora {} aat-schema 2 facade {} wire-schema {} (git {})",
        env!("CARGO_PKG_VERSION"),
        ab_aozora_facade_version(),
        aozora_json::SCHEMA_VERSION,
        env!("AB_AOZORA_GIT_REV"),
    )
}

fn ab_aozora_facade_version() -> &'static str {
    // The facade crate version stands in for the deleted wire
    // schemaVersion check as the compatibility coordinate.
    ab_aozora_facade::VERSION
}

fn hex_sha256(bytes: &[u8]) -> String {
    let digest = Sha256::digest(bytes);
    let mut out = String::with_capacity(digest.len() * 2);
    for byte in digest {
        #[allow(unused_must_use, reason = "write to Vec never fails")]
        {
            write!(out, "{byte:02x}");
        }
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

    /// `preserve_order` tripwire (see
    /// `docs/handoffs/2026-07-10-parser-fork-provenance.md`'s
    /// feature-unification hazard section): `aat_json_from_bytes` builds its
    /// `serde_json::Value` output via object literals (`json!` macro
    /// insertion order), so if `serde_json/preserve_order` ever leaks into
    /// this crate's compiled feature graph, `Value`'s map switches from
    /// `BTreeMap` (alphabetical-by-key serialization) to `IndexMap`
    /// (insertion-order serialization) and this exact-byte assertion goes
    /// red — a parsed/`Value`-equality check would NOT catch this, since
    /// `Value::eq` for objects is order-independent.
    ///
    /// Expected output re-pasted 2026-07-12 (Task 9: `ab-aozora` `0.3.0` →
    /// `0.4.0`, `aat-schema 1` → `aat-schema 2`, document `"version"` `1` →
    /// `2`) via:
    /// ```text
    /// export RUSTC_WRAPPER= SCCACHE_DISABLE=1
    /// cd ab-validator
    /// cargo test -p ab-aozora-aat --lib aat_json_from_bytes_is_byte_exact_under_default_map_ordering -- --nocapture
    /// ```
    /// (this test's own literal is the deliberately-stale assertion; the
    /// panic message prints the actual bytes, decoded and pasted here
    /// verbatim). The `(git unknown)` suffix in `adapter_version` is
    /// `build.rs`'s fallback when `AB_AOZORA_GIT_REV` is unset, which is the
    /// case for a plain `cargo test` invocation (only flake-built release
    /// binaries bake in a real rev; see `flake.nix`'s `AB_AOZORA_GIT_REV =
    /// self.rev or "unknown"` and `build.rs`'s doc comment).
    #[test]
    fn aat_json_from_bytes_is_byte_exact_under_default_map_ordering() {
        let expected = "{\"blocks\":[{\"content\":[{\"kind\":\"text\",\"span\":{\"byte_end\":4,\"byte_start\":0,\"line_end\":1,\"line_start\":1},\"value\":\"あ\\n\"}],\"kind\":\"paragraph\"}],\"meta\":{\"adapter\":\"ab-aozora\",\"adapter_version\":\"ab-aozora 0.4.0 aat-schema 2 facade 0.3.0 wire-schema 3 (git unknown)\",\"parse_complete\":true,\"source_encoding\":\"utf-8\",\"source_hash\":\"sha256:872f53a70d5e2b801dcad8ade42fa36f20a64f64e6c3af6b7de01ca026405843\",\"warnings\":[]},\"version\":2,\"work_id\":\"stdin\"}\n";
        let actual = aat_json_from_bytes("あ\n".as_bytes()).unwrap();
        assert_eq!(actual, expected.as_bytes());
    }

    #[test]
    fn diagnostics_json_from_bytes_emits_schema3_envelope_with_codes() {
        // Unclosed bracket → one error diagnostic.
        let out = diagnostics_json_from_bytes("あ［＃ここから".as_bytes()).unwrap();
        assert_eq!(out.last(), Some(&b'\n'));
        let doc: Value = serde_json::from_slice(&out).unwrap();
        assert_eq!(doc["schemaVersion"], 3);
        let data = doc["data"].as_array().unwrap();
        assert!(!data.is_empty());
        for entry in data {
            let code = entry["code"].as_str().unwrap();
            assert!(!code.contains('_') && !code.contains("::"), "not kebab: {code}");
            assert!(entry["severity"].is_string());
            assert!(entry["span"]["start"].is_u64() && entry["span"]["end"].is_u64());
        }
    }

    #[test]
    fn warnings_carry_facade_code_severity_span() {
        // Same input as diagnostics_json_from_bytes_emits_schema3_envelope_with_codes:
        // an unclosed bracket produces at least one (parser-stage) diagnostic.
        let aat = aat_value_for("あ［＃ここから");
        let w = &aat["meta"]["warnings"][0];
        assert!(
            w["code"].as_str().unwrap().chars().all(|c| c != '_'),
            "kebab code"
        );
        assert!(matches!(
            w["severity"].as_str().unwrap(),
            "error" | "warning" | "note"
        ));
        assert!(w.get("message").is_some());
        assert!(w.get("line").is_none(), "line dropped in v2");
        let span = &w["span"];
        assert!(span["line_start"].as_u64().unwrap() >= 1);
        assert!(span["byte_end"].as_u64().unwrap() >= span["byte_start"].as_u64().unwrap());
    }

    #[test]
    fn diagnostics_json_from_bytes_clean_source_is_empty_data() {
        let doc: Value =
            serde_json::from_slice(&diagnostics_json_from_bytes("あ\n".as_bytes()).unwrap())
                .unwrap();
        assert_eq!(doc["data"], json!([]));
    }

    #[test]
    fn sanitize_stage_pua_diagnostic_survives_to_the_envelope() {
        // Raw U+E001 in the source: sanitize neutralizes it to U+FFFD and
        // emits SourceContainsPua — the parse of the neutralized text can
        // never rediscover it, so it MUST come from the retained sanitize
        // diagnostics. あ = bytes 0..3, U+E001 = bytes 3..6.
        let out = diagnostics_json_from_bytes("あ\u{e001}い\n".as_bytes()).unwrap();
        let doc: Value = serde_json::from_slice(&out).unwrap();
        let data = doc["data"].as_array().unwrap();
        let pua: Vec<&Value> = data
            .iter()
            .filter(|e| e["code"] == "source-contains-pua")
            .collect();
        assert_eq!(pua.len(), 1, "expected exactly one PUA diagnostic: {data:?}");
        assert_eq!(pua[0]["severity"], "warning");
        assert_eq!(pua[0]["span"]["start"], 3);
        assert_eq!(pua[0]["span"]["end"], 6);
    }

    #[test]
    #[allow(
        clippy::manual_contains,
        reason = "assertion expression is a pinned test invariant, not touched per fix-wave scope"
    )]
    fn sanitize_and_parser_diagnostics_merge_in_order_without_duplicates() {
        // PUA (sanitize-stage) + unclosed bracket (parser-stage) in one input:
        // sanitize entries come first, parser entries after, one of each.
        let out = diagnostics_json_from_bytes("あ\u{e001}い［＃ここから".as_bytes()).unwrap();
        let doc: Value = serde_json::from_slice(&out).unwrap();
        let codes: Vec<&str> = doc["data"]
            .as_array()
            .unwrap()
            .iter()
            .map(|e| e["code"].as_str().unwrap())
            .collect();
        let pua_count = codes.iter().filter(|c| **c == "source-contains-pua").count();
        assert_eq!(pua_count, 1, "duplicate or missing PUA entry: {codes:?}");
        assert!(
            codes[0] == "source-contains-pua",
            "sanitize entries must come first: {codes:?}"
        );
        assert!(
            codes.iter().any(|c| *c == "unclosed-bracket"),
            "{codes:?}"
        );
    }

    #[test]
    fn diagnostics_json_from_bytes_reports_tcy_target_not_found() {
        // Named must vector `tate_chu_yoko` (upstream-aozora-notation-spec
        // conformance vectors): the tcy directive's target "12" does not
        // occur in the preceding text, so no run exists to rotate.
        let source = "昭和［＃「12」は縦中横］年\n";
        let out = diagnostics_json_from_bytes(source.as_bytes()).unwrap();
        let doc: Value = serde_json::from_slice(&out).unwrap();
        let data = doc["data"].as_array().unwrap();
        let tcy: Vec<&Value> = data
            .iter()
            .filter(|e| e["code"] == "tcy-target-not-found")
            .collect();
        assert_eq!(tcy.len(), 1, "expected exactly one tcy diagnostic: {data:?}");
        assert_eq!(tcy[0]["severity"], "warning");
        assert_eq!(tcy[0]["span"]["start"], 6);
        assert_eq!(tcy[0]["span"]["end"], 35);
    }

    #[test]
    fn diagnostics_json_from_bytes_rebases_sanitize_span_through_bom_and_crlf() {
        // BOM(3) + "あ\r\n" + PUA(U+E001) + "い\n": decoded text (post-
        // BOM-strip, per decode_source_bytes) is "あ\r\n\u{e001}い\n" —
        // あ 0..3, \r 3..4, \n 4..5, PUA 5..8, い 8..11, \n 11..12 (12
        // bytes). Sanitize normalizes \r\n (bytes 3..5) → \n (1 byte),
        // shifting everything after by one byte, and neutralizes the PUA
        // byte-length-preserving; the resulting SANITIZED text is
        // "あ\n\u{fffd}い\n" (あ 0..3, \n 3..4, PUA 4..7, い 7..10, \n
        // 10..11 — 11 bytes). The PUA sanitize diagnostic's span is born
        // in that sanitized-text coordinate system: 4..7. Rebasing
        // through the CRLF map (no body offset — sanitize entries skip
        // it) must land the span back at the PUA's own decoded-text
        // offsets: 5..8.
        let bytes = [b"\xef\xbb\xbf".as_ref(), "あ\r\n\u{e001}い\n".as_bytes()].concat();
        let out = diagnostics_json_from_bytes(&bytes).unwrap();
        let doc: Value = serde_json::from_slice(&out).unwrap();
        let data = doc["data"].as_array().unwrap();
        let pua: Vec<&Value> = data
            .iter()
            .filter(|e| e["code"] == "source-contains-pua")
            .collect();
        assert_eq!(pua.len(), 1, "expected exactly one PUA diagnostic: {data:?}");
        assert_eq!(pua[0]["span"]["start"], 5);
        assert_eq!(pua[0]["span"]["end"], 8);
    }

    #[test]
    fn spans_are_decoded_source_offsets_with_real_lines() {
        // BOM + CRLF + a page-break directive on its own line: decoded
        // text (post-BOM-strip, per decode_source_bytes) is
        // "あ\r\n［＃改ページ］\nい\n" — あ 0..3, \r 3..4, \n 4..5,
        // ［＃改ページ］ 5..26 (7 fullwidth chars × 3 bytes each), \n
        // 26..27, い 27..30, \n 30..31 (31 bytes total). The directive
        // is markup, so the parser splits it into its own node — unlike
        // a markup-free input, which the adapter merges into ONE
        // source-gap node spanning the whole body and so can never
        // exercise a line >1 (verified by running the brief's original
        // markup-free literal: it produces a single span, byte_end 9,
        // line_end 2, line_start 1 — never a span whose line_START is
        // 2, hence this input adds the directive).
        let bytes = [
            b"\xef\xbb\xbf".as_ref(),
            "あ\r\n［＃改ページ］\nい\n".as_bytes(),
        ]
        .concat();
        let doc: Value = serde_json::from_slice(&aat_json_from_bytes(&bytes).unwrap()).unwrap();
        let spans: Vec<&Value> = collect_spans(&doc["blocks"]);
        assert!(!spans.is_empty());
        // First text node "あ\n" ← decoded "あ\r\n" = bytes 0..5, line 1.
        assert_eq!(spans[0]["byte_start"], 0);
        assert_eq!(spans[0]["byte_end"], 5);
        assert_eq!(spans[0]["line_start"], 1);
        // The page-break directive itself: the CRLF collapse shifts its
        // post-BOM-strip position (4..25 in the sanitized body) forward
        // by exactly one decoded byte, landing at 5..26 — real line 2.
        let page_break = spans
            .iter()
            .find(|s| s["byte_start"] == 5)
            .unwrap_or_else(|| panic!("no span at decoded byte_start 5: {spans:?}"));
        assert_eq!(page_break["byte_end"], 26);
        assert_eq!(page_break["line_start"], 2);
        // Some span must sit on line 2 (the directive line).
        assert!(
            spans.iter().any(|s| s["line_start"] == 2),
            "no real line >1: {spans:?}"
        );
        // The legacy synthesized warning is gone.
        let warnings = doc["meta"]["warnings"].as_array().unwrap();
        assert!(warnings.iter().all(|w| {
            w["message"].as_str()
                != Some(
                    "aozora upstream spans are sanitized-source byte offsets; \
                     line_start and line_end are synthesized as 1",
                )
        }));
    }

    #[test]
    fn line_starts_handles_lf_crlf_and_bare_cr() {
        // CRLF: ONE boundary after the pair — byte-identical to the old
        // \n-only index ([0, 3] is exactly what match_indices('\n')+1
        // produced), proving CRLF sources' line numbering is unchanged.
        assert_eq!(line_starts("a\r\nb"), vec![0, 3]);
        // LF-only: unchanged classic behavior.
        assert_eq!(line_starts("a\nb"), vec![0, 2]);
        // Bare CR (classic Mac): now a real boundary.
        assert_eq!(line_starts("a\rb"), vec![0, 2]);
        // Two bare CRs: two boundaries, not one.
        assert_eq!(line_starts("a\r\rb"), vec![0, 2, 3]);
    }

    #[test]
    fn bare_cr_source_gets_real_line_numbers() {
        // Classic-Mac terminators: zero \n in the whole input. Decoded
        // text is byte-identical to the input:
        //   あ 0..3, \r 3..4, ［＃改ページ］ 4..25 (7 fullwidth chars ×
        //   3 bytes), \r 25..26, い 26..29, \r 29..30 (30 bytes).
        // line_starts = [0, 4, 26, 30]. Sanitize rewrites each lone \r
        // to \n WIDTH-EQUAL (1 byte → 1 byte), so sanitized-body offsets
        // are numerically identical to decoded offsets and the map edits
        // all carry delta 0 — the interesting part is purely the line
        // index, which the old \n-only implementation computed as 1
        // everywhere for this input.
        let src = "あ\r［＃改ページ］\rい\r";
        let doc: Value =
            serde_json::from_slice(&aat_json_from_bytes(src.as_bytes()).unwrap()).unwrap();
        let spans: Vec<&Value> = collect_spans(&doc["blocks"]);
        assert!(!spans.is_empty());
        // First text node "あ\n" ← decoded "あ\r" = bytes 0..4, line 1.
        assert_eq!(spans[0]["byte_start"], 0);
        assert_eq!(spans[0]["byte_end"], 4);
        assert_eq!(spans[0]["line_start"], 1);
        assert_eq!(spans[0]["line_end"], 1);
        // The page-break directive: decoded bytes 4..25, real line 2.
        let page_break = spans
            .iter()
            .find(|s| s["byte_start"] == 4)
            .unwrap_or_else(|| panic!("no span at decoded byte_start 4: {spans:?}"));
        assert_eq!(page_break["byte_end"], 25);
        assert_eq!(page_break["line_start"], 2);
        // The trailing gap "\nい\n" ← decoded "\rい\r" = bytes 25..30:
        // starts on line 2 (the \r closing the directive line), ends on
        // line 3 (line_of(29), the い line).
        assert!(
            spans
                .iter()
                .any(|s| s["line_start"] == 2 && s["line_end"] == 3),
            "no span reaching real line 3: {spans:?}"
        );
    }

    fn collect_spans(v: &Value) -> Vec<&Value> {
        let mut out = Vec::new();
        match v {
            Value::Object(map) => {
                if let Some(span) = map.get("span") {
                    out.push(span);
                }
                for val in map.values() {
                    out.extend(collect_spans(val));
                }
            }
            Value::Array(items) => {
                for val in items {
                    out.extend(collect_spans(val));
                }
            }
            _ => {}
        }
        out
    }

    fn block_kinds(doc: &Value) -> Vec<String> {
        doc["blocks"]
            .as_array()
            .unwrap()
            .iter()
            .map(|b| b["kind"].as_str().unwrap().to_owned())
            .collect()
    }

    /// Parse `src` through the full `aat_json_from_bytes` path and return the
    /// resulting AAT `Value`.
    fn aat_value_for(src: &str) -> Value {
        serde_json::from_slice(&aat_json_from_bytes(src.as_bytes()).unwrap()).unwrap()
    }

    /// Depth-first search over `blocks`/`content`/`children` for the first
    /// node whose `"kind"` equals `kind`, or `None` if absent.
    fn find_node<'a>(v: &'a Value, kind: &str) -> Option<&'a Value> {
        match v {
            Value::Object(map) => {
                if map.get("kind").and_then(Value::as_str) == Some(kind) {
                    return Some(v);
                }
                for key in ["blocks", "content", "children"] {
                    if let Some(child) = map.get(key)
                        && let Some(found) = find_node(child, kind)
                    {
                        return Some(found);
                    }
                }
                None
            }
            Value::Array(items) => items.iter().find_map(|item| find_node(item, kind)),
            _ => None,
        }
    }

    /// Like [`find_node`], but panics if no matching node is found.
    fn find_first_node<'a>(v: &'a Value, kind: &str) -> &'a Value {
        find_node(v, kind).unwrap_or_else(|| panic!("no {kind:?} node found in {v}"))
    }

    #[test]
    fn ruby_emission_uses_structured_entries_right_parity() {
        let aat = aat_value_for("｜漢字《かんじ》\n");
        let ruby = find_first_node(&aat, "ruby");
        assert_eq!(ruby["base"], "漢字");
        assert_eq!(ruby["reading"], "かんじ");
        assert_eq!(ruby["direction"], "right");
    }

    #[test]
    fn left_ruby_emits_direction_left() {
        let aat = aat_value_for("名［＃「名」の左に「な」のルビ］\n");
        let ruby = find_first_node(&aat, "ruby");
        assert_eq!(ruby["direction"], "left");
        assert_eq!(ruby["base"], "名");
        assert_eq!(ruby["reading"], "な");
    }

    /// Gaiji-base ruby (`※［＃…］《reading》`): the base is a deferred gaiji
    /// (`ab-aozora-pipeline`'s `try_ruby_over_gaiji_base`), which becomes a
    /// `Content::Segments` base — `content_range_as_plain` returns `None`
    /// for it, so `ruby_entries` has no entry for this node's span and
    /// `ruby_node` falls through to the `RUBY_RE` regex path. This asserts
    /// that fallback keeps v1's typed emission (byte-identical `base`,
    /// `direction: "right"`) rather than silently downgrading to a `raw`
    /// node — see the `RUBY_RE` doc comment and Task 4's fix-wave concern
    /// 1 (delta-audit ruby class requires byte-identical typed ruby).
    #[test]
    fn gaiji_base_ruby_keeps_v1_typed_emission() {
        let src = "※［＃「木＋吶のつくり」、第3水準1-85-54］《かい》\n";
        let aat = aat_value_for(src);
        let ruby = find_first_node(&aat, "ruby");
        assert_eq!(ruby["kind"], "ruby");
        assert_eq!(ruby["direction"], "right");
        assert_eq!(ruby["base"], "※［＃「木＋吶のつくり」、第3水準1-85-54］");
        assert_eq!(ruby["reading"], "かい");
    }

    #[test]
    fn keigakomi_container_classifies_as_block() {
        let src = "前文\n［＃ここから罫囲み］\n中身\n［＃ここで罫囲み終わり］\n後文\n";
        let doc: Value =
            serde_json::from_slice(&aat_json_from_bytes(src.as_bytes()).unwrap()).unwrap();
        assert_eq!(
            block_kinds(&doc),
            ["paragraph", "keigakomi_block", "paragraph"]
        );
        let block = &doc["blocks"][1];
        assert!(block.get("span").is_none() && block.get("indent").is_none());
        let children = block["children"].as_array().unwrap();
        assert_eq!(children[0]["kind"], "paragraph");
        let text: String = children
            .iter()
            .flat_map(|c| c["content"].as_array().unwrap())
            .filter_map(|n| n["value"].as_str())
            .collect();
        assert!(text.contains("中身"));
        // No raw container markers survive inside or around the block.
        assert!(!serde_json::to_string(&doc).unwrap().contains("罫囲み］"));
    }

    #[test]
    fn yokogumi_container_classifies_as_block() {
        let src = "［＃ここから横組み］\nＡＢＣ\n［＃ここで横組み終わり］\n";
        let doc: Value =
            serde_json::from_slice(&aat_json_from_bytes(src.as_bytes()).unwrap()).unwrap();
        assert!(block_kinds(&doc).contains(&"yokogumi_block".to_owned()));
    }

    #[test]
    fn unpaired_keigakomi_open_stays_raw() {
        let src = "前\n［＃ここから罫囲み］\n中身\n";
        let doc: Value =
            serde_json::from_slice(&aat_json_from_bytes(src.as_bytes()).unwrap()).unwrap();
        assert!(!block_kinds(&doc).contains(&"keigakomi_block".to_owned()));
        assert!(
            serde_json::to_string(&doc)
                .unwrap()
                .contains("containerOpen")
        );
    }

    #[test]
    fn intervening_container_open_aborts_keigakomi_pairing() {
        let src = "［＃ここから罫囲み］\n［＃ここから２字下げ］\nａ\n［＃ここで字下げ終わり］\n［＃ここで罫囲み終わり］\n";
        let doc: Value =
            serde_json::from_slice(&aat_json_from_bytes(src.as_bytes()).unwrap()).unwrap();
        let kinds = block_kinds(&doc);
        assert!(
            !kinds.contains(&"keigakomi_block".to_owned()),
            "pairing must abort: {kinds:?}"
        );
        assert!(kinds.contains(&"jisage_block".to_owned()));
    }

    #[test]
    fn jizume_open_chars_recognizes_standalone_and_compound() {
        assert_eq!(jizume_open_chars("［＃ここから２３字詰め］"), Some(23));
        assert_eq!(jizume_open_chars("［＃ここから６字下げ、折り返して７字下げ、２１字詰め］"), Some(21));
        assert_eq!(jizume_open_chars("［＃ここから２字下げ］"), None);
        assert_eq!(jizume_open_chars("［＃ここで字詰め終わり］"), None);
        assert!(is_jizume_close("［＃ここで字詰め終わり］"));
        assert!(!is_jizume_close("［＃ここで字下げ終わり］"));
    }

    #[test]
    fn paired_jizume_emits_jizume_block() {
        let aat = aat_value_for("［＃ここから２１字詰め］\n本文\n［＃ここで字詰め終わり］\n");
        let block = find_first_node(&aat, "jizume_block");
        assert_eq!(block["width"], 21);
        assert_eq!(block["children"].as_array().unwrap().len(), 1);
    }

    #[test]
    fn unpaired_jizume_open_stays_raw() {
        let aat = aat_value_for("［＃ここから２１字詰め］\n本文\n");
        assert!(find_node(&aat, "jizume_block").is_none());
        // the open survives as a raw containerOpen node — zero silent drops
        assert!(
            serde_json::to_string(&aat)
                .unwrap()
                .contains("containerOpen")
        );
    }

    #[test]
    fn compound_jisage_jizume_nests_jizume_block() {
        // The compound container still classifies as burasage (6,7) — the
        // ２１字詰め clause now additionally wraps that classified output in
        // a jizume_block instead of leaving it unemitted.
        assert_eq!(
            burasage_open_indent("［＃ここから６字下げ、折り返して７字下げ、２１字詰め］"),
            Some((6, 7))
        );
        let aat = aat_value_for(
            "［＃ここから６字下げ、折り返して７字下げ、２１字詰め］\n本文\n［＃ここで字下げ終わり］\n",
        );
        let jizume = find_first_node(&aat, "jizume_block");
        assert_eq!(jizume["width"], 21);
        // children carry the burasage classification exactly as before, now typed (6,7)
        let style = find_first_node(jizume, "style");
        assert_eq!(style["indent_first"], 6);
        assert_eq!(style["indent_rest"], 7);
    }

    #[test]
    fn compound_jizume_boundary_fallback_still_wraps() {
        // Compound container with jizume width (21) but NO ［＃ここで字下げ終わり］
        // close — a following container open (罫囲み) triggers the boundary-fallback
        // arm, which finds the next container marker and classifies what's between.
        // The burasage classification still nests inside the jizume_block (6,7,21).
        let aat = aat_value_for(
            "［＃ここから６字下げ、折り返して７字下げ、２１字詰め］\n本文\n［＃ここから罫囲み］\nX\n［＃ここで罫囲み終わり］\n",
        );
        // 1. A jizume_block exists with width 21
        let jizume = find_first_node(&aat, "jizume_block");
        assert_eq!(jizume["width"], 21);
        // 2. Inside it, a style node with style_type "burasage", indent_first 6, indent_rest 7
        let style = find_first_node(jizume, "style");
        assert_eq!(style["style_type"], "burasage");
        assert_eq!(style["indent_first"], 6);
        assert_eq!(style["indent_rest"], 7);
        // 3. A keigakomi_block also exists at top level
        let keigakomi = find_first_node(&aat, "keigakomi_block");
        assert_eq!(keigakomi["kind"], "keigakomi_block");
    }

    #[test]
    fn jisage_block_emits_typed_indent() {
        let aat = aat_value_for("［＃ここから２字下げ］\n本文\n［＃ここで字下げ終わり］\n");
        let block = find_first_node(&aat, "jisage_block");
        assert_eq!(block["indent"], 2);
        assert!(block.get("x-indent").is_none());
    }

    #[test]
    fn chitsuki_style_emits_typed_align_offset() {
        let aat = aat_value_for("本文［＃地から２字上げ］\n");
        let style = find_first_node(&aat, "style");
        assert_eq!(style["align"], "right");
        assert_eq!(style["offset_from_end"], 2);
        assert!(style.get("x-align").is_none() && style.get("x-offset").is_none());
        assert_eq!(style["x-provenance"], "source-derived"); // provenance retained
    }

    #[test]
    fn burasage_style_emits_typed_first_rest() {
        // Pinned (6,7) compound input — same source string exercised by
        // `compound_jisage_jizume_nests_jizume_block`.
        let src = "［＃ここから６字下げ、折り返して７字下げ、２１字詰め］\nあ\n［＃ここで字下げ終わり］\n";
        let aat = aat_value_for(src);
        let style = find_first_node(&aat, "style");
        assert_eq!(style["indent_first"], 6);
        assert_eq!(style["indent_rest"], 7);
        assert!(style.get("x-indent-first").is_none() && style.get("x-indent-rest").is_none());
        assert_eq!(style["x-provenance"], "source-derived");
    }

    #[test]
    fn heading_emits_typed_indent_when_indented() {
        // Same indented-heading line as `full-markup-utf8.txt` line 9.
        let aat = aat_value_for("［＃５字下げ］一［＃「一」は中見出し］\n");
        let heading = find_first_node(&aat, "heading");
        assert_eq!(heading["indent"], 5);
        assert!(heading.get("x-indent").is_none());
    }

    #[test]
    fn c3_identity_join_key_and_document_version() {
        assert!(
            adapter_version().starts_with("ab-aozora 0.4.0 aat-schema 2 facade 0.3.0 wire-schema 3")
        );
        let aat = aat_value_for("あ\n");
        assert_eq!(aat["version"], 2);
    }
}
