//! AAT (Aozora AST Transform) projection of native source interpretation.

use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Write as _,
    mem,
    ops::Range,
    str,
    sync::LazyLock,
};

use ab_aozora_pipeline::lexer::sanitize::{SanitizeMaps, sanitize_mapped};
use ab_aozora_pipeline::text_variant::{TextVariantTarget, text_variant};
use ab_aozora_pipeline::{LexOutput, Pipeline};
use anyhow::Result;
use encoding_rs::SHIFT_JIS;
use regex::Regex;
use serde_json::{Value, json};

use sha2::{Digest, Sha256};

use ab_aozora_facade::{
    self, BoutenPosition, Diagnostic, DirectiveKind, ForwardAttr, Node, NodeKind, NodeRef,
    RegionClose, RegionFormat, Severity, encoding, json as aozora_json,
};
// Body/tail boundary detection is the shared `ab-source-syntax` authority
// so the checker's comparison source (`ab-check::body_text`) can never
// drift from the parser's own cut.
use ab_aozora_facade::syntax::AbsoluteSize;
use ab_source_syntax::{RegionError, SourceRegions, aozora_body_range};

/// Sanitization and parsing share the native diagnostic type.
pub type AozoraSanitizeDiagnostic = Diagnostic;

/// Fallback for ruby nodes with no resolvable `ruby_entries` entry
/// (gaiji-base ruby, `※［＃…］《reading》`, whose base is
/// `Content::Segments` and so has no plain-text range to resolve — see
/// `ruby_node`'s fallback branch). The captured base is then enriched from
/// the gaiji scan: a base that is exactly one gaiji marker emits the
/// resolved glyph as `base` plus a structured `base_content` gaiji node,
/// instead of v1's verbatim marker text (which leaked `※［＃…］` into
/// every downstream text projection and hid the gaiji from
/// `gaiji_resolution` accounting).
static RUBY_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^｜?(?P<base>.+?)《(?P<reading>[^》]+)》$").unwrap());

/// Decoded source text and its byte-coordinate basis.
#[derive(Debug)]
pub struct DecodedSource {
    /// Decoded text.
    pub text: String,
    /// Decoded text with sanitization for span alignment.
    pub span_text: String,
    /// Complete sanitized text before the body/tail projection.
    ///
    /// `pub` (not `pub(crate)`) because the classified-source capture reader
    /// now lives in the `ab-aozora-capture` crate; it aligns spans against
    /// this field (see `classified_source::…`). Nothing inside `ab-aozora-aat`
    /// reads it, so `pub(crate)` would be both dead code here and unreachable
    /// from the capture crate.
    pub sanitized_text: String,
    /// The encoding that was successfully decoded (utf-8, utf-8-bom, windows-31j, or lossy variant).
    pub encoding: &'static str,
    /// Hex-encoded SHA256 hash of the input bytes.
    pub source_hash: String,
    /// Sanitize-stage diagnostics (accent decomposition notes) — born
    /// BEFORE the parse; the parse of rewritten text cannot rediscover
    /// them. Spans are full-sanitized-text byte offsets.
    pub sanitize_diagnostics: Vec<AozoraSanitizeDiagnostic>,
    /// Sanitized source apparatus from `aozora_body_range`'s tail-start line,
    /// including an explicit body-end marker when present. Starts at the
    /// boundary line rather than the preceding blank lines trimmed from the
    /// body. Empty when no source tail boundary is recognized.
    pub sanitized_tail: String,
    /// Byte offset of `sanitized_tail`'s start within the full SANITIZED
    /// text (same coordinate system `sanitize_diagnostics` spans use —
    /// composes through `span_ctx`'s `maps` directly, with NO
    /// `body_offset` added, exactly like `diagnostics_json_from_bytes`'s
    /// sanitize-stage rebase).
    pub tail_offset: usize,
    /// Sanitize offset maps + body offset + line index for span rebasing
    /// for full decoded-source offsets and real 1-based line numbers.
    /// Sanitized→decoded span-mapping context. `pub` so the extracted
    /// classified-source capture crate can rebase spans (see `SpanContext`).
    pub span_ctx: SpanContext,
}

impl DecodedSource {
    /// The three declared source regions, in DECODED-text coordinates.
    ///
    /// `aozora_body_range` runs over the *sanitized* text, so its boundaries
    /// cannot be used verbatim: they are mapped back through `span_ctx` here.
    /// That is the whole reason this derivation lives beside the mapping
    /// rather than in `ab-source-syntax` — `SourceRegions::derive` is correct
    /// only for a caller that has no sanitize map to compose through, and this
    /// crate always does.
    ///
    /// The body is `span_text` seen in decoded coordinates: its start is the
    /// body-relative offset 0 mapped forward, and its end is `span_text.len()`
    /// mapped forward. Everything before is header and everything after is
    /// tail, so the three regions close the decoded file.
    ///
    /// # Errors
    ///
    /// Returns [`RegionError`] when the mapped boundaries do not partition the
    /// decoded text — which would mean the sanitize map and the body range
    /// disagree, and is a fail-closed condition rather than something to
    /// round off.
    pub fn source_regions(&self) -> Result<SourceRegions, RegionError> {
        let body_start = self.span_ctx.to_decoded(0);
        let body_end = self.span_ctx.to_decoded_end(self.span_text.len());
        SourceRegions::declare(&self.text, body_start, body_end)
    }
}

/// Composition chain for translating a parser/sanitize-stage byte offset
/// into `DecodedSource.text` coordinates plus a real line number.
///
/// Two offset systems feed span emission: the parser (and AAT node
/// construction) work in `span_text` (sanitized-body-relative) offsets;
/// `sanitize_diagnostics` carry full-sanitized-text offsets. Both compose
/// through `maps` (sanitized → decoded `text`); the former additionally
/// needs `body_offset` added first (body-relative → full-sanitized).
///
/// `pub` (with `pub` `maps`/`body_offset`/`to_decoded`) because the
/// classified-source capture reader now lives in the `ab-aozora-capture`
/// crate and composes sanitized→decoded offsets through this context. The
/// remaining members stay private (used only within this crate).
#[derive(Debug)]
pub struct SpanContext {
    /// Sanitized→decoded byte-offset maps.
    pub maps: SanitizeMaps,
    /// Byte offset of the body slice within the SANITIZED text.
    pub body_offset: usize,
    /// Byte offsets of line starts in the DECODED text (`text`).
    line_starts: Vec<usize>,
}

impl SpanContext {
    /// Map a body-relative sanitized offset to its DECODED-text offset.
    #[must_use]
    pub fn to_decoded(&self, body_offset: usize) -> usize {
        self.maps.to_source_offset(body_offset + self.body_offset)
    }

    /// Map a body-relative sanitized end offset to its DECODED-text offset.
    #[must_use]
    pub fn to_decoded_end(&self, body_end: usize) -> usize {
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
/// bare-CR source (zero `\n` in the whole file), losing its
/// real line numbers. Counting `\r\n` as one boundary keeps CRLF
/// sources' boundary set byte-identical to a `\n`-only index (the
/// boundary sits after the pair, exactly where `\n`+1 put it), and
/// treating lone `\r` as a terminator mirrors sanitize's lone-`\r`→`\n`
/// rewrite, so decoded-source lines and sanitized-text lines agree in
/// count for every input.
fn line_starts(text: &str) -> Vec<usize> {
    let mut starts = vec![0];
    starts.extend(terminator_ends(text));
    starts
}

/// Byte offsets immediately after every line terminator in `text`,
/// treating `\n`, `\r\n` (one boundary, after the pair), and bare `\r` as
/// boundaries — the exact rule `line_starts` is built from (factored out
/// so the tail line-splitting below shares it rather than re-deriving a
/// different one; see `line_starts`'s doc comment for why bare `\r` must
/// count).
fn terminator_ends(text: &str) -> Vec<usize> {
    let bytes = text.as_bytes();
    let mut ends = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b'\n' => {
                i += 1;
                ends.push(i);
            }
            b'\r' => {
                i += if bytes.get(i + 1) == Some(&b'\n') {
                    2
                } else {
                    1
                };
                ends.push(i);
            }
            _ => i += 1,
        }
    }
    ends
}

/// Terminator-INCLUSIVE `(start, end)` byte ranges of every line in
/// `text`, built from the same boundary set as `line_starts`/
/// `terminator_ends`. Unlike `split_inclusive('\n')` (which only splits on
/// `\n`, missing bare-CR tails — see the `line_starts` doc comment),
/// this honors `\n`, `\r\n`, and bare `\r`. A final line lacking a
/// terminator (the text doesn't end in one) still yields one last range
/// ending at `text.len()`.
///
/// Other Unicode separators remain text, matching the sanitize stage.
fn line_ranges(text: &str) -> Vec<Range<usize>> {
    let mut out = Vec::new();
    let mut start = 0;
    for end in terminator_ends(text) {
        out.push(start..end);
        start = end;
    }
    if start < text.len() {
        out.push(start..text.len());
    }
    out
}

/// Terminator-inclusive per-line ranges over a source-gap slice, with
/// terminator-only lines (blank lines) coalesced into the preceding range so
/// a blank run stays with the paragraph it ends rather than forming a
/// whitespace-only paragraph of its own. A gap that begins with a terminator
/// keeps that terminator as its own leading range: it ends the line the
/// PREVIOUS inline node (e.g. a line-final ruby) sits on, and the paragraph
/// accumulator attaches it there.
fn paragraph_segments(source: &str) -> Vec<Range<usize>> {
    let mut out: Vec<Range<usize>> = Vec::new();
    for range in line_ranges(source) {
        let blank = source[range.clone()]
            .chars()
            .all(|ch| ch == '\n' || ch == '\r');
        match out.last_mut() {
            Some(previous) if blank => previous.end = range.end,
            _ => out.push(range),
        }
    }
    out
}

#[derive(Debug, Clone, Copy)]
struct Span {
    start: usize,
    end: usize,
}

impl From<ab_aozora_facade::Span> for Span {
    fn from(span: ab_aozora_facade::Span) -> Self {
        Self {
            start: span.start as usize,
            end: span.end as usize,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ProjectedKind {
    Node(NodeKind),
    Format(ForwardAttr),
    Region(RegionFormat),
    RegionClose(RegionClose),
    Directive(DirectiveKind),
    TextVariant {
        target: TextVariantTarget,
        current: String,
        base_text: String,
    },
    QuoteOpen,
    QuoteClose,
    RecoveredSource,
}

impl ProjectedKind {
    fn as_str(&self) -> &'static str {
        match self {
            Self::Node(kind) => kind.as_json_tag(),
            Self::Region(_) => "containerOpen",
            Self::RegionClose(_) => "containerClose",
            Self::Format(ForwardAttr::Bouten { .. }) => "bouten",
            Self::Format(ForwardAttr::CombineUpright) => "combineUpright",
            Self::Format(_) => "emphasis",
            Self::Directive(DirectiveKind::WarichuOpen) => "warichuOpen",
            Self::Directive(DirectiveKind::WarichuClose) => "warichuClose",
            Self::Directive(_) | Self::TextVariant { .. } => "directive",
            Self::QuoteOpen => "angleQuoteOpen",
            Self::QuoteClose => "angleQuoteClose",
            Self::RecoveredSource => "unparsed-source-gap",
        }
    }
}

#[derive(Debug, Clone)]
struct AozoraNode {
    kind: ProjectedKind,
    span: Span,
    marker_span: Option<Span>,
}

type AozoraGaiji = encoding::gaiji::GaijiResolution;

#[derive(Debug, Clone)]
struct AozoraRubyEntry {
    span: Span,
    base: String,
    reading: String,
    side: &'static str,
}

fn node_projection(tree: &LexOutput) -> Vec<AozoraNode> {
    let pairs: BTreeMap<usize, _> = tree
        .pairs
        .iter()
        .map(|pair| {
            (
                usize::try_from(pair.close.end).expect("source offset fits usize"),
                pair,
            )
        })
        .collect();
    tree.source_nodes
        .iter()
        .map(|source_node| {
            let kind = match source_node.node {
                NodeRef::Inline(Node::Format(format))
                | NodeRef::BlockLeaf(Node::Format(format)) => ProjectedKind::Format(format.attr),
                NodeRef::BlockOpen(region) => ProjectedKind::Region(region),
                NodeRef::BlockClose(close) => ProjectedKind::RegionClose(close),
                NodeRef::Inline(Node::Directive(directive))
                | NodeRef::BlockLeaf(Node::Directive(directive)) => {
                    ProjectedKind::Directive(directive.kind)
                }
                node => ProjectedKind::Node(node.kind()),
            };
            let span: Span = source_node.source_span.into();
            let kind = if matches!(
                kind,
                ProjectedKind::Directive(DirectiveKind::BaseTextVariant | DirectiveKind::Unknown)
            ) {
                text_variant(&tree.sanitized[span.start..span.end]).map_or(kind, |variant| {
                    ProjectedKind::TextVariant {
                        target: variant.target,
                        current: variant.current.to_owned(),
                        base_text: variant.base_text.to_owned(),
                    }
                })
            } else {
                kind
            };
            // Classifier consume ranges end at their owned delimiter, even when
            // retrospective formatting also reclaims preceding target text.
            let marker_kind = match source_node.node {
                NodeRef::Inline(Node::Format(format))
                | NodeRef::BlockLeaf(Node::Format(format))
                    if format.origin != ab_aozora_facade::ForwardOrigin::Detached =>
                {
                    Some(ab_aozora_facade::PairKind::Bracket)
                }
                NodeRef::Inline(Node::Ruby(_)) | NodeRef::BlockLeaf(Node::Ruby(_)) => {
                    Some(ab_aozora_facade::PairKind::Ruby)
                }
                _ => None,
            };
            let mut marker_span = pairs
                .get(&span.end)
                .filter(|pair| {
                    Some(pair.kind) == marker_kind
                        && usize::try_from(pair.open.start).expect("source offset fits usize")
                            >= span.start
                })
                .map(|pair| Span {
                    start: usize::try_from(pair.open.start).expect("source offset fits usize"),
                    end: usize::try_from(pair.close.end).expect("source offset fits usize"),
                });
            if marker_kind == Some(ab_aozora_facade::PairKind::Ruby)
                && tree.sanitized[span.start..span.end].starts_with('｜')
                && marker_span.is_some()
            {
                marker_span = Some(span);
            }
            AozoraNode {
                kind,
                span,
                marker_span,
            }
        })
        .chain(tree.classified_source_facts.iter().filter_map(|fact| {
            let span: Span = fact.source_span.into();
            (fact.construct_id == ab_aozora_pipeline::ConstructId::RecoveredVerbatim
                && contains_aozora_markup(&tree.sanitized[span.start..span.end]))
            .then_some(AozoraNode {
                kind: ProjectedKind::RecoveredSource,
                span,
                marker_span: None,
            })
        }))
        .collect()
}

fn ruby_projection(tree: &LexOutput) -> Result<Vec<AozoraRubyEntry>> {
    let store = &tree.store;
    let mut entries = Vec::new();
    for source_node in &tree.source_nodes {
        let (NodeRef::Inline(Node::Ruby(ruby)) | NodeRef::BlockLeaf(Node::Ruby(ruby))) =
            source_node.node
        else {
            continue;
        };
        let (Some(base), Some(reading)) = (
            store.content_range_as_plain(ruby.base),
            store.content_range_as_plain(ruby.reading),
        ) else {
            continue;
        };
        let side = match ruby.side {
            ab_aozora_facade::RubySide::Left => "left",
            ab_aozora_facade::RubySide::Right => "right",
            _ => anyhow::bail!("unhandled ruby side: {:?}", ruby.side),
        };
        entries.push(AozoraRubyEntry {
            span: source_node.source_span.into(),
            base: base.to_owned(),
            reading: reading.to_owned(),
            side,
        });
    }
    Ok(entries)
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
        let sanitized = sanitize_for_aat(&text);
        let span_ctx = SpanContext {
            maps: sanitized.maps,
            body_offset: sanitized.body_offset,
            line_starts: line_starts(&text),
        };
        return Ok(DecodedSource {
            text,
            span_text: sanitized.body,
            sanitized_text: sanitized.full,
            encoding: "utf-8-bom",
            source_hash,
            sanitize_diagnostics: sanitized.diagnostics,
            sanitized_tail: sanitized.tail,
            tail_offset: sanitized.tail_offset,
            span_ctx,
        });
    }
    if let Ok(text) = str::from_utf8(bytes) {
        let text = text.to_owned();
        let sanitized = sanitize_for_aat(&text);
        let span_ctx = SpanContext {
            maps: sanitized.maps,
            body_offset: sanitized.body_offset,
            line_starts: line_starts(&text),
        };
        return Ok(DecodedSource {
            text,
            span_text: sanitized.body,
            sanitized_text: sanitized.full,
            encoding: "utf-8",
            source_hash,
            sanitize_diagnostics: sanitized.diagnostics,
            sanitized_tail: sanitized.tail,
            tail_offset: sanitized.tail_offset,
            span_ctx,
        });
    }
    let (cow, _, had_errors) = SHIFT_JIS.decode(bytes);
    let text = cow.into_owned();
    let sanitized = sanitize_for_aat(&text);
    let span_ctx = SpanContext {
        maps: sanitized.maps,
        body_offset: sanitized.body_offset,
        line_starts: line_starts(&text),
    };
    Ok(DecodedSource {
        text,
        span_text: sanitized.body,
        sanitized_text: sanitized.full,
        encoding: if had_errors {
            "windows-31j-lossy"
        } else {
            "windows-31j"
        },
        source_hash,
        sanitize_diagnostics: sanitized.diagnostics,
        sanitized_tail: sanitized.tail,
        tail_offset: sanitized.tail_offset,
        span_ctx,
    })
}

/// Intermediate result of the sanitize + body/tail split stage — a named
/// struct rather than a wide tuple so `decode_source_bytes`'s three
/// branches read as field access, not positional unpacking (added
/// the tail fields; a tuple would have grown to six positional slots).
struct SanitizedForAat {
    /// The BODY slice (`sanitized[body_range]`), same content
    /// `sanitize_for_aat` always returned as its first element.
    body: String,
    full: String,
    diagnostics: Vec<AozoraSanitizeDiagnostic>,
    maps: SanitizeMaps,
    /// `body_range.start`, sanitized coordinates.
    body_offset: usize,
    /// The TAIL slice (`sanitized[tail_start..]`) — see
    /// `DecodedSource::sanitized_tail`'s doc comment for why this is
    /// anchored on the `底本：` line, not `body_range.end`.
    tail: String,
    /// `tail_start`, sanitized coordinates.
    tail_offset: usize,
}

fn sanitize_for_aat(text: &str) -> SanitizedForAat {
    let mapped = sanitize_mapped(text);
    let sanitize_diagnostics = mapped.diagnostics;
    let sanitized = mapped.text.into_owned();
    let (body, tail_start) = aozora_body_range(&sanitized);
    let body_text = sanitized[body.clone()].to_owned();
    let tail = sanitized[tail_start..].to_owned();
    SanitizedForAat {
        body: body_text,
        full: sanitized,
        diagnostics: sanitize_diagnostics,
        maps: mapped.maps,
        body_offset: body.start,
        tail,
        tail_offset: tail_start,
    }
}

#[allow(
    clippy::type_complexity,
    reason = "one tuple per typed projection consumed by build_aat"
)]
fn projections(
    span_text: &str,
) -> Result<(
    Vec<AozoraNode>,
    Vec<Diagnostic>,
    Vec<AozoraGaiji>,
    Vec<AozoraRubyEntry>,
    Vec<Span>,
)> {
    let paired = Pipeline::from_sanitized(span_text).tokenize().pair();
    let retained_accents = paired
        .retained_multiline_accent_spans()
        .into_iter()
        .map(|span| Span {
            start: span.start as usize,
            end: span.end as usize,
        })
        .collect();
    let tree = paired.build();
    let initial_nodes = node_projection(&tree);
    let diagnostics = tree.diagnostics.clone();
    let gaiji = encoding::gaiji::gaiji_resolutions(span_text);
    let mut ruby = ruby_projection(&tree)?;
    let mut nodes = Vec::new();
    let mut pending = initial_nodes;
    // The source-node projection exposes only outer nodes. Parse quote interiors
    // to recover ruby with exact source offsets; the worklist avoids recursive
    // stack growth, and every new interior is strictly smaller than its owner.
    while let Some(node) = pending.pop() {
        if node.kind != ProjectedKind::Node(NodeKind::AngleQuote) {
            nodes.push(node);
            continue;
        }
        let start = node.span.start + '≪'.len_utf8();
        let end = node.span.end - '≫'.len_utf8();
        nodes.push(AozoraNode {
            kind: ProjectedKind::QuoteOpen,
            marker_span: None,
            span: Span {
                start: node.span.start,
                end: start,
            },
        });
        nodes.push(AozoraNode {
            kind: ProjectedKind::QuoteClose,
            marker_span: None,
            span: Span {
                start: end,
                end: node.span.end,
            },
        });
        let inner_tree = Pipeline::from_sanitized(&span_text[start..end])
            .tokenize()
            .pair()
            .build();
        let inner_nodes = node_projection(&inner_tree);
        for mut inner in inner_nodes {
            inner.span.start += start;
            inner.span.end += start;
            if let Some(marker) = &mut inner.marker_span {
                marker.start += start;
                marker.end += start;
            }
            pending.push(inner);
        }
        let inner_ruby = ruby_projection(&inner_tree)?;
        for mut entry in inner_ruby {
            entry.span.start += start;
            entry.span.end += start;
            ruby.push(entry);
        }
    }
    nodes.sort_by_key(|node| (node.span.start, node.span.end));
    Ok((nodes, diagnostics, gaiji, ruby, retained_accents))
}

/// Transform Aozora source bytes into AAT JSON output.
///
/// # Errors
///
/// Returns an error if source decoding, projection parsing, or JSON serialization fails.
pub fn aat_json_from_bytes(bytes: &[u8]) -> Result<Vec<u8>> {
    let decoded = decode_source_bytes(bytes)?;
    let (nodes, diagnostics, gaiji, ruby, retained_accents) = projections(&decoded.span_text)?;
    let mut aat = build_aat(&decoded, &nodes, &diagnostics, &gaiji, &ruby);
    if !retained_accents.is_empty() {
        aat["meta"]["interpretation_problems"] = Value::Array(
            retained_accents
                .iter()
                .map(|span| {
                    let start = decoded.span_ctx.to_decoded(span.start);
                    let end = decoded.span_ctx.to_decoded_end(span.end);
                    json!({"kind":"uninterpreted-notation", "code":"uninterpreted-notation",
                "raw": &decoded.text[start..end],
                "source_span": {"coordinate_system":"decoded_utf8", "start":start, "end":end,
                    "line":decoded.span_ctx.line_of(start)},
                "aspects":["content","layout"], "influence":{"kind":"document"}})
                })
                .collect(),
        );
    }
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
/// Single owner of the diagnostics path: decoding,
/// sanitization, and body selection use `decode_source_bytes`, as for AAT.
/// The parser consumes that normalized body without sanitizing it again.
/// Entry order is sanitize-stage diagnostics, then parser diagnostics.
///
/// # Errors
///
/// Returns an error if source decoding, projection parsing, or JSON
/// serialization fails.
pub fn diagnostics_json_from_bytes(bytes: &[u8]) -> Result<Vec<u8>> {
    let decoded = decode_source_bytes(bytes)?;
    let tree = Pipeline::from_sanitized(&decoded.span_text)
        .tokenize()
        .pair()
        .build();
    let mut data = serde_json::to_value(aozora_json::diagnostic_entries(
        &decoded.sanitize_diagnostics,
    ))?;
    let mut parser_entries =
        serde_json::to_value(aozora_json::diagnostic_entries(&tree.diagnostics))?;
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
        "schemaVersion": aozora_json::DIAGNOSTICS_SCHEMA_VERSION,
        "data": data,
    });
    let mut out = Vec::new();
    serde_json::to_writer(&mut out, &envelope)?;
    out.push(b'\n');
    Ok(out)
}

#[derive(Clone, Copy)]
enum EstablishedInterpretation {
    Ruby,
    Gaiji,
    Emphasis,
    Warichu,
}

impl EstablishedInterpretation {
    fn kind(self) -> &'static str {
        match self {
            Self::Ruby => "ruby",
            Self::Gaiji => "gaiji",
            Self::Emphasis => "emphasis",
            Self::Warichu => "warichu",
        }
    }

    fn aspects(self) -> &'static [&'static str] {
        match self {
            Self::Ruby => &["content", "structure"],
            Self::Gaiji => &["content"],
            Self::Emphasis => &["layout"],
            Self::Warichu => &["structure", "layout"],
        }
    }
}

fn established_interpretations(blocks: &[Value]) -> Vec<Value> {
    let mut facts = Vec::new();
    let mut pending = blocks.iter().rev().collect::<Vec<_>>();
    while let Some(node) = pending.pop() {
        let interpretation = match node["kind"].as_str() {
            Some("ruby")
                if node["base"].as_str().is_some_and(|text| !text.is_empty())
                    && node["reading"]
                        .as_str()
                        .is_some_and(|text| !text.is_empty()) =>
            {
                Some(EstablishedInterpretation::Ruby)
            }
            Some("gaiji")
                if node["resolved"]
                    .as_str()
                    .is_some_and(|text| !text.is_empty()) =>
            {
                Some(EstablishedInterpretation::Gaiji)
            }
            Some("style")
                if matches!(
                    node["style_type"].as_str(),
                    Some(
                        "bold"
                            | "gothic"
                            | "italic"
                            | "superscript"
                            | "subscript"
                            | "bouten"
                            | "bosen"
                    )
                ) && node["content"]
                    .as_array()
                    .is_some_and(|children| !children.is_empty()) =>
            {
                Some(EstablishedInterpretation::Emphasis)
            }
            Some("font_size" | "small_script")
                if node["content"]
                    .as_array()
                    .is_some_and(|children| !children.is_empty()) =>
            {
                Some(EstablishedInterpretation::Emphasis)
            }
            Some("warichu") => Some(EstablishedInterpretation::Warichu),
            _ => None,
        };
        if let Some(interpretation) = interpretation {
            let spans = if node["kind"] == "gaiji" {
                node.get("span").into_iter().collect::<Vec<_>>()
            } else {
                node["interpretation_marker_spans"]
                    .as_array()
                    .map_or_else(Vec::new, |spans| spans.iter().collect())
            };
            for span in spans {
                if let (Some(start), Some(end)) =
                    (span["byte_start"].as_u64(), span["byte_end"].as_u64())
                    && start < end
                {
                    facts.push(json!({"kind":interpretation.kind(), "outcome":"established", "aspects":interpretation.aspects(),
                        "source_span":{"start":start,"end":end,"line":span["line_start"],"coordinate_system":"decoded_utf8"}}));
                }
            }
        }
        for key in [
            "children",
            "content",
            "upper",
            "lower",
            "base_content",
            "reading_content",
        ] {
            if let Some(children) = node.get(key).and_then(Value::as_array) {
                pending.extend(children.iter().rev());
            }
        }
    }
    facts.sort_by_key(|fact| {
        (
            fact["source_span"]["start"].as_u64(),
            fact["source_span"]["end"].as_u64(),
        )
    });
    facts
}

fn build_aat(
    decoded: &DecodedSource,
    nodes: &[AozoraNode],
    diagnostics: &[Diagnostic],
    gaiji: &[AozoraGaiji],
    ruby: &[AozoraRubyEntry],
) -> Value {
    let gaiji_by_start = gaiji
        .iter()
        .map(|entry| (entry.start, entry.clone()))
        .collect::<BTreeMap<_, _>>();
    let ruby_by_span = ruby
        .iter()
        .map(|entry| ((entry.span.start, entry.span.end), entry.clone()))
        .collect::<BTreeMap<_, _>>();
    // Bare-toggle pairing runs after block classification:
    // `blocks_from_inline_content` must see the original node
    // stream — raw toggle markers included — so paragraph/jizume
    // segmentation matches the no-pass baseline; adoption then rewrites
    // only the toggle spans inside the built tree.
    let mut blocks = pair_bare_toggles_in_blocks(blocks_from_inline_content(
        inline_content(decoded, nodes, &gaiji_by_start, &ruby_by_span),
        &decoded.text,
    ));
    let mut warnings = diagnostics
        .iter()
        .map(|diagnostic| diagnostic_warning(diagnostic, &decoded.span_ctx))
        .collect::<Vec<_>>();
    let (source_notes, tail_warnings) = source_notes_from_tail(decoded);
    blocks.extend(source_notes);
    warnings.extend(tail_warnings);
    let facts = established_interpretations(&blocks);
    let mut aat = json!({
        "version": 2,
        "work_id": "stdin",
        "blocks": blocks,
        "meta": {
            "adapter": "ab-aozora",
            "adapter_version": adapter_version(),
            "source_encoding": decoded.encoding,
            "primary_text_hash": decoded.source_hash.clone(),
            "source_hash": decoded.source_hash,
            "parse_complete": diagnostics.iter().all(|d| matches!(d.severity(), Severity::Warning | Severity::Note)),
            "warnings": warnings
        }
    });
    if !facts.is_empty() {
        aat["meta"]["interpretation_facts"] = json!(facts);
    }
    aat
}

/// Tail classification separates source attribution from transcription metadata.
/// Unclassified nonblank lines remain colophon data and produce a warning.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TailLineClass {
    TerminalProvenance,
    Colophon,
    Blank,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TailState {
    Provenance,
    Colophon,
}

const PROVENANCE_HEADS: [&str; 3] = ["底本：", "底本の親本：", "翻訳の底本："];

const COLOPHON_HEADS: [&str; 4] = ["入力：", "校正：", "青空文庫作成ファイル：", "※"];

/// Nonblank continuation lines inherit the preceding head's class; blanks do
/// not change that state. Lines before any head remain colophon data, with
/// their indices returned for `tail-line-unclassified` warnings. This preserves
/// arbitrary input without assigning unsupported source-attribution authority.
fn classify_tail(lines: &[&str]) -> (Vec<TailLineClass>, Vec<usize>) {
    let mut state: Option<TailState> = None;
    let mut classes = Vec::with_capacity(lines.len());
    let mut unclassifiable = Vec::new();
    for (index, line) in lines.iter().enumerate() {
        let stripped = line.trim();
        if stripped.is_empty() {
            classes.push(TailLineClass::Blank);
            continue;
        }
        if stripped == "［＃本文終わり］" {
            state = Some(TailState::Colophon);
            classes.push(TailLineClass::Colophon);
            continue;
        }
        if PROVENANCE_HEADS
            .iter()
            .any(|head| stripped.starts_with(head))
        {
            state = Some(TailState::Provenance);
            classes.push(TailLineClass::TerminalProvenance);
        } else if COLOPHON_HEADS.iter().any(|head| stripped.starts_with(head)) {
            state = Some(TailState::Colophon);
            classes.push(TailLineClass::Colophon);
        } else {
            match state {
                Some(TailState::Provenance) => classes.push(TailLineClass::TerminalProvenance),
                Some(TailState::Colophon) => classes.push(TailLineClass::Colophon),
                None => {
                    classes.push(TailLineClass::Colophon);
                    unclassifiable.push(index);
                }
            }
        }
    }
    (classes, unclassifiable)
}

/// Byte-offset span of a tail-relative range, composed exactly like
/// `diagnostics_json_from_bytes`'s sanitize-stage rebase (lib.rs's
/// `rebase_spans` call with `maps.to_source_offset`/`to_source_end`
/// directly — NO `body_offset` added, since `tail_offset` is already a
/// full-sanitized-text coordinate, the same coordinate system
/// `sanitize_diagnostics` spans use).
fn tail_span_json(range: &Range<usize>, tail_offset: usize, ctx: &SpanContext) -> Value {
    let byte_start = ctx.maps.to_source_offset(tail_offset + range.start);
    let byte_end = ctx.maps.to_source_end(tail_offset + range.end);
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

/// Group nonblank tail lines into source notes. Attribution and colophon keep
/// distinct region classes; blanks and class changes end a group. Content
/// values and spans retain the source lines, including their terminators.
fn source_notes_from_tail(decoded: &DecodedSource) -> (Vec<Value>, Vec<Value>) {
    if decoded.sanitized_tail.is_empty() {
        return (Vec::new(), Vec::new());
    }
    let ranges = line_ranges(&decoded.sanitized_tail);
    let lines = ranges
        .iter()
        .map(|range| &decoded.sanitized_tail[range.clone()])
        .collect::<Vec<_>>();
    let (classes, unclassifiable) = classify_tail(&lines);
    let warnings = unclassifiable
        .iter()
        .map(|&index| {
            json!({
                "code": "tail-line-unclassified",
                "severity": "warning",
                "message": lines[index].trim_end_matches(['\n', '\r']),
                "span": tail_span_json(&ranges[index], decoded.tail_offset, &decoded.span_ctx)
            })
        })
        .collect::<Vec<_>>();
    let mut blocks = Vec::new();
    let mut group = Vec::new();
    let mut group_region = None;
    for index in 0..classes.len() {
        let region = match classes[index] {
            TailLineClass::TerminalProvenance => Some("terminal_provenance"),
            TailLineClass::Colophon => Some("colophon_metadata"),
            TailLineClass::Blank => None,
        };
        if region != group_region && !group.is_empty() {
            blocks.push(source_note_block(
                mem::take(&mut group),
                group_region.expect("nonempty group has a region"),
            ));
        }
        group_region = region;
        if region.is_some() {
            group.push(json!({
                "kind": "text",
                "value": lines[index],
                "span": tail_span_json(&ranges[index], decoded.tail_offset, &decoded.span_ctx)
            }));
        }
    }
    if !group.is_empty() {
        blocks.push(source_note_block(
            group,
            group_region.expect("nonempty group has a region"),
        ));
    }
    (blocks, warnings)
}

/// One back-matter `source_note` block from a non-empty run of
/// `{kind: "text", value, span}` content nodes. The block `span`
/// aggregates the first content span's `byte_start`/`line_start` and the
/// last content span's `byte_end`/`line_end`.
#[allow(
    clippy::needless_pass_by_value,
    reason = "content is consumed (moved into the returned block); a slice would force an extra clone at the one call site"
)]
fn source_note_block(content: Vec<Value>, region_class: &str) -> Value {
    let first_span = content[0]["span"].clone();
    let last_span = content[content.len() - 1]["span"].clone();
    json!({
        "kind": "source_note",
        "placement": "back",
        "region_class": region_class,
        "content": content,
        "span": {
            "byte_start": first_span["byte_start"],
            "byte_end": last_span["byte_end"],
            "line_start": first_span["line_start"],
            "line_end": last_span["line_end"]
        }
    })
}

#[allow(
    clippy::needless_pass_by_value,
    reason = "block assembly may replace or drain the content vector"
)]
#[allow(
    clippy::too_many_lines,
    reason = "classifier precedence determines which block consumes each source marker"
)]
fn blocks_from_inline_content(content: Vec<Value>, source: &str) -> Vec<Value> {
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
            let boundary = content[index + 1..]
                .iter()
                .position(ends_source_line)
                .map_or(content.len(), |offset| index + offset + 2);
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
        } else if let Some(indent) = jisage_container_indent(&node) {
            if let Some(close_index) = find_matching_jisage_close(&content, index + 1) {
                push_paragraph_if_not_empty(&mut blocks, mem::take(&mut paragraph));
                let mut inner = content[index + 1..close_index].to_vec();
                strip_boundary_newlines(&mut inner);
                blocks.push(jisage_block(&node, indent, inner, source));
                strip_next_leading_newline = true;
                index = close_index + 1;
                continue;
            }
            let boundary = find_next_container_boundary(&content, index + 1);
            if boundary > index + 1 {
                push_paragraph_if_not_empty(&mut blocks, mem::take(&mut paragraph));
                let mut inner = content[index + 1..boundary].to_vec();
                strip_boundary_newlines(&mut inner);
                blocks.push(jisage_block(&node, indent, inner, source));
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
                    "children": blocks_from_inline_content(inner, source)
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
                "children": blocks_from_inline_content(inner, source)
            }));
            strip_next_leading_newline = true;
            index = close_index + 1;
            continue;
        }

        if is_heading_hint_raw(&node)
            && let Some(heading) = heading_block_from_hint(&mut paragraph, &node, source)
        {
            push_paragraph_if_not_empty(&mut blocks, mem::take(&mut paragraph));
            blocks.push(heading);
            strip_next_leading_newline = true;
            index += 1;
            continue;
        }
        let ends_line = ends_source_line(&node);
        paragraph.push(node);
        if ends_line {
            push_paragraph_if_not_empty(&mut blocks, mem::take(&mut paragraph));
        }
        index += 1;
    }

    push_paragraph_if_not_empty(&mut blocks, paragraph);
    if blocks.is_empty() {
        blocks.push(json!({"kind": "paragraph", "content": []}));
    }
    blocks
}

/// True when this inline node carries the end of a source line: a gap-derived
/// text or unparsed-source-gap raw node whose value ends with a line
/// terminator (gap segmentation is terminator-inclusive, see
/// `paragraph_segments`). The paragraph accumulator flushes after such a
/// node, so each body paragraph holds exactly one source line plus any blank
/// run that follows it.
fn ends_source_line(node: &Value) -> bool {
    let value = match node.get("kind").and_then(Value::as_str) {
        Some("text") => node.get("value").and_then(Value::as_str),
        Some("raw")
            if node.get("x-source-marker-kind").and_then(Value::as_str)
                == Some("unparsed-source-gap") =>
        {
            node.get("source").and_then(Value::as_str)
        }
        _ => None,
    };
    value.is_some_and(|value| value.ends_with(['\n', '\r']))
}

#[allow(
    clippy::needless_pass_by_value,
    reason = "block assembly may replace or drain the content vector"
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
    reason = "block assembly may replace or drain the content vector"
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
    reason = "block assembly may replace or drain the content vector"
)]
fn push_burasage_paragraph(blocks: &mut Vec<Value>, first: u64, rest: u64, content: Vec<Value>) {
    for paragraph in content.split_inclusive(ends_source_line) {
        blocks.push(json!({
        "kind": "paragraph",
        "content": [{
            "kind": "style",
            "style_type": "burasage",
            "content": paragraph,
            "indent_first": first,
            "indent_rest": rest,
            "x-provenance": "source-derived"
        }]
        }));
    }
}

/// As [`push_burasage_paragraph`], but if `jizume_width` is present (the
/// compound container's marker also carried a `字詰め` clause) the burasage
/// paragraph nests inside a `jizume_block { width }` instead of landing
/// directly in `blocks` — the compound close-matching logic that got us
/// here is untouched; only the destination of the classified output moves.
#[allow(
    clippy::needless_pass_by_value,
    reason = "block assembly may replace or drain the content vector"
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

fn jisage_block(opener: &Value, indent: u64, inner: Vec<Value>, source: &str) -> Value {
    let mut block = json!({
        "kind": "jisage_block", "indent": indent,
        "children": blocks_from_inline_content(inner, source)
    });
    if opener["source"]
        .as_str()
        .is_some_and(|source| source.ends_with("、横書き、中央揃え、罫囲み］"))
    {
        block["block_style"] =
            json!({"direction": "horizontal", "align": "center", "border": "solid"});
    }
    block
}

fn simple_jisage_open_indent(source: &str) -> Option<u64> {
    let marker = source.trim();
    if !marker.starts_with("［＃ここから") || !marker.ends_with('］') {
        return None;
    }
    let (_, after_indent) = marker.split_once("字下げ")?;
    if after_indent != "］" && after_indent != "、横書き、中央揃え、罫囲み］" {
        return None;
    }
    Some(parse_aozora_number_before(marker, "字下げ").unwrap_or(1))
}

/// Recognize a `字詰め` open marker and extract the chars-per-line count.
///
/// Matches the standalone line-width form (`［＃ここからN字詰め］`) or a
/// `字詰め` carried as the FINAL clause of a compound indent container
/// (`［＃ここから６字下げ、折り返して７字下げ、２１字詰め］`).
///
/// NOTE (C3 gate fix): the *standalone* `［＃ここからN字詰め］` is the
/// `line-width` container family (upstream notation spec §6.6,
/// `line-width-open = ［＃ここから 1*DIGIT 字詰め］`). It is NOT a typed
/// `jizume_block`; it must round-trip as a raw `containerOpen`/
/// `containerClose` pair (conformance vector `line_width_container`). This
/// recognizer therefore feeds ONLY the compound-indent wrap path
/// (`push_burasage_paragraph_maybe_jizume`): a `字詰め` that appears as a
/// clause on a `字下げ`-carrying indent opener projects the compound
/// `jizume_block { width }`. It intentionally still *recognizes* the
/// standalone form (a pure predicate — its semantics are pinned by
/// `jizume_open_chars_recognizes_standalone_and_compound`), but no block
/// classifier arm emits a standalone `jizume_block` from it.
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
    node["x-source-flow"] != "inline"
        && node.get("kind").and_then(Value::as_str) == Some("raw")
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

fn is_container_marker_raw(node: &Value) -> bool {
    is_container_open_raw(node)
        || node["x-source-flow"] != "inline"
            && node.get("kind").and_then(Value::as_str) == Some("raw")
            && node
                .get("x-source-marker-kind")
                .and_then(Value::as_str)
                .is_some_and(|kind| kind == "containerClose")
}

fn is_container_open_raw(node: &Value) -> bool {
    node["x-source-flow"] != "inline"
        && node.get("kind").and_then(Value::as_str) == Some("raw")
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

fn heading_block_from_hint(
    paragraph: &mut Vec<Value>,
    node: &Value,
    decoded_source: &str,
) -> Option<Value> {
    let source = node.get("source").and_then(Value::as_str)?;
    let (target, directive) = source.strip_prefix("［＃「")?.rsplit_once("」は")?;
    let level = heading_level(directive);
    let style = heading_style(directive);
    let heading_content = take_visible_suffix(paragraph, target, decoded_source)?;
    let indent = paragraph.last().and_then(heading_indent_marker);
    if indent.is_some() {
        paragraph.pop();
    }
    let mut heading = json!({
        "kind": "heading",
        "level": level,
        "style": style,
        "content": heading_content,
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
        match node.kind {
            ProjectedKind::RecoveredSource => {
                push_source_gap(&mut content, decoded, node.span.start, node.span.end);
            }
            ProjectedKind::QuoteOpen | ProjectedKind::QuoteClose => content.push(json!({
                "kind": "text",
                "value": if node.kind == ProjectedKind::QuoteOpen { "《" } else { "》" },
                "span": span_json(&node.span, &decoded.span_ctx)
            })),
            ProjectedKind::Node(NodeKind::Ruby) => {
                let mut ruby = ruby_node(decoded, node, ruby_by_span, gaiji_by_start);
                ruby["interpretation_marker_spans"] = json!(
                    node.marker_span
                        .as_ref()
                        .map(|span| span_json(span, &decoded.span_ctx))
                        .into_iter()
                        .collect::<Vec<_>>()
                );
                content.push(ruby);
            }
            ProjectedKind::Node(NodeKind::Gaiji) => {
                content.push(gaiji_node(decoded, node, gaiji_by_start));
            }
            ProjectedKind::Format(ForwardAttr::Bouten { kind, .. }) => {
                push_style_node(
                    &mut content,
                    decoded,
                    node,
                    if kind.is_line() { "bosen" } else { "bouten" },
                );
            }
            ProjectedKind::Format(ForwardAttr::CombineUpright) => {
                content.push(tcy_node(decoded, node));
            }
            ProjectedKind::Format(_) => {
                push_style_node(&mut content, decoded, node, "emphasis");
            }
            ProjectedKind::Node(NodeKind::ForcedBreak) => content.push(json!({
                "kind":"text", "value":"\n", "x-provenance":"source-derived",
                "x-break-kind":"line", "x-break-marker":"forced",
                "span":span_json(&node.span, &decoded.span_ctx)
            })),
            ProjectedKind::Node(NodeKind::Kaeriten) => {
                content.push(raw_node(decoded, node, "kaeriten"));
            }
            ProjectedKind::Node(NodeKind::Directive)
                if source_slice(&decoded.span_text, &node.span).contains("返り点") =>
            {
                content.push(raw_node(decoded, node, "kaeriten"));
            }
            ProjectedKind::Node(NodeKind::PageBreak) => content.push(json!({
                "kind": "raw",
                "source": source_slice(&decoded.span_text, &node.span),
                "x-provenance": "parser-derived",
                "x-source-marker-kind": "pageBreak",
                "x-break-kind": "page",
                "span": span_json(&node.span, &decoded.span_ctx)
            })),
            ProjectedKind::TextVariant {
                target: TextVariantTarget::Text,
                ..
            } => {
                push_text_variant_node(&mut content, decoded, node);
            }
            ProjectedKind::Node(_)
            | ProjectedKind::Region(_)
            | ProjectedKind::RegionClose(_)
            | ProjectedKind::Directive(_)
            | ProjectedKind::TextVariant { .. } => {
                content.push(raw_node(decoded, node, node.kind.as_str()));
            }
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
    // Assemble source paragraphs and enclosing block layouts before consuming
    // same-line formatting markers in pair_bare_toggles_in_blocks.
    pair_warichu(content)
}

fn pair_warichu(content: Vec<Value>) -> Vec<Value> {
    let mut output = Vec::new();
    let mut stack: Vec<(Value, Vec<Value>)> = Vec::new();
    for node in content {
        if node["x-source-marker-kind"] == "warichuOpen" {
            stack.push((node, Vec::new()));
            continue;
        }
        let node = if node["x-source-marker-kind"] == "warichuClose" && !stack.is_empty() {
            let (open, children) = stack.pop().expect("nonempty warichu stack");
            let mut span = open["span"].clone();
            span["byte_end"] = node["span"]["byte_end"].clone();
            span["line_end"] = node["span"]["line_end"].clone();
            json!({"kind":"warichu", "content":children, "span":span, "interpretation_marker_spans":[open["span"], node["span"]]})
        } else {
            node
        };
        if let Some((_, children)) = stack.last_mut() {
            children.push(node);
        } else {
            output.push(node);
        }
    }
    while let Some((open, children)) = stack.pop() {
        let target = stack
            .last_mut()
            .map_or(&mut output, |(_, children)| children);
        target.push(open);
        target.extend(children);
    }
    output
}

/// A same-line marker and the formatting family its close must match.
struct BareToggleMarker {
    index: usize,
    construct: &'static str,
    is_open: bool,
    line: u64,
}

/// Read typed formatting markers; recognize the exact bare horizontal and box forms.
/// Block forms remain available to block classification.
fn bare_toggle_marker(node: &Value) -> Option<(&'static str, bool)> {
    if node.get("kind").and_then(Value::as_str) != Some("raw") {
        return None;
    }
    if let Some(key) = node["x-format-key"].as_str() {
        let key = [
            "bold",
            "gothic",
            "italic",
            "small-script-right",
            "small-script-left",
            "font-large",
            "font-small",
            "tcy",
        ]
        .into_iter()
        .find(|candidate| *candidate == key)?;
        return Some((key, node["x-format-open"].as_bool()?));
    }
    match node.get("source").and_then(Value::as_str)? {
        "［＃横組み］" => Some(("yokogumi", true)),
        "［＃横組み終わり］" => Some(("yokogumi", false)),
        "［＃罫囲み］" => Some(("keigakomi", true)),
        "［＃罫囲み終わり］" => Some(("keigakomi", false)),
        _ => None,
    }
}

/// Apply `pair_bare_toggles` to every content array of an already-built
/// block tree: recurse into each node's `content`/
/// `children` arrays FIRST, then run the pairing pass over the array at this
/// level. Post-order matters — a container the pass creates holds nodes the
/// per-line grammar already ruled on (adopted-into or declined-in-place), and
/// re-running the pass inside it could re-adopt a pair the full line
/// declined (e.g. a rolled-back same-construct pair sitting inside the other
/// construct's adopted container). Block assembly runs first so source paragraph
/// boundaries and enclosing layouts are established before inline markers vanish.
fn pair_bare_toggles_in_blocks(nodes: Vec<Value>) -> Vec<Value> {
    let mut nodes = nodes;
    for node in &mut nodes {
        if let Some(map) = node.as_object_mut() {
            for key in ["content", "children"] {
                if let Some(Value::Array(items)) = map.get_mut(key) {
                    *items = pair_bare_toggles_in_blocks(mem::take(items));
                }
            }
        }
    }
    pair_bare_toggles(nodes)
}

/// Fold noncrossing same-line formatting pairs after block classification.
///
/// Pass 1 runs one global nesting stack over each line's markers in array (==
/// source) order: a same-construct reopen invalidates the construct but still
/// pushes; an orphan close invalidates; a close matching the stack top pops
/// and records a candidate pair; a mismatched close invalidates BOTH the
/// closing and the top construct and pops nothing; any open frame left on the
/// stack at end of line invalidates its construct. Pass 2 adopts a candidate
/// iff its construct was not invalidated on that line — invalidation is
/// construct-scoped, so a valid construct's pair nested inside an invalid
/// construct's markers still adopts. Adopted marker nodes are consumed into the
/// container; every other node, including the raw markers of invalid
/// constructs, is preserved unchanged and in its original order.
///
/// Perf: an O(n) scan that early-returns the input moved (no clone) whenever
/// the paragraph carries no bare-toggle marker — the corpus hot path.
pub(crate) fn pair_bare_toggles(content: Vec<Value>) -> Vec<Value> {
    let markers: Vec<BareToggleMarker> = content
        .iter()
        .enumerate()
        .filter_map(|(index, node)| {
            let (construct, is_open) = bare_toggle_marker(node)?;
            // Markers never span lines; treat a line-spanning token as a
            // non-marker rather than adopting across a line boundary.
            let span = node.get("span")?;
            let line = span.get("line_start")?.as_u64()?;
            let line_end = span.get("line_end")?.as_u64()?;
            if line != line_end {
                return None;
            }
            Some(BareToggleMarker {
                index,
                construct,
                is_open,
                line,
            })
        })
        .collect();
    if markers.is_empty() {
        return content;
    }

    // Run the two-pass grammar per line (markers are already in source order
    // because `inline_content` sorts by span). Adopted candidates across all
    // lines form a laminar family — matched pairs from one stack nest
    // properly, and different lines occupy disjoint index ranges — so they
    // splice cleanly as a forest.
    let mut adopted: Vec<(usize, usize, &'static str)> = Vec::new();
    let mut group_start = 0;
    while group_start < markers.len() {
        let line = markers[group_start].line;
        let mut group_end = group_start;
        while group_end < markers.len() && markers[group_end].line == line {
            group_end += 1;
        }
        pair_line_markers(&markers[group_start..group_end], &mut adopted);
        group_start = group_end;
    }
    if adopted.is_empty() {
        return content;
    }

    let opens: BTreeMap<usize, (usize, &'static str)> = adopted
        .into_iter()
        .map(|(open, close, kind)| (open, (close, kind)))
        .collect();
    let mut content = content;
    let len = content.len();
    splice_bare_toggle_containers(&mut content, 0, len, &opens)
}

/// Pass 1 + Pass 2 over one line's markers (see `pair_bare_toggles`). Pushes
/// each adopted `(open_index, close_index, kind)` onto `adopted`.
fn pair_line_markers(line: &[BareToggleMarker], adopted: &mut Vec<(usize, usize, &'static str)>) {
    let mut stack: Vec<(&'static str, usize)> = Vec::new();
    let mut candidates: Vec<(usize, usize, &'static str)> = Vec::new();
    let mut invalid = BTreeSet::new();
    for marker in line {
        if marker.is_open {
            if stack
                .iter()
                .any(|(construct, _)| *construct == marker.construct)
            {
                // same-construct reopen
                invalid.insert(marker.construct);
            }
            stack.push((marker.construct, marker.index));
        } else {
            match stack.last().copied() {
                // orphan close
                None => {
                    invalid.insert(marker.construct);
                }
                Some((top, open_index)) if top == marker.construct => {
                    stack.pop();
                    candidates.push((open_index, marker.index, marker.construct));
                }
                Some((top, _)) => {
                    // improper interleave: both constructs invalid, pop nothing
                    invalid.insert(marker.construct);
                    invalid.insert(top);
                }
            }
        }
    }
    // Leftover open frames are orphan opens: invalidate their construct.
    for (construct, _) in &stack {
        invalid.insert(*construct);
    }
    // Pass 2: adopt candidates whose construct was not invalidated.
    for (open_index, close_index, construct) in candidates {
        if !invalid.contains(construct) {
            adopted.push((open_index, close_index, construct));
        }
    }
}

/// Rebuild `content[start..end]` folding each adopted `open_index` (looked up
/// in `opens`) and its matching close into one container node whose content is
/// the nodes strictly between the markers. Recurses on the interior first, so
/// nested pairs become child containers; moves each surviving node exactly once
/// (`mem::take`) rather than cloning.
fn splice_bare_toggle_containers(
    content: &mut Vec<Value>,
    start: usize,
    end: usize,
    opens: &BTreeMap<usize, (usize, &'static str)>,
) -> Vec<Value> {
    let mut out = Vec::new();
    let mut index = start;
    while index < end {
        if let Some(&(close, kind)) = opens.get(&index) {
            let child = splice_bare_toggle_containers(content, index + 1, close, opens);
            let open_node = mem::take(&mut content[index]);
            let close_node = mem::take(&mut content[close]);
            out.push(bare_toggle_container(kind, child, &open_node, &close_node));
            index = close + 1;
        } else {
            out.push(mem::take(&mut content[index]));
            index += 1;
        }
    }
    out
}

/// Build one `inline_container` node (same shape family as `style_node`): the
/// span runs from the open marker's start to the close marker's end.
fn bare_toggle_container(
    kind: &'static str,
    content: Vec<Value>,
    open: &Value,
    close: &Value,
) -> Value {
    let mut value = json!({
        "kind": kind,
        // `Value::Array` moves `content` in (json! would otherwise borrow it,
        // reading as a needless by-value param); the pass has no further use.
        "content": Value::Array(content),
        "span": {
            "line_start": open["span"]["line_start"],
            "line_end": close["span"]["line_end"],
            "byte_start": open["span"]["byte_start"],
            "byte_end": close["span"]["byte_end"]
        }
    });
    value["interpretation_marker_spans"] = json!([open["span"], close["span"]]);
    if let Some(fields) = open["x-formatting"].as_object() {
        value
            .as_object_mut()
            .expect("inline container")
            .extend(fields.clone());
    }
    value
}

fn push_source_gap(content: &mut Vec<Value>, decoded: &DecodedSource, start: usize, end: usize) {
    let Some(source) = decoded.span_text.get(start..end) else {
        return;
    };
    if source.is_empty() || source == "｜" {
        return;
    }
    // Emit the gap one source line at a time (terminator-inclusive) so the
    // block builder can end a paragraph at each line end. Splitting here is
    // load-bearing for spans: this is the last point where the body-relative
    // offsets are available to re-map each line's byte/line coordinates.
    for segment in paragraph_segments(source) {
        let segment_source = &source[segment.clone()];
        let span = Span {
            start: start + segment.start,
            end: start + segment.end,
        };
        if contains_aozora_markup(segment_source) {
            let source_start = decoded.span_ctx.to_decoded(span.start);
            let source_end = decoded.span_ctx.to_decoded_end(span.end);
            content.push(json!({
                "kind": "raw",
                "source": &decoded.text[source_start..source_end],
                "x-provenance": "source-derived",
                "x-source-marker-kind": "unparsed-source-gap",
                "interpretation_problem": {
                    "kind": "uninterpreted-notation", "code": "unparsed-source-gap",
                    "aspects": ["content", "structure", "layout"],
                    "influence": {"kind": "document"}
                },
                "span": span_json(&span, &decoded.span_ctx)
            }));
        } else {
            content.push(json!({
                "kind": "text",
                "value": segment_source,
                "span": span_json(&span, &decoded.span_ctx)
            }));
        }
    }
}

fn contains_aozora_markup(source: &str) -> bool {
    source.contains('※')
        || source.contains("［＃")
        || source.contains("[#")
        || source.contains('《')
        || source.contains('》')
}

#[allow(
    clippy::option_if_let_else,
    reason = "branches construct different source-derived node payloads"
)]
fn ruby_node(
    decoded: &DecodedSource,
    node: &AozoraNode,
    ruby_by_span: &BTreeMap<(usize, usize), AozoraRubyEntry>,
    gaiji_by_start: &BTreeMap<usize, AozoraGaiji>,
) -> Value {
    if let Some(entry) = ruby_by_span.get(&(node.span.start, node.span.end)) {
        // A retrospective annotation whose node span is the marker alone
        // (`…《ルビ》［＃「X」の左に「Y」のルビ］`) re-quotes a target that
        // was already emitted; keeping the quoted base would double the
        // word in every projection (the style/tcy duplication class, in
        // its ruby form). The base text lives outside this span, so the
        // node keeps reading + direction and projects no base of its own.
        let marker_only = {
            let source = source_slice(&decoded.span_text, &node.span);
            source.starts_with("［＃") || source.starts_with("[#")
        };
        return json!({
            "kind": "ruby",
            "base": if marker_only { "" } else { entry.base.as_str() },
            "reading": entry.reading,
            "direction": entry.side,
            "span": span_json(&node.span, &decoded.span_ctx)
        });
    }
    // No resolvable structured entry (e.g. gaiji-base ruby, whose base is
    // `Content::Segments` and so has no plain-text range for
    // `ruby_entries` to resolve — see the `RUBY_RE` doc comment). Fall
    // back to the v1 regex reparse rather than silently downgrading to
    // `raw`, then enrich a gaiji-marker base from the gaiji scan.
    let source = source_slice(&decoded.span_text, &node.span);
    if let Some(caps) = RUBY_RE.captures(source) {
        let base = caps.name("base").unwrap();
        let reading = caps.name("reading").unwrap();
        let mut ruby = json!({
            "kind": "ruby",
            "base": base.as_str(),
            "reading": reading.as_str(),
            "direction": "right",
            "span": span_json(&node.span, &decoded.span_ctx)
        });
        let fields = ruby.as_object_mut().unwrap();
        if let Some(segments) = gaiji_segments(
            decoded,
            node.span.start + base.start(),
            base.as_str(),
            gaiji_by_start,
        ) {
            // A fully resolved base projects the glyphs; any unresolved
            // marker keeps the verbatim form (no glyph to project).
            if let Some(resolved) = segments.resolved_text {
                fields.insert("base".to_owned(), json!(resolved));
            }
            fields.insert("base_content".to_owned(), json!(segments.content));
        }
        if let Some(segments) = gaiji_segments(
            decoded,
            node.span.start + reading.start(),
            reading.as_str(),
            gaiji_by_start,
        ) {
            // The reading string stays verbatim (ruby_completeness matches
            // source reading text exactly); the typed nodes ride alongside.
            fields.insert("reading_content".to_owned(), json!(segments.content));
        }
        ruby
    } else {
        raw_node(decoded, node, "ruby")
    }
}

struct GaijiSegments {
    /// One inline node per segment: a gaiji node per marker, a text node
    /// per plain run between markers.
    content: Vec<Value>,
    /// The segment text with every marker replaced by its resolved glyph;
    /// `None` when any marker is unresolved.
    resolved_text: Option<String>,
}

/// Split `text` (starting at source offset `start`) into gaiji markers and
/// plain runs using the gaiji scan. Returns `None` when no scanned gaiji
/// marker lies inside the range or a marker crosses its end.
fn gaiji_segments(
    decoded: &DecodedSource,
    start: usize,
    text: &str,
    gaiji_by_start: &BTreeMap<usize, AozoraGaiji>,
) -> Option<GaijiSegments> {
    let end = start + text.len();
    let mut content = Vec::new();
    let mut resolved_text = Some(String::new());
    let mut gaiji_count = 0_usize;
    let mut cursor = start;
    while cursor < end {
        if let Some(gaiji) = gaiji_by_start.get(&cursor) {
            if gaiji.end <= cursor || gaiji.end > end {
                return None;
            }
            content.push(gaiji_json(
                decoded,
                &Span {
                    start: gaiji.start,
                    end: gaiji.end,
                },
                gaiji,
            ));
            match (&mut resolved_text, &gaiji.resolved) {
                (Some(out), Some(glyph)) => out.push_str(glyph),
                _ => resolved_text = None,
            }
            gaiji_count += 1;
            cursor = gaiji.end;
            continue;
        }
        let next_start = gaiji_by_start
            .range(cursor..end)
            .next()
            .map_or(end, |(offset, _)| *offset);
        let segment = &text[cursor - start..next_start - start];
        content.push(json!({
            "kind": "text",
            "value": segment,
            "span": span_json(
                &Span { start: cursor, end: next_start },
                &decoded.span_ctx
            )
        }));
        if let Some(out) = &mut resolved_text {
            out.push_str(segment);
        }
        cursor = next_start;
    }
    (gaiji_count > 0).then_some(GaijiSegments {
        content,
        resolved_text,
    })
}

fn gaiji_node(
    decoded: &DecodedSource,
    node: &AozoraNode,
    gaiji_by_start: &BTreeMap<usize, AozoraGaiji>,
) -> Value {
    let Some(gaiji) = gaiji_by_start.get(&node.span.start) else {
        return raw_node(decoded, node, "gaiji");
    };
    gaiji_json(decoded, &node.span, gaiji)
}

fn gaiji_json(decoded: &DecodedSource, span: &Span, gaiji: &AozoraGaiji) -> Value {
    json!({
        "kind": "gaiji",
        "description": gaiji.description,
        "resolved": gaiji.resolved,
        "jis_code": gaiji.mencode,
        "unresolved_reason": if gaiji.resolved.is_some() { None::<String> } else { Some("unresolved".to_owned()) },
        "x-codepoint": gaiji.codepoint,
        "span": span_json(span, &decoded.span_ctx)
    })
}

// Retrospective targets may cross already-emitted ruby or gaiji nodes.
// Adopt the exact visible suffix instead of copying the marker's quotation.
fn push_style_node(
    content: &mut Vec<Value>,
    decoded: &DecodedSource,
    node: &AozoraNode,
    style_type: &str,
) {
    let mut style = style_node(decoded, node, style_type);
    if let ProjectedKind::Format(attr) = node.kind
        && let Some(fields) = formatting_fields(attr)
    {
        let object = style.as_object_mut().expect("style object");
        object.remove("style_type");
        object.extend(fields.as_object().expect("formatting fields").clone());
    }
    if let ProjectedKind::Format(ForwardAttr::Bouten { kind, position }) = node.kind {
        style["decoration"] = json!({"kind": kind.keyword(), "position": match position {
            BoutenPosition::Right => "right", BoutenPosition::Left => "left", BoutenPosition::Both => "both", _ => "unknown"
        }});
    }
    if style["content"].as_array().is_some_and(Vec::is_empty)
        && let Some(target) = marker_target(source_slice(&decoded.span_text, &node.span))
        && let Some(children) = take_visible_suffix(content, target, &decoded.text)
    {
        style["span"]["byte_start"] = children[0]["span"]["byte_start"].clone();
        style["span"]["line_start"] = children[0]["span"]["line_start"].clone();
        style["content"] = json!(children);
    }
    style["interpretation_marker_spans"] = json!(
        node.marker_span
            .as_ref()
            .map(|span| span_json(span, &decoded.span_ctx))
            .into_iter()
            .collect::<Vec<_>>()
    );
    content.push(style);
    if let ProjectedKind::Format(attr) = node.kind
        && !matches!(
            attr,
            ForwardAttr::Bouten { .. } | ForwardAttr::CombineUpright
        )
        && formatting_fields(attr).is_none()
    {
        let mut retained = raw_node(decoded, node, "uninterpreted-formatting");
        retained["interpretation_problem"] = json!({"kind":"uninterpreted-notation", "code":"uninterpreted-notation",
            "aspects":if matches!(attr, ForwardAttr::Accent(_) | ForwardAttr::AccentDot) {
                vec!["content", "layout"]
            } else { vec!["structure", "layout"] }, "influence":{"kind":"document"}});
        content.push(retained);
    }
}

fn push_text_variant_node(content: &mut Vec<Value>, decoded: &DecodedSource, node: &AozoraNode) {
    let raw = raw_node(decoded, node, "directive");
    if let ProjectedKind::TextVariant {
        current, base_text, ..
    } = &node.kind
        && let Some(children) = take_visible_suffix(content, current, &decoded.text)
    {
        content.push(json!({"kind":"text-variant", "content":children,
            "base_text":base_text, "source":raw["source"], "span":raw["span"]}));
    } else {
        content.push(raw);
    }
}

fn take_visible_suffix(content: &mut Vec<Value>, target: &str, source: &str) -> Option<Vec<Value>> {
    if target.is_empty() || target.contains(['\n', '\r']) {
        return None;
    }
    let mut remaining = target;
    for index in (0..content.len()).rev() {
        let node = &content[index];
        let kind = node["kind"].as_str()?;
        let text = match kind {
            "text" => node["value"].as_str()?,
            "ruby" => node["base"].as_str()?,
            "gaiji" => node["resolved"].as_str()?,
            _ => return None,
        };
        if text.is_empty() {
            return None;
        }
        if let Some(prefix) = remaining.strip_suffix(text) {
            remaining = prefix;
            if remaining.is_empty() {
                return Some(content.split_off(index));
            }
        } else if kind == "text" && text.ends_with(remaining) {
            let end = usize::try_from(node["span"]["byte_end"].as_u64()?).ok()?;
            let start = end.checked_sub(remaining.len())?;
            // Only split literal source text: normalized characters do not
            // establish a byte-for-byte source boundary at this position.
            if source.get(start..end)? != remaining {
                return None;
            }
            let mut tail = node.clone();
            tail["value"] = json!(remaining);
            tail["span"]["byte_start"] = json!(start);
            tail["span"]["line_start"] = node["span"]["line_end"].clone();
            content[index]["value"] = json!(&text[..text.len() - remaining.len()]);
            content[index]["span"]["byte_end"] = json!(start);
            let mut children = vec![tail];
            children.extend(content.split_off(index + 1));
            return Some(children);
        } else {
            return None;
        }
    }
    None
}

fn style_node(decoded: &DecodedSource, node: &AozoraNode, style_type: &str) -> Value {
    let source = source_slice(&decoded.span_text, &node.span);
    json!({
        "kind": "style",
        "style_type": style_type,
        "content": annotation_content(source),
        "span": span_json(&node.span, &decoded.span_ctx)
    })
}

fn tcy_node(decoded: &DecodedSource, node: &AozoraNode) -> Value {
    let source = source_slice(&decoded.span_text, &node.span);
    json!({
        "kind": "tcy",
        "content": annotation_content(source),
        "span": span_json(&node.span, &decoded.span_ctx)
    })
}

/// Content for a style/tcy annotation node. When the node span covers the
/// target text plus its marker, the quoted target is the node's one copy
/// of that text. A span that starts at the marker itself (the tree
/// anchors there when the target contains a ruby/gaiji and was already
/// emitted as its own nodes) contributes no new text. The style caller
/// adopts those existing nodes when their visible suffix matches the target.
fn annotation_content(source: &str) -> Vec<Value> {
    if source.starts_with("［＃") || source.starts_with("[#") {
        return Vec::new();
    }
    let text = marker_target(source).unwrap_or(source);
    vec![json!({"kind": "text", "value": text})]
}

fn formatting_fields(attr: ForwardAttr) -> Option<Value> {
    Some(match attr {
        ForwardAttr::Bold => json!({"kind":"style", "style_type":"bold"}),
        ForwardAttr::Gothic => json!({"kind":"style", "style_type":"gothic"}),
        ForwardAttr::Italic => json!({"kind":"style", "style_type":"italic"}),
        ForwardAttr::SuperScript => json!({"kind":"style", "style_type":"superscript"}),
        ForwardAttr::SubScript => json!({"kind":"style", "style_type":"subscript"}),
        ForwardAttr::SmallScript(BoutenPosition::Right) => {
            json!({"kind":"small_script", "position":"right"})
        }
        ForwardAttr::SmallScript(BoutenPosition::Left) => {
            json!({"kind":"small_script", "position":"left"})
        }
        ForwardAttr::FontSize(shift) => {
            json!({"kind":"font_size", "size_type":if shift.larger() { "large" } else { "small" }, "level":shift.magnitude()})
        }
        ForwardAttr::FontSizeAbsolute(size) => {
            json!({"kind":"font_size", "size_type":"absolute", "size":match size {
                AbsoluteSize::ExtraLarge => "extra-large", AbsoluteSize::Large => "large",
                AbsoluteSize::Medium => "medium", AbsoluteSize::Small => "small", _ => return None,
            }})
        }
        _ => return None,
    })
}

fn region_formatting(region: RegionFormat) -> Option<(&'static str, Value)> {
    let (key, attr) = match region {
        RegionFormat::Bold { padded: false } => ("bold", ForwardAttr::Bold),
        RegionFormat::Gothic { padded: false } => ("gothic", ForwardAttr::Gothic),
        RegionFormat::Italic { padded: false } => ("italic", ForwardAttr::Italic),
        RegionFormat::SmallScript(BoutenPosition::Right) => (
            "small-script-right",
            ForwardAttr::SmallScript(BoutenPosition::Right),
        ),
        RegionFormat::SmallScript(BoutenPosition::Left) => (
            "small-script-left",
            ForwardAttr::SmallScript(BoutenPosition::Left),
        ),
        RegionFormat::FontSize(shift) => (
            if shift.larger() {
                "font-large"
            } else {
                "font-small"
            },
            ForwardAttr::FontSize(shift),
        ),
        RegionFormat::CombineUpright => return Some(("tcy", json!({"kind":"tcy"}))),
        _ => return None,
    };
    Some((key, formatting_fields(attr)?))
}

fn region_formatting_close(close: RegionClose) -> Option<&'static str> {
    Some(match close {
        RegionClose::Bold { padded: false } => "bold",
        RegionClose::Gothic { padded: false } => "gothic",
        RegionClose::Italic { padded: false } => "italic",
        RegionClose::SmallScript(BoutenPosition::Right) => "small-script-right",
        RegionClose::SmallScript(BoutenPosition::Left) => "small-script-left",
        RegionClose::FontSize { larger: true } => "font-large",
        RegionClose::FontSize { larger: false } => "font-small",
        RegionClose::CombineUpright => "tcy",
        _ => return None,
    })
}

fn marker_target(source: &str) -> Option<&str> {
    let start = source.find("［＃「")? + "［＃「".len();
    let rest = &source[start..];
    let end = rest.find('」')?;
    Some(&rest[..end])
}

fn raw_node(decoded: &DecodedSource, node: &AozoraNode, marker_kind: &str) -> Value {
    let source_start = decoded.span_ctx.to_decoded(node.span.start);
    let source_end = decoded.span_ctx.to_decoded_end(node.span.end);
    let mut value = json!({
        "kind": "raw", "source": &decoded.text[source_start..source_end],
        "x-provenance": "parser-derived", "x-source-marker-kind": marker_kind,
        "span": span_json(&node.span, &decoded.span_ctx)
    });
    if let ProjectedKind::TextVariant {
        target,
        current,
        base_text,
    } = &node.kind
    {
        value["text_variant"] = json!({
            "target_kind": match target { TextVariantTarget::RubyReading => "ruby-reading", TextVariantTarget::Text => "text" },
            "current": current, "base_text": base_text
        });
    }
    match node.kind {
        ProjectedKind::Region(region) => {
            // AAT's paired inline scopes must not terminate enclosing blocks,
            // even when the native renderer uses block presentation for them.
            let formatting = region_formatting(region);
            if formatting.is_some() || region.is_inline() {
                value["x-source-flow"] = json!("inline");
            }
            if let Some((key, fields)) = formatting {
                value["x-format-key"] = json!(key);
                value["x-format-open"] = json!(true);
                value["x-formatting"] = fields;
            }
            value["interpretation_problem"] = json!({"kind":"uninterpreted-notation", "code":"uninterpreted-notation",
                "aspects":["structure","layout"], "influence":{"kind":"document"}});
        }
        ProjectedKind::RegionClose(close) => {
            let formatting = region_formatting_close(close);
            if formatting.is_some() || close.is_inline() {
                value["x-source-flow"] = json!("inline");
            }
            if let Some(key) = formatting {
                value["x-format-key"] = json!(key);
                value["x-format-open"] = json!(false);
            }
            value["interpretation_problem"] = json!({"kind":"uninterpreted-notation", "code":"uninterpreted-notation",
                "aspects":["structure","layout"], "influence":{"kind":"document"}});
        }
        _ => {}
    }
    if node.kind == ProjectedKind::Directive(DirectiveKind::BaseTextVariant) {
        value["interpretation_problem"] = json!({"kind":"unresolved-variant", "code":"unresolved-variant",
            "aspects":["content","structure"], "influence":{"kind":"document"}});
    }
    if node.kind == ProjectedKind::Node(NodeKind::Kaeriten) {
        value["interpretation_problem"] = json!({
            "kind":"uninterpreted-notation", "code":"uninterpreted-notation",
            "aspects":["content","structure"], "influence":{"kind":"document"}
        });
    }
    if node.kind == ProjectedKind::Directive(DirectiveKind::Unknown) {
        value["interpretation_problem"] = json!({
            "kind": "unknown-notation", "code": "unknown-notation",
            "aspects": ["content", "structure", "layout"], "influence": {"kind": "document"}
        });
    }
    value
}

/// Preserve native parser diagnostic identity and decoded-source location.
fn diagnostic_warning(diagnostic: &Diagnostic, ctx: &SpanContext) -> Value {
    let kind = diagnostic.code().rsplit("::").next().unwrap_or("unknown");
    let severity = match diagnostic.severity() {
        Severity::Warning => "warning",
        Severity::Note => "note",
        _ => "error",
    };
    json!({ "code": kind.replace('_', "-"), "severity": severity, "message": kind,
        "span": span_json(&diagnostic.span().into(), ctx) })
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
        "ab-aozora {} aat-schema 2 facade {} diagnostics-schema {} (git {})",
        env!("CARGO_PKG_VERSION"),
        ab_aozora_facade_version(),
        aozora_json::DIAGNOSTICS_SCHEMA_VERSION,
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
    use proptest::prelude::*;

    use super::*;

    #[test]
    fn angle_quote_preserves_visible_delimiters_and_nested_markup() {
        let source = "前≪中｜漢《かん》※［＃「てへん＋劣」、第3水準1-84-77］後≫末。\n";
        let aat = aat_value_for(source);
        let ruby = find_first_node(&aat, "ruby");
        assert_eq!(ruby["reading"], "かん");
        let gaiji = find_first_node(&aat, "gaiji");
        assert!(gaiji.is_object());
        let encoded = serde_json::to_string(&aat).unwrap();
        assert!(encoded.contains("《"));
        assert!(encoded.contains("》"));
        assert!(!encoded.contains("angleQuote"));
    }

    #[test]
    fn angle_quote_children_keep_full_source_spans() {
        let source = "\u{feff}作品名\r\n著者名\r\n\r\n≪前｜漢《かん》後≫\r\n";
        let aat = aat_value_for(source);
        let ruby = find_first_node(&aat, "ruby");
        let decoded_source = source.trim_start_matches('\u{feff}');
        let start = decoded_source.find("｜漢").unwrap();
        assert_eq!(ruby["span"]["byte_start"], start);
        assert_eq!(ruby["span"]["byte_end"], start + "｜漢《かん》".len());
        let contents = aat["blocks"].as_array().unwrap().last().unwrap()["content"]
            .as_array()
            .unwrap();
        let opening = contents
            .iter()
            .find(|n| n["value"].as_str().is_some_and(|v| v.starts_with('《')))
            .unwrap();
        assert_eq!(
            opening["span"]["byte_start"],
            decoded_source.find('≪').unwrap()
        );
        assert_eq!(
            opening["span"]["byte_end"],
            decoded_source.find('≪').unwrap() + 3
        );
    }

    #[test]
    fn angle_quote_projection_is_iterative_and_does_not_promote_annotations() {
        let source = format!("{}｜漢《かん》{}\n", "≪".repeat(256), "≫".repeat(256));
        let aat = aat_value_for(&source);
        assert_eq!(find_first_node(&aat, "ruby")["reading"], "かん");
        let content = aat["blocks"][0]["content"].as_array().unwrap();
        assert_eq!(content.iter().filter(|n| n["value"] == "《").count(), 256);
        assert_eq!(content.iter().filter(|n| n["value"] == "》").count(), 256);
        let aat = aat_value_for("≪前［＃未知の注記］後≫\n");
        let raw = find_first_node(&aat, "raw");
        assert_eq!(raw["source"], "［＃未知の注記］");
        let aat = aat_value_for("前≪閉じない文。\n");
        assert!(
            !serde_json::to_string(&aat)
                .unwrap()
                .contains("\"value\":\"《\"")
        );
    }

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

    #[test]
    fn aat_emits_equal_source_and_primary_text_hash_aliases() {
        let document: Value =
            serde_json::from_slice(&aat_json_from_bytes(b"identity\n").unwrap()).unwrap();
        assert_eq!(
            document["meta"]["primary_text_hash"],
            document["meta"]["source_hash"]
        );
    }

    /// `preserve_order` tripwire: `aat_json_from_bytes` builds its
    /// `serde_json::Value` output via object literals (`json!` macro
    /// insertion order), so if `serde_json/preserve_order` ever leaks into
    /// this crate's compiled feature graph, `Value`'s map switches from
    /// `BTreeMap` (alphabetical-by-key serialization) to `IndexMap`
    /// (insertion-order serialization) and this exact-byte assertion goes
    /// red — a parsed/`Value`-equality check would NOT catch this, since
    /// `Value::eq` for objects is order-independent.
    ///
    /// Expected output re-pasted 2026-07-12 (`ab-aozora` `0.5.0` →
    /// `0.6.0` — the C5 identity bump; no functional change, only the
    /// version string) via:
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
        let expected = "{\"blocks\":[{\"content\":[{\"kind\":\"text\",\"span\":{\"byte_end\":4,\"byte_start\":0,\"line_end\":1,\"line_start\":1},\"value\":\"あ\\n\"}],\"kind\":\"paragraph\"}],\"meta\":{\"adapter\":\"ab-aozora\",\"adapter_version\":\"ab-aozora 0.6.0 aat-schema 2 facade 0.3.0 diagnostics-schema 3 (git unknown)\",\"parse_complete\":true,\"primary_text_hash\":\"sha256:872f53a70d5e2b801dcad8ade42fa36f20a64f64e6c3af6b7de01ca026405843\",\"source_encoding\":\"utf-8\",\"source_hash\":\"sha256:872f53a70d5e2b801dcad8ade42fa36f20a64f64e6c3af6b7de01ca026405843\",\"warnings\":[]},\"version\":2,\"work_id\":\"stdin\"}\n";
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
            assert!(
                !code.contains('_') && !code.contains("::"),
                "not kebab: {code}"
            );
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
    #[allow(
        clippy::manual_contains,
        reason = "assertion expression is a pinned test invariant, not touched per fix-wave scope"
    )]
    fn sanitize_and_parser_diagnostics_merge_in_order_without_duplicates() {
        // accent (sanitize-stage) + unclosed bracket (parser-stage) in one input:
        // sanitize entries come first, parser entries after, one of each.
        let out = diagnostics_json_from_bytes("あ〔cafe'〕い［＃ここから".as_bytes()).unwrap();
        let doc: Value = serde_json::from_slice(&out).unwrap();
        let codes: Vec<&str> = doc["data"]
            .as_array()
            .unwrap()
            .iter()
            .map(|e| e["code"].as_str().unwrap())
            .collect();
        let accent_count = codes
            .iter()
            .filter(|c| **c == "accent-decomposition-applied")
            .count();
        assert_eq!(
            accent_count, 1,
            "duplicate or missing accent entry: {codes:?}"
        );
        assert!(
            codes[0] == "accent-decomposition-applied",
            "sanitize entries must come first: {codes:?}"
        );
        assert!(codes.iter().any(|c| *c == "unclosed-bracket"), "{codes:?}");
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
        assert_eq!(
            tcy.len(),
            1,
            "expected exactly one tcy diagnostic: {data:?}"
        );
        assert_eq!(tcy[0]["severity"], "warning");
        assert_eq!(tcy[0]["span"]["start"], 6);
        assert_eq!(tcy[0]["span"]["end"], 35);
    }

    #[test]
    fn literal_pua_survives_bom_and_crlf_with_decoded_source_extents() {
        let bytes = [
            b"\xef\xbb\xbf".as_ref(),
            "あ\r\n\u{e001}漢《かん》".as_bytes(),
        ]
        .concat();
        let aat: Value = serde_json::from_slice(&aat_json_from_bytes(&bytes).unwrap()).unwrap();
        let content = aat["blocks"][1]["content"].as_array().unwrap();
        assert_eq!(content[0]["value"], "\u{e001}");
        assert_eq!(content[0]["span"]["byte_start"], 5);
        assert_eq!(content[0]["span"]["byte_end"], 8);
        assert_eq!(content[1]["kind"], "ruby");
        assert_eq!(aat["meta"]["warnings"], json!([]));
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
        // The trailing gap "\nい\n" ← decoded "\rい\r" = bytes 25..30 is
        // emitted per line: the \r closing the directive line (25..26,
        // line 2) and the い line with its terminator (26..30, line 3).
        assert!(
            spans
                .iter()
                .any(|s| s["byte_start"] == 25 && s["byte_end"] == 26 && s["line_start"] == 2),
            "no line-2 terminator span: {spans:?}"
        );
        assert!(
            spans.iter().any(|s| s["byte_start"] == 26
                && s["byte_end"] == 30
                && s["line_start"] == 3
                && s["line_end"] == 3),
            "no span for the real line-3 い line: {spans:?}"
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

    #[test]
    fn prose_with_tortoise_brackets_remains_visible_text() {
        for source in [
            "　〔二十分停車〕と時計の下に書いてありました。\n",
            "「行つてみよう。」二人は、まるで一度に叫んで、そつちの方へ走りました。その白い岩になつた處の入口に〔プリオシン海岸〕といふ、瀬戸物のつるつるした標札が立つて、向うの渚には、ところどころ細い鐵の欄干も植ゑられ、木製のきれいなベンチも置いてありました。\n",
        ] {
            let aat = aat_value_for(source);
            assert!(find_node(&aat, "raw").is_none(), "{aat}");
            assert_eq!(find_first_node(&aat, "text")["value"], source);
        }
    }

    #[test]
    fn body_lines_become_separate_paragraphs() {
        let document = aat_value_for("一行目。\n\n　二行目。\n");
        let blocks = document["blocks"].as_array().unwrap();
        assert_eq!(blocks.len(), 2, "{blocks:?}");
        for block in blocks {
            assert_eq!(block["kind"], "paragraph");
        }
        let first = blocks[0]["content"].as_array().unwrap();
        assert_eq!(first.len(), 1);
        // Terminator-inclusive line; the following blank line coalesces into
        // the paragraph it ends, so no whitespace-only paragraph appears.
        assert_eq!(first[0]["value"], "一行目。\n\n");
        assert_eq!(first[0]["span"]["byte_start"], 0);
        assert_eq!(first[0]["span"]["byte_end"], 14);
        assert_eq!(first[0]["span"]["line_start"], 1);
        let second = blocks[1]["content"].as_array().unwrap();
        assert_eq!(second.len(), 1);
        assert_eq!(second[0]["value"], "　二行目。\n");
        assert_eq!(second[0]["span"]["byte_start"], 14);
        assert_eq!(second[0]["span"]["byte_end"], 30);
        assert_eq!(second[0]["span"]["line_start"], 3);
    }

    #[test]
    fn ruby_led_line_starts_its_own_paragraph() {
        let document = aat_value_for("本文《ほんぶん》続き。\n吾輩《わがはい》は走る。\n");
        let blocks = document["blocks"].as_array().unwrap();
        assert_eq!(blocks.len(), 2, "{blocks:?}");
        let first_kinds: Vec<&str> = blocks[0]["content"]
            .as_array()
            .unwrap()
            .iter()
            .map(|node| node["kind"].as_str().unwrap())
            .collect();
        // ruby + rest-of-line text (with its terminator)
        assert_eq!(first_kinds, ["ruby", "text"]);
        let second = blocks[1]["content"].as_array().unwrap();
        assert_eq!(second[0]["kind"], "ruby");
        assert_eq!(second[0]["base"], "吾輩");
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

    /// The top-level `blocks` array of an AAT document.
    fn top_level_blocks(v: &Value) -> &Vec<Value> {
        v["blocks"].as_array().unwrap()
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

    /// A retrospective left-ruby whose target already carries ordinary
    /// ruby (`謝肉《しゃにく》［＃「謝肉」の左に「カルネワル」のルビ］`)
    /// anchors on the marker alone — the target was already emitted — so
    /// re-quoting the target as `base` would double the word in every
    /// projection, plain.txt included (the 傍点/太字 duplication class
    /// fixed for style/tcy spans, in its ruby form).
    #[test]
    fn marker_only_left_ruby_span_does_not_duplicate_target() {
        let src = "謝肉《しゃにく》［＃「謝肉」の左に「カルネワル」のルビ］の祭\n";
        let aat = aat_value_for(src);
        let blocks = &aat["blocks"][0]["content"];
        let rubies: Vec<&Value> = blocks
            .as_array()
            .unwrap()
            .iter()
            .filter(|n| n["kind"] == "ruby")
            .collect();
        assert_eq!(rubies.len(), 2);
        assert_eq!(rubies[0]["base"], "謝肉");
        assert_eq!(rubies[0]["reading"], "しゃにく");
        assert_eq!(rubies[1]["direction"], "left");
        assert_eq!(rubies[1]["reading"], "カルネワル");
        // Marker-only span: the base is already emitted by the first node,
        // so the projection (text values + ruby bases) carries 謝肉 once.
        assert_eq!(rubies[1]["base"], "");
        let projected: String = blocks
            .as_array()
            .unwrap()
            .iter()
            .filter_map(|n| match n["kind"].as_str() {
                Some("text") => n["value"].as_str().map(ToOwned::to_owned),
                Some("ruby") => n["base"].as_str().map(ToOwned::to_owned),
                _ => None,
            })
            .collect();
        assert_eq!(projected.matches("謝肉").count(), 1);
    }

    /// Gaiji-base ruby (`※［＃…］《reading》`): the base is a deferred gaiji
    /// (`ab-aozora-pipeline`'s `try_ruby_over_gaiji_base`), which becomes a
    /// `Content::Segments` base — `content_range_as_plain` returns `None`
    /// for it, so `ruby_entries` has no entry for this node's span and
    /// `ruby_node` falls through to the `RUBY_RE` regex path. That fallback
    /// enriches the base from the gaiji scan: the typed ruby carries the
    /// resolved glyph as its `base` (what plaintext / parser-IR project)
    /// and a structured `base_content` gaiji node (what ab-check's
    /// `gaiji_resolution` counts) instead of the verbatim `※［＃…］`
    /// marker text v1 emitted.
    #[test]
    fn gaiji_base_ruby_resolves_base_and_emits_gaiji_base_content() {
        let src = "※［＃「木＋吶のつくり」、第3水準1-85-54］《かい》\n";
        let aat = aat_value_for(src);
        let ruby = find_first_node(&aat, "ruby");
        assert_eq!(ruby["kind"], "ruby");
        assert_eq!(ruby["direction"], "right");
        assert_eq!(ruby["base"], "枘");
        assert_eq!(ruby["reading"], "かい");
        let base_content = ruby["base_content"].as_array().unwrap();
        assert_eq!(base_content.len(), 1);
        let gaiji = &base_content[0];
        assert_eq!(gaiji["kind"], "gaiji");
        assert_eq!(gaiji["description"], "木＋吶のつくり");
        assert_eq!(gaiji["resolved"], "枘");
        assert_eq!(gaiji["jis_code"], "第3水準1-85-54");
        assert!(gaiji["unresolved_reason"].is_null());
    }

    #[test]
    fn kumo_no_ito_ruby_base_keeps_gaiji_and_following_kanji() {
        let src = "※［＃「特のへん＋廴＋聿」、第3水準1-87-71］陀多《かんだた》\n";
        let aat = aat_value_for(src);
        let ruby = find_first_node(&aat, "ruby");
        assert_eq!(ruby["base"], "犍陀多");
        assert_eq!(ruby["reading"], "かんだた");
        let base = ruby["base_content"].as_array().unwrap();
        assert_eq!(base.len(), 2);
        assert_eq!(base[0]["kind"], "gaiji");
        assert_eq!(base[0]["resolved"], "犍");
        assert_eq!(base[1]["kind"], "text");
        assert_eq!(base[1]["value"], "陀多");
    }

    #[test]
    fn retrospective_heading_adopts_ruby_and_retains_indentation() {
        let source = "［＃７字下げ］「大溝《おほどぶ》」［＃「「大溝」」は中見出し］\n本文。\n";
        let aat = aat_value_for(source);
        let heading = find_first_node(&aat, "heading");
        assert_eq!(heading["level"], 2);
        assert_eq!(heading["indent"], 7);
        assert_eq!(find_first_node(heading, "ruby")["reading"], "おほどぶ");
        let content = heading["content"].as_array().unwrap();
        assert_eq!(content[0]["value"], "「");
        assert_eq!(content[2]["value"], "」");
        assert_eq!(aat["blocks"][1]["content"][0]["value"], "本文。\n");
        let mismatch = aat_value_for(&source.replace("［＃「「大溝」」", "［＃「「小溝」」"));
        assert!(find_node(&mismatch, "heading").is_none());
        assert_eq!(find_first_node(&mismatch, "ruby")["base"], "大溝");
    }

    #[test]
    fn marker_only_style_adopts_ruby_target_without_duplication() {
        for (source, target) in [
            ("まえ牛《ベゴ》の舌［＃「牛の舌」に傍点］あと\n", "牛"),
            (
                "まえ扨、私事《わたくしこと》、［＃「扨、私事、」は太字］あと\n",
                "私事",
            ),
        ] {
            let aat = aat_value_for(source);
            let style = find_first_node(&aat, "style");
            assert!(!style["content"].as_array().unwrap().is_empty());
            assert_eq!(find_first_node(style, "ruby")["base"], target);
            assert_eq!(style["span"]["byte_start"], "まえ".len());
            assert_eq!(style["span"]["byte_end"], source.find("あと").unwrap());
            let content = aat["blocks"][0]["content"].as_array().unwrap();
            assert_eq!(content[0]["value"], "まえ");
            assert_eq!(content.len(), 3);
            assert_eq!(content[2]["value"], "あと\n");
        }
    }

    #[test]
    fn retrospective_style_does_not_adopt_mismatched_or_partial_ruby_targets() {
        let source = "まえ牛舌《ベゴ》の字\n";
        let aat = aat_value_for(source);
        let original = aat["blocks"][0]["content"].as_array().unwrap();
        for target in ["羊の字\n", "舌の字\n", "羊の字", "舌の字"] {
            let mut content = original.clone();
            if let Some(last) = content.last_mut() {
                last["value"] = json!("の字");
                last["span"]["byte_end"] = json!(source.len() - 1);
            }
            let before = content.clone();
            assert!(take_visible_suffix(&mut content, target, source).is_none());
            assert_eq!(content, before);
        }
    }

    /// The span-covering form (`文字［＃「文字」に傍点］` where the node
    /// span includes the target) keeps the target as content — there it
    /// is the only copy.
    #[test]
    fn target_covering_style_span_keeps_content() {
        let src = "まえ文字［＃「文字」に傍点］あと\n";
        let aat = aat_value_for(src);
        let style = find_first_node(&aat, "style");
        assert_eq!(style["style_type"], "bouten");
        assert_eq!(style["content"][0]["value"], "文字");
    }

    /// A ruby base made of several consecutive gaiji markers (`※［＃…］
    /// ※［＃…］《reading》`) resolves the whole chain: the base becomes
    /// the concatenated glyphs and `base_content` carries one gaiji node
    /// per marker.
    #[test]
    fn multi_gaiji_ruby_base_resolves_marker_chain() {
        let src = "※［＃「骨＋亢」、第4水準2-93-7］※［＃「骨＋葬」、第4水準2-93-15］《こうそう》\n";
        let aat = aat_value_for(src);
        let ruby = find_first_node(&aat, "ruby");
        assert_eq!(ruby["base"], "骯髒");
        assert_eq!(ruby["reading"], "こうそう");
        let base_content = ruby["base_content"].as_array().unwrap();
        assert_eq!(base_content.len(), 2);
        assert_eq!(base_content[0]["resolved"], "骯");
        assert_eq!(base_content[1]["resolved"], "髒");
    }

    /// A gaiji marker inside a ruby READING (`《※［＃…］エル》`) keeps the
    /// verbatim `reading` string (`ruby_completeness` matches source reading
    /// text exactly) but gains `reading_content` with the typed gaiji node
    /// so the reference stays countable.
    #[test]
    fn gaiji_in_ruby_reading_gains_reading_content() {
        let src = "淡絹《※［＃濁点付き片仮名ヱ、1-7-84］エル》\n";
        let aat = aat_value_for(src);
        let ruby = find_first_node(&aat, "ruby");
        assert_eq!(ruby["base"], "淡絹");
        assert_eq!(ruby["reading"], "※［＃濁点付き片仮名ヱ、1-7-84］エル");
        let reading_content = ruby["reading_content"].as_array().unwrap();
        assert_eq!(reading_content.len(), 2);
        assert_eq!(reading_content[0]["kind"], "gaiji");
        assert_eq!(reading_content[0]["resolved"], "ヹ");
        assert_eq!(reading_content[1]["kind"], "text");
        assert_eq!(reading_content[1]["value"], "エル");
    }

    /// An unresolvable gaiji base (description-only mencode) keeps the
    /// verbatim marker as `base` — there is no glyph to project — but the
    /// `base_content` gaiji node must still be present with
    /// `unresolved_reason` set so the reference stays typed and countable.
    #[test]
    fn unresolvable_gaiji_base_ruby_keeps_verbatim_base_with_unresolved_gaiji() {
        let src = "※［＃「参らせ候」のくずし字、13-9］《そろ》\n";
        let aat = aat_value_for(src);
        let ruby = find_first_node(&aat, "ruby");
        assert_eq!(ruby["base"], "※［＃「参らせ候」のくずし字、13-9］");
        assert_eq!(ruby["reading"], "そろ");
        let gaiji = &ruby["base_content"].as_array().unwrap()[0];
        assert_eq!(gaiji["kind"], "gaiji");
        assert_eq!(gaiji["description"], "「参らせ候」のくずし字");
        assert!(gaiji["resolved"].is_null());
        assert_eq!(gaiji["unresolved_reason"], "unresolved");
    }

    /// A stray plain `［` left open on one line (a gloss bracket in a
    /// German-textbook work, a doubled `［［＃…］`) must not bury the rest
    /// of the document: the pair stage expires it at the newline, its
    /// line replays as plain text, and every later line's markers emit
    /// normally.
    #[test]
    fn stray_open_bracket_does_not_swallow_later_lines() {
        let src = "彼の所有［当然\n二二※［＃「口＋世」、U+546D］が四\n";
        let aat = aat_value_for(src);
        let gaiji = find_first_node(&aat, "gaiji");
        assert_eq!(gaiji["description"], "口＋世");
        assert_eq!(gaiji["resolved"], "呭");
        // The stray line survives verbatim as text.
        assert!(aat.to_string().contains("彼の所有［当然"));
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
        assert_eq!(
            jizume_open_chars("［＃ここから６字下げ、折り返して７字下げ、２１字詰め］"),
            Some(21)
        );
        assert_eq!(jizume_open_chars("［＃ここから２字下げ］"), None);
        assert_eq!(jizume_open_chars("［＃ここで字詰め終わり］"), None);
        assert!(is_jizume_close("［＃ここで字詰め終わり］"));
        assert!(!is_jizume_close("［＃ここで字下げ終わり］"));
    }

    #[test]
    fn paired_line_width_container_stays_raw() {
        // C3 gate fix: standalone `［＃ここからN字詰め］` is the `line-width`
        // container family (upstream notation spec §6.6, `line-width-open`),
        // NOT a typed jizume_block. Even a fully paired open/close must
        // round-trip as raw containerOpen/containerClose — the conformance
        // vector `line_width_container` requires exactly this. (Pre-C3 this
        // arm emitted a jizume_block, which over-matched the vector.)
        let aat = aat_value_for("［＃ここから２１字詰め］\n本文\n［＃ここで字詰め終わり］\n");
        assert!(
            find_node(&aat, "jizume_block").is_none(),
            "standalone line-width container must not form a jizume_block"
        );
        let serialized = serde_json::to_string(&aat).unwrap();
        assert!(serialized.contains("containerOpen"));
        assert!(serialized.contains("containerClose"));
    }

    #[test]
    fn line_width_container_conformance_vector_stays_raw() {
        // Pins the exact source text of the upstream conformance vector
        // `line_width_container` (§6.6): the parser must leave it a raw
        // containerOpen/containerClose pair so the comparator maps it 1:1 to
        // the vector's expected node kinds. Verbatim vector source below.
        let aat = aat_value_for(
            "本文。\n［＃ここから26字詰め］\n詰めた段落。\n別の行。\n［＃ここで字詰め終わり］\n通常段落。\n",
        );
        assert!(
            find_node(&aat, "jizume_block").is_none(),
            "line_width_container vector must not form a jizume_block"
        );
        let serialized = serde_json::to_string(&aat).unwrap();
        assert!(serialized.contains("containerOpen"));
        assert!(serialized.contains("containerClose"));
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
    fn closing_offset_ends_at_source_line_boundary() {
        for newline in ["\n", "\r\n"] {
            let source = format!("［＃地から３字上げ］句《く》。{newline}続く。{newline}次。");
            let aat = aat_value_for(&source);
            let blocks = aat["blocks"].as_array().unwrap();
            assert_eq!(blocks.len(), 3, "{aat}");
            let style = &blocks[0]["content"][0];
            assert_eq!(style["style_type"], "chitsuki");
            assert_eq!(style["offset_from_end"], 3);
            assert_eq!(style["content"].as_array().unwrap().len(), 2);
            assert_eq!(blocks[1]["content"][0]["value"], "続く。\n");
            assert_eq!(blocks[2]["content"][0]["value"], "次。");
            let joined = aat_value_for(&source.replacen(newline, "", 1));
            assert_eq!(joined["blocks"].as_array().unwrap().len(), 2);
            assert_eq!(
                joined["blocks"][0]["content"][0]["content"][1]["value"],
                "。続く。\n"
            );
        }
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
    fn quoted_heading_text_cannot_change_directive_level_or_style() {
        for (title, directive, level, style) in [
            ("大阪入城", "中見出し", 2, "normal"),
            ("四　クーボー大博士", "中見出し", 2, "normal"),
            ("大窓の同行者", "小見出し", 3, "normal"),
            ("中庭", "大見出し", 1, "normal"),
            ("大窓", "同行中見出し", 2, "dogyo"),
            ("同行者", "窓小見出し", 3, "mado"),
            ("死語となつた「言文一致」", "中見出し", 2, "normal"),
            ("「言文一致」といふ語の終り", "中見出し", 2, "normal"),
        ] {
            let source = format!("［＃７字下げ］{title}［＃「{title}」は{directive}］\n");
            let aat = aat_value_for(&source);
            let heading = find_first_node(&aat, "heading");
            assert_eq!(heading["level"], level, "{source}");
            assert_eq!(heading["style"], style, "{source}");
            assert_eq!(heading["indent"], 7);
            assert_eq!(heading["content"][0]["value"], title);
        }
    }

    #[test]
    fn c5_identity_join_key_and_document_version() {
        assert!(
            adapter_version()
                .starts_with("ab-aozora 0.6.0 aat-schema 2 facade 0.3.0 diagnostics-schema 3")
        );
        let aat = aat_value_for("あ\n");
        assert_eq!(aat["version"], 2);
    }

    #[test]
    fn classify_tail_provenance_head_line() {
        let (classes, unclassifiable) = classify_tail(&["底本：「日本文学全集1」集英社"]);
        assert_eq!(classes, vec![TailLineClass::TerminalProvenance]);
        assert!(unclassifiable.is_empty());
    }

    #[test]
    fn classify_tail_continuation_after_provenance_head_is_provenance() {
        let lines = [
            "底本：「日本文学全集1」集英社",
            "　　　1969（昭和44）年12月25日初版",
        ];
        let (classes, _) = classify_tail(&lines);
        assert_eq!(
            classes,
            vec![
                TailLineClass::TerminalProvenance,
                TailLineClass::TerminalProvenance
            ]
        );
    }

    /// THE distinguishing case: the identically-shaped date line that is
    /// `TerminalProvenance` in the previous test is `Colophon` here,
    /// because it follows `入力：` instead of `底本：`. No per-line
    /// predicate can tell these apart — only carried state can.
    #[test]
    fn classify_tail_same_shaped_line_after_colophon_head_is_colophon() {
        let lines = ["入力：j.utiyama", "1998年7月28日公開"];
        let (classes, _) = classify_tail(&lines);
        assert_eq!(
            classes,
            vec![TailLineClass::Colophon, TailLineClass::Colophon]
        );
    }

    #[test]
    fn classify_tail_real_corpus_tail_end_to_end() {
        // cards/000005/files/5_ruby_21311.zip::aibiki.txt in the pinned
        // aozorabunko corpus (the motivating example from the report).
        let lines = [
            "底本：「日本文学全集1　坪内逍遥・二葉亭四迷集」集英社",
            "　　　1969（昭和44）年12月25日初版",
            "入力：j.utiyama",
            "校正：八巻美恵",
            "1998年7月28日公開",
            "2006年1月6日修正",
            "青空文庫作成ファイル：",
            "このファイルは、インターネットの図書館、青空文庫で作られました。",
        ];
        let (classes, unclassifiable) = classify_tail(&lines);
        assert!(unclassifiable.is_empty());
        assert_eq!(
            classes,
            vec![
                TailLineClass::TerminalProvenance,
                TailLineClass::TerminalProvenance,
                TailLineClass::Colophon,
                TailLineClass::Colophon,
                TailLineClass::Colophon,
                TailLineClass::Colophon,
                TailLineClass::Colophon,
                TailLineClass::Colophon,
            ]
        );
    }

    #[test]
    fn classify_tail_oyahon_continuation_is_provenance() {
        let lines = [
            "底本の親本：「新編 銀河鉄道の夜」新潮文庫",
            "　　　1989（平成元）年11月10日初版",
        ];
        let (classes, _) = classify_tail(&lines);
        assert_eq!(
            classes,
            vec![
                TailLineClass::TerminalProvenance,
                TailLineClass::TerminalProvenance
            ]
        );
    }

    #[test]
    fn classify_tail_all_named_colophon_heads() {
        let lines = [
            "入力：ある人",
            "校正：別の人",
            "青空文庫作成ファイル：",
            "※このファイルはインターネットの図書館、青空文庫で作られました。",
        ];
        let (classes, _) = classify_tail(&lines);
        assert_eq!(classes, vec![TailLineClass::Colophon; 4]);
    }

    #[test]
    fn classify_tail_blank_between_blocks_is_blank_and_preserves_state() {
        let lines = ["底本：「サンプル」出版社", "", "入力：誰か"];
        let (classes, _) = classify_tail(&lines);
        assert_eq!(
            classes,
            vec![
                TailLineClass::TerminalProvenance,
                TailLineClass::Blank,
                TailLineClass::Colophon
            ]
        );
    }

    #[test]
    fn classify_tail_blank_preserves_provenance_state_across_continuation() {
        let lines = ["底本：「サンプル」出版社", "", "　　　1999年1月1日初版"];
        let (classes, _) = classify_tail(&lines);
        assert_eq!(
            classes,
            vec![
                TailLineClass::TerminalProvenance,
                TailLineClass::Blank,
                TailLineClass::TerminalProvenance
            ]
        );
    }

    /// Divergence from the Python reference: `classify_tail` never fails
    /// closed. A non-blank line before any head classifies `Colophon` and
    /// its tail-relative index is reported so the caller can warn.
    #[test]
    fn classify_tail_nonblank_before_any_head_is_unclassifiable_fallback() {
        let lines = ["何かの一行", "底本：「サンプル」出版社"];
        let (classes, unclassifiable) = classify_tail(&lines);
        assert_eq!(classes[0], TailLineClass::Colophon);
        assert_eq!(classes[1], TailLineClass::TerminalProvenance);
        assert_eq!(unclassifiable, vec![0]);
    }

    // --- source_note emission -------------------------------------------

    #[test]
    fn explanatory_colophon_note_is_retained_separately_from_attribution() {
        let source = "本文。\n\n底本：「作品集」\n\n※「□」には、底本では「◆」が内接しています。\n入力：入力者\n";
        let aat = aat_value_for(source);
        let notes: Vec<_> = top_level_blocks(&aat)
            .iter()
            .filter(|block| block["kind"] == "source_note")
            .collect();
        assert_eq!(notes.len(), 2);
        assert_eq!(notes[0]["region_class"], "terminal_provenance");
        assert_eq!(notes[1]["region_class"], "colophon_metadata");
        assert_eq!(
            notes[1]["content"][0]["value"],
            "※「□」には、底本では「◆」が内接しています。\n"
        );
        assert_eq!(notes[1]["content"][1]["value"], "入力：入力者\n");
        for note in notes {
            for line in note["content"].as_array().unwrap() {
                let start = usize::try_from(line["span"]["byte_start"].as_u64().unwrap()).unwrap();
                let end = usize::try_from(line["span"]["byte_end"].as_u64().unwrap()).unwrap();
                assert_eq!(&source[start..end], line["value"].as_str().unwrap());
            }
        }
    }

    #[test]
    fn terminal_provenance_tail_emits_source_note() {
        let src = "本文です。\n\n底本：「作品集」文庫社\n　1990（平成2）年5月10日発行\n入力：someone\n校正：other\n";
        let aat = aat_value_for(src);
        let notes: Vec<&Value> = top_level_blocks(&aat)
            .iter()
            .filter(|b| b["kind"] == "source_note")
            .collect();
        assert_eq!(notes.len(), 2);
        assert_eq!(notes[1]["region_class"], "colophon_metadata");
        assert_eq!(notes[1]["content"][0]["value"], "入力：someone\n");
        assert_eq!(notes[1]["content"][1]["value"], "校正：other\n");
        let note = notes[0];
        assert_eq!(note["placement"], "back");
        assert_eq!(note["region_class"], "terminal_provenance");
        // One text inline per attribution line, with its terminator.
        let content = note["content"].as_array().unwrap();
        assert_eq!(content.len(), 2);
        assert_eq!(content[0]["value"], "底本：「作品集」文庫社\n");
        assert_eq!(content[1]["value"], "　1990（平成2）年5月10日発行\n");
        let s = &content[0]["span"];
        let (a, b) = (
            usize::try_from(s["byte_start"].as_u64().unwrap()).unwrap(),
            usize::try_from(s["byte_end"].as_u64().unwrap()).unwrap(),
        );
        assert_eq!(&src[a..b], "底本：「作品集」文庫社\n"); // terminator inside the span
        assert_eq!(s["line_start"], 3);
        // block span aggregates first content start .. last content end
        assert_eq!(note["span"]["byte_start"], content[0]["span"]["byte_start"]);
        assert_eq!(note["span"]["byte_end"], content[1]["span"]["byte_end"]);
        assert_eq!(note["span"]["line_start"], content[0]["span"]["line_start"]);
        assert_eq!(note["span"]["line_end"], content[1]["span"]["line_end"]);
    }

    #[test]
    fn stateful_boundary_date_after_colophon_head_stays_colophon() {
        // A date after input credits belongs to the colophon, not attribution.
        let src = "本文。\n\n底本：「X」Y社\n入力：someone\n　2005（平成17）年1月1日作成\n";
        let aat = aat_value_for(src);
        let note = top_level_blocks(&aat)
            .iter()
            .find(|b| b["kind"] == "source_note")
            .unwrap()
            .clone();
        assert_eq!(note["content"].as_array().unwrap().len(), 1);
        let colophon = top_level_blocks(&aat)
            .iter()
            .find(|block| block["region_class"] == "colophon_metadata")
            .unwrap();
        assert_eq!(
            colophon["content"][1]["value"],
            "　2005（平成17）年1月1日作成\n"
        );
    }

    #[test]
    fn tail_free_work_has_no_source_note() {
        let aat = aat_value_for("本文だけ。\n");
        assert!(
            top_level_blocks(&aat)
                .iter()
                .all(|b| b["kind"] != "source_note")
        );
    }

    #[test]
    fn alternating_attribution_and_colophon_groups_retain_source_order() {
        // A colophon block interrupts two provenance blocks — each
        // contiguous TerminalProvenance run is its own source_note.
        let src = "本文。\n\n底本：「A」X社\n入力：someone\n底本の親本：「B」Y社\n入力：other\n";
        let aat = aat_value_for(src);
        let notes: Vec<&Value> = top_level_blocks(&aat)
            .iter()
            .filter(|b| b["kind"] == "source_note")
            .collect();
        assert_eq!(notes.len(), 4);
        let regions: Vec<_> = notes
            .iter()
            .map(|note| note["region_class"].as_str().unwrap())
            .collect();
        assert_eq!(
            regions,
            [
                "terminal_provenance",
                "colophon_metadata",
                "terminal_provenance",
                "colophon_metadata"
            ]
        );
        assert_eq!(notes[0]["content"].as_array().unwrap().len(), 1);
        assert_eq!(notes[1]["content"][0]["value"], "入力：someone\n");
        assert_eq!(notes[2]["content"][0]["value"], "底本の親本：「B」Y社\n");
        assert_eq!(notes[3]["content"][0]["value"], "入力：other\n");
    }

    /// A real `decode_source_bytes` tail's first line is ALWAYS a
    /// recognized head: `aozora_body_range`'s tail-start rule
    /// (`line.trim_start().starts_with("底本：")`) IS
    /// `classify_tail`'s `PROVENANCE_HEADS[0]` check, so `state` is
    /// always set before `classify_tail` ever looks at a second line —
    /// the fallback branch is provably unreachable through the real
    /// pipeline (mirrors the Python module's own finding in
    /// `GeneratorCli.test_unclassifiable_residual_exits_2_with_bounded_examples`'s
    /// comment, which monkeypatches `classify_tail` for the same reason).
    /// This test therefore pins the PLUMBING directly against
    /// `source_notes_from_tail` with a hand-built `sanitized_tail` a
    /// future caller could still produce, rather than round-tripping
    /// through `decode_source_bytes` on synthetic source text.
    #[test]
    fn unclassifiable_tail_line_falls_back_to_colophon_with_warning() {
        let mut decoded = decode_source_bytes(b"foo\n").unwrap();
        assert!(
            decoded.sanitized_tail.is_empty(),
            "sanity: no real tail in this input"
        );
        decoded.sanitized_tail = "何かの一行\n底本：「X」Y社\n".to_owned();
        decoded.tail_offset = 0;
        let (blocks, warnings) = source_notes_from_tail(&decoded);
        assert_eq!(blocks.len(), 2);
        assert_eq!(blocks[0]["kind"], "source_note");
        assert_eq!(blocks[0]["region_class"], "colophon_metadata");
        assert_eq!(blocks[0]["content"][0]["value"], "何かの一行\n");
        assert_eq!(blocks[1]["region_class"], "terminal_provenance");
        let content = blocks[1]["content"].as_array().unwrap();
        assert_eq!(content.len(), 1);
        assert_eq!(content[0]["value"], "底本：「X」Y社\n");
        assert_eq!(warnings.len(), 1, "{warnings:?}");
        assert_eq!(warnings[0]["code"], "tail-line-unclassified");
        assert_eq!(warnings[0]["severity"], "warning");
        assert_eq!(warnings[0]["message"], "何かの一行");
    }

    #[test]
    fn line_ranges_handles_lf_crlf_bare_cr_and_unterminated_tail() {
        assert_eq!(line_ranges("a\nb"), vec![0..2, 2..3]);
        assert_eq!(line_ranges("a\r\nb"), vec![0..3, 3..4]);
        assert_eq!(line_ranges("a\rb"), vec![0..2, 2..3]);
        assert_eq!(line_ranges("a\n"), vec![0..2]);
    }

    #[test]
    fn bare_toggle_markers_arrive_as_ordered_container_wire_nodes() {
        // Wire-level preflight: the facade must deliver each bare-toggle
        // marker as its own wire node whose span slices the exact token, in
        // source order — paired markers included (adoption later consumes
        // them, so THIS test, not the AAT fallback test, pins the stream the
        // classifier reads). Observed wire mapping (main
        // 9a480e39): open tokens arrive as `containerOpen` nodes and close
        // tokens as `containerClose` nodes — not `directive`.
        let src =
            "ウサギ［＃横組み］（Hare）［＃横組み終わり］だ\n［＃罫囲み］三［＃罫囲み終わり］\n";
        // Follow `projections` (lib.rs:415) for decode + parse + node_entries.
        let decoded = decode_source_bytes(src.as_bytes()).unwrap();
        let nodes: Vec<AozoraNode> = projections(&decoded.span_text).unwrap().0;
        let expected = [
            ("containerOpen", "［＃横組み］"),
            ("containerClose", "［＃横組み終わり］"),
            ("containerOpen", "［＃罫囲み］"),
            ("containerClose", "［＃罫囲み終わり］"),
        ];
        let markers: Vec<&AozoraNode> = nodes
            .iter()
            .filter(|n| {
                matches!(
                    n.kind,
                    ProjectedKind::Node(NodeKind::ContainerOpen | NodeKind::ContainerClose)
                        | ProjectedKind::Region(_)
                        | ProjectedKind::RegionClose(_)
                )
            })
            .collect();
        let marker_shapes: Vec<(&str, &str)> = markers
            .iter()
            .map(|n| (n.kind.as_str(), source_slice(&decoded.span_text, &n.span)))
            .collect();
        // One wire node per marker, exact source slice, expected kind — no
        // merging into wider nodes, no extra marker nodes:
        assert_eq!(
            marker_shapes, expected,
            "bare-toggle wire nodes deviate from the pinned shape"
        );
        // Ordered by span (the classifier depends on source order):
        let starts: Vec<usize> = markers.iter().map(|n| n.span.start).collect();
        assert!(
            starts.windows(2).all(|w| w[0] < w[1]),
            "marker spans not ordered: {starts:?}"
        );
    }

    #[test]
    fn bare_toggle_orphan_markers_stay_raw_in_aat() {
        let src = "（例）［＃横組み］\nx［＃罫囲み終わり］y\n";
        let doc = aat_value_for(src);
        let mut raw_sources = Vec::new();
        collect_raw_sources(&doc, &mut raw_sources);
        for token in ["［＃横組み］", "［＃罫囲み終わり］"] {
            assert!(
                raw_sources.iter().any(|s| s == token),
                "orphan token {token} must stay a raw node; raw sources: {raw_sources:?}"
            );
        }
    }

    fn collect_raw_sources(v: &Value, out: &mut Vec<String>) {
        if let Some(obj) = v.as_object() {
            if obj.get("kind").and_then(Value::as_str) == Some("raw")
                && let Some(s) = obj.get("source").and_then(Value::as_str)
            {
                out.push(s.to_owned());
            }
            for key in ["blocks", "content", "children"] {
                if let Some(arr) = obj.get(key).and_then(Value::as_array) {
                    for item in arr {
                        collect_raw_sources(item, out);
                    }
                }
            }
        }
    }

    /// Depth-first collect of every node whose `"kind"` equals `kind`, in
    /// document order (for asserting on repeated containers on one line).
    fn collect_nodes<'a>(v: &'a Value, kind: &str, out: &mut Vec<&'a Value>) {
        match v {
            Value::Object(map) => {
                if map.get("kind").and_then(Value::as_str) == Some(kind) {
                    out.push(v);
                }
                for key in ["blocks", "content", "children"] {
                    if let Some(child) = map.get(key) {
                        collect_nodes(child, kind, out);
                    }
                }
            }
            Value::Array(items) => {
                for item in items {
                    collect_nodes(item, kind, out);
                }
            }
            _ => {}
        }
    }

    /// A REAL adapter-built node array for `pair_bare_toggles`, produced by
    /// reusing `inline_content` with the same inputs `build_aat` constructs
    /// The adapter
    /// applies the pass to the post-block-classification content arrays
    /// (`pair_bare_toggles_in_blocks`), but for these single-paragraph,
    /// zero-adoption lines block classification wraps the identical node
    /// sequence in one paragraph, so the array the pass reads is this one.
    fn inline_array_for(line: &str) -> Vec<Value> {
        let src = format!("{line}\n");
        let decoded = decode_source_bytes(src.as_bytes()).unwrap();
        let (nodes, _diagnostics, gaiji, ruby, _) = projections(&decoded.span_text).unwrap();
        let gaiji_by_start = gaiji
            .iter()
            .map(|entry| (entry.start, entry.clone()))
            .collect::<BTreeMap<_, _>>();
        let ruby_by_span = ruby
            .iter()
            .map(|entry| ((entry.span.start, entry.span.end), entry.clone()))
            .collect::<BTreeMap<_, _>>();
        inline_content(&decoded, &nodes, &gaiji_by_start, &ruby_by_span)
    }

    fn raw_sources_of(doc: &Value) -> Vec<String> {
        let mut raws = Vec::new();
        collect_raw_sources(doc, &mut raws);
        raws
    }

    // --- The ten shared bare-toggle vectors -------------------------------
    // (reports/aat-fidelity/bare-toggle-model-vectors.json; each Rust test
    // mirrors the normative `classify_line` adopt/decline decision.)

    #[test]
    fn bare_toggle_simple_pair_adopts_inline_container() {
        let doc = aat_value_for("ab［＃横組み］xy［＃横組み終わり］cd\n");
        let node = find_first_node(&doc, "yokogumi");
        let content = node["content"].as_array().unwrap();
        assert_eq!(content.len(), 1);
        assert_eq!(content[0]["kind"], "text");
        assert_eq!(content[0]["value"], "xy");
        let raws = raw_sources_of(&doc);
        assert!(
            raws.iter().all(|s| !s.starts_with("［＃横組み")),
            "markers must be consumed: {raws:?}"
        );
    }

    #[test]
    fn bare_toggle_two_sequential_pairs_adopt_two_containers() {
        let doc =
            aat_value_for("［＃罫囲み］a［＃罫囲み終わり］ b ［＃罫囲み］c［＃罫囲み終わり］\n");
        let mut boxes = Vec::new();
        collect_nodes(&doc, "keigakomi", &mut boxes);
        assert_eq!(boxes.len(), 2, "both keigakomi pairs adopt");
        assert_eq!(boxes[0]["content"][0]["value"], "a");
        assert_eq!(boxes[1]["content"][0]["value"], "c");
        let raws = raw_sources_of(&doc);
        assert!(
            raws.iter().all(|s| !s.starts_with("［＃罫囲み")),
            "all four markers consumed: {raws:?}"
        );
    }

    #[test]
    fn bare_toggle_nested_pair_becomes_child_container() {
        let doc = aat_value_for("［＃罫囲み］［＃横組み］x［＃横組み終わり］［＃罫囲み終わり］\n");
        let outer = find_first_node(&doc, "keigakomi");
        let inner = find_node(outer, "yokogumi").expect("nested yokogumi child");
        assert_eq!(inner["content"][0]["value"], "x");
        // span containment: parent covers child
        let (po, pc) = (
            outer["span"]["byte_start"].as_u64().unwrap(),
            outer["span"]["byte_end"].as_u64().unwrap(),
        );
        let (io, ic) = (
            inner["span"]["byte_start"].as_u64().unwrap(),
            inner["span"]["byte_end"].as_u64().unwrap(),
        );
        assert!(po < io && ic < pc);
        assert!(raw_sources_of(&doc).is_empty(), "no markers survive");
    }

    #[test]
    fn bare_toggle_improper_interleave_declines_both() {
        // ［＃横組み］［＃罫囲み］…［＃横組み終わり］…: the yoko close mismatches
        // the kei top → BOTH invalid, nothing pops; the kei pair rolls back.
        let doc = aat_value_for("［＃横組み］［＃罫囲み］x［＃横組み終わり］［＃罫囲み終わり］\n");
        assert!(find_node(&doc, "yokogumi").is_none());
        assert!(find_node(&doc, "keigakomi").is_none());
        let raws = raw_sources_of(&doc);
        for token in [
            "［＃横組み］",
            "［＃罫囲み］",
            "［＃横組み終わり］",
            "［＃罫囲み終わり］",
        ] {
            assert!(
                raws.iter().any(|s| s == token),
                "{token} stays raw: {raws:?}"
            );
        }
    }

    #[test]
    fn bare_toggle_same_construct_reopen_declines() {
        let doc = aat_value_for("［＃横組み］a［＃横組み］b［＃横組み終わり］\n");
        assert!(find_node(&doc, "yokogumi").is_none());
        let raws = raw_sources_of(&doc);
        assert_eq!(
            raws.iter().filter(|s| *s == "［＃横組み］").count(),
            2,
            "both opens stay raw: {raws:?}"
        );
        assert!(raws.iter().any(|s| s == "［＃横組み終わり］"));
    }

    #[test]
    fn bare_toggle_orphan_open_stays_raw() {
        let doc = aat_value_for("（例）［＃横組み］\n");
        assert!(find_node(&doc, "yokogumi").is_none());
        assert!(raw_sources_of(&doc).iter().any(|s| s == "［＃横組み］"));
    }

    #[test]
    fn bare_toggle_orphan_close_stays_raw() {
        let doc = aat_value_for("ab［＃横組み終わり］cd\n");
        assert!(find_node(&doc, "yokogumi").is_none());
        assert!(
            raw_sources_of(&doc)
                .iter()
                .any(|s| s == "［＃横組み終わり］")
        );
    }

    #[test]
    fn bare_toggle_valid_beside_invalid_other_construct() {
        // A valid keigakomi pair sits beside a yokogumi orphan open on the
        // same line: the keigakomi adopts, the yokogumi marker stays raw.
        let doc = aat_value_for("［＃罫囲み］x［＃罫囲み終わり］ ［＃横組み］\n");
        let box_node = find_first_node(&doc, "keigakomi");
        assert_eq!(box_node["content"][0]["value"], "x");
        assert!(find_node(&doc, "yokogumi").is_none());
        assert!(raw_sources_of(&doc).iter().any(|s| s == "［＃横組み］"));
    }

    #[test]
    fn bare_toggle_valid_nested_inside_invalid_outer_still_adopts() {
        // Construct-scoped invalidation: the keigakomi open is an orphan
        // (invalid), but the yokogumi pair nested inside it is valid and
        // STILL adopts.
        let doc = aat_value_for("［＃罫囲み］［＃横組み］x［＃横組み終わり］\n");
        let inner = find_first_node(&doc, "yokogumi");
        assert_eq!(inner["content"][0]["value"], "x");
        assert!(find_node(&doc, "keigakomi").is_none());
        assert!(raw_sources_of(&doc).iter().any(|s| s == "［＃罫囲み］"));
    }

    #[test]
    fn bare_toggle_later_orphan_rolls_back_earlier_pair() {
        // A matched yokogumi pair is followed by a third yokogumi open on the
        // same line: the leftover open invalidates yokogumi, rolling back the
        // earlier pair — nothing adopts.
        let doc = aat_value_for("［＃横組み］a［＃横組み終わり］［＃横組み］\n");
        assert!(find_node(&doc, "yokogumi").is_none());
        let raws = raw_sources_of(&doc);
        assert_eq!(
            raws.iter().filter(|s| *s == "［＃横組み］").count(),
            2,
            "both opens stay raw: {raws:?}"
        );
        assert!(raws.iter().any(|s| s == "［＃横組み終わり］"));
    }

    // --- Invariants (review P5-6: STRUCTURAL EQUALITY) --------------------

    #[test]
    fn bare_toggle_zero_adoption_input_is_structurally_unchanged() {
        // For every shared vector with zero adoptions, the pass must return
        // the input array UNCHANGED — full structural equality, so no text,
        // span, provenance, ordering, or field can drift unnoticed.
        for line in [
            "（例）［＃横組み］",                                            // orphan open
            "ab［＃横組み終わり］cd",                                        // orphan close
            "［＃横組み］a［＃横組み］b［＃横組み終わり］",                  // reopen
            "［＃横組み］［＃罫囲み］x［＃横組み終わり］［＃罫囲み終わり］", // interleave
            "［＃横組み］a［＃横組み終わり］［＃横組み］",                   // rollback
        ] {
            let content = inline_array_for(line);
            let out = pair_bare_toggles(content.clone());
            assert_eq!(out, content, "zero-adoption line must be identity: {line}");
        }
    }

    #[test]
    fn bare_toggle_no_marker_input_is_identity() {
        // The perf early-return path: a paragraph with no bare-toggle marker
        // must return byte-for-byte unchanged.
        let content = inline_array_for("ただの本文《ほんぶん》です［＃ここから罫囲み］");
        let out = pair_bare_toggles(content.clone());
        assert_eq!(out, content);
    }

    #[test]
    fn bare_toggle_multi_line_isolation() {
        // Line 1 carries a valid pair; line 2 carries an orphan open (corpus
        // case 000106_55753). Line 1 adopts; line 2's marker stays raw and its
        // nodes are untouched.
        let doc = aat_value_for("ab［＃横組み］xy［＃横組み終わり］cd\n（例）［＃横組み］\n");
        let mut boxes = Vec::new();
        collect_nodes(&doc, "yokogumi", &mut boxes);
        assert_eq!(boxes.len(), 1, "exactly line 1 adopts");
        assert_eq!(boxes[0]["content"][0]["value"], "xy");
        let raws = raw_sources_of(&doc);
        assert_eq!(
            raws.iter().filter(|s| *s == "［＃横組み］").count(),
            1,
            "only line 2's orphan open stays raw: {raws:?}"
        );
    }

    /// Depth-first search for a `kind:"raw"` node whose `source` equals
    /// `source` exactly.
    fn find_raw_with_source<'a>(v: &'a Value, source: &str) -> Option<&'a Value> {
        match v {
            Value::Object(map) => {
                if map.get("kind").and_then(Value::as_str) == Some("raw")
                    && map.get("source").and_then(Value::as_str) == Some(source)
                {
                    return Some(v);
                }
                for key in ["blocks", "content", "children"] {
                    if let Some(child) = map.get(key)
                        && let Some(found) = find_raw_with_source(child, source)
                    {
                        return Some(found);
                    }
                }
                None
            }
            Value::Array(items) => items
                .iter()
                .find_map(|item| find_raw_with_source(item, source)),
            _ => None,
        }
    }

    /// Rebuild `v` with every yokogumi/keigakomi inline container expanded
    /// back to `[open, ...content, close]` — the delta audit's projection
    /// (adoption undone, everything else untouched). `open`/`close` are the
    /// raw marker nodes the no-pass tree kept; suitable for single-pair
    /// inputs (every container expands to the same marker pair).
    fn expand_bare_toggle_containers(v: &Value, open: &Value, close: &Value) -> Value {
        match v {
            Value::Array(items) => Value::Array(
                items
                    .iter()
                    .flat_map(|item| {
                        let kind = item.get("kind").and_then(Value::as_str);
                        if kind == Some("yokogumi") || kind == Some("keigakomi") {
                            let mut out = vec![open.clone()];
                            if let Some(children) = item.get("content").and_then(Value::as_array) {
                                out.extend(
                                    children
                                        .iter()
                                        .map(|c| expand_bare_toggle_containers(c, open, close)),
                                );
                            }
                            out.push(close.clone());
                            out
                        } else {
                            vec![expand_bare_toggle_containers(item, open, close)]
                        }
                    })
                    .collect(),
            ),
            Value::Object(map) => {
                let mut expanded = map.clone();
                for key in ["blocks", "content", "children"] {
                    if let Some(child) = map.get(key) {
                        expanded.insert(
                            key.to_owned(),
                            expand_bare_toggle_containers(child, open, close),
                        );
                    }
                }
                Value::Object(expanded)
            }
            _ => v.clone(),
        }
    }

    #[test]
    fn bare_toggle_inside_jizume_block_preserves_block_structure() {
        // Corpus shape 000026_55738 (delta-gate BLOCK →
        // amendment 2): a compound 字下げ (burasage) block whose body line
        // carries a bare yokogumi pair, closed by ここで字下げ終わり, then
        // another paragraph. With the pass running BEFORE block
        // classification, consuming the marker nodes changed segmentation
        // (`find_matching_jisage_close` no longer aborted at the bare
        // `containerOpen`, so the terminator was consumed and the following
        // paragraph merged into the burasage paragraph). The pass now runs
        // post-block-classification: the FULL block tree must equal the
        // no-pass tree except for exactly the adopted-pair rewrite.
        let src = "［＃ここから２字下げ、折り返して３字下げ］\n\
                   Ａ＝Ａ［＃横組み］ＡＢ［＃横組み終わり］\n\
                   ［＃ここで字下げ終わり］\n\
                   次の段落\n";

        // (a) the pair adopts with the right content.
        let doc = aat_value_for(src);
        let container = find_first_node(&doc, "yokogumi");
        let content = container["content"].as_array().unwrap();
        assert_eq!(content.len(), 1);
        assert_eq!(content[0]["kind"], "text");
        assert_eq!(content[0]["value"], "ＡＢ");

        // (b) strongest form (mirrors the delta audit's projection): expand
        // the adopted container back to [open, content, close] and assert
        // the whole block tree equals the tree built WITHOUT the pass —
        // block kinds, paragraph segmentation, jizume terminator handling,
        // spans, provenance: everything.
        let decoded = decode_source_bytes(src.as_bytes()).unwrap();
        let (nodes, _diagnostics, gaiji, ruby, _) = projections(&decoded.span_text).unwrap();
        let gaiji_by_start = gaiji
            .iter()
            .map(|entry| (entry.start, entry.clone()))
            .collect::<BTreeMap<_, _>>();
        let ruby_by_span = ruby
            .iter()
            .map(|entry| ((entry.span.start, entry.span.end), entry.clone()))
            .collect::<BTreeMap<_, _>>();
        let no_pass_blocks = Value::Array(blocks_from_inline_content(
            inline_content(&decoded, &nodes, &gaiji_by_start, &ruby_by_span),
            &decoded.text,
        ));
        let open = find_raw_with_source(&no_pass_blocks, "［＃横組み］")
            .expect("no-pass tree keeps the open marker raw")
            .clone();
        let close = find_raw_with_source(&no_pass_blocks, "［＃横組み終わり］")
            .expect("no-pass tree keeps the close marker raw")
            .clone();
        let expanded = expand_bare_toggle_containers(&doc["blocks"], &open, &close);
        assert_eq!(
            expanded, no_pass_blocks,
            "block tree must differ from the no-pass tree ONLY by the adopted-pair rewrite"
        );

        // The regression's visible symptom, pinned directly: the 字下げ
        // terminator must not be dropped.
        let raws = raw_sources_of(&doc);
        assert!(
            raws.iter().any(|s| s == "［＃ここで字下げ終わり］"),
            "jisage terminator dropped: {raws:?}"
        );
    }

    // --- property-test target ---------------------------------------------
    //
    // `pair_bare_toggles` is `pub(crate)`, unreachable from an integration
    // test under `tests/`. The mirror test over the shared vector file and
    // the three properties that only need OBSERVABLE adapter outcomes
    // (`every_marker_consumed_or_preserved_exactly_once`, `line_isolation`,
    // `nesting_well_formed`) live in `tests/bare_toggle_model.rs`, reached
    // through the public `aat_json_from_bytes`. The two properties below
    // need a DIRECT `pair_bare_toggles` call (zero-adoption structural
    // identity; determinism at the pass level, not the whole-adapter
    // level), so they live here instead.

    /// Recognize one of the four bare-toggle marker literals from its bare
    /// token text. Independent of `bare_toggle_marker` (which classifies a
    /// parsed `Value` node, not a token string) — this just restates the
    /// fixed four-token vocabulary for the property generator below, not a
    /// dependency on the classifier's internals.
    fn oracle_marker_kind(token: &str) -> Option<(&'static str, bool)> {
        match token {
            "［＃横組み］" => Some(("yokogumi", true)),
            "［＃横組み終わり］" => Some(("yokogumi", false)),
            "［＃罫囲み］" => Some(("keigakomi", true)),
            "［＃罫囲み終わり］" => Some(("keigakomi", false)),
            _ => None,
        }
    }

    /// Independent reimplementation of the two-pass bare-toggle grammar —
    /// mirrors `reports/aat-fidelity/bare-toggle-placement.py`'s
    /// `classify_tokens` exactly — used ONLY as the property oracle for
    /// `bare_toggle_zero_adoption_is_structurally_unchanged`: true iff at
    /// least one marker pair in `markers` (one line's marker sequence, in
    /// source order) would adopt. Deliberately independent of
    /// `pair_line_markers`/`pair_bare_toggles` so the property doesn't test
    /// the production pass against itself.
    fn oracle_line_has_adoption(markers: &[(&'static str, bool)]) -> bool {
        let mut stack: Vec<&'static str> = Vec::new();
        let mut candidates: Vec<&'static str> = Vec::new();
        let mut invalid_yokogumi = false;
        let mut invalid_keigakomi = false;
        for &(construct, is_open) in markers {
            if is_open {
                if stack.contains(&construct) {
                    match construct {
                        "yokogumi" => invalid_yokogumi = true,
                        _ => invalid_keigakomi = true,
                    }
                }
                stack.push(construct);
            } else {
                match stack.last().copied() {
                    None => match construct {
                        "yokogumi" => invalid_yokogumi = true,
                        _ => invalid_keigakomi = true,
                    },
                    Some(top) if top == construct => {
                        stack.pop();
                        candidates.push(construct);
                    }
                    Some(top) => {
                        for invalidated in [construct, top] {
                            match invalidated {
                                "yokogumi" => invalid_yokogumi = true,
                                _ => invalid_keigakomi = true,
                            }
                        }
                    }
                }
            }
        }
        for construct in &stack {
            match *construct {
                "yokogumi" => invalid_yokogumi = true,
                _ => invalid_keigakomi = true,
            }
        }
        candidates.into_iter().any(|construct| match construct {
            "yokogumi" => !invalid_yokogumi,
            _ => !invalid_keigakomi,
        })
    }

    /// One token: a bare-toggle marker literal, or a short run of ASCII/
    /// hiragana filler standing in for ordinary text (same "token soup"
    /// shape as `tests/bare_toggle_model.rs`'s generator; duplicated here
    /// rather than shared, since that file can't reach this `mod tests`).
    fn bare_toggle_token() -> impl Strategy<Value = String> {
        prop_oneof![
            Just("［＃横組み］".to_owned()),
            Just("［＃横組み終わり］".to_owned()),
            Just("［＃罫囲み］".to_owned()),
            Just("［＃罫囲み終わり］".to_owned()),
            "[a-zあ-ん]{1,4}",
        ]
    }

    /// 1-3 lines, each 0-11 tokens, kept as un-joined per-line token lists
    /// so the property oracle can read each line's marker sequence
    /// directly (in source order) without re-tokenizing joined text.
    fn bare_toggle_lines() -> impl Strategy<Value = Vec<Vec<String>>> {
        prop::collection::vec(prop::collection::vec(bare_toggle_token(), 0..12), 1..=3)
    }

    fn join_bare_toggle_lines(lines: &[Vec<String>]) -> String {
        lines
            .iter()
            .map(|tokens| tokens.join(""))
            .collect::<Vec<_>>()
            .join("\n")
    }

    proptest! {
        #![proptest_config(ProptestConfig { cases: 512, ..ProptestConfig::default() })]

        /// Property 2: a line with zero adoptions (per the independent
        /// oracle above) is passed through `pair_bare_toggles` byte-for-
        /// byte unchanged — full structural equality (review P5-6), not
        /// mere marker survival.
        #[test]
        fn bare_toggle_zero_adoption_is_structurally_unchanged(lines in bare_toggle_lines()) {
            let any_adoption = lines.iter().any(|tokens| {
                let markers: Vec<(&'static str, bool)> = tokens
                    .iter()
                    .filter_map(|token| oracle_marker_kind(token))
                    .collect();
                oracle_line_has_adoption(&markers)
            });
            prop_assume!(!any_adoption);
            let content = inline_array_for(&join_bare_toggle_lines(&lines));
            let out = pair_bare_toggles(content.clone());
            prop_assert_eq!(out, content);
        }

        /// Property 3: `pair_bare_toggles` is a pure function of its input
        /// — running it twice on the same pre-pass inline array yields
        /// identical output.
        #[test]
        fn bare_toggle_pass_is_deterministic(lines in bare_toggle_lines()) {
            let content = inline_array_for(&join_bare_toggle_lines(&lines));
            let first = pair_bare_toggles(content.clone());
            let second = pair_bare_toggles(content);
            prop_assert_eq!(first, second);
        }
    }
}
