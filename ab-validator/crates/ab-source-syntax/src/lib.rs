use std::borrow::Cow;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceSpan {
    pub start: usize,
    pub end: usize,
    pub line: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceEvent<'a> {
    pub span: SourceSpan,
    pub kind: SourceEventKind<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SourceEventKind<'a> {
    Text(&'a str),
    Ruby {
        base_source: Option<&'a str>,
        reading: &'a str,
    },
    Gaiji {
        description: &'a str,
    },
    Command {
        body: &'a str,
    },
    EditorialNote {
        raw: &'a str,
        kind: EditorialNoteKind<'a>,
    },
    SegmentBoundary {
        kind: SegmentBoundaryKind,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SourceMarkerKind {
    IterationNotation,
    RubyExplicit,
    RubyImplicit,
    GaijiFullwidth,
    GaijiAscii,
    CommandFullwidth,
    CommandAscii,
    AccentNotation,
    BracketNote,
    EditorialNoteRubyCorrection,
    EditorialNoteBottomTextCorrection,
    SegmentBoundaryTerminalProvenance,
    MalformedGaiji,
    MalformedGaijiAscii,
    MalformedCommand,
    MalformedCommandAscii,
    MalformedRuby,
    MalformedImplicitRuby,
    MalformedAccentNotation,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceMarker<'a> {
    pub span: SourceSpan,
    pub raw: &'a str,
    pub body: &'a str,
    pub kind: SourceMarkerKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EditorialNoteKind<'a> {
    RubyCorrection {
        target_reading: &'a str,
        source_reading: &'a str,
    },
    BottomTextCorrection,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SegmentBoundaryKind {
    TerminalProvenanceNote,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LocatedMarker<'a> {
    pub value: &'a str,
    pub byte_offset: usize,
    pub line: usize,
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct SourceAnnotations<'a> {
    pub ruby_readings: Vec<LocatedMarker<'a>>,
    pub gaiji_descriptions: Vec<LocatedMarker<'a>>,
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct SourceAnnotationsBoth<'a> {
    pub validation: SourceAnnotations<'a>,
    pub full: SourceAnnotations<'a>,
}

/// The three declared regions of an Aozora source, in one coordinate.
///
/// The body constitutes the work (both prose and annotations). The header and tail
/// contain metadata *about* the work. They are two populations, not two candidate
/// denominators for one ratio, and measuring their union averages parser
/// fidelity against packaging attribution and can mean neither.
///
/// The regions are derived from [`aozora_body_range`] alone; no separator or
/// `底本：` heuristic is maintained anywhere else. The tail is anchored on
/// the body **end**, rather than on the returned tail start. Because `body_end`
/// is trim-adjusted for trailing newlines and `tail_start` is not, anchoring
/// on the latter would leave the blank lines between them in no region at all.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceRegions {
    header: core::ops::Range<usize>,
    body: core::ops::Range<usize>,
    tail: core::ops::Range<usize>,
}

/// Why a proposed region set is not a partition of its source.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegionError {
    /// A region's start exceeds its end.
    Inverted,
    /// Two adjacent regions do not meet, or the set does not span `[0, len)`.
    NotContiguous,
    /// A boundary does not land on a UTF-8 character boundary.
    NotCharBoundary,
}

impl SourceRegions {
    /// Declare the partition from two boundaries and a length.
    ///
    /// This is the only constructor, so the conservation identity cannot be
    /// bypassed by assembling the regions field by field. Every defect traced
    /// in this area shared one property: no invariant existed that could
    /// catch it, so this one is checked rather than documented.
    ///
    /// # Errors
    ///
    /// Returns [`RegionError`] when the boundaries do not partition
    /// `[0, len)`, or when either lands mid-character in `source`.
    pub fn declare(source: &str, body_start: usize, body_end: usize) -> Result<Self, RegionError> {
        let len = source.len();
        if body_start > body_end || body_end > len {
            return Err(RegionError::Inverted);
        }
        if !source.is_char_boundary(body_start) || !source.is_char_boundary(body_end) {
            return Err(RegionError::NotCharBoundary);
        }
        let regions = Self {
            header: 0..body_start,
            body: body_start..body_end,
            tail: body_end..len,
        };
        // Conservation and disjointness, asserted rather than assumed. The
        // regions are half-open and adjacent by construction above, so this
        // can only fail on arithmetic overflow, which is the case a
        // reader would otherwise assume away.
        let covered = regions
            .header
            .len()
            .checked_add(regions.body.len())
            .and_then(|sum| sum.checked_add(regions.tail.len()));
        if covered != Some(len) {
            return Err(RegionError::NotContiguous);
        }
        Ok(regions)
    }

    /// Derive the partition for `source` directly, using [`aozora_body_range`]
    /// as the sole boundary authority.
    ///
    /// Callers holding a sanitized→decoded mapping must not use this: they
    /// have to map the boundaries through it first, because
    /// `aozora_body_range` runs over sanitized text while the regions must be
    /// stated in decoded coordinates.
    ///
    /// # Errors
    ///
    /// Returns [`RegionError`] when the derived boundaries do not partition
    /// `source`.
    pub fn derive(source: &str) -> Result<Self, RegionError> {
        let (body, _tail_start) = aozora_body_range(source);
        Self::declare(source, body.start, body.end)
    }

    #[must_use]
    pub fn header(&self) -> core::ops::Range<usize> {
        self.header.clone()
    }

    #[must_use]
    pub fn body(&self) -> core::ops::Range<usize> {
        self.body.clone()
    }

    #[must_use]
    pub fn tail(&self) -> core::ops::Range<usize> {
        self.tail.clone()
    }

    /// The two metadata regions, in file order. They are recorded separately
    /// so a failure localizes to one end of the file, but they qualify under a
    /// single conjunctive predicate.
    #[must_use]
    pub fn metadata(&self) -> [core::ops::Range<usize>; 2] {
        [self.header(), self.tail()]
    }

    #[must_use]
    pub fn decoded_len(&self) -> usize {
        self.tail.end
    }
}

/// Returns the Aozora BODY range and TAIL start of a source text: the single
/// authority for "where does the body start/end", shared by the parser
/// (`ab-aat` sanitize stage) and the checker (`ab-check::body_text`)
/// so the two sides of any source↔AAT comparison cannot drift.
///
/// Head: the header is cut ONLY when the region between the first two
/// dash-run separator lines carries the editorial legend
/// (テキスト中に現れる記号について / 《》：ルビ / ［＃］：入力者注). Dash-runs used as scene or
/// poem dividers carry no legend and leave the body uncut. With no separators,
/// a two-line title/author header followed by a blank line is recognized only
/// when a source tail boundary is present.
///
/// Tail starts at an indent-trimmed `底本：` line or a standalone
/// `［＃本文終わり］` marker, allowing surrounding whitespace. The boundary
/// line remains in the tail; preceding line endings are trimmed from the body.
/// Prose mentions and `翻訳の底本：` alone do not establish a boundary.
/// Without a boundary, both body end and tail start are `source.len()`.
#[must_use]
pub fn aozora_body_range(source: &str) -> (core::ops::Range<usize>, usize) {
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
        if legend.contains("テキスト中に現れる記号について")
            || legend.contains("《》：ルビ")
            || legend
                .lines()
                .any(|line| line.starts_with("［＃］：入力者注"))
        {
            body_start = skip_blank_lines(source, separators[1].1);
        }
    }
    if body_start == 0 {
        body_start = hyoki_note_header_end(source);
    }
    if body_start == 0 && separators.is_empty() {
        body_start = plain_title_author_header_end(source);
    }

    let mut body_end = source.len();
    let mut tail_start = source.len();
    let mut cursor = body_start;
    for line in source[body_start..].split_inclusive('\n') {
        if starts_source_tail(line) {
            body_end = source[..cursor].trim_end_matches(['\n', '\r']).len();
            tail_start = cursor;
            break;
        }
        cursor += line.len();
    }

    (body_start..body_end, tail_start)
}

fn starts_source_tail(line: &str) -> bool {
    line.trim_start().starts_with("底本：") || line.trim() == "［＃本文終わり］"
}

/// A separator-free Aozora header has exactly two nonempty title/author
/// lines followed by a blank line. Require a source tail boundary so isolated
/// body snippets retain their first lines.
fn plain_title_author_header_end(source: &str) -> usize {
    let mut lines = source.split_inclusive('\n');
    let Some(title) = lines.next() else { return 0 };
    let Some(author) = lines.next() else { return 0 };
    let Some(blank) = lines.next() else { return 0 };
    if title.trim().is_empty()
        || author.trim().is_empty()
        || !blank.trim().is_empty()
        || title.contains(['［', '］'])
        || author.contains(['［', '］'])
        || !source.lines().any(starts_source_tail)
    {
        return 0;
    }
    skip_blank_lines(source, title.len() + author.len() + blank.len())
}

fn is_aozora_separator(line: &str) -> bool {
    let trimmed = line.trim();
    trimmed.len() >= 10 && trimmed.chars().all(|ch| ch == '-')
}

/// Variant editorial header: a bracketed ［表記について］ heading within the
/// first 15 lines, whose note block (example text carries literal marker
/// syntax such as 「《ルビ》」) runs to the next dash- or equals-run
/// separator line. Returns the body start after that separator, or 0 when
/// the shape is absent.
fn hyoki_note_header_end(source: &str) -> usize {
    let mut offset = 0_usize;
    let mut heading_seen = false;
    for (idx, line) in source.split_inclusive('\n').enumerate() {
        let end = offset + line.len();
        let trimmed = line.trim();
        if !heading_seen {
            if idx >= 15 {
                return 0;
            }
            if (trimmed.starts_with('［') || trimmed.starts_with('['))
                && trimmed.contains("表記について")
            {
                heading_seen = true;
            }
        } else {
            let is_run =
                |ch: char| trimmed.chars().count() >= 10 && trimmed.chars().all(|c| c == ch);
            if is_run('-') || is_run('=') {
                return skip_blank_lines(source, end);
            }
        }
        offset = end;
    }
    0
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

#[must_use]
pub fn comparison_lossy_body(txt: &str) -> Cow<'_, str> {
    if !needs_lossy_projection(txt) {
        return Cow::Borrowed(txt);
    }

    let events = source_events(txt);
    Cow::Owned(comparison_lossy_body_from_events(&events))
}

#[must_use]
pub fn comparison_lossy_body_from_events(events: &[SourceEvent<'_>]) -> String {
    let mut out = String::new();
    for event in events {
        match event.kind {
            SourceEventKind::Text(value) => out.push_str(value),
            SourceEventKind::Ruby {
                base_source: Some(base),
                ..
            } => out.push_str(&comparison_lossy_body(base)),
            SourceEventKind::Ruby {
                base_source: None, ..
            }
            | SourceEventKind::Gaiji { .. }
            | SourceEventKind::Command { .. }
            | SourceEventKind::SegmentBoundary { .. } => {}
            SourceEventKind::EditorialNote {
                kind: EditorialNoteKind::BottomTextCorrection,
                ..
            } => trim_note_prefix(&mut out),
            SourceEventKind::EditorialNote { .. } => {}
        }
    }

    remove_bottom_note_fragments(&out)
}

#[must_use]
pub fn source_markers(txt: &str) -> Vec<SourceMarker<'_>> {
    scan_markers(txt)
        .into_iter()
        .map(|marker| SourceMarker {
            span: marker.span,
            raw: marker.raw,
            body: marker.body,
            kind: marker.kind,
        })
        .collect()
}

#[must_use]
pub fn source_events(txt: &str) -> Vec<SourceEvent<'_>> {
    let mut events = Vec::new();
    let mut offset = 0;
    let mut text_start = 0;
    let mut text_start_line = 1;
    let mut line = 1;
    while offset < txt.len() {
        if let Some(marker) = scan_next_marker(txt, offset, line) {
            match marker.event {
                RawMarkerEvent::Emit(kind) => {
                    push_text_event(txt, &mut events, &mut text_start, text_start_line, offset);
                    events.push(SourceEvent {
                        span: marker.span,
                        kind,
                    });
                    offset = marker.span.end;
                    text_start = offset;
                    text_start_line = line;
                    continue;
                }
                RawMarkerEvent::SkipBytes(bytes) => {
                    push_text_event(txt, &mut events, &mut text_start, text_start_line, offset);
                    offset += bytes;
                    text_start = offset;
                    text_start_line = line;
                    continue;
                }
                RawMarkerEvent::PreserveText => {}
            }
        }

        let rest = &txt[offset..];
        let ch = rest.chars().next().expect("non-empty rest has a char");
        if matches!(ch, '※' | '｜') {
            push_text_event(txt, &mut events, &mut text_start, text_start_line, offset);
            offset += ch.len_utf8();
            text_start = offset;
            text_start_line = line;
        } else {
            offset += ch.len_utf8();
            if ch == '\n' {
                line += 1;
            }
        }
    }
    push_text_event(
        txt,
        &mut events,
        &mut text_start,
        text_start_line,
        txt.len(),
    );
    events
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct RawMarker<'a> {
    span: SourceSpan,
    raw: &'a str,
    body: &'a str,
    kind: SourceMarkerKind,
    event: RawMarkerEvent<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum RawMarkerEvent<'a> {
    Emit(SourceEventKind<'a>),
    PreserveText,
    SkipBytes(usize),
}

fn scan_markers(txt: &str) -> Vec<RawMarker<'_>> {
    let mut markers = Vec::new();
    let mut offset = 0;
    let mut line = 1;
    while offset < txt.len() {
        if let Some(marker) = scan_next_marker(txt, offset, line) {
            let end = marker.span.end;
            line += count_newlines(&txt[offset..end]);
            markers.push(marker);
            offset = end;
            continue;
        }

        let rest = &txt[offset..];
        let ch = rest.chars().next().expect("non-empty rest has a char");
        offset += ch.len_utf8();
        if ch == '\n' {
            line += 1;
        }
    }
    markers
}

fn scan_next_marker(txt: &str, offset: usize, line: usize) -> Option<RawMarker<'_>> {
    let rest = &txt[offset..];

    if let Some(notation) = ["／＼", "／″＼"]
        .into_iter()
        .find(|notation| rest.starts_with(notation))
    {
        let end = offset + notation.len();
        return Some(raw_marker(
            txt,
            offset,
            end,
            line,
            SourceMarkerKind::IterationNotation,
            offset,
            end,
            RawMarkerEvent::PreserveText,
        ));
    }

    if let Some((note_end, content_start)) = bottom_text_correction_note_bounds(txt, offset) {
        return Some(raw_marker(
            txt,
            offset,
            note_end,
            line,
            SourceMarkerKind::EditorialNoteBottomTextCorrection,
            content_start,
            note_end,
            RawMarkerEvent::Emit(SourceEventKind::EditorialNote {
                raw: &txt[offset..note_end],
                kind: EditorialNoteKind::BottomTextCorrection,
            }),
        ));
    }
    if let Some((note_end, target, source)) = ruby_correction_note_bounds(txt, offset) {
        return Some(raw_marker(
            txt,
            offset,
            note_end,
            line,
            SourceMarkerKind::EditorialNoteRubyCorrection,
            offset,
            note_end,
            RawMarkerEvent::Emit(SourceEventKind::EditorialNote {
                raw: &txt[offset..note_end],
                kind: EditorialNoteKind::RubyCorrection {
                    target_reading: target,
                    source_reading: source,
                },
            }),
        ));
    }
    if let Some(note_end) = terminal_provenance_note_end(txt, offset) {
        return Some(raw_marker(
            txt,
            offset,
            note_end,
            line,
            SourceMarkerKind::SegmentBoundaryTerminalProvenance,
            offset,
            note_end,
            RawMarkerEvent::Emit(SourceEventKind::SegmentBoundary {
                kind: SegmentBoundaryKind::TerminalProvenanceNote,
            }),
        ));
    }
    if rest.starts_with("※［＃") {
        let content_start = offset + "※［＃".len();
        if let Some(content_end) = gaiji_end_on_same_line(txt, content_start, '］') {
            let marker_end = content_end + '］'.len_utf8();
            return Some(raw_marker(
                txt,
                offset,
                marker_end,
                line,
                SourceMarkerKind::GaijiFullwidth,
                content_start,
                content_end,
                RawMarkerEvent::Emit(SourceEventKind::Gaiji {
                    description: &txt[content_start..content_end],
                }),
            ));
        }
        return Some(raw_marker(
            txt,
            offset,
            content_start,
            line,
            SourceMarkerKind::MalformedGaiji,
            offset,
            content_start,
            RawMarkerEvent::SkipBytes('※'.len_utf8()),
        ));
    }
    if rest.starts_with("※[#") {
        let content_start = offset + "※[#".len();
        if let Some(content_end) = gaiji_end_on_same_line(txt, content_start, ']') {
            let marker_end = content_end + 1;
            return Some(raw_marker(
                txt,
                offset,
                marker_end,
                line,
                SourceMarkerKind::GaijiAscii,
                content_start,
                content_end,
                RawMarkerEvent::Emit(SourceEventKind::Gaiji {
                    description: &txt[content_start..content_end],
                }),
            ));
        }
        return Some(raw_marker(
            txt,
            offset,
            content_start,
            line,
            SourceMarkerKind::MalformedGaijiAscii,
            offset,
            content_start,
            RawMarkerEvent::SkipBytes('※'.len_utf8()),
        ));
    }
    if rest.starts_with("［＃") {
        let content_start = offset + "［＃".len();
        if let Some(content_end) = command_end(txt, content_start, '］') {
            let marker_end = content_end + '］'.len_utf8();
            return Some(raw_marker(
                txt,
                offset,
                marker_end,
                line,
                SourceMarkerKind::CommandFullwidth,
                content_start,
                content_end,
                RawMarkerEvent::Emit(SourceEventKind::Command {
                    body: &txt[content_start..content_end],
                }),
            ));
        }
        return Some(raw_marker(
            txt,
            offset,
            content_start,
            line,
            SourceMarkerKind::MalformedCommand,
            offset,
            content_start,
            RawMarkerEvent::PreserveText,
        ));
    }
    if rest.starts_with("[#") {
        let content_start = offset + "[#".len();
        if let Some(content_end) = command_end(txt, content_start, ']') {
            let marker_end = content_end + 1;
            return Some(raw_marker(
                txt,
                offset,
                marker_end,
                line,
                SourceMarkerKind::CommandAscii,
                content_start,
                content_end,
                RawMarkerEvent::Emit(SourceEventKind::Command {
                    body: &txt[content_start..content_end],
                }),
            ));
        }
        return Some(raw_marker(
            txt,
            offset,
            content_start,
            line,
            SourceMarkerKind::MalformedCommandAscii,
            offset,
            content_start,
            RawMarkerEvent::PreserveText,
        ));
    }
    if rest.starts_with('｜') {
        let base_start = offset + '｜'.len_utf8();
        if starts_with_ruby_marker_legend_delimiter(txt, base_start) {
            return None;
        }
        if let Some((base_end, reading_start, reading_end, marker_end)) =
            explicit_ruby_bounds(txt, base_start)
        {
            return Some(raw_marker(
                txt,
                offset,
                marker_end,
                line,
                SourceMarkerKind::RubyExplicit,
                base_start,
                marker_end,
                RawMarkerEvent::Emit(SourceEventKind::Ruby {
                    base_source: Some(&txt[base_start..base_end]),
                    reading: &txt[reading_start..reading_end],
                }),
            ));
        }
        return None;
    }
    if rest.starts_with('《') {
        let reading_start = offset + '《'.len_utf8();
        if let Some(reading_end) = marker_end_on_same_line(txt, reading_start, '》') {
            let marker_end = reading_end + '》'.len_utf8();
            return Some(raw_marker(
                txt,
                offset,
                marker_end,
                line,
                SourceMarkerKind::RubyImplicit,
                reading_start,
                reading_end,
                RawMarkerEvent::Emit(SourceEventKind::Ruby {
                    base_source: None,
                    reading: &txt[reading_start..reading_end],
                }),
            ));
        }
        return Some(raw_marker(
            txt,
            offset,
            reading_start,
            line,
            SourceMarkerKind::MalformedImplicitRuby,
            offset,
            reading_start,
            RawMarkerEvent::PreserveText,
        ));
    }
    if rest.starts_with('〔') {
        let body_start = offset + '〔'.len_utf8();
        if let Some(body_end) = marker_end(txt, body_start, '〕') {
            if body_start == body_end {
                return None;
            }
            let marker_end = body_end + '〕'.len_utf8();
            let body = &txt[body_start..body_end];
            return Some(raw_marker(
                txt,
                offset,
                marker_end,
                line,
                bracket_marker_kind(body),
                body_start,
                body_end,
                RawMarkerEvent::PreserveText,
            ));
        }
        return Some(raw_marker(
            txt,
            offset,
            body_start,
            line,
            SourceMarkerKind::MalformedAccentNotation,
            offset,
            body_start,
            RawMarkerEvent::PreserveText,
        ));
    }

    None
}

fn starts_with_ruby_marker_legend_delimiter(txt: &str, offset: usize) -> bool {
    txt[offset..].starts_with(['：', '；'])
}

#[allow(clippy::too_many_arguments)]
fn raw_marker<'a>(
    txt: &'a str,
    start: usize,
    end: usize,
    line: usize,
    kind: SourceMarkerKind,
    body_start: usize,
    body_end: usize,
    event: RawMarkerEvent<'a>,
) -> RawMarker<'a> {
    RawMarker {
        span: span_for(start, end, line),
        raw: &txt[start..end],
        body: &txt[body_start..body_end],
        kind,
        event,
    }
}

#[must_use]
pub fn remove_bottom_note_fragments(txt: &str) -> String {
    if !txt.contains('」') {
        return txt.to_owned();
    }

    let mut out = String::with_capacity(txt.len());
    let mut offset = 0;
    while offset < txt.len() {
        let rest = &txt[offset..];
        if rest.starts_with("」は底本では「") {
            trim_note_prefix(&mut out);
            offset += "」は底本では「".len();
            offset = skip_until_any_bracket(txt, offset);
            continue;
        }
        if rest.starts_with("」はママ") {
            trim_note_prefix(&mut out);
            offset += "」はママ".len();
            offset = skip_until_any_bracket(txt, offset);
            continue;
        }
        let ch = rest.chars().next().expect("non-empty rest has a char");
        out.push(ch);
        offset += ch.len_utf8();
    }
    out
}

#[must_use]
pub fn source_annotations(body: &str) -> SourceAnnotations<'_> {
    let events = source_events(body);
    source_annotations_from_events(&events, false)
}

#[must_use]
pub fn source_annotations_for_validation(body: &str) -> SourceAnnotations<'_> {
    let events = source_events(body);
    source_annotations_from_events(&events, true)
}

#[must_use]
pub fn source_annotations_both(body: &str) -> SourceAnnotationsBoth<'_> {
    let events = source_events(body);
    source_annotations_both_from_events(&events)
}

#[must_use]
pub fn source_annotations_from_events<'a>(
    events: &[SourceEvent<'a>],
    skip_gaiji_orphan_ruby: bool,
) -> SourceAnnotations<'a> {
    let mut annotations = SourceAnnotations::default();
    let mut last_gaiji_end = None;
    for event in events {
        match &event.kind {
            SourceEventKind::Gaiji { description } => {
                annotations.gaiji_descriptions.push(LocatedMarker {
                    value: description,
                    byte_offset: event.span.start,
                    line: event.span.line,
                });
                last_gaiji_end = Some(event.span.end);
            }
            SourceEventKind::Ruby { reading, .. } => {
                if !(skip_gaiji_orphan_ruby && last_gaiji_end == Some(event.span.start)) {
                    annotations.ruby_readings.push(LocatedMarker {
                        value: reading,
                        byte_offset: event.span.start,
                        line: event.span.line,
                    });
                    collect_gaiji_markers(
                        reading,
                        event.span.start + '《'.len_utf8(),
                        event.span.line,
                        &mut annotations,
                    );
                }
                last_gaiji_end = None;
            }
            SourceEventKind::Text(_)
            | SourceEventKind::Command { .. }
            | SourceEventKind::EditorialNote { .. }
            | SourceEventKind::SegmentBoundary { .. } => {
                last_gaiji_end = None;
            }
        }
    }
    annotations
}

#[must_use]
pub fn source_annotations_both_from_events<'a>(
    events: &[SourceEvent<'a>],
) -> SourceAnnotationsBoth<'a> {
    let mut validation = SourceAnnotations::default();
    let mut full = SourceAnnotations::default();
    let mut last_gaiji_end = None;
    for event in events {
        match &event.kind {
            SourceEventKind::Gaiji { description } => {
                let marker = LocatedMarker {
                    value: description,
                    byte_offset: event.span.start,
                    line: event.span.line,
                };
                full.gaiji_descriptions.push(marker);
                validation.gaiji_descriptions.push(marker);
                last_gaiji_end = Some(event.span.end);
            }
            SourceEventKind::Ruby { reading, .. } => {
                let is_orphan = last_gaiji_end == Some(event.span.start);
                if !is_orphan {
                    validation.ruby_readings.push(LocatedMarker {
                        value: reading,
                        byte_offset: event.span.start,
                        line: event.span.line,
                    });
                    collect_gaiji_markers(
                        reading,
                        event.span.start + '《'.len_utf8(),
                        event.span.line,
                        &mut validation,
                    );
                }
                full.ruby_readings.push(LocatedMarker {
                    value: reading,
                    byte_offset: event.span.start,
                    line: event.span.line,
                });
                collect_gaiji_markers(
                    reading,
                    event.span.start + '《'.len_utf8(),
                    event.span.line,
                    &mut full,
                );
                last_gaiji_end = None;
            }
            SourceEventKind::Text(_)
            | SourceEventKind::Command { .. }
            | SourceEventKind::EditorialNote { .. }
            | SourceEventKind::SegmentBoundary { .. } => {
                last_gaiji_end = None;
            }
        }
    }
    SourceAnnotationsBoth { validation, full }
}

#[must_use]
pub fn gaiji_marker_count(body: &str) -> usize {
    let mut count = 0;
    let mut offset = 0;
    while offset < body.len() {
        let rest = &body[offset..];
        if rest.starts_with("※［＃") {
            let content_start = offset + "※［＃".len();
            if let Some(end) = marker_end_on_same_line(body, content_start, '］') {
                count += 1;
                offset = end + '］'.len_utf8();
                continue;
            }
        }
        if rest.starts_with("※[#") {
            let content_start = offset + "※[#".len();
            if let Some(end) = marker_end_on_same_line(body, content_start, ']') {
                count += 1;
                offset = end + 1;
                continue;
            }
        }
        let ch = rest.chars().next().expect("non-empty rest has a char");
        offset += ch.len_utf8();
    }
    count
}

fn collect_gaiji_markers<'a>(
    text: &'a str,
    base_offset: usize,
    line_number: usize,
    annotations: &mut SourceAnnotations<'a>,
) {
    let mut offset = 0;
    while offset < text.len() {
        let rest = &text[offset..];
        if rest.starts_with("※［＃") {
            let content_start = offset + "※［＃".len();
            if let Some(content_end) = marker_end_on_same_line(text, content_start, '］') {
                annotations.gaiji_descriptions.push(LocatedMarker {
                    value: &text[content_start..content_end],
                    byte_offset: base_offset + offset,
                    line: line_number,
                });
                offset = content_end + '］'.len_utf8();
                continue;
            }
        }
        if rest.starts_with("※[#") {
            let content_start = offset + "※[#".len();
            if let Some(content_end) = marker_end_on_same_line(text, content_start, ']') {
                annotations.gaiji_descriptions.push(LocatedMarker {
                    value: &text[content_start..content_end],
                    byte_offset: base_offset + offset,
                    line: line_number,
                });
                offset = content_end + 1;
                continue;
            }
        }
        let ch = rest.chars().next().expect("non-empty rest has a char");
        offset += ch.len_utf8();
    }
}

#[must_use]
pub fn needs_lossy_projection(txt: &str) -> bool {
    txt.find(['※', '《', '｜', '［', '[', '」']).is_some()
}

fn explicit_ruby_bounds(txt: &str, base_start: usize) -> Option<(usize, usize, usize, usize)> {
    let base_end = txt[base_start..]
        .find('《')
        .map(|offset| base_start + offset)?;

    if txt[base_start..base_end]
        .chars()
        .any(|ch| matches!(ch, '\r' | '\n' | '》'))
    {
        return None;
    }
    let reading_start = base_end + '《'.len_utf8();
    let reading_end = marker_end_on_same_line(txt, reading_start, '》')?;
    Some((
        base_end,
        reading_start,
        reading_end,
        reading_end + '》'.len_utf8(),
    ))
}

fn push_text_event<'a>(
    txt: &'a str,
    events: &mut Vec<SourceEvent<'a>>,
    text_start: &mut usize,
    line: usize,
    text_end: usize,
) {
    if *text_start >= text_end {
        return;
    }
    events.push(SourceEvent {
        span: span_for(*text_start, text_end, line),
        kind: SourceEventKind::Text(&txt[*text_start..text_end]),
    });
}

fn span_for(start: usize, end: usize, line: usize) -> SourceSpan {
    SourceSpan { start, end, line }
}

fn ruby_correction_note_bounds(txt: &str, offset: usize) -> Option<(usize, &str, &str)> {
    let rest = &txt[offset..];
    let prefix = "［ルビの「";
    if !rest.starts_with(prefix) {
        return None;
    }
    let target_start = offset + prefix.len();
    let separator = "」は底本では「";
    let target_end = txt[target_start..]
        .find(separator)
        .map(|inner| target_start + inner)?;
    if txt[target_start..target_end]
        .chars()
        .any(|ch| matches!(ch, '\r' | '\n'))
    {
        return None;
    }
    let source_start = target_end + separator.len();
    let suffix = "」］";
    let source_end = txt[source_start..]
        .find(suffix)
        .map(|inner| source_start + inner)?;
    if txt[source_start..source_end]
        .chars()
        .any(|ch| matches!(ch, '\r' | '\n'))
    {
        return None;
    }
    Some((
        source_end + suffix.len(),
        &txt[target_start..target_end],
        &txt[source_start..source_end],
    ))
}

fn bottom_text_correction_note_bounds(txt: &str, offset: usize) -> Option<(usize, usize)> {
    ["」は底本では「", "」はママ"]
        .iter()
        .find(|prefix| txt[offset..].starts_with(**prefix))
        .map(|prefix| (skip_until_any_bracket(txt, offset), offset + prefix.len()))
}

fn terminal_provenance_note_end(txt: &str, offset: usize) -> Option<usize> {
    let rest = &txt[offset..];
    let prefix = "［＃地付き］（";
    if !rest.starts_with(prefix) {
        return None;
    }
    let note_content_start = offset + prefix.len();
    let note_content_end = marker_end_on_same_line(txt, note_content_start, '）')?;
    let note_end = note_content_end + '）'.len_utf8();
    let after = txt[note_end..].trim_start_matches(['\r', '\n', ' ', '　', '\t']);
    if after.starts_with("底本：") {
        Some(note_end)
    } else {
        None
    }
}

fn marker_end_on_same_line(text: &str, content_start: usize, end_marker: char) -> Option<usize> {
    for (offset, ch) in text[content_start..].char_indices() {
        if ch == end_marker {
            return Some(content_start + offset);
        }
        if matches!(ch, '\r' | '\n') {
            return None;
        }
    }
    None
}

fn marker_end(text: &str, content_start: usize, end_marker: char) -> Option<usize> {
    let mut offset = content_start;
    while offset < text.len() {
        let rest = &text[offset..];
        let command = if rest.starts_with("［＃") {
            Some(("［＃".len(), '］'))
        } else if rest.starts_with("[#") {
            Some(("[#".len(), ']'))
        } else {
            None
        };
        if let Some((prefix_len, close)) = command
            && let Some(end) = command_end(text, offset + prefix_len, close)
        {
            offset = end + close.len_utf8();
            continue;
        }
        let ch = rest.chars().next()?;
        if ch == end_marker {
            return Some(offset);
        }
        offset += ch.len_utf8();
    }
    None
}

fn bracket_marker_kind(body: &str) -> SourceMarkerKind {
    if body.is_ascii() && !body.contains(['\r', '\n']) {
        SourceMarkerKind::AccentNotation
    } else {
        SourceMarkerKind::BracketNote
    }
}

fn count_newlines(value: &str) -> usize {
    value.bytes().filter(|byte| *byte == b'\n').count()
}

fn quoted_text_end_on_same_line(text: &str, start: usize, end_marker: char) -> Option<usize> {
    if let Some(after_open) = text[start..].strip_prefix('「')
        && let Some(after_marker) = after_open.strip_prefix(end_marker)
        && after_marker.starts_with('」')
    {
        return Some(start + '「'.len_utf8() + end_marker.len_utf8() + '」'.len_utf8());
    }
    let mut depth = 0_u32;
    for (offset, ch) in text[start..].char_indices() {
        match ch {
            '「' => depth += 1,
            '」' => {
                depth -= 1;
                if depth == 0 {
                    return Some(start + offset + ch.len_utf8());
                }
            }
            '\r' | '\n' => return None,
            ch if ch == end_marker => return None,
            _ => {}
        }
    }
    None
}

fn gaiji_end_on_same_line(text: &str, content_start: usize, end_marker: char) -> Option<usize> {
    let end = command_end(text, content_start, end_marker)?;
    (!text[content_start..end].contains(['\r', '\n'])).then_some(end)
}

fn command_end(text: &str, content_start: usize, end_marker: char) -> Option<usize> {
    let multiline = text[content_start..].starts_with("入力者註：")
        || text[content_start..].starts_with("入力者注：");
    let mut offset = content_start;
    while offset < text.len() {
        let rest = &text[offset..];
        if rest.starts_with('「')
            && let Some(end) = quoted_text_end_on_same_line(text, offset, end_marker)
        {
            offset = end;
            continue;
        }
        if rest.starts_with("※［＃") {
            let nested_start = offset + "※［＃".len();
            if let Some(end) = gaiji_end_on_same_line(text, nested_start, '］') {
                offset = end + '］'.len_utf8();
                continue;
            }
        }
        if rest.starts_with("※[#") {
            let nested_start = offset + "※[#".len();
            if let Some(end) = gaiji_end_on_same_line(text, nested_start, ']') {
                offset = end + 1;
                continue;
            }
        }
        if rest.starts_with("［＃") {
            let nested_start = offset + "［＃".len();
            if let Some(end) = command_end(text, nested_start, '］') {
                offset = end + '］'.len_utf8();
                continue;
            }
        }
        if rest.starts_with("[#") {
            let nested_start = offset + "[#".len();
            if let Some(end) = command_end(text, nested_start, ']') {
                offset = end + 1;
                continue;
            }
        }
        if end_marker == '］' && rest.starts_with('［') {
            let nested_start = offset + '［'.len_utf8();
            if let Some(end) = marker_end_on_same_line(text, nested_start, '］') {
                offset = end + '］'.len_utf8();
                continue;
            }
        }
        if end_marker == ']' && rest.starts_with('[') {
            let nested_start = offset + 1;
            if let Some(end) = marker_end_on_same_line(text, nested_start, ']') {
                offset = end + 1;
                continue;
            }
        }

        let ch = rest.chars().next().expect("non-empty rest has a char");
        if ch == end_marker {
            return Some(offset);
        }
        if matches!(ch, '\r' | '\n') {
            if !multiline {
                return None;
            }
            let next = rest.strip_prefix("\r\n").unwrap_or(&rest[ch.len_utf8()..]);
            let next_line = next.split(['\r', '\n']).next()?.trim();
            if next_line.is_empty()
                || starts_source_tail(next_line)
                || next_line.starts_with("［＃")
            {
                return None;
            }
            offset = text.len() - next.len();
            continue;
        }
        offset += ch.len_utf8();
    }
    None
}

fn skip_until_any_bracket(txt: &str, offset: usize) -> usize {
    let rest = &txt[offset..];
    let fullwidth = rest.find('］');
    let ascii = rest.find(']');
    match (fullwidth, ascii) {
        (Some(left), Some(right)) if left <= right => offset + left + '］'.len_utf8(),
        (Some(_), Some(right)) => offset + right + 1,
        (Some(left), None) => offset + left + '］'.len_utf8(),
        (None, Some(right)) => offset + right + 1,
        (None, None) => txt.len(),
    }
}

fn trim_note_prefix(out: &mut String) {
    let Some(close_quote) = out.rfind('「') else {
        return;
    };
    let prefix = &out[..close_quote];
    let Some(start) = prefix
        .char_indices()
        .rev()
        .find_map(|(offset, ch)| is_note_boundary(ch).then_some(offset + ch.len_utf8()))
    else {
        out.truncate(0);
        return;
    };
    out.truncate(start);
}

fn is_note_boundary(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '、' | '。' | '，' | '．')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn aozora_body_range_cuts_legend_fenced_header() {
        let src = "題名\n著者\n\n----------\n【テキスト中に現れる記号について】\n《》：ルビ\n----------\n\n本文です。\n\n底本：底本社\n";
        let (body, tail_start) = aozora_body_range(src);
        assert_eq!(&src[body], "本文です。");
        assert!(src[tail_start..].starts_with("底本："));
    }

    #[test]
    fn aozora_body_range_cuts_editorial_legend_without_ruby() {
        let src = "“現代風俗”に就いて\n岸田國士\n\n----------\n［＃］：入力者注　主に外字の説明や、傍点の位置の指定\n（例）※［＃二の字点、1-2-22］\n\n〔〕：アクセント分解された欧文をかこむ\n（例）〔moe&urs〕\n----------\n\n　僕は近頃。\n\n底本：底本社\n";
        let (body, tail_start) = aozora_body_range(src);
        assert_eq!(&src[body], "　僕は近頃。");
        assert!(src[tail_start..].starts_with("底本："));
    }

    #[test]
    fn aozora_body_range_cuts_hyoki_note_header_at_dash_run() {
        // Variant editorial header (9 works corpus-wide): a bracketed
        // ［表記について］ heading near the top, notes whose example text
        // contains literal marker syntax (「《ルビ》」), closed by a
        // dash-run. The header ends at that separator.
        let src = "ガドルフの百合\n宮沢賢治\n\n［表記について］\n●ルビは「《ルビ》」の形式で処理した。\n------------------\n本文《ほんぶん》です。\n";
        let (body, _) = aozora_body_range(src);
        assert_eq!(&src[body], "本文《ほんぶん》です。\n");
    }

    #[test]
    fn aozora_body_range_cuts_hyoki_note_header_at_equals_run() {
        // 000067_395 shape: the note block is closed by an equals-run.
        let src = "散文詩集\n著者\n\n［表記について］\n●ルビは「《ルビ》」の形式で処理した。\n==================\n　海\n";
        let (body, _) = aozora_body_range(src);
        assert_eq!(&src[body], "　海\n");
    }

    #[test]
    fn aozora_body_range_ignores_hyoki_note_deep_in_body() {
        // The variant heading only counts near the top of the file; a
        // bracketed mention later is body text.
        let line = "本文の一行。\n";
        let mut src = line.repeat(20);
        src.push_str("［表記について］\n後記の説明。\n------------------\nあとがき。\n");
        let (body, _) = aozora_body_range(&src);
        assert_eq!(body.start, 0);
    }

    #[test]
    fn aozora_body_range_keeps_content_dash_runs_uncut() {
        // Dash-run lines used as scene/poem dividers carry no legend
        // between the first two runs, so the header cut must not fire
        // (e.g. the 小熊秀雄全集 volumes, poem-per-fence collections).
        let src = "小熊秀雄全集-1\n短歌集\n\n第一歌\n--------------------\n第二歌\n--------------------\n第三歌\n";
        let (body, tail_start) = aozora_body_range(src);
        assert_eq!(body, 0..src.len());
        assert_eq!(tail_start, src.len());
    }

    #[test]
    fn separator_free_title_author_header_does_not_enter_the_body() {
        let source = "こころ\n今野大力\n\nこころ　こころ\nくるしいこころ\n\n底本：作品集\n";
        let (body, tail) = aozora_body_range(source);
        assert_eq!(&source[body], "こころ　こころ\nくるしいこころ");
        assert_eq!(&source[tail..], "底本：作品集\n");
        let snippet = "こころ　こころ\nくるしいこころ\n\n次の連\n";
        assert_eq!(aozora_body_range(snippet).0, 0..snippet.len());
        let no_boundary = "こころ\n今野大力\nこころ　こころ\n底本：作品集\n";
        assert_eq!(aozora_body_range(no_boundary).0.start, 0);
    }

    #[test]
    fn aozora_body_range_tail_cut_is_line_anchored() {
        // 翻訳の底本： mid-body must not truncate; the trailer is the
        // line whose (indent-trimmed) start is 底本：.
        let src = "本文の前半。\n翻訳の底本：原書\n本文の後半。\n底本：底本社\n";
        let (body, tail_start) = aozora_body_range(src);
        assert_eq!(&src[body], "本文の前半。\n翻訳の底本：原書\n本文の後半。");
        assert!(src[tail_start..].starts_with("底本："));
    }

    // Boundary cases for the three-region partition Q15 derives from this
    // function. They are here rather than in the consumer because this is the
    // one authority for where the body starts and ends, and a partition is
    // only as sound as the range it is derived from.
    //
    // Region set, in this function's own coordinates:
    //   header [0, body_start)   body [body_start, body_end)   tail [body_end, len)
    // The tail is anchored on `body_end`, not on the returned
    // `tail_start`. The two differ, and the next test says by how much.

    fn assert_partitions(source: &str, label: &str) {
        let regions = SourceRegions::derive(source).expect(label);
        let (header, body, tail) = (regions.header(), regions.body(), regions.tail());
        assert_eq!(header.start, 0, "{label}: header must open the file");
        assert_eq!(header.end, body.start, "{label}: header/body gap");
        assert_eq!(body.end, tail.start, "{label}: body/tail gap");
        assert_eq!(tail.end, source.len(), "{label}: tail must close the file");
        let covered = header.len() + body.len() + tail.len();
        assert_eq!(covered, source.len(), "{label}: conservation");
    }

    #[test]
    fn the_returned_tail_start_leaves_a_gap_that_body_end_does_not() {
        // This is the defect the partition must resolve, pinned as behaviour
        // rather than left as prose. `body_end` is trim-adjusted for trailing
        // newlines (:146) while `tail_start` is not (:147), so whenever a tail
        // exists at least one byte lies between them and belongs to no region.
        let src = "本文です。\n\n\n底本：底本社\n";
        let (body, tail_start) = aozora_body_range(src);
        assert!(
            body.end < tail_start,
            "expected a gap between body_end and tail_start"
        );
        assert_eq!(&src[body.end..tail_start], "\n\n\n");
        // Anchoring the tail on `tail_start` loses those bytes; anchoring it on
        // `body_end` does not. That is why the region set uses `body_end`.
        assert_ne!(
            body.len() + (src.len() - tail_start),
            src.len() - body.start
        );
        assert_partitions(src, "gap");
    }

    #[test]
    fn the_three_regions_close_every_boundary_shape() {
        for (label, src) in [
            // No 底本 line at all: body_end == tail_start == len, empty tail.
            ("no colophon", "本文です。\n"),
            // Fewer than two separator lines: no legend cut, header is empty.
            (
                "one separator",
                "題名\n----------\n本文です。\n底本：底本社\n",
            ),
            ("no separator", "本文です。\n底本：底本社\n"),
            // Legend-fenced header, the ordinary shape.
            (
                "legend header",
                "題名\n著者\n\n----------\n【テキスト中に現れる記号について】\n《》：ルビ\n----------\n\n本文です。\n\n底本：底本社\n",
            ),
            // Bare CR line endings: `split_inclusive('\n')` does not split on
            // them, so the whole file is one line and no separator is seen.
            ("bare cr", "題名\r----------\r本文です。\r底本：底本社\r"),
            // Empty and whitespace-only inputs.
            ("empty", ""),
            ("blank", "\n\n\n"),
            // A colophon with nothing before it.
            ("colophon only", "底本：底本社\n"),
        ] {
            assert_partitions(src, label);
        }
    }

    #[test]
    fn bare_cr_sources_do_not_find_a_colophon_line() {
        // Recorded because it decides which region a bare-CR work's colophon
        // lands in, and the answer is "the body". `split_inclusive('\n')`
        // yields one line for the whole file, so the 底本： line is never seen
        // at a line start. The partition still conserves (the tail is simply
        // empty), but a metadata predicate over the tail measures nothing
        // here; that is a property of the input rather than of the partition.
        let src = "本文です。\r底本：底本社\r";
        let (body, tail_start) = aozora_body_range(src);
        assert_eq!(body, 0..src.len());
        assert_eq!(tail_start, src.len());
        assert_partitions(src, "bare cr colophon");
    }

    #[test]
    fn a_colophon_with_no_body_still_partitions() {
        // `body_start` is 0 and the 底本： line is the first line, so
        // `body_end` trims to 0 and the body region is empty. Every byte is
        // tail. An empty body is a legitimate region, not an error.
        let src = "底本：底本社\n奥付\n";
        let regions = SourceRegions::derive(src).unwrap();
        assert!(regions.header().is_empty());
        assert!(regions.body().is_empty());
        assert_eq!(regions.tail(), 0..src.len());
    }

    #[test]
    fn declaring_a_region_set_that_is_not_a_partition_fails() {
        let src = "本文です。\n底本：底本社\n";
        // Inverted, out of bounds, and mid-character boundaries are all
        // refused. The last matters most: every other guard traced in this
        // area accepted intervals landing mid-character, because it checked
        // only `start <= end <= bound`.
        assert_eq!(
            SourceRegions::declare(src, 6, 3),
            Err(RegionError::Inverted)
        );
        assert_eq!(
            SourceRegions::declare(src, 0, src.len() + 1),
            Err(RegionError::Inverted)
        );
        assert_eq!(
            SourceRegions::declare(src, 1, src.len()),
            Err(RegionError::NotCharBoundary)
        );
        assert!(SourceRegions::declare(src, 0, src.len()).is_ok());
    }

    #[test]
    fn the_metadata_regions_are_the_header_and_tail_in_file_order() {
        let src = "題名\n著者\n\n----------\n【テキスト中に現れる記号について】\n《》：ルビ\n----------\n\n本文です。\n\n底本：底本社\n";
        let regions = SourceRegions::derive(src).unwrap();
        assert_eq!(regions.metadata(), [regions.header(), regions.tail()]);
        assert_eq!(&src[regions.body()], "本文です。");
        assert!(src[regions.header()].contains("《》：ルビ"));
        // The tail absorbs the blank line the body end trimmed, which is the
        // whole reason it is anchored on `body_end` and not on `tail_start`.
        assert_eq!(&src[regions.tail()], "\n\n底本：底本社\n");
        assert_eq!(regions.decoded_len(), src.len());
    }

    #[test]
    fn comparison_lossy_body_without_markup_borrows() {
        let projected = comparison_lossy_body("吾輩は猫");

        assert!(matches!(projected, Cow::Borrowed(_)));
        assert_eq!(projected, "吾輩は猫");
    }

    #[test]
    fn comparison_lossy_body_from_events_matches_legacy() {
        let body = "吾輩《わがはい》は※［＃「口＋世」、U+546D］でも、末尾まで見通す。";
        let expected = comparison_lossy_body(body).into_owned();
        let events = source_events(body);

        assert_eq!(comparison_lossy_body_from_events(&events), expected);
    }

    #[test]
    fn comparison_lossy_body_removes_ruby_gaiji_and_commands() {
        let projected =
            comparison_lossy_body("吾輩《わがはい》は※［＃「口＋世」、U+546D］［＃注記］猫");

        assert_eq!(projected, "吾輩は猫");
    }

    #[test]
    fn comparison_lossy_body_projects_explicit_ruby_base_without_marker() {
        let projected =
            comparison_lossy_body("――『｜あのひとにとって、わたし《ルビ》はなんだろう？」");

        assert_eq!(projected, "――『あのひとにとって、わたしはなんだろう？」");
    }

    #[test]
    fn comparison_lossy_body_removes_orphan_ruby_after_unresolved_gaiji() {
        let projected =
            comparison_lossy_body("ことを、※［＃「口＋愛」、第3水準1-15-23］《おくび》にも");

        assert_eq!(projected, "ことを、にも");
    }

    #[test]
    fn comparison_lossy_body_projects_markup_inside_explicit_ruby_base() {
        let projected = comparison_lossy_body(
            "｜前※［＃「二点しんにょう＋官」、第3水準1-92-56］後《まえあと》",
        );

        assert_eq!(projected, "前後");
    }

    #[test]
    fn comparison_lossy_body_removes_command_with_nested_gaiji_marker() {
        let projected = comparison_lossy_body(
            "豌豆《ゑんどう》［＃「豌豆」は底本では「※［＃「足＋宛」、第3水準1-92-36］豆」］の大さ",
        );

        assert_eq!(projected, "豌豆の大さ");
    }

    #[test]
    fn comparison_lossy_body_preserves_quoted_title_pairs() {
        let projected = comparison_lossy_body(
            "斎はこれを取つて校刻した。是が「狩谷望之審定宋本」の「御注孝経」である。\n次の段。",
        );

        assert_eq!(
            projected,
            "斎はこれを取つて校刻した。是が「狩谷望之審定宋本」の「御注孝経」である。\n次の段。"
        );
    }

    #[test]
    fn comparison_lossy_body_removes_command_with_nested_commands() {
        let projected = comparison_lossy_body(
            "アヌンチヤタ［＃「アヌンチヤタ［＃「アヌンチヤタ」に傍線］」は底本では「アンヌチヤタ［＃「アンヌチヤタ」に傍線］」］ありて",
        );

        assert_eq!(projected, "アヌンチヤタありて");
    }

    #[test]
    fn comparison_lossy_body_removes_unmatched_ruby_delimiters() {
        let projected = comparison_lossy_body(
            "今日｜民族観念［＃「民族観念」に傍点］と呼ぶ。悲憤｜慷慨《こうがい》も知悉《ちしつ》した",
        );

        assert_eq!(projected, "今日民族観念と呼ぶ。悲憤慷慨も知悉した");
    }

    #[test]
    fn source_annotations_collect_ruby_and_gaiji_in_one_scan() {
        let markers = source_annotations(
            "吾輩《わがはい》は※［＃「口＋世」、U+546D］で、※[#ascii-gaiji]もある。",
        );

        assert_eq!(
            markers
                .ruby_readings
                .iter()
                .map(|marker| marker.value)
                .collect::<Vec<_>>(),
            vec!["わがはい"]
        );
        assert_eq!(
            markers
                .gaiji_descriptions
                .iter()
                .map(|marker| marker.value)
                .collect::<Vec<_>>(),
            vec!["「口＋世」、U+546D", "ascii-gaiji"]
        );
    }

    #[test]
    fn source_annotations_both_collects_validation_and_full_variants() {
        let markers = source_annotations_both(
            "ことを、※［＃「口＋愛」、第3水準1-15-23］《おくび》にも※［＃「口＋世」、U+546D］",
        );

        assert_eq!(
            markers
                .validation
                .ruby_readings
                .iter()
                .map(|marker| marker.value)
                .collect::<Vec<_>>(),
            Vec::<&str>::new()
        );
        assert_eq!(
            markers
                .full
                .ruby_readings
                .iter()
                .map(|marker| marker.value)
                .collect::<Vec<_>>(),
            vec!["おくび"]
        );
        assert_eq!(
            markers
                .validation
                .gaiji_descriptions
                .iter()
                .map(|marker| marker.value)
                .collect::<Vec<_>>(),
            vec!["「口＋愛」、第3水準1-15-23", "「口＋世」、U+546D"]
        );
        assert_eq!(
            markers
                .full
                .gaiji_descriptions
                .iter()
                .map(|marker| marker.value)
                .collect::<Vec<_>>(),
            vec!["「口＋愛」、第3水準1-15-23", "「口＋世」、U+546D"]
        );
    }

    #[test]
    fn source_annotations_from_events_matches_string_scan() {
        let body = "ことを、※［＃「口＋愛」、第3水準1-15-23］《おくび》にも※[#ascii-gaiji]もある。";
        let by_string = source_annotations(body);
        let events = source_events(body);
        let by_events = source_annotations_from_events(&events, false);

        assert_eq!(by_string.ruby_readings, by_events.ruby_readings);
        assert_eq!(by_string.gaiji_descriptions, by_events.gaiji_descriptions);
        assert_eq!(
            source_annotations_for_validation(body),
            source_annotations_from_events(&events, true)
        );
    }

    #[test]
    fn source_annotations_both_from_events_matches_string_scan() {
        let body =
            "ことを、※［＃「口＋愛」、第3水準1-15-23］《おくび》にも※［＃「口＋世」、U+546D］";
        let by_string = source_annotations_both(body);
        let by_events = source_annotations_both_from_events(&source_events(body));

        assert_eq!(
            by_string.validation.ruby_readings,
            by_events.validation.ruby_readings
        );
        assert_eq!(
            by_string.validation.gaiji_descriptions,
            by_events.validation.gaiji_descriptions
        );
        assert_eq!(by_string.full.ruby_readings, by_events.full.ruby_readings);
        assert_eq!(
            by_string.full.gaiji_descriptions,
            by_events.full.gaiji_descriptions
        );
    }

    #[test]
    fn source_annotations_collect_gaiji_inside_ruby_text() {
        let markers = source_annotations("淡絹《※［＃濁点付き片仮名ヱ、1-7-84］エル》");

        assert_eq!(
            markers
                .ruby_readings
                .iter()
                .map(|marker| marker.value)
                .collect::<Vec<_>>(),
            vec!["※［＃濁点付き片仮名ヱ、1-7-84］エル"]
        );
        assert_eq!(
            markers
                .gaiji_descriptions
                .iter()
                .map(|marker| marker.value)
                .collect::<Vec<_>>(),
            vec!["濁点付き片仮名ヱ、1-7-84"]
        );
    }

    #[test]
    fn validation_annotations_skip_orphan_ruby_after_gaiji() {
        let markers = source_annotations_for_validation(
            "ことを、※［＃「口＋愛」、第3水準1-15-23］《おくび》にも",
        );

        assert!(markers.ruby_readings.is_empty());
        assert_eq!(markers.gaiji_descriptions.len(), 1);
    }

    #[test]
    fn source_events_classify_non_hash_ruby_correction_notes() {
        let events = source_events(
            "『断頭台《ラギュイヨチーン》［ルビの「ラギュイヨチーン」は底本では「ラギュイヨケーン」］』",
        );

        assert!(events.iter().any(|event| matches!(
            event.kind,
            SourceEventKind::EditorialNote {
                kind: EditorialNoteKind::RubyCorrection {
                    target_reading: "ラギュイヨチーン",
                    source_reading: "ラギュイヨケーン",
                },
                ..
            }
        )));
        assert_eq!(
            comparison_lossy_body(
                "『断頭台《ラギュイヨチーン》［ルビの「ラギュイヨチーン」は底本では「ラギュイヨケーン」］』"
            ),
            "『断頭台』"
        );
    }

    #[test]
    fn source_events_classify_bottom_text_correction_notes() {
        let events = source_events("豌豆《ゑんどう》「豌豆」は底本では「跣豆」］の大さ");

        assert!(events.iter().any(|event| matches!(
            event.kind,
            SourceEventKind::EditorialNote {
                kind: EditorialNoteKind::BottomTextCorrection,
                ..
            }
        )));
    }

    #[test]
    fn source_events_classify_terminal_provenance_notes_before_colophon() {
        let body = "私のお話は之で終りといたします。［＃地付き］（昭和九年十一月十五日ラジオ放送の遺稿より）\n\n底本：「ある英語教師の思い出」";
        let events = source_events(body);

        assert!(events.iter().any(|event| matches!(
            event.kind,
            SourceEventKind::SegmentBoundary {
                kind: SegmentBoundaryKind::TerminalProvenanceNote
            }
        )));
        assert_eq!(
            comparison_lossy_body(body),
            "私のお話は之で終りといたします。\n\n底本：「ある英語教師の思い出」"
        );
    }

    #[test]
    fn source_markers_return_raw_marker_body_and_span() {
        let text = "吾輩《わがはい》\n※［＃「口＋世」、U+546D］\n［＃ここから横組み］\n〔e'tude〕\n［＃地付き］（fixture）\n底本：fixture";
        let markers = source_markers(text);

        assert_eq!(markers.len(), 5);
        assert_eq!(markers[0].kind, SourceMarkerKind::RubyImplicit);
        assert_eq!(markers[0].raw, "《わがはい》");
        assert_eq!(markers[0].body, "わがはい");
        assert_eq!(markers[0].span.line, 1);

        assert_eq!(markers[1].kind, SourceMarkerKind::GaijiFullwidth);
        assert_eq!(markers[1].raw, "※［＃「口＋世」、U+546D］");
        assert_eq!(markers[1].body, "「口＋世」、U+546D");
        assert_eq!(markers[1].span.line, 2);

        assert_eq!(markers[2].kind, SourceMarkerKind::CommandFullwidth);
        assert_eq!(markers[2].body, "ここから横組み");

        assert_eq!(markers[3].kind, SourceMarkerKind::AccentNotation);
        assert_eq!(markers[3].raw, "〔e'tude〕");
        assert_eq!(markers[3].body, "e'tude");

        assert_eq!(
            markers[4].kind,
            SourceMarkerKind::SegmentBoundaryTerminalProvenance
        );
        assert_eq!(markers[4].raw, "［＃地付き］（fixture）");
    }

    #[test]
    fn source_markers_keep_literal_fullwidth_brackets_inside_commands() {
        let text = "［＃「［Ａ］のようにも」は底本では「［Ａ］ようにも」］";
        let markers = source_markers(text);

        assert_eq!(markers.len(), 1);
        assert_eq!(markers[0].kind, SourceMarkerKind::CommandFullwidth);
        assert_eq!(markers[0].raw, text);
        assert_eq!(
            markers[0].body,
            "「［Ａ］のようにも」は底本では「［Ａ］ようにも」"
        );
    }

    #[test]
    fn command_quotes_can_supply_unmatched_literal_brackets() {
        let text = "［＃「［「風邪そのもの」」は底本では「（「風邪そのもの」」］";
        let markers = source_markers(text);
        assert_eq!(markers.len(), 1);
        assert_eq!(markers[0].kind, SourceMarkerKind::CommandFullwidth);
        assert_eq!(markers[0].raw, text);
        let literal_quote = "「［＃「「」は底本では欠落］本文。」";
        assert_eq!(
            source_markers(literal_quote)[0].raw,
            "［＃「「」は底本では欠落］"
        );
        let nested = "［＃「※［＃濁点付き片仮名ヱ、1-7-84］」に「］」の注記］";
        assert_eq!(source_markers(nested)[0].raw, nested);
    }

    #[test]
    fn multiline_inputter_note_requires_a_contiguous_supplied_close() {
        let note = "［＃入力者註：以下を修正した。\r\n　全集版：「底本」→「修正」\r\n　5-13「識らず墜《お》ち込んで」→「識らず堕《お》ち込んで」］";
        let text = format!("{note}\r\n\r\n底本：文庫\r\n");
        let markers = source_markers(&text);
        assert_eq!(markers[0].kind, SourceMarkerKind::CommandFullwidth);
        assert_eq!(markers[0].raw, note);
        for text in [
            "［＃入力者註：未完了\n\n本文］",
            "［＃入力者註：未完了\n底本：文庫］",
            "［＃入力者註：未完了\n［＃改頁］本文",
            "［＃ここから割り注\n本文］",
            "［＃改丁」",
            "ろっかん［＃「ろっかん」に傍点」山",
        ] {
            assert_eq!(
                source_markers(text)[0].kind,
                SourceMarkerKind::MalformedCommand,
                "{text}"
            );
        }
    }

    #[test]
    fn source_markers_surface_malformed_starts() {
        let text =
            "※［＃未完了\n※[#broken\n［＃ここから割り注\n[#broken\n｜未完了\n《未完了\n〔未完了";
        let markers = source_markers(text);

        assert_eq!(markers.len(), 6);
        assert_eq!(markers[0].kind, SourceMarkerKind::MalformedGaiji);
        assert_eq!(markers[0].raw, "※［＃");
        assert_eq!(markers[1].kind, SourceMarkerKind::MalformedGaijiAscii);
        assert_eq!(markers[1].raw, "※[#");
        assert_eq!(markers[2].kind, SourceMarkerKind::MalformedCommand);
        assert_eq!(markers[2].raw, "［＃");
        assert_eq!(markers[3].kind, SourceMarkerKind::MalformedCommandAscii);
        assert_eq!(markers[3].raw, "[#");
        assert_eq!(markers[4].kind, SourceMarkerKind::MalformedImplicitRuby);
        assert_eq!(markers[4].raw, "《");
        assert_eq!(markers[5].kind, SourceMarkerKind::MalformedAccentNotation);
        assert_eq!(markers[5].raw, "〔");
    }

    #[test]
    fn source_markers_ignore_bare_ruby_base_bars() {
        let markers = source_markers("本文｜そのまま\n｜未完了\n｜吾輩《わがはい》");

        assert_eq!(markers.len(), 1);
        assert_eq!(markers[0].kind, SourceMarkerKind::RubyExplicit);
        assert_eq!(markers[0].raw, "｜吾輩《わがはい》");
    }

    #[test]
    fn source_markers_ignore_ruby_marker_legend_bars() {
        let markers = source_markers(
            "｜：ルビの付く文字列の始まりを特定する記号\n｜；ルビの付く文字列の始まりを特定する記号",
        );

        assert!(
            markers
                .iter()
                .all(|marker| marker.kind != SourceMarkerKind::MalformedRuby),
            "legend delimiter bars are literal boilerplate, not malformed ruby markers: {markers:?}"
        );
    }

    #[test]
    fn source_markers_ignore_empty_accent_brackets() {
        let markers = source_markers("〔〕：アクセント分解された欧文をかこむ\n〔e'tude〕");

        assert_eq!(markers.len(), 1);
        assert_eq!(markers[0].kind, SourceMarkerKind::AccentNotation);
        assert_eq!(markers[0].raw, "〔e'tude〕");
    }

    #[test]
    fn bracket_scope_does_not_end_inside_an_embedded_command() {
        for command in [
            "［＃「〔schla:gt〕」は底本では「〔scha:gt〕」］",
            "[#「〔schla:gt〕」は底本では「〔scha:gt〕」]",
        ] {
            let source = format!("〔sein Puls schla:gt{command} ihm noch.〕後");
            let markers = source_markers(&source);
            assert_eq!(markers.len(), 1);
            assert_eq!(markers[0].raw, source.strip_suffix('後').unwrap());
            assert!(markers[0].body.contains(command));
        }
    }

    #[test]
    fn source_markers_capture_multiline_bracket_notes() {
        let text = "〔空しき秋二十数篇は散佚して今はなし。その第十二のみ、諸井\n三郎の作曲によりて残りしものなり。〕\n［＃地付き］（fixture）\n底本：fixture";
        let markers = source_markers(text);

        assert_eq!(markers.len(), 2);
        assert_eq!(markers[0].kind, SourceMarkerKind::BracketNote);
        assert_eq!(
            markers[0].raw,
            "〔空しき秋二十数篇は散佚して今はなし。その第十二のみ、諸井\n三郎の作曲によりて残りしものなり。〕"
        );
        assert_eq!(markers[0].span.line, 1);
        assert_eq!(
            markers[1].kind,
            SourceMarkerKind::SegmentBoundaryTerminalProvenance
        );
        assert_eq!(markers[1].span.line, 3);
    }

    #[test]
    fn source_markers_share_recognition_with_source_events() {
        let text =
            "｜吾輩《わがはい》\n※［＃「口＋世」、U+546D］\n［ルビの「おもて」は底本では「うら」］";
        let markers = source_markers(text);
        let events = source_events(text);

        assert_eq!(markers.len(), 3);
        assert!(
            events
                .iter()
                .any(|event| matches!(event.kind, SourceEventKind::Ruby { .. }))
        );
        assert!(
            events
                .iter()
                .any(|event| matches!(event.kind, SourceEventKind::Gaiji { .. }))
        );
        assert!(
            events
                .iter()
                .any(|event| matches!(event.kind, SourceEventKind::EditorialNote { .. }))
        );
    }

    #[test]
    fn gaiji_marker_count_counts_markers_inside_commands() {
        let count = gaiji_marker_count(
            "豌豆《ゑんどう》［＃「豌豆」は底本では「※［＃「足＋宛」、第3水準1-92-36］豆」］",
        );

        assert_eq!(count, 1);
    }
}
