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

pub fn comparison_lossy_body(txt: &str) -> Cow<'_, str> {
    if !needs_lossy_projection(txt) {
        return Cow::Borrowed(txt);
    }

    let events = source_events(txt);
    Cow::Owned(comparison_lossy_body_from_events(&events))
}

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

pub fn source_events(txt: &str) -> Vec<SourceEvent<'_>> {
    let mut events = Vec::new();
    let mut offset = 0;
    let mut text_start = 0;
    let mut text_start_line = 1;
    let mut line = 1;
    while offset < txt.len() {
        let rest = &txt[offset..];

        if let Some(note_end) = bottom_text_correction_note_end(txt, offset) {
            push_text_event(txt, &mut events, &mut text_start, text_start_line, offset);
            events.push(SourceEvent {
                span: span_for(offset, note_end, line),
                kind: SourceEventKind::EditorialNote {
                    raw: &txt[offset..note_end],
                    kind: EditorialNoteKind::BottomTextCorrection,
                },
            });
            offset = note_end;
            text_start = offset;
            text_start_line = line;
            continue;
        }
        if let Some((note_end, target, source)) = ruby_correction_note_bounds(txt, offset) {
            push_text_event(txt, &mut events, &mut text_start, text_start_line, offset);
            events.push(SourceEvent {
                span: span_for(offset, note_end, line),
                kind: SourceEventKind::EditorialNote {
                    raw: &txt[offset..note_end],
                    kind: EditorialNoteKind::RubyCorrection {
                        target_reading: target,
                        source_reading: source,
                    },
                },
            });
            offset = note_end;
            text_start = offset;
            text_start_line = line;
            continue;
        }
        if let Some(note_end) = terminal_provenance_note_end(txt, offset) {
            push_text_event(txt, &mut events, &mut text_start, text_start_line, offset);
            events.push(SourceEvent {
                span: span_for(offset, note_end, line),
                kind: SourceEventKind::SegmentBoundary {
                    kind: SegmentBoundaryKind::TerminalProvenanceNote,
                },
            });
            offset = note_end;
            text_start = offset;
            text_start_line = line;
            continue;
        }
        if rest.starts_with("※［＃") {
            let content_start = offset + "※［＃".len();
            if let Some(content_end) = marker_end_on_same_line(txt, content_start, '］') {
                let marker_end = content_end + '］'.len_utf8();
                push_text_event(txt, &mut events, &mut text_start, text_start_line, offset);
                events.push(SourceEvent {
                    span: span_for(offset, marker_end, line),
                    kind: SourceEventKind::Gaiji {
                        description: &txt[content_start..content_end],
                    },
                });
                offset = marker_end;
                text_start = offset;
                text_start_line = line;
                continue;
            }
        }
        if rest.starts_with("※[#") {
            let content_start = offset + "※[#".len();
            if let Some(content_end) = marker_end_on_same_line(txt, content_start, ']') {
                let marker_end = content_end + 1;
                push_text_event(txt, &mut events, &mut text_start, text_start_line, offset);
                events.push(SourceEvent {
                    span: span_for(offset, marker_end, line),
                    kind: SourceEventKind::Gaiji {
                        description: &txt[content_start..content_end],
                    },
                });
                offset = marker_end;
                text_start = offset;
                text_start_line = line;
                continue;
            }
        }
        if rest.starts_with("［＃") {
            let content_start = offset + "［＃".len();
            if let Some(content_end) = command_end_on_same_line(txt, content_start, '］') {
                let marker_end = content_end + '］'.len_utf8();
                push_text_event(txt, &mut events, &mut text_start, text_start_line, offset);
                events.push(SourceEvent {
                    span: span_for(offset, marker_end, line),
                    kind: SourceEventKind::Command {
                        body: &txt[content_start..content_end],
                    },
                });
                offset = marker_end;
                text_start = offset;
                text_start_line = line;
                continue;
            }
        }
        if rest.starts_with("[#") {
            let content_start = offset + "[#".len();
            if let Some(content_end) = command_end_on_same_line(txt, content_start, ']') {
                let marker_end = content_end + 1;
                push_text_event(txt, &mut events, &mut text_start, text_start_line, offset);
                events.push(SourceEvent {
                    span: span_for(offset, marker_end, line),
                    kind: SourceEventKind::Command {
                        body: &txt[content_start..content_end],
                    },
                });
                offset = marker_end;
                text_start = offset;
                text_start_line = line;
                continue;
            }
        }
        if rest.starts_with('｜') {
            let base_start = offset + '｜'.len_utf8();
            if let Some((base_end, reading_start, reading_end, marker_end)) =
                explicit_ruby_bounds(txt, base_start)
            {
                push_text_event(txt, &mut events, &mut text_start, text_start_line, offset);
                events.push(SourceEvent {
                    span: span_for(offset, marker_end, line),
                    kind: SourceEventKind::Ruby {
                        base_source: Some(&txt[base_start..base_end]),
                        reading: &txt[reading_start..reading_end],
                    },
                });
                offset = marker_end;
                text_start = offset;
                text_start_line = line;
                continue;
            }
        }
        if rest.starts_with('《') {
            let reading_start = offset + '《'.len_utf8();
            if let Some(reading_end) = marker_end_on_same_line(txt, reading_start, '》') {
                let marker_end = reading_end + '》'.len_utf8();
                push_text_event(txt, &mut events, &mut text_start, text_start_line, offset);
                events.push(SourceEvent {
                    span: span_for(offset, marker_end, line),
                    kind: SourceEventKind::Ruby {
                        base_source: None,
                        reading: &txt[reading_start..reading_end],
                    },
                });
                offset = marker_end;
                text_start = offset;
                text_start_line = line;
                continue;
            }
        }

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

pub fn source_annotations(body: &str) -> SourceAnnotations<'_> {
    let events = source_events(body);
    source_annotations_from_events(&events, false)
}

pub fn source_annotations_for_validation(body: &str) -> SourceAnnotations<'_> {
    let events = source_events(body);
    source_annotations_from_events(&events, true)
}

pub fn source_annotations_both(body: &str) -> SourceAnnotationsBoth<'_> {
    let events = source_events(body);
    source_annotations_both_from_events(&events)
}

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

fn bottom_text_correction_note_end(txt: &str, offset: usize) -> Option<usize> {
    ["」は底本では「", "」はママ"]
        .iter()
        .any(|prefix| txt[offset..].starts_with(prefix))
        .then(|| skip_until_any_bracket(txt, offset))
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

fn command_end_on_same_line(text: &str, content_start: usize, end_marker: char) -> Option<usize> {
    let mut offset = content_start;
    while offset < text.len() {
        let rest = &text[offset..];
        if rest.starts_with("※［＃") {
            let nested_start = offset + "※［＃".len();
            if let Some(end) = marker_end_on_same_line(text, nested_start, '］') {
                offset = end + '］'.len_utf8();
                continue;
            }
        }
        if rest.starts_with("※[#") {
            let nested_start = offset + "※[#".len();
            if let Some(end) = marker_end_on_same_line(text, nested_start, ']') {
                offset = end + 1;
                continue;
            }
        }
        if rest.starts_with("［＃") {
            let nested_start = offset + "［＃".len();
            if let Some(end) = command_end_on_same_line(text, nested_start, '］') {
                offset = end + '］'.len_utf8();
                continue;
            }
        }
        if rest.starts_with("[#") {
            let nested_start = offset + "[#".len();
            if let Some(end) = command_end_on_same_line(text, nested_start, ']') {
                offset = end + 1;
                continue;
            }
        }

        let ch = rest.chars().next().expect("non-empty rest has a char");
        if ch == end_marker {
            return Some(offset);
        }
        if matches!(ch, '\r' | '\n') {
            return None;
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
    fn gaiji_marker_count_counts_markers_inside_commands() {
        let count = gaiji_marker_count(
            "豌豆《ゑんどう》［＃「豌豆」は底本では「※［＃「足＋宛」、第3水準1-92-36］豆」］",
        );

        assert_eq!(count, 1);
    }
}
