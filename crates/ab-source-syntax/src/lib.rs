use std::borrow::Cow;

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

pub fn comparison_lossy_body(txt: &str) -> Cow<'_, str> {
    if !needs_lossy_projection(txt) {
        return Cow::Borrowed(txt);
    }

    let mut out = String::with_capacity(txt.len());
    let mut offset = 0;
    while offset < txt.len() {
        let rest = &txt[offset..];

        if rest.starts_with("※［＃") {
            let content_start = offset + "※［＃".len();
            if let Some(end) = marker_end_on_same_line(txt, content_start, '］') {
                offset = end + '］'.len_utf8();
            } else {
                offset += '※'.len_utf8();
            }
            continue;
        }
        if rest.starts_with("※[#") {
            let content_start = offset + "※[#".len();
            if let Some(end) = marker_end_on_same_line(txt, content_start, ']') {
                offset = end + 1;
            } else {
                offset += '※'.len_utf8();
            }
            continue;
        }
        if rest.starts_with("［＃") {
            let content_start = offset + "［＃".len();
            if let Some(end) = command_end_on_same_line(txt, content_start, '］') {
                offset = end + '］'.len_utf8();
                continue;
            }
        }
        if rest.starts_with("[#") {
            let content_start = offset + "[#".len();
            if let Some(end) = command_end_on_same_line(txt, content_start, ']') {
                offset = end + 1;
                continue;
            }
        }
        if rest.starts_with('｜') {
            let base_start = offset + '｜'.len_utf8();
            if let Some((base_end, reading_end)) = explicit_ruby_bounds(txt, base_start) {
                out.push_str(&comparison_lossy_body(&txt[base_start..base_end]));
                offset = reading_end;
                continue;
            }
        }
        if rest.starts_with('《') {
            let reading_start = offset + '《'.len_utf8();
            if let Some(end) = marker_end_on_same_line(txt, reading_start, '》') {
                offset = end + '》'.len_utf8();
                continue;
            }
        }

        let ch = rest.chars().next().expect("non-empty rest has a char");
        if ch != '※' && ch != '｜' {
            out.push(ch);
        }
        offset += ch.len_utf8();
    }

    Cow::Owned(remove_bottom_note_fragments(&out))
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
        if rest.starts_with("」の「") {
            trim_note_prefix(&mut out);
            offset += "」の「".len();
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
    collect_source_annotations(body, false)
}

pub fn source_annotations_for_validation(body: &str) -> SourceAnnotations<'_> {
    collect_source_annotations(body, true)
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

fn collect_source_annotations(body: &str, skip_gaiji_orphan_ruby: bool) -> SourceAnnotations<'_> {
    let mut annotations = SourceAnnotations::default();
    let mut base_offset = 0;
    for (line_idx, line) in body.split_inclusive('\n').enumerate() {
        collect_line_annotations(
            line,
            base_offset,
            line_idx + 1,
            skip_gaiji_orphan_ruby,
            &mut annotations,
        );
        base_offset += line.len();
    }
    annotations
}

fn collect_line_annotations<'a>(
    line: &'a str,
    base_offset: usize,
    line_number: usize,
    skip_gaiji_orphan_ruby: bool,
    annotations: &mut SourceAnnotations<'a>,
) {
    let mut offset = 0;
    let mut last_gaiji_end = None;
    while offset < line.len() {
        let rest = &line[offset..];
        if rest.starts_with("※［＃") {
            let content_start = offset + "※［＃".len();
            if let Some(content_end) = marker_end_on_same_line(line, content_start, '］') {
                annotations.gaiji_descriptions.push(LocatedMarker {
                    value: &line[content_start..content_end],
                    byte_offset: base_offset + offset,
                    line: line_number,
                });
                offset = content_end + '］'.len_utf8();
                last_gaiji_end = Some(offset);
                continue;
            }
        }
        if rest.starts_with("※[#") {
            let content_start = offset + "※[#".len();
            if let Some(content_end) = marker_end_on_same_line(line, content_start, ']') {
                annotations.gaiji_descriptions.push(LocatedMarker {
                    value: &line[content_start..content_end],
                    byte_offset: base_offset + offset,
                    line: line_number,
                });
                offset = content_end + 1;
                last_gaiji_end = Some(offset);
                continue;
            }
        }
        if rest.starts_with("［＃") {
            let content_start = offset + "［＃".len();
            if let Some(end) = command_end_on_same_line(line, content_start, '］') {
                offset = end + '］'.len_utf8();
                last_gaiji_end = None;
                continue;
            }
        }
        if rest.starts_with("[#") {
            let content_start = offset + "[#".len();
            if let Some(end) = command_end_on_same_line(line, content_start, ']') {
                offset = end + 1;
                last_gaiji_end = None;
                continue;
            }
        }
        if rest.starts_with('《') {
            let content_start = offset + '《'.len_utf8();
            if let Some(content_end) = marker_end_on_same_line(line, content_start, '》') {
                if !(skip_gaiji_orphan_ruby && last_gaiji_end == Some(offset)) {
                    let reading = &line[content_start..content_end];
                    annotations.ruby_readings.push(LocatedMarker {
                        value: reading,
                        byte_offset: base_offset + offset,
                        line: line_number,
                    });
                    collect_gaiji_markers(
                        reading,
                        base_offset + content_start,
                        line_number,
                        annotations,
                    );
                }
                offset = content_end + '》'.len_utf8();
                last_gaiji_end = None;
                continue;
            }
        }

        let ch = rest.chars().next().expect("non-empty rest has a char");
        offset += ch.len_utf8();
        last_gaiji_end = None;
    }
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

fn needs_lossy_projection(txt: &str) -> bool {
    txt.find(['※', '《', '｜', '［', '[', '」']).is_some()
}

fn explicit_ruby_bounds(txt: &str, base_start: usize) -> Option<(usize, usize)> {
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
    Some((base_end, reading_end + '》'.len_utf8()))
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
    fn gaiji_marker_count_counts_markers_inside_commands() {
        let count = gaiji_marker_count(
            "豌豆《ゑんどう》［＃「豌豆」は底本では「※［＃「足＋宛」、第3水準1-92-36］豆」］",
        );

        assert_eq!(count, 1);
    }
}
