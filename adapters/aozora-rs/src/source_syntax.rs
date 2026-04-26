use std::borrow::Cow;

pub fn project_lossy_visible_text(txt: &str) -> Cow<'_, str> {
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
                out.push_str(&project_lossy_visible_text(&txt[base_start..base_end]));
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

fn needs_lossy_projection(txt: &str) -> bool {
    txt.find(['※', '《', '｜', '［', '[', '」']).is_some()
}

fn explicit_ruby_bounds(txt: &str, base_start: usize) -> Option<(usize, usize)> {
    let base_end = txt[base_start..]
        .char_indices()
        .find_map(|(offset, ch)| match ch {
            '《' => Some(base_start + offset),
            '\r' | '\n' | '》' => None,
            _ => None,
        })
        .or_else(|| {
            txt[base_start..]
                .find('《')
                .map(|offset| base_start + offset)
        })?;

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
    fn projects_lossy_visible_text_without_markup() {
        let projected =
            project_lossy_visible_text("吾輩《わがはい》は※［＃「口＋世」、U+546D］［＃注記］猫");

        assert_eq!(projected, "吾輩は猫");
    }

    #[test]
    fn projects_lossy_visible_text_removes_jis_gaiji_markers() {
        let projected = project_lossy_visible_text(
            "この卷見※［＃「二点しんにょう＋官」、第3水準1-92-56］すべき",
        );

        assert_eq!(projected, "この卷見すべき");
    }

    #[test]
    fn projects_lossy_visible_text_removes_repeated_jis_gaiji_markers() {
        let source = "前※［＃「二点しんにょう＋官」、第3水準1-92-56］後\n\
            前※［＃「廴＋囘」、第4水準2-12-11］後\n\
            前※［＃二の字点、1-2-22］後";

        let projected = project_lossy_visible_text(source);

        assert_eq!(projected, "前後\n前後\n前後");
    }

    #[test]
    fn projects_lossy_visible_text_projects_markup_inside_explicit_ruby_base() {
        let projected = project_lossy_visible_text(
            "｜前※［＃「二点しんにょう＋官」、第3水準1-92-56］後《まえあと》",
        );

        assert_eq!(projected, "前後");
    }

    #[test]
    fn projects_lossy_visible_text_removes_command_with_nested_gaiji_marker() {
        let projected = project_lossy_visible_text(
            "豌豆《ゑんどう》［＃「豌豆」は底本では「※［＃「足＋宛」、第3水準1-92-36］豆」］の大さ",
        );

        assert_eq!(projected, "豌豆の大さ");
    }

    #[test]
    fn projects_lossy_visible_text_removes_unmatched_ruby_delimiters() {
        let projected = project_lossy_visible_text(
            "今日｜民族観念［＃「民族観念」に傍点］と呼ぶ。悲憤｜慷慨《こうがい》も知悉《ちしつ》した",
        );

        assert_eq!(projected, "今日民族観念と呼ぶ。悲憤慷慨も知悉した");
    }
}
