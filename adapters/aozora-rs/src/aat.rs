use std::{borrow::Cow, collections::HashSet, time::Instant};

use ab_ir::{Block, Inline, ProjectedText, Provenance, RubyPlacement};
use aozora_rs_core::{Break, Deco, Retokenized};

use crate::{
    metrics::FallbackDecision,
    parser::ParsedSource,
    source::{remove_bottom_note_fragments, source_visible_text},
};

#[derive(Debug, Clone)]
pub struct InitialAatBuildResult {
    pub blocks: Vec<Block>,
    pub projected: ProjectedText,
    pub timings: AatBuildTimings,
}

#[derive(Debug, Clone)]
pub struct AatBuildResult {
    pub blocks: Vec<Block>,
    pub fallback: FallbackDecision,
    pub timings: AatBuildTimings,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AatBuildTimings {
    pub build: std::time::Duration,
}

pub fn build_initial(parsed: &ParsedSource<'_>) -> InitialAatBuildResult {
    let start = Instant::now();
    let body = parsed.body.validation_body;
    let mut blocks = retokenized_to_aat_blocks(&parsed.retokenized);
    append_source_annotation_supplements(&mut blocks, body);
    strip_cross_node_commands(&mut blocks);
    let projected = ab_ir::visible_projection(&blocks);
    InitialAatBuildResult {
        blocks,
        projected,
        timings: AatBuildTimings {
            build: start.elapsed(),
        },
    }
}

pub fn build_fallback(body: &str) -> (Vec<Block>, ProjectedText) {
    let source_visible = source_visible_text(body).into_owned();
    build_fallback_from_source_visible(body, source_visible)
}

pub fn build_fallback_from_source_visible(
    body: &str,
    source_visible: String,
) -> (Vec<Block>, ProjectedText) {
    let blocks = source_visible_fallback_blocks(body, source_visible);
    let projected = ab_ir::visible_projection(&blocks);
    (blocks, projected)
}

pub fn blocks_cover_validation_annotations(body: &str, blocks: &[Block]) -> bool {
    let annotations = ab_source_syntax::source_annotations_for_validation(body);
    let readings = ruby_readings_in_blocks(blocks);
    annotations
        .ruby_readings
        .iter()
        .all(|marker| readings.contains(marker.value))
        && gaiji_count_in_blocks(blocks) >= annotations.gaiji_descriptions.len()
}

fn retokenized_to_aat_blocks(tokens: &[Retokenized<'_>]) -> Vec<Block> {
    let mut blocks = Vec::new();
    let mut content = Vec::new();
    let mut idx = 0;
    while idx < tokens.len() {
        match &tokens[idx] {
            Retokenized::Text(text) => push_text(&mut content, &source_visible_text(text)),
            Retokenized::Odoriji(odoriji) => push_text(&mut content, odoriji_source_text(*odoriji)),
            Retokenized::Kunten(_) | Retokenized::Okurigana(_) => {}
            Retokenized::Break(Break::BreakLine) => flush_paragraph(&mut blocks, &mut content),
            Retokenized::Break(_) => flush_paragraph(&mut blocks, &mut content),
            Retokenized::Figure(figure) => content.push(Inline::gaiji(
                figure.to_string(),
                "",
                Some(
                    "aozora-rs-core Figure Display output; original gaiji notation is not preserved by Figure",
                ),
            )),
            Retokenized::DecoBegin(Deco::Ruby(reading)) => {
                let (base, next_idx) = collect_decorated_visible_text(tokens, idx + 1, |deco| {
                    matches!(deco, Deco::Ruby(_))
                });
                if !is_pathological_ruby_base(&base) {
                    content.push(Inline::ruby(base, *reading));
                }
                idx = next_idx;
                continue;
            }
            Retokenized::DecoBegin(Deco::AHead | Deco::BHead | Deco::CHead) => {
                flush_paragraph(&mut blocks, &mut content);
                let deco = match &tokens[idx] {
                    Retokenized::DecoBegin(deco) => deco,
                    _ => unreachable!(),
                };
                let (value, next_idx) =
                    collect_decorated_visible_text(tokens, idx + 1, |candidate| {
                        matches!(
                            (deco, candidate),
                            (Deco::AHead, Deco::AHead)
                                | (Deco::BHead, Deco::BHead)
                                | (Deco::CHead, Deco::CHead)
                        )
                    });
                let level = match deco {
                    Deco::AHead => 1,
                    Deco::BHead => 2,
                    Deco::CHead => 3,
                    _ => unreachable!(),
                };
                blocks.push(Block::Heading {
                    level,
                    style: stable_style_type(deco),
                    content: vec![Inline::text(value)],
                });
                idx = next_idx;
                continue;
            }
            Retokenized::DecoBegin(deco) => {
                let (value, next_idx) =
                    collect_decorated_visible_text(tokens, idx + 1, |candidate| {
                        same_deco_kind(candidate, deco)
                    });
                content.push(Inline::style(stable_style_type(deco), vec![Inline::text(value)]));
                idx = next_idx;
                continue;
            }
            Retokenized::DecoEnd(_) => {}
        }
        idx += 1;
    }
    flush_paragraph(&mut blocks, &mut content);
    if blocks.is_empty() {
        blocks.push(Block::Paragraph { content: vec![] });
    }
    blocks
}

fn collect_decorated_visible_text(
    tokens: &[Retokenized<'_>],
    mut idx: usize,
    is_matching_end: impl Fn(&Deco<'_>) -> bool,
) -> (String, usize) {
    let mut value = String::new();
    let mut depth = 1;
    while idx < tokens.len() {
        match &tokens[idx] {
            Retokenized::Text(text) => value.push_str(&source_visible_text(text)),
            Retokenized::Odoriji(odoriji) => value.push_str(odoriji_source_text(*odoriji)),
            Retokenized::Kunten(_) | Retokenized::Okurigana(_) => {}
            Retokenized::Break(_) => value.push('\n'),
            Retokenized::Figure(_) => {}
            Retokenized::DecoBegin(_) => depth += 1,
            Retokenized::DecoEnd(deco) if depth == 1 && is_matching_end(deco) => {
                return (value, idx + 1);
            }
            Retokenized::DecoEnd(_) => depth -= 1,
        }
        idx += 1;
    }
    (value, idx)
}

fn is_pathological_ruby_base(base: &str) -> bool {
    base.contains('\n') || base.chars().count() > 80
}

fn flush_paragraph(blocks: &mut Vec<Block>, content: &mut Vec<Inline>) {
    if content.is_empty() {
        return;
    }
    blocks.push(Block::Paragraph {
        content: std::mem::take(content),
    });
}

fn same_deco_kind(a: &Deco<'_>, b: &Deco<'_>) -> bool {
    matches!(
        (a, b),
        (Deco::Bold, Deco::Bold)
            | (Deco::Italic, Deco::Italic)
            | (Deco::Bosen(_), Deco::Bosen(_))
            | (Deco::Boten(_), Deco::Boten(_))
            | (Deco::Indent(_), Deco::Indent(_))
            | (Deco::Hanging(_), Deco::Hanging(_))
            | (Deco::Grounded, Deco::Grounded)
            | (Deco::LowFlying(_), Deco::LowFlying(_))
            | (Deco::HinV, Deco::HinV)
            | (Deco::Mama, Deco::Mama)
            | (Deco::Smaller(_), Deco::Smaller(_))
            | (Deco::Bigger(_), Deco::Bigger(_))
            | (Deco::VHCentre, Deco::VHCentre)
            | (Deco::Warichu, Deco::Warichu)
            | (Deco::HorizontalLayout, Deco::HorizontalLayout)
            | (Deco::Kerning(_), Deco::Kerning(_))
            | (Deco::Sub, Deco::Sub)
            | (Deco::Sup, Deco::Sup)
    )
}

fn stable_style_type(deco: &Deco<'_>) -> &'static str {
    match deco {
        Deco::Bold => "bold",
        Deco::Italic => "italic",
        Deco::Bosen(_) => "bosen",
        Deco::Boten(_) => "boten",
        Deco::Indent(_) => "indent",
        Deco::Hanging(_) => "hanging",
        Deco::Grounded => "grounded",
        Deco::LowFlying(_) => "low_flying",
        Deco::HinV => "tcy",
        Deco::Mama => "mama",
        Deco::Smaller(_) => "smaller",
        Deco::Bigger(_) => "bigger",
        Deco::VHCentre => "vh_centre",
        Deco::Warichu => "warichu",
        Deco::HorizontalLayout => "horizontal_layout",
        Deco::Kerning(_) => "kerning",
        Deco::Sub => "sub",
        Deco::Sup => "sup",
        Deco::Ruby(_) | Deco::AHead | Deco::BHead | Deco::CHead => "handled_elsewhere",
    }
}

fn odoriji_source_text(odoriji: aozora_rs_core::Odoriji) -> &'static str {
    if odoriji.has_dakuten {
        "／″＼"
    } else {
        "／＼"
    }
}

fn push_text(content: &mut Vec<Inline>, value: &str) {
    if value.is_empty() {
        return;
    }
    content.push(Inline::text_with_provenance(
        value,
        Provenance::ParserNormalized,
    ));
}

fn strip_cross_node_commands(blocks: &mut [Block]) {
    let mut state = CommandStripState::None;
    for block in blocks {
        for child in ab_ir::block_content_mut(block) {
            strip_commands_in_inline(child, &mut state);
        }
    }
}

fn strip_commands_in_inline(value: &mut Inline, state: &mut CommandStripState) {
    match value {
        Inline::Text { value, .. } => strip_string(value, state),
        Inline::Ruby { base, .. } => {
            for child in base {
                strip_commands_in_inline(child, state);
            }
        }
        Inline::Style { content, .. } => {
            for child in content {
                strip_commands_in_inline(child, state);
            }
        }
        Inline::GaijiRef(_) => {}
    }
}

fn strip_string(text: &mut String, state: &mut CommandStripState) {
    if let Cow::Owned(stripped) = stripped_command_fragments(text, state) {
        *text = stripped;
    }
}

fn stripped_command_fragments<'a>(text: &'a str, state: &mut CommandStripState) -> Cow<'a, str> {
    if matches!(state, CommandStripState::None) && !needs_command_strip(text) {
        return Cow::Borrowed(text);
    }

    Cow::Owned(remove_bottom_note_fragments(&strip_command_fragments(
        text, state,
    )))
}

fn needs_command_strip(text: &str) -> bool {
    text.find(['［', '[', '」']).is_some()
}

#[derive(Clone, Copy)]
enum CommandStripState {
    None,
    FullWidth,
    Ascii,
    AnyBracket,
}

fn strip_command_fragments(text: &str, state: &mut CommandStripState) -> String {
    let mut output = String::new();
    let mut rest = text;
    while !rest.is_empty() {
        match state {
            CommandStripState::None => {
                let fullwidth = rest.find("［＃");
                let ascii = rest.find("[#");
                let bottom_note = rest.find("」は底本では「");
                let mama_note = rest.find("」はママ");
                let next = [
                    fullwidth.map(|offset| (offset, NoteStart::FullWidthCommand)),
                    ascii.map(|offset| (offset, NoteStart::AsciiCommand)),
                    bottom_note.map(|offset| (offset, NoteStart::BottomNote)),
                    mama_note.map(|offset| (offset, NoteStart::MamaNote)),
                ]
                .into_iter()
                .flatten()
                .min_by_key(|(offset, _)| *offset);
                let Some((start, note_start)) = next else {
                    output.push_str(rest);
                    break;
                };
                if !matches!(note_start, NoteStart::BottomNote) {
                    output.push_str(&rest[..start]);
                }
                match note_start {
                    NoteStart::FullWidthCommand => {
                        rest = &rest[start + "［＃".len()..];
                        *state = CommandStripState::FullWidth;
                    }
                    NoteStart::AsciiCommand => {
                        rest = &rest[start + "[#".len()..];
                        *state = CommandStripState::Ascii;
                    }
                    NoteStart::BottomNote => {
                        rest = &rest[start + "」は底本では「".len()..];
                        *state = CommandStripState::AnyBracket;
                    }
                    NoteStart::MamaNote => {
                        rest = &rest[start + "」はママ".len()..];
                        *state = CommandStripState::AnyBracket;
                    }
                };
            }
            CommandStripState::FullWidth => {
                if let Some(end) = rest.find('］') {
                    rest = &rest[end + '］'.len_utf8()..];
                    *state = CommandStripState::None;
                } else {
                    break;
                }
            }
            CommandStripState::Ascii => {
                if let Some(end) = rest.find(']') {
                    rest = &rest[end + 1..];
                    *state = CommandStripState::None;
                } else {
                    break;
                }
            }
            CommandStripState::AnyBracket => {
                let fullwidth = rest.find('］');
                let ascii = rest.find(']');
                let end = match (fullwidth, ascii) {
                    (Some(left), Some(right)) => Some((left.min(right), left <= right)),
                    (Some(left), None) => Some((left, true)),
                    (None, Some(right)) => Some((right, false)),
                    (None, None) => None,
                };
                if let Some((end, is_fullwidth)) = end {
                    rest = if is_fullwidth {
                        &rest[end + '］'.len_utf8()..]
                    } else {
                        &rest[end + 1..]
                    };
                    *state = CommandStripState::None;
                } else {
                    break;
                }
            }
        }
    }
    output
}

#[derive(Clone, Copy)]
enum NoteStart {
    FullWidthCommand,
    AsciiCommand,
    BottomNote,
    MamaNote,
}

fn append_source_annotation_supplements(blocks: &mut [Block], body: &str) {
    let existing_ruby_readings = ruby_readings_in_blocks(blocks);
    let existing_gaiji_count = gaiji_count_in_blocks(blocks);
    let markers = ab_source_syntax::source_annotations(body);
    let Some(first_block) = blocks.first_mut() else {
        return;
    };
    let content = ab_ir::block_content_mut(first_block);
    append_ruby_supplements(
        content,
        markers.ruby_readings.iter().map(|marker| marker.value),
        &existing_ruby_readings,
    );
    append_gaiji_supplements(
        content,
        markers.gaiji_descriptions.iter().map(|marker| marker.value),
        existing_gaiji_count,
    );
}

fn source_visible_fallback_blocks(body: &str, source_visible: String) -> Vec<Block> {
    let blocks = structured_source_fallback_blocks(body);
    if ab_ir::visible_projection(&blocks).visible_text == source_visible
        && blocks_cover_validation_annotations(body, &blocks)
    {
        return blocks;
    }
    legacy_source_visible_fallback_blocks(body, source_visible)
}

fn legacy_source_visible_fallback_blocks(body: &str, source_visible: String) -> Vec<Block> {
    let mut blocks = vec![Block::Paragraph {
        content: vec![Inline::text_with_provenance(
            source_visible,
            Provenance::SourceFallback,
        )],
    }];
    append_source_annotation_supplements(&mut blocks, body);
    blocks
}

fn structured_source_fallback_blocks(body: &str) -> Vec<Block> {
    let mut content = Vec::new();
    let mut offset = 0;
    let mut last_gaiji_end = None;
    while offset < body.len() {
        let rest = &body[offset..];
        if let Some(prefix_len) = fallback_bottom_note_fragment_prefix_len(rest) {
            trim_source_fallback_note_prefix(&mut content);
            offset += prefix_len;
            offset = fallback_skip_until_any_bracket(body, offset);
            last_gaiji_end = None;
            continue;
        }
        if rest.starts_with("※［＃") {
            let content_start = offset + "※［＃".len();
            if let Some(content_end) = fallback_marker_end_on_same_line(body, content_start, '］')
            {
                content.push(Inline::gaiji_with_provenance(
                    &body[content_start..content_end],
                    "",
                    None,
                    Provenance::SourceFallback,
                ));
                offset = content_end + '］'.len_utf8();
                last_gaiji_end = Some(offset);
                continue;
            }
        }
        if rest.starts_with("※[#") {
            let content_start = offset + "※[#".len();
            if let Some(content_end) = fallback_marker_end_on_same_line(body, content_start, ']') {
                content.push(Inline::gaiji_with_provenance(
                    &body[content_start..content_end],
                    "",
                    None,
                    Provenance::SourceFallback,
                ));
                offset = content_end + 1;
                last_gaiji_end = Some(offset);
                continue;
            }
        }
        if rest.starts_with("［＃") {
            let content_start = offset + "［＃".len();
            if let Some(end) = fallback_command_end_on_same_line(body, content_start, '］') {
                offset = end + '］'.len_utf8();
                last_gaiji_end = None;
                continue;
            }
        }
        if rest.starts_with("[#") {
            let content_start = offset + "[#".len();
            if let Some(end) = fallback_command_end_on_same_line(body, content_start, ']') {
                offset = end + 1;
                last_gaiji_end = None;
                continue;
            }
        }
        if rest.starts_with('｜') {
            let base_start = offset + '｜'.len_utf8();
            if let Some((base_end, reading_start, reading_end, marker_end)) =
                explicit_source_ruby_bounds(body, base_start)
            {
                let base_source = &body[base_start..base_end];
                let base = source_visible_text(base_source).into_owned();
                push_source_fallback_ruby(
                    &mut content,
                    base,
                    base_source,
                    &body[reading_start..reading_end],
                );
                offset = marker_end;
                last_gaiji_end = None;
                continue;
            }
        }
        if rest.starts_with('《') {
            let reading_start = offset + '《'.len_utf8();
            if let Some(reading_end) = fallback_marker_end_on_same_line(body, reading_start, '》')
            {
                if last_gaiji_end != Some(offset) {
                    let base = take_implicit_ruby_base(&mut content);
                    push_source_fallback_ruby(
                        &mut content,
                        base,
                        "",
                        &body[reading_start..reading_end],
                    );
                }
                offset = reading_end + '》'.len_utf8();
                last_gaiji_end = None;
                continue;
            }
        }

        let ch = rest.chars().next().expect("non-empty rest has a char");
        if ch != '※' && ch != '｜' {
            push_source_fallback_text(&mut content, ch.encode_utf8(&mut [0; 4]));
        }
        offset += ch.len_utf8();
        last_gaiji_end = None;
    }

    vec![Block::Paragraph { content }]
}

fn fallback_bottom_note_fragment_prefix_len(rest: &str) -> Option<usize> {
    ["」は底本では「", "」の「", "」はママ"]
        .iter()
        .find_map(|prefix| rest.starts_with(prefix).then_some(prefix.len()))
}

fn trim_source_fallback_note_prefix(content: &mut Vec<Inline>) {
    let visible = inline_visible_text(content);
    let Some(target) = note_prefix_trim_len(&visible) else {
        return;
    };
    truncate_inline_content_to_visible_len(content, target);
}

fn note_prefix_trim_len(visible: &str) -> Option<usize> {
    let close_quote = visible.rfind('「')?;
    let prefix = &visible[..close_quote];
    prefix
        .char_indices()
        .rev()
        .find_map(|(offset, ch)| is_note_boundary(ch).then_some(offset + ch.len_utf8()))
        .or(Some(0))
}

fn is_note_boundary(ch: char) -> bool {
    ch.is_whitespace() || matches!(ch, '、' | '。' | '，' | '．')
}

fn truncate_inline_content_to_visible_len(content: &mut Vec<Inline>, target: usize) {
    let mut visible_len = 0;
    let mut idx = 0;
    while idx < content.len() {
        let node_visible_len = inline_visible_len(&content[idx]);
        if visible_len + node_visible_len < target {
            visible_len += node_visible_len;
            idx += 1;
            continue;
        }
        if visible_len + node_visible_len == target {
            content.truncate(idx + 1);
            return;
        }
        match &mut content[idx] {
            Inline::Text { value, .. } => {
                let keep = target - visible_len;
                value.truncate(keep);
                if value.is_empty() {
                    content.truncate(idx);
                } else {
                    content.truncate(idx + 1);
                }
            }
            Inline::Ruby { base, .. } | Inline::Style { content: base, .. } => {
                truncate_inline_content_to_visible_len(base, target - visible_len);
                content.truncate(idx + 1);
            }
            Inline::GaijiRef(_) => {
                content.truncate(idx);
            }
        }
        return;
    }
}

fn inline_visible_text(content: &[Inline]) -> String {
    let mut visible = String::new();
    for node in content {
        push_inline_visible_text(node, &mut visible);
    }
    visible
}

fn push_inline_visible_text(node: &Inline, visible: &mut String) {
    match node {
        Inline::Text { value, .. } => visible.push_str(value),
        Inline::Ruby { base, .. } | Inline::Style { content: base, .. } => {
            for child in base {
                push_inline_visible_text(child, visible);
            }
        }
        Inline::GaijiRef(gaiji) => {
            if let Some(resolved) = &gaiji.resolved {
                visible.push_str(resolved);
            }
        }
    }
}

fn inline_visible_len(node: &Inline) -> usize {
    match node {
        Inline::Text { value, .. } => value.len(),
        Inline::Ruby { base, .. } | Inline::Style { content: base, .. } => {
            base.iter().map(inline_visible_len).sum()
        }
        Inline::GaijiRef(gaiji) => gaiji.resolved.as_ref().map_or(0, String::len),
    }
}

fn fallback_skip_until_any_bracket(text: &str, offset: usize) -> usize {
    let rest = &text[offset..];
    let fullwidth = rest.find('］');
    let ascii = rest.find(']');
    match (fullwidth, ascii) {
        (Some(left), Some(right)) if left <= right => offset + left + '］'.len_utf8(),
        (Some(_), Some(right)) => offset + right + 1,
        (Some(left), None) => offset + left + '］'.len_utf8(),
        (None, Some(right)) => offset + right + 1,
        (None, None) => text.len(),
    }
}

fn push_source_fallback_ruby(
    content: &mut Vec<Inline>,
    base: String,
    base_source: &str,
    reading: &str,
) {
    let base = if base.is_empty() {
        Vec::new()
    } else {
        vec![Inline::text_with_provenance(
            base,
            Provenance::SourceFallback,
        )]
    };
    content.push(Inline::ruby_with_base_and_provenance(
        base,
        reading,
        RubyPlacement::Right,
        Provenance::SourceFallback,
    ));
    for marker in ab_source_syntax::source_annotations(base_source)
        .gaiji_descriptions
        .into_iter()
        .chain(ab_source_syntax::source_annotations(reading).gaiji_descriptions)
    {
        content.push(Inline::gaiji_with_provenance(
            marker.value,
            "",
            None,
            Provenance::SourceFallback,
        ));
    }
}

fn push_source_fallback_text(content: &mut Vec<Inline>, text: &str) {
    if let Some(Inline::Text { value, provenance }) = content.last_mut()
        && *provenance == Provenance::SourceFallback
    {
        value.push_str(text);
        return;
    }
    content.push(Inline::text_with_provenance(
        text,
        Provenance::SourceFallback,
    ));
}

fn take_implicit_ruby_base(content: &mut Vec<Inline>) -> String {
    let Some(Inline::Text { value, .. }) = content.last_mut() else {
        return String::new();
    };
    let start = value
        .char_indices()
        .rev()
        .find_map(|(offset, ch)| implicit_ruby_boundary(ch).then_some(offset + ch.len_utf8()))
        .unwrap_or(0);
    let base = value.split_off(start);
    if value.is_empty() {
        content.pop();
    }
    base
}

fn implicit_ruby_boundary(ch: char) -> bool {
    ch.is_whitespace()
        || matches!(
            ch,
            '｜' | '《'
                | '》'
                | '※'
                | '［'
                | '＃'
                | '['
                | '#'
                | ']'
                | '］'
                | '、'
                | '。'
                | '，'
                | '．'
                | '「'
                | '」'
                | '『'
                | '』'
                | '（'
                | '）'
                | '('
                | ')'
        )
}

fn explicit_source_ruby_bounds(
    txt: &str,
    base_start: usize,
) -> Option<(usize, usize, usize, usize)> {
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
    let reading_end = fallback_marker_end_on_same_line(txt, reading_start, '》')?;
    Some((
        base_end,
        reading_start,
        reading_end,
        reading_end + '》'.len_utf8(),
    ))
}

fn fallback_marker_end_on_same_line(
    text: &str,
    content_start: usize,
    end_marker: char,
) -> Option<usize> {
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

fn fallback_command_end_on_same_line(
    text: &str,
    content_start: usize,
    end_marker: char,
) -> Option<usize> {
    let mut offset = content_start;
    while offset < text.len() {
        let rest = &text[offset..];
        if rest.starts_with("※［＃") {
            let nested_start = offset + "※［＃".len();
            if let Some(end) = fallback_marker_end_on_same_line(text, nested_start, '］') {
                offset = end + '］'.len_utf8();
                continue;
            }
        }
        if rest.starts_with("※[#") {
            let nested_start = offset + "※[#".len();
            if let Some(end) = fallback_marker_end_on_same_line(text, nested_start, ']') {
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

fn append_ruby_supplements(
    content: &mut Vec<Inline>,
    readings: impl IntoIterator<Item = impl AsRef<str>>,
    existing: &HashSet<String>,
) {
    for reading in readings {
        let reading = reading.as_ref();
        if existing.contains(reading) {
            continue;
        }
        content.push(Inline::ruby_with_provenance(
            "",
            reading,
            Provenance::SourceSupplement,
        ));
    }
}

fn append_gaiji_supplements(
    content: &mut Vec<Inline>,
    descriptions: impl IntoIterator<Item = impl AsRef<str>>,
    existing_count: usize,
) {
    for description in descriptions.into_iter().skip(existing_count) {
        let description = description.as_ref();
        content.push(Inline::gaiji_with_provenance(
            description,
            "",
            None,
            Provenance::SourceSupplement,
        ));
    }
}

fn ruby_readings_in_blocks(blocks: &[Block]) -> HashSet<String> {
    let mut readings = HashSet::new();
    for block in blocks {
        for child in ab_ir::block_content(block) {
            collect_ruby_readings(child, &mut readings);
        }
    }
    readings
}

fn collect_ruby_readings(node: &Inline, readings: &mut HashSet<String>) {
    match node {
        Inline::Ruby { reading, .. } => {
            readings.insert(reading.clone());
        }
        Inline::Style { content, .. } => {
            for child in content {
                collect_ruby_readings(child, readings);
            }
        }
        Inline::GaijiRef(_) => {}
        Inline::Text { .. } => {}
    }
}

fn gaiji_count_in_blocks(blocks: &[Block]) -> usize {
    let mut count = 0;
    for block in blocks {
        for child in ab_ir::block_content(block) {
            count += gaiji_count_in_inline(child);
        }
    }
    count
}

fn gaiji_count_in_inline(node: &Inline) -> usize {
    match node {
        Inline::GaijiRef(_) => 1,
        Inline::Style { content, .. } => content.iter().map(gaiji_count_in_inline).sum(),
        Inline::Ruby { base, .. } => base.iter().map(gaiji_count_in_inline).sum(),
        Inline::Text { .. } => 0,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        parser::parse_with_aozora_rs,
        source::{BodySelection, source_visible_text},
    };
    use std::time::Duration;

    #[test]
    fn builds_typed_blocks_and_projected_text() {
        let body = "吾輩《わがはい》は猫である。\n";
        let parsed = parse_with_aozora_rs(BodySelection {
            validation_body: body,
            found_separators: false,
            elapsed: Duration::ZERO,
        })
        .unwrap();

        let built = build_initial(&parsed);
        assert!(matches!(built.blocks[0], Block::Paragraph { .. }));
        assert!(built.projected.visible_text.contains("吾輩"));
        assert!(built.timings.build >= Duration::ZERO);
    }

    #[test]
    fn typed_blocks_serialize_to_schema_shape() {
        let blocks = vec![Block::Paragraph {
            content: vec![Inline::ruby("吾輩", "わがはい")],
        }];

        let json = ab_ir::blocks_to_aat_json(&blocks);
        assert_eq!(json[0]["kind"], "paragraph");
        assert_eq!(json[0]["content"][0]["kind"], "ruby");
        assert_eq!(json[0]["content"][0]["base"], "吾輩");
    }

    #[test]
    fn fallback_blocks_use_source_visible_text() {
        let body = "吾輩《わがはい》は※［＃「口＋世」、U+546D］である。";
        let (blocks, projected) = build_fallback(body);
        assert!(matches!(blocks[0], Block::Paragraph { .. }));
        let visible = source_visible_text(body);
        assert!(projected.visible_text.contains("吾輩"));
        assert!(!visible.contains("「口＋世」、U+546D"));
        assert!(!projected.visible_text.contains("わがはい"));
        assert!(ab_ir::provenance_counts(&blocks).source_fallback > 0);
    }

    #[test]
    fn fallback_blocks_preserve_source_markers_in_place_without_supplements() {
        let body = "吾輩《わがはい》は※［＃「口＋世」、U+546D］である。";
        let (blocks, projected) = build_fallback(body);
        let counts = ab_ir::provenance_counts(&blocks);
        let json = ab_ir::blocks_to_aat_json(&blocks);

        assert_eq!(projected.visible_text, "吾輩はである。");
        assert_eq!(counts.source_supplement, 0);
        assert_eq!(json[0]["content"][0]["kind"], "ruby");
        assert_eq!(json[0]["content"][0]["base"], "吾輩");
        assert_eq!(json[0]["content"][0]["reading"], "わがはい");
        assert_eq!(json[0]["content"][2]["kind"], "gaiji");
        assert_eq!(json[0]["content"][2]["description"], "「口＋世」、U+546D");
        assert_eq!(json[0]["content"][2]["x-provenance"], "source_fallback");
    }

    #[test]
    fn fallback_blocks_do_not_emit_orphan_ruby_after_unresolved_gaiji() {
        let body = "ことを、※［＃「口＋愛」、第3水準1-15-23］《おくび》にも";
        let (blocks, projected) = build_fallback(body);
        let json = ab_ir::blocks_to_aat_json(&blocks);
        let content = json[0]["content"].as_array().unwrap();

        assert_eq!(projected.visible_text, "ことを、にも");
        assert!(content.iter().any(|node| node["kind"] == "gaiji"));
        assert!(!content.iter().any(|node| node["reading"] == "おくび"));
    }

    #[test]
    fn fallback_blocks_preserve_gaiji_markers_inside_ruby_readings() {
        let body = "淡絹《※［＃濁点付き片仮名ヱ、1-7-84］エル》";
        let (blocks, projected) = build_fallback(body);
        let json = ab_ir::blocks_to_aat_json(&blocks);
        let content = json[0]["content"].as_array().unwrap();

        assert_eq!(projected.visible_text, "淡絹");
        assert!(content.iter().any(|node| node["kind"] == "ruby"));
        assert!(content.iter().any(|node| {
            node["kind"] == "gaiji" && node["description"] == "濁点付き片仮名ヱ、1-7-84"
        }));
        assert_eq!(ab_ir::provenance_counts(&blocks).source_supplement, 0);
    }

    #[test]
    fn fallback_blocks_preserve_gaiji_markers_inside_explicit_ruby_bases() {
        let body = "木部｜孤※［＃「筑」の「凡」に代えて「卩」、第3水準1-89-60］《こきょう》";
        let (blocks, projected) = build_fallback(body);
        let json = ab_ir::blocks_to_aat_json(&blocks);
        let content = json[0]["content"].as_array().unwrap();

        assert_eq!(projected.visible_text, "木部孤");
        assert!(content.iter().any(|node| {
            node["kind"] == "ruby" && node["base"] == "孤" && node["reading"] == "こきょう"
        }));
        assert!(content.iter().any(|node| {
            node["kind"] == "gaiji"
                && node["description"] == "「筑」の「凡」に代えて「卩」、第3水準1-89-60"
        }));
        assert_eq!(ab_ir::provenance_counts(&blocks).source_supplement, 0);
    }

    #[test]
    fn fallback_blocks_trim_malformed_bottom_note_fragments() {
        let body = "前文。\n評に曰く百圓［は＃「百圓は」はママ］密かに続く。";
        let (blocks, projected) = build_fallback(body);

        assert_eq!(projected.visible_text, "前文。\n密かに続く。");
        assert_eq!(source_visible_text(body), "前文。\n密かに続く。");
        assert_eq!(ab_ir::provenance_counts(&blocks).source_supplement, 0);
    }

    #[test]
    fn fallback_blocks_use_legacy_supplements_when_structured_drops_validation_ruby() {
        let body = "軌［＃「軌」に「（ママ）」の注記］り［＃「軌［＃「軌」に「（ママ）」の注記］り」は底本では「軌《きし》り」］";
        let (blocks, _projected) = build_fallback(body);
        let json = ab_ir::blocks_to_aat_json(&blocks);
        let content = json[0]["content"].as_array().unwrap();

        assert!(content.iter().any(|node| {
            node["kind"] == "ruby"
                && node["reading"] == "きし"
                && node["x-provenance"] == "source_supplement"
        }));
        assert!(ab_ir::provenance_counts(&blocks).source_supplement > 0);
    }

    #[test]
    fn fallback_blocks_can_reuse_source_visible_text() {
        let body = "吾輩《わがはい》は猫である。";
        let source_visible = source_visible_text(body).into_owned();

        let (blocks, projected) = build_fallback_from_source_visible(body, source_visible);

        assert!(matches!(blocks[0], Block::Paragraph { .. }));
        assert_eq!(projected.visible_text, "吾輩は猫である。");
        assert!(ab_ir::provenance_counts(&blocks).source_fallback > 0);
        assert_eq!(ab_ir::provenance_counts(&blocks).source_supplement, 0);
    }

    #[test]
    fn fallback_blocks_remove_jis_gaiji_markers_from_large_body_projection() {
        let mut body =
            "この卷見※［＃「二点しんにょう＋官」、第3水準1-92-56］すべきもの\n".repeat(10_000);
        body.push_str("終わり");

        let (_blocks, projected) = build_fallback(&body);

        assert!(!projected.visible_text.contains("二点しんにょう"));
        assert!(projected.visible_text.contains("この卷見すべきもの"));
    }

    #[test]
    fn source_annotation_supplements_are_marked_as_source_derived() {
        let mut blocks = vec![Block::Paragraph { content: vec![] }];

        append_source_annotation_supplements(
            &mut blocks,
            "吾輩《わがはい》は※［＃「口＋世」、U+546D］である。",
        );

        let counts = ab_ir::provenance_counts(&blocks);
        assert_eq!(counts.source_supplement, 2);
    }

    #[test]
    fn source_annotation_supplements_consider_all_existing_blocks() {
        let mut blocks = vec![
            Block::Paragraph {
                content: vec![Inline::text("吾輩")],
            },
            Block::Paragraph {
                content: vec![
                    Inline::ruby("吾輩", "わがはい"),
                    Inline::gaiji("「口＋世」、U+546D", "", None),
                ],
            },
        ];

        append_source_annotation_supplements(
            &mut blocks,
            "吾輩《わがはい》は※［＃「口＋世」、U+546D］である。",
        );

        let counts = ab_ir::provenance_counts(&blocks);
        assert_eq!(counts.source_supplement, 0);
    }

    #[test]
    fn command_strip_borrows_marker_free_text() {
        let mut state = CommandStripState::None;

        let stripped = stripped_command_fragments("吾輩は猫である。", &mut state);

        assert!(matches!(stripped, std::borrow::Cow::Borrowed(_)));
        assert_eq!(stripped, "吾輩は猫である。");
    }

    #[test]
    fn source_annotation_markers_collects_ruby_and_gaiji_in_one_scan() {
        let markers = ab_source_syntax::source_annotations(
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
    fn source_annotation_markers_collects_gaiji_inside_ruby_text() {
        let markers =
            ab_source_syntax::source_annotations("淡絹《※［＃濁点付き片仮名ヱ、1-7-84］エル》");

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
    fn source_annotation_markers_continue_after_unclosed_gaiji_marker() {
        let markers = ab_source_syntax::source_annotations("※［＃壊れた注記」然《ぼうぜん》");

        assert_eq!(
            markers
                .ruby_readings
                .iter()
                .map(|marker| marker.value)
                .collect::<Vec<_>>(),
            vec!["ぼうぜん"]
        );
        assert!(markers.gaiji_descriptions.is_empty());
    }

    #[test]
    fn source_annotation_markers_continue_after_unclosed_gaiji_marker_with_inner_marker() {
        let markers = ab_source_syntax::source_annotations(
            "※［＃「※」は「りっしんべん＋夢」と同義、読みは「ぼう」、第4水準2-12-81、7-5」然《ぼうぜん》",
        );

        assert_eq!(
            markers
                .ruby_readings
                .iter()
                .map(|marker| marker.value)
                .collect::<Vec<_>>(),
            vec!["ぼうぜん"]
        );
    }

    #[test]
    fn source_annotation_markers_do_not_let_unclosed_gaiji_cross_lines() {
        let markers = ab_source_syntax::source_annotations(
            "※［＃壊れた注記」然《ぼうぜん》\n次行《つぎ》［＃注記］",
        );

        assert_eq!(
            markers
                .ruby_readings
                .iter()
                .map(|marker| marker.value)
                .collect::<Vec<_>>(),
            vec!["ぼうぜん", "つぎ"]
        );
        assert!(markers.gaiji_descriptions.is_empty());
    }
}
