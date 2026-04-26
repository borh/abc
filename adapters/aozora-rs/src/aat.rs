use std::{borrow::Cow, collections::HashMap, time::Instant};

use ab_ir::{Block, GaijiKind, GaijiRef, Inline, ProjectedText, Provenance, RubyPlacement};
use aozora_rs_core::{Break, Deco, Retokenized};
use winnow::Parser;

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
    let mut readings = ruby_readings_in_blocks(blocks);
    for marker in &annotations.ruby_readings {
        let remaining = readings.get_mut(marker.value);
        if let Some(remaining) = remaining && *remaining > 0 {
            *remaining -= 1;
            continue;
        }
        return false;
    }
    gaiji_count_in_blocks(blocks) >= annotations.gaiji_descriptions.len()
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
                let (base, next_idx) =
                    collect_decorated_content(tokens, idx + 1, false, &|deco| {
                        matches!(deco, Deco::Ruby(_))
                    });
                push_ruby_inline(&mut content, base, reading, Provenance::Parser);
                idx = next_idx;
                continue;
            }
            Retokenized::DecoBegin(Deco::AHead | Deco::BHead | Deco::CHead) => {
                flush_paragraph(&mut blocks, &mut content);
                let deco = match &tokens[idx] {
                    Retokenized::DecoBegin(deco) => deco,
                    _ => unreachable!(),
                };
                let (heading_content, next_idx) =
                    collect_decorated_content(tokens, idx + 1, false, &|candidate| {
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
                    content: heading_content,
                });
                idx = next_idx;
                continue;
            }
            Retokenized::DecoBegin(deco) => {
                let (inner, next_idx) =
                    collect_decorated_content(tokens, idx + 1, false, &|candidate| {
                        same_deco_kind(candidate, deco)
                    });
                content.push(Inline::style(stable_style_type(deco), inner));
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

fn collect_decorated_content<'a>(
    tokens: &[Retokenized<'a>],
    mut idx: usize,
    preserve_styles: bool,
    is_matching_end: &dyn Fn(&Deco<'a>) -> bool,
) -> (Vec<Inline>, usize) {
    let mut content = Vec::new();
    while idx < tokens.len() {
        match &tokens[idx] {
            Retokenized::Text(text) => push_text(&mut content, &source_visible_text(text)),
            Retokenized::Odoriji(odoriji) => push_text(&mut content, odoriji_source_text(*odoriji)),
            Retokenized::Kunten(_) | Retokenized::Okurigana(_) => {}
            Retokenized::Break(_) => push_text(&mut content, "\n"),
            Retokenized::Figure(figure) => content.push(Inline::gaiji(
                figure.to_string(),
                "",
                Some(
                    "aozora-rs-core Figure Display output; original gaiji notation is not preserved by Figure",
                ),
            )),
            Retokenized::DecoBegin(Deco::Ruby(reading)) => {
                let (base, next_idx) =
                    collect_decorated_content(tokens, idx + 1, false, &|deco| {
                        matches!(deco, Deco::Ruby(_))
                    });
                push_ruby_inline(&mut content, base, reading, Provenance::Parser);
                idx = next_idx;
                continue;
            }
            Retokenized::DecoBegin(deco) => {
                let (inner, next_idx) =
                    collect_decorated_content(tokens, idx + 1, false, &|candidate| {
                        same_deco_kind(candidate, deco)
                    });
                if preserve_styles {
                    content.push(Inline::style(stable_style_type(deco), inner));
                } else {
                    content.extend(inner);
                }
                idx = next_idx;
                continue;
            }
            Retokenized::DecoEnd(deco) if is_matching_end(deco) => {
                return (content, idx + 1);
            }
            Retokenized::DecoEnd(_) => {}
        }
        idx += 1;
    }
    (content, idx)
}

fn push_ruby_inline(
    content: &mut Vec<Inline>,
    base: Vec<Inline>,
    reading: &str,
    provenance: Provenance,
) {
    if contains_ruby_inline(&base) {
        content.extend(base);
    } else if !is_pathological_ruby_base(&inline_visible_text(&base)) {
        content.push(Inline::ruby_with_base_and_provenance(
            base,
            reading,
            RubyPlacement::Right,
            provenance,
        ));
    }
}

fn is_pathological_ruby_base(base: &str) -> bool {
    base.contains('\n') || base.chars().count() > 80
}

fn contains_ruby_inline(content: &[Inline]) -> bool {
    content.iter().any(|node| match node {
        Inline::Ruby { .. } => true,
        Inline::Style { content, .. } => contains_ruby_inline(content),
        Inline::Text { .. } | Inline::GaijiRef(_) => false,
    })
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
    let mut existing_ruby_readings = ruby_readings_in_blocks(blocks);
    let existing_gaiji_count = gaiji_count_in_blocks(blocks);
    repair_or_supplement_source_ruby(blocks, body, &mut existing_ruby_readings);
    let Some(first_block) = blocks.first_mut() else {
        return;
    };
    let content = ab_ir::block_content_mut(first_block);
    insert_gaiji_supplements(content, body, existing_gaiji_count);
}

fn append_legacy_source_annotation_supplements(blocks: &mut [Block], body: &str) {
    let mut existing_ruby_readings = ruby_readings_in_blocks(blocks);
    let existing_gaiji_count = gaiji_count_in_blocks(blocks);
    let markers = ab_source_syntax::source_annotations(body);
    let Some(first_block) = blocks.first_mut() else {
        return;
    };
    let content = ab_ir::block_content_mut(first_block);
    append_ruby_supplements(
        content,
        markers.ruby_readings.iter().map(|marker| marker.value),
        &mut existing_ruby_readings,
    );
    append_unresolved_gaiji_supplements(
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
    append_legacy_source_annotation_supplements(&mut blocks, body);
    blocks
}

fn structured_source_fallback_blocks(body: &str) -> Vec<Block> {
    let mut content = Vec::new();
    let mut last_gaiji_end = None;
    for event in ab_source_syntax::source_events(body) {
        match event.kind {
            ab_source_syntax::SourceEventKind::Text(text) => {
                push_source_fallback_text(&mut content, text);
                last_gaiji_end = None;
            }
            ab_source_syntax::SourceEventKind::Gaiji { description } => {
                content.push(gaiji_inline(description, Provenance::SourceFallback));
                last_gaiji_end = Some(event.span.end);
            }
            ab_source_syntax::SourceEventKind::Ruby {
                base_source: Some(base_source),
                reading,
            } => {
                let base = source_visible_text(base_source).into_owned();
                push_source_fallback_ruby(
                    &mut content,
                    base,
                    base_source,
                    reading,
                );
                last_gaiji_end = None;
            }
            ab_source_syntax::SourceEventKind::Ruby {
                base_source: None,
                reading,
            } => {
                if last_gaiji_end != Some(event.span.start) {
                    let base = take_implicit_ruby_base(&mut content);
                    push_source_fallback_ruby(&mut content, base, "", reading);
                }
                last_gaiji_end = None;
            }
            ab_source_syntax::SourceEventKind::EditorialNote {
                kind: ab_source_syntax::EditorialNoteKind::BottomTextCorrection,
                ..
            } => {
                trim_source_fallback_note_prefix(&mut content);
                last_gaiji_end = None;
            }
            ab_source_syntax::SourceEventKind::Command { .. }
            | ab_source_syntax::SourceEventKind::EditorialNote { .. }
            | ab_source_syntax::SourceEventKind::SegmentBoundary { .. } => {
                last_gaiji_end = None;
            }
        }
    }

    vec![Block::Paragraph { content }]
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
    let start = implicit_ruby_base_start(value);
    let base = value.split_off(start);
    if value.is_empty() {
        content.pop();
    }
    base
}

fn implicit_ruby_base_start(value: &str) -> usize {
    value
        .char_indices()
        .rev()
        .take_while(|(_, ch)| is_implicit_ruby_base_char(*ch))
        .last()
        .map(|(offset, _)| offset)
        .unwrap_or(value.len())
}

fn is_implicit_ruby_base_char(ch: char) -> bool {
    matches!(
        ch,
        '\u{3400}'..='\u{9fff}'
            | '\u{f900}'..='\u{faff}'
            | '\u{20000}'..='\u{2fa1f}'
            | '々'
            | '〻'
            | '〆'
            | 'ヶ'
            | 'ヵ'
    )
}

fn append_ruby_supplements(
    content: &mut Vec<Inline>,
    readings: impl IntoIterator<Item = impl AsRef<str>>,
    existing: &mut HashMap<String, usize>,
) {
    for reading in readings {
        let reading = reading.as_ref();
        match existing.get_mut(reading) {
            Some(count) if *count > 0 => {
                *count -= 1;
            }
            _ => {
                content.push(Inline::ruby_with_provenance(
                    "",
                    reading,
                    Provenance::SourceSupplement,
                ));
            }
        }
    }
}

fn repair_or_supplement_source_ruby(
    blocks: &mut [Block],
    body: &str,
    existing: &mut HashMap<String, usize>,
) {
    let mut last_gaiji_end = None;
    let mut search_start = 0;
    let mut supplements = Vec::new();
    for event in ab_source_syntax::source_events(body) {
        match event.kind {
            ab_source_syntax::SourceEventKind::Gaiji { .. } => {
                last_gaiji_end = Some(event.span.end);
            }
            ab_source_syntax::SourceEventKind::Ruby {
                base_source,
                reading,
            } => {
                let base = source_event_ruby_base(body, &event, base_source);
                let should_consume = existing
                    .get_mut(reading)
                    .is_some_and(|count| {
                        if *count > 0 {
                            *count -= 1;
                            true
                        } else {
                            false
                        }
                    });
                if should_consume {
                    last_gaiji_end = None;
                    continue;
                }
                if last_gaiji_end == Some(event.span.start) {
                    last_gaiji_end = None;
                    continue;
                }
                let repaired = base.as_ref().and_then(|base| {
                    wrap_next_text_match_in_blocks(blocks, base.as_ref(), reading, search_start)
                });
                if let Some(end) = repaired {
                    search_start = end;
                } else {
                    supplements.push(reading.to_owned());
                }
                last_gaiji_end = None;
            }
            ab_source_syntax::SourceEventKind::Text(_)
            | ab_source_syntax::SourceEventKind::Command { .. }
            | ab_source_syntax::SourceEventKind::EditorialNote { .. }
            | ab_source_syntax::SourceEventKind::SegmentBoundary { .. } => {
                last_gaiji_end = None;
            }
        }
    }

    let Some(first_block) = blocks.first_mut() else {
        return;
    };
    let content = ab_ir::block_content_mut(first_block);
    let mut empty = HashMap::new();
    append_ruby_supplements(content, supplements, &mut empty);
}

fn source_event_ruby_base(
    body: &str,
    event: &ab_source_syntax::SourceEvent<'_>,
    base_source: Option<&str>,
) -> Option<String> {
    match base_source {
        Some(base_source) => {
            let base = source_visible_text(base_source);
            if base.is_empty() {
                return None;
            }
            Some(base.into_owned())
        }
        None => {
            let base = implicit_ruby_base_before_marker(body, event.span.start);
            if base.is_empty() {
                return None;
            }
            Some(base.to_owned())
        }
    }
}

fn implicit_ruby_base_before_marker(body: &str, marker_start: usize) -> &str {
    let prefix = &body[..marker_start];
    let start = implicit_ruby_base_start(prefix);
    &prefix[start..]
}

fn wrap_text_visible_range_with_ruby(
    content: &mut Vec<Inline>,
    target_start: usize,
    target_end: usize,
    reading: &str,
) -> bool {
    if target_start >= target_end {
        return false;
    }

    let mut visible_len = 0;
    let mut idx = 0;
    while idx < content.len() {
        let node_len = inline_visible_len(&content[idx]);
        if visible_len + node_len <= target_start {
            visible_len += node_len;
            idx += 1;
            continue;
        }

        let Inline::Text { value, provenance } = &content[idx] else {
            return false;
        };
        if target_end > visible_len + value.len() {
            return false;
        }

        let local_start = target_start - visible_len;
        let local_end = target_end - visible_len;
        if !value.is_char_boundary(local_start) || !value.is_char_boundary(local_end) {
            return false;
        }

        let before = value[..local_start].to_owned();
        let base = value[local_start..local_end].to_owned();
        let after = value[local_end..].to_owned();
        let provenance = *provenance;
        let mut replacement = Vec::new();
        if !before.is_empty() {
            replacement.push(Inline::text_with_provenance(before, provenance));
        }
        replacement.push(Inline::ruby_with_base_and_provenance(
            vec![Inline::text_with_provenance(base, provenance)],
            reading,
            RubyPlacement::Right,
            Provenance::ParserNormalized,
        ));
        if !after.is_empty() {
            replacement.push(Inline::text_with_provenance(after, provenance));
        }
        content.splice(idx..=idx, replacement);
        return true;
    }

    false
}

fn wrap_next_text_match_in_blocks(
    blocks: &mut [Block],
    base: &str,
    reading: &str,
    search_start: usize,
) -> Option<usize> {
    if base.is_empty() {
        return None;
    }

    let mut visible_len = 0;
    for block in blocks {
        let content = ab_ir::block_content_mut(block);
        let content_len = content.iter().map(inline_visible_len).sum::<usize>();
        if let Some(end) = wrap_next_text_match_in_content(
            content,
            base,
            reading,
            search_start.saturating_sub(visible_len),
        ) {
            return Some(visible_len + end);
        }
        visible_len += content_len;
    }

    None
}

fn wrap_next_text_match_in_content(
    content: &mut Vec<Inline>,
    base: &str,
    reading: &str,
    search_start: usize,
) -> Option<usize> {
    let mut visible_len = 0;
    let mut idx = 0;
    while idx < content.len() {
        let node_len = inline_visible_len(&content[idx]);
        if let Inline::Style { content: inner, .. } = &mut content[idx] {
            if let Some(end) = wrap_next_text_match_in_content(
                inner,
                base,
                reading,
                search_start.saturating_sub(visible_len),
            ) {
                return Some(visible_len + end);
            }
            visible_len += node_len;
            idx += 1;
            continue;
        }

        if let Inline::Text { value, .. } = &content[idx] {
            let local_search_start = search_start.saturating_sub(visible_len).min(value.len());
            if value.is_char_boundary(local_search_start)
                && let Some(match_start) = value[local_search_start..].find(base)
            {
                let target_start = visible_len + local_search_start + match_start;
                let target_end = target_start + base.len();
                if wrap_text_visible_range_with_ruby(content, target_start, target_end, reading) {
                    return Some(target_end);
                }
            }
        }
        visible_len += node_len;
        idx += 1;
    }

    None
}

fn insert_gaiji_supplements(content: &mut Vec<Inline>, body: &str, existing_count: usize) {
    let mut seen = 0;
    let mut inserted = 0;
    for event in ab_source_syntax::source_events(body) {
        let ab_source_syntax::SourceEventKind::Gaiji { description } = event.kind else {
            continue;
        };
        if seen < existing_count {
            seen += 1;
            continue;
        }
        let prefix = source_visible_text(&body[..event.span.start]);
        let gaiji = gaiji_inline(description, Provenance::ParserNormalized);
        insert_inline_at_visible_len(content, prefix.len(), gaiji);
        inserted += 1;
        seen += 1;
    }
    let markers = ab_source_syntax::source_annotations(body);
    append_unresolved_gaiji_supplements(
        content,
        markers.gaiji_descriptions.iter().map(|marker| marker.value),
        existing_count + inserted,
    );
}

fn gaiji_inline(description: &str, provenance: Provenance) -> Inline {
    parsed_gaiji(description, provenance).unwrap_or_else(|| {
        let fallback_provenance = if provenance == Provenance::ParserNormalized {
            Provenance::SourceSupplement
        } else {
            provenance
        };
        Inline::gaiji_with_provenance(description, "", None, fallback_provenance)
    })
}

fn parsed_gaiji(description: &str, provenance: Provenance) -> Option<Inline> {
    let mut input = description;
    let parsed = gaiji_chuki_parser::parse_tag.parse_next(&mut input).ok()?;
    if !input.is_empty() {
        return None;
    }

    let (kind, resolved) = if let Some(unicode) = parsed.unicode {
        let values = unicode.chars().collect::<Vec<_>>();
        let kind = match values.as_slice() {
            [value] => GaijiKind::UnicodeCodepoint { value: *value },
            _ => GaijiKind::UnicodeSequence {
                values: values.clone(),
            },
        };
        (kind, Some(String::new()))
    } else if let Some((plane, row, cell)) = parsed.sjis {
        (
            GaijiKind::JisCode {
                plane: Some(plane),
                row,
                cell,
            },
            Some(String::new()),
        )
    } else {
        return None;
    };

    Some(Inline::gaiji_ref(GaijiRef {
        source: format!("※［＃{description}］"),
        description: description.to_owned(),
        description_format: Some("aozora-gaiji-tag".to_owned()),
        kind,
        resolved,
        provenance,
    }))
}

fn insert_inline_at_visible_len(content: &mut Vec<Inline>, target: usize, node: Inline) {
    let mut visible_len = 0;
    let mut idx = 0;
    while idx < content.len() {
        let node_len = inline_visible_len(&content[idx]);
        if visible_len + node_len < target {
            visible_len += node_len;
            idx += 1;
            continue;
        }
        if visible_len + node_len == target {
            content.insert(idx + 1, node);
            return;
        }
        if let Inline::Text { value, provenance } = &mut content[idx] {
            let split_at = target - visible_len;
            if value.is_char_boundary(split_at) {
                let trailing = value.split_off(split_at);
                let provenance = *provenance;
                content.insert(idx + 1, node);
                if !trailing.is_empty() {
                    content.insert(idx + 2, Inline::text_with_provenance(trailing, provenance));
                }
                return;
            }
        }
        break;
    }
    content.push(node);
}

fn append_unresolved_gaiji_supplements(
    content: &mut Vec<Inline>,
    descriptions: impl IntoIterator<Item = impl AsRef<str>>,
    existing_count: usize,
) {
    for description in descriptions.into_iter().skip(existing_count) {
        content.push(Inline::gaiji_with_provenance(
            description.as_ref(),
            "",
            None,
            Provenance::SourceSupplement,
        ));
    }
}

fn ruby_readings_in_blocks(blocks: &[Block]) -> HashMap<String, usize> {
    let mut readings = HashMap::new();
    for block in blocks {
        for child in ab_ir::block_content(block) {
            collect_ruby_readings(child, &mut readings);
        }
    }
    readings
}

fn collect_ruby_readings(node: &Inline, readings: &mut HashMap<String, usize>) {
    match node {
        Inline::Ruby { reading, .. } => {
            *readings.entry(reading.clone()).or_insert(0) += 1;
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
    fn blocks_cover_validation_annotations_respects_duplicate_ruby_occurrences() {
        let blocks = vec![Block::Paragraph {
            content: vec![Inline::ruby("地球", "ちきゆう")],
        }];

        assert!(blocks_cover_validation_annotations(
            "地球《ちきゆう》。",
            &blocks,
        ));
        assert!(!blocks_cover_validation_annotations(
            "地球《まいち》は月《ちきゆう》。",
            &blocks,
        ));
        assert!(!blocks_cover_validation_annotations(
            "地球《ちきゆう》と月《ちきゆう》。",
            &blocks,
        ));
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
    fn fallback_blocks_remove_commands_with_nested_commands() {
        let body = "アヌンチヤタ［＃「アヌンチヤタ［＃「アヌンチヤタ」に傍線］」は底本では「アンヌチヤタ［＃「アンヌチヤタ」に傍線］」］ありて";
        let blocks = structured_source_fallback_blocks(body);
        let projected = ab_ir::visible_projection(&blocks);

        assert_eq!(projected.visible_text, "アヌンチヤタありて");
        assert_eq!(ab_ir::provenance_counts(&blocks).source_supplement, 0);
    }

    #[test]
    fn fallback_blocks_model_non_hash_ruby_correction_notes_without_supplements() {
        let body =
            "『断頭台《ラギュイヨチーン》［ルビの「ラギュイヨチーン」は底本では「ラギュイヨケーン」］』";

        let (blocks, projected) = build_fallback(body);
        let json = ab_ir::blocks_to_aat_json(&blocks);
        let content = json[0]["content"].as_array().unwrap();

        assert_eq!(projected.visible_text, "『断頭台』");
        assert!(content.iter().any(|node| {
            node["kind"] == "ruby"
                && node["base"] == "断頭台"
                && node["reading"] == "ラギュイヨチーン"
        }));
        assert_eq!(ab_ir::provenance_counts(&blocks).source_supplement, 0);
    }

    #[test]
    fn fallback_blocks_drop_terminal_provenance_note_before_colophon() {
        let body = "私のお話は之で終りといたします。［＃地付き］（昭和九年十一月十五日ラジオ放送の遺稿より）\n\n底本：「ある英語教師の思い出」";

        let (blocks, projected) = build_fallback(body);

        assert_eq!(
            projected.visible_text,
            "私のお話は之で終りといたします。\n\n底本：「ある英語教師の思い出」"
        );
        assert_eq!(ab_ir::provenance_counts(&blocks).source_supplement, 0);
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
    fn source_annotation_supplements_keep_ruby_source_derived() {
        let mut blocks = vec![Block::Paragraph {
            content: vec![Inline::text_with_provenance(
                "吾輩はである。",
                Provenance::ParserNormalized,
            )],
        }];

        append_source_annotation_supplements(
            &mut blocks,
            "吾輩《わがはい》は※［＃「口＋世」、U+546D］である。",
        );

        let counts = ab_ir::provenance_counts(&blocks);
        assert_eq!(ab_ir::visible_projection(&blocks).visible_text, "吾輩はである。");
        assert_eq!(counts.source_supplement, 0);
        assert!(counts.parser_normalized >= 2);
        let json = ab_ir::blocks_to_aat_json(&blocks);
        assert_eq!(json[0]["content"][0]["kind"], "ruby");
        assert_eq!(json[0]["content"][0]["base"], "吾輩");
        assert_eq!(json[0]["content"][0]["reading"], "わがはい");
        assert_eq!(json[0]["content"][0]["x-provenance"], "parser_normalized");
    }

    #[test]
    fn source_annotation_supplements_repair_implicit_ruby_from_kanji_suffix() {
        let mut blocks = vec![Block::Paragraph {
            content: vec![Inline::text_with_provenance(
                "邦語に直譯しては通ぜざれば",
                Provenance::ParserNormalized,
            )],
        }];

        append_source_annotation_supplements(
            &mut blocks,
            "邦語《はうご》に直譯《ちょくやく》しては通《つう》ぜざれば",
        );

        let counts = ab_ir::provenance_counts(&blocks);
        let json = ab_ir::blocks_to_aat_json(&blocks);
        let content = json[0]["content"].as_array().unwrap();

        assert_eq!(
            ab_ir::visible_projection(&blocks).visible_text,
            "邦語に直譯しては通ぜざれば"
        );
        assert_eq!(counts.source_supplement, 0);
        assert!(content.iter().any(|node| {
            node["kind"] == "ruby" && node["base"] == "邦語" && node["reading"] == "はうご"
        }));
        assert!(content.iter().any(|node| {
            node["kind"] == "ruby" && node["base"] == "直譯" && node["reading"] == "ちょくやく"
        }));
        assert!(content.iter().any(|node| {
            node["kind"] == "ruby" && node["base"] == "通" && node["reading"] == "つう"
        }));
    }

    #[test]
    fn source_annotation_supplements_repair_same_reading_multiple_times() {
        let mut blocks = vec![Block::Paragraph {
            content: vec![
                Inline::ruby("地球", "ちきゆう"),
                Inline::text_with_provenance("は", Provenance::ParserNormalized),
                Inline::text_with_provenance("月。", Provenance::ParserNormalized),
            ],
        }];

        append_source_annotation_supplements(
            &mut blocks,
            "地球《ちきゆう》は月《ちきゆう》。",
        );

        let counts = ab_ir::provenance_counts(&blocks);
        let json = ab_ir::blocks_to_aat_json(&blocks);
        let content = json[0]["content"].as_array().unwrap();

        assert_eq!(ab_ir::visible_projection(&blocks).visible_text, "地球は月。");
        assert_eq!(counts.source_supplement, 0);
        assert!(content.iter().any(|node| {
            node["kind"] == "ruby" && node["base"] == "月" && node["reading"] == "ちきゆう"
        }));
    }

    #[test]
    fn source_annotation_supplements_repair_ruby_inside_style_content() {
        let mut blocks = vec![Block::Paragraph {
            content: vec![Inline::style(
                "indent",
                vec![Inline::text_with_provenance(
                    "パーリスの侍童。",
                    Provenance::ParserNormalized,
                )],
            )],
        }];

        append_source_annotation_supplements(&mut blocks, "パーリスの侍童《こしゃう》。");

        let counts = ab_ir::provenance_counts(&blocks);
        let json = ab_ir::blocks_to_aat_json(&blocks);
        let style_content = json[0]["content"][0]["content"].as_array().unwrap();

        assert_eq!(ab_ir::visible_projection(&blocks).visible_text, "パーリスの侍童。");
        assert_eq!(counts.source_supplement, 0);
        assert!(style_content.iter().any(|node| {
            node["kind"] == "ruby" && node["base"] == "侍童" && node["reading"] == "こしゃう"
        }));
    }

    #[test]
    fn build_initial_preserves_nested_ruby_inside_decorated_scope() {
        let body = "［＃ここから１字下げ］吾輩《わがはい》は猫《ねこ》。［＃ここで字下げ終わり］\n";
        let parsed = parse_with_aozora_rs(BodySelection {
            validation_body: body,
            found_separators: false,
            elapsed: Duration::ZERO,
        })
        .unwrap();

        let built = build_initial(&parsed);
        let counts = ab_ir::provenance_counts(&built.blocks);
        let json = ab_ir::blocks_to_aat_json(&built.blocks);

        assert_eq!(built.projected.visible_text, "吾輩は猫。");
        assert_eq!(counts.source_supplement, 0);
        assert!(json_has_ruby(&json[0], "吾輩", "わがはい"));
        assert!(json_has_ruby(&json[0], "猫", "ねこ"));
    }

    #[test]
    fn retokenized_blocks_flatten_nested_non_ruby_styles() {
        let tokens = vec![
            Retokenized::DecoBegin(Deco::Bold),
            Retokenized::DecoBegin(Deco::Italic),
            Retokenized::Text("吾輩"),
            Retokenized::DecoEnd(Deco::Italic),
            Retokenized::DecoEnd(Deco::Bold),
        ];

        let blocks = retokenized_to_aat_blocks(&tokens);
        let json = ab_ir::blocks_to_aat_json(&blocks);
        let style = &json[0]["content"][0];

        assert_eq!(style["kind"], "style");
        assert_eq!(style["style_type"], "bold");
        assert_eq!(style["content"][0]["kind"], "text");
        assert!(!json_has_nested_style(style));
    }

    #[test]
    fn source_annotation_supplements_parse_unicode_gaiji_as_parser_normalized() {
        let mut blocks = vec![Block::Paragraph { content: vec![] }];

        append_source_annotation_supplements(&mut blocks, "※［＃「口＋世」、U+546D］");

        let counts = ab_ir::provenance_counts(&blocks);
        assert_eq!(counts.source_supplement, 0);
        assert_eq!(counts.parser_normalized, 1);
        let json = ab_ir::blocks_to_aat_json(&blocks);
        assert_eq!(json[0]["content"][0]["kind"], "gaiji");
        assert_eq!(json[0]["content"][0]["description"], "「口＋世」、U+546D");
        assert_eq!(json[0]["content"][0]["resolved"], "");
    }

    #[test]
    fn source_annotation_supplements_parse_jis_gaiji_as_invisible_parser_normalized() {
        let mut blocks = vec![Block::Paragraph { content: vec![] }];

        append_source_annotation_supplements(
            &mut blocks,
            "※［＃「二点しんにょう＋官」、第3水準1-92-56］",
        );

        let counts = ab_ir::provenance_counts(&blocks);
        assert_eq!(counts.source_supplement, 0);
        assert_eq!(counts.parser_normalized, 1);
        let json = ab_ir::blocks_to_aat_json(&blocks);
        assert_eq!(json[0]["content"][0]["kind"], "gaiji");
        assert_eq!(
            json[0]["content"][0]["description"],
            "「二点しんにょう＋官」、第3水準1-92-56"
        );
        assert_eq!(json[0]["content"][0]["resolved"], "");
    }

    #[test]
    fn source_annotation_supplements_keep_gaiji_inside_ruby_reading_as_invisible_metadata() {
        let mut blocks = vec![Block::Paragraph {
            content: vec![Inline::ruby(
                "淡絹",
                "※［＃濁点付き片仮名ヱ、1-7-84］エル",
            )],
        }];

        append_source_annotation_supplements(
            &mut blocks,
            "淡絹《※［＃濁点付き片仮名ヱ、1-7-84］エル》",
        );

        let counts = ab_ir::provenance_counts(&blocks);
        assert_eq!(counts.source_supplement, 1);
        let json = ab_ir::blocks_to_aat_json(&blocks);
        assert_eq!(json[0]["content"][1]["kind"], "gaiji");
        assert_eq!(json[0]["content"][1]["resolved"], "");
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

    fn json_has_ruby(value: &serde_json::Value, base: &str, reading: &str) -> bool {
        match value {
            serde_json::Value::Object(object) => {
                if object.get("kind").and_then(serde_json::Value::as_str) == Some("ruby")
                    && object.get("base").and_then(serde_json::Value::as_str) == Some(base)
                    && object.get("reading").and_then(serde_json::Value::as_str) == Some(reading)
                {
                    return true;
                }
                object.values().any(|child| json_has_ruby(child, base, reading))
            }
            serde_json::Value::Array(values) => {
                values.iter().any(|child| json_has_ruby(child, base, reading))
            }
            _ => false,
        }
    }

    fn json_has_nested_style(style: &serde_json::Value) -> bool {
        style["content"].as_array().is_some_and(|children| {
            children.iter().any(|child| {
                child["kind"] == "style" || json_has_nested_style(child)
            })
        })
    }
}
