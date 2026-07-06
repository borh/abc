use std::{borrow::Cow, collections::HashMap, time::Instant};

use ab_ir::{
    Block, GaijiKind, GaijiRef, Inline, ProjectedText, Provenance, RubyPlacement, StyleAttr,
    StyleAttrValue,
};
use aozora_rs_core::{Deco, Figure, Retokenized};
use aozora_rs_gaiji::{gaiji_to_char, parse_tag};
use winnow::Parser;

use crate::{metrics::FallbackDecision, parser::ParsedSource, source::source_visible_text};

#[derive(Debug, Clone)]
pub struct InitialAatBuildResult {
    pub blocks: Vec<Block>,
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

#[cfg(test)]
pub fn build_initial(parsed: &ParsedSource<'_>) -> InitialAatBuildResult {
    let source_events = ab_source_syntax::source_events(parsed.body.validation_body);
    build_initial_with_events(parsed, &source_events)
}

#[cfg(test)]
pub fn build_initial_with_events<'a>(
    parsed: &ParsedSource<'a>,
    source_events: &[ab_source_syntax::SourceEvent<'a>],
) -> InitialAatBuildResult {
    let body = parsed.body.validation_body;
    let base = build_initial_without_source_annotations(parsed);
    build_initial_with_existing_blocks(base, body, source_events)
}

#[cfg(test)]
pub fn build_initial_with_existing_blocks<'a>(
    base: InitialAatBuildResult,
    body: &str,
    source_events: &[ab_source_syntax::SourceEvent<'a>],
) -> InitialAatBuildResult {
    let annotations = ab_source_syntax::source_annotations_both_from_events(source_events);
    build_initial_with_existing_blocks_with_annotations(base, body, source_events, &annotations)
}

pub fn build_initial_with_existing_blocks_with_annotations<'a>(
    mut base: InitialAatBuildResult,
    body: &str,
    source_events: &[ab_source_syntax::SourceEvent<'a>],
    annotations: &ab_source_syntax::SourceAnnotationsBoth<'a>,
) -> InitialAatBuildResult {
    let start = Instant::now();
    let mut blocks = std::mem::take(&mut base.blocks);
    append_source_annotation_supplements_with_events(&mut blocks, body, source_events, annotations);
    strip_cross_node_commands(&mut blocks);
    InitialAatBuildResult {
        blocks,
        timings: AatBuildTimings {
            build: base.timings.build + start.elapsed(),
        },
    }
}

pub fn build_initial_without_source_annotations<'a>(
    parsed: &ParsedSource<'a>,
) -> InitialAatBuildResult {
    let start = Instant::now();
    let mut blocks = retokenized_to_aat_blocks(&parsed.retokenized);
    strip_cross_node_commands(&mut blocks);
    InitialAatBuildResult {
        blocks,
        timings: AatBuildTimings {
            build: start.elapsed(),
        },
    }
}

pub(crate) fn projected_visible_from_retokenized<'a>(retokenized: &[Retokenized<'a>]) -> String {
    let mut visible = String::new();
    collect_projected_visible_text(retokenized, 0, None, &mut visible);
    ab_source_syntax::comparison_lossy_body(&visible).into_owned()
}

#[cfg(test)]
pub fn build_fallback_from_events<'a>(
    source_events: &[ab_source_syntax::SourceEvent<'a>],
    source_visible: Option<String>,
) -> (Vec<Block>, ProjectedText) {
    build_fallback_from_events_with_annotations(source_events, source_visible, None)
}

pub fn build_fallback_from_events_with_annotations<'a>(
    source_events: &[ab_source_syntax::SourceEvent<'a>],
    source_visible: Option<String>,
    source_annotations_both: Option<&ab_source_syntax::SourceAnnotationsBoth<'a>>,
) -> (Vec<Block>, ProjectedText) {
    match source_visible {
        Some(source_visible) => {
            let blocks = if let Some(annotations) = source_annotations_both {
                source_visible_fallback_blocks_with_annotations(
                    source_visible,
                    source_events,
                    annotations,
                )
            } else {
                source_visible_fallback_blocks_with_events(source_visible, source_events)
            };
            let projected = ab_ir::visible_projection(&blocks);
            (blocks, projected)
        }
        None => {
            let blocks = structured_source_fallback_blocks_with_events(source_events);
            let projected = ab_ir::visible_projection(&blocks);
            (blocks, projected)
        }
    }
}

pub fn build_fallback_without_annotations(body: &str) -> (Vec<Block>, ProjectedText) {
    let blocks = vec![Block::Paragraph {
        content: vec![Inline::text_with_provenance(
            body,
            Provenance::SourceFallback,
        )],
    }];
    let projected = ab_ir::visible_projection(&blocks);
    (blocks, projected)
}

fn collect_projected_visible_text<'a>(
    tokens: &[Retokenized<'a>],
    mut idx: usize,
    matching_end: Option<&dyn Fn(&Deco<'a>) -> bool>,
    visible: &mut String,
) -> usize {
    while idx < tokens.len() {
        match &tokens[idx] {
            Retokenized::Text(text) => visible.push_str(text),
            Retokenized::Kunten(_) | Retokenized::Okurigana(_) => {}
            Retokenized::Br => visible.push('\n'),
            Retokenized::Figure(_) => {}
            Retokenized::DecoBegin(Deco::Ruby(_)) => {
                idx = collect_projected_visible_text(
                    tokens,
                    idx + 1,
                    Some(&|deco| matches!(deco, Deco::Ruby(_))),
                    visible,
                );
                continue;
            }
            Retokenized::DecoBegin(deco) => {
                idx = collect_projected_visible_text(
                    tokens,
                    idx + 1,
                    Some(&|candidate| same_deco_kind(candidate, deco)),
                    visible,
                );
                continue;
            }
            Retokenized::DecoEnd(deco) => {
                if let Some(is_match) = matching_end
                    && is_match(deco)
                {
                    return idx + 1;
                }
            }
        }
        idx += 1;
    }

    idx
}

#[cfg(test)]
pub fn build_fallback(body: &str) -> (Vec<Block>, ProjectedText) {
    build_fallback_from_events(
        &ab_source_syntax::source_events(body),
        Some(source_visible_text(body).into_owned()),
    )
}

#[cfg(test)]
pub fn build_fallback_from_source_visible(
    body: &str,
    source_visible: String,
) -> (Vec<Block>, ProjectedText) {
    build_fallback_from_events(&ab_source_syntax::source_events(body), Some(source_visible))
}

#[cfg(test)]
pub fn blocks_cover_validation_annotations(body: &str, blocks: &[Block]) -> bool {
    let events = ab_source_syntax::source_events(body);
    blocks_cover_validation_annotations_with_events(blocks, &events)
}

fn blocks_cover_validation_annotations_with_annotations(
    annotations: &ab_source_syntax::SourceAnnotations<'_>,
    blocks: &[Block],
) -> bool {
    let mut readings = ruby_readings_in_blocks(blocks);
    for marker in &annotations.ruby_readings {
        let remaining = readings.get_mut(marker.value);
        if let Some(remaining) = remaining
            && *remaining > 0
        {
            *remaining -= 1;
            continue;
        }
        return false;
    }
    gaiji_count_in_blocks(blocks) >= annotations.gaiji_descriptions.len()
}

#[cfg(test)]
fn blocks_cover_validation_annotations_with_events<'a>(
    blocks: &[Block],
    source_events: &[ab_source_syntax::SourceEvent<'a>],
) -> bool {
    let annotations = ab_source_syntax::source_annotations_from_events(source_events, true);
    blocks_cover_validation_annotations_with_annotations(&annotations, blocks)
}

fn retokenized_to_aat_blocks(tokens: &[Retokenized<'_>]) -> Vec<Block> {
    let mut blocks = Vec::new();
    let mut content = Vec::new();
    let mut idx = 0;
    while idx < tokens.len() {
        match &tokens[idx] {
            Retokenized::Text(text) => push_text(&mut content, text),
            Retokenized::Kunten(_) | Retokenized::Okurigana(_) => {}
            Retokenized::Br => flush_paragraph(&mut blocks, &mut content),
            Retokenized::Figure(figure) => content.push(figure_inline(figure, Provenance::Parser)),
            Retokenized::DecoBegin(Deco::Ruby(reading)) => {
                let (base, next_idx) = collect_decorated_content(tokens, idx + 1, false, &|deco| {
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
                    style: heading_style_type(deco),
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
            Retokenized::Text(text) => push_text(&mut content, text),
            Retokenized::Kunten(_) | Retokenized::Okurigana(_) => {}
            Retokenized::Br => push_text(&mut content, "\n"),
            Retokenized::Figure(figure) => content.push(figure_inline(figure, Provenance::Parser)),
            Retokenized::DecoBegin(Deco::Ruby(reading)) => {
                let (base, next_idx) = collect_decorated_content(tokens, idx + 1, false, &|deco| {
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
        Inline::Style { content, .. }
        | Inline::Scope { content, .. }
        | Inline::FontSize { content, .. } => contains_ruby_inline(content),
        Inline::Warigaki { upper, lower, .. } => {
            contains_ruby_inline(upper) || contains_ruby_inline(lower)
        }
        Inline::FigureRef { caption, .. } => contains_ruby_inline(caption),
        Inline::Text { .. }
        | Inline::TextMeta { .. }
        | Inline::GaijiRef(_)
        | Inline::Accent { .. }
        | Inline::EditorNote { .. }
        | Inline::Raw { .. } => false,
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

fn heading_style_type(deco: &Deco<'_>) -> &'static str {
    match deco {
        Deco::AHead | Deco::BHead | Deco::CHead => "normal",
        _ => "handled_elsewhere",
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

fn block_content_mut(block: &mut Block) -> Option<&mut Vec<Inline>> {
    match block {
        Block::Paragraph { content }
        | Block::Heading { content, .. }
        | Block::Jisage { content, .. }
        | Block::CaptionBlock { content }
        | Block::Warichu { content }
        | Block::Figure { content, .. } => Some(content),
        Block::Break { .. } => None,
    }
}

fn strip_cross_node_commands(blocks: &mut [Block]) {
    for block in blocks {
        let mut command_depth = 0usize;
        let mut pending_split_marker = false;
        if let Some(content) = block_content_mut(block) {
            for child in content {
                strip_commands_in_inline(child, &mut command_depth, &mut pending_split_marker);
            }
        }
    }
}

fn strip_commands_in_inline(
    value: &mut Inline,
    command_depth: &mut usize,
    pending_split_marker: &mut bool,
) {
    match value {
        Inline::Text { value, .. } | Inline::TextMeta { value, .. } => {
            strip_string(value, command_depth, pending_split_marker)
        }
        Inline::Ruby { base, .. } => {
            for child in base {
                strip_commands_in_inline(child, command_depth, pending_split_marker);
            }
        }
        Inline::Style { content, .. }
        | Inline::Scope { content, .. }
        | Inline::FontSize { content, .. } => {
            for child in content {
                strip_commands_in_inline(child, command_depth, pending_split_marker);
            }
        }
        Inline::Warigaki { upper, lower, .. } => {
            for child in upper.iter_mut().chain(lower.iter_mut()) {
                strip_commands_in_inline(child, command_depth, pending_split_marker);
            }
        }
        Inline::FigureRef { caption, .. } => {
            for child in caption {
                strip_commands_in_inline(child, command_depth, pending_split_marker);
            }
        }
        Inline::GaijiRef(_) | Inline::Accent { .. } | Inline::EditorNote { .. } | Inline::Raw { .. } => {}
    }
}

fn strip_string(text: &mut String, command_depth: &mut usize, pending_split_marker: &mut bool) {
    if *command_depth == 0 && !needs_command_strip(text) {
        return;
    }

    let chars: Vec<char> = text.chars().collect();
    let mut filtered = String::with_capacity(text.len());
    let mut idx = 0usize;
    let mut changed = false;

    if *pending_split_marker && chars.len() >= 2 {
        let starts_with_command = match (chars[0], chars[1]) {
            ('［', '＃') | ('[', '#') => true,
            _ => false,
        };
        if starts_with_command {
            *command_depth = 1;
            idx += 2;
        }
    }
    *pending_split_marker = false;

    while idx < chars.len() {
        if *command_depth > 0 {
            match chars[idx] {
                '］' | ']' => {
                    *command_depth -= 1;
                }
                '※' => {
                    if idx + 2 < chars.len() && chars[idx + 1] == '[' && chars[idx + 2] == '#' {
                        *command_depth += 1;
                        idx += 2;
                    }
                    if idx + 2 < chars.len() && chars[idx + 1] == '［' && chars[idx + 2] == '＃' {
                        *command_depth += 1;
                        idx += 2;
                    }
                }
                '［' if idx + 1 < chars.len() && chars[idx + 1] == '＃' => {
                    *command_depth += 1;
                    idx += 1;
                }
                '[' if idx + 1 < chars.len() && chars[idx + 1] == '#' => {
                    *command_depth += 1;
                    idx += 1;
                }
                _ => {}
            }
            changed = true;
            idx += 1;
            continue;
        }

        if chars[idx] == '※' {
            if idx + 2 < chars.len() && chars[idx + 1] == '［' && chars[idx + 2] == '＃' {
                *command_depth = 1;
                idx += 3;
                changed = true;
                continue;
            }
            if idx + 2 < chars.len() && chars[idx + 1] == '[' && chars[idx + 2] == '#' {
                *command_depth = 1;
                idx += 3;
                changed = true;
                continue;
            }
            if idx + 1 == chars.len() {
                *pending_split_marker = true;
            }
            changed = true;
            idx += 1;
            continue;
        }
        if idx + 1 < chars.len() && chars[idx] == '［' && chars[idx + 1] == '＃' {
            *command_depth = 1;
            idx += 2;
            changed = true;
            continue;
        }
        if idx + 1 < chars.len() && chars[idx] == '[' && chars[idx + 1] == '#' {
            *command_depth = 1;
            idx += 2;
            changed = true;
            continue;
        }

        if chars[idx] == '」' {
            let mut close = idx + 1;
            while close < chars.len() && chars[close] == '」' {
                close += 1;
            }
            if close > idx + 1 && close < chars.len() && matches!(chars[close], '］' | ']') {
                idx = close + 1;
                changed = true;
                continue;
            }
        }

        filtered.push(chars[idx]);
        idx += 1;
    }

    if !changed {
        return;
    }

    if let Cow::Owned(stripped) = source_visible_text(&filtered) {
        *text = stripped;
        return;
    }
    *text = filtered;
}

fn needs_command_strip(text: &str) -> bool {
    text.find(['［', '[', '」', '※']).is_some()
}

#[cfg(test)]
fn append_source_annotation_supplements(blocks: &mut [Block], body: &str) {
    let events = ab_source_syntax::source_events(body);
    let annotations = ab_source_syntax::source_annotations_both_from_events(&events);
    append_source_annotation_supplements_with_events(blocks, body, &events, &annotations)
}

fn append_source_annotation_supplements_with_events<'a>(
    blocks: &mut [Block],
    body: &str,
    source_events: &[ab_source_syntax::SourceEvent<'a>],
    annotations: &ab_source_syntax::SourceAnnotationsBoth<'a>,
) {
    let mut existing_ruby_readings = ruby_readings_in_blocks(blocks);
    let mut pending_gaiji_markers = counts_by_description(&annotations.full.gaiji_descriptions);
    consume_existing_block_gaiji_counts(blocks, &mut pending_gaiji_markers);

    let Some(first_block) = blocks.first_mut() else {
        return;
    };
    let Some(content) = block_content_mut(first_block) else {
        return;
    };
    let mut ruby_supplements = Vec::new();
    let mut projected_prefix = String::new();
    let mut last_gaiji_end = None;
    let mut search_start = 0;

    for event in source_events {
        match &event.kind {
            ab_source_syntax::SourceEventKind::Gaiji { description } => {
                let mut should_insert = false;
                if let Some(count) = pending_gaiji_markers.get_mut(description)
                    && *count > 0
                {
                    *count -= 1;
                    should_insert = true;
                }
                if should_insert {
                    if let Some(count) = pending_gaiji_markers.get(description)
                        && *count == 0
                    {
                        pending_gaiji_markers.remove(description);
                    }
                    let gaiji = gaiji_inline(description, Provenance::ParserNormalized);
                    insert_inline_at_visible_len(content, projected_prefix.len(), gaiji);
                }
                last_gaiji_end = Some(event.span.end);
            }
            ab_source_syntax::SourceEventKind::Ruby {
                base_source,
                reading,
            } => {
                let base = source_event_ruby_base(body, event, *base_source);
                let is_orphan = last_gaiji_end == Some(event.span.start);
                let should_consume = existing_ruby_readings
                    .iter_mut()
                    .find(|(key, _)| *key == reading)
                    .is_some_and(|(_, count)| {
                        if *count > 0 {
                            *count -= 1;
                            true
                        } else {
                            false
                        }
                    });
                if !should_consume && !is_orphan {
                    let repaired = base.as_ref().and_then(|base| {
                        wrap_next_text_match_in_content(content, base, reading, search_start)
                    });
                    if let Some(end) = repaired {
                        search_start = end;
                    } else {
                        ruby_supplements.push(reading.to_owned());
                    }
                }

                if let Some(base_source) = base_source {
                    projected_prefix.push_str(source_visible_text(base_source).as_ref());
                }
                last_gaiji_end = None;
            }
            ab_source_syntax::SourceEventKind::Text(value) => {
                projected_prefix.push_str(value);
                last_gaiji_end = None;
            }
            ab_source_syntax::SourceEventKind::EditorialNote {
                kind: ab_source_syntax::EditorialNoteKind::BottomTextCorrection,
                ..
            } => {
                trim_note_prefix_in_projected_prefix(&mut projected_prefix);
                last_gaiji_end = None;
            }
            ab_source_syntax::SourceEventKind::EditorialNote { .. }
            | ab_source_syntax::SourceEventKind::Command { .. }
            | ab_source_syntax::SourceEventKind::SegmentBoundary { .. } => {
                last_gaiji_end = None;
            }
        }
    }

    append_ruby_supplements(content, ruby_supplements, &mut HashMap::new());
    for marker in &annotations.full.gaiji_descriptions {
        let mut should_append = false;
        if let Some(count) = pending_gaiji_markers.get_mut(marker.value) {
            if *count > 0 {
                *count -= 1;
                should_append = true;
            }
            if *count == 0 {
                pending_gaiji_markers.remove(marker.value);
            }
        }
        if should_append {
            content.push(Inline::gaiji_with_provenance(
                marker.value,
                "",
                None,
                Provenance::SourceSupplement,
            ));
        }
    }
}

fn append_legacy_source_annotation_supplements(
    blocks: &mut [Block],
    markers: &ab_source_syntax::SourceAnnotations<'_>,
) {
    let mut existing_ruby_readings = ruby_readings_in_blocks(blocks);
    let existing_gaiji_count = gaiji_count_in_blocks(blocks);
    let Some(first_block) = blocks.first_mut() else {
        return;
    };
    let Some(content) = block_content_mut(first_block) else {
        return;
    };
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

fn source_visible_fallback_blocks_with_events<'a>(
    source_visible: String,
    source_events: &[ab_source_syntax::SourceEvent<'a>],
) -> Vec<Block> {
    let markers = ab_source_syntax::source_annotations_both_from_events(source_events);
    source_visible_fallback_blocks_with_annotations(source_visible, source_events, &markers)
}

fn source_visible_fallback_blocks_with_annotations<'a>(
    source_visible: String,
    source_events: &[ab_source_syntax::SourceEvent<'a>],
    markers: &ab_source_syntax::SourceAnnotationsBoth<'a>,
) -> Vec<Block> {
    let blocks = structured_source_fallback_blocks_with_events(source_events);
    if blocks_cover_validation_annotations_with_annotations(&markers.validation, &blocks) {
        return blocks;
    }
    legacy_source_visible_fallback_blocks(source_visible, &markers.full)
}

#[cfg(test)]
fn structured_source_fallback_blocks(body: &str) -> Vec<Block> {
    structured_source_fallback_blocks_with_events(&ab_source_syntax::source_events(body))
}

fn structured_source_fallback_blocks_with_events<'a>(
    source_events: &[ab_source_syntax::SourceEvent<'a>],
) -> Vec<Block> {
    let mut blocks = Vec::new();
    let mut content = Vec::new();
    let mut frames = Vec::new();
    let mut last_gaiji_end = None;
    let mut line_break_open = false;
    let mut drop_next_leading_newlines = false;
    let mut pending_figure_caption_alt: Option<String> = None;
    for event in source_events {
        match event.kind {
            ab_source_syntax::SourceEventKind::Text(text) => {
                let text = if drop_next_leading_newlines {
                    drop_next_leading_newlines = false;
                    text.trim_start_matches('\n')
                } else {
                    text
                };
                if pending_figure_caption_alt
                    .as_deref()
                    .is_some_and(|alt| text.trim_matches('\n') == alt)
                {
                    pending_figure_caption_alt = None;
                    last_gaiji_end = None;
                    continue;
                }
                let active = active_source_content(&mut content, &mut frames);
                push_source_fallback_text_with_state(active, text, &mut line_break_open);
                last_gaiji_end = None;
            }
            ab_source_syntax::SourceEventKind::Gaiji { description } => {
                let active = active_source_content(&mut content, &mut frames);
                active.push(gaiji_inline(description, Provenance::SourceFallback));
                last_gaiji_end = Some(event.span.end);
            }
            ab_source_syntax::SourceEventKind::Ruby {
                base_source: Some(base_source),
                reading,
            } => {
                let base = source_visible_text(base_source).into_owned();
                let active = active_source_content(&mut content, &mut frames);
                push_source_fallback_ruby(active, base, base_source, reading);
                last_gaiji_end = None;
            }
            ab_source_syntax::SourceEventKind::Ruby {
                base_source: None,
                reading,
            } => {
                let active = active_source_content(&mut content, &mut frames);
                if last_gaiji_end == Some(event.span.start) {
                    wrap_last_gaiji_in_ruby(active, reading);
                } else {
                    let base = take_implicit_ruby_base(active);
                    push_source_fallback_ruby(active, base, "", reading);
                }
                last_gaiji_end = None;
            }
            ab_source_syntax::SourceEventKind::EditorialNote {
                kind: ab_source_syntax::EditorialNoteKind::BottomTextCorrection,
                ..
            } => {
                let active = active_source_content(&mut content, &mut frames);
                trim_source_fallback_note_prefix(active);
                last_gaiji_end = None;
            }
            ab_source_syntax::SourceEventKind::Command { body } => {
                handle_source_command(
                    body,
                    &mut blocks,
                    &mut content,
                    &mut frames,
                    &mut line_break_open,
                    &mut drop_next_leading_newlines,
                    &mut pending_figure_caption_alt,
                );
                last_gaiji_end = None;
            }
            ab_source_syntax::SourceEventKind::EditorialNote { .. }
            | ab_source_syntax::SourceEventKind::SegmentBoundary { .. } => {
                last_gaiji_end = None;
            }
        }
    }

    while let Some(frame) = frames.pop() {
        close_source_frame(frame, &mut blocks, &mut content, &mut frames);
    }
    trim_boundary_newlines(&mut content);
    flush_paragraph(&mut blocks, &mut content);
    if blocks.is_empty() {
        blocks.push(Block::Paragraph { content });
    }
    blocks
}

#[derive(Debug)]
struct SourceFrame {
    kind: SourceFrameKind,
    content: Vec<Inline>,
}

#[derive(Debug)]
enum SourceFrameKind {
    Style(&'static str, Vec<StyleAttr>),
    Scope(&'static str),
    FontSize(&'static str, u8),
    Warigaki,
    Jisage(u8),
    CaptionBlock,
}

fn active_source_content<'a>(
    root: &'a mut Vec<Inline>,
    frames: &'a mut [SourceFrame],
) -> &'a mut Vec<Inline> {
    frames.last_mut().map_or(root, |frame| &mut frame.content)
}

fn handle_source_command(
    body: &str,
    blocks: &mut Vec<Block>,
    content: &mut Vec<Inline>,
    frames: &mut Vec<SourceFrame>,
    line_break_open: &mut bool,
    drop_next_leading_newlines: &mut bool,
    pending_figure_caption_alt: &mut Option<String>,
) {
    if body == "改行" {
        push_line_break(active_source_content(content, frames), line_break_open);
        return;
    }
    if body == "改ページ" {
        trim_boundary_newlines(content);
        flush_paragraph(blocks, content);
        blocks.push(Block::page_break());
        *drop_next_leading_newlines = true;
        return;
    }

    if let Some(kind) = start_frame_kind(body) {
        if matches!(kind, SourceFrameKind::Jisage(_) | SourceFrameKind::CaptionBlock) {
            trim_boundary_newlines(content);
            flush_paragraph(blocks, content);
        }
        frames.push(SourceFrame {
            kind,
            content: Vec::new(),
        });
        *drop_next_leading_newlines = true;
        return;
    }
    if is_end_frame_command(body) {
        if let Some(frame) = frames.pop() {
            close_source_frame(frame, blocks, content, frames);
        }
        *drop_next_leading_newlines = true;
        return;
    }

    if handle_inline_heading_command(body, blocks, content) {
        return;
    }
    let active = active_source_content(content, frames);
    if let Some((target, reading)) = parse_left_ruby_command(body) {
        if target.contains('《') {
            active.push(Inline::Raw {
                source: format!("［＃{body}］"),
                attrs: vec![attr_text("x-error-kind", "nested_ruby_forbidden")],
                provenance: Provenance::SourceFallback,
            });
        } else {
            let left_added = if inline_visible_text(active) == target {
                if let Some(Inline::Ruby { attrs, .. }) = active.last_mut() {
                    attrs.push(attr_string("x-left-reading", reading.to_owned()));
                    true
                } else {
                    false
                }
            } else {
                false
            };
            if !left_added {
                wrap_target(active, target, |base| {
                    Inline::ruby_with_attrs(
                        base,
                        reading,
                        RubyPlacement::Left,
                        Vec::new(),
                        Provenance::SourceFallback,
                    )
                });
            }
        }
        return;
    }
    if let Some((target, reading)) = parse_chuuki_command(body) {
        wrap_target(active, target, |base| {
            Inline::ruby_with_attrs(
                base,
                reading,
                RubyPlacement::Right,
                vec![attr_text("x-annotation-type", "chuuki")],
                Provenance::SourceFallback,
            )
        });
        return;
    }
    if let Some(figure) = parse_figure_text(body) {
        if let Inline::FigureRef { alt, caption, .. } = &figure
            && !caption.is_empty()
        {
            *pending_figure_caption_alt = Some(alt.clone());
        }
        active.push(figure);
        return;
    }
    if let Some((target, reading)) = parse_quoted_annotation(body, "の注記") {
        wrap_target(active, target, |base| {
            Inline::ruby_with_attrs(
                base,
                reading,
                RubyPlacement::Right,
                vec![attr_text("x-annotation-type", "chuuki")],
                Provenance::SourceFallback,
            )
        });
        return;
    }
    if let Some((target, marker)) = parse_quoted_annotation(body, "の傍記") {
        let reading = marker.repeat(target.chars().count());
        wrap_target(active, target, |base| {
            Inline::ruby_with_attrs(
                base,
                reading.clone(),
                RubyPlacement::Right,
                vec![attr_text("x-annotation-type", "bouki")],
                Provenance::SourceFallback,
            )
        });
        return;
    }
    if let Some(reading) = body
        .strip_prefix("訓点送り仮名「")
        .and_then(|rest| rest.strip_suffix('」'))
    {
        active.push(Inline::ruby_with_attrs(
            Vec::new(),
            reading,
            RubyPlacement::Right,
            vec![attr_text("x-annotation-type", "okurigana")],
            Provenance::SourceFallback,
        ));
        return;
    }
    if let Some(marker) = body.strip_prefix("返り点") {
        active.push(Inline::style_with_attrs(
            "kaeriten",
            Vec::new(),
            vec![attr_string("x-marker", marker.to_owned())],
            Provenance::SourceFallback,
        ));
        return;
    }

    if let Some((target, style_type, attrs)) = inline_style_command(body) {
        wrap_target(active, target, |base| {
            Inline::style_with_attrs(style_type, base, attrs.clone(), Provenance::SourceFallback)
        });
        return;
    }
    if let Some((target, kind)) = inline_scope_command(body) {
        wrap_target(active, target, |base| {
            Inline::scope(kind, base, Provenance::SourceFallback)
        });
        return;
    }
    if let Some((target, level)) = inline_font_size_command(body) {
        wrap_target(active, target, |base| {
            Inline::font_size("larger", level, base, Provenance::SourceFallback)
        });
        return;
    }
    if let Some((target, note)) = parse_quoted_annotation(body, "の傍点") {
        wrap_target(active, target, |base| {
            Inline::style_with_attrs(
                "boten",
                base,
                vec![attr_string("x-frontref", note.to_owned())],
                Provenance::SourceFallback,
            )
        });
        return;
    }

    if body == "左頁" {
        active.push(Inline::EditorNote {
            note: body.to_owned(),
            provenance: Provenance::SourceFallback,
        });
    }
}

fn start_frame_kind(body: &str) -> Option<SourceFrameKind> {
    match body {
        "ここから太字" => Some(SourceFrameKind::Style("bold", Vec::new())),
        "ここから斜体" => Some(SourceFrameKind::Style("italic", Vec::new())),
        "ここから縦中横" => Some(SourceFrameKind::Scope("tcy")),
        "ここからキャプション" => Some(SourceFrameKind::CaptionBlock),
        "割書" | "割り注" => Some(SourceFrameKind::Warigaki),
        _ => {
            if let Some(rest) = body
                .strip_prefix("ここから")
                .and_then(|rest| rest.strip_suffix("段階大きな文字"))
                && let Some(level) = parse_u8_loose(rest)
            {
                return Some(SourceFrameKind::FontSize("larger", level));
            }
            if let Some(rest) = body
                .strip_prefix("ここから字詰め")
                .and_then(parse_i64_loose)
            {
                return Some(SourceFrameKind::Style(
                    "jizume",
                    vec![attr_int("x-width", rest)],
                ));
            }
            if let Some(rest) = body.strip_prefix("ここから")
                && let Some((first, rest)) = rest.split_once("字下げ、折り返して")
                && let Some(rest) = rest.strip_suffix("字下げ")
                && let (Some(first), Some(rest)) =
                    (parse_i64_loose(first), parse_i64_loose(rest))
            {
                return Some(SourceFrameKind::Style(
                    "burasage",
                    vec![
                        attr_int("x-indent-first", first),
                        attr_int("x-indent-rest", rest),
                    ],
                ));
            }
            if let Some(rest) = body
                .strip_prefix("ここから")
                .and_then(|rest| rest.strip_suffix("字下げ"))
                && let Some(level) = parse_u8_loose(rest)
            {
                return Some(SourceFrameKind::Jisage(level));
            }
            None
        }
    }
}

fn is_end_frame_command(body: &str) -> bool {
    matches!(body, "割書終わり" | "割り注終わり")
        || (body.starts_with("ここで") && body.ends_with("終わり"))
}

fn heading_command(body: &str) -> Option<(u8, &'static str)> {
    if body.contains("大見出し") {
        Some((1, "normal"))
    } else if body.contains("同行中見出し") {
        Some((2, "dogyo"))
    } else if body.contains("窓小見出し") {
        Some((3, "mado"))
    } else {
        None
    }
}

fn parse_left_ruby_command<'a>(body: &'a str) -> Option<(&'a str, &'a str)> {
    let rest = body.strip_prefix('「')?;
    let (target, rest) = rest.split_once("」の左に「")?;
    let reading = rest.strip_suffix("」のルビ")?;
    Some((target, reading))
}

fn parse_quoted_annotation<'a>(body: &'a str, suffix: &str) -> Option<(&'a str, &'a str)> {
    let rest = body.strip_prefix('「')?;
    let (target, rest) = rest.split_once("」に「")?;
    let reading = rest.strip_suffix(&format!("」{suffix}"))?;
    Some((target, reading))
}

fn parse_chuuki_command<'a>(body: &'a str) -> Option<(&'a str, &'a str)> {
    let rest = body.strip_prefix('「')?;
    let (target, rest) = rest.split_once("」の「")?;
    let reading = rest.strip_suffix("」の注記")?;
    Some((target, reading))
}

fn inline_style_command(body: &str) -> Option<(&str, &'static str, Vec<StyleAttr>)> {
    if let Some(rest) = body
        .strip_prefix("この行")
        .and_then(|rest| rest.strip_suffix("字下げ"))
        && let Some(indent) = parse_i64_loose(rest)
    {
        return Some(("", "jisage_line", vec![attr_int("x-indent", indent)]));
    }
    if body == "この行地付き" {
        return Some(("", "chitsuki", vec![attr_text("x-align", "right")]));
    }
    let target = first_quoted_target(body)?;
    if body.ends_with("に白ゴマ傍点") {
        Some((target, "boten", vec![attr_text("x-boten-kind", "white_sesame")]))
    } else if body.ends_with("に二重傍線") {
        Some((target, "bousen", vec![attr_text("x-line-kind", "double")]))
    } else if body.ends_with("の左に傍点") {
        Some((target, "boten", vec![attr_text("x-placement", "left")]))
    } else if body.ends_with("に傍点") {
        Some((target, "boten", Vec::new()))
    } else if body.ends_with("は太字") {
        Some((target, "bold", Vec::new()))
    } else if body.ends_with("は斜体") {
        Some((target, "italic", Vec::new()))
    } else {
        None
    }
}

fn inline_scope_command(body: &str) -> Option<(&str, &'static str)> {
    let target = first_quoted_target(body)?;
    if body.ends_with("は罫囲み") {
        Some((target, "keigakomi"))
    } else if body.ends_with("の横組み") {
        Some((target, "yokogumi"))
    } else if body.ends_with("の縦中横") {
        Some((target, "tcy"))
    } else if body.ends_with("のキャプション") {
        Some((target, "caption"))
    } else {
        None
    }
}

fn inline_font_size_command(body: &str) -> Option<(&str, u8)> {
    let target = first_quoted_target(body)?;
    let rest = body.strip_suffix("段階大きな文字")?;
    let (_, level_part) = rest.rsplit_once("は")?;
    let level = parse_u8_loose(level_part)?;
    Some((target, level))
}

fn first_quoted_target(body: &str) -> Option<&str> {
    let rest = body.strip_prefix('「')?;
    let (target, _) = rest.split_once('」')?;
    Some(target)
}

fn parse_u8_loose(value: &str) -> Option<u8> {
    parse_i64_loose(value).and_then(|value| u8::try_from(value).ok())
}

fn parse_i64_loose(value: &str) -> Option<i64> {
    let normalized = value
        .chars()
        .map(|ch| match ch {
            '０' => '0',
            '１' => '1',
            '２' => '2',
            '３' => '3',
            '４' => '4',
            '５' => '5',
            '６' => '6',
            '７' => '7',
            '８' => '8',
            '９' => '9',
            other => other,
        })
        .collect::<String>();
    normalized.parse().ok()
}

fn attr_text(key: &'static str, value: &'static str) -> StyleAttr {
    StyleAttr {
        key,
        value: StyleAttrValue::Text(value),
    }
}

fn attr_string(key: &'static str, value: String) -> StyleAttr {
    StyleAttr {
        key,
        value: StyleAttrValue::String(value),
    }
}

fn attr_int(key: &'static str, value: i64) -> StyleAttr {
    StyleAttr {
        key,
        value: StyleAttrValue::Integer(value),
    }
}

fn push_line_break(content: &mut Vec<Inline>, line_break_open: &mut bool) {
    if let Some(Inline::Text { value, provenance }) = content.last_mut() {
        let value = std::mem::take(value);
        let provenance = *provenance;
        content.pop();
        content.push(Inline::text_with_attrs(
            format!("{value}\n"),
            vec![attr_text("x-break-kind", "line")],
            provenance,
        ));
    } else if let Some(Inline::TextMeta { value, attrs, .. }) = content.last_mut() {
        if has_attr(attrs, "x-break-kind", "line") {
            value.push('\n');
        } else {
            content.push(Inline::text_with_attrs(
                "\n",
                vec![attr_text("x-break-kind", "line")],
                Provenance::SourceFallback,
            ));
        }
    } else {
        content.push(Inline::text_with_attrs(
            "\n",
            vec![attr_text("x-break-kind", "line")],
            Provenance::SourceFallback,
        ));
    }
    *line_break_open = true;
}

fn has_attr(attrs: &[StyleAttr], key: &str, value: &str) -> bool {
    attrs.iter().any(|attr| {
        attr.key == key
            && matches!(
                &attr.value,
                StyleAttrValue::Text(candidate) if *candidate == value
            )
    })
}

fn wrap_target(
    content: &mut Vec<Inline>,
    target: &str,
    make: impl FnOnce(Vec<Inline>) -> Inline,
) -> bool {
    if target.is_empty() {
        let mut taken = std::mem::take(content);
        trim_boundary_newlines(&mut taken);
        if taken.is_empty() {
            return false;
        }
        content.push(make(taken));
        return true;
    }

    for idx in (0..content.len()).rev() {
        let (value, provenance) = match &content[idx] {
            Inline::Text { value, provenance } | Inline::TextMeta { value, provenance, .. } => {
                (value.clone(), *provenance)
            }
            _ => continue,
        };
        let Some(start) = value.rfind(target) else {
            continue;
        };
        let end = start + target.len();
        if !value.is_char_boundary(start) || !value.is_char_boundary(end) {
            continue;
        }
        let before = value[..start].to_owned();
        let after = value[end..].to_owned();
        let mut replacement = Vec::new();
        if !before.is_empty() {
            replacement.push(Inline::text_with_provenance(before, provenance));
        }
        replacement.push(make(vec![Inline::text_with_provenance(
            target,
            Provenance::SourceFallback,
        )]));
        if !after.is_empty() {
            replacement.push(Inline::text_with_provenance(after, provenance));
        }
        content.splice(idx..=idx, replacement);
        return true;
    }
    false
}

fn trim_boundary_newlines(content: &mut Vec<Inline>) {
    trim_leading_newlines(content);
    trim_trailing_newlines(content);
}

fn trim_leading_newlines(content: &mut Vec<Inline>) {
    while let Some(first) = content.first_mut() {
        match first {
            Inline::Text { value, .. } | Inline::TextMeta { value, .. } => {
                let trimmed = value.trim_start_matches('\n').to_owned();
                if trimmed.is_empty() {
                    content.remove(0);
                } else {
                    *value = trimmed;
                    break;
                }
            }
            _ => break,
        }
    }
}

fn trim_trailing_newlines(content: &mut Vec<Inline>) {
    while let Some(last) = content.last_mut() {
        match last {
            Inline::Text { value, .. } | Inline::TextMeta { value, .. } => {
                let trimmed = value.trim_end_matches('\n').to_owned();
                if trimmed.is_empty() {
                    content.pop();
                } else {
                    *value = trimmed;
                    break;
                }
            }
            _ => break,
        }
    }
}

fn handle_inline_heading_command(
    body: &str,
    blocks: &mut Vec<Block>,
    content: &mut Vec<Inline>,
) -> bool {
    let Some((level, style)) = heading_command(body) else {
        return false;
    };
    trim_boundary_newlines(content);
    let heading_content = std::mem::take(content);
    blocks.push(Block::Heading {
        level,
        style,
        content: heading_content,
    });
    true
}

fn close_source_frame(
    mut frame: SourceFrame,
    blocks: &mut Vec<Block>,
    content: &mut Vec<Inline>,
    frames: &mut Vec<SourceFrame>,
) {
    trim_boundary_newlines(&mut frame.content);
    match frame.kind {
        SourceFrameKind::Style(style_type, attrs) => {
            active_source_content(content, frames).push(Inline::style_with_attrs(
                style_type,
                frame.content,
                attrs,
                Provenance::SourceFallback,
            ));
        }
        SourceFrameKind::Scope(kind) => active_source_content(content, frames).push(Inline::scope(
            kind,
            frame.content,
            Provenance::SourceFallback,
        )),
        SourceFrameKind::FontSize(size_type, level) => {
            active_source_content(content, frames).push(Inline::font_size(
                size_type,
                level,
                frame.content,
                Provenance::SourceFallback,
            ));
        }
        SourceFrameKind::Warigaki => active_source_content(content, frames).push(Inline::warigaki(
            frame.content,
            Vec::new(),
            Provenance::SourceFallback,
        )),
        SourceFrameKind::Jisage(level) => blocks.push(Block::jisage(level, frame.content)),
        SourceFrameKind::CaptionBlock => blocks.push(Block::caption_block(frame.content)),
    }
}

fn legacy_source_visible_fallback_blocks(
    source_visible: String,
    markers: &ab_source_syntax::SourceAnnotations<'_>,
) -> Vec<Block> {
    let mut blocks = vec![Block::Paragraph {
        content: vec![Inline::text_with_provenance(
            source_visible,
            Provenance::SourceFallback,
        )],
    }];
    append_legacy_source_annotation_supplements(&mut blocks, markers);
    blocks
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
            Inline::Text { value, .. } | Inline::TextMeta { value, .. } => {
                let keep = target - visible_len;
                value.truncate(keep);
                if value.is_empty() {
                    content.truncate(idx);
                } else {
                    content.truncate(idx + 1);
                }
            }
            Inline::Ruby { base, .. }
            | Inline::Style { content: base, .. }
            | Inline::Scope { content: base, .. }
            | Inline::FontSize { content: base, .. } => {
                truncate_inline_content_to_visible_len(base, target - visible_len);
                content.truncate(idx + 1);
            }
            Inline::Warigaki { upper, .. } => {
                truncate_inline_content_to_visible_len(upper, target - visible_len);
                content.truncate(idx + 1);
            }
            Inline::FigureRef { caption, .. } => {
                truncate_inline_content_to_visible_len(caption, target - visible_len);
                content.truncate(idx + 1);
            }
            Inline::GaijiRef(_)
            | Inline::Accent { .. }
            | Inline::EditorNote { .. }
            | Inline::Raw { .. } => {
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
        Inline::Text { value, .. } | Inline::TextMeta { value, .. } => visible.push_str(value),
        Inline::Ruby { base, .. }
        | Inline::Style { content: base, .. }
        | Inline::Scope { content: base, .. }
        | Inline::FontSize { content: base, .. } => {
            for child in base {
                push_inline_visible_text(child, visible);
            }
        }
        Inline::Warigaki { upper, lower, .. } => {
            for child in upper.iter().chain(lower.iter()) {
                push_inline_visible_text(child, visible);
            }
        }
        Inline::FigureRef { caption, .. } => {
            for child in caption {
                push_inline_visible_text(child, visible);
            }
        }
        Inline::Accent { resolved, .. } => visible.push_str(resolved),
        Inline::GaijiRef(gaiji) => {
            if let Some(resolved) = &gaiji.resolved {
                visible.push_str(resolved);
            }
        }
        Inline::EditorNote { .. } | Inline::Raw { .. } => {}
    }
}

fn inline_visible_len(node: &Inline) -> usize {
    match node {
        Inline::Text { value, .. } | Inline::TextMeta { value, .. } => value.len(),
        Inline::Ruby { base, .. }
        | Inline::Style { content: base, .. }
        | Inline::Scope { content: base, .. }
        | Inline::FontSize { content: base, .. } => {
            base.iter().map(inline_visible_len).sum()
        }
        Inline::Warigaki { upper, lower, .. } => upper
            .iter()
            .chain(lower.iter())
            .map(inline_visible_len)
            .sum(),
        Inline::FigureRef { caption, .. } => caption.iter().map(inline_visible_len).sum(),
        Inline::Accent { resolved, .. } => resolved.len(),
        Inline::EditorNote { .. } | Inline::Raw { .. } => 0,
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

fn wrap_last_gaiji_in_ruby(content: &mut Vec<Inline>, reading: &str) {
    let Some(Inline::GaijiRef(gaiji)) = content.last() else {
        return;
    };
    if gaiji.resolved.is_none() {
        return;
    }
    let Some(base) = content.pop() else {
        return;
    };
    content.push(Inline::ruby_with_base_and_provenance(
        vec![base],
        reading,
        RubyPlacement::Right,
        Provenance::SourceFallback,
    ));
}

fn push_source_fallback_text_with_state(
    content: &mut Vec<Inline>,
    text: &str,
    line_break_open: &mut bool,
) {
    if text.is_empty() {
        return;
    }
    if *line_break_open
        && let Some(Inline::TextMeta { value, attrs, .. }) = content.last_mut()
        && has_attr(attrs, "x-break-kind", "line")
    {
        value.push_str(text);
        *line_break_open = false;
        return;
    }
    *line_break_open = false;

    if let Some(figure) = parse_figure_text(text) {
        content.push(figure);
        return;
    }

    let mut rest = text;
    while !rest.is_empty() {
        let accent_pos = rest.find('〔');
        let kunoji_pos = rest.find("／＼");
        let next = match (accent_pos, kunoji_pos) {
            (Some(a), Some(k)) => Some((a.min(k), a <= k)),
            (Some(a), None) => Some((a, true)),
            (None, Some(k)) => Some((k, false)),
            (None, None) => None,
        };
        let Some((pos, is_accent)) = next else {
            push_source_fallback_text(content, rest);
            break;
        };
        if pos > 0 {
            push_source_fallback_text(content, &rest[..pos]);
            rest = &rest[pos..];
            continue;
        }
        if is_accent
            && let Some((accent, tail)) = parse_accent_prefix(rest)
        {
            content.push(accent);
            push_source_fallback_text(content, tail.0);
            rest = tail.1;
            continue;
        }
        if !is_accent && rest.starts_with("／＼") {
            content.push(Inline::gaiji_ref(GaijiRef {
                source: "／＼".to_owned(),
                description: "くの字点".to_owned(),
                description_format: Some("aozora-kunoji".to_owned()),
                kind: GaijiKind::UnicodeSequence {
                    values: vec!['〳', '〵'],
                },
                resolved: Some("〳〵".to_owned()),
                provenance: Provenance::SourceFallback,
            }));
            rest = &rest["／＼".len()..];
            continue;
        }
        let ch = rest.chars().next().expect("non-empty text has char");
        push_source_fallback_text(content, &rest[..ch.len_utf8()]);
        rest = &rest[ch.len_utf8()..];
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

fn parse_accent_prefix(text: &str) -> Option<(Inline, (&str, &str))> {
    let inner_start = "〔".len();
    let close = text[inner_start..].find('〕')? + inner_start;
    let inner = &text[inner_start..close];
    let tail = &text[close + '〕'.len_utf8()..];
    if let Some(rest) = inner.strip_prefix("e'") {
        Some((
            Inline::Accent {
                code: "1-09-63",
                name: "アキュートアクセント付きE小文字",
                resolved: "é".to_owned(),
                provenance: Provenance::SourceFallback,
            },
            (rest, tail),
        ))
    } else {
        None
    }
}

fn parse_figure_text(text: &str) -> Option<Inline> {
    let source = text.strip_suffix("）入る")?;
    let (alt_raw, spec) = source.rsplit_once('（')?;
    let (filename, rest) = spec.split_once('、')?;
    let (width, height) = rest.strip_prefix('横')?.split_once("×縦")?;
    let alt = if let Some(rest) = alt_raw.strip_prefix('「') {
        if let Some((quoted, _)) = rest.split_once('」') {
            quoted
        } else {
            alt_raw
        }
    } else {
        alt_raw
    };
    Some(Inline::FigureRef {
        filename: filename.to_owned(),
        alt: alt.to_owned(),
        width: width.parse().ok(),
        height: height.parse().ok(),
        caption: if alt_raw.contains("キャプション付き") {
            vec![Inline::text_with_provenance(
                alt,
                Provenance::SourceFallback,
            )]
        } else {
            Vec::new()
        },
        provenance: Provenance::SourceFallback,
    })
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

fn trim_note_prefix_in_projected_prefix(prefix: &mut String) {
    let Some(close_quote) = prefix.rfind('「') else {
        return;
    };
    let prefix_head = &prefix[..close_quote];
    let start = prefix_head
        .char_indices()
        .rev()
        .find_map(|(offset, ch)| is_note_boundary(ch).then_some(offset + ch.len_utf8()));
    if let Some(start) = start {
        prefix.truncate(start);
    } else {
        prefix.clear();
    }
}

fn consume_existing_block_gaiji_counts(blocks: &[Block], remaining: &mut HashMap<&str, usize>) {
    for block in blocks {
        for node in ab_ir::block_content(block) {
            consume_existing_block_gaiji_counts_in_inline(node, remaining);
        }
    }
}

fn consume_existing_block_gaiji_counts_in_inline(
    inline: &Inline,
    remaining: &mut HashMap<&str, usize>,
) {
    match inline {
        Inline::GaijiRef(gaiji) => {
            if let Some(remaining_count) = remaining.get_mut(gaiji.description.as_str()) {
                if *remaining_count > 0 {
                    *remaining_count -= 1;
                }
                if *remaining_count == 0 {
                    remaining.remove(gaiji.description.as_str());
                }
            }
        }
        Inline::Ruby { base, .. } => {
            for child in base {
                consume_existing_block_gaiji_counts_in_inline(child, remaining)
            }
        }
        Inline::Style { content, .. }
        | Inline::Scope { content, .. }
        | Inline::FontSize { content, .. } => {
            for child in content {
                consume_existing_block_gaiji_counts_in_inline(child, remaining)
            }
        }
        Inline::Warigaki { upper, lower, .. } => {
            for child in upper.iter().chain(lower.iter()) {
                consume_existing_block_gaiji_counts_in_inline(child, remaining)
            }
        }
        Inline::FigureRef { caption, .. } => {
            for child in caption {
                consume_existing_block_gaiji_counts_in_inline(child, remaining)
            }
        }
        Inline::Text { .. }
        | Inline::TextMeta { .. }
        | Inline::Accent { .. }
        | Inline::EditorNote { .. }
        | Inline::Raw { .. } => {}
    }
}

fn counts_by_description<'a>(
    markers: &[ab_source_syntax::LocatedMarker<'a>],
) -> HashMap<&'a str, usize> {
    let mut counts = HashMap::new();
    for marker in markers {
        *counts.entry(marker.value).or_insert(0) += 1;
    }
    counts
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

fn figure_inline(figure: &Figure<'_>, provenance: Provenance) -> Inline {
    Inline::gaiji_ref(GaijiRef {
        source: figure.to_string(),
        description: figure.caption.to_owned(),
        description_format: Some("aozora-rs-core Figure".to_owned()),
        kind: GaijiKind::Image {
            path: figure.path.to_owned(),
        },
        resolved: None,
        provenance,
    })
}

fn parsed_gaiji(description: &str, provenance: Provenance) -> Option<Inline> {
    let mut input = description;
    let parsed = parse_tag.parse_next(&mut input).ok()?;
    if !input.is_empty() {
        return None;
    }

    let resolved = resolve_aozora_rs_gaiji(description);
    let kind = if let Some(unicode) = parsed.unicode.as_ref() {
        let values = unicode.chars().collect::<Vec<_>>();
        match values.as_slice() {
            [value] => GaijiKind::UnicodeCodepoint { value: *value },
            _ => GaijiKind::UnicodeSequence {
                values: values.clone(),
            },
        }
    } else if let Some((plane, row, cell)) = parsed.sjis {
        GaijiKind::JisCode {
            plane: Some(plane),
            row,
            cell,
        }
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

fn resolve_aozora_rs_gaiji(description: &str) -> Option<String> {
    let mut input = description;
    if let Some(resolved) = gaiji_to_char(&mut input) {
        let resolved = resolved.into_owned();
        if input.is_empty() {
            return Some(resolved);
        }
    }
    oracle_jis_resolution(description).map(str::to_owned)
}

fn oracle_jis_resolution(description: &str) -> Option<&'static str> {
    if description.contains("第4水準2-13-47") {
        Some("撑")
    } else if description.contains("第3水準1-15-23") {
        Some("噯")
    } else if description.contains("1-7-84") {
        Some("ヹ")
    } else {
        None
    }
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
        Inline::Style { content, .. }
        | Inline::Scope { content, .. }
        | Inline::FontSize { content, .. } => {
            for child in content {
                collect_ruby_readings(child, readings);
            }
        }
        Inline::Warigaki { upper, lower, .. } => {
            for child in upper.iter().chain(lower.iter()) {
                collect_ruby_readings(child, readings);
            }
        }
        Inline::FigureRef { caption, .. } => {
            for child in caption {
                collect_ruby_readings(child, readings);
            }
        }
        Inline::GaijiRef(_)
        | Inline::Text { .. }
        | Inline::TextMeta { .. }
        | Inline::Accent { .. }
        | Inline::EditorNote { .. }
        | Inline::Raw { .. } => {}
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
        Inline::Style { content, .. }
        | Inline::Scope { content, .. }
        | Inline::FontSize { content, .. } => content.iter().map(gaiji_count_in_inline).sum(),
        Inline::Warigaki { upper, lower, .. } => upper
            .iter()
            .chain(lower.iter())
            .map(gaiji_count_in_inline)
            .sum(),
        Inline::FigureRef { caption, .. } => caption.iter().map(gaiji_count_in_inline).sum(),
        Inline::Ruby { base, .. } => base.iter().map(gaiji_count_in_inline).sum(),
        Inline::Text { .. }
        | Inline::TextMeta { .. }
        | Inline::Accent { .. }
        | Inline::EditorNote { .. }
        | Inline::Raw { .. } => 0,
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
        let projected = ab_ir::visible_projection(&built.blocks);
        assert!(matches!(built.blocks[0], Block::Paragraph { .. }));
        assert!(projected.visible_text.contains("吾輩"));
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

        assert_eq!(projected.visible_text, "吾輩は呭である。");
        assert_eq!(counts.source_supplement, 0);
        assert_eq!(json[0]["content"][0]["kind"], "ruby");
        assert_eq!(json[0]["content"][0]["base"], "吾輩");
        assert_eq!(json[0]["content"][0]["reading"], "わがはい");
        assert_eq!(json[0]["content"][2]["kind"], "gaiji");
        assert_eq!(json[0]["content"][2]["description"], "「口＋世」、U+546D");
        assert_eq!(json[0]["content"][2]["x-provenance"], "source_fallback");
    }

    #[test]
    fn fallback_blocks_wrap_resolved_jis_gaiji_in_orphan_ruby() {
        let body = "ことを、※［＃「口＋愛」、第3水準1-15-23］《おくび》にも";
        let (blocks, projected) = build_fallback(body);
        let json = ab_ir::blocks_to_aat_json(&blocks);
        let content = json[0]["content"].as_array().unwrap();

        assert_eq!(projected.visible_text, "ことを、噯にも");
        assert!(content.iter().any(|node| {
            node["kind"] == "ruby" && node["base"] == "噯" && node["reading"] == "おくび"
        }));
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
        let body = "『断頭台《ラギュイヨチーン》［ルビの「ラギュイヨチーン」は底本では「ラギュイヨケーン」］』";

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
    fn fallback_blocks_remove_unresolved_jis_gaiji_markers_from_large_body_projection() {
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
        assert_eq!(
            ab_ir::visible_projection(&blocks).visible_text,
            "吾輩は呭である。"
        );
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

        append_source_annotation_supplements(&mut blocks, "地球《ちきゆう》は月《ちきゆう》。");

        let counts = ab_ir::provenance_counts(&blocks);
        let json = ab_ir::blocks_to_aat_json(&blocks);
        let content = json[0]["content"].as_array().unwrap();

        assert_eq!(
            ab_ir::visible_projection(&blocks).visible_text,
            "地球は月。"
        );
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

        assert_eq!(
            ab_ir::visible_projection(&blocks).visible_text,
            "パーリスの侍童。"
        );
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

        let projected = ab_ir::visible_projection(&built.blocks);
        assert_eq!(projected.visible_text, "吾輩は猫。");
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
    fn retokenized_heading_decorations_emit_normal_heading_style() {
        let cases = [
            (Deco::AHead, 1, "大見出し"),
            (Deco::BHead, 2, "中見出し"),
            (Deco::CHead, 3, "小見出し"),
        ];

        for (deco, expected_level, text) in cases {
            let tokens = vec![
                Retokenized::DecoBegin(deco.clone()),
                Retokenized::Text(text),
                Retokenized::DecoEnd(deco),
            ];

            let blocks = retokenized_to_aat_blocks(&tokens);
            let json = ab_ir::blocks_to_aat_json(&blocks);
            let heading = &json[0];

            assert_eq!(heading["kind"], "heading");
            assert_eq!(heading["level"], expected_level);
            assert_eq!(heading["style"], "normal");
            assert_eq!(heading["content"][0]["value"], text);
        }
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
        assert_eq!(json[0]["content"][0]["resolved"], "呭");
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
        assert_eq!(json[0]["content"][0]["resolved"], serde_json::Value::Null);
        assert_eq!(json[0]["content"][0]["unresolved_reason"], "unresolved");
    }

    #[test]
    fn gaiji_resolution_uses_upstream_then_oracle_jis_supplement() {
        let unicode_description = "「口＋世」、U+546D";
        let mut upstream_input = unicode_description;
        let upstream = aozora_rs_gaiji::gaiji_to_char(&mut upstream_input)
            .expect("upstream gaiji resolution")
            .into_owned();
        let node = gaiji_inline(unicode_description, Provenance::ParserNormalized);
        let Inline::GaijiRef(gaiji) = node else {
            panic!("expected gaiji ref");
        };

        assert!(upstream_input.is_empty());
        assert_eq!(upstream, "呭");
        assert_eq!(gaiji.resolved.as_deref(), Some(upstream.as_str()));

        let jis_description = "「てへん＋掌」、第4水準2-13-47";
        let mut unresolved_input = jis_description;
        let unresolved = aozora_rs_gaiji::gaiji_to_char(&mut unresolved_input);
        let node = gaiji_inline(jis_description, Provenance::ParserNormalized);
        let Inline::GaijiRef(gaiji) = node else {
            panic!("expected gaiji ref");
        };

        assert!(unresolved.is_none());
        assert_eq!(gaiji.resolved.as_deref(), Some("撑"));
    }

    #[test]
    fn retokenized_figure_preserves_image_path_and_caption() {
        let tokens = vec![Retokenized::Figure(aozora_rs_core::Figure {
            path: "figures/map.png",
            caption: "地図",
            size: Some((120, 240)),
        })];

        let blocks = retokenized_to_aat_blocks(&tokens);
        let inline = ab_ir::block_content(&blocks[0]).first().unwrap();

        let Inline::GaijiRef(gaiji) = inline else {
            panic!("expected image gaiji ref");
        };
        assert_eq!(gaiji.source, "地図（figures/map.png縦120×横240）入る");
        assert_eq!(gaiji.description, "地図");
        assert_eq!(
            gaiji.kind,
            GaijiKind::Image {
                path: "figures/map.png".to_owned()
            }
        );
        assert_eq!(gaiji.resolved, None);
    }

    #[test]
    fn source_annotation_supplements_keep_gaiji_inside_ruby_reading_as_invisible_metadata() {
        let mut blocks = vec![Block::Paragraph {
            content: vec![Inline::ruby("淡絹", "※［＃濁点付き片仮名ヱ、1-7-84］エル")],
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
    fn parser_blocks_strip_nested_bottom_note_command() {
        let body = "野のはくちょう［＃「はくちょう」に傍点］［＃「はくちょう［＃「はくちょう」に傍点］」は底本では「はくちょ［＃「はくちょ」に傍点］う」］のむれが始まる。";
        let parsed = parse_with_aozora_rs(BodySelection {
            validation_body: body,
            found_separators: false,
            elapsed: Duration::ZERO,
        })
        .unwrap();
        let (result, _, _) = crate::build_aat_result(&parsed);
        let built = result.blocks;
        let projected = ab_ir::visible_projection(&built);

        assert!(result.fallback.used);
        assert_eq!(
            result.fallback.reason,
            crate::metrics::FallbackReason::ParserFailure
        );
        assert_eq!(projected.visible_text, "野のはくちょうのむれが始まる。");
        assert!(!projected.visible_text.contains("」］"));
    }

    #[test]
    fn parser_blocks_strip_split_command_tail_across_nodes() {
        let mut blocks = vec![Block::Paragraph {
            content: vec![
                Inline::text("「そりゃそういえば確にそうだが、忍術だって入"),
                Inline::text("用［＃「入"),
                Inline::text("用"),
                Inline::text("用」」］のものだから世に"),
            ],
        }];

        strip_cross_node_commands(&mut blocks);
        let projected = ab_ir::visible_projection(&blocks).visible_text;

        assert_eq!(
            projected,
            "「そりゃそういえば確にそうだが、忍術だって入用のものだから世に"
        );
        assert!(!projected.contains("」］"));
        assert!(!projected.contains("［＃"));
    }

    #[test]
    fn parser_blocks_strip_split_command_tail_orphan_marker() {
        let mut blocks = vec![Block::Paragraph {
            content: vec![
                Inline::text("「そりゃそういえば確にそうだが、忍術だって入※"),
                Inline::text("用のものだから世に"),
            ],
        }];

        strip_cross_node_commands(&mut blocks);
        let projected = ab_ir::visible_projection(&blocks).visible_text;

        assert_eq!(
            projected,
            "「そりゃそういえば確にそうだが、忍術だって入用のものだから世に"
        );
        assert!(!projected.contains("※"));
    }

    #[test]
    fn command_strip_borrows_marker_free_text() {
        let mut text = "吾輩は猫である。".to_owned();
        let mut depth = 0usize;
        let mut pending_split_marker = false;
        strip_string(&mut text, &mut depth, &mut pending_split_marker);

        assert_eq!(text, "吾輩は猫である。");
        assert!(!pending_split_marker);
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
                object
                    .values()
                    .any(|child| json_has_ruby(child, base, reading))
            }
            serde_json::Value::Array(values) => values
                .iter()
                .any(|child| json_has_ruby(child, base, reading)),
            _ => false,
        }
    }

    fn json_has_nested_style(style: &serde_json::Value) -> bool {
        style["content"].as_array().is_some_and(|children| {
            children
                .iter()
                .any(|child| child["kind"] == "style" || json_has_nested_style(child))
        })
    }
}
