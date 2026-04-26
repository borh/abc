use std::{collections::HashSet, sync::OnceLock, time::Instant};

use ab_ir::{Block, Inline, ProjectedText, Provenance};
use aozora_rs_core::{Break, Deco, Retokenized};
use regex::Regex;

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
    let blocks = source_visible_fallback_blocks(body);
    let projected = ab_ir::visible_projection(&blocks);
    (blocks, projected)
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
        Inline::Ruby { base, .. } => strip_string(base, state),
        Inline::Style { content, .. } => {
            for child in content {
                strip_commands_in_inline(child, state);
            }
        }
        Inline::Gaiji { .. } => {}
    }
}

fn strip_string(text: &mut String, state: &mut CommandStripState) {
    *text = remove_bottom_note_fragments(&strip_command_fragments(text, state));
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
    let Some(first_block) = blocks.first_mut() else {
        return;
    };
    let content = ab_ir::block_content_mut(first_block);
    append_ruby_supplements(content, body, &existing_ruby_readings);
    append_gaiji_supplements(content, body, existing_gaiji_count);
}

fn source_visible_fallback_blocks(body: &str) -> Vec<Block> {
    let mut blocks = vec![Block::Paragraph {
        content: vec![Inline::text_with_provenance(
            source_visible_text(body),
            Provenance::RegexFallback,
        )],
    }];
    append_source_annotation_supplements(&mut blocks, body);
    blocks
}

fn append_ruby_supplements(content: &mut Vec<Inline>, body: &str, existing: &HashSet<String>) {
    for capture in ruby_marker_regex().captures_iter(body) {
        let reading = capture.get(1).unwrap().as_str();
        if existing.contains(reading) {
            continue;
        }
        content.push(Inline::ruby_with_provenance(
            "",
            reading,
            Provenance::RegexSupplement,
        ));
    }
}

fn append_gaiji_supplements(content: &mut Vec<Inline>, body: &str, existing_count: usize) {
    for capture in gaiji_marker_regex().captures_iter(body).skip(existing_count) {
        let description = capture
            .get(1)
            .or_else(|| capture.get(2))
            .map(|matched| matched.as_str())
            .unwrap_or_default();
        content.push(Inline::gaiji_with_provenance(
            description,
            "",
            None,
            Provenance::RegexSupplement,
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
        Inline::Text { .. } | Inline::Gaiji { .. } => {}
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
        Inline::Gaiji { .. } => 1,
        Inline::Style { content, .. } => content.iter().map(gaiji_count_in_inline).sum(),
        Inline::Text { .. } | Inline::Ruby { .. } => 0,
    }
}

fn ruby_marker_regex() -> &'static Regex {
    static REGEX: OnceLock<Regex> = OnceLock::new();
    REGEX.get_or_init(|| Regex::new(r"《([^》]+)》").unwrap())
}

fn gaiji_marker_regex() -> &'static Regex {
    static REGEX: OnceLock<Regex> = OnceLock::new();
    REGEX.get_or_init(|| Regex::new(r"※(?:［＃([^］]+)］|\[#([^\]]+)\])").unwrap())
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
        assert!(visible.contains("「口＋世」、U+546D"));
        assert!(!projected.visible_text.contains("わがはい"));
        assert!(ab_ir::provenance_counts(&blocks).regex_fallback > 0);
    }

    #[test]
    fn source_annotation_supplements_are_marked_as_regex_derived() {
        let mut blocks = vec![Block::Paragraph { content: vec![] }];

        append_source_annotation_supplements(
            &mut blocks,
            "吾輩《わがはい》は※［＃「口＋世」、U+546D］である。",
        );

        let counts = ab_ir::provenance_counts(&blocks);
        assert_eq!(counts.regex_supplement, 2);
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
        assert_eq!(counts.regex_supplement, 0);
    }
}
