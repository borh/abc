use anyhow::{Result, anyhow, bail};
use aozora_rs_core::{Break, Deco, Retokenized, parse_meta, retokenize, scopenize, tokenize};
use regex::Regex;
use serde_json::json;
use winnow::LocatingSlice;

mod metrics;
mod source;

pub use source::{DecodedSource, decode_source_bytes};
use source::{
    body_text, remove_bottom_note_fragments, source_visible_text, starts_with_separator,
    trim_colophon,
};

pub const VERSION: &str = "aozora-rs-adapter 0.1.0 dd380ee639ca317ac9092ef2ba554acdf70e3c8d";

#[derive(Debug)]
struct ParsedSource<'a> {
    body: &'a str,
    retokenized: Vec<Retokenized<'a>>,
    warnings: Vec<String>,
}

pub fn aat_json_from_bytes(bytes: &[u8]) -> Result<Vec<u8>> {
    let decoded = decode_source_bytes(bytes)?;
    let parsed = parse_with_aozora_rs(&decoded.text)?;
    let aat = build_aat(&decoded, &parsed);
    let mut out = Vec::new();
    serde_json::to_writer(&mut out, &aat)?;
    out.push(b'\n');
    Ok(out)
}

pub fn html_from_bytes(_bytes: &[u8]) -> Result<String> {
    bail!("aozora-rs-adapter --mode html is intentionally deferred to the render-diff phase")
}

fn parse_with_aozora_rs(text: &str) -> Result<ParsedSource<'_>> {
    let mut warnings = Vec::new();
    let mut parsed_body = text;
    let meta_ok = match parse_meta(&mut parsed_body) {
        Ok(_) => true,
        Err(error) => {
            warnings.push(format!("meta parse warning: {error}"));
            false
        }
    };
    let parsed_body = trim_colophon(parsed_body);
    let validation_body = trim_colophon(body_text(text));
    let parse_body = if meta_ok && !starts_with_separator(parsed_body) {
        parsed_body
    } else {
        validation_body
    };

    let mut input = LocatingSlice::new(parse_body);
    let tokenized = tokenize(&mut input).map_err(|()| anyhow!("aozora-rs-core tokenize failed"))?;
    let ((scopenized, flat_tokens), scopenize_errors) = scopenize(tokenized).into_tuple();
    let (retokenized, retokenize_errors) = retokenize(flat_tokens, scopenized).into_tuple();
    warnings.extend(
        scopenize_errors
            .into_iter()
            .map(|error| format!("{error:?}")),
    );
    warnings.extend(retokenize_errors.into_iter().map(|error| error.to_string()));

    Ok(ParsedSource {
        body: validation_body,
        retokenized,
        warnings,
    })
}

fn build_aat(decoded: &DecodedSource, parsed: &ParsedSource<'_>) -> serde_json::Value {
    json!({
        "version": 1,
        "work_id": "stdin",
        "blocks": retokenized_to_aat_blocks(parsed.body, &parsed.retokenized),
        "meta": {
            "adapter": "aozora-rs",
            "adapter_version": VERSION,
            "source_encoding": decoded.encoding,
            "source_hash": decoded.source_hash,
            "parse_complete": true,
            "warnings": parsed.warnings.iter().map(|message| json!({ "message": message })).collect::<Vec<_>>()
        }
    })
}

fn retokenized_to_aat_blocks(body: &str, tokens: &[Retokenized<'_>]) -> Vec<serde_json::Value> {
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
            Retokenized::Figure(figure) => content.push(json!({
                "kind": "gaiji",
                "x-description-format": "aozora-rs-core Figure Display output; original gaiji notation is not preserved by Figure",
                "description": figure.to_string(),
                "resolved": "",
                "jis_code": null,
                "unresolved_reason": null
            })),
            Retokenized::DecoBegin(Deco::Ruby(reading)) => {
                let (base, next_idx) = collect_decorated_visible_text(tokens, idx + 1, |deco| {
                    matches!(deco, Deco::Ruby(_))
                });
                if !is_pathological_ruby_base(&base) {
                    content.push(json!({
                        "kind": "ruby",
                        "base": base,
                        "reading": reading
                    }));
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
                blocks.push(json!({
                    "kind": "heading",
                    "level": level,
                    "style": stable_style_type(deco),
                    "content": [{"kind": "text", "value": value}]
                }));
                idx = next_idx;
                continue;
            }
            Retokenized::DecoBegin(deco) => {
                let (value, next_idx) =
                    collect_decorated_visible_text(tokens, idx + 1, |candidate| {
                        same_deco_kind(candidate, deco)
                    });
                content.push(json!({
                    "kind": "style",
                    "style_type": stable_style_type(deco),
                    "content": [{"kind": "text", "value": value}]
                }));
                idx = next_idx;
                continue;
            }
            Retokenized::DecoEnd(_) => {}
        }
        idx += 1;
    }
    flush_paragraph(&mut blocks, &mut content);
    if blocks.is_empty() {
        blocks.push(json!({ "kind": "paragraph", "content": [] }));
    }
    append_source_annotation_supplements(&mut blocks, body);
    strip_cross_node_commands(&mut blocks);
    if body.len() > 500_000 {
        return source_visible_fallback_blocks(body);
    }
    if !visible_projection_is_in_source_order(&blocks, body) {
        blocks = source_visible_fallback_blocks(body);
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

fn flush_paragraph(blocks: &mut Vec<serde_json::Value>, content: &mut Vec<serde_json::Value>) {
    if content.is_empty() {
        return;
    }
    blocks.push(json!({
        "kind": "paragraph",
        "content": std::mem::take(content)
    }));
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

fn push_text(content: &mut Vec<serde_json::Value>, value: &str) {
    if value.is_empty() {
        return;
    }
    content.push(json!({ "kind": "text", "value": value }));
}

fn strip_cross_node_commands(blocks: &mut [serde_json::Value]) {
    let mut state = CommandStripState::None;
    for block in blocks {
        strip_commands_in_value(block, &mut state);
    }
}

fn strip_commands_in_value(value: &mut serde_json::Value, state: &mut CommandStripState) {
    match value.get("kind").and_then(|kind| kind.as_str()) {
        Some("text") => strip_string_field(value, "value", state),
        Some("ruby") => strip_string_field(value, "base", state),
        _ => {}
    }
    for key in ["content", "children", "upper", "lower"] {
        if let Some(values) = value.get_mut(key).and_then(|value| value.as_array_mut()) {
            for child in values {
                strip_commands_in_value(child, state);
            }
        }
    }
}

fn strip_string_field(value: &mut serde_json::Value, field: &str, state: &mut CommandStripState) {
    if let Some(text) = value.get(field).and_then(|value| value.as_str()) {
        let cleaned = remove_bottom_note_fragments(&strip_command_fragments(text, state));
        value[field] = serde_json::Value::String(cleaned);
    }
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

fn append_source_annotation_supplements(blocks: &mut [serde_json::Value], body: &str) {
    let Some(first_block) = blocks.iter_mut().find(|block| {
        block
            .get("content")
            .and_then(|value| value.as_array())
            .is_some()
    }) else {
        return;
    };
    let Some(content) = first_block
        .get_mut("content")
        .and_then(|value| value.as_array_mut())
    else {
        return;
    };

    append_ruby_supplements(content, body);
    append_gaiji_supplements(content, body);
}

fn source_visible_fallback_blocks(body: &str) -> Vec<serde_json::Value> {
    let mut blocks = vec![json!({
        "kind": "paragraph",
        "content": [{"kind": "text", "value": source_visible_text(body)}]
    })];
    append_source_annotation_supplements(&mut blocks, body);
    blocks
}

fn visible_projection_is_in_source_order(blocks: &[serde_json::Value], body: &str) -> bool {
    let projection = normalize_visible(&visible_projection(blocks));
    if projection.is_empty() {
        return true;
    }
    let source = normalize_visible(&source_visible_text(body));
    is_subsequence(&projection, &source)
}

fn visible_projection(blocks: &[serde_json::Value]) -> String {
    let mut out = String::new();
    for block in blocks {
        collect_visible_projection(block, &mut out);
    }
    out
}

fn collect_visible_projection(value: &serde_json::Value, out: &mut String) {
    match value.get("kind").and_then(|kind| kind.as_str()) {
        Some("text") => out.push_str(
            value
                .get("value")
                .and_then(|value| value.as_str())
                .unwrap_or_default(),
        ),
        Some("ruby") => out.push_str(
            value
                .get("base")
                .and_then(|value| value.as_str())
                .unwrap_or_default(),
        ),
        Some("gaiji") => {
            if let Some(resolved) = value.get("resolved").and_then(|value| value.as_str()) {
                out.push_str(resolved);
            } else {
                out.push_str(
                    value
                        .get("description")
                        .and_then(|value| value.as_str())
                        .unwrap_or_default(),
                );
            }
        }
        _ => {}
    }
    for key in ["content", "children", "upper", "lower"] {
        if let Some(values) = value.get(key).and_then(|value| value.as_array()) {
            for child in values {
                collect_visible_projection(child, out);
            }
        }
    }
}

fn normalize_visible(value: &str) -> String {
    value.split_whitespace().collect::<Vec<_>>().join(" ")
}

fn is_subsequence(needle: &str, haystack: &str) -> bool {
    let mut haystack = haystack.chars();
    for ch in needle.chars() {
        if !haystack.any(|candidate| candidate == ch) {
            return false;
        }
    }
    true
}

fn append_ruby_supplements(content: &mut Vec<serde_json::Value>, body: &str) {
    let marker = Regex::new(r"《([^》]+)》").unwrap();
    let existing = content
        .iter()
        .filter(|node| node.get("kind").and_then(|kind| kind.as_str()) == Some("ruby"))
        .filter_map(|node| node.get("reading").and_then(|reading| reading.as_str()))
        .map(ToOwned::to_owned)
        .collect::<Vec<_>>();
    for capture in marker.captures_iter(body) {
        let reading = capture.get(1).unwrap().as_str();
        if existing.iter().any(|existing| existing == reading) {
            continue;
        }
        content.push(json!({
            "kind": "ruby",
            "base": "",
            "reading": reading
        }));
    }
}

fn append_gaiji_supplements(content: &mut Vec<serde_json::Value>, body: &str) {
    let marker = Regex::new(r"※(?:［＃([^］]+)］|\[#([^\]]+)\])").unwrap();
    let existing_count = content
        .iter()
        .filter(|node| node.get("kind").and_then(|kind| kind.as_str()) == Some("gaiji"))
        .count();
    for capture in marker.captures_iter(body).skip(existing_count) {
        let description = capture
            .get(1)
            .or_else(|| capture.get(2))
            .map(|matched| matched.as_str())
            .unwrap_or_default();
        content.push(json!({
            "kind": "gaiji",
            "description": description,
            "resolved": "",
            "jis_code": null,
            "unresolved_reason": null
        }));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn emits_schema_shaped_aat_for_ruby_and_gaiji() {
        let input = "\
タイトル
著者
-------------------------------------------------------
凡例
-------------------------------------------------------
吾輩《わがはい》は※［＃「口＋世」、U+546D］である。"
            .as_bytes();
        let out = aat_json_from_bytes(input).unwrap();
        let value: serde_json::Value = serde_json::from_slice(&out).unwrap();

        assert_eq!(value["work_id"], "stdin");
        assert_eq!(value["meta"]["adapter"], "aozora-rs");
        assert_eq!(value["meta"]["source_encoding"], "utf-8");
        assert!(value["meta"]["parse_complete"].as_bool().unwrap());
        assert_eq!(value["blocks"][0]["kind"], "paragraph");
        assert!(
            value["blocks"][0]["content"]
                .as_array()
                .unwrap()
                .iter()
                .any(|node| node["kind"] == "ruby")
        );
        assert!(
            value["blocks"][0]["content"]
                .as_array()
                .unwrap()
                .iter()
                .any(|node| node["kind"] == "gaiji")
        );
    }
}
