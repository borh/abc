use anyhow::Result;
use encoding_rs::SHIFT_JIS;
use ab_source_syntax as source_syntax;
use serde::Serialize;
use serde_json::json;
use sha2::{Digest, Sha256};

pub const VERSION: &str = "aozora2-adapter 0.1.0 93420b53c7d52579a0ca3fde466cef8ce6d89879";

#[derive(Debug)]
pub struct DecodedSource {
    pub text: String,
    pub encoding: &'static str,
    pub source_hash: String,
}

#[derive(Debug, Serialize)]
struct Span {
    line_start: usize,
    line_end: usize,
    byte_start: usize,
    byte_end: usize,
}

pub fn decode_source_bytes(bytes: &[u8]) -> Result<DecodedSource> {
    let source_hash = format!("sha256:{}", hex_sha256(bytes));
    if bytes.starts_with(&[0xef, 0xbb, 0xbf]) {
        return Ok(DecodedSource {
            text: std::str::from_utf8(&bytes[3..])?.to_owned(),
            encoding: "utf-8-bom",
            source_hash,
        });
    }
    if let Ok(text) = std::str::from_utf8(bytes) {
        return Ok(DecodedSource {
            text: text.to_owned(),
            encoding: "utf-8",
            source_hash,
        });
    }
    let (cow, _, had_errors) = SHIFT_JIS.decode(bytes);
    Ok(DecodedSource {
        text: cow.into_owned(),
        encoding: if had_errors {
            "windows-31j-lossy"
        } else {
            "windows-31j"
        },
        source_hash,
    })
}

pub fn build_aat(decoded: &DecodedSource) -> serde_json::Value {
    let content = parse_inline_content(body_text(&decoded.text));
    json!({
        "version": 1,
        "work_id": "stdin",
        "blocks": [
            {
                "kind": "paragraph",
                "content": content
            }
        ],
        "meta": {
            "adapter": "aozora2",
            "adapter_version": VERSION,
            "source_encoding": decoded.encoding,
            "source_hash": decoded.source_hash,
            "parse_complete": true,
            "warnings": []
        }
    })
}

pub fn aat_json_from_bytes(bytes: &[u8]) -> Result<Vec<u8>> {
    let decoded = decode_source_bytes(bytes)?;
    let aat = build_aat(&decoded);
    let mut out = Vec::new();
    serde_json::to_writer(&mut out, &aat)?;
    out.push(b'\n');
    Ok(out)
}

pub fn body_text(text: &str) -> &str {
    let mut separator_count = 0;
    let mut body_start = 0;
    let mut offset = 0;
    for line in text.split_inclusive('\n') {
        if line
            .trim_end_matches(['\r', '\n'])
            .chars()
            .all(|ch| ch == '-')
            && line.trim_end_matches(['\r', '\n']).chars().count() >= 20
        {
            separator_count += 1;
            if separator_count == 2 {
                body_start = offset + line.len();
                break;
            }
        }
        offset += line.len();
    }
    let body = &text[body_start..];
    let body_end = body
        .char_indices()
        .find_map(|(offset, _)| {
            let rest = &body[offset..];
            if rest.starts_with("底本：") || rest.starts_with("底本:") {
                Some(offset)
            } else {
                None
            }
        })
        .unwrap_or(body.len());
    &body[..body_end]
}

fn parse_inline_content(text: &str) -> Vec<serde_json::Value> {
    let events = source_syntax::source_events(text);
    let source_visible = source_syntax::comparison_lossy_body_from_events(&events);
    let mut content = Vec::new();
    content.push(json!({
        "kind": "text",
        "value": source_visible,
        "span": span_for(text, 0, text.len())
    }));
    append_source_annotations_from_events(&mut content, &events);
    content
}

fn append_source_annotations_from_events(
    content: &mut Vec<serde_json::Value>,
    events: &[source_syntax::SourceEvent],
) {
    let mut last_gaiji_end = None;
    for event in events {
        match event.kind {
            source_syntax::SourceEventKind::Ruby { reading, .. } => {
                if last_gaiji_end != Some(event.span.start) {
                    content.push(json!({
                        "kind": "ruby",
                        "base": "",
                        "reading": reading
                    }));

                    for marker in source_syntax::source_annotations(reading)
                        .gaiji_descriptions
                        .iter()
                        .map(|marker| marker.value)
                    {
                        content.push(json!({
                            "kind": "gaiji",
                            "description": marker,
                            "resolved": "",
                            "jis_code": null,
                            "unresolved_reason": null
                        }));
                    }
                }
                last_gaiji_end = None;
            }
            source_syntax::SourceEventKind::Gaiji { description } => {
                content.push(json!({
                    "kind": "gaiji",
                    "description": description,
                    "resolved": "",
                    "jis_code": null,
                    "unresolved_reason": null
                }));
                last_gaiji_end = Some(event.span.end);
            }
            source_syntax::SourceEventKind::Text(_)
            | source_syntax::SourceEventKind::Command { .. }
            | source_syntax::SourceEventKind::EditorialNote { .. }
            | source_syntax::SourceEventKind::SegmentBoundary { .. } => {
                last_gaiji_end = None;
            }
        }
    }
}

#[cfg(test)]
fn source_visible_text(text: &str) -> String {
    source_syntax::comparison_lossy_body(text).into_owned()
}

fn span_for(text: &str, start: usize, end: usize) -> Span {
    let line = text[..start].chars().filter(|ch| *ch == '\n').count() + 1;
    Span {
        line_start: line,
        line_end: line,
        byte_start: text[..start].len(),
        byte_end: text[..end].len(),
    }
}

fn hex_sha256(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    format!("{:x}", hasher.finalize())
}

pub fn html_escape(value: &str) -> String {
    value
        .replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn source_visible_text_excludes_unresolved_gaiji_descriptions() {
        let visible = source_visible_text("二二※［＃小書き片仮名ン、237-11］が四");

        assert_eq!(visible, "二二が四");
    }

    #[test]
    fn source_visible_text_projects_explicit_ruby_base_without_marker() {
        let visible = source_visible_text("――『｜あのひとにとって、わたし《ルビ》はなんだろう？」");

        assert_eq!(visible, "――『あのひとにとって、わたしはなんだろう？」");
    }

    #[test]
    fn source_visible_text_removes_orphan_ruby_after_unresolved_gaiji() {
        let visible = source_visible_text("ことを、※［＃「口＋愛」、第3水準1-15-23］《おくび》にも");

        assert_eq!(visible, "ことを、にも");
    }

    #[test]
    fn parse_inline_content_emits_nested_gaiji_inside_ruby_reading() {
        let content = parse_inline_content("淡絹《※［＃濁点付き片仮名ヱ、1-7-84］エル》");

        assert_eq!(content.iter().filter(|node| node["kind"] == "gaiji").count(), 1);
        assert!(content.iter().any(|node| node["kind"] == "ruby"));
    }
}
