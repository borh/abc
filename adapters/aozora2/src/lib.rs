use anyhow::Result;
use encoding_rs::SHIFT_JIS;
use regex::Regex;
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
    let mut content = Vec::new();
    content.push(json!({
        "kind": "text",
        "value": source_visible_text(text),
        "span": span_for(text, 0, text.len())
    }));
    append_ruby_annotations(&mut content, text);
    append_gaiji_annotations(&mut content, text);
    content
}

fn append_ruby_annotations(content: &mut Vec<serde_json::Value>, text: &str) {
    let marker = Regex::new(r"《([^》]+)》").unwrap();
    for capture in marker.captures_iter(text) {
        content.push(json!({
            "kind": "ruby",
            "base": "",
            "reading": capture.get(1).unwrap().as_str()
        }));
    }
}

fn append_gaiji_annotations(content: &mut Vec<serde_json::Value>, text: &str) {
    let marker = Regex::new(r"※(?:［＃([^］]+)］|\[#([^\]]+)\])").unwrap();
    for capture in marker.captures_iter(text) {
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

pub fn source_visible_text(text: &str) -> String {
    let gaiji = Regex::new(r"※(?:［＃([^］]+)］|\[#([^\]]+)\])").unwrap();
    let explicit_ruby = Regex::new(r"｜([^《》\r\n]+)《[^》]+》").unwrap();
    let ruby = Regex::new(r"｜?([^｜\s《》※［＃\[\]］、。，．「」『』（）()]+)《[^》]+》").unwrap();
    let orphan_ruby = Regex::new(r"《[^》]+》").unwrap();
    let command = Regex::new(r"［＃[^］]+］|\[#[^\]]+\]").unwrap();
    let without_gaiji = gaiji.replace_all(text, "");
    let without_explicit_ruby = explicit_ruby.replace_all(&without_gaiji, "$1");
    let without_ruby = ruby.replace_all(&without_explicit_ruby, "$1");
    let without_orphan_ruby = orphan_ruby.replace_all(&without_ruby, "");
    command.replace_all(&without_orphan_ruby, "").into_owned()
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
}
