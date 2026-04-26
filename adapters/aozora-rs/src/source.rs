use std::{
    sync::OnceLock,
    time::{Duration, Instant},
};

use anyhow::Result;
use encoding_rs::SHIFT_JIS;
use regex::Regex;
use sha2::{Digest, Sha256};

#[derive(Debug)]
pub struct DecodedSource {
    pub text: String,
    pub encoding: &'static str,
    pub source_hash: String,
    pub source_bytes: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct BodySelection<'a> {
    pub validation_body: &'a str,
    pub found_separators: bool,
    pub elapsed: Duration,
}

pub fn decode_source_bytes(bytes: &[u8]) -> Result<DecodedSource> {
    let source_hash = format!("sha256:{}", hex_sha256(bytes));
    if bytes.starts_with(&[0xef, 0xbb, 0xbf]) {
        return Ok(DecodedSource {
            text: std::str::from_utf8(&bytes[3..])?.to_owned(),
            encoding: "utf-8-bom",
            source_hash,
            source_bytes: bytes.len(),
        });
    }
    if let Ok(text) = std::str::from_utf8(bytes) {
        return Ok(DecodedSource {
            text: text.to_owned(),
            encoding: "utf-8",
            source_hash,
            source_bytes: bytes.len(),
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
        source_bytes: bytes.len(),
    })
}

pub fn select_body(decoded: &DecodedSource) -> BodySelection<'_> {
    let start = Instant::now();
    let body = body_text(&decoded.text);
    let validation_body = trim_colophon(body.text);
    BodySelection {
        validation_body,
        found_separators: body.found_separators,
        elapsed: start.elapsed(),
    }
}

pub(crate) fn trim_colophon(body: &str) -> &str {
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

pub(crate) struct BodyText<'a> {
    pub text: &'a str,
    pub found_separators: bool,
}

pub(crate) fn body_text(text: &str) -> BodyText<'_> {
    let mut separator_count = 0;
    let mut body_start = 0;
    let mut found_separators = false;
    let mut offset = 0;
    for line in text.split_inclusive('\n') {
        let trimmed = line.trim_end_matches(['\r', '\n']);
        if trimmed.chars().all(|ch| ch == '-') && trimmed.chars().count() >= 20 {
            separator_count += 1;
            if separator_count == 2 {
                body_start = offset + line.len();
                found_separators = true;
                break;
            }
        }
        offset += line.len();
    }
    BodyText {
        text: &text[body_start..],
        found_separators,
    }
}

pub(crate) fn starts_with_separator(text: &str) -> bool {
    text.lines()
        .find(|line| !line.trim().is_empty())
        .is_some_and(|line| {
            let trimmed = line.trim();
            !trimmed.is_empty() && trimmed.chars().all(|ch| ch == '-')
        })
}

pub(crate) fn source_visible_text(txt: &str) -> String {
    let without_gaiji = gaiji_regex().replace_all(txt, "");
    let without_explicit_ruby = explicit_ruby_regex().replace_all(&without_gaiji, "$1");
    let without_ruby = ruby_regex().replace_all(&without_explicit_ruby, "$1");
    let without_commands = command_regex()
        .replace_all(&without_ruby, "")
        .replace('※', "");
    remove_bottom_note_fragments(&without_commands)
}

pub(crate) fn remove_bottom_note_fragments(txt: &str) -> String {
    let without_bottom_notes = bottom_note_regex().replace_all(txt, "");
    let without_gaiji_notes = gaiji_note_regex().replace_all(&without_bottom_notes, "");
    mama_note_regex()
        .replace_all(&without_gaiji_notes, "")
        .into_owned()
}

fn gaiji_regex() -> &'static Regex {
    static REGEX: OnceLock<Regex> = OnceLock::new();
    REGEX.get_or_init(|| Regex::new(r"※(?:［＃([^］]+)］|\[#([^\]]+)\])").unwrap())
}

fn ruby_regex() -> &'static Regex {
    static REGEX: OnceLock<Regex> = OnceLock::new();
    REGEX.get_or_init(|| {
        Regex::new(r"｜?([^｜\s《》※［＃\[\]］、。，．「」『』（）()]+)《[^》]+》").unwrap()
    })
}

fn explicit_ruby_regex() -> &'static Regex {
    static REGEX: OnceLock<Regex> = OnceLock::new();
    REGEX.get_or_init(|| Regex::new(r"｜([^《》\r\n]+)《[^》]+》").unwrap())
}

fn command_regex() -> &'static Regex {
    static REGEX: OnceLock<Regex> = OnceLock::new();
    REGEX.get_or_init(|| Regex::new(r"［＃[^］]+］|\[#[^\]]+\]").unwrap())
}

fn bottom_note_regex() -> &'static Regex {
    static REGEX: OnceLock<Regex> = OnceLock::new();
    REGEX.get_or_init(|| Regex::new(r#"[^「」\s、。，．]+」は底本では「[^］\]]+[］\]]"#).unwrap())
}

fn gaiji_note_regex() -> &'static Regex {
    static REGEX: OnceLock<Regex> = OnceLock::new();
    REGEX.get_or_init(|| Regex::new(r#"[^「」\s、。，．]*」の「[^］\]]+[］\]]"#).unwrap())
}

fn mama_note_regex() -> &'static Regex {
    static REGEX: OnceLock<Regex> = OnceLock::new();
    REGEX.get_or_init(|| Regex::new(r#"[^「」\s、。，．]{1,80}」はママ[］\]]"#).unwrap())
}

fn hex_sha256(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    format!("{:x}", hasher.finalize())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decodes_utf8_and_records_source_bytes() {
        let decoded = decode_source_bytes("本文".as_bytes()).unwrap();
        assert_eq!(decoded.text, "本文");
        assert_eq!(decoded.encoding, "utf-8");
        assert_eq!(decoded.source_bytes, 6);
        assert!(decoded.source_hash.starts_with("sha256:"));
    }

    #[test]
    fn selects_validation_body_from_separators() {
        let text = "題名\n著者\n--------------------\n凡例\n--------------------\n本文\n底本：x\n";
        let decoded = DecodedSource {
            text: text.to_owned(),
            encoding: "utf-8",
            source_hash: "sha256:test".to_owned(),
            source_bytes: text.len(),
        };

        let selection = select_body(&decoded);
        assert_eq!(selection.validation_body, "本文\n");
        assert!(selection.found_separators);
    }

    #[test]
    fn reports_when_body_separators_are_missing() {
        let decoded = DecodedSource {
            text: "題名\n著者\n本文\n".to_owned(),
            encoding: "utf-8",
            source_hash: "sha256:test".to_owned(),
            source_bytes: 20,
        };

        let selection = select_body(&decoded);
        assert_eq!(selection.validation_body, "題名\n著者\n本文\n");
        assert!(!selection.found_separators);
    }

    #[test]
    fn source_visible_text_handles_ruby_gaiji_and_commands_repeatedly() {
        for _ in 0..100 {
            let visible =
                source_visible_text("吾輩《わがはい》は※［＃「口＋世」、U+546D］［＃ここは注記］");
            assert!(visible.contains("吾輩"));
            assert!(!visible.contains("「口＋世」、U+546D"));
            assert!(!visible.contains("わがはい"));
            assert!(!visible.contains("ここは注記"));
        }
    }

    #[test]
    fn source_visible_text_projects_explicit_ruby_base_without_marker() {
        let visible = source_visible_text("――『｜あのひとにとって、わたし《ルビ》はなんだろう？」");

        assert_eq!(visible, "――『あのひとにとって、わたしはなんだろう？」");
    }
}
