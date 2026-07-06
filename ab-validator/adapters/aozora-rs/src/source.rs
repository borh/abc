use std::{
    borrow::Cow,
    time::{Duration, Instant},
};

use ab_source_syntax::{self, SourceAnnotationsBoth, SourceEvent};
use anyhow::Result;
use encoding_rs::SHIFT_JIS;
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

#[derive(Debug)]
pub struct SourceArtifacts<'a> {
    pub events: Vec<SourceEvent<'a>>,
    pub source_visible: String,
    pub annotations_both: SourceAnnotationsBoth<'a>,
}

impl<'a> SourceArtifacts<'a> {
    pub fn collect(body: &'a str) -> Self {
        let events = ab_source_syntax::source_events(body);
        let source_visible = ab_source_syntax::comparison_lossy_body_from_events(&events);
        let annotations_both = ab_source_syntax::source_annotations_both_from_events(&events);

        Self {
            events,
            source_visible,
            annotations_both,
        }
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

pub(crate) fn source_visible_text(txt: &str) -> Cow<'_, str> {
    let projected = ab_source_syntax::comparison_lossy_body(txt);
    match projected {
        Cow::Borrowed(text) => strip_orphaned_command_tails(text)
            .map(Cow::Owned)
            .unwrap_or(Cow::Borrowed(text)),
        Cow::Owned(text) => strip_orphaned_command_tails(&text)
            .map(Cow::Owned)
            .unwrap_or(Cow::Owned(text)),
    }
}

fn strip_orphaned_command_tails(text: &str) -> Option<String> {
    if !text.contains('」')
        && !text.contains('］')
        && !text.contains(']')
        && !text.contains('［')
        && !text.contains('[')
    {
        return None;
    }

    let chars: Vec<char> = text.chars().collect();
    let mut out = String::with_capacity(text.len());
    let mut idx = 0;
    let mut changed = false;

    while idx < chars.len() {
        if chars[idx] == '」' {
            let mut next = idx;
            while next < chars.len() && chars[next] == '」' {
                next += 1;
            }
            if next < chars.len() && matches!(chars[next], '］' | ']') {
                idx = next + 1;
                changed = true;
                continue;
            }
        }

        if idx + 1 < chars.len() && chars[idx] == '［' && chars[idx + 1] == '＃' {
            if !chars[idx + 2..].contains(&'］') && !chars[idx + 2..].contains(&']') {
                changed = true;
                break;
            }
        }

        if idx + 1 < chars.len() && chars[idx] == '[' && chars[idx + 1] == '#' {
            if !chars[idx + 2..].contains(&']') {
                changed = true;
                break;
            }
        }

        out.push(chars[idx]);
        idx += 1;
    }

    if changed { Some(out) } else { None }
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
    fn source_visible_text_borrows_marker_free_input() {
        let visible = source_visible_text("吾輩は猫である。");

        assert!(matches!(visible, std::borrow::Cow::Borrowed(_)));
        assert_eq!(visible, "吾輩は猫である。");
    }

    #[test]
    fn source_visible_text_projects_explicit_ruby_base_without_marker() {
        let visible = source_visible_text("――『｜あのひとにとって、わたし《ルビ》はなんだろう？」");

        assert!(matches!(visible, std::borrow::Cow::Owned(_)));
        assert_eq!(visible, "――『あのひとにとって、わたしはなんだろう？」");
    }

    #[test]
    fn source_visible_text_removes_orphan_ruby_after_unresolved_gaiji() {
        let visible =
            source_visible_text("ことを、※［＃「口＋愛」、第3水準1-15-23］《おくび》にも");

        assert_eq!(visible, "ことを、にも");
    }

    #[test]
    fn source_visible_text_strips_orphaned_closing_command_marker_tail() {
        let visible = source_visible_text("しい」］程仲よく暮しました。");

        assert_eq!(visible, "しい程仲よく暮しました。");
    }

    #[test]
    fn source_visible_text_strips_repeated_orphaned_closing_command_marker_tail() {
        let visible = source_visible_text("用」」］のものだから世に");

        assert_eq!(visible, "用のものだから世に");
    }

    #[test]
    fn source_visible_text_strips_orphaned_command_opening_tail() {
        let visible = source_visible_text("用［＃「入");

        assert_eq!(visible, "用");
    }

    #[test]
    fn source_visible_text_strips_orphaned_ascii_command_opening_tail() {
        let visible = source_visible_text("用[#入");

        assert_eq!(visible, "用");
    }

    #[test]
    fn source_visible_text_handles_ascii_close_bracket_in_same_pattern() {
        let visible = source_visible_text("あ」]の");

        assert_eq!(visible, "あの");
    }
}
