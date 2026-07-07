mod aat;
mod aozora;

use std::error::Error;
use std::fmt;

use serde::Serialize;

pub use aat::{
    ProjectionSpan, from_aat_value, from_aat_value_with_spans, visible_text_projection,
    visible_text_projection_with_spans,
};
pub use aozora::from_aozora_honbun_bytes;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PlainTextDocument {
    pub text_id: String,
    pub source_format: SourceFormat,
    pub text: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum SourceFormat {
    AozoraHonbun,
    AatVisibleText,
}

/// A sentence span from the sentence splitter. Borrows from source text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SentenceSpan<'a> {
    pub text: &'a str,
    pub byte_offset: usize,
    pub char_offset: usize,
}

/// Split text into sentences on terminal punctuation.
/// Boundaries: `。`, `！`, `？`, `!`, `?`, `.`, `．`.
/// Adjacent terminals are kept together (e.g. `本当！？` is one sentence).
/// ASCII and fullwidth periods are not boundaries inside alphanumeric runs
/// (e.g. `3.14`).
/// Terminals followed by a closing quote/bracket do not split until the next
/// terminal boundary.
/// Newlines are NOT sentence boundaries (they are paragraph breaks).
pub fn sentence_split(text: &str) -> Vec<SentenceSpan<'_>> {
    let mut spans = Vec::new();
    let mut byte_start = 0usize;
    let mut char_start = 0usize;
    let chars: Vec<(usize, char)> = text.char_indices().collect();

    for (i, &(_byte_pos, ch)) in chars.iter().enumerate() {
        if is_sentence_terminal(ch) {
            let previous = (i > 0).then_some(chars[i - 1].1);
            let next = chars.get(i + 1).map(|(_, ch)| *ch);
            if is_decimal_or_alphanumeric_period(previous, ch, next)
                || next.is_some_and(is_closing_quote_or_bracket)
            {
                continue;
            }
            // Consume adjacent terminals
            let mut end_idx = i + 1;
            while end_idx < chars.len() && is_sentence_terminal(chars[end_idx].1) {
                end_idx += 1;
            }
            let byte_end = if end_idx < chars.len() {
                chars[end_idx].0
            } else {
                text.len()
            };
            let text_slice = &text[byte_start..byte_end];
            if !text_slice.trim().is_empty() {
                spans.push(SentenceSpan {
                    text: text_slice,
                    byte_offset: byte_start,
                    char_offset: char_start,
                });
            }
            byte_start = byte_end;
            char_start += text_slice.chars().count();
        }
    }

    // Trailing text after last terminal
    if byte_start < text.len() {
        let text_slice = &text[byte_start..];
        if !text_slice.trim().is_empty() {
            spans.push(SentenceSpan {
                text: text_slice,
                byte_offset: byte_start,
                char_offset: char_start,
            });
        }
    }

    spans
}

fn is_sentence_terminal(ch: char) -> bool {
    matches!(ch, '。' | '！' | '？' | '!' | '?' | '.' | '．')
}

fn is_decimal_or_alphanumeric_period(previous: Option<char>, ch: char, next: Option<char>) -> bool {
    matches!(ch, '.' | '．')
        && previous.is_some_and(is_period_non_boundary_neighbor)
        && next.is_some_and(is_period_non_boundary_neighbor)
}

fn is_period_non_boundary_neighbor(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || matches!(ch, '０'..='９' | 'Ａ'..='Ｚ' | 'ａ'..='ｚ')
}

fn is_closing_quote_or_bracket(ch: char) -> bool {
    matches!(
        ch,
        ')' | '）' | '」' | '』' | '】' | '］' | '〕' | '〉' | '》' | ']'
    )
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PlainTextError {
    MissingAatWorkId,
}

impl fmt::Display for PlainTextError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PlainTextError::MissingAatWorkId => write!(f, "AAT is missing string work_id"),
        }
    }
}

impl Error for PlainTextError {}

pub(crate) fn canonicalize_line_endings(text: impl Into<String>) -> String {
    let text = text.into();
    if !text.as_bytes().contains(&b'\r') {
        return text;
    }

    let mut out = String::with_capacity(text.len());
    let mut chars = text.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '\r' {
            if chars.peek() == Some(&'\n') {
                chars.next();
            }
            out.push('\n');
        } else {
            out.push(ch);
        }
    }
    out
}

#[cfg(test)]
mod sentence_split_tests {
    use super::*;

    #[test]
    fn splits_on_japanese_period() {
        let spans = sentence_split("吾輩は猫である。名前はまだ無い。");
        assert_eq!(spans.len(), 2);
        assert_eq!(spans[0].text, "吾輩は猫である。");
        assert_eq!(spans[0].byte_offset, 0);
        assert_eq!(spans[1].text, "名前はまだ無い。");
    }

    #[test]
    fn keeps_adjacent_terminals_together() {
        let spans = sentence_split("本当！？そう！！！");
        assert_eq!(spans.len(), 2);
        assert_eq!(spans[0].text, "本当！？");
        assert_eq!(spans[1].text, "そう！！！");
    }

    #[test]
    fn handles_ascii_question_and_exclamation() {
        let spans = sentence_split("何だ!何だ?");
        assert_eq!(spans.len(), 2);
        assert_eq!(spans[0].text, "何だ!");
        assert_eq!(spans[1].text, "何だ?");
    }

    #[test]
    fn does_not_split_decimal_points() {
        let spans = sentence_split("これは3.14です。終わり。");
        assert_eq!(
            spans.iter().map(|span| span.text).collect::<Vec<_>>(),
            vec!["これは3.14です。", "終わり。"]
        );
    }

    #[test]
    fn does_not_split_before_closing_quote_or_bracket() {
        let spans = sentence_split("彼は言った。）次。");
        assert_eq!(
            spans.iter().map(|span| span.text).collect::<Vec<_>>(),
            vec!["彼は言った。）次。"]
        );
    }

    #[test]
    fn newlines_are_not_sentence_boundaries() {
        let spans = sentence_split("一行目\n二行目。");
        assert_eq!(spans.len(), 1);
        assert_eq!(spans[0].text, "一行目\n二行目。");
    }

    #[test]
    fn trailing_text_without_terminal_is_kept() {
        let spans = sentence_split("完成。未完成");
        assert_eq!(spans.len(), 2);
        assert_eq!(spans[0].text, "完成。");
        assert_eq!(spans[1].text, "未完成");
    }

    #[test]
    fn empty_text_returns_empty() {
        let spans = sentence_split("");
        assert!(spans.is_empty());
    }

    #[test]
    fn char_offsets_are_correct() {
        let spans = sentence_split("AB。CD。");
        assert_eq!(spans[0].char_offset, 0);
        assert_eq!(spans[1].char_offset, 3); // "AB。" is 3 chars
    }

    #[test]
    fn fixture_matrix_matches_abc_legacy_cases() {
        let cases = [
            (
                "吾輩は猫である。名前はまだ無い。",
                vec!["吾輩は猫である。", "名前はまだ無い。"],
            ),
            ("え！？本当。", vec!["え！？", "本当。"]),
            (
                "これは3.14です。終わり。",
                vec!["これは3.14です。", "終わり。"],
            ),
            ("彼は言った。）次。", vec!["彼は言った。）次。"]),
            ("一行目\n二行目。", vec!["一行目\n二行目。"]),
        ];

        for (input, expected) in cases {
            let actual = sentence_split(input)
                .iter()
                .map(|span| span.text)
                .collect::<Vec<_>>();
            assert_eq!(actual, expected, "input: {input}");
        }
    }
}
