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

/// Options controlling sentence-splitting behavior.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct SplitOptions {
    /// When true, suppress all bracket/quote-based split suppression.
    /// Used when re-splitting text inside a known quote region so inner
    /// sentence terminals (`。`) split even when a closing bracket follows.
    pub suppress_closing_bracket_check: bool,
}

/// Split text into sentences on terminal punctuation with explicit options.
///
/// Boundaries: `。`, `！`, `？`, `!`, `?`, `.`, `．`.
///
/// Adjacent terminals are kept together (e.g. `本当！？` is one sentence).
/// Improved rules over the legacy splitter: CJK numbered lists (`１．`) do not
/// split; `！`/`？` followed by Japanese continuation marks (`笑`, small kana)
/// stay together; `closing_bracket_ahead` scans with bracket-depth tracking so
/// a terminal before a closing bracket suppresses only when a bracket actually
/// follows (not infinitely ahead); Western closing quotes also suppress; `。`
/// at end-of-input always splits while other terminals at end-of-input stay
/// attached.
///
/// Newlines are NOT sentence boundaries (they are paragraph breaks).
pub fn split_sentences_with_options<'a>(
    input: &'a str,
    opts: &SplitOptions,
) -> Vec<SentenceSpan<'a>> {
    let mut spans = Vec::new();
    let mut byte_start = 0usize;
    let mut char_start = 0usize;
    let chars: Vec<(usize, char)> = input.char_indices().collect();

    for (i, &(_byte_pos, ch)) in chars.iter().enumerate() {
        if !is_sentence_terminal(ch) {
            continue;
        }

        let prev = i.checked_sub(1).map(|prev_i| chars[prev_i].1);
        let next = chars.get(i + 1).map(|(_, ch)| *ch);
        let suppress = opts.suppress_closing_bracket_check;

        let should_split = match (prev, next) {
            (Some(prev_ch), Some(next_ch)) => {
                // Suppress when any of these hold; split otherwise.
                !(is_sentence_terminal(next_ch)
                    || (!suppress && is_closing_quote_or_bracket(next_ch))
                    || (ch == '。'
                        && !suppress
                        && closing_bracket_ahead(input, chars[i].0 + ch.len_utf8()))
                    || ((ch == '.' || ch == '．') && is_cjk_digit(prev_ch))
                    || ((ch == '！' || ch == '？') && is_japanese_continuation(next_ch))
                    || (is_period_non_boundary_neighbor(prev_ch)
                        && is_period_non_boundary_neighbor(next_ch)
                        && ch != '。'))
            }
            // End of input: split only on `。` so a trailing `?`/`!` stays attached.
            (Some(_), None) => ch == '。',
            _ => true,
        };

        if should_split {
            // Consume adjacent terminals so `！？` is one boundary.
            let mut end_idx = i + 1;
            while end_idx < chars.len() && is_sentence_terminal(chars[end_idx].1) {
                end_idx += 1;
            }
            let byte_end = if end_idx < chars.len() {
                chars[end_idx].0
            } else {
                input.len()
            };
            let text_slice = &input[byte_start..byte_end];
            if !text_slice.trim().is_empty() {
                spans.push(SentenceSpan {
                    text: text_slice,
                    byte_offset: byte_start,
                    char_offset: char_start,
                });
            }
            char_start += text_slice.chars().count();
            byte_start = byte_end;
        }
    }

    // Trailing text after last terminal
    if byte_start < input.len() {
        let text_slice = &input[byte_start..];
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

/// Convenience wrapper with default options.
pub fn split_sentences(input: &str) -> Vec<SentenceSpan<'_>> {
    split_sentences_with_options(input, &SplitOptions::default())
}

/// Legacy entry point preserved for existing call sites (ab-ortho-detect).
/// Delegates to [`split_sentences_with_options`] with default
/// options, so the improved v2 rules propagate to all consumers.
pub fn sentence_split(text: &str) -> Vec<SentenceSpan<'_>> {
    split_sentences_with_options(text, &SplitOptions::default())
}

fn is_sentence_terminal(ch: char) -> bool {
    matches!(ch, '。' | '！' | '？' | '!' | '?' | '.' | '．')
}

fn is_cjk_digit(ch: char) -> bool {
    matches!(
        ch,
        '0'..='9' | '０'..='９' | '〇' | '一' | '二' | '三' | '四' | '五' | '六'
            | '七' | '八' | '九' | '十'
    )
}

fn is_japanese_continuation(ch: char) -> bool {
    matches!(
        ch,
        '笑' | '泣'
            | '汗'
            | '涙'
            | '怒'
            | '嬉'
            | '爆'
            | '驚'
            | '喜'
            | '悲'
            | '謎'
            | '恥'
            | '焦'
            | '苦'
            | '照'
            | '憂'
            | '…'
            | '〜'
            | 'と'
            | 'っ'
            | 'ぁ'
            | 'ぃ'
            | 'ぅ'
            | 'ぇ'
            | 'ぉ'
            | 'ッ'
            | 'ァ'
            | 'ィ'
            | 'ゥ'
            | 'ェ'
            | 'ォ'
    )
}

/// Returns true when a closing bracket/quote appears ahead on the current line
/// (before a newline), tracking opening/closing bracket depth so nested pairs
/// resolve correctly.
fn closing_bracket_ahead(input: &str, byte_pos: usize) -> bool {
    const CLOSING: &[char] = &[')', '）', '」', '』', '】', '］', '〕', '〉', '》'];
    const OPENING: &[char] = &['(', '（', '「', '『', '【', '［', '〔', '〈', '《'];
    let mut depth = 0i32;
    for ch in input[byte_pos..].chars() {
        if ch == '\n' {
            return false;
        }
        if CLOSING.contains(&ch) {
            if depth == 0 {
                return true;
            }
            depth -= 1;
        } else if OPENING.contains(&ch) {
            depth += 1;
        }
    }
    false
}

fn is_period_non_boundary_neighbor(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || matches!(ch, '０'..='９' | 'Ａ'..='Ｚ' | 'ａ'..='ｚ')
}

fn is_closing_quote_or_bracket(ch: char) -> bool {
    matches!(
        ch,
        ')' | '）'
            | '」'
            | '』'
            | '】'
            | '］'
            | '〕'
            | '〉'
            | '》'
            | ']'
            | '"'
            | '\u{201D}'
            | '\u{2019}'
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

    #[test]
    fn split_sentences_basic() {
        let spans = split_sentences("吾輩は猫である。名前はまだ無い。");
        assert_eq!(spans.len(), 2);
        assert_eq!(spans[0].text, "吾輩は猫である。");
        assert_eq!(spans[1].text, "名前はまだ無い。");
    }

    #[test]
    fn split_sentences_adjacent_terminals() {
        let spans = split_sentences("本当！？そう！！！");
        assert_eq!(spans.len(), 2);
        assert_eq!(spans[0].text, "本当！？");
        assert_eq!(spans[1].text, "そう！！！");
    }

    #[test]
    fn split_sentences_terminal_at_start_does_not_panic() {
        let spans = split_sentences("。始まり。");
        assert_eq!(
            spans.iter().map(|span| span.text).collect::<Vec<_>>(),
            vec!["。", "始まり。"]
        );
        assert_eq!(spans[0].byte_offset, 0);
        assert_eq!(spans[0].char_offset, 0);
        assert_eq!(spans[1].byte_offset, "。".len());
        assert_eq!(spans[1].char_offset, 1);
    }

    #[test]
    fn split_sentences_cjk_numbered_list() {
        let spans = split_sentences("１．項目。２．項目。");
        assert_eq!(spans.len(), 2);
        assert_eq!(spans[0].text, "１．項目。");
        assert_eq!(spans[1].text, "２．項目。");
    }

    #[test]
    fn split_sentences_exclamation_continuation() {
        let spans = split_sentences("すごい！笑った。");
        assert_eq!(spans.len(), 1);
        assert_eq!(spans[0].text, "すごい！笑った。");
    }

    #[test]
    fn split_sentences_closing_bracket_depth() {
        // A terminal before a closing quote is suppressed because the real
        // boundary comes after the bracket.
        let spans = split_sentences("先生は「綺麗ですよ。落葉で埋まります」といった。");
        assert_eq!(spans.len(), 1);
        assert_eq!(
            spans[0].text,
            "先生は「綺麗ですよ。落葉で埋まります」といった。"
        );
    }

    #[test]
    fn split_sentences_suppress_mode_splits_before_close() {
        // Re-splitting inside a known quote region: the closing 」 no longer
        // suppresses the inner 。 boundary.
        let text = "もう少しすると、綺麗ですよ。この木が埋まるようになります」";
        let opts = SplitOptions {
            suppress_closing_bracket_check: true,
        };
        let spans = split_sentences_with_options(text, &opts);
        assert_eq!(spans.len(), 2);
        assert_eq!(spans[0].text, "もう少しすると、綺麗ですよ。");
        assert_eq!(spans[1].text, "この木が埋まるようになります」");
    }

    #[test]
    fn split_sentences_newlines_not_boundaries() {
        let spans = split_sentences("一行目\n二行目。");
        assert_eq!(spans.len(), 1);
        assert_eq!(spans[0].text, "一行目\n二行目。");
    }

    #[test]
    fn split_sentences_matches_legacy_wrapper() {
        // The legacy entry point is a pure delegator to the v2 splitter.
        let cases = [
            "吾輩は猫である。名前はまだ無い。",
            "本当！？そう！！！",
            "これは3.14です。終わり。",
            "彼は言った。）次。",
            "一行目\n二行目。",
        ];
        for input in cases {
            assert_eq!(
                sentence_split(input)
                    .iter()
                    .map(|s| s.text)
                    .collect::<Vec<_>>(),
                split_sentences(input)
                    .iter()
                    .map(|s| s.text)
                    .collect::<Vec<_>>(),
                "input: {input}"
            );
        }
    }
}
