#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectionSummary {
    pub source_visible_text: Option<String>,
    pub source_visible_chars: usize,
    pub projected_visible_chars: usize,
    pub in_source_order: bool,
}

pub fn check(validation_body: &str, projected_visible_text: &str) -> ProjectionSummary {
    let source_visible_text = crate::source::source_visible_text(validation_body);
    let source_visible_chars = normalized_char_count(source_visible_text.as_ref());
    let projected_visible_chars = normalized_char_count(projected_visible_text);
    let in_source_order = if projected_visible_chars == 0 {
        source_visible_chars == 0
    } else if projected_visible_chars > source_visible_chars {
        false
    } else {
        is_normalized_subsequence(projected_visible_text, source_visible_text.as_ref())
    };
    ProjectionSummary {
        source_visible_text: if in_source_order {
            None
        } else {
            Some(source_visible_text.into_owned())
        },
        source_visible_chars,
        projected_visible_chars,
        in_source_order,
    }
}

fn normalized_char_count(value: &str) -> usize {
    NormalizedChars::new(value).count()
}

fn is_normalized_subsequence(needle: &str, haystack: &str) -> bool {
    let mut haystack = NormalizedChars::new(haystack);
    for ch in NormalizedChars::new(needle) {
        if !haystack.any(|candidate| candidate == ch) {
            return false;
        }
    }
    true
}

struct NormalizedChars<'a> {
    chars: std::str::Chars<'a>,
    buffered: Option<char>,
    pending_space: bool,
    emitted_any: bool,
}

impl<'a> NormalizedChars<'a> {
    fn new(value: &'a str) -> Self {
        Self {
            chars: value.chars(),
            buffered: None,
            pending_space: false,
            emitted_any: false,
        }
    }
}

impl Iterator for NormalizedChars<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ch) = self.buffered.take() {
            self.emitted_any = true;
            return Some(ch);
        }

        for ch in self.chars.by_ref() {
            if ch.is_whitespace() {
                if self.emitted_any {
                    self.pending_space = true;
                }
                continue;
            }

            if self.pending_space {
                self.pending_space = false;
                self.buffered = Some(ch);
                return Some(' ');
            }

            self.emitted_any = true;
            return Some(ch);
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn accepts_projected_text_in_source_order() {
        let summary = check("吾輩《わがはい》は猫である。", "吾輩は猫");
        assert!(summary.in_source_order);
        assert!(summary.source_visible_chars >= summary.projected_visible_chars);
        assert!(summary.source_visible_text.is_none());
    }

    #[test]
    fn rejects_projected_text_out_of_order() {
        let summary = check("吾輩は猫である。", "猫吾輩");
        assert!(!summary.in_source_order);
    }

    #[test]
    fn rejects_projected_text_longer_than_source() {
        let summary = check("短い本文", "短い本文より長い投影テキスト");

        assert!(!summary.in_source_order);
        assert!(summary.projected_visible_chars > summary.source_visible_chars);
        assert_eq!(summary.source_visible_text.as_deref(), Some("短い本文"));
    }

    #[test]
    fn rejects_empty_projection_for_nonempty_source() {
        let summary = check("本文", "");

        assert!(!summary.in_source_order);
        assert_eq!(summary.projected_visible_chars, 0);
        assert!(summary.source_visible_chars > 0);
        assert_eq!(summary.source_visible_text.as_deref(), Some("本文"));
    }

    #[test]
    fn streams_normalized_whitespace_like_split_join() {
        let value = "  吾輩\n\tは  猫  ";

        assert_eq!(
            NormalizedChars::new(value).collect::<String>(),
            "吾輩 は 猫"
        );
        assert!(is_normalized_subsequence("吾輩 は", value));
        assert!(!is_normalized_subsequence("吾輩  は", "吾輩は"));
    }
}
