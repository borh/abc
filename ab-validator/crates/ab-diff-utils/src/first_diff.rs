/// Result of finding the first character-level difference between two strings.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
pub struct FirstDifference {
    pub char_index: usize,
    pub left_snippet: String,
    pub right_snippet: String,
}

/// Finds the first character index where `left` and `right` diverge.
/// Returns `None` if the strings are identical.
#[must_use]
pub fn first_difference(left: &str, right: &str) -> Option<FirstDifference> {
    let mut left_chars = left.chars();
    let mut right_chars = right.chars();
    let mut char_index = 0usize;
    loop {
        match (left_chars.next(), right_chars.next()) {
            (Some(left), Some(right)) => {
                if left == right {
                    char_index += 1;
                } else {
                    break;
                }
            }
            (None, None) => return None,
            _ => break,
        }
    }
    Some(FirstDifference {
        char_index,
        left_snippet: snippet(left, char_index),
        right_snippet: snippet(right, char_index),
    })
}

fn snippet(value: &str, center: usize) -> String {
    let start = center.saturating_sub(24);
    value
        .chars()
        .skip(start)
        .take(center + 24 - start)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identical_strings_return_none() {
        assert!(first_difference("abc", "abc").is_none());
    }

    #[test]
    fn single_char_difference() {
        let diff = first_difference("abc", "axc").unwrap();
        assert_eq!(diff.char_index, 1);
    }

    #[test]
    fn length_difference() {
        let diff = first_difference("abc", "abcd").unwrap();
        assert_eq!(diff.char_index, 3);
        assert!(diff.left_snippet.is_empty() || diff.left_snippet.len() <= 48);
    }

    #[test]
    fn context_window_bounded() {
        let left: String = std::iter::repeat_n('a', 100).collect();
        let right: String = std::iter::repeat_n('a', 99)
            .chain(std::iter::once('b'))
            .collect();
        let diff = first_difference(&left, &right).unwrap();
        assert!(diff.left_snippet.len() <= 48);
        assert!(diff.right_snippet.len() <= 48);
    }

    #[test]
    fn multi_byte_chars_use_char_index_not_byte() {
        let diff = first_difference("今日", "今").unwrap();
        assert_eq!(diff.char_index, 1);
    }
}
