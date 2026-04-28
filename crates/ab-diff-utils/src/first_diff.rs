/// Result of finding the first character-level difference between two strings.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
pub struct FirstDifference {
    pub char_index: usize,
    pub left_snippet: String,
    pub right_snippet: String,
}

/// Finds the first character index where `left` and `right` diverge.
/// Returns `None` if the strings are identical.
pub fn first_difference(left: &str, right: &str) -> Option<FirstDifference> {
    let left_chars: Vec<char> = left.chars().collect();
    let right_chars: Vec<char> = right.chars().collect();
    let max_common = left_chars.len().min(right_chars.len());
    let char_index = (0..max_common)
        .find(|idx| left_chars[*idx] != right_chars[*idx])
        .or_else(|| (left_chars.len() != right_chars.len()).then_some(max_common))?;
    Some(FirstDifference {
        char_index,
        left_snippet: snippet(&left_chars, char_index),
        right_snippet: snippet(&right_chars, char_index),
    })
}

fn snippet(chars: &[char], center: usize) -> String {
    let start = center.saturating_sub(24);
    let end = chars.len().min(center + 24);
    chars[start..end].iter().collect()
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
