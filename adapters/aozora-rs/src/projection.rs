#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectionSummary {
    pub source_visible_text: Option<String>,
    pub source_visible_chars: usize,
    pub projected_visible_chars: usize,
    pub in_source_order: bool,
}

#[cfg(test)]
pub fn check(validation_body: &str, projected_visible_text: &str) -> ProjectionSummary {
    let source_visible_text = crate::source::source_visible_text(validation_body);
    check_with_source_visible(source_visible_text.as_ref(), projected_visible_text)
}

pub fn check_with_source_visible(
    source_visible_text: &str,
    projected_visible_text: &str,
) -> ProjectionSummary {
    let source_visible_chars = normalized_char_count(source_visible_text);
    let projected_visible_chars = normalized_char_count(projected_visible_text);
    let in_source_order = is_normalized_subsequence(projected_visible_text, source_visible_text);
    let lengths_match = source_visible_chars == projected_visible_chars;

    if in_source_order && lengths_match {
        return ProjectionSummary {
            source_visible_text: None,
            source_visible_chars,
            projected_visible_chars,
            in_source_order: true,
        };
    }

    ProjectionSummary {
        source_visible_text: Some(source_visible_text.to_owned()),
        source_visible_chars,
        projected_visible_chars,
        in_source_order,
    }
}

fn normalized_char_count(value: &str) -> usize {
    normalize_visible(value).chars().count()
}

fn is_normalized_subsequence(needle: &str, haystack: &str) -> bool {
    let normalized_haystack = normalize_visible(haystack);
    let normalized_needle = normalize_visible(needle);
    let mut haystack = normalized_haystack.chars();
    for ch in normalized_needle.chars() {
        if !haystack.any(|candidate| candidate == ch) {
            return false;
        }
    }
    true
}

fn normalize_visible(value: &str) -> String {
    use unicode_normalization::UnicodeNormalization;

    value
        .nfkc()
        .map(normalize_iteration_mark)
        .collect::<String>()
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

fn normalize_iteration_mark(ch: char) -> char {
    match ch {
        'ゝ' | 'ヽ' => 'ゝ',
        'ゞ' | 'ヾ' => 'ゞ',
        _ => ch,
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
        assert_eq!(
            summary.source_visible_text.as_deref(),
            Some("吾輩は猫である。")
        );
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

        assert!(summary.in_source_order);
        assert_eq!(summary.projected_visible_chars, 0);
        assert!(summary.source_visible_chars > 0);
        assert_eq!(summary.source_visible_text.as_deref(), Some("本文"));
    }

    #[test]
    fn stream_normalization_like_property_check() {
        let value = "　吾輩\n\tは　 猫  ";

        assert_eq!(normalize_visible(value), "吾輩 は 猫");
        assert!(is_normalized_subsequence("吾輩 は", value));
        assert!(!is_normalized_subsequence("吾輩  は", "吾輩は"));
    }

    #[test]
    fn normalizes_nfkc_for_matching() {
        assert!(is_normalized_subsequence("ABC（1）", "ＡＢＣ(１)"));
        assert!(is_normalized_subsequence("ヽ", "ゝ"));
    }

    #[test]
    fn check_single_pass_does_not_alloc_huge_intermediates() {
        let source = "吾輩《わがはい》は猫である。\n今日も\n";
        let projected = "吾輩は猫である";

        let summary = super::check(source, projected);

        assert!(summary.source_visible_chars >= summary.projected_visible_chars);
        assert!(summary.in_source_order);
    }

    #[test]
    fn streams_normalized_whitespace_like_split_join() {
        let value = "  吾輩\n\tは  猫  ";

        assert_eq!(normalize_visible(value), "吾輩 は 猫");
        assert!(is_normalized_subsequence("吾輩 は", value));
        assert!(!is_normalized_subsequence("吾輩  は", "吾輩は"));
    }
}
