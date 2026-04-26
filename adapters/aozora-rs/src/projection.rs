#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ProjectionSummary {
    pub source_visible_chars: usize,
    pub projected_visible_chars: usize,
    pub in_source_order: bool,
}

pub fn check(validation_body: &str, projected_visible_text: &str) -> ProjectionSummary {
    let source = normalize_visible(&crate::source::source_visible_text(validation_body));
    let projected = normalize_visible(projected_visible_text);
    ProjectionSummary {
        source_visible_chars: source.chars().count(),
        projected_visible_chars: projected.chars().count(),
        in_source_order: projected.is_empty() || is_subsequence(&projected, &source),
    }
}

fn normalize_visible(value: &str) -> String {
    value.split_whitespace().collect::<Vec<_>>().join(" ")
}

fn is_subsequence(needle: &str, haystack: &str) -> bool {
    let mut haystack = haystack.chars();
    for ch in needle.chars() {
        if !haystack.any(|candidate| candidate == ch) {
            return false;
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn accepts_projected_text_in_source_order() {
        let summary = check("吾輩《わがはい》は猫である。", "吾輩は猫");
        assert!(summary.in_source_order);
        assert!(summary.source_visible_chars >= summary.projected_visible_chars);
    }

    #[test]
    fn rejects_projected_text_out_of_order() {
        let summary = check("吾輩は猫である。", "猫吾輩");
        assert!(!summary.in_source_order);
    }
}
