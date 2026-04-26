#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectionSummary {
    pub source_visible_text: Option<String>,
    pub source_visible_chars: usize,
    pub projected_visible_chars: usize,
    pub in_source_order: bool,
}

pub fn check(validation_body: &str, projected_visible_text: &str) -> ProjectionSummary {
    let source_visible_text = crate::source::source_visible_text(validation_body).into_owned();
    let source = normalize_visible(&source_visible_text);
    let projected = normalize_visible(projected_visible_text);
    let source_visible_chars = source.chars().count();
    let projected_visible_chars = projected.chars().count();
    let in_source_order = if projected.is_empty() {
        true
    } else if projected_visible_chars > source_visible_chars {
        false
    } else {
        is_subsequence(&projected, &source)
    };
    ProjectionSummary {
        source_visible_text: if in_source_order {
            None
        } else {
            Some(source_visible_text)
        },
        source_visible_chars,
        projected_visible_chars,
        in_source_order,
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
}
