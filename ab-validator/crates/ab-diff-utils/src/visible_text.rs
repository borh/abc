use crate::ComparisonToken;

#[must_use]
pub fn sentence_like_tokens(text: &str) -> Vec<ComparisonToken> {
    sentence_like_runs(text)
        .into_iter()
        .enumerate()
        .filter_map(|(ordinal, run)| {
            let normalized = remove_unicode_whitespace(&run);
            (!normalized.is_empty()).then_some(ComparisonToken {
                ordinal,
                text: run,
                normalized,
            })
        })
        .collect()
}

#[must_use]
pub fn sentence_like_runs(text: &str) -> Vec<String> {
    let mut runs = Vec::new();
    let mut current = String::new();
    let mut boundary_pending = false;
    for ch in text.chars() {
        if boundary_pending && !matches!(ch, '」' | '』' | '）' | '】' | '〉' | '》' | ')' | ']')
        {
            push_nonblank(&mut runs, &mut current);
            boundary_pending = false;
        }
        current.push(ch);
        if matches!(ch, '。' | '？' | '！' | '?' | '!') {
            boundary_pending = true;
        }
    }
    push_nonblank(&mut runs, &mut current);
    runs
}

#[must_use]
pub fn remove_unicode_whitespace(value: &str) -> String {
    value.chars().filter(|ch| !ch.is_whitespace()).collect()
}

fn push_nonblank(runs: &mut Vec<String>, current: &mut String) {
    if !remove_unicode_whitespace(current).is_empty() {
        runs.push(std::mem::take(current));
    } else {
        current.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sentence_like_runs_keep_closing_quotes_with_sentence() {
        assert_eq!(
            sentence_like_runs("一。「二。」三。"),
            vec!["一。", "「二。」", "三。"]
        );
    }

    #[test]
    fn sentence_like_tokens_strip_unicode_whitespace_for_matching() {
        let tokens = sentence_like_tokens("一。\n二 。");
        assert_eq!(tokens[1].text, "\n二 。");
        assert_eq!(tokens[1].normalized, "二。");
    }
}
