/// Character-level features extracted in a single pass over a sentence.
/// No tokenization required.
#[derive(Debug, Clone, PartialEq)]
pub struct CharFeatures {
    pub total_chars: usize,
    pub hiragana_count: usize,
    pub katakana_count: usize,
    pub kanji_count: usize,
    pub hiragana_ratio: f64,
    pub katakana_ratio: f64,
    pub kanji_ratio: f64,
    pub unique_char_ratio: f64,
    pub max_bigram_repeat_ratio: f64,
    pub char_run_repeat_ratio: f64,
    pub repeated_bigram_pattern_ratio: f64,
    pub katakana_at_sentence_end: bool,
}

/// Extract character-level features from a single sentence.
#[must_use]
pub fn extract_char_features(text: &str) -> CharFeatures {
    let total_chars = text.chars().count();
    if total_chars == 0 {
        return CharFeatures {
            total_chars: 0,
            hiragana_count: 0,
            katakana_count: 0,
            kanji_count: 0,
            hiragana_ratio: 0.0,
            katakana_ratio: 0.0,
            kanji_ratio: 0.0,
            unique_char_ratio: 0.0,
            max_bigram_repeat_ratio: 0.0,
            char_run_repeat_ratio: 0.0,
            repeated_bigram_pattern_ratio: 0.0,
            katakana_at_sentence_end: false,
        };
    }

    let chars: Vec<char> = text.chars().collect();
    let total = total_chars as f64;

    let mut hiragana = 0usize;
    let mut katakana = 0usize;
    let mut kanji = 0usize;

    for ch in &chars {
        if is_hiragana(*ch) {
            hiragana += 1;
        } else if is_katakana(*ch) {
            katakana += 1;
        } else if is_kanji(*ch) {
            kanji += 1;
        }
    }

    // Unique char ratio
    let unique_count = {
        let mut sorted: Vec<char> = chars.clone();
        sorted.sort_unstable();
        sorted.dedup();
        sorted.len()
    };

    // Bigram repeat: count of most frequent adjacent character bigram
    let max_bigram_repeat = if total_chars >= 2 {
        let mut bigram_counts = std::collections::HashMap::new();
        for window in chars.windows(2) {
            *bigram_counts.entry((window[0], window[1])).or_insert(0usize) += 1;
        }
        bigram_counts.values().max().copied().unwrap_or(0)
    } else {
        0
    };

    // Character run repeat: count of runs of ≥2 identical chars (Python definition)
    let char_run_count = {
        let mut runs = 0usize;
        let mut i = 0usize;
        while i < chars.len() {
            let ch = chars[i];
            let mut run_len = 1usize;
            while i + run_len < chars.len() && chars[i + run_len] == ch {
                run_len += 1;
            }
            if run_len >= 2 {
                runs += 1;
            }
            i += run_len;
        }
        runs
    };

    // Repeated bigram pattern ratio: count of distinct bigrams that appear ≥2 times
    let repeated_bigram_pattern_count = if total_chars >= 2 {
        let mut bigram_counts = std::collections::HashMap::new();
        for window in chars.windows(2) {
            *bigram_counts.entry((window[0], window[1])).or_insert(0usize) += 1;
        }
        bigram_counts.values().filter(|&&c| c >= 2).count()
    } else {
        0
    };

    // Does the sentence end in katakana?
    let katakana_at_end = chars.last().is_some_and(|ch| is_katakana(*ch));

    CharFeatures {
        total_chars,
        hiragana_count: hiragana,
        katakana_count: katakana,
        kanji_count: kanji,
        hiragana_ratio: hiragana as f64 / total,
        katakana_ratio: katakana as f64 / total,
        kanji_ratio: kanji as f64 / total,
        unique_char_ratio: unique_count as f64 / total,
        max_bigram_repeat_ratio: max_bigram_repeat as f64 / total,
        char_run_repeat_ratio: char_run_count as f64 / total,
        repeated_bigram_pattern_ratio: repeated_bigram_pattern_count as f64 / total,
        katakana_at_sentence_end: katakana_at_end,
    }
}

/// Tokenization-derived features. Computed from a Vibrato/Sudachi first pass.
/// The `features` module only defines the struct; a higher layer populates it.
#[derive(Debug, Clone, Default)]
pub struct TokenFeatures {
    pub token_count: usize,
    pub oov_count: usize,
    pub oov_ratio: f64,
    pub proper_noun_char_ratio: f64,
}

fn is_hiragana(ch: char) -> bool {
    ('\u{3041}'..='\u{3096}').contains(&ch)
}

fn is_katakana(ch: char) -> bool {
    ('\u{30A1}'..='\u{30FA}').contains(&ch)
        || ('\u{31F0}'..='\u{31FF}').contains(&ch)
        || ('\u{FF65}'..='\u{FF9F}').contains(&ch)
}

fn is_kanji(ch: char) -> bool {
    ('\u{4E00}'..='\u{9FFF}').contains(&ch)
        || ('\u{3400}'..='\u{4DBF}').contains(&ch) // CJK Ext-A
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extracts_zero_for_empty() {
        let f = extract_char_features("");
        assert_eq!(f.total_chars, 0);
        assert_eq!(f.katakana_ratio, 0.0);
    }

    #[test]
    fn detects_mixed_script() {
        let f = extract_char_features("吾輩ハ猫デアル");
        assert_eq!(f.total_chars, 7);
        assert_eq!(f.hiragana_count, 0);
        assert_eq!(f.katakana_count, 4);
        assert_eq!(f.kanji_count, 3);
        assert!((f.katakana_ratio - 4.0 / 7.0).abs() < 0.01);
    }

    #[test]
    fn detects_hiragana_sentence() {
        let f = extract_char_features("これは猫です");
        assert!(f.hiragana_count > 0);
    }

    #[test]
    fn detects_char_runs() {
        let f = extract_char_features("アアアア");
        assert_eq!(f.total_chars, 4);
        assert!((f.char_run_repeat_ratio - 0.25).abs() < 0.01); // 1 run / 4 chars
    }

    #[test]
    fn detects_bigram_patterns() {
        let f = extract_char_features("ABABAB");
        assert!(f.repeated_bigram_pattern_ratio > 0.0);
    }

    #[test]
    fn detects_katakana_at_end() {
        let f = extract_char_features("これは猫ダ");
        assert!(f.katakana_at_sentence_end);
    }

    #[test]
    fn katakana_not_at_end() {
        let f = extract_char_features("ダという猫");
        assert!(!f.katakana_at_sentence_end);
    }
}
