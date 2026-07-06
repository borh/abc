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
            *bigram_counts
                .entry((window[0], window[1]))
                .or_insert(0usize) += 1;
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

    // Repeated bigram pattern ratio: count of IMMEDIATE ABAB echoes
    // (optionally ッ-separated). Faithful port of the Python heuristic's
    // `len(re.findall(r"(..)ッ?\1", text))` — NOT "distinct bigrams appearing
    // >=2 times anywhere" (the earlier Rust definition was broader and caused
    // false rejections; see reports/ortho-detect/2026-07-05-phase2-recall-floor.md).
    let repeated_bigram_pattern_count = count_immediate_bigram_echoes(&chars);

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
    ('\u{4E00}'..='\u{9FFF}').contains(&ch) || ('\u{3400}'..='\u{4DBF}').contains(&ch) // CJK Ext-A
}

/// Count immediate ABAB-style bigram echoes (optionally ッ-separated), faithful
/// to the Python heuristic's `len(re.findall(r"(..)ッ?\1", text))`.
///
/// A match at position `i` is: chars[i..i+2] == chars[i+2..i+4] (no separator)
/// OR chars[i+2] == 'ッ' AND chars[i..i+2] == chars[i+3..i+5] (ッ-separated).
/// Matches are non-overlapping (advance past a match); this mirrors Python's
/// `re.findall` leftmost-non-overlapping semantics.
fn count_immediate_bigram_echoes(chars: &[char]) -> usize {
    let n = chars.len();
    if n < 4 {
        return 0;
    }
    let mut count = 0usize;
    let mut i = 0usize;
    while i + 4 <= n {
        let ab = (chars[i], chars[i + 1]);
        // No separator: AB AB (positions i, i+2).
        if i + 4 <= n && ab == (chars[i + 2], chars[i + 3]) {
            count += 1;
            i += 4;
            continue;
        }
        // ッ separator: AB ッ AB (positions i, i+2, i+3).
        if i + 5 <= n && chars[i + 2] == 'ッ' && ab == (chars[i + 3], chars[i + 4]) {
            count += 1;
            i += 5;
            continue;
        }
        i += 1;
    }
    count
}

/// Canonical feature vector order used by the ML trainer + runtime classifier.
/// The model's weight vector indexes match this order. DO NOT reorder without
/// retraining + rehashing (spec Decision #10 partitions the model hash by
/// this exact order).
pub const FEATURE_NAMES: &[&str] = &[
    "total_chars",
    "hiragana_ratio",
    "katakana_ratio",
    "kanji_ratio",
    "unique_char_ratio",
    "max_bigram_repeat_ratio",
    "char_run_repeat_ratio",
    "repeated_bigram_pattern_ratio",
    "katakana_at_sentence_end",
];

/// Project `CharFeatures` into the canonical ML feature vector.
/// `katakana_at_sentence_end` (bool) becomes 0.0/1.0 so the vector is `f64`-uniform.
#[must_use]
pub fn features_to_vector(f: &CharFeatures) -> Vec<f64> {
    vec![
        f.total_chars as f64,
        f.hiragana_ratio,
        f.katakana_ratio,
        f.kanji_ratio,
        f.unique_char_ratio,
        f.max_bigram_repeat_ratio,
        f.char_run_repeat_ratio,
        f.repeated_bigram_pattern_ratio,
        if f.katakana_at_sentence_end { 1.0 } else { 0.0 },
    ]
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
        // "ABABAB" — immediate ABAB echoes at positions 0 and 2.
        let f = extract_char_features("ABABAB");
        assert!(f.repeated_bigram_pattern_ratio > 0.0);
    }

    #[test]
    fn immediate_bigram_echo_counts_abab() {
        // ABAB = 1 echo (positions 0..2 == 2..4).
        let chars: Vec<char> = "ABAB".chars().collect();
        assert_eq!(count_immediate_bigram_echoes(&chars), 1);
        // ABABAB = 1 non-overlapping echo (advance by 4 after the first match).
        let chars: Vec<char> = "ABABAB".chars().collect();
        assert_eq!(count_immediate_bigram_echoes(&chars), 1);
    }

    #[test]
    fn immediate_bigram_echo_counts_tsu_separated() {
        // ABッAB = 1 ッ-separated echo.
        let chars: Vec<char> = "ABッAB".chars().collect();
        assert_eq!(count_immediate_bigram_echoes(&chars), 1);
    }

    #[test]
    fn immediate_bigram_echo_zero_for_non_repeating() {
        // ABXY — different bigrams, no immediate echo.
        let chars: Vec<char> = "ABXY".chars().collect();
        assert_eq!(count_immediate_bigram_echoes(&chars), 0);
        // AB AB with separator 、 (not ッ) — does NOT match.
        let chars2: Vec<char> = "AB、AB".chars().collect();
        assert_eq!(count_immediate_bigram_echoes(&chars2), 0);
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

    #[test]
    fn feature_vector_length_matches_names() {
        let f = extract_char_features("アアアア");
        assert_eq!(FEATURE_NAMES.len(), features_to_vector(&f).len());
    }

    #[test]
    fn boolean_feature_is_zero_or_one() {
        // "猫ダ" ends in katakana (ダ) → katakana_at_sentence_end == true.
        let f = extract_char_features("猫ダ");
        let v = features_to_vector(&f);
        let idx = FEATURE_NAMES
            .iter()
            .position(|n| *n == "katakana_at_sentence_end")
            .unwrap();
        assert!((v[idx] - 1.0).abs() < 1e-9);

        // "ダという猫" ends in kanji (猫) → false.
        let f2 = extract_char_features("ダという猫");
        let v2 = features_to_vector(&f2);
        assert!((v2[idx] - 0.0).abs() < 1e-9);
    }
}
