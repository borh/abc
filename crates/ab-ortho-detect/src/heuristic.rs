use std::sync::Arc;

use ab_plaintext::SentenceSpan;

use crate::features::{TokenFeatures, extract_char_features};
use crate::script::kata_to_hira;
use crate::types::{OrthoAnnotation, OrthoDetectorId, OrthoNormalization};
use crate::{OrthoDetector, OrthoTokenizer};

/// Configurable thresholds for the v1 heuristic.
#[derive(Debug, Clone)]
pub struct HeuristicConfig {
    pub min_total_chars: usize,
    pub short_sentence_max_chars: usize,
    pub long_sentence_min_chars: usize,
    pub katakana_ratio_threshold: f64,
    pub oov_ratio_threshold: f64,
    pub proper_noun_char_ratio_threshold: f64,
    pub unique_char_ratio_min: f64,
    pub max_bigram_repeat_ratio_max: f64,
    pub char_run_repeat_ratio_max: f64,
    pub repeated_bigram_ratio_max: f64,
}

impl Default for HeuristicConfig {
    fn default() -> Self {
        Self {
            min_total_chars: 8,
            short_sentence_max_chars: 10,
            long_sentence_min_chars: 100,
            katakana_ratio_threshold: 0.5,
            oov_ratio_threshold: 0.2,
            proper_noun_char_ratio_threshold: 0.3,
            unique_char_ratio_min: 0.5,
            max_bigram_repeat_ratio_max: 0.5,
            char_run_repeat_ratio_max: 0.1,
            repeated_bigram_ratio_max: 0.1,
        }
    }
}

/// V1 heuristic: two-pass detection based on the aozora-corpus-generator
/// `is_katakana_sentence` logic.
///
/// Branch B option (b): the OOV guard is dropped (Unidic-CWJ has dictionary
/// entries for katakana particles/copulas, so `LexType::Unknown` rarely fires
/// on this corpus — see reports/ortho-detect/2026-07-05-sudachi-baseline.md).
/// The proper-noun guard and character-level cascade remain.
pub struct HeuristicV1 {
    tokenizer: Arc<dyn OrthoTokenizer>,
    config: HeuristicConfig,
    sentence_end_re: regex::Regex,
}

impl HeuristicV1 {
    /// Create a new HeuristicV1 detector.
    /// `tokenizer` provides the first-pass `(surface, pos2)` stream for the
    /// proper-noun guard. Sentences accepted by the heuristic are re-tokenized
    /// with all analyzers on the normalized text (first-pass output is discarded).
    #[must_use]
    pub fn new(tokenizer: Arc<dyn OrthoTokenizer>, config: HeuristicConfig) -> Self {
        Self {
            tokenizer,
            config,
            sentence_end_re: regex::Regex::new(r"ッ?.?[？！]」$").unwrap(),
        }
    }
}

impl OrthoDetector for HeuristicV1 {
    fn detector_id(&self) -> OrthoDetectorId {
        OrthoDetectorId::HeuristicV1
    }

    fn detect(
        &self,
        sentences: &[SentenceSpan<'_>],
    ) -> Vec<OrthoAnnotation> {
        let mut annotations = Vec::new();

        for sentence in sentences {
            if self.should_normalize(sentence) {
                let normalized_text = kata_to_hira(sentence.text);
                let byte_end = sentence.byte_offset + sentence.text.len();
                annotations.push(OrthoAnnotation {
                    source_byte_range: sentence.byte_offset..byte_end,
                    normalized_text,
                    kind: OrthoNormalization::ScriptKatakanaToHiragana,
                    confidence: None,
                });
            }
        }

        annotations
    }
}

impl HeuristicV1 {
    fn should_normalize(&self, sentence: &SentenceSpan<'_>) -> bool {
        let features = extract_char_features(sentence.text);

        // Gate: must have zero hiragana
        if features.hiragana_count > 0 {
            return false;
        }

        // Gate: must be katakana-dominant
        if features.katakana_ratio <= self.config.katakana_ratio_threshold {
            return false;
        }

        // Pass 1: tokenization-derived signals (first-pass tokenizer)
        let token_features = self.first_pass_tokenize(sentence);

        // If all tokens are known and mostly proper nouns → list of names, not era-ortho
        // (Branch B: oov_count is always 0, so this reduces to the proper-noun check alone.)
        if token_features.oov_count == 0
            && token_features.proper_noun_char_ratio > self.config.proper_noun_char_ratio_threshold
        {
            return false;
        }

        // High OOV → likely garbage (always false in Branch B; kept for config parity)
        if token_features.oov_ratio > self.config.oov_ratio_threshold {
            return false;
        }

        // Pass 2: character-level rejection cascade

        // Too short
        if features.total_chars < self.config.min_total_chars {
            return false;
        }

        // Short sentence ending in terminal punctuation → exclamation, not prose
        if features.total_chars < self.config.short_sentence_max_chars {
            let last_3 = if sentence.text.len() >= 3 {
                &sentence.text[sentence.text.len() - 3..]
            } else {
                sentence.text
            };
            if self.sentence_end_re.is_match(last_3) {
                return false;
            }
        }

        // Longer sentence with low uniqueness → repetitive
        if features.total_chars < self.config.long_sentence_min_chars
            && features.unique_char_ratio < self.config.unique_char_ratio_min
        {
            return false;
        }

        // Stuttering
        if features.max_bigram_repeat_ratio > self.config.max_bigram_repeat_ratio_max {
            return false;
        }

        // Character runs
        if features.char_run_repeat_ratio > self.config.char_run_repeat_ratio_max {
            return false;
        }

        // Patterned repetition
        if features.repeated_bigram_pattern_ratio > self.config.repeated_bigram_ratio_max {
            return false;
        }

        true
    }

    /// First-pass tokenization to compute proper-noun coverage.
    ///
    /// Branch B option (b): `oov_count` is always 0 (the guard is dropped;
    /// see `reports/ortho-detect/2026-07-05-sudachi-baseline.md`).
    fn first_pass_tokenize(&self, sentence: &SentenceSpan<'_>) -> TokenFeatures {
        let tokens = self.tokenizer.tokenize(sentence.text);

        let token_count = tokens.len();

        let proper_noun_chars: usize = tokens
            .iter()
            .filter(|tok| tok.pos2.as_deref() == Some("固有名詞"))
            .map(|tok| tok.surface.chars().count())
            .sum();

        let total_chars = sentence.text.chars().count();

        TokenFeatures {
            token_count,
            oov_count: 0,
            oov_ratio: 0.0,
            proper_noun_char_ratio: if total_chars > 0 {
                proper_noun_chars as f64 / total_chars as f64
            } else {
                0.0
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::OrthoToken;
    use ab_plaintext::SentenceSpan;

    fn make_span(text: &str) -> SentenceSpan<'_> {
        SentenceSpan {
            text,
            byte_offset: 0,
            char_offset: 0,
        }
    }

    /// A stub tokenizer that returns no tokens — lets us exercise the
    /// character-level cascade without a real dictionary.
    struct StubTokenizer;
    impl OrthoTokenizer for StubTokenizer {
        fn tokenize(&self, _text: &str) -> Vec<OrthoToken> {
            Vec::new()
        }
    }

    #[test]
    fn config_defaults_match_spec() {
        let config = HeuristicConfig::default();
        assert_eq!(config.min_total_chars, 8);
        assert_eq!(config.katakana_ratio_threshold, 0.5);
        assert_eq!(config.oov_ratio_threshold, 0.2);
        assert_eq!(config.proper_noun_char_ratio_threshold, 0.3);
    }

    #[test]
    fn rejects_hiragana_sentence_without_dictionary() {
        let detector = HeuristicV1::new(
            Arc::new(StubTokenizer),
            HeuristicConfig::default(),
        );
        let annotations = detector.detect(&[make_span("これは猫です")]);
        assert!(annotations.is_empty(), "hiragana sentence must not be normalized");
    }

    #[test]
    fn rejects_short_sentence_without_dictionary() {
        let detector = HeuristicV1::new(
            Arc::new(StubTokenizer),
            HeuristicConfig::default(),
        );
        let annotations = detector.detect(&[make_span("猫ダ")]);
        assert!(annotations.is_empty(), "3-char sentence must be rejected");
    }

    #[test]
    fn rejects_character_runs_without_dictionary() {
        let detector = HeuristicV1::new(
            Arc::new(StubTokenizer),
            HeuristicConfig::default(),
        );
        let annotations = detector.detect(&[make_span("アアアアアアアア")]);
        assert!(annotations.is_empty(), "stuttering must be rejected");
    }

    #[test]
    fn accepts_katakana_prose_without_dictionary() {
        let detector = HeuristicV1::new(
            Arc::new(StubTokenizer),
            HeuristicConfig::default(),
        );
        // 吾輩ハ猫デアル果テ: 9 chars, 5 katakana (ハデアルテ) + 4 kanji (吾輩猫果),
        // ratio 5/9 ≈ 0.556 > 0.5. No hiragana, ≥8 chars, uniqueness 9/9 = 1.0,
        // max_bigram_repeat 1/9, char_run_repeat 0/9, repeated_bigram_pattern 0/9,
        // does not end in [？！]」. Passes every gate.
        // (Original draft sentence 私ハ毎日学校ヘ行ク had ratio 3/9 < 0.5 and was
        // rejected; this sentence is the plan-author's documented substitution.)
        let spans = &[make_span("吾輩ハ猫デアル果テ")];
        let annotations = detector.detect(spans);
        assert!(
            !annotations.is_empty(),
            "katakana-dominant prose sentence should be accepted"
        );
    }

    /// Compile-time assertion: `dyn OrthoTokenizer` is `Send + Sync`,
    /// so `Arc<dyn OrthoTokenizer>` is too (the trait bound requires it).
    #[test]
    fn ortho_tokenizer_dyn_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Box<dyn OrthoTokenizer>>();
    }
}
