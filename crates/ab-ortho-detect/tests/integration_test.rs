//! Integration tests for the ortho-detect pipeline (kata→hira + OffsetMap
//! + HeuristicV1 cascade).
//!
//! Tests that require the Vibrato dictionary are marked `#[ignore]` and run
//! with `--ignored` when `AB_VIBRATO_DICT` is set.

use std::sync::Arc;

use ab_ortho_detect::heuristic::{HeuristicConfig, HeuristicV1};
use ab_ortho_detect::{
    OrthoAnnotation, OrthoDetector, OrthoNormalization, OrthoTokenizer, ortho_normalize,
};

/// A stub tokenizer that returns no tokens — exercises the character-level
/// cascade without needing a real dictionary.
struct StubTokenizer;
impl OrthoTokenizer for StubTokenizer {
    fn tokenize(&self, _text: &str) -> Vec<ab_ortho_detect::OrthoToken> {
        Vec::new()
    }
}

#[test]
fn kata_to_hira_roundtrip_preserves_meaning() {
    let original = "私ハ学生デアル";
    let normalized = ab_ortho_detect::script::kata_to_hira(original);
    assert_eq!(normalized, "私は学生である");
}

#[test]
fn kata_to_hira_passthrough_for_modern_text() {
    // Modern hiragana text must pass through unchanged.
    let original = "吾輩は猫である";
    let normalized = ab_ortho_detect::script::kata_to_hira(original);
    assert_eq!(normalized, original);
}

#[test]
fn offset_map_correct_for_full_sentence() {
    // Annotation covers the whole source string (a contraction-free kata→hira:
    // both "名前ハマダ無イ" and "名前はまだ無い" are 7 chars = 21 bytes).
    let annotations = vec![OrthoAnnotation {
        source_byte_range: 0..21,
        normalized_text: "名前はまだ無い".into(),
        kind: OrthoNormalization::ScriptKatakanaToHiragana,
        confidence: None,
    }];
    let original = "名前ハマダ無イ";
    let (normalized, map) = ortho_normalize(original, &annotations);
    assert_eq!(normalized, "名前はまだ無い");
    // Identity-length entry → full-range query returns 0..21.
    assert_eq!(map.to_original(0..21).unwrap(), 0..21);
}

#[test]
fn offset_map_preserves_unchanged_prefix() {
    // Unchanged prefix "吾輩は猫である。" (8 chars = 24 bytes), then
    // contract the katakana suffix "名前ハマダ無イ" (21 bytes) → "名前はまだ無い" (21 bytes).
    let annotations = vec![OrthoAnnotation {
        source_byte_range: 24..45,
        normalized_text: "名前はまだ無い".into(),
        kind: OrthoNormalization::ScriptKatakanaToHiragana,
        confidence: None,
    }];
    let original = "吾輩は猫である。名前ハマダ無イ";
    let (normalized, map) = ortho_normalize(original, &annotations);
    assert_eq!(normalized, "吾輩は猫である。名前はまだ無い");
    // Unchanged prefix (bytes 0..24) maps identity.
    assert_eq!(map.to_original(0..24).unwrap(), 0..24);
    // Annotation suffix (normalized bytes 24..45) maps to original bytes 24..45.
    assert_eq!(map.to_original(24..45).unwrap(), 24..45);
}

#[test]
fn cascade_rejects_hiragana_sentence() {
    let detector = HeuristicV1::new(Arc::new(StubTokenizer), HeuristicConfig::default());
    let sentences = ab_plaintext::sentence_split("吾輩は猫である。名前はまだ無い。");
    let annotations = detector.detect(&sentences);
    assert!(
        annotations.is_empty(),
        "modern hiragana text must not be normalized"
    );
}

#[test]
fn cascade_rejects_short_katakana_sentence() {
    let detector = HeuristicV1::new(Arc::new(StubTokenizer), HeuristicConfig::default());
    let sentences = ab_plaintext::sentence_split("猫ダ。");
    let annotations = detector.detect(&sentences);
    assert!(
        annotations.is_empty(),
        "short katakana sentence must be rejected"
    );
}

#[test]
fn cascade_accepts_full_katakana_prose() {
    let detector = HeuristicV1::new(Arc::new(StubTokenizer), HeuristicConfig::default());
    // "吾輩ハ猫デアル果テ" (no trailing terminal) is 9 chars: 5 katakana
    // (ハデアルテ) + 4 kanji (吾輩猫果), ratio ≈ 0.556 > 0.5, uniqueness 1.0,
    // no runs/bigram patterns, ≥8 chars, ends in non-terminal テ.
    // (The plan's draft with a trailing 。yielded 10 chars and ratio 0.5 == threshold,
    // which the cascade rejects at `katakana_ratio <= 0.5`; we omit the 。to match
    // the documented character analysis and to stay self-consistent.)
    let sentences = ab_plaintext::sentence_split("吾輩ハ猫デアル果テ");
    let annotations = detector.detect(&sentences);
    assert_eq!(annotations.len(), 1);
    assert_eq!(
        annotations[0].kind,
        OrthoNormalization::ScriptKatakanaToHiragana
    );
    assert_eq!(annotations[0].normalized_text, "吾輩は猫である果て");
}

#[test]
#[ignore = "requires Vibrato dictionary (set AB_VIBRATO_DICT)"]
fn detect_and_normalize_katakana_prose() {
    let vibrato = Arc::new(
        ab_morph_analyzers::VibratoAnalyzer::unidic_cwj_default()
            .expect("AB_VIBRATO_DICT must point at a unidic-cwj dictionary"),
    ) as Arc<dyn OrthoTokenizer>;
    let detector = HeuristicV1::new(vibrato, HeuristicConfig::default());
    // First two sentences are modern hiragana → rejected. The third sentence
    // uses pre-war katakana orthography for particles/copulas (ハ→は, ニ→に,
    // ッ→っ, テ→て, シ→し, イ→い, マ→ま, タ→た, ヨ→よ) and is katakana-dominant:
    // 11 katakana / 21 chars ≈ 0.524 > 0.5, all-distinct (unique ratio 19/21),
    // no char runs, no bigram repeats. (The plan's draft sentence
    // `私ハ毎日学校ヘ行ク。` has katakana ratio 3/10 = 0.3 and is rejected by
    // the cascade regardless of tokenizer; we substitute one that exercises
    // both the cascade acceptance and the proper-noun guard against `学校`,
    // which is `普通名詞` — not `固有名詞` — under Unidic-CWJ.)
    let text = "吾輩は猫である。名前はまだ無い。私ハ学校ニ毎日通ッテ、勉強シテイマシタヨ。";
    let sentences = ab_plaintext::sentence_split(text);
    let annotations = detector.detect(&sentences);
    assert!(
        annotations
            .iter()
            .any(|a| a.normalized_text == "私は学校に毎日通って、勉強していましたよ。"),
        "expected 私ハ学校ニ毎日通ッテ、勉強シテイマシタヨ。 to be normalized; got: {:?}",
        annotations
            .iter()
            .map(|a| &a.normalized_text)
            .collect::<Vec<_>>()
    );
}
