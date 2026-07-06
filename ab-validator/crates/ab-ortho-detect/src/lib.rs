pub mod features;
pub mod heuristic;
pub mod ml;
pub mod script;
pub mod types;

pub use types::{OffsetMap, OrthoAnnotation, OrthoDetectorId, OrthoMapError, OrthoNormalization};

use ab_plaintext::SentenceSpan;

/// Minimal per-token summary returned by [`OrthoTokenizer`].
/// The detector only needs surface text and the `pos2` feature;
/// everything else is discarded to keep the boundary narrow.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OrthoToken {
    pub surface: String,
    /// Unidic `pos2` field, e.g. `"固有名詞"`. `None` if absent (`*`/empty).
    pub pos2: Option<String>,
}

/// Abstract first-pass tokenizer used by [`HeuristicV1`] for the
/// proper-noun guard. Defined here so `ab-ortho-detect` does not depend
/// on any concrete analyzer crate; adapters implement this in their own
/// crates (e.g. `ab-morph-analyzers` impls it for `VibratoAnalyzer`).
pub trait OrthoTokenizer: Send + Sync {
    /// Tokenize `text` and return `(surface, pos2)` for each morpheme.
    fn tokenize(&self, text: &str) -> Vec<OrthoToken>;
}

/// The detection trait. Decoupled from normalization application.
pub trait OrthoDetector: Send + Sync {
    fn detector_id(&self) -> OrthoDetectorId;
    fn detect(&self, sentences: &[SentenceSpan<'_>]) -> Vec<OrthoAnnotation>;
}

/// Apply annotations to produce a normalized text view and an OffsetMap.
/// Annotations must be sorted by source_byte_range and non-overlapping.
/// Gaps of unchanged text between annotations produce identity map entries.
#[must_use]
pub fn ortho_normalize(original: &str, annotations: &[OrthoAnnotation]) -> (String, OffsetMap) {
    if annotations.is_empty() {
        return (original.to_owned(), OffsetMap::empty());
    }

    let mut normalized = String::with_capacity(original.len());
    let mut entries: Vec<(usize, usize, usize, usize)> = Vec::new();
    let mut orig_cursor = 0usize;
    let mut norm_cursor = 0usize;

    for annotation in annotations {
        let span = &annotation.source_byte_range;

        if orig_cursor < span.start {
            let unchanged = &original[orig_cursor..span.start];
            normalized.push_str(unchanged);
            let len = unchanged.len();
            entries.push((norm_cursor, orig_cursor, len, len));
            norm_cursor += len;
        }
        orig_cursor = span.start;

        normalized.push_str(&annotation.normalized_text);
        let norm_len = annotation.normalized_text.len();
        let orig_len = span.end - span.start;
        entries.push((norm_cursor, orig_cursor, norm_len, orig_len));
        norm_cursor += norm_len;
        orig_cursor = span.end;
    }

    if orig_cursor < original.len() {
        let unchanged = &original[orig_cursor..];
        normalized.push_str(unchanged);
        let len = unchanged.len();
        entries.push((norm_cursor, orig_cursor, len, len));
    }

    (normalized, OffsetMap { entries })
}

#[cfg(test)]
mod tests {
    use super::*;
    use types::OrthoNormalization;

    fn ann(start: usize, end: usize, norm: &str) -> OrthoAnnotation {
        OrthoAnnotation {
            source_byte_range: start..end,
            normalized_text: norm.to_owned(),
            kind: OrthoNormalization::ScriptKatakanaToHiragana,
            confidence: None,
        }
    }

    #[test]
    fn empty_annotations_returns_original() {
        let (text, map) = ortho_normalize("hello", &[]);
        assert_eq!(text, "hello");
        assert!(map.is_empty());
    }

    #[test]
    fn normalizes_katakana_sentence() {
        let annotations = vec![ann(0, 21, "吾輩は猫である")];
        let original = "吾輩ハ猫デアル";
        let (text, map) = ortho_normalize(original, &annotations);
        assert_eq!(text, "吾輩は猫である");
        assert_eq!(map.to_original(0..text.len()).unwrap(), 0..original.len());
    }

    #[test]
    fn preserves_unchanged_text_between_annotations() {
        let annotations = vec![ann(24, 45, "名前はまだ無い")];
        let original = "吾輩は猫である。名前ハマダ無イ";
        let (text, _map) = ortho_normalize(original, &annotations);
        assert_eq!(text, "吾輩は猫である。名前はまだ無い");
    }

    #[test]
    fn offset_map_handles_vu_expansion() {
        let annotations = vec![ann(6, 9, "う゛")];
        let original = "今日ヴ";
        let (text, map) = ortho_normalize(original, &annotations);
        assert_eq!(text, "今日う゛");
        assert_eq!(map.to_original(0..6).unwrap(), 0..6);
        assert_eq!(map.to_original(6..12).unwrap(), 6..9);
    }
}
