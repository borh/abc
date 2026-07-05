use std::ops::Range;

use serde::{Deserialize, Serialize};

/// What kind of normalization was applied.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum OrthoNormalization {
    /// Mechanical kata→hira conversion. Bijection except for deliberate
    /// ヴ→う゛ decomposition (chosen for analyzer dictionary compatibility).
    ScriptKatakanaToHiragana,
    /// Historical kana → modern kana. Dictionary-backed. Not reversible.
    /// No detector emits this in v1; reserved for future use.
    HistoricalToModern,
}

/// A single normalized span with provenance.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OrthoAnnotation {
    /// Byte range in the original source text (sentence boundaries).
    pub source_byte_range: Range<usize>,
    /// The text after normalization.
    pub normalized_text: String,
    /// What kind of normalization was applied.
    pub kind: OrthoNormalization,
    /// Confidence percentage 0–100. None = heuristic (deterministic).
    pub confidence: Option<u8>,
}

/// Which detector produced the annotations.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum OrthoDetectorId {
    HeuristicV1,
    MlLogisticRegression {
        /// SHA-256 hex digest of serialized model weight bytes (little-endian f32).
        model_hash: String,
    },
}

/// Maps byte ranges from normalized text coordinates to original text
/// coordinates. Built by `ortho_normalize()` alongside the output string.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OffsetMap {
    /// Sorted, non-overlapping entries. Each entry maps a contiguous
    /// normalized-text byte range to the corresponding original-text range.
    /// Tuple: (normalized_byte_offset, original_byte_offset,
    ///         length_in_normalized, length_in_original).
    entries: Vec<(usize, usize, usize, usize)>,
}

impl OffsetMap {
    /// Create an empty OffsetMap (identity mapping).
    #[must_use]
    pub fn empty() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    /// Map a byte range in normalized text to the corresponding range in
    /// original text.
    ///
    /// # Panics
    ///
    /// Panics if `norm_range` crosses an entry boundary where byte-length
    /// changed (e.g., ヴ→う゛). Callers must split spans at annotation
    /// boundaries before calling this method.
    #[must_use]
    pub fn to_original(&self, norm_range: Range<usize>) -> Range<usize> {
        if self.entries.is_empty() {
            return norm_range;
        }

        let start_entry = self
            .entries
            .iter()
            .find(|&&(noff, _, nlen, _)| {
                norm_range.start >= noff && norm_range.start < noff + nlen
            })
            .unwrap_or_else(|| {
                panic!(
                    "normalized offset {} not covered by any OffsetMap entry",
                    norm_range.start
                )
            });

        let end_entry = self
            .entries
            .iter()
            .find(|&&(noff, _, nlen, _)| {
                norm_range.end > noff && norm_range.end <= noff + nlen
            })
            .unwrap_or_else(|| {
                panic!(
                    "normalized offset {} not covered by any OffsetMap entry",
                    norm_range.end
                )
            });

        assert_eq!(
            start_entry, end_entry,
            "norm_range {:?} crosses OffsetMap entry boundary; split span first",
            norm_range
        );

        let &(noff, ooff, nlen, olen) = start_entry;
        let delta = norm_range.start - noff;
        let norm_len = norm_range.end - norm_range.start;
        let orig_start = ooff + delta;
        // Sub-ranges are only valid within identity entries (nlen == olen);
        // for entries where byte-length changed, callers must query the
        // full entry range.
        let orig_len = if norm_len == nlen {
            olen
        } else {
            assert_eq!(
                nlen, olen,
                "cannot map sub-range of OffsetMap entry with differing byte lengths"
            );
            norm_len
        };
        orig_start..orig_start + orig_len
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ortho_annotation_derives_eq() {
        let a = OrthoAnnotation {
            source_byte_range: 0..5,
            normalized_text: "abc".into(),
            kind: OrthoNormalization::ScriptKatakanaToHiragana,
            confidence: None,
        };
        let b = a.clone();
        assert_eq!(a, b);
    }

    #[test]
    fn confidence_is_option_u8() {
        let a = OrthoAnnotation {
            source_byte_range: 0..3,
            normalized_text: "x".into(),
            kind: OrthoNormalization::ScriptKatakanaToHiragana,
            confidence: Some(95),
        };
        assert_eq!(a.confidence, Some(95));
        assert!(serde_json::to_string(&a).is_ok());
    }

    #[test]
    fn offset_map_empty_is_identity() {
        let map = OffsetMap::empty();
        assert_eq!(map.to_original(10..20), 10..20);
    }

    #[test]
    fn offset_map_maps_identity_region() {
        // normalized bytes 0..6 map 1:1 to original bytes 0..6
        let map = OffsetMap {
            entries: vec![(0, 0, 6, 6), (6, 6, 6, 3)],
        };
        assert_eq!(map.to_original(0..6), 0..6);
    }

    #[test]
    fn offset_map_contracts_byte_length() {
        // normalized bytes 6..12 (う゛, 6 bytes) → original bytes 6..9 (ヴ, 3 bytes)
        let map = OffsetMap {
            entries: vec![(0, 0, 6, 6), (6, 6, 6, 3)],
        };
        assert_eq!(map.to_original(6..12), 6..9);
    }

    #[test]
    #[should_panic(expected = "crosses OffsetMap entry boundary")]
    fn offset_map_panics_on_cross_entry_span() {
        let map = OffsetMap {
            entries: vec![(0, 0, 6, 6), (6, 6, 6, 3)],
        };
        let _ = map.to_original(3..9); // crosses the 0..6 / 6..12 boundary
    }

    #[test]
    #[should_panic(expected = "not covered by any OffsetMap entry")]
    fn offset_map_panics_on_uncovered_offset() {
        let map = OffsetMap {
            entries: vec![(0, 0, 6, 6)],
        };
        let _ = map.to_original(10..15);
    }

    #[test]
    fn detector_id_serializes() {
        let id = OrthoDetectorId::HeuristicV1;
        let json = serde_json::to_string(&id).unwrap();
        assert!(json.contains("HeuristicV1"));
    }
}
