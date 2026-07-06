use std::ops::Range;

use serde::{Deserialize, Serialize};

/// Errors returned by [`OffsetMap::to_original`].
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum OrthoMapError {
    /// The normalized byte range crosses an OffsetMap entry boundary where
    /// byte-length changed (e.g. ヴ→う゛). Callers must split the span first.
    #[error(
        "normalized range {range:?} cannot be remapped: it crosses an OffsetMap entry boundary (byte {boundary}) or is a sub-span of a length-changing entry (e.g. ヴ→う゛); split the span at annotation boundaries first"
    )]
    CrossesBoundary {
        range: std::ops::Range<usize>,
        boundary: usize,
    },
    /// The normalized byte offset is not covered by any OffsetMap entry.
    #[error("normalized offset {offset} not covered by any OffsetMap entry")]
    UncoveredOffset { offset: usize },
}

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
    pub(crate) entries: Vec<(usize, usize, usize, usize)>,
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
    /// # Errors
    ///
    /// Returns [`OrthoMapError::CrossesBoundary`] if `norm_range` crosses an
    /// entry boundary where byte-length changed (e.g., ヴ→う゛) — callers
    /// must split the span at annotation boundaries first.
    /// Returns [`OrthoMapError::UncoveredOffset`] if either endpoint is not
    /// covered by any entry.
    pub fn to_original(&self, norm_range: Range<usize>) -> Result<Range<usize>, OrthoMapError> {
        if self.entries.is_empty() {
            return Ok(norm_range);
        }

        let start_entry = self.entries.iter().find(|&&(noff, _, nlen, _)| {
            norm_range.start >= noff && norm_range.start < noff + nlen
        });
        let start_entry = match start_entry {
            Some(e) => e,
            None => {
                return Err(OrthoMapError::UncoveredOffset {
                    offset: norm_range.start,
                });
            }
        };

        let end_entry = self
            .entries
            .iter()
            .find(|&&(noff, _, nlen, _)| norm_range.end > noff && norm_range.end <= noff + nlen);
        let end_entry = match end_entry {
            Some(e) => e,
            None => {
                return Err(OrthoMapError::UncoveredOffset {
                    offset: norm_range.end,
                });
            }
        };

        if start_entry != end_entry {
            // `start_entry.2` is the normalized length of the start entry;
            // together with its normalized offset it identifies where the
            // caller must split the span.
            let boundary = start_entry.0 + start_entry.2;
            return Err(OrthoMapError::CrossesBoundary {
                range: norm_range,
                boundary,
            });
        }

        let &(noff, ooff, nlen, olen) = start_entry;
        let delta = norm_range.start - noff;
        let norm_len = norm_range.end - norm_range.start;
        let orig_start = ooff + delta;
        if nlen == olen {
            // Identity entry: any sub-range maps 1:1 by byte offset.
            return Ok(orig_start..orig_start + norm_len);
        }
        // Length-changing entry (e.g. ヴ→う゛). Only a span that exactly
        // covers the entry has a clean original-doc equivalent; sub-spans
        // straddle a char boundary that was collapsed/expanded by the
        // normalization and cannot be remapped as a byte range.
        if norm_range.start == noff && norm_range.end == noff + nlen {
            return Ok(ooff..ooff + olen);
        }
        Err(OrthoMapError::CrossesBoundary {
            range: norm_range,
            boundary: noff + nlen,
        })
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
        assert_eq!(map.to_original(10..20).unwrap(), 10..20);
    }

    #[test]
    fn offset_map_maps_identity_region() {
        // normalized bytes 0..6 map 1:1 to original bytes 0..6
        let map = OffsetMap {
            entries: vec![(0, 0, 6, 6), (6, 6, 6, 3)],
        };
        assert_eq!(map.to_original(0..6).unwrap(), 0..6);
    }

    #[test]
    fn offset_map_contracts_byte_length() {
        // normalized bytes 6..12 (う゛, 6 bytes) → original bytes 6..9 (ヴ, 3 bytes)
        let map = OffsetMap {
            entries: vec![(0, 0, 6, 6), (6, 6, 6, 3)],
        };
        assert_eq!(map.to_original(6..12).unwrap(), 6..9);
    }

    #[test]
    fn offset_map_errors_on_cross_entry_span() {
        let map = OffsetMap {
            entries: vec![(0, 0, 6, 6), (6, 6, 6, 3)],
        };
        let err = map.to_original(3..9).unwrap_err(); // crosses the 0..6 / 6..12 boundary
        assert!(matches!(err, OrthoMapError::CrossesBoundary { .. }));
    }

    #[test]
    fn offset_map_errors_on_uncovered_offset() {
        let map = OffsetMap {
            entries: vec![(0, 0, 6, 6)],
        };
        let err = map.to_original(10..15).unwrap_err();
        assert!(matches!(err, OrthoMapError::UncoveredOffset { .. }));
    }

    #[test]
    fn detector_id_serializes() {
        let id = OrthoDetectorId::HeuristicV1;
        let json = serde_json::to_string(&id).unwrap();
        assert!(json.contains("HeuristicV1"));
    }
}
