//! ML logistic-regression ortho detector.

use ab_plaintext::SentenceSpan;
use sha2::{Digest, Sha256};

use crate::features::{FEATURE_NAMES, extract_char_features, features_to_vector};
use crate::script::kata_to_hira;
use crate::types::{OrthoAnnotation, OrthoDetectorId, OrthoNormalization};

/// The on-disk model file shape. Serialized with bincode.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MlModel {
    /// Must equal FEATURE_NAMES at load time, else load fails (prevents silent misweight).
    pub feature_names: Vec<String>,
    /// One weight per feature (the trainer writes LE f32; stored as f32 for bandwidth).
    pub weights: Vec<f32>,
    pub intercept: f32,
    /// Decision threshold on the sigmoid output. Default 0.5.
    pub threshold: f32,
}

/// SHA-256 hex digest of the model's weight bytes in little-endian f32.
/// NOT a debug-format string. The digest covers
/// weights followed by intercept (both as LE f32); threshold is excluded
/// (it is a runtime knob, not a learned parameter).
#[must_use]
pub fn model_hash(model: &MlModel) -> String {
    let mut bytes = Vec::with_capacity(model.weights.len() * 4 + 4);
    for &w in &model.weights {
        bytes.extend_from_slice(&w.to_le_bytes());
    }
    bytes.extend_from_slice(&model.intercept.to_le_bytes());
    let digest = Sha256::digest(&bytes);
    format!("{:x}", digest)
}

/// Logistic regression ortho detector. Character-only by construction;
/// does NOT require an OrthoTokenizer (the ablation-winning shape).
#[derive(Debug)]
pub struct MlLogisticRegression {
    model: MlModel,
}

impl MlLogisticRegression {
    #[must_use]
    pub fn new(model: MlModel) -> Self {
        Self { model }
    }

    /// Load a model from a bincode file. Validates `feature_names` match
    /// `FEATURE_NAMES` (canonical order) — a mismatch is a hard error to
    /// prevent silently applying wrong weights to the wrong features.
    ///
    /// # Errors
    /// Returns `MlError::Io` on read failure, `MlError::Bincode` on decode
    /// failure, or `MlError::FeatureNameMismatch` if the model's feature
    /// names diverge from `FEATURE_NAMES`.
    pub fn load(path: &std::path::Path) -> Result<Self, MlError> {
        let bytes = std::fs::read(path).map_err(MlError::Io)?;
        let model: MlModel = bincode::deserialize(&bytes).map_err(MlError::Bincode)?;
        if model.feature_names != FEATURE_NAMES {
            return Err(MlError::FeatureNameMismatch {
                expected: FEATURE_NAMES.iter().map(|s| (*s).to_string()).collect(),
                got: model.feature_names,
            });
        }
        Ok(Self::new(model))
    }

    fn score(&self, features: &crate::features::CharFeatures) -> f64 {
        let v = features_to_vector(features);
        let mut z = self.model.intercept as f64;
        for (w, x) in self.model.weights.iter().zip(v.iter()) {
            z += (*w as f64) * x;
        }
        1.0 / (1.0 + (-z).exp())
    }
}

impl crate::OrthoDetector for MlLogisticRegression {
    fn detector_id(&self) -> OrthoDetectorId {
        OrthoDetectorId::MlLogisticRegression {
            model_hash: model_hash(&self.model),
        }
    }

    fn detect(&self, sentences: &[SentenceSpan<'_>]) -> Vec<OrthoAnnotation> {
        let mut out = Vec::new();
        for s in sentences {
            let f = extract_char_features(s.text);
            let p = self.score(&f);
            if p >= self.model.threshold as f64 {
                let normalized = kata_to_hira(s.text);
                let byte_end = s.byte_offset + s.text.len();
                out.push(OrthoAnnotation {
                    source_byte_range: s.byte_offset..byte_end,
                    normalized_text: normalized,
                    kind: OrthoNormalization::ScriptKatakanaToHiragana,
                    confidence: Some((p * 100.0).round().clamp(0.0, 100.0) as u8),
                });
            }
        }
        out
    }
}

#[derive(Debug, thiserror::Error)]
pub enum MlError {
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),
    #[error("bincode deserialize error: {0}")]
    Bincode(#[from] bincode::Error),
    #[error("feature name mismatch: expected {expected:?}, got {got:?}")]
    FeatureNameMismatch {
        expected: Vec<String>,
        got: Vec<String>,
    },
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::OrthoDetector;

    #[test]
    fn model_hash_is_hex_sha256_of_le_f32_weights() {
        let m = MlModel {
            feature_names: FEATURE_NAMES.iter().map(|s| (*s).to_string()).collect(),
            weights: vec![1.0_f32, 2.0, 3.0],
            intercept: 0.5_f32,
            threshold: 0.5,
        };
        let h = model_hash(&m);
        assert_eq!(h.len(), 64);
        assert!(
            h.chars()
                .all(|c| c.is_ascii_digit() || ('a'..='f').contains(&c))
        );
        // Deterministic across calls.
        assert_eq!(h, model_hash(&m));
    }

    #[test]
    fn ml_detector_accepts_katakana_with_positive_weights() {
        // Weight katakana_ratio high, rest zero; intercept chosen so ratio>0.5 → accept.
        let mut weights = vec![0.0; FEATURE_NAMES.len()];
        let kidx = FEATURE_NAMES
            .iter()
            .position(|n| *n == "katakana_ratio")
            .unwrap();
        weights[kidx] = 10.0;
        let m = MlModel {
            feature_names: FEATURE_NAMES.iter().map(|s| (*s).to_string()).collect(),
            weights,
            intercept: -5.0, // 0.5 ratio * 10 - 5 = 0 → p=0.5; >0.5 ratio → p>0.5 → accept
            threshold: 0.5,
        };
        let det = MlLogisticRegression::new(m);
        // "吾輩ハ猫デアル" — katakana_ratio 4/7 ≈ 0.571 > 0.5.
        let spans = ab_plaintext::sentence_split("吾輩ハ猫デアル");
        let anns = det.detect(&spans);
        assert!(
            !anns.is_empty(),
            "katakana-dominant sentence should be accepted"
        );
        assert!(
            anns[0].confidence.is_some(),
            "ML detector sets confidence (unlike heuristic None)"
        );
    }

    #[test]
    fn ml_detector_rejects_hiragana() {
        let mut weights = vec![0.0; FEATURE_NAMES.len()];
        let kidx = FEATURE_NAMES
            .iter()
            .position(|n| *n == "katakana_ratio")
            .unwrap();
        weights[kidx] = 10.0;
        let m = MlModel {
            feature_names: FEATURE_NAMES.iter().map(|s| (*s).to_string()).collect(),
            weights,
            intercept: -5.0,
            threshold: 0.5,
        };
        let det = MlLogisticRegression::new(m);
        // "吾輩は猫である" — katakana_ratio 0 → z = -5 → p ≈ 0.0067 < 0.5 → reject.
        let spans = ab_plaintext::sentence_split("吾輩は猫である");
        assert!(det.detect(&spans).is_empty());
    }

    #[test]
    fn load_rejects_feature_name_mismatch() {
        let m = MlModel {
            feature_names: vec!["wrong".to_string()],
            weights: vec![0.0],
            intercept: 0.0,
            threshold: 0.5,
        };
        let bytes = bincode::serialize(&m).unwrap();
        let tmp = std::env::temp_dir().join("ab-ortho-mismatch.bin");
        std::fs::write(&tmp, &bytes).unwrap();
        let err = MlLogisticRegression::load(&tmp).unwrap_err();
        assert!(matches!(err, MlError::FeatureNameMismatch { .. }));
        let _ = std::fs::remove_file(&tmp);
    }
}
