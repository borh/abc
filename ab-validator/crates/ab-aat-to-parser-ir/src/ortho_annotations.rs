use std::path::Path;

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Deserializer, Serialize, de};
use serde_json::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum OrthoCoordinateSystem {
    #[serde(rename = "decoded_utf8")]
    DecodedUtf8,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct OrthoAnnotationsBundle {
    pub work_id: String,
    pub primary_text_hash: String,
    pub coordinate_system: OrthoCoordinateSystem,
    pub detector_id: ab_ortho_detect::OrthoDetectorId,
    pub annotations: Vec<ab_ortho_detect::OrthoAnnotation>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct OrthoAnnotationsBundleWire {
    work_id: String,
    primary_text_hash: Option<String>,
    work_content_hash: Option<String>,
    coordinate_system: OrthoCoordinateSystem,
    detector_id: ab_ortho_detect::OrthoDetectorId,
    annotations: Vec<ab_ortho_detect::OrthoAnnotation>,
}

impl<'de> Deserialize<'de> for OrthoAnnotationsBundle {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let wire = OrthoAnnotationsBundleWire::deserialize(deserializer)?;
        let primary_text_hash = match (wire.primary_text_hash, wire.work_content_hash) {
            (Some(primary), Some(historical)) if primary != historical => {
                return Err(de::Error::custom(format!(
                    "orthographic_annotations primary_text_hash conflicts with historical work_content_hash: primary_text_hash={primary} work_content_hash={historical}"
                )));
            }
            (Some(primary), _) => primary,
            (None, Some(historical)) => historical,
            (None, None) => {
                return Err(de::Error::missing_field("primary_text_hash"));
            }
        };
        Ok(Self {
            work_id: wire.work_id,
            primary_text_hash,
            coordinate_system: wire.coordinate_system,
            detector_id: wire.detector_id,
            annotations: wire.annotations,
        })
    }
}

impl OrthoAnnotationsBundle {
    pub fn validate_against_aat(&self, aat: &Value) -> Result<()> {
        let aat_work_id = aat
            .get("work_id")
            .and_then(Value::as_str)
            .context("AAT missing string work_id")?;
        let aat_source_hash = aat
            .pointer("/meta/source_hash")
            .and_then(Value::as_str)
            .context("AAT missing string meta.source_hash")?;
        let aat_primary_text_hash = aat
            .pointer("/meta/primary_text_hash")
            .and_then(Value::as_str)
            .unwrap_or(aat_source_hash);
        if aat.pointer("/meta/primary_text_hash").is_some()
            && aat_primary_text_hash != aat_source_hash
        {
            bail!(
                "AAT primary_text_hash conflicts with historical source_hash alias: primary_text_hash={} source_hash={}",
                aat_primary_text_hash,
                aat_source_hash
            );
        }

        if self.work_id != aat_work_id {
            bail!(
                "orthographic_annotations work_id mismatch: bundle={} aat={}",
                self.work_id,
                aat_work_id
            );
        }
        if self.primary_text_hash != aat_primary_text_hash {
            bail!(
                "orthographic_annotations primary_text_hash mismatch: bundle={} aat={}",
                self.primary_text_hash,
                aat_primary_text_hash
            );
        }

        Ok(())
    }
}

pub fn read_ortho_annotations_bundle(path: &Path) -> Result<OrthoAnnotationsBundle> {
    let bytes = std::fs::read(path).with_context(|| {
        format!(
            "failed to read orthographic annotations file {}",
            path.display()
        )
    })?;
    serde_json::from_slice(&bytes).with_context(|| {
        format!(
            "failed to parse orthographic annotations file {}",
            path.display()
        )
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrips_through_json() {
        let input = serde_json::json!({
            "work_id": "000000",
            "primary_text_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
            "coordinate_system": "decoded_utf8",
            "detector_id": "HeuristicV1",
            "annotations": [
                {
                    "source_byte_range": { "start": 0, "end": 24 },
                    "normalized_text": "吾輩は猫である。",
                    "kind": "ScriptKatakanaToHiragana",
                    "confidence": null
                }
            ]
        });
        let parsed: OrthoAnnotationsBundle = serde_json::from_value(input.clone()).unwrap();
        assert_eq!(parsed.work_id, "000000");
        assert_eq!(parsed.coordinate_system, OrthoCoordinateSystem::DecodedUtf8);
        assert_eq!(
            parsed.detector_id,
            ab_ortho_detect::OrthoDetectorId::HeuristicV1
        );
        assert_eq!(parsed.annotations.len(), 1);
        assert_eq!(parsed.annotations[0].source_byte_range.start, 0);
        assert_eq!(parsed.annotations[0].source_byte_range.end, 24);
        assert_eq!(parsed.annotations[0].normalized_text, "吾輩は猫である。");

        let output = serde_json::to_value(&parsed).unwrap();
        assert_eq!(output, input);
    }

    #[test]
    fn roundtrips_ml_detector_id_shape() {
        let input = serde_json::json!({
            "work_id": "000000",
            "primary_text_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
            "coordinate_system": "decoded_utf8",
            "detector_id": {
                "MlLogisticRegression": {
                    "model_hash": "sha256:2222222222222222222222222222222222222222222222222222222222222222"
                }
            },
            "annotations": []
        });
        let parsed: OrthoAnnotationsBundle = serde_json::from_value(input.clone()).unwrap();
        assert!(matches!(
            parsed.detector_id,
            ab_ortho_detect::OrthoDetectorId::MlLogisticRegression { .. }
        ));
        assert_eq!(serde_json::to_value(&parsed).unwrap(), input);
    }

    #[test]
    fn historical_work_content_hash_migrates_to_canonical_primary_text_hash() {
        let parsed: OrthoAnnotationsBundle = serde_json::from_value(serde_json::json!({
            "work_id": "000000",
            "work_content_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
            "coordinate_system": "decoded_utf8",
            "detector_id": "HeuristicV1",
            "annotations": []
        }))
        .unwrap();

        let output = serde_json::to_value(parsed).unwrap();
        assert_eq!(
            output["primary_text_hash"],
            "sha256:1111111111111111111111111111111111111111111111111111111111111111"
        );
        assert!(output.get("work_content_hash").is_none());
    }

    #[test]
    fn conflicting_hash_aliases_are_rejected() {
        let error = serde_json::from_value::<OrthoAnnotationsBundle>(serde_json::json!({
            "work_id": "000000",
            "primary_text_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
            "work_content_hash": "sha256:2222222222222222222222222222222222222222222222222222222222222222",
            "coordinate_system": "decoded_utf8",
            "detector_id": "HeuristicV1",
            "annotations": []
        }))
        .unwrap_err();

        assert!(error.to_string().contains("primary_text_hash"), "{error}");
    }

    #[test]
    fn rejects_missing_identity_fields() {
        let input = serde_json::json!({
            "detector_id": "HeuristicV1",
            "annotations": []
        });
        let err = serde_json::from_value::<OrthoAnnotationsBundle>(input).unwrap_err();
        let text = err.to_string();
        assert!(text.contains("work_id") || text.contains("primary_text_hash"));
    }

    #[test]
    fn validates_identity_against_aat() {
        let bundle: OrthoAnnotationsBundle = serde_json::from_value(serde_json::json!({
            "work_id": "000000",
            "work_content_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
            "coordinate_system": "decoded_utf8",
            "detector_id": "HeuristicV1",
            "annotations": []
        }))
        .unwrap();
        let aat = serde_json::json!({
            "work_id": "000000",
            "meta": {
                "source_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111"
            }
        });

        bundle.validate_against_aat(&aat).unwrap();
    }

    #[test]
    fn rejects_mismatched_aat_identity() {
        let bundle: OrthoAnnotationsBundle = serde_json::from_value(serde_json::json!({
            "work_id": "000000",
            "work_content_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
            "coordinate_system": "decoded_utf8",
            "detector_id": "HeuristicV1",
            "annotations": []
        }))
        .unwrap();
        let aat = serde_json::json!({
            "work_id": "000001",
            "meta": {
                "source_hash": "sha256:2222222222222222222222222222222222222222222222222222222222222222"
            }
        });

        let err = bundle.validate_against_aat(&aat).unwrap_err().to_string();
        assert!(err.contains("work_id mismatch"));
    }
}
