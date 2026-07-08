//! Normalization-policy identity (Issue 2, F3 / P1).
//!
//! A [`NormalizationPolicy`] is the structured, canonicalized descriptor of the
//! orthographic normalization applied to analyzer input. Its content hash
//! ([`NormalizationPolicy::policy_hash`]) is the single identity for an
//! ortho-normalized input view: the tokenizer profile *declares* the expected
//! hash and the run *records* the applied hash; a run is valid iff they agree.
//!
//! **Producer/consumer split (U1 / I2-D8).** All compute-heavy work stays in
//! Rust: this crate is the sole producer of the policy hash. ABC reads the
//! hash opaquely — it never recomputes normalization nor re-derives the hash.
//!
//! **Canonicalization.** The hash is SHA-256 over a JCS canonicalization of the
//! descriptor (RFC 8785 subset: object keys sorted, compact separators, UTF-8),
//! mirroring `abc.tools.jcs` so the discipline matches `tokenizer-profile-hash`.
//! The descriptor deliberately contains only strings and string arrays with
//! slash-free ASCII identifiers, so the escaping-sensitive corners of RFC 8785
//! (numbers, `/`, non-ASCII) never fire and the two implementations would agree
//! byte-for-byte if ABC ever needed to verify.

use serde::Serialize;
use sha2::{Digest, Sha256};

use crate::types::{OrthoDetectorId, OrthoNormalization};

/// Version tag for the descriptor shape. Bump only on a breaking shape change
/// (which changes every hash) — new detectors/kinds do not bump it.
pub const POLICY_SCHEMA_VERSION: &str = "ortho-input-normalization-v1";
/// Algorithm tag for the span-scoped, detector-driven ortho-normalize path.
pub const ALGORITHM_ORTHO_NORMALIZE_V1: &str = "ortho-normalize-v1";
/// Algorithm tag for the no-op (source-identity) sentinel policy.
pub const ALGORITHM_IDENTITY: &str = "identity";
/// The output coordinate contract: output spans stay in source coordinates via
/// the offset-map remap. Invariant across every policy in this family.
pub const COORDINATE_SYSTEM: &str = "source-preserving-remap";

/// A structured, hashable description of the input-normalization policy.
///
/// Build via [`NormalizationPolicy::identity`] (no normalization) or
/// [`NormalizationPolicy::ortho_normalize_v1`] (span-scoped detector-driven
/// kata→hira). The canonical serialized form is stable and documented; changing
/// it changes every downstream hash and is a breaking change.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct NormalizationPolicy {
    policy_schema_version: &'static str,
    algorithm: &'static str,
    /// The detector that produced the normalization spans, or `None` for the
    /// identity sentinel.
    detector: Option<DetectorDescriptor>,
    /// Normalization kinds the policy applies, in a stable declared order.
    kinds: Vec<OrthoNormalization>,
    coordinate_system: &'static str,
}

/// Serialized detector identity nested inside a policy descriptor.
///
/// Mirrors [`OrthoDetectorId`] but is defined here so the canonical descriptor
/// shape is owned by this module and independent of the wire form of the
/// detector enum elsewhere.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
struct DetectorDescriptor {
    detector_id: OrthoDetectorId,
}

impl NormalizationPolicy {
    /// The identity (no-op) sentinel: analyzer input is the source text verbatim.
    /// Its [`policy_hash`](Self::policy_hash) is the default carried by
    /// source-identity runs.
    #[must_use]
    pub fn identity() -> Self {
        Self {
            policy_schema_version: POLICY_SCHEMA_VERSION,
            algorithm: ALGORITHM_IDENTITY,
            detector: None,
            kinds: Vec::new(),
            coordinate_system: COORDINATE_SYSTEM,
        }
    }

    /// A span-scoped, detector-driven ortho-normalization policy. `kinds` are
    /// the normalization kinds the run applies (v1 emits only
    /// [`OrthoNormalization::ScriptKatakanaToHiragana`]); they are recorded in
    /// the given order.
    #[must_use]
    pub fn ortho_normalize_v1(
        detector_id: OrthoDetectorId,
        kinds: Vec<OrthoNormalization>,
    ) -> Self {
        Self {
            policy_schema_version: POLICY_SCHEMA_VERSION,
            algorithm: ALGORITHM_ORTHO_NORMALIZE_V1,
            detector: Some(DetectorDescriptor { detector_id }),
            kinds,
            coordinate_system: COORDINATE_SYSTEM,
        }
    }

    /// `true` for the identity sentinel (no normalization applied).
    #[must_use]
    pub fn is_identity(&self) -> bool {
        self.algorithm == ALGORITHM_IDENTITY
    }

    /// The descriptor as a JSON value (for provenance persistence / audit).
    ///
    /// # Panics
    ///
    /// Never in practice: the descriptor contains only strings, string arrays,
    /// and nested objects, all of which serialize infallibly.
    #[must_use]
    pub fn descriptor_json(&self) -> serde_json::Value {
        serde_json::to_value(self).expect("NormalizationPolicy serializes infallibly")
    }

    /// Canonical JCS serialization of the descriptor (RFC 8785 subset).
    #[must_use]
    pub fn canonical_json(&self) -> String {
        canonical_json_string(&self.descriptor_json())
    }

    /// The content identity: `sha256:<hex>` of the canonical descriptor bytes.
    #[must_use]
    pub fn policy_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.canonical_json().as_bytes());
        format!("sha256:{:x}", hasher.finalize())
    }
}

/// RFC 8785 (JCS) canonicalization for the value shapes used by policy
/// descriptors: objects (keys sorted), arrays (order preserved), strings,
/// numbers, booleans, null. Mirrors `abc.tools.jcs/canonical-json-string`.
fn canonical_json_string(value: &serde_json::Value) -> String {
    match value {
        serde_json::Value::Null => "null".to_owned(),
        serde_json::Value::Bool(b) => {
            if *b {
                "true".to_owned()
            } else {
                "false".to_owned()
            }
        }
        // serde_json escapes strings per the JSON standard (and, like ES6/JCS,
        // does NOT escape `/`); our ASCII identifiers need no escaping anyway.
        serde_json::Value::String(_) | serde_json::Value::Number(_) => value.to_string(),
        serde_json::Value::Array(items) => {
            let inner = items
                .iter()
                .map(canonical_json_string)
                .collect::<Vec<_>>()
                .join(",");
            format!("[{inner}]")
        }
        serde_json::Value::Object(map) => {
            let mut keys: Vec<&String> = map.keys().collect();
            keys.sort();
            let inner = keys
                .into_iter()
                .map(|k| {
                    let key = serde_json::Value::String(k.clone()).to_string();
                    format!("{key}:{}", canonical_json_string(&map[k]))
                })
                .collect::<Vec<_>>()
                .join(",");
            format!("{{{inner}}}")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn identity_canonical_form_is_sorted_and_compact() {
        let policy = NormalizationPolicy::identity();
        // Keys sorted: algorithm, coordinate_system, detector, kinds,
        // policy_schema_version.
        assert_eq!(
            policy.canonical_json(),
            r#"{"algorithm":"identity","coordinate_system":"source-preserving-remap","detector":null,"kinds":[],"policy_schema_version":"ortho-input-normalization-v1"}"#
        );
    }

    #[test]
    fn identity_hash_is_stable() {
        // Pin the identity sentinel hash: a change here changes every
        // source-identity run's recorded policy and is a breaking change.
        assert_eq!(
            NormalizationPolicy::identity().policy_hash(),
            "sha256:530c59689dd909c171790036cddc7916f8685897b6342bfa794d4611816d3813"
        );
    }

    #[test]
    fn heuristic_v1_hash_is_stable_and_differs_from_identity() {
        let policy = NormalizationPolicy::ortho_normalize_v1(
            OrthoDetectorId::HeuristicV1,
            vec![OrthoNormalization::ScriptKatakanaToHiragana],
        );
        assert!(!policy.is_identity());
        assert_ne!(
            policy.policy_hash(),
            NormalizationPolicy::identity().policy_hash()
        );
        // Pinned agreed constant (Rust is the sole producer; ABC reads it).
        assert_eq!(
            policy.policy_hash(),
            "sha256:1670ff1d5ff27575dc63ffd448cb140b3d497247bb7b36e1b4e2f1623aa0fa2c"
        );
        assert_eq!(
            policy.canonical_json(),
            r#"{"algorithm":"ortho-normalize-v1","coordinate_system":"source-preserving-remap","detector":{"detector_id":"HeuristicV1"},"kinds":["ScriptKatakanaToHiragana"],"policy_schema_version":"ortho-input-normalization-v1"}"#
        );
    }

    #[test]
    fn ml_detector_hash_binds_model_hash() {
        let a = NormalizationPolicy::ortho_normalize_v1(
            OrthoDetectorId::MlLogisticRegression {
                model_hash: "sha256:aaaa".to_owned(),
            },
            vec![OrthoNormalization::ScriptKatakanaToHiragana],
        );
        let b = NormalizationPolicy::ortho_normalize_v1(
            OrthoDetectorId::MlLogisticRegression {
                model_hash: "sha256:bbbb".to_owned(),
            },
            vec![OrthoNormalization::ScriptKatakanaToHiragana],
        );
        // Different model bytes → different policy identity.
        assert_ne!(a.policy_hash(), b.policy_hash());
        assert!(a.canonical_json().contains(
            r#""detector":{"detector_id":{"MlLogisticRegression":{"model_hash":"sha256:aaaa"}}}"#
        ));
    }

    #[test]
    fn kind_order_is_significant() {
        // Declared order is part of the identity (two kinds; reserved kind
        // included here only to exercise ordering).
        let ab = NormalizationPolicy::ortho_normalize_v1(
            OrthoDetectorId::HeuristicV1,
            vec![
                OrthoNormalization::ScriptKatakanaToHiragana,
                OrthoNormalization::HistoricalToModern,
            ],
        );
        let ba = NormalizationPolicy::ortho_normalize_v1(
            OrthoDetectorId::HeuristicV1,
            vec![
                OrthoNormalization::HistoricalToModern,
                OrthoNormalization::ScriptKatakanaToHiragana,
            ],
        );
        assert_ne!(ab.policy_hash(), ba.policy_hash());
    }
}
