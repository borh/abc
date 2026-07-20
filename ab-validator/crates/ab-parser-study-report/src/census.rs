//! Pure completeness census derived from frozen study values and authenticated evidence.

use std::collections::BTreeSet;

use serde::Deserialize;
use thiserror::Error;

use crate::evidence::{AxisEvidenceSet, InputRole, RecordKey, RequiredInputState};
use crate::{Axis, Candidate, MeasurementMode};

/// One required third-party capture that remains absent.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct RequiredInputKey {
    pub candidate: Candidate,
    pub axis: Axis,
    pub measurement_mode: MeasurementMode,
    pub role: InputRole,
    pub artifact: String,
}

/// One row of the frozen comparison matrix and its required evidence states.
#[derive(Clone, Debug)]
pub struct CensusRow {
    key: RecordKey,
    required_inputs: Vec<RequiredInputState>,
    preregistered_build_failure: bool,
}

impl CensusRow {
    /// Frozen matrix key.
    pub const fn key(&self) -> RecordKey {
        self.key
    }

    /// Evidence requirements, present or explicitly absent.
    pub fn required_inputs(&self) -> &[RequiredInputState] {
        &self.required_inputs
    }

    /// Whether execution was replaced by the frozen build-failure evidence.
    pub const fn has_preregistered_build_failure(&self) -> bool {
        self.preregistered_build_failure
    }
}

/// Exact matrix plus mechanically derived execution debt.
#[derive(Clone, Debug)]
pub struct CompletenessCensus {
    rows: Vec<CensusRow>,
    capture_debt: Vec<RequiredInputKey>,
}

impl CompletenessCensus {
    /// Rows in frozen candidate, axis, and mode order.
    pub fn rows(&self) -> &[CensusRow] {
        &self.rows
    }

    /// Missing third-party captures only; prose cannot alter this set.
    pub fn capture_debt(&self) -> &[RequiredInputKey] {
        &self.capture_debt
    }
}

#[derive(Deserialize)]
struct Preregistration {
    study_id: String,
    candidates: Vec<PreregCandidate>,
    axes: Vec<PreregAxis>,
}

#[derive(Deserialize)]
struct PreregCandidate {
    id: Candidate,
    disposition: String,
}

#[derive(Deserialize)]
struct PreregAxis {
    id: Axis,
}

#[derive(Deserialize)]
struct AxisPolicy {
    policy_id: String,
    axes: Vec<PolicyAxis>,
}

#[derive(Deserialize)]
struct PolicyAxis {
    id: Axis,
    required_inputs: Vec<InputRequirement>,
}

#[derive(Clone, Deserialize)]
struct InputRequirement {
    role: InputRole,
    artifact: String,
}

/// Census derivation failure.
#[derive(Debug, Error)]
pub enum CensusError {
    #[error("invalid preregistration: {0}")]
    InvalidPreregistration(serde_json::Error),
    #[error("invalid axis policy: {0}")]
    InvalidPolicy(serde_json::Error),
    #[error("study or policy identity is not the frozen comparison")]
    IdentityMismatch,
    #[error("axis policy order does not match the preregistration")]
    AxisMismatch,
    #[error("candidate {0:?} has an unknown preregistered disposition")]
    UnknownDisposition(Candidate),
}

/// Derives all 108 rows and capture debt without consulting prose reasons.
pub fn derive_census(
    preregistration_json: &str,
    policy_json: &str,
    evidence: &AxisEvidenceSet,
) -> Result<CompletenessCensus, CensusError> {
    let prereg: Preregistration =
        serde_json::from_str(preregistration_json).map_err(CensusError::InvalidPreregistration)?;
    let policy: AxisPolicy =
        serde_json::from_str(policy_json).map_err(CensusError::InvalidPolicy)?;
    if prereg.study_id != "aozora-parser-neutral-comparison-2026-07"
        || policy.policy_id != "aozora-parser-neutral-comparison-axis-policy-v1"
    {
        return Err(CensusError::IdentityMismatch);
    }
    if prereg.axes.iter().map(|axis| axis.id).collect::<Vec<_>>()
        != policy.axes.iter().map(|axis| axis.id).collect::<Vec<_>>()
    {
        return Err(CensusError::AxisMismatch);
    }

    let mut rows = Vec::with_capacity(108);
    let mut debt = BTreeSet::new();
    for candidate in prereg.candidates {
        let modes: &[MeasurementMode] = match candidate.disposition.as_str() {
            "included" => &[MeasurementMode::Native, MeasurementMode::AdapterNormalized],
            "excluded" | "shared_instrument_appendix" => &[MeasurementMode::Native],
            _ => return Err(CensusError::UnknownDisposition(candidate.id)),
        };
        for axis in &policy.axes {
            for &measurement_mode in modes {
                let key = RecordKey {
                    candidate: candidate.id,
                    axis: axis.id,
                    measurement_mode,
                };
                let mut required_inputs = evidence.get(&key).map_or_else(
                    || {
                        axis.required_inputs
                            .iter()
                            .filter(|requirement| {
                                candidate.disposition != "excluded"
                                    || requirement.role != InputRole::ThirdPartyCapture
                            })
                            .map(|requirement| RequiredInputState::Absent {
                                role: requirement.role,
                                artifact: requirement.artifact.clone(),
                                reason:
                                    "required evidence is not present in the authenticated index"
                                        .to_owned(),
                            })
                            .collect()
                    },
                    |record| record.required_inputs.clone(),
                );
                if candidate.disposition == "excluded" {
                    required_inputs.retain(|input| input.role() != InputRole::ThirdPartyCapture);
                }
                if candidate.disposition != "excluded" {
                    for input in &required_inputs {
                        if input.role() == InputRole::ThirdPartyCapture && input.is_absent() {
                            debt.insert(RequiredInputKey {
                                candidate: candidate.id,
                                axis: axis.id,
                                measurement_mode,
                                role: input.role(),
                                artifact: input.artifact().to_owned(),
                            });
                        }
                    }
                }
                rows.push(CensusRow {
                    key,
                    required_inputs,
                    preregistered_build_failure: candidate.disposition == "excluded",
                });
            }
        }
    }
    Ok(CompletenessCensus {
        rows,
        capture_debt: debt.into_iter().collect(),
    })
}
