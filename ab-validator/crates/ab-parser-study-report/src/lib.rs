//! Validated machine-readable rows for the neutral parser comparison.

use std::collections::BTreeSet;

use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Frozen candidate identity.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum Candidate {
    Aozora,
    Aozora2,
    AozoraRs,
    Aozora2html,
    AozoraEpub3,
    #[serde(rename = "aozora-parser.js")]
    AozoraParserJs,
    AbAozora,
}

impl Candidate {
    /// Candidate order frozen by the preregistration.
    pub const ALL: [Self; 7] = [
        Self::Aozora,
        Self::Aozora2,
        Self::AozoraRs,
        Self::Aozora2html,
        Self::AozoraEpub3,
        Self::AozoraParserJs,
        Self::AbAozora,
    ];
}

/// Frozen comparison axis.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Axis {
    ConstructCoverage,
    Fidelity,
    Robustness,
    Diagnostics,
    Spans,
    Performance,
    Maintenance,
    Packaging,
    License,
}

impl Axis {
    /// Axis order frozen by the preregistration.
    pub const ALL: [Self; 9] = [
        Self::ConstructCoverage,
        Self::Fidelity,
        Self::Robustness,
        Self::Diagnostics,
        Self::Spans,
        Self::Performance,
        Self::Maintenance,
        Self::Packaging,
        Self::License,
    ];
}

/// Lane in which the observation was produced.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum MeasurementMode {
    Native,
    AdapterNormalized,
}

/// A stage that materially produced the reported observation.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ProvenanceStage {
    NativeParser,
    Adapter,
    SourceLexerFallback,
    Mapper,
    SourceAuthority,
}

/// Whether an axis produced a measurement.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum RowStatus {
    Measured,
    Failed,
    NonComparable,
}

/// Explicit reason a value is absent.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum Missingness {
    None,
    NotApplicable,
    Unavailable,
    BuildFailure,
    RunFailure,
    Timeout,
    NonComparable,
}

/// One candidate, axis, and measurement-lane result.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ResultRow {
    pub candidate: Candidate,
    pub axis: Axis,
    pub measurement_mode: MeasurementMode,
    pub parser_revision: String,
    pub adapter_revision: Option<String>,
    pub corpus_hash: String,
    pub status: RowStatus,
    pub numerator: Option<u64>,
    pub denominator: Option<u64>,
    pub missingness: Missingness,
    pub caveats: Vec<String>,
    pub provenance: Vec<ProvenanceStage>,
}

impl ResultRow {
    /// Constructs and validates a result row.
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        candidate: Candidate,
        axis: Axis,
        measurement_mode: MeasurementMode,
        parser_revision: impl Into<String>,
        adapter_revision: Option<String>,
        corpus_hash: impl Into<String>,
        status: RowStatus,
        numerator: Option<u64>,
        denominator: Option<u64>,
        missingness: Missingness,
        caveats: Vec<String>,
        provenance: Vec<ProvenanceStage>,
    ) -> Result<Self, ContractError> {
        let row = Self {
            candidate,
            axis,
            measurement_mode,
            parser_revision: parser_revision.into(),
            adapter_revision,
            corpus_hash: corpus_hash.into(),
            status,
            numerator,
            denominator,
            missingness,
            caveats,
            provenance,
        };
        row.validate()?;
        Ok(row)
    }

    /// Checks attribution, count, provenance, and missingness invariants.
    pub fn validate(&self) -> Result<(), ContractError> {
        if self.parser_revision.is_empty()
            || !valid_sha256(&self.corpus_hash)
            || self.caveats.is_empty()
            || self.caveats.iter().any(String::is_empty)
            || self.provenance.is_empty()
        {
            return Err(ContractError::MissingRequiredProvenance);
        }
        if self.measurement_mode == MeasurementMode::Native
            && self.provenance.iter().any(|stage| {
                matches!(
                    stage,
                    ProvenanceStage::Adapter
                        | ProvenanceStage::SourceLexerFallback
                        | ProvenanceStage::Mapper
                )
            })
        {
            return Err(ContractError::AdaptedClaimedNative);
        }
        if self.measurement_mode == MeasurementMode::AdapterNormalized
            && self.adapter_revision.as_deref().is_none_or(str::is_empty)
        {
            return Err(ContractError::MissingAdapterRevision);
        }
        match self.status {
            RowStatus::Measured => {
                if self.missingness != Missingness::None
                    || self.numerator.is_none()
                    || self.denominator.is_none()
                    || self.numerator > self.denominator
                {
                    return Err(ContractError::InvalidCounts);
                }
            }
            RowStatus::Failed | RowStatus::NonComparable => {
                if self.missingness == Missingness::None
                    || self.numerator.is_some()
                    || self.denominator.is_some()
                {
                    return Err(ContractError::InvalidMissingness);
                }
            }
        }
        Ok(())
    }
}

fn valid_sha256(value: &str) -> bool {
    value.strip_prefix("sha256:").is_some_and(|digest| {
        digest.len() == 64
            && digest
                .bytes()
                .all(|byte| byte.is_ascii_hexdigit() && !byte.is_ascii_uppercase())
    })
}

/// Complete report result matrix.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct StudyReport {
    pub schema_version: u8,
    pub study_id: String,
    pub rows: Vec<ResultRow>,
}

impl StudyReport {
    /// Constructs a report only when every frozen candidate-axis pair is present.
    pub fn new(study_id: impl Into<String>, rows: Vec<ResultRow>) -> Result<Self, ContractError> {
        let study_id = study_id.into();
        if study_id.is_empty() {
            return Err(ContractError::MissingStudyIdentity);
        }
        for row in &rows {
            row.validate()?;
        }
        let present: BTreeSet<_> = rows.iter().map(|row| (row.candidate, row.axis)).collect();
        let required: BTreeSet<_> = Candidate::ALL
            .into_iter()
            .flat_map(|candidate| Axis::ALL.into_iter().map(move |axis| (candidate, axis)))
            .collect();
        if present != required {
            return Err(ContractError::IncompleteMatrix);
        }
        Ok(Self {
            schema_version: 1,
            study_id,
            rows,
        })
    }
}

/// Report-contract validation failure.
#[derive(Clone, Copy, Debug, Error, Eq, PartialEq)]
pub enum ContractError {
    #[error("adapter, mapper, or fallback output cannot be attributed as native")]
    AdaptedClaimedNative,
    #[error("adapter-normalized rows require an adapter revision")]
    MissingAdapterRevision,
    #[error("required identity, caveat, or provenance is absent")]
    MissingRequiredProvenance,
    #[error("measured rows require exact valid counts and no missingness")]
    InvalidCounts,
    #[error("unmeasured rows require explicit missingness and absent counts")]
    InvalidMissingness,
    #[error("report must contain every frozen candidate-axis pair")]
    IncompleteMatrix,
    #[error("report requires a study identity")]
    MissingStudyIdentity,
}
