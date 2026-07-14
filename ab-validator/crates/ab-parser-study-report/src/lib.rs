//! Validated machine-readable rows for the neutral parser comparison.

use std::collections::BTreeSet;

use serde::{Deserialize, Deserializer, Serialize};
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

    /// Measurement lanes required by the candidate's frozen disposition.
    pub const fn required_modes(self) -> &'static [MeasurementMode] {
        match self {
            Self::Aozora
            | Self::Aozora2
            | Self::AozoraRs
            | Self::Aozora2html
            | Self::AozoraEpub3 => &MeasurementMode::ALL,
            Self::AozoraParserJs | Self::AbAozora => &MeasurementMode::NATIVE_ONLY,
        }
    }
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
#[derive(Clone, Copy, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum MeasurementMode {
    Native,
    AdapterNormalized,
}

impl MeasurementMode {
    const ALL: [Self; 2] = [Self::Native, Self::AdapterNormalized];
    const NATIVE_ONLY: [Self; 1] = [Self::Native];
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
#[derive(Clone, Debug, Serialize)]
pub struct ResultRow {
    candidate: Candidate,
    axis: Axis,
    measurement_mode: MeasurementMode,
    parser_revision: String,
    adapter_revision: Option<String>,
    corpus_hash: String,
    status: RowStatus,
    numerator: Option<u64>,
    denominator: Option<u64>,
    missingness: Missingness,
    caveats: Vec<String>,
    provenance: Vec<ProvenanceStage>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct UncheckedResultRow {
    candidate: Candidate,
    axis: Axis,
    measurement_mode: MeasurementMode,
    parser_revision: String,
    adapter_revision: Option<String>,
    corpus_hash: String,
    status: RowStatus,
    numerator: Option<u64>,
    denominator: Option<u64>,
    missingness: Missingness,
    caveats: Vec<String>,
    provenance: Vec<ProvenanceStage>,
}

impl TryFrom<UncheckedResultRow> for ResultRow {
    type Error = ContractError;

    fn try_from(row: UncheckedResultRow) -> Result<Self, Self::Error> {
        Self::new(
            row.candidate,
            row.axis,
            row.measurement_mode,
            row.parser_revision,
            row.adapter_revision,
            row.corpus_hash,
            row.status,
            row.numerator,
            row.denominator,
            row.missingness,
            row.caveats,
            row.provenance,
        )
    }
}

impl<'de> Deserialize<'de> for ResultRow {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        UncheckedResultRow::deserialize(deserializer)?
            .try_into()
            .map_err(serde::de::Error::custom)
    }
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

    /// Candidate identity.
    pub const fn candidate(&self) -> Candidate {
        self.candidate
    }

    /// Comparison axis.
    pub const fn axis(&self) -> Axis {
        self.axis
    }

    /// Measurement lane.
    pub const fn measurement_mode(&self) -> MeasurementMode {
        self.measurement_mode
    }

    /// Pinned parser revision.
    pub fn parser_revision(&self) -> &str {
        &self.parser_revision
    }

    /// Pinned adapter revision for an adapted lane.
    pub fn adapter_revision(&self) -> Option<&str> {
        self.adapter_revision.as_deref()
    }

    /// Canonical corpus content hash.
    pub fn corpus_hash(&self) -> &str {
        &self.corpus_hash
    }

    /// Measurement outcome.
    pub const fn status(&self) -> RowStatus {
        self.status
    }

    /// Exact numerator, when measured.
    pub const fn numerator(&self) -> Option<u64> {
        self.numerator
    }

    /// Explicit missingness classification.
    pub const fn missingness(&self) -> Missingness {
        self.missingness
    }

    /// Exact denominator, when measured.
    pub const fn denominator(&self) -> Option<u64> {
        self.denominator
    }

    /// Declared limitations on the observation.
    pub fn caveats(&self) -> &[String] {
        &self.caveats
    }

    /// Stages that materially produced the observation.
    pub fn provenance(&self) -> &[ProvenanceStage] {
        &self.provenance
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
        let has_native = self.provenance.contains(&ProvenanceStage::NativeParser);
        let has_adapted = self.provenance.iter().any(|stage| {
            matches!(
                stage,
                ProvenanceStage::Adapter
                    | ProvenanceStage::SourceLexerFallback
                    | ProvenanceStage::Mapper
            )
        });
        match self.measurement_mode {
            MeasurementMode::Native => {
                if !has_native || has_adapted || self.adapter_revision.is_some() {
                    return Err(ContractError::InvalidNativeAttribution);
                }
            }
            MeasurementMode::AdapterNormalized => {
                if !has_adapted || self.adapter_revision.as_deref().is_none_or(str::is_empty) {
                    return Err(ContractError::InvalidAdapterAttribution);
                }
            }
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
            RowStatus::Failed => {
                if !matches!(
                    self.missingness,
                    Missingness::Unavailable
                        | Missingness::BuildFailure
                        | Missingness::RunFailure
                        | Missingness::Timeout
                ) || self.numerator.is_some()
                    || self.denominator.is_some()
                {
                    return Err(ContractError::InvalidMissingness);
                }
            }
            RowStatus::NonComparable => {
                if !matches!(
                    self.missingness,
                    Missingness::NotApplicable | Missingness::NonComparable
                ) || self.numerator.is_some()
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
#[derive(Clone, Debug, Serialize)]
pub struct StudyReport {
    schema_version: u8,
    study_id: String,
    rows: Vec<ResultRow>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct UncheckedStudyReport {
    schema_version: u8,
    study_id: String,
    rows: Vec<ResultRow>,
}

impl TryFrom<UncheckedStudyReport> for StudyReport {
    type Error = ContractError;

    fn try_from(report: UncheckedStudyReport) -> Result<Self, Self::Error> {
        if report.schema_version != 1 {
            return Err(ContractError::UnsupportedSchemaVersion);
        }
        Self::new(report.study_id, report.rows)
    }
}

impl<'de> Deserialize<'de> for StudyReport {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        UncheckedStudyReport::deserialize(deserializer)?
            .try_into()
            .map_err(serde::de::Error::custom)
    }
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
        let present: BTreeSet<_> = rows
            .iter()
            .map(|row| (row.candidate, row.axis, row.measurement_mode))
            .collect();
        let required: BTreeSet<_> = Candidate::ALL
            .into_iter()
            .flat_map(|candidate| {
                Axis::ALL.into_iter().flat_map(move |axis| {
                    candidate
                        .required_modes()
                        .iter()
                        .map(move |mode| (candidate, axis, *mode))
                })
            })
            .collect();
        if present != required || rows.len() != required.len() {
            return Err(ContractError::IncompleteMatrix);
        }
        Ok(Self {
            schema_version: 1,
            study_id,
            rows,
        })
    }

    /// Validated result rows.
    pub fn rows(&self) -> &[ResultRow] {
        &self.rows
    }

    /// Report schema version.
    pub const fn schema_version(&self) -> u8 {
        self.schema_version
    }

    /// Frozen study identity.
    pub fn study_id(&self) -> &str {
        &self.study_id
    }
}

/// Report-contract validation failure.
#[derive(Clone, Copy, Debug, Error, Eq, PartialEq)]
pub enum ContractError {
    #[error(
        "native rows require native-parser provenance and prohibit adapted provenance or adapter revision"
    )]
    InvalidNativeAttribution,
    #[error("adapter-normalized rows require an adapter-derived stage and adapter revision")]
    InvalidAdapterAttribution,
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
    #[error("unsupported report schema version")]
    UnsupportedSchemaVersion,
}
