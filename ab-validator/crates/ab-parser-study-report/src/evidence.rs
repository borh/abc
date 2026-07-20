//! Authenticated, location-independent parser-study axis evidence.

use std::collections::BTreeMap;
use std::fs::File;
use std::io::Read;
use std::path::{Component, Path, PathBuf};

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use thiserror::Error;

use crate::{Axis, Candidate, MeasurementMode};

const INDEX_SCHEMA_ID: &str = "https://w3id.org/abc/schemas/parser-study-evidence-index-v1";
const EVIDENCE_SCHEMA_ID: &str = "https://w3id.org/abc/schemas/parser-study-axis-evidence-v1";

/// Logical content identity independent of storage location.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ContentRef {
    pub sha256: String,
    pub bytes: u64,
    pub media_type: String,
}

/// Closed role of an input required by an axis.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum InputRole {
    SourceMarkup,
    IndependentReference,
    ThirdPartyCapture,
}

/// Present or explicitly absent required evidence.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(tag = "state", rename_all = "snake_case")]
pub enum RequiredInputState {
    Present {
        role: InputRole,
        artifact: String,
        content_ref: ContentRef,
        locator: String,
    },
    Absent {
        role: InputRole,
        artifact: String,
        reason: String,
    },
}

impl RequiredInputState {
    /// Closed role whose presence or absence this value records.
    pub const fn role(&self) -> InputRole {
        match self {
            Self::Present { role, .. } | Self::Absent { role, .. } => *role,
        }
    }

    /// Stable artifact identifier within the axis policy.
    pub fn artifact(&self) -> &str {
        match self {
            Self::Present { artifact, .. } | Self::Absent { artifact, .. } => artifact,
        }
    }

    pub(crate) const fn is_absent(&self) -> bool {
        matches!(self, Self::Absent { .. })
    }
}

/// Whether an axis metric produced an accountable value.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum MetricDisposition {
    Measured,
    NonComparable,
    Unavailable,
    Failed,
}

/// Typed value carried only by a measured metric.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum MetricValue {
    Count {
        value: u64,
    },
    Ratio {
        numerator: u64,
        denominator: u64,
    },
    Bytes {
        value: u64,
    },
    Seconds {
        value: f64,
    },
    Boolean {
        value: bool,
    },
    Text {
        value: String,
    },
    Interval {
        lower: f64,
        upper: Option<f64>,
        unit: String,
    },
}

/// One typed metric observation.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct MetricObservation {
    pub metric: String,
    pub disposition: MetricDisposition,
    #[serde(default)]
    pub value: Option<MetricValue>,
    #[serde(default)]
    pub reason: Option<String>,
}

/// Terminal process state retained as a witness, not a diagnostic verdict.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ProcessStatus {
    Exited,
    TimedOut,
    SpawnFailed,
}

/// Whether the selected diagnostic projector produced a usable envelope.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ProjectionStatus {
    Usable,
    NotConfigured,
    Invalid,
}

/// Per-fixture disclosure retained by diagnostic evidence.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct CaseWitness {
    pub case_id: String,
    pub process_status: ProcessStatus,
    pub returncode: Option<i64>,
    pub projection_status: ProjectionStatus,
    pub diagnostic_count: Option<u64>,
    pub relevant_diagnostic_count: Option<u64>,
    pub false_positive_count: Option<u64>,
}

/// Unique candidate/axis/lane key.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
pub struct RecordKey {
    pub candidate: Candidate,
    pub axis: Axis,
    pub measurement_mode: MeasurementMode,
}

/// Authenticated evidence for one frozen matrix row.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct AxisEvidenceRecord {
    schema_id: String,
    schema_version: u8,
    study_id: String,
    pub candidate: Candidate,
    pub axis: Axis,
    pub measurement_mode: MeasurementMode,
    pub parser_revision: String,
    pub adapter_revision: Option<String>,
    pub corpus_hash: String,
    pub required_inputs: Vec<RequiredInputState>,
    pub metrics: Vec<MetricObservation>,
    pub case_witnesses: Vec<CaseWitness>,
}

impl AxisEvidenceRecord {
    /// Stable matrix key carried by this record.
    pub const fn key(&self) -> RecordKey {
        RecordKey {
            candidate: self.candidate,
            axis: self.axis,
            measurement_mode: self.measurement_mode,
        }
    }

    fn validate(&self) -> Result<(), &'static str> {
        if self.schema_id != EVIDENCE_SCHEMA_ID
            || self.schema_version != 1
            || self.study_id != "aozora-parser-neutral-comparison-2026-07"
            || self.parser_revision.is_empty()
            || !valid_sha256(&self.corpus_hash)
            || self.metrics.is_empty()
        {
            return Err("invalid evidence identity or empty metric set");
        }
        if matches!(self.measurement_mode, MeasurementMode::AdapterNormalized)
            != self
                .adapter_revision
                .as_deref()
                .is_some_and(|value| !value.is_empty())
        {
            return Err("adapter revision does not match measurement mode");
        }
        let mut inputs = std::collections::BTreeSet::new();
        for input in &self.required_inputs {
            if input.artifact().is_empty() || !inputs.insert((input.role(), input.artifact())) {
                return Err("duplicate or empty required input");
            }
            if let RequiredInputState::Present {
                content_ref,
                locator,
                ..
            } = input
                && (!valid_content_ref(content_ref) || !portable_locator(locator))
            {
                return Err("invalid present input identity or locator");
            }
        }
        let mut metrics = std::collections::BTreeSet::new();
        for metric in &self.metrics {
            if metric.metric.is_empty() || !metrics.insert(metric.metric.as_str()) {
                return Err("duplicate or empty metric id");
            }
            match metric.disposition {
                MetricDisposition::Measured => {
                    if metric.value.is_none() || metric.reason.is_some() {
                        return Err("metric disposition, value, and reason disagree");
                    }
                }
                _ => {
                    if metric.value.is_some() || metric.reason.as_deref().is_none_or(str::is_empty)
                    {
                        return Err("metric disposition, value, and reason disagree");
                    }
                }
            }
        }
        let mut cases = std::collections::BTreeSet::new();
        if self
            .case_witnesses
            .iter()
            .any(|case| case.case_id.is_empty() || !cases.insert(case.case_id.as_str()))
        {
            return Err("duplicate or empty case witness id");
        }
        Ok(())
    }
}

/// Closed authenticated record set.
#[derive(Clone, Debug, Default)]
pub struct AxisEvidenceSet(BTreeMap<RecordKey, AxisEvidenceRecord>);

impl AxisEvidenceSet {
    /// Empty evidence for an initial completeness census.
    pub const fn empty() -> Self {
        Self(BTreeMap::new())
    }

    /// Constructs a set while enforcing unique candidate/axis/lane identity.
    pub fn from_records(
        records: impl IntoIterator<Item = AxisEvidenceRecord>,
    ) -> Result<Self, EvidenceError> {
        let mut by_key = BTreeMap::new();
        for record in records {
            record
                .validate()
                .map_err(EvidenceError::InvalidRecordContract)?;
            let key = record.key();
            if by_key.insert(key, record).is_some() {
                return Err(EvidenceError::DuplicateRecordIdentity {
                    candidate: key.candidate,
                    axis: key.axis,
                    measurement_mode: key.measurement_mode,
                });
            }
        }
        Ok(Self(by_key))
    }

    /// Number of authenticated records.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Whether the set has no records.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub(crate) fn get(&self, key: &RecordKey) -> Option<&AxisEvidenceRecord> {
        self.0.get(key)
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct EvidenceIndex {
    schema_id: String,
    schema_version: u8,
    study_id: String,
    records: Vec<IndexMember>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct IndexMember {
    candidate: Candidate,
    axis: Axis,
    measurement_mode: MeasurementMode,
    record_ref: ContentRef,
    locator: String,
}

/// Evidence authentication failure.
#[derive(Debug, Error)]
pub enum EvidenceError {
    #[error("invalid evidence index: {0}")]
    InvalidIndex(serde_json::Error),
    #[error("unsafe evidence locator {0:?}")]
    UnsafeLocator(String),
    #[error("failed to read evidence {path}: {source}")]
    Io {
        path: PathBuf,
        source: std::io::Error,
    },
    #[error("content mismatch for {locator}: expected {expected}, observed {observed}")]
    ContentMismatch {
        locator: String,
        expected: String,
        observed: String,
    },
    #[error("invalid axis evidence at {locator}: {source}")]
    InvalidRecord {
        locator: String,
        source: serde_json::Error,
    },
    #[error("axis evidence contract violation: {0}")]
    InvalidRecordContract(&'static str),
    #[error("index identity does not match record at {0}")]
    IdentityMismatch(String),
    #[error("duplicate evidence identity {candidate:?}/{axis:?}/{measurement_mode:?}")]
    DuplicateRecordIdentity {
        candidate: Candidate,
        axis: Axis,
        measurement_mode: MeasurementMode,
    },
}

/// Loads only records whose bytes and closed index identities authenticate.
pub fn load_index(root: &Path, index_json: &str) -> Result<AxisEvidenceSet, EvidenceError> {
    let index: EvidenceIndex =
        serde_json::from_str(index_json).map_err(EvidenceError::InvalidIndex)?;
    if index.schema_id != INDEX_SCHEMA_ID
        || index.schema_version != 1
        || index.study_id != "aozora-parser-neutral-comparison-2026-07"
    {
        return Err(EvidenceError::InvalidIndex(serde_json::Error::io(
            std::io::Error::new(std::io::ErrorKind::InvalidData, "index identity mismatch"),
        )));
    }
    let canonical_root = root.canonicalize().map_err(|source| EvidenceError::Io {
        path: root.to_path_buf(),
        source,
    })?;
    let mut records = BTreeMap::new();
    for member in index.records {
        let path = resolve_member(&canonical_root, &member.locator)?;
        let (bytes, digest) = read_and_hash(&path)?;
        let observed = format!("sha256:{digest}");
        if member.record_ref.bytes != bytes.len() as u64
            || member.record_ref.sha256 != observed
            || member.record_ref.media_type != "application/json"
        {
            return Err(EvidenceError::ContentMismatch {
                locator: member.locator,
                expected: member.record_ref.sha256,
                observed,
            });
        }
        let record: AxisEvidenceRecord =
            serde_json::from_slice(&bytes).map_err(|source| EvidenceError::InvalidRecord {
                locator: member.locator.clone(),
                source,
            })?;
        record
            .validate()
            .map_err(EvidenceError::InvalidRecordContract)?;
        if record.schema_id != EVIDENCE_SCHEMA_ID
            || record.schema_version != 1
            || record.study_id != index.study_id
            || record.candidate != member.candidate
            || record.axis != member.axis
            || record.measurement_mode != member.measurement_mode
        {
            return Err(EvidenceError::IdentityMismatch(member.locator));
        }
        let key = record.key();
        if records.insert(key, record).is_some() {
            return Err(EvidenceError::DuplicateRecordIdentity {
                candidate: key.candidate,
                axis: key.axis,
                measurement_mode: key.measurement_mode,
            });
        }
    }
    Ok(AxisEvidenceSet(records))
}

fn resolve_member(root: &Path, locator: &str) -> Result<PathBuf, EvidenceError> {
    let relative = Path::new(locator);
    if !portable_locator(locator) {
        return Err(EvidenceError::UnsafeLocator(locator.to_owned()));
    }
    let path = root.join(relative);
    let canonical = path.canonicalize().map_err(|source| EvidenceError::Io {
        path: path.clone(),
        source,
    })?;
    if !canonical.starts_with(root) {
        return Err(EvidenceError::UnsafeLocator(locator.to_owned()));
    }
    Ok(canonical)
}

fn portable_locator(locator: &str) -> bool {
    !locator.is_empty()
        && !Path::new(locator).is_absolute()
        && Path::new(locator)
            .components()
            .all(|component| matches!(component, Component::Normal(_)))
}

fn valid_sha256(value: &str) -> bool {
    value.strip_prefix("sha256:").is_some_and(|digest| {
        digest.len() == 64
            && digest
                .bytes()
                .all(|byte| byte.is_ascii_hexdigit() && !byte.is_ascii_uppercase())
    })
}

fn valid_content_ref(content_ref: &ContentRef) -> bool {
    valid_sha256(&content_ref.sha256) && !content_ref.media_type.is_empty()
}

fn read_and_hash(path: &Path) -> Result<(Vec<u8>, String), EvidenceError> {
    let mut file = File::open(path).map_err(|source| EvidenceError::Io {
        path: path.to_path_buf(),
        source,
    })?;
    let mut bytes = Vec::new();
    let mut hasher = Sha256::new();
    let mut buffer = [0_u8; 65_536];
    loop {
        let read = file.read(&mut buffer).map_err(|source| EvidenceError::Io {
            path: path.to_path_buf(),
            source,
        })?;
        if read == 0 {
            break;
        }
        hasher.update(&buffer[..read]);
        bytes.extend_from_slice(&buffer[..read]);
    }
    Ok((bytes, format!("{:x}", hasher.finalize())))
}
