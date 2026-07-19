use std::collections::BTreeMap;

use ab_parser_rq_source_accountability::{RecognitionBlobRef, RecognitionWorkRecord};
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Disposition {
    AuthorizeExactSpan,
    ObserveOnly,
    RejectInternal,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(deny_unknown_fields)]
pub struct Interval {
    pub start: u64,
    pub end: u64,
}

impl From<(u64, u64)> for Interval {
    fn from((start, end): (u64, u64)) -> Self {
        Self { start, end }
    }
}

#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct DiagnosticEntry {
    pub kind: String,
    pub code: String,
    pub severity: String,
    pub source: String,
    pub span: Interval,
    pub codepoint: Option<String>,
}

#[derive(Clone, Debug)]
pub struct ValidatedDiagnosticCapture {
    pub(crate) entries: Vec<DiagnosticEntry>,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct PolicyRule {
    pub code: String,
    pub kind: String,
    pub severity: String,
    pub source: String,
    pub disposition: Disposition,
}

#[derive(Clone, Debug)]
pub struct ValidatedGapPolicy {
    pub(crate) rules: BTreeMap<String, PolicyRule>,
    pub(crate) policy_hash: String,
    pub(crate) artifact_hash: String,
    pub(crate) artifact_bytes: u64,
}

impl ValidatedGapPolicy {
    #[must_use]
    pub fn policy_hash(&self) -> &str {
        &self.policy_hash
    }
}

/// An exact-byte-authenticated source-accountability context.
///
/// Values cannot be assembled by callers; use
/// [`crate::validate_work_context`].
///
/// ```compile_fail
/// use ab_parser_rq_diagnostic_authorization::WorkContext;
///
/// let _forged = WorkContext {
///     decoded_source: b"",
///     work_id: "",
///     capture_generation_ref: "",
///     qualification_identity_ref: "",
///     source_recognition: panic!(),
/// };
/// ```
#[derive(Clone, Debug)]
pub struct WorkContext<'a> {
    pub(crate) decoded_source: &'a [u8],
    pub(crate) work_id: &'a str,
    pub(crate) capture_generation_ref: &'a str,
    pub(crate) qualification_identity_ref: &'a str,
    pub(crate) source_recognition: &'a RecognitionWorkRecord,
    pub(crate) decoded_source_hash: String,
    pub(crate) source_recognition_hash: String,
}

#[derive(Clone, Copy, Debug)]
pub struct BoundaryInput<'a> {
    pub raw_diagnostics: &'a [u8],
    pub raw_diagnostics_hash: &'a str,
    pub raw_diagnostics_bytes: u64,
    pub policy_bytes: &'a [u8],
    pub policy_bytes_hash: &'a str,
    pub decoded_source: &'a [u8],
    pub decoded_source_hash: &'a str,
    pub work_id: &'a str,
    pub capture_generation_ref: &'a str,
    pub qualification_identity_ref: &'a str,
    pub source_recognition_bytes: &'a [u8],
    pub source_recognition_hash: &'a str,
    pub source_recognition: &'a RecognitionWorkRecord,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AuthorizationStatus {
    Ok,
    Unavailable,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AuthorizationOrigin {
    pub(crate) work_id: String,
    pub(crate) capture_generation_ref: String,
    pub(crate) qualification_identity_ref: String,
    pub(crate) decoded_source_hash: String,
    pub(crate) raw_diagnostics_hash: String,
    pub(crate) raw_diagnostics_bytes: u64,
    pub(crate) policy_hash: String,
    pub(crate) policy_artifact_hash: String,
    pub(crate) policy_artifact_bytes: u64,
    pub(crate) source_recognition_hash: String,
    pub(crate) diagnostic_count: u64,
    pub(crate) authorizing_diagnostic_count: u64,
    pub(crate) observe_only_diagnostic_count: u64,
    pub(crate) vacuous: bool,
    pub(crate) authorized_intervals: Vec<Interval>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AuthorizationAnalysis {
    pub status: AuthorizationStatus,
    pub policy_hash: Option<String>,
    pub diagnostic_count: Option<u64>,
    pub authorizing_diagnostic_count: Option<u64>,
    pub observe_only_diagnostic_count: Option<u64>,
    pub vacuous: Option<bool>,
    pub authorized_intervals: Option<Vec<Interval>>,
    pub(crate) origin: Option<AuthorizationOrigin>,
    pub errors: Vec<String>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DiagnosticGapWorkStatus {
    Ok,
    Unavailable,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct SourceRecognitionEvidence {
    pub relation: String,
    pub artifact_ref: RecognitionBlobRef,
    pub value_hash: String,
    pub qualification_identity_ref: String,
    pub capture_generation_ref: String,
    pub work_id: String,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct DiagnosticAuthorizationEvidence {
    pub decoded_source_hash: String,
    pub raw_diagnostics_hash: String,
    pub raw_diagnostics_bytes: u64,
    pub policy_hash: String,
    pub policy_artifact_hash: String,
    pub policy_artifact_bytes: u64,
    pub source_recognition_hash: String,
}

#[derive(Clone, Debug)]
pub struct DiagnosticGapWorkInput<'a> {
    pub source_recognition: &'a RecognitionWorkRecord,
    pub source_recognition_bytes: &'a [u8],
    pub source_recognition_artifact_ref: RecognitionBlobRef,
    pub source_recognition_value_hash: String,
    pub authorization: &'a AuthorizationAnalysis,
}

/// A partition derived by this crate. Its private seal prevents callers from
/// constructing an apparently available result.
///
/// ```compile_fail
/// use ab_parser_rq_diagnostic_authorization::DiagnosticGapWorkResult;
/// let _forged = DiagnosticGapWorkResult { /* public summaries are insufficient */ };
/// ```
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DiagnosticGapWorkResult {
    pub status: DiagnosticGapWorkStatus,
    pub work_id: Option<String>,
    pub capture_generation_ref: Option<String>,
    pub qualification_identity_ref: Option<String>,
    pub policy_hash: Option<String>,
    pub source_recognition_evidence: Option<SourceRecognitionEvidence>,
    pub diagnostic_authorization_evidence: Option<DiagnosticAuthorizationEvidence>,
    pub authorized_intervals: Option<Vec<Interval>>,
    pub silent_intervals: Option<Vec<Interval>>,
    pub authorized_bytes: Option<u64>,
    pub silent_bytes: Option<u64>,
    pub silent_drop_count: Option<u64>,
    pub diagnostic_count: Option<u64>,
    pub authorizing_diagnostic_count: Option<u64>,
    pub observe_only_diagnostic_count: Option<u64>,
    pub vacuous: Option<bool>,
    pub errors: Vec<String>,
    pub(crate) seal: Option<WorkResultSeal>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct WorkResultSeal {
    pub(crate) authorized_intervals: Vec<Interval>,
    pub(crate) silent_intervals: Vec<Interval>,
    pub(crate) authorized_bytes: u64,
    pub(crate) silent_bytes: u64,
    pub(crate) silent_drop_count: u64,
    pub(crate) diagnostic_count: u64,
    pub(crate) authorizing_diagnostic_count: u64,
    pub(crate) observe_only_diagnostic_count: u64,
    pub(crate) vacuous: bool,
}

impl DiagnosticGapWorkResult {
    pub(crate) fn unavailable(error: &str) -> Self {
        Self {
            status: DiagnosticGapWorkStatus::Unavailable,
            work_id: None,
            capture_generation_ref: None,
            qualification_identity_ref: None,
            policy_hash: None,
            source_recognition_evidence: None,
            diagnostic_authorization_evidence: None,
            authorized_intervals: None,
            silent_intervals: None,
            authorized_bytes: None,
            silent_bytes: None,
            silent_drop_count: None,
            diagnostic_count: None,
            authorizing_diagnostic_count: None,
            observe_only_diagnostic_count: None,
            vacuous: None,
            errors: vec![error.to_owned()],
            seal: None,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct DiagnosticGapAggregateInput<'a> {
    pub expected_works: &'a [DiagnosticGapExpectedWork],
    pub qualification_identity_ref: &'a str,
    pub corpus_generation_ref: &'a str,
    pub policy_hash: &'a str,
    pub works: &'a [DiagnosticGapWorkResult],
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct DiagnosticGapExpectedWork {
    pub work_id: String,
    pub capture_generation_ref: String,
    pub source_recognition_value_hash: String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DiagnosticGapAggregate {
    pub schema_version: Option<String>,
    pub status: DiagnosticGapWorkStatus,
    pub qualification_identity_ref: Option<String>,
    pub corpus_generation_ref: Option<String>,
    pub policy_hash: Option<String>,
    pub policy_artifact_hash: Option<String>,
    pub expected_work_ids: Vec<String>,
    pub observed_work_ids: Vec<String>,
    pub authorized_bytes: Option<u64>,
    pub silent_bytes: Option<u64>,
    pub silent_drop_count: Option<u64>,
    pub diagnostic_count: Option<u64>,
    pub authorizing_diagnostic_count: Option<u64>,
    pub observe_only_diagnostic_count: Option<u64>,
    pub authorized_interval_count: Option<u64>,
    pub vacuous: Option<bool>,
    pub errors: Vec<String>,
}

impl AuthorizationAnalysis {
    pub(crate) fn unavailable(error: &str) -> Self {
        Self {
            status: AuthorizationStatus::Unavailable,
            policy_hash: None,
            diagnostic_count: None,
            authorizing_diagnostic_count: None,
            observe_only_diagnostic_count: None,
            vacuous: None,
            authorized_intervals: None,
            origin: None,
            errors: vec![error.to_owned()],
        }
    }
}
