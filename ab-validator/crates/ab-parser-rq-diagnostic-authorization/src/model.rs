use std::collections::BTreeMap;

use ab_parser_rq_source_accountability::RecognitionWorkRecord;
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
    pub policy_hash: String,
}

#[derive(Clone, Copy, Debug)]
pub struct WorkContext<'a> {
    pub decoded_source: &'a [u8],
    pub work_id: &'a str,
    pub capture_generation_ref: &'a str,
    pub qualification_identity_ref: &'a str,
    pub source_recognition: &'a RecognitionWorkRecord,
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
pub struct AuthorizationAnalysis {
    pub status: AuthorizationStatus,
    pub policy_hash: Option<String>,
    pub diagnostic_count: Option<u64>,
    pub authorizing_diagnostic_count: Option<u64>,
    pub observe_only_diagnostic_count: Option<u64>,
    pub vacuous: Option<bool>,
    pub authorized_intervals: Option<Vec<Interval>>,
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
            errors: vec![error.to_owned()],
        }
    }
}
