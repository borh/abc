use std::collections::BTreeSet;

use ab_aozora_spec::Diagnostic;
use ab_parser_rq_source_accountability::{RecognitionStatus, canonical_json};
use serde::Deserialize;
use serde_json::Value;
use sha2::{Digest, Sha256};

use crate::model::{
    AuthorizationAnalysis, AuthorizationOrigin, AuthorizationStatus, BoundaryInput,
    DiagnosticEntry, Disposition, Interval, PolicyRule, ValidatedDiagnosticCapture,
    ValidatedGapPolicy, WorkContext,
};

const RAW_SCHEMA: &[u8] =
    include_bytes!("../../../research/schemas/parser-rq-ab-aozora-diagnostics-v3.schema.json");
const POLICY_SCHEMA: &[u8] =
    include_bytes!("../../../research/schemas/parser-rq-diagnostic-gap-policy.schema.json");

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Capture {
    #[serde(rename = "schemaVersion")]
    _version: u64,
    data: Vec<DiagnosticEntry>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Policy {
    #[serde(rename = "schema_version")]
    _schema_version: String,
    #[serde(rename = "policy_id")]
    _policy_id: String,
    policy_hash: String,
    raw_diagnostic_schema_hash: String,
    #[serde(rename = "identity")]
    _identity: Value,
    #[serde(rename = "parser")]
    _parser: String,
    #[serde(rename = "coordinate_system")]
    _coordinate_system: String,
    rules: Vec<PolicyRule>,
}

fn sha256(bytes: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(bytes))
}

fn abc_jcs(value: &Value) -> Option<String> {
    Some(match value {
        Value::Null => "null".to_owned(),
        Value::Bool(v) => v.to_string(),
        Value::Number(v) => v.to_string(),
        Value::String(v) => serde_json::to_string(v).ok()?.replace('/', "\\/"),
        Value::Array(v) => format!(
            "[{}]",
            v.iter().map(abc_jcs).collect::<Option<Vec<_>>>()?.join(",")
        ),
        Value::Object(v) => {
            let mut entries: Vec<_> = v.iter().collect();
            entries.sort_by_key(|(k, _)| *k);
            format!(
                "{{{}}}",
                entries
                    .into_iter()
                    .map(|(k, v)| Some(format!(
                        "{}:{}",
                        serde_json::to_string(k).ok()?.replace('/', "\\/"),
                        abc_jcs(v)?
                    )))
                    .collect::<Option<Vec<_>>>()?
                    .join(",")
            )
        }
    })
}

fn schema_valid(schema: &[u8], value: &Value) -> bool {
    serde_json::from_slice::<Value>(schema)
        .ok()
        .and_then(|s| jsonschema::validator_for(&s).ok())
        .is_some_and(|validator| validator.validate(value).is_ok())
}

fn validate_capture(bytes: &[u8]) -> Option<ValidatedDiagnosticCapture> {
    let value: Value = serde_json::from_slice(bytes).ok()?;
    if !schema_valid(RAW_SCHEMA, &value) {
        return None;
    }
    let capture: Capture = serde_json::from_value(value).ok()?;
    Some(ValidatedDiagnosticCapture {
        entries: capture.data,
    })
}

/// Authenticate and close one exact raw schema-v3 diagnostic value.
pub fn validate_diagnostic_capture(
    bytes: &[u8],
    expected_hash: &str,
    expected_bytes: u64,
) -> Result<ValidatedDiagnosticCapture, &'static str> {
    if expected_bytes != bytes.len() as u64 || sha256(bytes) != expected_hash {
        return Err("raw-diagnostics-authentication-failed");
    }
    validate_capture(bytes).ok_or("raw-diagnostics-invalid")
}

fn wire_code(code: &str) -> Option<String> {
    code.rsplit("::").next().map(|s| s.replace('_', "-"))
}

fn injective_wire_codes<'a>(codes: impl IntoIterator<Item = &'a str>) -> Option<BTreeSet<String>> {
    let projected = codes
        .into_iter()
        .map(wire_code)
        .collect::<Option<Vec<_>>>()?;
    let unique = projected.iter().cloned().collect::<BTreeSet<_>>();
    (unique.len() == projected.len()).then_some(unique)
}

fn validate_policy(bytes: &[u8]) -> Option<ValidatedGapPolicy> {
    let mut value: Value = serde_json::from_slice(bytes).ok()?;
    if !schema_valid(POLICY_SCHEMA, &value) {
        return None;
    }
    let policy: Policy = serde_json::from_value(value.clone()).ok()?;
    let raw_schema_value: Value = serde_json::from_slice(RAW_SCHEMA).ok()?;
    if policy.raw_diagnostic_schema_hash != sha256(abc_jcs(&raw_schema_value)?.as_bytes()) {
        return None;
    }
    value.as_object_mut()?.remove("policy_hash");
    let projection = abc_jcs(&value)?;
    if policy.policy_hash != sha256(projection.as_bytes()) {
        return None;
    }
    let live = injective_wire_codes(Diagnostic::ALL_CODES.iter().copied())?;
    let declared = policy
        .rules
        .iter()
        .map(|r| r.code.clone())
        .collect::<BTreeSet<_>>();
    if live != declared || declared.len() != policy.rules.len() {
        return None;
    }
    let rules = policy
        .rules
        .into_iter()
        .map(|r| (r.code.clone(), r))
        .collect();
    Some(ValidatedGapPolicy {
        rules,
        policy_hash: policy.policy_hash,
        artifact_hash: sha256(bytes),
        artifact_bytes: bytes.len() as u64,
    })
}

/// Authenticate and close the independently governed ABC policy bytes.
pub fn validate_gap_policy(
    bytes: &[u8],
    expected_hash: &str,
) -> Result<ValidatedGapPolicy, &'static str> {
    if sha256(bytes) != expected_hash {
        return Err("policy-authentication-failed");
    }
    validate_policy(bytes).ok_or("policy-invalid-or-drifted")
}

/// Authenticate decoded source and canonical R1 evidence into a coherent work context.
#[allow(
    clippy::too_many_arguments,
    reason = "all independent identities cross the trust boundary"
)]
pub fn validate_work_context<'a>(
    decoded_source: &'a [u8],
    decoded_source_hash: &str,
    work_id: &'a str,
    capture_generation_ref: &'a str,
    qualification_identity_ref: &'a str,
    source_recognition_bytes: &[u8],
    source_recognition_hash: &str,
    source_recognition: &'a ab_parser_rq_source_accountability::RecognitionWorkRecord,
) -> Result<WorkContext<'a>, &'static str> {
    if sha256(decoded_source) != decoded_source_hash {
        return Err("decoded-source-authentication-failed");
    }
    if sha256(source_recognition_bytes) != source_recognition_hash
        || canonical_json(source_recognition)
            .ok()
            .as_deref()
            .map(str::as_bytes)
            != Some(source_recognition_bytes)
    {
        return Err("source-recognition-authentication-failed");
    }
    if source_recognition.status != RecognitionStatus::Ok
        || source_recognition.work_id.as_deref() != Some(work_id)
        || source_recognition.capture_generation_ref.as_deref() != Some(capture_generation_ref)
        || source_recognition.qualification_identity_ref.as_deref()
            != Some(qualification_identity_ref)
        || source_recognition.coordinate_system != "decoded_utf8"
        || source_recognition.eligible_bytes != Some(decoded_source.len() as u64)
    {
        return Err("source-recognition-identity-mismatch");
    }
    Ok(WorkContext {
        decoded_source,
        work_id,
        capture_generation_ref,
        qualification_identity_ref,
        source_recognition,
        decoded_source_hash: decoded_source_hash.to_owned(),
        source_recognition_hash: source_recognition_hash.to_owned(),
    })
}

fn normalized(intervals: impl IntoIterator<Item = Interval>) -> Vec<Interval> {
    let mut values: Vec<_> = intervals.into_iter().collect();
    values.sort();
    let mut out: Vec<Interval> = Vec::new();
    for value in values {
        if let Some(last) = out.last_mut().filter(|last| value.start <= last.end) {
            last.end = last.end.max(value.end);
        } else {
            out.push(value);
        }
    }
    out
}

/// Pure authorization over values already closed by the byte boundary.
#[must_use]
pub fn authorize(
    capture: &ValidatedDiagnosticCapture,
    policy: &ValidatedGapPolicy,
    context: WorkContext<'_>,
) -> AuthorizationAnalysis {
    let r1 = context.source_recognition;
    if r1.status != RecognitionStatus::Ok
        || r1.work_id.as_deref() != Some(context.work_id)
        || r1.capture_generation_ref.as_deref() != Some(context.capture_generation_ref)
        || r1.qualification_identity_ref.as_deref() != Some(context.qualification_identity_ref)
        || r1.coordinate_system != "decoded_utf8"
        || r1.eligible_bytes != Some(context.decoded_source.len() as u64)
    {
        return AuthorizationAnalysis::unavailable("source-recognition-identity-mismatch");
    }
    let Ok(source) = std::str::from_utf8(context.decoded_source) else {
        return AuthorizationAnalysis::unavailable("decoded-source-invalid-utf8");
    };
    let mut identities = BTreeSet::new();
    let mut authorized = Vec::new();
    let mut authorizing = 0;
    let mut observed = 0;
    for entry in &capture.entries {
        let Some(rule) = policy.rules.get(&entry.code) else {
            return AuthorizationAnalysis::unavailable("diagnostic-code-unknown");
        };
        if entry.kind != rule.kind || entry.severity != rule.severity || entry.source != rule.source
        {
            return AuthorizationAnalysis::unavailable("diagnostic-policy-mismatch");
        }
        if entry.source == "internal" || rule.disposition == Disposition::RejectInternal {
            return AuthorizationAnalysis::unavailable("internal-diagnostic");
        }
        let identity = (
            &entry.code,
            &entry.severity,
            &entry.source,
            entry.span.start,
            entry.span.end,
        );
        if !identities.insert(identity) {
            return AuthorizationAnalysis::unavailable("duplicate-diagnostic");
        }
        let (Ok(start), Ok(end)) = (
            usize::try_from(entry.span.start),
            usize::try_from(entry.span.end),
        ) else {
            return AuthorizationAnalysis::unavailable("diagnostic-interval-invalid");
        };
        if start >= end
            || end > source.len()
            || !source.is_char_boundary(start)
            || !source.is_char_boundary(end)
        {
            return AuthorizationAnalysis::unavailable("diagnostic-interval-invalid");
        }
        if entry.code == "source-contains-pua" {
            let Some(codepoint) = entry.codepoint.as_deref() else {
                return AuthorizationAnalysis::unavailable("diagnostic-codepoint-invalid");
            };
            let mut chars = codepoint.chars();
            let Some(c) = chars.next() else {
                return AuthorizationAnalysis::unavailable("diagnostic-codepoint-invalid");
            };
            if chars.next().is_some()
                || !(('\u{e000}'..='\u{f8ff}').contains(&c)
                    || ('\u{f0000}'..='\u{ffffd}').contains(&c)
                    || ('\u{100000}'..='\u{10fffd}').contains(&c))
                || source.get(start..end) != Some(codepoint)
            {
                return AuthorizationAnalysis::unavailable("diagnostic-codepoint-invalid");
            }
        } else if entry.codepoint.is_some() {
            return AuthorizationAnalysis::unavailable("diagnostic-codepoint-invalid");
        }
        match rule.disposition {
            Disposition::AuthorizeExactSpan => {
                authorizing += 1;
                authorized.push(entry.span);
            }
            Disposition::ObserveOnly => observed += 1,
            Disposition::RejectInternal => unreachable!(),
        }
    }
    let authorized_intervals = normalized(authorized);
    AuthorizationAnalysis {
        status: AuthorizationStatus::Ok,
        policy_hash: Some(policy.policy_hash.clone()),
        diagnostic_count: Some(capture.entries.len() as u64),
        authorizing_diagnostic_count: Some(authorizing),
        observe_only_diagnostic_count: Some(observed),
        vacuous: Some(capture.entries.is_empty()),
        authorized_intervals: Some(authorized_intervals.clone()),
        origin: Some(AuthorizationOrigin {
            work_id: context.work_id.to_owned(),
            capture_generation_ref: context.capture_generation_ref.to_owned(),
            qualification_identity_ref: context.qualification_identity_ref.to_owned(),
            decoded_source_hash: context.decoded_source_hash.clone(),
            raw_diagnostics_hash: String::new(),
            raw_diagnostics_bytes: 0,
            policy_hash: policy.policy_hash.clone(),
            policy_artifact_hash: policy.artifact_hash.clone(),
            policy_artifact_bytes: policy.artifact_bytes,
            source_recognition_hash: context.source_recognition_hash.clone(),
            diagnostic_count: capture.entries.len() as u64,
            authorizing_diagnostic_count: authorizing,
            observe_only_diagnostic_count: observed,
            vacuous: capture.entries.is_empty(),
            authorized_intervals,
        }),
        errors: vec![],
    }
}

/// Authenticate exact boundary bytes before entering the pure authorizer.
#[must_use]
pub fn authorize_boundary(input: BoundaryInput<'_>) -> AuthorizationAnalysis {
    let capture = match validate_diagnostic_capture(
        input.raw_diagnostics,
        input.raw_diagnostics_hash,
        input.raw_diagnostics_bytes,
    ) {
        Ok(value) => value,
        Err(error) => return AuthorizationAnalysis::unavailable(error),
    };
    let policy = match validate_gap_policy(input.policy_bytes, input.policy_bytes_hash) {
        Ok(value) => value,
        Err(error) => return AuthorizationAnalysis::unavailable(error),
    };
    let context = match validate_work_context(
        input.decoded_source,
        input.decoded_source_hash,
        input.work_id,
        input.capture_generation_ref,
        input.qualification_identity_ref,
        input.source_recognition_bytes,
        input.source_recognition_hash,
        input.source_recognition,
    ) {
        Ok(value) => value,
        Err(error) => return AuthorizationAnalysis::unavailable(error),
    };
    let mut result = authorize(&capture, &policy, context);
    if let Some(origin) = result.origin.as_mut() {
        origin.raw_diagnostics_hash = input.raw_diagnostics_hash.to_owned();
        origin.raw_diagnostics_bytes = input.raw_diagnostics_bytes;
    }
    result
}

#[cfg(test)]
mod tests {
    use super::injective_wire_codes;

    #[test]
    fn live_wire_projection_rejects_distinct_namespaced_code_collisions() {
        assert!(injective_wire_codes(["aozora::syntax::same_code", "other::same_code"]).is_none());
        assert_eq!(
            injective_wire_codes(["aozora::syntax::first_code", "other::second_code"])
                .unwrap()
                .len(),
            2
        );
    }
}
