use std::collections::BTreeSet;

use anyhow::{Result, bail};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use sha2::{Digest, Sha256};

use crate::{
    ConversionOptions, PreparedConverter, QualificationConversion, to_canonical_json_pretty,
};

const WORK_SCHEMA_ID: &str =
    "https://w3id.org/abc/schemas/parser-rq-parser-ir-conformance-work.schema.json";
const AGGREGATE_SCHEMA_ID: &str =
    "https://w3id.org/abc/schemas/parser-rq-parser-ir-conformance-aggregate.schema.json";

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum QualificationStatus {
    SchemaValid,
    SchemaInvalid,
    NoOutput,
    Unavailable,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BlobReference {
    pub sha256: String,
    pub bytes: u64,
    pub media_type: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct QualificationFailure {
    pub kind: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct QualificationWorkRecord {
    pub schema_id: String,
    pub schema_version: String,
    pub work_id: String,
    pub qualification_identity_ref: String,
    pub policy_hash: String,
    pub status: QualificationStatus,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parser_ir: Option<BlobReference>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub validation_ledger: Option<BlobReference>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub validation_witnesses: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub failure: Option<QualificationFailure>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reason: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum QualificationAggregateStatus {
    Measured,
    NoParserIrOutput,
    Unavailable,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct QualificationAggregate {
    pub schema_id: String,
    pub schema_version: String,
    pub qualification_identity_ref: String,
    pub policy_hash: String,
    pub status: QualificationAggregateStatus,
    pub expected_works: u64,
    pub generated_outputs: u64,
    pub schema_valid_outputs: u64,
    pub schema_invalid_outputs: u64,
    pub no_output_works: u64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parser_ir_schema_validation: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reason: Option<String>,
}

pub struct QualificationRequest<'a> {
    pub converter: &'a PreparedConverter,
    pub aat: Value,
    pub options: ConversionOptions,
    pub work_id: String,
    pub qualification_identity_ref: String,
    pub policy_hash: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct QualificationWorkCapture {
    pub record: QualificationWorkRecord,
    pub parser_ir_bytes: Option<Vec<u8>>,
    pub validation_ledger_bytes: Option<Vec<u8>>,
}

pub fn qualify_work(request: QualificationRequest<'_>) -> Result<QualificationWorkCapture> {
    let base_record = || QualificationWorkRecord {
        schema_id: WORK_SCHEMA_ID.to_owned(),
        schema_version: "1.0.0".to_owned(),
        work_id: request.work_id.clone(),
        qualification_identity_ref: request.qualification_identity_ref.clone(),
        policy_hash: request.policy_hash.clone(),
        status: QualificationStatus::Unavailable,
        parser_ir: None,
        validation_ledger: None,
        validation_witnesses: None,
        failure: None,
        reason: None,
    };

    match request
        .converter
        .convert_for_qualification(request.aat, request.options)
    {
        Ok(QualificationConversion::Valid(output)) => {
            let parser_ir_bytes = canonical_bytes(output.parser_ir)?;
            let mut record = base_record();
            record.status = QualificationStatus::SchemaValid;
            record.parser_ir = Some(blob_reference(&parser_ir_bytes));
            Ok(QualificationWorkCapture {
                record,
                parser_ir_bytes: Some(parser_ir_bytes),
                validation_ledger_bytes: None,
            })
        }
        Ok(QualificationConversion::Invalid { parser_ir, errors }) => {
            let parser_ir_bytes = canonical_bytes(parser_ir)?;
            let validation_ledger_bytes = canonical_bytes(json!({ "errors": errors }))?;
            let ledger: Value = serde_json::from_slice(&validation_ledger_bytes)?;
            let witnesses = ledger["errors"]
                .as_array()
                .expect("qualification ledger errors must be an array")
                .iter()
                .take(20)
                .filter_map(Value::as_str)
                .map(str::to_owned)
                .collect();
            let mut record = base_record();
            record.status = QualificationStatus::SchemaInvalid;
            record.parser_ir = Some(blob_reference(&parser_ir_bytes));
            record.validation_ledger = Some(blob_reference(&validation_ledger_bytes));
            record.validation_witnesses = Some(witnesses);
            Ok(QualificationWorkCapture {
                record,
                parser_ir_bytes: Some(parser_ir_bytes),
                validation_ledger_bytes: Some(validation_ledger_bytes),
            })
        }
        Err(error) => {
            let mut record = base_record();
            record.status = QualificationStatus::NoOutput;
            record.failure = Some(QualificationFailure {
                kind: "conversion_failed".to_owned(),
                message: Some(error.to_string()),
            });
            Ok(QualificationWorkCapture {
                record,
                parser_ir_bytes: None,
                validation_ledger_bytes: None,
            })
        }
    }
}

fn canonical_bytes(value: Value) -> Result<Vec<u8>> {
    Ok((to_canonical_json_pretty(value)? + "\n").into_bytes())
}

fn blob_reference(bytes: &[u8]) -> BlobReference {
    BlobReference {
        sha256: format!("sha256:{:x}", Sha256::digest(bytes)),
        bytes: bytes.len() as u64,
        media_type: "application/json".to_owned(),
    }
}

pub fn aggregate_work_records(
    expected_work_ids: &[String],
    records: &[QualificationWorkRecord],
) -> Result<QualificationAggregate> {
    if expected_work_ids.is_empty() {
        bail!("qualification index must contain at least one work");
    }
    let expected: BTreeSet<_> = expected_work_ids.iter().collect();
    if expected.len() != expected_work_ids.len() {
        bail!("qualification index contains duplicate work ids");
    }
    let actual: BTreeSet<_> = records.iter().map(|record| &record.work_id).collect();
    if actual.len() != records.len() {
        bail!("qualification records contain duplicate work ids");
    }
    if expected != actual {
        bail!("qualification record membership does not match the closed index");
    }

    let first = &records[0];
    if records.iter().any(|record| {
        record.schema_id != WORK_SCHEMA_ID
            || record.schema_version != "1.0.0"
            || record.qualification_identity_ref != first.qualification_identity_ref
            || record.policy_hash != first.policy_hash
    }) {
        bail!("qualification records do not share one authenticated contract");
    }

    let schema_valid_outputs = records
        .iter()
        .filter(|record| record.status == QualificationStatus::SchemaValid)
        .count() as u64;
    let schema_invalid_outputs = records
        .iter()
        .filter(|record| record.status == QualificationStatus::SchemaInvalid)
        .count() as u64;
    let no_output_works = records
        .iter()
        .filter(|record| record.status == QualificationStatus::NoOutput)
        .count() as u64;
    let generated_outputs = schema_valid_outputs + schema_invalid_outputs;
    let unavailable = records
        .iter()
        .any(|record| record.status == QualificationStatus::Unavailable);
    let (status, parser_ir_schema_validation, reason) = if unavailable {
        (
            QualificationAggregateStatus::Unavailable,
            None,
            Some("blob_unavailable".to_owned()),
        )
    } else if generated_outputs == 0 {
        (QualificationAggregateStatus::NoParserIrOutput, None, None)
    } else {
        (
            QualificationAggregateStatus::Measured,
            Some(schema_valid_outputs as f64 / generated_outputs as f64),
            None,
        )
    };

    Ok(QualificationAggregate {
        schema_id: AGGREGATE_SCHEMA_ID.to_owned(),
        schema_version: "1.0.0".to_owned(),
        qualification_identity_ref: first.qualification_identity_ref.clone(),
        policy_hash: first.policy_hash.clone(),
        status,
        expected_works: expected_work_ids.len() as u64,
        generated_outputs,
        schema_valid_outputs,
        schema_invalid_outputs,
        no_output_works,
        parser_ir_schema_validation,
        reason,
    })
}
