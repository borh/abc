use ab_aat::{decode_source_bytes, diagnostics_json_from_bytes};
use serde_json::Value;
use sha2::{Digest, Sha256};

use crate::index::qualification_identity_ref;
use crate::model::{
    BlobRef, CoordinateSystem, DecodedBlobRef, DecodedEncoding, DerivedFrom, DiagnosticBlobRef,
    DiagnosticProfile, InstrumentVersion, JsonMediaType, ParserIrBlobRef, WorkAnalysis, WorkInput,
    WorkRecord, WorkSchemaVersion, WorkStatus,
};

fn hash(bytes: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(bytes))
}

pub fn analyze_work(input: WorkInput) -> WorkAnalysis {
    analyze_work_with_diagnostics(input, diagnostics_json_from_bytes)
}

fn analyze_work_with_diagnostics<F>(input: WorkInput, diagnostics: F) -> WorkAnalysis
where
    F: FnOnce(&[u8]) -> anyhow::Result<Vec<u8>>,
{
    let decoded =
        decode_source_bytes(&input.original_bytes).expect("decoder is total for byte input");
    let diagnostics_result = diagnostics(&input.original_bytes);
    let parser_value: Option<Value> = serde_json::from_slice(&input.parser_ir_bytes).ok();
    let schema_id = parser_value
        .as_ref()
        .and_then(|v| v.get("schema_id"))
        .and_then(Value::as_str)
        .unwrap_or("")
        .to_owned();
    let schema_hash = parser_value
        .as_ref()
        .and_then(|v| v.get("schema_hash"))
        .and_then(Value::as_str)
        .unwrap_or("")
        .to_owned();
    let identity_ref =
        qualification_identity_ref(&input.qualification_identity).expect("identity serializes");
    let decoded_len = decoded.text.len();
    let mut errors = Vec::new();

    if diagnostics_result.is_err() {
        errors.push("diagnostics-unavailable".to_owned());
    }
    if decoded.encoding == "windows-31j-lossy" {
        errors.push("lossy-source-decode".to_owned());
    }
    if hash(&input.original_bytes) != input.corpus_entry.original_sha256 {
        errors.push("source-identity-mismatch".to_owned());
    }
    if hash(&input.taxonomy.taxonomy_jcs_bytes) != input.taxonomy.taxonomy_hash {
        errors.push("taxonomy-identity-mismatch".to_owned());
    }
    if schema_id != input.qualification_identity.parser_ir_schema_id
        || schema_hash != input.qualification_identity.parser_ir_schema_hash
    {
        errors.push("parser-ir-schema-identity-mismatch".to_owned());
    }

    if let Some(value) = parser_value.as_ref() {
        let source_hash = value
            .pointer("/source/work_content_hash")
            .and_then(Value::as_str);
        if source_hash != Some(input.corpus_entry.original_sha256.as_str()) {
            errors.push("parser-ir-source-identity-mismatch".to_owned());
        }
        match value
            .get("derived_from")
            .cloned()
            .and_then(|v| serde_json::from_value::<DerivedFrom>(v).ok())
        {
            Some(d)
                if d.aat_version == input.qualification_identity.aat_version
                    && d.aat_adapter == input.qualification_identity.aat_adapter
                    && d.aat_adapter_version.as_deref()
                        == Some(input.qualification_identity.aat_adapter_version.as_str())
                    && d.mapping_id == input.qualification_identity.mapping_id
                    && d.mapping_version == input.qualification_identity.mapping_version
                    && d.mapping_schema_hash
                        == input.qualification_identity.mapping_schema_hash => {}
            _ => errors.push("parser-ir-derived-from-identity-mismatch".to_owned()),
        }
    } else {
        errors.push("parser-ir-malformed".to_owned());
    }

    // v1 walked `nodes[*].span` here, bounding each span against the decoded
    // source and unioning the result into a coverage quantity. The walk went
    // with the quantity: parser-IR spans are offsets into each node's own
    // emitted visible text, so bounding them against the decoded file checked
    // nothing -- emitted text is shorter than its source, so every span passed.
    // Parser-IR node structure is the conformance instrument's subject
    // (`parser-rq-parser-ir-conformance-*`), not this one's. What this
    // instrument authenticates about parser-IR is above: its schema identity
    // and its `derived_from` coordinates.

    let record = WorkRecord {
        schema_version: WorkSchemaVersion::V2,
        identity_ref,
        instrument_version: InstrumentVersion::V1,
        work_id: input.corpus_entry.work_id,
        original_source: BlobRef {
            sha256: hash(&input.original_bytes),
            bytes: input.original_bytes.len() as u64,
        },
        decoded_source: DecodedBlobRef {
            sha256: hash(decoded.text.as_bytes()),
            bytes: decoded_len as u64,
            encoding: match decoded.encoding {
                "utf-8" => DecodedEncoding::Utf8,
                "utf-8-bom" => DecodedEncoding::Utf8Bom,
                "windows-31j" => DecodedEncoding::Windows31j,
                "windows-31j-lossy" => DecodedEncoding::Windows31jLossy,
                _ => unreachable!("decoder returned an unknown encoding"),
            },
        },
        parser_ir: ParserIrBlobRef {
            schema_id,
            schema_hash,
            sha256: hash(&input.parser_ir_bytes),
            bytes: input.parser_ir_bytes.len() as u64,
        },
        diagnostics: diagnostics_result
            .as_ref()
            .ok()
            .map(|bytes| DiagnosticBlobRef {
                profile: DiagnosticProfile::RawSchemaV3,
                sha256: hash(bytes),
                bytes: bytes.len() as u64,
                media_type: JsonMediaType::ApplicationJson,
                locator: input.diagnostics_locator,
            }),
        taxonomy_version: input.taxonomy.taxonomy_version,
        taxonomy_hash: input.taxonomy.taxonomy_hash,
        coordinate_system: CoordinateSystem::DecodedUtf8,
        status: if errors.is_empty() {
            WorkStatus::Ok
        } else {
            WorkStatus::Unavailable
        },
        errors,
    };
    WorkAnalysis {
        record,
        diagnostics_bytes: diagnostics_result.ok(),
    }
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;
    use crate::model::{CorpusEntry, QualificationIdentity, TaxonomyIdentity, TaxonomyVersion};

    fn input() -> WorkInput {
        let original_bytes = b"x".to_vec();
        let original_sha256 = hash(&original_bytes);
        let schema_hash = format!("sha256:{}", "3".repeat(64));
        let mapping_schema_hash = format!("sha256:{}", "2".repeat(64));
        let parser_ir_bytes = serde_json::to_vec(&json!({
            "schema_id": "https://example.test/parser-ir",
            "schema_hash": schema_hash,
            "derived_from": {
                "aat_version": 2,
                "aat_adapter": "ab-aat",
                "aat_adapter_version": "0.1.0",
                "mapping_id": "https://example.test/mapping",
                "mapping_version": "1",
                "mapping_schema_hash": mapping_schema_hash,
            },
            "source": {"work_content_hash": original_sha256},
            "nodes": [{"span": {
                "start": 0, "end": 1, "coordinate_system": "decoded_utf8"
            }}],
        }))
        .unwrap();
        let taxonomy_jcs_bytes = br#"{"coordinate_system":"decoded_utf8","rules":[],"schema_version":"abc/parser-rq-ignored-regions/v1","taxonomy_version":"parser-rq-ignored-regions-v1"}"#.to_vec();
        WorkInput {
            original_bytes,
            parser_ir_bytes,
            corpus_entry: CorpusEntry {
                work_id: "work-1".into(),
                original_sha256,
            },
            qualification_identity: QualificationIdentity {
                parser_git_rev: "abc123".into(),
                aat_version: 2,
                aat_adapter: "ab-aat".into(),
                aat_adapter_version: "0.1.0".into(),
                mapping_id: "https://example.test/mapping".into(),
                mapping_version: "1".into(),
                mapping_hash: format!("sha256:{}", "1".repeat(64)),
                mapping_schema_hash,
                parser_ir_schema_id: "https://example.test/parser-ir".into(),
                parser_ir_schema_hash: schema_hash,
                corpus_snapshot_hash: format!("sha256:{}", "4".repeat(64)),
                corpus_list_hash: format!("sha256:{}", "5".repeat(64)),
                predicate_set_hash: format!("sha256:{}", "6".repeat(64)),
                instrument_versions: std::collections::BTreeMap::from([(
                    "source_accountability".into(),
                    "parser-rq-source-accountability-v1".into(),
                )]),
                instrument_policy_hashes: std::collections::BTreeMap::from([(
                    "source_recognition".into(),
                    format!("sha256:{}", "7".repeat(64)),
                )]),
            },
            taxonomy: TaxonomyIdentity {
                taxonomy_version: TaxonomyVersion::V1,
                taxonomy_hash: hash(&taxonomy_jcs_bytes),
                taxonomy_jcs_bytes,
            },
            diagnostics_locator: "work-1.diagnostics.json".into(),
        }
    }

    #[test]
    fn diagnostic_failure_is_unavailable_without_a_blob_claim() {
        let result = analyze_work_with_diagnostics(input(), |_| {
            anyhow::bail!("injected diagnostic failure")
        });
        assert_eq!(result.record.status, WorkStatus::Unavailable);
        assert!(result.record.diagnostics.is_none());
        assert!(result.diagnostics_bytes.is_none());
        assert!(
            serde_json::to_value(&result.record)
                .unwrap()
                .get("diagnostics")
                .is_none()
        );
        assert!(
            result
                .record
                .errors
                .contains(&"diagnostics-unavailable".into())
        );
    }
}
