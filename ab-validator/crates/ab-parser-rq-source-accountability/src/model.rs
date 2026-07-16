use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct AdapterCoordinates {
    pub aat_version: u64,
    pub aat_adapter: String,
    pub aat_adapter_version: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct QualificationIdentity {
    pub parser_git_rev: String,
    pub adapter_coordinates: AdapterCoordinates,
    pub mapping_id: String,
    pub mapping_version: String,
    pub mapping_hash: String,
    pub mapping_schema_hash: String,
    pub parser_ir_schema_id: String,
    pub parser_ir_schema_hash: String,
    pub corpus_snapshot_hash: String,
    pub corpus_list_hash: String,
    pub predicate_set_hash: String,
    pub instrument_versions: Vec<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct CorpusEntry {
    pub work_id: String,
    pub original_sha256: String,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct TaxonomyIdentity {
    pub taxonomy_version: String,
    pub taxonomy_hash: String,
    #[serde(skip)]
    pub taxonomy_jcs_bytes: Vec<u8>,
}

#[derive(Clone, Debug)]
pub struct WorkInput {
    pub original_bytes: Vec<u8>,
    pub parser_ir_bytes: Vec<u8>,
    pub corpus_entry: CorpusEntry,
    pub qualification_identity: QualificationIdentity,
    pub taxonomy: TaxonomyIdentity,
    pub diagnostics_locator: String,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct BlobRef {
    pub sha256: String,
    pub bytes: u64,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct DecodedBlobRef {
    pub sha256: String,
    pub bytes: u64,
    pub encoding: String,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ParserIrBlobRef {
    pub schema_id: String,
    pub schema_hash: String,
    pub sha256: String,
    pub bytes: u64,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct DiagnosticBlobRef {
    pub profile: String,
    pub sha256: String,
    pub bytes: u64,
    pub media_type: String,
    pub locator: String,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct WireInterval {
    pub start: u64,
    pub end: u64,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct WorkRecord {
    pub schema_version: String,
    pub identity_ref: String,
    pub instrument_version: String,
    pub work_id: String,
    pub original_source: BlobRef,
    pub decoded_source: DecodedBlobRef,
    pub parser_ir: ParserIrBlobRef,
    pub diagnostics: DiagnosticBlobRef,
    pub taxonomy_version: String,
    pub taxonomy_hash: String,
    pub coordinate_system: String,
    pub coverage_basis: String,
    pub status: String,
    pub ignored: Vec<WireInterval>,
    pub eligible: Vec<WireInterval>,
    pub covered_eligible: Vec<WireInterval>,
    pub uncovered_eligible: Vec<WireInterval>,
    pub decoded_source_bytes: u64,
    pub ignored_bytes: u64,
    pub eligible_bytes: u64,
    pub covered_eligible_bytes: u64,
    pub uncovered_eligible_bytes: u64,
    pub errors: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct WorkAnalysis {
    pub record: WorkRecord,
    pub diagnostics_bytes: Vec<u8>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct DerivedFrom {
    pub aat_version: u64,
    pub aat_adapter: String,
    pub aat_adapter_version: Option<String>,
    pub mapping_id: String,
    pub mapping_version: String,
    pub mapping_schema_hash: String,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct NodeSpan {
    pub start: usize,
    pub end: usize,
    pub coordinate_system: Option<String>,
    #[serde(rename = "line")]
    pub _line: Option<u64>,
    #[serde(rename = "column")]
    pub _column: Option<u64>,
}
