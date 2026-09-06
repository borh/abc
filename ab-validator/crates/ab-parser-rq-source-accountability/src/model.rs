use std::collections::BTreeMap;
use std::path::PathBuf;

use serde::{Deserialize, Serialize};

macro_rules! wire_enum {
    ($name:ident { $($variant:ident => $wire:literal),+ $(,)? }) => {
        #[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
        pub enum $name {
            $(#[serde(rename = $wire)] $variant),+
        }
    };
}

wire_enum!(WorkSchemaVersion { V2 => "abc/parser-rq-source-accountability-work/v2" });
wire_enum!(InstrumentVersion { V1 => "parser-rq-source-accountability-v1" });
wire_enum!(DecodedEncoding {
    Utf8 => "utf-8",
    Utf8Bom => "utf-8-bom",
    Windows31j => "windows-31j",
    Windows31jLossy => "windows-31j-lossy",
});
wire_enum!(DiagnosticProfile { RawSchemaV3 => "abc/raw-parser-diagnostics-schema-v3" });
wire_enum!(JsonMediaType { ApplicationJson => "application/json" });
wire_enum!(TaxonomyVersion { V1 => "parser-rq-ignored-regions-v1" });
wire_enum!(CoordinateSystem { DecodedUtf8 => "decoded_utf8" });
wire_enum!(WorkStatus { Ok => "ok", Unavailable => "unavailable" });

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct QualificationIdentity {
    pub aat_version: u64,
    pub aat_adapter: String,
    pub aat_adapter_version: String,
    pub parser_git_rev: String,
    pub mapping_id: String,
    pub mapping_version: String,
    pub mapping_hash: String,
    pub mapping_schema_hash: String,
    pub parser_ir_schema_id: String,
    pub parser_ir_schema_hash: String,
    pub corpus_snapshot_hash: String,
    pub corpus_list_hash: String,
    pub predicate_set_hash: String,
    pub instrument_versions: BTreeMap<String, String>,
    /// Content identity of each instrument's governed policy document. The
    /// prose in `instrument_versions` describes how a measurement was taken;
    /// this is the coordinate that actually moves when that changes.
    pub instrument_policy_hashes: BTreeMap<String, String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct CorpusEntry {
    pub work_id: String,
    pub original_sha256: String,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct CorpusSourceEntry {
    #[serde(flatten)]
    pub corpus_entry: CorpusEntry,
    pub source_path: PathBuf,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct TaxonomyIdentity {
    pub taxonomy_version: TaxonomyVersion,
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
    pub encoding: DecodedEncoding,
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
    pub profile: DiagnosticProfile,
    pub sha256: String,
    pub bytes: u64,
    pub media_type: JsonMediaType,
    pub locator: String,
}

/// What the instrument records about one work: which source, decoded how, which
/// parser-IR, which diagnostics, under which taxonomy, authenticated against
/// which qualification identity.
///
/// The record carries no coverage quantity: parser-IR spans address emitted
/// text and cannot be unioned against decoded source bytes. `coordinate_system`
/// describes `decoded_source.bytes`; it does not qualify any interval.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct WorkRecord {
    pub schema_version: WorkSchemaVersion,
    pub identity_ref: String,
    pub instrument_version: InstrumentVersion,
    pub work_id: String,
    pub original_source: BlobRef,
    pub decoded_source: DecodedBlobRef,
    pub parser_ir: ParserIrBlobRef,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub diagnostics: Option<DiagnosticBlobRef>,
    pub taxonomy_version: TaxonomyVersion,
    pub taxonomy_hash: String,
    pub coordinate_system: CoordinateSystem,
    pub status: WorkStatus,
    pub errors: Vec<String>,
}

#[derive(Clone, Debug)]
pub struct WorkAnalysis {
    pub record: WorkRecord,
    pub diagnostics_bytes: Option<Vec<u8>>,
}

#[derive(Clone, Debug)]
pub struct CorpusInput {
    /// Immutable, campaign-owned capture inputs. Callers must not mutate either
    /// input root while analysis is running.
    pub entries: Vec<CorpusSourceEntry>,
    pub source_root: PathBuf,
    pub parser_ir_root: PathBuf,
    pub store_root: PathBuf,
    pub index_out: PathBuf,
    pub qualification_identity: QualificationIdentity,
    pub taxonomy: TaxonomyIdentity,
}

wire_enum!(RecordIndexSchemaVersion { V1 => "abc/parser-rq-source-accountability-index/v1" });

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RecordIndexEntry {
    pub work_id: String,
    pub sha256: String,
    pub bytes: u64,
    pub media_type: JsonMediaType,
    pub locator: String,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RecordIndex {
    pub schema_version: RecordIndexSchemaVersion,
    pub identity_ref: String,
    pub taxonomy_version: TaxonomyVersion,
    pub taxonomy_hash: String,
    pub coordinate_system: CoordinateSystem,
    pub status: WorkStatus,
    pub expected_work_count: u64,
    pub record_count: u64,
    pub records: Vec<RecordIndexEntry>,
    pub errors: Vec<String>,
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
