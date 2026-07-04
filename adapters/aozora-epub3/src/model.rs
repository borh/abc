use serde_json::{json, Value};

pub const ADAPTER_NAME: &str = "aozora-epub3";
pub const ADAPTER_VERSION: &str = "aozora-epub3-adapter 0.1.0 AozoraEpub3-JDK21-1.3.4-jdk21";

#[derive(Debug)]
pub struct DecodedSource {
    pub text: String,
    pub encoding: &'static str,
    pub source_hash: String,
}

#[derive(Debug)]
pub struct MappingInput {
    pub source_bytes: Vec<u8>,
    pub xhtml_documents: Vec<XhtmlDocument>,
    pub parser_failed: bool,
    pub parser_error_message: Option<String>,
}

#[derive(Debug)]
pub struct XhtmlDocument {
    pub bytes: Vec<u8>,
    pub kind: XhtmlDocumentKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XhtmlDocumentKind {
    BodySection,
    Colophon,
}

/// Build an AAT envelope describing a parser failure (parse_complete=false).
#[must_use]
pub fn parse_failure_envelope(
    source_encoding: &str,
    source_hash: &str,
    message: impl Into<String>,
) -> Value {
    json!({
        "version": 1,
        "work_id": "stdin",
        "blocks": [],
        "meta": {
            "adapter": ADAPTER_NAME,
            "adapter_version": ADAPTER_VERSION,
            "source_encoding": source_encoding,
            "source_hash": source_hash,
            "parse_complete": false,
            "warnings": [{"message": message.into()}],
        },
    })
}