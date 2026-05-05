use serde_json::{json, Value};
use std::collections::BTreeMap;

pub const ADAPTER_NAME: &str = "aozora2html";
pub const ADAPTER_VERSION: &str = "aozora2html-adapter 0.1.0 gem-3.0.1";

pub type AtBlock = Value;
pub type AtInline = Value;

#[derive(Debug, Default, Clone)]
pub struct SourceDerivedSummary {
    pub syntax: BTreeMap<String, Vec<Value>>,
}

impl SourceDerivedSummary {
    pub fn push_syntax(&mut self, key: impl Into<String>, value: Value) {
        self.syntax.entry(key.into()).or_default().push(value);
    }

    pub fn as_json(&self) -> Value {
        if self.syntax.is_empty() {
            json!({})
        } else {
            json!({
                "syntax": self.syntax,
            })
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct SourceDerivedContext {
    pub warnings: Vec<Value>,
    pub summary: SourceDerivedSummary,
}

#[derive(Debug)]
pub struct MappingInput {
    pub xhtml: Vec<u8>,
    pub source_bytes: Vec<u8>,
    pub parse_complete: bool,
    pub parser_error_message: Option<String>,
}

#[derive(Debug)]
pub struct MappingResult {
    pub blocks: Vec<AtBlock>,
    pub warnings: Vec<Value>,
    pub semantic_summary: SourceDerivedSummary,
}

#[derive(Debug)]
pub enum MappingErrorKind {
    DecodeError,
    ParseError,
    XmlError,
    EncodingError,
}

#[derive(Debug)]
pub struct MappingError {
    pub kind: MappingErrorKind,
    pub message: String,
    pub warnings: Vec<Value>,
    pub parse_complete: bool,
}

impl MappingError {
    pub fn parse_error(message: impl Into<String>, warnings: Vec<Value>, parse_complete: bool) -> Self {
        Self {
            kind: MappingErrorKind::ParseError,
            message: message.into(),
            warnings,
            parse_complete,
        }
    }

    pub fn parser_failed(message: impl Into<String>) -> Self {
        Self {
            kind: MappingErrorKind::ParseError,
            message: message.into(),
            warnings: Vec::new(),
            parse_complete: false,
        }
    }

    pub fn into_envelope(&self, source_encoding: &str, source_hash: &str) -> Value {
        let mut warnings = self.warnings.clone();
        if !self.message.is_empty() {
            warnings.push(json!({"message": self.message}));
        }
        json!({
            "version": 1,
            "work_id": "stdin",
            "blocks": [],
            "meta": {
                "adapter": ADAPTER_NAME,
                "adapter_version": ADAPTER_VERSION,
                "source_encoding": source_encoding,
                "source_hash": source_hash,
                "parse_complete": self.parse_complete,
                "warnings": warnings,
            },
        })
    }

    pub fn with_warnings(
        message: impl Into<String>,
        warnings: Vec<Value>,
        parse_complete: bool,
    ) -> Self {
        Self {
            kind: MappingErrorKind::ParseError,
            message: message.into(),
            warnings,
            parse_complete,
        }
    }
}

pub fn parse_failure_envelope(
    source_encoding: &str,
    source_hash: &str,
    message: impl Into<String>,
) -> Value {
    MappingError::with_warnings(message, Vec::new(), false)
        .into_envelope(source_encoding, source_hash)
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct DecodedSource {
    pub text: String,
    pub encoding: &'static str,
    pub source_hash: String,
}

#[derive(Debug, Serialize, Deserialize)]
