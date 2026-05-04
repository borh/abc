use anyhow::{Context, Result};
use serde_json::json;
use sha2::{Digest, Sha256};
use std::{error, fmt};

mod jis2ucs;
mod model;
mod source_derived;
mod xhtml_mapper;

pub use model::{
    parse_failure_envelope, AtBlock, AtInline, DecodedSource, MappingError, MappingInput,
    MappingResult, SourceDerivedContext, SourceDerivedSummary,
};

pub use model::{ADAPTER_NAME, ADAPTER_VERSION};

#[derive(Debug)]
pub struct DecodeError {
    pub message: String,
}

impl error::Error for DecodeError {}

impl fmt::Display for DecodeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)
    }
}

pub fn decode_source_bytes(bytes: &[u8]) -> Result<DecodedSource> {
    if bytes.starts_with(&[0xef, 0xbb, 0xbf]) {
        let text = std::str::from_utf8(&bytes[3..])
            .context("input claimed UTF-8 BOM but was not valid UTF-8")?;
        return Ok(DecodedSource {
            text: text.to_string(),
            encoding: "utf-8-bom",
            source_hash: source_hash(bytes),
        });
    }

    if let Ok(text) = std::str::from_utf8(bytes) {
        return Ok(DecodedSource {
            text: text.to_string(),
            encoding: "utf-8",
            source_hash: source_hash(bytes),
        });
    }

    let (text, _, had_errors) = encoding_rs::SHIFT_JIS.decode(bytes);
    let encoding = if had_errors {
        "windows-31j-lossy"
    } else {
        "windows-31j"
    };

    Ok(DecodedSource {
        text: text.into_owned(),
        encoding,
        source_hash: source_hash(bytes),
    })
}

pub fn source_hash(bytes: &[u8]) -> String {
    format!("sha256:{}", hex_sha256(bytes))
}

fn hex_sha256(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    let digest = hasher.finalize();
    digest.iter().map(|b| format!("{b:02x}")).collect()
}

pub fn map_with_protocol_bytes(
    xhtml: &[u8],
    source_bytes: &[u8],
    parse_complete: bool,
) -> anyhow::Result<serde_json::Value> {
    map_with_protocol(MappingInput {
        xhtml: xhtml.to_vec(),
        source_bytes: source_bytes.to_vec(),
        parse_complete,
        parser_error_message: None,
    })
}

pub fn map_with_protocol(input: MappingInput) -> anyhow::Result<serde_json::Value> {
    let decoded = decode_source_bytes(&input.source_bytes)?;

    if !input.parse_complete {
        let warning = input.parser_error_message.clone().unwrap_or_default();
        return Ok(parse_failure_envelope(
            decoded.encoding,
            &decoded.source_hash,
            if warning.is_empty() {
                "aozora2html parser aborted".to_string()
            } else {
                format!("aozora2html parser aborted: {warning}")
            },
        ));
    }

    let mut warnings = Vec::new();
    let mut ctx = SourceDerivedContext::default();
    let (mut blocks, parse_complete) = match xhtml_mapper::map_blocks_from_xhtml_bytes(
        &input.xhtml,
        &mut warnings,
        &mut ctx.summary,
    ) {
        Ok(result) => result,
        Err(err) => {
            return Ok(err.into_envelope(decoded.encoding, &decoded.source_hash));
        }
    };
    // Source-derived recovery is still scaffolded to preserve behavior while the
    // full port is migrated from adapter.py.
    source_derived::apply_source_derived_recovery(&mut blocks, &decoded.text, &mut ctx);
    let blocks = source_derived::attach_following_captions(blocks, &mut ctx);
    warnings.extend(ctx.warnings);

    let mut semantic_summary = ctx.summary.as_json();
    if semantic_summary.is_object() && semantic_summary.as_object().is_some_and(|obj| obj.is_empty()) {
        semantic_summary = serde_json::Value::Null;
    }

    let mut result = json!({
        "version": 1,
        "work_id": "stdin",
        "blocks": blocks,
        "meta": {
            "adapter": ADAPTER_NAME,
            "adapter_version": ADAPTER_VERSION,
            "source_encoding": decoded.encoding,
            "source_hash": decoded.source_hash,
            "parse_complete": parse_complete,
            "warnings": warnings,
        },
    });

    if !semantic_summary.is_null() {
        result["meta"]["semantic_summary"] = semantic_summary;
    }
    Ok(result)
}

pub fn map_with_error_message(
    xhtml: &[u8],
    source_bytes: &[u8],
    parser_failed: bool,
    parser_error_message: Option<String>,
) -> anyhow::Result<serde_json::Value> {
    map_with_protocol(MappingInput {
        xhtml: xhtml.to_vec(),
        source_bytes: source_bytes.to_vec(),
        parse_complete: !parser_failed,
        parser_error_message,
    })
}
