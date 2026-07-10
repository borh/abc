use anyhow::{Context, Result};
use sha2::{Digest, Sha256};

use crate::model::DecodedSource;

/// Decode raw work bytes into text, detecting UTF-8 (with optional BOM) and
/// falling back to Shift-JIS / windows-31j. Records the detected encoding and
/// a sha256 hash of the original bytes.
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
    Ok(DecodedSource {
        text: text.into_owned(),
        encoding: if had_errors {
            "windows-31j-lossy"
        } else {
            "windows-31j"
        },
        source_hash: source_hash(bytes),
    })
}

/// `sha256:<hex>` digest of the raw work bytes (stable across encodings).
#[must_use]
pub fn source_hash(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    format!("sha256:{:x}", hasher.finalize())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(serde::Deserialize)]
    struct SourceDecodingVector {
        name: String,
        bytes: Vec<u8>,
        text: String,
        encoding: String,
        sha256: String,
    }

    #[test]
    fn source_decoding_contract() {
        let vectors: Vec<SourceDecodingVector> = serde_json::from_str(include_str!(
            "../../../data/fixtures/source-decoding-contract.json"
        ))
        .unwrap();
        for vector in vectors {
            let decoded = decode_source_bytes(&vector.bytes).unwrap();
            assert_eq!(decoded.text, vector.text, "{} text", vector.name);
            assert_eq!(
                decoded.encoding, vector.encoding,
                "{} encoding",
                vector.name
            );
            assert_eq!(decoded.source_hash, vector.sha256, "{} hash", vector.name);
        }
    }

    #[test]
    fn decodes_utf8_and_hashes() {
        let bytes = "テスト".as_bytes();
        let decoded = decode_source_bytes(bytes).unwrap();
        assert_eq!(decoded.text, "テスト");
        assert_eq!(decoded.encoding, "utf-8");
        assert!(decoded.source_hash.starts_with("sha256:"));
    }

    #[test]
    fn decodes_utf8_bom() {
        let mut bytes = vec![0xef, 0xbb, 0xbf];
        bytes.extend_from_slice("テスト".as_bytes());
        let decoded = decode_source_bytes(&bytes).unwrap();
        assert_eq!(decoded.text, "テスト");
        assert_eq!(decoded.encoding, "utf-8-bom");
    }

    #[test]
    fn decodes_shift_jis() {
        let bytes = encoding_rs::SHIFT_JIS.encode("テスト").0.into_owned();
        let decoded = decode_source_bytes(&bytes).unwrap();
        assert_eq!(decoded.text, "テスト");
        assert_eq!(decoded.encoding, "windows-31j");
    }
}
