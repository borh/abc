//! Canonical source-byte decoder for Aozora Bunko source files.
//!
//! Single source of truth for the decode + hash pipeline shared by `ab-check`
//! and (text-only) `ab-coverage`. The decoder strips a UTF-8 BOM,
//! then tries strict UTF-8, then falls back to Windows-31J (Shift_JIS) with a
//! `windows-31j-lossy` classification when bytes are invalid (matching the
//! behaviour of reference Aozora parsers).

use anyhow::Result;
use encoding_rs::SHIFT_JIS;
use sha2::{Digest, Sha256};

/// Decoded source text plus its detected encoding and raw byte SHA-256.
///
/// `encoding` is one of: `utf-8-bom`, `utf-8`, `windows-31j`, `windows-31j-lossy`.
/// `raw_sha256` is the `sha256:<hex>` of the original bytes (pre-BOM-strip).
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct DecodedSource {
    pub text: String,
    pub encoding: String,
    pub raw_sha256: String,
}

/// Decode source bytes with UTF-8 (BOM-aware) detection and SHIFT-JIS fallback.
///
/// # Errors
///
/// Returns an error when the bytes start with a UTF-8 BOM but the bytes that
/// follow are not valid UTF-8 (such as a file that claims to be UTF-8-encoded but
/// is not). Plain UTF-8 and Windows-31J inputs never error.
pub fn decode_source_bytes(bytes: &[u8]) -> Result<DecodedSource> {
    let raw_sha256 = format!("sha256:{}", hex_sha256(bytes));

    if bytes.starts_with(&[0xef, 0xbb, 0xbf]) {
        let text = std::str::from_utf8(&bytes[3..])?.to_owned();
        return Ok(DecodedSource {
            text,
            encoding: "utf-8-bom".to_owned(),
            raw_sha256,
        });
    }

    if let Ok(text) = std::str::from_utf8(bytes) {
        return Ok(DecodedSource {
            text: text.to_owned(),
            encoding: "utf-8".to_owned(),
            raw_sha256,
        });
    }

    let (cow, _, had_errors) = SHIFT_JIS.decode(bytes);
    Ok(DecodedSource {
        text: cow.into_owned(),
        encoding: if had_errors {
            "windows-31j-lossy"
        } else {
            "windows-31j"
        }
        .to_owned(),
        raw_sha256,
    })
}

/// Lowercase hex SHA-256 of the input bytes.
#[must_use]
pub fn hex_sha256(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    format!("{:x}", hasher.finalize())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decodes_utf8_bom() {
        let decoded = decode_source_bytes(b"\xef\xbb\xbfabc").unwrap();
        assert_eq!(decoded.text, "abc");
        assert_eq!(decoded.encoding, "utf-8-bom");
    }

    #[test]
    fn decodes_windows_31j() {
        let bytes = [0x8c, 0xe1, 0x94, 0x79];
        let decoded = decode_source_bytes(&bytes).unwrap();
        assert_eq!(decoded.text, "吾輩");
        assert_eq!(decoded.encoding, "windows-31j");
    }

    #[test]
    fn decodes_windows_31j_lossy_like_reference_parsers() {
        let decoded = decode_source_bytes(&[0x82, 0xa0, 0xff]).unwrap();
        assert_eq!(decoded.text, "あ�");
        assert_eq!(decoded.encoding, "windows-31j-lossy");
    }

    #[test]
    fn raw_sha256_is_hex_of_original_bytes() {
        let decoded = decode_source_bytes(b"\xef\xbb\xbfabc").unwrap();
        // sha256 over the BOM+payload, not the stripped text.
        let mut h = Sha256::new();
        h.update(b"\xef\xbb\xbfabc");
        assert_eq!(decoded.raw_sha256, format!("sha256:{:x}", h.finalize()));
    }

    #[test]
    fn errors_when_bom_prefix_but_body_is_not_utf8() {
        // BOM claims UTF-8, but a lone 0xff is not valid UTF-8.
        let err = decode_source_bytes(&[0xef, 0xbb, 0xbf, 0xff]).unwrap_err();
        assert!(
            err.to_string().contains("utf-8"),
            "expected utf-8 error, got: {err}"
        );
    }
}
