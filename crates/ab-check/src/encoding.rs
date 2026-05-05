use anyhow::Result;
use encoding_rs::SHIFT_JIS;
use sha2::{Digest, Sha256};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DecodedSource {
    pub text: String,
    pub encoding: String,
    pub raw_sha256: String,
}

/// Decode UTF-8 bytes with SHIFT_JIS fallback.
///
/// # Errors
///
/// Returns an error when UTF-8 conversion fails unexpectedly.
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

#[must_use] 
pub fn hex_sha256(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    format!("{:x}", hasher.finalize())
}
