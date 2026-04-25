use anyhow::{Result, bail};
use encoding_rs::SHIFT_JIS;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DecodedSource {
    pub text: String,
    pub encoding: String,
    pub raw_sha256: String,
}

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
    if had_errors {
        bail!("source is neither valid UTF-8 nor decodable Windows-31J");
    }

    Ok(DecodedSource {
        text: cow.into_owned(),
        encoding: "windows-31j".to_owned(),
        raw_sha256,
    })
}

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
}
