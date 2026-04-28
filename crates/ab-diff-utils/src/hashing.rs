use serde_json::Value;
use sha2::{Digest, Sha256};

/// Returns `"sha256:{hex}"` for the given bytes.
pub fn hash_bytes(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    format!("sha256:{:x}", hasher.finalize())
}

/// Serializes `value` to JSON and returns its SHA256 hash.
pub fn hash_json(value: &Value) -> anyhow::Result<String> {
    let bytes = serde_json::to_vec(value)?;
    Ok(hash_bytes(&bytes))
}

/// Hashes a sequence of strings using length-prefixing.
///
/// Each string is prefixed with its 4-byte little-endian length before hashing,
/// so `[
/// "a", "b"]` differs from `["ab"]` and strings containing null bytes are
/// unambiguous.
pub fn hash_string_sequence(values: &[String]) -> String {
    let mut hasher = Sha256::new();
    for value in values {
        let len = value.len() as u32;
        hasher.update(len.to_le_bytes());
        hasher.update(value.as_bytes());
    }
    format!("sha256:{:x}", hasher.finalize())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hash_bytes_is_deterministic() {
        assert_eq!(hash_bytes(b"hello"), hash_bytes(b"hello"));
        assert_ne!(hash_bytes(b"hello"), hash_bytes(b"world"));
    }

    #[test]
    fn hash_json_is_deterministic() {
        let v = serde_json::json!({"a": 1});
        assert_eq!(hash_json(&v).unwrap(), hash_json(&v).unwrap());
    }

    #[test]
    fn hash_string_sequence_uses_length_prefix_not_null_delimiter() {
        let a = hash_string_sequence(&["a".into(), "b".into()]);
        let b = hash_string_sequence(&["ab".into()]);
        assert_ne!(a, b);
        let c = hash_string_sequence(&["a\0b".into()]);
        let d = hash_string_sequence(&["a".into(), "b".into()]);
        assert_ne!(c, d);
        let e = hash_string_sequence(&["a".into(), "b".into()]);
        assert_eq!(a, e);
    }
}
