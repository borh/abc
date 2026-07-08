use serde_json::Value;
use sha2::{Digest, Sha256};

/// Returns `"sha256:{hex}"` for the given bytes.
#[must_use]
pub fn hash_bytes(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    format!("sha256:{:x}", hasher.finalize())
}

/// Serializes `value` to JSON and returns its SHA256 hash.
///
/// # Errors
///
/// Returns an error when JSON serialization fails.
#[must_use = "hash_json returns a digest string"]
pub fn hash_json(value: &Value) -> anyhow::Result<String> {
    let bytes = serde_json::to_vec(value)?;
    Ok(hash_bytes(&bytes))
}

/// Returns canonical JSON for ABC hashable values that do not contain floats.
///
/// This helper is intentionally conservative: it supports the JSON value kinds
/// used by alignment configs and returns an error for floating-point numbers.
#[must_use = "canonical_json_string returns canonical bytes as a string"]
pub fn canonical_json_string(value: &Value) -> anyhow::Result<String> {
    Ok(match value {
        Value::Null => "null".to_owned(),
        Value::Bool(value) => value.to_string(),
        Value::Number(value) => {
            if value.is_i64() || value.is_u64() {
                value.to_string()
            } else {
                anyhow::bail!("canonical_json_string does not support floating point numbers");
            }
        }
        Value::String(value) => serde_json::to_string(value)?,
        Value::Array(values) => {
            let inner = values
                .iter()
                .map(canonical_json_string)
                .collect::<anyhow::Result<Vec<_>>>()?
                .join(",");
            format!("[{inner}]")
        }
        Value::Object(values) => {
            let mut entries: Vec<_> = values.iter().collect();
            entries.sort_by(|(left, _), (right, _)| left.cmp(right));
            let inner = entries
                .into_iter()
                .map(|(key, value)| {
                    Ok(format!(
                        "{}:{}",
                        serde_json::to_string(key)?,
                        canonical_json_string(value)?
                    ))
                })
                .collect::<anyhow::Result<Vec<_>>>()?
                .join(",");
            format!("{{{inner}}}")
        }
    })
}

/// Hashes canonical JSON bytes and returns `"sha256:{hex}"`.
#[must_use = "hash_json_canonical returns a digest string"]
pub fn hash_json_canonical(value: &Value) -> anyhow::Result<String> {
    Ok(hash_bytes(canonical_json_string(value)?.as_bytes()))
}

/// Hashes a sequence of strings using length-prefixing.
///
/// Each string is prefixed with its 4-byte little-endian length before hashing,
/// so `[
/// "a", "b"]` differs from `["ab"]` and strings containing null bytes are
/// unambiguous.
#[must_use]
pub fn hash_string_sequence(values: &[String]) -> String {
    let digest = hash_string_sequence_raw(values);
    let mut out = String::with_capacity(7 + 64);
    out.push_str("sha256:");
    for byte in digest {
        use std::fmt::Write;
        let _ = write!(out, "{byte:02x}");
    }
    out
}

/// Raw 32-byte digest variant of [`hash_string_sequence`], for callers
/// that key maps by digest without paying for hex strings.
#[must_use]
pub fn hash_string_sequence_raw(values: &[String]) -> [u8; 32] {
    let mut hasher = Sha256::new();
    for value in values {
        let len = value.len() as u32;
        hasher.update(len.to_le_bytes());
        hasher.update(value.as_bytes());
    }
    hasher.finalize().into()
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
    fn canonical_json_string_sorts_object_keys() {
        let value = serde_json::json!({"b": 2, "a": {"d": 4, "c": 3}});
        assert_eq!(
            canonical_json_string(&value).unwrap(),
            r#"{"a":{"c":3,"d":4},"b":2}"#
        );
    }

    #[test]
    fn hash_json_canonical_matches_alignment_config_fixture_hash() {
        let value = serde_json::json!({
            "anchor_ngram_size": 3,
            "max_tokens_per_window": 512,
            "max_chars_per_window": 8192,
            "near_match": "disabled",
            "move_detection": "exact-normalized-sequence-v1",
            "scoring": {
                "match": 2,
                "gap": -1,
                "substitution": -1
            }
        });
        assert_eq!(
            hash_json_canonical(&value).unwrap(),
            "sha256:1d05a5ac38086965f8e995448b98f160a42ff03ed2fdfe7b09dbed9124be6d2c"
        );
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
