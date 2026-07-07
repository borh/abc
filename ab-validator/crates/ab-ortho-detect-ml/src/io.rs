use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct GoldRecord {
    pub sentence: String,
    /// "accept", "normalize", or "reject".
    /// Both "accept" and "normalize" are positive class (should trigger
    /// normalization); "reject" is negative class.
    pub label: String,
}

impl GoldRecord {
    /// Returns `true` when the label is positive class for ML training
    /// and recall measurement: `"accept"` (historical orthography) or
    /// `"normalize"` (emphatic/stylistic katakana where conversion
    /// improves tokenization).
    #[must_use]
    pub fn is_positive(&self) -> bool {
        self.label == "accept" || self.label == "normalize"
    }
}

/// Read gold JSONL (one `{sentence, label, ...}` per line).
/// # Errors
/// Returns an error on IO failure or malformed JSON.
pub fn read_gold(path: &std::path::Path) -> std::io::Result<Vec<GoldRecord>> {
    let mut out = Vec::new();
    for line in std::io::BufRead::lines(std::io::BufReader::new(std::fs::File::open(path)?)) {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }
        out.push(
            serde_json::from_str(&line)
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?,
        );
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_positive_treats_normalize_as_accept_class() {
        assert!(
            GoldRecord {
                sentence: String::new(),
                label: "accept".into(),
            }
            .is_positive()
        );
        assert!(
            GoldRecord {
                sentence: String::new(),
                label: "normalize".into(),
            }
            .is_positive()
        );
        assert!(
            !GoldRecord {
                sentence: String::new(),
                label: "reject".into(),
            }
            .is_positive()
        );
    }
}
