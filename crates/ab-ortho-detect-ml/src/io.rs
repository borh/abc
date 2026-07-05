use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct GoldRecord {
    pub sentence: String,
    /// "accept" or "reject"
    pub label: String,
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
