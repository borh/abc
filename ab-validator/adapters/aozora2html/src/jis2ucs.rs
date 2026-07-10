#[cfg(test)]
use std::sync::atomic::{AtomicUsize, Ordering};
use std::{collections::HashMap, sync::OnceLock};

const JIS2UCS_YML: &str = include_str!("../data/jis2ucs.yml");
static JIS2UCS_MAP: OnceLock<HashMap<String, String>> = OnceLock::new();

#[cfg(test)]
static LOAD_COUNT: AtomicUsize = AtomicUsize::new(0);

fn load_map() -> HashMap<String, String> {
    #[cfg(test)]
    LOAD_COUNT.fetch_add(1, Ordering::SeqCst);

    let mut table = HashMap::new();
    for line in JIS2UCS_YML.lines() {
        if let Some(rest) = line.strip_prefix(':') {
            let mut parts = rest.splitn(2, ": ");
            let jis = parts.next().unwrap_or_default();
            let rhs = parts.next().unwrap_or_default().trim().trim_matches('"');
            if let Some(hex) = rhs.strip_prefix("&#x").and_then(|it| it.strip_suffix(';'))
                && let Ok(codepoint) = u32::from_str_radix(hex, 16)
                && let Some(ch) = std::char::from_u32(codepoint)
            {
                table.insert(normalize_jis_code(jis), ch.to_string());
            }
        }
    }
    table
}

pub fn resolve_jis2ucs(jis_code: &str) -> Option<String> {
    JIS2UCS_MAP
        .get_or_init(load_map)
        .get(&normalize_jis_code(jis_code))
        .cloned()
}

pub fn normalize_jis_code(jis_code: &str) -> String {
    let parts = jis_code.split('-').collect::<Vec<_>>();
    if parts.len() != 3 {
        return jis_code.to_string();
    }
    let normalized = parts
        .iter()
        .map(|part| {
            part.parse::<u32>()
                .ok()
                .map(|n| n.to_string())
                .unwrap_or_else(|| part.to_string())
        })
        .collect::<Vec<_>>();
    normalized.join("-")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn resolve_jis2ucs_reuses_loaded_table() {
        let before = LOAD_COUNT.load(Ordering::SeqCst);

        assert_eq!(resolve_jis2ucs("1-1-18").as_deref(), Some("＿"));
        assert_eq!(resolve_jis2ucs("01-01-18").as_deref(), Some("＿"));

        let loads = LOAD_COUNT.load(Ordering::SeqCst) - before;
        assert!(
            loads <= 1,
            "expected at most one table load for repeated lookups, got {loads}"
        );
    }
}
