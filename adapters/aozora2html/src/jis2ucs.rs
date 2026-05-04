use std::collections::HashMap;

const JIS2UCS_YML: &str = include_str!("../data/jis2ucs.yml");

fn load_map() -> HashMap<String, String> {
    let mut table = HashMap::new();
    for line in JIS2UCS_YML.lines() {
        if let Some(rest) = line.strip_prefix(':') {
            let mut parts = rest.splitn(2, ": ");
            let jis = parts.next().unwrap_or_default();
            let rhs = parts.next().unwrap_or_default().trim().trim_matches('"');
            if let Some(hex) = rhs.strip_prefix("&#x").and_then(|it| it.strip_suffix(';')) {
                if let Ok(codepoint) = u32::from_str_radix(hex, 16) {
                    if let Some(ch) = std::char::from_u32(codepoint) {
                        table.insert(normalize_jis_code(jis), ch.to_string());
                    }
                }
            }
        }
    }
    table
}

pub fn resolve_jis2ucs(jis_code: &str) -> Option<String> {
    let table = load_map();
    table.get(&normalize_jis_code(jis_code)).cloned()
}

pub fn normalize_jis_code(jis_code: &str) -> String {
    let parts = jis_code.split('-').collect::<Vec<_>>();
    if parts.len() != 3 {
        return jis_code.to_string();
    }
    let normalized = parts
        .iter()
        .map(|part| part.parse::<u32>().ok().map(|n| n.to_string()).unwrap_or_else(|| part.to_string()))
        .collect::<Vec<_>>();
    normalized.join("-")
}
