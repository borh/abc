//! Per-row detectors. Most rows count occurrences of one or more AAT inline
//! kinds; a small minority falls back to scanning the raw source bytes.
//!
//! `DetectorKind::Default` covers the bulk of rows: count nodes whose
//! `kind` matches one of the row's `aat_nodes`, with optional refinement
//! attribute matching. `Source` matches a regex on the decoded source.
//! `Composite` ORs several detectors.

use std::collections::HashMap;

use regex::Regex;
use serde_json::Value;

use crate::matrix::Row;

#[derive(Debug)]
pub struct DetectorRegistry {
    detectors: HashMap<String, Detector>,
}

impl DetectorRegistry {
    pub fn from_matrix(rows: &[Row]) -> Self {
        let mut detectors = HashMap::new();
        for row in rows {
            let detector = build_detector_for_row(row);
            detectors.insert(row.id.clone(), detector);
        }
        Self { detectors }
    }

    pub fn detect(&self, row_id: &str, ctx: &DetectorContext<'_>) -> u64 {
        match self.detectors.get(row_id) {
            Some(d) => d.run(ctx),
            None => 0,
        }
    }

    pub fn rows(&self) -> impl Iterator<Item = &str> {
        self.detectors.keys().map(|s| s.as_str())
    }
}

pub struct DetectorContext<'a> {
    pub aat: &'a Value,
    pub source: &'a str,
}

#[derive(Debug)]
struct Detector {
    rules: Vec<Rule>,
}

#[derive(Debug)]
enum Rule {
    /// Count AAT inline / block nodes whose `kind` is in the list.
    AatKindCount(Vec<String>),
    /// Match a regex against the decoded source text.
    SourceRegex(Regex),
}

impl Detector {
    fn run(&self, ctx: &DetectorContext<'_>) -> u64 {
        let mut total = 0u64;
        for rule in &self.rules {
            total += match rule {
                Rule::AatKindCount(kinds) => count_aat_kinds(ctx.aat, kinds),
                Rule::SourceRegex(re) => re.find_iter(ctx.source).count() as u64,
            };
        }
        total
    }
}

fn build_detector_for_row(row: &Row) -> Detector {
    let mut rules = Vec::new();
    // Default: count any AAT node whose kind matches the row's `aat_nodes`.
    // Generic kinds like "text" / "paragraph" are dropped because they match
    // the structural backbone of every work and would dwarf real signal; for
    // those rows the source-side regex is the authoritative detector.
    let kinds = row
        .aat_nodes
        .iter()
        .map(|s| s.to_string())
        .filter(|s| !s.is_empty() && !is_generic_aat_kind(s))
        .collect::<Vec<_>>();
    if !kinds.is_empty() {
        rules.push(Rule::AatKindCount(kinds));
    }
    for pat in &row.source_patterns {
        if let Ok(re) = Regex::new(pat) {
            rules.push(Rule::SourceRegex(re));
        }
    }
    Detector { rules }
}

fn is_generic_aat_kind(kind: &str) -> bool {
    matches!(kind, "text" | "paragraph")
}

fn count_aat_kinds(aat: &Value, kinds: &[String]) -> u64 {
    let mut count = 0u64;
    walk(aat, &mut |node| {
        if let Some(kind) = node.get("kind").and_then(Value::as_str)
            && kinds.iter().any(|k| k == kind)
        {
            count += 1;
        }
    });
    count
}

fn walk<F>(node: &Value, visit: &mut F)
where
    F: FnMut(&Value),
{
    match node {
        Value::Object(map) => {
            visit(node);
            for (_, child) in map {
                walk(child, visit);
            }
        }
        Value::Array(arr) => {
            for child in arr {
                walk(child, visit);
            }
        }
        _ => {}
    }
}
