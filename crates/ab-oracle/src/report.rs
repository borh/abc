use std::{fs, path::Path};

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct OracleReport {
    pub rows: Vec<ReportRow>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ReportRow {
    pub case_id: String,
    pub adapter: String,
    pub schema_status: String,
    pub upstream_status: String,
    pub oracle_status: String,
    pub failures: Vec<String>,
}

pub fn write_json_report(report: &OracleReport, path: &Path) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    let file =
        fs::File::create(path).with_context(|| format!("failed to create {}", path.display()))?;
    serde_json::to_writer_pretty(file, report)
        .with_context(|| format!("failed to write {}", path.display()))
}

pub fn read_json_report(path: &Path) -> Result<OracleReport> {
    let input =
        fs::read(path).with_context(|| format!("failed to read report {}", path.display()))?;
    serde_json::from_slice(&input)
        .with_context(|| format!("failed to parse report {}", path.display()))
}

pub fn render_markdown(report: &OracleReport) -> String {
    let mut out = String::from(
        "| case_id | adapter | schema_status | upstream_status | oracle_status | failures |\n\
         | --- | --- | --- | --- | --- | --- |\n",
    );
    for row in &report.rows {
        out.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} |\n",
            row.case_id,
            row.adapter,
            row.schema_status,
            row.upstream_status,
            row.oracle_status,
            row.failures.join("<br>")
        ));
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn markdown_names_all_result_axes() {
        let markdown = render_markdown(&OracleReport {
            rows: vec![ReportRow {
                case_id: "case".to_owned(),
                adapter: "adapter".to_owned(),
                schema_status: "pass".to_owned(),
                upstream_status: "faithful".to_owned(),
                oracle_status: "pass".to_owned(),
                failures: Vec::new(),
            }],
        });

        assert!(markdown.contains("schema_status"));
        assert!(markdown.contains("upstream_status"));
        assert!(markdown.contains("oracle_status"));
    }
}
