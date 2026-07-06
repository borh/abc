use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::Path,
};

use anyhow::{Context, Result};
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use walkdir::WalkDir;

pub mod aat_diff;
pub mod metrics;
pub mod triage;

#[derive(Debug, Deserialize)]
pub struct CheckReport {
    pub adapter: String,
    pub adapter_version: String,
    pub work_id: String,
    pub results: BTreeMap<String, CheckResult>,
}

#[derive(Debug, Deserialize)]
pub struct CheckResult {
    pub pass: bool,
    pub message: Option<String>,
    pub confidence: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct CompareSummary {
    pub adapter_a: String,
    pub adapter_b: String,
    pub common_reports: usize,
    pub only_a: usize,
    pub only_b: usize,
    pub result_differences: Vec<ResultDifference>,
}

#[derive(Debug, Serialize)]
pub struct ResultDifference {
    pub work_id: String,
    pub property: String,
    pub a_pass: bool,
    pub b_pass: bool,
}

/// Compare check results from two directories and return a summary.
///
/// # Errors
///
/// Returns an error when report loading or JSON parsing fails, or when directory
/// traversal fails.
pub fn compare_report_dirs(a: &Path, b: &Path) -> Result<CompareSummary> {
    let reports_a = read_reports(a)?;
    let reports_b = read_reports(b)?;
    let keys_a = reports_a.keys().cloned().collect::<BTreeSet<_>>();
    let keys_b = reports_b.keys().cloned().collect::<BTreeSet<_>>();
    let common = keys_a.intersection(&keys_b).cloned().collect::<Vec<_>>();

    let mut result_differences = Vec::new();
    for key in &common {
        let a_report = &reports_a[key];
        let b_report = &reports_b[key];
        let properties = a_report
            .results
            .keys()
            .chain(b_report.results.keys())
            .cloned()
            .collect::<BTreeSet<_>>();
        for property in properties {
            let a_pass = a_report.results.get(&property).is_some_and(|r| r.pass);
            let b_pass = b_report.results.get(&property).is_some_and(|r| r.pass);
            if a_pass != b_pass {
                result_differences.push(ResultDifference {
                    work_id: a_report.work_id.clone(),
                    property,
                    a_pass,
                    b_pass,
                });
            }
        }
    }

    Ok(CompareSummary {
        adapter_a: reports_a
            .values()
            .next()
            .map(|report| report.adapter.clone())
            .unwrap_or_default(),
        adapter_b: reports_b
            .values()
            .next()
            .map(|report| report.adapter.clone())
            .unwrap_or_default(),
        common_reports: common.len(),
        only_a: keys_a.difference(&keys_b).count(),
        only_b: keys_b.difference(&keys_a).count(),
        result_differences,
    })
}

fn read_reports(root: &Path) -> Result<BTreeMap<String, CheckReport>> {
    let entries: Vec<_> = WalkDir::new(root)
        .into_iter()
        .filter_map(|entry| entry.ok())
        .filter(|entry| {
            entry.file_type().is_file()
                && entry
                    .path()
                    .extension()
                    .is_some_and(|extension| extension == "json")
        })
        .map(|entry| entry.path().to_owned())
        .collect();

    let loaded: Vec<(std::path::PathBuf, CheckReport)> = entries
        .par_iter()
        .map(|path| {
            let bytes =
                fs::read(path).with_context(|| format!("failed to read {}", path.display()))?;
            let report: CheckReport = serde_json::from_slice(&bytes)
                .with_context(|| format!("failed to parse {}", path.display()))?;
            Ok::<_, anyhow::Error>((path.to_owned(), report))
        })
        .collect::<Result<_>>()?;

    let mut work_id_counts = BTreeMap::new();
    for (_, report) in &loaded {
        *work_id_counts
            .entry(report.work_id.clone())
            .or_insert(0usize) += 1;
    }

    let mut reports = BTreeMap::new();
    for (path, report) in loaded {
        let key = report_key(&work_id_counts, &path, &report);
        reports.insert(key, report);
    }
    Ok(reports)
}

fn report_key(
    work_id_counts: &BTreeMap<String, usize>,
    path: &Path,
    report: &CheckReport,
) -> String {
    if work_id_counts
        .get(&report.work_id)
        .copied()
        .unwrap_or_default()
        <= 1
    {
        return report.work_id.clone();
    }
    let filename = path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("duplicate");
    format!("{}::{filename}", report.work_id)
}
