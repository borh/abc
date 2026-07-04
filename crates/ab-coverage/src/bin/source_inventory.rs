use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::{Path, PathBuf},
};

use ab_coverage::{
    matrix::{CoverageMatrix, RepresentabilityStatus, Row},
    source_corpus::{load_index, read_source_work},
    source_inventory::{UnknownMarkerExample, inventory_document, patterns_from_rows},
};
use anyhow::{Context, Result, bail};
use clap::Parser;
use rayon::prelude::*;
use regex::Regex;
use serde::{Deserialize, Serialize};

#[derive(Parser, Debug)]
#[command(version, about = "Source-authority Aozora marker inventory.")]
struct Cli {
    #[arg(long, default_value = "data/aozora-syntax-coverage.toml")]
    matrix: PathBuf,
    #[arg(long)]
    index: PathBuf,
    #[arg(long, default_value = "references/aozorabunko")]
    corpus: PathBuf,
    #[arg(long)]
    work_ids: Option<PathBuf>,
    #[arg(long)]
    allowlist: Option<PathBuf>,
    #[arg(long)]
    output_json: PathBuf,
    #[arg(long)]
    report_md: PathBuf,
    #[arg(long)]
    unknown_workset: Option<PathBuf>,
    #[arg(long)]
    fail_on_unknown: bool,
    #[arg(long)]
    strict_representability: bool,
    #[arg(long)]
    jobs: Option<usize>,
}

#[derive(Debug, Default, Serialize)]
struct InventoryOutput {
    works_scanned: u64,
    works_failed: u64,
    markers_total: u64,
    unknown_markers_total: u64,
    unallowlisted_unknown_markers_total: u64,
    allowlisted_unknown_markers_total: u64,
    rows: BTreeMap<String, RowOutput>,
    unknown_examples: Vec<UnknownMarkerExample>,
    decode_failures: Vec<DecodeFailure>,
    representability: RepresentabilityOutput,
    inputs: Inputs,
}

#[derive(Debug, Default, Serialize)]
struct RowOutput {
    works_with_marker: u64,
    occurrences: u64,
    sample_works: Vec<String>,
}

#[derive(Debug, Serialize)]
struct DecodeFailure {
    work_id: String,
    indexed_path: String,
    error: String,
}

#[derive(Debug, Default, Serialize)]
struct Inputs {
    matrix: String,
    index: String,
    corpus: String,
    allowlist: Option<String>,
    work_ids: Option<String>,
}

#[derive(Debug, Default, Serialize)]
struct RepresentabilityOutput {
    typed_occurrences: u64,
    raw_preserved_occurrences: u64,
    out_of_body_occurrences: u64,
    unsupported_occurrences: u64,
    needs_research_occurrences: u64,
}

#[derive(Debug, Deserialize)]
struct AllowlistFile {
    #[serde(default)]
    allow: Vec<AllowRule>,
}

#[derive(Debug, Deserialize)]
struct AllowRule {
    id: String,
    kind: String,
    body_pattern: Option<String>,
    raw_pattern: Option<String>,
    scope: String,
    reason: String,
    evidence: String,
}

struct CompiledAllowRule {
    rule: AllowRule,
    body_pattern: Option<Regex>,
    raw_pattern: Option<Regex>,
}

enum WorkInventoryResult {
    Scanned {
        work_id: String,
        summary: ab_coverage::source_inventory::SourceInventorySummary,
    },
    Failed {
        work_id: String,
        indexed_path: String,
        error: String,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    if let Some(jobs) = cli.jobs {
        rayon::ThreadPoolBuilder::new()
            .num_threads(jobs)
            .build_global()
            .context("rayon thread pool")?;
    }
    let matrix = CoverageMatrix::from_toml(&cli.matrix)
        .with_context(|| format!("load matrix {}", cli.matrix.display()))?;
    let patterns = patterns_from_rows(matrix.rows());
    let rows_by_id = matrix
        .rows()
        .iter()
        .map(|row| (row.id.as_str(), row))
        .collect::<BTreeMap<_, _>>();
    let index = load_index(&cli.index)?;
    let allowlist = load_allowlist(cli.allowlist.as_deref())?;
    let work_ids = load_work_ids(cli.work_ids.as_deref(), &index)?;

    let mut output = InventoryOutput {
        inputs: Inputs {
            matrix: cli.matrix.display().to_string(),
            index: cli.index.display().to_string(),
            corpus: cli.corpus.display().to_string(),
            allowlist: cli
                .allowlist
                .as_ref()
                .map(|path| path.display().to_string()),
            work_ids: cli.work_ids.as_ref().map(|path| path.display().to_string()),
        },
        ..InventoryOutput::default()
    };
    let mut unknown_work_ids = BTreeSet::new();
    let mut strict_errors = Vec::new();

    let work_results = work_ids
        .par_iter()
        .map(|work_id| scan_work(work_id, &cli.corpus, &index, &patterns))
        .collect::<Vec<_>>();

    for result in work_results {
        match result {
            WorkInventoryResult::Scanned { work_id, summary } => {
                output.works_scanned += 1;
                output.markers_total += summary.markers_total;
                for (row_id, count) in summary.row_counts {
                    let row = output.rows.entry(row_id).or_default();
                    row.occurrences += count.occurrences;
                    row.works_with_marker += 1;
                    if row.sample_works.len() < 5 && !row.sample_works.contains(&work_id) {
                        row.sample_works.push(work_id.clone());
                    }
                }

                for example in summary.unknown_examples {
                    output.unknown_markers_total += 1;
                    unknown_work_ids.insert(work_id.clone());
                    if let Some(rule) = matching_allowlist_rule(&example, &allowlist) {
                        output.allowlisted_unknown_markers_total += 1;
                        observe_allowlisted_representability(
                            &mut output.representability,
                            &rule.rule,
                        );
                    } else {
                        output.unallowlisted_unknown_markers_total += 1;
                        if output.unknown_examples.len() < 100 {
                            output.unknown_examples.push(example);
                        }
                    }
                }
            }
            WorkInventoryResult::Failed {
                work_id,
                indexed_path,
                error,
            } => {
                output.works_failed += 1;
                output.decode_failures.push(DecodeFailure {
                    work_id,
                    indexed_path,
                    error,
                });
            }
        }
    }

    observe_row_representability(&mut output, &rows_by_id, &mut strict_errors);
    if output.unallowlisted_unknown_markers_total > 0 {
        strict_errors.push(format!(
            "{} unallowlisted source markers",
            output.unallowlisted_unknown_markers_total
        ));
    }

    write_json(&cli.output_json, &output)?;
    write_report(&cli.report_md, &output)?;
    if let Some(path) = &cli.unknown_workset {
        let ids = unknown_work_ids.into_iter().collect::<Vec<_>>();
        write_json(path, &ids)?;
    }

    if cli.fail_on_unknown && output.unallowlisted_unknown_markers_total > 0 {
        bail!(
            "{} unallowlisted source markers",
            output.unallowlisted_unknown_markers_total
        );
    }
    if cli.strict_representability && !strict_errors.is_empty() {
        bail!(
            "strict representability failed:\n{}",
            strict_errors.join("\n")
        );
    }

    Ok(())
}

fn scan_work(
    work_id: &str,
    corpus: &Path,
    index: &BTreeMap<String, String>,
    patterns: &[ab_coverage::source_inventory::SourceInventoryPattern],
) -> WorkInventoryResult {
    let Some(indexed_path) = index.get(work_id) else {
        return WorkInventoryResult::Failed {
            work_id: work_id.to_owned(),
            indexed_path: String::new(),
            error: "work id missing from index".to_owned(),
        };
    };
    match read_source_work(corpus, work_id, indexed_path) {
        Ok(work) => WorkInventoryResult::Scanned {
            work_id: work_id.to_owned(),
            summary: inventory_document(work_id, &work.decoded.text, patterns),
        },
        Err(err) => WorkInventoryResult::Failed {
            work_id: work_id.to_owned(),
            indexed_path: indexed_path.clone(),
            error: format!("{err:#}"),
        },
    }
}

fn load_work_ids(path: Option<&Path>, index: &BTreeMap<String, String>) -> Result<Vec<String>> {
    if let Some(path) = path {
        let bytes = fs::read(path).with_context(|| format!("read {}", path.display()))?;
        let ids: Vec<String> = serde_json::from_slice(&bytes)
            .with_context(|| format!("parse work id array {}", path.display()))?;
        return Ok(ids);
    }
    Ok(index.keys().cloned().collect())
}

fn load_allowlist(path: Option<&Path>) -> Result<Vec<CompiledAllowRule>> {
    let Some(path) = path else {
        return Ok(Vec::new());
    };
    let raw = fs::read_to_string(path).with_context(|| format!("read {}", path.display()))?;
    let parsed: AllowlistFile =
        toml::from_str(&raw).with_context(|| format!("parse {}", path.display()))?;
    parsed
        .allow
        .into_iter()
        .map(|rule| {
            if rule.body_pattern.is_none() && rule.raw_pattern.is_none() {
                bail!(
                    "allowlist rule {} needs body_pattern or raw_pattern",
                    rule.id
                );
            }
            if !matches!(
                rule.scope.as_str(),
                "out_of_body" | "malformed_noise" | "unsupported_v1"
            ) {
                bail!(
                    "allowlist rule {} has invalid scope {}",
                    rule.id,
                    rule.scope
                );
            }
            if rule.reason.trim().is_empty() || rule.evidence.trim().is_empty() {
                bail!("allowlist rule {} needs reason and evidence", rule.id);
            }
            let body_pattern =
                compile_optional_regex(&rule.id, "body_pattern", &rule.body_pattern)?;
            let raw_pattern = compile_optional_regex(&rule.id, "raw_pattern", &rule.raw_pattern)?;
            Ok(CompiledAllowRule {
                rule,
                body_pattern,
                raw_pattern,
            })
        })
        .collect()
}

fn compile_optional_regex(
    id: &str,
    field: &str,
    pattern: &Option<String>,
) -> Result<Option<Regex>> {
    pattern
        .as_ref()
        .map(|pattern| Regex::new(pattern).with_context(|| format!("allowlist {id} {field}")))
        .transpose()
}

fn matching_allowlist_rule<'a>(
    example: &UnknownMarkerExample,
    allowlist: &'a [CompiledAllowRule],
) -> Option<&'a CompiledAllowRule> {
    allowlist.iter().find(|entry| {
        entry.rule.kind == example.kind
            && (entry
                .body_pattern
                .as_ref()
                .is_some_and(|pattern| pattern.is_match(&example.body))
                || entry
                    .raw_pattern
                    .as_ref()
                    .is_some_and(|pattern| pattern.is_match(&example.raw)))
    })
}

fn observe_allowlisted_representability(
    representability: &mut RepresentabilityOutput,
    rule: &AllowRule,
) {
    match rule.scope.as_str() {
        "out_of_body" => representability.out_of_body_occurrences += 1,
        "unsupported_v1" => representability.raw_preserved_occurrences += 1,
        "malformed_noise" => representability.unsupported_occurrences += 1,
        _ => {}
    }
}

fn observe_row_representability(
    output: &mut InventoryOutput,
    rows_by_id: &BTreeMap<&str, &Row>,
    strict_errors: &mut Vec<String>,
) {
    for (row_id, row_output) in &output.rows {
        if row_output.occurrences == 0 {
            continue;
        }
        let Some(row) = rows_by_id.get(row_id.as_str()) else {
            strict_errors.push(format!(
                "source inventory row {row_id} is not present in matrix"
            ));
            continue;
        };
        let Some(cell) = &row.representability else {
            strict_errors.push(format!(
                "source inventory row {row_id} has occurrences but no representability table"
            ));
            continue;
        };
        match cell.status {
            RepresentabilityStatus::Typed => {
                if cell.aat_nodes.is_empty() {
                    strict_errors.push(format!(
                        "source inventory row {row_id} is typed but has no representability.aat_nodes"
                    ));
                }
                output.representability.typed_occurrences += row_output.occurrences;
            }
            RepresentabilityStatus::RawPreserved => {
                output.representability.raw_preserved_occurrences += row_output.occurrences;
            }
            RepresentabilityStatus::OutOfBody => {
                output.representability.out_of_body_occurrences += row_output.occurrences;
            }
            RepresentabilityStatus::Unsupported => {
                output.representability.unsupported_occurrences += row_output.occurrences;
            }
            RepresentabilityStatus::NeedsResearch => {
                output.representability.needs_research_occurrences += row_output.occurrences;
                strict_errors.push(format!(
                    "source inventory row {row_id} has representability.status = needs_research"
                ));
            }
        }
    }
}

fn write_json<T: Serialize>(path: &Path, value: &T) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).with_context(|| format!("create {}", parent.display()))?;
    }
    fs::write(path, serde_json::to_vec_pretty(value)?)
        .with_context(|| format!("write {}", path.display()))
}

fn write_report(path: &Path, output: &InventoryOutput) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).with_context(|| format!("create {}", parent.display()))?;
    }
    let mut report = String::new();
    report.push_str("# Source Authority Representability Inventory\n\n");
    report.push_str("## Summary\n\n");
    report.push_str(&format!("- works_scanned: {}\n", output.works_scanned));
    report.push_str(&format!("- works_failed: {}\n", output.works_failed));
    report.push_str(&format!("- markers_total: {}\n", output.markers_total));
    report.push_str(&format!(
        "- unknown_markers_total: {}\n",
        output.unknown_markers_total
    ));
    report.push_str(&format!(
        "- unallowlisted_unknown_markers_total: {}\n",
        output.unallowlisted_unknown_markers_total
    ));
    report.push_str(&format!(
        "- allowlisted_unknown_markers_total: {}\n",
        output.allowlisted_unknown_markers_total
    ));

    report.push_str("\n## Representability\n\n");
    report.push_str(&format!(
        "- typed_occurrences: {}\n",
        output.representability.typed_occurrences
    ));
    report.push_str(&format!(
        "- raw_preserved_occurrences: {}\n",
        output.representability.raw_preserved_occurrences
    ));
    report.push_str(&format!(
        "- out_of_body_occurrences: {}\n",
        output.representability.out_of_body_occurrences
    ));
    report.push_str(&format!(
        "- unsupported_occurrences: {}\n",
        output.representability.unsupported_occurrences
    ));
    report.push_str(&format!(
        "- needs_research_occurrences: {}\n",
        output.representability.needs_research_occurrences
    ));

    report.push_str("\n## Rows\n\n");
    report.push_str("| row | works | occurrences | samples |\n");
    report.push_str("|---|---:|---:|---|\n");
    for (row_id, row) in &output.rows {
        report.push_str(&format!(
            "| {} | {} | {} | {} |\n",
            row_id,
            row.works_with_marker,
            row.occurrences,
            row.sample_works.join(", ")
        ));
    }

    report.push_str("\n## Unknown Source Markers\n\n");
    if output.unknown_examples.is_empty() {
        report.push_str("None.\n");
    } else {
        report.push_str("| work_id | line | kind | raw | body |\n");
        report.push_str("|---|---:|---|---|---|\n");
        for example in &output.unknown_examples {
            report.push_str(&format!(
                "| {} | {} | {} | {} | {} |\n",
                example.work_id,
                example.line,
                example.kind,
                escape_md(&example.raw),
                escape_md(&example.body)
            ));
        }
    }

    report.push_str("\n## Decode Failures\n\n");
    if output.decode_failures.is_empty() {
        report.push_str("None.\n");
    } else {
        for failure in &output.decode_failures {
            report.push_str(&format!(
                "- {} `{}`: {}\n",
                failure.work_id, failure.indexed_path, failure.error
            ));
        }
    }

    report.push_str("\n## Inputs\n\n");
    report.push_str(&format!("- matrix: `{}`\n", output.inputs.matrix));
    report.push_str(&format!("- index: `{}`\n", output.inputs.index));
    report.push_str(&format!("- corpus: `{}`\n", output.inputs.corpus));
    if let Some(path) = &output.inputs.allowlist {
        report.push_str(&format!("- allowlist: `{path}`\n"));
    }
    if let Some(path) = &output.inputs.work_ids {
        report.push_str(&format!("- work_ids: `{path}`\n"));
    }

    fs::write(path, report).with_context(|| format!("write {}", path.display()))
}

fn escape_md(value: &str) -> String {
    value.replace('|', "\\|").replace('\n', "\\n")
}
