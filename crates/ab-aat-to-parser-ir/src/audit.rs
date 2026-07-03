use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::{Path, PathBuf},
    time::Instant,
};

use ab_aat_to_parser_ir::{
    ConversionOptions, MappingDocument, PreparedConverter, SchemaSet, schema::read_json,
};
use anyhow::{Context, Result};
use rayon::prelude::*;
use serde::Serialize;
use serde_json::Value;

#[derive(Debug, Clone)]
pub struct CorpusAuditConfig {
    pub aat_dirs: Vec<PathBuf>,
    pub mapping_path: PathBuf,
    pub summary_json: PathBuf,
    pub report_md: PathBuf,
    pub abc_root: Option<PathBuf>,
    pub repo_root: PathBuf,
    pub jobs: usize,
}

#[derive(Debug, Serialize)]
pub struct AuditSummary {
    generated_unix_seconds: u64,
    mapping: MappingSummary,
    inputs: Vec<InputSummary>,
    totals: AuditTotals,
    by_corpus: BTreeMap<String, CorpusTotals>,
    categories: BTreeMap<String, u64>,
    rule_coverage: RuleCoverage,
    top_errors: Vec<ErrorGroup>,
    failure_samples: Vec<FailureSample>,
}

#[derive(Debug, Serialize)]
struct MappingSummary {
    path: String,
    mapping_id: String,
    mapping_version: String,
    mapping_schema_hash: String,
    target_parser_ir_schema_id: String,
    target_parser_ir_schema_hash: String,
    rules_total: u64,
}

#[derive(Debug, Serialize)]
struct InputSummary {
    label: String,
    aat_dir: String,
    files: u64,
}

#[derive(Debug, Default, Serialize)]
struct AuditTotals {
    files_attempted: u64,
    files_succeeded: u64,
    files_failed: u64,
    parser_ir_nodes: u64,
    divergence_records: u64,
    divergence_occurrences: u64,
    elapsed_seconds: f64,
}

#[derive(Debug, Default, Serialize)]
struct CorpusTotals {
    files_attempted: u64,
    files_succeeded: u64,
    files_failed: u64,
    parser_ir_nodes: u64,
    divergence_records: u64,
    divergence_occurrences: u64,
}

#[derive(Debug, Serialize)]
struct RuleCoverage {
    rules_total: u64,
    rules_emitted: u64,
    rules_missing: Vec<String>,
    rules_by_id: BTreeMap<String, RuleStats>,
}

#[derive(Debug, Clone, Serialize)]
struct RuleStats {
    category: String,
    files: u64,
    occurrences: u64,
}

#[derive(Debug, Serialize)]
struct ErrorGroup {
    message: String,
    count: u64,
    samples: Vec<FileSample>,
}

#[derive(Debug, Clone, Serialize)]
struct FileSample {
    corpus: String,
    path: String,
}

#[derive(Debug, Clone, Serialize)]
struct FailureSample {
    corpus: String,
    path: String,
    message: String,
}

#[derive(Debug, Clone)]
struct AuditFile {
    corpus: String,
    root: PathBuf,
    path: PathBuf,
}

#[derive(Debug)]
struct FileSuccess {
    parser_ir_nodes: u64,
    divergence_records: u64,
    divergence_occurrences: u64,
    category_occurrences: BTreeMap<String, u64>,
    rule_occurrences: BTreeMap<String, u64>,
    emitted_rule_ids: BTreeSet<String>,
}

#[derive(Debug)]
enum FileOutcome {
    Success(FileSuccess),
    Failure { message: String },
}

#[derive(Debug)]
struct AuditFileResult {
    corpus: String,
    relative_path: String,
    outcome: FileOutcome,
}

pub fn run_audit(config: CorpusAuditConfig) -> Result<AuditSummary> {
    let started = Instant::now();
    let mapping = MappingDocument::from_path(&config.mapping_path)?;
    let abc_root = config
        .abc_root
        .clone()
        .or_else(|| std::env::var_os("AB_ABC_ROOT").map(PathBuf::from))
        .unwrap_or_else(|| config.repo_root.join("data/abc-schemas"));
    let schemas = SchemaSet::load(&config.repo_root, &abc_root)?;
    let converter = PreparedConverter::new(mapping.clone(), schemas)?;
    let inputs = collect_inputs(&config.aat_dirs)?;
    let files: Vec<AuditFile> = inputs
        .iter()
        .flat_map(|input| {
            input
                .files
                .iter()
                .map(|path| AuditFile {
                    corpus: input.label.clone(),
                    root: input.aat_dir.clone(),
                    path: path.clone(),
                })
                .collect::<Vec<_>>()
        })
        .collect();

    let results = if config.jobs == 0 {
        files
            .par_iter()
            .map(|file| audit_file(file, &converter))
            .collect::<Vec<_>>()
    } else {
        rayon::ThreadPoolBuilder::new()
            .num_threads(config.jobs)
            .build()
            .context("failed to build audit worker pool")?
            .install(|| {
                files
                    .par_iter()
                    .map(|file| audit_file(file, &converter))
                    .collect::<Vec<_>>()
            })
    };

    let mut summary = summarize(
        &config,
        mapping,
        inputs,
        results,
        started.elapsed().as_secs_f64(),
    )?;
    summary.totals.elapsed_seconds = round_seconds(summary.totals.elapsed_seconds);
    write_outputs(&config, &summary)?;
    Ok(summary)
}

impl AuditSummary {
    pub fn files_attempted(&self) -> u64 {
        self.totals.files_attempted
    }

    pub fn files_succeeded(&self) -> u64 {
        self.totals.files_succeeded
    }

    pub fn files_failed(&self) -> u64 {
        self.totals.files_failed
    }
}

fn collect_inputs(aat_dirs: &[PathBuf]) -> Result<Vec<CollectedInput>> {
    let mut labels = BTreeMap::<String, u64>::new();
    let mut inputs = Vec::new();
    for (index, dir) in aat_dirs.iter().enumerate() {
        let mut label = dir
            .file_name()
            .and_then(|name| name.to_str())
            .filter(|name| !name.is_empty())
            .map(ToOwned::to_owned)
            .unwrap_or_else(|| format!("corpus-{}", index + 1));
        let seen = labels.entry(label.clone()).or_default();
        if *seen > 0 {
            label = format!("{label}-{}", *seen + 1);
        }
        *seen += 1;

        let files = collect_json_files(dir)
            .with_context(|| format!("failed to collect AAT JSON files under {}", dir.display()))?;
        inputs.push(CollectedInput {
            label,
            aat_dir: dir.clone(),
            files,
        });
    }
    Ok(inputs)
}

#[derive(Debug)]
struct CollectedInput {
    label: String,
    aat_dir: PathBuf,
    files: Vec<PathBuf>,
}

fn collect_json_files(dir: &Path) -> Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    let mut stack = vec![dir.to_path_buf()];
    while let Some(path) = stack.pop() {
        for entry in fs::read_dir(&path)
            .with_context(|| format!("failed to read directory {}", path.display()))?
        {
            let entry =
                entry.with_context(|| format!("failed to read entry under {}", path.display()))?;
            let entry_path = entry.path();
            let file_type = entry
                .file_type()
                .with_context(|| format!("failed to stat {}", entry_path.display()))?;
            if file_type.is_dir() {
                stack.push(entry_path);
            } else if file_type.is_file()
                && entry_path.extension().and_then(|value| value.to_str()) == Some("json")
            {
                files.push(entry_path);
            }
        }
    }
    files.sort();
    Ok(files)
}

fn audit_file(file: &AuditFile, converter: &PreparedConverter) -> AuditFileResult {
    let relative_path = file
        .path
        .strip_prefix(&file.root)
        .unwrap_or(&file.path)
        .display()
        .to_string();
    let outcome = match read_json(&file.path).and_then(|aat| {
        converter.convert(
            aat,
            ConversionOptions {
                validate_input_aat: true,
                validate_output_parser_ir: true,
            },
        )
    }) {
        Ok(output) => FileOutcome::Success(summarize_output(&output)),
        Err(error) => FileOutcome::Failure {
            message: format!("{error:#}"),
        },
    };
    AuditFileResult {
        corpus: file.corpus.clone(),
        relative_path,
        outcome,
    }
}

fn summarize_output(output: &ab_aat_to_parser_ir::ConversionOutput) -> FileSuccess {
    let parser_ir_nodes = output
        .parser_ir
        .pointer("/nodes")
        .and_then(Value::as_array)
        .map_or(0, |nodes| nodes.len() as u64);
    let records = output
        .divergence_bundle
        .pointer("/records")
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_default();
    let divergence_records = records.len() as u64;
    let mut divergence_occurrences = 0;
    let mut category_occurrences = BTreeMap::new();
    let mut rule_occurrences = BTreeMap::new();
    for record in records {
        let count = record.get("count").and_then(Value::as_u64).unwrap_or(0);
        divergence_occurrences += count;
        if let Some(category) = record.get("category").and_then(Value::as_str) {
            *category_occurrences.entry(category.to_owned()).or_insert(0) += count;
        }
        if let Some(rule_id) = record.get("rule_id").and_then(Value::as_str) {
            *rule_occurrences.entry(rule_id.to_owned()).or_insert(0) += count;
        }
    }
    FileSuccess {
        parser_ir_nodes,
        divergence_records,
        divergence_occurrences,
        category_occurrences,
        rule_occurrences,
        emitted_rule_ids: output.emitted_rule_ids.clone(),
    }
}

fn summarize(
    config: &CorpusAuditConfig,
    mapping: MappingDocument,
    inputs: Vec<CollectedInput>,
    results: Vec<AuditFileResult>,
    elapsed_seconds: f64,
) -> Result<AuditSummary> {
    let mut totals = AuditTotals {
        elapsed_seconds,
        ..AuditTotals::default()
    };
    let mut by_corpus = BTreeMap::<String, CorpusTotals>::new();
    let mut categories = BTreeMap::<String, u64>::new();
    let mut rules_by_id = mapping
        .transform_rule_descriptions
        .iter()
        .map(|rule| {
            (
                rule.rule_id.clone(),
                RuleStats {
                    category: rule.category.clone(),
                    files: 0,
                    occurrences: 0,
                },
            )
        })
        .collect::<BTreeMap<_, _>>();
    let mut error_groups = BTreeMap::<String, (u64, Vec<FileSample>)>::new();
    let mut failure_samples = Vec::new();

    for result in results {
        totals.files_attempted += 1;
        let corpus_totals = by_corpus.entry(result.corpus.clone()).or_default();
        corpus_totals.files_attempted += 1;
        match result.outcome {
            FileOutcome::Success(success) => {
                totals.files_succeeded += 1;
                totals.parser_ir_nodes += success.parser_ir_nodes;
                totals.divergence_records += success.divergence_records;
                totals.divergence_occurrences += success.divergence_occurrences;
                corpus_totals.files_succeeded += 1;
                corpus_totals.parser_ir_nodes += success.parser_ir_nodes;
                corpus_totals.divergence_records += success.divergence_records;
                corpus_totals.divergence_occurrences += success.divergence_occurrences;
                for (category, count) in success.category_occurrences {
                    *categories.entry(category).or_insert(0) += count;
                }
                for rule_id in success.emitted_rule_ids {
                    if let Some(stats) = rules_by_id.get_mut(&rule_id) {
                        stats.files += 1;
                    }
                }
                for (rule_id, count) in success.rule_occurrences {
                    if let Some(stats) = rules_by_id.get_mut(&rule_id) {
                        stats.occurrences += count;
                    }
                }
            }
            FileOutcome::Failure { message } => {
                totals.files_failed += 1;
                corpus_totals.files_failed += 1;
                let sample = FileSample {
                    corpus: result.corpus.clone(),
                    path: result.relative_path.clone(),
                };
                let (count, samples) = error_groups.entry(message.clone()).or_default();
                *count += 1;
                if samples.len() < 5 {
                    samples.push(sample.clone());
                }
                if failure_samples.len() < 50 {
                    failure_samples.push(FailureSample {
                        corpus: result.corpus,
                        path: result.relative_path,
                        message,
                    });
                }
            }
        }
    }

    let mut top_errors = error_groups
        .into_iter()
        .map(|(message, (count, samples))| ErrorGroup {
            message,
            count,
            samples,
        })
        .collect::<Vec<_>>();
    top_errors.sort_by(|left, right| {
        right
            .count
            .cmp(&left.count)
            .then_with(|| left.message.cmp(&right.message))
    });
    top_errors.truncate(20);

    let rules_missing = rules_by_id
        .iter()
        .filter_map(|(rule_id, stats)| (stats.files == 0).then_some(rule_id.clone()))
        .collect::<Vec<_>>();
    let rules_emitted = rules_by_id.len() as u64 - rules_missing.len() as u64;

    Ok(AuditSummary {
        generated_unix_seconds: std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .context("system clock is before Unix epoch")?
            .as_secs(),
        mapping: MappingSummary {
            path: config.mapping_path.display().to_string(),
            mapping_id: mapping.mapping_id,
            mapping_version: mapping.mapping_version,
            mapping_schema_hash: mapping.mapping_schema_hash,
            target_parser_ir_schema_id: mapping.target_parser_ir_schema_id,
            target_parser_ir_schema_hash: mapping.target_parser_ir_schema_hash,
            rules_total: mapping.transform_rule_descriptions.len() as u64,
        },
        inputs: inputs
            .into_iter()
            .map(|input| InputSummary {
                label: input.label,
                aat_dir: input.aat_dir.display().to_string(),
                files: input.files.len() as u64,
            })
            .collect(),
        totals,
        by_corpus,
        categories,
        rule_coverage: RuleCoverage {
            rules_total: rules_by_id.len() as u64,
            rules_emitted,
            rules_missing,
            rules_by_id,
        },
        top_errors,
        failure_samples,
    })
}

fn write_outputs(config: &CorpusAuditConfig, summary: &AuditSummary) -> Result<()> {
    if let Some(parent) = config.summary_json.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    if let Some(parent) = config.report_md.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    fs::write(
        &config.summary_json,
        serde_json::to_string_pretty(summary)? + "\n",
    )
    .with_context(|| format!("failed to write {}", config.summary_json.display()))?;
    fs::write(&config.report_md, render_report(summary))
        .with_context(|| format!("failed to write {}", config.report_md.display()))?;
    Ok(())
}

fn render_report(summary: &AuditSummary) -> String {
    let mut out = String::new();
    out.push_str("# Full-Corpus AAT Parser-IR Conversion Audit\n\n");
    out.push_str(&format!(
        "- generated_unix_seconds: `{}`\n",
        summary.generated_unix_seconds
    ));
    out.push_str(&format!(
        "- mapping: `{}` `{}`\n",
        summary.mapping.mapping_id, summary.mapping.mapping_version
    ));
    out.push_str(&format!(
        "- mapping_schema_hash: `{}`\n",
        summary.mapping.mapping_schema_hash
    ));
    out.push_str(&format!(
        "- target_parser_ir_schema_hash: `{}`\n\n",
        summary.mapping.target_parser_ir_schema_hash
    ));

    out.push_str("## Totals\n\n");
    out.push_str("| files_attempted | files_succeeded | files_failed | parser_ir_nodes | divergence_records | divergence_occurrences | elapsed_seconds |\n");
    out.push_str("|---:|---:|---:|---:|---:|---:|---:|\n");
    out.push_str(&format!(
        "| {} | {} | {} | {} | {} | {} | {:.3} |\n\n",
        summary.totals.files_attempted,
        summary.totals.files_succeeded,
        summary.totals.files_failed,
        summary.totals.parser_ir_nodes,
        summary.totals.divergence_records,
        summary.totals.divergence_occurrences,
        summary.totals.elapsed_seconds,
    ));

    out.push_str("## Inputs\n\n");
    out.push_str("| label | files | aat_dir |\n|---|---:|---|\n");
    for input in &summary.inputs {
        out.push_str(&format!(
            "| {} | {} | `{}` |\n",
            table_cell(&input.label),
            input.files,
            input.aat_dir
        ));
    }
    out.push('\n');

    out.push_str("## Corpus Results\n\n");
    out.push_str(
        "| corpus | attempted | succeeded | failed | parser_ir_nodes | divergence_occurrences |\n",
    );
    out.push_str("|---|---:|---:|---:|---:|---:|\n");
    for (corpus, totals) in &summary.by_corpus {
        out.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} |\n",
            table_cell(corpus),
            totals.files_attempted,
            totals.files_succeeded,
            totals.files_failed,
            totals.parser_ir_nodes,
            totals.divergence_occurrences,
        ));
    }
    out.push('\n');

    out.push_str("## Divergence Categories\n\n");
    out.push_str("| category | occurrences |\n|---|---:|\n");
    for (category, count) in &summary.categories {
        out.push_str(&format!("| {} | {} |\n", table_cell(category), count));
    }
    out.push('\n');

    out.push_str("## Rule Coverage\n\n");
    out.push_str(&format!(
        "- rules_total: `{}`\n- rules_emitted: `{}`\n- rules_missing: `{}`\n\n",
        summary.rule_coverage.rules_total,
        summary.rule_coverage.rules_emitted,
        summary.rule_coverage.rules_missing.len()
    ));
    out.push_str("| rule_id | category | files | occurrences |\n|---|---|---:|---:|\n");
    for (rule_id, stats) in &summary.rule_coverage.rules_by_id {
        out.push_str(&format!(
            "| {} | {} | {} | {} |\n",
            table_cell(rule_id),
            table_cell(&stats.category),
            stats.files,
            stats.occurrences
        ));
    }
    out.push('\n');

    out.push_str("## Top Errors\n\n");
    if summary.top_errors.is_empty() {
        out.push_str("No conversion failures were observed.\n\n");
    } else {
        out.push_str("| count | message | samples |\n|---:|---|---|\n");
        for error in &summary.top_errors {
            let samples = error
                .samples
                .iter()
                .map(|sample| format!("{}:{}", sample.corpus, sample.path))
                .collect::<Vec<_>>()
                .join("<br>");
            out.push_str(&format!(
                "| {} | {} | {} |\n",
                error.count,
                table_cell(&error.message),
                table_cell(&samples)
            ));
        }
        out.push('\n');
    }

    out.push_str("## Failure Samples\n\n");
    if summary.failure_samples.is_empty() {
        out.push_str("No conversion failure samples.\n");
    } else {
        out.push_str("| corpus | path | message |\n|---|---|---|\n");
        for sample in &summary.failure_samples {
            out.push_str(&format!(
                "| {} | `{}` | {} |\n",
                table_cell(&sample.corpus),
                sample.path,
                table_cell(&sample.message)
            ));
        }
    }
    out
}

fn table_cell(value: &str) -> String {
    value
        .replace('|', "\\|")
        .replace('\n', "<br>")
        .replace('\r', "")
}

fn round_seconds(seconds: f64) -> f64 {
    (seconds * 1000.0).round() / 1000.0
}
