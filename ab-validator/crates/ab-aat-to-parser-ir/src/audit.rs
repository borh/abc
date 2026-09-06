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
    pub compat_edn_out: Option<PathBuf>,
    pub research_root: Option<PathBuf>,
    pub repo_root: PathBuf,
    pub jobs: usize,
    pub expect_mapping_version: Option<String>,
    pub expect_mapping_hash: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct AuditSummary {
    generated_unix_seconds: u64,
    mapping: MappingSummary,
    inputs: Vec<InputSummary>,
    totals: AuditTotals,
    by_corpus: BTreeMap<String, CorpusTotals>,
    raw_nodes: RawNodeSummary,
    sentence_projection_failures: SentenceProjectionFailureSummary,
    categories: BTreeMap<String, u64>,
    rule_coverage: RuleCoverage,
    compatibility_candidates: Vec<CompatibilityCandidate>,
    top_errors: Vec<ErrorGroup>,
    failure_samples: Vec<FailureSample>,
}

#[derive(Debug, Serialize)]
struct MappingSummary {
    path: String,
    mapping_id: String,
    mapping_version: String,
    mapping_hash: String,
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
struct RawNodeSummary {
    nodes_total: u64,
    files_with_raw: u64,
    fatal_direct_failures: u64,
    by_corpus: BTreeMap<String, RawCorpusStats>,
    inferred_provenance: BTreeMap<String, u64>,
    source_marker_kinds: BTreeMap<String, u64>,
    source_classes: BTreeMap<String, u64>,
    samples: Vec<RawNodeSample>,
}

#[derive(Debug, Default, Serialize)]
struct RawCorpusStats {
    nodes_total: u64,
    files_with_raw: u64,
    fatal_direct_failures: u64,
}

#[derive(Debug, Clone, Serialize)]
struct RawNodeSample {
    corpus: String,
    path: String,
    pointer: String,
    inferred_provenance: String,
    source_class: String,
    source_marker_kind: Option<String>,
    source_preview: String,
}

#[derive(Debug, Default, Serialize)]
struct SentenceProjectionFailureSummary {
    files_failed: u64,
    atomic_boundary_failures_by_node_type: BTreeMap<String, u64>,
    other_sentence_projection_failures: u64,
    samples: Vec<SentenceProjectionFailureSample>,
}

#[derive(Debug, Clone, Serialize)]
struct SentenceProjectionFailureSample {
    corpus: String,
    path: String,
    class: String,
    node_type: Option<String>,
    byte_offset: Option<u64>,
    message: String,
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

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Serialize)]
struct CompatibilityIdentity {
    aat_version: u64,
    aat_adapter: String,
    aat_adapter_version: Option<String>,
    mapping_id: String,
    mapping_version: String,
    mapping_hash: String,
    mapping_schema_hash: String,
    parser_ir_schema_id: String,
    parser_ir_schema_hash: String,
}

#[derive(Debug, Serialize)]
struct CompatibilityCandidate {
    aat_version: u64,
    aat_adapter: String,
    aat_adapter_version: Option<String>,
    mapping_id: String,
    mapping_version: String,
    mapping_hash: String,
    mapping_schema_hash: String,
    parser_ir_schema_id: String,
    parser_ir_schema_hash: String,
    evidence_scope: CompatibilityEvidenceScope,
    compatibility: String,
}

#[derive(Debug, Default, Serialize)]
struct CompatibilityEvidenceScope {
    evidence_type: String,
    adapter: String,
    adapter_version: Option<String>,
    corpus: String,
    files_scanned: u64,
    files_succeeded: u64,
    files_failed: u64,
    parser_ir_nodes: u64,
    divergence_records: u64,
    divergence_occurrences: u64,
    rules_total: u64,
    rules_emitted: u64,
    rules_missing: u64,
    unsupported_occurrences: u64,
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

#[derive(Debug, Clone, PartialEq, Eq)]
enum SentenceProjectionFailureClass {
    AtomicBoundary { node_type: String, byte_offset: u64 },
    Other,
}

#[derive(Debug, Default)]
struct FileRawStats {
    nodes_total: u64,
    inferred_provenance: BTreeMap<String, u64>,
    source_marker_kinds: BTreeMap<String, u64>,
    source_classes: BTreeMap<String, u64>,
    observations: Vec<RawNodeObservation>,
}

#[derive(Debug)]
struct RawNodeObservation {
    pointer: String,
    inferred_provenance: String,
    source_class: String,
    source_marker_kind: Option<String>,
    source_preview: String,
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
    compatibility_identity: CompatibilityIdentity,
}

#[derive(Debug)]
enum FileOutcome {
    Success(Box<FileSuccess>),
    Failure { message: String },
}

#[derive(Debug)]
struct AuditFileResult {
    corpus: String,
    relative_path: String,
    raw_nodes: FileRawStats,
    outcome: FileOutcome,
}

pub fn run_audit(config: CorpusAuditConfig) -> Result<AuditSummary> {
    let started = Instant::now();
    let mapping = MappingDocument::from_path(&config.mapping_path)?;
    mapping.check_expected_generation(
        config.expect_mapping_version.as_deref(),
        config.expect_mapping_hash.as_deref(),
    )?;
    let research_root = config
        .research_root
        .clone()
        .or_else(|| std::env::var_os("AB_RESEARCH_ROOT").map(PathBuf::from));
    let schemas = SchemaSet::for_aat_version(
        &config.repo_root,
        research_root.as_deref(),
        mapping.source_aat_version,
    )?;
    let converter = PreparedConverter::new(mapping.clone(), schemas)?;
    let mapping_hash = mapping.document_hash.clone();
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
            .map(|file| audit_file(file, &converter, mapping_hash.as_str()))
            .collect::<Vec<_>>()
    } else {
        rayon::ThreadPoolBuilder::new()
            .num_threads(config.jobs)
            .build()
            .context("failed to build audit worker pool")?
            .install(|| {
                files
                    .par_iter()
                    .map(|file| audit_file(file, &converter, mapping_hash.as_str()))
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
        if files.is_empty() {
            anyhow::bail!(
                "no AAT JSON files found under {} (does it exist and contain .json files, \
                 or symlinks to them?)",
                dir.display()
            );
        }
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
            // Follow symlinks: corpus subsets (e.g. calib-*) are directories of
            // symlinks into the main corpus. `DirEntry::file_type` reports the link
            // itself, not its target, so using it here would silently skip every
            // symlinked AAT and audit as "0 files". `fs::metadata` resolves the
            // target. A broken symlink resolves to an error; skip it rather than
            // aborting the whole audit (the empty-input guard still catches a
            // directory that collects nothing).
            let Ok(metadata) = fs::metadata(&entry_path) else {
                continue;
            };
            if metadata.is_dir() {
                stack.push(entry_path);
            } else if metadata.is_file()
                && entry_path.extension().and_then(|value| value.to_str()) == Some("json")
            {
                files.push(entry_path);
            }
        }
    }
    files.sort();
    Ok(files)
}

fn audit_file(
    file: &AuditFile,
    converter: &PreparedConverter,
    mapping_hash: &str,
) -> AuditFileResult {
    let relative_path = file
        .path
        .strip_prefix(&file.root)
        .unwrap_or(&file.path)
        .display()
        .to_string();
    let (raw_nodes, outcome) = match read_json(&file.path) {
        Ok(aat) => {
            let raw_nodes = summarize_raw_nodes(&aat);
            // Corpus-scan resilience: a latent bug in conversion/sentence-projection
            // that panics on one edge-case file (e.g. an inconsistent node span in a
            // repinned dump) must not abort the whole audit. Catch the panic and
            // record it as a per-file failure, exactly as a returned Err would be.
            // The unit tests call convert() directly (outside this guard), so
            // panic-shaped regressions still surface there — only the corpus scan is
            // made fault-tolerant.
            let converted = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                converter.convert(
                    aat,
                    ConversionOptions {
                        validate_input_aat: true,
                        validate_output_parser_ir: true,
                        ..ConversionOptions::default()
                    },
                )
            }));
            let outcome = match converted {
                Ok(Ok(output)) => {
                    FileOutcome::Success(Box::new(summarize_output(&output, mapping_hash)))
                }
                Ok(Err(error)) => FileOutcome::Failure {
                    message: format!("{error:#}"),
                },
                Err(panic) => FileOutcome::Failure {
                    message: format!(
                        "internal panic during conversion: {}",
                        panic_message(&panic)
                    ),
                },
            };
            (raw_nodes, outcome)
        }
        Err(error) => (
            FileRawStats::default(),
            FileOutcome::Failure {
                message: format!("{error:#}"),
            },
        ),
    };
    AuditFileResult {
        corpus: file.corpus.clone(),
        relative_path,
        raw_nodes,
        outcome,
    }
}

/// Best-effort extraction of a panic payload's message (the string passed to
/// `panic!` / produced by `unwrap`/slice/index panics).
fn panic_message(panic: &Box<dyn std::any::Any + Send>) -> String {
    if let Some(s) = panic.downcast_ref::<&str>() {
        (*s).to_owned()
    } else if let Some(s) = panic.downcast_ref::<String>() {
        s.clone()
    } else {
        "unknown panic payload".to_owned()
    }
}

fn summarize_raw_nodes(aat: &Value) -> FileRawStats {
    let mut stats = FileRawStats::default();
    let mut pointer = String::from("$");
    collect_raw_nodes(aat, &mut pointer, &mut stats);
    stats
}

fn collect_raw_nodes(value: &Value, pointer: &mut String, stats: &mut FileRawStats) {
    // The pointer is a shared push/truncate buffer so the full-document walk
    // does not allocate a path string per visited node.
    match value {
        Value::Object(object) => {
            if object.get("kind").and_then(Value::as_str) == Some("raw") {
                record_raw_node(value, pointer, stats);
            }
            for (key, child) in object {
                let length = pointer.len();
                pointer.push('.');
                pointer.push_str(key);
                collect_raw_nodes(child, pointer, stats);
                pointer.truncate(length);
            }
        }
        Value::Array(items) => {
            use std::fmt::Write;
            for (index, child) in items.iter().enumerate() {
                let length = pointer.len();
                write!(pointer, "[{index}]").expect("pointer buffer write");
                collect_raw_nodes(child, pointer, stats);
                pointer.truncate(length);
            }
        }
        _ => {}
    }
}

fn record_raw_node(node: &Value, pointer: &str, stats: &mut FileRawStats) {
    let source = node.get("source").and_then(Value::as_str).unwrap_or("");
    let inferred_provenance = infer_raw_provenance(node, source);
    let source_class = classify_raw_source(source);
    let source_marker_kind = node
        .get("x-source-marker-kind")
        .and_then(Value::as_str)
        .filter(|value| !value.is_empty())
        .map(ToOwned::to_owned);
    stats.nodes_total += 1;
    *stats
        .inferred_provenance
        .entry(inferred_provenance.clone())
        .or_insert(0) += 1;
    *stats
        .source_classes
        .entry(source_class.clone())
        .or_insert(0) += 1;
    if let Some(kind) = &source_marker_kind {
        *stats.source_marker_kinds.entry(kind.clone()).or_insert(0) += 1;
    }
    if stats.observations.len() < 10 {
        stats.observations.push(RawNodeObservation {
            pointer: pointer.to_owned(),
            inferred_provenance,
            source_class,
            source_marker_kind,
            source_preview: source_preview(source),
        });
    }
}

fn infer_raw_provenance(node: &Value, source: &str) -> String {
    if let Some(provenance) = node
        .get("x-provenance")
        .and_then(Value::as_str)
        .filter(|value| !value.is_empty())
    {
        return provenance.to_owned();
    }
    match classify_raw_source(source).as_str() {
        "empty" | "html-fragment" | "parser-token" => "parser-derived".to_owned(),
        _ => "source-derived".to_owned(),
    }
}

fn classify_raw_source(source: &str) -> String {
    let trimmed = source.trim();
    if trimmed.is_empty() {
        "empty"
    } else if is_html_fragment(trimmed) {
        "html-fragment"
    } else if is_parser_token(trimmed) {
        "parser-token"
    } else if is_aozora_command(trimmed) {
        "aozora-command"
    } else if is_aozora_marker_text(trimmed) {
        "aozora-marker"
    } else if trimmed.contains("底本では") || trimmed.contains("入力者注") {
        "editorial-note"
    } else {
        "text"
    }
    .to_owned()
}

fn is_html_fragment(value: &str) -> bool {
    value.starts_with('<') && value.ends_with('>')
}

fn is_parser_token(value: &str) -> bool {
    value.starts_with("BlockStart(")
        || value.starts_with("BlockEnd(")
        || value.starts_with("InlineStart(")
        || value.starts_with("InlineEnd(")
}

fn is_aozora_command(value: &str) -> bool {
    matches!(
        value,
        "改頁" | "改丁" | "改段" | "改見開き" | "改行" | "改ページ"
    )
}

fn is_aozora_marker_text(value: &str) -> bool {
    value.contains("［＃")
        || value.contains("[#")
        || value.starts_with('※')
        || value.starts_with('｜')
        || value.starts_with('《')
        || value.starts_with('〔')
}

fn source_preview(source: &str) -> String {
    let normalized = source.split_whitespace().collect::<Vec<_>>().join(" ");
    let mut preview = normalized.chars().take(80).collect::<String>();
    if normalized.chars().count() > 80 {
        preview.push_str("...");
    }
    preview
}

fn summarize_output(
    output: &ab_aat_to_parser_ir::ConversionOutput,
    mapping_hash: &str,
) -> FileSuccess {
    let parser_ir_nodes = output
        .parser_ir
        .pointer("/nodes")
        .and_then(Value::as_array)
        .map_or(0, |nodes| nodes.len() as u64);
    let records = output
        .divergence_bundle
        .pointer("/records")
        .and_then(Value::as_array)
        .map(Vec::as_slice)
        .unwrap_or(&[]);
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
    let derived_from = &output.parser_ir["derived_from"];
    let compatibility_identity = CompatibilityIdentity {
        aat_version: derived_from["aat_version"]
            .as_u64()
            .expect("validated parser-IR derived_from.aat_version"),
        aat_adapter: derived_from["aat_adapter"]
            .as_str()
            .expect("validated parser-IR derived_from.aat_adapter")
            .to_owned(),
        aat_adapter_version: derived_from
            .get("aat_adapter_version")
            .and_then(Value::as_str)
            .map(ToOwned::to_owned),
        mapping_id: derived_from["mapping_id"]
            .as_str()
            .expect("validated parser-IR derived_from.mapping_id")
            .to_owned(),
        mapping_version: derived_from["mapping_version"]
            .as_str()
            .expect("validated parser-IR derived_from.mapping_version")
            .to_owned(),
        mapping_hash: mapping_hash.to_owned(),
        mapping_schema_hash: derived_from["mapping_schema_hash"]
            .as_str()
            .expect("validated parser-IR derived_from.mapping_schema_hash")
            .to_owned(),
        parser_ir_schema_id: output.parser_ir["schema_id"]
            .as_str()
            .expect("validated parser-IR schema_id")
            .to_owned(),
        parser_ir_schema_hash: output.parser_ir["schema_hash"]
            .as_str()
            .expect("validated parser-IR schema_hash")
            .to_owned(),
    };
    FileSuccess {
        parser_ir_nodes,
        divergence_records,
        divergence_occurrences,
        category_occurrences,
        rule_occurrences,
        emitted_rule_ids: output.emitted_rule_ids.clone(),
        compatibility_identity,
    }
}

fn merge_raw_file_stats(
    summary: &mut RawNodeSummary,
    corpus: &str,
    path: &str,
    file_stats: &FileRawStats,
) {
    if file_stats.nodes_total == 0 {
        return;
    }
    summary.nodes_total += file_stats.nodes_total;
    summary.files_with_raw += 1;
    let corpus_stats = summary.by_corpus.entry(corpus.to_owned()).or_default();
    corpus_stats.nodes_total += file_stats.nodes_total;
    corpus_stats.files_with_raw += 1;
    for (provenance, count) in &file_stats.inferred_provenance {
        *summary
            .inferred_provenance
            .entry(provenance.clone())
            .or_insert(0) += count;
    }
    for (kind, count) in &file_stats.source_marker_kinds {
        *summary.source_marker_kinds.entry(kind.clone()).or_insert(0) += count;
    }
    for (source_class, count) in &file_stats.source_classes {
        *summary
            .source_classes
            .entry(source_class.clone())
            .or_insert(0) += count;
    }
    for observation in &file_stats.observations {
        if summary.samples.len() >= 20 {
            break;
        }
        summary.samples.push(RawNodeSample {
            corpus: corpus.to_owned(),
            path: path.to_owned(),
            pointer: observation.pointer.clone(),
            inferred_provenance: observation.inferred_provenance.clone(),
            source_class: observation.source_class.clone(),
            source_marker_kind: observation.source_marker_kind.clone(),
            source_preview: observation.source_preview.clone(),
        });
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
    let mut raw_nodes = RawNodeSummary::default();
    let mut categories = BTreeMap::<String, u64>::new();
    let rules_total = mapping.transform_rule_descriptions.len() as u64;
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
    let mut sentence_projection_failures = SentenceProjectionFailureSummary::default();
    let mut evidence_by_identity =
        BTreeMap::<CompatibilityIdentity, CompatibilityEvidenceScope>::new();
    let mut rule_ids_by_identity = BTreeMap::<CompatibilityIdentity, BTreeSet<String>>::new();
    let mut identities_by_corpus = BTreeMap::<String, BTreeSet<CompatibilityIdentity>>::new();

    for result in results {
        let corpus_label = result.corpus.clone();
        let relative_path = result.relative_path.clone();
        totals.files_attempted += 1;
        let corpus_totals = by_corpus.entry(result.corpus.clone()).or_default();
        corpus_totals.files_attempted += 1;
        merge_raw_file_stats(
            &mut raw_nodes,
            &corpus_label,
            &relative_path,
            &result.raw_nodes,
        );
        match result.outcome {
            FileOutcome::Success(success) => {
                let success = *success;
                totals.files_succeeded += 1;
                totals.parser_ir_nodes += success.parser_ir_nodes;
                totals.divergence_records += success.divergence_records;
                totals.divergence_occurrences += success.divergence_occurrences;
                corpus_totals.files_succeeded += 1;
                corpus_totals.parser_ir_nodes += success.parser_ir_nodes;
                corpus_totals.divergence_records += success.divergence_records;
                corpus_totals.divergence_occurrences += success.divergence_occurrences;
                let unsupported_occurrences = success
                    .category_occurrences
                    .get("UNSUPPORTED")
                    .copied()
                    .unwrap_or(0);
                let compatibility_identity = success.compatibility_identity.clone();
                let evidence = evidence_by_identity
                    .entry(compatibility_identity.clone())
                    .or_insert_with(|| CompatibilityEvidenceScope {
                        evidence_type: "conversion-audit".to_owned(),
                        adapter: compatibility_identity.aat_adapter.clone(),
                        adapter_version: compatibility_identity.aat_adapter_version.clone(),
                        corpus: result.corpus.clone(),
                        rules_total,
                        ..CompatibilityEvidenceScope::default()
                    });
                identities_by_corpus
                    .entry(result.corpus.clone())
                    .or_default()
                    .insert(compatibility_identity.clone());
                evidence.files_scanned += 1;
                evidence.files_succeeded += 1;
                evidence.parser_ir_nodes += success.parser_ir_nodes;
                evidence.divergence_records += success.divergence_records;
                evidence.divergence_occurrences += success.divergence_occurrences;
                evidence.unsupported_occurrences += unsupported_occurrences;
                rule_ids_by_identity
                    .entry(compatibility_identity)
                    .or_default()
                    .extend(success.emitted_rule_ids.iter().cloned());
                for (category, count) in success.category_occurrences {
                    *categories.entry(category).or_insert(0) += count;
                }
                for rule_id in &success.emitted_rule_ids {
                    if let Some(stats) = rules_by_id.get_mut(rule_id) {
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
                if let Some(class) = classify_sentence_projection_failure(&message) {
                    sentence_projection_failures.files_failed += 1;
                    let (class_name, node_type, byte_offset) = match class {
                        SentenceProjectionFailureClass::AtomicBoundary {
                            node_type,
                            byte_offset,
                        } => {
                            *sentence_projection_failures
                                .atomic_boundary_failures_by_node_type
                                .entry(node_type.clone())
                                .or_default() += 1;
                            (
                                "atomic_boundary".to_owned(),
                                Some(node_type),
                                Some(byte_offset),
                            )
                        }
                        SentenceProjectionFailureClass::Other => {
                            sentence_projection_failures.other_sentence_projection_failures += 1;
                            ("other_sentence_projection".to_owned(), None, None)
                        }
                    };
                    if sentence_projection_failures.samples.len() < 20 {
                        sentence_projection_failures.samples.push(
                            SentenceProjectionFailureSample {
                                corpus: result.corpus.clone(),
                                path: result.relative_path.clone(),
                                class: class_name,
                                node_type,
                                byte_offset,
                                message: message.clone(),
                            },
                        );
                    }
                }
                if message == "unsupported inline kind: raw" {
                    raw_nodes.fatal_direct_failures += 1;
                    raw_nodes
                        .by_corpus
                        .entry(corpus_label)
                        .or_default()
                        .fatal_direct_failures += 1;
                }
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

    for (corpus, identities) in identities_by_corpus {
        if identities.len() != 1 {
            continue;
        }
        let Some(failed) = by_corpus.get(&corpus).map(|totals| totals.files_failed) else {
            continue;
        };
        if failed == 0 {
            continue;
        }
        let identity = identities
            .iter()
            .next()
            .expect("len checked above for sole corpus identity");
        if let Some(evidence) = evidence_by_identity.get_mut(identity) {
            evidence.files_scanned += failed;
            evidence.files_failed += failed;
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

    for (identity, evidence) in &mut evidence_by_identity {
        let emitted = rule_ids_by_identity
            .get(identity)
            .map_or(0, |rule_ids| rule_ids.len() as u64);
        evidence.rules_emitted = emitted;
        evidence.rules_missing = evidence.rules_total.saturating_sub(emitted);
    }
    let compatibility_candidates = evidence_by_identity
        .into_iter()
        .map(|(identity, evidence_scope)| CompatibilityCandidate {
            aat_version: identity.aat_version,
            aat_adapter: identity.aat_adapter,
            aat_adapter_version: identity.aat_adapter_version,
            mapping_id: identity.mapping_id,
            mapping_version: identity.mapping_version,
            mapping_hash: identity.mapping_hash,
            mapping_schema_hash: identity.mapping_schema_hash,
            parser_ir_schema_id: identity.parser_ir_schema_id,
            parser_ir_schema_hash: identity.parser_ir_schema_hash,
            evidence_scope,
            compatibility: "lossy".to_owned(),
        })
        .collect::<Vec<_>>();
    let mapping_hash = mapping.document_hash.clone();

    Ok(AuditSummary {
        generated_unix_seconds: std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .context("system clock is before Unix epoch")?
            .as_secs(),
        mapping: MappingSummary {
            path: config.mapping_path.display().to_string(),
            mapping_id: mapping.mapping_id,
            mapping_version: mapping.mapping_version,
            mapping_hash,
            mapping_schema_hash: mapping.mapping_schema_hash,
            target_parser_ir_schema_id: mapping.target_parser_ir_schema_id,
            target_parser_ir_schema_hash: mapping.target_parser_ir_schema_hash,
            rules_total,
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
        raw_nodes,
        sentence_projection_failures,
        categories,
        rule_coverage: RuleCoverage {
            rules_total: rules_by_id.len() as u64,
            rules_emitted,
            rules_missing,
            rules_by_id,
        },
        compatibility_candidates,
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
    if let Some(path) = &config.compat_edn_out {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("failed to create {}", parent.display()))?;
        }
        fs::write(path, render_compatibility_candidates_edn(summary))
            .with_context(|| format!("failed to write {}", path.display()))?;
    }
    Ok(())
}

fn render_report(summary: &AuditSummary) -> String {
    let mut out = String::new();
    out.push_str("# AAT Parser-IR Conversion Audit\n\n");
    out.push_str(&format!(
        "- generated_unix_seconds: `{}`\n",
        summary.generated_unix_seconds
    ));
    out.push_str(&format!(
        "- mapping: `{}` `{}`\n",
        summary.mapping.mapping_id, summary.mapping.mapping_version
    ));
    out.push_str(&format!(
        "- mapping_hash: `{}`\n",
        summary.mapping.mapping_hash
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

    out.push_str("## Raw Nodes\n\n");
    out.push_str(&format!(
        "- nodes_total: `{}`\n- files_with_raw: `{}`\n- fatal_direct_failures: `{}`\n\n",
        summary.raw_nodes.nodes_total,
        summary.raw_nodes.files_with_raw,
        summary.raw_nodes.fatal_direct_failures
    ));
    if !summary.raw_nodes.by_corpus.is_empty() {
        out.push_str("| corpus | nodes_total | files_with_raw | fatal_direct_failures |\n");
        out.push_str("|---|---:|---:|---:|\n");
        for (corpus, stats) in &summary.raw_nodes.by_corpus {
            out.push_str(&format!(
                "| {} | {} | {} | {} |\n",
                table_cell(corpus),
                stats.nodes_total,
                stats.files_with_raw,
                stats.fatal_direct_failures
            ));
        }
        out.push('\n');
    }
    if !summary.raw_nodes.inferred_provenance.is_empty() {
        out.push_str("| inferred_provenance | nodes |\n|---|---:|\n");
        for (provenance, count) in &summary.raw_nodes.inferred_provenance {
            out.push_str(&format!("| {} | {} |\n", table_cell(provenance), count));
        }
        out.push('\n');
    }
    if !summary.raw_nodes.source_classes.is_empty() {
        out.push_str("| source_class | nodes |\n|---|---:|\n");
        for (source_class, count) in &summary.raw_nodes.source_classes {
            out.push_str(&format!("| {} | {} |\n", table_cell(source_class), count));
        }
        out.push('\n');
    }
    if !summary.raw_nodes.samples.is_empty() {
        out.push_str("| corpus | path | pointer | provenance | class | source_marker_kind | source_preview |\n");
        out.push_str("|---|---|---|---|---|---|---|\n");
        for sample in &summary.raw_nodes.samples {
            out.push_str(&format!(
                "| {} | `{}` | `{}` | {} | {} | {} | {} |\n",
                table_cell(&sample.corpus),
                sample.path,
                sample.pointer,
                table_cell(&sample.inferred_provenance),
                table_cell(&sample.source_class),
                table_cell(sample.source_marker_kind.as_deref().unwrap_or("")),
                table_cell(&sample.source_preview)
            ));
        }
        out.push('\n');
    }

    out.push_str("## Sentence Projection Failures\n\n");
    out.push_str(&format!(
        "Files failed during sentence projection: {}\n\n",
        summary.sentence_projection_failures.files_failed
    ));
    out.push_str("| Atomic Node Type | Failures |\n");
    out.push_str("|---|---:|\n");
    for (node_type, count) in &summary
        .sentence_projection_failures
        .atomic_boundary_failures_by_node_type
    {
        out.push_str(&format!("| {} | {} |\n", table_cell(node_type), count));
    }
    out.push('\n');
    out.push_str(&format!(
        "Other sentence projection failures: {}\n\n",
        summary
            .sentence_projection_failures
            .other_sentence_projection_failures
    ));

    out.push_str("## Divergence Categories\n\n");
    out.push_str("| category | occurrences |\n|---|---:|\n");
    for (category, count) in &summary.categories {
        out.push_str(&format!("| {} | {} |\n", table_cell(category), count));
    }
    out.push('\n');

    out.push_str("## Compatibility Candidates\n\n");
    if summary.compatibility_candidates.is_empty() {
        out.push_str("No compatibility candidates were emitted.\n\n");
    } else {
        out.push_str("| adapter | adapter_version | mapping_version | mapping_hash | files_succeeded | files_failed | rules_emitted | rules_missing | unsupported_occurrences |\n");
        out.push_str("|---|---|---|---|---:|---:|---:|---:|---:|\n");
        for candidate in &summary.compatibility_candidates {
            let scope = &candidate.evidence_scope;
            out.push_str(&format!(
                "| {} | {} | {} | `{}` | {} | {} | {} | {} | {} |\n",
                table_cell(&candidate.aat_adapter),
                table_cell(candidate.aat_adapter_version.as_deref().unwrap_or("")),
                table_cell(&candidate.mapping_version),
                candidate.mapping_hash,
                scope.files_succeeded,
                scope.files_failed,
                scope.rules_emitted,
                scope.rules_missing,
                scope.unsupported_occurrences,
            ));
        }
        out.push('\n');
    }

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

fn edn_quote(value: &str) -> String {
    let mut out = String::from("\"");
    for ch in value.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\u{08}' => out.push_str("\\b"),
            '\u{0c}' => out.push_str("\\f"),
            ch if ch.is_control() => out.push_str(&format!("\\u{:04x}", ch as u32)),
            ch => out.push(ch),
        }
    }
    out.push('"');
    out
}

fn edn_option_string(value: Option<&str>) -> String {
    value.map(edn_quote).unwrap_or_else(|| "nil".to_owned())
}

fn render_compatibility_candidates_edn(summary: &AuditSummary) -> String {
    let mut out = String::from("{:entries\n [");
    for (index, candidate) in summary.compatibility_candidates.iter().enumerate() {
        if index > 0 {
            out.push('\n');
        }
        let scope = &candidate.evidence_scope;
        out.push_str(&format!(
            "{{:aat_version {}\n  :aat_adapter {}\n  :aat_adapter_version {}\n  :mapping_id {}\n  :mapping_version {}\n  :mapping_hash {}\n  :mapping_schema_hash {}\n  :parser_ir_schema_id {}\n  :parser_ir_schema_hash {}\n  :evidence_scope {{:evidence_type :conversion-audit\n                   :adapter {}\n                   :adapter_version {}\n                   :corpus {}\n                   :files_scanned {}\n                   :files_succeeded {}\n                   :files_failed {}\n                   :parser_ir_nodes {}\n                   :divergence_records {}\n                   :divergence_occurrences {}\n                   :rules_total {}\n                   :rules_emitted {}\n                   :rules_missing {}\n                   :unsupported_occurrences {}}}\n  :compatibility {}}}",
            candidate.aat_version,
            edn_quote(&candidate.aat_adapter),
            edn_option_string(candidate.aat_adapter_version.as_deref()),
            edn_quote(&candidate.mapping_id),
            edn_quote(&candidate.mapping_version),
            edn_quote(&candidate.mapping_hash),
            edn_quote(&candidate.mapping_schema_hash),
            edn_quote(&candidate.parser_ir_schema_id),
            edn_quote(&candidate.parser_ir_schema_hash),
            edn_quote(&scope.adapter),
            edn_option_string(scope.adapter_version.as_deref()),
            edn_quote(&scope.corpus),
            scope.files_scanned,
            scope.files_succeeded,
            scope.files_failed,
            scope.parser_ir_nodes,
            scope.divergence_records,
            scope.divergence_occurrences,
            scope.rules_total,
            scope.rules_emitted,
            scope.rules_missing,
            scope.unsupported_occurrences,
            edn_quote(&candidate.compatibility),
        ));
    }
    out.push_str("]}\n");
    out
}

fn round_seconds(seconds: f64) -> f64 {
    (seconds * 1000.0).round() / 1000.0
}

fn classify_sentence_projection_failure(message: &str) -> Option<SentenceProjectionFailureClass> {
    let marker = "sentence boundary falls inside atomic node ";
    if let Some(rest) = message.strip_prefix(marker)
        && let Some((node_type, byte_text)) = rest.split_once(" at byte ")
        && let Ok(byte_offset) = byte_text.parse::<u64>()
    {
        return Some(SentenceProjectionFailureClass::AtomicBoundary {
            node_type: node_type.to_owned(),
            byte_offset,
        });
    }
    if message.contains("sentence boundary")
        || message.contains("sentence spans")
        || message.contains("sentence node ranges")
        || message.contains("body paragraph has no sentence spans")
    {
        Some(SentenceProjectionFailureClass::Other)
    } else {
        None
    }
}

#[cfg(test)]
mod sentence_projection_audit_tests {
    use super::*;

    #[test]
    fn classifies_atomic_boundary_projection_failure() {
        let classified = classify_sentence_projection_failure(
            "sentence boundary falls inside atomic node ruby at byte 24",
        );
        assert_eq!(
            classified,
            Some(SentenceProjectionFailureClass::AtomicBoundary {
                node_type: "ruby".to_owned(),
                byte_offset: 24,
            })
        );
    }

    #[test]
    fn classifies_other_sentence_projection_failure() {
        let classified = classify_sentence_projection_failure(
            "sentence spans end at 24, expected paragraph end 30",
        );
        assert_eq!(classified, Some(SentenceProjectionFailureClass::Other));
    }

    #[test]
    fn ignores_non_sentence_failures() {
        assert_eq!(
            classify_sentence_projection_failure("mapping schema hash mismatch"),
            None
        );
    }

    #[test]
    fn collect_json_files_follows_symlinked_entries() {
        // Corpus subsets are directories of symlinks into the main corpus; the
        // collector must resolve link targets, not skip them.
        let target_dir = tempfile::tempdir().unwrap();
        let real = target_dir.path().join("work.json");
        std::fs::write(&real, b"{}").unwrap();

        let subset = tempfile::tempdir().unwrap();
        std::fs::write(subset.path().join("direct.json"), b"{}").unwrap();
        std::os::unix::fs::symlink(&real, subset.path().join("linked.json")).unwrap();
        // A broken symlink must be skipped, not abort collection.
        std::os::unix::fs::symlink(
            target_dir.path().join("missing.json"),
            subset.path().join("broken.json"),
        )
        .unwrap();

        let mut names: Vec<String> = collect_json_files(subset.path())
            .unwrap()
            .iter()
            .map(|p| p.file_name().unwrap().to_string_lossy().into_owned())
            .collect();
        names.sort();
        assert_eq!(names, vec!["direct.json", "linked.json"]);
    }

    #[test]
    fn collect_inputs_errors_on_directory_with_no_aat_files() {
        // A directory that collects zero files must fail loudly rather than
        // producing a silent "0 succeeded, 0 failed" success.
        let empty = tempfile::tempdir().unwrap();
        let error = collect_inputs(&[empty.path().to_path_buf()])
            .unwrap_err()
            .to_string();
        assert!(error.contains("no AAT JSON files found"), "got: {error}");
    }
}
