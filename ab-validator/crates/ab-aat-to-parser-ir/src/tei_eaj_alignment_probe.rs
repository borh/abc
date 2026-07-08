use std::{collections::BTreeMap, path::PathBuf};

use ab_diff_utils::{
    AlignmentConfig, AlignmentKind, AlignmentRegion, ComparisonEvidence, ComparisonToken,
    algorithm_config_hash, align_pair, remove_unicode_whitespace, sentence_like_runs,
};
use anyhow::{Context, Result, bail};
use roxmltree::{Document, Node};
use serde::Serialize;
use serde_json::{Value, json};

#[derive(Debug, Clone)]
pub struct TeiEajAlignmentProbeConfig {
    pub workset_path: PathBuf,
    pub max_rows: Option<usize>,
}

#[derive(Debug, Clone, Serialize)]
pub struct TeiEajAlignmentProbeReport {
    pub schema_version: String,
    pub rows: Vec<TeiEajAlignmentProbeRow>,
}

#[derive(Debug, Clone, Serialize)]
pub struct TeiEajAlignmentProbeRow {
    pub work_id: Option<String>,
    pub title: Option<String>,
    pub tei_eaj_file: String,
    pub alignment_probe: Option<AlignmentProbe>,
}

#[derive(Debug, Clone, Serialize)]
pub struct AlignmentProbe {
    pub schema_version: String,
    pub evidence_level: ComparisonEvidence,
    pub algorithm_id: String,
    pub algorithm_config_hash: String,
    pub algorithm_config: Value,
    pub tokenization_id: String,
    pub normalization_id: String,
    pub left_witness: String,
    pub right_witness: String,
    pub summary: Value,
    pub diagnosis_event_count: usize,
    pub diagnosis_counts: BTreeMap<String, usize>,
    pub samples: Vec<ProbeSample>,
    pub truncated: bool,
    pub limits: Value,
}

#[derive(Debug, Clone, Serialize)]
pub struct ProbeSample {
    pub kind: String,
    pub diagnosis: String,
    pub left_range: [usize; 2],
    pub right_range: [usize; 2],
    pub left_text: String,
    pub right_text: String,
    pub truncated: bool,
    pub adapter_context: BTreeMap<String, Value>,
}

#[derive(Debug, Clone)]
struct TeiTokenContext {
    path: String,
    features: Vec<String>,
}

#[derive(Debug, Clone)]
struct TeiToken {
    token: ComparisonToken,
    context: TeiTokenContext,
}

const MAX_SAMPLES: usize = 8;
const MAX_SAMPLE_CHARS: usize = 160;
const TOKENIZATION_ID: &str = "tei-body-sentence-like-text-run-v1";
const NORMALIZATION_ID: &str = "tei-eaj-base-text-no-ws-v1";

pub fn run_tei_eaj_alignment_probe(
    config: TeiEajAlignmentProbeConfig,
) -> Result<TeiEajAlignmentProbeReport> {
    let workset = crate::tei_eaj_workset::read_tei_eaj_workset(&config.workset_path)?;
    let mut rows = Vec::new();
    for file in &workset.files {
        if config.max_rows.is_some_and(|limit| rows.len() >= limit) {
            break;
        }
        if file.comparison_status != "compared" || file.base_text_equal == Some(true) {
            continue;
        }
        let Some(abc_tei) = file.abc_tei.as_deref() else {
            continue;
        };
        let left_xml = std::fs::read_to_string(abc_tei)
            .with_context(|| format!("failed to read ABC TEI {abc_tei}"))?;
        let right_path = crate::tei_eaj_workset::resolve_tei_eaj_path(&workset, &file.tei_eaj_file);
        let right_xml = std::fs::read_to_string(&right_path)
            .with_context(|| format!("failed to read TEI-EAJ TEI {}", right_path.display()))?;
        let left = extract_body_tokens(&left_xml)?;
        let right = extract_body_tokens(&right_xml)?;
        let probe = build_probe(&left, &right)?;
        assert_probe_invariants(&probe)?;
        rows.push(TeiEajAlignmentProbeRow {
            work_id: file.work_id.clone(),
            title: file.title.clone(),
            tei_eaj_file: file.tei_eaj_file.clone(),
            alignment_probe: Some(probe),
        });
    }
    Ok(TeiEajAlignmentProbeReport {
        schema_version: "tei-eaj-alignment-probe-report-v1".to_owned(),
        rows,
    })
}

pub fn write_tei_eaj_alignment_probe_reports(
    report: &TeiEajAlignmentProbeReport,
    summary_json: &std::path::Path,
    report_md: &std::path::Path,
) -> Result<()> {
    write_parent(summary_json)?;
    write_parent(report_md)?;
    std::fs::write(summary_json, serde_json::to_string_pretty(report)? + "\n")
        .with_context(|| format!("failed to write {}", summary_json.display()))?;
    std::fs::write(report_md, render_tei_eaj_alignment_probe_markdown(report))
        .with_context(|| format!("failed to write {}", report_md.display()))?;
    Ok(())
}

pub fn render_tei_eaj_alignment_probe_markdown(report: &TeiEajAlignmentProbeReport) -> String {
    let mut out = String::from("# TEI-EAJ Alignment Probe\n\n");
    out.push_str("| work_id | title | tei_eaj_file | diagnosis_counts |\n");
    out.push_str("| --- | --- | --- | --- |\n");
    for row in &report.rows {
        let counts = row
            .alignment_probe
            .as_ref()
            .map(|probe| serde_json::to_string(&probe.diagnosis_counts).unwrap_or_default())
            .unwrap_or_else(|| "{}".to_owned());
        out.push_str(&format!(
            "| {} | {} | `{}` | `{}` |\n",
            md_cell(row.work_id.as_deref().unwrap_or("")),
            md_cell(row.title.as_deref().unwrap_or("")),
            md_code(&row.tei_eaj_file),
            md_code(&counts)
        ));
    }
    out
}

fn write_parent(path: &std::path::Path) -> Result<()> {
    if let Some(parent) = path.parent()
        && !parent.as_os_str().is_empty()
    {
        std::fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    Ok(())
}

fn md_cell(value: &str) -> String {
    value.replace('|', "\\|")
}

fn md_code(value: &str) -> String {
    value.replace('`', "\\`")
}

fn build_probe(left: &[TeiToken], right: &[TeiToken]) -> Result<AlignmentProbe> {
    let config = AlignmentConfig::default();
    let left_tokens = comparison_tokens(left);
    let right_tokens = comparison_tokens(right);
    let result = align_pair(&left_tokens, &right_tokens, &config);
    let algorithm_config =
        serde_json::to_value(&config).context("failed to serialize alignment config")?;
    let mut diagnosis_counts = BTreeMap::new();
    let mut samples = Vec::new();

    for region in result
        .regions
        .iter()
        .filter(|region| region.kind != AlignmentKind::Equal)
    {
        let diagnosis = diagnose_region(region, left, right);
        *diagnosis_counts.entry(diagnosis.clone()).or_insert(0) += 1;
        if samples.len() < MAX_SAMPLES {
            samples.push(sample_for_region(region, &diagnosis, left, right));
        }
    }

    let diagnosis_event_count = diagnosis_counts.values().sum();
    Ok(AlignmentProbe {
        schema_version: "alignment-probe-v1".to_owned(),
        evidence_level: ComparisonEvidence::TokenSequenceAligned,
        algorithm_id: "abc-pairwise-token-align-v1".to_owned(),
        algorithm_config_hash: algorithm_config_hash(&config),
        algorithm_config,
        tokenization_id: TOKENIZATION_ID.to_owned(),
        normalization_id: NORMALIZATION_ID.to_owned(),
        left_witness: "abc".to_owned(),
        right_witness: "tei_eaj".to_owned(),
        summary: serde_json::to_value(&result.summary)
            .context("failed to serialize alignment summary")?,
        diagnosis_event_count,
        diagnosis_counts,
        samples,
        truncated: result.truncated,
        limits: json!({
            "max_tokens_per_window": config.max_tokens_per_window,
            "max_samples": MAX_SAMPLES,
            "max_sample_chars": MAX_SAMPLE_CHARS
        }),
    })
}

fn extract_body_tokens(xml: &str) -> Result<Vec<TeiToken>> {
    let document = Document::parse(xml).context("failed to parse TEI XML")?;
    let body = document
        .descendants()
        .find(|node| node.is_element() && node.tag_name().name() == "body")
        .context("TEI body not found")?;
    let mut tokens = Vec::new();
    for element in body
        .descendants()
        .filter(|node| node.is_element() && matches!(node.tag_name().name(), "p" | "head"))
    {
        let base_text = base_text_excluding_apparatus(element);
        for (run_idx, run) in sentence_like_runs(&base_text).into_iter().enumerate() {
            let normalized = remove_unicode_whitespace(&run);
            if normalized.is_empty() {
                continue;
            }
            let mut features = Vec::new();
            if normalized.starts_with('（') && normalized.contains("シルレルの詩から") {
                features.push("source-attribution".to_owned());
            }
            let ordinal = tokens.len();
            tokens.push(TeiToken {
                token: ComparisonToken {
                    ordinal,
                    text: run,
                    normalized,
                },
                context: TeiTokenContext {
                    path: format!("{}#run{}", element_path(element), run_idx),
                    features,
                },
            });
        }
    }
    Ok(tokens)
}

fn base_text_excluding_apparatus(node: Node<'_, '_>) -> String {
    fn visit(node: Node<'_, '_>, out: &mut String) {
        if node.is_text() {
            out.push_str(node.text().unwrap_or_default());
            return;
        }
        if node.is_element() && skip_base_text_element(node) {
            return;
        }
        for child in node.children() {
            visit(child, out);
        }
    }

    let mut out = String::new();
    visit(node, &mut out);
    out
}

fn skip_base_text_element(node: Node<'_, '_>) -> bool {
    let name = node.tag_name().name();
    if matches!(name, "rt" | "rp" | "note") {
        return true;
    }
    name == "span"
        && (matches!(node.attribute("type"), Some("rt" | "rp"))
            || node.attribute("rend") == Some("notes"))
}

fn element_path(node: Node<'_, '_>) -> String {
    let mut parts = Vec::new();
    for ancestor in node.ancestors().filter(|candidate| candidate.is_element()) {
        let name = ancestor.tag_name().name();
        let mut index = 1;
        let mut previous = ancestor.prev_sibling();
        while let Some(sibling) = previous {
            if sibling.is_element() && sibling.tag_name().name() == name {
                index += 1;
            }
            previous = sibling.prev_sibling();
        }
        parts.push(format!("{name}[{index}]"));
    }
    parts.reverse();
    format!("/{}", parts.join("/"))
}

fn comparison_tokens(tokens: &[TeiToken]) -> Vec<ComparisonToken> {
    tokens.iter().map(|token| token.token.clone()).collect()
}

fn diagnose_region(region: &AlignmentRegion, left: &[TeiToken], _right: &[TeiToken]) -> String {
    if region.kind == AlignmentKind::Insertion
        && region.right_range[0] == region.right_range[1]
        && region.left_range[1] == left.len()
        && left[region.left_range[0]..region.left_range[1]]
            .iter()
            .any(|token| {
                token
                    .context
                    .features
                    .iter()
                    .any(|feature| feature == "source-attribution")
            })
    {
        return "tail_addition".to_owned();
    }
    match region.kind {
        AlignmentKind::Insertion => "insertion".to_owned(),
        AlignmentKind::Deletion => "deletion".to_owned(),
        AlignmentKind::Substitution => "substitution".to_owned(),
        AlignmentKind::LikelyMovedBlock => "likely_moved_block".to_owned(),
        AlignmentKind::UnclassifiedMismatch => "unclassified_mismatch".to_owned(),
        AlignmentKind::Equal => "equal".to_owned(),
    }
}

fn sample_for_region(
    region: &AlignmentRegion,
    diagnosis: &str,
    left: &[TeiToken],
    right: &[TeiToken],
) -> ProbeSample {
    let mut adapter_context = BTreeMap::new();
    adapter_context.insert("left_path".to_owned(), first_path(left, region.left_range));
    adapter_context.insert(
        "right_path".to_owned(),
        first_path(right, region.right_range),
    );
    adapter_context.insert(
        "left_features".to_owned(),
        serde_json::to_value(features_in_range(left, region.left_range))
            .expect("features serialize"),
    );
    adapter_context.insert(
        "right_features".to_owned(),
        serde_json::to_value(features_in_range(right, region.right_range))
            .expect("features serialize"),
    );
    ProbeSample {
        kind: kind_name(region.kind).to_owned(),
        diagnosis: diagnosis.to_owned(),
        left_range: region.left_range,
        right_range: region.right_range,
        left_text: truncate_sample(&region.left_text_sample),
        right_text: truncate_sample(&region.right_text_sample),
        truncated: region.truncated
            || region.left_text_sample.chars().count() > MAX_SAMPLE_CHARS
            || region.right_text_sample.chars().count() > MAX_SAMPLE_CHARS,
        adapter_context,
    }
}

fn first_path(tokens: &[TeiToken], range: [usize; 2]) -> Value {
    tokens
        .get(range[0])
        .filter(|_| range[0] < range[1])
        .map(|token| json!(token.context.path))
        .unwrap_or(Value::Null)
}

fn features_in_range(tokens: &[TeiToken], range: [usize; 2]) -> Vec<String> {
    let mut features = Vec::new();
    for token in &tokens[range[0]..range[1]] {
        for feature in &token.context.features {
            if !features.contains(feature) {
                features.push(feature.clone());
            }
        }
    }
    features
}

fn kind_name(kind: AlignmentKind) -> &'static str {
    match kind {
        AlignmentKind::Equal => "equal",
        AlignmentKind::Insertion => "insertion",
        AlignmentKind::Deletion => "deletion",
        AlignmentKind::Substitution => "substitution",
        AlignmentKind::LikelyMovedBlock => "likely_moved_block",
        AlignmentKind::UnclassifiedMismatch => "unclassified_mismatch",
    }
}

fn truncate_sample(value: &str) -> String {
    value.chars().take(MAX_SAMPLE_CHARS).collect()
}

fn assert_probe_invariants(probe: &AlignmentProbe) -> Result<()> {
    let diagnosis_sum: usize = probe.diagnosis_counts.values().sum();
    if diagnosis_sum != probe.diagnosis_event_count {
        bail!(
            "alignment probe diagnosis_event_count {} does not match diagnosis_counts sum {}",
            probe.diagnosis_event_count,
            diagnosis_sum
        );
    }
    let recomputed_hash = ab_diff_utils::hash_json_canonical(&probe.algorithm_config)
        .context("failed to hash emitted algorithm_config")?;
    if recomputed_hash != probe.algorithm_config_hash {
        bail!(
            "alignment probe algorithm_config_hash {} does not match emitted algorithm_config hash {}",
            probe.algorithm_config_hash,
            recomputed_hash
        );
    }
    Ok(())
}
