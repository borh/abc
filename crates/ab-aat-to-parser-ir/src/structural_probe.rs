use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::{ConversionOptions, MappingDocument, PreparedConverter, SchemaSet, schema::read_json};

#[derive(Debug, Clone)]
pub struct StructuralProbeConfig {
    pub inputs: Vec<StructuralProbeInput>,
    pub mapping: MappingDocument,
    pub schemas: SchemaSet,
}

#[derive(Debug, Clone)]
pub struct StructuralProbeInput {
    pub label: String,
    pub path: PathBuf,
}

#[derive(Debug, Clone)]
pub struct TeiEajStructuralExpansionConfig {
    pub workset_path: PathBuf,
    pub aat_dirs: Vec<StructuralProbeInput>,
    pub mapping: MappingDocument,
    pub schemas: SchemaSet,
}

#[derive(Debug, Clone, Serialize)]
pub struct StructuralProbeSummary {
    pub mapping: StructuralProbeMappingSummary,
    pub totals: StructuralProbeTotals,
    pub inputs: Vec<StructuralProbeInputSummary>,
}

#[derive(Debug, Clone, Serialize)]
pub struct TeiEajStructuralExpansionSummary {
    pub mapping: StructuralProbeMappingSummary,
    pub workset: TeiEajWorksetSummary,
    pub totals: TeiEajExpansionTotals,
    pub rows: Vec<TeiEajStructuralRowSummary>,
}

#[derive(Debug, Clone, Serialize)]
pub struct StructuralProbeMappingSummary {
    pub mapping_id: String,
    pub mapping_version: String,
    pub mapping_hash: String,
    pub mapping_schema_hash: String,
    pub target_parser_ir_schema_id: String,
    pub target_parser_ir_schema_hash: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct TeiEajWorksetSummary {
    pub schema_version: String,
    pub tei_eaj_source_revision: Option<String>,
    pub tei_eaj_source_root: Option<String>,
    pub tei_eaj_files: u64,
    pub candidate_work_ids: u64,
    pub compared_files: u64,
    pub missing_abc_counterpart_work_ids: u64,
    pub no_work_id_files: u64,
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct StructuralProbeTotals {
    pub inputs: u64,
    pub conversions_succeeded: u64,
    pub conversions_failed: u64,
    pub paragraph_gap_inputs: u64,
    pub source_attribution_gap_inputs: u64,
    pub residual_free_inputs: u64,
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct TeiEajExpansionTotals {
    pub tei_eaj_files: u64,
    pub candidate_work_ids: u64,
    pub compared_files: u64,
    pub missing_abc_counterpart_work_ids: u64,
    pub no_work_id_files: u64,
    pub rows_with_aat_evidence: u64,
    pub parser_ir_gap_rows: u64,
    pub adapter_gap_rows: u64,
    pub source_attribution_gap_rows: u64,
    pub evidence_gap_rows: u64,
}

#[derive(Debug, Clone, Serialize)]
pub struct StructuralProbeInputSummary {
    pub label: String,
    pub path: String,
    pub aat: AatStructuralSummary,
    pub conversion: ConversionProbeSummary,
    pub parser_ir: ParserIrStructuralSummary,
    pub divergence: DivergenceProbeSummary,
    pub verdict: StructuralProbeVerdict,
}

#[derive(Debug, Clone, Serialize)]
pub struct TeiEajStructuralRowSummary {
    pub tei: TeiEajFileSummary,
    pub aat_inputs: Vec<StructuralProbeInputSummary>,
    pub classification: TeiEajGapClassification,
}

#[derive(Debug, Clone, Serialize)]
pub struct TeiEajFileSummary {
    pub work_id: Option<String>,
    pub title: Option<String>,
    pub tei_eaj_file: String,
    pub level: Option<String>,
    pub state: Option<String>,
    pub comparison_status: String,
    pub tei_eaj_p_count: Option<u64>,
    pub tei_eaj_note_count: Option<u64>,
    pub abc_p_count: Option<u64>,
    pub abc_note_count: Option<u64>,
    pub base_text_equal: Option<bool>,
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct TeiEajGapClassification {
    pub parser_ir_gap: bool,
    pub adapter_gap: bool,
    pub source_attribution_gap: bool,
    pub evidence_gap: bool,
    pub owners: Vec<String>,
    pub notes: Vec<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct AatStructuralSummary {
    pub work_id: Option<String>,
    pub adapter: Option<String>,
    pub adapter_version: Option<String>,
    pub parse_complete: Option<bool>,
    pub block_count: u64,
    pub block_kinds: BTreeMap<String, u64>,
    pub paragraph_blocks: u64,
    pub final_block_kind: Option<String>,
    pub final_visible_text: Option<String>,
    pub final_source_attribution_text: Option<String>,
    pub final_source_attribution_candidate: bool,
    pub hints: Vec<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct ConversionProbeSummary {
    pub success: bool,
    pub error: Option<String>,
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct ParserIrStructuralSummary {
    pub node_count: u64,
    pub node_kinds: BTreeMap<String, u64>,
    pub paragraph_count: u64,
    pub paragraphs_represented: bool,
    pub source_attribution_represented: bool,
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct DivergenceProbeSummary {
    pub categories: BTreeMap<String, u64>,
    pub paragraph_structural_records: Vec<DivergenceRecordProbeSummary>,
}

#[derive(Debug, Clone, Serialize)]
pub struct DivergenceRecordProbeSummary {
    pub rule_id: String,
    pub category: String,
    pub aat_pointer: Option<String>,
    pub parser_ir_pointer: Option<String>,
    pub count: u64,
    pub first_path: Option<String>,
    pub message: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct StructuralProbeVerdict {
    pub paragraphs_represented_in_parser_ir: bool,
    pub source_attribution_represented_in_parser_ir: bool,
    pub residual_free: bool,
    pub notes: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct TeiEajWorksetExport {
    schema_version: String,
    summary: TeiEajExportSummary,
    #[serde(default)]
    tei_eaj_source: Option<TeiEajSourceExport>,
    #[serde(default)]
    candidate_work_ids: Vec<String>,
    #[serde(default)]
    missing_abc_counterpart_work_ids: Vec<String>,
    #[serde(default)]
    no_work_id_files: Vec<String>,
    #[serde(default)]
    files: Vec<TeiEajFileExport>,
}

#[derive(Debug, Default, Deserialize)]
struct TeiEajExportSummary {
    #[serde(default)]
    tei_eaj_file_count: u64,
    #[serde(default)]
    tei_eaj_work_id_count: u64,
    #[serde(default)]
    compared_file_count: u64,
    #[serde(default)]
    missing_counterpart_count: u64,
    #[serde(default)]
    no_work_id_count: u64,
}

#[derive(Debug, Deserialize)]
struct TeiEajSourceExport {
    revision: Option<String>,
    root: Option<String>,
}

#[derive(Debug, Deserialize)]
struct TeiEajFileExport {
    work_id: Option<String>,
    title: Option<String>,
    tei_eaj_file: String,
    level: Option<String>,
    state: Option<String>,
    comparison_status: String,
    tei_eaj_p_count: Option<u64>,
    tei_eaj_note_count: Option<u64>,
    abc_p_count: Option<u64>,
    abc_note_count: Option<u64>,
    base_text_equal: Option<bool>,
}

pub fn parse_input_spec(spec: &str) -> Result<StructuralProbeInput> {
    let Some((label, path)) = spec.split_once('=') else {
        bail!("--aat must be label=/path/to/input.aat.json, got {spec:?}");
    };
    if label.trim().is_empty() {
        bail!("--aat label must not be empty");
    }
    if path.trim().is_empty() {
        bail!("--aat path must not be empty");
    }
    Ok(StructuralProbeInput {
        label: label.to_owned(),
        path: PathBuf::from(path),
    })
}

pub fn run_structural_probe(config: StructuralProbeConfig) -> Result<StructuralProbeSummary> {
    if config.inputs.is_empty() {
        bail!("at least one --aat input is required");
    }

    let mapping_summary = StructuralProbeMappingSummary {
        mapping_id: config.mapping.mapping_id.clone(),
        mapping_version: config.mapping.mapping_version.clone(),
        mapping_hash: config.mapping.document_hash.clone(),
        mapping_schema_hash: config.mapping.mapping_schema_hash.clone(),
        target_parser_ir_schema_id: config.mapping.target_parser_ir_schema_id.clone(),
        target_parser_ir_schema_hash: config.mapping.target_parser_ir_schema_hash.clone(),
    };
    let converter = PreparedConverter::new(config.mapping, config.schemas)?;

    let mut inputs = Vec::new();
    for input in config.inputs {
        inputs.push(summarize_structural_input(&converter, input)?);
    }

    let totals = summarize_totals(&inputs);
    Ok(StructuralProbeSummary {
        mapping: mapping_summary,
        totals,
        inputs,
    })
}

pub fn run_tei_eaj_structural_expansion(
    config: TeiEajStructuralExpansionConfig,
) -> Result<TeiEajStructuralExpansionSummary> {
    let workset_value = read_json(&config.workset_path).with_context(|| {
        format!(
            "failed to read TEI-EAJ workset {}",
            config.workset_path.display()
        )
    })?;
    let workset: TeiEajWorksetExport =
        serde_json::from_value(workset_value).with_context(|| {
            format!(
                "failed to parse TEI-EAJ workset {}",
                config.workset_path.display()
            )
        })?;
    let mapping_summary = mapping_summary(&config.mapping);
    let converter = PreparedConverter::new(config.mapping, config.schemas)?;
    let work_id_aliases = tei_eaj_work_id_aliases(&workset);
    let aat_index = index_aat_dirs(&config.aat_dirs, &work_id_aliases)?;

    let mut rows = Vec::new();
    let tei_eaj_source_root = workset
        .tei_eaj_source
        .as_ref()
        .and_then(|source| source.root.as_deref());
    for tei_row in workset.files {
        let tei = tei_row.to_summary(tei_eaj_source_root);
        let mut aat_inputs = Vec::new();
        if let Some(work_id) = &tei.work_id
            && let Some(inputs) = aat_index.get(work_id)
        {
            for input in inputs {
                aat_inputs.push(summarize_structural_input(&converter, input.clone())?);
            }
        }
        let classification = classify_tei_eaj_row(&tei, &aat_inputs);
        rows.push(TeiEajStructuralRowSummary {
            tei,
            aat_inputs,
            classification,
        });
    }

    let workset_summary = TeiEajWorksetSummary {
        schema_version: workset.schema_version,
        tei_eaj_source_revision: workset
            .tei_eaj_source
            .as_ref()
            .and_then(|source| source.revision.clone()),
        tei_eaj_source_root: workset
            .tei_eaj_source
            .as_ref()
            .and_then(|source| source.root.clone()),
        tei_eaj_files: workset.summary.tei_eaj_file_count,
        candidate_work_ids: workset
            .candidate_work_ids
            .len()
            .try_into()
            .unwrap_or(workset.summary.tei_eaj_work_id_count),
        compared_files: workset.summary.compared_file_count,
        missing_abc_counterpart_work_ids: workset
            .missing_abc_counterpart_work_ids
            .len()
            .try_into()
            .unwrap_or(workset.summary.missing_counterpart_count),
        no_work_id_files: workset.summary.no_work_id_count.max(
            workset
                .no_work_id_files
                .len()
                .try_into()
                .unwrap_or(u64::MAX),
        ),
    };
    let totals = summarize_tei_eaj_totals(&workset_summary, &rows);

    Ok(TeiEajStructuralExpansionSummary {
        mapping: mapping_summary,
        workset: workset_summary,
        totals,
        rows,
    })
}

pub fn write_structural_probe_reports(
    summary: &StructuralProbeSummary,
    summary_json: &Path,
    report_md: &Path,
) -> Result<()> {
    write_parent(summary_json)?;
    write_parent(report_md)?;
    fs::write(summary_json, serde_json::to_string_pretty(summary)? + "\n")
        .with_context(|| format!("failed to write {}", summary_json.display()))?;
    fs::write(report_md, render_markdown(summary))
        .with_context(|| format!("failed to write {}", report_md.display()))?;
    Ok(())
}

pub fn write_tei_eaj_expansion_reports(
    summary: &TeiEajStructuralExpansionSummary,
    summary_json: &Path,
    report_md: &Path,
) -> Result<()> {
    write_parent(summary_json)?;
    write_parent(report_md)?;
    fs::write(summary_json, serde_json::to_string_pretty(summary)? + "\n")
        .with_context(|| format!("failed to write {}", summary_json.display()))?;
    fs::write(report_md, render_tei_eaj_expansion_markdown(summary))
        .with_context(|| format!("failed to write {}", report_md.display()))?;
    Ok(())
}

pub fn render_tei_eaj_expansion_markdown(summary: &TeiEajStructuralExpansionSummary) -> String {
    let mut out = String::new();
    out.push_str("# TEI-EAJ Structural Expansion\n\n");
    out.push_str("Measurement-only expansion over ABC's TEI-EAJ workset export. TEI-EAJ is comparison evidence; source inventory remains the authority for Aozora source constructs.\n\n");
    out.push_str("## Mapping\n\n");
    out.push_str(&format!(
        "- mapping: `{}` `{}`\n",
        summary.mapping.mapping_id, summary.mapping.mapping_version
    ));
    out.push_str(&format!(
        "- mapping hash: `{}`\n",
        summary.mapping.mapping_hash
    ));
    out.push_str(&format!(
        "- parser-IR schema hash: `{}`\n\n",
        summary.mapping.target_parser_ir_schema_hash
    ));

    out.push_str("## Workset\n\n");
    out.push_str(&format!(
        "- schema version: `{}`\n- TEI-EAJ files: {}\n- candidate work IDs: {}\n- compared files: {}\n- missing ABC counterpart work IDs: {}\n- no-work-ID files: {}\n",
        summary.workset.schema_version,
        summary.workset.tei_eaj_files,
        summary.workset.candidate_work_ids,
        summary.workset.compared_files,
        summary.workset.missing_abc_counterpart_work_ids,
        summary.workset.no_work_id_files
    ));
    if let Some(revision) = &summary.workset.tei_eaj_source_revision {
        out.push_str(&format!("- TEI-EAJ revision: `{}`\n", md_code(revision)));
    }
    if let Some(root) = &summary.workset.tei_eaj_source_root {
        out.push_str(&format!("- TEI-EAJ root: `{}`\n", md_code(root)));
    }
    out.push_str("- TEI p/note counts: ABC export values when present; otherwise counted from pinned TEI-EAJ XML files under the reported root.\n");

    out.push_str("\n## Totals\n\n");
    out.push_str(&format!(
        "- rows with AAT evidence: {}\n- parser-IR gap rows: {}\n- adapter gap rows: {}\n- source-attribution gap rows: {}\n- evidence gap rows: {}\n\n",
        summary.totals.rows_with_aat_evidence,
        summary.totals.parser_ir_gap_rows,
        summary.totals.adapter_gap_rows,
        summary.totals.source_attribution_gap_rows,
        summary.totals.evidence_gap_rows
    ));

    out.push_str("## Rows\n\n");
    out.push_str("| work_id | title | status | TEI p | ABC p | AAT inputs | parser-IR gap | evidence gap | adapter gap | source attribution gap |\n");
    out.push_str("|---|---|---|---:|---:|---:|---:|---:|---:|---:|\n");
    for row in &summary.rows {
        out.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} | {} | {} | {} | {} |\n",
            md_cell(row.tei.work_id.as_deref().unwrap_or("unknown")),
            md_cell(row.tei.title.as_deref().unwrap_or("unknown")),
            md_cell(&row.tei.comparison_status),
            display_optional_u64(row.tei.tei_eaj_p_count),
            display_optional_u64(row.tei.abc_p_count),
            row.aat_inputs.len(),
            row.classification.parser_ir_gap,
            row.classification.evidence_gap,
            row.classification.adapter_gap,
            row.classification.source_attribution_gap
        ));
    }

    out.push_str("\n## Classification Notes\n\n");
    for row in &summary.rows {
        if row.classification.notes.is_empty() {
            continue;
        }
        out.push_str(&format!(
            "### {} {}\n\n",
            row.tei.work_id.as_deref().unwrap_or("unknown"),
            row.tei.title.as_deref().unwrap_or("")
        ));
        out.push_str(&format!("- TEI-EAJ file: `{}`\n", row.tei.tei_eaj_file));
        if !row.classification.owners.is_empty() {
            out.push_str(&format!(
                "- owners: `{}`\n",
                md_code(&row.classification.owners.join(", "))
            ));
        }
        for note in &row.classification.notes {
            out.push_str(&format!("- {}\n", note));
        }
        for input in &row.aat_inputs {
            out.push_str(&format!(
                "- AAT `{}`: paragraphs={}, final attribution={}, conversion={}\n",
                md_code(&input.label),
                input.aat.paragraph_blocks,
                input.aat.final_source_attribution_candidate,
                input.conversion.success
            ));
        }
        out.push('\n');
    }

    out.push_str("## Interpretation\n\n");
    out.push_str("- `parser-IR gap` means adapter/AAT evidence exposes structure that current parser-IR does not explicitly represent.\n");
    out.push_str("- `adapter gap` means TEI-EAJ has multi-paragraph structure while at least one adapter AAT collapsed that row to zero or one paragraph.\n");
    out.push_str("- `evidence gap` means this repo does not yet have enough AAT/TEI structural evidence for that row; it is not a parser-IR design conclusion.\n");
    out
}

pub fn render_markdown(summary: &StructuralProbeSummary) -> String {
    let mut out = String::new();
    out.push_str("# AAT Structural Probe\n\n");
    out.push_str("Measurement-only probe for paragraph segmentation and final source-attribution representation in current parser-IR.\n\n");
    out.push_str("## Mapping\n\n");
    out.push_str(&format!(
        "- mapping: `{}` `{}`\n",
        summary.mapping.mapping_id, summary.mapping.mapping_version
    ));
    out.push_str(&format!(
        "- mapping hash: `{}`\n",
        summary.mapping.mapping_hash
    ));
    out.push_str(&format!(
        "- parser-IR schema hash: `{}`\n\n",
        summary.mapping.target_parser_ir_schema_hash
    ));

    out.push_str("## Totals\n\n");
    out.push_str(&format!(
        "- inputs: {}\n- conversions succeeded: {}\n- conversions failed: {}\n- paragraph gap inputs: {}\n- source-attribution gap inputs: {}\n- residual-free inputs: {}\n\n",
        summary.totals.inputs,
        summary.totals.conversions_succeeded,
        summary.totals.conversions_failed,
        summary.totals.paragraph_gap_inputs,
        summary.totals.source_attribution_gap_inputs,
        summary.totals.residual_free_inputs
    ));

    out.push_str("## Inputs\n\n");
    out.push_str("| label | adapter | paragraphs in AAT | final attribution candidate | parser-IR paragraph | parser-IR source attribution | residual free |\n");
    out.push_str("|---|---|---:|---:|---:|---:|---:|\n");
    for input in &summary.inputs {
        out.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} | {} |\n",
            md_cell(&input.label),
            md_cell(input.aat.adapter.as_deref().unwrap_or("unknown")),
            input.aat.paragraph_blocks,
            input.aat.final_source_attribution_candidate,
            input.parser_ir.paragraphs_represented,
            input.parser_ir.source_attribution_represented,
            input.verdict.residual_free
        ));
    }

    out.push_str("\n## Detail\n\n");
    for input in &summary.inputs {
        out.push_str(&format!("### {}\n\n", input.label));
        out.push_str(&format!("- path: `{}`\n", input.path));
        out.push_str(&format!(
            "- work: `{}`\n",
            input.aat.work_id.as_deref().unwrap_or("unknown")
        ));
        out.push_str(&format!(
            "- adapter: `{}` `{}`\n",
            input.aat.adapter.as_deref().unwrap_or("unknown"),
            input.aat.adapter_version.as_deref().unwrap_or("unknown")
        ));
        out.push_str(&format!(
            "- block kinds: `{}`\n",
            serde_json::to_string(&input.aat.block_kinds).unwrap_or_else(|_| "{}".to_owned())
        ));
        if let Some(text) = &input.aat.final_source_attribution_text {
            out.push_str(&format!("- final attribution text: `{}`\n", md_code(text)));
        }
        if let Some(text) = &input.aat.final_visible_text {
            out.push_str(&format!(
                "- final visible text: `{}`\n",
                md_code(&preview_text(text, 180))
            ));
        }
        if !input.aat.hints.is_empty() {
            out.push_str(&format!(
                "- AAT hints: `{}`\n",
                md_code(&input.aat.hints.join("; "))
            ));
        }
        out.push_str(&format!(
            "- parser-IR node kinds: `{}`\n",
            serde_json::to_string(&input.parser_ir.node_kinds).unwrap_or_else(|_| "{}".to_owned())
        ));
        out.push_str(&format!(
            "- parser-IR paragraphs[] rows: {}\n",
            input.parser_ir.paragraph_count
        ));
        if !input.divergence.paragraph_structural_records.is_empty() {
            out.push_str("- paragraph structural records:\n");
            for record in &input.divergence.paragraph_structural_records {
                out.push_str(&format!(
                    "  - `{}` count={} pointer=`{}`\n",
                    record.rule_id,
                    record.count,
                    record.aat_pointer.as_deref().unwrap_or("null")
                ));
            }
        }
        for note in &input.verdict.notes {
            out.push_str(&format!("- verdict note: {}\n", note));
        }
        if let Some(error) = &input.conversion.error {
            out.push_str(&format!("- conversion error: `{}`\n", md_code(error)));
        }
        out.push('\n');
    }

    out.push_str("## Interpretation\n\n");
    out.push_str("AAT paragraph segmentation is adapter evidence. Current parser-IR representation is a separate question. A false parser-IR paragraph/source-attribution flag means Level 3 structure is not yet represented by parser-IR, even when visible text is preserved.\n");
    out.push_str("\n## Next Expansion\n\n");
    out.push_str("Use ABC's machine-readable `../abc/docs/handoffs/tei-eaj-aozora-workset-export.json` for the TEI-EAJ missing-counterpart expansion. The expansion should materialize structural evidence for those rows; it should not turn conversion compatibility evidence into a parser-selection claim.\n");
    out
}

fn summarize_aat(aat: &Value) -> AatStructuralSummary {
    let mut block_kinds = BTreeMap::new();
    for block in aat
        .pointer("/blocks")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
    {
        collect_block_kinds(block, &mut block_kinds);
    }
    let block_count = block_kinds.values().sum();
    let paragraph_blocks = *block_kinds.get("paragraph").unwrap_or(&0);
    let final_block = final_non_empty_block(aat);
    let final_visible_text = final_block
        .map(visible_text)
        .map(|text| normalize_visible_text(&text))
        .filter(|text| !text.is_empty());
    let final_source_attribution_text = final_visible_text
        .as_deref()
        .and_then(final_attribution_text);
    let final_source_attribution_candidate = final_source_attribution_text.is_some();
    let mut hints = BTreeSet::new();
    collect_aat_hints(aat, &mut hints);
    let meta = &aat["meta"];

    AatStructuralSummary {
        work_id: aat["work_id"].as_str().map(ToOwned::to_owned),
        adapter: meta["adapter"].as_str().map(ToOwned::to_owned),
        adapter_version: meta["adapter_version"].as_str().map(ToOwned::to_owned),
        parse_complete: meta["parse_complete"].as_bool(),
        block_count,
        block_kinds,
        paragraph_blocks,
        final_block_kind: final_block
            .and_then(|block| block["kind"].as_str())
            .map(ToOwned::to_owned),
        final_visible_text,
        final_source_attribution_text,
        final_source_attribution_candidate,
        hints: hints.into_iter().collect(),
    }
}

fn mapping_summary(mapping: &MappingDocument) -> StructuralProbeMappingSummary {
    StructuralProbeMappingSummary {
        mapping_id: mapping.mapping_id.clone(),
        mapping_version: mapping.mapping_version.clone(),
        mapping_hash: mapping.document_hash.clone(),
        mapping_schema_hash: mapping.mapping_schema_hash.clone(),
        target_parser_ir_schema_id: mapping.target_parser_ir_schema_id.clone(),
        target_parser_ir_schema_hash: mapping.target_parser_ir_schema_hash.clone(),
    }
}

fn summarize_structural_input(
    converter: &PreparedConverter,
    input: StructuralProbeInput,
) -> Result<StructuralProbeInputSummary> {
    let aat = read_json(&input.path)
        .with_context(|| format!("failed to read AAT input {}", input.path.display()))?;
    let aat_summary = summarize_aat(&aat);
    let conversion = converter.convert(aat, ConversionOptions::default());
    let (conversion_summary, parser_ir, divergence) = match conversion {
        Ok(output) => (
            ConversionProbeSummary {
                success: true,
                error: None,
            },
            summarize_parser_ir(&output.parser_ir),
            summarize_divergence(&output.divergence_bundle),
        ),
        Err(error) => (
            ConversionProbeSummary {
                success: false,
                error: Some(error.to_string()),
            },
            ParserIrStructuralSummary::default(),
            DivergenceProbeSummary::default(),
        ),
    };
    let verdict = structural_verdict(&aat_summary, &conversion_summary, &parser_ir, &divergence);
    Ok(StructuralProbeInputSummary {
        label: input.label,
        path: input.path.display().to_string(),
        aat: aat_summary,
        conversion: conversion_summary,
        parser_ir,
        divergence,
        verdict,
    })
}

fn index_aat_dirs(
    dirs: &[StructuralProbeInput],
    work_id_aliases: &BTreeMap<String, String>,
) -> Result<BTreeMap<String, Vec<StructuralProbeInput>>> {
    let mut index: BTreeMap<String, Vec<StructuralProbeInput>> = BTreeMap::new();
    for dir in dirs {
        let entries = match fs::read_dir(&dir.path) {
            Ok(entries) => entries,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => continue,
            Err(error) => {
                return Err(error)
                    .with_context(|| format!("failed to read AAT dir {}", dir.path.display()));
            }
        };
        for entry in entries {
            let entry = entry.with_context(|| format!("failed to read {}", dir.path.display()))?;
            let path = entry.path();
            if !path.is_file() || path.extension().and_then(|ext| ext.to_str()) != Some("json") {
                continue;
            }
            let mut matched_work_ids = BTreeSet::new();
            for work_id in work_ids_from_aat_path(&path) {
                if let Some(target_work_id) = work_id_aliases.get(&work_id)
                    && matched_work_ids.insert(target_work_id.clone())
                {
                    index
                        .entry(target_work_id.clone())
                        .or_default()
                        .push(StructuralProbeInput {
                            label: format!("{}:{target_work_id}", dir.label),
                            path: path.clone(),
                        });
                }
            }
        }
    }
    Ok(index)
}

fn tei_eaj_work_id_aliases(workset: &TeiEajWorksetExport) -> BTreeMap<String, String> {
    let mut aliases: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    for row in &workset.files {
        let Some(work_id) = row.work_id.as_deref() else {
            continue;
        };
        aliases
            .entry(work_id.to_owned())
            .or_default()
            .insert(work_id.to_owned());
        for alias in tei_eaj_file_id_aliases(&row.tei_eaj_file, work_id) {
            aliases.entry(alias).or_default().insert(work_id.to_owned());
        }
    }

    aliases
        .into_iter()
        .filter_map(|(alias, work_ids)| {
            let mut work_ids = work_ids.into_iter();
            let work_id = work_ids.next()?;
            if work_ids.next().is_none() {
                Some((alias, work_id))
            } else {
                None
            }
        })
        .collect()
}

fn tei_eaj_file_id_aliases(tei_eaj_file: &str, work_id: &str) -> BTreeSet<String> {
    let Some(stem) = Path::new(tei_eaj_file)
        .file_stem()
        .and_then(|stem| stem.to_str())
    else {
        return BTreeSet::new();
    };
    let numeric_tokens: Vec<&str> = stem
        .split(|ch: char| !ch.is_ascii_digit())
        .filter(|part| !part.is_empty())
        .collect();
    if !numeric_tokens.iter().any(|token| *token == work_id) {
        return BTreeSet::new();
    }

    numeric_tokens
        .into_iter()
        // Avoid unsafe aliases from suffixes like 4244-1_tei.xml; ABC should
        // eventually export durable source aliases instead of relying on this.
        .filter(|token| *token != work_id && token.len() >= 3)
        .map(ToOwned::to_owned)
        .collect()
}

fn work_ids_from_aat_path(path: &Path) -> Vec<String> {
    let Some(stem) = path.file_stem().and_then(|stem| stem.to_str()) else {
        return Vec::new();
    };
    let id_part = stem.split_once('-').map_or(stem, |(id, _)| id);
    let mut candidates = BTreeSet::new();
    candidates.insert(id_part.to_owned());
    if let Some((_, suffix)) = id_part.rsplit_once('_') {
        candidates.insert(suffix.to_owned());
    }
    candidates.into_iter().collect()
}

impl TeiEajFileExport {
    fn to_summary(&self, tei_eaj_source_root: Option<&str>) -> TeiEajFileSummary {
        let xml_counts = if self.tei_eaj_p_count.is_none() || self.tei_eaj_note_count.is_none() {
            tei_counts_from_xml(tei_eaj_source_root, &self.tei_eaj_file)
        } else {
            None
        };
        TeiEajFileSummary {
            work_id: self.work_id.clone(),
            title: self.title.clone(),
            tei_eaj_file: self.tei_eaj_file.clone(),
            level: self.level.clone(),
            state: self.state.clone(),
            comparison_status: self.comparison_status.clone(),
            tei_eaj_p_count: self
                .tei_eaj_p_count
                .or_else(|| xml_counts.map(|counts| counts.0)),
            tei_eaj_note_count: self
                .tei_eaj_note_count
                .or_else(|| xml_counts.map(|counts| counts.1)),
            abc_p_count: self.abc_p_count,
            abc_note_count: self.abc_note_count,
            base_text_equal: self.base_text_equal,
        }
    }
}

fn tei_counts_from_xml(
    tei_eaj_source_root: Option<&str>,
    tei_eaj_file: &str,
) -> Option<(u64, u64)> {
    let root = tei_eaj_source_root?;
    let path = Path::new(root).join(tei_eaj_file);
    let xml = fs::read_to_string(path).ok()?;
    let body = tei_body_xml(&xml);
    Some((
        count_xml_element(body, "p"),
        count_xml_element(body, "note"),
    ))
}

fn tei_body_xml(xml: &str) -> &str {
    let body_regex =
        regex::Regex::new(r#"(?s)<(?:[A-Za-z_][A-Za-z0-9_.-]*:)?body(?:\s[^>]*)?>(.*?)</(?:[A-Za-z_][A-Za-z0-9_.-]*:)?body>"#)
            .expect("static TEI body regex");
    body_regex
        .captures(xml)
        .and_then(|captures| captures.get(1))
        .map_or(xml, |body| body.as_str())
}

fn count_xml_element(xml: &str, local_name: &str) -> u64 {
    let pattern = format!(
        r#"<(?:[A-Za-z_][A-Za-z0-9_.-]*:)?{}(?:[\s>/])"#,
        regex::escape(local_name)
    );
    regex::Regex::new(&pattern)
        .expect("static XML element count regex")
        .find_iter(xml)
        .count()
        .try_into()
        .unwrap_or(u64::MAX)
}

fn classify_tei_eaj_row(
    tei: &TeiEajFileSummary,
    aat_inputs: &[StructuralProbeInputSummary],
) -> TeiEajGapClassification {
    let mut classification = TeiEajGapClassification::default();
    if aat_inputs.is_empty() {
        classification.evidence_gap = true;
        classification
            .notes
            .push("no AAT evidence found for this TEI-EAJ work ID".to_owned());
    }
    if tei.tei_eaj_p_count.is_none() && tei.tei_eaj_note_count.is_none() {
        classification.evidence_gap = true;
        classification
            .notes
            .push("TEI-EAJ paragraph/note counts are absent in the ABC workset export".to_owned());
    }

    let paragraph_claim_present = aat_inputs
        .iter()
        .any(|input| input.conversion.success && input.aat.paragraph_blocks > 0);
    let paragraph_represented = aat_inputs.iter().any(|input| {
        input.conversion.success
            && input.aat.paragraph_blocks > 0
            && input.parser_ir.paragraphs_represented
    });
    let source_attribution_claim_present = aat_inputs
        .iter()
        .any(|input| input.conversion.success && input.aat.final_source_attribution_candidate);
    let source_attribution_represented = aat_inputs.iter().any(|input| {
        input.conversion.success
            && input.aat.final_source_attribution_candidate
            && input.parser_ir.source_attribution_represented
    });

    for input in aat_inputs {
        if !input.conversion.success {
            classification.evidence_gap = true;
            classification.notes.push(format!(
                "{} conversion failed; parser-IR representability is unmeasured for this adapter",
                input.label
            ));
            continue;
        }
        if tei.tei_eaj_p_count.is_some_and(|count| count > 1) && input.aat.paragraph_blocks <= 1 {
            classification.adapter_gap = true;
            classification.notes.push(format!(
                "{} has {} TEI-EAJ paragraph(s), but {} AAT paragraph block(s) in {}",
                tei.tei_eaj_file,
                tei.tei_eaj_p_count.unwrap_or(0),
                input.aat.paragraph_blocks,
                input.label
            ));
        }
    }

    if paragraph_claim_present && !paragraph_represented {
        classification.parser_ir_gap = true;
        classification.notes.push(
            "AAT evidence preserves paragraph blocks, but no successful parser-IR output contains paragraphs[] rows"
                .to_owned(),
        );
    }
    if source_attribution_claim_present && !source_attribution_represented {
        classification.parser_ir_gap = true;
        classification.source_attribution_gap = true;
        classification.notes.push(
            "AAT evidence exposes a final source-attribution candidate, but no successful parser-IR output contains a source-note node"
                .to_owned(),
        );
    }

    let mut owners = BTreeSet::new();
    if classification.parser_ir_gap {
        owners.insert("parser-ir".to_owned());
    }
    if classification.adapter_gap {
        owners.insert("adapter".to_owned());
    }
    if classification.evidence_gap {
        owners.insert("evidence".to_owned());
    }
    classification.owners = owners.into_iter().collect();
    classification
}

fn summarize_tei_eaj_totals(
    workset: &TeiEajWorksetSummary,
    rows: &[TeiEajStructuralRowSummary],
) -> TeiEajExpansionTotals {
    let mut totals = TeiEajExpansionTotals {
        tei_eaj_files: workset.tei_eaj_files,
        candidate_work_ids: workset.candidate_work_ids,
        compared_files: workset.compared_files,
        missing_abc_counterpart_work_ids: workset.missing_abc_counterpart_work_ids,
        no_work_id_files: workset.no_work_id_files,
        ..TeiEajExpansionTotals::default()
    };
    for row in rows {
        if !row.aat_inputs.is_empty() {
            totals.rows_with_aat_evidence += 1;
        }
        if row.classification.parser_ir_gap {
            totals.parser_ir_gap_rows += 1;
        }
        if row.classification.adapter_gap {
            totals.adapter_gap_rows += 1;
        }
        if row.classification.source_attribution_gap {
            totals.source_attribution_gap_rows += 1;
        }
        if row.classification.evidence_gap {
            totals.evidence_gap_rows += 1;
        }
    }
    totals
}

fn summarize_parser_ir(parser_ir: &Value) -> ParserIrStructuralSummary {
    let mut node_kinds = BTreeMap::new();
    for node in parser_ir
        .pointer("/nodes")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
    {
        if let Some(kind) = node["type"].as_str() {
            *node_kinds.entry(kind.to_owned()).or_insert(0) += 1;
        }
    }
    let node_count = node_kinds.values().sum();
    let paragraph_count = parser_ir
        .pointer("/paragraphs")
        .and_then(Value::as_array)
        .map_or(0, |paragraphs| paragraphs.len() as u64);
    let paragraphs_represented = paragraph_count > 0;
    let source_attribution_represented = parser_ir
        .pointer("/nodes")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .any(node_is_source_attribution);

    ParserIrStructuralSummary {
        node_count,
        node_kinds,
        paragraph_count,
        paragraphs_represented,
        source_attribution_represented,
    }
}

fn summarize_divergence(bundle: &Value) -> DivergenceProbeSummary {
    let mut categories = BTreeMap::new();
    if let Some(summary) = bundle.pointer("/summary").and_then(Value::as_object) {
        for (category, count) in summary {
            if let Some(count) = count.as_u64() {
                categories.insert(category.clone(), count);
            }
        }
    }

    let paragraph_structural_records = bundle
        .pointer("/records")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .filter(|record| {
            record["category"] == "STRUCTURAL"
                && record["aat_pointer"]
                    .as_str()
                    .is_some_and(|pointer| pointer.contains("paragraph"))
        })
        .map(|record| DivergenceRecordProbeSummary {
            rule_id: record["rule_id"].as_str().unwrap_or("").to_owned(),
            category: record["category"].as_str().unwrap_or("").to_owned(),
            aat_pointer: record["aat_pointer"].as_str().map(ToOwned::to_owned),
            parser_ir_pointer: record["parser_ir_pointer"].as_str().map(ToOwned::to_owned),
            count: record["count"].as_u64().unwrap_or(0),
            first_path: record["first_path"].as_str().map(ToOwned::to_owned),
            message: record["message"].as_str().unwrap_or("").to_owned(),
        })
        .collect();

    DivergenceProbeSummary {
        categories,
        paragraph_structural_records,
    }
}

fn structural_verdict(
    aat: &AatStructuralSummary,
    conversion: &ConversionProbeSummary,
    parser_ir: &ParserIrStructuralSummary,
    divergence: &DivergenceProbeSummary,
) -> StructuralProbeVerdict {
    let paragraphs_represented_in_parser_ir =
        aat.paragraph_blocks == 0 || parser_ir.paragraphs_represented;
    let source_attribution_represented_in_parser_ir =
        !aat.final_source_attribution_candidate || parser_ir.source_attribution_represented;

    let mut notes = Vec::new();
    if !conversion.success {
        notes
            .push("conversion failed; parser-IR representability could not be measured".to_owned());
    }
    if aat.paragraph_blocks > 0 && !parser_ir.paragraphs_represented {
        notes.push(format!(
            "AAT preserved {} paragraph block(s), but parser-IR has no explicit paragraphs[] row",
            aat.paragraph_blocks
        ));
    }
    if aat.final_source_attribution_candidate && !parser_ir.source_attribution_represented {
        notes.push("final parenthetical attribution is visible text, not a source-attribution/source-note parser-IR node".to_owned());
    }
    if !divergence.paragraph_structural_records.is_empty() {
        notes.push(
            "paragraph boundaries are present as measured STRUCTURAL divergence records".to_owned(),
        );
    }

    StructuralProbeVerdict {
        paragraphs_represented_in_parser_ir,
        source_attribution_represented_in_parser_ir,
        residual_free: conversion.success
            && paragraphs_represented_in_parser_ir
            && source_attribution_represented_in_parser_ir,
        notes,
    }
}

fn summarize_totals(inputs: &[StructuralProbeInputSummary]) -> StructuralProbeTotals {
    let mut totals = StructuralProbeTotals {
        inputs: inputs.len() as u64,
        ..StructuralProbeTotals::default()
    };
    for input in inputs {
        if input.conversion.success {
            totals.conversions_succeeded += 1;
        } else {
            totals.conversions_failed += 1;
        }
        if input.aat.paragraph_blocks > 0 && !input.parser_ir.paragraphs_represented {
            totals.paragraph_gap_inputs += 1;
        }
        if input.aat.final_source_attribution_candidate
            && !input.parser_ir.source_attribution_represented
        {
            totals.source_attribution_gap_inputs += 1;
        }
        if input.verdict.residual_free {
            totals.residual_free_inputs += 1;
        }
    }
    totals
}

fn collect_block_kinds(block: &Value, counts: &mut BTreeMap<String, u64>) {
    if let Some(kind) = block["kind"].as_str() {
        *counts.entry(kind.to_owned()).or_insert(0) += 1;
    }
    for child in block
        .get("children")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
    {
        collect_block_kinds(child, counts);
    }
}

fn final_non_empty_block(aat: &Value) -> Option<&Value> {
    aat.pointer("/blocks")
        .and_then(Value::as_array)?
        .iter()
        .rev()
        .find(|block| !normalize_visible_text(&visible_text(block)).is_empty())
}

fn visible_text(value: &Value) -> String {
    if let Some(array) = value.as_array() {
        return array.iter().map(visible_text).collect();
    }
    let Some(object) = value.as_object() else {
        return String::new();
    };
    match object.get("kind").and_then(Value::as_str) {
        Some("text") => object
            .get("value")
            .and_then(Value::as_str)
            .unwrap_or("")
            .to_owned(),
        Some("ruby") => object
            .get("base")
            .and_then(Value::as_str)
            .map(ToOwned::to_owned)
            .unwrap_or_else(|| visible_text(object.get("base_content").unwrap_or(&Value::Null))),
        Some("gaiji") => object
            .get("resolved")
            .and_then(Value::as_str)
            .or_else(|| object.get("description").and_then(Value::as_str))
            .unwrap_or("")
            .to_owned(),
        Some("warigaki") => format!(
            "{}{}",
            visible_text(object.get("upper").unwrap_or(&Value::Null)),
            visible_text(object.get("lower").unwrap_or(&Value::Null))
        ),
        Some("figure") => object
            .get("alt")
            .and_then(Value::as_str)
            .unwrap_or("")
            .to_owned(),
        _ => {
            let mut text = String::new();
            for key in ["content", "children", "caption"] {
                text.push_str(&visible_text(object.get(key).unwrap_or(&Value::Null)));
            }
            text
        }
    }
}

fn normalize_visible_text(text: &str) -> String {
    text.chars()
        .filter(|ch| !ch.is_whitespace() && *ch != '\u{3000}')
        .collect()
}

fn final_attribution_text(text: &str) -> Option<String> {
    if !text.ends_with('）') {
        return None;
    }
    let start = text
        .char_indices()
        .rev()
        .find_map(|(index, ch)| if ch == '（' { Some(index) } else { None })?;
    let candidate = &text[start..];
    if candidate.contains("から") {
        Some(candidate.to_owned())
    } else {
        None
    }
}

fn collect_aat_hints(value: &Value, hints: &mut BTreeSet<String>) {
    match value {
        Value::Array(items) => {
            for item in items {
                collect_aat_hints(item, hints);
            }
        }
        Value::Object(object) => {
            if object.get("kind").and_then(Value::as_str) == Some("style")
                && let Some(style_type) = object.get("style_type").and_then(Value::as_str)
            {
                hints.insert(format!("style:{style_type}"));
            }
            if let Some(unmapped) = object.get("x-aozora2html-unmapped").and_then(Value::as_str) {
                hints.insert(format!("x-aozora2html-unmapped:{unmapped}"));
            }
            if let Some(message) = object.get("message").and_then(Value::as_str)
                && object.contains_key("path")
            {
                hints.insert(format!("warning:{message}"));
            }
            for child in object.values() {
                collect_aat_hints(child, hints);
            }
        }
        _ => {}
    }
}

fn node_is_source_attribution(node: &Value) -> bool {
    match node["type"].as_str().unwrap_or("") {
        "source-attribution" | "source_attribution" | "source-note" | "source_note" => true,
        "editor-note" => node
            .pointer("/note/category")
            .and_then(Value::as_str)
            .is_some_and(|category| category.contains("source")),
        _ => false,
    }
}

fn write_parent(path: &Path) -> Result<()> {
    if let Some(parent) = path.parent()
        && !parent.as_os_str().is_empty()
    {
        fs::create_dir_all(parent)
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

fn display_optional_u64(value: Option<u64>) -> String {
    value.map_or_else(|| "unknown".to_owned(), |value| value.to_string())
}

fn preview_text(value: &str, max_chars: usize) -> String {
    let mut preview = String::new();
    for (index, ch) in value.chars().enumerate() {
        if index == max_chars {
            preview.push_str("...");
            return preview;
        }
        preview.push(ch);
    }
    preview
}
