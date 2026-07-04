use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result, bail};
use serde::Serialize;
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

#[derive(Debug, Clone, Serialize)]
pub struct StructuralProbeSummary {
    pub mapping: StructuralProbeMappingSummary,
    pub totals: StructuralProbeTotals,
    pub inputs: Vec<StructuralProbeInputSummary>,
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

#[derive(Debug, Clone, Default, Serialize)]
pub struct StructuralProbeTotals {
    pub inputs: u64,
    pub conversions_succeeded: u64,
    pub conversions_failed: u64,
    pub paragraph_gap_inputs: u64,
    pub source_attribution_gap_inputs: u64,
    pub residual_free_inputs: u64,
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
        let verdict =
            structural_verdict(&aat_summary, &conversion_summary, &parser_ir, &divergence);
        inputs.push(StructuralProbeInputSummary {
            label: input.label,
            path: input.path.display().to_string(),
            aat: aat_summary,
            conversion: conversion_summary,
            parser_ir,
            divergence,
            verdict,
        });
    }

    let totals = summarize_totals(&inputs);
    Ok(StructuralProbeSummary {
        mapping: mapping_summary,
        totals,
        inputs,
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

pub fn render_markdown(summary: &StructuralProbeSummary) -> String {
    let mut out = String::new();
    out.push_str("# Melos Structural Probe\n\n");
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
        if let Some(text) = &input.aat.final_visible_text {
            out.push_str(&format!("- final visible text: `{}`\n", md_code(text)));
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
    let final_source_attribution_candidate = final_visible_text
        .as_deref()
        .is_some_and(is_source_attribution_candidate);
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
        final_source_attribution_candidate,
        hints: hints.into_iter().collect(),
    }
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
    let paragraphs_represented = node_kinds
        .keys()
        .any(|kind| matches!(kind.as_str(), "paragraph" | "paragraph-boundary"));
    let source_attribution_represented = parser_ir
        .pointer("/nodes")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .any(node_is_source_attribution);

    ParserIrStructuralSummary {
        node_count,
        node_kinds,
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
            "AAT preserved {} paragraph block(s), but parser-IR has no explicit paragraph node",
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

fn is_source_attribution_candidate(text: &str) -> bool {
    text.starts_with('（') && text.ends_with('）') && text.contains("から")
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
