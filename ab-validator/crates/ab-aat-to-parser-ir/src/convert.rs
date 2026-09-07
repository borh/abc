use std::{collections::BTreeSet, sync::Arc};

use anyhow::{Context, Result, bail};
use serde_json::{Value, json};

use crate::{
    divergence::{AatMeta, DivergenceRecorder},
    mapping::{MappingDocument, MappingIndex},
    ortho_annotations::OrthoAnnotationsBundle,
    schema::{SchemaSet, SchemaValidators, validate_compiled, validation_errors},
};

#[derive(Debug, Clone)]
pub struct ConversionRequest {
    pub aat: Value,
    pub mapping: MappingDocument,
    pub schemas: SchemaSet,
    pub options: ConversionOptions,
}

#[derive(Debug, Clone)]
pub struct ConversionOptions {
    pub validate_input_aat: bool,
    pub validate_output_parser_ir: bool,
    pub orthographic_annotations: Option<OrthoAnnotationsBundle>,
    pub work_content_hash: Option<String>,
}

impl Default for ConversionOptions {
    fn default() -> Self {
        Self {
            validate_input_aat: true,
            validate_output_parser_ir: true,
            orthographic_annotations: None,
            work_content_hash: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConversionOutput {
    pub parser_ir: Value,
    pub divergence_bundle: Value,
    pub emitted_rule_ids: BTreeSet<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserIrValidation {
    Valid,
    Invalid { errors: Vec<String> },
}

#[derive(Debug, Clone)]
pub enum QualificationConversion {
    Valid(ConversionOutput),
    Invalid {
        parser_ir: Value,
        errors: Vec<String>,
    },
}

#[derive(Debug, Clone)]
pub struct PreparedConverter {
    mapping: MappingDocument,
    schemas: SchemaSet,
    validators: Arc<SchemaValidators>,
    index: Arc<MappingIndex>,
}

impl PreparedConverter {
    pub fn new(mapping: MappingDocument, schemas: SchemaSet) -> Result<Self> {
        let index = Arc::new(mapping.preflight(&schemas)?);
        let validators = Arc::new(SchemaValidators::compile(&schemas)?);
        Ok(Self {
            mapping,
            schemas,
            validators,
            index,
        })
    }

    pub fn convert(&self, aat: Value, options: ConversionOptions) -> Result<ConversionOutput> {
        match self.convert_for_qualification(aat, options)? {
            QualificationConversion::Valid(output) => Ok(output),
            QualificationConversion::Invalid { errors, .. } => {
                Err(anyhow::anyhow!(errors.into_iter().next().expect(
                    "invalid Parser-IR validation has at least one error"
                )))
            }
        }
    }

    pub fn convert_for_qualification(
        &self,
        aat: Value,
        options: ConversionOptions,
    ) -> Result<QualificationConversion> {
        convert_preflighted_for_qualification(
            aat,
            &self.mapping,
            &self.schemas,
            &self.validators,
            Arc::clone(&self.index),
            options,
        )
    }
}

pub fn convert(request: ConversionRequest) -> Result<ConversionOutput> {
    PreparedConverter::new(request.mapping, request.schemas)?.convert(request.aat, request.options)
}

fn convert_preflighted_for_qualification(
    mut aat: Value,
    mapping: &MappingDocument,
    schemas: &SchemaSet,
    validators: &SchemaValidators,
    index: Arc<MappingIndex>,
    options: ConversionOptions,
) -> Result<QualificationConversion> {
    if options.validate_input_aat {
        validate_compiled(&validators.aat, &aat, "AAT")?;
    }

    let mut recorder = DivergenceRecorder::new(index);
    let mut nodes = Vec::new();
    let mut paragraphs = Vec::new();
    let mut layout_blocks = Vec::new();
    let mut synthetic_warnings = Vec::new();
    let mut offset = 0_u64;

    {
        let mut block_outputs = BlockOutputs {
            nodes: &mut nodes,
            paragraphs: &mut paragraphs,
            layout_blocks: &mut layout_blocks,
            synthetic_warnings: &mut synthetic_warnings,
        };

        // AAT `version` selects the source-attribution heuristic: it only ever
        // fires for v1 documents. v2 documents carry an explicit `source_note`
        // block (see the `source_note` match arm below) and must never trigger
        // the heuristic, even if a final paragraph happens to look like an
        // attribution string.
        let aat_version = aat.get("version").and_then(Value::as_u64).unwrap_or(1);
        let heuristic_enabled = aat_version == 1;

        let blocks = aat
            .pointer("/blocks")
            .and_then(Value::as_array)
            .map(Vec::as_slice)
            .unwrap_or(&[]);
        let top_level_block_count = blocks.len();
        for (block_index, block) in blocks.iter().enumerate() {
            offset = map_block(
                block,
                &mut block_outputs,
                &mut recorder,
                offset,
                &format!("blocks[{block_index}]"),
                None,
                block_index + 1 == top_level_block_count,
                heuristic_enabled,
            )?;
        }
    }

    let source = map_source(&aat, &options, &mut recorder)?;
    recorder.record("INVENTION", None, Some("schema_id/schema_hash"), None, None)?;
    let (mut warnings, errors) = map_diagnostics(&aat, &mut recorder)?;
    warnings.append(&mut synthetic_warnings);

    let orthographic_annotations = options.orthographic_annotations;
    if let Some(orthographic_annotations) = orthographic_annotations.as_ref() {
        ensure_schema_declares_orthographic_annotations(&schemas.parser_ir_schema)?;
        orthographic_annotations.validate_against_aat(&aat)?;
    }

    let mut parser_ir = json!({
        "schema_id": mapping.target_parser_ir_schema_id,
        "schema_hash": mapping.target_parser_ir_schema_hash,
        "derived_from": derived_from(&aat, mapping)?,
        "source": source,
        "interpretation_problems": interpretation_problems(&aat, &nodes)?,
        "interpretation_facts": aat.pointer("/meta/interpretation_facts").cloned().unwrap_or_else(|| json!([])),
        "nodes": nodes,
        "paragraphs": paragraphs,
        "layout_blocks": layout_blocks,
        "warnings": warnings,
        "errors": errors,
    });

    let parser_ir_object = parser_ir.as_object_mut().expect("parser_ir is an object");
    if let Some(orthographic_annotations) = orthographic_annotations {
        parser_ir_object.insert(
            "orthographic_annotations".to_owned(),
            serde_json::to_value(&orthographic_annotations)?,
        );
    }

    let validation = if options.validate_output_parser_ir {
        let errors = validation_errors(&validators.parser_ir, &parser_ir, "parser-IR");
        if errors.is_empty() {
            ParserIrValidation::Valid
        } else {
            ParserIrValidation::Invalid { errors }
        }
    } else {
        ParserIrValidation::Valid
    };
    if let ParserIrValidation::Invalid { errors } = validation {
        return Ok(QualificationConversion::Invalid { parser_ir, errors });
    }

    let emitted_rule_ids = recorder.emitted_rule_ids();
    let divergence_bundle = recorder.bundle(aat_meta(&mut aat), validators, mapping)?;
    Ok(QualificationConversion::Valid(ConversionOutput {
        parser_ir,
        divergence_bundle,
        emitted_rule_ids,
    }))
}

fn ensure_schema_declares_orthographic_annotations(schema: &Value) -> Result<()> {
    if schema
        .pointer("/properties/orthographic_annotations")
        .is_some()
    {
        Ok(())
    } else {
        bail!(
            "loaded parser-IR schema does not declare orthographic_annotations; use an updated ABC schema/mapping bundle before passing --ortho-annotations"
        )
    }
}

struct BlockOutputs<'a> {
    nodes: &'a mut Vec<Value>,
    paragraphs: &'a mut Vec<Value>,
    layout_blocks: &'a mut Vec<Value>,
    synthetic_warnings: &'a mut Vec<Value>,
}

#[allow(clippy::too_many_arguments)]
fn map_block(
    block: &Value,
    outputs: &mut BlockOutputs<'_>,
    recorder: &mut DivergenceRecorder,
    offset: u64,
    path: &str,
    inherited_layout: Option<Value>,
    is_final_top_level: bool,
    heuristic_enabled: bool,
) -> Result<u64> {
    let node_start = outputs.nodes.len();
    let paragraph_start = outputs.paragraphs.len();
    let end = map_block_content(
        block,
        outputs,
        recorder,
        offset,
        path,
        inherited_layout,
        is_final_top_level,
        heuristic_enabled,
    )?;
    let kind = block["kind"].as_str().unwrap_or("");
    if matches!(kind, "heading" | "source_note")
        || (kind == "paragraph"
            && outputs.nodes.len() == node_start + 1
            && matches!(
                outputs.nodes[node_start]["type"].as_str(),
                Some("page-break" | "source-note")
            ))
    {
        attach_single_source_span(&mut outputs.nodes[node_start..], block.get("span"))?;
    }
    if matches!(kind, "paragraph" | "source_note") {
        attach_single_source_span(
            &mut outputs.paragraphs[paragraph_start..],
            block.get("span"),
        )?;
    }
    Ok(end)
}

#[allow(clippy::too_many_arguments)]
fn map_block_content(
    block: &Value,
    outputs: &mut BlockOutputs<'_>,
    recorder: &mut DivergenceRecorder,
    offset: u64,
    path: &str,
    inherited_layout: Option<Value>,
    is_final_top_level: bool,
    heuristic_enabled: bool,
) -> Result<u64> {
    let kind = block["kind"].as_str().unwrap_or("unknown");
    let structural_pointer = format!("{path}.{kind}");
    let mut current = offset;
    match kind {
        "paragraph" => {
            if is_source_derived_page_break(block) {
                let span = map_node_span(block.get("span"), current, current, recorder, path)?;
                outputs.nodes.push(json!({
                    "type": "page-break",
                    "span": span,
                    "marker": "page",
                    "page_number": null,
                }));
                return Ok(current);
            }

            let layout_wrapper = paragraph_layout_wrapper(block);
            let layout = layout_wrapper
                .and_then(paragraph_layout_from_style)
                .or(inherited_layout);
            let paragraph_content = layout_wrapper
                .and_then(|wrapper| wrapper.get("content"))
                .or_else(|| block.get("content"));
            let paragraph_content_path = if layout_wrapper.is_some() {
                format!("{path}.content[0].content")
            } else {
                format!("{path}.content")
            };
            let paragraph_id = format!("p{:06}", outputs.paragraphs.len());
            let node_start = outputs.nodes.len();
            let source_note_text = if is_final_top_level && heuristic_enabled {
                source_attribution_text(paragraph_content)?
            } else {
                None
            };
            let role;
            let classification;
            if source_note_text.is_some() {
                let text = visible_content_text(
                    paragraph_content,
                    recorder,
                    &paragraph_content_path,
                    Some("source-note.text"),
                )?;
                let end = current + utf8_len(&text);
                let span = map_node_span(block.get("span"), current, end, recorder, path)?;
                outputs.nodes.push(json!({
                    "type": "source-note",
                    "span": span,
                    "text": text,
                    "note_type": "source-attribution",
                    "placement": "back",
                    "classification": "heuristic",
                    "source_pointer": path,
                }));
                current = end;
                role = "source-note";
                classification = "heuristic";
            } else {
                current = map_inline_content(
                    paragraph_content,
                    outputs.nodes,
                    recorder,
                    outputs.synthetic_warnings,
                    current,
                    &paragraph_content_path,
                )?;
                role = "body";
                classification = "direct";
            }
            let node_end = outputs.nodes.len();
            if role == "body" && node_start == node_end {
                return Ok(current);
            }
            let (span, span_source) =
                paragraph_span(outputs.nodes, node_start, node_end, offset, current)?;
            let mut paragraph = json!({
                "id": paragraph_id,
                "span": span,
                "span_source": span_source,
                "node_range": {
                    "start": node_start,
                    "end": node_end,
                },
                "role": role,
                "source_pointer": path,
                "classification": classification,
            });
            if let Some(layout) = layout {
                paragraph
                    .as_object_mut()
                    .expect("paragraph JSON object")
                    .insert("layout".to_owned(), layout);
            }
            outputs.paragraphs.push(paragraph);
        }
        "heading" => {
            recorder.record(
                "STRUCTURAL",
                Some(structural_pointer.as_str()),
                None,
                None,
                None,
            )?;
            let level_pointer = format!("{path}.heading.level");
            recorder.record(
                "AMBIGUITY",
                Some(level_pointer.as_str()),
                Some("heading.level"),
                block.get("level").cloned(),
                block.get("level").cloned(),
            )?;
            let style_pointer = format!("{path}.heading.style");
            recorder.record(
                "LOSS",
                Some(style_pointer.as_str()),
                None,
                block.get("style").cloned(),
                None,
            )?;
            let text = plain_visible_content_text(block.get("content"))?;
            let inline_children = inline_children_nodes(
                block.get("content"),
                recorder,
                outputs.synthetic_warnings,
                current,
                &format!("{path}.heading.content"),
                0,
            )?;
            let end = current + utf8_len(&text);
            let span = map_node_span(block.get("span"), current, end, recorder, path)?;
            let mut heading = json!({
                "type": "heading",
                "span": span,
                "text": text,
                "inline_children": inline_children,
                "level": block.get("level").and_then(Value::as_u64).unwrap_or(1),
            });
            if let Some(indent) = block.get("indent").or_else(|| block.get("x-indent")) {
                heading["indent"] = indent.clone();
            }
            outputs.nodes.push(heading);
            current = end;
        }
        "jisage_block" => {
            let children: Vec<&Value> =
                block["children"].as_array().into_iter().flatten().collect();
            if children
                .iter()
                .all(|child| child.get("kind").and_then(Value::as_str) == Some("paragraph"))
            {
                let layout = paragraph_layout_from_jisage_block(block);
                let paragraph_start = outputs.paragraphs.len();
                let block_style = block.get("block_style");
                let enclosing_scope = block_style.is_some()
                    || children
                        .iter()
                        .any(|child| paragraph_layout_wrapper(child).is_some());
                for (index, child) in children.into_iter().enumerate() {
                    current = map_block(
                        child,
                        outputs,
                        recorder,
                        current,
                        &format!("{path}.children[{index}]"),
                        (!enclosing_scope).then(|| layout.clone()),
                        false,
                        heuristic_enabled,
                    )?;
                }
                if enclosing_scope {
                    let mut scope = json!({
                        "paragraph_range": {"start": paragraph_start, "end": outputs.paragraphs.len()},
                        "indent": layout["indent"], "source_pointer": path
                    });
                    if let Some(style) = block_style {
                        for property in ["direction", "align", "border"] {
                            if let Some(value) = style.get(property) {
                                scope[property] = value.clone();
                            }
                        }
                    }
                    outputs.layout_blocks.push(scope);
                }
            } else {
                recorder.record(
                    "STRUCTURAL",
                    Some(structural_pointer.as_str()),
                    None,
                    None,
                    None,
                )?;
                recorder.record(
                    "INVENTION",
                    Some(structural_pointer.as_str()),
                    Some("indentation"),
                    None,
                    Some(json!(1)),
                )?;
                let span = map_node_span(block.get("span"), current, current, recorder, path)?;
                outputs.nodes.push(json!({
                    "type": "indentation",
                    "span": span,
                    "depth": 1,
                    "text": null,
                }));
                for (index, child) in children.into_iter().enumerate() {
                    current = map_block(
                        child,
                        outputs,
                        recorder,
                        current,
                        &format!("{path}.children[{index}]"),
                        None,
                        false,
                        heuristic_enabled,
                    )?;
                }
            }
        }
        "jizume_block" => {
            let children: Vec<&Value> =
                block["children"].as_array().into_iter().flatten().collect();
            if children
                .iter()
                .all(|child| child.get("kind").and_then(Value::as_str) == Some("paragraph"))
            {
                let layout = paragraph_layout_from_jizume_block(block);
                for (index, child) in children.into_iter().enumerate() {
                    current = map_block(
                        child,
                        outputs,
                        recorder,
                        current,
                        &format!("{path}.children[{index}]"),
                        Some(layout.clone()),
                        false,
                        heuristic_enabled,
                    )?;
                }
            } else {
                recorder.record(
                    "STRUCTURAL",
                    Some(structural_pointer.as_str()),
                    None,
                    None,
                    None,
                )?;
                recorder.record(
                    "INVENTION",
                    Some(structural_pointer.as_str()),
                    Some("indentation"),
                    None,
                    Some(json!(1)),
                )?;
                let span = map_node_span(block.get("span"), current, current, recorder, path)?;
                outputs.nodes.push(json!({
                    "type": "indentation",
                    "span": span,
                    "depth": 1,
                    "text": null,
                }));
                for (index, child) in children.into_iter().enumerate() {
                    current = map_block(
                        child,
                        outputs,
                        recorder,
                        current,
                        &format!("{path}.children[{index}]"),
                        None,
                        false,
                        heuristic_enabled,
                    )?;
                }
            }
        }
        "source_note" => {
            let placement = block
                .get("placement")
                .and_then(Value::as_str)
                .unwrap_or("unknown");
            let region_class = block
                .get("region_class")
                .and_then(Value::as_str)
                .unwrap_or("");
            let region_class_pointer = format!("{structural_pointer}.region_class");
            let note_type = match region_class {
                "terminal_provenance" => "source-attribution",
                "colophon_metadata" => "transcriber-note",
                other => bail!("unmapped source_note region_class {other:?} at {path}"),
            };
            recorder.record(
                "STRUCTURAL",
                Some(structural_pointer.as_str()),
                Some("source-note"),
                None,
                None,
            )?;
            recorder.record(
                "LOSS",
                Some(region_class_pointer.as_str()),
                Some("(source-note.note_type)"),
                Some(json!(region_class)),
                Some(json!(note_type)),
            )?;
            let text = visible_content_text(
                block.get("content"),
                recorder,
                &format!("{path}.content"),
                Some("source-note.text"),
            )?;
            let node_start = outputs.nodes.len();
            let end = current + utf8_len(&text);
            let span = map_node_span(block.get("span"), current, end, recorder, path)?;
            outputs.nodes.push(json!({
                "type": "source-note",
                "span": span,
                "text": text,
                "note_type": note_type,
                "placement": placement,
                "classification": "direct",
                "source_pointer": path,
            }));
            current = end;
            let node_end = outputs.nodes.len();
            let paragraph_id = format!("p{:06}", outputs.paragraphs.len());
            let (note_paragraph_span, span_source) =
                paragraph_span(outputs.nodes, node_start, node_end, offset, current)?;
            outputs.paragraphs.push(json!({
                "id": paragraph_id,
                "span": note_paragraph_span,
                "span_source": span_source,
                "node_range": {
                    "start": node_start,
                    "end": node_end,
                },
                "role": "source-note",
                "source_pointer": path,
                "classification": "direct",
            }));
            return Ok(current);
        }
        "keigakomi_block" | "yokogumi_block" => {
            recorder.record(
                "STRUCTURAL",
                Some(structural_pointer.as_str()),
                None,
                None,
                None,
            )?;
            recorder.record(
                "UNSUPPORTED",
                Some(structural_pointer.as_str()),
                None,
                None,
                None,
            )?;
            for (index, child) in block["children"]
                .as_array()
                .into_iter()
                .flatten()
                .enumerate()
            {
                current = map_block(
                    child,
                    outputs,
                    recorder,
                    current,
                    &format!("{path}.children[{index}]"),
                    None,
                    false,
                    heuristic_enabled,
                )?;
            }
        }
        "quote_block" | "caption_block" => {
            recorder.record_if_measured(
                "STRUCTURAL",
                Some(structural_pointer.as_str()),
                None,
                None,
                None,
            );
            for (index, child) in block["children"]
                .as_array()
                .into_iter()
                .flatten()
                .enumerate()
            {
                current = map_block(
                    child,
                    outputs,
                    recorder,
                    current,
                    &format!("{path}.children[{index}]"),
                    None,
                    false,
                    heuristic_enabled,
                )?;
            }
        }
        other => bail!("unsupported block kind: {other}"),
    }
    Ok(current)
}

fn is_source_derived_page_break(block: &Value) -> bool {
    block.get("x-break-kind").and_then(Value::as_str) == Some("page")
        && block.get("x-provenance").and_then(Value::as_str) == Some("source-derived")
        && block
            .get("content")
            .and_then(Value::as_array)
            .is_some_and(Vec::is_empty)
}

fn paragraph_layout_wrapper(block: &Value) -> Option<&Value> {
    let content = block.get("content")?.as_array()?;
    if content.len() != 1 {
        return None;
    }
    let wrapper = &content[0];
    if wrapper.get("kind").and_then(Value::as_str) == Some("style")
        && paragraph_layout_from_style(wrapper).is_some()
    {
        Some(wrapper)
    } else {
        None
    }
}

/// Read a layout field by its v2 typed name first, falling back to the v1
/// `x-`-prefixed name. v1 documents never carry the typed name (the v1 AAT
/// schema doesn't declare it), so the fallback makes this one code path
/// bit-identical for v1 input while also serving v2 documents that use the
/// typed field.
fn typed_or_x<'a>(node: &'a Value, typed: &str, x_prefixed: &str) -> Option<&'a Value> {
    node.get(typed).or_else(|| node.get(x_prefixed))
}

fn paragraph_layout_from_style(node: &Value) -> Option<Value> {
    match node.get("style_type").and_then(Value::as_str)? {
        "burasage" => Some(json!({
            "kind": "burasage",
            "source": "aat-style",
            "first_line_indent": typed_or_x(node, "indent_first", "x-indent-first")?.as_u64()?,
            "continuation_indent": typed_or_x(node, "indent_rest", "x-indent-rest")?.as_u64()?,
        })),
        "chitsuki" => {
            let align = typed_or_x(node, "align", "x-align")
                .and_then(Value::as_str)
                .unwrap_or("right");
            if align != "right" {
                return None;
            }
            Some(json!({
                "kind": "chitsuki",
                "source": "aat-style",
                "align": align,
                "offset_from_end": typed_or_x(node, "offset_from_end", "x-offset")?.as_u64()?,
            }))
        }
        "jisage" => Some(json!({
            "kind": "jisage",
            "source": "aat-style",
            "indent": typed_or_x(node, "indent", "x-indent")?.as_u64()?,
        })),
        "jizume" => Some(json!({
            "kind": "jizume",
            "source": "aat-style",
            "width": typed_or_x(node, "width", "x-width")?.as_u64()?,
        })),
        "line-jisage" | "jisage_line" => Some(json!({
            "kind": "line-jisage",
            "source": "aat-style",
            "indent": typed_or_x(node, "indent", "x-indent")?.as_u64()?,
        })),
        _ => None,
    }
}

fn paragraph_layout_from_jisage_block(block: &Value) -> Value {
    json!({
        "kind": "jisage",
        "source": "aat-block",
        "indent": typed_or_x(block, "indent", "x-indent")
            .and_then(Value::as_u64)
            .unwrap_or(1),
    })
}

fn paragraph_layout_from_jizume_block(block: &Value) -> Value {
    let width = block
        .get("width")
        .or_else(|| block.get("x-width"))
        .and_then(Value::as_u64)
        .unwrap_or(1);
    json!({ "kind": "jizume", "source": "aat-block", "width": width })
}

fn paragraph_span(
    nodes: &[Value],
    node_start: usize,
    node_end: usize,
    fallback_start: u64,
    fallback_end: u64,
) -> Result<(Value, &'static str)> {
    if node_start < node_end
        && let (Some(first), Some(last)) = (nodes.get(node_start), nodes.get(node_end - 1))
    {
        let start = first
            .pointer("/span/start")
            .and_then(Value::as_u64)
            .unwrap_or(fallback_start);
        let end = last
            .pointer("/span/end")
            .and_then(Value::as_u64)
            .unwrap_or(fallback_end);
        return Ok((synthetic_span(start, end), "derived"));
    }
    Ok((synthetic_span(fallback_start, fallback_end), "synthesized"))
}

fn source_attribution_text(content: Option<&Value>) -> Result<Option<String>> {
    let text = plain_visible_content_text(content)?;
    if is_source_attribution_text(&text) {
        Ok(Some(text))
    } else {
        Ok(None)
    }
}

fn is_source_attribution_text(text: &str) -> bool {
    let trimmed = text.trim();
    trimmed.starts_with('（')
        && trimmed.ends_with('）')
        && (trimmed.contains("から。")
            || trimmed.contains("から）")
            || trimmed.contains("から。）"))
}

fn plain_visible_content_text(content: Option<&Value>) -> Result<String> {
    let mut text = String::new();
    append_plain_visible_content_text(content, &mut text)?;
    Ok(text)
}

fn append_plain_visible_content_text(content: Option<&Value>, out: &mut String) -> Result<()> {
    for child in content.and_then(Value::as_array).into_iter().flatten() {
        append_plain_visible_inline_text(child, out)?;
    }
    Ok(())
}

fn plain_visible_inline_text(node: &Value) -> Result<String> {
    let mut text = String::new();
    append_plain_visible_inline_text(node, &mut text)?;
    Ok(text)
}

fn ruby_component_text(node: &Value, value_key: &str, content_key: &str) -> Result<String> {
    match node.get(content_key) {
        Some(Value::Array(children)) if !children.is_empty() => {
            plain_visible_content_text(node.get(content_key))
        }
        _ => Ok(node[value_key].as_str().unwrap_or("").to_owned()),
    }
}

fn gaiji_payload(node: &Value) -> Value {
    json!({
        "raw_marker": node["description"].as_str().unwrap_or(""),
        "reference": node.get("jis_code").cloned().unwrap_or(Value::Null),
        "unicode": node.get("resolved").cloned().unwrap_or(Value::Null),
        "ivs": null,
        "image_or_glyph_fallback": null,
        "resolved": node.get("resolved").is_some_and(|value| !value.is_null())
    })
}

fn interpretation_problems(aat: &Value, nodes: &[Value]) -> Result<Vec<Value>> {
    let mut problems = Vec::new();
    let mut pending = vec![aat];
    while let Some(node) = pending.pop() {
        if let Some(problem) = node.get("interpretation_problem") {
            let mut problem = problem.clone();
            problem["raw"] = node["source"].clone();
            problem["source_span"] = source_span(node.get("span"))?
                .context("source interpretation problem requires its exact source span")?;
            problems.push(problem);
        }
        for key in [
            "blocks",
            "children",
            "content",
            "upper",
            "lower",
            "base_content",
            "reading_content",
        ] {
            if let Some(children) = node.get(key).and_then(Value::as_array) {
                pending.extend(children.iter().rev());
            }
        }
    }
    let mut pending = nodes.iter().collect::<Vec<_>>();
    while let Some(node) = pending.pop() {
        if node["type"] == "editor-note"
            && node["note"]["category"] == "variant"
            && node["note"]["resolution"] == "unresolved"
        {
            problems.push(
                json!({"kind":"unresolved-variant", "code":"unresolved-variant",
                    "raw":node["note"]["raw"], "source_span":node["source_span"],
                    "aspects":["content","structure"], "influence":{"kind":"document"}}),
            );
        }
        for key in [
            "inline_children",
            "reading_children",
            "upper_children",
            "lower_children",
        ] {
            if let Some(children) = node.get(key).and_then(Value::as_array) {
                pending.extend(children);
            }
        }
    }
    problems.sort_by_key(|problem| problem["source_span"]["start"].as_u64());
    Ok(problems)
}

fn ruby_reading_children(
    content: Option<&Value>,
    recorder: &mut DivergenceRecorder,
    warnings: &mut Vec<Value>,
    path: &str,
    depth: usize,
) -> Result<Vec<Value>> {
    let mut children = inline_children_nodes(content, recorder, warnings, 0, path, depth)?;
    reading_coordinates(&mut children);
    Ok(children)
}

fn reading_coordinates(nodes: &mut [Value]) {
    for node in nodes {
        if let Some(span) = node.get_mut("span") {
            span["coordinate_system"] = json!("reading_utf8");
        }
        for key in ["inline_children", "upper_children", "lower_children"] {
            if let Some(children) = node.get_mut(key).and_then(Value::as_array_mut) {
                reading_coordinates(children);
            }
        }
    }
}

fn append_plain_visible_inline_text(node: &Value, out: &mut String) -> Result<()> {
    match node["kind"].as_str().unwrap_or("") {
        "text" => out.push_str(node["value"].as_str().unwrap_or("")),
        "ruby" => out.push_str(&ruby_component_text(node, "base", "base_content")?),
        "gaiji" => out.push_str(
            node["resolved"]
                .as_str()
                .or_else(|| node["description"].as_str())
                .unwrap_or(""),
        ),
        "accent" => out.push_str(node["resolved"].as_str().unwrap_or("")),
        "style" | "font_size" | "tcy" | "keigakomi" | "caption" | "yokogumi" | "warichu" => {
            append_plain_visible_content_text(node.get("content"), out)?;
        }
        "warigaki" => {
            append_plain_visible_content_text(node.get("upper"), out)?;
            append_plain_visible_content_text(node.get("lower"), out)?;
        }
        "raw" => {}
        "figure" => out.push_str(node["alt"].as_str().unwrap_or("")),
        other => bail!("unsupported inline kind in source attribution projection: {other}"),
    }
    Ok(())
}

fn map_inline_content(
    content: Option<&Value>,
    nodes: &mut Vec<Value>,
    recorder: &mut DivergenceRecorder,
    synthetic_warnings: &mut Vec<Value>,
    offset: u64,
    path: &str,
) -> Result<u64> {
    let mut current = offset;
    for (index, child) in content
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .enumerate()
    {
        let node_start = nodes.len();
        current = map_inline_to_nodes(
            child,
            nodes,
            recorder,
            synthetic_warnings,
            current,
            &format!("{path}[{index}]"),
        )?;
        attach_single_source_span(&mut nodes[node_start..], child.get("span"))?;
    }
    Ok(current)
}

/// Emit text and source-derived line breaks in the parser text projection.
fn map_text_node(
    node: &Value,
    nodes: &mut Vec<Value>,
    recorder: &mut DivergenceRecorder,
    synthetic_warnings: &mut Vec<Value>,
    offset: u64,
    path: &str,
) -> Result<u64> {
    if is_source_derived_line_break_text(node) {
        return map_source_derived_line_break_text(node, nodes, recorder, offset, path);
    }
    let _ = synthetic_warnings; // signature symmetry with map_text_node_with_quotes
    let text = node["value"].as_str().unwrap_or("");
    let end = offset + utf8_len(text);
    let span = map_node_span(node.get("span"), offset, end, recorder, path)?;
    nodes.push(json!({"type": "text", "span": span, "text": text}));
    Ok(end)
}

/// Like [`map_text_node`] but splits the text at `「」`/`『』` markers and
/// synthesizes `quote` nodes for each marker. Sub-segments and quote nodes use
/// synthetic spans (no false source-pointer claims) — a marker-free text node
/// delegates to [`map_text_node`] and keeps its real span.
///
/// `nesting_level` is stored as `null`; it is derivable from the ordered
/// `marker_type` sequence downstream.
fn map_text_node_with_quotes(
    node: &Value,
    nodes: &mut Vec<Value>,
    recorder: &mut DivergenceRecorder,
    synthetic_warnings: &mut Vec<Value>,
    offset: u64,
    path: &str,
) -> Result<u64> {
    let text = node["value"].as_str().unwrap_or("");
    let quote_chars: &[char] = &['「', '」', '『', '』'];

    if !text.chars().any(|c| quote_chars.contains(&c)) {
        return map_text_node(node, nodes, recorder, synthetic_warnings, offset, path);
    }

    let mut pos = offset;
    let mut segment_start = 0usize;

    for (i, ch) in text.char_indices() {
        if !quote_chars.contains(&ch) {
            continue;
        }
        // Emit the text segment before the marker (if non-empty).
        if segment_start < i {
            let segment_text = &text[segment_start..i];
            let end = pos + utf8_len(segment_text);
            nodes.push(json!({
                "type": "text",
                "span": synthetic_span(pos, end),
                "text": segment_text,
            }));
            pos = end;
        }

        // Emit the quote marker node with a synthetic single-char span.
        let marker_type = if ch == '「' || ch == '『' {
            "open"
        } else {
            "close"
        };
        let ch_len = ch.len_utf8() as u64;
        let span = synthetic_span(pos, pos + ch_len);
        nodes.push(json!({
            "type": "quote",
            "span": span,
            "marker_type": marker_type,
            "nesting_level": null,
            "text": ch.to_string(),
        }));
        pos += ch_len;
        segment_start = i + ch.len_utf8();
    }

    // Emit any trailing text after the last marker.
    if segment_start < text.len() {
        let segment_text = &text[segment_start..];
        let end = pos + utf8_len(segment_text);
        nodes.push(json!({
            "type": "text",
            "span": synthetic_span(pos, end),
            "text": segment_text,
        }));
        pos = end;
    }

    Ok(pos)
}

fn map_inline_to_nodes(
    node: &Value,
    nodes: &mut Vec<Value>,
    recorder: &mut DivergenceRecorder,
    synthetic_warnings: &mut Vec<Value>,
    offset: u64,
    path: &str,
) -> Result<u64> {
    match node["kind"].as_str().unwrap_or("") {
        "text" => {
            map_text_node_with_quotes(node, nodes, recorder, synthetic_warnings, offset, path)
        }
        "ruby" => {
            let base = ruby_component_text(node, "base", "base_content")?;
            let end = offset + utf8_len(&base);
            let span = map_node_span(node.get("span"), offset, end, recorder, path)?;
            recorder.record(
                "INVENTION",
                None,
                Some("ruby.scope"),
                None,
                Some(json!("explicit")),
            )?;
            let mut ruby_node = json!({
                "type": "ruby",
                "span": span,
                "ruby": {
                    "base": base,
                    "reading": ruby_component_text(node, "reading", "reading_content")?,
                    "scope": "explicit",
                    "direction": node.get("direction").and_then(Value::as_str)
                }
            });
            if node.get("base_content").is_some() {
                ruby_node["inline_children"] = json!(inline_children_nodes(
                    node.get("base_content"),
                    recorder,
                    synthetic_warnings,
                    offset,
                    &format!("{path}.ruby.base_content"),
                    0,
                )?);
            }
            if let Some(content) = node.get("reading_content") {
                let reading = ruby_reading_children(
                    Some(content),
                    recorder,
                    synthetic_warnings,
                    &format!("{path}.ruby.reading_content"),
                    1,
                )?;
                if !reading.is_empty() {
                    ruby_node["reading_children"] = json!(reading);
                }
            }
            nodes.push(ruby_node);
            Ok(end)
        }
        "gaiji" => {
            let visible = node["resolved"]
                .as_str()
                .or_else(|| node["description"].as_str())
                .unwrap_or("");
            let end = offset + utf8_len(visible);
            let span = map_node_span(node.get("span"), offset, end, recorder, path)?;
            let description_pointer = format!("{path}.gaiji.description");
            recorder.record(
                "INVENTION",
                Some(description_pointer.as_str()),
                Some("gaiji.raw_marker"),
                node.get("description").cloned(),
                node.get("description").cloned(),
            )?;
            let resolved_pointer = format!("{path}.gaiji.resolved");
            recorder.record(
                "AMBIGUITY",
                Some(resolved_pointer.as_str()),
                Some("gaiji.resolved"),
                node.get("resolved").cloned(),
                Some(json!(
                    node.get("resolved").is_some_and(|value| !value.is_null())
                )),
            )?;
            if node.get("jis_code").is_some_and(|value| !value.is_null()) {
                let jis_pointer = format!("{path}.gaiji.jis_code");
                recorder.record_if_measured(
                    "AMBIGUITY",
                    Some(jis_pointer.as_str()),
                    Some("gaiji.reference"),
                    node.get("jis_code").cloned(),
                    node.get("jis_code").cloned(),
                );
            }
            if node
                .get("unresolved_reason")
                .is_some_and(|value| !value.is_null())
            {
                let unresolved_pointer = format!("{path}.gaiji.unresolved_reason");
                recorder.record_if_measured(
                    "LOSS",
                    Some(unresolved_pointer.as_str()),
                    None,
                    node.get("unresolved_reason").cloned(),
                    None,
                );
            }
            if node.get("resolved").is_none_or(Value::is_null) {
                recorder.record("LOSS", None, Some("gaiji.unicode"), None, Some(Value::Null))?;
            }
            nodes.push(json!({
                "type": "gaiji",
                "span": span,
                "gaiji": gaiji_payload(node)
            }));
            Ok(end)
        }
        "style" => {
            let style_pointer = format!("{path}.style");
            recorder.record(
                "AMBIGUITY",
                Some(style_pointer.as_str()),
                Some("emphasis"),
                node.get("style_type").cloned(),
                None,
            )?;
            let text = visible_content_text(
                node.get("content"),
                recorder,
                &format!("{path}.content"),
                Some("(emphasis.text)"),
            )?;
            let end = offset + utf8_len(&text);
            let span = map_node_span(node.get("span"), offset, end, recorder, path)?;
            let inline_children = inline_children_nodes(
                node.get("content"),
                recorder,
                synthetic_warnings,
                offset,
                &format!("{path}.content"),
                0,
            )?;
            nodes.push(json!({
                "type": "emphasis",
                "span": span,
                "text": text,
                "inline_children": inline_children,
                "style": node.get("style_type").and_then(Value::as_str).unwrap_or("style"),
            }));
            if let Some(decoration) = node.get("decoration") {
                nodes.last_mut().expect("emphasis node emitted")["decoration"] = decoration.clone();
            }
            Ok(end)
        }
        "accent" => map_accent_to_node(node, nodes, recorder, offset, path),
        "warigaki" | "warichu" => {
            map_warigaki_to_nodes(node, nodes, recorder, synthetic_warnings, offset, path, 0)
        }
        "figure" => map_figure_to_node(node, nodes, recorder, offset, path),
        "raw" => map_raw_to_nodes(node, nodes, recorder, offset, path),
        "font_size" | "tcy" | "keigakomi" | "yokogumi" => {
            map_layout_span_to_node(node, nodes, recorder, synthetic_warnings, offset, path, 0)
        }
        "caption" => {
            let kind = node["kind"].as_str().unwrap_or("");
            let pointer = format!("{path}.{kind}");
            recorder.record(
                "UNSUPPORTED",
                Some(pointer.as_str()),
                Some("emphasis(?)"),
                None,
                None,
            )?;
            let text = visible_content_text(
                node.get("content"),
                recorder,
                &format!("{path}.content"),
                Some("(emphasis.text)"),
            )?;
            let end = offset + utf8_len(&text);
            let span = map_node_span(node.get("span"), offset, end, recorder, path)?;
            let inline_children = inline_children_nodes(
                node.get("content"),
                recorder,
                synthetic_warnings,
                offset,
                &format!("{path}.content"),
                0,
            )?;
            nodes.push(json!({
                "type": "emphasis",
                "span": span,
                "text": text,
                "inline_children": inline_children,
                "style": kind,
            }));
            Ok(end)
        }
        other => bail!("unsupported inline kind: {other}"),
    }
}

fn layout_scope(node: &Value) -> Result<Value> {
    match node["kind"].as_str().unwrap_or("") {
        "font_size" => Ok(json!({
            "kind": "font-size",
            "source": "aat-inline",
            "size_type": node["size_type"].as_str().unwrap_or("unknown"),
            "level": node["level"].as_u64().unwrap_or(0),
        })),
        "tcy" => Ok(json!({
            "kind": "tcy",
            "source": "aat-inline",
            "marker": node.get("marker").and_then(Value::as_str),
        })),
        "keigakomi" => Ok(json!({
            "kind": "keigakomi",
            "source": "aat-inline",
            "border": node.get("border").and_then(Value::as_str),
            "marker": node.get("marker").and_then(Value::as_str),
        })),
        "yokogumi" => Ok(json!({
            "kind": "yokogumi",
            "source": "aat-inline",
            "direction": "horizontal",
            "marker": node.get("marker").and_then(Value::as_str),
        })),
        other => bail!("unsupported layout-span kind: {other}"),
    }
}

fn map_layout_span_to_node(
    node: &Value,
    nodes: &mut Vec<Value>,
    recorder: &mut DivergenceRecorder,
    synthetic_warnings: &mut Vec<Value>,
    offset: u64,
    path: &str,
    depth: usize,
) -> Result<u64> {
    let text = plain_visible_content_text(node.get("content"))?;
    let end = offset + utf8_len(&text);
    let span = map_node_span(node.get("span"), offset, end, recorder, path)?;
    let inline_children = inline_children_nodes(
        node.get("content"),
        recorder,
        synthetic_warnings,
        offset,
        &format!("{path}.content"),
        depth + 1,
    )?;
    nodes.push(json!({
        "type": "layout-span",
        "span": span,
        "text": text,
        "inline_children": inline_children,
        "layout": layout_scope(node)?,
    }));
    Ok(end)
}

fn inline_children_nodes(
    content: Option<&Value>,
    recorder: &mut DivergenceRecorder,
    synthetic_warnings: &mut Vec<Value>,
    offset: u64,
    path: &str,
    depth: usize,
) -> Result<Vec<Value>> {
    let mut nodes = Vec::new();
    let mut current = offset;
    for (index, child) in content
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .enumerate()
    {
        let node_start = nodes.len();
        current = inline_child_node(
            child,
            &mut nodes,
            recorder,
            synthetic_warnings,
            current,
            &format!("{path}[{index}]"),
            depth,
        )?;
        attach_single_source_span(&mut nodes[node_start..], child.get("span"))?;
    }
    Ok(nodes)
}

fn inline_child_node(
    node: &Value,
    nodes: &mut Vec<Value>,
    recorder: &mut DivergenceRecorder,
    synthetic_warnings: &mut Vec<Value>,
    offset: u64,
    path: &str,
    depth: usize,
) -> Result<u64> {
    if depth >= 64 {
        synthetic_warnings.push(inline_children_depth_limit_warning(path));
        recorder.record(
            "INVENTION",
            None,
            Some("warnings[].severity"),
            None,
            Some(json!("warning")),
        )?;
        recorder.record(
            "INVENTION",
            None,
            Some("warnings[].code"),
            None,
            Some(json!("INLINE_CHILDREN_DEPTH_LIMIT")),
        )?;
        return push_unrecorded_text_node(&plain_visible_inline_text(node)?, nodes, offset);
    }

    match node["kind"].as_str().unwrap_or("") {
        "text" => {
            if is_source_derived_line_break_text(node) {
                return push_unrecorded_line_break_text(node, nodes, offset);
            }
            push_unrecorded_text_node(node["value"].as_str().unwrap_or(""), nodes, offset)
        }
        "ruby" => {
            let base = ruby_component_text(node, "base", "base_content")?;
            let end = offset + utf8_len(&base);
            let mut ruby_node = json!({
                "type": "ruby",
                "span": synthetic_span(offset, end),
                "ruby": {
                    "base": base,
                    "reading": ruby_component_text(node, "reading", "reading_content")?,
                    "scope": "explicit",
                    "direction": node.get("direction").and_then(Value::as_str)
                }
            });
            if node.get("base_content").is_some() {
                ruby_node["inline_children"] = json!(inline_children_nodes(
                    node.get("base_content"),
                    recorder,
                    synthetic_warnings,
                    offset,
                    &format!("{path}.ruby.base_content"),
                    depth + 1,
                )?);
            }
            if let Some(content) = node.get("reading_content") {
                let reading = ruby_reading_children(
                    Some(content),
                    recorder,
                    synthetic_warnings,
                    &format!("{path}.ruby.reading_content"),
                    depth + 1,
                )?;
                if !reading.is_empty() {
                    ruby_node["reading_children"] = json!(reading);
                }
            }
            nodes.push(ruby_node);
            Ok(end)
        }
        "gaiji" => {
            let visible = node["resolved"]
                .as_str()
                .or_else(|| node["description"].as_str())
                .unwrap_or("");
            let end = offset + utf8_len(visible);
            nodes.push(json!({
                "type": "gaiji",
                "span": synthetic_span(offset, end),
                "gaiji": gaiji_payload(node)
            }));
            Ok(end)
        }
        "style" => {
            let text = plain_visible_content_text(node.get("content"))?;
            let end = offset + utf8_len(&text);
            let inline_children = inline_children_nodes(
                node.get("content"),
                recorder,
                synthetic_warnings,
                offset,
                &format!("{path}.content"),
                depth + 1,
            )?;
            nodes.push(json!({
                "type": "emphasis",
                "span": synthetic_span(offset, end),
                "text": text,
                "inline_children": inline_children,
                "style": node.get("style_type").and_then(Value::as_str).unwrap_or("style"),
            }));
            if let Some(decoration) = node.get("decoration") {
                nodes.last_mut().expect("emphasis node emitted")["decoration"] = decoration.clone();
            }
            Ok(end)
        }
        "accent" => {
            let text = accent_text(node);
            let end = offset + utf8_len(&text);
            nodes.push(json!({
                "type": "emphasis",
                "span": synthetic_span(offset, end),
                "text": text,
                "style": accent_style(node),
            }));
            Ok(end)
        }
        "font_size" | "tcy" | "keigakomi" | "yokogumi" => {
            let text = plain_visible_content_text(node.get("content"))?;
            let end = offset + utf8_len(&text);
            let inline_children = inline_children_nodes(
                node.get("content"),
                recorder,
                synthetic_warnings,
                offset,
                &format!("{path}.content"),
                depth + 1,
            )?;
            nodes.push(json!({
                "type": "layout-span",
                "span": synthetic_span(offset, end),
                "text": text,
                "inline_children": inline_children,
                "layout": layout_scope(node)?,
            }));
            Ok(end)
        }
        "caption" => {
            let kind = node["kind"].as_str().unwrap_or("");
            let text = plain_visible_content_text(node.get("content"))?;
            let end = offset + utf8_len(&text);
            let inline_children = inline_children_nodes(
                node.get("content"),
                recorder,
                synthetic_warnings,
                offset,
                &format!("{path}.content"),
                depth + 1,
            )?;
            nodes.push(json!({
                "type": "emphasis",
                "span": synthetic_span(offset, end),
                "text": text,
                "inline_children": inline_children,
                "style": kind,
            }));
            Ok(end)
        }
        "warigaki" | "warichu" => map_warigaki_to_nodes(
            node,
            nodes,
            recorder,
            synthetic_warnings,
            offset,
            path,
            depth,
        ),
        "figure" => {
            let alt = node["alt"].as_str().unwrap_or("");
            push_unrecorded_text_node(alt, nodes, offset)
        }
        "raw" => map_raw_to_nodes(node, nodes, recorder, offset, path),
        other => bail!("unsupported inline kind in inline_children projection at {path}: {other}"),
    }
}

fn push_unrecorded_text_node(text: &str, nodes: &mut Vec<Value>, offset: u64) -> Result<u64> {
    let end = offset + utf8_len(text);
    nodes.push(json!({"type": "text", "span": synthetic_span(offset, end), "text": text}));
    Ok(end)
}

fn push_unrecorded_line_break_text(
    node: &Value,
    nodes: &mut Vec<Value>,
    offset: u64,
) -> Result<u64> {
    let mut current = offset;
    let mut pending = String::new();
    for ch in node["value"].as_str().unwrap_or("").chars() {
        if ch == '\n' {
            if !pending.is_empty() {
                current = push_unrecorded_text_node(&pending, nodes, current)?;
                pending.clear();
            }
            let end = current + utf8_len("\n");
            nodes.push(json!({
                "type": "line-break",
                "span": synthetic_span(current, end),
                "marker": node.get("x-break-marker").and_then(Value::as_str).unwrap_or("line"),
            }));
            current = end;
        } else {
            pending.push(ch);
        }
    }
    if !pending.is_empty() {
        current = push_unrecorded_text_node(&pending, nodes, current)?;
    }
    Ok(current)
}

fn synthetic_span(start: u64, end: u64) -> Value {
    json!({"start": start, "end": end, "coordinate_system": "parser_text_utf8"})
}

fn map_accent_to_node(
    node: &Value,
    nodes: &mut Vec<Value>,
    recorder: &mut DivergenceRecorder,
    offset: u64,
    path: &str,
) -> Result<u64> {
    let accent_pointer = format!("{path}.accent");
    recorder.record(
        "AMBIGUITY",
        Some(accent_pointer.as_str()),
        Some("emphasis"),
        node.get("name").cloned(),
        None,
    )?;

    if node.get("code").is_some_and(|value| !value.is_null()) {
        let code_pointer = format!("{path}.accent.code");
        recorder.record_if_measured(
            "INVENTION",
            Some(code_pointer.as_str()),
            Some("emphasis.style"),
            node.get("code").cloned(),
            node.get("code").cloned(),
        );
    }

    if node.get("name").is_some_and(|value| !value.is_null()) {
        let name_pointer = format!("{path}.accent.name");
        recorder.record_if_measured(
            "LOSS",
            Some(name_pointer.as_str()),
            None,
            node.get("name").cloned(),
            None,
        );
    }

    let text = accent_text(node);
    let end = offset + utf8_len(&text);
    let span = map_node_span(node.get("span"), offset, end, recorder, path)?;
    nodes.push(json!({
        "type": "emphasis",
        "span": span,
        "text": text,
        "style": accent_style(node),
    }));
    Ok(end)
}

fn accent_text(node: &Value) -> String {
    node["resolved"].as_str().unwrap_or("").to_owned()
}

fn accent_style(node: &Value) -> String {
    node["code"].as_str().unwrap_or("accent").to_owned()
}

fn attach_ruby_variant(source_node: &Value, variant: &Value, nodes: &mut [Value]) -> Result<bool> {
    if variant["target_kind"] != "ruby-reading" {
        return Ok(false);
    }
    let Some(ruby) = nodes.last_mut() else {
        return Ok(false);
    };
    if ruby["type"] != "ruby"
        || ruby["ruby"]["reading"] != variant["current"]
        || ruby["source_span"]["end"].as_u64().is_none()
        || ruby["source_span"]["end"].as_u64() != source_node["span"]["byte_start"].as_u64()
    {
        return Ok(false);
    }
    let Some(current) = variant["current"].as_str() else {
        return Ok(false);
    };
    let children = ruby.get_mut("reading_children").map(Value::take).unwrap_or_else(|| json!([
        {"type": "text", "text": current, "span": {"start": 0, "end": utf8_len(current), "coordinate_system": "reading_utf8"}}
    ]));
    if children.as_array().is_some_and(|children| {
        children
            .iter()
            .any(|child| child["type"] == "base-text-variant")
    }) {
        ruby["reading_children"] = children;
        return Ok(false);
    }
    let mut annotation = json!({"type": "base-text-variant", "text": current,
        "variant": {"base_text": variant["base_text"]}, "inline_children": children,
        "span": {"start": 0, "end": utf8_len(current), "coordinate_system": "reading_utf8"}});
    attach_source_span(&mut annotation, source_node.get("span"))?;
    ruby["reading_children"] = json!([annotation]);
    Ok(true)
}

fn map_raw_to_nodes(
    node: &Value,
    nodes: &mut Vec<Value>,
    recorder: &mut DivergenceRecorder,
    offset: u64,
    path: &str,
) -> Result<u64> {
    if let Some(variant) = node.get("text_variant") {
        if attach_ruby_variant(node, variant, nodes)? {
            return Ok(offset);
        }
        nodes.push(
            json!({"type": "editor-note", "span": synthetic_span(offset, offset),
            "note": {"raw": node["source"], "category": "variant", "resolution": "unresolved"}}),
        );
        return Ok(offset);
    }
    let raw_pointer = format!("{path}.raw");
    if !recorder.record_if_measured(
        "UNSUPPORTED",
        Some(raw_pointer.as_str()),
        None,
        node.get("source").cloned(),
        None,
    ) {
        bail!("unmeasured raw divergence at {raw_pointer}");
    }

    let source = node.get("source").and_then(Value::as_str).unwrap_or("");
    match raw_recovery_class(node, source) {
        RawRecoveryClass::PageBreak => {
            let span = map_node_span(node.get("span"), offset, offset, recorder, path)?;
            nodes.push(json!({
                "type": "page-break",
                "span": span,
                "marker": "page",
                "page_number": null,
            }));
        }
        RawRecoveryClass::SourceNote => {
            let span = map_node_span(node.get("span"), offset, offset, recorder, path)?;
            let mut note = json!({"raw": source, "category": "misc"});
            if node.get("x-source-marker-kind").is_some() {
                note["resolution"] = json!("unresolved");
            }
            nodes.push(json!({"type": "editor-note", "span": span, "note": note}));
        }
        RawRecoveryClass::ParserResidue => {}
    }
    Ok(offset)
}

#[derive(Debug, Clone, Copy)]
enum RawRecoveryClass {
    PageBreak,
    SourceNote,
    ParserResidue,
}

fn raw_recovery_class(node: &Value, source: &str) -> RawRecoveryClass {
    if node["x-source-marker-kind"] == "pageBreak" {
        return RawRecoveryClass::PageBreak;
    }
    if node.get("x-source-marker-kind").is_some() {
        return RawRecoveryClass::SourceNote;
    }
    let provenance = node.get("x-provenance").and_then(Value::as_str);
    let trimmed = source.trim();
    if provenance == Some("parser-derived") || is_parser_raw_residue(trimmed) {
        return RawRecoveryClass::ParserResidue;
    }
    if matches!(trimmed, "改頁" | "改ページ") {
        RawRecoveryClass::PageBreak
    } else {
        RawRecoveryClass::SourceNote
    }
}

fn is_parser_raw_residue(value: &str) -> bool {
    value.is_empty()
        || (value.starts_with('<') && value.ends_with('>'))
        || value.starts_with("BlockStart(")
        || value.starts_with("BlockEnd(")
        || value.starts_with("InlineStart(")
        || value.starts_with("InlineEnd(")
}

fn is_source_derived_line_break_text(node: &Value) -> bool {
    node.get("x-break-kind").and_then(Value::as_str) == Some("line")
}

fn map_source_derived_line_break_text(
    node: &Value,
    nodes: &mut Vec<Value>,
    recorder: &mut DivergenceRecorder,
    offset: u64,
    path: &str,
) -> Result<u64> {
    let mut current = offset;
    let mut pending = String::new();
    for ch in node["value"].as_str().unwrap_or("").chars() {
        if ch == '\n' {
            if !pending.is_empty() {
                current = push_text_node(&pending, nodes, recorder, current, path)?;
                pending.clear();
            }
            let end = current + utf8_len("\n");
            let span = map_node_span(None, current, end, recorder, path)?;
            nodes.push(json!({
                "type": "line-break",
                "span": span,
                "marker": node.get("x-break-marker").and_then(Value::as_str).unwrap_or("line"),
            }));
            current = end;
        } else {
            pending.push(ch);
        }
    }
    if !pending.is_empty() {
        current = push_text_node(&pending, nodes, recorder, current, path)?;
    }
    Ok(current)
}

fn push_text_node(
    text: &str,
    nodes: &mut Vec<Value>,
    recorder: &mut DivergenceRecorder,
    offset: u64,
    path: &str,
) -> Result<u64> {
    let end = offset + utf8_len(text);
    let span = map_node_span(None, offset, end, recorder, path)?;
    nodes.push(json!({"type": "text", "span": span, "text": text}));
    Ok(end)
}

fn map_figure_to_node(
    node: &Value,
    nodes: &mut Vec<Value>,
    recorder: &mut DivergenceRecorder,
    offset: u64,
    path: &str,
) -> Result<u64> {
    let filename_pointer = format!("{path}.figure.filename");
    let filename = node["filename"].as_str().unwrap_or("");
    recorder.record(
        "INVENTION",
        Some(filename_pointer.as_str()),
        Some("image.src"),
        node.get("filename").cloned(),
        Some(json!(filename)),
    )?;

    record_optional_figure_loss(node, recorder, path, "caption")?;
    record_optional_figure_loss(node, recorder, path, "css_class")?;
    record_optional_figure_loss(node, recorder, path, "height")?;
    record_optional_figure_loss(node, recorder, path, "width")?;

    let span = map_node_span(node.get("span"), offset, offset, recorder, path)?;
    nodes.push(json!({
        "type": "image",
        "span": span,
        "src": filename,
        "alt": node.get("alt").cloned().unwrap_or(Value::Null),
    }));
    Ok(offset)
}

fn record_optional_figure_loss(
    node: &Value,
    recorder: &mut DivergenceRecorder,
    path: &str,
    field: &str,
) -> Result<()> {
    if node.get(field).is_some_and(|value| !value.is_null()) {
        let pointer = format!("{path}.figure.{field}");
        recorder.record(
            "LOSS",
            Some(pointer.as_str()),
            None,
            node.get(field).and_then(scalar_divergence_value),
            None,
        )?;
    }
    Ok(())
}

fn scalar_divergence_value(value: &Value) -> Option<Value> {
    if value.is_boolean() || value.is_i64() || value.is_u64() || value.is_string() {
        Some(value.clone())
    } else {
        None
    }
}

fn map_warigaki_to_nodes(
    node: &Value,
    nodes: &mut Vec<Value>,
    recorder: &mut DivergenceRecorder,
    synthetic_warnings: &mut Vec<Value>,
    offset: u64,
    path: &str,
    depth: usize,
) -> Result<u64> {
    if let Some(content) = node.get("content") {
        let text = plain_visible_content_text(Some(content))?;
        let end = offset + utf8_len(&text);
        let children = inline_children_nodes(
            Some(content),
            recorder,
            synthetic_warnings,
            offset,
            &format!("{path}.content"),
            depth + 1,
        )?;
        nodes.push(json!({"type":"warichu", "text":text, "span":synthetic_span(offset,end), "inline_children":children}));
        return Ok(end);
    }
    let upper_text = plain_visible_content_text(node.get("upper"))?;
    let lower_text = plain_visible_content_text(node.get("lower"))?;
    let lower_offset = offset + utf8_len(&upper_text);
    let end = lower_offset + utf8_len(&lower_text);
    let upper = inline_children_nodes(
        node.get("upper"),
        recorder,
        synthetic_warnings,
        offset,
        &format!("{path}.warigaki.upper"),
        depth + 1,
    )?;
    let lower = inline_children_nodes(
        node.get("lower"),
        recorder,
        synthetic_warnings,
        lower_offset,
        &format!("{path}.warigaki.lower"),
        depth + 1,
    )?;
    nodes.push(json!({"type": "warichu", "span": synthetic_span(offset, end),
        "text": format!("{upper_text}{lower_text}"), "upper_children": upper, "lower_children": lower}));
    Ok(end)
}

fn inline_children_depth_limit_warning(path: &str) -> Value {
    json!({
        "severity": "warning",
        "code": "INLINE_CHILDREN_DEPTH_LIMIT",
        "message": format!("inline_children depth limit 64 reached at {path}; flattened visible text"),
        "span": null,
        "construct": path,
        "recovery": "Flattened nested inline content to visible text."
    })
}

fn visible_content_text(
    content: Option<&Value>,
    recorder: &mut DivergenceRecorder,
    path: &str,
    target_pointer: Option<&str>,
) -> Result<String> {
    let mut text = String::new();
    append_visible_content_text(content, recorder, path, target_pointer, &mut text)?;
    Ok(text)
}

fn append_visible_content_text(
    content: Option<&Value>,
    recorder: &mut DivergenceRecorder,
    path: &str,
    target_pointer: Option<&str>,
    out: &mut String,
) -> Result<()> {
    for (index, child) in content
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .enumerate()
    {
        append_visible_inline_text(
            child,
            recorder,
            &format!("{path}[{index}]"),
            target_pointer,
            out,
        )?;
    }
    Ok(())
}

fn append_visible_inline_text(
    node: &Value,
    recorder: &mut DivergenceRecorder,
    path: &str,
    target_pointer: Option<&str>,
    out: &mut String,
) -> Result<()> {
    match node["kind"].as_str().unwrap_or("") {
        "text" => out.push_str(node["value"].as_str().unwrap_or("")),
        "ruby" => {
            let container_pointer = format!("{path}.ruby");
            record_measured_loss(
                recorder,
                container_pointer.as_str(),
                None,
                Some(json!("ruby")),
            )?;
            let reading_pointer = format!("{path}.ruby.reading");
            recorder.record_if_measured(
                "LOSS",
                Some(reading_pointer.as_str()),
                target_pointer,
                node.get("reading").cloned(),
                None,
            );
            out.push_str(&ruby_component_text(node, "base", "base_content")?);
        }
        "gaiji" => {
            let container_pointer = format!("{path}.gaiji");
            record_measured_loss(
                recorder,
                container_pointer.as_str(),
                None,
                Some(json!("gaiji")),
            )?;
            let resolved_pointer = format!("{path}.gaiji.resolved");
            recorder.record_if_measured(
                "AMBIGUITY",
                Some(resolved_pointer.as_str()),
                target_pointer,
                node.get("resolved").cloned(),
                node.get("resolved").cloned(),
            );
            out.push_str(
                node["resolved"]
                    .as_str()
                    .or_else(|| node["description"].as_str())
                    .unwrap_or(""),
            );
        }
        "accent" => {
            if let Some(target) = target_pointer {
                let accent_pointer = format!("{path}.accent");
                recorder.record_if_measured(
                    "LOSS",
                    Some(accent_pointer.as_str()),
                    Some(target),
                    node.get("name").cloned(),
                    None,
                );
            }
            out.push_str(node["resolved"].as_str().unwrap_or(""));
        }
        "style" | "caption" => {
            let kind = node["kind"].as_str().unwrap_or("");
            let container_pointer = format!("{path}.{kind}");
            record_measured_loss(
                recorder,
                container_pointer.as_str(),
                None,
                Some(json!(kind)),
            )?;
            append_visible_content_text(
                node.get("content"),
                recorder,
                &format!("{path}.content"),
                target_pointer,
                out,
            )?;
        }
        "font_size" | "tcy" | "keigakomi" | "yokogumi" | "warichu" => append_visible_content_text(
            node.get("content"),
            recorder,
            &format!("{path}.content"),
            target_pointer,
            out,
        )?,
        "warigaki" => {
            append_visible_content_text(
                node.get("upper"),
                recorder,
                &format!("{path}.warigaki.upper"),
                target_pointer,
                out,
            )?;
            append_visible_content_text(
                node.get("lower"),
                recorder,
                &format!("{path}.warigaki.lower"),
                target_pointer,
                out,
            )?;
        }
        "raw" => {
            let raw_pointer = format!("{path}.raw");
            record_measured_loss(
                recorder,
                raw_pointer.as_str(),
                None,
                node.get("source").cloned(),
            )?;
            recorder.record_if_measured(
                "LOSS",
                Some(raw_pointer.as_str()),
                target_pointer,
                None,
                None,
            );
        }
        "figure" => {
            let figure_pointer = format!("{path}.figure");
            recorder.record_if_measured(
                "LOSS",
                Some(figure_pointer.as_str()),
                target_pointer,
                None,
                None,
            );
            out.push_str(node["alt"].as_str().unwrap_or(""));
        }
        other => bail!("unsupported inline kind in visible projection: {other}"),
    }
    Ok(())
}

fn record_measured_loss(
    recorder: &mut DivergenceRecorder,
    aat_pointer: &str,
    parser_ir_pointer: Option<&str>,
    source_value: Option<Value>,
) -> Result<()> {
    recorder.record_if_measured(
        "LOSS",
        Some(aat_pointer),
        parser_ir_pointer,
        source_value,
        None,
    );
    Ok(())
}

fn map_node_span(
    aat_span: Option<&Value>,
    start: u64,
    end: u64,
    recorder: &mut DivergenceRecorder,
    path: &str,
) -> Result<Value> {
    if aat_span.is_none() {
        let span_pointer = format!("{path}.span");
        recorder.record_if_measured(
            "AMBIGUITY",
            Some(span_pointer.as_str()),
            Some("span"),
            None,
            None,
        );
    }
    Ok(synthetic_span(start, end))
}

fn attach_source_span(value: &mut Value, aat_span: Option<&Value>) -> Result<()> {
    if let Some(span) = source_span(aat_span)? {
        value["source_span"] = span;
    }
    Ok(())
}

fn source_span(aat_span: Option<&Value>) -> Result<Option<Value>> {
    let Some(span) = aat_span else {
        return Ok(None);
    };
    let (Some(start), Some(end)) = (
        span.get("byte_start").and_then(Value::as_u64),
        span.get("byte_end").and_then(Value::as_u64),
    ) else {
        return Ok(None);
    };
    if start > end {
        bail!("AAT source span is inverted: {start}..{end}");
    }
    let mut source = json!({"start": start, "end": end, "coordinate_system": "decoded_utf8"});
    if let Some(line) = span.get("line_start") {
        source["line"] = line.clone();
    }
    Ok(Some(source))
}

fn attach_single_source_span(nodes: &mut [Value], aat_span: Option<&Value>) -> Result<()> {
    if let [node] = nodes {
        attach_source_span(node, aat_span)?;
    }
    Ok(())
}

fn derived_from(aat: &Value, mapping: &MappingDocument) -> Result<Value> {
    let meta = &aat["meta"];
    let Some(aat_version) = aat["version"].as_u64() else {
        bail!("AAT version is required for parser-IR derived_from");
    };
    let Some(adapter) = meta["adapter"].as_str() else {
        bail!("AAT meta.adapter is required for parser-IR derived_from");
    };
    let Some(parse_complete) = meta["parse_complete"].as_bool() else {
        bail!("AAT meta.parse_complete is required for parser-IR derived_from");
    };
    Ok(json!({
        "aat_version": aat_version,
        "aat_adapter": adapter,
        "aat_adapter_version": meta.get("adapter_version").and_then(Value::as_str),
        "parse_complete": parse_complete,
        "mapping_id": mapping.mapping_id,
        "mapping_version": mapping.mapping_version,
        "mapping_schema_hash": mapping.mapping_schema_hash,
    }))
}

fn map_source(
    aat: &Value,
    options: &ConversionOptions,
    recorder: &mut DivergenceRecorder,
) -> Result<Value> {
    let meta = &aat["meta"];
    let source_hash = meta
        .get("source_hash")
        .and_then(Value::as_str)
        .unwrap_or("sha256:0000000000000000000000000000000000000000000000000000000000000000");
    let primary_text_hash = meta
        .get("primary_text_hash")
        .and_then(Value::as_str)
        .unwrap_or(source_hash);
    if meta.get("primary_text_hash").is_some() && primary_text_hash != source_hash {
        bail!(
            "AAT meta.primary_text_hash must equal compatibility alias meta.source_hash: primary_text_hash={primary_text_hash} source_hash={source_hash}"
        );
    }
    let work_content_hash = options
        .work_content_hash
        .as_deref()
        .unwrap_or(primary_text_hash);
    if !is_sha256_hash(work_content_hash) {
        bail!("invalid work_content_hash; expected sha256 followed by 64 lowercase hex digits");
    }
    let Some(source_encoding) = meta["source_encoding"].as_str() else {
        bail!("AAT meta.source_encoding is required for parser-IR decode_outcome");
    };
    let encoding = match source_encoding {
        "utf-8" | "utf-8-bom" => "UTF-8",
        "windows-31j" | "windows-31j-lossy" => "Shift_JIS",
        _ => "unknown",
    };
    let primary_text_pointer = if meta.get("primary_text_hash").is_some() {
        "meta.primary_text_hash"
    } else {
        "meta.source_hash"
    };
    recorder.record(
        "AMBIGUITY",
        Some(primary_text_pointer),
        Some("source.primary_text_hash"),
        meta.get(primary_text_pointer.trim_start_matches("meta."))
            .cloned(),
        Some(json!(primary_text_hash)),
    )?;
    recorder.record(
        "INVENTION",
        None,
        Some("source.normalization"),
        None,
        Some(json!("source")),
    )?;
    recorder.record(
        "INVENTION",
        None,
        Some("source.source_path"),
        None,
        Some(Value::Null),
    )?;
    if meta.get("metrics").is_some() {
        recorder.record("LOSS", Some("meta.metrics"), None, None, None)?;
    }
    if meta.get("semantic_summary").is_some() {
        recorder.record("LOSS", Some("meta.semantic_summary"), None, None, None)?;
    }
    Ok(json!({
        "work_content_hash": work_content_hash,
        "primary_text_hash": primary_text_hash,
        "source_path": null,
        "encoding": encoding,
        "decode_outcome": source_encoding,
        "normalization": "source",
    }))
}

fn is_sha256_hash(value: &str) -> bool {
    let Some(hex) = value.strip_prefix("sha256:") else {
        return false;
    };
    hex.len() == 64
        && hex
            .bytes()
            .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
}

fn map_diagnostics(
    aat: &Value,
    recorder: &mut DivergenceRecorder,
) -> Result<(Vec<Value>, Vec<Value>)> {
    let mut warnings = Vec::new();
    let mut errors = Vec::new();
    for warning in aat
        .pointer("/meta/warnings")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
    {
        let severity = warning.get("severity").and_then(Value::as_str);
        if severity.is_none() {
            recorder.record(
                "INVENTION",
                None,
                Some("warnings[].severity"),
                None,
                Some(json!("warning")),
            )?;
        }
        let code = warning.get("code").and_then(Value::as_str);
        if code.is_none() {
            recorder.record(
                "INVENTION",
                None,
                Some("warnings[].code"),
                None,
                Some(json!("AAT_WARNING")),
            )?;
        }
        let severity = severity.unwrap_or("warning");
        let mut diagnostic = json!({
            "severity": severity,
            "code": code.unwrap_or("AAT_WARNING"),
            "message": warning["message"].as_str().unwrap_or(""),
            "span": null,
            "construct": warning.get("path").cloned().unwrap_or(Value::Null),
            "recovery": null,
        });
        if let Some(mut span) = source_span(warning.get("span"))? {
            if let Some(end_line) = warning.pointer("/span/line_end") {
                span["end_line"] = end_line.clone();
            }
            diagnostic["span"] = span;
        }
        if matches!(severity, "error" | "fatal") {
            errors.push(diagnostic);
        } else {
            warnings.push(diagnostic);
        }
    }
    Ok((warnings, errors))
}

fn aat_meta(aat: &mut Value) -> AatMeta {
    let meta = &aat["meta"];
    let work_id = aat["work_id"].as_str().unwrap_or("unknown").to_owned();
    let version = aat["version"].as_u64().unwrap_or(1);
    let adapter = meta["adapter"].as_str().unwrap_or("unknown").to_owned();
    let adapter_version = meta["adapter_version"]
        .as_str()
        .unwrap_or("unknown")
        .to_owned();
    let source_hash = meta["source_hash"]
        .as_str()
        .unwrap_or("sha256:0000000000000000000000000000000000000000000000000000000000000000")
        .to_owned();
    let parse_complete = meta["parse_complete"].as_bool().unwrap_or(false);
    // The AAT is owned and dead after this call, so move the potentially
    // large metrics/semantic_summary subtrees instead of cloning them.
    let (metrics, semantic_summary) = match aat.get_mut("meta").and_then(Value::as_object_mut) {
        Some(meta) => (
            meta.get_mut("metrics")
                .map(Value::take)
                .unwrap_or(Value::Null),
            meta.get_mut("semantic_summary")
                .map(Value::take)
                .unwrap_or(Value::Null),
        ),
        None => (Value::Null, Value::Null),
    };
    AatMeta {
        work_id,
        version,
        adapter,
        adapter_version,
        source_hash,
        parse_complete,
        metrics,
        semantic_summary,
    }
}

fn utf8_len(value: &str) -> u64 {
    value.len() as u64
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use serde_json::json;

    use super::*;
    use crate::{
        divergence::{bundle_invocation_count, reset_bundle_invocation_count},
        schema::{read_json, schema_hash},
    };

    #[test]
    fn invalid_production_conversion_stops_before_divergence_bundle() {
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let research_root = repo_root.join("research");
        let mut mapping =
            MappingDocument::from_path(&repo_root.join("data/aat-to-parser-ir-mapping-v1.json"))
                .unwrap();
        let mut schemas =
            SchemaSet::load_for_aat_version(&repo_root, &research_root, mapping.source_aat_version)
                .unwrap();
        schemas.parser_ir_schema = json!({
            "$schema": "https://json-schema.org/draft/2020-12/schema",
            "not": {}
        });
        mapping.target_parser_ir_schema_hash = schema_hash(&schemas.parser_ir_schema).unwrap();
        let converter = PreparedConverter::new(mapping, schemas).unwrap();
        let aat = read_json(
            &repo_root
                .join("crates/ab-aat-to-parser-ir/tests/fixtures/nested-sentence-basic.aat.json"),
        )
        .unwrap();

        reset_bundle_invocation_count();
        converter
            .convert(aat, ConversionOptions::default())
            .unwrap_err();
        assert_eq!(bundle_invocation_count(), 0);
    }
}
