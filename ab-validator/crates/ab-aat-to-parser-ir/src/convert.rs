use std::{collections::BTreeSet, sync::Arc};

use anyhow::{Result, bail};
use serde_json::{Value, json};

use crate::{
    divergence::{AatMeta, DivergenceRecorder},
    mapping::{MappingDocument, MappingIndex},
    ortho_annotations::OrthoAnnotationsBundle,
    schema::{SchemaSet, SchemaValidators, validate_compiled},
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
}

impl Default for ConversionOptions {
    fn default() -> Self {
        Self {
            validate_input_aat: true,
            validate_output_parser_ir: true,
            orthographic_annotations: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConversionOutput {
    pub parser_ir: Value,
    pub divergence_bundle: Value,
    pub emitted_rule_ids: BTreeSet<String>,
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
        convert_preflighted(
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

fn convert_preflighted(
    mut aat: Value,
    mapping: &MappingDocument,
    schemas: &SchemaSet,
    validators: &SchemaValidators,
    index: Arc<MappingIndex>,
    options: ConversionOptions,
) -> Result<ConversionOutput> {
    if options.validate_input_aat {
        validate_compiled(&validators.aat, &aat, "AAT")?;
    }

    let mut recorder = DivergenceRecorder::new(index);
    let mut nodes = Vec::new();
    let mut paragraphs = Vec::new();
    let mut synthetic_warnings = Vec::new();
    let mut offset = 0_u64;

    {
        let mut block_outputs = BlockOutputs {
            nodes: &mut nodes,
            paragraphs: &mut paragraphs,
            synthetic_warnings: &mut synthetic_warnings,
        };

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
            )?;
        }
    }

    let source = map_source(&aat, &mut recorder)?;
    recorder.record("INVENTION", None, Some("schema_id/schema_hash"), None, None)?;
    let mut warnings = map_warnings(&aat, &mut recorder)?;
    warnings
        .as_array_mut()
        .expect("warnings array")
        .append(&mut synthetic_warnings);
    recorder.record("INVENTION", None, Some("errors[]"), None, None)?;

    let orthographic_annotations = options.orthographic_annotations;
    if let Some(orthographic_annotations) = orthographic_annotations.as_ref() {
        ensure_schema_declares_orthographic_annotations(&schemas.parser_ir_schema)?;
        orthographic_annotations.validate_against_aat(&aat)?;
    }

    let mut sentence_segmentation = None;
    let mut sentences = None;
    if schema_declares_sentence_segmentation(&schemas.parser_ir_schema) {
        let sentence_projection = crate::sentences::project_sentences(
            nodes,
            paragraphs,
            orthographic_annotations.as_ref(),
        )?;
        nodes = sentence_projection.nodes;
        paragraphs = sentence_projection.paragraphs;
        sentence_segmentation = Some(sentence_projection.segmentation);
        sentences = Some(sentence_projection.sentences);
    }

    let mut parser_ir = json!({
        "schema_id": mapping.target_parser_ir_schema_id,
        "schema_hash": mapping.target_parser_ir_schema_hash,
        "derived_from": derived_from(&aat, mapping)?,
        "source": source,
        "nodes": nodes,
        "paragraphs": paragraphs,
        "warnings": warnings,
        "errors": [],
    });

    let parser_ir_object = parser_ir.as_object_mut().expect("parser_ir is an object");
    if let Some(sentence_segmentation) = sentence_segmentation {
        parser_ir_object.insert(
            "sentence_segmentation".to_owned(),
            serde_json::to_value(sentence_segmentation)?,
        );
    }
    if let Some(sentences) = sentences {
        parser_ir_object.insert("sentences".to_owned(), serde_json::to_value(sentences)?);
    }
    if let Some(orthographic_annotations) = orthographic_annotations {
        parser_ir_object.insert(
            "orthographic_annotations".to_owned(),
            serde_json::to_value(&orthographic_annotations)?,
        );
    }

    if options.validate_output_parser_ir {
        validate_compiled(&validators.parser_ir, &parser_ir, "parser-IR")?;
    }

    let emitted_rule_ids = recorder.emitted_rule_ids();
    let divergence_bundle = recorder.bundle(aat_meta(&mut aat), validators, mapping)?;
    Ok(ConversionOutput {
        parser_ir,
        divergence_bundle,
        emitted_rule_ids,
    })
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

fn schema_declares_sentence_segmentation(schema: &Value) -> bool {
    schema
        .pointer("/properties/sentence_segmentation")
        .is_some()
        && schema.pointer("/properties/sentences").is_some()
}

struct BlockOutputs<'a> {
    nodes: &'a mut Vec<Value>,
    paragraphs: &'a mut Vec<Value>,
    synthetic_warnings: &'a mut Vec<Value>,
}

fn map_block(
    block: &Value,
    outputs: &mut BlockOutputs<'_>,
    recorder: &mut DivergenceRecorder,
    offset: u64,
    path: &str,
    inherited_layout: Option<Value>,
    is_final_top_level: bool,
) -> Result<u64> {
    let kind = block["kind"].as_str().unwrap_or("unknown");
    let structural_pointer = format!("{path}.{kind}");
    let mut current = offset;
    match kind {
        "paragraph" => {
            if is_source_derived_page_break(block) {
                let span = map_span(block.get("span"), current, current, recorder, path)?;
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
            let source_note_text = if is_final_top_level {
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
                let span = map_span(block.get("span"), current, end, recorder, path)?;
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
            let (span, span_source) = paragraph_span(
                block.get("span"),
                outputs.nodes,
                node_start,
                node_end,
                offset,
                current,
            )?;
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
            let span = map_span(block.get("span"), current, end, recorder, path)?;
            outputs.nodes.push(json!({
                "type": "heading",
                "span": span,
                "text": text,
                "inline_children": inline_children,
                "level": block.get("level").and_then(Value::as_u64).unwrap_or(1),
            }));
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
                for (index, child) in children.into_iter().enumerate() {
                    current = map_block(
                        child,
                        outputs,
                        recorder,
                        current,
                        &format!("{path}.children[{index}]"),
                        Some(layout.clone()),
                        false,
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
                let span = map_span(block.get("span"), current, current, recorder, path)?;
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
                    )?;
                }
            }
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

fn paragraph_layout_from_style(node: &Value) -> Option<Value> {
    match node.get("style_type").and_then(Value::as_str)? {
        "burasage" => Some(json!({
            "kind": "burasage",
            "source": "aat-style",
            "first_line_indent": node.get("x-indent-first")?.as_u64()?,
            "continuation_indent": node.get("x-indent-rest")?.as_u64()?,
        })),
        "chitsuki" => {
            let align = node
                .get("x-align")
                .and_then(Value::as_str)
                .unwrap_or("right");
            if align != "right" {
                return None;
            }
            Some(json!({
                "kind": "chitsuki",
                "source": "aat-style",
                "align": align,
                "offset_from_end": node.get("x-offset")?.as_u64()?,
            }))
        }
        "jisage" => Some(json!({
            "kind": "jisage",
            "source": "aat-style",
            "indent": node.get("x-indent")?.as_u64()?,
        })),
        "jizume" => Some(json!({
            "kind": "jizume",
            "source": "aat-style",
            "width": node.get("x-width")?.as_u64()?,
        })),
        "line-jisage" | "jisage_line" => Some(json!({
            "kind": "line-jisage",
            "source": "aat-style",
            "indent": node.get("x-indent")?.as_u64()?,
        })),
        _ => None,
    }
}

fn paragraph_layout_from_jisage_block(block: &Value) -> Value {
    json!({
        "kind": "jisage",
        "source": "aat-block",
        "indent": block.get("x-indent").and_then(Value::as_u64).unwrap_or(1),
    })
}

fn paragraph_span(
    block_span: Option<&Value>,
    nodes: &[Value],
    node_start: usize,
    node_end: usize,
    fallback_start: u64,
    fallback_end: u64,
) -> Result<(Value, &'static str)> {
    // Derive the paragraph span from its child node spans, which are in decoded
    // coordinates. This is the ONLY source consistent with `paragraph_start`/`_end`
    // as used by the sentence splitter — the AAT block `byte_start`/`byte_end` are
    // raw-source offsets (they include ruby/gaiji markup) and the `current`
    // accumulator can overshoot the last visible node, both of which desynchronise
    // the paragraph span from its nodes. The block span, when present, only supplies
    // `line` provenance and marks the span_source as "direct".
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
        let (line, column, span_source) = if let Some(span) = block_span {
            (
                span.get("line_start").cloned().unwrap_or(Value::Null),
                Value::Null,
                "direct",
            )
        } else {
            (
                first.pointer("/span/line").cloned().unwrap_or(Value::Null),
                first
                    .pointer("/span/column")
                    .cloned()
                    .unwrap_or(Value::Null),
                "derived",
            )
        };
        return Ok((
            json!({
                "start": start,
                "end": end,
                "line": line,
                "column": column,
                "coordinate_system": "decoded_utf8",
            }),
            span_source,
        ));
    }
    Ok((
        json!({
            "start": fallback_start,
            "end": fallback_end,
            "line": null,
            "column": null,
            "coordinate_system": "decoded_utf8",
        }),
        "synthesized",
    ))
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

fn append_plain_visible_inline_text(node: &Value, out: &mut String) -> Result<()> {
    match node["kind"].as_str().unwrap_or("") {
        "text" => out.push_str(node["value"].as_str().unwrap_or("")),
        "ruby" => out.push_str(node["base"].as_str().unwrap_or("")),
        "gaiji" => out.push_str(
            node["resolved"]
                .as_str()
                .or_else(|| node["description"].as_str())
                .unwrap_or(""),
        ),
        "accent" => out.push_str(node["resolved"].as_str().unwrap_or("")),
        "style" | "font_size" | "tcy" | "keigakomi" | "caption" | "yokogumi" => {
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
        current = map_inline_to_nodes(
            child,
            nodes,
            recorder,
            synthetic_warnings,
            current,
            &format!("{path}[{index}]"),
        )?;
    }
    Ok(current)
}

/// Emit the parser-IR text node for an AAT `text` inline, preserving its
/// source span. Handles source-derived line-break text. Behavior-preserving
/// extraction of the former `"text"` arm of [`map_inline_to_nodes`].
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
            let base = node["base"].as_str().unwrap_or("");
            let end = offset + utf8_len(base);
            let span = map_node_span(node.get("span"), offset, end, recorder, path)?;
            recorder.record(
                "INVENTION",
                None,
                Some("ruby.scope"),
                None,
                Some(json!("explicit")),
            )?;
            if node.get("base_content").is_some() {
                let base_content_pointer = format!("{path}.ruby.base_content");
                recorder.record_if_measured(
                    "LOSS",
                    Some(base_content_pointer.as_str()),
                    None,
                    None,
                    None,
                );
            }
            nodes.push(json!({
                "type": "ruby",
                "span": span,
                "ruby": {
                    "base": base,
                    "reading": node["reading"].as_str().unwrap_or(""),
                    "scope": "explicit",
                    "direction": node.get("direction").and_then(Value::as_str)
                }
            }));
            Ok(end)
        }
        "gaiji" => {
            let visible = node["resolved"]
                .as_str()
                .or_else(|| node["description"].as_str())
                .unwrap_or("");
            let end = offset + utf8_len(visible);
            let span = map_node_span(node.get("span"), offset, end, recorder, path)?;
            let unicode = node.get("resolved").cloned().unwrap_or(Value::Null);
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
            if unicode.is_null() {
                recorder.record("LOSS", None, Some("gaiji.unicode"), None, Some(Value::Null))?;
            }
            nodes.push(json!({
                "type": "gaiji",
                "span": span,
                "gaiji": {
                    "raw_marker": node["description"].as_str().unwrap_or(""),
                    "reference": node.get("jis_code").cloned().unwrap_or(Value::Null),
                    "unicode": unicode,
                    "ivs": null,
                    "image_or_glyph_fallback": null,
                    "resolved": node.get("resolved").is_some_and(|value| !value.is_null())
                }
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
            Ok(end)
        }
        "accent" => map_accent_to_node(node, nodes, recorder, offset, path),
        "warigaki" => {
            map_warigaki_to_nodes(node, nodes, recorder, synthetic_warnings, offset, path)
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
        current = inline_child_node(
            child,
            &mut nodes,
            recorder,
            synthetic_warnings,
            current,
            &format!("{path}[{index}]"),
            depth,
        )?;
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
            let base = node["base"].as_str().unwrap_or("");
            let end = offset + utf8_len(base);
            nodes.push(json!({
                "type": "ruby",
                "span": synthetic_span(offset, end),
                "ruby": {
                    "base": base,
                    "reading": node["reading"].as_str().unwrap_or(""),
                    "scope": "explicit",
                    "direction": node.get("direction").and_then(Value::as_str)
                }
            }));
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
                "gaiji": {
                    "raw_marker": node["description"].as_str().unwrap_or(""),
                    "reference": node.get("jis_code").cloned().unwrap_or(Value::Null),
                    "unicode": node.get("resolved").cloned().unwrap_or(Value::Null),
                    "ivs": null,
                    "image_or_glyph_fallback": null,
                    "resolved": node.get("resolved").is_some_and(|value| !value.is_null())
                }
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
        "warigaki" => {
            let warigaki_pointer = format!("{path}.warigaki");
            let target = warigaki_target(recorder, &warigaki_pointer)?;
            recorder.record(
                "UNSUPPORTED",
                Some(warigaki_pointer.as_str()),
                target,
                Some(json!("warigaki")),
                None,
            )?;
            let upper = inline_children_nodes(
                node.get("upper"),
                recorder,
                synthetic_warnings,
                offset,
                &format!("{path}.warigaki.upper"),
                depth + 1,
            )?;
            let upper_text = plain_visible_content_text(node.get("upper"))?;
            let lower_offset = offset + utf8_len(&upper_text);
            let lower = inline_children_nodes(
                node.get("lower"),
                recorder,
                synthetic_warnings,
                lower_offset,
                &format!("{path}.warigaki.lower"),
                depth + 1,
            )?;
            nodes.extend(upper);
            nodes.extend(lower);
            let lower_text = plain_visible_content_text(node.get("lower"))?;
            Ok(lower_offset + utf8_len(&lower_text))
        }
        "figure" => {
            let alt = node["alt"].as_str().unwrap_or("");
            push_unrecorded_text_node(alt, nodes, offset)
        }
        "raw" => Ok(offset),
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
    json!({
        "start": start,
        "end": end,
        "line": null,
        "column": null,
        "coordinate_system": "decoded_utf8"
    })
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

fn map_raw_to_nodes(
    node: &Value,
    nodes: &mut Vec<Value>,
    recorder: &mut DivergenceRecorder,
    offset: u64,
    path: &str,
) -> Result<u64> {
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
            nodes.push(json!({
                "type": "editor-note",
                "span": span,
                "note": {
                    "raw": source,
                    "category": "misc",
                },
            }));
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
            let span = map_span(None, current, end, recorder, path)?;
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
    let span = map_span(None, offset, end, recorder, path)?;
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
) -> Result<u64> {
    let warigaki_pointer = format!("{path}.warigaki");
    let target = warigaki_target(recorder, &warigaki_pointer)?;
    recorder.record(
        "UNSUPPORTED",
        Some(warigaki_pointer.as_str()),
        target,
        None,
        None,
    )?;
    let mut current = offset;
    current = map_inline_content(
        node.get("upper"),
        nodes,
        recorder,
        synthetic_warnings,
        current,
        &format!("{path}.warigaki.upper"),
    )?;
    map_inline_content(
        node.get("lower"),
        nodes,
        recorder,
        synthetic_warnings,
        current,
        &format!("{path}.warigaki.lower"),
    )
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
            out.push_str(node["base"].as_str().unwrap_or(""));
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
        "font_size" | "tcy" | "keigakomi" | "yokogumi" => append_visible_content_text(
            node.get("content"),
            recorder,
            &format!("{path}.content"),
            target_pointer,
            out,
        )?,
        "warigaki" => {
            let warigaki_pointer = format!("{path}.warigaki");
            let target = match warigaki_target(recorder, &warigaki_pointer) {
                Ok(target) => target,
                Err(_) => target_pointer,
            };
            recorder.record(
                "UNSUPPORTED",
                Some(warigaki_pointer.as_str()),
                target,
                None,
                None,
            )?;
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

fn warigaki_target<'a>(
    recorder: &DivergenceRecorder,
    warigaki_pointer: &str,
) -> Result<Option<&'a str>> {
    if recorder.has_rule("UNSUPPORTED", Some(warigaki_pointer), None) {
        Ok(None)
    } else if recorder.has_rule(
        "UNSUPPORTED",
        Some(warigaki_pointer),
        Some("(emphasis.text)"),
    ) {
        Ok(Some("(emphasis.text)"))
    } else {
        bail!("unmeasured warigaki divergence at {warigaki_pointer}")
    }
}

/// Build a parser-IR node span in the pipeline's `decoded_utf8` coordinate system.
///
/// Unlike [`map_span`], this IGNORES the AAT `byte_start`/`byte_end` for the span
/// value and uses the caller's accumulated decoded offsets `[start, end)`. The AAT
/// offsets are raw-source bytes that include Aozora markup (ruby `《…》`, gaiji /
/// bouten `［＃…］`), so they run ahead of the decoded stream and must NOT be used
/// as `decoded_utf8` span values — doing so is what desynchronised container /
/// ruby / gaiji node spans from their (decoded) siblings and children and made the
/// sentence projection fail or crash on ruby-heavy corpora. The AAT `line_start`
/// is kept for provenance; the AMBIGUITY divergence for a missing span is still
/// recorded, matching `map_span`. Inline children already build spans this way via
/// `synthetic_span`; this is the top-level equivalent that also carries `line`.
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
    Ok(json!({
        "start": start,
        "end": end,
        "line": aat_span
            .and_then(|s| s.get("line_start"))
            .cloned()
            .unwrap_or(Value::Null),
        "column": null,
        "coordinate_system": "decoded_utf8",
    }))
}

fn map_span(
    aat_span: Option<&Value>,
    fallback_start: u64,
    fallback_end: u64,
    recorder: &mut DivergenceRecorder,
    path: &str,
) -> Result<Value> {
    let Some(span) = aat_span else {
        let span_pointer = format!("{path}.span");
        recorder.record_if_measured(
            "AMBIGUITY",
            Some(span_pointer.as_str()),
            Some("span"),
            None,
            None,
        );
        return Ok(json!({
            "start": fallback_start,
            "end": fallback_end,
            "line": null,
            "column": null,
            "coordinate_system": "decoded_utf8",
        }));
    };
    Ok(json!({
        "start": span.get("byte_start").and_then(Value::as_u64).unwrap_or(fallback_start),
        "end": span.get("byte_end").and_then(Value::as_u64).unwrap_or(fallback_end),
        "line": span.get("line_start").cloned().unwrap_or(Value::Null),
        "column": null,
        "coordinate_system": "decoded_utf8",
    }))
}

fn derived_from(aat: &Value, mapping: &MappingDocument) -> Result<Value> {
    let meta = &aat["meta"];
    let Some(aat_version) = aat["version"].as_u64() else {
        bail!("AAT version is required for parser-IR derived_from");
    };
    let Some(adapter) = meta["adapter"].as_str() else {
        bail!("AAT meta.adapter is required for parser-IR derived_from");
    };
    Ok(json!({
        "aat_version": aat_version,
        "aat_adapter": adapter,
        "aat_adapter_version": meta.get("adapter_version").and_then(Value::as_str),
        "mapping_id": mapping.mapping_id,
        "mapping_version": mapping.mapping_version,
        "mapping_schema_hash": mapping.mapping_schema_hash,
    }))
}

fn map_source(aat: &Value, recorder: &mut DivergenceRecorder) -> Result<Value> {
    let meta = &aat["meta"];
    let source_encoding = meta["source_encoding"].as_str().unwrap_or("utf-8");
    let encoding = match source_encoding {
        "utf-8" | "utf-8-bom" => "UTF-8",
        "windows-31j" | "windows-31j-lossy" => "Shift_JIS",
        _ => "unknown",
    };
    if source_encoding == "windows-31j-lossy" {
        let encoding_pointer = format!("meta.source_encoding={source_encoding}");
        recorder.record(
            "AMBIGUITY",
            Some(encoding_pointer.as_str()),
            Some("source.encoding"),
            Some(json!(source_encoding)),
            Some(json!(encoding)),
        )?;
    }
    recorder.record(
        "AMBIGUITY",
        Some("meta.source_hash"),
        Some("source.work_content_hash"),
        meta.get("source_hash").cloned(),
        meta.get("source_hash").cloned(),
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
    let field = "parse_complete";
    let field_pointer = format!("meta.{field}");
    recorder.record(
        "LOSS",
        Some(field_pointer.as_str()),
        None,
        meta.get(field).cloned(),
        None,
    )?;
    if meta.get("metrics").is_some() {
        recorder.record("LOSS", Some("meta.metrics"), None, None, None)?;
    }
    if meta.get("semantic_summary").is_some() {
        recorder.record("LOSS", Some("meta.semantic_summary"), None, None, None)?;
    }
    Ok(json!({
        "work_content_hash": meta["source_hash"].as_str().unwrap_or("sha256:0000000000000000000000000000000000000000000000000000000000000000"),
        "source_path": null,
        "encoding": encoding,
        "normalization": "source",
    }))
}

fn map_warnings(aat: &Value, recorder: &mut DivergenceRecorder) -> Result<Value> {
    let mut warnings = Vec::new();
    for warning in aat
        .pointer("/meta/warnings")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
    {
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
            Some(json!("AAT_WARNING")),
        )?;
        warnings.push(json!({
            "severity": "warning",
            "code": "AAT_WARNING",
            "message": warning["message"].as_str().unwrap_or(""),
            "span": null,
            "construct": warning.get("path").cloned().unwrap_or(Value::Null),
            "recovery": null,
        }));
    }
    Ok(Value::Array(warnings))
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
