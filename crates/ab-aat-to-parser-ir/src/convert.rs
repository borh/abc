use std::collections::BTreeSet;

use anyhow::{Result, bail};
use serde_json::{Value, json};

use crate::{
    divergence::{AatMeta, DivergenceRecorder},
    mapping::{MappingDocument, MappingIndex},
    schema::{SchemaSet, validate_value},
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
}

impl Default for ConversionOptions {
    fn default() -> Self {
        Self {
            validate_input_aat: true,
            validate_output_parser_ir: true,
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
    index: MappingIndex,
}

impl PreparedConverter {
    pub fn new(mapping: MappingDocument, schemas: SchemaSet) -> Result<Self> {
        let index = mapping.preflight(&schemas)?;
        Ok(Self {
            mapping,
            schemas,
            index,
        })
    }

    pub fn convert(&self, aat: Value, options: ConversionOptions) -> Result<ConversionOutput> {
        convert_preflighted(
            aat,
            &self.mapping,
            &self.schemas,
            self.index.clone(),
            options,
        )
    }
}

pub fn convert(request: ConversionRequest) -> Result<ConversionOutput> {
    PreparedConverter::new(request.mapping, request.schemas)?.convert(request.aat, request.options)
}

fn convert_preflighted(
    aat: Value,
    mapping: &MappingDocument,
    schemas: &SchemaSet,
    index: MappingIndex,
    options: ConversionOptions,
) -> Result<ConversionOutput> {
    if options.validate_input_aat {
        ab_check::check::validate_aat_value(&aat)?;
    }

    let mut recorder = DivergenceRecorder::new(index);
    let mut nodes = Vec::new();
    let mut paragraphs = Vec::new();
    let mut offset = 0_u64;

    let blocks: Vec<&Value> = aat
        .pointer("/blocks")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .collect();
    let top_level_block_count = blocks.len();
    for (block_index, block) in blocks.into_iter().enumerate() {
        offset = map_block(
            block,
            &mut nodes,
            &mut paragraphs,
            &mut recorder,
            offset,
            &format!("blocks[{block_index}]"),
            None,
            block_index + 1 == top_level_block_count,
        )?;
    }

    let source = map_source(&aat, &mut recorder)?;
    recorder.record("INVENTION", None, Some("schema_id/schema_hash"), None, None)?;
    let warnings = map_warnings(&aat, &mut recorder)?;
    recorder.record("INVENTION", None, Some("errors[]"), None, None)?;

    let parser_ir = json!({
        "schema_id": mapping.target_parser_ir_schema_id,
        "schema_hash": mapping.target_parser_ir_schema_hash,
        "derived_from": derived_from(&aat, mapping)?,
        "source": source,
        "nodes": nodes,
        "paragraphs": paragraphs,
        "warnings": warnings,
        "errors": [],
    });

    if options.validate_output_parser_ir {
        validate_value(&schemas.parser_ir_schema, &parser_ir, "parser-IR")?;
    }

    let emitted_rule_ids = recorder.emitted_rule_ids();
    let divergence_bundle = recorder.bundle(aat_meta(&aat), schemas, mapping)?;
    Ok(ConversionOutput {
        parser_ir,
        divergence_bundle,
        emitted_rule_ids,
    })
}

fn map_block(
    block: &Value,
    nodes: &mut Vec<Value>,
    paragraphs: &mut Vec<Value>,
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
                nodes.push(json!({
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
            let paragraph_id = format!("p{:06}", paragraphs.len());
            let node_start = nodes.len();
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
                nodes.push(json!({
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
                    nodes,
                    recorder,
                    current,
                    &paragraph_content_path,
                )?;
                role = "body";
                classification = "direct";
            }
            let node_end = nodes.len();
            let (span, span_source) = paragraph_span(
                block.get("span"),
                nodes,
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
            paragraphs.push(paragraph);
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
            let text = visible_content_text(
                block.get("content"),
                recorder,
                &format!("{path}.heading.content"),
                Some("(emphasis.text)"),
            )?;
            let end = current + utf8_len(&text);
            let span = map_span(block.get("span"), current, end, recorder, path)?;
            nodes.push(json!({
                "type": "heading",
                "span": span,
                "text": text,
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
                        nodes,
                        paragraphs,
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
                nodes.push(json!({
                    "type": "indentation",
                    "span": span,
                    "depth": 1,
                    "text": null,
                }));
                for (index, child) in children.into_iter().enumerate() {
                    current = map_block(
                        child,
                        nodes,
                        paragraphs,
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
                    nodes,
                    paragraphs,
                    recorder,
                    current,
                    &format!("{path}.children[{index}]"),
                    None,
                    false,
                )?;
            }
        }
        "quote_block" | "caption_block" => {
            if recorder.has_rule("STRUCTURAL", Some(structural_pointer.as_str()), None) {
                recorder.record(
                    "STRUCTURAL",
                    Some(structural_pointer.as_str()),
                    None,
                    None,
                    None,
                )?;
            }
            for (index, child) in block["children"]
                .as_array()
                .into_iter()
                .flatten()
                .enumerate()
            {
                current = map_block(
                    child,
                    nodes,
                    paragraphs,
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
    if let Some(span) = block_span {
        return Ok((
            json!({
                "start": span.get("byte_start").and_then(Value::as_u64).unwrap_or(fallback_start),
                "end": span.get("byte_end").and_then(Value::as_u64).unwrap_or(fallback_end),
                "line": span.get("line_start").cloned().unwrap_or(Value::Null),
                "column": null,
                "coordinate_system": "decoded_utf8",
            }),
            "direct",
        ));
    }
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
        let line = first.pointer("/span/line").cloned().unwrap_or(Value::Null);
        let column = first
            .pointer("/span/column")
            .cloned()
            .unwrap_or(Value::Null);
        return Ok((
            json!({
                "start": start,
                "end": end,
                "line": line,
                "column": column,
                "coordinate_system": "decoded_utf8",
            }),
            "derived",
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
    for child in content.and_then(Value::as_array).into_iter().flatten() {
        text.push_str(&plain_visible_inline_text(child)?);
    }
    Ok(text)
}

fn plain_visible_inline_text(node: &Value) -> Result<String> {
    match node["kind"].as_str().unwrap_or("") {
        "text" => Ok(node["value"].as_str().unwrap_or("").to_owned()),
        "ruby" => Ok(node["base"].as_str().unwrap_or("").to_owned()),
        "gaiji" => Ok(node["resolved"]
            .as_str()
            .or_else(|| node["description"].as_str())
            .unwrap_or("")
            .to_owned()),
        "style" | "font_size" | "tcy" | "keigakomi" | "caption" => {
            plain_visible_content_text(node.get("content"))
        }
        "warigaki" => {
            let upper = plain_visible_content_text(node.get("upper"))?;
            let lower = plain_visible_content_text(node.get("lower"))?;
            Ok(format!("{upper}{lower}"))
        }
        "raw" => Ok(String::new()),
        "figure" => Ok(node["alt"].as_str().unwrap_or("").to_owned()),
        other => bail!("unsupported inline kind in source attribution projection: {other}"),
    }
}

fn map_inline_content(
    content: Option<&Value>,
    nodes: &mut Vec<Value>,
    recorder: &mut DivergenceRecorder,
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
        current =
            map_inline_to_nodes(child, nodes, recorder, current, &format!("{path}[{index}]"))?;
    }
    Ok(current)
}

fn map_inline_to_nodes(
    node: &Value,
    nodes: &mut Vec<Value>,
    recorder: &mut DivergenceRecorder,
    offset: u64,
    path: &str,
) -> Result<u64> {
    match node["kind"].as_str().unwrap_or("") {
        "text" => {
            if is_source_derived_line_break_text(node) {
                return map_source_derived_line_break_text(node, nodes, recorder, offset, path);
            }
            let text = node["value"].as_str().unwrap_or("");
            let end = offset + utf8_len(text);
            let span = map_span(node.get("span"), offset, end, recorder, path)?;
            nodes.push(json!({"type": "text", "span": span, "text": text}));
            Ok(end)
        }
        "ruby" => {
            let base = node["base"].as_str().unwrap_or("");
            let end = offset + utf8_len(base);
            let span = map_span(node.get("span"), offset, end, recorder, path)?;
            recorder.record(
                "INVENTION",
                None,
                Some("ruby.scope"),
                None,
                Some(json!("explicit")),
            )?;
            let base_content_pointer = format!("{path}.ruby.base_content");
            if node.get("base_content").is_some()
                && recorder.has_rule("LOSS", Some(base_content_pointer.as_str()), None)
            {
                recorder.record(
                    "LOSS",
                    Some(base_content_pointer.as_str()),
                    None,
                    None,
                    None,
                )?;
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
            let span = map_span(node.get("span"), offset, end, recorder, path)?;
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
            let jis_pointer = format!("{path}.gaiji.jis_code");
            if node.get("jis_code").is_some_and(|value| !value.is_null())
                && recorder.has_rule(
                    "AMBIGUITY",
                    Some(jis_pointer.as_str()),
                    Some("gaiji.reference"),
                )
            {
                recorder.record(
                    "AMBIGUITY",
                    Some(jis_pointer.as_str()),
                    Some("gaiji.reference"),
                    node.get("jis_code").cloned(),
                    node.get("jis_code").cloned(),
                )?;
            }
            let unresolved_pointer = format!("{path}.gaiji.unresolved_reason");
            if node
                .get("unresolved_reason")
                .is_some_and(|value| !value.is_null())
                && recorder.has_rule("LOSS", Some(unresolved_pointer.as_str()), None)
            {
                recorder.record(
                    "LOSS",
                    Some(unresolved_pointer.as_str()),
                    None,
                    node.get("unresolved_reason").cloned(),
                    None,
                )?;
            }
            recorder.record("LOSS", None, Some("gaiji.unicode"), None, Some(Value::Null))?;
            nodes.push(json!({
                "type": "gaiji",
                "span": span,
                "gaiji": {
                    "raw_marker": node["description"].as_str().unwrap_or(""),
                    "reference": node.get("jis_code").cloned().unwrap_or(Value::Null),
                    "unicode": null,
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
            let span = map_span(node.get("span"), offset, end, recorder, path)?;
            nodes.push(json!({
                "type": "emphasis",
                "span": span,
                "text": text,
                "style": node.get("style_type").and_then(Value::as_str).unwrap_or("style"),
            }));
            Ok(end)
        }
        "warigaki" => map_warigaki_to_nodes(node, nodes, recorder, offset, path),
        "figure" => map_figure_to_node(node, nodes, recorder, offset, path),
        "raw" => map_raw_to_nodes(node, nodes, recorder, offset, path),
        "font_size" | "tcy" | "keigakomi" | "caption" => {
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
            let span = map_span(node.get("span"), offset, end, recorder, path)?;
            nodes.push(json!({
                "type": "emphasis",
                "span": span,
                "text": text,
                "style": kind,
            }));
            Ok(end)
        }
        other => bail!("unsupported inline kind: {other}"),
    }
}

fn map_raw_to_nodes(
    node: &Value,
    nodes: &mut Vec<Value>,
    recorder: &mut DivergenceRecorder,
    offset: u64,
    path: &str,
) -> Result<u64> {
    let raw_pointer = format!("{path}.raw");
    if !recorder.has_rule("UNSUPPORTED", Some(raw_pointer.as_str()), None) {
        bail!("unmeasured raw divergence at {raw_pointer}");
    }
    recorder.record(
        "UNSUPPORTED",
        Some(raw_pointer.as_str()),
        None,
        node.get("source").cloned(),
        None,
    )?;

    let source = node.get("source").and_then(Value::as_str).unwrap_or("");
    match raw_recovery_class(node, source) {
        RawRecoveryClass::PageBreak => {
            let span = map_span(node.get("span"), offset, offset, recorder, path)?;
            nodes.push(json!({
                "type": "page-break",
                "span": span,
                "marker": "page",
                "page_number": null,
            }));
        }
        RawRecoveryClass::SourceNote => {
            let span = map_span(node.get("span"), offset, offset, recorder, path)?;
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

    let span = map_span(node.get("span"), offset, offset, recorder, path)?;
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
        current,
        &format!("{path}.warigaki.upper"),
    )?;
    map_inline_content(
        node.get("lower"),
        nodes,
        recorder,
        current,
        &format!("{path}.warigaki.lower"),
    )
}

fn visible_content_text(
    content: Option<&Value>,
    recorder: &mut DivergenceRecorder,
    path: &str,
    target_pointer: Option<&str>,
) -> Result<String> {
    let mut text = String::new();
    for (index, child) in content
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .enumerate()
    {
        text.push_str(&visible_inline_text(
            child,
            recorder,
            &format!("{path}[{index}]"),
            target_pointer,
        )?);
    }
    Ok(text)
}

fn visible_inline_text(
    node: &Value,
    recorder: &mut DivergenceRecorder,
    path: &str,
    target_pointer: Option<&str>,
) -> Result<String> {
    match node["kind"].as_str().unwrap_or("") {
        "text" => Ok(node["value"].as_str().unwrap_or("").to_owned()),
        "ruby" => {
            let container_pointer = format!("{path}.ruby");
            record_measured_loss(
                recorder,
                container_pointer.as_str(),
                None,
                Some(json!("ruby")),
            )?;
            let reading_pointer = format!("{path}.ruby.reading");
            if recorder.has_rule("LOSS", Some(reading_pointer.as_str()), target_pointer) {
                recorder.record(
                    "LOSS",
                    Some(reading_pointer.as_str()),
                    target_pointer,
                    node.get("reading").cloned(),
                    None,
                )?;
            }
            Ok(node["base"].as_str().unwrap_or("").to_owned())
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
            if recorder.has_rule("AMBIGUITY", Some(resolved_pointer.as_str()), target_pointer) {
                recorder.record(
                    "AMBIGUITY",
                    Some(resolved_pointer.as_str()),
                    target_pointer,
                    node.get("resolved").cloned(),
                    node.get("resolved").cloned(),
                )?;
            }
            Ok(node["resolved"]
                .as_str()
                .or_else(|| node["description"].as_str())
                .unwrap_or("")
                .to_owned())
        }
        "style" | "font_size" | "tcy" | "keigakomi" | "caption" => {
            let kind = node["kind"].as_str().unwrap_or("");
            let container_pointer = format!("{path}.{kind}");
            record_measured_loss(
                recorder,
                container_pointer.as_str(),
                None,
                Some(json!(kind)),
            )?;
            visible_content_text(
                node.get("content"),
                recorder,
                &format!("{path}.content"),
                target_pointer,
            )
        }
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
            let upper = visible_content_text(
                node.get("upper"),
                recorder,
                &format!("{path}.warigaki.upper"),
                target_pointer,
            )?;
            let lower = visible_content_text(
                node.get("lower"),
                recorder,
                &format!("{path}.warigaki.lower"),
                target_pointer,
            )?;
            Ok(format!("{upper}{lower}"))
        }
        "raw" => {
            let raw_pointer = format!("{path}.raw");
            record_measured_loss(
                recorder,
                raw_pointer.as_str(),
                None,
                node.get("source").cloned(),
            )?;
            if recorder.has_rule("LOSS", Some(raw_pointer.as_str()), target_pointer) {
                recorder.record(
                    "LOSS",
                    Some(raw_pointer.as_str()),
                    target_pointer,
                    None,
                    None,
                )?;
            }
            Ok(String::new())
        }
        "figure" => {
            let figure_pointer = format!("{path}.figure");
            if recorder.has_rule("LOSS", Some(figure_pointer.as_str()), target_pointer) {
                recorder.record(
                    "LOSS",
                    Some(figure_pointer.as_str()),
                    target_pointer,
                    None,
                    None,
                )?;
            }
            Ok(node["alt"].as_str().unwrap_or("").to_owned())
        }
        other => bail!("unsupported inline kind in visible projection: {other}"),
    }
}

fn record_measured_loss(
    recorder: &mut DivergenceRecorder,
    aat_pointer: &str,
    parser_ir_pointer: Option<&str>,
    source_value: Option<Value>,
) -> Result<()> {
    if recorder.has_rule("LOSS", Some(aat_pointer), parser_ir_pointer) {
        recorder.record(
            "LOSS",
            Some(aat_pointer),
            parser_ir_pointer,
            source_value,
            None,
        )?;
    }
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

fn map_span(
    aat_span: Option<&Value>,
    fallback_start: u64,
    fallback_end: u64,
    recorder: &mut DivergenceRecorder,
    path: &str,
) -> Result<Value> {
    let Some(span) = aat_span else {
        let span_pointer = format!("{path}.span");
        if recorder.has_rule("AMBIGUITY", Some(span_pointer.as_str()), Some("span")) {
            recorder.record(
                "AMBIGUITY",
                Some(span_pointer.as_str()),
                Some("span"),
                None,
                None,
            )?;
        }
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
    for field in ["parse_complete"] {
        let field_pointer = format!("meta.{field}");
        recorder.record(
            "LOSS",
            Some(field_pointer.as_str()),
            None,
            meta.get(field).cloned(),
            None,
        )?;
    }
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

fn aat_meta(aat: &Value) -> AatMeta {
    let meta = &aat["meta"];
    AatMeta {
        work_id: aat["work_id"].as_str().unwrap_or("unknown").to_owned(),
        version: aat["version"].as_u64().unwrap_or(1),
        adapter: meta["adapter"].as_str().unwrap_or("unknown").to_owned(),
        adapter_version: meta["adapter_version"]
            .as_str()
            .unwrap_or("unknown")
            .to_owned(),
        source_hash: meta["source_hash"]
            .as_str()
            .unwrap_or("sha256:0000000000000000000000000000000000000000000000000000000000000000")
            .to_owned(),
        parse_complete: meta["parse_complete"].as_bool().unwrap_or(false),
        metrics: meta.get("metrics").cloned().unwrap_or(Value::Null),
        semantic_summary: meta.get("semantic_summary").cloned().unwrap_or(Value::Null),
    }
}

fn utf8_len(value: &str) -> u64 {
    value.len() as u64
}
