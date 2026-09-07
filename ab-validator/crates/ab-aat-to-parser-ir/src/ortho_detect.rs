use ab_ortho_detect::OrthoDetector;
use ab_plaintext::SentenceSpan;
use anyhow::{Context, Result, bail};
use serde_json::Value;

use crate::{
    ConversionOptions, MappingDocument, PreparedConverter, SchemaSet,
    ortho_annotations::{OrthoAnnotationsBundle, OrthoCoordinateSystem},
};

pub fn detect_orthographic_annotations(
    aat: Value,
    mapping: MappingDocument,
    schemas: SchemaSet,
    detector: &dyn OrthoDetector,
) -> Result<OrthoAnnotationsBundle> {
    let work_id = aat
        .get("work_id")
        .and_then(Value::as_str)
        .context("AAT missing string work_id")?
        .to_owned();
    let primary_text_hash = aat
        .pointer("/meta/primary_text_hash")
        .or_else(|| aat.pointer("/meta/source_hash"))
        .and_then(Value::as_str)
        .context("AAT missing string meta.source_hash")?
        .to_owned();

    let output =
        PreparedConverter::new(mapping, schemas)?.convert(aat, ConversionOptions::default())?;

    let nodes = output
        .parser_ir
        .get("nodes")
        .and_then(Value::as_array)
        .context("parser-IR missing nodes[]")?;
    let paragraphs = output
        .parser_ir
        .get("paragraphs")
        .and_then(Value::as_array)
        .context("parser-IR missing paragraphs[]")?;
    let mut paragraph_texts = Vec::new();
    let mut byte_offsets = Vec::new();
    for paragraph in paragraphs {
        if paragraph.get("role").and_then(Value::as_str) != Some("body") {
            continue;
        }
        let start = paragraph
            .pointer("/node_range/start")
            .and_then(Value::as_u64)
            .context("paragraph missing node_range.start")? as usize;
        let end = paragraph
            .pointer("/node_range/end")
            .and_then(Value::as_u64)
            .context("paragraph missing node_range.end")? as usize;
        if start > end || end > nodes.len() {
            bail!(
                "paragraph node_range {start}..{end} is out of bounds for {} parser-IR nodes",
                nodes.len()
            );
        }
        let byte_offset = paragraph
            .pointer("/span/start")
            .and_then(Value::as_u64)
            .context("paragraph missing span.start")? as usize;
        paragraph_texts.push(
            nodes[start..end]
                .iter()
                .map(crate::content::parser_ir_node_visible_text)
                .collect::<Result<Vec<_>>>()?
                .concat(),
        );
        byte_offsets.push(byte_offset);
    }
    let mut spans = Vec::new();
    let mut next_char_offset = 0usize;
    for (text, byte_offset) in paragraph_texts.iter().zip(byte_offsets) {
        spans.extend(
            ab_plaintext::split_sentences(text)
                .into_iter()
                .map(|span| SentenceSpan {
                    text: span.text,
                    byte_offset: byte_offset + span.byte_offset,
                    char_offset: next_char_offset + span.char_offset,
                }),
        );
        next_char_offset += text.chars().count();
    }

    Ok(OrthoAnnotationsBundle {
        work_id,
        primary_text_hash,
        coordinate_system: OrthoCoordinateSystem::ParserTextUtf8,
        detector_id: detector.detector_id(),
        annotations: detector.detect(&spans),
    })
}
