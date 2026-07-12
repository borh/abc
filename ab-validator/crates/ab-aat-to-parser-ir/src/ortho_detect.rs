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
    let sentences = output
        .parser_ir
        .get("sentences")
        .and_then(Value::as_array)
        .context("parser-IR missing sentences[]")?;

    let mut owned_sentence_text = Vec::with_capacity(sentences.len());
    let mut sentence_offsets = Vec::with_capacity(sentences.len());
    let mut sentence_char_offsets = Vec::with_capacity(sentences.len());
    let mut next_char_offset = 0usize;
    for sentence in sentences {
        let start = sentence
            .pointer("/node_range/start")
            .and_then(Value::as_u64)
            .context("sentence missing node_range.start")? as usize;
        let end = sentence
            .pointer("/node_range/end")
            .and_then(Value::as_u64)
            .context("sentence missing node_range.end")? as usize;
        if start > end || end > nodes.len() {
            bail!(
                "sentence node_range {}..{} is out of bounds for {} parser-IR nodes",
                start,
                end,
                nodes.len()
            );
        }
        let byte_offset = sentence
            .pointer("/span/start")
            .and_then(Value::as_u64)
            .context("sentence missing span.start")? as usize;
        let text = nodes[start..end]
            .iter()
            .map(crate::sentences::parser_ir_node_visible_text)
            .collect::<Result<Vec<_>>>()?
            .concat();
        sentence_offsets.push(byte_offset);
        sentence_char_offsets.push(next_char_offset);
        next_char_offset += text.chars().count();
        owned_sentence_text.push(text);
    }

    let spans = owned_sentence_text
        .iter()
        .zip(sentence_offsets.iter().zip(sentence_char_offsets.iter()))
        .map(|(text, (byte_offset, char_offset))| SentenceSpan {
            text,
            byte_offset: *byte_offset,
            char_offset: *char_offset,
        })
        .collect::<Vec<_>>();

    Ok(OrthoAnnotationsBundle {
        work_id,
        primary_text_hash,
        coordinate_system: OrthoCoordinateSystem::DecodedUtf8,
        detector_id: detector.detector_id(),
        annotations: detector.detect(&spans),
    })
}
