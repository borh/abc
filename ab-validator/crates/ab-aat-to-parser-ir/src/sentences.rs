use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SentenceSegmentation {
    pub schema_version: String,
    pub splitter_id: String,
    pub coordinate_system: String,
    pub coverage: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ParserIrSentence {
    pub id: String,
    pub paragraph_id: String,
    pub span: Value,
    pub node_range: Value,
    pub tags: Vec<String>,
    pub orthographic_annotation_indices: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SentenceProjection {
    pub nodes: Vec<Value>,
    pub paragraphs: Vec<Value>,
    pub segmentation: SentenceSegmentation,
    pub sentences: Vec<ParserIrSentence>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SentenceBounds {
    start: usize,
    end: usize,
}

pub fn segmentation_meta() -> SentenceSegmentation {
    SentenceSegmentation {
        schema_version: "sentence-segmentation-v1".to_owned(),
        splitter_id: "ab-plaintext-japanese-v1".to_owned(),
        coordinate_system: "decoded_utf8".to_owned(),
        coverage: "body-paragraphs".to_owned(),
    }
}

pub fn project_sentences(
    nodes: Vec<Value>,
    paragraphs: Vec<Value>,
    ortho: Option<&crate::ortho_annotations::OrthoAnnotationsBundle>,
) -> Result<SentenceProjection> {
    project_paragraphs(nodes, paragraphs, ortho)
}

fn project_paragraphs(
    nodes: Vec<Value>,
    paragraphs: Vec<Value>,
    ortho: Option<&crate::ortho_annotations::OrthoAnnotationsBundle>,
) -> Result<SentenceProjection> {
    let mut rewritten_nodes = Vec::with_capacity(nodes.len());
    let mut rewritten_paragraphs = Vec::with_capacity(paragraphs.len());
    let mut sentences = Vec::new();
    let mut original_cursor = 0usize;

    for paragraph in paragraphs {
        let range = node_range(&paragraph, "paragraph node_range")?;
        if range.0 < original_cursor || range.1 > nodes.len() || range.0 > range.1 {
            bail!(
                "paragraph {} node_range {}..{} is not in source node order",
                paragraph_id(&paragraph),
                range.0,
                range.1
            );
        }

        copy_nodes(&nodes, original_cursor, range.0, &mut rewritten_nodes);
        let rewritten_start = rewritten_nodes.len();
        let paragraph_sentences = if paragraph.get("role").and_then(Value::as_str) == Some("body") {
            project_body_paragraph(
                &nodes[range.0..range.1],
                &mut rewritten_nodes,
                &paragraph,
                rewritten_start,
                sentences.len(),
                ortho,
            )?
        } else {
            copy_nodes(&nodes, range.0, range.1, &mut rewritten_nodes);
            Vec::new()
        };
        let rewritten_end = rewritten_nodes.len();

        let mut rewritten_paragraph = paragraph;
        set_node_range(&mut rewritten_paragraph, rewritten_start, rewritten_end)?;
        rewritten_paragraphs.push(rewritten_paragraph);
        sentences.extend(paragraph_sentences);
        original_cursor = range.1;
    }

    copy_nodes(&nodes, original_cursor, nodes.len(), &mut rewritten_nodes);

    Ok(SentenceProjection {
        nodes: rewritten_nodes,
        paragraphs: rewritten_paragraphs,
        segmentation: segmentation_meta(),
        sentences,
    })
}

fn project_body_paragraph(
    original_nodes: &[Value],
    rewritten_nodes: &mut Vec<Value>,
    paragraph: &Value,
    paragraph_rewritten_start: usize,
    sentence_index_start: usize,
    ortho: Option<&crate::ortho_annotations::OrthoAnnotationsBundle>,
) -> Result<Vec<ParserIrSentence>> {
    let paragraph_start = value_usize(paragraph, "/span/start", "paragraph span.start")?;
    let paragraph_end = value_usize(paragraph, "/span/end", "paragraph span.end")?;
    let paragraph_text = paragraph_visible_text(original_nodes)?;
    let bounds: Vec<SentenceBounds> = ab_plaintext::sentence_split(&paragraph_text)
        .into_iter()
        .map(|span| SentenceBounds {
            start: paragraph_start + span.byte_offset,
            end: paragraph_start + span.byte_offset + span.text.len(),
        })
        .collect();

    let split_boundaries: Vec<usize> = bounds
        .iter()
        .take(bounds.len().saturating_sub(1))
        .map(|sentence| sentence.end)
        .collect();

    for node in original_nodes {
        let segments = split_node_at_boundaries(node, &split_boundaries)?;
        rewritten_nodes.extend(segments);
    }

    let paragraph_rewritten_end = rewritten_nodes.len();
    if bounds.is_empty() && paragraph_start == paragraph_end {
        return Ok(Vec::new());
    }

    assert_sentence_span_tiling(paragraph_start, paragraph_end, &bounds)?;

    let mut rows = Vec::with_capacity(bounds.len());
    let mut node_cursor = paragraph_rewritten_start;
    for (local_index, bounds) in bounds.iter().enumerate() {
        let sentence_node_start = node_cursor;
        while node_cursor < paragraph_rewritten_end
            && node_belongs_to_sentence(&rewritten_nodes[node_cursor], *bounds)?
        {
            node_cursor += 1;
        }
        let sentence_node_end = node_cursor;
        let annotation_indices = overlapping_ortho_indices(bounds.start, bounds.end, ortho);
        let tags = if annotation_indices.is_empty() {
            Vec::new()
        } else {
            vec!["orthographic-katakana".to_owned()]
        };

        rows.push(ParserIrSentence {
            id: format!("s{:06}", sentence_index_start + local_index),
            paragraph_id: paragraph_id(paragraph),
            span: decoded_span(bounds.start, bounds.end),
            node_range: json!({
                "start": sentence_node_start,
                "end": sentence_node_end,
            }),
            tags,
            orthographic_annotation_indices: annotation_indices,
        });
    }

    assert_sentence_node_tiling(paragraph_rewritten_start, paragraph_rewritten_end, &rows)?;

    Ok(rows)
}

fn copy_nodes(nodes: &[Value], start: usize, end: usize, out: &mut Vec<Value>) {
    out.extend(nodes[start..end].iter().cloned());
}

fn node_range(value: &Value, context: &str) -> Result<(usize, usize)> {
    Ok((
        value_usize(value, "/node_range/start", &format!("{context}.start"))?,
        value_usize(value, "/node_range/end", &format!("{context}.end"))?,
    ))
}

fn range_value(value: &Value, context: &str) -> Result<(usize, usize)> {
    Ok((
        value_usize(value, "/start", &format!("{context}.start"))?,
        value_usize(value, "/end", &format!("{context}.end"))?,
    ))
}

fn set_node_range(paragraph: &mut Value, start: usize, end: usize) -> Result<()> {
    let object = paragraph
        .as_object_mut()
        .context("paragraph row is not an object")?;
    object.insert(
        "node_range".to_owned(),
        json!({
            "start": start,
            "end": end,
        }),
    );
    Ok(())
}

fn value_usize(value: &Value, pointer: &str, context: &str) -> Result<usize> {
    value
        .pointer(pointer)
        .and_then(Value::as_u64)
        .map(|value| value as usize)
        .with_context(|| format!("{context} missing unsigned integer"))
}

fn paragraph_id(paragraph: &Value) -> String {
    paragraph
        .get("id")
        .and_then(Value::as_str)
        .unwrap_or("<missing-paragraph-id>")
        .to_owned()
}

fn paragraph_visible_text(nodes: &[Value]) -> Result<String> {
    let mut text = String::new();
    for node in nodes {
        text.push_str(&parser_ir_node_visible_text(node)?);
    }
    Ok(text)
}

pub(crate) fn parser_ir_node_visible_text(node: &Value) -> Result<String> {
    let node_type = node_type(node);
    match node_type {
        "text" | "quote" | "emphasis" | "layout-span" | "heading" | "source-note" => Ok(node
            .get("text")
            .and_then(Value::as_str)
            .unwrap_or("")
            .to_owned()),
        "ruby" => Ok(node
            .pointer("/ruby/base")
            .and_then(Value::as_str)
            .unwrap_or("")
            .to_owned()),
        "gaiji" => Ok(node
            .pointer("/gaiji/unicode")
            .and_then(Value::as_str)
            .or_else(|| node.pointer("/gaiji/raw_marker").and_then(Value::as_str))
            .unwrap_or("")
            .to_owned()),
        "line-break" => Ok("\n".to_owned()),
        "page-break" | "image" | "editor-note" | "indentation" => Ok(String::new()),
        other => bail!("unsupported parser-IR node type for sentence projection: {other}"),
    }
}

fn node_type(node: &Value) -> &str {
    node.get("type")
        .and_then(Value::as_str)
        .unwrap_or("unknown")
}

fn split_node_at_boundaries(node: &Value, boundaries: &[usize]) -> Result<Vec<Value>> {
    let start = value_usize(node, "/span/start", "node span.start")?;
    let end = value_usize(node, "/span/end", "node span.end")?;
    let interior: Vec<usize> = boundaries
        .iter()
        .copied()
        .filter(|boundary| start < *boundary && *boundary < end)
        .collect();

    if interior.is_empty() {
        return Ok(vec![node.clone()]);
    }

    if !is_splittable_text_node(node) {
        bail!(
            "sentence boundary falls inside atomic node {} at byte {}",
            node_type(node),
            interior[0]
        );
    }

    let text = node
        .get("text")
        .and_then(Value::as_str)
        .context("splittable node missing text")?;
    let mut segment_starts = Vec::with_capacity(interior.len() + 1);
    let mut segment_ends = Vec::with_capacity(interior.len() + 1);
    segment_starts.push(start);
    segment_starts.extend(interior.iter().copied());
    segment_ends.extend(interior.iter().copied());
    segment_ends.push(end);

    let mut segments = Vec::with_capacity(segment_ends.len());
    for (segment_start, segment_end) in segment_starts.into_iter().zip(segment_ends) {
        let local_start = segment_start - start;
        let local_end = segment_end - start;
        if !text.is_char_boundary(local_start) || !text.is_char_boundary(local_end) {
            bail!(
                "sentence boundary falls outside UTF-8 character boundary in node {} at byte {}",
                node_type(node),
                segment_start
            );
        }
        let mut segment = node.clone();
        set_span(&mut segment, segment_start, segment_end)?;
        let object = segment
            .as_object_mut()
            .context("node row is not an object")?;
        object.insert("text".to_owned(), json!(&text[local_start..local_end]));
        segments.push(segment);
    }

    Ok(segments)
}

fn is_splittable_text_node(node: &Value) -> bool {
    matches!(
        node_type(node),
        "text" | "quote" | "emphasis" | "layout-span"
    ) && node.get("inline_children").is_none()
}

fn set_span(node: &mut Value, start: usize, end: usize) -> Result<()> {
    let span = node
        .get_mut("span")
        .and_then(Value::as_object_mut)
        .context("node span is not an object")?;
    span.insert("start".to_owned(), json!(start));
    span.insert("end".to_owned(), json!(end));
    Ok(())
}

fn node_belongs_to_sentence(node: &Value, sentence: SentenceBounds) -> Result<bool> {
    let node_start = value_usize(node, "/span/start", "node span.start")?;
    let node_end = value_usize(node, "/span/end", "node span.end")?;
    if node_start == node_end {
        return Ok(sentence.start <= node_start && node_start <= sentence.end);
    }
    Ok(sentence.start <= node_start && node_end <= sentence.end)
}

fn decoded_span(start: usize, end: usize) -> Value {
    json!({
        "start": start,
        "end": end,
        "coordinate_system": "decoded_utf8",
    })
}

fn overlapping_ortho_indices(
    start: usize,
    end: usize,
    ortho: Option<&crate::ortho_annotations::OrthoAnnotationsBundle>,
) -> Vec<usize> {
    ortho
        .into_iter()
        .flat_map(|bundle| bundle.annotations.iter().enumerate())
        .filter_map(|(idx, ann)| {
            (ann.source_byte_range.start < end && start < ann.source_byte_range.end).then_some(idx)
        })
        .collect()
}

fn assert_sentence_span_tiling(
    paragraph_start: usize,
    paragraph_end: usize,
    sentences: &[SentenceBounds],
) -> Result<()> {
    if sentences.is_empty() {
        if paragraph_start == paragraph_end {
            return Ok(());
        }
        bail!("body paragraph has no sentence spans for non-empty byte span");
    }

    let mut expected_start = paragraph_start;
    for sentence in sentences {
        if sentence.start != expected_start {
            bail!(
                "sentence spans do not tile paragraph: got start {}, expected {}",
                sentence.start,
                expected_start
            );
        }
        expected_start = sentence.end;
    }
    if expected_start != paragraph_end {
        bail!(
            "sentence spans end at {}, expected paragraph end {}",
            expected_start,
            paragraph_end
        );
    }
    Ok(())
}

fn assert_sentence_node_tiling(
    paragraph_start: usize,
    paragraph_end: usize,
    sentences: &[ParserIrSentence],
) -> Result<()> {
    let mut expected_start = paragraph_start;
    for sentence in sentences {
        let (start, end) = range_value(&sentence.node_range, "sentence node_range")?;
        if start != expected_start {
            bail!(
                "sentence node ranges do not tile paragraph: got start {}, expected {}",
                start,
                expected_start
            );
        }
        expected_start = end;
    }
    if expected_start != paragraph_end {
        bail!(
            "sentence node ranges end at {}, expected paragraph end {}",
            expected_start,
            paragraph_end
        );
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn ortho_bundle() -> crate::ortho_annotations::OrthoAnnotationsBundle {
        serde_json::from_value(json!({
            "work_id": "000000",
            "work_content_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
            "coordinate_system": "decoded_utf8",
            "detector_id": "HeuristicV1",
            "annotations": [{
                "source_byte_range": { "start": 0, "end": 24 },
                "normalized_text": "吾輩は猫である。",
                "kind": "ScriptKatakanaToHiragana",
                "confidence": null
            }]
        }))
        .unwrap()
    }

    fn span(start: usize, end: usize) -> serde_json::Value {
        json!({"start": start, "end": end, "coordinate_system": "decoded_utf8"})
    }

    #[test]
    fn splits_single_text_node_into_sentence_nodes_and_rows() {
        let nodes = vec![json!({
            "type":"text",
            "span": span(0, 48),
            "text":"吾輩ハ猫デアル。名前はまだ無い。"
        })];
        let paragraphs = vec![json!({
            "id":"p000000",
            "span": span(0, 48),
            "span_source":"direct",
            "node_range":{"start":0,"end":1},
            "role":"body",
            "source_pointer":"blocks[0]",
            "classification":"direct"
        })];

        let projection = project_sentences(nodes, paragraphs, None).unwrap();

        assert_eq!(projection.nodes.len(), 2);
        assert_eq!(projection.nodes[0]["text"], "吾輩ハ猫デアル。");
        assert_eq!(projection.nodes[0]["span"], span(0, 24));
        assert_eq!(projection.nodes[1]["text"], "名前はまだ無い。");
        assert_eq!(projection.nodes[1]["span"], span(24, 48));
        assert_eq!(
            projection.paragraphs[0]["node_range"],
            json!({"start":0,"end":2})
        );
        assert_eq!(projection.sentences.len(), 2);
        assert_eq!(
            projection.sentences[0].node_range,
            json!({"start":0,"end":1})
        );
        assert_eq!(
            projection.sentences[1].node_range,
            json!({"start":1,"end":2})
        );
    }

    #[test]
    fn rewrites_later_paragraph_ranges_after_node_split() {
        let nodes = vec![
            json!({"type":"text","span":span(0,48),"text":"吾輩ハ猫デアル。名前はまだ無い。"}),
            json!({"type":"text","span":span(48,63),"text":"後続段落。"}),
        ];
        let paragraphs = vec![
            json!({"id":"p000000","span":span(0,48),"span_source":"direct","node_range":{"start":0,"end":1},"role":"body","source_pointer":"blocks[0]","classification":"direct"}),
            json!({"id":"p000001","span":span(48,63),"span_source":"direct","node_range":{"start":1,"end":2},"role":"body","source_pointer":"blocks[1]","classification":"direct"}),
        ];

        let projection = project_sentences(nodes, paragraphs, None).unwrap();

        assert_eq!(projection.nodes.len(), 3);
        assert_eq!(
            projection.paragraphs[0]["node_range"],
            json!({"start":0,"end":2})
        );
        assert_eq!(
            projection.paragraphs[1]["node_range"],
            json!({"start":2,"end":3})
        );
        assert_eq!(projection.sentences[2].paragraph_id, "p000001");
        assert_eq!(
            projection.sentences[2].node_range,
            json!({"start":2,"end":3})
        );
    }

    #[test]
    fn partial_ortho_overlap_tags_every_overlapping_sentence() {
        let nodes = vec![json!({
            "type":"text",
            "span": span(0, 48),
            "text":"吾輩ハ猫デアル。名前はまだ無い。"
        })];
        let paragraphs = vec![
            json!({"id":"p000000","span":span(0,48),"span_source":"direct","node_range":{"start":0,"end":1},"role":"body","source_pointer":"blocks[0]","classification":"direct"}),
        ];
        let mut bundle = ortho_bundle();
        bundle.annotations[0].source_byte_range = 20..30;

        let projection = project_sentences(nodes, paragraphs, Some(&bundle)).unwrap();

        assert_eq!(projection.sentences[0].tags, vec!["orthographic-katakana"]);
        assert_eq!(
            projection.sentences[0].orthographic_annotation_indices,
            vec![0]
        );
        assert_eq!(projection.sentences[1].tags, vec!["orthographic-katakana"]);
        assert_eq!(
            projection.sentences[1].orthographic_annotation_indices,
            vec![0]
        );
    }

    #[test]
    fn uses_ruby_base_for_spans_and_splits_following_text() {
        let nodes = vec![
            json!({"type":"ruby","span":span(0,6),"ruby":{"base":"名前","reading":"めいしょう","scope":"explicit"}}),
            json!({"type":"text","span":span(6,39),"text":"はまだ無い。ここは次。"}),
        ];
        let paragraphs = vec![
            json!({"id":"p000000","span":span(0,39),"span_source":"direct","node_range":{"start":0,"end":2},"role":"body","source_pointer":"blocks[0]","classification":"direct"}),
        ];

        let projection = project_sentences(nodes, paragraphs, None).unwrap();

        assert_eq!(projection.nodes.len(), 3);
        assert_eq!(projection.sentences.len(), 2);
        assert_eq!(projection.sentences[0].span, span(0, 24));
        assert_eq!(
            projection.sentences[0].node_range,
            json!({"start":0,"end":2})
        );
        assert_eq!(projection.sentences[1].span, span(24, 39));
        assert_eq!(
            projection.sentences[1].node_range,
            json!({"start":2,"end":3})
        );
    }

    #[test]
    fn splits_layout_span_without_inline_children() {
        let nodes = vec![json!({
            "type":"layout-span",
            "span":span(0,12),
            "text":"甲。乙。",
            "layout":{"kind":"jitai"}
        })];
        let paragraphs = vec![
            json!({"id":"p000000","span":span(0,12),"span_source":"direct","node_range":{"start":0,"end":1},"role":"body","source_pointer":"blocks[0]","classification":"direct"}),
        ];

        let projection = project_sentences(nodes, paragraphs, None).unwrap();

        assert_eq!(projection.nodes.len(), 2);
        assert_eq!(projection.nodes[0]["type"], "layout-span");
        assert_eq!(projection.nodes[0]["text"], "甲。");
        assert_eq!(projection.nodes[1]["text"], "乙。");
    }

    #[test]
    fn rejects_boundary_inside_atomic_inline_children_node() {
        let nodes = vec![json!({
            "type":"emphasis",
            "span":span(0,12),
            "text":"甲。乙。",
            "style":"bold",
            "inline_children":[{"type":"text","span":span(0,12),"text":"甲。乙。"}]
        })];
        let paragraphs = vec![
            json!({"id":"p000000","span":span(0,12),"span_source":"direct","node_range":{"start":0,"end":1},"role":"body","source_pointer":"blocks[0]","classification":"direct"}),
        ];

        let error = project_sentences(nodes, paragraphs, None)
            .unwrap_err()
            .to_string();
        assert!(error.contains("sentence boundary falls inside atomic node emphasis at byte 6"));
    }

    #[test]
    fn rejects_boundary_inside_atomic_ruby_node() {
        let nodes = vec![json!({
            "type":"ruby",
            "span":span(0,48),
            "ruby":{"base":"吾輩ハ猫デアル。名前ハマダ無イ。","reading":"わがはいはねこであるなまえはまだない","scope":"explicit"}
        })];
        let paragraphs = vec![
            json!({"id":"p000000","span":span(0,48),"span_source":"direct","node_range":{"start":0,"end":1},"role":"body","source_pointer":"blocks[0]","classification":"direct"}),
        ];

        let error = project_sentences(nodes, paragraphs, None)
            .unwrap_err()
            .to_string();
        assert!(error.contains("sentence boundary falls inside atomic node ruby at byte 24"));
    }

    #[test]
    fn zero_width_body_nodes_do_not_require_sentence_rows() {
        let nodes = vec![json!({
            "type":"page-break",
            "span":span(0,0),
            "marker":"page",
            "page_number":null
        })];
        let paragraphs = vec![json!({
            "id":"p000000",
            "span":span(0,0),
            "span_source":"direct",
            "node_range":{"start":0,"end":1},
            "role":"body",
            "source_pointer":"blocks[0]",
            "classification":"direct"
        })];

        let projection = project_sentences(nodes, paragraphs, None).unwrap();

        assert_eq!(projection.nodes.len(), 1);
        assert!(projection.sentences.is_empty());
        assert_eq!(
            projection.paragraphs[0]["node_range"],
            json!({"start":0,"end":1})
        );
    }
}
