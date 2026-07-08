use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

/// Maximum quote-nesting depth the detector will recurse into. Real literary
/// text rarely exceeds 2-3 levels; the cap guards against runaway recursion on
/// malformed marker sequences.
const MAX_NESTING_DEPTH: usize = 5;

/// A region of text enclosed by a matching pair of synthesized `quote` nodes
/// (an `open` marker followed by its matching `close`).
///
/// `inner_byte_start`/`inner_byte_end` bound the text strictly between the
/// markers (exclusive of the marker glyphs). `outer_byte_start`/`outer_byte_end`
/// bound the markers themselves so that framing-punctuation redistribution can
/// attach the open marker to the first inner sentence and the close marker to
/// the last inner sentence. All offsets are absolute `decoded_utf8` byte
/// offsets.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct NestedRegion {
    pub inner_byte_start: usize,
    pub inner_byte_end: usize,
    pub outer_byte_start: usize,
    pub outer_byte_end: usize,
    pub opening_marker_node: usize,
    pub closing_marker_node: usize,
    pub nesting_level: usize,
}

/// Detect all quote-enclosed regions in `nodes[range_start..range_end]`,
/// recursing into each matched pair so nested quotes are reported at their own
/// (deeper) nesting level. Unmatched `open` markers produce no region.

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
    #[serde(skip_serializing_if = "Option::is_none")]
    pub part: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fragment_group: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub next_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub prev_id: Option<String>,
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

struct FineBound {
    bounds: SentenceBounds,
    group: Option<usize>,
    outer_pos: Option<usize>,
}

struct AtomicSpan {
    start: usize,
    end: usize,
}

pub fn segmentation_meta() -> SentenceSegmentation {
    SentenceSegmentation {
        schema_version: "sentence-segmentation-v1".to_owned(),
        splitter_id: "ab-plaintext-japanese-v2".to_owned(),
        coordinate_system: "decoded_utf8".to_owned(),
        coverage: "body-paragraphs".to_owned(),
    }
}

pub(crate) fn detect_nested_regions(
    nodes: &[Value],
    node_start: usize,
    node_end: usize,
) -> Vec<NestedRegion> {
    detect_nested_regions_inner(nodes, node_start, node_end, 0)
}

fn detect_nested_regions_inner(
    nodes: &[Value],
    range_start: usize,
    range_end: usize,
    depth: usize,
) -> Vec<NestedRegion> {
    if depth >= MAX_NESTING_DEPTH {
        return Vec::new();
    }

    let mut regions = Vec::new();
    let mut i = range_start;

    while i < range_end {
        let node = &nodes[i];
        if node.get("type").and_then(Value::as_str) != Some("quote")
            || node.get("marker_type").and_then(Value::as_str) != Some("open")
        {
            i += 1;
            continue;
        }

        let open_byte_start = value_usize(node, "/span/start", "quote span.start").unwrap_or(0);
        let open_byte_end = value_usize(node, "/span/end", "quote span.end").unwrap_or(0);
        let open_node = i;

        // Find the matching close at the same depth: count intervening opens.
        let mut j = i + 1;
        let mut inner_depth = 0usize;
        let mut matched = false;
        while j < range_end {
            let next = &nodes[j];
            if next.get("type").and_then(Value::as_str) == Some("quote") {
                match next.get("marker_type").and_then(Value::as_str) {
                    Some("open") => inner_depth += 1,
                    Some("close") => {
                        if inner_depth == 0 {
                            let close_byte_start =
                                value_usize(next, "/span/start", "quote span.start").unwrap_or(0);
                            let close_byte_end =
                                value_usize(next, "/span/end", "quote span.end").unwrap_or(0);
                            let close_node = j;

                            regions.push(NestedRegion {
                                inner_byte_start: open_byte_end,
                                inner_byte_end: close_byte_start,
                                outer_byte_start: open_byte_start,
                                outer_byte_end: close_byte_end,
                                opening_marker_node: open_node,
                                closing_marker_node: close_node,
                                nesting_level: depth,
                            });

                            // Recurse on the inner node slice for nested quotes.
                            regions.extend(detect_nested_regions_inner(
                                nodes,
                                open_node + 1,
                                close_node,
                                depth + 1,
                            ));

                            i = close_node + 1;
                            matched = true;
                            break;
                        }
                        inner_depth -= 1;
                    }
                    _ => {}
                }
            }
            j += 1;
        }

        if !matched {
            // Unmatched open marker — skip it, no region emitted.
            i += 1;
        }
    }

    regions
}

pub fn project_sentences(
    nodes: Vec<Value>,
    paragraphs: Vec<Value>,
    ortho: Option<&crate::ortho_annotations::OrthoAnnotationsBundle>,
) -> Result<SentenceProjection> {
    project_paragraphs(nodes, paragraphs, ortho)
}

fn project_paragraphs(
    mut nodes: Vec<Value>,
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

        move_nodes(&mut nodes, original_cursor, range.0, &mut rewritten_nodes);
        let rewritten_start = rewritten_nodes.len();
        let paragraph_sentences = if paragraph.get("role").and_then(Value::as_str) == Some("body") {
            project_body_paragraph(
                &mut nodes[range.0..range.1],
                &mut rewritten_nodes,
                &paragraph,
                rewritten_start,
                sentences.len(),
                ortho,
            )?
        } else {
            move_nodes(&mut nodes, range.0, range.1, &mut rewritten_nodes);
            Vec::new()
        };
        let rewritten_end = rewritten_nodes.len();

        let mut rewritten_paragraph = paragraph;
        set_node_range(&mut rewritten_paragraph, rewritten_start, rewritten_end)?;
        rewritten_paragraphs.push(rewritten_paragraph);
        sentences.extend(paragraph_sentences);
        original_cursor = range.1;
    }

    let remaining = nodes.len();
    move_nodes(&mut nodes, original_cursor, remaining, &mut rewritten_nodes);

    Ok(SentenceProjection {
        nodes: rewritten_nodes,
        paragraphs: rewritten_paragraphs,
        segmentation: segmentation_meta(),
        sentences,
    })
}

fn project_body_paragraph(
    original_nodes: &mut [Value],
    rewritten_nodes: &mut Vec<Value>,
    paragraph: &Value,
    paragraph_rewritten_start: usize,
    sentence_index_start: usize,
    ortho: Option<&crate::ortho_annotations::OrthoAnnotationsBundle>,
) -> Result<Vec<ParserIrSentence>> {
    let paragraph_start = value_usize(paragraph, "/span/start", "paragraph span.start")?;
    let paragraph_end = value_usize(paragraph, "/span/end", "paragraph span.end")?;
    let paragraph_text = paragraph_visible_text(original_nodes)?;

    debug_assert_eq!(
        paragraph_text.len(),
        (paragraph_end - paragraph_start) as usize,
        "visible text length must equal byte span length"
    );

    // --- Phase 0: flat sentence split (default rules; quotes suppress splits). ---
    let mut flat_bounds: Vec<SentenceBounds> = ab_plaintext::split_sentences(&paragraph_text)
        .into_iter()
        .map(|span| SentenceBounds {
            start: paragraph_start + span.byte_offset,
            end: paragraph_start + span.byte_offset + span.text.len(),
        })
        .collect();

    // Whitespace handling (B2): the splitter drops leading/trailing whitespace-only
    // runs. Absorb those so a non-empty body paragraph tiles with no gaps.
    if flat_bounds.is_empty() {
        if paragraph_start != paragraph_end {
            flat_bounds.push(SentenceBounds {
                start: paragraph_start,
                end: paragraph_end,
            });
        }
    } else {
        flat_bounds.first_mut().unwrap().start = paragraph_start;
        flat_bounds.last_mut().unwrap().end = paragraph_end;
    }

    // --- Phase 1: nesting detection over the whole paragraph's nodes. ---
    // A quote region may cross a flat splitter boundary when its closing marker
    // appears after a newline. Coalesce those flat bounds before assigning
    // regions to fragment groups. Only level-0 regions drive fragmentation in
    // v1; deeper nesting is left as inner-sentence text (future <q> work).
    let regions = detect_nested_regions(original_nodes, 0, original_nodes.len());
    let top_regions: Vec<&NestedRegion> = regions.iter().filter(|r| r.nesting_level == 0).collect();
    coalesce_flat_bounds_around_regions(&mut flat_bounds, &top_regions)?;

    // Assign each region to its containing flat sentence; allocate ONE fragment
    // group per flat sentence that owns >=1 region.
    let mut flat_group: Vec<Option<usize>> = vec![None; flat_bounds.len()];
    let mut group_count = 0usize;
    for region in &top_regions {
        let fi = flat_bounds
            .iter()
            .position(|f| f.start <= region.outer_byte_start && region.outer_byte_end <= f.end)
            .with_context(|| {
                format!(
                    "nested region {}..{} is not contained in any flat sentence",
                    region.outer_byte_start, region.outer_byte_end
                )
            })?;
        if flat_group[fi].is_none() {
            flat_group[fi] = Some(group_count);
            group_count += 1;
        }
    }

    // --- Phase 2: build fine-grained bounds tiling the paragraph. ---
    // Framing-punctuation redistribution: the open marker is attached to the
    // first inner sentence, the close marker to the last inner sentence (so
    // `<s>「...」</s>` keeps its brackets). Outer fragments are the prose before
    // the open marker and after the close marker.
    let mut fine: Vec<FineBound> = Vec::new();
    let mut outer_count_by_group: Vec<usize> = vec![0; group_count];

    for (fi, flat) in flat_bounds.iter().enumerate() {
        let group = flat_group[fi];
        let sentence_regions: Vec<&NestedRegion> = top_regions
            .iter()
            .copied()
            .filter(|r| flat.start <= r.outer_byte_start && r.outer_byte_end <= flat.end)
            .collect();

        if group.is_none() || sentence_regions.is_empty() {
            fine.push(FineBound {
                bounds: *flat,
                group: None,
                outer_pos: None,
            });
            continue;
        }

        let group = group.unwrap();
        let mut cursor = flat.start;
        for region in &sentence_regions {
            // Outer fragment before the opening marker (part I / M).
            if cursor < region.outer_byte_start {
                let pos = outer_count_by_group[group];
                outer_count_by_group[group] += 1;
                fine.push(FineBound {
                    bounds: SentenceBounds {
                        start: cursor,
                        end: region.outer_byte_start,
                    },
                    group: Some(group),
                    outer_pos: Some(pos),
                });
            }

            // Inner sentences: re-split the between-marker text with bracket
            // suppression off, then extend the first span to the open marker and
            // the last span to the close marker so the brackets are enclosed.
            let inner_start_local = region.inner_byte_start - paragraph_start;
            let inner_end_local = region.inner_byte_end - paragraph_start;
            let inner_text = &paragraph_text[inner_start_local..inner_end_local];
            let inner_spans = ab_plaintext::split_sentences_with_options(
                inner_text,
                &ab_plaintext::SplitOptions {
                    suppress_closing_bracket_check: true,
                },
            );
            let n = inner_spans.len();
            let mut prev_end_abs = region.outer_byte_start;
            for (k, span) in inner_spans.iter().enumerate() {
                let end_abs = if k + 1 == n {
                    region.outer_byte_end
                } else {
                    region.inner_byte_start + span.byte_offset + span.text.len()
                };
                fine.push(FineBound {
                    bounds: SentenceBounds {
                        start: prev_end_abs,
                        end: end_abs,
                    },
                    group: None,
                    outer_pos: None,
                });
                prev_end_abs = end_abs;
            }
            // No inner spans (e.g. `「」`): one inner sentence covers the markers.
            if inner_spans.is_empty() {
                fine.push(FineBound {
                    bounds: SentenceBounds {
                        start: region.outer_byte_start,
                        end: region.outer_byte_end,
                    },
                    group: None,
                    outer_pos: None,
                });
            }
            cursor = region.outer_byte_end;
        }
        // Outer fragment after the last region (part F / M).
        if cursor < flat.end {
            let pos = outer_count_by_group[group];
            outer_count_by_group[group] += 1;
            fine.push(FineBound {
                bounds: SentenceBounds {
                    start: cursor,
                    end: flat.end,
                },
                group: Some(group),
                outer_pos: Some(pos),
            });
        }
    }

    coalesce_fine_bounds_around_atomic_nodes(original_nodes, &mut fine)?;

    // --- Phase 3: split nodes at every interior fine boundary in one pass. ---
    let split_boundaries: Vec<usize> = fine
        .iter()
        .take(fine.len().saturating_sub(1))
        .map(|fb| fb.bounds.end)
        .collect();
    for node in original_nodes.iter_mut() {
        split_node_at_boundaries(node, &split_boundaries, rewritten_nodes)?;
    }
    let paragraph_rewritten_end = rewritten_nodes.len();

    if fine.is_empty() && paragraph_start == paragraph_end {
        return Ok(Vec::new());
    }

    let fine_bounds_for_tiling: Vec<SentenceBounds> = fine.iter().map(|fb| fb.bounds).collect();
    assert_sentence_span_tiling(paragraph_start, paragraph_end, &fine_bounds_for_tiling)?;

    // --- Phase 4: resolve fragment fields. Outer fragments in a group form a ---
    // chain I -> M... -> F, each linking to the adjacent outer fragment.
    let mut group_outer_fi: Vec<Vec<usize>> = vec![Vec::new(); group_count];
    for (gi, fb) in fine.iter().enumerate() {
        if let (Some(g), Some(_pos)) = (fb.group, fb.outer_pos) {
            group_outer_fi[g].push(gi);
        }
    }

    // --- Phase 5: walk rewritten nodes and build sentence rows. ---
    let mut rows = Vec::with_capacity(fine.len());
    let mut node_cursor = paragraph_rewritten_start;
    for (gi, fb) in fine.iter().enumerate() {
        let sentence_node_start = node_cursor;
        while node_cursor < paragraph_rewritten_end
            && node_belongs_to_sentence(&rewritten_nodes[node_cursor], fb.bounds)?
        {
            node_cursor += 1;
        }
        let sentence_node_end = node_cursor;
        let annotation_indices = overlapping_ortho_indices(fb.bounds.start, fb.bounds.end, ortho);
        let tags = if annotation_indices.is_empty() {
            Vec::new()
        } else {
            vec!["orthographic-katakana".to_owned()]
        };

        let (part, fragment_group, next_id, prev_id): (
            Option<String>,
            Option<String>,
            Option<String>,
            Option<String>,
        ) = if let (Some(g), Some(pos)) = (fb.group, fb.outer_pos) {
            let count = group_outer_fi[g].len();
            if count < 2 {
                (None, None, None, None)
            } else {
                let part = if pos == 0 {
                    "I"
                } else if pos + 1 == count {
                    "F"
                } else {
                    "M"
                };
                let next_id = (pos + 1 < count)
                    .then(|| format!("s{:06}", sentence_index_start + group_outer_fi[g][pos + 1]));
                let prev_id = (pos > 0)
                    .then(|| format!("s{:06}", sentence_index_start + group_outer_fi[g][pos - 1]));
                (
                    Some(part.to_owned()),
                    Some(format!("fg{:06}", g)),
                    next_id,
                    prev_id,
                )
            }
        } else {
            (None, None, None, None)
        };

        rows.push(ParserIrSentence {
            id: format!("s{:06}", sentence_index_start + gi),
            paragraph_id: paragraph_id(paragraph),
            span: decoded_span(fb.bounds.start, fb.bounds.end),
            node_range: json!({
                "start": sentence_node_start,
                "end": sentence_node_end,
            }),
            tags,
            orthographic_annotation_indices: annotation_indices,
            part,
            fragment_group,
            next_id,
            prev_id,
        });
    }

    assert_sentence_node_tiling(paragraph_rewritten_start, paragraph_rewritten_end, &rows)?;
    assert_fragment_field_coherence(&rows)?;

    Ok(rows)
}

fn coalesce_flat_bounds_around_regions(
    flat_bounds: &mut Vec<SentenceBounds>,
    regions: &[&NestedRegion],
) -> Result<()> {
    for region in regions {
        let first = flat_bounds
            .iter()
            .position(|f| f.end > region.outer_byte_start && f.start < region.outer_byte_end)
            .with_context(|| {
                format!(
                    "nested region {}..{} does not overlap any flat sentence",
                    region.outer_byte_start, region.outer_byte_end
                )
            })?;
        let last = flat_bounds
            .iter()
            .rposition(|f| f.end > region.outer_byte_start && f.start < region.outer_byte_end)
            .expect("first overlap implies last overlap");

        if first == last {
            continue;
        }

        let merged = SentenceBounds {
            start: flat_bounds[first].start,
            end: flat_bounds[last].end,
        };
        flat_bounds.splice(first..=last, [merged]);
    }
    Ok(())
}

fn coalesce_fine_bounds_around_atomic_nodes(
    nodes: &[Value],
    fine: &mut Vec<FineBound>,
) -> Result<()> {
    let mut atomic_spans = Vec::new();
    collect_atomic_spans(nodes, &mut atomic_spans)?;

    let mut i = 0usize;
    while i + 1 < fine.len() {
        let boundary = fine[i].bounds.end;
        if atomic_spans
            .iter()
            .any(|span| span.start < boundary && boundary < span.end)
        {
            fine[i].bounds.end = fine[i + 1].bounds.end;
            fine[i].group = None;
            fine[i].outer_pos = None;
            fine.remove(i + 1);
        } else {
            i += 1;
        }
    }
    Ok(())
}

fn collect_atomic_spans(nodes: &[Value], out: &mut Vec<AtomicSpan>) -> Result<()> {
    for node in nodes {
        if let Some(children) = inline_children(node) {
            collect_atomic_spans(children, out)?;
            continue;
        }
        if is_splittable_text_node(node) || is_splittable_container(node) {
            continue;
        }
        let start = value_usize(node, "/span/start", "node span.start")?;
        let end = value_usize(node, "/span/end", "node span.end")?;
        if start < end {
            out.push(AtomicSpan { start, end });
        }
    }
    Ok(())
}

fn move_nodes(nodes: &mut [Value], start: usize, end: usize, out: &mut Vec<Value>) {
    // Each source node is consumed exactly once (the cursor advances
    // monotonically), so move the nodes out instead of deep-cloning them.
    out.extend(nodes[start..end].iter_mut().map(std::mem::take));
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

/// Authoritative visible-text projection for a parser-IR node.
///
/// For inline containers (`emphasis` / `layout-span` / `heading`) the projection
/// is derived from `inline_children`, not the flat `text`: the schema permits an
/// `emphasis` carrying `inline_children` with no `text`, so the children are the
/// source of truth. When a flat `text` is also present the split path guards that
/// it equals this projection.
pub(crate) fn parser_ir_node_visible_text(node: &Value) -> Result<String> {
    if let Some(children) = inline_children(node) {
        let mut text = String::new();
        for child in children {
            text.push_str(&parser_ir_node_visible_text(child)?);
        }
        return Ok(text);
    }
    let node_type = node_type(node);
    let text = match node_type {
        "text" | "quote" | "emphasis" | "layout-span" | "heading" | "source-note" => {
            node.get("text").and_then(Value::as_str).unwrap_or("")
        }
        "ruby" => node
            .pointer("/ruby/base")
            .and_then(Value::as_str)
            .unwrap_or(""),
        "gaiji" => node
            .pointer("/gaiji/unicode")
            .and_then(Value::as_str)
            .or_else(|| node.pointer("/gaiji/raw_marker").and_then(Value::as_str))
            .unwrap_or(""),
        "line-break" => "\n",
        "page-break" | "image" | "editor-note" | "indentation" => "",
        other => bail!("unsupported parser-IR node type for sentence projection: {other}"),
    };
    Ok(text.to_owned())
}

/// The non-empty `inline_children` array of an inline container, if present.
fn inline_children(node: &Value) -> Option<&Vec<Value>> {
    node.get("inline_children")
        .and_then(Value::as_array)
        .filter(|children| !children.is_empty())
}

fn node_type(node: &Value) -> &str {
    node.get("type")
        .and_then(Value::as_str)
        .unwrap_or("unknown")
}

fn split_node_at_boundaries(
    node: &mut Value,
    boundaries: &[usize],
    out: &mut Vec<Value>,
) -> Result<()> {
    let start = value_usize(node, "/span/start", "node span.start")?;
    let end = value_usize(node, "/span/end", "node span.end")?;
    let interior: Vec<usize> = boundaries
        .iter()
        .copied()
        .filter(|boundary| start < *boundary && *boundary < end)
        .collect();

    if interior.is_empty() {
        out.push(std::mem::take(node));
        return Ok(());
    }

    if is_splittable_container(node) {
        return split_container_at_boundaries(node, &interior, out);
    }

    if !is_splittable_text_node(node) {
        bail!(
            "sentence boundary falls inside atomic node {} at byte {}",
            node_type(node),
            interior[0]
        );
    }

    let mut segment_starts = Vec::with_capacity(interior.len() + 1);
    let mut segment_ends = Vec::with_capacity(interior.len() + 1);
    segment_starts.push(start);
    segment_starts.extend(interior.iter().copied());
    segment_ends.extend(interior.iter().copied());
    segment_ends.push(end);

    let last_index = segment_starts.len() - 1;
    for (index, (segment_start, segment_end)) in
        segment_starts.into_iter().zip(segment_ends).enumerate()
    {
        let text = node
            .get("text")
            .and_then(Value::as_str)
            .context("splittable node missing text")?;
        let local_start = segment_start - start;
        let local_end = segment_end - start;
        if !text.is_char_boundary(local_start) || !text.is_char_boundary(local_end) {
            bail!(
                "sentence boundary falls outside UTF-8 character boundary in node {} at byte {}",
                node_type(node),
                segment_start
            );
        }
        let segment_text = json!(&text[local_start..local_end]);
        // The last segment moves the original node; earlier segments clone it.
        let mut segment = if index == last_index {
            std::mem::take(node)
        } else {
            node.clone()
        };
        set_span(&mut segment, segment_start, segment_end)?;
        let object = segment
            .as_object_mut()
            .context("node row is not an object")?;
        object.insert("text".to_owned(), segment_text);
        out.push(segment);
    }

    Ok(())
}

fn is_splittable_text_node(node: &Value) -> bool {
    matches!(
        node_type(node),
        "text" | "quote" | "emphasis" | "layout-span"
    ) && node.get("inline_children").is_none()
}

/// An `emphasis` / `layout-span` carrying `inline_children` — splittable by
/// recursing into its children rather than slicing a flat string.
fn is_splittable_container(node: &Value) -> bool {
    matches!(node_type(node), "emphasis" | "layout-span") && inline_children(node).is_some()
}

/// Split an inline container at sentence boundaries into N+1 sibling containers of
/// the same type/style, each carrying its partition of `inline_children`.
///
/// Children are first split recursively at the interior boundaries: a straddling
/// `text` child is sliced, a nested `emphasis`/`layout-span` recurses, and a
/// straddling atomic child (`ruby`/`gaiji`) fails via `split_node_at_boundaries`
/// with the atomic-node diagnostic. Zero-width / boundary children are owned by
/// the left sibling, matching top-level `node_belongs_to_sentence` grouping.
fn split_container_at_boundaries(
    node: &mut Value,
    interior: &[usize],
    out: &mut Vec<Value>,
) -> Result<()> {
    let start = value_usize(node, "/span/start", "container span.start")?;
    let end = value_usize(node, "/span/end", "container span.end")?;

    let children = node
        .get_mut("inline_children")
        .and_then(Value::as_array_mut)
        .context("container inline_children is not an array")?;
    let mut split_children: Vec<Value> = Vec::with_capacity(children.len());
    for child in children.iter_mut() {
        split_node_at_boundaries(child, interior, &mut split_children)?;
    }

    // Consistency guard (B-D5): a present flat `text` must equal the child
    // projection, so a split can never silently diverge from the flat form.
    if let Some(flat) = node.get("text").and_then(Value::as_str) {
        let mut projected = String::new();
        for child in &split_children {
            projected.push_str(&parser_ir_node_visible_text(child)?);
        }
        if flat != projected {
            bail!(
                "container {} flat text disagrees with inline_children projection",
                node_type(node)
            );
        }
    }

    // Sibling shell: type/style/layout minus the per-segment span/text/children.
    let mut shell = std::mem::take(node);
    if let Some(object) = shell.as_object_mut() {
        object.remove("inline_children");
        object.remove("text");
    }

    let mut segment_starts = Vec::with_capacity(interior.len() + 1);
    let mut segment_ends = Vec::with_capacity(interior.len() + 1);
    segment_starts.push(start);
    segment_starts.extend(interior.iter().copied());
    segment_ends.extend(interior.iter().copied());
    segment_ends.push(end);

    let mut cursor = 0usize;
    for (segment_start, segment_end) in segment_starts.into_iter().zip(segment_ends) {
        let bounds = SentenceBounds {
            start: segment_start,
            end: segment_end,
        };
        let mut segment_children: Vec<Value> = Vec::new();
        while cursor < split_children.len()
            && node_belongs_to_sentence(&split_children[cursor], bounds)?
        {
            segment_children.push(std::mem::take(&mut split_children[cursor]));
            cursor += 1;
        }

        let mut text = String::new();
        for child in &segment_children {
            text.push_str(&parser_ir_node_visible_text(child)?);
        }

        let mut sibling = shell.clone();
        set_span(&mut sibling, segment_start, segment_end)?;
        let object = sibling
            .as_object_mut()
            .context("container node is not an object")?;
        object.insert("text".to_owned(), json!(text));
        object.insert("inline_children".to_owned(), Value::Array(segment_children));
        out.push(sibling);
    }

    if cursor != split_children.len() {
        bail!(
            "container {} split left {} child node(s) unassigned to a segment",
            node_type(&shell),
            split_children.len() - cursor
        );
    }

    Ok(())
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

/// Assert the combinatorial coherence of sentence fragment fields:
/// - `part` present iff `fragment_group` present.
/// - `part=I` requires `next_id`, forbids `prev_id`.
/// - `part=M` requires both `next_id` and `prev_id`.
/// - `part=F` requires `prev_id`, forbids `next_id`.
/// - complete sentences (no `part`) have none of the fragment fields.
fn assert_fragment_field_coherence(rows: &[ParserIrSentence]) -> Result<()> {
    for row in rows {
        let has_part = row.part.is_some();
        let has_group = row.fragment_group.is_some();
        let has_next = row.next_id.is_some();
        let has_prev = row.prev_id.is_some();

        if has_part != has_group {
            bail!(
                "sentence {} has part but no fragment_group (or vice versa)",
                row.id
            );
        }
        match row.part.as_deref() {
            Some("I") => {
                if !has_next {
                    bail!("sentence {} part=I but no next_id", row.id);
                }
                if has_prev {
                    bail!("sentence {} part=I but has prev_id", row.id);
                }
            }
            Some("M") => {
                if !has_next {
                    bail!("sentence {} part=M but no next_id", row.id);
                }
                if !has_prev {
                    bail!("sentence {} part=M but no prev_id", row.id);
                }
            }
            Some("F") => {
                if has_next {
                    bail!("sentence {} part=F but has next_id", row.id);
                }
                if !has_prev {
                    bail!("sentence {} part=F but no prev_id", row.id);
                }
            }
            None => {
                if has_group {
                    bail!("sentence {} has fragment_group but no part", row.id);
                }
                if has_next {
                    bail!("sentence {} has next_id but no part", row.id);
                }
                if has_prev {
                    bail!("sentence {} has prev_id but no part", row.id);
                }
            }
            Some(other) => {
                bail!("sentence {} has invalid part: {}", row.id, other);
            }
        }
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
    fn keeps_atomic_ruby_whole_when_boundary_falls_inside_base() {
        let nodes = vec![json!({
            "type":"ruby",
            "span":span(0,48),
            "ruby":{"base":"吾輩ハ猫デアル。名前ハマダ無イ。","reading":"わがはいはねこであるなまえはまだない","scope":"explicit"}
        })];
        let paragraphs = vec![
            json!({"id":"p000000","span":span(0,48),"span_source":"direct","node_range":{"start":0,"end":1},"role":"body","source_pointer":"blocks[0]","classification":"direct"}),
        ];

        let projection = project_sentences(nodes, paragraphs, None).unwrap();

        assert_eq!(projection.nodes.len(), 1);
        assert_eq!(projection.sentences.len(), 1);
        assert_eq!(projection.sentences[0].span, span(0, 48));
        assert_eq!(
            projection.sentences[0].node_range,
            json!({"start":0,"end":1})
        );
    }

    #[test]
    fn splits_emphasis_container_into_sibling_emphases() {
        // B1: an emphasis carrying inline_children with an interior terminal splits
        // into two sibling emphases, each with its own text and child slice.
        let nodes = vec![json!({
            "type":"emphasis",
            "span":span(0,12),
            "style":"bold",
            "text":"甲。乙。",
            "inline_children":[{"type":"text","span":span(0,12),"text":"甲。乙。"}]
        })];
        let paragraphs = vec![json!({
            "id":"p000000","span":span(0,12),"span_source":"direct",
            "node_range":{"start":0,"end":1},"role":"body",
            "source_pointer":"blocks[0]","classification":"direct"
        })];

        let projection = project_sentences(nodes, paragraphs, None).unwrap();

        assert_eq!(projection.nodes.len(), 2);
        assert_eq!(projection.nodes[0]["type"], "emphasis");
        assert_eq!(projection.nodes[0]["style"], "bold");
        assert_eq!(projection.nodes[0]["text"], "甲。");
        assert_eq!(projection.nodes[0]["span"], span(0, 6));
        assert_eq!(projection.nodes[0]["inline_children"][0]["text"], "甲。");
        assert_eq!(projection.nodes[1]["text"], "乙。");
        assert_eq!(projection.nodes[1]["span"], span(6, 12));
        assert_eq!(projection.sentences.len(), 2);
        assert_eq!(
            projection.paragraphs[0]["node_range"],
            json!({"start":0,"end":2})
        );
    }

    #[test]
    fn multiline_quote_region_crossing_flat_split_still_fragments() {
        // The flat splitter deliberately does not scan for closing brackets past a
        // newline. A top-level quote may still span that newline, so sentence
        // projection must coalesce the flat bounds around the quote region before
        // fragment assembly.
        let nodes = vec![
            json!({"type":"text","span":span(0,6),"text":"彼は"}),
            json!({"type":"quote","span":span(6,9),"marker_type":"open","text":"「"}),
            json!({"type":"text","span":span(9,28),"text":"第一。\n第二。"}),
            json!({"type":"quote","span":span(28,31),"marker_type":"close","text":"」"}),
            json!({"type":"text","span":span(31,46),"text":"と言った。"}),
        ];
        let paragraphs = vec![json!({
            "id":"p000000","span":span(0,46),"span_source":"direct",
            "node_range":{"start":0,"end":5},"role":"body",
            "source_pointer":"blocks[0]","classification":"direct"
        })];

        let projection = project_sentences(nodes, paragraphs, None).unwrap();

        assert_eq!(projection.sentences.len(), 4);
        assert_eq!(projection.sentences[0].part.as_deref(), Some("I"));
        assert_eq!(projection.sentences[3].part.as_deref(), Some("F"));
        assert_eq!(
            projection.sentences[0].next_id.as_deref(),
            Some(projection.sentences[3].id.as_str())
        );
        assert_eq!(
            projection.sentences[3].prev_id.as_deref(),
            Some(projection.sentences[0].id.as_str())
        );
        assert_eq!(projection.sentences[0].span, span(0, 6));
        assert_eq!(projection.sentences[1].span, span(6, 18));
        assert_eq!(projection.sentences[2].span, span(18, 31));
        assert_eq!(projection.sentences[3].span, span(31, 46));
    }

    #[test]
    fn singleton_outer_fragment_has_no_fragment_fields() {
        let nodes = vec![
            json!({"type":"quote","span":span(0,3),"marker_type":"open","text":"「"}),
            json!({"type":"text","span":span(3,9),"text":"否。"}),
            json!({"type":"quote","span":span(9,12),"marker_type":"close","text":"」"}),
            json!({"type":"text","span":span(12,27),"text":"と言った。"}),
        ];
        let paragraphs = vec![json!({
            "id":"p000000","span":span(0,27),"span_source":"direct",
            "node_range":{"start":0,"end":4},"role":"body",
            "source_pointer":"blocks[0]","classification":"direct"
        })];

        let projection = project_sentences(nodes, paragraphs, None).unwrap();

        assert_eq!(projection.sentences.len(), 2);
        assert!(projection.sentences[1].part.is_none());
        assert!(projection.sentences[1].fragment_group.is_none());
        assert!(projection.sentences[1].next_id.is_none());
        assert!(projection.sentences[1].prev_id.is_none());
    }

    #[test]
    fn keeps_ruby_child_whole_when_boundary_falls_between_children() {
        // A boundary exactly between an emphasis's text child and its ruby child is
        // a clean partition — the ruby stays whole.
        let nodes = vec![json!({
            "type":"emphasis",
            "span":span(0,9),
            "style":"bold",
            "text":"甲。乙",
            "inline_children":[
                {"type":"text","span":span(0,6),"text":"甲。"},
                {"type":"ruby","span":span(6,9),"ruby":{"base":"乙","reading":"おつ","scope":"explicit"}}
            ]
        })];
        let paragraphs = vec![json!({
            "id":"p000000","span":span(0,9),"span_source":"direct",
            "node_range":{"start":0,"end":1},"role":"body",
            "source_pointer":"blocks[0]","classification":"direct"
        })];

        let projection = project_sentences(nodes, paragraphs, None).unwrap();

        assert_eq!(projection.nodes.len(), 2);
        assert_eq!(projection.nodes[0]["text"], "甲。");
        assert_eq!(projection.nodes[1]["inline_children"][0]["type"], "ruby");
        assert_eq!(
            projection.nodes[1]["inline_children"][0]["ruby"]["base"],
            "乙"
        );
        assert_eq!(projection.sentences.len(), 2);
    }

    #[test]
    fn keeps_nested_ruby_child_whole_when_boundary_falls_inside_base() {
        let nodes = vec![json!({
            "type":"emphasis",
            "span":span(0,9),
            "style":"bold",
            "text":"甲。乙",
            "inline_children":[
                {"type":"ruby","span":span(0,9),"ruby":{"base":"甲。乙","reading":"こう","scope":"explicit"}}
            ]
        })];
        let paragraphs = vec![json!({
            "id":"p000000","span":span(0,9),"span_source":"direct",
            "node_range":{"start":0,"end":1},"role":"body",
            "source_pointer":"blocks[0]","classification":"direct"
        })];

        let projection = project_sentences(nodes, paragraphs, None).unwrap();

        assert_eq!(projection.nodes.len(), 1);
        assert_eq!(projection.sentences.len(), 1);
        assert_eq!(projection.sentences[0].span, span(0, 9));
        assert_eq!(
            projection.sentences[0].node_range,
            json!({"start":0,"end":1})
        );
    }

    #[test]
    fn whitespace_only_paragraph_yields_single_sentence() {
        // B2: a body paragraph whose visible text is whitespace-only but whose byte
        // span is non-empty yields one covering sentence instead of failing.
        let nodes = vec![json!({
            "type":"text","span":span(0,4),"text":"　\n"
        })];
        let paragraphs = vec![json!({
            "id":"p000000","span":span(0,4),"span_source":"direct",
            "node_range":{"start":0,"end":1},"role":"body",
            "source_pointer":"blocks[0]","classification":"direct"
        })];

        let projection = project_sentences(nodes, paragraphs, None).unwrap();

        assert_eq!(projection.sentences.len(), 1);
        assert_eq!(projection.sentences[0].span, span(0, 4));
        assert_eq!(
            projection.sentences[0].node_range,
            json!({"start":0,"end":1})
        );
    }

    #[test]
    fn trailing_whitespace_absorbed_into_last_sentence() {
        // B2: a trailing newline the splitter drops is absorbed into the last
        // sentence so the paragraph tiles to its end.
        let nodes = vec![json!({
            "type":"text","span":span(0,7),"text":"文。\n"
        })];
        let paragraphs = vec![json!({
            "id":"p000000","span":span(0,7),"span_source":"direct",
            "node_range":{"start":0,"end":1},"role":"body",
            "source_pointer":"blocks[0]","classification":"direct"
        })];

        let projection = project_sentences(nodes, paragraphs, None).unwrap();

        assert_eq!(projection.sentences.len(), 1);
        assert_eq!(projection.sentences[0].span, span(0, 7));
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

    #[test]
    fn detect_nested_regions_basic() {
        let nodes = vec![
            json!({"type":"text","span":{"start":0,"end":6},"text":"先生は"}),
            json!({"type":"quote","span":{"start":6,"end":9},"marker_type":"open","text":"「"}),
            json!({"type":"text","span":{"start":9,"end":15},"text":"綺麗だ"}),
            json!({"type":"quote","span":{"start":15,"end":18},"marker_type":"close","text":"」"}),
            json!({"type":"text","span":{"start":18,"end":24},"text":"といった"}),
        ];
        let regions = detect_nested_regions(&nodes, 0, nodes.len());
        assert_eq!(regions.len(), 1);
        assert_eq!(regions[0].inner_byte_start, 9);
        assert_eq!(regions[0].inner_byte_end, 15);
        assert_eq!(regions[0].opening_marker_node, 1);
        assert_eq!(regions[0].closing_marker_node, 3);
        assert_eq!(regions[0].nesting_level, 0);
    }

    #[test]
    fn detect_nested_regions_recursive() {
        let nodes = vec![
            json!({"type":"quote","span":{"start":0,"end":3},"marker_type":"open","text":"「"}),
            json!({"type":"text","span":{"start":3,"end":9},"text":"outer "}),
            json!({"type":"quote","span":{"start":9,"end":12},"marker_type":"open","text":"「"}),
            json!({"type":"text","span":{"start":12,"end":17},"text":"inner"}),
            json!({"type":"quote","span":{"start":17,"end":21},"marker_type":"close","text":"」"}),
            json!({"type":"text","span":{"start":21,"end":27},"text":" text"}),
            json!({"type":"quote","span":{"start":27,"end":30},"marker_type":"close","text":"」"}),
        ];
        let regions = detect_nested_regions(&nodes, 0, nodes.len());
        assert_eq!(regions.len(), 2);
        assert_eq!(regions[0].nesting_level, 0);
        assert_eq!(regions[1].nesting_level, 1);
    }

    #[test]
    fn detect_nested_regions_unmatched_open_emits_nothing() {
        let nodes = vec![
            json!({"type":"quote","span":{"start":0,"end":3},"marker_type":"open","text":"「"}),
            json!({"type":"text","span":{"start":3,"end":9},"text":"閉じない"}),
        ];
        let regions = detect_nested_regions(&nodes, 0, nodes.len());
        assert!(regions.is_empty());
    }
}
