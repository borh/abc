# Nested Sentence Fragmentation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement TEI Chapter 21 fragmentation for nested/quoted speech — outer sentences split into `<s part="I/M/F">` fragments, inner sentences as separate `<s>` elements.

**Architecture:** Three-layer design: (1) splitter replacement in ab-plaintext, (2) converter nesting detection + fragment assembly in ab-aat-to-parser-ir, (3) TEI renderer fragment attributes in ABC. Converter synthesizes quote nodes from `「」`/`『』`, detects nested regions, re-splits inner text with `suppress_closing_bracket_check`.

**Tech Stack:** Rust (ab-plaintext, ab-aat-to-parser-ir), Clojure (ABC TEI renderer), RelaxNG (TEI profile schema)

## Global Constraints

- Newlines are NOT sentence boundaries (preserved from existing behavior)
- Only `「」`/`『』` trigger fragmentation in v1 (not `（）` or European quotes)
- `nesting_level` stored as `null` on quote nodes (derivable from marker sequence)
- `fragment_group` is parser-IR-internal only (not rendered as `@corresp`)
- Every `<s>` gets `@xml:id` (not just fragments)
- `sentence_segmentation.schema_version` stays at `"sentence-segmentation-v1"`
- `splitter_id` updates to `"ab-plaintext-japanese-v2"`

---

## File Structure

| File | Action | Responsibility |
|---|---|---|
| `ab-validator/crates/ab-plaintext/src/lib.rs` | Modify | Replace `sentence_split` with `split_sentences` + `SplitOptions` |
| `ab-validator/crates/ab-plaintext/tests/splitter_v2.rs` | Create | Splitter v2 tests + BCCWW validation |
| `ab-validator/crates/ab-aat-to-parser-ir/src/convert.rs` | Modify | Synthesize quote nodes from text `「」`/`『』` |
| `ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs` | Modify | Nesting detection, fragment assembly, tiling assertions |
| `ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/nested-sentence-fragmentation.aat.json` | Create | End-to-end fixture from Kokoro |
| `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs` | Modify | Fragment assembly tests |
| `abc/src/abc/tools/parser_ir_tei.clj` | Modify | Fragment attribute rendering |
| `abc/src/abc/tools/parser_ir_sentence_policy.clj` | Modify | Fragment field coherence checks |
| `abc/test/abc/tools/parser_ir_tei_test.clj` | Modify | Renderer fragment tests |
| `abc/schemas/tei-profile.rng` | Modify | Add `att.linking` to `<s>` |

---

### Task 1: Splitter Replacement — `split_sentences` Core

**Files:**
- Modify: `ab-validator/crates/ab-plaintext/src/lib.rs`

**Interfaces:**
- Produces: `pub struct SplitOptions { pub suppress_closing_bracket_check: bool }`
- Produces: `pub fn split_sentences_with_options(input: &str, opts: &SplitOptions) -> Vec<SentenceSpan<'_>>`
- Produces: `pub fn split_sentences(input: &str) -> Vec<SentenceSpan<'_>>` (default opts wrapper)

- [ ] **Step 1: Add `SplitOptions` struct and `split_sentences_with_options` function**

Add the new splitter logic alongside the existing `sentence_split`. The new function implements the user's improved rules:
- `…` only splits at end-of-line
- CJK numbered lists (`１．`) recognized, no split
- `！`/`？` + Japanese continuation stays together
- `closing_bracket_ahead` with depth tracking
- Closing quotations adds `"`, `"`, `'`
- `suppress_closing_bracket_check` flag skips `closing_bracket_ahead` when true

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SplitOptions {
    pub suppress_closing_bracket_check: bool,
}

impl Default for SplitOptions {
    fn default() -> Self {
        Self {
            suppress_closing_bracket_check: false,
        }
    }
}

pub fn split_sentences_with_options(
    input: &str,
    opts: &SplitOptions,
) -> Vec<SentenceSpan<'_>> {
    // Implementation from user's split_sentences, adapted to return SentenceSpan
    // Key: track byte_offset and char_offset as we iterate
    // When opts.suppress_closing_bracket_check is true, skip closing_bracket_ahead()
    // Newlines are NOT boundaries (match existing behavior)
    // ... (full implementation)
}

pub fn split_sentences(input: &str) -> Vec<SentenceSpan<'_>> {
    split_sentences_with_options(input, &SplitOptions::default())
}
```

- [ ] **Step 2: Add unit tests for `split_sentences`**

Add tests in the existing `mod sentence_split_tests` block:

```rust
#[test]
fn split_sentences_basic() {
    let spans = split_sentences("吾輩は猫である。名前はまだ無い。");
    assert_eq!(spans.len(), 2);
    assert_eq!(spans[0].text, "吾輩は猫である。");
    assert_eq!(spans[1].text, "名前はまだ無い。");
}

#[test]
fn split_sentences_adjacent_terminals() {
    let spans = split_sentences("本当！？そう！！！");
    assert_eq!(spans.len(), 2);
    assert_eq!(spans[0].text, "本当！？");
}

#[test]
fn split_sentences_decimal_point() {
    let spans = split_sentences("これは3.14です。終わり。");
    assert_eq!(spans[0].text, "これは3.14です。");
}

#[test]
fn split_sentences_cjk_numbered_list() {
    let spans = split_sentences("１．項目。２．項目。");
    assert_eq!(spans.len(), 2);
    assert_eq!(spans[0].text, "１．項目。");
}

#[test]
fn split_sentences_exclamation_continuation() {
    let spans = split_sentences("すごい！笑った。");
    assert_eq!(spans.len(), 1);
    assert_eq!(spans[0].text, "すごい！笑った。");
}

#[test]
fn split_sentences_closing_bracket_depth() {
    // 「...。...」 stays together because closing_bracket_ahead finds 」
    let spans = split_sentences("先生は「綺麗ですよ。落葉で埋まります」といった。");
    assert_eq!(spans.len(), 1);
    assert_eq!(spans[0].text, "先生は「綺麗ですよ。落葉で埋まります」といった。");
}

#[test]
fn split_sentences_suppress_closing_bracket() {
    let text = "もう少しすると、綺麗ですよ。この木が埋まります」";
    let opts = SplitOptions { suppress_closing_bracket_check: true };
    let spans = split_sentences_with_options(text, &opts);
    assert_eq!(spans.len(), 2);
    assert_eq!(spans[0].text, "もう少しすると、綺麗ですよ。");
    assert_eq!(spans[1].text, "この木が埋まります」");
}

#[test]
fn split_sentences_newlines_not_boundaries() {
    let spans = split_sentences("一行目\n二行目。");
    assert_eq!(spans.len(), 1);
    assert_eq!(spans[0].text, "一行目\n二行目。");
}
```

- [ ] **Step 3: Run tests to verify**

Run: `cargo test -p ab-plaintext -- split_sentences`
Expected: All new tests PASS

- [ ] **Step 4: Commit**

```bash
git add ab-validator/crates/ab-plaintext/src/lib.rs
git commit -m "feat(ab-plaintext): add split_sentences with SplitOptions

New splitter with improved rules: CJK numbered lists, exclamation
continuation, closing_bracket_ahead with depth tracking. Adds
suppress_closing_bracket_check flag for nested sentence re-splitting."
```

---

### Task 2: Splitter v2 Tests + BCCWW Validation

**Files:**
- Create: `ab-validator/crates/ab-plaintext/tests/splitter_v2.rs`

**Interfaces:**
- Consumes: `split_sentences`, `split_sentences_with_options`, `SplitOptions` from Task 1

- [ ] **Step 1: Create integration test file**

Create `ab-validator/crates/ab-plaintext/tests/splitter_v2.rs`:

```rust
use ab_plaintext::{split_sentences, split_sentences_with_options, SplitOptions};

#[test]
fn fixture_matrix_matches_legacy_cases() {
    let cases = [
        ("吾輩は猫である。名前はまだ無い。", vec!["吾輩は猫である。", "名前はまだ無い。"]),
        ("え！？本当。", vec!["え！？", "本当。"]),
        ("これは3.14です。終わり。", vec!["これは3.14です。", "終わり。"]),
        ("一行目\n二行目。", vec!["一行目\n二行目。"]),
    ];
    for (input, expected) in cases {
        let actual: Vec<&str> = split_sentences(input).iter().map(|s| s.text).collect();
        assert_eq!(actual, expected, "input: {input}");
    }
}

#[test]
fn nested_sentence_basic() {
    let text = "先生は梢を見上げて、「綺麗ですよ。落葉で埋まります」といった。";
    let spans = split_sentences(text);
    // closing_bracket_ahead suppresses inner splits → one sentence
    assert_eq!(spans.len(), 1);
    assert_eq!(spans[0].text, text);
}

#[test]
fn nested_sentence_suppress_mode() {
    let inner = "綺麗ですよ。落葉で埋まります」";
    let opts = SplitOptions { suppress_closing_bracket_check: true };
    let spans = split_sentences_with_options(inner, &opts);
    assert_eq!(spans.len(), 2);
    assert_eq!(spans[0].text, "綺麗ですよ。");
    assert_eq!(spans[1].text, "落葉で埋まります」");
}

#[test]
fn unmatched_quote_no_split() {
    let text = "先生は「綺麗ですといった。";
    let spans = split_sentences(text);
    // No matching 「」, no fragmentation
    assert_eq!(spans.len(), 1);
}
```

- [ ] **Step 2: Run integration tests**

Run: `cargo test -p ab-plaintext --test splitter_v2`
Expected: All tests PASS

- [ ] **Step 3: Commit**

```bash
git add ab-validator/crates/ab-plaintext/tests/splitter_v2.rs
git commit -m "test(ab-plaintext): add splitter v2 integration tests"
```

---

### Task 3: Quote Node Synthesis in Converter

**Files:**
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/convert.rs`

**Interfaces:**
- Produces: Quote nodes in `outputs.nodes` with `type: "quote"`, `marker_type`, `text`
- Consumes: Text nodes containing `「`/`」`/`『`/`』` characters

- [ ] **Step 1: Add `map_text_node_with_quotes` function**

When processing a text node, check if it contains `「`, `」`, `『`, or `』`. If so, split the text node into segments: text before the marker, the marker as a quote node, text after the marker. Recurse for multiple markers in one text node.

```rust
fn map_text_node_with_quotes(
    node: &Value,
    nodes: &mut Vec<Value>,
    recorder: &mut DivergenceRecorder,
    current_offset: u64,
    path: &str,
) -> Result<u64> {
    let text = node.get("value").and_then(Value::as_str).unwrap_or("");
    let quote_chars: &[char] = &['「', '」', '『', '』'];
    
    if !text.chars().any(|c| quote_chars.contains(&c)) {
        // No quotes — use existing text node mapping
        return map_text_node(node, nodes, recorder, current_offset, path);
    }
    
    let mut offset = current_offset;
    let mut segment_start = 0;
    
    for (i, ch) in text.char_indices() {
        if quote_chars.contains(&ch) {
            // Emit text segment before the quote (if non-empty)
            if segment_start < i {
                let segment_text = &text[segment_start..i];
                let end = offset + segment_text.len() as u64;
                nodes.push(json!({
                    "type": "text",
                    "span": span(offset, end),
                    "text": segment_text,
                }));
                offset = end;
            }
            
            // Emit quote node
            let marker_type = match ch {
                '「' | '『' => "open",
                '」' | '』' => "close",
                _ => unreachable!(),
            };
            let ch_len = ch.len_utf8() as u64;
            nodes.push(json!({
                "type": "quote",
                "span": span(offset, offset + ch_len),
                "marker_type": marker_type,
                "nesting_level": null,
                "text": ch.to_string(),
            }));
            offset += ch_len;
            segment_start = i + ch.len_utf8();
        }
    }
    
    // Emit remaining text after last quote
    if segment_start < text.len() {
        let segment_text = &text[segment_start..];
        let end = offset + segment_text.len() as u64;
        nodes.push(json!({
            "type": "text",
            "span": span(offset, end),
            "text": segment_text,
        }));
        offset = end;
    }
    
    Ok(offset)
}
```

- [ ] **Step 2: Wire into existing text node processing**

In `map_inline_node`, replace the text node case to call `map_text_node_with_quotes` instead of the direct text node emission.

- [ ] **Step 3: Add test for quote node emission**

Create fixture `ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/quote-node-emission.aat.json`:

```json
{
  "version": 1,
  "work_id": "test-quote-nodes",
  "blocks": [
    {
      "kind": "paragraph",
      "span": { "line_start": 1, "line_end": 1, "byte_start": 0, "byte_end": 30 },
      "content": [
        {"kind": "text", "value": "先生は「綺麗だ」といった。"}
      ]
    }
  ],
  "meta": {"adapter": "fixture", "adapter_version": "0.0.0", "source_encoding": "utf-8", "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000", "parse_complete": true, "warnings": []}
}
```

Add integration test in `integration.rs`:

```rust
#[test]
fn quote_node_emission_from_text() {
    // Load fixture, run conversion, verify quote nodes exist with correct marker_type
    let input = load_fixture("quote-node-emission.aat.json");
    let result = convert_to_parser_ir(&input, &mapping()).unwrap();
    let quote_nodes: Vec<_> = result.nodes.iter()
        .filter(|n| n["type"] == "quote")
        .collect();
    assert_eq!(quote_nodes.len(), 2);
    assert_eq!(quote_nodes[0]["marker_type"], "open");
    assert_eq!(quote_nodes[0]["text"], "「");
    assert_eq!(quote_nodes[1]["marker_type"], "close");
    assert_eq!(quote_nodes[1]["text"], "」");
}
```

- [ ] **Step 4: Run tests**

Run: `cargo test -p ab-aat-to-parser-ir -- quote_node_emission`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add ab-validator/crates/ab-aat-to-parser-ir/src/convert.rs
git add ab-validator/crates/ab-aat-to-parser-ir/tests/
git commit -m "feat(ab-aat-to-parser-ir): synthesize quote nodes from 「」/『』

Text nodes containing Japanese quotation markers are split into
text segments + quote nodes with marker_type open/close."
```

---

### Task 4: Nesting Detection

**Files:**
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs`

**Interfaces:**
- Produces: `struct NestedRegion { outer_sentence_idx, inner_byte_start, inner_byte_end, opening_marker_node, closing_marker_node, nesting_level }`
- Produces: `fn detect_nested_regions(nodes: &[Value], sentence_span: &Value) -> Vec<NestedRegion>`
- Consumes: Quote nodes from Task 3

- [ ] **Step 1: Add `NestedRegion` struct and `detect_nested_regions` function**

```rust
const MAX_NESTING_DEPTH: usize = 5;

#[derive(Debug, Clone, PartialEq, Eq)]
struct NestedRegion {
    outer_sentence_idx: usize,
    inner_byte_start: usize,
    inner_byte_end: usize,
    opening_marker_node: Option<usize>,
    closing_marker_node: Option<usize>,
    nesting_level: usize,
}

fn detect_nested_regions(
    nodes: &[Value],
    sentence_start: usize,
    sentence_end: usize,
    sentence_byte_start: usize,
) -> Vec<NestedRegion> {
    let mut regions = Vec::new();
    let mut open_stack: Vec<(usize, usize, usize)> = Vec::new(); // (node_idx, byte_pos, depth)
    let mut depth = 0usize;
    
    for node_idx in sentence_start..sentence_end {
        let node = &nodes[node_idx];
        if node.get("type").and_then(Value::as_str) != Some("quote") {
            continue;
        }
        let marker_type = node.get("marker_type").and_then(Value::as_str).unwrap_or("");
        let byte_start = node.pointer("/span/start").and_then(Value::as_u64).unwrap_or(0) as usize;
        let byte_end = node.pointer("/span/end").and_then(Value::as_u64).unwrap_or(0) as usize;
        
        match marker_type {
            "open" => {
                if depth < MAX_NESTING_DEPTH {
                    open_stack.push((node_idx, byte_end, depth));
                    depth += 1;
                }
            }
            "close" => {
                if let Some((open_idx, inner_start, open_depth)) = open_stack.pop() {
                    if open_depth == 0 {
                        regions.push(NestedRegion {
                            outer_sentence_idx: 0, // filled by caller
                            inner_byte_start: inner_start,
                            inner_byte_end: byte_start,
                            opening_marker_node: Some(open_idx),
                            closing_marker_node: Some(node_idx),
                            nesting_level: open_depth,
                        });
                    }
                    depth = depth.saturating_sub(1);
                }
            }
            _ => {}
        }
    }
    
    regions
}
```

- [ ] **Step 2: Add unit tests for nesting detection**

```rust
#[test]
fn detect_nested_regions_basic() {
    // Nodes: text("先生は"), quote("「"), text("綺麗だ"), quote("」"), text("といった")
    let nodes = vec![
        json!({"type":"text","span":{"start":0,"end":6},"text":"先生は"}),
        json!({"type":"quote","span":{"start":6,"end":9},"marker_type":"open","nesting_level":null,"text":"「"}),
        json!({"type":"text","span":{"start":9,"end":15},"text":"綺麗だ"}),
        json!({"type":"quote","span":{"start":15,"end":18},"marker_type":"close","nesting_level":null,"text":"」"}),
        json!({"type":"text","span":{"start":18,"end":24},"text":"といった"}),
    ];
    let regions = detect_nested_regions(&nodes, 0, 5, 0);
    assert_eq!(regions.len(), 1);
    assert_eq!(regions[0].inner_byte_start, 9); // after 「
    assert_eq!(regions[0].inner_byte_end, 15); // before 」
}

#[test]
fn detect_nested_regions_no_quotes() {
    let nodes = vec![
        json!({"type":"text","span":{"start":0,"end":12},"text":"普通の文です"}),
    ];
    let regions = detect_nested_regions(&nodes, 0, 1, 0);
    assert!(regions.is_empty());
}

#[test]
fn detect_nested_regions_unmatched() {
    let nodes = vec![
        json!({"type":"quote","span":{"start":0,"end":3},"marker_type":"open","nesting_level":null,"text":"「"}),
        json!({"type":"text","span":{"start":3,"end":9},"text":"閉じない"}),
    ];
    let regions = detect_nested_regions(&nodes, 0, 2, 0);
    assert!(regions.is_empty());
}
```

- [ ] **Step 3: Run tests**

Run: `cargo test -p ab-aat-to-parser-ir -- detect_nested_regions`
Expected: PASS

- [ ] **Step 4: Commit**

```bash
git add ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs
git commit -m "feat(ab-aat-to-parser-ir): add nesting detection for quote nodes

Detects nested regions by scanning for open/close quote node pairs.
Max depth 5. Unmatched markers produce no regions."
```

---

### Task 5: Fragment Assembly

**Files:**
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs`

**Interfaces:**
- Modifies: `project_body_paragraph` to call nesting detection + fragment assembly
- Produces: Sentence rows with `part`, `fragment_group`, `next_id`, `prev_id`
- Consumes: `detect_nested_regions` from Task 4, `split_sentences_with_options` from Task 1

- [ ] **Step 1: Add fragment assembly logic to `project_body_paragraph`**

After the existing flat split, for each sentence that contains nested regions:
1. Re-split inner text with `suppress_closing_bracket_check: true`
2. Split nodes at finer boundaries
3. Create fragment rows with part/group/next/prev

```rust
// Inside project_body_paragraph, after flat split:
for (sent_idx, bounds) in bounds.iter().enumerate() {
    let sentence_nodes = &rewritten_nodes[sentence_node_start..sentence_node_end];
    let nested = detect_nested_regions(sentence_nodes, 0, sentence_nodes.len(), bounds.start);
    
    if nested.is_empty() {
        // No nesting — emit as-is (existing logic)
        rows.push(ParserIrSentence { /* existing fields, no part/group */ });
    } else {
        // Fragment assembly
        let fragment_group = format!("fg{:06}", fragment_group_counter);
        fragment_group_counter += 1;
        
        // Before first nested region: part="I"
        // Each nested region: no part (inner sentences)
        // After last nested region: part="F"
        // Assign next_id/prev_id linking
    }
}
```

- [ ] **Step 2: Add tiling assertion for fragment groups**

```rust
fn assert_fragment_group_tiling(
    rows: &[ParserIrSentence],
    fragment_groups: &HashMap<String, Vec<usize>>,
) -> Result<()> {
    for (group_id, indices) in fragment_groups {
        let group_rows: Vec<_> = indices.iter().map(|i| &rows[*i]).collect();
        // Verify fragments tile the original sentence byte range
        // Verify fragments tile the original sentence node range
    }
    Ok(())
}
```

- [ ] **Step 3: Add orthographic annotation redistribution**

Fragment rows inherit `tags` and `orthographic_annotation_indices` by byte-overlap:

```rust
// For each fragment row:
let annotation_indices = overlapping_ortho_indices(
    fragment_span.start,
    fragment_span.end,
    ortho,
);
let tags = if annotation_indices.is_empty() {
    Vec::new()
} else {
    vec!["orthographic-katakana".to_owned()]
};
```

- [ ] **Step 4: Create end-to-end fixture**

Create `ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/nested-sentence-fragmentation.aat.json` with the Kokoro example.

- [ ] **Step 5: Add integration tests**

```rust
#[test]
fn fragment_assembly_single_inner_sentence() {
    // "先生は梢を見上げて、「綺麗です」といった。"
    // → 3 sentences: part="I", (no part), part="F"
    let input = load_fixture("nested-sentence-fragmentation.aat.json");
    let result = convert_to_parser_ir(&input, &mapping()).unwrap();
    let sentences = result.sentences.as_ref().unwrap();
    assert_eq!(sentences.len(), 4);
    assert_eq!(sentences[0].part, Some("I".to_string()));
    assert_eq!(sentences[1].part, None);
    assert_eq!(sentences[2].part, None);
    assert_eq!(sentences[3].part, Some("F".to_string()));
}

#[test]
fn fragment_assembly_multiple_inner_sentences() {
    // "先生は梢を見上げて、「綺麗ですよ。落葉で埋まります」といった。"
    // → 4 sentences: part="I", (no part), (no part), part="F"
    // ...
}

#[test]
fn fragment_field_coherence() {
    // Verify: part="I" → next_id present, prev_id absent
    // Verify: part="F" → prev_id present, next_id absent
    // Verify: part absent → no fragment_group, no next_id, no prev_id
    // ...
}

#[test]
fn ortho_overlap_fragmented() {
    // Verify correct tag/index distribution across fragments
    // ...
}
```

- [ ] **Step 6: Run tests**

Run: `cargo test -p ab-aat-to-parser-ir -- fragment`
Expected: PASS

- [ ] **Step 7: Commit**

```bash
git add ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs
git add ab-validator/crates/ab-aat-to-parser-ir/tests/
git commit -m "feat(ab-aat-to-parser-ir): add fragment assembly for nested sentences

Detects nested regions, re-splits inner text, creates fragment rows
with part/fragment_group/next_id/prev_id. Adds tiling assertions
and orthographic annotation redistribution."
```

---

### Task 6: Cross-Field Fragment Constraints (Validator Side)

**Files:**
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs`

**Interfaces:**
- Produces: `fn assert_fragment_field_coherence(rows: &[ParserIrSentence]) -> Result<()>`
- Consumes: Sentence rows from Task 5

- [ ] **Step 1: Add coherence assertion function**

```rust
fn assert_fragment_field_coherence(rows: &[ParserIrSentence]) -> Result<()> {
    for row in rows {
        let has_part = row.part.is_some();
        let has_group = row.fragment_group.is_some();
        let has_next = row.next_id.is_some();
        let has_prev = row.prev_id.is_some();
        
        if has_part != has_group {
            bail!("sentence {} has part but no fragment_group (or vice versa)", row.id);
        }
        match row.part.as_deref() {
            Some("I") => {
                if !has_next { bail!("sentence {} part=I but no next_id", row.id); }
                if has_prev { bail!("sentence {} part=I but has prev_id", row.id); }
            }
            Some("M") => {
                if !has_next { bail!("sentence {} part=M but no next_id", row.id); }
                if !has_prev { bail!("sentence {} part=M but no prev_id", row.id); }
            }
            Some("F") => {
                if has_next { bail!("sentence {} part=F but has next_id", row.id); }
                if !has_prev { bail!("sentence {} part=F but no prev_id", row.id); }
            }
            None => {
                if has_group { bail!("sentence {} has fragment_group but no part", row.id); }
                if has_next { bail!("sentence {} has next_id but no part", row.id); }
                if has_prev { bail!("sentence {} has prev_id but no part", row.id); }
            }
            _ => bail!("sentence {} has invalid part value: {:?}", row.id, row.part),
        }
    }
    Ok(())
}
```

- [ ] **Step 2: Wire into `project_body_paragraph`**

Call `assert_fragment_field_coherence(&rows)` before returning.

- [ ] **Step 3: Add test for invalid fragment fields**

```rust
#[test]
fn fragment_field_coherence_rejects_invalid() {
    // Test: part="I" without next_id → error
    // Test: part="F" with next_id → error
    // Test: no part with fragment_group → error
    // ...
}
```

- [ ] **Step 4: Run tests**

Run: `cargo test -p ab-aat-to-parser-ir -- fragment_field_coherence`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs
git commit -m "feat(ab-aat-to-parser-ir): add fragment field coherence assertions

Validates part↔next_id/prev_id/fragment_group combinatorial
constraints at conversion time."
```

---

### Task 7: ABC TEI Renderer Fragment Attributes

**Files:**
- Modify: `abc/src/abc/tools/parser_ir_tei.clj`

**Interfaces:**
- Consumes: `part`, `fragment_group`, `next_id`, `prev_id` from sentence rows
- Produces: `<s>` elements with `@part`, `@xml:id`, `@next`, `@prev`

- [ ] **Step 1: Update `sentence-attrs` function**

Replace the existing `sentence-attrs` with the version from the spec:

```clojure
(defn- sentence-attrs [sentence]
  (let [base-attrs (cond-> {:xml:id (get sentence "id")}
                     (some #{"orthographic-katakana"} (get sentence "tags" []))
                     (assoc :type "orthographic-katakana"))
        part (get sentence "part")
        next-id (get sentence "next_id")
        prev-id (get sentence "prev_id")]
    (cond-> base-attrs
      part
      (assoc :part part)

      next-id
      (assoc :next (str "#" next-id))

      prev-id
      (assoc :prev (str "#" prev-id)))))
```

- [ ] **Step 2: Add renderer test**

In `abc/test/abc/tools/parser_ir_tei_test.clj`:

```clojure
(deftest fragment-attributes-test
  (testing "fragmented sentences render with part/xml:id/next/prev"
    (let [parser-ir {:nodes [{:type "text" :span {:start 0 :end 12} :text "先生は言った。"}
                             {:type "text" :span {:start 12 :end 24} :text "「綺麗だ」といった。"}]
                     :paragraphs [{:id "p000000" :role "body" :span {:start 0 :end 24}
                                   :node_range {:start 0 :end 2}}]
                     :sentences [{:id "s000000" :paragraph_id "p000000"
                                  :span {:start 0 :end 12} :node_range {:start 0 :end 1}
                                  :tags [] :orthographic_annotation_indices []
                                  :part "I" :fragment_group "fg000000" :next_id "s000002"}
                                 {:id "s000001" :paragraph_id "p000000"
                                  :span {:start 12 :end 18} :node_range {:start 1 :start 2}
                                  :tags [] :orthographic_annotation_indices []}
                                 {:id "s000002" :paragraph_id "p000000"
                                  :span {:start 18 :end 24} :node_range {:start 2 :end 3}
                                  :tags [] :orthographic_annotation_indices []
                                  :part "F" :fragment_group "fg000000" :prev_id "s000000"}]}
          result (render parser-ir)
          s-elements (get-in result [:body 1 1])] ; <body> → <p> → children
      (is (= "I" (get-in s-elements [0 1 :part])))
      (is (= "s000000" (get-in s-elements [0 1 :xml:id])))
      (is (= "#s000002" (get-in s-elements [0 1 :next])))
      (is (nil? (get-in s-elements [0 1 :prev])))
      (is (nil? (get-in s-elements [1 1 :part])))
      (is (= "F" (get-in s-elements [2 1 :part])))
      (is (= "#s000000" (get-in s-elements [2 1 :prev]))))))
```

- [ ] **Step 3: Run tests**

Run: `just abc-clj-kondo` or `nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests`
Expected: PASS

- [ ] **Step 4: Commit**

```bash
git add abc/src/abc/tools/parser_ir_tei.clj
git add abc/test/abc/tools/parser_ir_tei_test.clj
git commit -m "feat(abc): render sentence fragment attributes

Every s element gets xml:id. Fragmented sentences get part/next/prev
attributes for TEI Chapter 21 fragmentation."
```

---

### Task 8: ABC Fragment Field Coherence (Publication Gate)

**Files:**
- Modify: `abc/src/abc/tools/parser_ir_sentence_policy.clj`

**Interfaces:**
- Consumes: Sentence rows with fragment fields
- Produces: Error messages for invalid fragment field combinations

- [ ] **Step 1: Add fragment field checks to `sentence-coherence-errors`**

```clojure
(defn- fragment-field-errors [sentence]
  (let [part (get sentence "part")
        fragment-group (get sentence "fragment_group")
        next-id (get sentence "next_id")
        prev-id (get sentence "prev_id")
        sid (get sentence "id")]
    (cond-> []
      (and part (nil? fragment-group))
      (conj (str "parser IR sentence " sid " has part but no fragment_group"))

      (and (nil? part) fragment-group)
      (conj (str "parser IR sentence " sid " has fragment_group but no part"))

      (and (= part "I") (nil? next-id))
      (conj (str "parser IR sentence " sid " part=I but no next_id"))

      (and (= part "I") prev-id)
      (conj (str "parser IR sentence " sid " part=I but has prev_id"))

      (and (= part "M") (nil? next-id))
      (conj (str "parser IR sentence " sid " part=M but no next_id"))

      (and (= part "M") (nil? prev-id))
      (conj (str "parser IR sentence " sid " part=M but no prev_id"))

      (and (= part "F") next-id)
      (conj (str "parser IR sentence " sid " part=F but has next_id"))

      (and (= part "F") (nil? prev-id))
      (conj (str "parser IR sentence " sid " part=F but no prev_id"))

      (and (nil? part) next-id)
      (conj (str "parser IR sentence " sid " has next_id but no part"))

      (and (nil? part) prev-id)
      (conj (str "parser IR sentence " sid " has prev_id but no part")))))
```

- [ ] **Step 2: Wire into `sentence-coherence-errors`**

Add `(mapcat fragment-field-errors sentences)` to the error collection.

- [ ] **Step 3: Add test**

```clojure
(deftest fragment-field-coherence-test
  (testing "valid fragment fields pass"
    (let [sentences [{:id "s0" :part "I" :fragment_group "fg0" :next_id "s1"}
                     {:id "s1" :part "F" :fragment_group "fg0" :prev_id "s0"}]]
      (is (empty? (fragment-field-errors (first sentences))))))
  (testing "part=I without next_id fails"
    (let [sentence {:id "s0" :part "I" :fragment_group "fg0"}]
      (is (seq (fragment-field-errors sentence))))))
```

- [ ] **Step 4: Run tests**

Run: `just abc-clj-kondo`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add abc/src/abc/tools/parser_ir_sentence_policy.clj
git commit -m "feat(abc): add fragment field coherence checks

Validates part↔next_id/prev_id/fragment_group constraints in the
publication gate."
```

---

### Task 9: TEI Profile Schema Update

**Files:**
- Modify: `abc/schemas/tei-profile.rng`

**Interfaces:**
- Produces: `att.linking.attributes` referenced by `<s>`
- Allows: `@next` and `@prev` on `<s>`

- [ ] **Step 1: Add `att.linking` reference to `<s>` element**

Find the `<s>` element definition (around line 13270) and add:

```xml
<ref name="att.linking.attributes"/>
```

After the existing `<ref name="att.segLike.attributes"/>` line.

- [ ] **Step 2: Verify schema validates**

Run: `just nix-format-check` (if RNG is nix-formatted) or manual validation

- [ ] **Step 3: Commit**

```bash
git add abc/schemas/tei-profile.rng
git commit -m "feat(abc): allow @next/@prev on s element

Adds att.linking reference to s for TEI Chapter 21 fragmentation."
```

---

### Task 10: Splitter ID Update + Final Integration

**Files:**
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs` (splitter_id)

**Interfaces:**
- Produces: `splitter_id: "ab-plaintext-japanese-v2"` in sentence_segmentation

- [ ] **Step 1: Update `segmentation_meta` function**

```rust
pub fn segmentation_meta() -> SentenceSegmentation {
    SentenceSegmentation {
        schema_version: "sentence-segmentation-v1".to_owned(),
        splitter_id: "ab-plaintext-japanese-v2".to_owned(),
        coordinate_system: "decoded_utf8".to_owned(),
        coverage: "body-paragraphs".to_owned(),
    }
}
```

- [ ] **Step 2: Run full test suite**

Run: `just validate-migration`
Expected: PASS

- [ ] **Step 3: Commit**

```bash
git add ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs
git commit -m "feat(ab-aat-to-parser-ir): update splitter_id to v2

Reflects the new split_sentences logic with improved rules."
```

---

### Task 11: Corpus Validation

**Files:**
- None (validation run only)

- [ ] **Step 1: Run full-adapter corpus audit**

Run: `just aat-to-parser-ir-full-audit 24`
Expected: 0 new failures beyond the accepted gaiji residual

- [ ] **Step 2: Document results**

Create report at `ab-validator/docs/reports/2026-07-08-nested-sentence-fragmentation-audit.md` with:
- Total works audited
- Works with fragmented sentences
- New failure classes (if any)
- Residual accepted failures

- [ ] **Step 3: Commit**

```bash
git add ab-validator/docs/reports/
git commit -m "docs: add nested sentence fragmentation audit report"
```
