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
- `splitter_id` updates to `"ab-plaintext-japanese-v2"` (gated on compatibility report)

---

## File Structure

| File | Action | Responsibility |
|---|---|---|
| `ab-validator/crates/ab-plaintext/src/lib.rs` | Modify | Replace `sentence_split` with `split_sentences` + `SplitOptions` |
| `abc/schemas/parser-ir.schema.json` | Modify | Add optional fragment fields to `$defs/sentence` |
| `ab-validator/data/abc-schemas/nix-schemas/parser-ir.schema.json` | Modify | Mirror ABC schema |
| `ab-validator/data/aat-to-parser-ir-mapping-v1.json` | Modify | Update `target_parser_ir_schema_hash` |
| `ab-validator/crates/ab-aat-to-parser-ir/src/convert.rs` | Modify | Extract `map_text_node`, add quote synthesis |
| `ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs` | Modify | Extend `ParserIrSentence`, nesting detection, fragment assembly |
| `ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/*.json` | Create | Test fixtures |
| `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs` | Modify | Fragment assembly tests |
| `abc/src/abc/tools/parser_ir_tei.clj` | Modify | Fragment attribute rendering |
| `abc/src/abc/tools/parser_ir_sentence_policy.clj` | Modify | Fragment field coherence checks |
| `abc/test/abc/tools/parser_ir_tei_test.clj` | Modify | Renderer fragment tests |
| `abc/schemas/tei-profile.rng` | Modify | Add `@next`/`@prev` to `<s>` |

---

### Task 1: Splitter Replacement — `split_sentences` Core

**Files:**
- Modify: `ab-validator/crates/ab-plaintext/src/lib.rs`

**Interfaces:**
- Produces: `pub struct SplitOptions { pub suppress_closing_bracket_check: bool }`
- Produces: `pub fn split_sentences_with_options(input: &str, opts: &SplitOptions) -> Vec<SentenceSpan<'_>>`
- Produces: `pub fn split_sentences(input: &str) -> Vec<SentenceSpan<'_>>` (default opts wrapper)

- [ ] **Step 1: Add `SplitOptions` struct**

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SplitOptions {
    /// When true, suppress all bracket/quote-based split suppression.
    /// Used when re-splitting text inside a known quote region.
    pub suppress_closing_bracket_check: bool,
}

impl Default for SplitOptions {
    fn default() -> Self {
        Self {
            suppress_closing_bracket_check: false,
        }
    }
}
```

- [ ] **Step 2: Add `split_sentences_with_options` function**

Rewrite on `char_indices()` matching the existing `sentence_split` structure. The five new rules layer onto the proven pattern:

```rust
pub fn split_sentences_with_options(
    input: &str,
    opts: &SplitOptions,
) -> Vec<SentenceSpan<'_>> {
    let mut spans = Vec::new();
    let mut byte_start = 0usize;
    let mut char_start = 0usize;
    let chars: Vec<(usize, char)> = input.char_indices().collect();

    for (i, &(_byte_pos, ch)) in chars.iter().enumerate() {
        if !is_sentence_terminal(ch) {
            continue;
        }

        let prev = (i > 0).then_some(chars[i - 1].1);
        let next = chars.get(i + 1).map(|(_, ch)| *ch);

        let should_split = match (prev, next) {
            (Some(prev_ch), Some(next_ch)) => {
                // Rule: repeated delimiters don't split
                if is_sentence_terminal(next_ch) {
                    false
                }
                // Rule: CLOSING_QUOTATIONS after delimiter suppress split
                // BUT only when suppress_closing_bracket_check is false
                else if !opts.suppress_closing_bracket_check
                    && is_closing_quote_or_bracket(next_ch)
                {
                    false
                }
                // Rule: 。 splits unless closing_bracket_ahead finds a bracket
                // BUT only when suppress_closing_bracket_check is false
                else if ch == '。'
                    && !opts.suppress_closing_bracket_check
                    && closing_bracket_ahead(input, chars[i].0 + ch.len_utf8())
                {
                    false
                }
                // Rule: CJK numbered list (１．)
                else if (ch == '.' || ch == '．') && is_cjk_digit(prev_ch)
                {
                    false
                }
                // Rule: ！/？ + Japanese continuation
                else if (ch == '！' || ch == '？') && is_japanese_continuation(next_ch)
                {
                    false
                }
                // Rule: alphanumeric period between alphanumerics
                else if is_period_non_boundary_neighbor(prev_ch)
                    && is_period_non_boundary_neighbor(next_ch)
                    && ch != '。'
                {
                    false
                }
                else {
                    true
                }
            }
            (Some(_prev_ch), None) => {
                // End of input: split only on 。
                ch == '。'
            }
            _ => true,
        };

        if should_split {
            // Consume adjacent terminals
            let mut end_idx = i + 1;
            while end_idx < chars.len() && is_sentence_terminal(chars[end_idx].1) {
                end_idx += 1;
            }
            let byte_end = if end_idx < chars.len() {
                chars[end_idx].0
            } else {
                input.len()
            };
            let text_slice = &input[byte_start..byte_end];
            if !text_slice.trim().is_empty() {
                spans.push(SentenceSpan {
                    text: text_slice,
                    byte_offset: byte_start,
                    char_offset: char_start,
                });
            }
            char_start += text_slice.chars().count();
            byte_start = byte_end;
        }
    }

    // Trailing text
    if byte_start < input.len() {
        let text_slice = &input[byte_start..];
        if !text_slice.trim().is_empty() {
            spans.push(SentenceSpan {
                text: text_slice,
                byte_offset: byte_start,
                char_offset: char_start,
            });
        }
    }

    spans
}

/// Convenience wrapper with default options.
pub fn split_sentences(input: &str) -> Vec<SentenceSpan<'_>> {
    split_sentences_with_options(input, &SplitOptions::default())
}
```

Helper functions (add alongside existing `is_sentence_terminal`):

```rust
fn is_cjk_digit(ch: char) -> bool {
    matches!(ch,
        '0'..='9' | '０'..='９' | '〇' |
        '一' | '二' | '三' | '四' | '五' | '六' | '七' | '八' | '九' | '十'
    )
}

fn is_japanese_continuation(ch: char) -> bool {
    matches!(ch,
        '笑' | '泣' | '汗' | '涙' | '怒' | '嬉' | '爆' | '驚' | '喜' | '悲' |
        '謎' | '恥' | '焦' | '苦' | '照' | '憂' |
        '…' | '〜' |
        'と' | 'っ' | 'ぁ' | 'ぃ' | 'ぅ' | 'ぇ' | 'ぉ' |
        'ッ' | 'ァ' | 'ィ' | 'ゥ' | 'ェ' | 'ォ'
    )
}

fn closing_bracket_ahead(input: &str, byte_pos: usize) -> bool {
    const CLOSING: &[char] = &[')', '）', '」', '』', '】', '］', '〕', '〉', '》'];
    const OPENING: &[char] = &['(', '（', '「', '『', '【', '［', '〔', '〈', '《'];
    let mut depth = 0i32;
    for ch in input[byte_pos..].chars() {
        if ch == '\n' { return false; }
        if CLOSING.contains(&ch) {
            if depth == 0 { return true; }
            depth -= 1;
        } else if OPENING.contains(&ch) {
            depth += 1;
        }
    }
    false
}

fn is_closing_quote_or_bracket(ch: char) -> bool {
    matches!(ch,
        ')' | '）' | '」' | '』' | '】' | '］' | '〕' | '〉' | '》' | ']' |
        '"' | '\u{201D}' | '\u{2019}'
    )
}

fn is_period_non_boundary_neighbor(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || matches!(ch, '０'..='９' | 'Ａ'..='Ｚ' | 'ａ'..='ｚ')
}
```

- [ ] **Step 3: Add unit tests**

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
    let spans = split_sentences("先生は「綺麗ですよ。落葉で埋まります」といった。");
    assert_eq!(spans.len(), 1);
}

#[test]
fn split_sentences_suppress_mode_splits_before_close() {
    // Real Kokoro inner text ending with 。」
    let text = "もう少しすると、綺麗ですよ。この木が埋まるようになります」";
    let opts = SplitOptions { suppress_closing_bracket_check: true };
    let spans = split_sentences_with_options(text, &opts);
    assert_eq!(spans.len(), 2);
    assert_eq!(spans[0].text, "もう少しすると、綺麗ですよ。");
    assert_eq!(spans[1].text, "この木が埋まるようになります」");
}

#[test]
fn split_sentences_newlines_not_boundaries() {
    let spans = split_sentences("一行目\n二行目。");
    assert_eq!(spans.len(), 1);
    assert_eq!(spans[0].text, "一行目\n二行目。");
}
```

- [ ] **Step 4: Run tests**

Run: `cargo test -p ab-plaintext -- split_sentences`
Expected: All new tests PASS

- [ ] **Step 5: Commit**

```bash
git add ab-validator/crates/ab-plaintext/src/lib.rs
git commit -m "feat(ab-plaintext): add split_sentences with SplitOptions

New splitter on char_indices with improved rules: CJK numbered lists,
exclamation continuation, closing_bracket_ahead with depth tracking.
suppress_closing_bracket_check bypasses all bracket/quote suppression."
```

---

### Task 2: Parser-IR Schema Update + Mapping Hash

**Files:**
- Modify: `abc/schemas/parser-ir.schema.json`
- Modify: `ab-validator/data/abc-schemas/nix-schemas/parser-ir.schema.json`
- Modify: `ab-validator/data/aat-to-parser-ir-mapping-v1.json`

**Interfaces:**
- Produces: Optional `part`, `fragment_group`, `next_id`, `prev_id` fields on `$defs/sentence`
- Produces: Updated `target_parser_ir_schema_hash` in mapping doc

- [ ] **Step 1: Add fragment fields to `$defs/sentence`**

In both `abc/schemas/parser-ir.schema.json` and the mirror, add to `properties` of `sentence`:

```json
"part": { "type": "string", "enum": ["I", "M", "F"] },
"fragment_group": { "type": "string", "pattern": "^fg[0-9]{6}$" },
"next_id": { "type": "string", "pattern": "^s[0-9]{6}$" },
"prev_id": { "type": "string", "pattern": "^s[0-9]{6}$" }
```

- [ ] **Step 2: Regenerate mapping hash**

The converter bails if `mapping.target_parser_ir_schema_hash` doesn't match the schema content hash. After editing the schema:

```bash
# Compute new hash
sha256sum abc/schemas/parser-ir.schema.json
# Update ab-validator/data/aat-to-parser-ir-mapping-v1.json:
#   "target_parser_ir_schema_hash": "sha256:<new-hash>"
#   "mapping_version": "0.2.7"  (bump patch)
```

- [ ] **Step 3: Verify converter loads mapping**

Run: `cargo check -p ab-aat-to-parser-ir`
Expected: PASS

- [ ] **Step 4: Commit**

```bash
git add abc/schemas/parser-ir.schema.json
git add ab-validator/data/abc-schemas/nix-schemas/parser-ir.schema.json
git add ab-validator/data/aat-to-parser-ir-mapping-v1.json
git commit -m "feat(abc): add fragment fields to sentence schema + update mapping hash

Adds optional part, fragment_group, next_id, prev_id. Bumps
target_parser_ir_schema_hash and mapping_version."
```

---

### Task 3: Extend `ParserIrSentence` Struct

**Files:**
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs`

**Interfaces:**
- Modifies: `ParserIrSentence` with new optional fields
- Produces: Serializable fragment fields (omitted when None)

- [ ] **Step 1: Add fragment fields to `ParserIrSentence`**

```rust
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
```

- [ ] **Step 2: Update all `ParserIrSentence` construction sites**

Add `part: None, fragment_group: None, next_id: None, prev_id: None` to every existing `ParserIrSentence { ... }` literal.

- [ ] **Step 3: Run tests**

Run: `cargo test -p ab-aat-to-parser-ir`
Expected: PASS

- [ ] **Step 4: Commit**

```bash
git add ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs
git commit -m "feat(ab-aat-to-parser-ir): add fragment fields to ParserIrSentence"
```

---

### Task 4: Quote Node Synthesis in Converter

**Files:**
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/convert.rs`

**Interfaces:**
- Produces: Quote nodes with `type: "quote"`, `marker_type`, `text`
- Preserves: Source spans via synthetic spans (no false source-pointer claims)

- [ ] **Step 0: Extract `map_text_node` from `map_inline_to_nodes`**

Refactor: extract the existing inline `"text"` arm into a standalone function. Behavior-preserving — no quote logic yet.

```rust
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
    let text = node["value"].as_str().unwrap_or("");
    let end = offset + utf8_len(text);
    let span = map_span(node.get("span"), offset, end, recorder, path)?;
    nodes.push(json!({"type": "text", "span": span, "text": text}));
    Ok(end)
}
```

Change the `"text"` arm in `map_inline_to_nodes` to: `return map_text_node(node, nodes, recorder, synthetic_warnings, offset, path);`

Run: `cargo test -p ab-aat-to-parser-ir`
Expected: PASS (behavior unchanged)

- [ ] **Step 1: Add `map_text_node_with_quotes`**

Same 6-param signature. Splits text at `「」`/`『』`. Uses `map_span(None, ...)` for sub-segments (synthetic spans, no false source-pointer):

```rust
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
        if quote_chars.contains(&ch) {
            // Emit text segment before quote (if non-empty)
            if segment_start < i {
                let segment_text = &text[segment_start..i];
                let end = pos + utf8_len(segment_text);
                let span = map_span(None, pos, end, recorder, path)?;
                nodes.push(json!({"type": "text", "span": span, "text": segment_text}));
                pos = end;
            }

            // Emit quote node with synthetic span
            let marker_type = match ch {
                '「' | '『' => "open",
                '」' | '』' => "close",
                _ => unreachable!(),
            };
            let ch_len = ch.len_utf8() as u64;
            let span = map_span(None, pos, pos + ch_len, recorder, path)?;
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
    }

    // Emit remaining text
    if segment_start < text.len() {
        let segment_text = &text[segment_start..];
        let end = pos + utf8_len(segment_text);
        let span = map_span(None, pos, end, recorder, path)?;
        nodes.push(json!({"type": "text", "span": span, "text": segment_text}));
        pos = end;
    }

    Ok(pos)
}
```

- [ ] **Step 2: Wire into `map_inline_to_nodes`**

Change the `"text"` arm to: `return map_text_node_with_quotes(node, nodes, recorder, synthetic_warnings, offset, path);`

- [ ] **Step 3: Create fixture**

Create `ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/quote-node-emission.aat.json`:

```json
{
  "version": 1,
  "work_id": "test-quote-nodes",
  "blocks": [
    {
      "kind": "paragraph",
      "content": [
        {"kind": "text", "value": "先生は「綺麗だ」といった。"}
      ]
    }
  ],
  "meta": {
    "adapter": "fixture",
    "adapter_version": "0.0.0",
    "source_encoding": "utf-8",
    "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
    "parse_complete": true,
    "warnings": []
  }
}
```

- [ ] **Step 4: Add integration test**

```rust
#[test]
fn quote_node_emission_from_text() {
    let (schemas, mapping) = schemas_and_mapping();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: include_fixture_json("quote-node-emission.aat.json"),
        schemas: &schemas,
        mapping: &mapping,
        options: ConversionOptions::default(),
    }).unwrap();
    let nodes = output.parser_ir["nodes"].as_array().unwrap();
    let quote_nodes: Vec<_> = nodes.iter()
        .filter(|n| n["type"] == "quote")
        .collect();
    assert_eq!(quote_nodes.len(), 2);
    assert_eq!(quote_nodes[0]["marker_type"], "open");
    assert_eq!(quote_nodes[0]["text"], "「");
    assert_eq!(quote_nodes[1]["marker_type"], "close");
    assert_eq!(quote_nodes[1]["text"], "」");
    // Sub-segments have synthetic spans (no parent source span)
    assert_eq!(quote_nodes[0]["span"]["coordinate_system"], "decoded_utf8");
}
```

- [ ] **Step 5: Run tests**

Run: `cargo test -p ab-aat-to-parser-ir -- quote_node_emission`
Expected: PASS

- [ ] **Step 6: Commit**

```bash
git add ab-validator/crates/ab-aat-to-parser-ir/src/convert.rs
git add ab-validator/crates/ab-aat-to-parser-ir/tests/
git commit -m "feat(ab-aat-to-parser-ir): synthesize quote nodes from 「」/『』

Extract map_text_node from map_inline_to_nodes, then add
map_text_node_with_quotes. Sub-segments use synthetic spans."
```

---

### Task 5: Nesting Detection (Recursive)

**Files:**
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs`

**Interfaces:**
- Produces: `struct NestedRegion { inner_byte_start, inner_byte_end, opening_marker_node, closing_marker_node, nesting_level }`
- Produces: `fn detect_nested_regions(nodes: &[Value], node_start: usize, node_end: usize) -> Vec<NestedRegion>`
- Consumes: Quote nodes from Task 4

- [ ] **Step 1: Add `NestedRegion` struct**

```rust
const MAX_NESTING_DEPTH: usize = 5;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct NestedRegion {
    pub inner_byte_start: usize,
    pub inner_byte_end: usize,
    pub opening_marker_node: usize,
    pub closing_marker_node: usize,
    pub nesting_level: usize,
}
```

- [ ] **Step 2: Add recursive `detect_nested_regions` function**

Operates on **node indices** (not byte offsets). Recurses on inner node slices:

```rust
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
        if node.get("type").and_then(Value::as_str) != Some("quote") {
            i += 1;
            continue;
        }
        if node.get("marker_type").and_then(Value::as_str) != Some("open") {
            i += 1;
            continue;
        }

        let open_byte_end = value_usize(node, "/span/end", "quote span.end").unwrap_or(0);
        let open_node = i;

        // Find matching close
        let mut j = i + 1;
        let mut inner_depth = 0usize;
        while j < range_end {
            let next = &nodes[j];
            if next.get("type").and_then(Value::as_str) == Some("quote") {
                match next.get("marker_type").and_then(Value::as_str) {
                    Some("open") => inner_depth += 1,
                    Some("close") => {
                        if inner_depth == 0 {
                            let close_byte_start = value_usize(next, "/span/start", "quote span.start").unwrap_or(0);
                            let close_node = j;

                            // Emit this region
                            regions.push(NestedRegion {
                                inner_byte_start: open_byte_end,
                                inner_byte_end: close_byte_start,
                                opening_marker_node: open_node,
                                closing_marker_node: close_node,
                                nesting_level: depth,
                            });

                            // Recurse on inner nodes
                            let inner = detect_nested_regions_inner(
                                nodes, open_node + 1, close_node, depth + 1,
                            );
                            regions.extend(inner);

                            i = close_node + 1;
                            break;
                        }
                        inner_depth -= 1;
                    }
                    _ => {}
                }
            }
            j += 1;
        }
        if j >= range_end {
            i += 1; // Unmatched open — skip
        }
    }

    regions
}
```

- [ ] **Step 3: Add unit tests**

```rust
#[test]
fn detect_nested_regions_basic() {
    let nodes = vec![
        json!({"type":"text","span":{"start":0,"end":6},"text":"先生は"}),
        json!({"type":"quote","span":{"start":6,"end":9},"marker_type":"open","text":"「"}),
        json!({"type":"text","span":{"start":9,"end":15},"text":"綺麗だ"}),
        json!({"type":"quote","span":{"start":15,"end":18},"marker_type":"close","text":"」"}),
        json!({"type":"text","span":{"start":18,"end":24},"text":"といった"}),
    ];
    let regions = detect_nested_regions(&nodes, 0, 5);
    assert_eq!(regions.len(), 1);
    assert_eq!(regions[0].inner_byte_start, 9);
    assert_eq!(regions[0].inner_byte_end, 15);
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
    let regions = detect_nested_regions(&nodes, 0, 7);
    assert_eq!(regions.len(), 2);
    assert_eq!(regions[0].nesting_level, 0);
    assert_eq!(regions[1].nesting_level, 1);
}

#[test]
fn detect_nested_regions_unmatched() {
    let nodes = vec![
        json!({"type":"quote","span":{"start":0,"end":3},"marker_type":"open","text":"「"}),
        json!({"type":"text","span":{"start":3,"end":9},"text":"閉じない"}),
    ];
    let regions = detect_nested_regions(&nodes, 0, 2);
    assert!(regions.is_empty());
}
```

- [ ] **Step 4: Run tests**

Run: `cargo test -p ab-aat-to-parser-ir -- detect_nested_regions`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs
git commit -m "feat(ab-aat-to-parser-ir): add recursive nesting detection

Operates on node indices. Recurses on inner node slices for nested
quotes. Max depth 5."
```

---

### Task 6: Fragment Assembly

**Files:**
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs`

**Interfaces:**
- Modifies: `project_body_paragraph` to merge flat + inner boundaries before single split pass
- Produces: Sentence rows with `part`, `fragment_group`, `next_id`, `prev_id`
- Consumes: `detect_nested_regions` from Task 5, `split_sentences_with_options` from Task 1

- [ ] **Step 1: Restructure `project_body_paragraph`**

The key insight: flat split and inner re-split boundaries must be merged BEFORE the single `split_node_at_boundaries` pass, because nodes are consumed (mem::taken) during that pass.

```rust
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

    // Invariant: visible text length == byte span length
    debug_assert_eq!(
        paragraph_text.len(),
        (paragraph_end - paragraph_start) as usize,
        "visible text length must equal byte span length"
    );

    // Step 1: Flat split
    let mut flat_bounds: Vec<SentenceBounds> = ab_plaintext::split_sentences(&paragraph_text)
        .into_iter()
        .map(|span| SentenceBounds {
            start: paragraph_start + span.byte_offset,
            end: paragraph_start + span.byte_offset + span.text.len(),
        })
        .collect();

    // Whitespace handling (existing)
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

    // Step 2: For each flat sentence, detect nested regions and compute inner boundaries
    let mut all_split_boundaries: Vec<usize> = flat_bounds
        .iter()
        .take(flat_bounds.len().saturating_sub(1))
        .map(|s| s.end)
        .collect();

    let mut sentence_nested_info: Vec<(SentenceBounds, Vec<NestedRegion>)> = Vec::new();

    for flat in &flat_bounds {
        // Find node-index range for this sentence's byte range
        let (node_start, node_end) = find_node_range_for_byte_range(
            original_nodes, flat.start, flat.end,
        );
        let nested = detect_nested_regions(original_nodes, node_start, node_end);

        for region in &nested {
            // Re-split inner text
            let inner_start = (region.inner_byte_start - paragraph_start) as usize;
            let inner_end = (region.inner_byte_end - paragraph_start) as usize;
            let inner_text = &paragraph_text[inner_start..inner_end];
            let inner_spans = ab_plaintext::split_sentences_with_options(
                inner_text,
                &SplitOptions { suppress_closing_bracket_check: true },
            );
            for span in &inner_spans {
                let abs_boundary = region.inner_byte_start + span.byte_offset + span.text.len();
                if abs_boundary < region.inner_byte_end {
                    all_split_boundaries.push(abs_boundary);
                }
            }
        }

        sentence_nested_info.push((*flat, nested));
    }

    all_split_boundaries.sort();
    all_split_boundaries.dedup();

    // Step 3: Single split_node_at_boundaries pass
    for node in original_nodes.iter_mut() {
        split_node_at_boundaries(node, &all_split_boundaries, rewritten_nodes)?;
    }

    let paragraph_rewritten_end = rewritten_nodes.len();

    // Step 4: Build fine-grained bounds
    let mut fine_bounds: Vec<(SentenceBounds, Option<&NestedRegion>)> = Vec::new();

    for (flat, nested) in &sentence_nested_info {
        if nested.is_empty() {
            fine_bounds.push((*flat, None));
        } else {
            let mut cursor = flat.start;
            for (idx, region) in nested.iter().enumerate() {
                // Outer fragment before this region
                if cursor < region.inner_byte_start {
                    fine_bounds.push((
                        SentenceBounds { start: cursor, end: region.inner_byte_start },
                        None, // outer fragment
                    ));
                }

                // Inner sentences within this region
                let inner_start = (region.inner_byte_start - paragraph_start) as usize;
                let inner_end = (region.inner_byte_end - paragraph_start) as usize;
                let inner_text = &paragraph_text[inner_start..inner_end];
                let inner_spans = ab_plaintext::split_sentences_with_options(
                    inner_text,
                    &SplitOptions { suppress_closing_bracket_check: true },
                );
                for span in &inner_spans {
                    let abs_start = region.inner_byte_start + span.byte_offset;
                    let abs_end = abs_start + span.text.len();
                    fine_bounds.push((
                        SentenceBounds { start: abs_start, end: abs_end },
                        Some(region),
                    ));
                }

                cursor = region.inner_byte_end;
            }
            // Outer fragment after last region
            if cursor < flat.end {
                fine_bounds.push((
                    SentenceBounds { start: cursor, end: flat.end },
                    None,
                ));
            }
        }
    }

    // Step 5: Build sentence rows with fragment fields
    let mut rows = Vec::new();
    let mut fragment_group_counter = 0usize;
    let mut node_cursor = paragraph_rewritten_start;

    for (bounds, region_opt) in &fine_bounds {
        let sentence_node_start = node_cursor;
        while node_cursor < paragraph_rewritten_end
            && node_belongs_to_sentence(&rewritten_nodes[node_cursor], *bounds)?
        {
            node_cursor += 1;
        }
        let sentence_node_end = node_cursor;

        let local_index = rows.len();
        let annotation_indices = overlapping_ortho_indices(bounds.start, bounds.end, ortho);
        let tags = if annotation_indices.is_empty() {
            Vec::new()
        } else {
            vec!["orthographic-katakana".to_owned()]
        };

        let (part, fragment_group, next_id, prev_id) = if region_opt.is_some() {
            // Inner sentence — no fragment fields
            (None, None, None, None)
        } else {
            // Check if this is an outer fragment
            // Find which nested regions this flat sentence contains
            let flat = sentence_nested_info.iter()
                .find(|(f, _)| f.start <= bounds.start && bounds.end <= f.end)
                .map(|(_, n)| n);

            match flat {
                Some(nested) if !nested.is_empty() => {
                    let group_id = format!("fg{:06}", fragment_group_counter);
                    fragment_group_counter += 1;

                    // Determine if this is I, M, or F
                    let is_before_first = bounds.end <= nested[0].inner_byte_start;
                    let is_after_last = bounds.start >= nested.last().unwrap().inner_byte_end;

                    if is_before_first {
                        // part="I" — find the next sentence in this group
                        // (first inner sentence or next outer fragment)
                        let next = find_next_in_group(&fine_bounds, &sentence_nested_info, bounds);
                        (Some("I".to_string()), Some(group_id), next, None)
                    } else if is_after_last {
                        // part="F" — find the previous sentence in this group
                        let prev = find_prev_in_group(&fine_bounds, &sentence_nested_info, bounds);
                        (Some("F".to_string()), Some(group_id), None, prev)
                    } else {
                        // part="M" — between nested regions
                        let next = find_next_in_group(&fine_bounds, &sentence_nested_info, bounds);
                        let prev = find_prev_in_group(&fine_bounds, &sentence_nested_info, bounds);
                        (Some("M".to_string()), Some(group_id), next, prev)
                    }
                }
                _ => (None, None, None, None),
            }
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
            part,
            fragment_group,
            next_id,
            prev_id,
        });
    }

    // Step 6: Assertions
    assert_sentence_span_tiling(paragraph_start, paragraph_end,
        &fine_bounds.iter().map(|(b, _)| *b).collect::<Vec<_>>())?;
    assert_fragment_field_coherence(&rows)?;

    Ok(rows)
}
```

Helper: find node-index range for a byte range:

```rust
fn find_node_range_for_byte_range(
    nodes: &[Value],
    byte_start: usize,
    byte_end: usize,
) -> (usize, usize) {
    let mut start = 0;
    let mut found_start = false;
    for (i, node) in nodes.iter().enumerate() {
        let n_start = value_usize(node, "/span/start", "").unwrap_or(0);
        let n_end = value_usize(node, "/span/end", "").unwrap_or(0);
        if n_start >= byte_start && !found_start {
            start = i;
            found_start = true;
        }
        if n_end >= byte_end {
            return (start, i + 1);
        }
    }
    (start, nodes.len())
}
```

- [ ] **Step 2: Add `assert_fragment_field_coherence`**

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
            _ => bail!("sentence {} has invalid part: {:?}", row.id, row.part),
        }
    }
    Ok(())
}
```

- [ ] **Step 3: Create fixtures**

Create `ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/nested-sentence-basic.aat.json` (single inner sentence):

```json
{
  "version": 1, "work_id": "test-frag-basic", "blocks": [{"kind": "paragraph", "content": [
    {"kind": "text", "value": "先生は梢を見上げて、「綺麗です」といった。"}
  ]}],
  "meta": {"adapter": "fixture", "adapter_version": "0.0.0", "source_encoding": "utf-8", "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000", "parse_complete": true, "warnings": []}
}
```

Create `ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/nested-sentence-multiple.aat.json` (two inner sentences — Kokoro):

```json
{
  "version": 1, "work_id": "test-frag-multi", "blocks": [{"kind": "paragraph", "content": [
    {"kind": "text", "value": "先生は高い梢を見上げて、「もう少しすると、綺麗ですよ。"},
    {"kind": "text", "value": "この木がすっかり黄葉して、ここいらの地面は金色の落葉で埋まるようになります」"},
    {"kind": "text", "value": "といった。"}
  ]}],
  "meta": {"adapter": "fixture", "adapter_version": "0.0.0", "source_encoding": "utf-8", "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000", "parse_complete": true, "warnings": []}
}
```

- [ ] **Step 4: Add integration tests**

```rust
#[test]
fn fragment_assembly_single_inner_sentence() {
    let (schemas, mapping) = schemas_and_mapping();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: include_fixture_json("nested-sentence-basic.aat.json"),
        schemas: &schemas,
        mapping: &mapping,
        options: ConversionOptions { validate_output_parser_ir: true, ..Default::default() },
    }).unwrap();
    let sentences = output.parser_ir["sentences"].as_array().unwrap();
    // I, inner, F = 3 rows
    assert_eq!(sentences.len(), 3);
    assert_eq!(sentences[0]["part"], "I");
    assert_eq!(sentences[1]["part"], serde_json::Value::Null);
    assert_eq!(sentences[2]["part"], "F");
    assert_eq!(sentences[0]["next_id"], sentences[2]["id"]);
    assert_eq!(sentences[2]["prev_id"], sentences[0]["id"]);
}

#[test]
fn fragment_assembly_multiple_inner_sentences() {
    let (schemas, mapping) = schemas_and_mapping();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: include_fixture_json("nested-sentence-multiple.aat.json"),
        schemas: &schemas,
        mapping: &mapping,
        options: ConversionOptions { validate_output_parser_ir: true, ..Default::default() },
    }).unwrap();
    let sentences = output.parser_ir["sentences"].as_array().unwrap();
    // I, inner1, inner2, F = 4 rows
    assert_eq!(sentences.len(), 4);
    assert_eq!(sentences[0]["part"], "I");
    assert_eq!(sentences[1]["part"], serde_json::Value::Null);
    assert_eq!(sentences[2]["part"], serde_json::Value::Null);
    assert_eq!(sentences[3]["part"], "F");
}

#[test]
fn fragment_field_coherence() {
    let (schemas, mapping) = schemas_and_mapping();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: include_fixture_json("nested-sentence-multiple.aat.json"),
        schemas: &schemas,
        mapping: &mapping,
        options: ConversionOptions::default(),
    }).unwrap();
    let sentences = output.parser_ir["sentences"].as_array().unwrap();
    for s in sentences {
        let part = s.get("part").and_then(|v| v.as_str());
        let next = s.get("next_id");
        let prev = s.get("prev_id");
        let group = s.get("fragment_group");
        match part {
            Some("I") => {
                assert!(next.is_some() && next != Some(&serde_json::Value::Null));
                assert!(prev.is_none() || prev == Some(&serde_json::Value::Null));
                assert!(group.is_some());
            }
            Some("F") => {
                assert!(prev.is_some() && prev != Some(&serde_json::Value::Null));
                assert!(next.is_none() || next == Some(&serde_json::Value::Null));
                assert!(group.is_some());
            }
            None => {
                assert!(group.is_none() || group == Some(&serde_json::Value::Null));
                assert!(next.is_none() || next == Some(&serde_json::Value::Null));
                assert!(prev.is_none() || prev == Some(&serde_json::Value::Null));
            }
            _ => panic!("unexpected part: {:?}", part),
        }
    }
}
```

- [ ] **Step 5: Run tests**

Run: `cargo test -p ab-aat-to-parser-ir -- fragment`
Expected: PASS

- [ ] **Step 6: Commit**

```bash
git add ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs
git add ab-validator/crates/ab-aat-to-parser-ir/tests/
git commit -m "feat(ab-aat-to-parser-ir): add fragment assembly for nested sentences

Merges flat + inner boundaries before single split_node_at_boundaries
pass. Creates fragment rows with part/fragment_group/next_id/prev_id."
```

---

### Task 7: ABC TEI Renderer Fragment Attributes

**Files:**
- Modify: `abc/src/abc/tools/parser_ir_tei.clj`

**Interfaces:**
- Consumes: `part`, `next_id`, `prev_id` from sentence rows
- Produces: `<s>` elements with `@part`, `@xml:id`, `@next`, `@prev`

- [ ] **Step 1: Update `sentence-attrs` function**

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

```clojure
(deftest fragment-attributes-test
  (testing "fragmented sentences render with part/xml:id/next/prev"
    (let [parser-ir {"nodes" [{"type" "text" "span" {"start" 0 "end" 12} "text" "先生は言った。"}
                              {"type" "text" "span" {"start" 12 "end" 24} "text" "「綺麗だ」といった。"}]
                     "paragraphs" [{"id" "p000000" "role" "body" "span" {"start" 0 "end" 24}
                                    "node_range" {"start" 0 "end" 2}}]
                     "sentences" [{"id" "s000000" "paragraph_id" "p000000"
                                   "span" {"start" 0 "end" 12} "node_range" {"start" 0 "end" 1}
                                   "tags" [] "orthographic_annotation_indices" []
                                   "part" "I" "fragment_group" "fg000000" "next_id" "s000002"}
                                  {"id" "s000001" "paragraph_id" "p000000"
                                   "span" {"start" 12 "end" 18} "node_range" {"start" 1 "end" 2}
                                   "tags" [] "orthographic_annotation_indices" []}
                                  {"id" "s000002" "paragraph_id" "p000000"
                                   "span" {"start" 18 "end" 24} "node_range" {"start" 2 "end" 3}
                                   "tags" [] "orthographic_annotation_indices" []
                                   "part" "F" "fragment_group" "fg000000" "prev_id" "s000000"}]}
          result (render parser-ir)
          body (get-in result [:body])
          s-elements (second (second body))]
      (is (= "s000000" (get-in s-elements [0 1 :xml:id])))
      (is (= "I" (get-in s-elements [0 1 :part])))
      (is (= "#s000002" (get-in s-elements [0 1 :next])))
      (is (nil? (get-in s-elements [0 1 :prev])))
      (is (= "s000001" (get-in s-elements [1 1 :xml:id])))
      (is (nil? (get-in s-elements [1 1 :part])))
      (is (= "F" (get-in s-elements [2 1 :part])))
      (is (= "#s000000" (get-in s-elements [2 1 :prev]))))))
```

- [ ] **Step 3: Run tests**

Run: `nix build .#checks.x86_64-linux.abc-clj-kondo`
Expected: PASS

- [ ] **Step 4: Commit**

```bash
git add abc/src/abc/tools/parser_ir_tei.clj
git add abc/test/abc/tools/parser_ir_tei_test.clj
git commit -m "feat(abc): render sentence fragment attributes"
```

---

### Task 8: ABC Fragment Field Coherence (Publication Gate)

**Files:**
- Modify: `abc/src/abc/tools/parser_ir_sentence_policy.clj`

- [ ] **Step 1: Add fragment field checks**

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

- [ ] **Step 3: Run tests**

Run: `nix build .#checks.x86_64-linux.abc-clj-kondo`
Expected: PASS

- [ ] **Step 4: Commit**

```bash
git add abc/src/abc/tools/parser_ir_sentence_policy.clj
git commit -m "feat(abc): add fragment field coherence checks"
```

---

### Task 9: TEI Profile Schema Update

**Files:**
- Modify: `abc/schemas/tei-profile.rng`

- [ ] **Step 1: Add `@next` and `@prev` to `<s>`**

Inline attribute definitions (att.linking is not defined in this RNG):

```xml
<optional>
   <attribute name="next">
      <a:documentation>points to the next fragment in a fragmented sentence</a:documentation>
      <data type="anyURI"/>
   </attribute>
</optional>
<optional>
   <attribute name="prev">
      <a:documentation>points to the previous fragment in a fragmented sentence</a:documentation>
      <data type="anyURI"/>
   </attribute>
</optional>
```

Add after `<ref name="att.segLike.attributes"/>` in the `<s>` element definition.

- [ ] **Step 2: Verify schema compiles**

Run: `nix build .#checks.x86_64-linux.abc-clj-kondo`
Expected: PASS

- [ ] **Step 3: Commit**

```bash
git add abc/schemas/tei-profile.rng
git commit -m "feat(abc): allow @next/@prev on s element"
```

---

### Task 10: Splitter Compatibility Report

**Files:**
- Create: `ab-validator/docs/reports/2026-07-08-sentence-splitter-v2-compatibility.md`

- [ ] **Step 1: Run old vs new splitter over shared fixtures**

Compare `sentence_split` vs `split_sentences` over the existing test fixtures. Document each intentional divergence.

- [ ] **Step 2: Write compatibility report**

```markdown
# Sentence Splitter v2 Compatibility Report

## Fixture Matrix
| Input | v1 output | v2 output | Divergence |
|---|---|---|---|
| ... | ... | ... | none / intentional |

## Intentional Divergences
1. CJK numbered lists: １． no longer splits
2. Exclamation continuation: ！笑 stays together
3. Closing bracket depth: 「...。...」 stays together
...
```

- [ ] **Step 3: Commit**

```bash
git add ab-validator/docs/reports/
git commit -m "docs: add sentence splitter v2 compatibility report"
```

---

### Task 11: Splitter ID Update + Final Validation

**Files:**
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs`

- [ ] **Step 1: Update `segmentation_meta`**

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

- [ ] **Step 2: Run all checks**

```bash
nix build .#checks.x86_64-linux.ab-validator-cargo-check
nix build .#checks.x86_64-linux.ab-validator-cargo-clippy
nix build .#checks.x86_64-linux.ab-validator-cargo-fmt
nix build .#checks.x86_64-linux.abc-clj-kondo
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests
```

Expected: All PASS

- [ ] **Step 3: Commit**

```bash
git add ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs
git commit -m "feat(ab-aat-to-parser-ir): update splitter_id to v2"
```
