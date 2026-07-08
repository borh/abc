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
| `abc/schemas/parser-ir.schema.json` | Modify | Add optional fragment fields to `$defs/sentence` |
| `ab-validator/data/abc-schemas/nix-schemas/parser-ir.schema.json` | Modify | Mirror ABC schema |
| `ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs` | Modify | Extend `ParserIrSentence`, nesting detection, fragment assembly |
| `ab-validator/crates/ab-aat-to-parser-ir/src/convert.rs` | Modify | Synthesize quote nodes from text `「」`/`『』` |
| `ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/nested-sentence-fragmentation.aat.json` | Create | End-to-end fixture from Kokoro |
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
    /// When true, skip the closing_bracket_ahead() check.
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

The algorithm matches the user's `split_sentences` with these adaptations:
- Returns `Vec<SentenceSpan<'_>>` (borrows from input, tracks byte/char offsets)
- Newlines are NOT boundaries (match existing `sentence_split` behavior)
- `suppress_closing_bracket_check` skips `closing_bracket_ahead()` when true

```rust
pub fn split_sentences_with_options(
    input: &str,
    opts: &SplitOptions,
) -> Vec<SentenceSpan<'_>> {
    const DELIMITERS: &[char] = &['.', '!', '?', '．', '。', '！', '？', '…'];
    const CLOSING_QUOTATIONS: &[char] = &[
        ')', '）', '」', '』', '】', '］', '〕', '〉', '》', ']',
        '"', '\u{201D}', '\u{2019}',
    ];

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

    fn is_period_non_boundary_neighbor(ch: char) -> bool {
        ch.is_ascii_alphanumeric() || matches!(ch, '０'..='９' | 'Ａ'..='Ｚ' | 'ａ'..='ｚ')
    }

    let mut spans = Vec::new();
    let mut byte_start = 0usize;
    let mut char_start = 0usize;
    let mut buffer: [Option<char>; 2] = [None, None];
    let mut chars = input.chars().peekable();

    while let Some(c) = chars.next() {
        let shifted = buffer;
        buffer[0] = shifted[1];
        buffer[1] = Some(c);

        // Newlines are NOT boundaries (preserved behavior)
        if c == '\n' {
            continue;
        }

        if !DELIMITERS.contains(&c) {
            continue;
        }

        let z = buffer[0];
        let _y = buffer[1];
        let next = chars.peek();
        let byte_offset = byte_start + &input[byte_start..].find(c).unwrap_or(0);

        let should_split = match (z, next) {
            (Some(_z), Some(next_c)) => {
                if DELIMITERS.contains(next_c) {
                    false // repeated delimiter
                } else if CLOSING_QUOTATIONS.contains(next_c) {
                    false // closing quote after delimiter
                } else if c == '。' && !opts.suppress_closing_bracket_check {
                    if closing_bracket_ahead(input, byte_offset + c.len_utf8()) {
                        false
                    } else {
                        true
                    }
                } else if c == '…' {
                    false // … only at EOL (never here since newlines aren't boundaries)
                } else if (c == '.' || c == '．') && is_cjk_digit(_z) {
                    false // CJK numbered list
                } else if (c == '！' || c == '？') && is_japanese_continuation(*next_c) {
                    false // exclamation continuation
                } else if is_period_non_boundary_neighbor(_z) && is_period_non_boundary_neighbor(*next_c) && c != '。' {
                    false // between alphanumerics
                } else {
                    true
                }
            }
            (Some(_z), None) => c == '。', // end of input: split only on 。
            _ => true,
        };

        if should_split {
            let end_byte = byte_offset + c.len_utf8();
            let text_slice = &input[byte_start..end_byte];
            if !text_slice.trim().is_empty() {
                spans.push(SentenceSpan {
                    text: text_slice,
                    byte_offset: byte_start,
                    char_offset: char_start,
                });
            }
            char_start += text_slice.chars().count();
            byte_start = end_byte;
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

- [ ] **Step 3: Add unit tests**

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

#[test]
fn split_sentences_closing_curly_quote() {
    let spans = split_sentences("He said \u{201C}hello.\u{201D} Then left.");
    assert_eq!(spans.len(), 1);
}
```

- [ ] **Step 4: Run tests**

Run: `cargo test -p ab-plaintext -- split_sentences`
Expected: All new tests PASS

- [ ] **Step 5: Commit**

```bash
git add ab-validator/crates/ab-plaintext/src/lib.rs
git commit -m "feat(ab-plaintext): add split_sentences with SplitOptions

New splitter with improved rules: CJK numbered lists, exclamation
continuation, closing_bracket_ahead with depth tracking. Adds
suppress_closing_bracket_check flag for nested sentence re-splitting."
```

---

### Task 2: Parser-IR Schema Update

**Files:**
- Modify: `abc/schemas/parser-ir.schema.json`
- Modify: `ab-validator/data/abc-schemas/nix-schemas/parser-ir.schema.json` (mirror)

**Interfaces:**
- Produces: Optional `part`, `fragment_group`, `next_id`, `prev_id` fields on `$defs/sentence`

- [ ] **Step 1: Add fragment fields to `$defs/sentence`**

In `abc/schemas/parser-ir.schema.json`, add to the `properties` of `sentence`:

```json
"part": { "type": "string", "enum": ["I", "M", "F"] },
"fragment_group": { "type": "string", "pattern": "^fg[0-9]{6}$" },
"next_id": { "type": "string", "pattern": "^s[0-9]{6}$" },
"prev_id": { "type": "string", "pattern": "^s[0-9]{6}$" }
```

These are optional (not in `required`), so backward compatible.

- [ ] **Step 2: Mirror to ab-validator**

Copy the same changes to `ab-validator/data/abc-schemas/nix-schemas/parser-ir.schema.json`.

- [ ] **Step 3: Verify schema validates**

Run: `cargo check -p ab-aat-to-parser-ir`
Expected: PASS

- [ ] **Step 4: Commit**

```bash
git add abc/schemas/parser-ir.schema.json
git add ab-validator/data/abc-schemas/nix-schemas/parser-ir.schema.json
git commit -m "feat(abc): add optional fragment fields to sentence schema

Adds part, fragment_group, next_id, prev_id as optional fields
for TEI Chapter 21 sentence fragmentation."
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

Add `part: None, fragment_group: None, next_id: None, prev_id: None` to every existing `ParserIrSentence { ... }` literal in `sentences.rs`.

- [ ] **Step 3: Run tests**

Run: `cargo test -p ab-aat-to-parser-ir`
Expected: PASS (existing tests pass with None fields)

- [ ] **Step 4: Commit**

```bash
git add ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs
git commit -m "feat(ab-aat-to-parser-ir): add fragment fields to ParserIrSentence

Adds optional part, fragment_group, next_id, prev_id fields.
Serialized only when present (skip_serializing_if None)."
```

---

### Task 4: Quote Node Synthesis in Converter

**Files:**
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/convert.rs`

**Interfaces:**
- Produces: Quote nodes in `outputs.nodes` with `type: "quote"`, `marker_type`, `text`
- Consumes: Text nodes containing `「`/`」`/`『`/`』` characters
- Preserves: Source spans via `map_span`

- [ ] **Step 1: Add `map_text_node_with_quotes` function**

When processing a text node, check if it contains `「`, `」`, `『`, or `』`. If so, split into segments. Preserve source spans via `map_span`. Keep `is_source_derived_line_break_text` short-circuit.

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

    let source_span = node.get("span");
    let mut offset = current_offset;
    let mut segment_start_byte = 0;

    for (i, ch) in text.char_indices() {
        if quote_chars.contains(&ch) {
            // Emit text segment before the quote (if non-empty)
            if segment_start_byte < i {
                let segment_text = &text[segment_start_byte..i];
                let end = offset + segment_text.len() as u64;
                let span = map_span(source_span, offset, end, recorder, path)?;
                nodes.push(json!({
                    "type": "text",
                    "span": span,
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
            let span = map_span(source_span, offset, offset + ch_len, recorder, path)?;
            nodes.push(json!({
                "type": "quote",
                "span": span,
                "marker_type": marker_type,
                "nesting_level": null,
                "text": ch.to_string(),
            }));
            offset += ch_len;
            segment_start_byte = i + ch.len_utf8();
        }
    }

    // Emit remaining text after last quote
    if segment_start_byte < text.len() {
        let segment_text = &text[segment_start_byte..];
        let end = offset + segment_text.len() as u64;
        let span = map_span(source_span, offset, end, recorder, path)?;
        nodes.push(json!({
            "type": "text",
            "span": span,
            "text": segment_text,
        }));
        offset = end;
    }

    Ok(offset)
}
```

- [ ] **Step 2: Wire into existing text node processing**

In `map_inline_node`, replace the text node arm to call `map_text_node_with_quotes` instead of direct text node emission. Keep `is_source_derived_line_break_text` short-circuit before the quote check.

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

Text nodes containing Japanese quotation markers are split into
text segments + quote nodes with marker_type open/close."
```

---

### Task 5: Nesting Detection (Recursive)

**Files:**
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs`

**Interfaces:**
- Produces: `struct NestedRegion { inner_byte_start, inner_byte_end, opening_marker_node, closing_marker_node, nesting_level }`
- Produces: `fn detect_nested_regions(nodes: &[Value], sentence_start: usize, sentence_end: usize) -> Vec<NestedRegion>`
- Consumes: Quote nodes from Task 4

- [ ] **Step 1: Add `NestedRegion` struct**

```rust
const MAX_NESTING_DEPTH: usize = 5;

#[derive(Debug, Clone, PartialEq, Eq)]
struct NestedRegion {
    inner_byte_start: usize,     // byte offset after opening marker
    inner_byte_end: usize,       // byte offset before closing marker
    opening_marker_node: Option<usize>,
    closing_marker_node: Option<usize>,
    nesting_level: usize,
}
```

- [ ] **Step 2: Add recursive `detect_nested_regions` function**

The function scans for open/close quote pairs, emits regions at every depth, and recurses on inner node slices:

```rust
fn detect_nested_regions(
    nodes: &[Value],
    sentence_start: usize,
    sentence_end: usize,
) -> Vec<NestedRegion> {
    detect_nested_regions_inner(nodes, sentence_start, sentence_end, 0)
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
        let marker_type = node.get("marker_type").and_then(Value::as_str).unwrap_or("");
        if marker_type != "open" {
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
                            // Found matching close
                            let close_byte_start = value_usize(next, "/span/start", "quote span.start").unwrap_or(0);
                            let close_node = j;

                            // Emit this region
                            regions.push(NestedRegion {
                                inner_byte_start: open_byte_end,
                                inner_byte_end: close_byte_start,
                                opening_marker_node: Some(open_node),
                                closing_marker_node: Some(close_node),
                                nesting_level: depth,
                            });

                            // Recurse on inner nodes
                            let inner_regions = detect_nested_regions_inner(
                                nodes, open_node + 1, close_node, depth + 1,
                            );
                            regions.extend(inner_regions);

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
            // Unmatched open — skip
            i += 1;
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
        json!({"type":"quote","span":{"start":6,"end":9},"marker_type":"open","nesting_level":null,"text":"「"}),
        json!({"type":"text","span":{"start":9,"end":15},"text":"綺麗だ"}),
        json!({"type":"quote","span":{"start":15,"end":18},"marker_type":"close","nesting_level":null,"text":"」"}),
        json!({"type":"text","span":{"start":18,"end":24},"text":"といった"}),
    ];
    let regions = detect_nested_regions(&nodes, 0, 5);
    assert_eq!(regions.len(), 1);
    assert_eq!(regions[0].inner_byte_start, 9);
    assert_eq!(regions[0].inner_byte_end, 15);
}

#[test]
fn detect_nested_regions_recursive() {
    // 「outer 「inner」 text」
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
    assert_eq!(regions.len(), 2); // outer + inner
    assert_eq!(regions[0].nesting_level, 0); // outer
    assert_eq!(regions[1].nesting_level, 1); // inner
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

Detects nested regions by scanning for open/close quote node pairs.
Recurses on inner node slices for nested quotes. Max depth 5."
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

    // Step 1: Flat split
    let mut flat_bounds: Vec<SentenceBounds> = ab_plaintext::split_sentences(&paragraph_text)
        .into_iter()
        .map(|span| SentenceBounds {
            start: paragraph_start + span.byte_offset,
            end: paragraph_start + span.byte_offset + span.text.len(),
        })
        .collect();

    // Whitespace handling (existing)
    // ... (existing whitespace absorption logic)

    // Step 2: For each flat sentence, detect nested regions and compute inner boundaries
    let mut all_inner_boundaries: Vec<usize> = Vec::new();
    let mut sentence_nested_info: Vec<Vec<NestedRegion>> = Vec::new();

    for bounds in &flat_bounds {
        // Find nodes in this sentence's byte range
        let nested = detect_nested_regions_for_sentence(
            original_nodes, bounds.start, bounds.end,
        );
        for region in &nested {
            // Re-split inner text
            let inner_text = &paragraph_text[
                (region.inner_byte_start - paragraph_start)..
                (region.inner_byte_end - paragraph_start)
            ];
            let inner_spans = ab_plaintext::split_sentences_with_options(
                inner_text,
                &SplitOptions { suppress_closing_bracket_check: true },
            );
            for span in &inner_spans {
                let abs_boundary = region.inner_byte_start + span.byte_offset + span.text.len();
                if abs_boundary < region.inner_byte_end {
                    all_inner_boundaries.push(abs_boundary);
                }
            }
        }
        sentence_nested_info.push(nested);
    }

    // Step 3: Merge flat + inner boundaries
    let mut split_boundaries: Vec<usize> = flat_bounds
        .iter()
        .take(flat_bounds.len().saturating_sub(1))
        .map(|s| s.end)
        .collect();
    split_boundaries.extend(all_inner_boundaries);
    split_boundaries.sort();
    split_boundaries.dedup();

    // Step 4: Single split_node_at_boundaries pass (nodes consumed here)
    for node in original_nodes.iter_mut() {
        split_node_at_boundaries(node, &split_boundaries, rewritten_nodes)?;
    }

    // Step 5: Build fine-grained bounds (flat + inner splits)
    let mut fine_bounds: Vec<SentenceBounds> = Vec::new();
    for (i, flat) in flat_bounds.iter().enumerate() {
        let nested = &sentence_nested_info[i];
        if nested.is_empty() {
            fine_bounds.push(*flat);
        } else {
            // Split flat bound at inner boundaries
            let mut cursor = flat.start;
            for region in nested {
                if cursor < region.inner_byte_start {
                    fine_bounds.push(SentenceBounds { start: cursor, end: region.inner_byte_start });
                }
                // Inner region becomes one or more sentences (already split above)
                // ... (collect inner bounds from re-split)
                cursor = region.inner_byte_end;
            }
            if cursor < flat.end {
                fine_bounds.push(SentenceBounds { start: cursor, end: flat.end });
            }
        }
    }

    // Step 6: Build sentence rows with fragment fields
    let mut rows = Vec::new();
    let mut fragment_group_counter = 0usize;
    // ... (fragment assembly logic using fine_bounds and sentence_nested_info)

    // Step 7: Assert tiling + field coherence
    assert_sentence_span_tiling(paragraph_start, paragraph_end, &fine_bounds)?;
    assert_fragment_field_coherence(&rows)?;

    Ok(rows)
}
```

- [ ] **Step 2: Add orthographic annotation redistribution**

Fragment rows inherit `tags` and `orthographic_annotation_indices` by byte-overlap:

```rust
let annotation_indices = overlapping_ortho_indices(
    bounds.start, bounds.end, ortho,
);
let tags = if annotation_indices.is_empty() {
    Vec::new()
} else {
    vec!["orthographic-katakana".to_owned()]
};
```

- [ ] **Step 3: Create end-to-end fixture**

Create `ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/nested-sentence-fragmentation.aat.json`:

```json
{
  "version": 1,
  "work_id": "test-fragmentation",
  "blocks": [
    {
      "kind": "paragraph",
      "content": [
        {"kind": "text", "value": "先生は高い梢を見上げて、「もう少しすると、綺麗ですよ。"},
        {"kind": "text", "value": "この木がすっかり黄葉して、ここいらの地面は金色の落葉で埋まるようになります」"},
        {"kind": "text", "value": "といった。"}
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

- [ ] **Step 4: Add integration tests**

```rust
#[test]
fn fragment_assembly_single_inner_sentence() {
    let (schemas, mapping) = schemas_and_mapping();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: include_fixture_json("nested-sentence-fragmentation.aat.json"),
        schemas: &schemas,
        mapping: &mapping,
        options: ConversionOptions { validate_output_parser_ir: true, ..Default::default() },
    }).unwrap();
    let sentences = output.parser_ir["sentences"].as_array().unwrap();
    assert_eq!(sentences.len(), 4);
    assert_eq!(sentences[0]["part"], "I");
    assert_eq!(sentences[1]["part"], serde_json::Value::Null);
    assert_eq!(sentences[2]["part"], serde_json::Value::Null);
    assert_eq!(sentences[3]["part"], "F");
    assert_eq!(sentences[0]["next_id"], sentences[3]["id"]);
    assert_eq!(sentences[3]["prev_id"], sentences[0]["id"]);
}

#[test]
fn fragment_field_coherence() {
    let (schemas, mapping) = schemas_and_mapping();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: include_fixture_json("nested-sentence-fragmentation.aat.json"),
        schemas: &schemas,
        mapping: &mapping,
        options: ConversionOptions::default(),
    }).unwrap();
    let sentences = output.parser_ir["sentences"].as_array().unwrap();
    for s in sentences {
        let part = s.get("part");
        let next = s.get("next_id");
        let prev = s.get("prev_id");
        let group = s.get("fragment_group");
        // Verify combinatorial constraints
        if part.is_some() && part != Some(&serde_json::Value::Null) {
            assert!(group.is_some());
        }
        // ... (full constraint checks)
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
- Consumes: `part`, `fragment_group`, `next_id`, `prev_id` from sentence rows
- Produces: `<s>` elements with `@part`, `@xml:id`, `@next`, `@prev`

- [ ] **Step 1: Update `sentence-attrs` function**

Replace the existing `sentence-attrs`:

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

In `abc/test/abc/tools/parser_ir_tei_test.clj`, use string keys (matching `(get sentence "id")` convention):

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
          s-elements (second (second body))] ; <body> → <p> → children
      ;; Verify fragment attributes
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
git commit -m "feat(abc): add fragment field coherence checks

Validates part/next_id/prev_id/fragment_group combinatorial
constraints in the publication gate."
```

---

### Task 9: TEI Profile Schema Update

**Files:**
- Modify: `abc/schemas/tei-profile.rng`

**Interfaces:**
- Produces: `@next` and `@prev` allowed on `<s>`

- [ ] **Step 1: Add `@next` and `@prev` to `<s>`**

Since `att.linking.attributes` is not defined in this RNG, add the attributes inline on the `<s>` element definition (around line 13270):

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

Add after the existing `<ref name="att.segLike.attributes"/>` line.

- [ ] **Step 2: Verify schema compiles**

Run: `nix build .#checks.x86_64-linux.abc-clj-kondo`
Expected: PASS

- [ ] **Step 3: Commit**

```bash
git add abc/schemas/tei-profile.rng
git commit -m "feat(abc): allow @next/@prev on s element

Adds inline attribute definitions for TEI Chapter 21 fragmentation."
```

---

### Task 10: Splitter ID Update

**Files:**
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs`

**Interfaces:**
- Produces: `splitter_id: "ab-plaintext-japanese-v2"` in sentence_segmentation

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

- [ ] **Step 2: Run full test suite**

Run: `nix build .#checks.x86_64-linux.ab-validator-cargo-check && nix build .#checks.x86_64-linux.ab-validator-cargo-clippy && nix build .#checks.x86_64-linux.abc-clj-kondo`
Expected: PASS

- [ ] **Step 3: Commit**

```bash
git add ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs
git commit -m "feat(ab-aat-to-parser-ir): update splitter_id to v2"
```

---

### Task 11: Final Validation

**Files:**
- None (validation run only)

- [ ] **Step 1: Run all checks**

```bash
nix build .#checks.x86_64-linux.ab-validator-cargo-check
nix build .#checks.x86_64-linux.ab-validator-cargo-clippy
nix build .#checks.x86_64-linux.ab-validator-cargo-fmt
nix build .#checks.x86_64-linux.abc-clj-kondo
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests
```

Expected: All PASS

- [ ] **Step 2: Run focused converter tests**

Run: `cargo test -p ab-aat-to-parser-ir`
Expected: All PASS

- [ ] **Step 3: Commit any fixes**

```bash
git add -A
git commit -m "fix: final validation fixes"
```
