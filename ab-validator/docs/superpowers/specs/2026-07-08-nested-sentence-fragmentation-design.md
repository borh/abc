# Nested Sentence Fragmentation Design

**Date:** 2026-07-08
**Status:** Design — approved for implementation planning
**Owner:** ab-validator + ABC
**Governs:** Sentence fragmentation for nested/quoted speech per TEI Chapter 21.

## Problem

Japanese literature frequently uses quoted speech with internal sentence boundaries:

```
先生は高い梢を見上げて、「もう少しすると、綺麗ですよ。この木がすっかり黄葉して、
ここいらの地面は金色の落葉で埋まるようになります」といった。
```

The current sentence splitter produces flat, non-overlapping sentence spans. It has no concept of hierarchical or nested sentence structure. The `closing_bracket_ahead` heuristic suppresses splits inside quoted regions, keeping quoted content together with the surrounding sentence.

This produces incorrect results for morphological analysis and TEI publication: the outer sentence is torn apart at inner terminals, or inner sentences are merged with the outer sentence. Neither matches the source text's semantic structure.

## Goal

Implement TEI Chapter 21 fragmentation (Strategy A) for nested sentences. The outer sentence is split into `<s part="I">`, `<s part="M">`, `<s part="F">` fragments, and inner sentences are separate `<s>` elements. Fragments are linked via `@xml:id`, `@next`, `@prev`.

## Design Decisions

| # | Decision | Rationale |
|---|---|---|
| D1 | TEI fragmentation strategy (`part`/`next`/`prev`) | Most common TEI practice. Keeps sentence markup inline. Compatible with existing schematron rule (no nested `<s>`). |
| D2 | Both AAT quote markers and punctuation heuristics | AAT markers are authoritative when available. Heuristics cover patterns without AAT markers (parenthetical `（）`). |
| D3 | Replace `sentence_split` with user's `split_sentences` | Improved rules: `…` only at EOL, CJK numbered lists, `！`/`？` continuation, `closing_bracket_ahead` with depth tracking. |
| D4 | Two-pass splitting | Pass 1: flat split (splitter). Pass 2: nesting detection + re-split (converter). Keeps splitter simple, validated independently. |
| D5 | Fragment fields on sentence rows | Add `part`, `fragment_group`, `next_id`, `prev_id` to existing `sentences[]`. No new top-level arrays. Backward compatible. |
| D6 | Recursive nesting | Real literary texts have 2-3 levels of quote nesting. Max depth limit (default 5) prevents runaway recursion. |
| D7 | Emit quote nodes from converter | Schema already supports `quoteNode`. AAT `quote_block` and inline markers → parser-IR `quote` nodes with `marker_type` and `nesting_level`. |
| D8 | Converter-driven nesting | Splitter stays simple (flat sentences). Converter detects nesting using quote nodes + heuristics. Best separation of concerns. |

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│ Layer 1: Splitter Replacement                                   │
│ ab-validator/crates/ab-plaintext/src/lib.rs                     │
│ - Replace sentence_split with split_sentences                   │
│ - Add SplitOptions { suppress_closing_bracket_check: bool }     │
│ - Newline-as-boundary behavior                                   │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│ Layer 2: Converter (nesting detection + fragment assembly)      │
│ ab-validator/crates/ab-aat-to-parser-ir/src/                    │
│   convert.rs  — emit quote nodes from AAT markers               │
│   sentences.rs — nesting detection + fragment assembly           │
│ - Detect quoted/parenthetical regions (quote nodes + heuristics) │
│ - Re-split inner regions with suppress_closing_bracket_check     │
│ - Emit sentence rows with part/group_id/next_id/prev_id         │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│ Layer 3: ABC TEI Renderer                                       │
│ abc/src/abc/tools/parser_ir_tei.clj                             │
│ - Consume sentence fragment fields                               │
│ - Emit <s part="I" xml:id="..." next="..."> fragments           │
│ - TEI profile schema update for fragment attributes              │
└─────────────────────────────────────────────────────────────────┘
```

## Layer 1: Splitter Replacement

### File: `ab-validator/crates/ab-plaintext/src/lib.rs`

Replace `sentence_split` with the user's `split_sentences` logic, adapted to preserve `SentenceSpan` return type.

### Key Behavioral Differences

| Behavior | Current `sentence_split` | New `split_sentences` |
|---|---|---|
| Newlines | NOT boundaries | ARE boundaries |
| `…` | Always splits | Only at end-of-line |
| CJK numbered lists (`１．`) | No special handling | Recognized, no split |
| `！`/`？` + continuation | No special handling | `！笑`, `？って` stay together |
| `closing_bracket_ahead` | Simple next-char check | Scans ahead with depth tracking |
| Closing quotations | `）」』】］〕〉》]` | Adds `"`, `"`, `’` |

### SplitOptions

```rust
pub struct SplitOptions {
    /// When true, skip the closing_bracket_ahead() check.
    /// Used when re-splitting text inside a known quote region.
    pub suppress_closing_bracket_check: bool,
}

pub fn split_sentences_with_options(
    input: &str,
    opts: &SplitOptions,
) -> Vec<SentenceSpan<'_>> {
    // When opts.suppress_closing_bracket_check is true,
    // skip the closing_bracket_ahead() call in the should_split logic
}
```

### Splitter Compatibility Gate

Per existing design (`2026-07-07-parser-ir-sentence-segmentation-and-ortho-tei-design.md`):

1. Run both old and new splitters over shared fixture texts
2. Run over representative corpus sample
3. Check in the divergence report
4. Update `splitter_id` from `"ab-plaintext-japanese-v1"` to `"ab-plaintext-japanese-v2"`

## Layer 2: Converter Changes

### 2a: Quote Node Emission (`convert.rs`)

**AAT structures → parser-IR quote nodes:**

| AAT kind | Parser-IR node | Fields |
|---|---|---|
| `quote_block` (open marker) | `quote` | `marker_type: "open"`, `nesting_level` from context |
| `quote_block` (close marker) | `quote` | `marker_type: "close"`, `nesting_level` from context |
| Inline quote marker (e.g., `「`) | `quote` | `marker_type: "open"` or `"close"` |
| Inline quote with text | `quote` | `marker_type: "inline"`, `text: "quoted content"` |

`nesting_level` tracking: converter maintains a counter incremented on open markers, decremented on close markers.

Quote nodes are inline nodes — they participate in sentence projection like `text`, `ruby`, etc. Their visible text is the marker character itself (`「`, `」`, etc.) or empty.

### 2b: Nesting Detection (`sentences.rs`)

**New function:** `detect_nested_regions(nodes, sentences) → Vec<NestedRegion>`

```rust
struct NestedRegion {
    outer_sentence_idx: usize,   // index into sentences[]
    inner_byte_start: usize,     // byte offset of the region (after opening marker)
    inner_byte_end: usize,       // byte offset (before closing marker)
    opening_marker_node: Option<usize>, // node index of opening quote node
    closing_marker_node: Option<usize>, // node index of closing quote node
    nesting_level: usize,
}
```

**Detection strategy (two signals):**

1. **Quote nodes (primary):** Scan the node slice for the sentence. Find `quote` nodes with `marker_type: "open"` and their matching `"close"`. Track depth. Each open→close pair at depth 0 is a candidate nested region.

2. **Punctuation heuristics (fallback):** For sentences without quote nodes, scan the visible text for `「...」` patterns. Treat each `「...」` span as a nested region.

**Recursive handling:** For nested quotes (quote inside quote), the detector recurses. Each nesting level produces its own `NestedRegion`. Max depth: 5 (configurable).

### 2c: Fragment Assembly (`sentences.rs`)

**Modified function:** `project_body_paragraph` now calls nesting detection after flat splitting.

**Algorithm:**

```
1. Run split_sentences(paragraph_text) → flat sentence spans
2. For each sentence span:
   a. Detect nested regions within this sentence's node slice
   b. If no nested regions → emit sentence row as-is (no part/fragment_group)
   c. If nested regions found:
      i.   Extract inner text from nested regions
      ii.  Re-split inner text with suppress_closing_bracket_check: true
      iii. Split parser-IR nodes at these finer boundaries
      iv.  Create fragment rows:
           - Before first nested region: part="I", next_id = first inner or next fragment
           - Each nested region: no part (complete inner sentences)
           - After last nested region: part="F", prev_id = last inner or previous fragment
           - Between nested regions: part="M" (if multiple regions)
      v.   Assign fragment_group, next_id, prev_id
3. Flatten all sentence rows (fragments + complete sentences)
4. Assert tiling (updated for fragments)
```

**Example trace** for `先生は高い梢を見上げて、「もう少しすると、綺麗ですよ。この木がすっかり黄葉して、ここいらの地面は金色の落葉で埋まるようになります」といった。`:

```
Pass 1 (flat split):
  ["先生は高い梢を見上げて、「もう少しすると、綺麗ですよ。この木が...ます」といった。"]
  (one sentence — closing_bracket_ahead suppresses inner splits)

Pass 2 (nesting detection):
  Nested region at bytes [21, 88) — 「...ます」

Re-split inner text with suppress_closing_bracket_check:
  "もう少しすると、綺麗ですよ。" → inner sentence 1
  "この木がすっかり黄葉して、ここいらの地面は金色の落葉で埋まるようになります」" → inner sentence 2

Node splitting + fragment assembly:
  "先生は高い梢を見上げて、" → outer fragment I
  "「もう少しすると、綺麗ですよ。" → inner sentence 1
  "この木が...ます」" → inner sentence 2
  "といった。" → outer fragment F

Sentence rows:
  s000000: part="I", fragment_group="fg000000", next_id="s000003"
  s000001: (no part — complete inner sentence)
  s000002: (no part — complete inner sentence)
  s000003: part="F", fragment_group="fg000000", prev_id="s000000"
```

### 2d: Tiling Assertions

Update `assert_sentence_span_tiling` and `assert_sentence_node_tiling`:

- Fragment groups must tile their enclosing sentence's byte/node range
- Non-fragmented sentences must tile the paragraph as before
- The overall paragraph must still tile with no gaps

## Layer 3: ABC TEI Renderer Changes

### 3a: Fragment Rendering

Modified function: `render-paragraph-row-with-sentences`

```clojure
(defn- sentence-attrs [sentence]
  (let [base-attrs (when (some #{"orthographic-katakana"} (get sentence "tags" []))
                     {:type "orthographic-katakana"})
        part (get sentence "part")
        fragment-group (get sentence "fragment_group")
        next-id (get sentence "next_id")
        prev-id (get sentence "prev_id")]
    (cond-> base-attrs
      part
      (assoc :part part)

      fragment-group
      (assoc :xml:id (str "s" (subs (get sentence "id") 1))
             :corresp (str "#" fragment-group))

      next-id
      (assoc :next (str "#s" (subs next-id 1)))

      prev-id
      (assoc :prev (str "#s" (subs prev-id 1))))))
```

### 3b: TEI Profile Schema Update

Add to `abc/schemas/tei-profile.rng`:

1. Allow `@part` on `<s>` with values `"I"`, `"M"`, `"F"`
2. Allow `@corresp` on `<s>` (from `att.pointing`)
3. Allow `@next` and `@prev` on `<s>` (from `att.linking`)
4. Allow `@xml:id` on `<s>` (from `att.global`)

These are standard TEI attributes — no custom extension needed.

## Parser-IR Schema Changes

### Quote Nodes

Schema already defines `quoteNode`. No schema change needed — just converter wiring.

```json
{
  "type": "quote",
  "span": { "start": 12, "end": 36, "coordinate_system": "decoded_utf8" },
  "marker_type": "open",
  "nesting_level": 0,
  "text": "「"
}
```

### Sentence Fragment Fields

Add optional fields to existing sentence row schema:

```json
{
  "id": "s000003",
  "paragraph_id": "p000000",
  "span": { "start": 24, "end": 48, "coordinate_system": "decoded_utf8" },
  "node_range": { "start": 5, "end": 8 },
  "tags": [],
  "orthographic_annotation_indices": [],
  "part": "I",
  "fragment_group": "fg000000",
  "next_id": "s000005"
}
```

| Field | Type | Required | Description |
|---|---|---|---|
| `part` | `"I"` \| `"M"` \| `"F"` | No | Fragment position. Absent for complete sentences. |
| `fragment_group` | string | No | Groups fragments. Format: `fg000000`, `fg000001`, ... |
| `next_id` | string | No | ID of next fragment. Absent for final fragments. |
| `prev_id` | string | No | ID of previous fragment. Absent for initial fragments. |

**Constraints:**
- If `part` is present, `fragment_group` MUST be present
- If `part` is `"I"`, `next_id` MUST be present, `prev_id` MUST be absent
- If `part` is `"M"`, both `next_id` and `prev_id` MUST be present
- If `part` is `"F"`, `prev_id` MUST be present, `next_id` MUST be absent
- If `part` is absent, `fragment_group`, `next_id`, `prev_id` MUST all be absent
- All fragment rows in a group MUST reference the same `paragraph_id`
- Fragment `span` values MUST tile the logical sentence's byte range
- Fragment `node_range` values MUST tile the logical sentence's node range

**Backward compatibility:** Non-fragmented sentences have no `part`, no `fragment_group`, no linking fields. Existing parser-IR consumers are unaffected.

## Error Handling

| Case | Handling |
|---|---|
| Unmatched `「` without `」` | No fragmentation. Treat as text. Log warning. |
| Recursive nesting > max depth (5) | Treat inner region as text. Log warning. |
| Atomic node (ruby/gaiji) straddles boundary | Hard fail with diagnostic. Same as existing atomic-boundary behavior. |
| AAT quote marker mismatch | Use quote nodes as signals, fall back to heuristics. Log warning. |
| Tiling assertion failure | Hard fail with diagnostic. Fragments must tile original sentence. |

## Testing Strategy

### Splitter Tests (`lib.rs`)

| Test | Input | Expected |
|---|---|---|
| Basic fragmentation | `先生は梢を見上げて、「綺麗ですよ。落葉で埋まります」といった。` | 3 sentences with options, 1 without |
| Suppress mode | Inner text with `suppress_closing_bracket_check: true` | Splits at `。` |
| Unmatched quote | `先生は「綺麗ですといった。` | 1 sentence (no fragmentation) |
| Nested quotes | `「outer 「inner。」 text」` | Recursive fragmentation |
| BCCWW validation | `bccwj-all.txt` with new splitter | <1% divergence (documented) |

### Converter Tests (`integration.rs`)

| Test | Description |
|---|---|
| `quote_node_emission` | AAT `quote_block` → parser-IR `quote` nodes |
| `fragment_assembly_single_inner` | One inner sentence → two outer fragments |
| `fragment_assembly_multiple_inner` | Multiple inner sentences → fragments + sentences |
| `fragment_assembly_nested_quotes` | Recursive nesting → correct groups |
| `fragment_tiling_assertion` | Fragments tile original sentence |
| `unmatched_quote_no_fragment` | Unmatched `「` → no fragmentation |
| `atomic_boundary_in_nested` | Ruby inside nested region → hard fail |

### Renderer Tests (`parser_ir_tei_test.clj`)

| Test | Description |
|---|---|
| `fragment_attributes` | `part`/`fragment_group`/`next_id`/`prev_id` → correct `<s>` attributes |
| `non_fragmented_unchanged` | No `part` → `<s>` without fragment attributes |
| `mixed_fragment_and_complete` | Paragraph with both types |
| `tei_profile_valid` | Output validates against updated schema |

### End-to-End Fixture

**File:** `ab-validator/crates/ab-aat-to-parser-ir/tests/fixtures/nested-sentence-fragmentation.aat.json`

Contains real Aozora example from *Kokoro*:

```json
{
  "kind": "paragraph",
  "content": [
    {"kind": "text", "value": "先生は高い梢を見上げて、「もう少しすると、綺麗ですよ。"},
    {"kind": "text", "value": "この木がすっかり黄葉して、ここいらの地面は金色の落葉で埋まるようになります」"},
    {"kind": "text", "value": "といった。"}
  ]
}
```

Expected TEI:

```xml
<p>
  <s part="I" xml:id="s000000" corresp="#fg000000" next="#s000003">先生は高い梢を見上げて、</s>
  <s xml:id="s000001">「もう少しすると、綺麗ですよ。</s>
  <s xml:id="s000002">この木がすっかり黄葉して、ここいらの地面は金色の落葉で埋まるようになります」</s>
  <s part="F" xml:id="s000003" corresp="#fg000000" prev="#s000000">といった。</s>
</p>
```

## Non-Goals

- `<q>` element emission in TEI (future enhancement)
- Sentence fragmentation for source notes or metadata apparatus
- Ruby-driven sentence splitting
- Fragmentation across paragraph boundaries
- Nested `<s>` elements (prohibited by schematron rule 41)

## Open Questions

1. **Full-adapter corpus validation:** After implementation, run `audit-corpus` at full scale to measure fragmentation frequency and catch edge cases.

2. **Quote node emission scope:** Should ALL AAT quote markers produce parser-IR quote nodes, or only those inside body paragraphs? Current design: all markers, but nesting detection only runs on body paragraphs.

3. **Fragment group ID format:** Current design uses `fg000000` sequential IDs. Should these be stable across re-conversion, or are ephemeral IDs acceptable?
