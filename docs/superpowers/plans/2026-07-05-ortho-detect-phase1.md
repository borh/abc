# Orthographic Detection and Normalization — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add an optional orthographic-detection and normalization layer (`ab-ortho-detect`) that converts katakana to hiragana in pre-war Japanese sentences before morphological analysis, improving analyzer comparison fairness.

**Architecture:** A new `ab-ortho-detect` crate provides the `OrthoDetector` trait, a v1 two-pass heuristic (Vibrato-first-pass OOV guards + character-level features), kata→hira conversion, and `OffsetMap` coordinate remapping. `ab-plaintext` gains a `sentence_split()` function. `ab-morph-run` wires detection → normalization → analyzer dispatch behind a `--ortho-detect` flag. All analyzers receive the same normalized text; original text is never mutated.

**Tech Stack:** Rust (edition 2024), serde, regex, unicode-blocks (or manual tables). No ML deps in Phase 1.

**Dependency graph:**
```
ab-plaintext  ←  ab-ortho-detect (for SentenceSpan type only)
ab-ortho-detect  ←  ab-morph-run (wires detector into pipeline)
ab-morph-diff  ←  ab-ortho-detect (OrthoAnnotation, OffsetMap types)
ab-morph-analyzers  ←  ab-ortho-detect (OffsetMap for span_builder)
```

## Global Constraints

- Rust edition 2024. All new code in `crates/ab-ortho-detect/`.
- `PlainTextDocument.text` is never mutated. Normalized text is a separate view.
- Morpheme `byte_span` and `char_span` are in original-document coordinates.
- `confidence` is `Option<u8>` (0–100, None = heuristic). All annotation types derive `Eq`.
- `SentenceSpan` is borrowed (`&'a str`), zero-copy.
- `ab-plaintext` must NOT depend on `ab-ortho-detect`.
- `ab-ortho-detect` depends on `ab-plaintext`, `ab-morph-analyzers`, `ab-morph-diff` (see spec Decision 5).
- No external crates beyond what's already in the workspace or trivial (regex, serde).

---

### Task 0: Pre-Phase-1 Verification

**Files:** Create `reports/ortho-detect/2026-07-05-sudachi-baseline.md`

**Interfaces:** None (measurement only, no code changes). Uses existing `ab-morph-run analyze aat` subcommand — each pilot sentence must be wrapped as a minimal AAT JSON document.

- [ ] **Step 1: Assess existing katakana fixtures**

```bash
grep -r "ハ\|デアル\|猫デアル" crates/ data/ tests/ --include="*.rs" --include="*.json" -l
```

Expected: existing test fixtures primarily use modern hiragana. The pilot file below provides the katakana-heavy sentences.

- [ ] **Step 2: Create pilot test data as minimal AAT JSON**

Create `data/ortho-pilot/sentences.aat.json`:

```json
{
  "work_id": "ortho-pilot-001",
  "title": "Ortho Detect Pilot",
  "author": "pilot",
  "blocks": [
    {"kind": "paragraph", "content": [{"kind": "text", "value": "吾輩ハ猫デアル。名前ハマダ無イ。彼ハ学生デアルカラ勉強スル。今日ハ天気ガ良イ。私ハ日本人デハナイ。コレハ何デスカ。アノ人ハ誰デスカ。東京ハ大キイ都市デアル。此レハ美味シイ料理ダ。明日ハ休ミダカラ映画ヲ見ル。"}]}
  ]
}
```

- [ ] **Step 2.5: Inspect Vibrato OOV representation (CRITICAL — determines Task 4 correctness)**

Write and run a small inline test to discover how Vibrato represents OOV tokens:

```rust
// Add temporarily to crates/ab-morph-analyzers/tests/ or run via cargo test -- --nocapture
#[test]
fn inspect_oov_representation() {
    let analyzer = VibratoAnalyzer::unidic_cwj_default().unwrap();
    let doc = PlainTextDocument {
        text_id: "oov-test".into(),
        source_format: SourceFormat::AozoraHonbun,
        text: "ホゲホゲフガフガ".into(),
    };
    let analysis = analyzer.analyze(&doc).unwrap();
    for m in &analysis.morphemes {
        println!("surface={} pos1={:?} pos2={:?}", m.surface, m.features.get("pos1"), m.features.get("pos2"));
    }
}
```

Record the actual `pos1` value for unknown tokens. If it is `None` (Vibrato emits `*` → feature parser maps to `None`), Task 4's OOV guard must be rewritten — the OOV signal must come from Vibrato's token-level `is_unk` flag (not currently exposed through `RawToken`), or the OOV guard must be dropped and the heuristic declared character-only. Document the finding.

- [ ] **Step 3: Run Sudachi baseline**

```bash
cargo run -p ab-morph-run -- analyze-aat \
  --analyzer sudachi-c \
  --aat data/ortho-pilot/sentences.aat.json \
  --output-dir reports/ortho-detect/
```

Manually inspect output: does Sudachi segment katakana particles (ハ, ガ, ヲ) correctly? Does `DefaultInputTextPlugin` (NFKC) help at all? Record findings.

- [ ] **Step 4: Run Vibrato baseline**

```bash
cargo run -p ab-morph-run -- analyze-aat \
  --analyzer vibrato:unidic-cwj-202512 \
  --aat data/ortho-pilot/sentences.aat.json \
  --output-dir reports/ortho-detect/
```

Record OOV rates and segmentation quality on raw katakana sentences.

- [ ] **Step 5: Write findings**

Document in `reports/ortho-detect/2026-07-05-sudachi-baseline.md`:
- Vibrato OOV representation (actual pos1 value). This gates Task 4's OOV detection approach.
- Sudachi segmentation on katakana particles.
- Vibrato OOV rate on pilot.
- Decision: proceed with Phase 1.

---

## File Map

| File | Action | Responsibility |
|---|---|---|
| `crates/ab-ortho-detect/Cargo.toml` | Create | Workspace member, deps: serde, regex, ab-plaintext |
| `crates/ab-ortho-detect/src/lib.rs` | Create | Public API: `OrthoDetector` trait, `ortho_normalize()`, re-exports |
| `crates/ab-ortho-detect/src/types.rs` | Create | `OrthoAnnotation`, `OrthoNormalization`, `OrthoDetectorId`, `OffsetMap` |
| `crates/ab-ortho-detect/src/script.rs` | Create | `kata_to_hira(text: &str) -> String` |
| `crates/ab-ortho-detect/src/features.rs` | Create | Feature extraction from `&str` + Vibrato tokens |
| `crates/ab-ortho-detect/src/heuristic.rs` | Create | `HeuristicV1` struct implementing `OrthoDetector` |
| `crates/ab-plaintext/src/lib.rs` | Modify | Add `SentenceSpan`, `sentence_split()` |
| `crates/ab-morph-diff/src/model.rs` | Modify | Add `ortho_annotations`, `ortho_offset_map` to `Analysis` |
| `crates/ab-morph-analyzers/src/span_builder.rs` | Modify | Accept optional `&OffsetMap` for coordinate remapping |
| `crates/ab-morph-run/src/options.rs` | Modify | Add `--ortho-detect` CLI flag |
| `crates/ab-morph-run/src/pipeline.rs` | Modify | Wire detection → normalization → dispatch |
| `Cargo.toml` (root) | Modify | Add `ab-ortho-detect` to workspace members and deps |

---

### Task 1: Create `ab-ortho-detect` crate skeleton and add `SentenceSpan` to `ab-plaintext`

**Files:**
- Create: `crates/ab-ortho-detect/Cargo.toml`
- Create: `crates/ab-ortho-detect/src/lib.rs` (empty, re-exports only)
- Create: `crates/ab-ortho-detect/src/types.rs`
- Modify: `crates/ab-plaintext/src/lib.rs` (add `SentenceSpan`, `sentence_split()`)
- Modify: `Cargo.toml` (root, add workspace member + dep)

**Interfaces:**
- Produces: `SentenceSpan<'a>` (in `ab-plaintext`), `OrthoNormalization`, `OrthoAnnotation`, `OrthoDetectorId`, `OffsetMap` (in `ab-ortho-detect::types`)

- [ ] **Step 1: Register workspace member**

Edit root `Cargo.toml`:

```toml
# In [workspace] members array, add:
"crates/ab-ortho-detect",

# In [workspace.dependencies], add:
ab-ortho-detect = { path = "crates/ab-ortho-detect" }
```

- [ ] **Step 2: Create Cargo.toml**

`crates/ab-ortho-detect/Cargo.toml`:

```toml
[package]
name = "ab-ortho-detect"
version.workspace = true
edition.workspace = true
license.workspace = true

[dependencies]
serde = { workspace = true, features = ["derive"] }
ab-plaintext = { workspace = true }
regex = { workspace = true }
```

Depends on `ab-plaintext` for `SentenceSpan` type only.

- [ ] **Step 3: Add SentenceSpan to ab-plaintext**

In `crates/ab-plaintext/src/lib.rs`, add after the existing `SourceFormat` enum:

```rust
/// A sentence span from the sentence splitter. Borrows from source text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SentenceSpan<'a> {
    pub text: &'a str,
    pub byte_offset: usize,
    pub char_offset: usize,
}
```

- [ ] **Step 4: Add sentence_split() to ab-plaintext**

In `crates/ab-plaintext/src/lib.rs`, add the public function:

```rust
/// Split text into sentences on terminal punctuation.
/// Boundaries: `。`, `！`, `？`, `!`, `?`.
/// Adjacent terminals are kept together (e.g., `本当！？` is one sentence).
/// Newlines are NOT sentence boundaries (they are paragraph breaks).
pub fn sentence_split(text: &str) -> Vec<SentenceSpan<'_>> {
    let mut spans = Vec::new();
    let mut byte_start = 0usize;
    let mut char_start = 0usize;
    let chars: Vec<(usize, char)> = text.char_indices().collect();

    for (i, &(byte_pos, ch)) in chars.iter().enumerate() {
        if is_sentence_terminal(ch) {
            // Consume adjacent terminals
            let mut end_idx = i + 1;
            while end_idx < chars.len() && is_sentence_terminal(chars[end_idx].1) {
                end_idx += 1;
            }
            let byte_end = if end_idx < chars.len() {
                chars[end_idx].0
            } else {
                text.len()
            };
            let text_slice = &text[byte_start..byte_end];
            if !text_slice.trim().is_empty() {
                spans.push(SentenceSpan {
                    text: text_slice,
                    byte_offset: byte_start,
                    char_offset: char_start,
                });
            }
            byte_start = byte_end;
            char_start += text_slice.chars().count();
        }
    }

    // Trailing text after last terminal
    if byte_start < text.len() {
        let text_slice = &text[byte_start..];
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

fn is_sentence_terminal(ch: char) -> bool {
    matches!(ch, '。' | '！' | '？' | '!' | '?')
}
```

- [ ] **Step 5: Write sentence_split tests**

In `crates/ab-plaintext/src/lib.rs`, add test module at bottom:

```rust
#[cfg(test)]
mod sentence_split_tests {
    use super::*;

    #[test]
    fn splits_on_japanese_period() {
        let spans = sentence_split("吾輩は猫である。名前はまだ無い。");
        assert_eq!(spans.len(), 2);
        assert_eq!(spans[0].text, "吾輩は猫である。");
        assert_eq!(spans[0].byte_offset, 0);
        assert_eq!(spans[1].text, "名前はまだ無い。");
    }

    #[test]
    fn keeps_adjacent_terminals_together() {
        let spans = sentence_split("本当！？そう！！！");
        assert_eq!(spans.len(), 2);
        assert_eq!(spans[0].text, "本当！？");
        assert_eq!(spans[1].text, "そう！！！");
    }

    #[test]
    fn handles_ascii_question_and_exclamation() {
        let spans = sentence_split("何だ!何だ?");
        assert_eq!(spans.len(), 2);
        assert_eq!(spans[0].text, "何だ!");
        assert_eq!(spans[1].text, "何だ?");
    }

    #[test]
    fn newlines_are_not_sentence_boundaries() {
        let spans = sentence_split("一行目\n二行目。");
        assert_eq!(spans.len(), 1);
        assert_eq!(spans[0].text, "一行目\n二行目。");
    }

    #[test]
    fn trailing_text_without_terminal_is_kept() {
        let spans = sentence_split("完成。未完成");
        assert_eq!(spans.len(), 2);
        assert_eq!(spans[0].text, "完成。");
        assert_eq!(spans[1].text, "未完成");
    }

    #[test]
    fn empty_text_returns_empty() {
        let spans = sentence_split("");
        assert!(spans.is_empty());
    }

    #[test]
    fn char_offsets_are_correct() {
        let spans = sentence_split("AB。CD。");
        assert_eq!(spans[0].char_offset, 0);
        assert_eq!(spans[1].char_offset, 3); // "AB。" is 3 chars
    }
}
```

- [ ] **Step 6: Run tests**

```bash
cargo test -p ab-plaintext -- sentence_split
```

Expected: all 7 tests PASS.

- [ ] **Step 7: Create types.rs**

`crates/ab-ortho-detect/src/types.rs`:

```rust
use std::ops::Range;

use serde::{Deserialize, Serialize};

/// What kind of normalization was applied.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum OrthoNormalization {
    /// Mechanical kata→hira conversion. Bijection except for deliberate
    /// ヴ→う゛ decomposition (chosen for analyzer dictionary compatibility).
    ScriptKatakanaToHiragana,
    /// Historical kana → modern kana. Dictionary-backed. Not reversible.
    /// No detector emits this in v1; reserved for future use.
    HistoricalToModern,
}

/// A single normalized span with provenance.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OrthoAnnotation {
    /// Byte range in the original source text (sentence boundaries).
    pub source_byte_range: Range<usize>,
    /// The text after normalization.
    pub normalized_text: String,
    /// What kind of normalization was applied.
    pub kind: OrthoNormalization,
    /// Confidence percentage 0–100. None = heuristic (deterministic).
    pub confidence: Option<u8>,
}

/// Which detector produced the annotations.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum OrthoDetectorId {
    HeuristicV1,
    MlLogisticRegression {
        /// SHA-256 hex digest of serialized model weight bytes (little-endian f32).
        model_hash: String,
    },
}

/// Maps byte ranges from normalized text coordinates to original text
/// coordinates. Built by `ortho_normalize()` alongside the output string.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OffsetMap {
    /// Sorted, non-overlapping entries. Each entry maps a contiguous
    /// normalized-text byte range to the corresponding original-text range.
    /// Tuple: (normalized_byte_offset, original_byte_offset,
    ///         length_in_normalized, length_in_original).
    entries: Vec<(usize, usize, usize, usize)>,
}

impl OffsetMap {
    /// Create an empty OffsetMap (identity mapping).
    #[must_use]
    pub fn empty() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    /// Map a byte range in normalized text to the corresponding range in
    /// original text.
    ///
    /// # Panics
    ///
    /// Panics if `norm_range` crosses an entry boundary where byte-length
    /// changed (e.g., ヴ→う゛). Callers must split spans at annotation
    /// boundaries before calling this method.
    #[must_use]
    pub fn to_original(&self, norm_range: Range<usize>) -> Range<usize> {
        if self.entries.is_empty() {
            return norm_range;
        }

        let start_entry = self
            .entries
            .iter()
            .find(|&&(noff, _, nlen, _)| {
                norm_range.start >= noff && norm_range.start < noff + nlen
            })
            .unwrap_or_else(|| {
                panic!(
                    "normalized offset {} not covered by any OffsetMap entry",
                    norm_range.start
                )
            });

        let end_entry = self
            .entries
            .iter()
            .find(|&&(noff, _, nlen, _)| {
                norm_range.end > noff && norm_range.end <= noff + nlen
            })
            .unwrap_or_else(|| {
                panic!(
                    "normalized offset {} not covered by any OffsetMap entry",
                    norm_range.end
                )
            });

        assert_eq!(
            start_entry, end_entry,
            "norm_range {:?} crosses OffsetMap entry boundary; split span first",
            norm_range
        );

        let &(noff, ooff, _nlen, _olen) = start_entry;
        let delta = norm_range.start - noff;
        let orig_len = norm_range.end - norm_range.start;
        let orig_start = ooff + delta;
        orig_start..orig_start + orig_len
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}
```

- [ ] **Step 8: Write types tests**

Add to bottom of `types.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ortho_annotation_derives_eq() {
        let a = OrthoAnnotation {
            source_byte_range: 0..5,
            normalized_text: "abc".into(),
            kind: OrthoNormalization::ScriptKatakanaToHiragana,
            confidence: None,
        };
        let b = a.clone();
        assert_eq!(a, b);
    }

    #[test]
    fn confidence_is_option_u8() {
        let a = OrthoAnnotation {
            source_byte_range: 0..3,
            normalized_text: "x".into(),
            kind: OrthoNormalization::ScriptKatakanaToHiragana,
            confidence: Some(95),
        };
        assert_eq!(a.confidence, Some(95));
        assert!(serde_json::to_string(&a).is_ok());
    }

    #[test]
    fn offset_map_empty_is_identity() {
        let map = OffsetMap::empty();
        assert_eq!(map.to_original(10..20), 10..20);
    }

    #[test]
    fn offset_map_maps_identity_region() {
        // normalized bytes 0..6 map 1:1 to original bytes 0..6
        let map = OffsetMap {
            entries: vec![(0, 0, 6, 6), (6, 6, 6, 3)],
        };
        assert_eq!(map.to_original(0..6), 0..6);
    }

    #[test]
    fn offset_map_contracts_byte_length() {
        // normalized bytes 6..12 (う゛, 6 bytes) → original bytes 6..9 (ヴ, 3 bytes)
        let map = OffsetMap {
            entries: vec![(0, 0, 6, 6), (6, 6, 6, 3)],
        };
        assert_eq!(map.to_original(6..12), 6..9);
    }

    #[test]
    #[should_panic(expected = "crosses OffsetMap entry boundary")]
    fn offset_map_panics_on_cross_entry_span() {
        let map = OffsetMap {
            entries: vec![(0, 0, 6, 6), (6, 6, 6, 3)],
        };
        let _ = map.to_original(3..9); // crosses the 0..6 / 6..12 boundary
    }

    #[test]
    #[should_panic(expected = "not covered by any OffsetMap entry")]
    fn offset_map_panics_on_uncovered_offset() {
        let map = OffsetMap {
            entries: vec![(0, 0, 6, 6)],
        };
        let _ = map.to_original(10..15);
    }

    #[test]
    fn detector_id_serializes() {
        let id = OrthoDetectorId::HeuristicV1;
        let json = serde_json::to_string(&id).unwrap();
        assert!(json.contains("HeuristicV1"));
    }
}
```

- [ ] **Step 9: Run types tests**

```bash
cargo test -p ab-ortho-detect -- types::tests
```

Expected: 7 tests PASS.

- [ ] **Step 10: Create lib.rs stub**

`crates/ab-ortho-detect/src/lib.rs`:

```rust
pub mod types;

pub use types::{
    OffsetMap, OrthoAnnotation, OrthoDetectorId, OrthoNormalization,
};
```

- [ ] **Step 11: Verify crate compiles**

```bash
cargo check -p ab-ortho-detect
```

Expected: no errors.

- [ ] **Step 12: Commit**

```bash
git add Cargo.toml Cargo.lock crates/ab-ortho-detect/ crates/ab-plaintext/src/lib.rs
git commit -m "feat: add ab-ortho-detect crate skeleton, SentenceSpan, sentence_split()

- New ab-ortho-detect crate with types.rs (OrthoAnnotation, OffsetMap, etc.)
- SentenceSpan<'a> and sentence_split() added to ab-plaintext
- ab-plaintext does NOT depend on ab-ortho-detect
- ab-ortho-detect depends on ab-plaintext for SentenceSpan type"
```

---

### Task 2: Implement kata→hira conversion

**Files:**
- Create: `crates/ab-ortho-detect/src/script.rs`
- Modify: `crates/ab-ortho-detect/src/lib.rs` (add `pub mod script`)

**Interfaces:**
- Produces: `pub fn kata_to_hira(text: &str) -> String`

- [ ] **Step 1: Write the lookup table**

`crates/ab-ortho-detect/src/script.rs`:

```rust
/// Convert full-width katakana to hiragana.
///
/// The primary U+30A1–U+30F6 range uses a constant -0x60 codepoint offset.
/// Half-width katakana (U+FF65–U+FF9F) is first converted to full-width
/// via a per-codepoint lookup, then the standard offset applies.
/// Edge cases (ヴ→う゛, small katakana extensions, iteration marks) are
/// handled explicitly.
///
/// # Examples
///
/// ```
/// use ab_ortho_detect::script::kata_to_hira;
/// assert_eq!(kata_to_hira("カタカナ"), "かたかな");
/// assert_eq!(kata_to_hira("吾輩ハ猫デアル"), "吾輩は猫である");
/// assert_eq!(kata_to_hira("今日ヴ"), "今日う゛");
/// ```
#[must_use]
pub fn kata_to_hira(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut chars = text.chars().peekable();

    while let Some(ch) = chars.next() {
        match ch {
            // Full-width katakana block: U+30A1–U+30F6 → subtract 0x60
            c if ('\u{30A1}'..='\u{30F6}').contains(&c) => {
                let hira = char::from_u32(c as u32 - 0x60).unwrap();
                out.push(hira);
            }

            // Iteration marks: U+30FD ヽ→ゝ, U+30FE ヾ→ゞ
            '\u{30FD}' => out.push('\u{309D}'),
            '\u{30FE}' => out.push('\u{309E}'),

            // Chōonpu: passthrough
            'ー' | '\u{30FC}' => out.push('ー'),

            // ヴ → う゛ (two characters for dictionary compatibility)
            '\u{30F4}' => {
                out.push('\u{3046}');
                out.push('\u{309B}');
            }

            // Obsolete kana: ヷ→わ゛, ヸ→ゐ゛, ヹ→ゑ゛, ヺ→を゛
            '\u{30F7}' => { out.push('\u{308F}'); out.push('\u{309B}'); }
            '\u{30F8}' => { out.push('\u{3090}'); out.push('\u{309B}'); }
            '\u{30F9}' => { out.push('\u{3091}'); out.push('\u{309B}'); }
            '\u{30FA}' => { out.push('\u{3092}'); out.push('\u{309B}'); }

            // Katakana Phonetic Extensions: small forms → full-size hiragana
            '\u{31F0}' => out.push('\u{304F}'), // ㇰ→く
            '\u{31F1}' => out.push('\u{3057}'), // ㇱ→し
            '\u{31F2}' => out.push('\u{3059}'), // ㇲ→す
            '\u{31F3}' => out.push('\u{3068}'), // ㇳ→と
            '\u{31F4}' => out.push('\u{306C}'), // ㇴ→ぬ
            '\u{31F5}' => out.push('\u{306F}'), // ㇵ→は
            '\u{31F6}' => out.push('\u{3072}'), // ㇶ→ひ
            '\u{31F7}' => out.push('\u{3075}'), // ㇷ→ふ
            '\u{31F8}' => out.push('\u{3078}'), // ㇸ→へ
            '\u{31F9}' => out.push('\u{307B}'), // ㇹ→ほ
            '\u{31FA}' => out.push('\u{307E}'), // ㇺ→ま
            '\u{31FB}' => out.push('\u{3084}'), // ㇻ→や
            '\u{31FC}' => out.push('\u{308A}'), // ㇼ→り
            '\u{31FD}' => out.push('\u{308B}'), // ㇽ→る
            '\u{31FE}' => out.push('\u{308C}'), // ㇾ→れ
            '\u{31FF}' => out.push('\u{308D}'), // ㇿ→ろ

            // Half-width katakana: first convert to full-width, recurse
            c if ('\u{FF65}'..='\u{FF9F}').contains(&c) => {
                if let Some(fullwidth) = halfwidth_to_fullwidth_katakana(c) {
                    // Recurse: the fullwidth char will hit the main block above
                    let s = fullwidth.to_string();
                    out.push_str(&kata_to_hira(&s));
                } else {
                    out.push(c);
                }
            }

            // Everything else: passthrough (kanji, hiragana, punctuation, etc.)
            other => out.push(other),
        }
    }

    out
}

/// Map half-width katakana codepoint to full-width katakana codepoint.
fn halfwidth_to_fullwidth_katakana(ch: char) -> Option<char> {
    let result = match ch {
        // U+FF65 ･ → U+30FB ・(katakana middle dot)
        '\u{FF65}' => '\u{30FB}',
        // U+FF66–U+FF9F: half-width katakana letters
        '\u{FF66}' => '\u{30F2}', // ｦ→ヲ
        '\u{FF67}' => '\u{30A1}', // ｧ→ァ
        '\u{FF68}' => '\u{30A3}', // ｨ→ィ
        '\u{FF69}' => '\u{30A5}', // ｩ→ゥ
        '\u{FF6A}' => '\u{30A7}', // ｪ→ェ
        '\u{FF6B}' => '\u{30A9}', // ｫ→ォ
        '\u{FF6C}' => '\u{30E3}', // ｬ→ャ
        '\u{FF6D}' => '\u{30E5}', // ｭ→ュ
        '\u{FF6E}' => '\u{30E7}', // ｮ→ョ
        '\u{FF6F}' => '\u{30C3}', // ｯ→ッ
        '\u{FF70}' => '\u{30FC}', // ｰ→ー
        '\u{FF71}' => '\u{30A2}', // ｱ→ア
        '\u{FF72}' => '\u{30A4}', // ｲ→イ
        '\u{FF73}' => '\u{30A6}', // ｳ→ウ
        '\u{FF74}' => '\u{30A8}', // ｴ→エ
        '\u{FF75}' => '\u{30AA}', // ｵ→オ
        '\u{FF76}' => '\u{30AB}', // ｶ→カ
        '\u{FF77}' => '\u{30AD}', // ｷ→キ
        '\u{FF78}' => '\u{30AF}', // ｸ→ク
        '\u{FF79}' => '\u{30B1}', // ｹ→ケ
        '\u{FF7A}' => '\u{30B3}', // ｺ→コ
        '\u{FF7B}' => '\u{30B5}', // ｻ→サ
        '\u{FF7C}' => '\u{30B7}', // ｼ→シ
        '\u{FF7D}' => '\u{30B9}', // ｽ→ス
        '\u{FF7E}' => '\u{30BB}', // ｾ→セ
        '\u{FF7F}' => '\u{30BD}', // ｿ→ソ
        '\u{FF80}' => '\u{30BF}', // ﾀ→タ
        '\u{FF81}' => '\u{30C1}', // ﾁ→チ
        '\u{FF82}' => '\u{30C4}', // ﾂ→ツ
        '\u{FF83}' => '\u{30C6}', // ﾃ→テ
        '\u{FF84}' => '\u{30C8}', // ﾄ→ト
        '\u{FF85}' => '\u{30CA}', // ﾅ→ナ
        '\u{FF86}' => '\u{30CB}', // ﾆ→ニ
        '\u{FF87}' => '\u{30CC}', // ﾇ→ヌ
        '\u{FF88}' => '\u{30CD}', // ﾈ→ネ
        '\u{FF89}' => '\u{30CE}', // ﾉ→ノ
        '\u{FF8A}' => '\u{30CF}', // ﾊ→ハ
        '\u{FF8B}' => '\u{30D2}', // ﾋ→ヒ
        '\u{FF8C}' => '\u{30D5}', // ﾌ→フ
        '\u{FF8D}' => '\u{30D8}', // ﾍ→ヘ
        '\u{FF8E}' => '\u{30DB}', // ﾎ→ホ
        '\u{FF8F}' => '\u{30DE}', // ﾏ→マ
        '\u{FF90}' => '\u{30DF}', // ﾐ→ミ
        '\u{FF91}' => '\u{30E0}', // ﾑ→ム
        '\u{FF92}' => '\u{30E1}', // ﾒ→メ
        '\u{FF93}' => '\u{30E2}', // ﾓ→モ
        '\u{FF94}' => '\u{30E4}', // ﾔ→ヤ
        '\u{FF95}' => '\u{30E6}', // ﾕ→ユ
        '\u{FF96}' => '\u{30E8}', // ﾖ→ヨ
        '\u{FF97}' => '\u{30E9}', // ﾗ→ラ
        '\u{FF98}' => '\u{30EA}', // ﾘ→リ
        '\u{FF99}' => '\u{30EB}', // ﾙ→ル
        '\u{FF9A}' => '\u{30EC}', // ﾚ→レ
        '\u{FF9B}' => '\u{30ED}', // ﾛ→ロ
        '\u{FF9C}' => '\u{30EF}', // ﾜ→ワ
        '\u{FF9D}' => '\u{30F3}', // ﾝ→ン
        '\u{FF9E}' => '\u{309B}', // ﾞ→゛ (voiced mark — maps directly to hiragana combining)
        '\u{FF9F}' => '\u{309C}', // ﾟ→゜ (semi-voiced mark)
        _ => return None,
    };
    Some(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn converts_fullwidth_katakana() {
        assert_eq!(kata_to_hira("カタカナ"), "かたかな");
    }

    #[test]
    fn converts_katakana_in_mixed_text() {
        assert_eq!(kata_to_hira("吾輩ハ猫デアル"), "吾輩は猫である");
    }

    #[test]
    fn passthrough_kanji_and_hiragana() {
        assert_eq!(kata_to_hira("漢字ひらがな"), "漢字ひらがな");
    }

    #[test]
    fn converts_vu_to_two_chars() {
        assert_eq!(kata_to_hira("今日ヴ"), "今日う゛");
    }

    #[test]
    fn converts_iteration_marks() {
        assert_eq!(kata_to_hira("ヽヽ"), "ゝゝ");
        assert_eq!(kata_to_hira("ヾ"), "ゞ");
    }

    #[test]
    fn converts_small_katakana_extensions() {
        assert_eq!(kata_to_hira("ㇱ"), "し");
        assert_eq!(kata_to_hira("ㇺ"), "ま");
        assert_eq!(kata_to_hira("ㇿ"), "ろ");
    }

    #[test]
    fn converts_halfwidth_katakana() {
        assert_eq!(kata_to_hira("\u{FF71}\u{FF72}"), "あい");
    }

    #[test]
    fn passthrough_chouonpu() {
        assert_eq!(kata_to_hira("カード"), "かーど");
    }

    #[test]
    fn empty_string() {
        assert_eq!(kata_to_hira(""), "");
    }

    #[test]
    fn converts_obsolete_kana() {
        assert_eq!(kata_to_hira("\u{30F7}"), "わ゛");
    }
}
```

- [ ] **Step 2: Add pub mod to lib.rs**

Edit `crates/ab-ortho-detect/src/lib.rs`, add:

```rust
pub mod script;
```

- [ ] **Step 3: Run tests**

```bash
cargo test -p ab-ortho-detect -- script::tests
```

Expected: 10 tests PASS.

- [ ] **Step 4: Commit**

```bash
git add crates/ab-ortho-detect/src/script.rs crates/ab-ortho-detect/src/lib.rs
git commit -m "feat: implement kata_to_hira() conversion"
```

---

### Task 3: Implement character-level feature extraction

**Files:**
- Create: `crates/ab-ortho-detect/src/features.rs`
- Modify: `crates/ab-ortho-detect/src/lib.rs` (add `pub mod features`)

**Interfaces:**
- Produces: `pub struct CharFeatures { ... }` and `pub fn extract_char_features(text: &str) -> CharFeatures`
- Produces: `pub struct TokenFeatures { ... }` (serialized, no tokenizer dependency in features.rs itself)

- [ ] **Step 1: Write CharFeatures**

`crates/ab-ortho-detect/src/features.rs`:

```rust
/// Character-level features extracted in a single pass over a sentence.
/// No tokenization required.
#[derive(Debug, Clone, PartialEq)]
pub struct CharFeatures {
    pub total_chars: usize,
    pub hiragana_count: usize,
    pub katakana_count: usize,
    pub kanji_count: usize,
    pub hiragana_ratio: f64,
    pub katakana_ratio: f64,
    pub kanji_ratio: f64,
    pub unique_char_ratio: f64,
    pub max_bigram_repeat_ratio: f64,
    pub char_run_repeat_ratio: f64,
    pub repeated_bigram_pattern_ratio: f64,
    pub katakana_at_sentence_end: bool,
}

/// Extract character-level features from a single sentence.
#[must_use]
pub fn extract_char_features(text: &str) -> CharFeatures {
    let total_chars = text.chars().count();
    if total_chars == 0 {
        return CharFeatures {
            total_chars: 0,
            hiragana_count: 0,
            katakana_count: 0,
            kanji_count: 0,
            hiragana_ratio: 0.0,
            katakana_ratio: 0.0,
            kanji_ratio: 0.0,
            unique_char_ratio: 0.0,
            max_bigram_repeat_ratio: 0.0,
            char_run_repeat_ratio: 0.0,
            repeated_bigram_pattern_ratio: 0.0,
            katakana_at_sentence_end: false,
        };
    }

    let chars: Vec<char> = text.chars().collect();
    let total = total_chars as f64;

    let mut hiragana = 0usize;
    let mut katakana = 0usize;
    let mut kanji = 0usize;

    for ch in &chars {
        if is_hiragana(*ch) {
            hiragana += 1;
        } else if is_katakana(*ch) {
            katakana += 1;
        } else if is_kanji(*ch) {
            kanji += 1;
        }
    }

    // Unique char ratio
    let unique_count = {
        let mut sorted: Vec<char> = chars.clone();
        sorted.sort_unstable();
        sorted.dedup();
        sorted.len()
    };

    // Bigram repeat: count of most frequent adjacent character bigram
    let max_bigram_repeat = if total_chars >= 2 {
        let mut bigram_counts = std::collections::HashMap::new();
        for window in chars.windows(2) {
            *bigram_counts.entry((window[0], window[1])).or_insert(0usize) += 1;
        }
        bigram_counts.values().max().copied().unwrap_or(0)
    } else {
        0
    };

    // Character run repeat: count of runs of ≥2 identical chars (Python definition)
    let char_run_count = {
        let mut runs = 0usize;
        let mut i = 0usize;
        while i < chars.len() {
            let ch = chars[i];
            let mut run_len = 1usize;
            while i + run_len < chars.len() && chars[i + run_len] == ch {
                run_len += 1;
            }
            if run_len >= 2 {
                runs += 1;
            }
            i += run_len;
        }
        runs
    };

    // Repeated bigram pattern ratio: count of distinct bigrams that appear ≥2 times
    let repeated_bigram_pattern_count = if total_chars >= 2 {
        let mut bigram_counts = std::collections::HashMap::new();
        for window in chars.windows(2) {
            *bigram_counts.entry((window[0], window[1])).or_insert(0usize) += 1;
        }
        bigram_counts.values().filter(|&&c| c >= 2).count()
    } else {
        0
    };

    // Does the sentence end in katakana?
    let katakana_at_end = chars.last().is_some_and(|ch| is_katakana(*ch));

    CharFeatures {
        total_chars,
        hiragana_count: hiragana,
        katakana_count: katakana,
        kanji_count: kanji,
        hiragana_ratio: hiragana as f64 / total,
        katakana_ratio: katakana as f64 / total,
        kanji_ratio: kanji as f64 / total,
        unique_char_ratio: unique_count as f64 / total,
        max_bigram_repeat_ratio: max_bigram_repeat as f64 / total,
        char_run_repeat_ratio: char_run_count as f64 / total,
        repeated_bigram_pattern_ratio: repeated_bigram_pattern_count as f64 / total,
        katakana_at_sentence_end: katakana_at_end,
    }
}

/// Tokenization-derived features. Computed from a Vibrato/Sudachi first pass.
/// The `features` module only defines the struct; a higher layer populates it.
#[derive(Debug, Clone, Default)]
pub struct TokenFeatures {
    pub token_count: usize,
    pub oov_count: usize,
    pub oov_ratio: f64,
    pub proper_noun_char_ratio: f64,
}

fn is_hiragana(ch: char) -> bool {
    ('\u{3041}'..='\u{3096}').contains(&ch)
}

fn is_katakana(ch: char) -> bool {
    ('\u{30A1}'..='\u{30FA}').contains(&ch)
        || ('\u{31F0}'..='\u{31FF}').contains(&ch)
        || ('\u{FF65}'..='\u{FF9F}').contains(&ch)
}

fn is_kanji(ch: char) -> bool {
    ('\u{4E00}'..='\u{9FFF}').contains(&ch)
        || ('\u{3400}'..='\u{4DBF}').contains(&ch) // CJK Ext-A
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extracts_zero_for_empty() {
        let f = extract_char_features("");
        assert_eq!(f.total_chars, 0);
        assert_eq!(f.katakana_ratio, 0.0);
    }

    #[test]
    fn detects_mixed_script() {
        let f = extract_char_features("吾輩ハ猫デアル");
        assert_eq!(f.total_chars, 8);
        assert_eq!(f.hiragana_count, 0);
        assert_eq!(f.katakana_count, 4);
        assert_eq!(f.kanji_count, 4);
        assert!((f.katakana_ratio - 0.5).abs() < 0.01);
    }

    #[test]
    fn detects_hiragana_sentence() {
        let f = extract_char_features("これは猫です");
        assert!(f.hiragana_count > 0);
    }

    #[test]
    fn detects_char_runs() {
        let f = extract_char_features("アアアア");
        assert_eq!(f.total_chars, 4);
        assert!((f.char_run_repeat_ratio - 0.25).abs() < 0.01); // 1 run / 4 chars
    }

    #[test]
    fn detects_bigram_patterns() {
        let f = extract_char_features("ABABAB");
        assert!(f.repeated_bigram_pattern_ratio > 0.0);
    }

    #[test]
    fn detects_katakana_at_end() {
        let f = extract_char_features("これは猫ダ");
        assert!(f.katakana_at_sentence_end);
    }

    #[test]
    fn katakana_not_at_end() {
        let f = extract_char_features("ダという猫");
        assert!(!f.katakana_at_sentence_end);
    }
}
```

- [ ] **Step 2: Add pub mod to lib.rs**

Edit `crates/ab-ortho-detect/src/lib.rs`, add:

```rust
pub mod features;
```

- [ ] **Step 3: Run tests**

```bash
cargo test -p ab-ortho-detect -- features::tests
```

Expected: 7 tests PASS.

- [ ] **Step 4: Commit**

```bash
git add crates/ab-ortho-detect/src/features.rs crates/ab-ortho-detect/src/lib.rs
git commit -m "feat: add character-level feature extraction"
```

---

### Task 4: Implement HeuristicV1 detector

**Files:**
- Create: `crates/ab-ortho-detect/src/heuristic.rs`
- Modify: `crates/ab-ortho-detect/src/lib.rs` (add `pub mod heuristic`)

**Interfaces:**
- Consumes: `SentenceSpan<'a>` (from `ab-plaintext`), `CharFeatures`, `TokenFeatures` (from `features`), `OrthoAnnotation`, `OrthoNormalization`, `OrthoDetectorId` (from `types`)
- Produces: `pub struct HeuristicConfig { ... }` and `pub struct HeuristicV1` implementing `OrthoDetector`

**Note:** This task introduces a dependency on `ab-morph-analyzers` for the Vibrato first pass. Add `ab-morph-analyzers` to `ab-ortho-detect/Cargo.toml`.

- [ ] **Step 1: Add dependency**

Edit `crates/ab-ortho-detect/Cargo.toml`:

```toml
[dependencies]
serde = { workspace = true, features = ["derive"] }
ab-plaintext = { workspace = true }
ab-morph-analyzers = { workspace = true }
ab-morph-diff = { workspace = true }
regex = { workspace = true }
```

- [ ] **Step 2: Write HeuristicV1**

**Pre-requisite:** Task 0 Step 2.5 must have run to determine Vibrato's actual OOV representation. If `pos1` for unknown tokens is `None` (Vibrato emits `*` → feature parser maps to `None`), the `first_pass_tokenize` OOV detection below must be adapted. Two pre-written branches:

**Branch A (pos1 is a known string like "未知語"):** Use the code below as-is.

**Branch B (pos1 is None for OOV):** Either (a) expose Vibrato's token-level `is_unk` flag by adding `is_unk: bool` to `RawToken` (in `span_builder.rs`), populate it in `vibrato.rs`, and check `m.is_unk` instead of `features.get("pos1")`, or (b) drop the OOV guard entirely (`oov_count` always 0), keep only the proper-noun guard (which depends on `pos2 == "固有名詞"`), and mark the heuristic as character-only-with-proper-noun-filter in its `OrthoDetectorId` or a warning field.

`crates/ab-ortho-detect/src/heuristic.rs`:

```rust
use std::sync::Arc;

use ab_morph_analyzers::VibratoAnalyzer;
use ab_morph_diff::FeatureMap;
use ab_plaintext::SentenceSpan;

use crate::features::{CharFeatures, TokenFeatures, extract_char_features};
use crate::script::kata_to_hira;
use crate::types::{
    OrthoAnnotation, OrthoDetectorId, OrthoNormalization,
};

/// Configurable thresholds for the v1 heuristic.
#[derive(Debug, Clone)]
pub struct HeuristicConfig {
    pub min_total_chars: usize,
    pub short_sentence_max_chars: usize,
    pub long_sentence_min_chars: usize,
    pub katakana_ratio_threshold: f64,
    pub oov_ratio_threshold: f64,
    pub proper_noun_char_ratio_threshold: f64,
    pub unique_char_ratio_min: f64,
    pub max_bigram_repeat_ratio_max: f64,
    pub char_run_repeat_ratio_max: f64,
    pub repeated_bigram_ratio_max: f64,
}

impl Default for HeuristicConfig {
    fn default() -> Self {
        Self {
            min_total_chars: 8,
            short_sentence_max_chars: 10,
            long_sentence_min_chars: 100,
            katakana_ratio_threshold: 0.5,
            oov_ratio_threshold: 0.2,
            proper_noun_char_ratio_threshold: 0.3,
            unique_char_ratio_min: 0.5,
            max_bigram_repeat_ratio_max: 0.5,
            char_run_repeat_ratio_max: 0.1,
            repeated_bigram_ratio_max: 0.1,
        }
    }
}

/// V1 heuristic: two-pass detection based on the aozora-corpus-generator
/// `is_katakana_sentence` logic.
pub struct HeuristicV1 {
    vibrato: Arc<VibratoAnalyzer>,
    config: HeuristicConfig,
    sentence_end_re: regex::Regex,
}

impl HeuristicV1 {
    /// Create a new HeuristicV1 detector.
    /// `vibrato` is used for the first-pass OOV/proper-noun analysis.
    /// Sentences accepted by the heuristic are re-tokenized with all
    /// analyzers on the normalized text (first-pass output is discarded).
    #[must_use]
    pub fn new(vibrato: Arc<VibratoAnalyzer>, config: HeuristicConfig) -> Self {
        Self {
            vibrato,
            config,
            sentence_end_re: regex::Regex::new(r"ッ?.?[？！]」$").unwrap(),
        }
    }
}

impl crate::OrthoDetector for HeuristicV1 {
    fn detector_id(&self) -> OrthoDetectorId {
        OrthoDetectorId::HeuristicV1
    }

    fn detect(
        &self,
        sentences: &[SentenceSpan<'_>],
    ) -> Vec<OrthoAnnotation> {
        let mut annotations = Vec::new();

        for sentence in sentences {
            if self.should_normalize(sentence) {
                let normalized_text = kata_to_hira(sentence.text);
                let byte_end = sentence.byte_offset + sentence.text.len();
                annotations.push(OrthoAnnotation {
                    source_byte_range: sentence.byte_offset..byte_end,
                    normalized_text,
                    kind: OrthoNormalization::ScriptKatakanaToHiragana,
                    confidence: None,
                });
            }
        }

        annotations
    }
}

impl HeuristicV1 {
    fn should_normalize(&self, sentence: &SentenceSpan<'_>) -> bool {
        let features = extract_char_features(sentence.text);

        // Gate: must have zero hiragana
        if features.hiragana_count > 0 {
            return false;
        }

        // Gate: must be katakana-dominant
        if features.katakana_ratio <= self.config.katakana_ratio_threshold {
            return false;
        }

        // Pass 1: tokenization-derived signals (Vibrato first pass)
        let token_features = self.first_pass_tokenize(sentence);

        // If all tokens are known and mostly proper nouns → list of names, not era-ortho
        if token_features.oov_count == 0
            && token_features.proper_noun_char_ratio > self.config.proper_noun_char_ratio_threshold
        {
            return false;
        }

        // High OOV → likely garbage
        if token_features.oov_ratio > self.config.oov_ratio_threshold {
            return false;
        }

        // Pass 2: character-level rejection cascade

        // Too short
        if features.total_chars < self.config.min_total_chars {
            return false;
        }

        // Short sentence ending in terminal punctuation → exclamation, not prose
        if features.total_chars < self.config.short_sentence_max_chars {
            let last_3 = if sentence.text.len() >= 3 {
                &sentence.text[sentence.text.len() - 3..]
            } else {
                sentence.text
            };
            if self.sentence_end_re.is_match(last_3) {
                return false;
            }
        }

        // Longer sentence with low uniqueness → repetitive
        if features.total_chars < self.config.long_sentence_min_chars
            && features.unique_char_ratio < self.config.unique_char_ratio_min
        {
            return false;
        }

        // Stuttering
        if features.max_bigram_repeat_ratio > self.config.max_bigram_repeat_ratio_max {
            return false;
        }

        // Character runs
        if features.char_run_repeat_ratio > self.config.char_run_repeat_ratio_max {
            return false;
        }

        // Patterned repetition
        if features.repeated_bigram_pattern_ratio > self.config.repeated_bigram_ratio_max {
            return false;
        }

        true
    }

    /// First-pass Vibrato tokenization to compute OOV ratio and proper-noun coverage.
    fn first_pass_tokenize(&self, sentence: &SentenceSpan<'_>) -> TokenFeatures {
        use ab_morph_diff::MorphAnalyzer;
        use ab_plaintext::{PlainTextDocument, SourceFormat};

        let doc = PlainTextDocument {
            text_id: String::new(),
            source_format: SourceFormat::AozoraHonbun,
            text: sentence.text.to_owned(),
        };

        let analysis = match self.vibrato.analyze(&doc) {
            Ok(a) => a,
            Err(_) => return TokenFeatures::default(),
        };

        let token_count = analysis.morphemes.len();
        let oov_count = analysis
            .morphemes
            .iter()
            .filter(|m| {
                m.features
                    .get("pos1")
                    .and_then(|v| v.as_ref())
                    .map_or(false, |pos| pos.as_ref() == "未知語")
            })
            .count();

        let proper_noun_chars: usize = analysis
            .morphemes
            .iter()
            .filter(|m| {
                m.features
                    .get("pos2")
                    .and_then(|v| v.as_ref())
                    .map_or(false, |pos| pos.as_ref() == "固有名詞")
            })
            .map(|m| m.surface.chars().count())
            .sum();

        let total_chars = sentence.text.chars().count();

        TokenFeatures {
            token_count,
            oov_count,
            oov_ratio: if token_count > 0 {
                oov_count as f64 / token_count as f64
            } else {
                0.0
            },
            proper_noun_char_ratio: if total_chars > 0 {
                proper_noun_chars as f64 / total_chars as f64
            } else {
                0.0
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ab_plaintext::SentenceSpan;

    fn make_span(text: &str) -> SentenceSpan<'_> {
        SentenceSpan {
            text,
            byte_offset: 0,
            char_offset: 0,
        }
    }

    #[test]
    #[ignore = "requires Vibrato dictionary"]
    fn rejects_hiragana_sentence() {
        let detector = HeuristicV1::new(
            Arc::new(VibratoAnalyzer::unidic_cwj_default().unwrap()),
            HeuristicConfig::default(),
        );
        let annotations = detector.detect(&[make_span("これは猫です")]);
        assert!(annotations.is_empty());
    }

    #[test]
    #[ignore = "requires Vibrato dictionary"]
    fn rejects_short_sentence() {
        let detector = HeuristicV1::new(
            Arc::new(VibratoAnalyzer::unidic_cwj_default().unwrap()),
            HeuristicConfig::default(),
        );
        let annotations = detector.detect(&[make_span("猫ダ")]);
        assert!(annotations.is_empty(), "3-char sentence should be rejected");
    }

    #[test]
    #[ignore = "requires Vibrato dictionary"]
    fn accepts_katakana_prose() {
        let detector = HeuristicV1::new(
            Arc::new(VibratoAnalyzer::unidic_cwj_default().unwrap()),
            HeuristicConfig::default(),
        );
        // 私学校行ク = 4 kanji, ハ毎日ヘ = 4 katakana → ratio = 4/8 = 0.5
        // This is exactly at the threshold; it may or may not be accepted.
        // Use a clearly katakana-dominant sentence:
        let spans = &[make_span("私ハ毎日学校ヘ行ク")];
        let annotations = detector.detect(spans);
        // 私学校行 = 3 kanji, ハ毎日ヘク = 5 katakana → ratio = 5/8 ≈ 0.625 > 0.5
        assert!(!annotations.is_empty(), "katakana-dominant sentence should be accepted");
    }

    #[test]
    #[ignore = "requires Vibrato dictionary"]
    fn rejects_character_runs() {
        let detector = HeuristicV1::new(
            Arc::new(VibratoAnalyzer::unidic_cwj_default().unwrap()),
            HeuristicConfig::default(),
        );
        let annotations = detector.detect(&[make_span("アアアアアアアア")]);
        assert!(annotations.is_empty());
    }

    /// Compile-time assertion: VibratoAnalyzer must be Send + Sync for OrthoDetector.
    /// If this fails, relax OrthoDetector bound to Send only.
    #[test]
    fn vibrato_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<VibratoAnalyzer>();
    }

    #[test]
    fn config_defaults_match_spec() {
        let config = HeuristicConfig::default();
        assert_eq!(config.min_total_chars, 8);
        assert_eq!(config.katakana_ratio_threshold, 0.5);
        assert_eq!(config.oov_ratio_threshold, 0.2);
        assert_eq!(config.proper_noun_char_ratio_threshold, 0.3);
    }
}
```

- [ ] **Step 3: Add OrthoDetector trait to lib.rs**

Edit `crates/ab-ortho-detect/src/lib.rs`, replace with:

```rust
pub mod features;
pub mod heuristic;
pub mod script;
pub mod types;

use ab_plaintext::SentenceSpan;

pub use types::{
    OffsetMap, OrthoAnnotation, OrthoDetectorId, OrthoNormalization,
};

/// The detection trait. Decoupled from normalization application.
pub trait OrthoDetector: Send + Sync {
    fn detector_id(&self) -> OrthoDetectorId;
    fn detect(&self, sentences: &[SentenceSpan<'_>]) -> Vec<OrthoAnnotation>;
}
```

- [ ] **Step 4: Add pub mod to lib.rs**

```rust
pub mod heuristic;
```

Already added above in the full replacement.

- [ ] **Step 5: Run tests (requires Vibrato dictionary)**

```bash
cargo test -p ab-ortho-detect -- heuristic::tests -- --ignored
```

Note: heuristic tests that use Vibrato require a dictionary and should be `#[ignore]` by default.

The config test runs without a dictionary:
```bash
cargo test -p ab-ortho-detect -- heuristic::tests::config_defaults_match_spec
```

- [ ] **Step 6: Commit**

```bash
git add crates/ab-ortho-detect/
git commit -m "feat: implement HeuristicV1 detector with OrthoDetector trait"
```

---

### Task 5: Implement `ortho_normalize()` and wire `OffsetMap` construction

**Files:**
- Modify: `crates/ab-ortho-detect/src/lib.rs` (add `ortho_normalize()`, `OffsetMap` builder)

**Interfaces:**
- Consumes: `OrthoAnnotation` (from `types`)
- Produces: `pub fn ortho_normalize(original: &str, annotations: &[OrthoAnnotation]) -> (String, OffsetMap)`

- [ ] **Step 1: Add ortho_normalize to lib.rs**

Edit `crates/ab-ortho-detect/src/lib.rs`, add after the `OrthoDetector` trait:

```rust
/// Apply annotations to produce a normalized text view and an OffsetMap.
/// Annotations must be sorted by source_byte_range and non-overlapping.
/// Gaps of unchanged text between annotations produce identity map entries.
#[must_use]
pub fn ortho_normalize(
    original: &str,
    annotations: &[OrthoAnnotation],
) -> (String, OffsetMap) {
    if annotations.is_empty() {
        return (original.to_owned(), OffsetMap::empty());
    }

    let mut normalized = String::with_capacity(original.len());
    let mut entries: Vec<(usize, usize, usize, usize)> = Vec::new();
    let mut orig_cursor = 0usize;
    let mut norm_cursor = 0usize;

    for annotation in annotations {
        let span = &annotation.source_byte_range;

        if orig_cursor < span.start {
            let unchanged = &original[orig_cursor..span.start];
            normalized.push_str(unchanged);
            let len = unchanged.len();
            entries.push((norm_cursor, orig_cursor, len, len));
            norm_cursor += len;
        }
        orig_cursor = span.start;

        normalized.push_str(&annotation.normalized_text);
        let norm_len = annotation.normalized_text.len();
        let orig_len = span.end - span.start;
        entries.push((norm_cursor, orig_cursor, norm_len, orig_len));
        norm_cursor += norm_len;
        orig_cursor = span.end;
    }

    if orig_cursor < original.len() {
        let unchanged = &original[orig_cursor..];
        normalized.push_str(unchanged);
        let len = unchanged.len();
        entries.push((norm_cursor, orig_cursor, len, len));
    }

    (normalized, OffsetMap { entries })
}
```

- [ ] **Step 2: Write ortho_normalize tests**

Add to bottom of `crates/ab-ortho-detect/src/lib.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use types::OrthoNormalization;

    fn ann(start: usize, end: usize, norm: &str) -> OrthoAnnotation {
        OrthoAnnotation {
            source_byte_range: start..end,
            normalized_text: norm.to_owned(),
            kind: OrthoNormalization::ScriptKatakanaToHiragana,
            confidence: None,
        }
    }

    #[test]
    fn empty_annotations_returns_original() {
        let (text, map) = ortho_normalize("hello", &[]);
        assert_eq!(text, "hello");
        assert!(map.is_empty());
    }

    #[test]
    fn normalizes_katakana_sentence() {
        let annotations = vec![ann(0, 18, "吾輩は猫である")];
        let original = "吾輩ハ猫デアル";
        let (text, map) = ortho_normalize(original, &annotations);
        assert_eq!(text, "吾輩は猫である");
        assert_eq!(map.to_original(0..text.len()), 0..original.len());
    }

    #[test]
    fn preserves_unchanged_text_between_annotations() {
        let annotations = vec![ann(21, 39, "名前はまだ無い")];
        let original = "吾輩は猫である。名前ハマダ無イ";
        let (text, _map) = ortho_normalize(original, &annotations);
        assert_eq!(text, "吾輩は猫である。名前はまだ無い");
    }

    #[test]
    fn offset_map_handles_vu_expansion() {
        let annotations = vec![ann(6, 9, "う゛")];
        let original = "今日ヴ";
        let (text, map) = ortho_normalize(original, &annotations);
        assert_eq!(text, "今日う゛");
        assert_eq!(map.to_original(0..6), 0..6);
        assert_eq!(map.to_original(6..12), 6..9);
    }
}
```

- [ ] **Step 3: Run tests**

```bash
cargo test -p ab-ortho-detect -- lib::tests
```

Expected: 4 tests PASS.

- [ ] **Step 4: Commit**

```bash
git add crates/ab-ortho-detect/src/lib.rs
git commit -m "feat: implement ortho_normalize() with OffsetMap construction"
```

---

### Task 6: Add ortho fields to `Analysis` model

**Files:**
- Modify: `crates/ab-morph-diff/src/model.rs`
- Modify: `crates/ab-morph-diff/Cargo.toml`

**Interfaces:**
- Consumes: `OrthoAnnotation`, `OffsetMap` (from `ab-ortho-detect`)
- Produces: two new `Option` fields on `Analysis`

- [ ] **Step 1: Add dep**

Edit `crates/ab-morph-diff/Cargo.toml`, add under `[dependencies]`:

```toml
ab-ortho-detect = { workspace = true }
```

- [ ] **Step 2: Add fields**

In `crates/ab-morph-diff/src/model.rs`, add to the `Analysis` struct:

```rust
    /// Orthographic normalizations applied before tokenization.
    pub ortho_annotations: Option<Vec<ab_ortho_detect::OrthoAnnotation>>,
    /// Maps normalized-text byte ranges to original-text byte ranges.
    pub ortho_offset_map: Option<ab_ortho_detect::OffsetMap>,
```

- [ ] **Step 3: Update all construction sites**

Add `ortho_annotations: None, ortho_offset_map: None` at these locations:

1. `crates/ab-morph-analyzers/src/span_builder.rs` — function `build_analysis_from_tokens` (~line 22), the `Analysis { ... }` struct literal.
2. Any test file that constructs `Analysis` directly — search with:

```bash
grep -rn "Analysis {" crates/ --include="*.rs" | grep -v ".cargo/" | grep -v "target/"
```

3. `crates/ab-morph-run/src/pipeline.rs` — if any direct `Analysis { ... }` construction exists (most paths use `analyzer.analyze()` which returns Analysis; these already compile after `span_builder` is updated).

- [ ] **Step 4: Verify compilation**

```bash
cargo check -p ab-morph-diff -p ab-morph-run -p ab-morph-analyzers 2>&1 | head -20
```

Expected: no errors.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-morph-diff/ crates/ab-morph-run/src/pipeline.rs crates/ab-morph-analyzers/src/span_builder.rs
git commit -m "feat: add ortho_annotations and ortho_offset_map to Analysis"
```

---

### Task 7: Add `remap_spans()` to span_builder

**Files:**
- Modify: `crates/ab-morph-analyzers/src/span_builder.rs`

**Interfaces:**
- Consumes: `OffsetMap` (from `ab-ortho-detect`)
- Produces: `pub fn remap_spans(analysis: &mut Analysis, offset_map: &OffsetMap)`

- [ ] **Step 1: Add remap_spans function**

At top of `crates/ab-morph-analyzers/src/span_builder.rs`, add import:

```rust
use ab_ortho_detect::OffsetMap;
```

Add at bottom of file (before tests):

```rust
/// Post-process an Analysis to remap morpheme byte_spans and char_spans
/// from normalized-text coordinates to original-text coordinates.
pub fn remap_spans(analysis: &mut ab_morph_diff::Analysis, offset_map: &OffsetMap) {
    if offset_map.is_empty() {
        return;
    }
    for morpheme in &mut analysis.morphemes {
        // Split spans that cross annotation boundaries before remapping.
        // For Phase 1, we assume single-annotation spans (the common case);
        // cross-boundary spans (ヴ→う゛ edge) are rare and handled by
        // the OffsetMap panic guard.
        morpheme.byte_span = offset_map.to_original(morpheme.byte_span.clone());
        // char_span recalculation requires the original source text.
        // For Phase 1, keep the normalized-text char_span as an approximation.
        // Follow-up: rebuild char_map from original text bytes.
    }
}
```

- [ ] **Step 2: Run existing tests**

```bash
cargo test -p ab-morph-analyzers -- span_builder
```

Expected: existing tests still PASS.

- [ ] **Step 3: Commit**

```bash
git add crates/ab-morph-analyzers/src/span_builder.rs
git commit -m "feat: add remap_spans() for OffsetMap coordinate conversion"
```

---

### Task 8: Wire into ab-morph-run pipeline and CLI

**Files:**
- Modify: `crates/ab-morph-run/src/options.rs`
- Modify: `crates/ab-morph-run/src/pipeline.rs`
- Modify: `crates/ab-morph-run/Cargo.toml`

- [ ] **Step 1: Add dep**

Edit `crates/ab-morph-run/Cargo.toml`:

```toml
ab-ortho-detect = { workspace = true }
```

- [ ] **Step 2: Add CLI flag and thread through pipeline**

**2a.** In `crates/ab-morph-run/src/options.rs`, add the field to `SerialRunOptions`:

```rust
/// Enable orthographic normalization (katakana→hiragana) for pre-war text.
pub ortho_detect: OrthoDetectMode,
```

And add the enum (before `SerialRunOptions`):

```rust
#[derive(Debug, Clone, Copy, clap::ValueEnum, PartialEq, Eq)]
pub enum OrthoDetectMode {
    Off,
    Heuristic,
}
```

**2b.** In `crates/ab-morph-run/src/main.rs`, find where `SerialRunOptions` is constructed from the `AnalyzeAat` subcommand args and add:

```rust
ortho_detect: args.ortho_detect,
```

**2c.** In `main.rs`, add `#[arg(long = "ortho-detect", value_enum, default_value_t = OrthoDetectMode::Off)]` to the `AnalyzeAat` subcommand's struct.

**2d.** Thread `ortho_detect` as a new parameter through `run_analyze_aat` (pipeline.rs:5) → `run_analyze_aat_with_nway_impl` (pipeline.rs:30). Add the parameter after existing args; the warehouse variants can be skipped in Phase 1 (pass `OrthoDetectMode::Off` for now).

- [ ] **Step 3: Wire normalization in run_analyze_aat_with_nway_impl**

In `crates/ab-morph-run/src/pipeline.rs`, function `run_analyze_aat_with_nway_impl` (~line 541). This function constructs `PlainTextDocument` via `from_aat_value(&aat)?`, then loops over analyzers calling `analyzer.analyze(&document)`, and passes `&document.text` to downstream consumers for example rendering.

**Location:** After `let document = from_aat_value(&aat)?;` (~line 541), before the analyzer loop.

```rust
use ab_ortho_detect::{OrthoDetector, heuristic::HeuristicV1, ortho_normalize};
use ab_plaintext::sentence_split;
use ab_morph_analyzers::VibratoAnalyzer;

// After `let document = from_aat_value(&aat)?;`:

// Construct detection Vibrato ONCE per pipeline invocation (not per document).
// For v1, construct a dedicated instance; the ZSTD lock serializes the load.
let detection_vibrato: Option<Arc<VibratoAnalyzer>> = if ortho_detect == OrthoDetectMode::Heuristic {
    Some(Arc::new(
        VibratoAnalyzer::unidic_cwj_default()
            .expect("Vibrato dictionary required for --ortho-detect heuristic"),
    ))
} else {
    None
};

// Then, for each document (if in corpus mode), or once for single-document mode:
let (normalized_text, offset_map_opt, annotations_opt) =
    if let Some(ref vibrato) = detection_vibrato {
        let config = ab_ortho_detect::heuristic::HeuristicConfig::default();
        let detector = HeuristicV1::new(Arc::clone(vibrato), config);
        let sentences = sentence_split(&document.text);
        let annotations = detector.detect(&sentences);

        if annotations.is_empty() {
            (document.text.clone(), None, None)
        } else {
            let (norm_text, map) = ortho_normalize(&document.text, &annotations);
            (norm_text, Some(map), Some(annotations))
        }
    } else {
        (document.text.clone(), None, None)
    };

let norm_doc = ab_plaintext::PlainTextDocument {
    text_id: document.text_id.clone(),
    source_format: document.source_format,
    text: normalized_text,
};

// In the analyzer loop (~line 583), replace analyzer.analyze(&document) with:
let mut analysis = analyzer.analyze(&norm_doc)?;
if let Some(ref map) = offset_map_opt {
    ab_morph_analyzers::span_builder::remap_spans(&mut analysis, map);
}
analysis.ortho_annotations = annotations_opt.clone();
analysis.ortho_offset_map = offset_map_opt.clone();

// INVARIANT for downstream consumers (comparison writer, warehouse, nway helpers):
// After remap_spans, morpheme.byte_span and char_span are in ORIGINAL-document
// coordinates. Existing helpers slice document.text (original) at these spans.
// Keep passing &document.text (original) to all three helper call sites —
// the spans are now in original coords, so slicing is consistent.
// analysis.source_text holds the normalized text (for inspection), but
// example rendering must use document.text + morpheme.byte_span.
```

**Per-sentence cost note:** `HeuristicV1::detect()` calls `first_pass_tokenize()` per sentence — each call constructs a `PlainTextDocument`, clones sentence text, and runs `VibratoAnalyzer::analyze()` (including `semantic_chunks` with 32KB limit and ZSTD lock overhead). For 500 sentences this is 500 tokenizer resets. Acceptable for an evaluation harness; Phase 2 could batch.

**Verification:** Run Task 8 Step 5 smoke test. Manually verify that `analyses/` output JSON has `.ortho_annotations` populated, and that `comparisons.jsonl` surfaces exist (even if they show 0 diffs for a single-analyzer run).

- [ ] **Step 4: Verify compilation**

```bash
cargo check -p ab-morph-run 2>&1 | head -20
```

Expected: no errors. Fix any missing imports or type mismatches.

- [ ] **Step 5: Manual smoke test**

```bash
cargo run -p ab-morph-run -- analyze-aat \
  --ortho-detect heuristic \
  --analyzer vibrato:unidic-cwj-202512 \
  --aat data/ortho-pilot/sentences.aat.json \
  --output-dir /tmp/ortho-smoke/ \
  --comparisons-output /tmp/ortho-smoke/comparisons.jsonl
```

Expected: runs without panic. Check `/tmp/ortho-smoke/comparisons.jsonl` is non-empty valid JSONL. Inspect `analyses/` output for `ortho_annotations` field.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-morph-run/
git commit -m "feat: wire --ortho-detect flag into ab-morph-run pipeline"
```

---

### Task 9: Integration test

**Files:**
- Create: `crates/ab-ortho-detect/tests/integration_test.rs`

- [ ] **Step 1: Write integration test**

```rust
use ab_ortho_detect::heuristic::{HeuristicConfig, HeuristicV1};
use ab_ortho_detect::{OrthoDetector, ortho_normalize};
use ab_plaintext::sentence_split;

#[test]
#[ignore = "requires Vibrato dictionary"]
fn detect_and_normalize_katakana_prose() {
    let vibrato = std::sync::Arc::new(
        ab_morph_analyzers::VibratoAnalyzer::unidic_cwj_default().unwrap(),
    );
    let detector = HeuristicV1::new(vibrato, HeuristicConfig::default());
    let text = "吾輩は猫である。名前はまだ無い。私ハ毎日学校ヘ行ク。";
    let sentences = sentence_split(text);
    let annotations = detector.detect(&sentences);
    assert_eq!(annotations.len(), 1);
    assert_eq!(
        annotations[0].kind,
        ab_ortho_detect::OrthoNormalization::ScriptKatakanaToHiragana,
    );
}

#[test]
fn no_normalization_for_modern_text() {
    // Requires dictionary; same pattern as above but expects empty annotations
}

#[test]
fn kata_to_hira_roundtrip_preserves_meaning() {
    let original = "私ハ学生デアル";
    let normalized = ab_ortho_detect::script::kata_to_hira(original);
    assert_eq!(normalized, "私は学生である");
}

#[test]
fn offset_map_correct_for_mixed_text() {
    let annotations = vec![ab_ortho_detect::OrthoAnnotation {
        source_byte_range: 21..39,
        normalized_text: "名前はまだ無い".into(),
        kind: ab_ortho_detect::OrthoNormalization::ScriptKatakanaToHiragana,
        confidence: None,
    }];
    let original = "吾輩は猫である。名前ハマダ無イ";
    let (_normalized, map) = ortho_normalize(original, &annotations);
    assert_eq!(map.to_original(0..21), 0..21);
    let nlen = "名前はまだ無い".len();
    let olen = "名前ハマダ無イ".len();
    assert_eq!(nlen, olen);
    assert_eq!(map.to_original(21..21 + nlen), 21..21 + olen);
}
```

- [ ] **Step 2: Run tests**

```bash
cargo test -p ab-ortho-detect --test integration_test -- kata_to_hira_roundtrip offset_map_correct
cargo test -p ab-ortho-detect --test integration_test -- --ignored  # requires dictionary
```

- [ ] **Step 3: Commit**

```bash
git add crates/ab-ortho-detect/tests/
git commit -m "test: add integration tests for ortho-detect pipeline"
```

---

## Task Dependency Graph

```
Task 0 (Pre-Phase-1 Verification) — gates Tasks 4, 8, 9 (CLI shape, OOV repr)
Task 1 (Crate skeleton + SentenceSpan) — no deps
Task 2 (kata_to_hira) — depends on Task 1
Task 3 (Feature extraction) — depends on Task 1
Task 4 (HeuristicV1) — depends on Tasks 0, 1, 2, 3
Task 5 (ortho_normalize) — depends on Tasks 1, 2
Task 6 (Analysis model) — depends on Task 1
Task 7 (span_builder remapping) — depends on Tasks 1, 6
Task 7.5 (char_span fix) — depends on Task 7 (see corrective note C4)
Task 8 (ab-morph-run wiring) — depends on Tasks 0, 1, 4, 5, 6, 7
Task 9 (Integration tests) — depends on Tasks 1–8
```

Tasks 2, 3, 5 can run in parallel after Task 1.
Tasks 6, 7 can run in parallel after Task 1.
Task 4 needs Task 0 completed (OOV representation known) plus Tasks 2+3.
Task 8 needs Tasks 4+5+7 completed.
Task 9 is the final gate.
