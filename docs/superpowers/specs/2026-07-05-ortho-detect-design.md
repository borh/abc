# Orthographic Detection and Normalization (`ab-ortho-detect`)

## Summary

Add an optional orthographic-detection and normalization layer to the
ab-validator pipeline so that morphological analyzers receive pre-normalized
text when the source exhibits pre-modern Japanese orthography. The layer is
applied once per document, uniformly to all analyzers, behind a configuration
flag. The original document text is never mutated; the normalized form is a
separate view fed to analyzers, with `OrthoAnnotation` spans bridging the two
coordinate spaces.

Two distinct phenomena are addressed:

1. **Script normalization** (katakana → hiragana). The source uses katakana
   where modern Japanese uses hiragana (particles, okurigana, auxiliary verbs)
   but the underlying kana orthography is modern. The kata→hira mapping is a
   bijection except for the deliberate ヴ→う゛ decomposition (chosen for
   analyzer dictionary compatibility; the single-codepoint ゔ U+3094 exists
   but is rarely recognized). All other mappings are invertible via codepoint
   offset. This is the problem the aozora-corpus-generator
   `is_katakana_sentence` heuristic targets. v1 implements this.

2. **Historical-orthography normalization** (歴史的仮名遣い → 現代仮名遣い).
   The kana sequence itself differs from modern usage (`けふ` → `きょう`).
   This requires dictionary lookup or learned mappings. Not mechanically
   reversible. v1 does not implement this (no detector emits
   `HistoricalToModern` yet); it is a future lane.

## Motivation

Japanese morphological analyzers (Vibrato/UniDic, Sudachi, Vaporetto) are
trained on modern orthography where hiragana carries grammatical function.
Pre-war and early post-war Aozora Bunko texts frequently use
kanji-katakana-majiri form where particles, okurigana, and auxiliary verbs
appear in katakana. These analyzers produce degraded segmentations and
inflated OOV rates on such text.

Normalizing before analysis removes the orthography confound from analyzer
comparisons, so evaluation measures each analyzer's best-effort output rather
than its raw tolerance of non-standard orthography.

## Architecture

```
Source bytes
 → ab-plaintext (visible-text projection)
 → PlainTextDocument { text: original }              ← never mutated
 → [NEW] sentence_split()        → Vec<SentenceSpan>
 → [NEW] OrthoDetector::detect() → Vec<OrthoAnnotation>
 → [NEW] ortho_normalize()       → (String, OffsetMap) (normalized text + coord map)
 → MorphAnalyzer::analyze() × N  → N × Analysis {
       source_text: normalized_text,
       ortho_annotations: what was changed,
       ortho_offset_map: coord bridge to original
   }
```

Key invariants:

- **PlainTextDocument.text is always the original source text.** It is never
  replaced.
- **Analyzers receive the normalized text** as their `source_text`.
- **Morpheme byte_span and char_span are in original-document coordinates.**
  The `OrthoAnnotation.source_byte_range` bridges normalized-surface token
  positions back to the original source.
- OrthoAnnotations are **sorted, non-overlapping**, and each maps to a
  complete sentence span.

### Crate: `ab-ortho-detect` (new, Rust)

Dependency direction: `ab-ortho-detect` has **zero workspace dependencies**
except the standard library and `serde`. It owns the detector trait, the
annotation types, the kata↔hira conversion, and the sentence-span type.
`ab-morph-run` depends on `ab-ortho-detect` and wires it into the pipeline.
`ab-plaintext` does **not** depend on `ab-ortho-detect`.

```
crates/ab-ortho-detect/
├── Cargo.toml
└── src/
    ├── lib.rs            # public API, OrthoDetector trait, ortho_normalize()
    ├── types.rs          # OrthoAnnotation, OrthoNormalization, OrthoDetectorId, SentenceSpan
    ├── script.rs         # kata→hira conversion
    ├── heuristic.rs      # v1 two-pass heuristic (Vibrato-first-pass + character features)
    ├── features.rs       # feature extraction (character-level + tokenization-derived)
    └── ml.rs             # future: logistic regression classifier
```

#### Key types

```rust
/// What kind of normalization was applied.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum OrthoNormalization {
    /// Mechanical kata→hira conversion. Bijection except for deliberate ヴ→う゛ decomposition (see script.rs).
    ScriptKatakanaToHiragana,
    /// Historical kana → modern kana. Dictionary-backed. Not reversible.
    /// No detector emits this in v1/v2; reserved for future use.
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
        /// SHA-256 hex digest of serialized model weights.
        model_hash: String,
    },
}

/// A sentence span. Borrows from the source text (zero-copy).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SentenceSpan<'a> {
    pub text: &'a str,
    pub byte_offset: usize,
    pub char_offset: usize,
}

/// The detection trait.
pub trait OrthoDetector: Send + Sync {
    fn detector_id(&self) -> OrthoDetectorId;
    fn detect(&self, sentences: &[SentenceSpan<'_>]) -> Vec<OrthoAnnotation>;
}

//// Apply annotations to produce a normalized text view and a coordinate
/// map from normalized-byte offsets back to original-byte offsets.
/// Annotations must be sorted and non-overlapping.
pub fn ortho_normalize(original: &str, annotations: &[OrthoAnnotation]) -> (String, OffsetMap);

/// Maps byte ranges from normalized text coordinates to original text
/// coordinates. Built by `ortho_normalize()` alongside the output string.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OffsetMap {
    // sorted entries: (normalized_byte_offset, original_byte_offset,
    //                   length_in_normalized, length_in_original)
    entries: Vec<(usize, usize, usize, usize)>,
}

impl OffsetMap {
    /// Map a byte range in normalized text to the corresponding range in
    /// original text. Panics if the range crosses an entry boundary where
    /// byte-length changed (ヴ→う゛); callers must split spans at annotation
    /// boundaries first.
    pub fn to_original(&self, norm_range: Range<usize>) -> Range<usize>;
}
```

### Sentence splitting (new, in `ab-plaintext`)

The existing `semantic_chunks()` in `crates/ab-morph-analyzers/src/chunking.rs`
is byte-bounded chunking for analyzer dispatch — it is **not** sentence
splitting. It returns borrowed `TextChunk<'a>` slices bounded by `max_bytes`
with a `hard_split` fallback, not linguistic sentences.

Phase 1 introduces a genuine sentence splitter as `ab-plaintext::sentence_split()`:

```rust
pub fn sentence_split(text: &str) -> Vec<SentenceSpan<'_>>;
```

It splits on the sentence-terminal characters: `。`, `！`, `？`, `!`, `?`.
(ASCII `!` and `?` are included because they appear in Aozora text alongside
fullwidth variants.) Newlines are **not** sentence boundaries for this
splitter — they are paragraph breaks already handled upstream by the
plaintext pipeline. Adjacent terminal characters are kept together (e.g.,
`本当！？` is one sentence). `semantic_chunks` remains separate for its
original purpose (byte-bounded chunking for tokenizer dispatch).

### Detection algorithm (v1, two-pass)

v1 is a faithful port of the aozora-corpus-generator `is_katakana_sentence`
heuristic (aozora.py:311–349). It uses a **two-pass** approach:

**Pass 1 (Vibrato tokenization):** Tokenize the sentence with Vibrato to
compute two coarse structural signals:

| Signal | Threshold | Meaning |
|---|---|---|
| `oov_ratio` | > 0.2 → REJECT | High OOV = garbage text, not era-orthography |
| `proper_noun_char_ratio` | > 0.3 AND oov_count == 0 → REJECT | List of names, not era-orthography |

**Pass 2 (character-level features):** If Pass 1 does not reject, apply
character-level gates in a single scan:

```
DETECT(sentence, oov_ratio, proper_noun_char_ratio):
  if hiragana_count > 0              → REJECT
  if katakana_ratio <= 0.5           → REJECT
  if oov_count == 0
     AND proper_noun_char_ratio > 0.3 → REJECT (rare case: all-known proper nouns)
  if oov_ratio > 0.2                 → REJECT (garbage)
  if total_chars < 8                 → REJECT (too short)
  if total_chars < 10
     AND ends_with_sentence_final    → REJECT (exclamation)
  if total_chars < 100
     AND unique_char_ratio < 0.5     → REJECT (repetitive)
  if max_bigram_repeat_ratio > 0.5   → REJECT (stuttering)
  if char_run_repeat_ratio > 0.1     → REJECT (character runs)
  if repeated_bigram_ratio > 0.1     → REJECT (patterned repetition)
  → ACCEPT
```

`ends_with_sentence_final` is a verbatim port of the Python heuristic:
match the last 1–3 characters of the sentence against the regex
`ッ?.?[？！]」$`. Short utterances that end in a terminal punctuation
mark followed by a closing quote (e.g., `ところだ。」`) are rejected.

`char_run_repeat_ratio` uses the Python heuristic's definition:
`count_of_runs / total_chars`, where `count_of_runs` is the number of
sequences of ≥2 identical consecutive characters (not the number of
characters in those runs). `repeated_bigram_ratio` is similarly
`count_of_repeated_bigram_patterns / total_chars`.

Parameters (`8`, `10`, `100`, `0.5`, `0.1`, `0.2`, `0.3`) are configurable
via a TOML configuration file with these defaults. CLI flags may override
for experimentation.

**Vibrato-first-pass coupling:** The OOV and proper-noun ratio signals require
tokenization, coupling Vibrato into the detection step. This is the same
coupling the Python heuristic has with MeCab. These signals are coarse
structural features (OOV count, proper-noun category), not analyzer-specific
judgments. If a sentence is accepted for normalization, Vibrato is re-run on
the normalized text alongside all other analyzers — the first-pass output is
discarded. If this coupling proves problematic in practice, the Phase 2 ML
classifier can be feature-ablated to character-only and retrained.

### Feature extraction (shared by heuristic and future ML)

Character-level features (single pass, no tokenization):

| Feature | Type | Signal |
|---|---|---|
| `hiragana_count` | usize | Must be 0 for detection |
| `hiragana_ratio` | f64 | Zero-gate |
| `katakana_ratio` | f64 | Dominance gate |
| `kanji_ratio` | f64 | Discriminates pure-katakana from prose |
| `total_chars` | usize | Length guard |
| `unique_char_ratio` | f64 | Low = repetition |
| `max_bigram_repeat_ratio` | f64 | Stuttering guard |
| `char_run_repeat_ratio` | f64 | Run-length guard |
| `repeated_bigram_pattern_ratio` | f64 | Pattern guard |
| `katakana_at_sentence_end` | bool | Exclamation/particle |

Tokenization-derived features (Vibrato first pass):

| Feature | Type | Signal |
|---|---|---|
| `oov_count` | usize | Raw OOV token count |
| `oov_ratio` | f64 | Normalized by token count |
| `proper_noun_char_ratio` | f64 | Characters covered by 固有名詞 tokens |

### Kata→hira conversion

Located in `ab-ortho-detect/src/script.rs`. The primary Unicode range uses a
constant offset:

- Hiragana block: U+3041–U+3096
- Katakana block: U+30A1–U+30F6
- Mapping: subtract 0x60 from katakana codepoint → hiragana codepoint

This range (U+30A1–U+30F6) is covered by a `[char; 0x100]` lookup table.
Characters outside this range are handled by explicit match arms.

**Edge cases and additional ranges:**

| Character(s) | Mapping | Range |
|---|---|---|
| ヴ (U+30F4) | う゛ (U+3046 U+309B) | U+30A0 block — two characters for analyzer compatibility (う゛ is more widely recognized than ゔ U+3094). Breaks the mathematical bijection; acceptable because script normalization is still practically reversible for the original-source round-trip. |
| ヷ ヸ ヹ ヺ (U+30F7–U+30FA) | わ゛ ゐ゛ ゑ゛ を゛ | Obsolete kana |
| ㇰ–ㇿ (U+31F0–U+31FF) | Explicit per-codepoint lookup | Katakana Phonetic Extensions — not contiguous to full hiragana; each maps to its full-size counterpart (e.g., ㇱ→し, ㇲ→す, ㇺ→む) |
| ー (U+30FC) | ー (passthrough) | Chōonpu — shared by both scripts, no conversion needed |
| ヽ ヾ (U+30FD–U+30FE) | ゝ ゞ (U+309D–U+309E) | Iteration marks — subtract 0x60 |
| Half-width katakana (U+FF65–U+FF9F) | Explicit per-codepoint lookup | Half-width→full-width katakana via a lookup table (U+FF66  ｦ→ヲ, U+FF67 ｧ→ァ, … U+FF9F ﾟ→U+309A combining mark), then apply the standard kata→hira mapping. Handled in `script.rs` with a separate match arm; no external dependency. Pre-war Aozora honbun overwhelmingly uses full-width kana, but half-width appears in some digitized texts. **Sudachi note:** Sudachi's `DefaultInputTextPlugin` already performs NFKC normalization (which includes half-width→full-width), so this step is redundant for Sudachi but harmless (idempotent). It is needed for Vibrato and Vaporetto, which do not normalize their input. |

Implementation is a free function `kata_to_hira(text: &str) -> String` with no
external dependency.

## Pipeline Integration

### Plaintext mode (`ab-morph-run`)

```
ab-morph-run --ortho-detect[=heuristic|off] ...
```

When enabled:

1. `PlainTextDocument` is constructed as today (original text).
2. The original text is sentence-split via `ab-plaintext::sentence_split()`.
3. `OrthoDetector::detect()` produces `Vec<OrthoAnnotation>`.
4. `ortho_normalize()` produces both the normalized text string and an
   `OffsetMap`. The normalized text (wrapped in a new `PlainTextDocument`)
   is passed to each `MorphAnalyzer::analyze()`. The original document
   is retained and not mutated.
6. Each `Analysis` records:
   - `source_text`: the normalized text (what the analyzer saw)
   - `ortho_annotations: Option<Vec<OrthoAnnotation>>`: what was changed
   - `ortho_offset_map: Option<OffsetMap>`: coordinate bridge
7. Morpheme `byte_span` and `char_span` are produced by `span_builder` in
   **normalized-text coordinates** (the analyzer operates on normalized
   text). A post-processing step applies `OffsetMap::to_original()` to
   remap spans to **original-document coordinates**. For the common case
   where kata→hira preserves byte lengths (same number of UTF-8 bytes per
   codepoint), the OffsetMap is an identity on all unchanged regions.
   For the ヴ→う゛ case (3 bytes → 6 bytes), the `to_original()` mapping
   contracts the span, and callers must split morpheme spans at annotation
   boundaries before remapping.

**OffsetMap algorithm (ヴ→う゛ example):** Given original `今日ヴ` (9 bytes:
今日=6, ヴ=3), normalized `今日う゛` (12 bytes: 今日=6, う=3, ゛=3). The
map entries are [(0,0,6,6), (6,6,6,3)] — the first 6 bytes map 1:1, then
normalized bytes 6–12 map to original bytes 6–9 (3 bytes original). A morpheme spanning
normalized bytes 6–12 is split into one or more annotations; the う゛ span
(6 bytes normalized) maps to ヴ (3 bytes original).

**Cost note:** v1's two-pass approach runs Vibrato twice for every sentence
accepted for normalization (once for OOV/proper-noun detection, once as a
compared analyzer). For texts with high katakana-sentence density, this
roughly doubles Vibrato's wall-clock cost. This is acceptable for an
evaluation harness where accuracy matters more than throughput. The
first-pass tokens are discarded rather than reused because the `MorphAnalyzer`
trait does not accept pre-tokenized input.

### IR/AAT mode (`ab-ir`)

The `AatProjection` gains an optional field. Note: `OrthoAnnotation` derives
`Eq` (confidence is `Option<u8>`), so `AatProjection`'s existing `Eq` derive
is preserved.

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AatProjection {
    pub blocks: Vec<serde_json::Value>,
    pub warnings: Vec<ProjectionWarning>,
    pub ortho_normalizations: Option<Vec<OrthoAnnotation>>,  // NEW
}
```

When rendering TEI P5, each normalized span becomes a `<choice>` element.
`<orig>` is taken from `original_doc.text[annotation.source_byte_range]`;
`<reg>` is `annotation.normalized_text`:

```xml
<choice>
  <orig>吾輩ハ猫デアル</orig>
  <reg>吾輩は猫である</reg>
</choice>
```

The annotation layer is external to the `Block`/`Inline` tree — it does not
mutate IR nodes. Ruby bases and gaiji-resolved text in the visible-text
projection may contain katakana and will be normalized (intended behavior).

### Crate impact summary

| Crate | Change |
|---|---|
| `ab-ortho-detect` | **New.** `OrthoDetector` trait, v1 heuristic, kata↔hira, annotation types, `ortho_normalize()`. Zero workspace deps. |
| `ab-plaintext` | New `sentence_split()` returning `SentenceSpan<'_>`. No dependency on `ab-ortho-detect`. |
| `ab-morph-analyzers` | `span_builder` gains optional `&[OrthoAnnotation]` for coordinate mapping. `semantic_chunks()` unchanged. |
| `ab-ir` | `AatProjection` gains `ortho_normalizations`. TEI renderer consumes it. |
| `ab-morph-diff` | `Analysis` gains `ortho_annotations: Option<Vec<OrthoAnnotation>>`. |
| `ab-morph-run` | CLI gains `--ortho-detect` flag. Wires detector → normalize → analyzer dispatch. |

## Validation Framework

### Gold data pipeline (Clojure, in `reports/`)

Era/author metadata for stratification comes from the Aozora Bunko catalog
(`list_person_all_extended_utf8.csv`), already consumed by
`aozora-corpus-generator`. For non-Aozora inputs, stratification is limited
to available metadata.

```
1. Candidate extraction
   Python heuristic (aozora-corpus-generator) → candidate katakana sentences
   Stratified across: era, author, katakana ratio, sentence length
   (where metadata is available)

2. Manual annotation (~200–500 sentences)
   Binary label: "normalize? yes/no"
   For "yes" sentences: annotate which type (script, historical-ortho, both)

3. Per-analyzer validation
   For each (sentence, analyzer, dictionary) pair:
     raw_tokens  = analyze(sentence_raw)
     norm_tokens = analyze(sentence_normalized)
     Compare: boundary agreement, OOV rate, POS tag shifts

4. Recall measurement (critical gate)
   Of human-labeled "normalize: yes" sentences, what fraction did the
   heuristic accept? If recall < 0.85, the heuristic is rejecting too
   many legitimate targets and the threshold parameters must be tuned
   before shipping. This is measured BEFORE any consensus mining or ML
   training, since low recall means the gold labels are skewed toward
   sentences the heuristic already handles.

5. Consensus mining
   Run v1 heuristic on validation set → compare with human labels
   Disagreements → manual review → seed for ML training
```

### Self-consistency metrics

For a sentence S and analyzer A, comparing raw vs. normalized output:

```
boundary_agreement  = |raw_boundaries ∩ norm_boundaries| / |raw_boundaries ∪ norm_boundaries|
pos_agreement       = mean(pos(raw_i) == pos(norm_j) for aligned token pairs)
oov_reduction       = oov_rate(raw) - oov_rate(norm)
segmentation_change = 1.0 if boundaries differ else 0.0
```

Expected outcomes:
- **Boundary consistency increases** — fewer erratic splits on particles and
  conjugations.
- **OOV rate decreases** — dictionary recognizes hiragana forms it missed in
  katakana.
- **POS tags shift** — katakana particles incorrectly tagged as nouns get
  correct particle tags.

### Per-analyzer validation report

For each analyzer × dictionary pair:

- Number of sentences where normalization changed output
- Breakdown by change type (boundary, POS, OOV, no change)
- Direction of change on human-annotated subset
- Agreement between analyzers on which sentences benefit

### Comparison integration

`ab-morph-run` produces pairwise `Comparison` structs. Ortho validation adds
a new axis: **(analyzer, raw) vs. (analyzer, normalized)** for the same
analyzer. Reuses the existing `ab-morph-diff` model.

## Phased Plan

### Phase 1: Heuristic v1 (immediate)

- `ab-ortho-detect` crate with `ScriptKatakanaToHiragana` detection + conversion.
- Two-pass heuristic with Vibrato-first-pass OOV/proper-noun guards.
- New `sentence_split()` in `ab-plaintext` (separate from `semantic_chunks`).
- `ab-morph-run --ortho-detect=heuristic` flag.
- `Analysis.ortho_annotations` field with original-coordinate spans.
- No IR/TEI integration yet. `HistoricalToModern` variant exists but no
  detector emits it.

### Phase 2: Gold data and ML (after Phase 1)

- Clojure gold-data pipeline: candidate extraction, annotation workflow.
- Logistic regression classifier trained on gold data.
- **Required ablation:** train and evaluate a character-only variant (no
  Vibrato-derived features). If character-only matches full-feature performance,
  the Vibrato coupling can be removed.
- ML-backed `OrthoDetector` impl behind same trait.
- `ab-morph-run --ortho-detect=ml` flag.
- IR/AAT annotation layer and TEI `<choice>` rendering.
- `model_hash` in `OrthoDetectorId::MlLogisticRegression` is a SHA-256 hex
  digest of serialized model weights for reproducibility.

### Phase 3: Historical orthography (future)

- `HistoricalToModern` detection and normalization.
- Requires dictionary integration or learned model.
- Not scoped for v1/v2.

## Pre-Phase-1 Verification

Before implementing Phase 1, verify the directional assumptions:

1. **Sudachi baseline check.** Run Sudachi (default config, mode C) and Vibrato
   raw on ~10 known katakana-heavy Aozora sentences (from existing test
   fixtures or `data/`). Confirm that Sudachi's `DefaultInputTextPlugin` does
   NOT convert katakana to hiragana — only NFKC/half-width normalization.
   Document the results in a one-page report in `reports/ortho-detect/`.

2. **Recall floor measurement.** On a small pilot set of 20–30 manually
   labeled katakana sentences, measure the v1 heuristic's recall. If recall
   is below 0.85, tune the OOV and proper-noun thresholds before proceeding
   to Phase 1 implementation. Document the tuned parameters.

## Non-goals

- **Speech-span filtering** (speech_mode from aozora-corpus-generator). This is
  corpus subsetting, not text normalization. Changes what is analyzed, not how.
  Out of scope for the morphology comparison pipeline. May be explored as a
  separate `ab-plaintext` filter mode in the future.
- **Automatic era/author dating from orthography.** Detection answers "should
  we normalize this sentence?", not "when was this text written?"
- **Normalizing within ruby/gaiji/notes structural markup.** The detector
  operates on visible-text projection only. Ruby bases and gaiji-resolved text
  in that projection may contain katakana and will be normalized — this is
  intended since the visible text is what analyzers tokenize.

## Resolved Design Decisions

1. **Original text is never mutated.** `PlainTextDocument.text` is always the
   original source. Normalized text is a separate string fed to analyzers.
   `OrthoAnnotation.source_byte_range` bridges both coordinate spaces.

2. **Coordinate invariant.** Morpheme `byte_span` and `char_span` are in
   original-document coordinates. `Analysis.source_text` is the normalized
   text (what the analyzer saw). `Analysis.ortho_annotations` records what
   was changed. `Analysis.ortho_offset_map` provides the coordinate bridge
   from normalized spans to original spans.

3. **Confidence is `Option<u8>` (0–100).** `None` = deterministic heuristic.
   Integer percentage preserves `Eq` on all annotation types.

4. **Sentence splitting is new work in `ab-plaintext`.** `semantic_chunks()`
   is not sentence splitting and remains separate for analyzer chunking.

5. **Dependency direction.** `ab-ortho-detect` depends on `ab-plaintext` (for `SentenceSpan`),
   `ab-morph-analyzers` (for Vibrato first pass), and `ab-morph-diff` (for
   `FeatureMap`). `ab-plaintext` does NOT depend on `ab-ortho-detect`.
   `ab-ir` remains ortho-unaware and independent. `ab-morph-run` depends on
   both `ab-ortho-detect` and `ab-plaintext` and wires them together.

6. **`SentenceSpan` is borrowed (`&'a str`).** Consistent with the existing
   codebase's zero-copy style (`TextChunk<'_>` in `chunking.rs`).

7. **Threshold configuration.** TOML config file with documented defaults.
   CLI flags for ad-hoc override.

8. **Gold data storage.** Versioned in `data/ortho-gold/`. Format: one JSONL
   file per work, each record = `{sentence, label, features, per_analyzer:
   {analyzer_id: {raw_metrics, norm_metrics}}}`.

9. **ML ablation is required, not optional.** Phase 2 plan includes training
   and evaluating a character-only variant. If it matches full-feature
   performance, the Vibrato coupling is removed.
10. **model_hash is a content hash.** `OrthoDetectorId::MlLogisticRegression`
    carries a SHA-256 hex digest of the raw model weight bytes in
    little-endian f32 format (not a debug-format string), ensuring
    reproducibility across serialization formats.

## ADR — Phase 1 implementation reconciles this spec's internal contradiction

**Status:** Accepted (folded into the spec after Phase 1 merged on
2026-07-05). Supersedes the contradictory statements below.

**Contradiction being resolved.** §Architecture says "`ab-ortho-detect`
has **zero workspace dependencies** except the standard library and
`serde`." §Resolved Decision #5 says "`ab-ortho-detect` depends on
`ab-plaintext` (for `SentenceSpan`), `ab-morph-analyzers` (for Vibrato
first pass), and `ab-morph-diff` (for `FeatureMap`)." These cannot both
hold. Additionally, the Phase 1 plan's Task 4 (as specified) would have
created a cyclic package dependency
(`ab-morph-analyzers ← ab-morph-diff ← ab-ortho-detect ← ab-morph-analyzers`).

**Decision.** The §Architecture statement is canonical: `ab-ortho-detect`
is a near-leaf crate. Concretely, the dependency graph is:

```
ab-plaintext  ←  ab-ortho-detect   (SentenceSpan lives in ab-plaintext)
ab-ortho-detect  ←  ab-morph-analyzers  (VibratoAnalyzer: OrthoTokenizer impl)
ab-ortho-detect  ←  ab-morph-diff       (Analysis carries ortho provenance)
ab-ortho-detect  ←  ab-morph-run        (pipeline wiring)
```

`ab-ortho-detect` depends only on `ab-plaintext` (plus `serde`, `regex`).
It does **not** depend on `ab-morph-analyzers` or `ab-morph-diff`. The
Vibrato first-pass coupling is inverted via a trait seam:

- `pub trait OrthoTokenizer: Send + Sync` is defined in `ab-ortho-detect`
  (returns `Vec<OrthoToken { surface, pos2 }>` per `&str`).
- `HeuristicV1::new(tokenizer: Arc<dyn OrthoTokenizer>, config)` takes
  the trait object, not a concrete `VibratoAnalyzer`.
- `impl OrthoTokenizer for VibratoAnalyzer` lives in
  `ab-morph-analyzers/src/ortho_compat.rs` (which already depends on
  `ab-ortho-detect`).

This breaks the cycle and matches the architecture statement. Decision #5
is amended accordingly: the listed workspace deps are the *effective*
runtime edges, achieved via the trait inversion, not direct Cargo edges.

**`SentenceSpan` location.** Lives in `ab-plaintext`, not
`ab-ortho-detect/src/types.rs` as the §Architecture file-map suggested.
This is forced by the leaf decision: the shared type must live in the
crate that does not depend on `ab-ortho-detect`. The §Architecture file
map is amended.

**Consequence for Phase 2 ML ablation.** If the character-only ablation
(spec Decision #9) shows character-only matches full-feature performance,
the `OrthoTokenizer` trait, `ortho_compat.rs`, the double-dictionary-load,
and the `oov_count`/`proper_noun_char_ratio` features can all be
**deleted**. The trait seam exists to make that deletion local.

## Open Questions

1. **Precision delta vs. Python baseline.** The v1 heuristic faithfully ports
   the Python logic, but the Vibrato dictionary differs from MeCab/UniDic as
   used in aozora-corpus-generator. OOV and proper-noun thresholds may need
   recalibration. The gold data pipeline will measure this; initial v1 ships
   with Python-derived defaults.

2. **Catalog metadata ingestion for gold stratification.** The Aozora Bunko
   catalog CSV (`list_person_all_extended_utf8.csv`) is consumed by
   aozora-corpus-generator (aozora.py:626) but is not currently read by
   ab-validator. The gold data Clojure pipeline can either shell out to the
   Python tool for metadata, or ingest the CSV directly. Decision deferred
   to implementation plan.

3. **Historical-orthography dictionary.** For Phase 3: which dictionary format?
   UniDic includes historical readings (`lForm` feature) for some entries.
   Scope (common auxiliary verbs vs. full lexicon) needs definition.

4. **Downstream annotation consumers.** Who reads `Analysis.ortho_annotations`?
   `ab-morph-run` summary reports should surface normalization stats.
   `ab-warehouse` should store them. Schema deferred to implementation plan.
   Gold-data records must include `dict_version` alongside `analyzer_id`
   since UniDic and Sudachi dictionary versions affect per-analyzer metrics.
