# Ortho-Detect Phase 1 — Pre-Phase-1 Baseline Findings

Date: 2026-07-05
Worktree: `.worktrees/ortho-detect-phase1`
Pilot: `data/ortho-pilot/sentences.aat.json` (10 pre-war-style katakana sentences, 117 chars)

## Task 0 Step 2.5 — Vibrato OOV representation (gates Task 4)

Determined by reading vibrato-rkyv source (checkout
`vibrato-rkyv-7c83ed49634f73de/6467251/vibrato/src/`) plus the project's
feature parser (`crates/ab-morph-analyzers/src/features.rs`):

- Vibrato exposes **no `is_unk()` method** on tokens. The OOV/unknown-word
  signal is `token.lex_type() -> LexType`, where
  `enum LexType { System, User, Unknown }`
  (`vibrato/src/dictionary.rs:174`). `LexType::Unknown` marks tokens
  emitted by the unknown-word handler (`unk.def`).
- Unidic-CWJ feature strings use `*` for absent fields, and
  `feature_value("*")` returns `None` in
  `crates/ab-morph-analyzers/src/features.rs`. So an unknown-token row
  whose `pos1` is literally `*` shows up as `features.get("pos1") ==
  Some(&None)` — the plan's "Branch B (pos1 is None)" prediction holds.

**Decision:** follow the plan's Task 4 **Branch B option (b)** — drop the
OOV guard (`oov_count = 0`, `oov_ratio = 0.0` always) and keep only
character-level rejection + the proper-noun guard
(`features.get("pos2") == Some(&Some("固有名詞"))`). This avoids
cross-crate API churn (`RawToken`/`Morpheme`/`Analysis` changes to
propagate `is_unk`) and matches the empirical reality (see below) that
Unidic-CWJ has *dictionary entries* for katakana particles/copulas, so
`LexType::Unknown` rarely fires on this corpus — the OOV signal is not a
useful guard for this task in any case.

## Empirical baseline (raw katakana pilot, 10 sentences, 117 chars)

### Sudachi-C (`--analyzer sudachi-c`, `AB_SUDACHI_DICT` = system_full.dic)

62 morphemes. **Sudachi does not normalize katakana.** Its
`DefaultInputTextPlugin` (NFKC) did not fire on these full-width
katakana particles — they remain katakana and are tagged as *nouns*:

| surface | pos1   | pos2     | count |
|---------|--------|----------|-------|
| ハ      | 名詞   | 普通名詞 | 8     |
| ガ      | 名詞   | 普通名詞 | 1     |
| ダ      | 助動詞 | (none)   | 1     |
| ヲ      | 助詞   | 格助詞   | 1     |
| デアル  | —      | (segmented as one token; not split) | — |

Only `ヲ` is tagged as a particle (`助詞/格助詞`). The copula `ダ` is
correctly tagged `助動詞` but only because Unidic-CWJ has a standalone
entry for katakana `ダ`. The case particle `ハ` is mis-tagged as
`名詞/普通名詞` 8 times — a severe fairness problem the ortho-detect
layer is designed to fix (normalizing `ハ→は` before analysis lets
Sudachi tag it as `助詞/係助詞`).

### Vibrato (`--analyzer vibrato` + `AB_VIBRATO_DICT`, unidic-cwj-202512)

67 morphemes. Handles katakana better than Sudachi but still produces
errors:

- `ハ` tagged `助詞/係助詞` (particle) **6 times**, but `記号/一般`
  (symbol/general) **3 times** — inconsistent.
- `ハマダ` emitted as a **single token** (segmented as
  `名詞/普通名詞`) instead of `ハ` + `マダ` — mis-segmentation.
- `デス` correctly tagged `助動詞` (2 occurrences).
- `ヲ` correctly tagged `助詞/格助詞`.
- `ダ`, `ガ`, `デ` tagged `名詞/普通名詞` (wrong: these are
  copula/particle/noun-of-で).
- **Zero unknown-word (`LexType::Unknown`) tokens** — every morpheme is
  a known dictionary entry, just sometimes mis-segmented/mis-tagged.
  This confirms the OOV guard would be a near-no-op and justifies
  Branch B option (b).

## Decision: proceed with Phase 1

- Task 4's first-pass OOV guard is dropped (Branch B option (b)). The
  proper-noun guard and character-level cascade remain.
- The kata→hira normalization will measurably improve both analyzers'
  particle tagging (Sudachi: `ハ` noun→particle; Vibrato: removes the
  `記号/一般` mis-tags).
- CLI note for Task 8: `analyze-aat` requires `--analyses-output`
  explicitly; `--output-dir` alone errors with
  "implicit default artifact names are no longer provided". The plan's
  `--output-dir`-only commands in Tasks 0/8 must use `--analyses-output`
  instead.

## Post-implementation review — known Phase 1 limitations (deferred per plan)

A Rich-Hickey-style design review after Tasks 1-9 surfaced one severity-Blocker
issue that the plan explicitly defers (dependency graph lists "Task 7.5
char_span fix — depends on Task 7"). Recording it here so it is not lost:

### Coordinate coherence of remapped `Analysis` (deferred to Task 7.5)

`span_builder::remap_spans` rewrites `morpheme.byte_span` to ORIGINAL-text
coordinates but leaves `morpheme.surface` and `morpheme.char_span` in
NORMALIZED-text coordinates. After remap, `analysis.source_text` is the
normalized text while `byte_span` points into the original.

- **Length-preserving normalizations (the common case: ハ→は, ガ→が, …)**:
  original and normalized text have identical byte lengths and byte ranges,
  so `validate_analysis_against_source` (called by `compare_pair`) recomputes
  `surface` and `char_span` consistently and validation passes. Verified
  empirically with a 2-analyzer (`vibrato` + `sudachi-c`) smoke test on the
  pilot: `compare_pair` produced 66 regions, 0 coverage mismatches, 0 errors.
- **Length-changing normalizations (`ヴ→う゛`, rare in pre-war katakana
  prose)**: byte ranges SHIFT, so `source_text[byte_span] != surface` and
  `char_span` diverges → `SurfaceMismatch`/`CharSpanMismatch` errors, or
  `OffsetMap::to_original` PANICS on cross-boundary spans.

The plan's Task 7 comments explicitly accept this: "char_span recalculation
requires the original source text. For Phase 1, keep the normalized-text
char_span as an approximation. Follow-up: rebuild char_map from original text
bytes." The plan's dependency graph names this follow-up "Task 7.5".

**Task 7.5 (follow-up, NOT in this Phase 1 scope):** give `remap_spans` the
original source text and rebuild each morpheme coherently:
`surface = original[remapped_byte_span]`, `char_span =
char_map.char_count_at_byte(...)`, `analysis.source_text = original`. For
sub-token splits crossing a length-changing entry, return an error rather
than panicking. Convert `OffsetMap::to_original` from panic to `Result` so
pipeline failures route to `errors_writer` with a typed stage/code.

### Other review findings (lower severity)

- **Double dictionary load** (Important): the pipeline constructs a fresh
  `VibratoAnalyzer` for detection even when `--analyzer vibrato` is also
  passed. Phase 1 accepts this (plan note). Follow-up: reuse the
  already-loaded analyzer as `Arc<dyn OrthoTokenizer>`.
- **`ab-morph-diff → ab-ortho-detect` edge** (Important): `Analysis` now
  stores typed `OrthoAnnotation`/`OffsetMap`, coupling the core diff model
  to the detector crate. Acceptable for Phase 1; revisit if the diff model
  churns.
- **Dead OOV config** (Strong suggestion): `HeuristicConfig.oov_ratio_threshold`
  and `TokenFeatures.oov_count`/`oov_ratio` are hardcoded to 0/0.0 (Branch B).
  Retained for config parity with the spec; remove if OOV guarding is not
  implemented.
- **`OrthoTokenizer` swallows tokenization errors** (Strong suggestion):
  `ortho_compat.rs` returns `Vec::new()` on `Err`. Consider returning
  `Result` so detection failures are observable.
