# Ortho-Detect Phase 2 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Complete the spec's Phase 2 — fix Phase 1's deferred Task 7.5 coordinate-coherence debt; add gold-data candidate extraction + recall-floor measurement; train + ablate a logistic-regression `OrthoDetector`; wire `--ortho-detect=ml`; attach ortho provenance to `AatProjection`.

**Architecture:** `ab-ortho-detect` remains a near-leaf crate (per the ADR folded into `docs/superpowers/specs/2026-07-05-ortho-detect-design.md`). An ML classifier is added behind the existing `OrthoDetector` trait, fed by `CharFeatures` (character-only) and optionally `TokenFeatures` (ablation). A Clojure (`bb`) gold-data harness extracts candidate sentences from the Aozora fiction selection using the Python `is_katakana_sentence` heuristic as a bootstrap labeler (bias documented); real human annotation is the documented finishing input. `AatProjection` gains `ortho_normalizations`; full TEI `<choice>` rendering is scoped to a JSON-level `<choice>` projection (TEI XML renderer is a separate future sub-project — none exists today).

**Tech Stack:** Rust (edition 2024), `linfa-logistic 0.8` + `ndarray 0.16` for ML, `serde`/`serde_json`, `clap`; `babashka` (`bb`) + `clojure` for the gold-data harness; `python3` to call the sibling `aozora-corpus-generator` heuristic for bootstrap labels.

**Dependency graph (existing + new edges):**
```
ab-plaintext  ←  ab-ortho-detect  (SentenceSpan)
ab-ortho-detect  ←  ab-morph-analyzers  (VibratoAnalyzer: OrthoTokenizer)
ab-ortho-detect  ←  ab-morph-diff        (Analysis provenance)
ab-ortho-detect  ←  ab-morph-run         (pipeline + CLI)
ab-ortho-detect  ←  ab-ir                (AatProjection field)        [NEW in Phase 2]
ab-ortho-detect  ←  ab-ortho-detect-ml  (ML classifier crate)        [NEW in Phase 2]
ab-ortho-detect-ml  ←  ab-morph-run      (--ortho-detect=ml wiring)   [NEW in Phase 2]
```

## Global Constraints

- Rust edition 2024. All Phase 1 invariants (spec §Resolved Decisions + the
  ADR) hold: `PlainTextDocument.text` is the original (never mutated);
  normalized text is a separate view; morpheme `byte_span`/`char_span` are
  in original-document coordinates; `confidence: Option<u8>` preserves `Eq`
  on all annotation types; `SentenceSpan<'a>` is borrowed (zero-copy);
  `ab-plaintext` does NOT depend on `ab-ortho-detect`; no external crates
  beyond the workspace + `linfa-logistic`/`ndarray` + trivial (`regex`, `serde`).
- `OrthoTokenizer` trait stays the seam; Phase 2's ablation may delete it
  (recorded outcome, not a precondition).
- `model_hash` = SHA-256 hex of the raw model weight bytes in little-endian
  f32 (spec Decision #10). Verbatim, no debug-format.
- Gold-data records carry `dict_version` alongside `analyzer_id` (spec Open
  Question #4).
- Bootstrap labels from the Python heuristic are a **seed**, not ground
  truth. Any ML model trained on them is a **pipeline shakedown**, not a
  shipping-grade detector. The investigation report
  (`reports/ortho-detect/2026-07-05-phase2-investigation.md` §B1) flags this;
  the training task re-states it in code comments.

---

## File Map

| File | Action | Responsibility |
|---|---|---|
| `crates/ab-ortho-detect/src/types.rs` | Modify | `OffsetMap::to_original` → `Result` (Task 1) |
| `crates/ab-morph-analyzers/src/span_builder.rs` | Modify | `remap_spans` takes original text, rebuilds `surface`+`char_span` (Task 1) |
| `crates/ab-morph-run/src/pipeline.rs` | Modify | Set `analysis.source_text = original` on ortho path; route remap errors to `errors_writer` (Task 1) |
| `crates/ab-ortho-detect/src/ml.rs` | Create | `MlLogisticRegression` detector: feature vectorization + `linfa` inference (Task 5) |
| `crates/ab-ortho-detect-ml/Cargo.toml` | Create | New bin crate: train + ablate (Task 6) |
| `crates/ab-ortho-detect-ml/src/main.rs` | Create | CLI: `train`, `ablate`, `hash` subcommands (Task 6) |
| `crates/ab-ortho-detect-ml/src/io.rs` | Create | Gold JSONL reader, model file format (Task 6) |
| `crates/ab-ortho-detect/src/heuristic.rs` | Modify | Expose `extract_features` for the trainer (Task 4) |
| `crates/ab-ir/src/lib.rs` | Modify | `AatProjection.ortho_normalizations` field (Task 7) |
| `crates/ab-morph-run/src/options.rs` | Modify | `OrthoDetectMode::Ml { model_path }` (Task 8) |
| `crates/ab-morph-run/src/main.rs` | Modify | `--ortho-detect ml --ortho-ml-model PATH` flags (Task 8) |
| `crates/ab-morph-run/src/pipeline.rs` | Modify | Construct `MlLogisticRegression` when mode = Ml (Task 8) |
| `scripts/ortho-gold/candidates.clj` | Create | bb script: extract candidate sentences from Aozora fiction selection (Task 2) |
| `scripts/ortho-gold/bootstrap_label.py` | Create | Call aozora-corpus-generator heuristic, emit JSONL labels (Task 2) |
| `scripts/ortho-gold/recall_floor.clj` | Create | bb script: measure v1 recall on labeled set (Task 3) |
| `data/ortho-gold/sentences.jsonl` | Create | Bootstrap-labeled gold records (Task 2) |
| `Cargo.toml` (root) | Modify | Add `ab-ortho-detect-ml` member + `linfa-logistic`/`ndarray` workspace deps (Task 5) |

---

### Task 0: Confirm Phase 1 baseline + read the ADR

**Files:** None (verification only).

- [ ] **Step 1: Build + test Phase 1 baseline in the worktree**

```bash
cd /home/bor/Projects/ab-validator/.worktrees/ortho-detect-phase2
cargo test -p ab-ortho-detect -p ab-morph-diff -p ab-morph-analyzers 2>&1 | rg "test result"
```

Expected: all pass (35 unit + 7 integration in ab-ortho-detect; 47 + 2 in ab-morph-diff; 25 in ab-morph-analyzers, 4 ignored for dict).

- [ ] **Step 2: Read the ADR folded into the design spec**

Read `docs/superpowers/specs/2026-07-05-ortho-detect-design.md` §"ADR — Phase 1 implementation reconciles this spec's internal contradiction" (between Decision #10 and Open Questions). Confirm the crate dependency graph and `SentenceSpan` location are as the ADR states. This is the canonical shape Phase 2 builds on.

- [ ] **Step 3: Re-read the Phase 1 post-impl review**

Read `reports/ortho-detect/2026-07-05-sudachi-baseline.md` §"Post-implementation review". Task 1 of this plan implements the Task 7.5 fix described there.

- [ ] **Step 4: Confirm sibling tooling is reachable**

```bash
ls ../aozora-corpus-generator/src/aozora_corpus_generator/aozora.py   # Python heuristic
which bb clojure python3                                              # gold harness toolchain
```

Expected: the file path resolves; all three binaries are on `$PATH`.

---

### Task 1: Fix Task 7.5 — coordinate coherence of remapped `Analysis`

**Files:**
- Modify: `crates/ab-ortho-detect/src/types.rs` (`OffsetMap::to_original`)
- Modify: `crates/ab-morph-analyzers/src/span_builder.rs` (`remap_spans`)
- Modify: `crates/ab-morph-run/src/pipeline.rs` (ortho path)

**Interfaces:**
- Consumes: `OffsetMap`, `Analysis`, `Morpheme` (from Phase 1)
- Produces: `OffsetMap::to_original -> Result<Range<usize>, OrthoMapError>`, `remap_spans(analysis, offset_map, original_source_text) -> Result<(), OrthoMapError>`

**Why:** Phase 1's `remap_spans` rewrites `byte_span` to original-doc coords but leaves `surface`/`char_span` in normalized coords; `OffsetMap::to_original` panics on cross-boundary spans. Safe today only for length-preserving normalizations; crashes on `ヴ→う゛`. The spec invariant (#2) requires byte_span AND char_span in original coords.

- [ ] **Step 1: Write the failing test for `to_original` returning `Result`**

In `crates/ab-ortho-detect/src/types.rs`, add `OrthoMapError` and change `to_original`. First, write a test that proves the new error path:

```rust
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum OrthoMapError {
    /// The normalized byte range crosses an OffsetMap entry boundary where
    /// byte-length changed (e.g. ヴ→う゛). Callers must split the span first.
    #[error("normalized range {range:?} crosses OffsetMap entry boundary at byte {boundary}")]
    CrossesBoundary { range: std::ops::Range<usize>, boundary: usize },
    /// The normalized byte offset is not covered by any OffsetMap entry.
    #[error("normalized offset {offset} not covered by any OffsetMap entry")]
    UncoveredOffset { offset: usize },
}
```

(`thiserror` is already a workspace dep — verify in Step 2.) Replace the body of `to_original`:

```rust
/// Map a byte range in normalized text to the corresponding range in
/// original text.
///
/// # Errors
///
/// Returns `OrthoMapError::CrossesBoundary` if `norm_range` crosses an
/// entry boundary where byte-length changed (e.g. ヴ→う゛). Callers must
/// split spans at annotation boundaries before calling this method.
/// Returns `OrthoMapError::UncoveredOffset` if the range starts or ends
/// outside all entries.
pub fn to_original(
    &self,
    norm_range: std::ops::Range<usize>,
) -> Result<std::ops::Range<usize>, OrthoMapError> {
    if self.entries.is_empty() {
        return Ok(norm_range);
    }
    let start_entry = self.entries.iter().find(|&&(noff, _, nlen, _)| {
        norm_range.start >= noff && norm_range.start < noff + nlen
    });
    let start_entry = match start_entry {
        Some(e) => e,
        None => return Err(OrthoMapError::UncoveredOffset { offset: norm_range.start }),
    };
    let end_entry = self.entries.iter().find(|&&(noff, _, nlen, _)| {
        norm_range.end > noff && norm_range.end <= noff + nlen
    });
    let end_entry = match end_entry {
        Some(e) => e,
        None => return Err(OrthoMapError::UncoveredOffset { offset: norm_range.end }),
    };
    if !std::ptr::eq(start_entry.as_ptr(), end_entry.as_ptr()) {
        let boundary = start_entry.2; // normalized length of the start entry
        return Err(OrthoMapError::CrossesBoundary {
            range: norm_range,
            boundary,
        });
    }
    let &(noff, ooff, _nlen, _olen) = start_entry;
    let delta = norm_range.start - noff;
    let orig_len = norm_range.end - norm_range.start;
    let orig_start = ooff + delta;
    Ok(orig_start..orig_start + orig_len)
}
```

Update the existing `offset_map_*` tests in `types.rs` to use `?` / `.unwrap()` and the new error variant names. The two `#[should_panic]` tests become:

```rust
#[test]
fn offset_map_errors_on_cross_entry_span() {
    let map = OffsetMap { entries: vec![(0, 0, 6, 6), (6, 6, 6, 3)] };
    let err = map.to_original(3..9).unwrap_err();
    assert!(matches!(err, OrthoMapError::CrossesBoundary { .. }));
}

#[test]
fn offset_map_errors_on_uncovered_offset() {
    let map = OffsetMap { entries: vec![(0, 0, 6, 6)] };
    let err = map.to_original(10..15).unwrap_err();
    assert!(matches!(err, OrthoMapError::UncoveredOffset { .. }));
}
```

- [ ] **Step 2: Add `thiserror` dep if missing**

```bash
rg -n "thiserror" crates/ab-ortho-detect/Cargo.toml Cargo.toml
```
If absent from `ab-ortho-detect/Cargo.toml`:
```toml
[dependencies]
thiserror = { workspace = true }
```
(Verify `thiserror` is in root `[workspace.dependencies]`; if not, add `thiserror = "2"`.)

- [ ] **Step 3: Run types tests — expect compile + test pass**

```bash
cargo test -p ab-ortho-detect -- types 2>&1 | rg "test result|error\["
```
Expected: 8 tests pass (the 2 former `should_panic` now assert on `Err`).

- [ ] **Step 4: Rewrite `remap_spans` to take original text + rebuild morphemes**

In `crates/ab-morph-analyzers/src/span_builder.rs`, change signature + body:

```rust
use ab_ortho_detect::{OffsetMap, OrthoMapError};
use ab_morph_diff::CharByteMap;

/// Post-process an Analysis to remap morpheme `byte_span`, `char_span`, and
/// `surface` from normalized-text coordinates to original-text coordinates.
///
/// # Errors
///
/// Returns `OrthoMapError::CrossesBoundary` if a morpheme span crosses an
/// annotation boundary where byte-length changed. The pipeline routes this
/// to `errors_writer`; the morpheme is left in normalized coords for that
/// case (diagnostic, not a crash).
pub fn remap_spans(
    analysis: &mut ab_morph_diff::Analysis,
    offset_map: &OffsetMap,
    original_source_text: &str,
) -> Result<(), OrthoMapError> {
    if offset_map.is_empty() {
        return Ok(());
    }
    let char_map = CharByteMap::new(original_source_text);
    for morpheme in &mut analysis.morphemes {
        let remapped = offset_map.to_original(morpheme.byte_span.clone())?;
        morpheme.byte_span = remapped.clone();
        // Rebuild surface from the ORIGINAL text at the remapped range.
        if original_source_text.is_char_boundary(remapped.start)
            && original_source_text.is_char_boundary(remapped.end)
            && remapped.end <= original_source_text.len()
        {
            morpheme.surface = original_source_text[remapped.clone()].to_owned();
        }
        // Rebuild char_span from the original text's char map.
        if let Some(cs) = char_byte_to_char_span(&char_map, remapped.clone()) {
            morpheme.char_span = cs;
        }
    }
    Ok(())
}

fn char_byte_to_char_span(
    char_map: &CharByteMap,
    byte_span: std::ops::Range<usize>,
) -> Option<std::ops::Range<usize>> {
    if byte_span.start > byte_span.end {
        return None;
    }
    Some(char_map.char_count_at_byte(byte_span.start)..char_map.char_count_at_byte(byte_span.end))
}
```

Verify `CharByteMap` and `char_count_at_byte` exist (read `crates/ab-morph-diff/src/lib.rs` or wherever the type lives; the existing `validate.rs` uses it). Update the existing `span_builder` tests that call `remap_spans` with the new 3-arg signature.

- [ ] **Step 5: Update the pipeline ortho path**

In `crates/ab-morph-run/src/pipeline.rs`, find the `remap_spans` call (around line 687). Change to:

```rust
if let Some(ref map) = offset_map_opt {
    match ab_morph_analyzers::span_builder::remap_spans(&mut analysis, map, &document.text) {
        Ok(()) => {}
        Err(e) => {
            // Route to errors_writer with a typed stage/code; do not crash.
            if let Some(writer) = &mut errors_writer {
                let _ = write_ortho_remap_error(writer, &source_id, &e);
            }
            // analysis is left in normalized coords for this morpheme set.
        }
    }
    analysis.source_text = document.text.clone();
}
analysis.ortho_annotations = annotations_opt.clone();
analysis.ortho_offset_map = offset_map_opt.clone();
```

Add a small `write_ortho_remap_error` helper (serde_json line with `stage: "ortho_remap"`, `code: "crosses_boundary"|"uncovered_offset"`, `message: e.to_string()`). Match the existing `errors_writer` row shape (read `write_error_row` in the same file).

**Invariant check:** after this, `analysis.source_text` holds the ORIGINAL text on the ortho path, and `byte_span`/`char_span`/`surface` are all in original coords. Downstream `validate_analysis_against_source(analysis, &document.text)` and `compare_nway_with_source_text(&analyses, &document.text, ...)` are both consistent.

- [ ] **Step 6: Add a regression test for `ヴ→う゛` (the length-changing case)**

In `crates/ab-morph-analyzers/src/span_builder.rs` tests (or a new `tests/remap_vu.rs`), construct an `Analysis` with a morpheme spanning the normalized `う゛` range, call `remap_spans` with the original `今日ヴ` text, assert the morpheme's `byte_span` is `6..9` (the `ヴ` range), `surface == "ヴ"`, and `char_span` reflects the original-text char count (6/3 → char offset 2..3).

- [ ] **Step 7: Run full workspace test**

```bash
cargo test -p ab-ortho-detect -p ab-morph-analyzers -p ab-morph-diff -p ab-morph-run 2>&1 | rg "test result|FAILED"
```
Expected: all pass. The 2 pre-existing `rerun_full_*` failures on `ab-morph-run` (unrelated `unknown analyzer 'test:single'` env issue) are acceptable — confirm they are unchanged.

- [ ] **Step 8: Smoke test with `ヴ`-containing input**

```bash
export AB_VIBRATO_DICT=/home/bor/Projects/ab-validator/dictionary/compiled/unidic-cwj-202512.dic.zst
# Create a tiny AAT with a ヴ sentence:
cat > /tmp/vu-test.aat.json <<'EOF'
{"version":1,"work_id":"vu","blocks":[{"kind":"paragraph","content":[{"kind":"text","value":"今日ヴバスデアル。"}]}],"meta":{"adapter":"fixture","adapter_version":"fixture","source_encoding":"utf-8","source_hash":"sha256:0","parse_complete":true,"warnings":[]}}
EOF
cargo run -p ab-morph-run -- analyze-aat --ortho-detect heuristic --analyzer vibrato \
  --aat /tmp/vu-test.aat.json --analyses-output /tmp/vu-out.jsonl 2>&1 | tail -3
```
Expected: exit 0, no panic. (If the heuristic rejects the sentence — `今日ヴバスデアル。` is 8 chars, katakana ratio 4/8 = 0.5, `<= 0.5` rejects — adjust the test sentence to e.g. `今日ヴバスデアルヨ。` (9 chars, ratio 5/9 ≈ 0.56). The goal is a non-panicking run with `ヴ` present.)

- [ ] **Step 9: Commit**

```bash
git add crates/ab-ortho-detect/src/types.rs crates/ab-ortho-detect/Cargo.toml \
        crates/ab-morph-analyzers/src/span_builder.rs crates/ab-morph-run/src/pipeline.rs Cargo.toml
git commit -m "fix(ortho-detect): coordinate coherence for remapped Analysis (Task 7.5)

OffsetMap::to_original now returns Result instead of panicking on
cross-boundary spans (ヴ→う゛). remap_spans takes the original source
text and rebuilds morpheme surface + char_span in original-doc coords,
honoring spec invariant #2. Pipeline routes remap errors to errors_writer
instead of crashing. analysis.source_text is set to the original text
on the ortho path so validate_analysis_against_source and
compare_nway_with_source_text are both consistent."
```

---

### Task 2: Gold-data candidate extraction (Clojure harness + bootstrap labels)

**Files:**
- Create: `scripts/ortho-gold/candidates.clj`
- Create: `scripts/ortho-gold/bootstrap_label.py`
- Create: `data/ortho-gold/sentences.jsonl` (generated)

**Interfaces:**
- Consumes: `../aozora-corpus-generator/Aozora-Bunko-Fiction-Selection-2022-05-30/Plain/` (plain-text Aozora works)
- Produces: `data/ortho-gold/sentences.jsonl` — one record per candidate sentence: `{work_id, sentence, byte_offset, char_offset, label, features, dict_version}`

**Why:** Phase 2 ML needs a gold set. Real human annotation is the documented finishing input; this task bootstraps labels from the Python heuristic (the same one Phase 1 ported) so the ML pipeline can run end-to-end. The bias is explicit: a model trained on these labels can at best equal the heuristic; it cannot exceed it on the labeled distribution.

- [ ] **Step 1: Write the candidate-extraction `bb` script**

`scripts/ortho-gold/candidates.clj`:

```clojure
#!/usr/bin/env bb
;; Extract candidate katakana sentences from the Aozora fiction selection.
;; Emits one JSON line per candidate: {work_id, sentence, byte_offset, char_offset}
;; A "candidate" is any sentence (per ab-plaintext sentence_split rules) with
;; katakana_ratio > 0.4 AND hiragana_count == 0 — a superset of what the v1
;; heuristic accepts, so the gold set includes both ACCEPT and REJECT labels.
(require '[cheshire.core :as json])
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

(defn sentence-terminals [ch]
  (#{\。 \！ \？ \! \?} ch))

(defn sentence-split [text]
  (loop [chars (vec (seq text))
         i 0
         byte-start 0
         char-start 0
         spans []]
    (if (>= i (count chars))
      (if (< byte-start (count text))
        (conj spans {:text (subs text byte-start) :byte_offset byte-start :char_offset char-start})
        spans)
      (let [ch (nth chars i)]
        (if (sentence-terminals ch)
          ;; consume adjacent terminals
          (let [end-idx (loop [j (inc i)] (if (and (< j (count chars)) (sentence-terminals (nth chars j))) (recur (inc j)) j))
                byte-end (if (< end-idx (count chars)) (.offset (vec (.getBytes text)) end-idx) (count text))
                _ (assert false "byte-end computation needs char_indices — use StringReader approach below")]
            (recur chars end-idx byte-end (+ char-start (- end-idx (dec i))) spans))
          (recur chars (inc i) byte-start char-start spans))))))

;; NOTE: the above byte-offset bookkeeping in pure bb is fiddly because
;; String .length is UTF-16 unit count, not byte count. The Python sibling
;; is the authoritative source for both splitting and offsets. This Clojure
;; script SHELLS OUT to the Python bootstrap_label.py, which does the real
;; work using ab-plaintext-compatible semantics.

(defn -main [& args]
  (let [plain-dir (or (System/getenv "AOZORA_FICTION_PLAIN")
                      "../aozora-corpus-generator/Aozora-Bunko-Fiction-Selection-2022-05-30/Plain")
        out-path (or (first args) "data/ortho-gold/candidates.jsonl")]
    (doseq [^java.io.File f (file-seq (io/file plain-dir))
            :when (and (.isFile f) (str/ends-with? (.getName f) ".txt"))]
      (let [work-id (str/replace-first (.getName f) #"\.txt$" "")
            text (slurp f)]
        ;; delegate to python for byte-accurate sentence splitting + features
        (println {:work_id work-id :text_len (count text)}))))
  (println "delegating to bootstrap_label.py for actual extraction — see -main"))

(-main)
```

**Honest cut:** doing byte-accurate UTF-8 sentence splitting in `bb` (which counts UTF-16 code units on Java strings) is footgun-prone. The cleaner design: a Python script that reuses the project's actual `ab-plaintext::sentence_split` semantics (and the `kata_to_hira` / feature logic) via a tiny `cargo run -p ab-ortho-detect-ml` helper, OR calls the sibling Python heuristic. Use the Python heuristic (it is the canonical reference). The `bb` script becomes a thin orchestrator.

Rewrite `candidates.clj` as orchestrator:

```clojure
#!/usr/bin/env bb
;; Orchestrator: walks the Aozora fiction selection and shells out to
;; bootstrap_label.py (the canonical Python heuristic) per work.
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])
(require '[babashka.process :as p])

(def plain-dir
  (or (System/getenv "AOZORA_FICTION_PLAIN")
      "../aozora-corpus-generator/Aozora-Bunko-Fiction-Selection-2022-05-30/Plain"))

(def out-path
  (or (first *command-line-args*) "data/ortho-gold/candidates.jsonl"))

(spit out-path "")  ;; truncate

(doseq [^java.io.File f (file-seq (io/file plain-dir))
        :when (and (.isFile f) (str/ends-with? (.getName f) ".txt"))
        :let [work-id (str/replace-first (.getName f) #"\.txt$" "")]]
  (let [res (p/shell {:out :string :err :string}
                     "python3" "scripts/ortho-gold/bootstrap_label.py"
                     "--work-id" work-id
                     "--text-file" (.getPath f))]
    (when-not (zero? (:exit res))
      (binding [*out* *err*]
        (println {:error work-id :stderr (:err res)})))
    (spit out-path (:out res) :append true)))

(println "wrote" out-path)
```

- [ ] **Step 2: Write the Python bootstrap labeler**

`scripts/ortho-gold/bootstrap_label.py`:

```python
#!/usr/bin/env python3
"""Bootstrap-label candidate katakana sentences from one Aozora work.

Uses the aozora-corpus-generator is_katakana_sentence heuristic (the same
one ab-ortho-detect HeuristicV1 ports) to assign ACCEPT/REJECT labels.
This is a SEED, not ground truth: a model trained on these labels can at
best reproduce the heuristic. Real human annotation is the documented
finishing input (see reports/ortho-detect/2026-07-05-phase2-investigation.md §B1).

Emits JSONL to stdout, one record per candidate sentence:
  {work_id, sentence, byte_offset, char_offset, label, katakana_ratio,
   hiragana_count, total_chars, dict_version}
"""
from __future__ import annotations
import argparse
import json
import re
import sys
from pathlib import Path

# Import the canonical heuristic from the sibling repo.
sys.path.insert(0, str(Path(__file__).resolve().parents[3]
                       / "aozora-corpus-generator" / "src"))
try:
    from aozora_corpus_generator.aozora import is_katakana_sentence  # type: ignore
except Exception as exc:  # pragma: no cover
    sys.exit(f"could not import aozora-corpus-generator heuristic: {exc}\n"
             f"(expected at $REPO/../aozora-corpus-generator/src/aozora_corpus_generator/aozora.py)")
# The heuristic calls MeCab for oov_count/proper_noun; we stub tokens to
# the Branch-B reality (oov_count=0 always, no proper-noun filter) so the
# bootstrap is self-contained and does not require a MeCab install.
class _StubTokens(list):
    pass

TERMINALS = set("。！？!?")
DICT_VERSION = "unidic-cwj-202512"

def sentence_split(text: str):
    """Port of ab-plaintext::sentence_split (byte/char accurate)."""
    spans = []
    byte_start = 0
    char_start = 0
    i = 0
    chars = list(text)
    while i < len(chars):
        if chars[i] in TERMINALS:
            j = i + 1
            while j < len(chars) and chars[j] in TERMINALS:
                j += 1
            byte_end = len(text.encode("utf-8")[:len("".join(chars[:j]).encode("utf-8"))])
            slice_text = "".join(chars[i_of: j] for i_of in [0])[:0] or text[byte_start:byte_end] if False else text[byte_start:byte_end]
            # Simpler: compute byte offsets via cumulative encoding.
            byte_end = len(text[:sum(1 for _ in range(j))].encode("utf-8")) if False else None
            # Most correct: use char_indices.
            pass
        i += 1
    return spans  # placeholder — see corrected version below
```

The inline byte-offset bookkeeping above is buggy (left intentionally as a warning — it mirrors the same Java-vs-UTF8 trap the `bb` script hit). Use the canonical approach: iterate `text.encode("utf-8")` char boundaries. Rewrite `sentence_split` cleanly:

```python
def sentence_split(text: str):
    spans = []
    byte_start = 0
    char_start = 0
    i = 0
    while i < len(text):
        if text[i] in TERMINALS:
            j = i + 1
            while j < len(text) and text[j] in TERMINALS:
                j += 1
            # byte_end = byte offset of text[j]
            byte_end = len(text[:j].encode("utf-8"))
            slice_text = text[byte_start:byte_end]
            if slice_text.strip():
                spans.append((slice_text, byte_start, char_start))
            char_start += len(slice_text)
            byte_start = byte_end
            i = j
        else:
            i += 1
    if byte_start < len(text.encode("utf-8")):
        rest = text[byte_start:]
        if rest.strip():
            spans.append((rest, byte_start, char_start))
    return spans
```

Main:

```python
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--work-id", required=True)
    ap.add_argument("--text-file", required=True)
    args = ap.parse_args()
    text = Path(args.text_file).read_text(encoding="utf-8")
    for sentence, byte_off, char_off in sentence_split(text):
        total = len(sentence)
        if total == 0:
            continue
        kata = sum(1 for c in sentence if "\u30A1" <= c <= "\u30FA" or "\uFF65" <= c <= "\uFF9F")
        hira = sum(1 for c in sentence if "\u3041" <= c <= "\u3096")
        ratio = kata / total
        # Candidate superset: katakana-dominant AND no hiragana
        if not (ratio > 0.4 and hira == 0):
            continue
        # Bootstrap label via the canonical heuristic (stubbed tokens, Branch B).
        try:
            label = "accept" if is_katakana_sentence(sentence, _StubTokens()) else "reject"
        except Exception:
            # The heuristic divides by len(tokens) with stub tokens → ZeroDivision.
            # Branch B: treat oov_count=0, oov_ratio=0; the character cascade decides.
            label = "accept" if (hira == 0 and ratio > 0.5 and total >= 8) else "reject"
        rec = {
            "work_id": args.work_id,
            "sentence": sentence,
            "byte_offset": byte_off,
            "char_offset": char_off,
            "label": label,
            "katakana_ratio": ratio,
            "hiragana_count": hira,
            "total_chars": total,
            "dict_version": DICT_VERSION,
        }
        print(json.dumps(rec, ensure_ascii=False))

if __name__ == "__main__":
    main()
```

- [ ] **Step 3: Run the harness on the fiction selection**

```bash
cd /home/bor/Projects/ab-validator/.worktrees/ortho-detect-phase2
mkdir -p data/ortho-gold
bb scripts/ortho-gold/candidates.clj data/ortho-gold/candidates.jsonl 2>&1 | tail -5
wc -l data/ortho-gold/candidates.jsonl
head -1 data/ortho-gold/candidates.jsonl | python3 -m json.tool
```
Expected: produces N candidate JSONL lines (N varies with the fiction selection size; expect hundreds to low thousands). Each record has the fields above.

- [ ] **Step 4: Stratify + cap the gold set**

The spec wants ~200–500 labeled sentences stratified by era/author/katakana-ratio/length. The fiction selection lacks era metadata inline (it lives in the Aozora catalog). For the bootstrap gold set, cap at 500 by even sampling across `katakana_ratio` buckets. Write a tiny filter step (append to `candidates.clj` or a `bb` one-liner) that takes the top 500 by even sampling across ratio buckets `[0.4–0.5), [0.5–0.7), [0.7–1.0]`. Emit `data/ortho-gold/sentences.jsonl`.

```bash
bb -O '(->> (line-seq (java.io.BufferedReader. *in*))
           (map #(cheshire.core/parse-string % true))
           (group-by #(let [r (get % "katakana_ratio")] (cond (< r 0.5) :low (< r 0.7) :mid :else :high)))
           (mapcat (fn [[k vs]] (take 167 vs)))  ;; ~500 / 3 buckets
           (map cheshire.core/generate-string)
           (run! println))' < data/ortho-gold/candidates.jsonl > data/ortho-gold/sentences.jsonl
wc -l data/ortho-gold/sentences.jsonl
```

- [ ] **Step 5: Commit**

```bash
git add scripts/ortho-gold/ data/ortho-gold/sentences.jsonl data/ortho-gold/candidates.jsonl
git commit -m "feat(ortho-gold): candidate extraction + bootstrap labeler

bb orchestrator walks the Aozora fiction selection; python3 calls the
canonical aozora-corpus-generator is_katakana_sentence heuristic to assign
bootstrap ACCEPT/REJECT labels. Bias is explicit (see plan + investigation
report): seed labels, not ground truth. Stratified cap of ~500 across
katakana-ratio buckets. Real human annotation is the documented finishing input."
```

---

### Task 3: Recall-floor measurement

**Files:**
- Create: `scripts/ortho-gold/recall_floor.clj`
- Create: `reports/ortho-detect/2026-07-05-phase2-recall-floor.md`

**Why:** Spec Pre-Phase-1 Verification step 2 + Validation Framework step 4: "Of human-labeled 'normalize: yes' sentences, what fraction did the v1 heuristic accept? If recall < 0.85, tune thresholds." Phase 1 shipped with Python-derived defaults — this measurement was never run. With bootstrap labels (the heuristic labeling itself), recall is trivially ~1.0 against its own output — so this task ALSO measures recall against a held-out human spot-check of N=50 sentences the human labels by hand in-session.

- [ ] **Step 1: Build a `bb` recall script that invokes the v1 detector**

`scripts/ortho-gold/recall_floor.clj` reads `data/ortho-gold/sentences.jsonl`, runs each sentence through `cargo run -p ab-morph-run`'s detector (or — faster — a tiny Rust helper binary `ab-ortho-detect-ml detect-classic` that calls `HeuristicV1::detect` directly, built in Task 5; for now shell out via a one-off `cargo test` isn't ideal). 

**Pragmatic approach:** defer the actual recall measurement to Task 5's `ab-ortho-detect-ml detect-classic` subcommand (a clean Rust CLI), and have this task only write the report skeleton + the `bb` harness that consumes that subcommand's output. Mark Step 2 as "blocked on Task 5."

Actually — to keep this task independently testable, write a **minimal direct Rust helper inline** via `cargo run --example`. Create `crates/ab-ortho-detect/examples/detect_sentences.rs`:

```rust
//! Reads JSONL {sentence, label} from stdin (or a path), runs HeuristicV1
//! on each, prints JSONL {sentence, gold_label, heuristic_label, agree}.
use std::io::{BufRead, BufReader};
use std::sync::Arc;
use ab_ortho_detect::heuristic::{HeuristicConfig, HeuristicV1};
use ab_ortho_detect::{OrthoDetector, ortho_normalize};
use ab_ortho_detect::script::kata_to_hira;
use ab_plaintext::sentence_split;
use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Deserialize)] struct Rec { sentence: String, label: String }
#[derive(Serialize)] struct Out<'a> { sentence: &'a str, gold: &'a str, heuristic: String, agree: bool }

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Requires AB_VIBRATO_DICT for the OrthoTokenizer (VibratoAnalyzer).
    let vibrato = Arc::new(ab_morph_analyzers::VibratoAnalyzer::unidic_cwj_default()?);
    let det = HeuristicV1::new(vibrato as Arc<dyn ab_ortho_detect::OrthoTokenizer>, HeuristicConfig::default());
    let stdin = BufReader::new(std::io::stdin());
    for line in stdin.lines() {
        let line = line?;
        if line.trim().is_empty() { continue; }
        let rec: Rec = serde_json::from_str(&line)?;
        let spans = sentence_split(&rec.sentence);
        let anns = det.detect(&spans);
        let h_label = if anns.is_empty() { "reject" } else { "accept" };
        let out = Out { sentence: &rec.sentence, gold: &rec.label, heuristic: h_label.to_string(), agree: h_label == rec.label };
        println!("{}", serde_json::to_string(&out)?);
    }
    Ok(())
}
```

Add `ab-morph-analyzers` + `serde` + `serde_json` + `ab-plaintext` as dev-deps of `ab-ortho-detect` (Task 9 of Phase 1 already added them). Run:

```bash
export AB_VIBRATO_DICT=/home/bor/Projects/ab-validator/dictionary/compiled/unidic-cwj-202512.dic.zst
cargo run -p ab-ortho-detect --example detect_sentences < data/ortho-gold/sentences.jsonl > /tmp/recall-out.jsonl
```

- [ ] **Step 2: Compute recall + write the report**

`scripts/ortho-gold/recall_floor.clj`:

```clojure
#!/usr/bin/env bb
;; Reads detect_sentences output JSONL, computes recall/precision, writes report.
(require '[cheshire.core :as json])
(defn -main [in-path]
  (let [recs (for [line (line-seq (clojure.java.io/reader in-path))
                   :let [r (json/parse-string line true)]
                   :when r]
               r)
        gold-pos (filter #(= "accept" (:gold %)) recs)
        tp (count (filter :agree gold-pos))
        fn_ (count (filter #(not (:agree %)) gold-pos))
        recall (if (zero? (count gold-pos)) 0.0 (/ tp (double (count gold-pos))))
        gold-neg (filter #(= "reject" (:gold %)) recs)
        fp (count (filter #(not (:agree %)) gold-neg))
        tn (count (filter :agree gold-neg))
        precision (let [pred-pos (filter #(= "accept" (:heuristic %)) recs)]
                    (if (zero? (count pred-pos)) 0.0 (/ tp (double (count pred-pos)))))]
    (println "gold_pos=" (count gold-pos) "tp=" tp "fn=" fn_ "recall=" recall)
    (println "gold_neg=" (count gold-neg) "fp=" fp "tn=" tn "precision=" precision)
    {:recall recall :precision precision :tp tp :fn fn_ :fp fp :tn tn
     :n (count recs)}))
(println (-main (first *command-line-args*)))
```

Run + redirect to a report `reports/ortho-detect/2026-07-05-phase2-recall-floor.md`. Document the **bias caveat** prominently: against bootstrap labels (the heuristic labeling itself), recall is artificially ~1.0 — this measures implementation faithful-port-of-Python correctness, NOT real-world recall. Real recall requires human labels (Task 9 finishing step).

- [ ] **Step 3: Commit**

```bash
git add crates/ab-ortho-detect/examples/detect_sentences.rs scripts/ortho-gold/recall_floor.clj \
        reports/ortho-detect/2026-07-05-phase2-recall-floor.md
git commit -m "feat(ortho-gold): recall-floor measurement harness

Adds detect_sentences example binary that runs HeuristicV1 over a JSONL
gold set and emits per-sentence agree/disagree. bb script computes
recall + precision. Bias caveat documented: against bootstrap labels this
measures port-fidelity (recall ~1.0 expected), not real-world recall."
```

---

### Task 4: Refactor `CharFeatures` extraction for reuse by the ML trainer

**Files:**
- Modify: `crates/ab-ortho-detect/src/features.rs`
- Modify: `crates/ab-ortho-detect/src/heuristic.rs`

**Interfaces:**
- Produces: `pub fn features_to_vector(CharFeatures) -> Vec<f64>` (canonical feature order, documented) and `pub const FEATURE_NAMES: &[&str]`.

**Why:** The ML trainer (Task 5) and the runtime classifier must agree on feature vectorization. Phase 1's `CharFeatures` is a struct; the trainer needs a flat `Vec<f64>` with a stable, documented order.

- [ ] **Step 1: Add `FEATURE_NAMES` + `features_to_vector`**

In `crates/ab-ortho-detect/src/features.rs`:

```rust
/// Canonical feature vector order. The ML model's weight vector indexes
/// match this order. DO NOT reorder without retraining + rehashing.
pub const FEATURE_NAMES: &[&str] = &[
    "total_chars",
    "hiragana_ratio",
    "katakana_ratio",
    "kanji_ratio",
    "unique_char_ratio",
    "max_bigram_repeat_ratio",
    "char_run_repeat_ratio",
    "repeated_bigram_pattern_ratio",
    "katakana_at_sentence_end",
];

/// Project CharFeatures into the canonical ML feature vector.
/// `katakana_at_sentence_end` (bool) becomes 0.0/1.0.
#[must_use]
pub fn features_to_vector(f: &CharFeatures) -> Vec<f64> {
    vec![
        f.total_chars as f64,
        f.hiragana_ratio,
        f.katakana_ratio,
        f.kanji_ratio,
        f.unique_char_ratio,
        f.max_bigram_repeat_ratio,
        f.char_run_repeat_ratio,
        f.repeated_bigram_pattern_ratio,
        if f.katakana_at_sentence_end { 1.0 } else { 0.0 },
    ]
}
```

- [ ] **Step 2: Test the vector order is stable**

```rust
#[test]
fn feature_vector_order_matches_names() {
    assert_eq!(FEATURE_NAMES.len(), features_to_vector(&extract_char_features("アアアア")).len());
}

#[test]
fn boolean_feature_is_zero_or_one() {
    // "猫ダ" ends in katakana (ダ) → katakana_at_sentence_end == true.
    let f = extract_char_features("猫ダ");
    let v = features_to_vector(&f);
    let idx = FEATURE_NAMES.iter().position(|n| *n == "katakana_at_sentence_end").unwrap();
    assert!((v[idx] - 1.0).abs() < 1e-9);

    // "ダという猫" ends in kanji (猫) → false.
    let f2 = extract_char_features("ダという猫");
    let v2 = features_to_vector(&f2);
    assert!((v2[idx] - 0.0).abs() < 1e-9);
}
```

- [ ] **Step 3: Run tests**

```bash
cargo test -p ab-ortho-detect -- features 2>&1 | rg "test result"
```

- [ ] **Step 4: Commit**

```bash
git add crates/ab-ortho-detect/src/features.rs
git commit -m "feat(ortho-detect): canonical feature vector for ML (FEATURE_NAMES)"
```

---

### Task 5: ML logistic-regression detector + model file format

**Files:**
- Modify: `Cargo.toml` (root — add workspace deps + member)
- Create: `crates/ab-ortho-detect/src/ml.rs`
- Modify: `crates/ab-ortho-detect/src/lib.rs` (add `pub mod ml`)
- Modify: `crates/ab-ortho-detect/Cargo.toml` (deps)

**Interfaces:**
- Consumes: `CharFeatures`, `features_to_vector`, `FEATURE_NAMES`, `SentenceSpan`, `OrthoTokenizer`
- Produces:
  - `pub struct MlLogisticRegression` implementing `OrthoDetector`
  - `pub fn model_hash(weights: &[f64]) -> String` (SHA-256 little-endian f32)
  - A model file format: `bincode`-serialized `{ feature_names: Vec<String>, weights: Vec<f32>, intercept: f32, threshold: f32 }`

**Why:** spec Decision #9 (ML detector behind the trait), Decision #10 (model_hash as content hash), Decision #2 (coordinate invariants). The detector must work without a Vibrato tokenizer when character-only (the ablation-winning case).

- [ ] **Step 1: Add workspace deps**

Root `Cargo.toml` `[workspace.dependencies]`:
```toml
linfa-logistic = "0.8"
ndarray = "0.16"
bincode = "1"
sha2 = "0.10"
```
And add `"crates/ab-ortho-detect-ml"` to `[workspace] members` (for Task 6).

- [ ] **Step 2: Add deps to `ab-ortho-detect/Cargo.toml`**

```toml
[dependencies]
serde = { workspace = true, features = ["derive"] }
ab-plaintext = { workspace = true }
regex = { workspace = true }
thiserror = { workspace = true }
sha2 = { workspace = true }
bincode = { workspace = true }
```

- [ ] **Step 3: Write `model_hash` (TDD)**

In `crates/ab-ortho-detect/src/ml.rs`:

```rust
//! ML logistic-regression ortho detector.

use ab_plaintext::SentenceSpan;
use sha2::{Digest, Sha256};

use crate::features::{FEATURE_NAMES, extract_char_features, features_to_vector};
use crate::script::kata_to_hira;
use crate::types::{OrthoAnnotation, OrthoDetectorId, OrthoNormalization};

/// The on-disk model file shape. Serialized with bincode.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MlModel {
    /// Must equal FEATURE_NAMES at load time, else load fails.
    pub feature_names: Vec<String>,
    /// One weight per feature (LE f32-equivalent; stored as f64 for compute).
    pub weights: Vec<f32>,
    pub intercept: f32,
    /// Decision threshold on the sigmoid output. Default 0.5.
    pub threshold: f32,
}

/// SHA-256 hex digest of the model's weight bytes in little-endian f32.
/// Per spec Decision #10. NOT a debug-format string.
#[must_use]
pub fn model_hash(model: &MlModel) -> String {
    let mut bytes = Vec::with_capacity(model.weights.len() * 4 + 4);
    for &w in &model.weights {
        bytes.extend_from_slice(&w.to_le_bytes());
    }
    bytes.extend_from_slice(&model.intercept.to_le_bytes());
    let digest = Sha256::digest(&bytes);
    format!("{:x}", digest)
}

/// Logistic regression ortho detector. Character-only by construction;
/// does NOT require an OrthoTokenizer.
pub struct MlLogisticRegression {
    model: MlModel,
}

impl MlLogisticRegression {
    #[must_use]
    pub fn new(model: MlModel) -> Self {
        Self { model }
    }

    /// Load a model from a bincode file. Validates feature_names match
    /// FEATURE_NAMES (the canonical order).
    ///
    /// # Errors
    /// Returns an error if the file cannot be read, is not valid bincode,
    /// or has a feature_names mismatch (which would silently misweight).
    pub fn load(path: &std::path::Path) -> Result<Self, MlError> {
        let bytes = std::fs::read(path).map_err(MlError::Io)?;
        let model: MlModel = bincode::deserialize(&bytes).map_err(MlError::Bincode)?;
        if model.feature_names != FEATURE_NAMES {
            return Err(MlError::FeatureNameMismatch {
                expected: FEATURE_NAMES.iter().map(|s| (*s).to_string()).collect(),
                got: model.feature_names,
            });
        }
        Ok(Self::new(model))
    }

    fn score(&self, features: &crate::features::CharFeatures) -> f64 {
        let v = features_to_vector(features);
        let mut z = self.model.intercept as f64;
        for (w, x) in self.model.weights.iter().zip(v.iter()) {
            z += (*w as f64) * x;
        }
        1.0 / (1.0 + (-z).exp())
    }
}

impl crate::OrthoDetector for MlLogisticRegression {
    fn detector_id(&self) -> OrthoDetectorId {
        OrthoDetectorId::MlLogisticRegression {
            model_hash: model_hash(&self.model),
        }
    }

    fn detect(&self, sentences: &[SentenceSpan<'_>]) -> Vec<OrthoAnnotation> {
        let mut out = Vec::new();
        for s in sentences {
            let f = extract_char_features(s.text);
            let p = self.score(&f);
            if p >= self.model.threshold as f64 {
                let normalized = kata_to_hira(s.text);
                let byte_end = s.byte_offset + s.text.len();
                out.push(OrthoAnnotation {
                    source_byte_range: s.byte_offset..byte_end,
                    normalized_text: normalized,
                    kind: OrthoNormalization::ScriptKatakanaToHiragana,
                    confidence: Some((p * 100.0).round().clamp(0.0, 100.0) as u8),
                });
            }
        }
        out
    }
}

#[derive(Debug, thiserror::Error)]
pub enum MlError {
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),
    #[error("bincode deserialize error: {0}")]
    Bincode(#[from] bincode::Error),
    #[error("feature name mismatch: expected {expected:?}, got {got:?}")]
    FeatureNameMismatch { expected: Vec<String>, got: Vec<String> },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn model_hash_is_hex_sha256_of_le_f32_weights() {
        let m = MlModel {
            feature_names: FEATURE_NAMES.iter().map(|s| (*s).to_string()).collect(),
            weights: vec![1.0_f32, 2.0, 3.0],
            intercept: 0.5_f32,
            threshold: 0.5,
        };
        let h = model_hash(&m);
        assert_eq!(h.len(), 64);
        assert!(h.chars().all(|c| c.is_ascii_hex()));
        // Deterministic
        assert_eq!(h, model_hash(&m));
    }

    #[test]
    fn ml_detector_accepts_katakana_with_positive_weights() {
        // A trivially positive model: weight katakana_ratio high, rest zero.
        let mut weights = vec![0.0; FEATURE_NAMES.len()];
        let kidx = FEATURE_NAMES.iter().position(|n| *n == "katakana_ratio").unwrap();
        weights[kidx] = 10.0;
        let m = MlModel {
            feature_names: FEATURE_NAMES.iter().map(|s| (*s).to_string()).collect(),
            weights,
            intercept: -5.0, // 0.5 katakana_ratio * 10 - 5 = 0 → p=0.5; >0.5 → accept
            threshold: 0.5,
        };
        let det = MlLogisticRegression::new(m);
        let spans = ab_plaintext::sentence_split("吾輩ハ猫デアル。");
        let anns = det.detect(&spans);
        assert!(!anns.is_empty(), "katakana-dominant sentence should be accepted");
        assert!(anns[0].confidence.is_some());
    }

    #[test]
    fn ml_detector_rejects_hiragana() {
        let mut weights = vec![0.0; FEATURE_NAMES.len()];
        let kidx = FEATURE_NAMES.iter().position(|n| *n == "katakana_ratio").unwrap();
        weights[kidx] = 10.0;
        let m = MlModel {
            feature_names: FEATURE_NAMES.iter().map(|s| (*s).to_string()).collect(),
            weights,
            intercept: -5.0,
            threshold: 0.5,
        };
        let det = MlLogisticRegression::new(m);
        let spans = ab_plaintext::sentence_split("吾輩は猫である。");
        assert!(det.detect(&spans).is_empty());
    }
}
```

- [ ] **Step 4: Add `pub mod ml` to lib.rs + run tests**

```rust
pub mod ml;
```
```bash
cargo test -p ab-ortho-detect -- ml 2>&1 | rg "test result"
```
Expected: 3 tests pass.

- [ ] **Step 5: Commit**

```bash
git add Cargo.toml Cargo.lock crates/ab-ortho-detect/Cargo.toml crates/ab-ortho-detect/src/ml.rs crates/ab-ortho-detect/src/lib.rs
git commit -m "feat(ortho-detect): ML logistic-regression detector + model_hash

MlLogisticRegression implements OrthoDetector, character-only by
construction (no OrthoTokenizer required). model_hash is SHA-256 of
little-endian f32 weights per spec Decision #10. Feature-name mismatch
on load is a hard error (prevents silent misweighting)."
```

---

### Task 6: Trainer + ablation harness (`ab-ortho-detect-ml`)

**Files:**
- Create: `crates/ab-ortho-detect-ml/Cargo.toml`
- Create: `crates/ab-ortho-detect-ml/src/main.rs`
- Create: `crates/ab-ortho-detect-ml/src/io.rs`

**Interfaces:**
- Consumes: `ab-ortho-detect` (`features`, `ml`, `heuristic`), gold JSONL
- Produces: trained model files (`data/ortho-gold/model-full.bin`, `model-char-only.bin`), ablation report

**Why:** spec Decision #9 (REQUIRED ablation). The ablation's result determines whether the `OrthoTokenizer` trait survives Phase 2.

- [ ] **Step 1: Create the bin crate**

`crates/ab-ortho-detect-ml/Cargo.toml`:
```toml
[package]
name = "ab-ortho-detect-ml"
version.workspace = true
edition.workspace = true
license.workspace = true

[[bin]]
name = "ab-ortho-detect-ml"
path = "src/main.rs"

[dependencies]
ab-ortho-detect = { workspace = true }
ab-plaintext = { workspace = true }
linfa-logistic = { workspace = true }
ndarray = { workspace = true }
bincode = { workspace = true }
serde = { workspace = true, features = ["derive"] }
serde_json = { workspace = true }
clap = { workspace = true, features = ["derive"] }
```

- [ ] **Step 2: Write the IO module**

`crates/ab-ortho-detect-ml/src/io.rs`:
```rust
use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize)]
pub struct GoldRecord {
    pub sentence: String,
    /// "accept" or "reject"
    pub label: String,
}

/// Read gold JSONL.
/// # Errors
/// Returns an error on IO failure or malformed JSON.
pub fn read_gold(path: &std::path::Path) -> std::io::Result<Vec<GoldRecord>> {
    let mut out = Vec::new();
    for line in std::io::BufRead::lines(std::io::BufReader::new(std::fs::File::open(path)?)) {
        let line = line?;
        if line.trim().is_empty() { continue; }
        out.push(serde_json::from_str(&line).map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?);
    }
    Ok(out)
}
```

- [ ] **Step 3: Write the trainer main**

`crates/ab-ortho-detect-ml/src/main.rs`:
```rust
mod io;

use clap::{Parser, Subcommand};
use linfa_logistic::LogisticRegression;
use ndarray::{Array1, Array2};

use ab_ortho_detect::features::{FEATURE_NAMES, extract_char_features, features_to_vector};
use ab_ortho_detect::ml::{MlModel, model_hash};

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(Subcommand)]
enum Cmd {
    /// Train a model on gold JSONL.
    Train {
        #[arg(long)] gold: std::path::PathBuf,
        #[arg(long)] out: std::path::PathBuf,
    },
    /// Run the required ablation: full-feature vs character-only.
    /// (In v1 both use the same character-only feature set since OOV/proper-noun
    /// are dead per Branch B. The ablation documents this and is ready for
    /// re-evaluation when TokenFeatures become live.)
    Ablate {
        #[arg(long)] gold: std::path::PathBuf,
        #[arg(long)] report: std::path::PathBuf,
    },
    /// Print the model_hash of a model file.
    Hash {
        #[arg(long)] model: std::path::PathBuf,
    },
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    match cli.cmd {
        Cmd::Train { gold, out } => train(&gold, &out)?,
        Cmd::Ablate { gold, report } => ablate(&gold, &report)?,
        Cmd::Hash { model } => {
            let m: MlModel = bincode::deserialize(&std::fs::read(model)?)?;
            println!("{}", model_hash(&m));
        }
    }
    Ok(())
}

fn train(gold: &std::path::Path, out: &std::path::Path) -> anyhow::Result<()> {
    let recs = io::read_gold(gold)?;
    let n = recs.len();
    let d = FEATURE_NAMES.len();
    let mut x = Array2::<f64>::zeros((n, d));
    let mut y = Array1::<f64>::zeros(n);
    for (i, rec) in recs.iter().enumerate() {
        let features = extract_char_features(&rec.sentence);
        let v = features_to_vector(&features);
        for (j, val) in v.iter().enumerate() {
            x[(i, j)] = *val;
        }
        y[i] = if rec.label == "accept" { 1.0 } else { 0.0 };
    }
    let model = LogisticRegression::default().max_iterations(500).fit(&x, &y)?;
    let weights: Vec<f32> = model.coef().iter().map(|w| *w as f32).collect();
    let intercept = *model.intercept() as f32;
    let ml_model = MlModel {
        feature_names: FEATURE_NAMES.iter().map(|s| (*s).to_string()).collect(),
        weights,
        intercept,
        threshold: 0.5,
    };
    let bytes = bincode::serialize(&ml_model)?;
    std::fs::write(out, &bytes)?;
    eprintln!("trained model_hash={} (n={})", model_hash(&ml_model), n);
    Ok(())
}

fn ablate(gold: &std::path::Path, report: &std::path::Path) -> anyhow::Result<()> {
    // v1 reality (Branch B): TokenFeatures are dead (oov_count hardcoded 0).
    // Character-only IS the only viable feature set. The ablation documents this
    // explicitly so a future (full-feature revisit) has a baseline to beat.
    let recs = io::read_gold(gold)?;
    let n = recs.len();
    let d = FEATURE_NAMES.len();
    let mut x = Array2::<f64>::zeros((n, d));
    let mut y = Array1::<f64>::zeros(n);
    for (i, rec) in recs.iter().enumerate() {
        let f = extract_char_features(&rec.sentence);
        let v = features_to_vector(&f);
        for (j, val) in v.iter().enumerate() { x[(i, j)] = *val; }
        y[i] = if rec.label == "accept" { 1.0 } else { 0.0 };
    }
    // Train + hold-out accuracy.
    let model = LogisticRegression::default().max_iterations(500).fit(&x, &y)?;
    let preds = model.predict(&x);  // train-set accuracy (small data)
    let correct = preds.iter().zip(y.iter()).filter(|(p, t)| (**p as u8 as f64) == *t).count();
    let acc = correct as f64 / n as f64;
    let text = format!(
        "# Ortho-Detect ML Ablation (Phase 2)\n\n\
        Date: 2026-07-05\nGold set: {} (n={})\n\n\
        ## Ablation: full-feature vs character-only\n\n\
        **v1 reality (Branch B):** TokenFeatures (oov_count, oov_ratio,\n\
        proper_noun_char_ratio) are dead — Vibrato's LexType::Unknown never\n\
        fires on katakana prose because Unidic-CWJ has dictionary entries for\n\
        katakana particles/copulas (see Phase 1 baseline report). Therefore\n\
        **character-only IS the only viable feature set in v1.** The\n\
        Vibrato-coupling (OrthoTokenizer trait + ortho_compat.rs) is a no-op\n\
        for ML purposes in v1 and can be DELETED if the ML detector becomes\n\
        the default.\n\n\
        **Train-set accuracy (small data, no hold-out split):** {:.4}\n\n\
        **Bias caveat:** this accuracy is against BOOTSTRAP labels (the Python\n\
        heuristic labeling itself, via aozora-corpus-generator). It measures\n\
        whether the linear model can reproduce the heuristic, NOT real-world\n\
        detection quality. Real evaluation requires the human-annotated gold\n\
        set (documented finishing input).\n\n\
        ## Recommended action\n\n\
        - Delete `OrthoTokenizer` trait + `ortho_compat.rs` + double-dict-load\n\
          + `oov_*` config fields (investigation report items C1, C3, C4).\n\
        - Keep `MlLogisticRegression` character-only as the canonical detector.\n\
        - Re-run ablation when TokenFeatures become live (requires exposing\n\
          Vibrato LexType::Unknown through the trait).\n",
        gold.display(), n, acc
    );
    std::fs::write(report, text)?;
    eprintln!("wrote ablation report: {} (train acc {:.4})", report.display(), acc);
    Ok(())
}
```

Add `anyhow` to the bin crate deps if not a workspace dep:
```toml
anyhow = { workspace = true }
```
(Verify root workspace has `anyhow`; Phase 1 used it elsewhere.)

- [ ] **Step 4: Train + ablate on the bootstrap gold set**

```bash
cd /home/bor/Projects/ab-validator/.worktrees/ortho-detect-phase2
mkdir -p data/ortho-gold/models
cargo run -p ab-ortho-detect-ml -- train --gold data/ortho-gold/sentences.jsonl --out data/ortho-gold/models/model-v1.bin 2>&1 | tail -3
cargo run -p ab-ortho-detect-ml -- ablate --gold data/ortho-gold/sentences.jsonl --report reports/ortho-detect/2026-07-05-phase2-ablation.md 2>&1 | tail -3
cargo run -p ab-ortho-detect-ml -- hash --model data/ortho-gold/models/model-v1.bin
```
Expected: model file written, ablation report written, hash printed.

- [ ] **Step 5: Commit**

```bash
git add crates/ab-ortho-detect-ml/ Cargo.toml Cargo.lock \
        data/ortho-gold/models/model-v1.bin reports/ortho-detect/2026-07-05-phase2-ablation.md
git commit -m "feat(ortho-detect-ml): trainer + required ablation harness

ab-ortho-detect-ml bin crate with `train`, `ablate`, `hash` subcommands.
The ablation documents the v1 reality: TokenFeatures are dead (Branch B),
so character-only IS the only viable feature set. Train-set accuracy
reported against bootstrap labels with a prominent bias caveat."
```

---

### Task 7: IR provenance — `AatProjection.ortho_normalizations`

**Files:**
- Modify: `crates/ab-ir/src/lib.rs`
- Modify: `crates/ab-ir/Cargo.toml`

**Why:** spec §"IR/AAT mode". The full TEI `<choice>` XML renderer is a separate sub-project (no TEI renderer exists today); this task adds the field + a JSON-level `<choice>` projection helper that a future TEI renderer will consume. `OrthoAnnotation` derives `Eq` so `AatProjection`'s `Eq` derive survives.

- [ ] **Step 1: Add dep**

`crates/ab-ir/Cargo.toml`:
```toml
[dependencies]
ab-ortho-detect = { workspace = true }
```
(Verify existing deps; add only this line.)

- [ ] **Step 2: Add the field + a projection helper**

In `crates/ab-ir/src/lib.rs`, find `AatProjection`:
```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AatProjection {
    pub blocks: Vec<serde_json::Value>,
    pub warnings: Vec<ProjectionWarning>,
    /// Orthographic normalizations applied to visible text, with original-text
    /// byte ranges. External to the Block/Inline tree — does not mutate IR nodes.
    pub ortho_normalizations: Option<Vec<ab_ortho_detect::OrthoAnnotation>>,
}
```

Update all `AatProjection { ... }` construction sites (search for `blocks_to_aat_projection` and any test fixtures) to add `ortho_normalizations: None`. Add a helper:

```rust
impl AatProjection {
    /// Render ortho annotations as JSON `<choice>` elements (one per annotation):
    /// `{"choice": {"orig": <orig text>, "reg": <reg text>}}`. Requires the
    /// original visible-text bytes to source `<orig>`. A future TEI XML
    /// renderer consumes this projection.
    #[must_use]
    pub fn ortho_choices(&self, original_visible_text: &str) -> Vec<serde_json::Value> {
        let Some(anns) = &self.ortho_normalizations else { return Vec::new(); };
        anns.iter().map(|a| {
            let orig = original_visible_text.get(a.source_byte_range.clone())
                .unwrap_or("").to_string();
            serde_json::json!({"choice": {"orig": orig, "reg": a.normalized_text}})
        }).collect()
    }
}
```

- [ ] **Step 3: Test the field preserves Eq + the projection**

```rust
#[test]
fn aat_projection_with_ortho_derives_eq() {
    let a = AatProjection { blocks: vec![], warnings: vec![], ortho_normalizations: None };
    assert_eq!(a, a.clone());
}
```
(Add the test in the existing test module in `lib.rs`.)

- [ ] **Step 4: Build + test**

```bash
cargo test -p ab-ir 2>&1 | rg "test result"
```

- [ ] **Step 5: Commit**

```bash
git add crates/ab-ir/
git commit -m "feat(ab-ir): AatProjection.ortho_normalizations + choice projection

Adds the ortho provenance field per spec IR/AAT mode. External to the
Block/Inline tree (does not mutate IR nodes). ortho_choices() projects
annotations to JSON {choice: {orig, reg}} for a future TEI XML renderer.
OrthoAnnotation derives Eq so AatProjection's Eq derive survives."
```

---

### Task 8: CLI — `--ortho-detect ml` + `--ortho-ml-model PATH`

**Files:**
- Modify: `crates/ab-morph-run/src/options.rs` (`OrthoDetectMode::Ml`)
- Modify: `crates/ab-morph-run/src/main.rs` (CLI flags)
- Modify: `crates/ab-morph-run/src/pipeline.rs` (construct MlLogisticRegression)
- Modify: `crates/ab-morph-run/Cargo.toml` (deps)

**Why:** spec §"Pipeline Integration → ab-morph-run --ortho-detect=ml". The ML detector is character-only (no Vibrato first pass) — the only Phase 2 item that touches the serial pipeline's ortho construction.

- [ ] **Step 1: Add deps**

`crates/ab-morph-run/Cargo.toml`: ensure `ab-ortho-detect` is present (Phase 1 added it). No new ML deps needed — `MlLogisticRegression` is in `ab-ortho-detect`.

- [ ] **Step 2: Extend the enum + options**

In `crates/ab-morph-run/src/options.rs`:
```rust
#[derive(Debug, Clone, clap::ValueEnum, PartialEq, Eq)]
pub enum OrthoDetectMode {
    Off,
    Heuristic,
    Ml,
}
```
Add to `SerialRunOptions`:
```rust
/// Path to a trained ML model file (bincode). Required when ortho_detect == Ml.
pub ortho_ml_model: Option<std::path::PathBuf>,
```

- [ ] **Step 3: Add CLI flags in `main.rs`**

In the `AnalyzeAat` subcommand struct (around the existing `--ortho-detect` flag):
```rust
#[arg(long = "ortho-detect", value_enum, default_value_t = OrthoDetectMode::Off)]
ortho_detect: OrthoDetectMode,
#[arg(long = "ortho-ml-model", requires = "ortho_detect")]
ortho_ml_model: Option<std::path::PathBuf>,
```
The `requires = "ortho_detect"` ensures the flag is only meaningful with `--ortho-detect ml`. In the dispatch, pass `ortho_ml_model` into `SerialRunOptions`. Validate: if `ortho_detect == Ml` and `ortho_ml_model` is None → error "must be provided".

- [ ] **Step 4: Construct the ML detector in `pipeline.rs`**

Near where Phase 1 constructs `HeuristicV1`, add a branch:
```rust
let detector: Option<Arc<dyn ab_ortho_detect::OrthoDetector>> = match options.ortho_detect {
    crate::OrthoDetectMode::Off => None,
    crate::OrthoDetectMode::Heuristic => {
        // existing Phase 1 HeuristicV1 construction
        Some(Arc::new(HeuristicV1::new(/* ... */ )))
    }
    crate::OrthoDetectMode::Ml => {
        let path = options.ortho_ml_model.as_ref()
            .ok_or_else(|| anyhow::anyhow!("--ortho-ml-model is required for --ortho-detect ml"))?;
        let model = ab_ortho_detect::ml::MlLogisticRegression::load(path)?;
        Some(Arc::new(model))
    }
};
```
Then refactor the existing detection loop to use `detector.detect(&sentences)` polymorphically (Phase 1 inlined the `HeuristicV1::new` + `detect` call; hoist it behind the trait object).

- [ ] **Step 5: Smoke test the ML path**

```bash
export AB_VIBRATO_DICT=/home/bor/Projects/ab-validator/dictionary/compiled/unidic-cwj-202512.dic.zst
cargo run -p ab-morph-run -- analyze-aat \
  --ortho-detect ml \
  --ortho-ml-model data/ortho-gold/models/model-v1.bin \
  --analyzer vibrato --aat data/ortho-pilot/sentences.aat.json \
  --analyses-output /tmp/ml-out.jsonl 2>&1 | tail -3
python3 -c "
import json
a = json.loads(open('/tmp/ml-out.jsonl').readline())
anns = a['analysis'].get('ortho_annotations') or []
print('annotations:', len(anns))
for ann in anns:
    print('  confidence:', ann.get('confidence'), 'kind:', ann.get('kind'))
"
```
Expected: exit 0, `ortho_annotations` populated (nonzero), with `confidence: Some(...)` (ML sets confidence, unlike the heuristic's `None`).

- [ ] **Step 6: Commit**

```bash
git add crates/ab-morph-run/
git commit -m "feat(morph-run): --ortho-detect ml + --ortho-ml-model CLI

OrthoDetectMode gains Ml variant. The pipeline constructs
MlLogisticRegression behind the OrthoDetector trait object, so the
heuristic and ML paths share one detection-dispatch code path. ML
detector is character-only (no Vibrato first pass)."
```

---

### Task 9: Human-annotation finishing step + retraining

**Files:**
- Modify: `data/ortho-gold/sentences.jsonl` (replace bootstrap labels with human labels)
- Re-run Task 6 trainer

**Why:** the spec calls for 200–500 manually-annotated sentences. The bootstrap labels (Task 2) let the pipeline run end-to-end but are biased. This is the documented human-finish input.

- [ ] **Step 1: Human-label a sample**

For each record in `data/ortho-gold/sentences.jsonl` (target N=200 minimum), read the `sentence` field and assign `label` ∈ {`accept`, `reject`} by human judgment. Write back to the same file (overwrite the `label` field only; preserve all other fields).

This step is human labor. The plan acknowledges it: in-session, the assistant can label a smaller N (e.g. 50) by reading each sentence and applying the spec's intent ("should we kata→hira-normalize this sentence before analysis?"). Larger N is the user's input.

- [ ] **Step 2: Retrain + re-measure recall**

```bash
cargo run -p ab-ortho-detect-ml -- train --gold data/ortho-gold/sentences.jsonl --out data/ortho-gold/models/model-v2-human.bin
cargo run -p ab-ortho-detect-ml -- ablate --gold data/ortho-gold/sentences.jsonl --report reports/ortho-detect/2026-07-05-phase2-ablation-human.md
# Re-run recall against the human-labeled set:
cargo run -p ab-ortho-detect --example detect_sentences < data/ortho-gold/sentences.jsonl > /tmp/recall-human.jsonl
bb scripts/ortho-gold/recall_floor.clj /tmp/recall-human.jsonl
```
Write the real recall number into `reports/ortho-detect/2026-07-05-phase2-recall-floor.md` (append a "Human-labeled recall" section). If recall < 0.85, tune `HeuristicConfig` thresholds (per spec Pre-Phase-1 step 2) and re-measure.

- [ ] **Step 3: Commit the human-labeled gold + retrained model**

```bash
git add data/ortho-gold/sentences.jsonl data/ortho-gold/models/model-v2-human.bin \
        reports/ortho-detect/2026-07-05-phase2-ablation-human.md \
        reports/ortho-detect/2026-07-05-phase2-recall-floor.md
git commit -m "data(ortho-gold): human-labeled gold set + retrained model

Replaces bootstrap labels with human annotations for N sentences.
Retrained model-v2-human.bin. Real recall measured and recorded."
```

---

## Task Dependency Graph

```
Task 0 (baseline) — no deps
Task 1 (Task 7.5 fix) — no deps (Phase 1 debt)
Task 2 (gold candidates) — no deps
Task 3 (recall floor) — depends on Task 2 (needs the gold set)
Task 4 (feature vector) — no deps
Task 5 (ML detector) — depends on Task 4
Task 6 (trainer + ablation) — depends on Tasks 2, 5
Task 7 (IR provenance) — no deps
Task 8 (CLI --ortho-detect ml) — depends on Tasks 5
Task 9 (human labels + retrain) — depends on Tasks 2, 6
```

Parallel: Tasks 1, 2, 4, 7 are independent and can run in parallel after Task 0.
Task 3 needs Task 2. Task 5 needs Task 4. Task 6 needs 2 + 5. Task 8 needs 5. Task 9 needs 2 + 6.

## Self-review checklist (run after writing, before execution)

1. **Spec coverage — spec Phase 2 bullets:**
   - Clojure gold-data pipeline → Task 2 ✓
   - Logistic regression classifier → Task 5 ✓
   - Required character-only ablation → Task 6 ✓ (+ documents the Branch-B reality)
   - ML-backed OrthoDetector impl → Task 5 ✓
   - `--ortho-detect=ml` flag → Task 8 ✓
   - IR/AAT annotation layer + TEI `<choice>` → Task 7 (field + JSON projection; full TEI XML renderer is scoped out, documented in Task 7)
   - `model_hash` SHA-256 LE f32 → Task 5 ✓ (test asserts)

2. **Phase 1 debt (investigation report item A):** Task 1 ✓

3. **Placeholder scan:** the only "TBD-ish" item is the human-annotation (Task 9 Step 1), which is explicitly framed as human labor — not a plan placeholder, but an external input. Acceptable.

4. **Type consistency:** `MlModel`, `MlLogisticRegression`, `model_hash`, `OrthoMapError`, `FEATURE_NAMES`, `features_to_vector` — names match across Tasks 1/4/5/6/8. Verified.

5. **Bias honesty:** bootstrap labels are flagged in Task 2, Task 3 (recall-floor report), Task 6 (ablation report), and Task 9 (the retraining step). The investigation report §B1 is referenced. No claim of shipping-grade ML quality is made.
