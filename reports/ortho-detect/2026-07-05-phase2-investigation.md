# Ortho-Detect Phase 2 — Remaining-Work Investigation

Date: 2026-07-05
Author: triage after Phase 1 merge (`main` @ `8268096`)
Sources of truth:
- Design spec: `docs/superpowers/specs/2026-07-05-ortho-detect-design.md`
- Phase 1 plan: `docs/superpowers/plans/2026-07-05-ortho-detect-phase1.md`
- Phase 1 baseline + post-impl review: `reports/ortho-detect/2026-07-05-sudachi-baseline.md`

## Method

Re-read the full design spec (the authoritative roadmap) and the Phase 1
plan, then diffed against the merged `main` state. Three reservoirs of
remaining work surfaced:

1. **Spec's explicit "Phase 2" section** — the canonical definition.
2. **Phase 1 plan's deferred "Task 7.5"** — a coordinate-coherence fix
   explicitly named in the plan's dependency graph but never implemented.
3. **Post-implementation review findings** — recorded in the baseline
   report; lower-severity items the Phase 1 implementer accepted as
   follow-ups.

Each item below is classified using the architecture-triage framing
(braided concern / fake seam / shallow module / protocol-state risk /
implementation refactor / upstream uncertainty) and tagged with a
suggested sequencing.

---

## A. Phase 1 debt — `Task 7.5` coordinate coherence (do FIRST)

**Severity: Blocker-for-Phase-2-readiness (latent crash today).**

`span_builder::remap_spans` rewrites `morpheme.byte_span` to ORIGINAL-text
coordinates but leaves `morpheme.surface` and `morpheme.char_span` in
NORMALIZED-text coordinates, and `OffsetMap::to_original` **panics** on
cross-boundary spans. This means:

- **Length-preserving normalizations** (ハ→は, ガ→が — the common case):
  validation passes by coincidence (identical byte lengths / char ratios).
  Verified by a 2-analyzer smoke test (66 regions, 0 mismatches).
- **Length-changing normalizations** (`ヴ→う゛`, 3 bytes → 6 bytes):
  `OffsetMap::to_original` panics, or `validate_analysis_against_source`
  emits `SurfaceMismatch`/`CharSpanMismatch`. Any document containing `ヴ`
  that the detector accepts will crash the pipeline.

**Classification:** protocol/state risk + implementation refactor. The
`Analysis` value holds two coordinate spaces simultaneously after remap —
that is braided state, not just a missing feature.

**Spec alignment:** the design spec's invariant #2 states "Morpheme
`byte_span` and `char_span` are in original-document coordinates. …
`Analysis.source_text` is the normalized text." Phase 1 only half-honors
this (byte_span yes, char_span/surface no). The plan's Task 7 comment
explicitly defers: "for Phase 1, keep the normalized-text char_span as an
approximation. Follow-up: rebuild char_map from original text bytes."

**Suggested fix (already drafted in the baseline report):** give
`remap_spans` the original source text and rebuild each morpheme
coherently: `surface = &original[remapped_byte_span]`,
`char_span = char_map.char_count_at_byte(...)`, `analysis.source_text =
original`. For sub-token splits crossing a length-changing entry, return
an error rather than panicking. Convert `OffsetMap::to_original` from
`panic` to `Result<_, OrthoMapError>` so pipeline failures route to
`errors_writer` with a typed stage/code instead of crashing the process.

**Sequencing:** do this **before** any Phase 2 work that exercises the
pipeline on real corpora (which will contain `ヴ`). It is a prerequisite
for trustable ML evaluation metrics, because broken coordinates mean
broken per-morpheme provenance.

**Relevant code:**
`crates/ab-ortho-detect/src/types.rs:68-108` (`OffsetMap::to_original`,
panic), `crates/ab-morph-analyzers/src/span_builder.rs:137` (`remap_spans`),
`crates/ab-morph-diff/src/validate.rs:22-73` (validation invariants),
`crates/ab-morph-run/src/pipeline.rs:685-700` (clear-guards on
`source_text` in compact/warehouse paths interact with the fix).

---

## B. Spec "Phase 2: Gold data and ML" (the canonical Phase 2 scope)

Verbatim from `ortho-detect-design.md §Phased Plan → Phase 2`:

> - Clojure gold-data pipeline: candidate extraction, annotation workflow.
> - Logistic regression classifier trained on gold data.
> - **Required ablation:** train and evaluate a character-only variant (no
>   Vibrato-derived features). If character-only matches full-feature
>   performance, the Vibrato coupling can be removed.
> - ML-backed `OrthoDetector` impl behind same trait.
> - `ab-morph-run --ortho-detect=ml` flag.
> - IR/AAT annotation layer and TEI `<choice>` rendering.
> - `model_hash` in `OrthoDetectorId::MlLogisticRegression` is a SHA-256
>   hex digest of serialized model weights for reproducibility.

### B1. Gold-data pipeline (Clojure) — candidate extraction + annotation workflow

**Current state:** `data/ortho-pilot/sentences.aat.json` (10 sentences)
exists from Task 0; `data/ortho-gold/` does **not** exist. No gold-label
JSONL files anywhere.

**Classification:** upstream design uncertainty (hammock before code). The
spec leaves two sub-decisions open:
- **Catalog metadata ingestion** (spec Open Question #2): the Aozora
  catalog CSV (`list_person_all_extended_utf8.csv`) is read by
  `aozora-corpus-generator` but not by ab-validator. Decision: shell out
  to the Python tool or ingest the CSV directly in Clojure. Either path
  is fine, but it gates stratification (era / author / katakana-ratio /
  sentence-length).
- **Recall floor** (spec Pre-Phase-1 Verification step 2 + Validation
  Framework step 4): "Of human-labeled 'normalize: yes' sentences, what
  fraction did the heuristic accept? If recall < 0.85, tune thresholds
  before shipping." Phase 1 shipped with Python-derived defaults — this
  measurement was never run on a labeled set. It is the **actual**
  quality gate and must precede ML training (low recall skews labels).

**Output shape (spec Decision #8):** `data/ortho-gold/`, one JSONL per
work, each record `{sentence, label, features, per_analyzer:
{analyzer_id: {raw_metrics, norm_metrics}}}`, with `dict_version`
recorded alongside `analyzer_id` (spec Open Question #4 — needed because
UniDic/Sudachi dict versions affect metrics).

**Sequencing:** after A. The candidate extraction is the seed; the recall
floor measurement is the gate. Estimate 200–500 manually-annotated
sentences per the spec.

### B2. ML logistic-regression classifier

**Current state:** no `crates/ab-ortho-detect/src/ml.rs`. The spec's file
map lists it as "future". `OrthoDetectorId::MlLogisticRegression {
model_hash: String }` is already defined in `types.rs` (Phase 1 shipped
the enum variant, matching the spec's "reserved" pattern).

**Classification:** implementation refactor behind an existing trait seam.
`OrthoDetector` is already `Send + Sync` and takes
`&[SentenceSpan<'_>]; -> Vec<OrthoAnnotation>`. After the dependency
inversion done in Phase 1 (the `OrthoTokenizer` trait in
`ab-ortho-detect`, impl'd for `VibratoAnalyzer` in
`ab-morph-analyzers/src/ortho_compat.rs`), an ML classifier can be a
pure-Rust leaf crate with zero tokenizer coupling — feed it
`CharFeatures` only.

**Required ablation (spec Decision #9, non-negotiable):** train and
evaluate a **character-only** variant (no `TokenFeatures` /
`oov_count` / `proper_noun_char_ratio`). If it matches full-feature
performance, **remove the Vibrato coupling entirely**. This is the
highest-leverage architectural decision in Phase 2: success means the
`OrthoTokenizer` trait, `ortho_compat.rs`, the double-dictionary-load,
and the cyclic-dep inversion all become deletable.

**`model_hash` provenance (spec Decision #10):** SHA-256 hex digest of
the raw model weight bytes in little-endian f32 (NOT a debug-format
string), carried in `OrthoDetectorId::MlLogisticRegression` for
reproducibility across serialization formats.

**Sequencing:** after B1 (gold data is the training set). The ablation
result determines whether to also delete the Phase 1 Vibrato coupling.

### B3. CLI wiring: `--ortho-detect=ml`

**Current state:** `OrthoDetectMode` enum in
`crates/ab-morph-run/src/options.rs` has only `Off`/`Heuristic`. No `Ml`
variant. `crate::OrthoDetectMode` is re-exported from `lib.rs`.

**Classification:** trivial implementation refactor once B2 lands. Add
`Ml { model_path: PathBuf }` (or a registry lookup) to the enum, plumb
through `run_analyze_aat_with_nway`, construct the ML detector in
`pipeline.rs` where Phase 1 constructs `HeuristicV1`.

**Note:** this is the only Phase 2 item that necessarily touches the
serial-pipeline plumbing; the rest of the ortho layer is behind the
trait.

### B4. IR/AAT annotation layer + TEI `<choice>` rendering

**Current state:** `crates/ab-ir/src/lib.rs`'s `AatProjection` has only
`{ blocks, warnings }` — no `ortho_normalizations` field. The spec's
"IR/AAT mode" section defines the target shape:
```rust
pub struct AatProjection {
    pub blocks: Vec<serde_json::Value>,
    pub warnings: Vec<ProjectionWarning>,
    pub ortho_normalizations: Option<Vec<OrthoAnnotation>>,  // NEW
}
```
and the TEI renderer target:
```xml
<choice><orig>吾輩ハ猫デアル</orig><reg>吾輩は猫である</reg></choice>
```

**Classification:** shallow-module deepening (one field + one renderer
branch). Lower risk than B1/B2 but the spec's `OrthoAnnotation`-derives-
`Eq` invariant was chosen precisely so `AatProjection`'s existing `Eq`
derive survives — verify that still holds when wiring (confidence is
`Option<u8>` to preserve `Eq`).

**Cross-crate dependency note (spec Non-goals + Decision #5):** the
annotation layer is **external to** the `Block`/`Inline` IR tree — it
must not mutate IR nodes. Ruby bases and gaiji-resolved text in the
visible-text projection may contain katakana and will be normalized
(intended). This is the one place `ab-ir` gains an `ab-ortho-detect`
dependency; currently `ab-ir` is ortho-unaware.

**Sequencing:** independent of B1/B2 (works on heuristic annotations
too). Can precede ML.

---

## C. Post-impl review findings (lower-severity Phase 2 candidates)

From the design review recorded in the baseline report. None are Phase 2
blockers, but B2's ablation may make some of them moot — sequence them
after the ablation result is known.

### C1. Double dictionary load (Important)

The pipeline constructs a fresh `VibratoAnalyzer` for detection even when
`--analyzer vibrato` is also passed (dictionary loaded twice per run).
Spec Decision: Phase 1 accepts this; Phase 2 fixes it. **After B2's
ablation:** if character-only wins, this entire concern disappears (no
detection tokenizer at all). If full-feature wins, reuse the already-
loaded analyzer by exposing it as `Arc<dyn OrthoTokenizer>` from the
analyzer loader.

`crates/ab-morph-run/src/pipeline.rs:599-601`.

### C2. `ab-morph-diff → ab-ortho-detect` coupling edge (Important)

`Analysis` now stores typed `OrthoAnnotation`/`OffsetMap` directly,
coupling the core diff model to the detector crate. Reviewer's
alternative: keep the provenance in an opaque metadata bag inside
`Analysis` (e.g. `serde_json::Value` or a small `OrthographicMetadata`
struct) and let `ab-ortho-detect` define the schema. **Classification:**
fake-seam elimination candidate if the typed coupling proves painful, but
the spec explicitly chose typed coupling (Decision #2); revisit only if
the diff model churns.

### C3. Dead OOV config (Strong suggestion)

`HeuristicConfig.oov_ratio_threshold` and `TokenFeatures.oov_count` /
`oov_ratio` are hardcoded to `0`/`0.0` (Phase 1 Branch B option (b):
Vibrato's `LexType::Unknown` never fires on katakana prose because
Unidic-CWJ has dictionary entries for katakana particles/copulas).
Retained "for config parity with the spec." **After B2's ablation:** if
character-only wins, delete the fields and the dead branch entirely. If
full-feature wins, reconsider whether to expose Vibrato's real
`LexType::Unknown` via the `OrthoTokenizer` trait instead.

### C4. `OrthoTokenizer` swallows tokenization errors (Strong suggestion)

`crates/ab-morph-analyzers/src/ortho_compat.rs` returns `Vec::new()` on
`Err`. Reviewer's alternative: change `tokenize` to return `Result<Vec<OrthoToken>,
_>` so detection failures are observable. **Classification:** protocol
risk (silent failure mode). Worth fixing alongside C1/C3 if the trait
survives the ablation — otherwise it is deleted with the trait.

---

## D. Phase 3 (explicitly out of scope, recorded for completeness)

- `HistoricalToModern` detection and normalization (歴史的仮名遣い →
  現代仮名遣い, e.g. `けふ` → `きょう`). Requires dictionary integration
  or a learned model. The enum variant exists in `types.rs` but no
  detector emits it; spec Open Question #3 (which dictionary? UniDic
  `lForm`? full lexicon vs. common auxiliary verbs?) is unresolved.
- Speech-span filtering (`speech_mode` from aozora-corpus-generator) —
  explicitly a Non-goal; may become a separate `ab-plaintext` filter
  later.
- Automatic era/author dating from orthography — explicitly a Non-goal.

---

## E. Spec-vs-implementation discrepancies to resolve before Phase 2

These are NOT bugs, but they are inconsistencies between the spec text
and the shipped Phase 1 that should be reconciled in a design ADR before
Phase 2 commits to either shape:

### E1. `ab-ortho-detect` dependency direction

- **Spec §Architecture:** "`ab-ortho-detect` has **zero workspace
  dependencies** except the standard library and `serde`."
- **Spec §Resolved Decision #5:** "`ab-ortho-detect` depends on
  `ab-plaintext` (for `SentenceSpan`), `ab-morph-analyzers` (for
  Vibrato first pass), and `ab-morph-diff` (for `FeatureMap`)."

These two statements contradict. Phase 1 resolved it by: `ab-ortho-detect`
depends only on `ab-plaintext` (not on `ab-morph-analyzers` / `ab-morph-
diff`); the Vibrato coupling is inverted via the `OrthoTokenizer` trait
defined in `ab-ortho-detect` and impl'd in `ab-morph-analyzers`. This
matches the architecture statement (near-zero deps) and the plan's
dependency graph, and it broke the cyclic package dependency the
plan's Task 4 would have created. **Recommend an ADR recording this as
the canonical shape** so Phase 2 doesn't relitigate it.

### E2. `SentenceSpan` location

- Spec §Architecture puts `SentenceSpan` in `ab-ortho-detect/src/types.rs`.
- Phase 1 plan Task 1 + implementation put it in `ab-plaintext`.

Phase 1's choice was forced by E1 (if `ab-ortho-detect` is leaf-ish and
`ab-plaintext` doesn't depend on it, the shared type must live in the
leaf). Consistent with E1 resolution. **Recommend the ADR record this
too.**

---

## Suggested Phase 2 sequencing

```
0. ADR reconciling E1/E2 (hammock, ~1h, no code) — locks the crate shape
1. Task 7.5 (item A) — coordinate coherence; prerequisite for trustable eval
2. B1 gold-data pipeline + recall-floor measurement — gates everything ML
3. B4 IR/TEI <choice> rendering — parallel to B1; works on heuristic output
4. B2 ML classifier + character-only ablation — the load-bearing decision
5. Branch on ablation result:
     - if character-only wins: delete OrthoTokenizer / ortho_compat.rs /
       oov fields (C1, C3, C4 all resolved by deletion)
     - if full-feature wins: C1 (reuse analyzer), C3 (real LexType), C4
       (Result-returning tokenize) — small focused follow-ups
6. B3 `--ortho-detect=ml` CLI — trivial once B2 lands
```

Items 5's branch is the single highest-leverage decision in Phase 2:
**the ablation determines whether ~150 lines of trait machinery and a
whole crate dependency edge get deleted.** Do not add ML complexity
until the ablation has answered it.

## Open questions for the user before Phase 2 starts

1. **Is the Phase 2 scope "ML + gold data + IR/TEI" (the full spec
   Phase 2), or just "fix Phase 1 debt + IR/TEI"?** The spec's Phase 2
   is large; a smaller "Phase 1.5" addressing only A + B4 + the ablation
   prep would be a credible milestone.
2. **Who labels the gold data?** 200–500 sentences of manual annotation
   is the spec's estimate. Is there an existing labeled set anywhere
   (aozora-corpus-generator?), or is this net-new human work?
3. **Is the recall-floor measurement acceptable as a Phase 1.5 gate?**
   The spec calls it "Pre-Phase-1 Verification step 2" but Phase 1
   shipped without it. Running it now would tell us whether the
   Python-derived defaults need tuning before any ML work.
4. **Should the ADR (item 0) be a separate doc, or folded into the
   design spec as an addendum?**
