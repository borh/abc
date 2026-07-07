# Parser-IR Orthographic Publication Followups Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Productionize parser-IR sentence and orthographic evidence so the real detector output reaches ABC publication TEI, sentence evidence is mandatory for publication-grade parser-IR, synthetic evidence is documented in schema, and ruby readings have an explicit evidence contract.

**Architecture:** Keep parser-IR as the shared evidence contract. ab-validator generates sentence rows and orthographic annotation bundles from the same parser-IR-visible body coordinates; ABC validates the evidence and renders TEI from those rows without re-splitting text. Ruby readings remain oracle evidence for tokenizer evaluation, not replacement text for parser-IR sentence splitting or tokenizer input.

**Tech Stack:** Rust 2024 (`ab-aat-to-parser-ir`, `ab-ortho-detect`, `ab-morph-analyzers`, `ab-plaintext`, `ab-morph-run`, `ab-warehouse`), Clojure (`abc.tools.parser-ir-tei`, `abc.tools.materialize-publication`, `abc.tools.validate-design-bundle`), JSON Schema draft 2020-12, Nix checks, shell smoke tests.

## Global Constraints

- All implementation work happens in an isolated worktree; do not move `main` onto a feature branch.
- Parser-IR sentence coordinates stay in decoded UTF-8 bytes: `"coordinate_system": "decoded_utf8"`.
- Sentence splitter identity remains `"ab-plaintext-japanese-v1"`.
- `orthographic_annotations.detector_id` must identify the detector that produced the bundle; no inferred detector defaults in stored JSON.
- `orthographic_annotations.annotations[].source_byte_range` must use the same decoded visible-body byte coordinates as `sentences[].span`.
- Sentence splitting and tokenizer input use visible base text. Ruby readings are evidence for analyzer evaluation, not parser-IR replacement text.
- No active code may depend on untracked `references/` paths or machine-local output roots such as `/db/<path>`.
- Corpus-scale outputs stay out of source. Checked-in reports contain summaries, command lines, and small fixtures only.
- Do not weaken the existing ABC sentence coherence checks for body paragraph tiling, orthographic annotation indices, or zero-span paragraphs.
- Direct publication materialization must fail before rendering when a non-empty body paragraph lacks sentence rows.
- Mapping divergence rules remain probe-observed AAT-to-parser-IR divergences. Synthetic parser-IR additions live in a dedicated schema field, not in `transform_rule_descriptions`.

---

## Current Baseline

The previous parser-IR sentence propagation work is already on `main` and provides these facts:

- `ab-aat-to-parser-ir` emits `sentence_segmentation`, `sentences[]`, and optional `orthographic_annotations`.
- `ab-aat-to-parser-ir convert --ortho-annotations <file>` validates sidecar identity and tags overlapping sentence rows.
- ABC schema accepts parser-IR sentence and orthographic fields.
- ABC validation has sentence tiling checks in `parser-ir-sentence-coherence-errors`.
- ABC TEI rendering wraps body sentence rows in `<s>` and preserves ruby readings inside `<ruby>`.
- Existing reports:
  - `ab-validator/docs/reports/2026-07-07-sentence-splitter-compatibility.md`
  - `ab-validator/docs/reports/2026-07-07-parser-ir-synthetic-evidence.md`

This plan starts from that baseline and fills the production gaps.

## File Responsibilities

- `ab-validator/crates/ab-aat-to-parser-ir/src/ortho_detect.rs`: creates production `OrthoAnnotationsBundle` values from AAT through parser-IR sentence coordinates.
- `ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs`: keeps sentence/node visible-text helpers reusable by both conversion and ortho detection.
- `ab-validator/crates/ab-aat-to-parser-ir/src/audit.rs`: reports corpus sentence-boundary projection failures with structured counts and samples.
- `abc/src/abc/tools/parser_ir_sentence_policy.clj`: shared ABC sentence-evidence admission and coherence logic, used by validation and materialization.
- `abc/src/abc/tools/materialize_publication.clj`: refuses publication rendering when sentence evidence is absent or incoherent.
- `abc/schemas/aat-parser-ir-mapping.schema.json`: records synthetic parser-IR additions outside divergence rules.
- `ab-validator/crates/ab-morph-run/src/oracle/ruby_contract.rs`: typed contract for ruby reading oracle evidence payloads.
- `ab-validator/tests/parser-ir-ortho-publication-smoke.sh`: end-to-end detector -> parser-IR -> ABC TEI smoke test.

---

### Task 1: Production Orthographic Annotation Sidecar Producer

**Files:**
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/Cargo.toml`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/lib.rs`
- Create: `ab-validator/crates/ab-aat-to-parser-ir/src/ortho_detect.rs`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/main.rs`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs`

**Interfaces:**
- Consumes:
  - `PreparedConverter::new(mapping, schemas)?.convert(aat, ConversionOptions::default())`
  - `ab_ortho_detect::OrthoDetector`
  - `ab_ortho_detect::heuristic::HeuristicV1`
- Produces:
  - `pub fn detect_orthographic_annotations(aat: Value, mapping: MappingDocument, schemas: SchemaSet, detector: &dyn ab_ortho_detect::OrthoDetector) -> anyhow::Result<OrthoAnnotationsBundle>`
  - CLI subcommand:

```text
ab-aat-to-parser-ir detect-ortho-annotations \
  --aat <AAT_JSON> \
  --mapping <MAPPING_JSON> \
  --ortho-annotations-out <OUTPUT_JSON> \
  [--abc-root <ABC_SCHEMA_ROOT>]
```

- [ ] **Step 1: Add deterministic library test for sidecar coordinates**

Add this test to `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs`:

```rust
struct AlwaysNormalizeDetector;

impl ab_ortho_detect::OrthoDetector for AlwaysNormalizeDetector {
    fn detector_id(&self) -> ab_ortho_detect::OrthoDetectorId {
        ab_ortho_detect::OrthoDetectorId::HeuristicV1
    }

    fn detect(&self, sentences: &[ab_plaintext::SentenceSpan<'_>]) -> Vec<ab_ortho_detect::OrthoAnnotation> {
        sentences
            .iter()
            .map(|sentence| ab_ortho_detect::OrthoAnnotation {
                source_byte_range: sentence.byte_offset..sentence.byte_offset + sentence.text.len(),
                normalized_text: sentence.text.replace('ハ', "は").replace('デ', "で").replace('ア', "あ"),
                kind: ab_ortho_detect::OrthoNormalization::ScriptKatakanaToHiragana,
                confidence: None,
            })
            .collect()
    }
}

#[test]
fn detect_orthographic_annotations_uses_parser_ir_sentence_coordinates() {
    let (schemas, mapping) = schemas_and_mapping();
    let bundle = ab_aat_to_parser_ir::ortho_detect::detect_orthographic_annotations(
        include_fixture_json("sentence-segmentation-input.aat.json"),
        mapping,
        schemas,
        &AlwaysNormalizeDetector,
    )
    .unwrap();

    assert_eq!(bundle.work_id, "000000");
    assert_eq!(
        bundle.work_content_hash,
        "sha256:1111111111111111111111111111111111111111111111111111111111111111"
    );
    assert_eq!(
        bundle.coordinate_system,
        ab_aat_to_parser_ir::ortho_annotations::OrthoCoordinateSystem::DecodedUtf8
    );
    assert_eq!(bundle.annotations.len(), 3);
    assert_eq!(bundle.annotations[0].source_byte_range, 0..24);
    assert_eq!(bundle.annotations[1].source_byte_range, 24..48);
    assert_eq!(bundle.annotations[2].source_byte_range, 48..63);
}
```

- [ ] **Step 2: Run the failing test**

Run:

```bash
cargo test --manifest-path ab-validator/Cargo.toml -p ab-aat-to-parser-ir -- detect_orthographic_annotations_uses_parser_ir_sentence_coordinates
```

Expected: FAIL because `ab_aat_to_parser_ir::ortho_detect` does not exist.

- [ ] **Step 3: Add analyzer dependency for the production CLI**

In `ab-validator/crates/ab-aat-to-parser-ir/Cargo.toml`, add:

```toml
ab-morph-analyzers = { workspace = true }
```

Keep the existing `ab-ortho-detect` dependency; do not move `OrthoAnnotationsBundle` into `ab-ortho-detect`, because that would create ownership confusion around the parser-IR converter contract.

- [ ] **Step 4: Expose sentence visible-text helpers**

In `ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs`, rename the private helper:

```rust
fn visible_text(node: &Value) -> Result<String>
```

to:

```rust
pub(crate) fn parser_ir_node_visible_text(node: &Value) -> Result<String>
```

Update the existing call in `paragraph_visible_text` to call `parser_ir_node_visible_text(node)?`.

- [ ] **Step 5: Add `ortho_detect.rs`**

Create `ab-validator/crates/ab-aat-to-parser-ir/src/ortho_detect.rs`:

```rust
use ab_ortho_detect::OrthoDetector;
use ab_plaintext::SentenceSpan;
use anyhow::{Context, Result};
use serde_json::Value;

use crate::{
    ConversionOptions, MappingDocument, PreparedConverter, SchemaSet,
    ortho_annotations::{OrthoAnnotationsBundle, OrthoCoordinateSystem},
};

pub fn detect_orthographic_annotations(
    aat: Value,
    mapping: MappingDocument,
    schemas: SchemaSet,
    detector: &dyn OrthoDetector,
) -> Result<OrthoAnnotationsBundle> {
    let work_id = aat
        .get("work_id")
        .and_then(Value::as_str)
        .context("AAT missing string work_id")?
        .to_owned();
    let work_content_hash = aat
        .pointer("/meta/source_hash")
        .and_then(Value::as_str)
        .context("AAT missing string meta.source_hash")?
        .to_owned();

    let output = PreparedConverter::new(mapping, schemas)?.convert(
        aat,
        ConversionOptions::default(),
    )?;

    let nodes = output
        .parser_ir
        .get("nodes")
        .and_then(Value::as_array)
        .context("parser-IR missing nodes[]")?;
    let sentences = output
        .parser_ir
        .get("sentences")
        .and_then(Value::as_array)
        .context("parser-IR missing sentences[]")?;

    let mut owned_sentence_text = Vec::with_capacity(sentences.len());
    let mut sentence_offsets = Vec::with_capacity(sentences.len());
    for sentence in sentences {
        let start = sentence
            .pointer("/node_range/start")
            .and_then(Value::as_u64)
            .context("sentence missing node_range.start")? as usize;
        let end = sentence
            .pointer("/node_range/end")
            .and_then(Value::as_u64)
            .context("sentence missing node_range.end")? as usize;
        let byte_offset = sentence
            .pointer("/span/start")
            .and_then(Value::as_u64)
            .context("sentence missing span.start")? as usize;
        let text = nodes[start..end]
            .iter()
            .map(crate::sentences::parser_ir_node_visible_text)
            .collect::<Result<Vec<_>>>()?
            .concat();
        sentence_offsets.push(byte_offset);
        owned_sentence_text.push(text);
    }

    let spans = owned_sentence_text
        .iter()
        .zip(sentence_offsets.iter())
        .map(|(text, byte_offset)| SentenceSpan {
            text,
            byte_offset: *byte_offset,
            char_offset: 0,
        })
        .collect::<Vec<_>>();

    Ok(OrthoAnnotationsBundle {
        work_id,
        work_content_hash,
        coordinate_system: OrthoCoordinateSystem::DecodedUtf8,
        detector_id: detector.detector_id(),
        annotations: detector.detect(&spans),
    })
}
```

- [ ] **Step 6: Export the module**

In `ab-validator/crates/ab-aat-to-parser-ir/src/lib.rs`, add:

```rust
pub mod ortho_detect;
```

- [ ] **Step 7: Add the CLI subcommand**

In `ab-validator/crates/ab-aat-to-parser-ir/src/main.rs`, add a `Command` variant:

```rust
DetectOrthoAnnotations {
    #[arg(long)]
    aat: PathBuf,
    #[arg(long)]
    mapping: PathBuf,
    #[arg(long)]
    ortho_annotations_out: PathBuf,
    #[arg(long)]
    abc_root: Option<PathBuf>,
},
```

Add this match arm:

```rust
Command::DetectOrthoAnnotations {
    aat,
    mapping,
    ortho_annotations_out,
    abc_root,
} => {
    let repo_root = resolve_repo_root(&mapping)?;
    let abc_root = abc_root
        .or_else(|| std::env::var_os("AB_ABC_ROOT").map(PathBuf::from))
        .unwrap_or_else(|| repo_root.join("data/abc-schemas"));
    let aat = ab_aat_to_parser_ir::schema::read_json(&aat)?;
    let mapping = MappingDocument::from_path(&mapping)?;
    let schemas = SchemaSet::load(&repo_root, &abc_root)?;
    let vibrato = std::sync::Arc::new(
        ab_morph_analyzers::VibratoAnalyzer::unidic_cwj_default()
            .context("detect-ortho-annotations requires AB_VIBRATO_DICT or the flake-provided Unidic CWJ dictionary")?,
    );
    let detector = ab_ortho_detect::heuristic::HeuristicV1::new(
        vibrato,
        ab_ortho_detect::heuristic::HeuristicConfig::default(),
    );
    let bundle = ab_aat_to_parser_ir::ortho_detect::detect_orthographic_annotations(
        aat,
        mapping,
        schemas,
        &detector,
    )?;
    std::fs::write(
        ortho_annotations_out,
        serde_json::to_string_pretty(&bundle)? + "\n",
    )?;
}
```

- [ ] **Step 8: Run focused tests**

Run:

```bash
cargo test --manifest-path ab-validator/Cargo.toml -p ab-aat-to-parser-ir -- detect_orthographic_annotations_uses_parser_ir_sentence_coordinates
cargo test --manifest-path ab-validator/Cargo.toml -p ab-aat-to-parser-ir -- cli_convert_with_ortho_annotations_emits_sentence_tags
```

Expected: PASS.

- [ ] **Step 9: Commit**

```bash
git add ab-validator/crates/ab-aat-to-parser-ir/Cargo.toml \
        ab-validator/crates/ab-aat-to-parser-ir/src/lib.rs \
        ab-validator/crates/ab-aat-to-parser-ir/src/ortho_detect.rs \
        ab-validator/crates/ab-aat-to-parser-ir/src/sentences.rs \
        ab-validator/crates/ab-aat-to-parser-ir/src/main.rs \
        ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs
git commit -m "feat(parser-ir): produce orthographic annotation sidecars"
```

---

### Task 2: Corpus Audit for Sentence Boundary Projection Failures

**Files:**
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/audit.rs`
- Modify: `ab-validator/docs/reports/2026-07-07-sentence-splitter-compatibility.md`

**Interfaces:**
- Consumes conversion failure messages from `FileOutcome::Failure`.
- Produces JSON summary field:

```json
"sentence_projection_failures": {
  "files_failed": 0,
  "atomic_boundary_failures_by_node_type": {},
  "other_sentence_projection_failures": 0,
  "samples": []
}
```

- [ ] **Step 1: Add classifier tests**

Add this unit test module to `ab-validator/crates/ab-aat-to-parser-ir/src/audit.rs`:

```rust
#[cfg(test)]
mod sentence_projection_audit_tests {
    use super::*;

    #[test]
    fn classifies_atomic_boundary_projection_failure() {
        let classified = classify_sentence_projection_failure(
            "sentence boundary falls inside atomic node ruby at byte 24",
        );
        assert_eq!(
            classified,
            Some(SentenceProjectionFailureClass::AtomicBoundary {
                node_type: "ruby".to_owned(),
                byte_offset: 24,
            })
        );
    }

    #[test]
    fn classifies_other_sentence_projection_failure() {
        let classified = classify_sentence_projection_failure(
            "sentence spans end at 24, expected paragraph end 30",
        );
        assert_eq!(
            classified,
            Some(SentenceProjectionFailureClass::Other)
        );
    }

    #[test]
    fn ignores_non_sentence_failures() {
        assert_eq!(
            classify_sentence_projection_failure("mapping schema hash mismatch"),
            None
        );
    }
}
```

- [ ] **Step 2: Run the failing tests**

Run:

```bash
cargo test --manifest-path ab-validator/Cargo.toml -p ab-aat-to-parser-ir -- sentence_projection_audit_tests
```

Expected: FAIL because `classify_sentence_projection_failure` is not defined.

- [ ] **Step 3: Add structured summary types and classifier**

Add these definitions near the other audit summary structs:

```rust
#[derive(Debug, Default, Serialize)]
struct SentenceProjectionFailureSummary {
    files_failed: u64,
    atomic_boundary_failures_by_node_type: BTreeMap<String, u64>,
    other_sentence_projection_failures: u64,
    samples: Vec<SentenceProjectionFailureSample>,
}

#[derive(Debug, Clone, Serialize)]
struct SentenceProjectionFailureSample {
    corpus: String,
    path: String,
    class: String,
    node_type: Option<String>,
    byte_offset: Option<u64>,
    message: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum SentenceProjectionFailureClass {
    AtomicBoundary { node_type: String, byte_offset: u64 },
    Other,
}

fn classify_sentence_projection_failure(message: &str) -> Option<SentenceProjectionFailureClass> {
    let marker = "sentence boundary falls inside atomic node ";
    if let Some(rest) = message.strip_prefix(marker) {
        if let Some((node_type, byte_text)) = rest.split_once(" at byte ") {
            if let Ok(byte_offset) = byte_text.parse::<u64>() {
                return Some(SentenceProjectionFailureClass::AtomicBoundary {
                    node_type: node_type.to_owned(),
                    byte_offset,
                });
            }
        }
    }
    if message.contains("sentence boundary")
        || message.contains("sentence spans")
        || message.contains("sentence node ranges")
        || message.contains("body paragraph has no sentence spans")
    {
        Some(SentenceProjectionFailureClass::Other)
    } else {
        None
    }
}
```

Add this field to `AuditSummary`:

```rust
sentence_projection_failures: SentenceProjectionFailureSummary,
```

- [ ] **Step 4: Populate the audit summary**

In the summary aggregation code in `run_audit`, when handling each `FileOutcome::Failure { message }`, call `classify_sentence_projection_failure(message)`. Update the summary exactly as follows:

```rust
if let Some(class) = classify_sentence_projection_failure(message) {
    sentence_projection_failures.files_failed += 1;
    let (class_name, node_type, byte_offset) = match class {
        SentenceProjectionFailureClass::AtomicBoundary {
            node_type,
            byte_offset,
        } => {
            *sentence_projection_failures
                .atomic_boundary_failures_by_node_type
                .entry(node_type.clone())
                .or_default() += 1;
            ("atomic_boundary".to_owned(), Some(node_type), Some(byte_offset))
        }
        SentenceProjectionFailureClass::Other => {
            sentence_projection_failures.other_sentence_projection_failures += 1;
            ("other_sentence_projection".to_owned(), None, None)
        }
    };
    if sentence_projection_failures.samples.len() < 20 {
        sentence_projection_failures.samples.push(SentenceProjectionFailureSample {
            corpus: result.corpus.clone(),
            path: result.relative_path.clone(),
            class: class_name,
            node_type,
            byte_offset,
            message: message.clone(),
        });
    }
}
```

Initialize `let mut sentence_projection_failures = SentenceProjectionFailureSummary::default();` before the aggregation loop and set the new field in the returned `AuditSummary`.

- [ ] **Step 5: Add report output**

In `write_report`, add a section:

```markdown
## Sentence Projection Failures

Files failed during sentence projection: {files_failed}

| Atomic Node Type | Failures |
|---|---:|
```

Render one row per `atomic_boundary_failures_by_node_type`, and render `Other sentence projection failures: {other_sentence_projection_failures}` below the table.

- [ ] **Step 6: Run focused tests**

Run:

```bash
cargo test --manifest-path ab-validator/Cargo.toml -p ab-aat-to-parser-ir -- sentence_projection_audit_tests
cargo test --manifest-path ab-validator/Cargo.toml -p ab-aat-to-parser-ir -- audit
```

Expected: PASS.

- [ ] **Step 7: Refresh the compatibility report with the audit field**

Run the fixture audit:

```bash
cargo run --manifest-path ab-validator/Cargo.toml -p ab-aat-to-parser-ir -- audit-corpus \
  --aat-dir ab-validator/tests/fixtures/aat-parser-ir \
  --mapping ab-validator/data/aat-to-parser-ir-mapping-v1.json \
  --summary-json /tmp/parser-ir-sentence-audit.summary.json \
  --report-md /tmp/parser-ir-sentence-audit.md \
  --abc-root ab-validator/data/abc-schemas
```

Append this line to `ab-validator/docs/reports/2026-07-07-sentence-splitter-compatibility.md` under `## Corpus Sample`:

```markdown
The fixture audit summary contains `sentence_projection_failures`; at this scope it reports zero atomic-boundary failures. Full-corpus runs must inspect that field before treating parser-IR sentence rows as publication-complete for a new adapter corpus.
```

- [ ] **Step 8: Commit**

```bash
git add ab-validator/crates/ab-aat-to-parser-ir/src/audit.rs \
        ab-validator/docs/reports/2026-07-07-sentence-splitter-compatibility.md
git commit -m "feat(parser-ir): audit sentence projection failures"
```

---

### Task 3: Publication Sentence Evidence Admission Gate

**Files:**
- Create: `abc/src/abc/tools/parser_ir_sentence_policy.clj`
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`
- Modify: `abc/src/abc/tools/materialize_publication.clj`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`
- Modify: `abc/test/abc/tools/materialize_publication_test.clj`

**Interfaces:**
- Produces:

```clojure
(abc.tools.parser-ir-sentence-policy/sentence-coherence-errors parser-ir) ;=> vector<string>
(abc.tools.parser-ir-sentence-policy/publication-sentence-evidence-errors parser-ir) ;=> vector<string>
(abc.tools.parser-ir-sentence-policy/ensure-publication-sentence-evidence! parser-ir) ;=> parser-ir or throws ex-info
```

- Consumes the existing validator behavior in `parser-ir-sentence-coherence-errors`; behavior must not change except for moving it into a shared namespace.

- [ ] **Step 1: Add materialization rejection test**

Add this test to `abc/test/abc/tools/materialize_publication_test.clj`:

```clojure
(deftest materialize-publication-requires-sentence-rows-test
  (let [work-dir (temp-dir "abc-materialize-publication-missing-sentences")
        out-dir (io/file work-dir "out")
        parser-ir-file (io/file work-dir "parser-ir.json")]
    (try
      (abc-json/write-deterministic-json-file!
       parser-ir-file
       {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
        "schema_hash" "sha256:0b495bb5c12c4d76482afefdaedb5464a74672ffbd5282f9c67d5f419d39a340"
        "source" {"work_content_hash" (files/example-hash "11")
                  "encoding" "UTF-8"
                  "normalization" "source"}
        "nodes" [{"type" "text"
                  "span" {"start" 0 "end" 24 "coordinate_system" "decoded_utf8"}
                  "text" "吾輩ハ猫デアル。"}]
        "paragraphs" [{"id" "p000000"
                       "span" {"start" 0 "end" 24 "coordinate_system" "decoded_utf8"}
                       "span_source" "direct"
                       "node_range" {"start" 0 "end" 1}
                       "role" "body"
                       "source_pointer" "blocks[0]"
                       "classification" "direct"}]
        "warnings" []
        "errors" []})
      (let [err (try
                  (materialize/materialize-publication!
                   {:parser-ir-path (str parser-ir-file)
                    :source-manifest-path "examples/v0/example-work/source.manifest.json"
                    :metadata-record-path "examples/v0/example-work/metadata-record.json"
                    :persons-dir "examples/v0/example-persons"
                    :output-dir out-dir
                    :generated-at generated-at})
                  nil
                  (catch clojure.lang.ExceptionInfo e
                    e))]
        (is (some? err))
        (is (= ["parser IR publication requires sentence_segmentation"
                "parser IR body paragraph p000000 has no sentence rows"]
               (:errors (ex-data err)))))
      (finally
        (delete-tree! work-dir)))))
```

- [ ] **Step 2: Run the failing test**

Run:

```bash
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: FAIL because `materialize-publication!` currently renders without calling the sentence admission gate.

- [ ] **Step 3: Create shared sentence policy namespace**

Create `abc/src/abc/tools/parser_ir_sentence_policy.clj`. Move the complete current definitions of `sentence-row-error-prefix`, `sentence-tiling-errors`, and `parser-ir-sentence-coherence-errors` from `abc/src/abc/tools/validate_design_bundle.clj` into this namespace. Rename the public moved function from `parser-ir-sentence-coherence-errors` to `sentence-coherence-errors`; keep every existing error string unchanged.

The new namespace must start with:

```clojure
(ns abc.tools.parser-ir-sentence-policy)
```

After the moved `sentence-coherence-errors` definition, add these new functions:

```clojure
(defn publication-sentence-evidence-errors [parser-ir]
  (let [coherence-errors (sentence-coherence-errors parser-ir)]
    (cond-> []
      (nil? (get parser-ir "sentence_segmentation"))
      (conj "parser IR publication requires sentence_segmentation")

      true
      (into coherence-errors))))

(defn ensure-publication-sentence-evidence! [parser-ir]
  (let [errors (publication-sentence-evidence-errors parser-ir)]
    (when (seq errors)
      (throw (ex-info "Parser-IR publication sentence evidence is invalid"
                      {:errors errors})))
    parser-ir))
```

Remove the moved private helper definitions from `validate_design_bundle.clj` after the new namespace compiles.

- [ ] **Step 4: Delegate validator to the shared namespace**

In `abc/src/abc/tools/validate_design_bundle.clj`, add:

```clojure
[abc.tools.parser-ir-sentence-policy :as sentence-policy]
```

Replace the old `parser-ir-sentence-coherence-errors` body with:

```clojure
(defn parser-ir-sentence-coherence-errors [parser-ir]
  (sentence-policy/sentence-coherence-errors parser-ir))
```

- [ ] **Step 5: Call the admission gate before publication rendering**

In `abc/src/abc/tools/materialize_publication.clj`, require the namespace:

```clojure
[abc.tools.parser-ir-sentence-policy :as sentence-policy]
```

Then, after reading `parser-ir`, bind:

```clojure
parser-ir (sentence-policy/ensure-publication-sentence-evidence! parser-ir)
```

inside the existing `let` in `materialize-publication!`, before `plaintext-result` and `tei-result` are computed.

- [ ] **Step 6: Update validation tests for the new namespace**

In `abc/test/abc/tools/validate_design_bundle_test.clj`, keep all existing sentence coherence assertions. Add one direct admission test:

```clojure
(deftest parser-ir-publication-sentence-evidence-errors-test
  (is (= ["parser IR publication requires sentence_segmentation"]
         (sentence-policy/publication-sentence-evidence-errors
          (dissoc sentence-parser-ir-fixture "sentence_segmentation")))))
```

Add the test namespace require:

```clojure
[abc.tools.parser-ir-sentence-policy :as sentence-policy]
```

- [ ] **Step 7: Run focused checks**

Run:

```bash
nix build .#checks.x86_64-linux.abc-clj-kondo --print-build-logs
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: PASS.

- [ ] **Step 8: Commit**

```bash
git add abc/src/abc/tools/parser_ir_sentence_policy.clj \
        abc/src/abc/tools/validate_design_bundle.clj \
        abc/src/abc/tools/materialize_publication.clj \
        abc/test/abc/tools/validate_design_bundle_test.clj \
        abc/test/abc/tools/materialize_publication_test.clj
git commit -m "feat(abc): require sentence evidence for publication"
```

---

### Task 4: Dedicated Synthetic Evidence Mapping Schema

**Files:**
- Modify: `abc/schemas/aat-parser-ir-mapping.schema.json`
- Modify: `abc/schemas/schema-contracts.json`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/src/mapping.rs`
- Modify: `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs`
- Modify: `ab-validator/data/aat-to-parser-ir-mapping-v1.json`
- Modify: `ab-validator/docs/reports/2026-07-07-parser-ir-synthetic-evidence.md`

**Interfaces:**
- Produces mapping document field:

```json
"synthetic_evidence_descriptions": [
  {
    "evidence_id": "SYN-001",
    "parser_ir_pointer": "sentence_segmentation",
    "source": "converter_policy",
    "action": "emit",
    "description": "Declares the sentence splitter identity and coordinate system used by parser-IR sentence rows."
  }
]
```

- Produces Rust field:

```rust
#[serde(default)]
pub synthetic_evidence_descriptions: Vec<SyntheticEvidenceDescription>,
```

- [ ] **Step 1: Add mapping schema test expectations**

In `ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs`, replace the assertion that sentence fields are absent from `transform_rule_descriptions` with this stronger assertion:

```rust
assert!(mapping.transform_rule_descriptions.iter().all(|rule| {
    !matches!(
        rule.parser_ir_pointer.as_deref(),
        Some("sentence_segmentation" | "sentences" | "orthographic_annotations")
    )
}));
let synthetic_pointers: std::collections::BTreeSet<_> = mapping
    .synthetic_evidence_descriptions
    .iter()
    .map(|entry| entry.parser_ir_pointer.as_str())
    .collect();
assert!(synthetic_pointers.contains("sentence_segmentation"));
assert!(synthetic_pointers.contains("sentences"));
assert!(synthetic_pointers.contains("sentences[].tags"));
assert!(synthetic_pointers.contains("orthographic_annotations"));
```

- [ ] **Step 2: Run the failing test**

Run:

```bash
cargo test --manifest-path ab-validator/Cargo.toml -p ab-aat-to-parser-ir -- mapping_preflight_accepts_checked_in_v2_artifact
```

Expected: FAIL because `MappingDocument` lacks `synthetic_evidence_descriptions`.

- [ ] **Step 3: Extend the JSON schema**

In `abc/schemas/aat-parser-ir-mapping.schema.json`, bump `"version"` from `"0.2.3"` to `"0.2.4"` and add a top-level property:

```json
"synthetic_evidence_descriptions": {
  "type": "array",
  "items": { "$ref": "#/$defs/syntheticEvidence" },
  "uniqueItems": true,
  "default": []
}
```

Add this definition under `$defs`:

```json
"syntheticEvidence": {
  "type": "object",
  "required": ["evidence_id", "parser_ir_pointer", "source", "action", "description"],
  "additionalProperties": false,
  "properties": {
    "evidence_id": { "type": "string", "pattern": "^SYN-[0-9]{3}$" },
    "parser_ir_pointer": { "type": "string", "minLength": 1 },
    "source": {
      "enum": [
        "converter_policy",
        "visible_body_text",
        "orthographic_sidecar",
        "sentence_overlap"
      ]
    },
    "action": { "enum": ["emit", "classify", "declare"] },
    "description": { "type": "string", "minLength": 1 }
  }
}
```

- [ ] **Step 4: Extend Rust mapping types and preflight**

In `ab-validator/crates/ab-aat-to-parser-ir/src/mapping.rs`, add:

```rust
pub synthetic_evidence_descriptions: Vec<SyntheticEvidenceDescription>,
```

with `#[serde(default)]` on the field.

Add:

```rust
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct SyntheticEvidenceDescription {
    pub evidence_id: String,
    pub parser_ir_pointer: String,
    pub source: String,
    pub action: String,
    pub description: String,
}
```

In `MappingDocument::preflight`, after transform rule validation, add duplicate checks:

```rust
let mut synthetic_ids = BTreeSet::new();
let mut synthetic_pointers = BTreeSet::new();
let transform_parser_pointers: BTreeSet<&str> = self
    .transform_rule_descriptions
    .iter()
    .filter_map(|rule| rule.parser_ir_pointer.as_deref())
    .collect();
for synthetic in &self.synthetic_evidence_descriptions {
    if !synthetic_ids.insert(synthetic.evidence_id.as_str()) {
        bail!("duplicate synthetic evidence id {}", synthetic.evidence_id);
    }
    if !synthetic_pointers.insert(synthetic.parser_ir_pointer.as_str()) {
        bail!(
            "duplicate synthetic evidence parser_ir_pointer {}",
            synthetic.parser_ir_pointer
        );
    }
    if transform_parser_pointers.contains(synthetic.parser_ir_pointer.as_str()) {
        bail!(
            "synthetic evidence {} overlaps transform_rule_descriptions parser_ir_pointer {}",
            synthetic.evidence_id,
            synthetic.parser_ir_pointer
        );
    }
}
```

- [ ] **Step 5: Update the checked-in mapping artifact**

In `ab-validator/data/aat-to-parser-ir-mapping-v1.json`:

1. Bump `"mapping_version"` from `"0.2.5"` to `"0.2.6"`.
2. Add:

```json
"synthetic_evidence_descriptions": [
  {
    "evidence_id": "SYN-001",
    "parser_ir_pointer": "sentence_segmentation",
    "source": "converter_policy",
    "action": "emit",
    "description": "Declares the sentence splitter identity, coverage, and decoded UTF-8 coordinate system used by parser-IR sentence rows."
  },
  {
    "evidence_id": "SYN-002",
    "parser_ir_pointer": "sentences",
    "source": "visible_body_text",
    "action": "emit",
    "description": "Records body paragraph sentence spans and node ranges derived from parser-IR visible text."
  },
  {
    "evidence_id": "SYN-003",
    "parser_ir_pointer": "sentences[].tags",
    "source": "sentence_overlap",
    "action": "classify",
    "description": "Marks sentence rows whose decoded byte span overlaps an orthographic annotation."
  },
  {
    "evidence_id": "SYN-004",
    "parser_ir_pointer": "orthographic_annotations",
    "source": "orthographic_sidecar",
    "action": "declare",
    "description": "Carries detector provenance and normalized text for tokenizer-facing katakana-to-hiragana normalization."
  }
]
```

- [ ] **Step 6: Refresh schema hashes and tests**

Run:

```bash
python scripts/abc_schema_contracts.py --profile abc --write
python ab-validator/scripts/schema_contracts.py --write
cargo test --manifest-path ab-validator/Cargo.toml -p ab-aat-to-parser-ir -- legacy_schema_hashes_match_mapping_artifact
```

Expected: the two Python commands update `abc/schemas/schema-contracts.json` and `ab-validator/data/abc-schemas/schema-contracts.json`. The cargo test fails with the new computed mapping schema hash. Copy the reported computed hash into:

- `ab-validator/data/aat-to-parser-ir-mapping-v1.json` field `mapping_schema_hash`
- `legacy_schema_hashes_match_mapping_artifact` expected mapping schema hash

Run the two Python commands and the cargo test again. If the test fails with a mapping document hash mismatch in a checked assertion, copy the new mapping hash into `ab-validator/docs/reports/2026-07-07-parser-ir-synthetic-evidence.md`.

- [ ] **Step 7: Update the synthetic evidence report**

Replace the final paragraph in `ab-validator/docs/reports/2026-07-07-parser-ir-synthetic-evidence.md` with:

```markdown
These fields are recorded in `synthetic_evidence_descriptions` in the mapping document. They remain absent from `transform_rule_descriptions` because they are not probe-observed AAT divergences.
```

Update the report's mapping version and mapping hash to the values from Step 6.

- [ ] **Step 8: Run focused checks**

Run:

```bash
cargo test --manifest-path ab-validator/Cargo.toml -p ab-aat-to-parser-ir -- mapping_preflight_accepts_checked_in_v2_artifact
cargo test --manifest-path ab-validator/Cargo.toml -p ab-aat-to-parser-ir -- legacy_schema_hashes_match_mapping_artifact
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: PASS.

- [ ] **Step 9: Commit**

```bash
git add abc/schemas/aat-parser-ir-mapping.schema.json \
        abc/schemas/schema-contracts.json \
        ab-validator/data/abc-schemas/schemas/aat-parser-ir-mapping.schema.json \
        ab-validator/data/abc-schemas/schema-contracts.json \
        ab-validator/crates/ab-aat-to-parser-ir/src/mapping.rs \
        ab-validator/crates/ab-aat-to-parser-ir/tests/integration.rs \
        ab-validator/data/aat-to-parser-ir-mapping-v1.json \
        ab-validator/docs/reports/2026-07-07-parser-ir-synthetic-evidence.md
git commit -m "feat(mapping): describe synthetic parser-ir evidence"
```

---

### Task 5: Ruby Reading Evidence Contract

**Files:**
- Create: `ab-validator/crates/ab-morph-run/src/oracle/ruby_contract.rs`
- Modify: `ab-validator/crates/ab-morph-run/src/oracle/mod.rs`
- Modify: `ab-validator/crates/ab-morph-run/src/oracle/ruby.rs`
- Modify: `ab-validator/docs/superpowers/specs/2026-07-07-ruby-oracle-design.md`
- Create: `ab-validator/docs/superpowers/specs/2026-07-07-ruby-reading-evidence-contract.md`

**Interfaces:**
- Produces typed JSON contract for `NwayRegionOracleEvidenceRow.evidence_detail` when `oracle_source == "ruby"`.
- Keeps the existing JSON keys:
  - `ruby_base`
  - `ruby_reading`
  - `ruby_reading_norm`
  - `classification`
  - `per_analyzer`
  - `per_analyzer.<analyzer>.reading`
  - `per_analyzer.<analyzer>.norm`
  - `per_analyzer.<analyzer>.match`
  - `per_analyzer.<analyzer>.align`

- [ ] **Step 1: Add contract round-trip test**

In `ab-validator/crates/ab-morph-run/src/oracle/ruby.rs`, add this assertion to `ruby_reading_does_not_change_base_span_alignment` after `rows` is produced:

```rust
let detail: super::ruby_contract::RubyReadingEvidenceDetail =
    serde_json::from_str(&rows[0].evidence_detail).unwrap();
assert_eq!(detail.ruby_base, "名前");
assert_eq!(detail.ruby_reading, "めいしょう");
assert_eq!(detail.ruby_reading_norm, "めいしょー");
assert_eq!(rows[0].projected_char_start, 0);
assert_eq!(rows[0].projected_char_end, 2);
```

- [ ] **Step 2: Run the failing test**

Run:

```bash
cargo test --manifest-path ab-validator/Cargo.toml -p ab-morph-run -- ruby_reading_does_not_change_base_span_alignment
```

Expected: FAIL because `ruby_contract::RubyReadingEvidenceDetail` does not exist.

- [ ] **Step 3: Add the typed contract module**

Create `ab-validator/crates/ab-morph-run/src/oracle/ruby_contract.rs`:

```rust
use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) struct RubyReadingEvidenceDetail {
    pub ruby_base: String,
    pub ruby_reading: String,
    pub ruby_reading_norm: String,
    pub classification: RubyOracleClassification,
    pub per_analyzer: BTreeMap<String, AnalyzerRubyReadingEvidence>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum RubyOracleClassification {
    Resolved,
    NonstandardRuby,
    NoComparableReading,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) struct AnalyzerRubyReadingEvidence {
    pub reading: Option<String>,
    pub norm: Option<String>,
    #[serde(rename = "match")]
    pub matches: bool,
    pub align: RubyReadingAlignment,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum RubyReadingAlignment {
    Exact,
    BoundaryMisalign,
    NoReading,
}
```

- [ ] **Step 4: Export the module**

In `ab-validator/crates/ab-morph-run/src/oracle/mod.rs`, add:

```rust
pub(crate) mod ruby_contract;
```

- [ ] **Step 5: Serialize typed evidence from the oracle**

In `ab-validator/crates/ab-morph-run/src/oracle/ruby.rs`, replace the loose `serde_json::Map` detail construction with typed structs:

```rust
use super::ruby_contract::{
    AnalyzerRubyReadingEvidence, RubyOracleClassification, RubyReadingAlignment,
    RubyReadingEvidenceDetail,
};
```

Map align strings with:

```rust
fn reading_alignment(value: &str) -> RubyReadingAlignment {
    match value {
        "exact" => RubyReadingAlignment::Exact,
        "boundary-misalign" => RubyReadingAlignment::BoundaryMisalign,
        "no-reading" => RubyReadingAlignment::NoReading,
        other => panic!("unknown ruby reading alignment {other}"),
    }
}
```

Map classification with:

```rust
let classification_enum = match classification {
    "resolved" => RubyOracleClassification::Resolved,
    "nonstandard_ruby" => RubyOracleClassification::NonstandardRuby,
    "no_comparable_reading" => RubyOracleClassification::NoComparableReading,
    other => panic!("unknown ruby oracle classification {other}"),
};
```

Build:

```rust
let evidence_detail = serde_json::to_string(&RubyReadingEvidenceDetail {
    ruby_base: base.base.clone(),
    ruby_reading: base.reading.clone(),
    ruby_reading_norm: ruby_norm.clone(),
    classification: classification_enum,
    per_analyzer: detail,
})?;
```

Because `adjudicate` currently returns `Vec<NwayRegionOracleEvidenceRow>` without `Result`, keep serialization infallible by using:

```rust
let evidence_detail = serde_json::to_string(&detail)
    .expect("ruby reading evidence detail serializes");
```

- [ ] **Step 6: Document the contract**

Create `ab-validator/docs/superpowers/specs/2026-07-07-ruby-reading-evidence-contract.md`:

```markdown
# Ruby Reading Evidence Contract

**Date:** 2026-07-07
**Owner:** ab-validator

Ruby readings are analyzer-evaluation evidence. They do not replace parser-IR visible text, sentence splitting input, or tokenizer input in the publication pipeline.

## Evidence Row

The contract applies when `nway_region_oracle_evidence.oracle_source == "ruby"`.

| Field | Meaning |
|---|---|
| `projected_char_start`, `projected_char_end` | Ruby base span in projected visible text coordinates. |
| `classification` | `resolved`, `nonstandard_ruby`, or `no_comparable_reading`. |
| `evidence_detail.ruby_base` | Source ruby base text used for analyzer span alignment. |
| `evidence_detail.ruby_reading` | Editor-provided ruby reading. |
| `evidence_detail.ruby_reading_norm` | Canonicalized reading for comparison. |
| `evidence_detail.per_analyzer.*.reading` | Analyzer reading concatenated over the base span, or null. |
| `evidence_detail.per_analyzer.*.norm` | Canonicalized analyzer reading, or null. |
| `evidence_detail.per_analyzer.*.match` | Whether the analyzer reading equals the normalized ruby reading. |
| `evidence_detail.per_analyzer.*.align` | `exact`, `boundary-misalign`, or `no-reading`. |

## Boundary Rule

Analyzer span alignment uses the ruby base span only. A reading such as `名前《めいしょう》` can adjudicate whether an analyzer reading is better, but it must not change the sentence span, parser-IR node span, or tokenizer input text.
```

In `ab-validator/docs/superpowers/specs/2026-07-07-ruby-oracle-design.md`, add a short pointer to this contract near the evidence detail example.

- [ ] **Step 7: Run focused tests**

Run:

```bash
cargo test --manifest-path ab-validator/Cargo.toml -p ab-morph-run -- ruby
```

Expected: PASS.

- [ ] **Step 8: Commit**

```bash
git add ab-validator/crates/ab-morph-run/src/oracle/ruby_contract.rs \
        ab-validator/crates/ab-morph-run/src/oracle/mod.rs \
        ab-validator/crates/ab-morph-run/src/oracle/ruby.rs \
        ab-validator/docs/superpowers/specs/2026-07-07-ruby-oracle-design.md \
        ab-validator/docs/superpowers/specs/2026-07-07-ruby-reading-evidence-contract.md
git commit -m "feat(morph): type ruby reading evidence"
```

---

### Task 6: End-to-End Orthographic Publication Fixture

**Files:**
- Create: `ab-validator/tests/fixtures/aat-parser-ir/orthographic-publication.aat.json`
- Create: `ab-validator/tests/parser-ir-ortho-publication-smoke.sh`
- Modify: `ab-validator/flake.nix`
- Modify: `ab-validator/justfile`

**Interfaces:**
- Consumes:
  - `ab-aat-to-parser-ir detect-ortho-annotations`
  - `ab-aat-to-parser-ir convert --ortho-annotations`
  - `clojure -M:abc/materialize-publication`
- Produces:
  - parser-IR with `orthographic_annotations` and sentence tags
  - TEI with `<s type="orthographic-katakana">`
  - TEI header with `<normalization method="markup">`

- [ ] **Step 1: Add the fixture AAT**

Create `ab-validator/tests/fixtures/aat-parser-ir/orthographic-publication.aat.json`:

```json
{
  "version": 1,
  "work_id": "orthographic-publication-fixture",
  "meta": {
    "adapter": "fixture",
    "adapter_version": "fixture 0.1.0",
    "source_encoding": "utf-8",
    "source_hash": "sha256:abababababababababababababababababababababababababababababababab",
    "parse_complete": true,
    "warnings": []
  },
  "blocks": [
    {
      "kind": "paragraph",
      "content": [
        { "kind": "text", "value": "吾輩ハ猫デアル果テ。" },
        { "kind": "ruby", "base": "名前", "reading": "めいしょう" },
        { "kind": "text", "value": "はまだ無い。" }
      ]
    },
    {
      "kind": "paragraph",
      "content": [
        { "kind": "text", "value": "ここは次。" }
      ]
    }
  ]
}
```

- [ ] **Step 2: Add the smoke script**

Create `ab-validator/tests/parser-ir-ortho-publication-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source "$repo_root/tests/lib/smoke-env.sh"

abc_root="$(smoke_abc_root)"
aat_path="$repo_root/tests/fixtures/aat-parser-ir/orthographic-publication.aat.json"

if [[ ! -f "${AB_VIBRATO_DICT:-}" ]]; then
  echo "AB_VIBRATO_DICT must point at a Unidic CWJ vibrato dictionary" >&2
  exit 2
fi

out_dir="$(mktemp -d "${TMPDIR:-/tmp}/ab-parser-ir-ortho-publication.XXXXXX")"
ortho="$out_dir/ortho.json"
parser_ir="$out_dir/parser-ir.json"
divergence="$out_dir/divergence.json"
publication_dir="$out_dir/publication"

if [[ -n "${AB_AAT_TO_PARSER_IR_BIN:-}" ]]; then
  converter=("$AB_AAT_TO_PARSER_IR_BIN")
else
  converter=(cargo run --quiet --manifest-path "$repo_root/Cargo.toml" -p ab-aat-to-parser-ir --)
fi

"${converter[@]}" detect-ortho-annotations \
  --aat "$aat_path" \
  --mapping "$repo_root/data/aat-to-parser-ir-mapping-v1.json" \
  --ortho-annotations-out "$ortho" \
  --abc-root "$repo_root/data/abc-schemas"

jq -e '.detector_id == "HeuristicV1"' "$ortho" >/dev/null
jq -e '.coordinate_system == "decoded_utf8"' "$ortho" >/dev/null
jq -e '(.annotations | length) >= 1' "$ortho" >/dev/null

"${converter[@]}" convert \
  --aat "$aat_path" \
  --mapping "$repo_root/data/aat-to-parser-ir-mapping-v1.json" \
  --ortho-annotations "$ortho" \
  --parser-ir-out "$parser_ir" \
  --divergence-out "$divergence" \
  --abc-root "$repo_root/data/abc-schemas"

jq -e '.orthographic_annotations.detector_id == "HeuristicV1"' "$parser_ir" >/dev/null
jq -e 'any(.sentences[]; (.tags | index("orthographic-katakana")))' "$parser_ir" >/dev/null
jq -e 'all(.sentences[]; .span.coordinate_system == "decoded_utf8")' "$parser_ir" >/dev/null

(cd "$abc_root" && clojure -M:abc/materialize-publication \
  "$parser_ir" \
  examples/v0/example-work/metadata-record.json \
  examples/v0/example-persons \
  "$publication_dir" \
  --source-manifest examples/v0/example-work/source.manifest.json \
  --generated-at 2026-07-07T00:00:00Z)

jq -e '.status == "passed" and (.findings | length) == 0' "$publication_dir/tei-validation-result.json" >/dev/null
rg -n '<[A-Za-z0-9_-]+:normalization method="markup"' "$publication_dir/tei.xml" >/dev/null
rg -n '<[A-Za-z0-9_-]+:s type="orthographic-katakana"' "$publication_dir/tei.xml" >/dev/null
rg -n '<[A-Za-z0-9_-]+:ruby type="furigana"' "$publication_dir/tei.xml" >/dev/null
rg -n '<[A-Za-z0-9_-]+:rt>めいしょう</[A-Za-z0-9_-]+:rt>' "$publication_dir/tei.xml" >/dev/null

echo "parser-IR orthographic publication smoke ok: $out_dir"
```

Make it executable:

```bash
chmod +x ab-validator/tests/parser-ir-ortho-publication-smoke.sh
```

- [ ] **Step 3: Add just target**

In `ab-validator/justfile`, add near the other parser-IR publication smoke targets:

```make
parser-ir-ortho-publication-smoke:
	@bash "{{repo_root}}/tests/parser-ir-ortho-publication-smoke.sh"
```

- [ ] **Step 4: Add Nix smoke check**

In `ab-validator/flake.nix`, add:

```nix
parserIrOrthoPublicationSmokeCheck = mkSmokeCheck {
  name = "parser-ir-ortho-publication-smoke-check";
  testScript = "tests/parser-ir-ortho-publication-smoke.sh";
  nativeBuildInputs = [
    pkgs.clojure
    pkgs.jq
    pkgs.ripgrep
  ];
  extraEnv = {
    AB_ABC_ROOT = "${abcSchemaRootForNix}";
    AB_AAT_TO_PARSER_IR_BIN = "${abAatToParserIr}/bin/ab-aat-to-parser-ir";
    AB_VIBRATO_DICT = "${vibratoDictCwj}/share/vibrato/unidic-cwj-202512.dic.zst";
  };
  extraPreScript = stageAbcSchemas;
};
```

Add it to `checks` as:

```nix
parser-ir-ortho-publication-smoke = parserIrOrthoPublicationSmokeCheck;
```

- [ ] **Step 5: Run the smoke check**

Run:

```bash
bash ab-validator/tests/parser-ir-ortho-publication-smoke.sh
nix build .#checks.x86_64-linux.parser-ir-ortho-publication-smoke --print-build-logs
```

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add ab-validator/tests/fixtures/aat-parser-ir/orthographic-publication.aat.json \
        ab-validator/tests/parser-ir-ortho-publication-smoke.sh \
        ab-validator/flake.nix \
        ab-validator/justfile
git commit -m "test(parser-ir): smoke orthographic publication flow"
```

---

### Task 7: Final Verification and Documentation

**Files:**
- Modify: `ab-validator/docs/superpowers/specs/2026-07-07-parser-ir-sentence-segmentation-and-ortho-tei-design.md`
- Modify: `ab-validator/docs/superpowers/plans/2026-07-07-parser-ir-sentence-segmentation-and-ortho-tei-plan.md`

**Interfaces:**
- Produces final checked evidence that all follow-ups have landed and records the new production flow.

- [ ] **Step 1: Update the design status**

In `ab-validator/docs/superpowers/specs/2026-07-07-parser-ir-sentence-segmentation-and-ortho-tei-design.md`, add a `Follow-up Status` section:

```markdown
## Follow-up Status

Production follow-ups landed in `2026-07-07-parser-ir-orthographic-publication-followups.md`:

- ab-validator produces orthographic annotation sidecars from parser-IR sentence coordinates.
- Corpus conversion audits summarize sentence projection failures.
- ABC publication materialization requires sentence evidence before rendering.
- Mapping documents describe synthetic parser-IR evidence outside divergence rules.
- Ruby reading evidence has a typed oracle contract and remains separate from sentence splitting/tokenizer input replacement.
- The orthographic publication smoke test proves detector -> parser-IR -> ABC TEI propagation.
```

- [ ] **Step 2: Update the previous plan status**

At the top of `ab-validator/docs/superpowers/plans/2026-07-07-parser-ir-sentence-segmentation-and-ortho-tei-plan.md`, add:

```markdown
**Follow-up:** Production hardening work is tracked in `2026-07-07-parser-ir-orthographic-publication-followups.md`.
```

- [ ] **Step 3: Run full verification**

Run from the monorepo root:

```bash
cargo test --manifest-path ab-validator/Cargo.toml -p ab-aat-to-parser-ir
cargo test --manifest-path ab-validator/Cargo.toml -p ab-morph-run -- ruby
nix build .#checks.x86_64-linux.ab-validator-cargo-check --print-build-logs
nix build .#checks.x86_64-linux.ab-validator-cargo-clippy --print-build-logs
nix build .#checks.x86_64-linux.ab-validator-cargo-fmt --print-build-logs
nix build .#checks.x86_64-linux.abc-clj-kondo --print-build-logs
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
nix build .#checks.x86_64-linux.parser-ir-ortho-publication-smoke --print-build-logs
```

Expected: all commands pass.

- [ ] **Step 4: Run migration gate**

Run:

```bash
just validate-migration
```

Expected: PASS.

- [ ] **Step 5: Inspect git state**

Run:

```bash
git status --short
git log --oneline --decorate -n 8
```

Expected: only intentional committed changes exist; the working tree is clean.

- [ ] **Step 6: Commit docs if changed after verification**

```bash
git add ab-validator/docs/superpowers/specs/2026-07-07-parser-ir-sentence-segmentation-and-ortho-tei-design.md \
        ab-validator/docs/superpowers/plans/2026-07-07-parser-ir-sentence-segmentation-and-ortho-tei-plan.md
git commit -m "docs(parser-ir): record orthographic publication followups"
```

---

## Self-Review

- Spec coverage: all requested follow-ups map to tasks. Production ortho sidecar wiring is Task 1; corpus sentence-boundary audit is Task 2; mandatory publication sentence rows are Task 3; synthetic-evidence schema is Task 4; ruby reading evidence contract is Task 5; end-to-end publication fixture is Task 6.
- Existing implementation respected: the plan does not reimplement sentence projection, ABC TEI `<s>` rendering, ruby preservation, or basic schema acceptance because those are already on `main`.
- Risk controls: direct publication rendering is gated before TEI output; sidecar generation uses parser-IR sentence coordinates rather than standalone plaintext projection; ruby readings are typed as evidence and remain outside parser-IR text replacement.
- Verification: each task ends with focused tests and a commit, and Task 7 runs the Rust, Clojure, Nix, e2e, and migration gates.
