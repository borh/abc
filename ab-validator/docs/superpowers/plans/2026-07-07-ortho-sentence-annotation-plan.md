# Orthographic Sentence Annotations — ab-validator Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Serialize ortho-detect annotations into parser-IR output as an `orthographic_annotations` field, so ABC's TEI renderer can produce `<s type="orthographic-katakana">` elements after the ABC parser-IR schema accepts the field.

**Architecture:** The `ab-aat-to-parser-ir` CLI gains a `--ortho-annotations <path.json>` flag. The CLI reads a JSON bundle containing `{work_id, work_content_hash, coordinate_system, detector_id, annotations}`, then passes the parsed value into conversion; conversion validates the bundle against the AAT identity, verifies the loaded parser-IR schema declares `orthographic_annotations`, injects the value, and keeps normal output validation enabled. No sentence splitting in ab-validator — ABC owns that. No `<s>` rendering in ab-validator — ABC owns that. The deliverable is validated parser-IR evidence.

**Tech Stack:** Rust (edition 2024), `serde_json`, `anyhow`, `ab-ortho-detect` types, `ab-aat-to-parser-ir` crate.

## Global Constraints

- **PRECONDITION:** ABC must accept `orthographic_annotations` into `parser-ir.schema.json` (currently `additionalProperties: false`) and ab-validator must mirror the matching parser-IR schema plus mapping artifact target hash. This plan builds the ab-validator producer; ABC schema acceptance is the gate for production use.
- `<s>` rendering is ABC's concern. ab-validator does NOT produce TEI. The deliverable is parser-IR JSON with an `orthographic_annotations` field.
- Reuse `OrthoAnnotation` and `OrthoDetectorId` from `ab-ortho-detect` — no new annotation record or detector-id serialization.
- `AatProjection.ortho_normalizations` stays unchanged (reserved for future `<choice>` rendering).
- The `orthographic_annotations` JSON file is a standalone input to the converter (not embedded in AAT). It must include `work_id`, `work_content_hash`, and `coordinate_system` so the converter can reject mismatches.
- Do not skip parser-IR output validation. If the loaded schema does not declare `orthographic_annotations`, fail with a clear precondition error.
- Rust edition 2024. All crate code in `crates/`.
- Tests: `cargo test -p ab-aat-to-parser-ir`.

---

### Task 1: Contract fixture — golden parser-IR with orthographic_annotations

**Files:**
- Create: `crates/ab-aat-to-parser-ir/tests/fixtures/ortho-annotations-input.json` (the annotation file)
- Create: `crates/ab-aat-to-parser-ir/tests/fixtures/ortho-annotations-expected.json` (the expected parser-IR output fragment)

**Why this task exists:** The plan's first task is contract-focused. Define the exact JSON shape, serialization names, and coordinate system before any code. The fixture serves as the acceptance test for Tasks 2-3.

**Interfaces:**
- Produces: two JSON fixture files that Task 3's integration test consumes.

- [ ] **Step 1: Create the annotation input fixture**

Create `crates/ab-aat-to-parser-ir/tests/fixtures/ortho-annotations-input.json`:

```json
{
  "work_id": "000000",
  "work_content_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
  "coordinate_system": "decoded_utf8",
  "detector_id": "HeuristicV1",
  "annotations": [
    {
      "source_byte_range": { "start": 0, "end": 24 },
      "normalized_text": "吾輩は猫である。",
      "kind": "ScriptKatakanaToHiragana",
      "confidence": null
    },
    {
      "source_byte_range": { "start": 24, "end": 48 },
      "normalized_text": "名前はまだ無い。",
      "kind": "ScriptKatakanaToHiragana",
      "confidence": null
    }
  ]
}
```

Key contract decisions captured here:
- `work_id` must equal the AAT `work_id`.
- `work_content_hash` must equal AAT `meta.source_hash` and parser-IR `source.work_content_hash`.
- `coordinate_system` is `"decoded_utf8"` — byte offsets in decoded UTF-8 visible text, aligned with parser-IR spans.
- `detector_id` uses `OrthoDetectorId` serde: `"HeuristicV1"` for v1, or `{"MlLogisticRegression":{"model_hash":"..."}}` for the ML detector.
- `source_byte_range` uses `{start, end}` — byte offsets in the original document text. Zero-based, half-open.
- `normalized_text` is the kata→hira version (informational, not rendered in TEI).
- `kind` is `"ScriptKatakanaToHiragana"` (v1). `"HistoricalToModern"` reserved for future.
- `confidence` is `null` for heuristic, an integer 0–100 for ML.

- [ ] **Step 2: Create the expected parser-IR output fragment**

Create `crates/ab-aat-to-parser-ir/tests/fixtures/ortho-annotations-expected.json`:

```json
{
  "work_id": "000000",
  "work_content_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
  "coordinate_system": "decoded_utf8",
  "detector_id": "HeuristicV1",
  "annotations": [
    {
      "source_byte_range": { "start": 0, "end": 24 },
      "normalized_text": "吾輩は猫である。",
      "kind": "ScriptKatakanaToHiragana",
      "confidence": null
    },
    {
      "source_byte_range": { "start": 24, "end": 48 },
      "normalized_text": "名前はまだ無い。",
      "kind": "ScriptKatakanaToHiragana",
      "confidence": null
    }
  ]
}
```

This is identical to the input — the converter passes annotations through
unchanged. The test in Task 3 asserts the parser-IR output's
`orthographic_annotations` field matches this fixture exactly.

- [ ] **Step 3: Commit**

```bash
git add crates/ab-aat-to-parser-ir/tests/fixtures/ortho-annotations-input.json \
        crates/ab-aat-to-parser-ir/tests/fixtures/ortho-annotations-expected.json
git commit -m "test(parser-ir): add orthographic_annotations contract fixture

Golden JSON defining the orthographic_annotations shape in parser-IR:
{work_id, work_content_hash, coordinate_system, detector_id,
annotations: [OrthoAnnotation]}. Serves as acceptance test for the
--ortho-annotations converter flag."
```

---

### Task 2: Serialization — Rust types for the annotation wrapper

**Files:**
- Create: `crates/ab-aat-to-parser-ir/src/ortho_annotations.rs`
- Modify: `crates/ab-aat-to-parser-ir/src/lib.rs` (add `mod ortho_annotations;`)
- Modify: `crates/ab-aat-to-parser-ir/Cargo.toml` (add `ab-ortho-detect`)

**Why:** The `orthographic_annotations` field wraps `OrthoAnnotation` with
detector provenance, source identity, and coordinate-system declaration. A
small Rust module handles serde and identity validation. This stays separate
from conversion so tests can exercise the contract without running the whole
AAT converter.

**Interfaces:**
- Produces: `OrthoAnnotationsBundle` struct and `read_ortho_annotations_bundle(path)`.
- Consumed by: Task 3 (`ConversionOptions.orthographic_annotations`).

- [ ] **Step 1: Add the crate dependency**

In `crates/ab-aat-to-parser-ir/Cargo.toml`, add under `[dependencies]`:

```toml
ab-ortho-detect = { workspace = true }
```

- [ ] **Step 2: Write the failing tests**

Add to the end of `crates/ab-aat-to-parser-ir/src/ortho_annotations.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrips_through_json() {
        let input = serde_json::json!({
            "work_id": "000000",
            "work_content_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
            "coordinate_system": "decoded_utf8",
            "detector_id": "HeuristicV1",
            "annotations": [
                {
                    "source_byte_range": { "start": 0, "end": 24 },
                    "normalized_text": "吾輩は猫である。",
                    "kind": "ScriptKatakanaToHiragana",
                    "confidence": null
                }
            ]
        });
        let parsed: OrthoAnnotationsBundle = serde_json::from_value(input.clone()).unwrap();
        assert_eq!(parsed.work_id, "000000");
        assert_eq!(parsed.coordinate_system, OrthoCoordinateSystem::DecodedUtf8);
        assert_eq!(parsed.detector_id, ab_ortho_detect::OrthoDetectorId::HeuristicV1);
        assert_eq!(parsed.annotations.len(), 1);
        assert_eq!(parsed.annotations[0].source_byte_range.start, 0);
        assert_eq!(parsed.annotations[0].source_byte_range.end, 24);
        assert_eq!(parsed.annotations[0].normalized_text, "吾輩は猫である。");

        let output = serde_json::to_value(&parsed).unwrap();
        assert_eq!(output, input);
    }

    #[test]
    fn roundtrips_ml_detector_id_shape() {
        let input = serde_json::json!({
            "work_id": "000000",
            "work_content_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
            "coordinate_system": "decoded_utf8",
            "detector_id": {
                "MlLogisticRegression": {
                    "model_hash": "sha256:2222222222222222222222222222222222222222222222222222222222222222"
                }
            },
            "annotations": []
        });
        let parsed: OrthoAnnotationsBundle = serde_json::from_value(input.clone()).unwrap();
        assert!(matches!(
            parsed.detector_id,
            ab_ortho_detect::OrthoDetectorId::MlLogisticRegression { .. }
        ));
        assert_eq!(serde_json::to_value(&parsed).unwrap(), input);
    }

    #[test]
    fn rejects_missing_identity_fields() {
        let input = serde_json::json!({
            "detector_id": "HeuristicV1",
            "annotations": []
        });
        let err = serde_json::from_value::<OrthoAnnotationsBundle>(input).unwrap_err();
        let text = err.to_string();
        assert!(text.contains("work_id") || text.contains("work_content_hash"));
    }

    #[test]
    fn validates_identity_against_aat() {
        let bundle: OrthoAnnotationsBundle = serde_json::from_value(serde_json::json!({
            "work_id": "000000",
            "work_content_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
            "coordinate_system": "decoded_utf8",
            "detector_id": "HeuristicV1",
            "annotations": []
        }))
        .unwrap();
        let aat = serde_json::json!({
            "work_id": "000000",
            "meta": {
                "source_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111"
            }
        });

        bundle.validate_against_aat(&aat).unwrap();
    }

    #[test]
    fn rejects_mismatched_aat_identity() {
        let bundle: OrthoAnnotationsBundle = serde_json::from_value(serde_json::json!({
            "work_id": "000000",
            "work_content_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
            "coordinate_system": "decoded_utf8",
            "detector_id": "HeuristicV1",
            "annotations": []
        }))
        .unwrap();
        let aat = serde_json::json!({
            "work_id": "000001",
            "meta": {
                "source_hash": "sha256:2222222222222222222222222222222222222222222222222222222222222222"
            }
        });

        let err = bundle.validate_against_aat(&aat).unwrap_err().to_string();
        assert!(err.contains("work_id mismatch"));
    }
}
```

- [ ] **Step 3: Verify tests fail**

```bash
cd /home/bor/Projects/soranoha/ab-validator
cargo test -p ab-aat-to-parser-ir -- ortho_annotations 2>&1 | tail -5
```
Expected: FAIL — `OrthoAnnotationsBundle` not defined.

- [ ] **Step 4: Write the implementation**

Create `crates/ab-aat-to-parser-ir/src/ortho_annotations.rs`:

```rust
//! Serialization contract for the `orthographic_annotations` parser-IR field.
//! See `docs/superpowers/specs/2026-07-07-ortho-sentence-annotation-design.md`.

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum OrthoCoordinateSystem {
    #[serde(rename = "decoded_utf8")]
    DecodedUtf8,
}

/// The `orthographic_annotations` field as it appears in parser-IR JSON.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OrthoAnnotationsBundle {
    /// AAT work_id for the text these annotations describe.
    pub work_id: String,

    /// AAT meta.source_hash / parser-IR source.work_content_hash.
    pub work_content_hash: String,

    /// Byte-offset coordinate system for annotation ranges.
    pub coordinate_system: OrthoCoordinateSystem,

    /// Detector that produced these annotations. Uses the existing
    /// `OrthoDetectorId` serde shape.
    pub detector_id: ab_ortho_detect::OrthoDetectorId,

    /// Sentence-level orthographic annotations.
    /// Byte ranges are in original-document coordinates.
    pub annotations: Vec<ab_ortho_detect::OrthoAnnotation>,
}

impl OrthoAnnotationsBundle {
    /// Validate that this standalone bundle belongs to the AAT document being
    /// converted.
    pub fn validate_against_aat(&self, aat: &Value) -> Result<()> {
        let aat_work_id = aat
            .get("work_id")
            .and_then(Value::as_str)
            .context("AAT missing string work_id")?;
        let aat_source_hash = aat
            .pointer("/meta/source_hash")
            .and_then(Value::as_str)
            .context("AAT missing string meta.source_hash")?;

        if self.work_id != aat_work_id {
            bail!(
                "orthographic_annotations work_id mismatch: bundle={} aat={}",
                self.work_id,
                aat_work_id
            );
        }
        if self.work_content_hash != aat_source_hash {
            bail!(
                "orthographic_annotations work_content_hash mismatch: bundle={} aat={}",
                self.work_content_hash,
                aat_source_hash
            );
        }
        Ok(())
    }
}

/// Load an `OrthoAnnotationsBundle` from a JSON file path.
pub fn read_ortho_annotations_bundle(path: &std::path::Path) -> Result<OrthoAnnotationsBundle> {
    let bytes = std::fs::read(path)?;
    let bundle: OrthoAnnotationsBundle = serde_json::from_slice(&bytes)?;
    Ok(bundle)
}
```

- [ ] **Step 5: Register the module**

In `crates/ab-aat-to-parser-ir/src/lib.rs`, add near the other `pub mod` lines:

```rust
pub mod ortho_annotations;
```

- [ ] **Step 6: Run tests**

```bash
cd /home/bor/Projects/soranoha/ab-validator
cargo test -p ab-aat-to-parser-ir -- ortho_annotations 2>&1 | tail -5
```
Expected: 5 passed.

- [ ] **Step 7: Commit**

```bash
git add crates/ab-aat-to-parser-ir/src/ortho_annotations.rs \
        crates/ab-aat-to-parser-ir/src/lib.rs \
        crates/ab-aat-to-parser-ir/Cargo.toml
git commit -m "feat(parser-ir): add OrthoAnnotationsBundle serialization type

{work_id, work_content_hash, coordinate_system, detector_id,
annotations: [OrthoAnnotation]} — the contract shape for the
orthographic_annotations parser-IR field. Includes JSON round-trip and
AAT identity validation tests."
```

---

### Task 3: Wire `--ortho-annotations` into the converter CLI + convert logic

**Files:**
- Modify: `crates/ab-aat-to-parser-ir/src/main.rs` (add CLI flag)
- Modify: `crates/ab-aat-to-parser-ir/src/convert.rs` (add field to parser_ir output)
- Modify: `crates/ab-aat-to-parser-ir/tests/integration.rs` (schema-gated integration tests)

**Why:** The converter needs a way to receive the ortho annotations file
and include it in parser-IR output. The CLI owns file I/O; conversion owns
identity validation, schema precondition checking, field injection, and normal
parser-IR validation.

**Schema validation note:** The current parser-IR schema has
`additionalProperties: false`, so `orthographic_annotations` fails validation
until ABC adds the field. Do not skip validation. When the loaded schema lacks
the field, fail early with a message telling the caller to use an updated
ABC schema/mapping bundle.

**Interfaces:**
- Consumes: `OrthoAnnotationsBundle` from Task 2, fixture from Task 1.
- Produces: parser-IR JSON with `orthographic_annotations` field.

- [ ] **Step 1: Add `orthographic_annotations` to `ConversionOptions`**

In `crates/ab-aat-to-parser-ir/src/convert.rs`, add to `ConversionOptions`:

```rust
    /// Orthographic annotations to serialize into parser-IR.
    ///
    /// When provided, the loaded parser-IR schema must declare the
    /// `orthographic_annotations` field and normal output validation remains
    /// enabled.
    pub orthographic_annotations: Option<crate::ortho_annotations::OrthoAnnotationsBundle>,
```

Update `Default` impl:

```rust
impl Default for ConversionOptions {
    fn default() -> Self {
        Self {
            validate_input_aat: true,
            validate_output_parser_ir: true,
            orthographic_annotations: None,
        }
    }
}
```

- [ ] **Step 2: Add schema precondition helper and inject into parser_ir**

In `crates/ab-aat-to-parser-ir/src/convert.rs`, add this helper near
`convert_preflighted`:

```rust
fn ensure_schema_declares_orthographic_annotations(schema: &Value) -> Result<()> {
    if schema.pointer("/properties/orthographic_annotations").is_some() {
        Ok(())
    } else {
        bail!(
            "loaded parser-IR schema does not declare orthographic_annotations; \
             use an updated ABC schema/mapping bundle before passing \
             --ortho-annotations"
        )
    }
}
```

Then in `convert_preflighted`, change `let parser_ir = json!({...});` to
`let mut parser_ir = json!({...});` and inject after the JSON block:

```rust
    let mut parser_ir = json!({
        "schema_id": mapping.target_parser_ir_schema_id,
        "schema_hash": mapping.target_parser_ir_schema_hash,
        "derived_from": derived_from(&aat, mapping)?,
        "source": source,
        "nodes": nodes,
        "paragraphs": paragraphs,
        "warnings": warnings,
        "errors": [],
    });

    if let Some(orthographic_annotations) = options.orthographic_annotations {
        ensure_schema_declares_orthographic_annotations(&schemas.parser_ir_schema)?;
        orthographic_annotations.validate_against_aat(&aat)?;
        let value = serde_json::to_value(&orthographic_annotations)?;
        parser_ir
            .as_object_mut()
            .expect("parser_ir is an object")
            .insert("orthographic_annotations".to_owned(), value);
    }

    if options.validate_output_parser_ir {
        validate_value(&schemas.parser_ir_schema, &parser_ir, "parser-IR")?;
    }
```

- [ ] **Step 3: Add `--ortho-annotations` CLI flag**

In `crates/ab-aat-to-parser-ir/src/main.rs`, add to the `Convert` command struct:

```rust
        #[arg(long)]
        ortho_annotations: Option<PathBuf>,
```

And pass it through to `ConversionOptions` in the convert block:

```rust
            let orthographic_annotations = match ortho_annotations {
                Some(path) => Some(
                    ab_aat_to_parser_ir::ortho_annotations::read_ortho_annotations_bundle(&path)?,
                ),
                None => None,
            };
            let options = ConversionOptions {
                orthographic_annotations,
                ..Default::default()
            };
            let output = PreparedConverter::new(mapping, schemas)?
                .convert(aat, options)?;
```

- [ ] **Step 4: Write integration tests**

Add to `crates/ab-aat-to-parser-ir/tests/integration.rs`:

```rust
fn schemas_and_mapping_accepting_orthographic_annotations() -> (SchemaSet, MappingDocument) {
    let (mut schemas, mut mapping) = schemas_and_mapping();
    let props = schemas
        .parser_ir_schema
        .get_mut("properties")
        .and_then(Value::as_object_mut)
        .expect("parser-IR schema has properties object");
    props.insert(
        "orthographic_annotations".to_owned(),
        json!({
            "type": "object",
            "additionalProperties": false,
            "required": [
                "work_id",
                "work_content_hash",
                "coordinate_system",
                "detector_id",
                "annotations"
            ],
            "properties": {
                "work_id": { "type": "string" },
                "work_content_hash": { "type": "string", "pattern": "^sha256:[0-9a-f]{64}$" },
                "coordinate_system": { "const": "decoded_utf8" },
                "detector_id": {},
                "annotations": { "type": "array" }
            }
        }),
    );
    mapping.target_parser_ir_schema_hash = schema_hash(&schemas.parser_ir_schema).unwrap();
    (schemas, mapping)
}

fn ortho_fixture_bundle() -> ab_aat_to_parser_ir::ortho_annotations::OrthoAnnotationsBundle {
    serde_json::from_value(json!({
        "work_id": "000000",
        "work_content_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
        "coordinate_system": "decoded_utf8",
        "detector_id": "HeuristicV1",
        "annotations": [
            {
                "source_byte_range": { "start": 0, "end": 24 },
                "normalized_text": "吾輩は猫である。",
                "kind": "ScriptKatakanaToHiragana",
                "confidence": null
            },
            {
                "source_byte_range": { "start": 24, "end": 48 },
                "normalized_text": "名前はまだ無い。",
                "kind": "ScriptKatakanaToHiragana",
                "confidence": null
            }
        ]
    }))
    .unwrap()
}

fn ortho_fixture_aat() -> Value {
    json!({
            "version": 1,
            "work_id": "000000",
            "meta": {
                "adapter": "fixture",
                "adapter_version": "fixture 0.1.0",
                "source_encoding": "utf-8",
                "source_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
                "parse_complete": true,
                "warnings": []
            },
            "blocks": [{
                "kind": "paragraph",
                "content": [{
                    "kind": "text",
                    "value": "吾輩ハ猫デアル。名前ハマダ無イ。"
                }]
            }]
    })
}

#[test]
fn orthographic_annotations_inject_into_schema_valid_parser_ir() {
    let (schemas, mapping) = schemas_and_mapping_accepting_orthographic_annotations();
    let bundle = ortho_fixture_bundle();
    let output = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: ortho_fixture_aat(),
        mapping,
        schemas,
        options: ConversionOptions {
            orthographic_annotations: Some(bundle.clone()),
            ..Default::default()
        },
    })
    .unwrap();

    assert_eq!(
        output.parser_ir.get("orthographic_annotations"),
        Some(&serde_json::to_value(bundle).unwrap())
    );
}

#[test]
fn orthographic_annotations_require_schema_support() {
    let (schemas, mapping) = schemas_and_mapping();
    let error = ab_aat_to_parser_ir::convert(ConversionRequest {
        aat: ortho_fixture_aat(),
        mapping,
        schemas,
        options: ConversionOptions {
            orthographic_annotations: Some(ortho_fixture_bundle()),
            ..Default::default()
        },
    })
    .unwrap_err()
    .to_string();

    assert!(error.contains("does not declare orthographic_annotations"));
}
```

- [ ] **Step 5: Verify the tests fail**

```bash
cd /home/bor/Projects/soranoha/ab-validator
cargo test -p ab-aat-to-parser-ir -- orthographic_annotations 2>&1 | tail -10
```
Expected: FAIL — `ConversionOptions` does not yet have `orthographic_annotations`.

- [ ] **Step 6: Build and run the integration tests**

```bash
cd /home/bor/Projects/soranoha/ab-validator
cargo test -p ab-aat-to-parser-ir -- orthographic_annotations 2>&1 | tail -5
```
Expected: PASS.

- [ ] **Step 7: Run full test suite**

```bash
cd /home/bor/Projects/soranoha/ab-validator
cargo test -p ab-aat-to-parser-ir 2>&1 | tail -5
```
Expected: all tests pass (unit + integration).

- [ ] **Step 8: Commit**

```bash
git add crates/ab-aat-to-parser-ir/src/convert.rs \
        crates/ab-aat-to-parser-ir/src/main.rs \
        crates/ab-aat-to-parser-ir/tests/integration.rs
git commit -m "feat(parser-ir): add --ortho-annotations flag to converter

Injects orthographic_annotations into parser-IR output when the loaded
schema declares the field. Keeps parser-IR validation enabled and fails
early against the current schema until ABC lands the schema/mapping bump."
```

---

### Task 4: Verify against golden fixtures + document ABC precondition

**Files:**
- Modify: `crates/ab-aat-to-parser-ir/README.md` (add --ortho-annotations docs)
- Modify: `crates/ab-aat-to-parser-ir/tests/integration.rs` (golden fixture assertion)

**Why:** Close the loop — confirm the integration test produces the exact
shape defined in Task 1's contract fixture. Document the ABC precondition.

- [ ] **Step 1: Verify contract fixture match**

Add this test to `crates/ab-aat-to-parser-ir/tests/integration.rs`:

```rust
#[test]
fn orthographic_annotations_bundle_matches_golden_fixture() {
    let expected = read_json(
        &Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/fixtures/ortho-annotations-expected.json"),
    )
    .unwrap();

    assert_eq!(serde_json::to_value(ortho_fixture_bundle()).unwrap(), expected);
}
```

Run:

```bash
cd /home/bor/Projects/soranoha/ab-validator
cargo test -p ab-aat-to-parser-ir -- orthographic_annotations_bundle_matches_golden_fixture 2>&1 | tail -5
```
Expected: PASS.

- [ ] **Step 2: Add converter docs**

Add to `crates/ab-aat-to-parser-ir/README.md` (or create if absent):

````markdown
### `--ortho-annotations <PATH>`

Optional path to an orthographic annotations JSON file produced by the
`ab-ortho-detect` layer. When provided, the output parser-IR includes an
`orthographic_annotations` field:

```json
{
  "work_id": "000000",
  "work_content_hash": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
  "coordinate_system": "decoded_utf8",
  "detector_id": "HeuristicV1",
  "annotations": [
    {
      "source_byte_range": { "start": 0, "end": 24 },
      "normalized_text": "吾輩は猫である。",
      "kind": "ScriptKatakanaToHiragana",
      "confidence": null
    }
  ]
}
```

**Precondition:** ABC must accept `orthographic_annotations` into the
parser-IR JSON Schema (`abc/schemas/parser-ir.schema.json`) and the loaded
mapping artifact must target the updated schema hash. The current schema has
`additionalProperties: false` and rejects unknown fields. Validation remains
enabled; with the current schema, the flag fails with a precondition error
instead of emitting invalid parser-IR.
````

- [ ] **Step 3: Commit**

```bash
git add crates/ab-aat-to-parser-ir/README.md \
        crates/ab-aat-to-parser-ir/tests/integration.rs
git commit -m "docs(parser-ir): document --ortho-annotations flag and ABC precondition"
```

---

## Self-Review

**1. Spec coverage:**
- D2 (new parser-IR field): Task 2 (`OrthoAnnotationsBundle`) + Task 3 (converter injection) ✓
- D6 (detector ID and source identity explicit): `detector_id`, `work_id`, `work_content_hash`, and `coordinate_system` fields in `OrthoAnnotationsBundle` ✓
- D7 (no validation skip): Task 3 schema precondition check + validation-on integration test ✓
- D3 (ABC owns sentence splitting): NOT in this plan (correct — out of scope) ✓
- Precondition (ABC schema bump): documented in Task 4 README and Global Constraints ✓
- `<s>` rendering: NOT in this plan (correct — out of scope) ✓

**2. Placeholder scan:** No unresolved placeholders. All code is explicit.

**3. Type consistency:**
- `OrthoAnnotationsBundle` defined in Task 2, used in Task 3 converter ✓
- `OrthoAnnotation` from `ab-ortho-detect` — existing type, unchanged ✓
- JSON shape matches between fixture (Task 1), struct (Task 2), and conversion (Task 3) ✓
- `ConversionOptions.orthographic_annotations` is `Option<OrthoAnnotationsBundle>`; CLI flag remains `--ortho-annotations <PATH>` and reads the file before conversion ✓
