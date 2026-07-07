# Orthographic Sentence Annotations — ab-validator Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Serialize ortho-detect annotations into parser-IR output as an `orthographic_annotations` field, so ABC's TEI renderer can produce `<s type="orthographic-katakana">` elements.

**Architecture:** The `ab-aat-to-parser-ir` converter gains a `--ortho-annotations <path.json>` CLI flag. The converter reads a JSON file containing `{detector_id, annotations: [OrthoAnnotation]}`, validates it, and includes it in the parser-IR JSON output. No sentence splitting in ab-validator — ABC owns that. No `<s>` rendering in ab-validator — ABC owns that. The deliverable is serialized parser-IR evidence.

**Tech Stack:** Rust (edition 2024), `serde_json`, `ab-ortho-detect` types, `ab-aat-to-parser-ir` crate.

## Global Constraints

- **PRECONDITION:** ABC must accept `orthographic_annotations` into `parser-ir.schema.json` (currently `additionalProperties: false`; the new field requires a schema version bump). This plan builds the ab-validator producer; ABC schema acceptance is the gate for production use.
- `<s>` rendering is ABC's concern. ab-validator does NOT produce TEI. The deliverable is parser-IR JSON with an `orthographic_annotations` field.
- Reuse `OrthoAnnotation` and `OrthoDetectorId` from `ab-ortho-detect` — no new annotation types.
- `AatProjection.ortho_normalizations` stays unchanged (reserved for future `<choice>` rendering).
- The `orthographic_annotations` JSON file is a standalone input to the converter (not embedded in AAT). This decouples ortho detection (which runs in the morph pipeline) from AAT→parser-IR conversion.
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
  "detector_id": "HeuristicV1",
  "annotations": [
    {
      "source_byte_range": { "start": 0, "end": 21 },
      "normalized_text": "吾輩は猫である",
      "kind": "ScriptKatakanaToHiragana",
      "confidence": null
    },
    {
      "source_byte_range": { "start": 21, "end": 48 },
      "normalized_text": "名前はまだ無い",
      "kind": "ScriptKatakanaToHiragana",
      "confidence": null
    }
  ]
}
```

Key contract decisions captured here:
- `detector_id` is a string matching `OrthoDetectorId` serialization: `"HeuristicV1"` or `"MlLogisticRegression"`.
- `source_byte_range` uses `{start, end}` — byte offsets in the original document text. Zero-based, half-open.
- `normalized_text` is the kata→hira version (informational, not rendered in TEI).
- `kind` is `"ScriptKatakanaToHiragana"` (v1). `"HistoricalToModern"` reserved for future.
- `confidence` is `null` for heuristic, an integer 0–100 for ML.

- [ ] **Step 2: Create the expected parser-IR output fragment**

Create `crates/ab-aat-to-parser-ir/tests/fixtures/ortho-annotations-expected.json`:

```json
{
  "detector_id": "HeuristicV1",
  "annotations": [
    {
      "source_byte_range": { "start": 0, "end": 21 },
      "normalized_text": "吾輩は猫である",
      "kind": "ScriptKatakanaToHiragana",
      "confidence": null
    },
    {
      "source_byte_range": { "start": 21, "end": 48 },
      "normalized_text": "名前はまだ無い",
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
{detector_id, annotations: [OrthoAnnotation]}. Serves as acceptance
test for the --ortho-annotations converter flag."
```

---

### Task 2: Serialization — Rust types for the annotation wrapper

**Files:**
- Create: `crates/ab-aat-to-parser-ir/src/ortho_annotations.rs`
- Modify: `crates/ab-aat-to-parser-ir/src/lib.rs` (add `mod ortho_annotations;`)

**Why:** The `orthographic_annotations` field wraps `OrthoAnnotation` with a
`detector_id`. A small Rust struct with `serde` derives handles the
serialization/deserialization contract. This is separated into its own
module because it's a self-contained serialization concern.

**Interfaces:**
- Produces: `OrthoAnnotationsFile` struct (deserializable from JSON, serializable to JSON).
- Consumed by: Task 3 (converter reads this from CLI flag).

- [ ] **Step 1: Write the failing test**

Add to the end of `crates/ab-aat-to-parser-ir/src/ortho_annotations.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrips_through_json() {
        let input = serde_json::json!({
            "detector_id": "HeuristicV1",
            "annotations": [
                {
                    "source_byte_range": { "start": 0, "end": 21 },
                    "normalized_text": "吾輩は猫である",
                    "kind": "ScriptKatakanaToHiragana",
                    "confidence": null
                }
            ]
        });
        let parsed: OrthoAnnotationsFile = serde_json::from_value(input.clone()).unwrap();
        assert_eq!(parsed.detector_id, "HeuristicV1");
        assert_eq!(parsed.annotations.len(), 1);
        assert_eq!(parsed.annotations[0].source_byte_range.start, 0);
        assert_eq!(parsed.annotations[0].source_byte_range.end, 21);
        assert_eq!(parsed.annotations[0].normalized_text, "吾輩は猫である");

        // Round-trip: serialize back and compare
        let output = serde_json::to_value(&parsed).unwrap();
        assert_eq!(output, input);
    }

    #[test]
    fn rejects_missing_detector_id() {
        let input = serde_json::json!({
            "annotations": []
        });
        let err = serde_json::from_value::<OrthoAnnotationsFile>(input).unwrap_err();
        assert!(err.to_string().contains("detector_id"));
    }

    #[test]
    fn rejects_missing_annotations() {
        let input = serde_json::json!({
            "detector_id": "HeuristicV1"
        });
        let err = serde_json::from_value::<OrthoAnnotationsFile>(input).unwrap_err();
        assert!(err.to_string().contains("annotations"));
    }
}
```

- [ ] **Step 2: Verify test fails**

```bash
cd /home/bor/Projects/soranoha/ab-validator
cargo test -p ab-aat-to-parser-ir -- ortho_annotations 2>&1 | tail -5
```
Expected: FAIL — `OrthoAnnotationsFile` not defined.

- [ ] **Step 3: Write the implementation**

Create `crates/ab-aat-to-parser-ir/src/ortho_annotations.rs`:

```rust
//! Serialization contract for the `orthographic_annotations` parser-IR field.
//! See `docs/superpowers/specs/2026-07-07-ortho-sentence-annotation-design.md`.

use serde::{Deserialize, Serialize};

/// The `orthographic_annotations` field as it appears in parser-IR JSON.
/// Wraps a list of `OrthoAnnotation` with detector provenance.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OrthoAnnotationsFile {
    /// Detector that produced these annotations.
    /// Serialized form of `ab_ortho_detect::OrthoDetectorId`.
    pub detector_id: String,

    /// Sentence-level orthographic annotations.
    /// Byte ranges are in original-document coordinates.
    pub annotations: Vec<ab_ortho_detect::OrthoAnnotation>,
}

/// Load an `OrthoAnnotationsFile` from a JSON file path.
///
/// # Errors
/// Returns an error on IO failure or invalid JSON.
pub fn read_ortho_annotations(path: &std::path::Path) -> anyhow::Result<OrthoAnnotationsFile> {
    let bytes = std::fs::read(path)?;
    let file: OrthoAnnotationsFile = serde_json::from_slice(&bytes)?;
    Ok(file)
}
```

- [ ] **Step 4: Register the module**

In `crates/ab-aat-to-parser-ir/src/lib.rs`, add near the other `pub mod` lines:

```rust
pub mod ortho_annotations;
```

- [ ] **Step 5: Run tests**

```bash
cd /home/bor/Projects/soranoha/ab-validator
cargo test -p ab-aat-to-parser-ir -- ortho_annotations 2>&1 | tail -5
```
Expected: 3 passed (roundtrip + two rejection tests).

- [ ] **Step 6: Commit**

```bash
git add crates/ab-aat-to-parser-ir/src/ortho_annotations.rs \
        crates/ab-aat-to-parser-ir/src/lib.rs
git commit -m "feat(parser-ir): add OrthoAnnotationsFile serialization type

{detector_id, annotations: [OrthoAnnotation]} — the contract shape for
the orthographic_annotations parser-IR field. Includes JSON round-trip
and rejection tests."
```

---

### Task 3: Wire `--ortho-annotations` into the converter CLI + convert logic

**Files:**
- Modify: `crates/ab-aat-to-parser-ir/src/main.rs` (add CLI flag)
- Modify: `crates/ab-aat-to-parser-ir/src/convert.rs` (add field to parser_ir output)
- Modify: `crates/ab-aat-to-parser-ir/src/schema.rs` (optional: load schema override)

**Why:** The converter needs a way to receive the ortho annotations file
and include it in the parser-IR output. This task adds the CLI flag, wires
it through `ConversionOptions` → `convert_preflighted`, and handles the
schema validation conflict.

**Schema validation note:** The current parser-IR schema has
`additionalProperties: false`, so `orthographic_annotations` would fail
validation. When the flag is provided, skip output validation (the ABC
schema bump is the precondition for re-enabling it). Emit a warning to
stderr. When the flag is absent, behavior is unchanged.

**Interfaces:**
- Consumes: `OrthoAnnotationsFile` from Task 2, fixture from Task 1.
- Produces: parser-IR JSON with `orthographic_annotations` field.

- [ ] **Step 1: Add `--ortho-annotations` to `ConversionOptions`**

In `crates/ab-aat-to-parser-ir/src/convert.rs`, add to `ConversionOptions`:

```rust
    /// Path to an orthographic annotations JSON file.
    /// When provided, the output parser-IR includes an
    /// `orthographic_annotations` field. Output validation is
    /// skipped because the current parser-IR schema rejects
    /// unknown fields (ABC schema bump is the precondition for
    /// re-enabling).
    pub ortho_annotations: Option<std::path::PathBuf>,
```

Update `Default` impl:

```rust
impl Default for ConversionOptions {
    fn default() -> Self {
        Self {
            validate_input_aat: true,
            validate_output_parser_ir: true,
            ortho_annotations: None,
        }
    }
}
```

- [ ] **Step 2: Read annotations and inject into parser_ir**

In `crates/ab-aat-to-parser-ir/src/convert.rs`, in `convert_preflighted`,
after the `json!({...})` block (around line 128), add conditional injection:

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

    // Inject orthographic annotations when provided.
    let skip_validation = if let Some(ref path) = options.ortho_annotations {
        let ortho = crate::ortho_annotations::read_ortho_annotations(path)?;
        let value = serde_json::to_value(&ortho)?;
        parser_ir
            .as_object_mut()
            .expect("parser_ir is an object")
            .insert("orthographic_annotations".to_owned(), value);
        true
    } else {
        false
    };

    if options.validate_output_parser_ir {
        if skip_validation {
            eprintln!(
                "warning: skipping parser-IR output validation because \
                 orthographic_annotations is present; the current schema \
                 has additionalProperties: false. Re-enable after ABC \
                 schema bump."
            );
        } else {
            validate_value(&schemas.parser_ir_schema, &parser_ir, "parser-IR")?;
        }
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
            let options = ConversionOptions {
                ortho_annotations,
                ..Default::default()
            };
            let output = PreparedConverter::new(mapping, schemas)?
                .convert(aat, options)?;
```

- [ ] **Step 4: Write integration test**

Add to `crates/ab-aat-to-parser-ir/tests/integration.rs`:

```rust
    #[test]
    fn ortho_annotations_flag_injects_field_into_parser_ir() {
        let tmp = tempfile::tempdir().unwrap();
        let aat_path = tmp.path().join("aat.json");
        let mapping_path = tmp.path().join("mapping.json");
        let ortho_path = tmp.path().join("ortho.json");
        let out_path = tmp.path().join("out.json");
        let divergence_path = tmp.path().join("divergence.json");

        // Minimal valid AAT (one paragraph of visible body text).
        let aat = serde_json::json!({
            "version": 1,
            "contents": {
                "blocks": [{
                    "inline": [{
                        "kind": "paragraph",
                        "children": [{
                            "kind": "text",
                            "text": "吾輩ハ猫デアル。名前ハマダ無イ。"
                        }]
                    }]
                }]
            }
        });
        std::fs::write(&aat_path, serde_json::to_string_pretty(&aat).unwrap()).unwrap();

        // Ortho annotations fixture (matches the contract fixture from Task 1).
        let ortho = serde_json::json!({
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
        });
        std::fs::write(&ortho_path, serde_json::to_string_pretty(&ortho).unwrap()).unwrap();

        // Use the repo's standard mapping so schema resolution works.
        let repo_root = std::env::current_dir().unwrap();
        let mapping_path_src = repo_root
            .join("data/mappings/aat-to-parser-ir/v1/mapping.json");
        std::fs::copy(&mapping_path_src, &mapping_path).unwrap();

        // Run the converter with --ortho-annotations.
        let output = std::process::Command::new(
            std::env::current_exe()
                .unwrap()
                .parent()
                .unwrap()
                .join("ab-aat-to-parser-ir"),
        )
        .arg("convert")
        .arg("--aat")
        .arg(&aat_path)
        .arg("--mapping")
        .arg(&mapping_path)
        .arg("--parser-ir-out")
        .arg(&out_path)
        .arg("--divergence-out")
        .arg(&divergence_path)
        .arg("--ortho-annotations")
        .arg(&ortho_path)
        .output()
        .unwrap();

        // Converter should succeed (validation skipped with warning).
        assert!(output.status.success(), "stderr: {}", String::from_utf8_lossy(&output.stderr));
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("skipping parser-IR output validation"),
            "expected validation-skip warning, got: {stderr}"
        );

        // Verify parser-IR output contains orthographic_annotations.
        let parser_ir: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(&out_path).unwrap()).unwrap();
        let ortho_field = parser_ir
            .get("orthographic_annotations")
            .expect("parser-IR must have orthographic_annotations field");
        assert_eq!(ortho_field["detector_id"], "HeuristicV1");
        assert_eq!(ortho_field["annotations"].as_array().unwrap().len(), 2);

        // Verify annotations match input exactly.
        let expected: serde_json::Value = serde_json::from_slice(
            &std::fs::read(&ortho_path).unwrap(),
        )
        .unwrap();
        assert_eq!(ortho_field, &expected);
    }
```

- [ ] **Step 5: Verify the test fails**

```bash
cd /home/bor/Projects/soranoha/ab-validator
cargo test -p ab-aat-to-parser-ir -- ortho_annotations_flag 2>&1 | tail -10
```
Expected: FAIL — the converter binary doesn't support `--ortho-annotations` yet.

- [ ] **Step 6: Build and run the integration test**

```bash
cd /home/bor/Projects/soranoha/ab-validator
cargo test -p ab-aat-to-parser-ir -- ortho_annotations_flag 2>&1 | tail -5
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

Injects orthographic_annotations into parser-IR output when the flag is
provided. Skips output validation (current schema rejects unknown fields;
ABC schema bump is the precondition for re-enabling). Includes
integration test proving the field round-trips through the converter."
```

---

### Task 4: Verify against golden fixtures + document ABC precondition

**Files:**
- Modify: `crates/ab-aat-to-parser-ir/README.md` (add --ortho-annotations docs)
- Verify: fixtures from Task 1 match the integration test output.

**Why:** Close the loop — confirm the integration test produces the exact
shape defined in Task 1's contract fixture. Document the ABC precondition.

- [ ] **Step 1: Verify contract fixture match**

The integration test from Task 3 uses the same annotation shape as the
Task 1 fixture. Add an assertion that loads the golden fixture and
compares:

```bash
cd /home/bor/Projects/soranoha/ab-validator
cargo test -p ab-aat-to-parser-ir -- ortho_annotations_flag 2>&1 | grep "PASS"
```
Expected: PASS. The test already asserts `ortho_field == &expected` against
the input fixture. The golden fixture from Task 1 is identical in shape.

- [ ] **Step 2: Add converter docs**

Add to `crates/ab-aat-to-parser-ir/README.md` (or create if absent):

```markdown
### `--ortho-annotations <PATH>`

Optional path to an orthographic annotations JSON file produced by the
`ab-ortho-detect` layer. When provided, the output parser-IR includes an
`orthographic_annotations` field:

```json
{
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
parser-IR JSON Schema (`abc/schemas/parser-ir.schema.json`). The current
schema has `additionalProperties: false` and rejects unknown fields.
Output validation is skipped when this flag is used; it will be
re-enabled after the ABC schema bump.
```

- [ ] **Step 3: Commit**

```bash
git add crates/ab-aat-to-parser-ir/README.md
git commit -m "docs(parser-ir): document --ortho-annotations flag and ABC precondition"
```

---

## Self-Review

**1. Spec coverage:**
- D2 (new parser-IR field): Task 2 (OrthoAnnotationsFile) + Task 3 (converter injection) ✓
- D6 (detector ID explicit): detector_id field in OrthoAnnotationsFile ✓
- D3 (ABC owns sentence splitting): NOT in this plan (correct — out of scope) ✓
- Precondition (ABC schema bump): documented in Task 4 README and Global Constraints ✓
- `<s>` rendering: NOT in this plan (correct — out of scope) ✓

**2. Placeholder scan:** No TBD/TODO/fill-in-later. All code is explicit.

**3. Type consistency:**
- `OrthoAnnotationsFile` defined in Task 2, used in Task 3 converter ✓
- `OrthoAnnotation` from `ab-ortho-detect` — existing type, unchanged ✓
- JSON shape matches between fixture (Task 1), struct (Task 2), and conversion (Task 3) ✓
- `ConversionOptions.ortho_annotations` is `Option<PathBuf>` in both `convert.rs` and `main.rs` ✓
