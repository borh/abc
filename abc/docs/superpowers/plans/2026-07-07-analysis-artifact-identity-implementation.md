# Analysis Artifact Identity First Slice Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement the first token-independent per-work `analysis` slice over `parser-ir-plaintext-body-v1`, including schemas, request-set identity helpers, manifest-index validation, and a prototype materializer that does not implement literary metric formulas.

**Architecture:** Keep canonical identity in JSON values and manifests. Add strict JSON Schemas for recipes/results, small Clojure helpers for request-set hashing, manifest-index checks for copied producer identity fields, and a prototype analysis materializer that accepts fixture metric values as input data. Tokenized slices, tokenizer profile identity, collection packs, and corpus-normalized metrics stay outside this plan.

**Tech Stack:** Clojure 1.12.4, JSON Schema draft 2020-12, RFC 8785 JCS helpers in `abc.tools.jcs`, SHA-256 helpers in `abc.tools.hash`, `clojure.test`/Kaocha, Nix flake checks.

## Global Constraints

- Follow ADR 0001: `artifact_id = sha256(RFC8785-JCS(manifest_identity_object))`.
- Follow ADR 0026: this slice accepts only `parser-ir-plaintext-body-v1`.
- Do not implement stylometric formulas in this plan; fixture metric values are data supplied to the materializer.
- Null in manifest identity means not applicable, never unknown.
- Copy `parser_build_hash`, `parser_config_hash`, `aat_parser_ir_mapping_hash`, and `parser_ir_schema_hash` exactly from the producer parser-IR manifest.
- Keep `tei_profile_hash`, `tokenizer_build_hash`, and `tokenizer_dictionary_hash` null for this slice.
- Add only the `analysis-result` sidecar role; do not add pack-only roles.
- Request-set hashes are over `request_set_identity_object`; `request_set_id` is excluded from its own hash input.
- The manifest index is a generated view and must not become artifact identity.
- If Nix is wired, pass per-hash recipe paths and producer artifact paths to builders; do not make a registry directory or full corpus snapshot a direct analysis input.

---

## File Structure

**Created schemas and fixtures:**

- `abc/schemas/analysis-recipe.schema.json` - strict recipe wire contract.
- `abc/schemas/analysis-result.schema.json` - strict per-work result payload contract.
- `abc/data/analysis-recipes/literary-basic-ja-v1.json` - canonical token-independent fixture recipe.
- `abc/examples/v0/example-work/analysis-result.json` - deterministic result fixture used by schema and materialization tests.

**Created source:**

- `abc/src/abc/tools/analysis_identity.clj` - recipe hashing and request-set canonicalization.
- `abc/src/abc/tools/materialize_analysis.clj` - prototype per-work analysis manifest/result writer.

**Modified source:**

- `abc/schemas/manifest.schema.json` - add `analysis-result` to sidecar role enum and bump manifest schema version.
- `abc/schemas/schema-contracts.json` - record the new analysis schemas and the new manifest schema hash.
- `abc/src/abc/tools/manifest_index.clj` - index identity/provenance fields and validate analysis copied fields.
- `abc/src/abc/tools/validate_design_bundle.clj` - validate analysis schemas, recipe/result fixtures, and copied-field constraints.

**Created tests:**

- `abc/test/abc/tools/analysis_identity_test.clj`
- `abc/test/abc/tools/materialize_analysis_test.clj`

**Modified tests:**

- `abc/test/abc/tools/schema_test.clj`
- `abc/test/abc/tools/manifest_index_test.clj`
- `abc/test/abc/tools/validate_design_bundle_test.clj`
- `abc/test/abc/tools/materialize_import_test.clj`

---

### Task 1: Analysis Schemas and Fixture Values

**Files:**
- Create: `abc/schemas/analysis-recipe.schema.json`
- Create: `abc/schemas/analysis-result.schema.json`
- Create: `abc/data/analysis-recipes/literary-basic-ja-v1.json`
- Create: `abc/examples/v0/example-work/analysis-result.json`
- Modify: `abc/schemas/schema-contracts.json`
- Modify: `abc/test/abc/tools/schema_test.clj`

**Interfaces:**
- Produces schema files consumed by Tasks 2, 5, and 6.
- Produces `literary-basic-ja-v1.json` consumed by Tasks 3 and 5.
- Produces `analysis-result.json` consumed by Tasks 1, 5, and 6.

- [ ] **Step 1: Write failing schema fixture test**

Add this require to `abc/test/abc/tools/schema_test.clj` if it is not already present:

```clojure
[abc.tools.manifest :as manifest]
```

Extend `cross-project-schema-versions` with:

```clojure
"schemas/analysis-recipe.schema.json" "0.1.0"
"schemas/analysis-result.schema.json" "0.1.0"
```

Add this test:

```clojure
(deftest analysis-schema-fixtures-validate-test
  (let [recipe-schema (schema/read-schema "schemas/analysis-recipe.schema.json")
        result-schema (schema/read-schema "schemas/analysis-result.schema.json")
        recipe (files/read-json "data/analysis-recipes/literary-basic-ja-v1.json")
        result (files/read-json "examples/v0/example-work/analysis-result.json")]
    (is (= "https://w3id.org/abc/schemas/analysis-recipe.schema.json"
           (get recipe "schema_id")))
    (is (= "https://w3id.org/abc/schemas/analysis-result.schema.json"
           (get result "schema_id")))
    (is (= (manifest/schema-hash "schemas/analysis-result.schema.json")
           (get recipe "required_output_schema_hash")))
    (is (= (manifest/schema-hash "schemas/analysis-result.schema.json")
           (get result "schema_hash")))
    (is (nil? (schema/validation-errors recipe-schema recipe)))
    (is (nil? (schema/validation-errors result-schema result)))))
```

- [ ] **Step 2: Run the focused schema test and confirm it fails**

Run:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.schema-test/analysis-schema-fixtures-validate-test"
```

Expected: failure because the analysis schema and fixture files do not exist.

- [ ] **Step 3: Add `analysis-recipe.schema.json`**

Create `abc/schemas/analysis-recipe.schema.json`:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://w3id.org/abc/schemas/analysis-recipe.schema.json",
  "title": "ABC Analysis Recipe",
  "version": "0.1.0",
  "type": "object",
  "additionalProperties": false,
  "required": [
    "schema_id",
    "recipe_id",
    "recipe_version",
    "supported_input_view_kinds",
    "tokenizer_required",
    "plaintext_policy_hash",
    "normalization_policy",
    "metrics",
    "required_output_schema_hash",
    "determinism_tier",
    "error_behavior"
  ],
  "properties": {
    "schema_id": {
      "const": "https://w3id.org/abc/schemas/analysis-recipe.schema.json"
    },
    "recipe_id": { "type": "string", "minLength": 1 },
    "recipe_version": {
      "type": "string",
      "pattern": "^[0-9]+\\.[0-9]+\\.[0-9]+$"
    },
    "supported_input_view_kinds": {
      "type": "array",
      "minItems": 1,
      "uniqueItems": true,
      "items": {
        "enum": ["parser-ir-plaintext-body-v1"]
      }
    },
    "tokenizer_required": { "const": false },
    "plaintext_policy_hash": { "$ref": "#/$defs/hash" },
    "normalization_policy": {
      "type": "object",
      "additionalProperties": false,
      "required": ["unicode_normalization", "newline_policy"],
      "properties": {
        "unicode_normalization": { "enum": ["producer-preserved"] },
        "newline_policy": { "enum": ["parser-ir-plaintext-body-v1"] }
      }
    },
    "metrics": {
      "type": "array",
      "items": { "$ref": "#/$defs/metricSpec" },
      "uniqueItems": true
    },
    "required_output_schema_hash": { "$ref": "#/$defs/hash" },
    "determinism_tier": { "enum": ["exact"] },
    "error_behavior": {
      "type": "object",
      "additionalProperties": false,
      "required": ["missing_required_identity_field", "metric_failure"],
      "properties": {
        "missing_required_identity_field": { "enum": ["failed-manifest-or-none"] },
        "metric_failure": { "enum": ["record-failed-metric"] }
      }
    }
  },
  "$defs": {
    "hash": {
      "type": "string",
      "pattern": "^sha256:[0-9a-f]{64}$"
    },
    "metricSpec": {
      "type": "object",
      "additionalProperties": false,
      "required": ["metric_id", "formula_version", "unit", "value_type"],
      "properties": {
        "metric_id": { "type": "string", "minLength": 1 },
        "formula_version": { "type": "string", "minLength": 1 },
        "unit": { "type": "string", "minLength": 1 },
        "value_type": { "enum": ["integer", "float64", "string"] }
      }
    }
  }
}
```

- [ ] **Step 4: Add `analysis-result.schema.json`**

Create `abc/schemas/analysis-result.schema.json`:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://w3id.org/abc/schemas/analysis-result.schema.json",
  "title": "ABC Analysis Result",
  "version": "0.1.0",
  "type": "object",
  "additionalProperties": false,
  "required": [
    "schema_id",
    "schema_hash",
    "subject",
    "input_view",
    "tokenizer_profile_hash",
    "analysis_recipe_hash",
    "metrics",
    "warnings"
  ],
  "properties": {
    "schema_id": {
      "const": "https://w3id.org/abc/schemas/analysis-result.schema.json"
    },
    "schema_hash": { "$ref": "#/$defs/hash" },
    "subject": { "$ref": "#/$defs/subject" },
    "input_view": { "$ref": "#/$defs/inputView" },
    "tokenizer_profile_hash": {
      "anyOf": [{ "$ref": "#/$defs/hash" }, { "type": "null" }]
    },
    "analysis_recipe_hash": { "$ref": "#/$defs/hash" },
    "metrics": {
      "type": "array",
      "items": { "$ref": "#/$defs/metricResult" }
    },
    "warnings": {
      "type": "array",
      "items": { "type": "string", "minLength": 1 }
    }
  },
  "$defs": {
    "hash": {
      "type": "string",
      "pattern": "^sha256:[0-9a-f]{64}$"
    },
    "subject": {
      "type": "object",
      "additionalProperties": false,
      "required": [
        "source_id",
        "work_id",
        "corpus_snapshot_hash",
        "work_content_hash",
        "metadata_record_hash"
      ],
      "properties": {
        "source_id": { "type": "string", "minLength": 1 },
        "work_id": { "type": "string", "minLength": 1 },
        "corpus_snapshot_hash": { "$ref": "#/$defs/hash" },
        "work_content_hash": { "$ref": "#/$defs/hash" },
        "metadata_record_hash": {
          "anyOf": [{ "$ref": "#/$defs/hash" }, { "type": "null" }]
        }
      }
    },
    "inputView": {
      "type": "object",
      "additionalProperties": false,
      "required": [
        "input_view_kind",
        "producer_artifact_id",
        "producer_content_hash",
        "plaintext_policy_hash",
        "coordinate_system"
      ],
      "properties": {
        "input_view_kind": { "const": "parser-ir-plaintext-body-v1" },
        "producer_artifact_id": { "$ref": "#/$defs/hash" },
        "producer_content_hash": { "$ref": "#/$defs/hash" },
        "plaintext_policy_hash": { "$ref": "#/$defs/hash" },
        "coordinate_system": { "enum": ["unicode-scalar-value"] }
      }
    },
    "metricResult": {
      "type": "object",
      "additionalProperties": false,
      "required": [
        "metric_id",
        "value",
        "value_type",
        "denominator",
        "unit",
        "status"
      ],
      "properties": {
        "metric_id": { "type": "string", "minLength": 1 },
        "value": {
          "anyOf": [
            { "type": "integer" },
            { "type": "number" },
            { "type": "string" },
            { "type": "null" }
          ]
        },
        "value_type": { "enum": ["integer", "float64", "string", "null"] },
        "denominator": {
          "anyOf": [{ "type": "integer", "minimum": 0 }, { "type": "null" }]
        },
        "unit": { "type": "string", "minLength": 1 },
        "status": { "enum": ["passed", "failed", "missing"] }
      }
    }
  }
}
```

- [ ] **Step 5: Create recipe and result fixtures with computed schema hashes**

Run this command to write both fixture files with the computed
`analysis-result.schema.json` hash:

```bash
cd abc
nix develop .#default --command clojure -M -e '
(require (quote [abc.tools.manifest :as manifest]))
(let [result-schema-hash (manifest/schema-hash "schemas/analysis-result.schema.json")]
  (manifest/write-json-file!
   "data/analysis-recipes/literary-basic-ja-v1.json"
   {"schema_id" "https://w3id.org/abc/schemas/analysis-recipe.schema.json"
    "recipe_id" "literary-basic-ja-v1"
    "recipe_version" "0.1.0"
    "supported_input_view_kinds" ["parser-ir-plaintext-body-v1"]
    "tokenizer_required" false
    "plaintext_policy_hash" "sha256:1000000000000000000000000000000000000000000000000000000000000001"
    "normalization_policy" {"unicode_normalization" "producer-preserved"
                            "newline_policy" "parser-ir-plaintext-body-v1"}
    "metrics" [{"metric_id" "fixture-line-count"
                "formula_version" "fixture-input-v1"
                "unit" "line"
                "value_type" "integer"}]
    "required_output_schema_hash" result-schema-hash
    "determinism_tier" "exact"
    "error_behavior" {"missing_required_identity_field" "failed-manifest-or-none"
                      "metric_failure" "record-failed-metric"}})
  (manifest/write-json-file!
   "examples/v0/example-work/analysis-result.json"
   {"schema_id" "https://w3id.org/abc/schemas/analysis-result.schema.json"
    "schema_hash" result-schema-hash
    "subject" {"source_id" "aozora:example-work"
               "work_id" "aozora:example-work"
               "corpus_snapshot_hash" "sha256:f00000000000000000000000000000000000000000000000000000000000000f"
               "work_content_hash" "sha256:f100000000000000000000000000000000000000000000000000000000000001"
               "metadata_record_hash" nil}
    "input_view" {"input_view_kind" "parser-ir-plaintext-body-v1"
                  "producer_artifact_id" "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                  "producer_content_hash" "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                  "plaintext_policy_hash" "sha256:1000000000000000000000000000000000000000000000000000000000000001"
                  "coordinate_system" "unicode-scalar-value"}
    "tokenizer_profile_hash" nil
    "analysis_recipe_hash" "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
    "metrics" [{"metric_id" "fixture-line-count"
                "value" 3
                "value_type" "integer"
                "denominator" nil
                "unit" "line"
                "status" "passed"}]
    "warnings" []}))'
```

- [ ] **Step 6: Record analysis schema hashes in `schema-contracts.json`**

Run:

```bash
cd abc
nix develop .#default --command clojure -M -e '
(require (quote [abc.tools.files :as files]))
(require (quote [abc.tools.json :as abc-json]))
(require (quote [abc.tools.manifest :as manifest]))
(let [paths #{"schemas/analysis-recipe.schema.json"
              "schemas/analysis-result.schema.json"}
      contracts (files/read-json "schemas/schema-contracts.json")
      old-schemas (remove #(contains? paths (get % "path"))
                          (get contracts "schemas"))
      new-schemas [{"path" "schemas/analysis-recipe.schema.json"
                    "id" "https://w3id.org/abc/schemas/analysis-recipe.schema.json"
                    "title" "ABC Analysis Recipe"
                    "version" "0.1.0"
                    "hash" (manifest/schema-hash "schemas/analysis-recipe.schema.json")}
                   {"path" "schemas/analysis-result.schema.json"
                    "id" "https://w3id.org/abc/schemas/analysis-result.schema.json"
                    "title" "ABC Analysis Result"
                    "version" "0.1.0"
                    "hash" (manifest/schema-hash "schemas/analysis-result.schema.json")}]
      updated (assoc contracts "schemas"
                     (vec (sort-by #(get % "path")
                                   (concat old-schemas new-schemas))))]
  (abc-json/write-deterministic-json-file! "schemas/schema-contracts.json" updated))'
```

- [ ] **Step 7: Run the focused schema test and confirm it passes**

Run:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.schema-test/analysis-schema-fixtures-validate-test"
```

Expected: PASS.

- [ ] **Step 8: Commit Task 1**

```bash
git add schemas/analysis-recipe.schema.json \
        schemas/analysis-result.schema.json \
        schemas/schema-contracts.json \
        data/analysis-recipes/literary-basic-ja-v1.json \
        examples/v0/example-work/analysis-result.json \
        test/abc/tools/schema_test.clj
git commit -m "feat(abc): add analysis schema fixtures"
```

---

### Task 2: Manifest Schema Sidecar Role

**Files:**
- Modify: `abc/schemas/manifest.schema.json`
- Modify: `abc/schemas/schema-contracts.json`
- Modify: `abc/test/abc/tools/schema_test.clj`
- Modify: `abc/test/abc/tools/materialize_import_test.clj`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`

**Interfaces:**
- Produces manifest schema support for `analysis-result` sidecars.
- Rotates the manifest schema hash consumed by materializers and tests.

- [ ] **Step 1: Write failing sidecar role test**

Add this test to `abc/test/abc/tools/validate_design_bundle_test.clj`:

```clojure
(deftest manifest-schema-accepts-analysis-result-sidecar-test
  (let [manifest-schema (files/read-json "schemas/manifest.schema.json")
        manifest {"manifest_schema_id" "https://w3id.org/abc/schemas/manifest.schema.json"
                  "artifact_id" (files/example-hash "26")
                  "artifact_kind" "analysis"
                  "validation_status" "passed"
                  "manifest_identity_object" {"manifest_schema_hash" (manifest/schema-hash "schemas/manifest.schema.json")
                                              "corpus_snapshot_hash" (files/example-hash "01")
                                              "work_content_hash" (files/example-hash "02")
                                              "metadata_record_hash" nil
                                              "parser_build_hash" (files/example-hash "03")
                                              "parser_config_hash" (files/example-hash "04")
                                              "aat_parser_ir_mapping_hash" (files/example-hash "05")
                                              "parser_ir_schema_hash" (files/example-hash "06")
                                              "tei_profile_hash" nil
                                              "tokenizer_build_hash" nil
                                              "tokenizer_dictionary_hash" nil
                                              "analysis_recipe_hash" (files/example-hash "07")
                                              "output_format_spec_hash" (manifest/schema-hash "schemas/analysis-result.schema.json")}
                  "content" {"content_hash" (files/example-hash "08")
                             "media_type" "application/json"
                             "byte_length" 10
                             "path_hint" "analysis-result.json"}
                  "sidecars" [{"role" "analysis-result"
                               "hash" (files/example-hash "08")
                               "media_type" "application/json"
                               "path_hint" "analysis-result.json"}]
                  "provenance" {"generated_at" "2026-07-07T00:00:00Z"
                                "activity_id" "https://w3id.org/abc/activity/materialize-analysis"
                                "agent" "abc.tools.materialize-analysis"
                                "plan_hash" nil
                                "used" [(files/example-hash "03")]
                                "was_derived_from" [(files/example-hash "03")]}
                  "license" nil
                  "signatures" []
                  "superseded_by" nil
                  "invalidated_at" nil
                  "replacement_reason" nil
                  "notes" nil}]
    (is (nil? (schema/validation-errors manifest-schema manifest)))))
```

Add `[abc.tools.manifest :as manifest]` to the namespace require if missing.

- [ ] **Step 2: Run the sidecar role test and confirm it fails**

Run:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.validate-design-bundle-test/manifest-schema-accepts-analysis-result-sidecar-test"
```

Expected: failure because `analysis-result` is not in the sidecar role enum.

- [ ] **Step 3: Update manifest schema**

In `abc/schemas/manifest.schema.json`:

- Change `"version": "0.4.0"` to `"version": "0.4.1"`.
- Add `"analysis-result"` to the sidecar `role` enum.

The final role enum must be:

```json
["warnings", "errors", "validation-result", "rdf-view", "index-entry", "mapping-divergence", "preservation", "source-region-coverage", "analysis-result"]
```

- [ ] **Step 4: Rotate manifest schema hash references**

Run:

```bash
cd abc
new_manifest_hash="$(nix develop .#default --command clojure -M -e '(require (quote abc.tools.manifest)) (print (abc.tools.manifest/schema-hash "schemas/manifest.schema.json"))')"
printf '%s\n' "$new_manifest_hash"
```

Update these files:

- In `abc/test/abc/tools/schema_test.clj`, set manifest schema version to
  `"0.4.1"`.
- In `abc/test/abc/tools/materialize_import_test.clj`, replace the old literal
  manifest schema hash with `new_manifest_hash`.
- In `abc/schemas/schema-contracts.json`, update the manifest schema entry:
  version `"0.4.1"` and hash `new_manifest_hash`.

- [ ] **Step 5: Run focused tests and confirm they pass**

Run:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.validate-design-bundle-test/manifest-schema-accepts-analysis-result-sidecar-test --focus abc.tools.schema-test/cross-project-schemas-carry-explicit-versions-test --focus abc.tools.materialize-import-test/schema-hash-test"
```

Expected: PASS.

- [ ] **Step 6: Commit Task 2**

```bash
git add schemas/manifest.schema.json \
        schemas/schema-contracts.json \
        test/abc/tools/schema_test.clj \
        test/abc/tools/materialize_import_test.clj \
        test/abc/tools/validate_design_bundle_test.clj
git commit -m "feat(abc): allow analysis result sidecars"
```

---

### Task 3: Request-Set and Recipe Identity Helpers

**Files:**
- Create: `abc/src/abc/tools/analysis_identity.clj`
- Create: `abc/test/abc/tools/analysis_identity_test.clj`

**Interfaces:**
- Produces:
  - `abc.tools.analysis-identity/hash-json-value`
  - `abc.tools.analysis-identity/analysis-recipe-hash`
  - `abc.tools.analysis-identity/canonical-subjects`
  - `abc.tools.analysis-identity/request-set-identity-object`
  - `abc.tools.analysis-identity/request-set-id`
  - `abc.tools.analysis-identity/resolved-recipe-label`

- [ ] **Step 1: Write failing identity tests**

Create `abc/test/abc/tools/analysis_identity_test.clj`:

```clojure
(ns abc.tools.analysis-identity-test
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [clojure.test :refer [deftest is testing]]))

(def subject-a
  {"source_id" "aozora:1"
   "work_id" "aozora:1-a"
   "work_content_hash" (files/example-hash "11")
   "metadata_record_hash" nil})

(def subject-b
  {"source_id" "aozora:1"
   "work_id" "aozora:1-b"
   "work_content_hash" (files/example-hash "12")
   "metadata_record_hash" (files/example-hash "13")})

(deftest canonical-subjects-sort-and-coalesce-test
  (is (= [subject-a subject-b]
         (analysis-identity/canonical-subjects
          [subject-b subject-a subject-a]))))

(deftest request-set-id-excludes-derived-and-label-fields-test
  (let [identity-object (analysis-identity/request-set-identity-object
                         {:schema-hash (files/example-hash "01")
                          :corpus-snapshot-hash (files/example-hash "02")
                          :subjects [subject-b subject-a subject-a]
                          :input-views [{"input_view_kind" "parser-ir-plaintext-body-v1"
                                         "policy_hash" (files/example-hash "03")}]
                          :tokenizer-profile-hashes []
                          :analysis-recipe-hashes [(files/example-hash "04")]
                          :missing-policy "build-missing-only"
                          :pack-policy-hash (files/example-hash "05")})
        request-set-a {"request_set_identity_object" identity-object
                       "request_set_id" (files/example-hash "98")
                       "resolved_labels" {"analysis_recipes" []}
                       "batch_policy" "100-works-or-512mb"}
        request-set-b (assoc request-set-a
                             "request_set_id" (files/example-hash "99")
                             "batch_policy" "1-work")]
    (is (= (analysis-identity/request-set-id request-set-a)
           (analysis-identity/request-set-id request-set-b)))
    (is (= (manifest/artifact-id identity-object)
           (analysis-identity/request-set-id request-set-a)))
    (is (nil? (get-in identity-object ["subjects" 0 "metadata_record_hash"])))))

(deftest resolved-recipe-label-is-non-identity-audit-data-test
  (is (= {"recipe_id" "literary-basic-ja-v1"
          "analysis_recipe_hash" (files/example-hash "21")
          "registry_entry_hash" (files/example-hash "22")
          "resolved_at" "2026-07-07T00:00:00Z"}
         (analysis-identity/resolved-recipe-label
          {:recipe-id "literary-basic-ja-v1"
           :analysis-recipe-hash (files/example-hash "21")
           :registry-entry-hash (files/example-hash "22")
           :resolved-at "2026-07-07T00:00:00Z"}))))
```

- [ ] **Step 2: Run identity tests and confirm they fail**

Run:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.analysis-identity-test"
```

Expected: failure because `abc.tools.analysis-identity` does not exist.

- [ ] **Step 3: Implement `analysis_identity.clj`**

Create `abc/src/abc/tools/analysis_identity.clj`:

```clojure
(ns abc.tools.analysis-identity
  (:require [abc.tools.hash :as hash]
            [clojure.string :as string]))

(def allowed-missing-policies
  #{"require-existing" "build-missing-only" "record-missing-status"})

(defn hash-json-value [value]
  (hash/format-sha256 (hash/sha256-json-jcs value)))

(def analysis-recipe-hash hash-json-value)

(defn- required-string [label value]
  (when (or (not (string? value)) (string/blank? value))
    (throw (ex-info (str label " must be a non-empty string")
                    {:label label
                     :value value})))
  value)

(defn- subject-sort-key [subject]
  [(required-string "source_id" (get subject "source_id"))
   (required-string "work_content_hash" (get subject "work_content_hash"))
   (or (get subject "metadata_record_hash") "")])

(defn canonical-subjects [subjects]
  (->> subjects
       (sort-by subject-sort-key)
       distinct
       vec))

(defn- canonical-input-views [input-views]
  (->> input-views
       (sort-by (juxt #(get % "input_view_kind")
                      #(get % "policy_hash")))
       distinct
       vec))

(defn- canonical-hashes [values]
  (->> values sort distinct vec))

(defn request-set-identity-object
  [{:keys [schema-hash corpus-snapshot-hash subjects input-views
           tokenizer-profile-hashes analysis-recipe-hashes missing-policy
           pack-policy-hash]}]
  (when-not (contains? allowed-missing-policies missing-policy)
    (throw (ex-info "Invalid missing policy"
                    {:missing_policy missing-policy})))
  {"schema_hash" schema-hash
   "corpus_snapshot_hash" corpus-snapshot-hash
   "subjects" (canonical-subjects subjects)
   "input_views" (canonical-input-views input-views)
   "tokenizer_profile_hashes" (canonical-hashes tokenizer-profile-hashes)
   "analysis_recipe_hashes" (canonical-hashes analysis-recipe-hashes)
   "missing_policy" missing-policy
   "pack_policy_hash" pack-policy-hash})

(defn request-set-id [request-set]
  (hash-json-value (get request-set "request_set_identity_object")))

(defn resolved-recipe-label
  [{:keys [recipe-id analysis-recipe-hash registry-entry-hash resolved-at]}]
  {"recipe_id" recipe-id
   "analysis_recipe_hash" analysis-recipe-hash
   "registry_entry_hash" registry-entry-hash
   "resolved_at" resolved-at})
```

- [ ] **Step 4: Run identity tests and confirm they pass**

Run:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.analysis-identity-test"
```

Expected: PASS.

- [ ] **Step 5: Commit Task 3**

```bash
git add src/abc/tools/analysis_identity.clj \
        test/abc/tools/analysis_identity_test.clj
git commit -m "feat(abc): add analysis request set identity helpers"
```

---

### Task 4: Manifest Index Producer Lookup and Copied-Field Validation

**Files:**
- Modify: `abc/src/abc/tools/manifest_index.clj`
- Modify: `abc/test/abc/tools/manifest_index_test.clj`

**Interfaces:**
- Consumes existing manifest JSON maps.
- Produces:
  - `manifest-index/parser-ir-producer-candidates`
  - `manifest-index/analysis-copied-field-errors`
  - `manifest-index/validate-analysis-copied-fields!`

- [ ] **Step 1: Write failing manifest-index tests**

Append these helpers and tests to `abc/test/abc/tools/manifest_index_test.clj`:

```clojure
(defn analysis-manifest
  [artifact-id producer-artifact-id parser-build-hash]
  {"artifact_id" artifact-id
   "artifact_kind" "analysis"
   "validation_status" "passed"
   "manifest_identity_object" {"manifest_schema_hash" (files/example-hash "01")
                               "corpus_snapshot_hash" (files/example-hash "02")
                               "work_content_hash" (files/example-hash "03")
                               "metadata_record_hash" nil
                               "parser_build_hash" parser-build-hash
                               "parser_config_hash" (files/example-hash "05")
                               "aat_parser_ir_mapping_hash" (files/example-hash "06")
                               "parser_ir_schema_hash" (files/example-hash "07")
                               "tei_profile_hash" nil
                               "tokenizer_build_hash" nil
                               "tokenizer_dictionary_hash" nil
                               "analysis_recipe_hash" (files/example-hash "08")
                               "output_format_spec_hash" (files/example-hash "09")}
   "content" {"content_hash" (files/example-hash "10")
              "media_type" "application/json"}
   "provenance" {"used" [producer-artifact-id (files/example-hash "20")]
                 "was_derived_from" [producer-artifact-id]}})

(defn parser-ir-manifest
  [artifact-id parser-build-hash]
  {"artifact_id" artifact-id
   "artifact_kind" "parser-ir"
   "validation_status" "passed"
   "manifest_identity_object" {"manifest_schema_hash" (files/example-hash "01")
                               "corpus_snapshot_hash" (files/example-hash "02")
                               "work_content_hash" (files/example-hash "03")
                               "metadata_record_hash" nil
                               "parser_build_hash" parser-build-hash
                               "parser_config_hash" (files/example-hash "05")
                               "aat_parser_ir_mapping_hash" (files/example-hash "06")
                               "parser_ir_schema_hash" (files/example-hash "07")
                               "tei_profile_hash" nil
                               "tokenizer_build_hash" nil
                               "tokenizer_dictionary_hash" nil
                               "analysis_recipe_hash" nil
                               "output_format_spec_hash" (files/example-hash "07")}
   "content" {"content_hash" (files/example-hash "11")
              "media_type" "application/json"}
   "provenance" {"used" []
                 "was_derived_from" [(files/example-hash "03")]}})

(deftest parser-ir-producer-lookup-test
  (let [producer-id (files/example-hash "31")
        entries (manifest-index/index-entries
                 {"parser.manifest.json" (parser-ir-manifest producer-id (files/example-hash "04"))
                  "analysis.manifest.json" (analysis-manifest (files/example-hash "32")
                                                              producer-id
                                                              (files/example-hash "04"))})]
    (is (= [producer-id]
           (mapv #(get % "artifact_id")
                 (manifest-index/parser-ir-producer-candidates
                  entries
                  {"work_content_hash" (files/example-hash "03")
                   "corpus_snapshot_hash" (files/example-hash "02")
                   "parser_build_hash" (files/example-hash "04")
                   "parser_config_hash" (files/example-hash "05")
                   "aat_parser_ir_mapping_hash" (files/example-hash "06")
                   "parser_ir_schema_hash" (files/example-hash "07")}))))))

(deftest analysis-copied-field-validation-test
  (let [producer-id (files/example-hash "31")
        good-analysis (analysis-manifest (files/example-hash "32")
                                         producer-id
                                         (files/example-hash "04"))
        bad-analysis (analysis-manifest (files/example-hash "33")
                                        producer-id
                                        (files/example-hash "99"))
        producer (parser-ir-manifest producer-id (files/example-hash "04"))]
    (is (empty? (manifest-index/analysis-copied-field-errors
                 (manifest-index/index-entries {"parser.manifest.json" producer
                                                "analysis.manifest.json" good-analysis}))))
    (is (= [{:analysis_artifact_id (files/example-hash "33")
             :producer_artifact_id producer-id
             :field "parser_build_hash"
             :analysis_value (files/example-hash "99")
             :producer_value (files/example-hash "04")}]
           (manifest-index/analysis-copied-field-errors
            (manifest-index/index-entries {"parser.manifest.json" producer
                                           "analysis.manifest.json" bad-analysis})))))))
```

Add `[abc.tools.files :as files]` to the namespace require.

- [ ] **Step 2: Run manifest-index tests and confirm they fail**

Run:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.manifest-index-test/parser-ir-producer-lookup-test --focus abc.tools.manifest-index-test/analysis-copied-field-validation-test"
```

Expected: failure because the new manifest-index functions and entry fields do not exist.

- [ ] **Step 3: Extend manifest index entries**

In `abc/src/abc/tools/manifest_index.clj`, change `manifest->index-entry` to include identity and provenance:

```clojure
(defn manifest->index-entry [manifest-path manifest]
  {"manifest_path" (str manifest-path)
   "artifact_id" (get manifest "artifact_id")
   "artifact_kind" (get manifest "artifact_kind")
   "validation_status" (get manifest "validation_status")
   "content_hash" (get-in manifest ["content" "content_hash"])
   "media_type" (get-in manifest ["content" "media_type"])
   "manifest_identity_object" (get manifest "manifest_identity_object")
   "provenance_used" (get-in manifest ["provenance" "used"] [])
   "provenance_was_derived_from" (get-in manifest ["provenance" "was_derived_from"] [])})
```

Update the existing `index-entries-test` expected maps to include:

```clojure
"manifest_identity_object" nil
"provenance_used" []
"provenance_was_derived_from" []
```

- [ ] **Step 4: Add producer lookup and copied-field validation**

Add these definitions to `abc/src/abc/tools/manifest_index.clj`:

```clojure
(def parser-ir-copied-fields
  ["parser_build_hash"
   "parser_config_hash"
   "aat_parser_ir_mapping_hash"
   "parser_ir_schema_hash"])

(defn- identity-value [entry field]
  (get-in entry ["manifest_identity_object" field]))

(defn parser-ir-producer-candidates [entries coordinate]
  (->> entries
       (filter successful-entry?)
       (filter #(= "parser-ir" (get % "artifact_kind")))
       (filter (fn [entry]
                 (every? (fn [[field expected]]
                           (= expected (identity-value entry field)))
                         coordinate)))
       (sort-by #(get % "artifact_id"))
       vec))

(defn- entry-by-artifact-id [entries]
  (into {}
        (map (fn [entry] [(get entry "artifact_id") entry]))
        entries))

(defn- find-producer-entry [by-artifact-id analysis-entry]
  (some #(get by-artifact-id %)
        (concat (get analysis-entry "provenance_was_derived_from")
                (get analysis-entry "provenance_used"))))

(defn- copied-field-errors [analysis-entry producer-entry]
  (->> parser-ir-copied-fields
       (keep (fn [field]
               (let [analysis-value (identity-value analysis-entry field)
                     producer-value (identity-value producer-entry field)]
                 (when-not (= analysis-value producer-value)
                   {:analysis_artifact_id (get analysis-entry "artifact_id")
                    :producer_artifact_id (get producer-entry "artifact_id")
                    :field field
                    :analysis_value analysis-value
                    :producer_value producer-value}))))
       vec))

(defn analysis-copied-field-errors [entries]
  (let [by-artifact-id (entry-by-artifact-id entries)]
    (->> entries
         (filter successful-entry?)
         (filter #(= "analysis" (get % "artifact_kind")))
         (mapcat (fn [analysis-entry]
                   (let [producer-entry (find-producer-entry by-artifact-id analysis-entry)]
                     (if producer-entry
                       (copied-field-errors analysis-entry producer-entry)
                       [{:analysis_artifact_id (get analysis-entry "artifact_id")
                         :producer_artifact_id nil
                         :field "__producer__"
                         :analysis_value (concat (get analysis-entry "provenance_was_derived_from")
                                                 (get analysis-entry "provenance_used"))
                         :producer_value nil}]))))
         vec)))

(defn validate-analysis-copied-fields! [entries]
  (let [errors (analysis-copied-field-errors entries)]
    (when (seq errors)
      (throw (ex-info "Analysis manifest copied identity fields differ from producer"
                      {:type :abc/analysis-copied-field-conflict
                       :errors errors}))))
  true)
```

- [ ] **Step 5: Run manifest-index tests and confirm they pass**

Run:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.manifest-index-test"
```

Expected: PASS.

- [ ] **Step 6: Commit Task 4**

```bash
git add src/abc/tools/manifest_index.clj \
        test/abc/tools/manifest_index_test.clj
git commit -m "feat(abc): validate analysis producer identity fields"
```

---

### Task 5: Prototype Analysis Slice Materializer

**Files:**
- Create: `abc/src/abc/tools/materialize_analysis.clj`
- Create: `abc/test/abc/tools/materialize_analysis_test.clj`

**Interfaces:**
- Consumes producer parser-IR manifest maps, recipe maps, and fixture metric values.
- Produces:
  - `materialize-analysis/analysis-identity-object`
  - `materialize-analysis/analysis-result-value`
  - `materialize-analysis/analysis-manifest`
  - `materialize-analysis/materialize-analysis!`

- [ ] **Step 1: Write failing materializer test**

Create `abc/test/abc/tools/materialize_analysis_test.clj`:

```clojure
(ns abc.tools.materialize-analysis-test
  (:require [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.materialize-analysis :as materialize-analysis]
            [abc.tools.schema :as schema]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn producer-manifest []
  {"artifact_id" (files/example-hash "31")
   "artifact_kind" "parser-ir"
   "validation_status" "passed"
   "manifest_identity_object" {"manifest_schema_hash" (manifest/schema-hash "schemas/manifest.schema.json")
                               "corpus_snapshot_hash" (files/example-hash "01")
                               "work_content_hash" (files/example-hash "02")
                               "metadata_record_hash" nil
                               "parser_build_hash" (files/example-hash "03")
                               "parser_config_hash" (files/example-hash "04")
                               "aat_parser_ir_mapping_hash" (files/example-hash "05")
                               "parser_ir_schema_hash" (files/example-hash "06")
                               "tei_profile_hash" nil
                               "tokenizer_build_hash" nil
                               "tokenizer_dictionary_hash" nil
                               "analysis_recipe_hash" nil
                               "output_format_spec_hash" (files/example-hash "06")}
   "content" {"content_hash" (files/example-hash "07")
              "media_type" "application/json"
              "byte_length" 100
              "path_hint" "parser-ir.json"}
   "sidecars" []
   "provenance" {"generated_at" "2026-07-07T00:00:00Z"
                 "activity_id" "https://w3id.org/abc/activity/materialize-imported-parser-ir"
                 "agent" "abc.tools.materialize-import"
                 "plan_hash" nil
                 "used" [(files/example-hash "02")]
                 "was_derived_from" [(files/example-hash "02")]}
   "license" nil
   "signatures" []
   "superseded_by" nil
   "invalidated_at" nil
   "replacement_reason" nil
   "notes" nil})

(deftest materialize-analysis-copies-parser-ir-identity-test
  (let [dir (Files/createTempDirectory "abc-analysis" (make-array FileAttribute 0))
        out-dir (.toFile dir)
        recipe (files/read-json "data/analysis-recipes/literary-basic-ja-v1.json")
        result (materialize-analysis/materialize-analysis!
                {:producer-manifest (producer-manifest)
                 :recipe recipe
                 :subject {"source_id" "aozora:example-work"
                           "work_id" "aozora:example-work"}
                 :metrics [{"metric_id" "fixture-line-count"
                            "value" 3
                            "value_type" "integer"
                            "denominator" nil
                            "unit" "line"
                            "status" "passed"}]
                 :output-dir out-dir
                 :generated-at "2026-07-07T00:00:00Z"})]
    (try
      (let [manifest-file (:manifest result)
            result-file (:analysis-result result)
            analysis-manifest (files/read-json manifest-file)
            analysis-result (files/read-json result-file)
            manifest-schema (files/read-json "schemas/manifest.schema.json")
            result-schema (files/read-json "schemas/analysis-result.schema.json")]
        (is (nil? (schema/validation-errors manifest-schema analysis-manifest)))
        (is (nil? (schema/validation-errors result-schema analysis-result)))
        (is (= "analysis" (get analysis-manifest "artifact_kind")))
        (is (= "analysis-result"
               (get-in analysis-manifest ["sidecars" 0 "role"])))
        (is (= (files/example-hash "03")
               (get-in analysis-manifest ["manifest_identity_object" "parser_build_hash"])))
        (is (= (files/example-hash "05")
               (get-in analysis-manifest ["manifest_identity_object" "aat_parser_ir_mapping_hash"])))
        (is (nil? (get-in analysis-manifest ["manifest_identity_object" "tokenizer_build_hash"])))
        (is (nil? (get-in analysis-manifest ["manifest_identity_object" "tokenizer_dictionary_hash"])))
        (is (some #{(files/example-hash "31")}
                  (get-in analysis-manifest ["provenance" "used"])))
        (is (some #{(files/example-hash "31")}
                  (get-in analysis-manifest ["provenance" "was_derived_from"])))
        (is (= (get analysis-manifest "artifact_id")
               (manifest/artifact-id (get analysis-manifest "manifest_identity_object")))))
      (finally
        (doseq [file (reverse (file-seq out-dir))]
          (.delete file))))))
```

- [ ] **Step 2: Run materializer test and confirm it fails**

Run:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.materialize-analysis-test"
```

Expected: failure because `abc.tools.materialize-analysis` does not exist.

- [ ] **Step 3: Implement materializer**

Create `abc/src/abc/tools/materialize_analysis.clj`:

```clojure
(ns abc.tools.materialize-analysis
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [clojure.java.io :as io]))

(def activity-id "https://w3id.org/abc/activity/materialize-analysis")
(def agent "abc.tools.materialize-analysis")

(def copied-parser-ir-fields
  ["parser_build_hash"
   "parser_config_hash"
   "aat_parser_ir_mapping_hash"
   "parser_ir_schema_hash"])

(defn analysis-identity-object
  [{:keys [producer-manifest recipe]}]
  (let [producer-identity (get producer-manifest "manifest_identity_object")
        analysis-recipe-hash (analysis-identity/analysis-recipe-hash recipe)]
    (merge
     {"manifest_schema_hash" (manifest/schema-hash "schemas/manifest.schema.json")
      "corpus_snapshot_hash" (get producer-identity "corpus_snapshot_hash")
      "work_content_hash" (get producer-identity "work_content_hash")
      "metadata_record_hash" nil
      "tei_profile_hash" nil
      "tokenizer_build_hash" nil
      "tokenizer_dictionary_hash" nil
      "analysis_recipe_hash" analysis-recipe-hash
      "output_format_spec_hash" (manifest/schema-hash "schemas/analysis-result.schema.json")}
     (select-keys producer-identity copied-parser-ir-fields))))

(defn analysis-result-value
  [{:keys [producer-manifest recipe subject metrics]}]
  (let [producer-identity (get producer-manifest "manifest_identity_object")
        output-schema-hash (manifest/schema-hash "schemas/analysis-result.schema.json")]
    {"schema_id" "https://w3id.org/abc/schemas/analysis-result.schema.json"
     "schema_hash" output-schema-hash
     "subject" {"source_id" (get subject "source_id")
                "work_id" (get subject "work_id")
                "corpus_snapshot_hash" (get producer-identity "corpus_snapshot_hash")
                "work_content_hash" (get producer-identity "work_content_hash")
                "metadata_record_hash" nil}
     "input_view" {"input_view_kind" "parser-ir-plaintext-body-v1"
                   "producer_artifact_id" (get producer-manifest "artifact_id")
                   "producer_content_hash" (get-in producer-manifest ["content" "content_hash"])
                   "plaintext_policy_hash" (get recipe "plaintext_policy_hash")
                   "coordinate_system" "unicode-scalar-value"}
     "tokenizer_profile_hash" nil
     "analysis_recipe_hash" (analysis-identity/analysis-recipe-hash recipe)
     "metrics" (vec metrics)
     "warnings" []}))

(defn- sidecar [file]
  {"role" "analysis-result"
   "hash" (str "sha256:" (files/sha256-file file))
   "media_type" "application/json"
   "path_hint" "analysis-result.json"})

(defn analysis-manifest
  [{:keys [producer-manifest recipe result-file generated-at]}]
  (let [identity-object (analysis-identity-object {:producer-manifest producer-manifest
                                                   :recipe recipe})
        producer-artifact-id (get producer-manifest "artifact_id")]
    (manifest/artifact-manifest
     {:artifact-kind "analysis"
      :validation-status "passed"
      :identity-object identity-object
      :content (manifest/content result-file
                                 "application/json"
                                 "analysis-result.json"
                                 files/sha256-file)
      :sidecars [(sidecar result-file)]
      :generated-at generated-at
      :activity-id activity-id
      :agent agent
      :plan-hash nil
      :used [producer-artifact-id
             (get recipe "plaintext_policy_hash")
             (analysis-identity/analysis-recipe-hash recipe)
             (manifest/schema-hash "schemas/analysis-result.schema.json")]
      :was-derived-from [producer-artifact-id]
      :notes "Generated from parser-ir-plaintext-body-v1 analysis fixture input."})))

(defn materialize-analysis!
  [{:keys [producer-manifest recipe subject metrics output-dir generated-at]}]
  (let [output-dir (io/file output-dir)
        result-file (io/file output-dir "analysis-result.json")
        manifest-file (io/file output-dir "analysis.manifest.json")
        result-value (analysis-result-value {:producer-manifest producer-manifest
                                             :recipe recipe
                                             :subject subject
                                             :metrics metrics})]
    (manifest/write-json-file! result-file result-value)
    (manifest/write-json-file! manifest-file
                               (analysis-manifest {:producer-manifest producer-manifest
                                                   :recipe recipe
                                                   :result-file result-file
                                                   :generated-at generated-at}))
    {:analysis-result result-file
     :manifest manifest-file}))
```

- [ ] **Step 4: Run materializer test and confirm it passes**

Run:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.materialize-analysis-test"
```

Expected: PASS.

- [ ] **Step 5: Commit Task 5**

```bash
git add src/abc/tools/materialize_analysis.clj \
        test/abc/tools/materialize_analysis_test.clj
git commit -m "feat(abc): materialize parser-ir analysis slices"
```

---

### Task 6: Validate Design Bundle Integration

**Files:**
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`
- Modify: `abc/test/abc/tools/validate_design_bundle_test.clj`

**Interfaces:**
- Consumes schemas and fixtures from Tasks 1 and 2.
- Consumes manifest-index copied-field validation from Task 4.
- Produces release-gate validation for analysis schema fixtures and copied producer fields.

- [ ] **Step 1: Write failing validation tests**

Add this require to `abc/test/abc/tools/validate_design_bundle_test.clj`:

```clojure
[abc.tools.manifest-index :as manifest-index]
```

Add these tests:

```clojure
(deftest validate-json-schemas-includes-analysis-fixtures-test
  (is (nil? (validate/validate-json-schemas! []))))

(deftest validate-analysis-copied-fields-gate-test
  (let [producer-id (files/example-hash "31")
        producer {"artifact_id" producer-id
                  "artifact_kind" "parser-ir"
                  "validation_status" "passed"
                  "manifest_identity_object" {"parser_build_hash" (files/example-hash "01")
                                              "parser_config_hash" (files/example-hash "02")
                                              "aat_parser_ir_mapping_hash" (files/example-hash "03")
                                              "parser_ir_schema_hash" (files/example-hash "04")}
                  "content" {"content_hash" (files/example-hash "05")
                             "media_type" "application/json"}
                  "provenance" {"used" []
                                "was_derived_from" []}}
        analysis (assoc producer
                        "artifact_id" (files/example-hash "32")
                        "artifact_kind" "analysis"
                        "manifest_identity_object" {"parser_build_hash" (files/example-hash "99")
                                                    "parser_config_hash" (files/example-hash "02")
                                                    "aat_parser_ir_mapping_hash" (files/example-hash "03")
                                                    "parser_ir_schema_hash" (files/example-hash "04")}
                        "provenance" {"used" [producer-id]
                                      "was_derived_from" [producer-id]})
        entries (manifest-index/index-entries {"parser.manifest.json" producer
                                               "analysis.manifest.json" analysis})]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Analysis manifest copied identity fields differ from producer"
                          (manifest-index/validate-analysis-copied-fields! entries)))))
```

- [ ] **Step 2: Run validation tests and confirm the relevant failure**

Run:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.validate-design-bundle-test/validate-json-schemas-includes-analysis-fixtures-test --focus abc.tools.validate-design-bundle-test/validate-analysis-copied-fields-gate-test"
```

Expected: the schema fixture test fails until `validate-json-schemas!` includes the new schemas and fixtures.

- [ ] **Step 3: Include analysis schemas and fixtures in validation**

In `abc/src/abc/tools/validate_design_bundle.clj`, inside `validate-json-schemas!`, add:

```clojure
analysis-recipe-schema (files/read-json "schemas/analysis-recipe.schema.json")
analysis-result-schema (files/read-json "schemas/analysis-result.schema.json")
```

Add them to the schema validity `doseq`:

```clojure
["schemas/analysis-recipe.schema.json" analysis-recipe-schema]
["schemas/analysis-result.schema.json" analysis-result-schema]
```

After existing fixture validation calls, add:

```clojure
(validate-json! analysis-recipe-schema
                "data/analysis-recipes/literary-basic-ja-v1.json")
(validate-json! analysis-result-schema
                "examples/v0/example-work/analysis-result.json")
```

- [ ] **Step 4: Add copied-field validation hook for manifest indexes**

In `abc/src/abc/tools/validate_design_bundle.clj`, replace the existing
`"==> Checking materialized manifest index"` block with:

```clojure
(tel/log! :info "==> Checking materialized manifest index")
(let [entries (manifest-index/index-manifest-files
               (concat (vals materialized)
                       [(:plaintext-manifest publication-output)
                        (:tei-manifest publication-output)]))]
    (manifest-index/validate-no-reproducibility-conflicts! entries)
    (manifest-index/validate-analysis-copied-fields! entries))
(tel/log! :info "materialized manifest index ok")
```

- [ ] **Step 5: Run focused validation tests and confirm they pass**

Run:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.validate-design-bundle-test/validate-json-schemas-includes-analysis-fixtures-test --focus abc.tools.validate-design-bundle-test/validate-analysis-copied-fields-gate-test"
```

Expected: PASS.

- [ ] **Step 6: Run full relevant test set**

Run:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.schema-test --focus abc.tools.analysis-identity-test --focus abc.tools.manifest-index-test --focus abc.tools.materialize-analysis-test --focus abc.tools.validate-design-bundle-test/validate-json-schemas-includes-analysis-fixtures-test --focus abc.tools.validate-design-bundle-test/validate-analysis-copied-fields-gate-test"
```

Expected: PASS.

- [ ] **Step 7: Commit Task 6**

```bash
git add src/abc/tools/validate_design_bundle.clj \
        test/abc/tools/validate_design_bundle_test.clj
git commit -m "feat(abc): validate analysis slice contracts"
```

---

## Final Verification

Run these commands after all tasks are complete:

```bash
cd abc
nix develop .#default --command bash -lc "./bin/kaocha --focus abc.tools.schema-test --focus abc.tools.analysis-identity-test --focus abc.tools.manifest-index-test --focus abc.tools.materialize-analysis-test --focus abc.tools.validate-design-bundle-test"
nix run .#validate-design-bundle
git diff --check
```

Expected:

- Kaocha exits 0.
- `nix run .#validate-design-bundle` exits 0.
- `git diff --check` exits 0.

## Self-Review

Spec coverage:

- Per-work `analysis` slice: Task 5.
- `parser-ir-plaintext-body-v1` identity map: Tasks 4 and 5.
- Recipe and result schema contracts: Task 1.
- `analysis-result` sidecar role: Task 2.
- Request-set identity construction: Task 3.
- Copied producer-field validation: Tasks 4 and 6.
- Parser-IR producer lookup: Task 4.
- Nix-safe realization constraints: encoded as global constraints; no Nix app is wired in this first implementation plan.

Out of scope by ADR 0026:

- Tokenized slices.
- Tokenizer profile/config identity field.
- Collection analysis packs.
- Parquet pack layout.
- Batch or single-CAS realization.
- Corpus-statistics artifacts.
- Warehouse run-provenance bridge.
- PROV-O JSON-LD export.
