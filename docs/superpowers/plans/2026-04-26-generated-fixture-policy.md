# Generated Fixture Policy Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add checked-in materialized import fixtures and make design-bundle validation regenerate and compare them deterministically.

**Architecture:** `examples/ab-validator-output/` remains the handoff contract from the external parser comparison work. `abc.tools.materialize-import` remains the only writer for imported output manifests. `abc.tools.validate-design-bundle` regenerates manifests into a temporary directory, validates them, and compares them byte-for-byte with checked-in fixtures under `examples/materialized-import/`.

**Tech Stack:** Clojure, `charred`, `clojure.test`, existing Nix apps, JSON Schema Draft 2020-12 validation through `abc.tools.validate-design-bundle`.

---

## File Structure

- Create: `docs/adr/0011-generated-fixture-policy.md`
  - Records why generated materialized-import manifests are checked in, how they are regenerated, and why byte-for-byte comparison is the v0 rule.
- Modify: `docs/v0-design-bundle/README.md`
  - Adds the materialized fixture to the v0 bundle deliverables and notes that validation-result and query-index schemas remain deferred.
- Modify: `src/abc/tools/manifest.clj`
  - Adds deterministic JSON preparation for generated manifest files.
  - Keeps `artifact_id` computation unchanged.
- Modify: `src/abc/tools/validate_design_bundle.clj`
  - Adds a fixture comparison step after regenerating imported manifests.
  - Compares `parser-ir.manifest.json` and `warnings.manifest.json` byte-for-byte against `examples/materialized-import/`.
- Modify: `test/abc/tools/materialize_import_test.clj`
  - Adds deterministic writer and generated fixture expectations around materialized import output.
- Create: `examples/materialized-import/parser-ir.manifest.json`
  - Checked-in generated parser IR manifest.
- Create: `examples/materialized-import/warnings.manifest.json`
  - Checked-in generated warning sidecar manifest.

## Task 1: Record Generated Fixture Policy

**Files:**
- Create: `docs/adr/0011-generated-fixture-policy.md`
- Modify: `docs/v0-design-bundle/README.md`

- [ ] **Step 1: Add the ADR**

Create `docs/adr/0011-generated-fixture-policy.md` with this content:

```markdown
# ADR 0011: Generated Fixture Policy

Date: 2026-04-26
Status: Accepted

## Context

The v0 design bundle imports parser comparison output from `examples/ab-validator-output/` and materializes ABC manifests from it. Without a checked-in generated fixture, changes to materialization code can silently alter manifest bytes while still passing schema validation.

The high-level architecture note treats artifact identity, canonicalization, and generated publication views as reproducibility boundaries. The materialized import fixtures are the first concrete regression target for that boundary.

## Decision

Check in generated materialized-import manifests under `examples/materialized-import/`.

The source of truth for those files is:

```bash
nix run .#materialize-import -- examples/ab-validator-output examples/materialized-import 2026-04-26T00:00:00Z
```

`nix run .#validate-design-bundle` regenerates the same manifests into a temporary directory and compares them byte-for-byte with the checked-in fixture files.

Generated JSON files must be written deterministically:

- Object keys are emitted in lexicographic order at every object level.
- Array order is preserved, because arrays are semantic order unless a producing function sorts them before writing.
- `manifest_identity_object` remains the only input to `artifact_id`.
- `artifact_id` is never nested inside `manifest_identity_object`.
- `provenance.used` and `provenance.was_derived_from` are sorted by the manifest writer before output.

## Consequences

The checked-in fixture becomes a reviewable contract for materialized import output.

Changes to generated manifest bytes require an intentional fixture update in the same commit as the code or schema change that caused them.

Byte-for-byte comparison is stricter than structural comparison. This is intentional for v0 because it exposes nondeterministic writers before the project has multiple implementations.

## Acceptance Criteria

- `examples/materialized-import/parser-ir.manifest.json` exists and validates against `schemas/manifest.schema.json`.
- `examples/materialized-import/warnings.manifest.json` exists and validates against `schemas/manifest.schema.json`.
- `nix run .#validate-design-bundle` regenerates both files and fails if either checked-in fixture differs.
- `clojure -M:test` includes a focused test proving deterministic JSON output for maps with differently ordered input keys.
```

- [ ] **Step 2: Update v0 bundle README**

In `docs/v0-design-bundle/README.md`, add this row to the deliverables table:

```markdown
| Materialized import fixtures | `examples/materialized-import/*.manifest.json` | Checked-in manifests generated from `examples/ab-validator-output/`; validation regenerates and compares them byte-for-byte. |
```

Add this note under the known gaps or non-goals section:

```markdown
Validation-result and query-index schemas are not part of v0. The example files remain documented fixtures until the first query/indexing ADR defines their schema contract.
```

- [ ] **Step 3: Verify documentation formatting**

Run:

```bash
git diff --check -- docs/adr/0011-generated-fixture-policy.md docs/v0-design-bundle/README.md
```

Expected: no output and exit code `0`.

- [ ] **Step 4: Commit Task 1**

Run:

```bash
git add docs/adr/0011-generated-fixture-policy.md docs/v0-design-bundle/README.md
git commit -m "docs: define generated fixture policy"
```

Expected: commit succeeds.

## Task 2: Make Generated Manifest JSON Deterministic

**Files:**
- Modify: `src/abc/tools/manifest.clj`
- Modify: `test/abc/tools/materialize_import_test.clj`

- [ ] **Step 1: Add a failing deterministic writer test**

In `test/abc/tools/materialize_import_test.clj`, add these imports to the namespace form:

```clojure
(:import [java.nio.file Files]
         [java.nio.file.attribute FileAttribute]))
```

If the namespace already imports `FileAttribute` later during implementation, keep one import block only.

Add this test after `artifact-id-test`:

```clojure
(deftest deterministic-json-writer-test
  (let [dir (Files/createTempDirectory "abc-json-writer" (make-array FileAttribute 0))
        file-a (.toFile (.resolve dir "a.json"))
        file-b (.toFile (.resolve dir "b.json"))
        value-a {"z" [{"b" "2" "a" "1"}]
                 "a" {"d" "4" "c" "3"}}
        value-b {"a" {"c" "3" "d" "4"}
                 "z" [{"a" "1" "b" "2"}]}]
    (try
      (manifest/write-json-file! file-a value-a)
      (manifest/write-json-file! file-b value-b)
      (is (= (slurp file-a) (slurp file-b)))
      (is (= "{\n  \"a\": {\n    \"c\": \"3\",\n    \"d\": \"4\"\n  },\n  \"z\": [\n    {\n      \"a\": \"1\",\n      \"b\": \"2\"\n    }\n  ]\n}\n"
             (slurp file-a)))
      (finally
        (doseq [file (reverse (file-seq (.toFile dir)))]
          (.delete file))))))
```

- [ ] **Step 2: Run the focused test and verify it fails**

Run:

```bash
clojure -M:test -e '(require (quote abc.tools.materialize-import-test)) (clojure.test/run-tests (quote abc.tools.materialize-import-test))'
```

Expected before implementation: the new `deterministic-json-writer-test` fails because `write-json-file!` does not recursively sort map keys before writing.

- [ ] **Step 3: Implement deterministic JSON preparation**

In `src/abc/tools/manifest.clj`, add this function near `write-json-file!`:

```clojure
(defn stable-json-value [value]
  (cond
    (map? value)
    (into (sorted-map)
          (map (fn [[k v]]
                 [k (stable-json-value v)]))
          value)

    (vector? value)
    (mapv stable-json-value value)

    (sequential? value)
    (mapv stable-json-value value)

    :else
    value))
```

Replace `write-json-file!` with:

```clojure
(defn write-json-file! [file value]
  (io/make-parents file)
  (with-open [writer (io/writer file)]
    (json/write-json writer (stable-json-value value) :indent-str "  ")
    (.write writer "\n"))
  file)
```

- [ ] **Step 4: Run the focused test and verify it passes**

Run:

```bash
clojure -M:test -e '(require (quote abc.tools.materialize-import-test)) (clojure.test/run-tests (quote abc.tools.materialize-import-test))'
```

Expected: `0 failures, 0 errors`.

- [ ] **Step 5: Run materialization tests**

Run:

```bash
clojure -M:test -e '(require (quote abc.tools.materialize-import-test)) (clojure.test/run-tests (quote abc.tools.materialize-import-test))'
```

Expected: `0 failures, 0 errors`.

- [ ] **Step 6: Commit Task 2**

Run:

```bash
git add src/abc/tools/manifest.clj test/abc/tools/materialize_import_test.clj
git commit -m "feat: write manifests deterministically"
```

Expected: commit succeeds.

## Task 3: Check In Materialized Import Fixtures

**Files:**
- Create: `examples/materialized-import/parser-ir.manifest.json`
- Create: `examples/materialized-import/warnings.manifest.json`
- Modify: `test/abc/tools/materialize_import_test.clj`

- [ ] **Step 1: Generate fixture files**

Run:

```bash
rm -rf examples/materialized-import
nix run .#materialize-import -- examples/ab-validator-output examples/materialized-import 2026-04-26T00:00:00Z
```

Expected output includes:

```text
materialized imported parser output to examples/materialized-import
```

- [ ] **Step 2: Add a fixture comparison test**

In `test/abc/tools/materialize_import_test.clj`, add this helper near the tests:

```clojure
(defn file-bytes [file]
  (Files/readAllBytes (.toPath (io/file file))))
```

Add this test after `materialize-import-test`:

```clojure
(deftest materialized-fixture-test
  (let [out-dir (Files/createTempDirectory "abc-materialize-fixture" (make-array FileAttribute 0))
        out-file (.toFile out-dir)]
    (try
      (materialize/materialize-import!
       {:input-dir (io/file "examples/ab-validator-output")
        :output-dir out-file
        :generated-at "2026-04-26T00:00:00Z"})
      (is (= (seq (file-bytes "examples/materialized-import/parser-ir.manifest.json"))
             (seq (file-bytes (io/file out-file "parser-ir.manifest.json")))))
      (is (= (seq (file-bytes "examples/materialized-import/warnings.manifest.json"))
             (seq (file-bytes (io/file out-file "warnings.manifest.json")))))
      (finally
        (doseq [file (reverse (file-seq out-file))]
          (.delete file))))))
```

- [ ] **Step 3: Run fixture comparison test**

Run:

```bash
clojure -M:test -e '(require (quote abc.tools.materialize-import-test)) (clojure.test/run-tests (quote abc.tools.materialize-import-test))'
```

Expected: `0 failures, 0 errors`.

- [ ] **Step 4: Confirm fixture manifests validate**

Run:

```bash
python - <<'PY'
import json
from pathlib import Path
from jsonschema import Draft202012Validator

schema = json.loads(Path("schemas/manifest.schema.json").read_text(encoding="utf-8"))
validator = Draft202012Validator(schema)
for path in [
    "examples/materialized-import/parser-ir.manifest.json",
    "examples/materialized-import/warnings.manifest.json",
]:
    validator.validate(json.loads(Path(path).read_text(encoding="utf-8")))
print("materialized fixtures validate")
PY
```

Expected:

```text
materialized fixtures validate
```

- [ ] **Step 5: Commit Task 3**

Run:

```bash
git add examples/materialized-import test/abc/tools/materialize_import_test.clj
git commit -m "test: add materialized import fixtures"
```

Expected: commit succeeds.

## Task 4: Validate Checked-In Fixtures in the Design Bundle

**Files:**
- Modify: `src/abc/tools/validate_design_bundle.clj`
- Modify: `test/abc/tools/validate_design_bundle_test.clj`

- [ ] **Step 1: Add fixture comparison helpers**

In `src/abc/tools/validate_design_bundle.clj`, add `clojure.java.io` to the namespace:

```clojure
[clojure.java.io :as io]
```

Add these helpers near `check-errors!`:

```clojure
(def materialized-fixture-paths
  {"parser-ir.manifest.json" (files/path "examples" "materialized-import" "parser-ir.manifest.json")
   "warnings.manifest.json" (files/path "examples" "materialized-import" "warnings.manifest.json")})

(defn file-bytes [file]
  (java.nio.file.Files/readAllBytes (.toPath (io/file file))))

(defn same-file-bytes? [left right]
  (java.util.Arrays/equals (file-bytes left) (file-bytes right)))

(defn validate-materialized-fixtures! [materialized-dir]
  (doseq [[name fixture-file] materialized-fixture-paths
          :let [generated-file (io/file materialized-dir name)]]
    (when-not (same-file-bytes? fixture-file generated-file)
      (throw (ex-info (str "materialized fixture differs: " name)
                      {:fixture (str fixture-file)
                       :generated (str generated-file)})))))
```

- [ ] **Step 2: Include checked-in fixtures in JSON Schema validation**

In `validate-design-bundle!`, change the call to `validate-json-schemas!` so it passes both generated files and checked-in fixtures:

```clojure
(validate-json-schemas! (concat (vals materialized)
                                (vals materialized-fixture-paths)))
```

- [ ] **Step 3: Add the byte-for-byte fixture check to the validation flow**

In `validate-design-bundle!`, after JSON Schema validation succeeds, add:

```clojure
(println "==> Comparing materialized import fixtures")
(validate-materialized-fixtures! materialized-dir)
(println "materialized import fixtures ok")
```

- [ ] **Step 4: Add focused tests for byte comparison**

In `test/abc/tools/validate_design_bundle_test.clj`, require the validator namespace if it is not already required:

```clojure
[abc.tools.validate-design-bundle :as validate]
```

Add this test:

```clojure
(deftest same-file-bytes-test
  (let [dir (java.nio.file.Files/createTempDirectory
             "abc-byte-compare"
             (make-array java.nio.file.attribute.FileAttribute 0))
        left (.toFile (.resolve dir "left.txt"))
        same (.toFile (.resolve dir "same.txt"))
        different (.toFile (.resolve dir "different.txt"))]
    (try
      (spit left "same\n")
      (spit same "same\n")
      (spit different "different\n")
      (is (true? (validate/same-file-bytes? left same)))
      (is (false? (validate/same-file-bytes? left different)))
      (finally
        (doseq [file (reverse (file-seq (.toFile dir)))]
          (.delete file))))))
```

- [ ] **Step 5: Run focused validation tests**

Run:

```bash
clojure -M:test -e '(require (quote abc.tools.validate-design-bundle-test)) (clojure.test/run-tests (quote abc.tools.validate-design-bundle-test))'
```

Expected: `0 failures, 0 errors`.

- [ ] **Step 6: Run design bundle validation**

Run:

```bash
nix run .#validate-design-bundle
```

Expected output includes:

```text
materialized import fixtures ok
design bundle validation ok
```

- [ ] **Step 7: Commit Task 4**

Run:

```bash
git add src/abc/tools/validate_design_bundle.clj test/abc/tools/validate_design_bundle_test.clj
git commit -m "feat: validate materialized import fixtures"
```

Expected: commit succeeds.

## Task 5: Final Verification

**Files:**
- Inspect all changed files from Tasks 1-4.

- [ ] **Step 1: Run all Clojure tests**

Run:

```bash
clojure -M:test
```

Expected: process exits `0`.

- [ ] **Step 2: Run the design bundle validator**

Run:

```bash
nix run .#validate-design-bundle
```

Expected output includes:

```text
design bundle validation ok
```

- [ ] **Step 3: Run flake checks**

Run:

```bash
nix flake check
```

Expected: process exits `0`.

- [ ] **Step 4: Check whitespace**

Run:

```bash
git diff --check -- docs examples src test
```

Expected: no output and exit code `0`.

- [ ] **Step 5: Inspect history and status**

Run:

```bash
git status --short
git log --oneline -5
```

Expected: `git status --short` has no output. Recent commits include:

```text
feat: validate materialized import fixtures
test: add materialized import fixtures
feat: write manifests deterministically
docs: define generated fixture policy
```

## Self-Review Checklist

- [ ] The plan creates the missing generated fixture policy ADR.
- [ ] The plan makes JSON writer output deterministic before checking in generated fixture bytes.
- [ ] The plan checks in materialized import fixtures under `examples/materialized-import/`.
- [ ] The plan validates generated and checked-in manifests against `schemas/manifest.schema.json`.
- [ ] The plan compares regenerated manifests with checked-in fixtures byte-for-byte.
- [ ] The plan leaves `artifact_id` computation scoped to `manifest_identity_object`.
- [ ] The plan avoids adding a new shell script; existing Clojure and Nix entry points remain the interface.
