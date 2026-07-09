# Ruby Annotation View (ADR 0028 First Slice) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Materialize per-work ruby/gaiji annotation views aligned to the plaintext body, with manifest identity, copied-field validation, and a token join, per ADR 0028 and the settled design in `docs/superpowers/specs/2026-07-09-ruby-annotation-view-design.md`.

**Architecture:** The plaintext renderer's single traversal is instrumented to also emit annotation records with unicode-scalar spans (design D5). A new materializer mirrors `abc.tools.materialize-tokenized`: copied parser/mapping identity from the producer parser-IR manifest plus a new `annotation_policy_hash` identity coordinate (D1, manifest schema v0.4.4). The token join is a pure function producing the probe's four classifications; it is a generated view, not an artifact.

**Tech Stack:** Clojure (abc tools), JSON Schema + RFC 8785 JCS hashing, Nix design-bundle validation.

## Global Constraints

- All work in a git worktree on a feature branch; merge to main when done (many concurrent sessions).
- Spans are **unicode scalar values**: count with `String#codePointCount`, never `String#length` (ADR 0027 coordinate family).
- `render`/`render-string` plaintext output bytes must not change (accepted ADR 0025 artifacts).
- Null identity fields mean "not applicable", never "unknown" (ADR 0026).
- Schema files carry a `"version"` field tracked in `test/abc/tools/schema_test.clj` `cross-project-schema-versions`.
- Run tests from `abc/`: `clojure -M:test` (all); design bundle: `nix run .#validate-design-bundle`.
- Working dir for all paths below: `abc/` unless prefixed `ab-validator/`.
- `ruby.scope` must NOT appear in any output (design D4).

---

### Task 1: Annotation output schema + ruby-gaiji-v1 policy value

**Files:**
- Create: `schemas/annotation-output.schema.json`
- Create: `data/annotation-policies/ruby-gaiji-v1.json`
- Modify: `src/abc/tools/analysis_identity.clj` (one def, after line 27 `pack-policy-hash`)
- Modify: `test/abc/tools/schema_test.clj` (`cross-project-schema-versions` map)
- Test: `test/abc/tools/annotation_identity_test.clj`

**Interfaces:**
- Produces: `abc.tools.analysis-identity/annotation-policy-hash` (JSON value → `"sha256:<hex>"` string); schema id `https://w3id.org/abc/schemas/annotation-output.schema.json`; policy file with `policy_id "ruby-gaiji-v1"`.

- [ ] **Step 1: Write the failing test**

```clojure
(ns abc.tools.annotation-identity-test
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [clojure.test :refer [deftest is testing]]))

(deftest annotation-policy-hash-test
  (testing "policy value hashes with JCS discipline and is key-order independent"
    (let [policy (files/read-json "data/annotation-policies/ruby-gaiji-v1.json")
          reordered (into (sorted-map-by (comp - compare)) policy)]
      (is (re-matches #"sha256:[0-9a-f]{64}"
                      (analysis-identity/annotation-policy-hash policy)))
      (is (= (analysis-identity/annotation-policy-hash policy)
             (analysis-identity/annotation-policy-hash reordered)))))

(deftest ruby-gaiji-policy-shape-test
  (let [policy (files/read-json "data/annotation-policies/ruby-gaiji-v1.json")]
    (is (= "ruby-gaiji-v1" (get policy "policy_id")))
    (is (= ["gaiji" "ruby"] (vec (sort (get policy "annotation_kinds")))))
    (is (= "parser-ir-plaintext-body-v1" (get policy "aligns_to")))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `clojure -M:test --focus abc.tools.annotation-identity-test`
Expected: FAIL — `annotation-policy-hash` unresolved / policy file missing.

- [ ] **Step 3: Add the hash def, policy value, and schema**

In `src/abc/tools/analysis_identity.clj`, after `(def pack-policy-hash hash-json-value)`:

```clojure
(def annotation-policy-hash hash-json-value)
```

`data/annotation-policies/ruby-gaiji-v1.json`:

```json
{
  "schema_id": "https://w3id.org/abc/schemas/annotation-policy.schema.json",
  "policy_id": "ruby-gaiji-v1",
  "annotation_kinds": ["ruby", "gaiji"],
  "aligns_to": "parser-ir-plaintext-body-v1",
  "coordinate_system": "unicode-scalar-value-plaintext-spans",
  "ruby_fields": ["base", "reading", "direction"],
  "gaiji_fields": ["raw_marker", "unicode", "resolved"],
  "notes": "ruby.scope withheld: mapping invention I-01 (design D4)."
}
```

`schemas/annotation-output.schema.json` (v0.1.0):

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://w3id.org/abc/schemas/annotation-output.schema.json",
  "version": "0.1.0",
  "type": "object",
  "additionalProperties": false,
  "required": ["schema_id", "schema_hash", "annotation_policy_hash",
               "producer", "input_plaintext_policy_hash",
               "coordinate_system", "annotations", "warnings"],
  "properties": {
    "schema_id": { "const": "https://w3id.org/abc/schemas/annotation-output.schema.json" },
    "schema_hash": { "$ref": "#/$defs/hash" },
    "annotation_policy_hash": { "$ref": "#/$defs/hash" },
    "producer": {
      "type": "object",
      "additionalProperties": false,
      "required": ["input_view_kind", "artifact_id", "content_hash"],
      "properties": {
        "input_view_kind": { "const": "parser-ir-plaintext-body-v1" },
        "artifact_id": { "$ref": "#/$defs/hash" },
        "content_hash": { "$ref": "#/$defs/hash" }
      }
    },
    "input_plaintext_policy_hash": { "$ref": "#/$defs/hash" },
    "coordinate_system": { "const": "unicode-scalar-value-plaintext-spans" },
    "annotations": { "type": "array", "items": { "$ref": "#/$defs/annotation" } },
    "warnings": { "type": "array", "items": { "type": "string", "minLength": 1 } }
  },
  "$defs": {
    "hash": { "type": "string", "pattern": "^sha256:[0-9a-f]{64}$" },
    "span": {
      "type": "object",
      "additionalProperties": false,
      "required": ["start", "end"],
      "properties": {
        "start": { "type": "integer", "minimum": 0 },
        "end": { "type": "integer", "minimum": 0 }
      }
    },
    "annotation": {
      "oneOf": [
        {
          "type": "object",
          "additionalProperties": false,
          "required": ["annotation_kind", "span", "ruby"],
          "properties": {
            "annotation_kind": { "const": "ruby" },
            "span": { "$ref": "#/$defs/span" },
            "source_span": { "$ref": "#/$defs/span" },
            "ruby": {
              "type": "object",
              "additionalProperties": false,
              "required": ["base", "reading", "direction"],
              "properties": {
                "base": { "type": "string" },
                "reading": { "type": "string" },
                "direction": { "type": ["string", "null"], "enum": ["left", "right", null] }
              }
            }
          }
        },
        {
          "type": "object",
          "additionalProperties": false,
          "required": ["annotation_kind", "span", "gaiji"],
          "properties": {
            "annotation_kind": { "const": "gaiji" },
            "span": { "$ref": "#/$defs/span" },
            "source_span": { "$ref": "#/$defs/span" },
            "gaiji": {
              "type": "object",
              "additionalProperties": false,
              "required": ["raw_marker", "unicode", "resolved"],
              "properties": {
                "raw_marker": { "type": "string" },
                "unicode": { "type": ["string", "null"] },
                "resolved": { "type": "boolean" }
              }
            }
          }
        }
      ]
    }
  }
}
```

Add to `cross-project-schema-versions` in `test/abc/tools/schema_test.clj`:

```clojure
"schemas/annotation-output.schema.json" "0.1.0"
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `clojure -M:test --focus abc.tools.annotation-identity-test` then `clojure -M:test --focus abc.tools.schema-test`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add schemas/annotation-output.schema.json data/annotation-policies/ src/abc/tools/analysis_identity.clj test/abc/tools/annotation_identity_test.clj test/abc/tools/schema_test.clj
git commit -m "feat(abc): annotation output schema v0.1.0 and ruby-gaiji-v1 policy (ADR 0028)"
```

---

### Task 2: Manifest schema v0.4.4 — `annotation` kind, `body-annotations` role, `annotation_policy_hash`

**Files:**
- Modify: `schemas/manifest.schema.json` (artifact_kind enum ~line 21, sidecar role enum ~line 126, identityObject required+properties ~lines 88–113, top-level `"version"`)
- Modify: `test/abc/tools/schema_test.clj` (version map: `"schemas/manifest.schema.json" "0.4.4"`)
- Modify: every committed manifest fixture whose `manifest_identity_object` lacks the new key (find them in Step 3)
- Test: `test/abc/tools/annotation_identity_test.clj` (extend)

**Interfaces:**
- Produces: manifest schema accepting `artifact_kind "annotation"`, sidecar role `"body-annotations"`, and requiring nullable `annotation_policy_hash` in every identity object. Every existing manifest gains `"annotation_policy_hash": null`.

- [ ] **Step 1: Write the failing test** (append to `annotation_identity_test.clj`)

```clojure
(deftest manifest-schema-accepts-annotation-kind-test
  (let [schema (files/read-json "schemas/manifest.schema.json")]
    (is (= "0.4.4" (get schema "version")))
    (is (some #{"annotation"} (get-in schema ["properties" "artifact_kind" "enum"])))
    (is (some #{"body-annotations"}
              (get-in schema ["$defs" "sidecar" "properties" "role" "enum"])))
    (is (some #{"annotation_policy_hash"}
              (get-in schema ["$defs" "identityObject" "required"])))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `clojure -M:test --focus abc.tools.annotation-identity-test`
Expected: FAIL on all three assertions.

- [ ] **Step 3: Edit the schema, then find and update every fixture**

In `schemas/manifest.schema.json`: bump `"version"` to `"0.4.4"`; add `"annotation"` to the `artifact_kind` enum; add `"body-annotations"` to the sidecar `role` enum; in `$defs.identityObject` add `"annotation_policy_hash"` to `required` and `"annotation_policy_hash": { "$ref": "#/$defs/nullableHash" }` to `properties` (adjacent to `analysis_recipe_hash`).

Then update fixtures:

```bash
grep -rln '"manifest_identity_object"' examples/ test/ data/ | while read f; do
  grep -q '"annotation_policy_hash"' "$f" || echo "NEEDS UPDATE: $f"
done
```

For each listed JSON file, add `"annotation_policy_hash": null` inside `manifest_identity_object` (adjacent to `analysis_recipe_hash`). For Clojure test files constructing identity objects inline, `grep -rn '"analysis_recipe_hash"' test/ src/` and add the null key alongside. `src/abc/tools/materialize_*.clj` identity-object builders each gain `"annotation_policy_hash" nil`.

- [ ] **Step 4: Run the full suite; fix every schema-validation failure it surfaces**

Run: `clojure -M:test`
Expected: PASS. Failures name the remaining fixtures/builders missing the key — fix each the same way. Do NOT relax the schema to optional.

- [ ] **Step 5: Rotate the cross-repo schema pin**

The ab-validator importer pins the ABC manifest schema hash. From `abc/`:

```bash
NEW_HASH=$(clojure -M -e '(require (quote [abc.tools.manifest :as m])) (println (m/schema-hash "schemas/manifest.schema.json"))')
echo "$NEW_HASH"
git -C .. grep -rn "$(git show HEAD:schemas/manifest.schema.json | clojure -M -e '(require (quote [abc.tools.manifest :as m]))' 2>/dev/null; true)" || true
```

Practical procedure: record the pre-edit hash first (`git stash; clojure -M -e …; git stash pop`), then `grep -rn "<old-hash-hex>" .. --include='*.edn' --include='*.clj' --include='*.rs' --include='*.json' -l` and replace with the new hash in every pin site (ab-validator `data/abc-schemas/` copies get the new schema file synced too: `cp schemas/manifest.schema.json ../ab-validator/data/abc-schemas/`). Precedent commit for this exact rotation: `adc3e7f3` (importer pin bump 692dfa23→55eeb377).

- [ ] **Step 6: Run design-bundle validation**

Run: `nix run .#validate-design-bundle`
Expected: PASS. Any failure names a stale pin or fixture — fix and re-run.

- [ ] **Step 7: Commit**

```bash
git add -A
git commit -m "feat(abc): manifest schema v0.4.4 adds annotation kind, body-annotations role, annotation_policy_hash (ADR 0028 D1/D2)"
```

---

### Task 3: Instrument the plaintext renderer traversal (design D5)

**Files:**
- Modify: `src/abc/tools/parser_ir_plaintext.clj`
- Test: `test/abc/tools/parser_ir_annotations_test.clj`

**Interfaces:**
- Produces: `abc.tools.parser-ir-plaintext/render-with-annotations` — parser-IR value → `{:text String :annotations [ann …] :node_counts m :omitted v}` where each `ann` is a string-keyed map matching `schemas/annotation-output.schema.json` `$defs.annotation`. `render` and `render-string` byte-identical to current behavior.

- [ ] **Step 1: Write the failing test**

```clojure
(ns abc.tools.parser-ir-annotations-test
  (:require [abc.tools.parser-ir-plaintext :as plaintext]
            [clojure.test :refer [deftest is testing]]))

(defn- scalar-subs
  "Substring by unicode scalar offsets (NOT UTF-16 indices)."
  [^String s start end]
  (let [i (.offsetByCodePoints s 0 start)
        j (.offsetByCodePoints s 0 end)]
    (subs s i j)))

(def ^:private ir
  {"nodes"
   [{"type" "text" "text" "冒頭"}
    {"type" "ruby" "span" {"start" 6 "end" 12}
     "ruby" {"base" "吾輩" "reading" "わがはい" "scope" "explicit" "direction" "right"}}
    {"type" "text" "text" "は"}
    ;; astral-plane gaiji: U+20B9F 𠮟 — 2 UTF-16 units, 1 unicode scalar
    {"type" "gaiji" "span" {"start" 15 "end" 23}
     "gaiji" {"raw_marker" "※［＃「口＋七」］" "unicode" "𠮟" "resolved" true}}
    {"type" "text" "text" "る"}]})

(deftest render-with-annotations-spans-test
  (let [{:keys [text annotations]} (plaintext/render-with-annotations ir)]
    (testing "plaintext unchanged from render-string"
      (is (= (plaintext/render-string ir) text))
      (is (= "冒頭吾輩は𠮟る" text)))
    (testing "ruby annotation carries plaintext scalar span over the base text"
      (let [ruby (first (filter #(= "ruby" (get % "annotation_kind")) annotations))]
        (is (= "吾輩" (scalar-subs text
                                   (get-in ruby ["span" "start"])
                                   (get-in ruby ["span" "end"]))))
        (is (= "わがはい" (get-in ruby ["ruby" "reading"])))
        (is (= "right" (get-in ruby ["ruby" "direction"])))
        (is (nil? (get-in ruby ["ruby" "scope"])) "scope withheld (D4)")
        (is (= {"start" 6 "end" 12} (get ruby "source_span")))))
    (testing "gaiji span counts astral chars as one scalar"
      (let [gaiji (first (filter #(= "gaiji" (get % "annotation_kind")) annotations))]
        (is (= {"start" 5 "end" 6} (get gaiji "span")))
        (is (= "𠮟" (scalar-subs text 5 6)))))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `clojure -M:test --focus abc.tools.parser-ir-annotations-test`
Expected: FAIL — `render-with-annotations` unresolved.

- [ ] **Step 3: Instrument the accumulator**

In `src/abc/tools/parser_ir_plaintext.clj`:

Add a scalar-count helper and offset/annotation tracking to the two append fns (`:offset` counts unicode scalars appended so far; every existing call site is covered because all text flows through these two fns):

```clojure
(defn- scalar-count [^String s]
  (.codePointCount s 0 (.length s)))

(defn- append-text [acc node-text]
  (let [s (or node-text "")]
    (-> acc
        (update :text str s)
        (update :offset (fnil + 0) (scalar-count s))
        (assoc :source-text-ended-with-newline? false))))

(defn- append-source-text [acc node-text]
  (let [s (whitespace/source-text->plaintext node-text (empty? (:text acc)))]
    (-> acc
        (update :text str s)
        (update :offset (fnil + 0) (scalar-count s))
        (assoc :source-text-ended-with-newline?
               (whitespace/source-text-ends-with-newline? node-text)))))
```

Ruby and gaiji renderers record annotations around the append (keep the existing single-arity delegating form):

```clojure
(defn- render-ruby-node
  ([acc node] (render-ruby-node acc node 0))
  ([acc node _depth]
   (let [start (:offset acc 0)
         acc (append-text acc (get-in node ["ruby" "base"]))]
     (update acc :annotations (fnil conj [])
             (cond-> {"annotation_kind" "ruby"
                      "span" {"start" start "end" (:offset acc)}
                      "ruby" {"base" (or (get-in node ["ruby" "base"]) "")
                              "reading" (or (get-in node ["ruby" "reading"]) "")
                              "direction" (get-in node ["ruby" "direction"])}}
               (get node "span") (assoc "source_span"
                                        (select-keys (get node "span") ["start" "end"])))))))

(defn- render-gaiji-node
  ([acc node] (render-gaiji-node acc node 0))
  ([acc node _depth]
   (let [start (:offset acc 0)
         acc (append-text acc (or (get-in node ["gaiji" "unicode"])
                                  (get-in node ["gaiji" "raw_marker"])))]
     (update acc :annotations (fnil conj [])
             (cond-> {"annotation_kind" "gaiji"
                      "span" {"start" start "end" (:offset acc)}
                      "gaiji" {"raw_marker" (or (get-in node ["gaiji" "raw_marker"]) "")
                               "unicode" (get-in node ["gaiji" "unicode"])
                               "resolved" (boolean (get-in node ["gaiji" "resolved"]))}}
               (get node "span") (assoc "source_span"
                                        (select-keys (get node "span") ["start" "end"])))))))
```

Add the new entry point next to `render` (annotation spans must survive the final text assembly: shift by the front-notes prefix scalar length; `trim-trailing-newlines` only removes trailing body text so spans are unaffected):

```clojure
(defn render-with-annotations [parser-ir]
  (let [{:keys [text front_notes source_notes node_counts omitted annotations
                source-text-ended-with-newline?]}
        (reduce render-node
                {:text "" :offset 0 :annotations []
                 :front_notes [] :source_notes []
                 :node_counts {} :omitted []
                 :source-text-ended-with-newline? false}
                (get parser-ir "nodes"))
        text (if source-text-ended-with-newline?
               (whitespace/trim-trailing-newlines text)
               text)
        prefix (when (seq front_notes)
                 (str (string/join "\n" front_notes) "\n\n"))
        shift (if prefix (scalar-count prefix) 0)
        text (str prefix text
                  (when (seq source_notes)
                    (str "\n\n" (string/join "\n" source_notes))))]
    {:text text
     :annotations (mapv (fn [ann]
                          (update ann "span"
                                  (fn [{:strs [start end]}]
                                    {"start" (+ start shift)
                                     "end" (+ end shift)})))
                        annotations)
     :node_counts node_counts
     :omitted omitted}))
```

Rewrite `render` as a thin projection so there is exactly one traversal implementation:

```clojure
(defn render [parser-ir]
  (dissoc (render-with-annotations parser-ir) :annotations))
```

- [ ] **Step 4: Run the new test AND the existing renderer/publication tests**

Run: `clojure -M:test --focus abc.tools.parser-ir-annotations-test` then `clojure -M:test --focus abc.tools.parser-ir-plaintext-test` then `clojure -M:test --focus abc.tools.materialize-publication-test`
Expected: all PASS — the plaintext byte-identity guard is the existing suites passing untouched.

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/parser_ir_plaintext.clj test/abc/tools/parser_ir_annotations_test.clj
git commit -m "feat(abc): instrument plaintext traversal with unicode-scalar annotation spans (ADR 0028 D5)"
```

---

### Task 4: Annotation materializer

**Files:**
- Create: `src/abc/tools/materialize_annotations.clj`
- Test: `test/abc/tools/materialize_annotations_test.clj`

**Interfaces:**
- Consumes: `plaintext/render-with-annotations` (Task 3), `analysis-identity/annotation-policy-hash` (Task 1), `manifest/artifact-manifest`, `manifest/content`, `manifest/write-json-file!`, `manifest/schema-hash`, `files/sha256-file`.
- Produces: `abc.tools.materialize-annotations/materialize-annotations!` taking `{:producer-manifest m :parser-ir v :annotation-policy p :input-plaintext-policy-hash h :output-dir d :generated-at t}`, writing `body-annotations.json` + `annotation.manifest.json`, returning `{:annotations f :manifest f}`.

- [ ] **Step 1: Write the failing test**

```clojure
(ns abc.tools.materialize-annotations-test
  (:require [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.materialize-annotations :as mat-ann]
            [abc.tools.schema :as schema]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]))

(def ^:private producer-manifest
  (files/read-json "examples/ab-validator-output/manifest-inputs.json"))
;; If that file is not itself a parser-IR manifest, load the committed
;; parser-IR example manifest used by materialize_tokenized_test.clj instead —
;; reuse the same producer fixture that test already loads.

(def ^:private parser-ir
  (files/read-json "examples/ab-validator-output/parser-ir.json"))

(deftest materialize-annotations-test
  (let [dir (.toFile (Files/createTempDirectory "abc-ann" (make-array java.nio.file.attribute.FileAttribute 0)))
        policy (files/read-json "data/annotation-policies/ruby-gaiji-v1.json")
        {:keys [annotations manifest]}
        (mat-ann/materialize-annotations!
         {:producer-manifest producer-manifest
          :parser-ir parser-ir
          :annotation-policy policy
          :input-plaintext-policy-hash "sha256:0000000000000000000000000000000000000000000000000000000000000000"
          :output-dir dir
          :generated-at "2026-07-09T00:00:00Z"})
        manifest-value (files/read-json (.getPath manifest))
        annotations-value (files/read-json (.getPath annotations))]
    (testing "sidecar validates against the annotation output schema"
      (is (schema/valid? "schemas/annotation-output.schema.json" annotations-value)))
    (testing "identity: annotation kind, policy hash present, tokenizer fields null"
      (is (= "annotation" (get manifest-value "artifact_kind")))
      (let [ident (get manifest-value "manifest_identity_object")]
        (is (re-matches #"sha256:[0-9a-f]{64}" (get ident "annotation_policy_hash")))
        (is (nil? (get ident "tokenizer_profile_hash")))
        (is (nil? (get ident "analysis_recipe_hash")))
        (testing "parser/mapping fields copied byte-for-byte incl. nulls"
          (doseq [k ["parser_build_hash" "parser_config_hash"
                     "aat_parser_ir_mapping_hash" "parser_ir_schema_hash"]]
            (is (= (get-in producer-manifest ["manifest_identity_object" k])
                   (get ident k))
                k)))))
    (testing "provenance cites producer in used and was_derived_from"
      (let [prov (get manifest-value "provenance")]
        (is (some #{(get producer-manifest "artifact_id")} (get prov "used")))
        (is (some #{(get producer-manifest "artifact_id")} (get prov "was_derived_from")))))
    (testing "sidecar role"
      (is (= "body-annotations" (get-in manifest-value ["sidecars" 0 "role"]))))))
```

(If `abc.tools.schema/valid?` has a different name, mirror whatever
`materialize_tokenized_test.clj` uses to schema-validate a sidecar — reuse,
don't invent.)

- [ ] **Step 2: Run test to verify it fails**

Run: `clojure -M:test --focus abc.tools.materialize-annotations-test`
Expected: FAIL — namespace `abc.tools.materialize-annotations` missing.

- [ ] **Step 3: Implement the materializer** (mirrors `materialize_tokenized.clj` line-for-line in structure)

```clojure
(ns abc.tools.materialize-annotations
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.parser-ir-plaintext :as plaintext]
            [clojure.java.io :as io]))

(def activity-id "https://w3id.org/abc/activity/materialize-annotations")
(def tool-agent "abc.tools.materialize-annotations")

(def ^:private copied-parser-ir-fields
  ["parser_build_hash"
   "parser_config_hash"
   "aat_parser_ir_mapping_hash"
   "parser_ir_schema_hash"])

(def ^:private coordinate-system "unicode-scalar-value-plaintext-spans")
(def ^:private input-view-kind "parser-ir-plaintext-body-v1")

(defn annotation-identity-object
  [{:keys [producer-manifest annotation-policy]}]
  (let [producer-identity (get producer-manifest "manifest_identity_object")]
    (merge
     {"manifest_schema_hash" (manifest/schema-hash "schemas/manifest.schema.json")
      "corpus_snapshot_hash" (get producer-identity "corpus_snapshot_hash")
      "work_content_hash" (get producer-identity "work_content_hash")
      "metadata_record_hash" (get producer-identity "metadata_record_hash")
      "tei_profile_hash" nil
      "tokenizer_build_hash" nil
      "tokenizer_dictionary_hash" nil
      "tokenizer_profile_hash" nil
      "analysis_recipe_hash" nil
      "annotation_policy_hash" (analysis-identity/annotation-policy-hash annotation-policy)
      "output_format_spec_hash" (manifest/schema-hash "schemas/annotation-output.schema.json")}
     (select-keys producer-identity copied-parser-ir-fields))))

(defn annotation-value
  [{:keys [producer-manifest annotation-policy input-plaintext-policy-hash
           annotations warnings]}]
  {"schema_id" "https://w3id.org/abc/schemas/annotation-output.schema.json"
   "schema_hash" (manifest/schema-hash "schemas/annotation-output.schema.json")
   "annotation_policy_hash" (analysis-identity/annotation-policy-hash annotation-policy)
   "producer" {"input_view_kind" input-view-kind
               "artifact_id" (get producer-manifest "artifact_id")
               "content_hash" (get-in producer-manifest ["content" "content_hash"])}
   "input_plaintext_policy_hash" input-plaintext-policy-hash
   "coordinate_system" coordinate-system
   "annotations" (vec annotations)
   "warnings" (vec (or warnings []))})

(defn- sidecar [file]
  {"role" "body-annotations"
   "hash" (str "sha256:" (files/sha256-file file))
   "media_type" "application/json"
   "path_hint" "body-annotations.json"})

(defn annotation-manifest
  [{:keys [producer-manifest annotation-policy input-plaintext-policy-hash
           annotations-file generated-at]}]
  (let [identity-object (annotation-identity-object
                         {:producer-manifest producer-manifest
                          :annotation-policy annotation-policy})
        producer-artifact-id (get producer-manifest "artifact_id")
        policy-hash (analysis-identity/annotation-policy-hash annotation-policy)]
    (manifest/artifact-manifest
     {:artifact-kind "annotation"
      :validation-status "passed"
      :identity-object identity-object
      :content (manifest/content annotations-file
                                 "application/json"
                                 "body-annotations.json"
                                 files/sha256-file)
      :sidecars [(sidecar annotations-file)]
      :generated-at generated-at
      :activity-id activity-id
      :agent tool-agent
      :plan-hash nil
      :used [producer-artifact-id
             (get-in producer-manifest ["content" "content_hash"])
             policy-hash
             input-plaintext-policy-hash]
      :was-derived-from [producer-artifact-id]
      :notes "Ruby/gaiji annotation view aligned to parser-ir-plaintext-body-v1."})))

(defn materialize-annotations!
  [{:keys [producer-manifest parser-ir annotation-policy
           input-plaintext-policy-hash output-dir generated-at]}]
  (let [output-dir (io/file output-dir)
        annotations-file (io/file output-dir "body-annotations.json")
        manifest-file (io/file output-dir "annotation.manifest.json")
        {:keys [annotations]} (plaintext/render-with-annotations parser-ir)]
    (manifest/write-json-file! annotations-file
                               (annotation-value
                                {:producer-manifest producer-manifest
                                 :annotation-policy annotation-policy
                                 :input-plaintext-policy-hash input-plaintext-policy-hash
                                 :annotations annotations}))
    (manifest/write-json-file! manifest-file
                               (annotation-manifest
                                {:producer-manifest producer-manifest
                                 :annotation-policy annotation-policy
                                 :input-plaintext-policy-hash input-plaintext-policy-hash
                                 :annotations-file annotations-file
                                 :generated-at generated-at}))
    {:annotations annotations-file
     :manifest manifest-file}))
```

- [ ] **Step 4: Run test to verify it passes**

Run: `clojure -M:test --focus abc.tools.materialize-annotations-test`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/materialize_annotations.clj test/abc/tools/materialize_annotations_test.clj
git commit -m "feat(abc): annotation-view materializer with copied producer identity (ADR 0028)"
```

---

### Task 5: Manifest-index release validation for annotation artifacts

**Files:**
- Modify: `src/abc/tools/manifest_index.clj` (locate the tokenized copied-field validation — `grep -n "tokenized" src/abc/tools/manifest_index.clj` — and extend the same dispatch to `"annotation"`)
- Test: `test/abc/tools/manifest_index_test.clj` (extend)

**Interfaces:**
- Consumes: Task 4 manifest shape.
- Produces: release validation failing when (a) an annotation manifest's copied parser/mapping fields differ from the producer parser-IR manifest named in provenance, or (b) a successful `annotation` manifest has missing/null `annotation_policy_hash`.

- [ ] **Step 1: Write the failing tests** (extend `manifest_index_test.clj`, mirroring its existing tokenized copied-field failure cases — copy the tokenized test pair in that file, switch `artifact_kind` to `"annotation"`, the mutated field to `parser_build_hash`, and the guardrail field to `annotation_policy_hash`)

Two cases:
1. Annotation manifest whose `parser_build_hash` differs from producer → validator reports the conflict.
2. Successful annotation manifest with `"annotation_policy_hash": null` → validator rejects.

Use the same builder helpers the tokenized cases in that file use — reuse, don't re-derive.

- [ ] **Step 2: Run to verify both fail**

Run: `clojure -M:test --focus abc.tools.manifest-index-test`
Expected: the two new cases FAIL (validator currently ignores annotation kind).

- [ ] **Step 3: Extend the validator dispatch**

In `manifest_index.clj`, wherever `artifact_kind = "tokenized"` selects `copied-parser-ir-fields` checking and the profile-coordinate guardrail, add `"annotation"` with the same copied field list and a `annotation_policy_hash`-must-be-non-null-when-successful rule. Follow the existing code shape exactly — this is a dispatch extension, not new machinery.

- [ ] **Step 4: Run to verify pass**

Run: `clojure -M:test --focus abc.tools.manifest-index-test`
Expected: PASS, including all pre-existing cases.

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/manifest_index.clj test/abc/tools/manifest_index_test.clj
git commit -m "feat(abc): manifest-index copied-field and policy-hash validation for annotation artifacts"
```

---

### Task 6: Token join with probe classifications

**Files:**
- Create: `src/abc/tools/annotation_join.clj`
- Test: `test/abc/tools/annotation_join_test.clj`

**Interfaces:**
- Consumes: token records `{"token_index" int "input_span" {"start" int "end" int} "text" str}` (token-output schema), annotation records (Task 1 schema).
- Produces: `abc.tools.annotation-join/join` — `(join tokens annotations)` → vector of `{"annotation" ann "token_indexes" [int …] "classification" c}` with `c` ∈ `"aligned-single" | "aligned-multi" | "stem-prefix" | "conflict"`. Pure function; a generated view, never an artifact (design: ADR 0028 decision 4).

- [ ] **Step 1: Write the failing test** (real cases from the probe: 吾輩 aligned-single; 手綱 aligned-multi; stem ruby 行《い》く; conflict from a start-straddle)

```clojure
(ns abc.tools.annotation-join-test
  (:require [abc.tools.annotation-join :as join]
            [clojure.test :refer [deftest is testing]]))

(defn- tok [i s e t] {"token_index" i "input_span" {"start" s "end" e} "text" t})
(defn- ruby-ann [s e base reading]
  {"annotation_kind" "ruby" "span" {"start" s "end" e}
   "ruby" {"base" base "reading" reading "direction" nil}})

(deftest join-classifications-test
  (let [;; text: 吾輩が手綱で行く道
        tokens [(tok 0 0 2 "吾輩") (tok 1 2 3 "が")
                (tok 2 3 4 "手") (tok 3 4 5 "綱")
                (tok 4 5 6 "で") (tok 5 6 8 "行く") (tok 6 8 9 "道")]
        annotations [(ruby-ann 0 2 "吾輩" "わがはい")   ; = token 0 exactly
                     (ruby-ann 3 5 "手綱" "たづな")     ; = tokens 2+3
                     (ruby-ann 6 7 "行" "い")           ; stem: token 5 crosses end
                     (ruby-ann 7 9 "く道" "くみち")]    ; crosses token 5 start: conflict
        result (join/join tokens annotations)
        by-base (fn [b] (first (filter #(= b (get-in % ["annotation" "ruby" "base"])) result)))]
    (is (= "aligned-single" (get (by-base "吾輩") "classification")))
    (is (= [0] (get (by-base "吾輩") "token_indexes")))
    (is (= "aligned-multi" (get (by-base "手綱") "classification")))
    (is (= [2 3] (get (by-base "手綱") "token_indexes")))
    (is (= "stem-prefix" (get (by-base "行") "classification")))
    (is (= [5] (get (by-base "行") "token_indexes")))
    (is (= "conflict" (get (by-base "く道") "classification")))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `clojure -M:test --focus abc.tools.annotation-join-test`
Expected: FAIL — namespace missing.

- [ ] **Step 3: Implement the join**

```clojure
(ns abc.tools.annotation-join)

(defn- overlapping [tokens s e]
  (filterv (fn [t]
             (let [{:strs [start end]} (get t "input_span")]
               (and (< start e) (< s end))))
           tokens))

(defn- classify [cover s e]
  (let [first-span (get (first cover) "input_span")
        last-span (get (last cover) "input_span")
        start-aligned? (= s (get first-span "start"))
        end-aligned? (= e (get last-span "end"))]
    (cond
      (and start-aligned? end-aligned? (= 1 (count cover))) "aligned-single"
      (and start-aligned? end-aligned?) "aligned-multi"
      ;; end-only straddle = stem ruby: base covers the token's leading kanji,
      ;; the token continues into okurigana (probe: 92.6% of straddles).
      (and start-aligned? (not end-aligned?)) "stem-prefix"
      :else "conflict")))

(defn join
  "Join annotations to tokens by span intersection in the shared plaintext
  unicode-scalar coordinate system. Returns a generated view; never a
  canonical artifact (ADR 0028)."
  [tokens annotations]
  (mapv (fn [ann]
          (let [{:strs [start end]} (get ann "span")
                cover (overlapping tokens start end)]
            {"annotation" ann
             "token_indexes" (mapv #(get % "token_index") cover)
             "classification" (if (empty? cover)
                                "conflict"
                                (classify cover start end))}))
        annotations))
```

- [ ] **Step 4: Run test to verify it passes**

Run: `clojure -M:test --focus abc.tools.annotation-join-test`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/annotation_join.clj test/abc/tools/annotation_join_test.clj
git commit -m "feat(abc): span-intersection annotation-token join with probe classifications"
```

---

### Task 7: Design-bundle wiring + example fixture

**Files:**
- Modify: `src/abc/tools/validate_design_bundle.clj` (or wherever `abc.tools.validate-design-bundle` materializes the tokenized fixture — `grep -rn "materialize-tokenized" src/` and mirror the call)
- Create: committed example outputs under the same directory scheme the tokenized fixture uses (`grep -rn "token-stream.json" src/ test/ nix/` to find it)
- Modify: `test/abc/tools/request_set_fixture_test.clj` — add an input-view fixture entry `{"input_view_kind" "parser-ir-body-annotations-v1", "policy_hash" <ruby-gaiji-v1 hash>}` per design D6

**Interfaces:**
- Consumes: Tasks 1–6.
- Produces: `nix run .#validate-design-bundle` materializes and validates the annotation fixture alongside the tokenized fixture; request-set fixtures demonstrate the annotation input view.

- [ ] **Step 1: Mirror the tokenized fixture materialization** — add a `materialize-annotations!` call in the same place `validate-design-bundle` calls `abc.tools.materialize-tokenized`, using the same producer parser-IR example, the ruby-gaiji-v1 policy, and the same plaintext policy hash the tokenized call passes.

- [ ] **Step 2: Run the bundle validation**

Run: `nix run .#validate-design-bundle`
Expected: PASS with the annotation fixture materialized and schema-validated. Failures name the missing wiring — fix and re-run.

- [ ] **Step 3: Add the request-set input-view fixture and run its test**

Run: `clojure -M:test --focus abc.tools.request-set-fixture-test`
Expected: PASS.

- [ ] **Step 4: Commit**

```bash
git add -A
git commit -m "feat(abc): wire annotation fixture into design-bundle validation and request-set fixtures"
```

---

### Task 8: Update ADR 0028 status and settle the open questions in place

**Files:**
- Modify: `docs/adr/0028-ruby-annotation-view.md`

- [ ] **Step 1: Edit the ADR** — set the Implementation Status to name every namespace, schema, policy, and test added by Tasks 1–7; move D1–D6 from "Open questions" into the Decision section citing `docs/superpowers/specs/2026-07-09-ruby-annotation-view-design.md`; leave D7 (span_preservation profile field) and D9 (exploratory provenance floor) listed as deferred with their spec pointers.

- [ ] **Step 2: Run the full suite one last time**

Run: `clojure -M:test && nix run .#validate-design-bundle`
Expected: PASS + PASS.

- [ ] **Step 3: Commit and merge**

```bash
git add docs/adr/0028-ruby-annotation-view.md
git commit -m "docs(adr): ADR 0028 first slice implemented; settle D1-D6 per design spec"
# merge the worktree branch to main once green (repo convention)
```
