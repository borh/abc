# Per-Request-Set Annotation Materialization + Join-Stats Tooling Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Wire annotation-view materialization into the request-set batch loop (`soranoha reproduce`), with release guardrails in the batch path, and build the fixture-tested `annotation-join-stats` tooling for the corpus run.

**Architecture:** Per the design spec `docs/superpowers/specs/2026-07-10-per-request-set-annotation-materialization-design.md` (read it if a decision here seems surprising — decisions A1-A8, B1-B6 are settled there). Annotation views resolve to policies ONCE per run (registry scan by content hash, fail closed), then thread through `materialize-entry!` as plain values; guardrail validation is a distinct step after the loop; the join-stats tool consumes parser-IR dirs + token JSONL files and never invokes tokenizers.

**Tech Stack:** Clojure (abc/), kaocha via `bin/kaocha`, JSON via `abc.tools.files`.

## Global Constraints

- All commands run from `abc/` inside this worktree (`/home/bor/Projects/soranoha/.worktrees/feat-annotation-request-set/abc`) unless stated otherwise.
- Test runner is exactly `bin/kaocha` (focused: `bin/kaocha --focus <test-ns>`). NEVER `clojure -M:test` (opens a REPL, hangs).
- Design-bundle gate: `clojure -M:abc/validate-design-bundle` from abc/ (NOT `nix run .#validate-design-bundle` — known pre-existing git-cliff failure).
- The ONLY schema file that may change is `schemas/snapshot-index.schema.json` (Task 2). Do not touch manifest/request-set/annotation-output/analysis-recipe schemas.
- Do not modify any existing file under `data/request-sets/`, `data/request-set-definitions/`, `data/analysis-recipes/`, `data/annotation-policies/`, or `examples/` — new files are allowed where a task says so; existing fixtures must remain byte-identical.
- Exact error messages (tests match on them): `"Unknown annotation policy hash"`, `"Duplicate annotation policy hash in registry"`, `"Annotation view requires exactly one aligned input view"`, `"Multiple annotation input views are not supported"`.
- The join-stats tool's token-file contract: `<tokens-dir>/<work-id>.tokens.jsonl`, one JSON object per token per line, in text order, required key `"surface"`; extra keys ignored.

---

### Task 1: Annotation-view resolution helpers (pure, fail closed)

**Files:**
- Modify: `abc/src/abc/tools/materialize_annotations.clj` (append after `materialize-annotations!`)
- Test: `abc/test/abc/tools/materialize_annotations_test.clj` (append)

**Interfaces:**
- Consumes: `abc.tools.analysis-identity/annotation-policy-hash`, `abc.tools.files/read-json`.
- Produces (Task 3 consumes exactly this): `abc.tools.materialize-annotations/resolve-annotation-materialization` — takes `{:request-set <resolved request-set map>, :registry-dir <string>}`, returns `nil` (no annotation view) or `{:view <input-view map>, :policy <policy value>, :input-plaintext-policy-hash <string>}`, throws on: unknown policy hash, duplicate registry match, ≠1 aligned view, >1 annotation view. Also `annotation-input-view-kind` (def, `"parser-ir-body-annotations-v1"`).

- [ ] **Step 1: Write the failing tests** (append to `abc/test/abc/tools/materialize_annotations_test.clj`; the ns already requires `abc.tools.materialize-annotations :as annotations` and `abc.tools.files :as files` and `clojure.test` — verify the alias at the top of the file and use the file's actual alias):

```clojure
;; --- resolve-annotation-materialization (spec A1-A3: per-run resolution,
;; content-hash registry lookup, aligned-view requirement, all fail closed) ---

(def ^:private ruby-gaiji-policy-hash
  "sha256:6b2b9f29b742d434a2a114ace68549c3ad2611454785cb7a6924f5eaf95babde")

(def ^:private plaintext-policy-hash
  "sha256:df21c590fd8d5b934fd426e632a3d8a09c2bd3fa299ca6b4c8fe4c797e1d1391")

(defn- request-set-with-views [views]
  {"label" "unit-annotation"
   "request_set_identity_object" {"input_views" views}})

(def ^:private plaintext-view
  {"input_view_kind" "parser-ir-plaintext-body-v1"
   "policy_hash" plaintext-policy-hash
   "input_normalization_policy_hash"
   "sha256:530c59689dd909c171790036cddc7916f8685897b6342bfa794d4611816d3813"})

(def ^:private annotation-view
  {"input_view_kind" "parser-ir-body-annotations-v1"
   "policy_hash" ruby-gaiji-policy-hash})

(deftest resolve-annotation-materialization-resolves-demo-shape-test
  (let [resolved (annotations/resolve-annotation-materialization
                  {:request-set (request-set-with-views
                                 [annotation-view plaintext-view])
                   :registry-dir "data/annotation-policies"})]
    (is (= annotation-view (:view resolved)))
    (is (= "ruby-gaiji-v1" (get-in resolved [:policy "policy_id"])))
    (is (= plaintext-policy-hash (:input-plaintext-policy-hash resolved)))))

(deftest resolve-annotation-materialization-nil-without-annotation-view-test
  (is (nil? (annotations/resolve-annotation-materialization
             {:request-set (request-set-with-views [plaintext-view])
              :registry-dir "data/annotation-policies"}))))

(deftest resolve-annotation-materialization-rejects-unknown-policy-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Unknown annotation policy hash"
       (annotations/resolve-annotation-materialization
        {:request-set (request-set-with-views
                       [(assoc annotation-view "policy_hash"
                               "sha256:ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")
                        plaintext-view])
         :registry-dir "data/annotation-policies"}))))

(deftest resolve-annotation-materialization-rejects-duplicate-registry-test
  (let [dir (java.nio.file.Files/createTempDirectory
             "abc-annotation-registry" (make-array java.nio.file.attribute.FileAttribute 0))
        dir-file (.toFile dir)]
    (try
      (doseq [name ["a.json" "b.json"]]
        (clojure.java.io/copy (clojure.java.io/file "data/annotation-policies/ruby-gaiji-v1.json")
                              (clojure.java.io/file dir-file name)))
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Duplicate annotation policy hash in registry"
           (annotations/resolve-annotation-materialization
            {:request-set (request-set-with-views [annotation-view plaintext-view])
             :registry-dir (str dir-file)})))
      (finally
        (doseq [f (reverse (file-seq dir-file))] (.delete f))))))

(deftest resolve-annotation-materialization-rejects-missing-aligned-view-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Annotation view requires exactly one aligned input view"
       (annotations/resolve-annotation-materialization
        {:request-set (request-set-with-views [annotation-view])
         :registry-dir "data/annotation-policies"}))))

(deftest resolve-annotation-materialization-rejects-ambiguous-aligned-view-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Annotation view requires exactly one aligned input view"
       (annotations/resolve-annotation-materialization
        {:request-set (request-set-with-views
                       [annotation-view
                        plaintext-view
                        (assoc plaintext-view "policy_hash"
                               "sha256:eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee")])
         :registry-dir "data/annotation-policies"}))))

(deftest resolve-annotation-materialization-rejects-multiple-annotation-views-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Multiple annotation input views are not supported"
       (annotations/resolve-annotation-materialization
        {:request-set (request-set-with-views
                       [annotation-view
                        (assoc annotation-view "policy_hash"
                               "sha256:dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd")
                        plaintext-view])
         :registry-dir "data/annotation-policies"}))))
```

If the test ns does not already require `clojure.java.io`, add it to the ns form.

- [ ] **Step 2: Run tests to verify they fail**

Run: `bin/kaocha --focus abc.tools.materialize-annotations-test`
Expected: FAIL — `resolve-annotation-materialization` does not exist.

- [ ] **Step 3: Implement** (append to `abc/src/abc/tools/materialize_annotations.clj`; the ns already requires `analysis-identity`, `files`, `clojure.java.io :as io`):

```clojure
(def annotation-input-view-kind "parser-ir-body-annotations-v1")

(defn- registry-policies [registry-dir]
  (->> (.listFiles (io/file registry-dir))
       (filter #(and (.isFile %) (string/ends-with? (.getName %) ".json")))
       (sort-by #(.getName %))
       (mapv (fn [f] {:path (str f)
                      :policy (files/read-json f)}))))

(defn- resolve-policy-by-hash [registry-dir policy-hash]
  (let [matches (filterv #(= policy-hash
                             (analysis-identity/annotation-policy-hash
                              (:policy %)))
                         (registry-policies registry-dir))]
    (case (count matches)
      0 (throw (ex-info "Unknown annotation policy hash"
                        {:policy_hash policy-hash
                         :registry_dir registry-dir}))
      1 (:policy (first matches))
      (throw (ex-info "Duplicate annotation policy hash in registry"
                      {:policy_hash policy-hash
                       :paths (mapv :path matches)})))))

(defn- aligned-plaintext-policy-hash [input-views policy]
  (let [aligns-to (get policy "aligns_to")
        aligned (filterv #(= aligns-to (get % "input_view_kind")) input-views)]
    (when-not (= 1 (count aligned))
      (throw (ex-info "Annotation view requires exactly one aligned input view"
                      {:aligns_to aligns-to
                       :aligned_view_count (count aligned)})))
    (get (first aligned) "policy_hash")))

(defn resolve-annotation-materialization
  "Resolves a request set's annotation input view to the concrete
  materialization inputs, ONCE per run (spec A1-A3). Returns nil when the
  request set declares no annotation view; otherwise
  {:view <input-view> :policy <registry value>
   :input-plaintext-policy-hash <the aligned view's policy_hash>}.
  Fails closed on: unknown/duplicate policy hash in the registry, more than
  one annotation view (v1 layout is single-view), and anything but exactly
  one input view of the policy's aligns_to kind."
  [{:keys [request-set registry-dir]}]
  (let [input-views (get-in request-set
                            ["request_set_identity_object" "input_views"])
        annotation-views (filterv #(= annotation-input-view-kind
                                      (get % "input_view_kind"))
                                  input-views)]
    (when (seq annotation-views)
      (when (> (count annotation-views) 1)
        (throw (ex-info "Multiple annotation input views are not supported"
                        {:label (get request-set "label")
                         :annotation_view_count (count annotation-views)})))
      (let [view (first annotation-views)
            policy (resolve-policy-by-hash registry-dir
                                           (get view "policy_hash"))]
        {:view view
         :policy policy
         :input-plaintext-policy-hash (aligned-plaintext-policy-hash
                                       input-views policy)}))))
```

Add `[clojure.string]` to the ns `:require` if not present (as `clojure.string`, matching existing style in the file — check first; if the file has no string require, use `(:require ... [clojure.string :as string])` and call `string/ends-with?`).

- [ ] **Step 4: Run tests to verify they pass**

Run: `bin/kaocha --focus abc.tools.materialize-annotations-test`
Expected: PASS (new + pre-existing).

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/materialize_annotations.clj test/abc/tools/materialize_annotations_test.clj
git commit -m "feat(abc): per-run annotation-view resolution with fail-closed registry lookup"
```

---

### Task 2: snapshot-index schema widening + schema-contracts drill

**Files:**
- Modify: `abc/schemas/snapshot-index.schema.json` (version + two enums)
- Regenerate: `abc/schemas/schema-contracts.json` (via generator, never by hand)
- Modify (byte mirror): `ab-validator/data/abc-schemas/nix-schemas/snapshot-index.schema.json`
- Regenerate: `ab-validator/data/abc-schemas/schema-contracts.json` (via its generator)

**Interfaces:**
- Produces: snapshot-index schema v0.1.1 admitting `"annotation"` in `layoutPolicy.loose_artifact_kinds` items and `artifactReference.artifact_kind`. Tasks 3-4 depend on this.

- [ ] **Step 1: Edit the schema.** In `abc/schemas/snapshot-index.schema.json`:
  - Line 5: `"version": "0.1.0",` → `"version": "0.1.1",`
  - In `$defs.layoutPolicy.properties.loose_artifact_kinds.items`: `{ "enum": ["tei", "plaintext"] }` → `{ "enum": ["tei", "plaintext", "annotation"] }`
  - In `$defs.artifactReference.properties.artifact_kind.enum`: add `"annotation"` between `"analysis"` and `"failure"`, i.e. `["source", "parser-ir", "warnings", "plaintext", "tei", "rdf-view", "tokenized", "analysis", "annotation", "failure"]`

- [ ] **Step 2: Regenerate abc contracts and verify** (from `abc/`):

```bash
python tools/schema_contracts.py --write
python tools/schema_contracts.py   # exit 0 required
```

- [ ] **Step 3: Mirror + regenerate ab-validator contracts** (from the worktree root):

```bash
cp abc/schemas/snapshot-index.schema.json ab-validator/data/abc-schemas/nix-schemas/snapshot-index.schema.json
cd ab-validator && python scripts/schema_contracts.py --write && python scripts/schema_contracts.py && cd ..
```

(The `cp` into `nix-schemas/` is correct — that directory is a deliberate byte mirror. NEVER `cp` a schema-contracts.json between projects.)

- [ ] **Step 4: Run the drift gates for real** (from the worktree root, then abc/):

```bash
bash scripts/monorepo-schema-drift.sh
cd abc && nix build .#checks.x86_64-linux.schema-contract-drift --no-link
```

Expected: drift script exit 0; nix build succeeds (do NOT substitute `nix flake check --no-build` — it never builds this check).

- [ ] **Step 5: Run affected tests**

Run (from `abc/`): `bin/kaocha --focus abc.tools.snapshot-index-test --focus abc.tools.schema-test`
Expected: PASS (the committed `examples/v0/snapshot/snapshot-index.json` is gated on internal consistency only; it must NOT be edited).

- [ ] **Step 6: Commit** (from the worktree root):

```bash
git add abc/schemas/snapshot-index.schema.json abc/schemas/schema-contracts.json ab-validator/data/abc-schemas/nix-schemas/snapshot-index.schema.json ab-validator/data/abc-schemas/schema-contracts.json
git commit -m "feat(schemas): snapshot-index v0.1.1 admits annotation artifact kind"
```

---

### Task 3: Batch wiring + demo snapshot plan + end-to-end test

**Files:**
- Modify: `abc/src/abc/tools/soranoha.clj` (ns require; `generated-snapshot-layout-policy`; `materialize-entry!`; `materialize-snapshot-root!`; new `write-annotation-artifacts!`)
- Create: `abc/data/snapshot-plans/demo-annotation-ja.json`
- Test: `abc/test/abc/tools/soranoha_annotation_test.clj` (new)

**Interfaces:**
- Consumes: Task 1's `resolve-annotation-materialization`; Task 2's schema.
- Produces: `materialize-snapshot-root!` (unchanged public signature `[label-or-path root]`) now writes `<root>/artifacts/works/<slug>/annotations/{body-annotations.json,annotation.manifest.json}` for request sets with an annotation view, and includes those manifests in the snapshot-index references. Task 4 consumes the produced manifest set.

- [ ] **Step 1: Create the snapshot plan.** Write `abc/data/snapshot-plans/demo-annotation-ja.json` as an exact copy of `abc/data/snapshot-plans/demo-basic-ja.json` with exactly three differences:
  - `"request_set_label": "demo-annotation-ja"`
  - `"snapshot_label": "soranoha-snapshot-2026-07-07-03"`
  - `"layout_policy"."loose_artifact_kinds": ["tei", "plaintext", "annotation"]`

  Everything else (both materializations, `generated_at`, policies, hashes) byte-identical to the demo-basic-ja plan. The two materializations' `work_content_hash` values (`2323…`, `2424…`) already match `demo-annotation-ja`'s subjects.

- [ ] **Step 2: Write the failing end-to-end test.** Create `abc/test/abc/tools/soranoha_annotation_test.clj`:

```clojure
(ns abc.tools.soranoha-annotation-test
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.schema :as schema]
            [abc.tools.snapshot-index :as snapshot-index]
            [abc.tools.soranoha :as soranoha]
            [abc.tools.source-snapshot-fixture :as fixture]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(def ^:private ruby-gaiji-policy-hash
  "sha256:6b2b9f29b742d434a2a114ace68549c3ad2611454785cb7a6924f5eaf95babde")

(deftest reproduce-demo-annotation-ja-materializes-annotation-artifacts-test
  (let [root (fixture/temp-dir "abc-demo-annotation-root")]
    (try
      (let [{:keys [snapshot]} (soranoha/materialize-snapshot-root!
                                "demo-annotation-ja" root)
            manifest-schema (schema/read-schema "schemas/manifest.schema.json")
            output-schema (schema/read-schema "schemas/annotation-output.schema.json")]
        (doseq [slug ["demo-fixture-a" "demo-fixture-b"]]
          (testing slug
            (let [base (io/file root "artifacts" "works" slug "annotations")
                  manifest (files/read-json (io/file base "annotation.manifest.json"))
                  value (files/read-json (io/file base "body-annotations.json"))
                  producer (files/read-json
                            (io/file root "artifacts" "works" slug
                                     "parser-ir" "parser-ir.manifest.json"))]
              (is (nil? (schema/validation-errors manifest-schema manifest)))
              (is (nil? (schema/validation-errors output-schema value)))
              (is (= "annotation" (get manifest "artifact_kind")))
              (is (= ruby-gaiji-policy-hash
                     (get-in manifest ["manifest_identity_object"
                                       "annotation_policy_hash"])))
              (is (= ruby-gaiji-policy-hash
                     (get value "annotation_policy_hash")))
              (testing "input_plaintext_policy_hash is the plaintext view's policy_hash"
                (let [request-set (files/read-json
                                   "data/request-sets/demo-annotation-ja.json")
                      plaintext-view (first
                                      (filter #(= "parser-ir-plaintext-body-v1"
                                                  (get % "input_view_kind"))
                                              (get-in request-set
                                                      ["request_set_identity_object"
                                                       "input_views"])))]
                  (is (= (get plaintext-view "policy_hash")
                         (get value "input_plaintext_policy_hash")))))
              (testing "copied parser-IR identity fields match the producer"
                (doseq [field ["parser_build_hash" "parser_config_hash"
                               "aat_parser_ir_mapping_hash" "parser_ir_schema_hash"]]
                  (is (= (get-in producer ["manifest_identity_object" field])
                         (get-in manifest ["manifest_identity_object" field]))
                      field))))))
        (testing "snapshot index references the annotation manifests"
          (let [kinds (frequencies
                       (map #(get % "artifact_kind")
                            (get snapshot "artifact_references")))]
            (is (= 2 (get kinds "annotation")))))
        (testing "snapshot index remains internally consistent"
          (is (true? (snapshot-index/validate-snapshot-index! snapshot)))))
      (finally
        (fixture/delete-tree! root)))))

(deftest reproduce-demo-basic-ja-produces-no-annotation-artifacts-test
  (let [root (fixture/temp-dir "abc-demo-basic-root")]
    (try
      (soranoha/materialize-snapshot-root! "demo-basic-ja" root)
      (doseq [slug ["demo-fixture-a" "demo-fixture-b"]]
        (is (not (.exists (io/file root "artifacts" "works" slug "annotations")))
            slug))
      (finally
        (fixture/delete-tree! root)))))
```

Note: `materialize-snapshot-root!` is currently `defn-` — making it public (plain `defn`) is part of this task (it is the tested seam; `reproduce!` stays the CLI wrapper). If `snapshot-index/validate-snapshot-index!` has a different name, check `snapshot_index.clj` and use the actual public validation fn (it is used by `checked-in-snapshot-index-fixture-validates-identity-test` in `test/abc/tools/snapshot_index_test.clj:200`).

- [ ] **Step 3: Run tests to verify they fail**

Run: `bin/kaocha --focus abc.tools.soranoha-annotation-test`
Expected: first test FAILS (no `annotations/` dir is produced yet — likely a missing-file read error); the demo-basic-ja negative test may already pass.

- [ ] **Step 4: Implement the wiring.** In `abc/src/abc/tools/soranoha.clj`:

1. Add to the ns `:require`: `[abc.tools.materialize-annotations :as materialize-annotations]`.
2. Find `generated-snapshot-layout-policy` (a def near the other `generated-snapshot-*` defs) and add `"annotation"` to its `"loose_artifact_kinds"` vector.
3. Make `materialize-snapshot-root!` public (`defn-` → `defn`).
4. Add the artifact writer (near `write-analysis-artifacts!`):

```clojure
(def ^:private annotation-policies-dir "data/annotation-policies")

(defn- write-annotation-artifacts!
  [{:keys [artifact-base annotation-materialization producer-manifest-file
           parser-ir-file generated-at]}]
  (let [annotations-dir (io/file artifact-base "annotations")
        result (materialize-annotations/materialize-annotations!
                {:producer-manifest (files/read-json producer-manifest-file)
                 :parser-ir (files/read-json parser-ir-file)
                 :annotation-policy (:policy annotation-materialization)
                 :input-plaintext-policy-hash (:input-plaintext-policy-hash
                                               annotation-materialization)
                 :output-dir annotations-dir
                 :generated-at generated-at})]
    {:annotations-file (:annotations result)
     :annotation-manifest-file (:manifest result)}))
```

5. In `materialize-entry!`, add `annotation-materialization` to the destructured keys, and after the `analysis-result` binding add:

```clojure
        annotation-result (when annotation-materialization
                            (write-annotation-artifacts!
                             {:artifact-base artifact-base
                              :annotation-materialization annotation-materialization
                              :producer-manifest-file (:manifest-file
                                                       parser-result)
                              :parser-ir-file (:parser-ir-file parser-result)
                              :generated-at generated-at}))
```

and extend the returned vector's `cond->` with:

```clojure
      annotation-result
      (conj (:annotation-manifest-file annotation-result))
```

6. In `materialize-snapshot-root!`, after `materializations` is bound, resolve once and pass down:

```clojure
        annotation-materialization (materialize-annotations/resolve-annotation-materialization
                                    {:request-set request-set
                                     :registry-dir annotation-policies-dir})
```

and add `:annotation-materialization annotation-materialization` to the map passed to `materialize-entry!`.

- [ ] **Step 5: Run the focused tests, then the full suite**

Run: `bin/kaocha --focus abc.tools.soranoha-annotation-test --focus abc.tools.soranoha-test`
Expected: PASS.

Run: `bin/kaocha`
Expected: PASS, 0 failures (existing `soranoha_test.clj` snapshot tests prove non-annotation request sets are unchanged).

- [ ] **Step 6: Commit**

```bash
git add src/abc/tools/soranoha.clj data/snapshot-plans/demo-annotation-ja.json test/abc/tools/soranoha_annotation_test.clj
git commit -m "feat(abc): materialize annotation artifacts in the request-set batch loop"
```

---

### Task 4: Annotation release guardrails in the batch path

**Files:**
- Modify: `abc/src/abc/tools/soranoha.clj` (new `validate-annotation-manifests!`; one call in `materialize-snapshot-root!`)
- Test: `abc/test/abc/tools/soranoha_annotation_test.clj` (append)

**Interfaces:**
- Consumes: `abc.tools.manifest-index/index-manifest-files`, `validate-annotation-release-guardrail!`, `validate-annotation-copied-fields!` (all existing, exercised today only by the design-bundle gate).
- Produces: `abc.tools.soranoha/validate-annotation-manifests!` — public, takes a seq of manifest file paths, returns nil or throws.

- [ ] **Step 1: Write the failing tests** (append to `soranoha_annotation_test.clj`; add `[abc.tools.manifest :as manifest]` to the ns require if needed):

```clojure
(deftest validate-annotation-manifests-passes-on-materialized-root-test
  (let [root (fixture/temp-dir "abc-annotation-guardrail-root")]
    (try
      (soranoha/materialize-snapshot-root! "demo-annotation-ja" root)
      (let [manifests (->> (file-seq (io/file root "artifacts"))
                           (filter #(and (.isFile %)
                                         (.endsWith (.getName %) ".manifest.json")))
                           (mapv str))]
        (is (nil? (soranoha/validate-annotation-manifests! manifests))))
      (finally
        (fixture/delete-tree! root)))))

(deftest validate-annotation-manifests-rejects-stripped-policy-hash-test
  (let [root (fixture/temp-dir "abc-annotation-guardrail-bad-root")]
    (try
      (soranoha/materialize-snapshot-root! "demo-annotation-ja" root)
      (let [manifest-file (io/file root "artifacts" "works" "demo-fixture-a"
                                   "annotations" "annotation.manifest.json")
            manifest-value (files/read-json manifest-file)]
        (manifest/write-json-file!
         manifest-file
         (update manifest-value "manifest_identity_object"
                 dissoc "annotation_policy_hash"))
        (let [manifests (->> (file-seq (io/file root "artifacts"))
                             (filter #(and (.isFile %)
                                           (.endsWith (.getName %) ".manifest.json")))
                             (mapv str))]
          (is (thrown? clojure.lang.ExceptionInfo
                       (soranoha/validate-annotation-manifests! manifests)))))
      (finally
        (fixture/delete-tree! root)))))
```

- [ ] **Step 2: Run to verify failure**

Run: `bin/kaocha --focus abc.tools.soranoha-annotation-test`
Expected: FAIL — `validate-annotation-manifests!` does not exist.

- [ ] **Step 3: Implement.** In `soranoha.clj`: add `[abc.tools.manifest-index :as manifest-index]` to the ns require, then near `materialize-snapshot-root!`:

```clojure
(defn validate-annotation-manifests!
  "Release guardrails for annotation manifests in the batch path (spec A6):
  indexes the produced manifests and applies the annotation release
  guardrail + copied-field validators. A distinct step (not braided into
  materialize-entry!) so it stays cheap to widen to other kinds later."
  [manifest-files]
  (let [entries (manifest-index/index-manifest-files manifest-files)]
    (manifest-index/validate-annotation-release-guardrail! entries)
    (manifest-index/validate-annotation-copied-fields! entries)
    nil))
```

In `materialize-snapshot-root!`, after `manifest-files` is realized (it is used to build `generated-plan`), add a call before the snapshot is built:

```clojure
          _guardrails (validate-annotation-manifests! (map str manifest-files))
```

(as a `let` binding after `manifest-files`; note `manifest-files` is the result of `mapcat` — realize it with `vec` first if laziness is a concern: bind `manifest-files (vec (mapcat ...))`.)

- [ ] **Step 4: Run focused + full suite**

Run: `bin/kaocha --focus abc.tools.soranoha-annotation-test`, then `bin/kaocha`
Expected: PASS both.

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/soranoha.clj test/abc/tools/soranoha_annotation_test.clj
git commit -m "feat(abc): run annotation release guardrails in the batch path"
```

---

### Task 5: Join-stats core (span reconstruction + statistics, pure)

**Files:**
- Create: `abc/src/abc/tools/annotation_join_stats.clj`
- Test: `abc/test/abc/tools/annotation_join_stats_test.clj` (new)

**Interfaces:**
- Consumes: `abc.tools.annotation-join/join`, `abc.tools.parser-ir-plaintext/render-with-annotations`.
- Produces (Task 6 consumes): `reconstruct-token-spans [text surfaces]` → `{:tokens [...]}` or `{:failure {...}}`; `work-stats [{:keys [annotations tokens]}]` → stats map; `aggregate [work-stats-seq]` → aggregate map.

- [ ] **Step 1: Write the failing tests.** Create `abc/test/abc/tools/annotation_join_stats_test.clj`:

```clojure
(ns abc.tools.annotation-join-stats-test
  (:require [abc.tools.annotation-join-stats :as stats]
            [abc.tools.files :as files]
            [abc.tools.parser-ir-plaintext :as plaintext]
            [clojure.test :refer [deftest is testing]]))

(deftest reconstruct-token-spans-walks-scalars-and-skips-whitespace-test
  (let [{:keys [tokens failure]} (stats/reconstruct-token-spans
                                  "吾輩は\n猫である"
                                  ["吾輩" "は" "猫" "で" "ある"])]
    (is (nil? failure))
    (is (= [{"token_index" 0 "input_span" {"start" 0 "end" 2} "text" "吾輩"}
            {"token_index" 1 "input_span" {"start" 2 "end" 3} "text" "は"}
            {"token_index" 2 "input_span" {"start" 4 "end" 5} "text" "猫"}
            {"token_index" 3 "input_span" {"start" 5 "end" 6} "text" "で"}
            {"token_index" 4 "input_span" {"start" 6 "end" 8} "text" "ある"}]
           tokens))))

(deftest reconstruct-token-spans-counts-astral-scalars-once-test
  (let [{:keys [tokens failure]} (stats/reconstruct-token-spans
                                  "𠮟る" ["𠮟る"])]
    (is (nil? failure))
    (is (= {"start" 0 "end" 2} (get-in tokens [0 "input_span"])))))

(deftest reconstruct-token-spans-records-failure-test
  (let [{:keys [tokens failure]} (stats/reconstruct-token-spans
                                  "吾輩は猫" ["吾輩" "犬"])]
    (is (nil? tokens))
    (is (= 1 (:token-index failure)))
    (is (= 2 (:offset failure)))))

(deftest work-stats-classifies-fixture-annotations-test
  (let [parser-ir (files/read-json "examples/ab-validator-output/parser-ir.json")
        {:keys [text annotations]} (plaintext/render-with-annotations parser-ir)
        ;; one token exactly covering the ruby base = aligned-single; the
        ;; rest of the text as whatever tokens the walk yields
        ruby-span (get-in (first (filter #(= "ruby" (get % "annotation_kind"))
                                         annotations))
                          ["span"])
        result (stats/work-stats
                {:annotations annotations
                 :tokens [{"token_index" 0
                           "input_span" ruby-span
                           "text" "x"}]})]
    (is (pos? (get-in result [:annotation_counts "ruby"])))
    (is (= 1 (get-in result [:classifications "ruby" "aligned-single"])))
    (testing "every annotation is classified"
      (is (= (reduce + (vals (:annotation_counts result)))
             (reduce + (mapcat vals (vals (:classifications result)))))))))

(deftest aggregate-sums-and-rates-test
  (let [agg (stats/aggregate
             [{:work-id "a"
               :annotation_counts {"ruby" 2 "gaiji" 1}
               :classifications {"ruby" {"aligned-single" 1 "stem-prefix" 1}
                                 "gaiji" {"conflict" 1}}}
              {:work-id "b"
               :annotation_counts {"ruby" 1}
               :classifications {"ruby" {"aligned-multi" 1}}}])]
    (is (= 2 (:work_count agg)))
    (is (= {"ruby" 3 "gaiji" 1} (:annotation_counts agg)))
    (is (= 1 (get-in agg [:classifications "ruby" "aligned-single"])))
    (is (= 1 (get-in agg [:classifications "ruby" "aligned-multi"])))
    (is (= 0.5 (get-in agg [:classification_rates "ruby" "stem-prefix"])))))
```

- [ ] **Step 2: Run to verify failure**

Run: `bin/kaocha --focus abc.tools.annotation-join-stats-test`
Expected: FAIL — namespace does not exist.

- [ ] **Step 3: Implement.** Create `abc/src/abc/tools/annotation_join_stats.clj`:

```clojure
(ns abc.tools.annotation-join-stats
  "Join-statistics core for the corpus-scale ruby/gaiji annotation run
  (design spec 2026-07-10, slice B). Pure: token-span reconstruction from a
  surface sequence, per-work classification statistics via
  abc.tools.annotation-join, and aggregation. Reconstructed spans are sorted
  and non-overlapping by construction (contiguous left-to-right walk), which
  is annotation-join/join's documented precondition."
  (:require [abc.tools.annotation-join :as annotation-join]
            [clojure.string :as string]))

(defn- scalar-count [^String s]
  (.codePointCount s 0 (.length s)))

(defn reconstruct-token-spans
  "Walks tokenizer surface forms over the rendered plaintext, assigning
  unicode-scalar input spans. Whitespace (incl. newlines) between tokens is
  skipped. Returns {:tokens [...]} on success or {:failure {:token-index i
  :offset scalar-offset :surface s}} on the first mismatch — failures are
  recorded, never papered over (spec B3)."
  [^String text surfaces]
  (let [scalars (vec (map #(String. (Character/toChars %))
                          (iterator-seq (.iterator (.codePoints text)))))
        total (count scalars)]
    (loop [offset 0
           token-index 0
           remaining (seq surfaces)
           tokens []]
      (if-not remaining
        {:tokens tokens}
        (let [surface (first remaining)
              width (scalar-count surface)
              slice (when (<= (+ offset width) total)
                      (apply str (subvec scalars offset (+ offset width))))]
          (cond
            (= slice surface)
            (recur (+ offset width)
                   (inc token-index)
                   (next remaining)
                   (conj tokens {"token_index" token-index
                                 "input_span" {"start" offset
                                               "end" (+ offset width)}
                                 "text" surface}))

            (and (< offset total)
                 (string/blank? (nth scalars offset)))
            (recur (inc offset) token-index remaining tokens)

            :else
            {:failure {:token-index token-index
                       :offset offset
                       :surface surface}}))))))

(defn work-stats
  "Classification statistics for one work: joins every annotation against
  the token spans and counts classifications per annotation kind."
  [{:keys [annotations tokens]}]
  (let [joined (annotation-join/join tokens annotations)
        by-kind (group-by #(get-in % ["annotation" "annotation_kind"]) joined)]
    {:annotation_counts (into {} (map (fn [[k v]] [k (count v)])) by-kind)
     :classifications (into {}
                            (map (fn [[k v]]
                                   [k (frequencies
                                       (map #(get % "classification") v))]))
                            by-kind)}))

(defn- merge-counts [maps]
  (apply merge-with + {} maps))

(defn aggregate
  "Aggregates per-work stats: summed counts plus per-kind classification
  rates (fractions of that kind's total)."
  [work-stats-seq]
  (let [annotation-counts (merge-counts (map :annotation_counts work-stats-seq))
        kinds (keys annotation-counts)
        classifications (into {}
                              (map (fn [kind]
                                     [kind (merge-counts
                                            (keep #(get-in % [:classifications kind])
                                                  work-stats-seq))]))
                              kinds)
        rates (into {}
                    (map (fn [kind]
                           (let [total (get annotation-counts kind)]
                             [kind (into {}
                                         (map (fn [[c n]]
                                                [c (double (/ n total))]))
                                         (get classifications kind))])))
                    kinds)]
    {:work_count (count work-stats-seq)
     :annotation_counts annotation-counts
     :classifications classifications
     :classification_rates rates}))
```

Add `[clojure.string]` to the require vector (`[clojure.string :as string]` and use `string/blank?` — match the codebase idiom).

- [ ] **Step 4: Run to verify pass**

Run: `bin/kaocha --focus abc.tools.annotation-join-stats-test`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/annotation_join_stats.clj test/abc/tools/annotation_join_stats_test.clj
git commit -m "feat(abc): annotation join-stats core — span reconstruction and aggregation"
```

---

### Task 6: `annotation-join-stats` CLI command

**Files:**
- Modify: `abc/src/abc/tools/annotation_join_stats.clj` (add run fns)
- Modify: `abc/src/abc/tools/soranoha.clj` (require + command registration + usage line)
- Test: `abc/test/abc/tools/annotation_join_stats_test.clj` (append)

**Interfaces:**
- Consumes: Task 5 core; `plaintext/render-with-annotations`; `files/read-json` + `files/read-json-lines` (exists in `abc/src/abc/tools/files.clj:19`).
- Produces: `soranoha annotation-join-stats <parser-ir-dir> <tokens-dir> <out-dir>` where `parser-ir-dir` contains `<work-id>/parser-ir.json` entries and `tokens-dir` contains `<work-id>.tokens.jsonl` (contract in Global Constraints). Writes `<out-dir>/per-work.jsonl`, `<out-dir>/aggregate.json`, `<out-dir>/report.md`. Works with a missing/failed token file are recorded under `:skipped`, never dropped silently.

- [ ] **Step 1: Write the failing test** (append to `annotation_join_stats_test.clj`; add `[abc.tools.manifest :as manifest]`, `[abc.tools.source-snapshot-fixture :as fixture]`, `[clojure.java.io :as io]` to the ns):

```clojure
(deftest run-join-stats-over-fixture-corpus-test
  (let [root (fixture/temp-dir "abc-join-stats")
        parser-ir-dir (io/file root "parser-ir")
        tokens-dir (io/file root "tokens")
        out-dir (io/file root "out")
        parser-ir (files/read-json "examples/ab-validator-output/parser-ir.json")
        {:keys [text annotations]} (plaintext/render-with-annotations parser-ir)]
    (try
      (.mkdirs (io/file parser-ir-dir "work-a"))
      (.mkdirs tokens-dir)
      (manifest/write-json-file! (io/file parser-ir-dir "work-a" "parser-ir.json")
                                 parser-ir)
      ;; tokens: one surface per scalar of the rendered text — guarantees a
      ;; successful walk regardless of fixture content
      (spit (io/file tokens-dir "work-a.tokens.jsonl")
            (apply str
                   (map #(str "{\"surface\":" (pr-str (str %)) "}\n")
                        (map #(String. (Character/toChars %))
                             (iterator-seq (.iterator (.codePoints ^String text)))))))
      ;; work-b has parser-IR but no token file → recorded as skipped
      (.mkdirs (io/file parser-ir-dir "work-b"))
      (manifest/write-json-file! (io/file parser-ir-dir "work-b" "parser-ir.json")
                                 parser-ir)
      (let [exit (stats/run-join-stats! (str parser-ir-dir)
                                        (str tokens-dir)
                                        (str out-dir))
            aggregate (files/read-json (io/file out-dir "aggregate.json"))
            per-work (files/read-json-lines (io/file out-dir "per-work.jsonl"))]
        (is (= 0 exit))
        (is (.exists (io/file out-dir "report.md")))
        (is (= 1 (get aggregate "work_count")))
        (is (= ["work-b"] (get aggregate "skipped_work_ids")))
        (is (= 1 (count per-work)))
        (is (= "work-a" (get (first per-work) "work_id")))
        (is (pos? (get-in aggregate ["annotation_counts" "ruby"]))))
      (finally
        (fixture/delete-tree! root)))))
```

- [ ] **Step 2: Run to verify failure**

Run: `bin/kaocha --focus abc.tools.annotation-join-stats-test`
Expected: FAIL — `run-join-stats!` does not exist.

- [ ] **Step 3: Implement.** Append to `annotation_join_stats.clj` (add requires: `[abc.tools.files :as files]`, `[abc.tools.manifest :as manifest]`, `[abc.tools.parser-ir-plaintext :as plaintext]`, `[clojure.java.io :as io]`):

```clojure
(defn- work-dirs [parser-ir-dir]
  (->> (.listFiles (io/file parser-ir-dir))
       (filter #(.isDirectory %))
       (filter #(.isFile (io/file % "parser-ir.json")))
       (sort-by #(.getName %))))

(defn- work-tokens [tokens-dir work-id]
  (let [file (io/file tokens-dir (str work-id ".tokens.jsonl"))]
    (when (.isFile file)
      (mapv #(get % "surface") (files/read-json-lines file)))))

(defn- stats-row [work-id text annotations tokens]
  (merge {"work_id" work-id
          "text_scalar_count" (scalar-count text)}
         (let [{:keys [annotation_counts classifications]}
               (work-stats {:annotations annotations :tokens tokens})]
           {"annotation_counts" annotation_counts
            "classifications" classifications})))

(defn- report-md [aggregate]
  (str "# Annotation join statistics\n\n"
       "- works: " (get aggregate "work_count")
       " (skipped: " (count (get aggregate "skipped_work_ids")) ")\n"
       "- annotation counts: " (pr-str (get aggregate "annotation_counts")) "\n\n"
       "## Classification rates\n\n"
       (apply str
              (for [[kind rates] (sort-by key (get aggregate "classification_rates"))]
                (str "- " kind ": "
                     (pr-str (into (sorted-map) rates))
                     "\n")))))

(defn run-join-stats!
  "CLI body for `soranoha annotation-join-stats` (spec B4/B5). Reads
  <parser-ir-dir>/<work-id>/parser-ir.json and
  <tokens-dir>/<work-id>.tokens.jsonl, writes per-work.jsonl,
  aggregate.json, and report.md to out-dir. Works without a token file or
  with a failed span walk are recorded in skipped_work_ids. Returns 0."
  [parser-ir-dir tokens-dir out-dir]
  (let [out (io/file out-dir)]
    (.mkdirs out)
    (loop [dirs (work-dirs parser-ir-dir)
           rows []
           work-stats-acc []
           skipped []]
      (if-let [dir (first dirs)]
        (let [work-id (.getName dir)
              surfaces (work-tokens tokens-dir work-id)]
          (if-not surfaces
            (recur (next dirs) rows work-stats-acc (conj skipped work-id))
            (let [parser-ir (files/read-json (io/file dir "parser-ir.json"))
                  {:keys [text annotations]} (plaintext/render-with-annotations
                                              parser-ir)
                  {:keys [tokens failure]} (reconstruct-token-spans text surfaces)]
              (if failure
                (recur (next dirs) rows work-stats-acc (conj skipped work-id))
                (recur (next dirs)
                       (conj rows (stats-row work-id text annotations tokens))
                       (conj work-stats-acc
                             (work-stats {:annotations annotations
                                          :tokens tokens}))
                       skipped)))))
        (let [aggregate-value
              (-> (aggregate work-stats-acc)
                  (update-keys name)
                  (assoc "skipped_work_ids" (vec (sort skipped))))]
          (spit (io/file out "per-work.jsonl")
                (apply str (map #(str (row-json-str %) "\n") rows)))
          (manifest/write-json-file! (io/file out "aggregate.json")
                                     aggregate-value)
          (spit (io/file out "report.md") (report-md aggregate-value))
          0)))))
```

with the row serializer defined above `run-join-stats!` (requires
`[abc.tools.json :as abc-json]` and `[charred.api :as charred]` — charred is
the JSON library `abc.tools.json` itself wraps, see
`abc/src/abc/tools/json.clj:2`):

```clojure
(defn- row-json-str [row]
  (charred/write-json-str (abc-json/prepare-deterministic-json row)))
```

The `aggregate` fn returns keyword keys; `update-keys name` converts for
stable JSON output.

`work-stats` is called twice in the loop for clarity of the row shape — if you prefer, compute once and reuse; keep the output identical.

- [ ] **Step 4: Register the CLI command.** In `soranoha.clj`: add `[abc.tools.annotation-join-stats :as annotation-join-stats]` to the ns require; add to the `commands` map:

```clojure
   "annotation-join-stats" {:args 3
                            :run (fn [parser-ir-dir tokens-dir out-dir]
                                   (annotation-join-stats/run-join-stats!
                                    parser-ir-dir tokens-dir out-dir))}
```

and add a usage line to `usage`:

```
    "  annotation-join-stats <parser-ir-dir> <tokens-dir> <out-dir>"
```

- [ ] **Step 5: Run focused + full suite**

Run: `bin/kaocha --focus abc.tools.annotation-join-stats-test`, then `bin/kaocha`
Expected: PASS both.

- [ ] **Step 6: Commit**

```bash
git add src/abc/tools/annotation_join_stats.clj src/abc/tools/soranoha.clj test/abc/tools/annotation_join_stats_test.clj
git commit -m "feat(abc): soranoha annotation-join-stats command"
```

---

### Task 7: ADR 0028 + spec status update

**Files:**
- Modify: `abc/docs/adr/0028-ruby-annotation-view.md` (Implementation Status D6/annotation bullets; Acceptance Criteria)
- Modify: `abc/docs/superpowers/specs/2026-07-10-per-request-set-annotation-materialization-design.md` (Status line)

**Interfaces:**
- Consumes: the fn/test names landed in Tasks 1-6.
- Produces: documentation only. Heading `## Acceptance Criteria` keeps exact case.

- [ ] **Step 1: Update the ADR.** In `abc/docs/adr/0028-ruby-annotation-view.md`, in the Implementation Status D6 bullet, replace the sentence fragment:

```
Materializing annotation
  artifacts *per request set* remains future work, as does widening
```

with:

```
Materializing annotation artifacts *per request set* landed 2026-07-10:
  `abc.tools.soranoha/materialize-snapshot-root!` resolves the annotation
  view once per run
  (`abc.tools.materialize-annotations/resolve-annotation-materialization`,
  fail-closed on unknown/duplicate policy hashes and on anything but exactly
  one aligned plaintext view), materializes per-work
  `annotations/body-annotations.json` + `annotation.manifest.json`, includes
  them in `snapshot-index.json` (snapshot-index schema v0.1.1 admits the
  `annotation` kind), and runs the annotation release guardrails in the
  batch path (`abc.tools.soranoha/validate-annotation-manifests!`). Covered
  by `test/abc/tools/soranoha_annotation_test.clj` and
  `test/abc/tools/materialize_annotations_test.clj`. Corpus-scale join
  statistics tooling exists (`soranoha annotation-join-stats`,
  `test/abc/tools/annotation_join_stats_test.clj`); the corpus run and its
  D7/D9 evidence are pending
  (`docs/superpowers/specs/2026-07-10-per-request-set-annotation-materialization-design.md`).
  Still future work: widening
```

(The original sentence continues "…`schemas/analysis-recipe.schema.json`'s `supported_input_view_kinds` enum…" — keep that continuation intact after "Still future work: widening".)

- [ ] **Step 2: Update the Acceptance Criteria.** In the same ADR's `## Acceptance Criteria` section, find the bullet beginning `- **Not yet built.** A tokenizer-backed analysis recipe fixture` and add immediately AFTER that bullet a new bullet:

```
- **Done (2026-07-10).** Annotation artifacts materialize per request set
  through the batch loop with release guardrails, and join-statistics
  tooling is fixture-tested: `test/abc/tools/soranoha_annotation_test.clj`,
  `test/abc/tools/annotation_join_stats_test.clj`.
```

- [ ] **Step 3: Update the spec status.** In `abc/docs/superpowers/specs/2026-07-10-per-request-set-annotation-materialization-design.md`, replace the `Status:` line with:

```
Status: Slice A and slice B tooling implemented (this branch); corpus run
and D7/D9 evidence handoff pending (post-merge, operator-side)
```

- [ ] **Step 4: Run the gates**

Run (from `abc/`): `bin/kaocha --focus abc.tools.acceptance-criteria-lint-test`
Expected: PASS.

Run (from `abc/`): `clojure -M:abc/validate-design-bundle`
Expected: `design bundle validation ok`.

- [ ] **Step 5: Commit**

```bash
git add docs/adr/0028-ruby-annotation-view.md docs/superpowers/specs/2026-07-10-per-request-set-annotation-materialization-design.md
git commit -m "docs(adr): record per-request-set annotation materialization in ADR 0028"
```
