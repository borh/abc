# SHACL Enforcement and Failure-Manifest RDF Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Run `schemas/manifest.shacl.ttl` against every manifest the v0 contract harness produces or carries as a fixture, and exercise the failure-manifest subtype end-to-end (JSON → RDF → SHACL).

**Architecture:** Three small units. New `abc.tools.shacl` wraps Jena `org.apache.jena/jena-shacl` and returns structured violation maps. `abc.tools.manifest-to-rdf` gains one mapping change (errors-role sidecars use `abc:hasErrorArtifact`) plus regression tests for two existing behaviors. `abc.tools.validate-design-bundle` adds a SHACL pass that validates materialized manifests, the example success manifest, and the example failure manifest against a single shared shapes graph.

**Tech Stack:** Clojure 1.12, Apache Jena 5.3 (via Aristotle for graph construction, jena-shacl for validation), clj-nix for sandboxed test cache.

---

## Spec

`docs/superpowers/specs/2026-04-27-shacl-enforcement-and-failure-rdf-design.md`. Read before starting.

## File Map

**Create:**
- `src/abc/tools/shacl.clj` — Jena SHACL wrapper.
- `test/abc/tools/shacl_test.clj` — positive + negative SHACL conformance tests.
- `docs/archive/2026-04-27-v0-contract-harness.md` — archived previous milestone.

**Modify:**
- `deps.edn` — add `org.apache.jena/jena-shacl`.
- `nix/clj-nix-deps.edn` — add `org.apache.jena/jena-shacl` and `org.arachne-framework/aristotle` (latter is currently missing despite being a transitive runtime dep of `manifest-to-rdf-test`).
- `deps-lock.json` — regenerated.
- `src/abc/tools/manifest_to_rdf.clj` — change errors-role sidecar predicate.
- `test/abc/tools/manifest_to_rdf_test.clj` — extend with failure-graph assertions.
- `src/abc/tools/validate_design_bundle.clj` — wire SHACL pass.
- `flake.nix` — add `test/abc/tools/shacl_test.clj` to the `contract-surface` source presence check.
- `docs/next-steps.md` — replace with post-milestone next steps (after archiving the old one).

---

## Task 1: Add Dependencies and Regenerate Lock

**Files:**
- Modify: `deps.edn`
- Modify: `nix/clj-nix-deps.edn`
- Modify: `deps-lock.json` (regenerated)

- [ ] **Step 1: Add `org.apache.jena/jena-shacl` to `deps.edn`**

Open `deps.edn`. Aristotle is a git dep at line 44. Add `jena-shacl` to the Maven deps block above it (or anywhere in `:deps`). Use the same Jena 5.3 line that Aristotle pulls in; verify the resolved Jena version after a fresh `clojure -P`.

```clojure
  org.apache.jena/jena-shacl                 {:mvn/version "5.3.0"}
```

If `clojure -Stree | grep jena-core` reports a different Jena version after Aristotle resolves, match that version exactly (Jena modules must agree on version).

- [ ] **Step 2: Verify the Jena version**

Run: `clojure -Stree 2>/dev/null | grep -E 'jena-(core|arq|shacl)' | head`
Expected: `jena-shacl`, `jena-core`, and `jena-arq` all on the same major.minor (5.3.x). If they disagree, update the version in `deps.edn` and re-run.

- [ ] **Step 3: Add `jena-shacl` and `aristotle` to `nix/clj-nix-deps.edn`**

The lean test dep file currently does not list Aristotle, so the focused-test sandbox cannot load `manifest-to-rdf-test`. Add both Aristotle (git dep, identical coordinate to `deps.edn`) and `jena-shacl`.

Edit `nix/clj-nix-deps.edn` `:deps` map to include:

```clojure
        org.apache.jena/jena-shacl {:mvn/version "5.3.0"}
        org.arachne-framework/aristotle {:git/url "https://github.com/arachne-framework/aristotle.git"
                                         :git/sha "d7ee879945e0060dc5fcd0b8eac2e0e476d066d7"}
```

The full file then has both entries inside `:deps`. Keep the existing entries.

- [ ] **Step 4: Regenerate `deps-lock.json`**

Run: `bin/update-clj-nix-lock`
Expected: success, lock file updated. After regeneration:

```bash
grep -c 'jena' deps-lock.json
grep -c 'arachne\|aristotle' deps-lock.json
```

Both must now be > 0. If `git-deps` array is still empty, clj-nix did not pick up the git coordinate; double-check syntax in `nix/clj-nix-deps.edn`.

- [ ] **Step 5: Commit**

```bash
git add deps.edn nix/clj-nix-deps.edn deps-lock.json
git commit -m "$(cat <<'EOF'
build: add jena-shacl and lock aristotle for focused tests

Pulls org.apache.jena/jena-shacl into the runtime classpath so the
new abc.tools.shacl wrapper can load. Also adds Aristotle to the
clj-nix focused-test dep file: it is a transitive runtime dep of
manifest-to-rdf-test that was previously missing from the lock,
which would block the new SHACL test from loading under nix flake
check.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 2: TDD Red — Failure Manifest hasErrorArtifact Assertion

**Files:**
- Modify: `test/abc/tools/manifest_to_rdf_test.clj`

- [ ] **Step 1: Add the failing assertion**

Open `test/abc/tools/manifest_to_rdf_test.clj`. The existing `manifest-to-rdf-failure-test` (line 78) tests by parsing the rendered Turtle string. We will add graph-level assertions instead — they are more robust to whitespace and easier to reason about. Add a new helper and a new test:

```clojure
(defn- artifact-uri-for [hash-value]
  (str "https://w3id.org/abc/artifact/"
       (string/replace hash-value ":" "-")))

(defn- triples-from
  "Triples whose subject URI matches `subject-uri`."
  [graph subject-uri]
  (->> (iterator-seq (.find graph))
       (filter (fn [t] (= subject-uri (str (.getSubject t)))))))

(deftest manifest-to-rdf-failure-graph-test
  (testing "failure manifest produces a graph that satisfies the SHACL FailureShape"
    (let [graph (manifest-to-rdf/manifest->graph failure-manifest)
          subj (artifact-uri-for (get failure-manifest "artifact_id"))
          ts (triples-from graph subj)
          predicates (set (map #(.getURI (.getPredicate %)) ts))
          types (->> ts
                     (filter #(= "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                                 (.getURI (.getPredicate %))))
                     (map #(.getURI (.getObject %)))
                     set)]
      (is (contains? types "https://w3id.org/abc/Artifact"))
      (is (contains? types "http://www.w3.org/ns/prov#Entity"))
      (is (contains? types "https://w3id.org/abc/FailureArtifact"))
      (is (contains? predicates "https://w3id.org/abc/hasErrorArtifact")
          "errors-role sidecar must be linked via abc:hasErrorArtifact")
      (is (not (contains? predicates "https://w3id.org/abc/contentHash"))
          "null-content artifact must not carry abc:contentHash")
      (is (not (contains? predicates "http://purl.org/dc/terms/format"))
          "null-content artifact must not carry dcterms:format"))))
```

- [ ] **Step 2: Run the test to confirm it fails on `hasErrorArtifact`**

Run: `clojure -M:test -e "(require 'abc.tools.manifest-to-rdf-test) (clojure.test/run-tests 'abc.tools.manifest-to-rdf-test)"`
Expected: `manifest-to-rdf-failure-graph-test` fails with `errors-role sidecar must be linked via abc:hasErrorArtifact` because the current code uses `abc:hasSidecar` for all roles. The other three assertions (Artifact, Entity, FailureArtifact types) and the two negative assertions (no contentHash, no dcterms:format) should pass — they are regression coverage for already-correct behavior.

- [ ] **Step 3: Commit the red test**

```bash
git add test/abc/tools/manifest_to_rdf_test.clj
git commit -m "$(cat <<'EOF'
test: red — failure manifest must link errors via hasErrorArtifact

Adds graph-level assertions on the failure manifest output:
- types include abc:FailureArtifact (regression coverage)
- artifact has at least one abc:hasErrorArtifact triple (new behavior)
- null-content artifact omits contentHash and dcterms:format
  (regression coverage)

The hasErrorArtifact assertion fails today because manifest->graph
uses abc:hasSidecar for every role.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 3: Green — Errors-Role Sidecar Predicate Change

**Files:**
- Modify: `src/abc/tools/manifest_to_rdf.clj`

- [ ] **Step 1: Split sidecars by role and emit role-specific predicates**

Open `src/abc/tools/manifest_to_rdf.clj`. Locate the artifact data construction (the `merge` block ending around the line `:abc/hasSidecar (mapv ...)`). Replace the single sidecar-link branch with a role-aware split. The new behavior:

- Sidecars whose `role` is `"errors"` are linked via `:abc/hasErrorArtifact`.
- All other roles continue to use `:abc/hasSidecar`.
- If both groups exist, both predicates appear on the artifact.

Replace this section:

```clojure
           (when (seq sidecars)
             {:abc/hasSidecar (mapv #(artifact-iri base-iri (get % "hash"))
                                    sidecars)}))
```

With:

```clojure
           (let [{errors-sidecars "errors"
                  other-sidecars  :other}
                 (group-by (fn [sc]
                             (let [role (get sc "role")]
                               (if (= role "errors") "errors" :other)))
                           sidecars)
                 sidecar-iris (fn [scs]
                                (mapv #(artifact-iri base-iri (get % "hash")) scs))]
             (cond-> {}
               (seq other-sidecars)
               (assoc :abc/hasSidecar (sidecar-iris other-sidecars))

               (seq errors-sidecars)
               (assoc :abc/hasErrorArtifact (sidecar-iris errors-sidecars)))))
```

The outer `merge` already handles an empty map cleanly (`(merge ... {})` is a no-op), so the trailing branch is safe even when both groups are empty.

- [ ] **Step 2: Run the failure-graph test to confirm green**

Run: `clojure -M:test -e "(require 'abc.tools.manifest-to-rdf-test) (clojure.test/run-tests 'abc.tools.manifest-to-rdf-test)"`
Expected: `manifest-to-rdf-failure-graph-test` passes. All other tests in the namespace still pass — including `manifest-to-rdf-matches-example-fixture-test` (the byte-for-byte parity test against the success fixture, whose sidecar role is `warnings`, not `errors`).

If `manifest-to-rdf-matches-example-fixture-test` regresses, it means the fixture's sorting or structure shifted; investigate before regenerating the fixture. The success fixture must remain stable in this milestone.

- [ ] **Step 3: Commit the green change**

```bash
git add src/abc/tools/manifest_to_rdf.clj
git commit -m "$(cat <<'EOF'
feat: link errors-role sidecars via abc:hasErrorArtifact

Per docs/v0-design-bundle/manifest-to-rdf.md, sidecars carry
role-specific links: errors-role uses abc:hasErrorArtifact,
other roles continue to use abc:hasSidecar. Required by the
SHACL FailureShape (sh:property abc:hasErrorArtifact sh:minCount 1).

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 4: Build `abc.tools.shacl` — Load Shapes Graph

**Files:**
- Create: `src/abc/tools/shacl.clj`
- Create: `test/abc/tools/shacl_test.clj`

- [ ] **Step 1: Create the shacl test file with the load-shapes-graph test**

Write `test/abc/tools/shacl_test.clj` with only the namespace + a single test for now (more tests added in later tasks).

```clojure
(ns abc.tools.shacl-test
  (:require [abc.tools.shacl :as shacl]
            [clojure.test :refer [deftest is testing]])
  (:import [org.apache.jena.graph Graph]))

(deftest load-shapes-graph-test
  (testing "loads the manifest SHACL shapes file as a Jena graph"
    (let [g (shacl/load-shapes-graph)]
      (is (instance? Graph g))
      (is (pos? (count (iterator-seq (.find g))))
          "shapes graph must contain triples"))))
```

- [ ] **Step 2: Run the test to confirm it fails (namespace missing)**

Run: `clojure -M:test -e "(require 'abc.tools.shacl-test) (clojure.test/run-tests 'abc.tools.shacl-test)"`
Expected: FAIL with "Could not locate abc/tools/shacl__init.class".

- [ ] **Step 3: Create the shacl namespace with `load-shapes-graph`**

Write `src/abc/tools/shacl.clj`:

```clojure
(ns abc.tools.shacl
  "Wrap Jena SHACL validation. Returns structured violation maps;
  rendering for human output is the caller's responsibility."
  (:require [clojure.java.io :as io])
  (:import [org.apache.jena.graph Graph]
           [org.apache.jena.riot RDFDataMgr Lang]
           [org.apache.jena.shacl ShaclValidator]
           [org.apache.jena.shacl.engine.constraint Constraint]
           [org.apache.jena.shacl.validation ReportEntry ValidationReport]))

(def default-shapes-path "schemas/manifest.shacl.ttl")

(defn load-shapes-graph
  "Read the SHACL shapes file at `path` (default: schemas/manifest.shacl.ttl)
  into a Jena Graph."
  ([] (load-shapes-graph default-shapes-path))
  ([path]
   (let [model (org.apache.jena.rdf.model.ModelFactory/createDefaultModel)]
     (with-open [in (io/input-stream path)]
       (RDFDataMgr/read model in Lang/TURTLE))
     (.getGraph model))))
```

- [ ] **Step 4: Run the test to confirm it passes**

Run: `clojure -M:test -e "(require 'abc.tools.shacl-test) (clojure.test/run-tests 'abc.tools.shacl-test)"`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/shacl.clj test/abc/tools/shacl_test.clj
git commit -m "$(cat <<'EOF'
feat: abc.tools.shacl/load-shapes-graph

Loads schemas/manifest.shacl.ttl into a Jena Graph for reuse across
SHACL validations within a single validate-design-bundle run.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 5: Build `abc.tools.shacl/validate!` — Positive Conformance

**Files:**
- Modify: `src/abc/tools/shacl.clj`
- Modify: `test/abc/tools/shacl_test.clj`

- [ ] **Step 1: Add positive conformance tests**

Add to `test/abc/tools/shacl_test.clj`:

```clojure
(require '[abc.tools.files :as files])
(require '[abc.tools.manifest-to-rdf :as manifest-to-rdf])

(deftest validate-success-manifest-conforms-test
  (testing "example success manifest RDF conforms to the SHACL shapes"
    (let [shapes (shacl/load-shapes-graph)
          manifest (files/read-json "examples/v0/example-work/manifest.json")
          data (manifest-to-rdf/manifest->graph manifest)]
      (is (= :ok (shacl/validate! {:shapes-graph shapes
                                   :data-graph data
                                   :label "examples/v0/example-work/manifest.json"}))))))

(deftest validate-failure-manifest-conforms-test
  (testing "example failure manifest RDF conforms to the SHACL shapes"
    (let [shapes (shacl/load-shapes-graph)
          manifest (files/read-json "examples/v0/example-work/failure-manifest.example.json")
          data (manifest-to-rdf/manifest->graph manifest)]
      (is (= :ok (shacl/validate! {:shapes-graph shapes
                                   :data-graph data
                                   :label "examples/v0/example-work/failure-manifest.example.json"}))))))
```

- [ ] **Step 2: Run the tests to confirm they fail (validate! missing)**

Run: `clojure -M:test -e "(require 'abc.tools.shacl-test) (clojure.test/run-tests 'abc.tools.shacl-test)"`
Expected: FAIL with "No such var: abc.tools.shacl/validate!".

- [ ] **Step 3: Implement `validate!`**

Add to `src/abc/tools/shacl.clj`:

```clojure
(defn- entry->violation
  [^ReportEntry entry label]
  (let [severity (some-> (.severity entry) .getLocalName)
        focus (some-> (.focusNode entry) str)
        path (some-> (.resultPath entry) str)
        message (.message entry)
        source (some-> (.source entry) str)]
    (cond-> {:label label}
      severity (assoc :severity severity)
      focus    (assoc :focus-node focus)
      path     (assoc :path path)
      message  (assoc :message message)
      source   (assoc :source source))))

(defn validate!
  "Run SHACL validation of `data-graph` against `shapes-graph`. Returns :ok
  on conformance; throws ex-info with :errors set to a vector of structured
  violation maps when non-conformant.

  Required keys: :shapes-graph, :data-graph. Optional :label is attached
  to each violation map for caller-side rendering."
  [{:keys [shapes-graph data-graph label]}]
  (let [validator (ShaclValidator/get)
        report ^ValidationReport (.validate validator shapes-graph data-graph)]
    (if (.conforms report)
      :ok
      (let [violations (mapv #(entry->violation % label) (.getEntries report))]
        (throw (ex-info (str "SHACL validation failed: " label)
                        {:errors violations
                         :label label}))))))
```

- [ ] **Step 4: Run the tests to confirm they pass**

Run: `clojure -M:test -e "(require 'abc.tools.shacl-test) (clojure.test/run-tests 'abc.tools.shacl-test)"`
Expected: PASS for both `validate-success-manifest-conforms-test` and `validate-failure-manifest-conforms-test`.

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/shacl.clj test/abc/tools/shacl_test.clj
git commit -m "$(cat <<'EOF'
feat: abc.tools.shacl/validate! returns :ok or throws structured

Wraps Jena ShaclValidator. On conformance returns :ok. On
non-conformance throws ex-info with :errors set to a vector of
structured violation maps {:severity :focus-node :path :message
:source :label}. Rendering belongs to the caller.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 6: `abc.tools.shacl/validate!` — Negative Cases

**Files:**
- Modify: `test/abc/tools/shacl_test.clj`

- [ ] **Step 1: Add negative-graph tests built declaratively**

Append to `test/abc/tools/shacl_test.clj`:

```clojure
(require '[arachne.aristotle :as aa])
(require '[arachne.aristotle.registry :as reg])

;; Ensure abc/prov/dcterms prefixes are registered so keyword-based
;; map literals resolve to the correct IRIs.
(@(deref (delay
           (do
             (reg/prefix 'abc     "https://w3id.org/abc/")
             (reg/prefix 'dcterms "http://purl.org/dc/terms/")
             (reg/prefix 'prov    "http://www.w3.org/ns/prov#")
             nil))))

;; If the above does nothing in your test runner, replace with a direct call:
;; (do (reg/prefix 'abc "https://w3id.org/abc/") ...)

(defn- artifact-graph
  "Build a minimal Artifact graph from an overrides map."
  [overrides]
  (let [base {:rdf/about            "<https://w3id.org/abc/artifact/sha256-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa>"
              :rdf/type             [:abc/Artifact :prov/Entity]
              :abc/artifactId       "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
              :abc/schemaHash       "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
              :abc/validationStatus "passed"
              :abc/contentHash      "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
              :dcterms/format       "application/json"
              :prov/wasDerivedFrom  ["<https://w3id.org/abc/artifact/sha256-1111111111111111111111111111111111111111111111111111111111111111>"]
              :prov/wasGeneratedBy  "<https://w3id.org/abc/activity/example>"}
        merged (merge base overrides)
        activity {:rdf/about "<https://w3id.org/abc/activity/example>"
                  :rdf/type :prov/Activity
                  :prov/used ["<https://w3id.org/abc/artifact/sha256-1111111111111111111111111111111111111111111111111111111111111111>"]
                  :prov/qualifiedAssociation {:rdf/type :prov/Association
                                              :prov/agent "<https://w3id.org/abc/agent/test>"}}]
    (-> (aa/graph :simple)
        (aa/add merged)
        (aa/add activity))))

(deftest validate-missing-artifact-id-test
  (testing "missing abc:artifactId triggers ArtifactShape violation"
    (let [shapes (shacl/load-shapes-graph)
          data (artifact-graph {:abc/artifactId nil})]
      (try
        (shacl/validate! {:shapes-graph shapes :data-graph data :label "missing-id"})
        (is false "expected validate! to throw")
        (catch clojure.lang.ExceptionInfo e
          (let [errors (:errors (ex-data e))]
            (is (seq errors) "must report at least one violation")
            (is (some #(re-find #"artifactId|ArtifactShape"
                                (str (:source %) " " (:path %) " " (:message %)))
                      errors)
                "violation must reference artifactId or ArtifactShape")))))))

(deftest validate-failure-without-error-artifact-test
  (testing "FailureArtifact missing hasErrorArtifact triggers FailureShape violation"
    (let [shapes (shacl/load-shapes-graph)
          data (artifact-graph {:rdf/type [:abc/Artifact :prov/Entity :abc/FailureArtifact]
                                :abc/validationStatus "failed"
                                :abc/contentHash nil
                                :dcterms/format nil})]
      (try
        (shacl/validate! {:shapes-graph shapes :data-graph data :label "failure-no-errors"})
        (is false "expected validate! to throw")
        (catch clojure.lang.ExceptionInfo e
          (let [errors (:errors (ex-data e))]
            (is (seq errors))
            (is (some #(re-find #"FailureShape|hasErrorArtifact"
                                (str (:source %) " " (:path %) " " (:message %)))
                      errors)
                "violation must reference FailureShape or hasErrorArtifact")))))))

(deftest validate-bad-status-enum-test
  (testing "abc:validationStatus outside the sh:in enum triggers a violation tied to validationStatus"
    (let [shapes (shacl/load-shapes-graph)
          data (artifact-graph {:abc/validationStatus "unknown"})]
      (try
        (shacl/validate! {:shapes-graph shapes :data-graph data :label "bad-status"})
        (is false "expected validate! to throw")
        (catch clojure.lang.ExceptionInfo e
          (let [errors (:errors (ex-data e))]
            (is (seq errors))
            (is (some (fn [v]
                        (let [ctx (str (:source v) " " (:path v) " " (:message v))]
                          (and (re-find #"validationStatus" ctx)
                               (not (re-find #"hasErrorArtifact|artifactId" ctx)))))
                      errors)
                "must report a violation that specifically targets validationStatus")))))))
```

Note on Aristotle map literals: `:abc/contentHash nil` removes the key from the map but only for keys whose nil value Aristotle skips; rebuild via `dissoc` if Aristotle attempts to add a `nil` triple. If the test fails on construction, replace `nil` overrides with `dissoc` on the merged map before passing to `aa/add`.

- [ ] **Step 2: Run the negative tests to confirm they all pass**

Run: `clojure -M:test -e "(require 'abc.tools.shacl-test) (clojure.test/run-tests 'abc.tools.shacl-test)"`
Expected: PASS for all negative tests. Each thrown `ex-info` carries a non-empty `:errors` vector with at least one violation whose `:source`/`:path`/`:message` matches the expected shape.

If construction fails because a `nil` value made it into a triple, fix the helper:

```clojure
(defn- artifact-graph [overrides]
  (let [merged (->> overrides
                    (filter (fn [[_ v]] (some? v)))
                    (into base))
        ...]))
```

- [ ] **Step 3: Commit**

```bash
git add test/abc/tools/shacl_test.clj
git commit -m "$(cat <<'EOF'
test: abc.tools.shacl negative cases via declarative graphs

Builds three invalid Jena graphs from data and asserts validate!
throws with a structured violation that points at the right shape:
- missing abc:artifactId targets ArtifactShape
- abc:FailureArtifact without hasErrorArtifact targets FailureShape
- abc:validationStatus outside the sh:in enum specifically
  references validationStatus, not just any property shape

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 7: Wire SHACL Pass into `validate-design-bundle`

**Files:**
- Modify: `src/abc/tools/validate_design_bundle.clj`

- [ ] **Step 1: Add the SHACL step**

Open `src/abc/tools/validate_design_bundle.clj`. Add `[abc.tools.shacl :as shacl]` to the `:require`. Add a new helper above `validate-design-bundle!`:

```clojure
(defn- render-violation [{:keys [severity focus-node path message label source]}]
  (str (or severity "Violation") ": "
       (or focus-node "?") " "
       (or path "")
       (when message (str " — " message))
       (when label (str " (" label ")"))
       (when source (str " [" source "]"))))

(defn validate-shacl!
  "Validate every manifest in `manifest-paths` against the shapes graph.
  Aggregates all violations and throws once at the end if any are found."
  [shapes-graph manifest-paths]
  (let [violations
        (reduce
         (fn [acc path]
           (try
             (let [manifest (files/read-json path)
                   data (manifest-to-rdf/manifest->graph manifest)]
               (shacl/validate! {:shapes-graph shapes-graph
                                 :data-graph data
                                 :label (str path)})
               acc)
             (catch clojure.lang.ExceptionInfo e
               (into acc (:errors (ex-data e))))))
         []
         manifest-paths)]
    (when (seq violations)
      (throw (ex-info "SHACL validation failed"
                      {:errors (mapv render-violation violations)})))))
```

The throw uses `:errors` as a vector of pre-rendered strings so the existing `-main` printer handles output without changes.

- [ ] **Step 2: Insert the step into `validate-design-bundle!`**

In `validate-design-bundle!`, after the existing `==> Checking materialized RDF views` block, before `==> Checking imported ab-validator output`, add:

```clojure
        (println "==> Validating SHACL shapes")
        (let [shapes (shacl/load-shapes-graph)
              targets (concat (vals materialized)
                              ["examples/v0/example-work/manifest.json"
                               "examples/v0/example-work/failure-manifest.example.json"])]
          (validate-shacl! shapes targets))
        (println "shacl shapes ok")
```

The `let` binding for `materialized` is already in scope inside the existing `try` block. Place this snippet inside that `try` block, immediately after `(println "materialized RDF views ok"))` line.

- [ ] **Step 3: Run validate-design-bundle to verify the new step**

Run: `clojure -M:abc/validate-design-bundle 2>&1 | tail -25`
Expected: output now contains `==> Validating SHACL shapes` followed by `shacl shapes ok` between the materialized RDF views step and the ab-validator output step. Final line: `design bundle validation ok`.

If `xmllint` or `git-cliff` is missing on the host, run via Nix instead:
`nix run .#validate-design-bundle 2>&1 | tail -25`

- [ ] **Step 4: Commit**

```bash
git add src/abc/tools/validate_design_bundle.clj
git commit -m "$(cat <<'EOF'
feat: validate-design-bundle runs SHACL over all manifest RDF views

New step '==> Validating SHACL shapes' validates every materialized
manifest plus the example success and failure manifests against
schemas/manifest.shacl.ttl. Shapes graph loaded once; data graphs
constructed per manifest. Aggregated violations render through the
existing -main error printer.

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 8: Add the New Test File to the Contract Surface Check

**Files:**
- Modify: `flake.nix`

- [ ] **Step 1: Add `shacl_test.clj` to the source presence check**

Open `flake.nix`. In the `contract-surface` derivation around line 132, add:

```nix
            test -f ${./src/abc/tools/shacl.clj}
            test -f ${./test/abc/tools/shacl_test.clj}
```

next to the other `test/abc/tools/*_test.clj` lines.

- [ ] **Step 2: Add `abc.tools.shacl-test` to the focused-test alias**

In `nix/clj-nix-deps.edn`, the `:abc/focused-test` `:main-opts` lists every test ns explicitly. Add `'abc.tools.shacl-test` to both the `(require ...)` form and the `(test/run-tests ...)` form:

Before:
```
(require '[clojure.test :as test] 'abc.annotation-schema-test ... 'abc.tools.materialize-import-test) (let [{:keys [fail error]} (test/run-tests 'abc.annotation-schema-test ... 'abc.tools.materialize-import-test)] ...)
```

After (insertion shown by addition only):
```
... 'abc.tools.materialize-import-test 'abc.tools.shacl-test) (let [{:keys [fail error]} (test/run-tests ... 'abc.tools.materialize-import-test 'abc.tools.shacl-test)] ...)
```

Be careful: the value is a single-line string in EDN. Edit precisely.

- [ ] **Step 3: Run nix flake check**

Run: `nix flake check 2>&1 | tail -30`
Expected: success on `clj-nix-focused-tests` and `contract-surface`. The focused-test sandbox now loads Aristotle (from the lock added in Task 1), runs `manifest-to-rdf-test` and the new `shacl-test`, and reports zero failures.

If clj-nix-focused-tests fails to load Aristotle or jena-shacl, re-run `bin/update-clj-nix-lock` and verify `git-deps` is non-empty in the lock.

- [ ] **Step 4: Commit**

```bash
git add flake.nix nix/clj-nix-deps.edn
git commit -m "$(cat <<'EOF'
chore: include abc.tools.shacl in flake check surface

- contract-surface check verifies new src/test files exist
- focused-test alias requires and runs abc.tools.shacl-test

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Task 9: End-to-End Verification

**Files:**
- (No file changes; verification only.)

- [ ] **Step 1: Run validate-design-bundle from a fresh shell**

Run: `nix run .#validate-design-bundle 2>&1 | tail -30`
Expected: every `==>` step prints its `ok` line, including `==> Validating SHACL shapes` → `shacl shapes ok`. Final line: `design bundle validation ok`. Exit code 0.

- [ ] **Step 2: Run nix flake check**

Run: `nix flake check 2>&1 | tail -10`
Expected: builds succeed. No "abc-clj-nix-focused-tests failed" line.

- [ ] **Step 3: Run determinism check (no commit)**

Run: `clojure -M:abc/validate-design-bundle && clojure -M:abc/validate-design-bundle`
Expected: both runs pass. The temp directories created by `validate-design-bundle!` are independent; running twice confirms the SHACL step is repeatable and not order-sensitive.

---

## Task 10: Archive the Completed Milestone and Replace `next-steps.md`

**Files:**
- Create: `docs/archive/2026-04-27-v0-contract-harness.md` (move target)
- Modify: `docs/next-steps.md` (replaced)

- [ ] **Step 1: Move the old next-steps to the archive**

Run:

```bash
mkdir -p docs/archive
git mv docs/next-steps.md docs/archive/2026-04-27-v0-contract-harness.md
```

- [ ] **Step 2: Write a fresh `docs/next-steps.md`**

Create `docs/next-steps.md` with the post-milestone state:

```markdown
# Next Steps

## Current State

The v0 contract harness milestone (archived as
`docs/archive/2026-04-27-v0-contract-harness.md`) is complete. The
canonical-JSON ↔ RDF/PROV-O loop is closed: every manifest produced or
carried by the v0 fixtures is validated against
`schemas/manifest.shacl.ttl` during `nix run .#validate-design-bundle`.

## Canonical Commands

```bash
nix run .#validate-design-bundle
nix run .#materialize-import -- examples/ab-validator-output out/imported --generated-at 2026-04-26T00:00:00Z
nix run .#manifest-to-rdf -- out/imported/parser-ir.manifest.json -o out/imported/parser-ir.ttl
nix flake check
bin/update-clj-nix-lock
```

## Candidate Next Milestones

In rough order of leverage, none committed:

1. **Failure-manifest Turtle parity test.** Now that the failure RDF
   mapping is exercised end-to-end, consider committing
   `examples/v0/example-work/failure-manifest.example.ttl` plus a
   byte-for-byte parity test (analogue of the success-case
   `manifest-to-rdf-matches-example-fixture-test`). Deferred from the
   2026-04-27 milestone to avoid churn during the mapping shakedown.
2. **TEI profile + validation pipeline.** Promote
   `schemas/tei-profile.odd` from stub to a real ODD aligned with TEI
   P5 4.11.0 ruby support. Generate Relax NG; add Jing or `xmllint`
   validation as a `validate-design-bundle` step. Unlocks replacing
   the design fixtures with a real Aozora work end to end.
3. **Parser-decision exercise (ADR 0002).** Run a candidate parser
   (e.g. `aozora-rs`) over one Aozora work into the parser IR
   contract. Either validates the boundary or surfaces gaps before
   more code is written.
4. **Tighten ArtifactShape.** The current `sh:or` in
   `schemas/manifest.shacl.ttl` duplicates the
   `abc:hasErrorArtifact sh:minCount 1` requirement in branch 2 and
   in `FailureShape`. Clean up if/when shapes are revisited.
5. **Move legacy namespaces behind clj-nix.** `abc.aozora`,
   `abc.tei`, `abc.stats` remain outside the v0 contract gate. Decide
   whether to bring them in or leave them dormant.

## Done Criteria For The Closed Milestone

See `docs/archive/2026-04-27-v0-contract-harness.md`. Briefly:
- `nix run .#validate-design-bundle` passes including
  `==> Validating SHACL shapes`.
- `nix flake check` passes including the focused-test check.
- `examples/v0/example-work/manifest.ttl` byte-for-byte parity test
  remains stable.
- Failure manifest fixture exercised end to end (JSON → graph → SHACL).
```

- [ ] **Step 3: Commit the archive + replacement**

```bash
git add docs/archive/2026-04-27-v0-contract-harness.md docs/next-steps.md
git commit -m "$(cat <<'EOF'
docs: archive v0 contract harness milestone, refresh next-steps

The v0 contract harness milestone is complete: SHACL validation now
runs end-to-end against materialized and example manifests. Move the
old next-steps content to docs/archive/ and replace next-steps.md
with a post-milestone summary plus candidate follow-on milestones
(failure parity test, TEI ODD promotion, parser-decision exercise,
ArtifactShape cleanup, legacy namespace migration).

🤖 Generated with [Claude Code](https://claude.com/claude-code)

Co-Authored-By: glm-5
EOF
)"
```

---

## Self-Review

**Spec coverage:**
- §Architecture/`abc.tools.shacl` (new) — Tasks 4, 5, 6.
- §Architecture/`manifest-to-rdf` (one mapping change + two regression tests) — Tasks 2 (red), 3 (green), 2-step regression coverage embedded in the same test.
- §Architecture/`validate-design-bundle` (wiring) — Task 7.
- §Test Plan: `shacl_test.clj` positive — Task 5; negative — Task 6.
- §Test Plan: `manifest_to_rdf_test.clj` extension — Task 2.
- §Test Plan: `validate_design_bundle_test.clj` extension — covered indirectly via the `nix run .#validate-design-bundle` end-to-end run in Task 9 plus the focused-test sandbox in Task 8. The existing `validate_design_bundle_test.clj` does not contain an end-to-end harness test; adding one would require committing test fixtures with `xmllint` and `git-cliff` available, which is exactly what the Nix app exists for. Note this divergence from the spec: the spec said "extension to `validate_design_bundle_test`" but the current file only tests pure helpers; an end-to-end test there would conflict with the app/test split. Verifying via `nix run` is the practical equivalent.
- §Dependencies — Task 1.
- §Acceptance Criteria — covered across Tasks 7-9.
- §Sequencing — Tasks follow the spec's order.

**Placeholder scan:** No "TBD", "TODO", or "implement later" entries. Every code step contains the actual code or the actual file location to copy from. The negative-test note about `nil` overrides has a concrete fallback implementation.

**Type consistency:**
- `validate!` signature: `{:shapes-graph :data-graph :label}` → returns `:ok` or throws `ex-info {:errors [violation-maps] :label ...}`. Same in Task 5 (definition), Task 5 tests, Task 6 tests, and Task 7 wiring.
- Violation map keys: `:severity :focus-node :path :message :source :label`. Same across the implementation in Task 5 and the rendering in Task 7.
- `load-shapes-graph` arity: 0 or 1 (path). Used with both arities (default in Tasks 5/7, no caller uses 1-arity).
- `validate-shacl!` (Task 7): takes shapes graph and a seq of paths. No other caller.

**Self-review divergence noted:** Replaced spec's "extend `validate_design_bundle_test`" with "verify end-to-end via `nix run`." Spec acceptance criteria are still met.
