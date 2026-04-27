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

**Pre-check (verified at plan time, 2026-04-27):** `nix flake check` is currently broken at the `clj-nix-focused-tests` step. Cause: `Could not locate arachne/aristotle__init.class` — the recent Aristotle refactor (commit `3fd2c53`) regenerated the lock but the `git-deps` array remained empty, and `nix/clj-nix-deps.edn` does not list Aristotle, so the focused-test sandbox cannot load `manifest-to-rdf-test`. This task must close that gap as well as add jena-shacl. If `bin/update-clj-nix-lock` does not produce a non-empty `git-deps` array after adding Aristotle to `nix/clj-nix-deps.edn`, fall back to listing Aristotle directly in `deps.edn` only and exclude `manifest-to-rdf-test` and `shacl-test` from the focused-test alias (with a note explaining why); raise the clj-nix git-deps question with the user before that fallback.

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

- [ ] **Step 4: Regenerate `deps-lock.json` and verify both Maven and git deps land**

Run: `bin/update-clj-nix-lock`
Expected: success, lock file updated. After regeneration, run all three:

```bash
grep -c 'jena' deps-lock.json
grep -c 'arachne\|aristotle' deps-lock.json
grep '"git-deps"' deps-lock.json
```

The first two must be > 0. The `git-deps` line must show a non-empty array (e.g. `"git-deps": [{...}]`).

If `git-deps` is still empty after the regenerate, clj-nix isn't picking up the Aristotle git dep. Try this in order:

1. Re-run `bin/update-clj-nix-lock` with no additional flags after confirming `nix/clj-nix-deps.edn` syntax is valid EDN.
2. If still empty, run `nix run github:jlesquembre/clj-nix#deps-lock -- --help` and check whether a `--include-git-deps` or equivalent flag exists.
3. If clj-nix genuinely cannot lock git deps in this configuration, stop and ask the user before proceeding. Possible workarounds (do NOT apply unilaterally):
   - Vendor Aristotle as a `:local/root` dep alongside `deps.edn`.
   - Use `org.arachne-framework/aristotle` from Clojars if a Maven release exists matching the current git sha.
   - Drop `manifest-to-rdf-test` and `shacl-test` from `:abc/focused-test` and document the gap in `docs/next-steps.md` until the lock issue is resolved upstream.

- [ ] **Step 4b: Confirm `nix flake check` passes after the lock change**

Run: `nix flake check 2>&1 | tail -10`
Expected: both `clj-nix-focused-tests` and `contract-surface` succeed. The Aristotle ClassNotFoundException seen at plan time should be gone (the new `shacl-test` does not exist yet — that's added in later tasks; this step confirms only that the existing tests now load Aristotle).

If `clj-nix-focused-tests` still fails with the same Aristotle error, do not commit; revisit Step 4 fallbacks first.

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

Append to `test/abc/tools/shacl_test.clj`. The helper takes the **complete** artifact data each test wants — no base+overrides, no nil-removal semantics. The shared invariant is just the Activity node (required by `prov:wasGeneratedBy sh:class prov:Activity`).

```clojure
(require '[arachne.aristotle :as aa])
(require '[arachne.aristotle.registry :as reg])

;; Register prefixes so keyword-based map literals resolve to the correct IRIs.
;; Idempotent; safe to evaluate at load time.
(reg/prefix 'abc     "https://w3id.org/abc/")
(reg/prefix 'dcterms "http://purl.org/dc/terms/")
(reg/prefix 'prov    "http://www.w3.org/ns/prov#")

(def example-activity
  {:rdf/about "<https://w3id.org/abc/activity/example>"
   :rdf/type :prov/Activity
   :prov/used ["<https://w3id.org/abc/artifact/sha256-1111111111111111111111111111111111111111111111111111111111111111>"]
   :prov/qualifiedAssociation {:rdf/type :prov/Association
                               :prov/agent "<https://w3id.org/abc/agent/test>"}})

(defn- graph-with-artifact
  "Build a Jena graph containing the supplied artifact node plus the shared
  example Activity. The caller supplies the full artifact map; this helper
  does not merge or remove keys."
  [artifact]
  (-> (aa/graph :simple)
      (aa/add artifact)
      (aa/add example-activity)))

(def artifact-uri
  "<https://w3id.org/abc/artifact/sha256-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa>")

(def derived-uri
  "<https://w3id.org/abc/artifact/sha256-1111111111111111111111111111111111111111111111111111111111111111>")

(deftest validate-missing-artifact-id-test
  (testing "missing abc:artifactId triggers ArtifactShape violation"
    (let [shapes (shacl/load-shapes-graph)
          data (graph-with-artifact
                {:rdf/about            artifact-uri
                 :rdf/type             [:abc/Artifact :prov/Entity]
                 ;; deliberately no :abc/artifactId
                 :abc/schemaHash       "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                 :abc/validationStatus "passed"
                 :abc/contentHash      "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                 :dcterms/format       "application/json"
                 :prov/wasDerivedFrom  [derived-uri]
                 :prov/wasGeneratedBy  "<https://w3id.org/abc/activity/example>"})]
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
          data (graph-with-artifact
                {:rdf/about            artifact-uri
                 :rdf/type             [:abc/Artifact :prov/Entity :abc/FailureArtifact]
                 :abc/artifactId       "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                 :abc/schemaHash       "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                 :abc/validationStatus "failed"
                 ;; deliberately no :abc/hasErrorArtifact
                 :prov/wasDerivedFrom  [derived-uri]
                 :prov/wasGeneratedBy  "<https://w3id.org/abc/activity/example>"})]
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
  (testing "abc:validationStatus outside the sh:in enum reports a violation tied to validationStatus"
    (let [shapes (shacl/load-shapes-graph)
          data (graph-with-artifact
                {:rdf/about            artifact-uri
                 :rdf/type             [:abc/Artifact :prov/Entity]
                 :abc/artifactId       "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                 :abc/schemaHash       "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                 :abc/validationStatus "unknown"
                 :abc/contentHash      "sha256:cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
                 :dcterms/format       "application/json"
                 :prov/wasDerivedFrom  [derived-uri]
                 :prov/wasGeneratedBy  "<https://w3id.org/abc/activity/example>"})]
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
                "must report a violation that specifically targets validationStatus, not hasErrorArtifact or artifactId")))))))
```

- [ ] **Step 2: Run the negative tests to confirm they all pass**

Run: `clojure -M:test -e "(require 'abc.tools.shacl-test) (clojure.test/run-tests 'abc.tools.shacl-test)"`
Expected: PASS for all negative tests. Each thrown `ex-info` carries a non-empty `:errors` vector with at least one violation whose `:source`/`:path`/`:message` matches the expected shape.

If a SHACL violation we expect does not surface (e.g. the validator passes a graph we built as invalid), the most likely cause is that the artifact node lacks one of the always-required edges (rdf:type triples for both `abc:Artifact` and `prov:Entity`, or the `prov:wasGeneratedBy` link), so the relevant shape never targets it. Re-check the literal map in the failing test against `schemas/manifest.shacl.ttl`'s `targetClass` and `sh:property` rules.

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
;; Spec format: "<severity>: <focus> <path> — <message> (<label>)".
;; We append "[<source>]" because the source shape IRI is high-signal
;; for debugging and the spec did not pin punctuation, only fields.
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

## Task 8.5: Extend `validate_design_bundle_test.clj` With a SHACL Smoke Test

**Files:**
- Modify: `test/abc/tools/validate_design_bundle_test.clj`

The spec's acceptance criteria require `clojure -M:test` (focused namespaces) to pass with an extended `validate_design_bundle_test`. The existing file tests only pure helpers, but adding a SHACL smoke assertion satisfies the spec without forcing an `xmllint`/`git-cliff` end-to-end run.

- [ ] **Step 1: Add the smoke test**

Open `test/abc/tools/validate_design_bundle_test.clj`. Extend the `:require` to include shacl and manifest-to-rdf:

```clojure
(:require [abc.tools.files :as files]
          [abc.tools.manifest-to-rdf :as manifest-to-rdf]
          [abc.tools.shacl :as shacl]
          [abc.tools.validate-design-bundle :as validate]
          [clojure.test :refer [deftest is testing]])
```

Append:

```clojure
(deftest validate-shacl-smoke-test
  (testing "validate-design-bundle SHACL pass conforms for the example success manifest"
    (let [shapes (shacl/load-shapes-graph)
          manifest (files/read-json "examples/v0/example-work/manifest.json")
          data (manifest-to-rdf/manifest->graph manifest)]
      (is (= :ok (shacl/validate! {:shapes-graph shapes
                                   :data-graph data
                                   :label "validate_design_bundle_test"})))))

  (testing "validate-shacl! aggregates and surfaces violations"
    (let [shapes (shacl/load-shapes-graph)
          manifest (files/read-json "examples/v0/example-work/failure-manifest.example.json")
          data (manifest-to-rdf/manifest->graph manifest)]
      (is (= :ok (shacl/validate! {:shapes-graph shapes
                                   :data-graph data
                                   :label "validate_design_bundle_test"}))))))
```

- [ ] **Step 2: Run the test**

Run: `clojure -M:test -e "(require 'abc.tools.validate-design-bundle-test) (clojure.test/run-tests 'abc.tools.validate-design-bundle-test)"`
Expected: PASS for the smoke tests; existing helper tests unchanged.

- [ ] **Step 3: Commit**

```bash
git add test/abc/tools/validate_design_bundle_test.clj
git commit -m "$(cat <<'EOF'
test: smoke-test SHACL conformance from validate-design-bundle-test

Asserts that the example success and failure manifests' RDF graphs
conform to schemas/manifest.shacl.ttl when validated through the
abc.tools.shacl wrapper. Satisfies the spec's acceptance criterion
that the focused validate-design-bundle test exercise the SHACL pass.

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
- §Test Plan: `shacl_test.clj` positive — Task 5; negative — Task 6 (declarative graph construction; helper takes complete artifact data, no nil-removal semantics).
- §Test Plan: `manifest_to_rdf_test.clj` extension — Task 2.
- §Test Plan: `validate_design_bundle_test.clj` extension — Task 8.5 adds a SHACL smoke test that exercises both the success and failure example manifests through `shacl/validate!`. Satisfies the spec's `clojure -M:test` acceptance criterion.
- §Dependencies — Task 1, with explicit fallback contingency if clj-nix cannot lock the Aristotle git dep.
- §Acceptance Criteria — covered across Tasks 7-9.
- §Sequencing — Tasks follow the spec's order; Task 8.5 is inserted between flake-surface wiring (Task 8) and end-to-end verification (Task 9).

**Placeholder scan:** No "TBD", "TODO", or "implement later" entries. Every code step contains the actual code or the actual file location to copy from. The negative-test note about `nil` overrides has a concrete fallback implementation.

**Type consistency:**
- `validate!` signature: `{:shapes-graph :data-graph :label}` → returns `:ok` or throws `ex-info {:errors [violation-maps] :label ...}`. Same in Task 5 (definition), Task 5 tests, Task 6 tests, and Task 7 wiring.
- Violation map keys: `:severity :focus-node :path :message :source :label`. Same across the implementation in Task 5 and the rendering in Task 7.
- `load-shapes-graph` arity: 0 or 1 (path). Used with both arities (default in Tasks 5/7, no caller uses 1-arity).
- `validate-shacl!` (Task 7): takes shapes graph and a seq of paths. No other caller.

**Pre-existing build gap:** `nix flake check` is broken at plan time because the recent Aristotle refactor's lock regeneration produced an empty `git-deps` array. Task 1 fixes this as part of the dependency change. If clj-nix cannot lock the Aristotle git coordinate, Task 1 stops and asks before applying any workaround.

**Render-violation deviation from spec:** the spec format is `<severity>: <focus> <path> — <message> (<label>)`. Task 7's renderer appends `[<source>]` because the source-shape IRI is high-signal for debugging and the spec explicitly does not pin punctuation. Documented inline in Task 7.
