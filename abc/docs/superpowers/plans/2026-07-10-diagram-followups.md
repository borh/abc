# Diagram Feature Follow-ups Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.
>
> **STATUS: DRAFT FOR REVIEW — not yet approved for execution.** The executable scope is Tasks 1–5. Schema rendering/registration remains explicitly deferred under “Deferred design work”; it is not an implementation task in this plan.

**Goal:** Harden the ADR and architecture diagram tooling without accepting malformed ADR syntax, leaking test files, silently merging Mermaid nodes, or adding a second generated copy of the architecture diagram.

**Architecture:** Preserve ADR 0029’s three sources of truth and whole-artifact drift model. Malformed ADR references become explicit lint data while only four-digit references enter graph edges; Mermaid validates node identity before rendering; tests own temporary paths for the duration of one test; `architecture.md` links to the single generated `.mmd` artifact instead of acquiring marker-injection machinery.

**Tech Stack:** Clojure (`abc.tools.diagram.*`), EDN, Mermaid, kaocha (`bin/kaocha`), and the `abc/` Nix checks.

## Global Constraints

- Run Clojure commands from `abc/`: `bin/kaocha …` and `clojure -M:abc/diagrams …`. Never use `clojure -M:test`.
- Preserve ADR 0029’s four-digit reference width: references are `ADR NNNN`. A shorter or longer digit token must produce a lint problem even when its normalized number names an existing ADR.
- Malformed input must be reported through `lint-adrs`/`lint*`, not by `NumberFormatException`.
- Do not change the header-vs-sidecar edge-type partition from ADR 0029.
- Do not make private parsing helpers public solely to unit-test them; exercise header parsing through `parse-adr`.
- Keep generated Mermaid byte-stable. Any renderer/builder change must leave `clojure -M:abc/diagrams --check` and the flake `diagram-drift` check green.
- Keep temporary filesystem ownership inside the test that mutates the file and delete the file in `finally`.
- Run cljfmt/clj-kondo through the repository checks for touched Clojure files; no compiler warnings.
- Preserve unrelated user changes. The plan file itself is an untracked review draft until explicitly approved.

## File Structure

- `abc/src/abc/tools/diagram/adr_graph.clj` — parse raw ADR reference tokens and lint malformed widths; validate optional relation notes.
- `abc/test/abc/tools/diagram/adr_graph_test.clj` — parser/lint tests through public behavior.
- `abc/src/abc/tools/diagram/mermaid.clj` — reject colliding sanitized node IDs before rendering.
- `abc/test/abc/tools/diagram/mermaid_test.clj` — collision regression test.
- `abc/test/abc/tools/diagram/core_test.clj` — per-test temporary output ownership and cleanup.
- `abc/docs/adr/adr-relations.edn` — document `:note` as source-only metadata.
- `abc/docs/architecture.md` — replace the hand-maintained pipeline sketch with a link to the generated Mermaid artifact.

---

## Batch A — Safe hardening

### Task 1: Report malformed ADR reference widths without rendering them

**Problem:** The current regex sees only `ADR` followed by exactly four digits. `ADR 6`, `ADR 60`, and `ADR 12345` therefore vanish before linting. Simply loosening the regex and calling `Integer/parseInt` would incorrectly accept `ADR 6` when ADR 0006 exists and could throw on an arbitrarily long token.

**Files:**
- Modify: `abc/src/abc/tools/diagram/adr_graph.clj`
- Test: `abc/test/abc/tools/diagram/adr_graph_test.clj`

**Interfaces:**
- `parse-adr [dir filename]` remains public and retains its existing graph fields.
- Parsed ADR maps additionally contain `:malformed-refs`, a vector of `{:field string :token string}` values.
- `lint-adrs [adrs relations]` emits one problem for each malformed token.
- Private reference helpers remain private.

- [ ] **Step 1: Add the failing parser/lint regression test**

Add:

```clojure
(deftest malformed-reference-width-is-linted-even-when-the-adr-exists
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "abc-adr-graph-test"
                      (make-array java.nio.file.attribute.FileAttribute 0)))
        file (clojure.java.io/file dir "0001-test.md")]
    (try
      (spit file (str "# ADR 0001: Test\n\n"
                      "Status: Accepted\n"
                      "Depends on: ADR 6, ADR 60, ADR 0006, ADR 12345678901234567890\n"))
      (let [parsed (adr/parse-adr (.getPath dir) (.getName file))
            existing {:num 6 :title "Existing" :status "Accepted"
                      :supersedes [] :amends [] :amended-by [] :depends-on []}
            problems (adr/lint-adrs [parsed existing] [])]
        ;; Only the syntactically valid token becomes a graph reference.
        (is (= [6] (:depends-on parsed)))
        (is (= #{{:field "Depends on" :token "6"}
                 {:field "Depends on" :token "60"}
                 {:field "Depends on" :token "12345678901234567890"}}
               (set (:malformed-refs parsed))))
        ;; ADR 6 is malformed even though ADR 0006 can exist in a real set.
        (is (some #(str/includes? % "ADR 6 must use exactly four digits") problems))
        (is (some #(str/includes? % "ADR 60 must use exactly four digits") problems))
        (is (some #(str/includes? % "12345678901234567890") problems)))
      (finally
        (java.nio.file.Files/deleteIfExists (.toPath file))
        (java.nio.file.Files/deleteIfExists (.toPath dir))))))
```

- [ ] **Step 2: Run the focused test and verify it fails**

Run:

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.adr-graph-test
```

Expected: FAIL because malformed references are absent and `:malformed-refs` is not populated. The run must not modify committed files.

- [ ] **Step 3: Parse all digit tokens but normalize only valid four-digit tokens**

Replace the current private `adr-refs` helper with:

```clojure
(defn- adr-ref-tokens [s]
  (mapv second (re-seq #"ADR\s+(\d+)" (or s ""))))
```

Then add this helper immediately after the existing `field` definition:

```clojure
(defn- parse-adr-ref-field [header field-name]
  (let [tokens (adr-ref-tokens (field header field-name))]
    {:refs (->> tokens
                (filter #(re-matches #"\d{4}" %))
                (map #(Integer/parseInt %))
                distinct
                vec)
     :malformed-refs (->> tokens
                          (remove #(re-matches #"\d{4}" %))
                          (mapv (fn [token] {:field field-name :token token})))}))
```

In `parse-adr`, parse the four fields once and carry both valid references and malformed-token evidence:

```clojure
(defn parse-adr [dir filename]
  (let [content (slurp (io/file dir filename))
        header (header-block content)
        title (-> (first (str/split-lines content))
                  (str/replace #"^# ADR \d{4}:" "") str/trim)
        field-names ["Supersedes" "Amends" "Amended by" "Depends on"]
        parsed-fields (mapv #(parse-adr-ref-field header %) field-names)
        by-field (zipmap field-names parsed-fields)]
    {:num (adr-number filename) :file filename :title title
     :status (some-> (field header "Status") (str/split #"\s+") first)
     :supersedes (get-in by-field ["Supersedes" :refs])
     :amends (get-in by-field ["Amends" :refs])
     :amended-by (get-in by-field ["Amended by" :refs])
     :depends-on (get-in by-field ["Depends on" :refs])
     :malformed-refs (vec (mapcat :malformed-refs parsed-fields))}))
```

- [ ] **Step 4: Add the malformed-width lint**

Add this clause near the start of `lint-adrs`, before referential-integrity checks:

```clojure
(for [a adrs {:keys [field token]} (:malformed-refs a)]
  (format "ADR %04d %s reference ADR %s must use exactly four digits"
          (:num a) field token))
```

- [ ] **Step 5: Run focused and drift tests**

Run:

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.adr-graph-test
bin/kaocha --focus abc.tools.diagram.registry-test
clojure -M:abc/diagrams --check
nix build .#checks.x86_64-linux.diagram-drift --no-link
```

Expected: all commands exit 0; the current ADR set produces no new lint and neither committed `.mmd` changes.

- [ ] **Step 6: Commit**

```sh
git add abc/src/abc/tools/diagram/adr_graph.clj abc/test/abc/tools/diagram/adr_graph_test.clj
git commit -m "fix(diagram): lint malformed ADR reference widths"
```

### Task 2: Reject sanitized Mermaid node-ID collisions

**Problem:** Distinct graph IDs such as `:a-b` and `:a_b` both render as `a_b`, silently merging nodes.

**Files:**
- Modify: `abc/src/abc/tools/diagram/mermaid.clj`
- Test: `abc/test/abc/tools/diagram/mermaid_test.clj`

**Interfaces:**
- `flowchart` retains its input and output shape.
- On a collision, it throws `ExceptionInfo` with `:collisions`, a map from rendered ID to the original IDs that collided.

- [ ] **Step 1: Write the failing collision test**

```clojure
(deftest flowchart-rejects-id-collisions
  (try
    (mermaid/flowchart {:direction "TD"
                        :nodes [{:id :a-b :label "one"}
                                {:id :a_b :label "two"}]
                        :edges []
                        :class-defs {}})
    (is false "expected sanitized node ids to collide")
    (catch clojure.lang.ExceptionInfo ex
      (is (str/includes? (ex-message ex) "collide"))
      (is (= {"a_b" [:a-b :a_b]} (:collisions (ex-data ex)))))))
```

- [ ] **Step 2: Run the focused test and verify it fails**

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.mermaid-test
```

Expected: FAIL because `flowchart` currently renders without throwing.

- [ ] **Step 3: Add the guard before constructing output lines**

Replace `flowchart` with the complete guarded implementation:

```clojure
(defn flowchart [{:keys [direction nodes edges class-defs]}]
  (let [nodes (vec nodes)
        collisions (->> nodes
                        (map :id)
                        (group-by sanitize-id)
                        (keep (fn [[rendered ids]]
                                (when (< 1 (count ids)) [rendered (vec ids)])))
                        (into {}))]
    (when (seq collisions)
      (throw (ex-info "node ids collide after sanitize-id"
                      {:collisions collisions})))
    (let [dir (or direction "TD")
          node-lines (->> nodes
                          (sort-by (comp sanitize-id :id))
                          (map (fn [{:keys [id label class]}]
                                 (str "  " (sanitize-id id) "[" (quote-label label) "]"
                                      (when class (str ":::" (name class)))))))
          edge-lines (->> edges
                          (sort-by (fn [{:keys [from to label]}]
                                     [(sanitize-id from) (sanitize-id to) (str label)]))
                          (map (fn [{:keys [from to label style]}]
                                 (str "  " (sanitize-id from) (edge-arrow style)
                                      (when label (str "|" (quote-label label) "|"))
                                      " " (sanitize-id to)))))
          class-lines (->> class-defs
                           (sort-by (comp name key))
                           (map (fn [[k v]] (str "  classDef " (name k) " " v))))]
      (str (str/join "\n" (concat [(str "flowchart " dir)]
                                    node-lines edge-lines class-lines))
           "\n"))))
```

- [ ] **Step 4: Run focused and drift tests**

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.mermaid-test
bin/kaocha --focus abc.tools.diagram.registry-test
clojure -M:abc/diagrams --check
nix build .#checks.x86_64-linux.diagram-drift --no-link
```

Expected: all commands exit 0; existing diagrams have no collisions and remain byte-identical.

- [ ] **Step 5: Commit**

```sh
git add abc/src/abc/tools/diagram/mermaid.clj abc/test/abc/tools/diagram/mermaid_test.clj
git commit -m "fix(diagram): reject sanitized node id collisions"
```

### Task 3: Give each mutating core test a scoped temporary path

**Problem:** `core_test` writes a fixed `/tmp/abc-diagram-core-test.mmd`. A namespace-level random path would avoid cross-process collision but would still leak a file and leave mutable state shared across tests.

**Files:**
- Modify: `abc/test/abc/tools/diagram/core_test.clj`

**Interfaces:**
- Replace the `sample` value with `sample [out-path]`, returning the same diagram definition with the supplied path.

- [ ] **Step 1: Parameterize the sample diagram**

```clojure
(defn sample [out-path]
  {:id :t
   :out-path out-path
   :regen "regen-cmd"
   :build (fn [] {:direction "TD"
                  :nodes [{:id :a :label "A"}]
                  :edges []
                  :class-defs {}})
   :lint (fn [] [])})
```

Update rendering-only uses to call `(sample "unused-by-render")`.

- [ ] **Step 2: Scope file creation and deletion to the writing test**

Replace `run-writes-then-check-passes` with:

```clojure
(deftest run-writes-then-check-passes
  (let [file (java.io.File/createTempFile "abc-diagram-core" ".mmd")
        diagram (sample (.getPath file))]
    (try
      (is (:ok? (core/run! [diagram] {:check? false})))
      (let [r (core/run! [diagram] {:check? true})]
        (is (:ok? r))
        (is (= [] (:drifts r))))
      (finally
        (java.nio.file.Files/deleteIfExists (.toPath file))))))
```

Update the lint test to use `(sample "unused-by-lint")`; lint failure returns before any write.

- [ ] **Step 3: Run the focused test twice**

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.core-test
bin/kaocha --focus abc.tools.diagram.core-test
```

Expected: both runs exit 0 and leave no `abc-diagram-core*.mmd` file owned by either run.

- [ ] **Step 4: Commit**

```sh
git add abc/test/abc/tools/diagram/core_test.clj
git commit -m "test(diagram): scope and clean core test output"
```

### Task 4: Validate `:note` as source-only relation metadata

**Decision:** Keep `:note` because the two current values preserve relationship rationale. Do not render it: edge labels remain the stable typed vocabulary, while the prose stays with the hand-authored source record.

**Files:**
- Modify: `abc/src/abc/tools/diagram/adr_graph.clj`
- Modify: `abc/docs/adr/adr-relations.edn`
- Test: `abc/test/abc/tools/diagram/adr_graph_test.clj`

**Interfaces:**
- `lint-adrs` rejects a present `:note` unless it is a string.
- `graph-from` and Mermaid output remain unchanged.

- [ ] **Step 1: Write the failing shape test**

```clojure
(deftest sidecar-note-must-be-a-string
  (let [adrs [{:num 1 :title "A" :status "Accepted" :supersedes []
               :amends [] :amended-by [] :depends-on []}]
        relations [{:from 1 :to 1 :type :extends :note 42}]]
    (is (some #(str/includes? % ":note must be a string")
              (adr/lint-adrs adrs relations)))))
```

- [ ] **Step 2: Run the focused test and verify it fails**

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.adr-graph-test
```

Expected: FAIL because extra relation keys are currently accepted without validating `:note`.

- [ ] **Step 3: Add the shape lint**

Add after the general sidecar shape clause:

```clojure
(for [r relations
      :when (and (contains? r :note) (not (string? (:note r))))]
  (format "adr-relations edge %s->%s :note must be a string"
          (:from r) (:to r)))
```

- [ ] **Step 4: Document the non-rendered status**

Add this line to the header comments in `abc/docs/adr/adr-relations.edn`:

```clojure
;; Optional :note values are source-only human rationale and are not rendered.
```

- [ ] **Step 5: Run focused and drift tests**

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.adr-graph-test
bin/kaocha --focus abc.tools.diagram.registry-test
clojure -M:abc/diagrams --check
nix build .#checks.x86_64-linux.diagram-drift --no-link
```

Expected: all commands exit 0 and committed Mermaid output remains byte-identical.

- [ ] **Step 6: Commit**

```sh
git add abc/src/abc/tools/diagram/adr_graph.clj abc/test/abc/tools/diagram/adr_graph_test.clj abc/docs/adr/adr-relations.edn
git commit -m "feat(diagram): validate source-only relation notes"
```

---

## Batch B — Documentation de-duplication without a new mechanism

### Task 5: Replace the hand-maintained pipeline sketch with a link

**Problem:** `architecture-stages.edn` and the text pipeline in `architecture.md` both enumerate pipeline topology. Marker injection would keep them synchronized, but it would expand `core` from whole-artifact reconciliation into partial document rewriting and create a second generated copy of the Mermaid body.

**Files:**
- Modify: `abc/docs/architecture.md`

**Interfaces:**
- `abc/docs/architecture.mmd` remains the only generated system-architecture diagram.
- `core`, `registry`, and the diagram drift contract do not change.

- [ ] **Step 1: Replace only the text pipeline block**

In `## Pipeline Layers`, replace the fenced `text` block with:

```markdown
The drift-checked pipeline topology is the generated
[system architecture diagram](architecture.mmd), derived from
`architecture-stages.edn` and cross-checked against registered schemas and
ADRs. The diagram is a documentation view, not a competing source of truth.
```

Keep the following paragraphs about invalidation and person-record drift; they explain semantics not represented by graph topology.

- [ ] **Step 2: Verify the target and documentation diff**

Run:

```sh
test -f abc/docs/architecture.mmd
git diff --check -- abc/docs/architecture.md
git diff -- abc/docs/architecture.md
```

Expected: the target exists; `git diff --check` exits 0; the diff removes only the duplicated pipeline enumeration and adds the relative link paragraph.

- [ ] **Step 3: Commit**

```sh
git add abc/docs/architecture.md
git commit -m "docs(architecture): link the generated pipeline diagram"
```

---

## Deferred design work — not executable in this plan

### Stage-to-contract rendering

Do not render schema basenames or click targets yet. ADR 0029 requires stage schemas to be cross-checked, but does not require them to be visible or clickable. More importantly, the current singular `:schema` field cannot honestly describe several stages:

- `:metadata` represents metadata plus person records, not one schema.
- `:tei` represents a TEI publication view; `tei-validation-result.schema.json` is a validation sidecar contract, not the TEI artifact’s profile.
- `:rdf` combines RDF/PROV-O/Linked Art and has no single JSON Schema contract.

Before scheduling this feature, write and approve a small design that defines whether a stage points to a primary artifact schema, a validation contract, or a role-tagged collection such as `:contracts [{:role :artifact :path …}]`. Only then decide what the diagram should render.

### Registration of metadata, TEI-validation, and IIIF schemas

Do not add these files to `SCHEMA_FILES` in this plan. The prerequisites are:

1. Add or deliberately waive top-level semver `version` values. The shared generator rejects all three current schemas because their `version` values are absent.
2. Decide whether all three belong to the shared ABC↔ab-validator contract surface. The current comparator requires both manifests to contain the same schema-ID set, so “per-profile lists” is not an isolated generator change.
3. Preserve the actual monorepo layout: `ab-validator/data/abc-schemas/schemas` is a symlink to `abc/schemas`, so the files are already visible there. The missing copies are in `ab-validator/data/abc-schemas/nix-schemas`.
4. If shared registration is approved, regenerate both manifests independently, run `just sync-schema-mirror`, then run `just schema-drift`. Never copy one `schema-contracts.json` over the other.

Deferral leaves `:schema nil` on the affected architecture stages and does not change current diagram output.

---

## Final Verification

After Tasks 1–5 are complete, run from the monorepo root:

```sh
just validate-migration
```

Also run the focused ABC checks:

```sh
cd abc
bin/kaocha
clojure -M:abc/diagrams --check
nix build .#checks.x86_64-linux.clj-kondo --no-link
nix build .#checks.x86_64-linux.diagram-drift --no-link
```

Expected: every command exits 0, no compiler warnings appear, and `git status --short` shows only the intended committed work plus any pre-existing user changes.

## Self-Review

- **Spec coverage:** ADR reference disappearance, Mermaid ID collisions, temp-file hygiene, inert relation notes, and architecture prose/EDN duplication each have an executable task.
- **Behavioral honesty:** `ADR 6` is rejected as malformed even when ADR 0006 exists; malformed long tokens cannot overflow integer parsing.
- **Boundary preservation:** Tasks 1–5 do not change graph-value interfaces, the edge-type ownership partition, artifact identity, or whole-file drift ownership.
- **No unresolved implementation choices:** Schema association and schema registration are recorded as deferred design work rather than decision placeholders inside executable tasks.
- **Repository facts:** The ab-validator schema directory symlink, isolated Nix mirror, semver requirement, and exact manifest comparison are reflected accurately.

**Recommendation:** approve Tasks 1–5 as two reviewable batches. Keep schema rendering and registration deferred until their contract semantics are designed independently.
