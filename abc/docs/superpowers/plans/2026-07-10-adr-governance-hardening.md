# ADR Governance Hardening Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Repair the current ADR corpus and replace its partial, duplicated checks with one strict Markdown-backed governance validator shared by the decision graph, architecture completeness checks, and Nix gates.

**Architecture:** `abc.tools.adr` becomes the sole parser and policy validator for ADR Markdown; diagram builders consume its structured values instead of parsing headers themselves. The migration makes every current ADR valid before deleting the legacy shell gate and allowlist. Architecture identity ownership becomes per-coordinate and schema-checked, while workflow diagrams reuse the existing JSON Schema and semantic validators before rendering.

**Tech Stack:** Clojure 1.12, `clojure.test`/Kaocha, EDN, Markdown, JSON Schema via `abc.tools.schema`, Mermaid, Nix flakes, and the root `justfile` validation entry points.

## Global Constraints

- Follow the approved design in `docs/superpowers/specs/2026-07-10-adr-governance-hardening-design.md`.
- Run Clojure commands from `abc/` using `bin/kaocha`, `clojure -M:abc/adr-governance`, or `clojure -M:abc/diagrams`; do not invoke ad hoc test aliases.
- Markdown ADR files remain authoritative. Do not add a canonical EDN ADR registry or generate ADR Markdown.
- Preserve ADR 0029's ownership split: headers own `amends`, `supersedes`, and `depends-on`; `adr-relations.edn` owns only header-inexpressible semantic edges.
- Parse invalid input without normalizing it away. Validation must return all problems in one run and must not throw incidental `NumberFormatException` errors.
- Status is exact: `Draft`, `Proposed`, `Accepted`, `Superseded`, or `Withdrawn`. Do not promote ADRs 0026–0029 as a side effect.
- Proposed amendments are pending, not canonically effective; implemented schema behavior and ADR acceptance remain distinct facts.
- Executable evidence is mandatory for Accepted ADRs only. Draft/Proposed evidence paths are optional, but any path they cite must exist.
- Use `[scope: …]` for relation scope. Do not retain parenthetical or free-prose relation syntax.
- Keep generated Mermaid byte-stable and regenerate it only from validated sources.
- Preserve unrelated worktree changes, especially the pre-existing untracked `docs/superpowers/plans/2026-07-10-clojure-quality-remediation.md`.
- Use TDD for every behavior change: focused failing test, minimal implementation, focused passing test, then the relevant drift/Nix check.

## File Structure

- `abc/src/abc/tools/adr.clj` — sole ADR discovery, closed-header parser, relation parser, evidence extractor, and repository policy validator.
- `abc/src/abc/tools/adr_governance.clj` — thin CLI adapter that prints accumulated problem values and owns process exit.
- `abc/test/abc/tools/adr_test.clj` — synthetic parser/policy tests plus the clean-current-repository assertion.
- `abc/src/abc/tools/diagram/adr_graph.clj` — graph construction and semantic-sidecar lint only; consumes `abc.tools.adr` values.
- `abc/test/abc/tools/diagram/adr_graph_test.clj` — graph/sidecar tests after parser tests relocate.
- `abc/docs/adr/README.md`, the affected existing ADR files, and new ADR 0030 — repaired corpus and durable governance decision.
- `abc/deps.edn` — `:abc/adr-governance` CLI alias.
- `abc/flake.nix` — replace `adr-acceptance-criteria` with `adr-governance`.
- Delete `abc/nix/check-acceptance-criteria.sh`, `abc/docs/adr/.acceptance-legacy-allowlist`, and the shell-specific test `abc/test/abc/tools/acceptance_criteria_lint_test.clj` after the Clojure gate is green.
- `abc/docs/architecture-stages.edn` — per-coordinate manifest identity ownership.
- `abc/docs/architecture.md` — complete, marked identity-coordinate list.
- `abc/src/abc/tools/diagram/architecture_graph.clj` and its test — schema/doc/ownership completeness validation.
- `abc/src/abc/tools/diagram/workflow_graph.clj` and its test — schema and semantic validation before rendering.
- Generated: `abc/docs/adr/adr-graph.mmd`, `abc/docs/architecture.mmd`.

---

### Task 1: Introduce the closed ADR parser

**Files:**
- Create: `abc/src/abc/tools/adr.clj`
- Create: `abc/test/abc/tools/adr_test.clj`

**Interfaces:**
- Produces: `adr-files [dir] -> vector<string>` sorted by filename.
- Produces: `parse-adr [dir filename] -> {:num int-or-nil :file string :title string-or-nil :fields map :status string-or-nil :date string-or-nil :accepted string-or-nil :relations map :sections set<string> :section-bodies map :evidence vector<map> :parse-problems vector<problem>}`.
- Produces: `parse-all [dir] -> vector<adr>`.
- Relation items have exact shape `{:target integer :scope string-or-nil}`.
- Problem values have exact base shape `{:kind keyword :file string :message string}` plus `:field`, `:section`, `:value`, or `:path` when applicable.

- [ ] **Step 1: Write closed-header and relation parser tests**

Create `test/abc/tools/adr_test.clj` with a temporary-directory helper and these initial cases:

```clojure
(ns abc.tools.adr-test
  (:require [abc.tools.adr :as adr]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory
            "abc-adr-test" (make-array FileAttribute 0))))

(defn- write-adr! [dir filename body]
  (let [file (io/file dir filename)]
    (spit file body)
    file))

(defn- write-path! [dir path body]
  (let [file (io/file dir path)]
    (.mkdirs (.getParentFile file))
    (spit file body)
    file))

(deftest parses-exact-header-relations-sections-and-evidence
  (let [dir (temp-dir)]
    (write-adr! dir "0002-next.md"
                (str "# ADR 0002: Next\n\n"
                     "Status: Accepted\n"
                     "Date: 2026-07-10\n"
                     "Accepted: 2026-07-10\n"
                     "Supersedes: ADR 0001 [scope: old rule]\n"
                     "Depends on: ADR 0001 [scope: fixture contract]\n"
                     "Source: `docs/spec.md`\n\n"
                     "## Decision\n\nUse the next rule.\n\n"
                     "## Implementation Status\n\nImplemented.\n\n"
                     "## Acceptance Criteria\n\n"
                     "- Covered by `test/abc/tools/adr_test.clj`.\n"))
    (let [parsed (adr/parse-adr (.getPath dir) "0002-next.md")]
      (is (= 2 (:num parsed)))
      (is (= "Accepted" (:status parsed)))
      (is (= [{:target 1 :scope "old rule"}]
             (get-in parsed [:relations :supersedes])))
      (is (= #{{:path "test/abc/tools/adr_test.clj"
                :section "Acceptance Criteria"
                :criterion-index 0}}
             (set (:evidence parsed))))
      (is (= #{"Decision" "Implementation Status" "Acceptance Criteria"}
             (:sections parsed)))
      (is (empty? (:parse-problems parsed))))))

(deftest preserves-decorated-status-and-rejects-continuations
  (let [dir (temp-dir)]
    (write-adr! dir "0028-bad.md"
                (str "# ADR 0028: Bad\n\n"
                     "Status: Proposed (implemented)\n"
                     "continued status prose\n"
                     "Date: 2026-07-10\n\n"
                     "## Decision\n\nProposed.\n"))
    (let [parsed (adr/parse-adr (.getPath dir) "0028-bad.md")]
      (is (= "Proposed (implemented)" (:status parsed)))
      (is (some #(= :invalid-header-line (:kind %))
                (:parse-problems parsed))))))

(deftest rejects-unknown-duplicate-empty-and-multiline-header-fields
  (let [dir (temp-dir)]
    (write-adr! dir "0001-bad.md"
                (str "# ADR 0001: Bad\n\n"
                     "Status: Draft\n"
                     "Staus: Accepted\n"
                     "Status: Proposed\n"
                     "Source:\n"
                     "wrapped source\n\n"
                     "## Decision\n\nBad.\n"))
    (let [kinds (set (map :kind (:parse-problems
                                 (adr/parse-adr (.getPath dir) "0001-bad.md"))))]
      (is (contains? kinds :unknown-header-field))
      (is (contains? kinds :duplicate-header-field))
      (is (contains? kinds :empty-header-value))
      (is (contains? kinds :invalid-header-line)))))

(deftest relation-grammar-is-closed
  (let [dir (temp-dir)]
    (write-adr! dir "0024-bad.md"
                (str "# ADR 0024: Bad relation\n\n"
                     "Status: Accepted\nDate: 2026-07-10\nAccepted: 2026-07-10\n"
                     "Depends on: ADR 0002 (old scope), `docs/probe.md`\n\n"
                     "## Decision\n\nX.\n\n"
                     "## Implementation Status\n\nX.\n\n"
                     "## Acceptance Criteria\n\n- `test/abc/tools/adr_test.clj`.\n"))
    (is (some #(= :invalid-relation-item (:kind %))
              (:parse-problems
               (adr/parse-adr (.getPath dir) "0024-bad.md"))))))
```

- [ ] **Step 2: Run the focused test and verify it fails**

Run from `abc/`:

```sh
bin/kaocha --focus abc.tools.adr-test
```

Expected: FAIL because `abc.tools.adr` does not exist.

- [ ] **Step 3: Implement the parser without policy normalization**

Create `src/abc/tools/adr.clj` with these constants and public functions:

```clojure
(ns abc.tools.adr
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def statuses #{"Draft" "Proposed" "Accepted" "Superseded" "Withdrawn"})
(def header-fields
  #{"Status" "Date" "Accepted" "Supersedes" "Superseded by"
    "Amends" "Amended by" "Depends on" "Source"})
(def relation-fields
  {"Supersedes" :supersedes
   "Superseded by" :superseded-by
   "Amends" :amends
   "Amended by" :amended-by
   "Depends on" :depends-on})
(def evidence-prefixes ["test/" "fixtures/" "nix/"])

(defn problem [kind file message & {:as data}]
  (merge {:kind kind :file file :message message} data))

(defn adr-files [dir]
  (->> (.listFiles (io/file dir))
       (map #(.getName %))
       (filter #(re-matches #"\d{4}-.*\.md" %))
       sort
       vec))

(defn- parse-title [file line]
  (if-let [[_ digits title] (re-matches #"# ADR (\d{4}): (.+)" (or line ""))]
    {:num (Integer/parseInt digits) :title title :problems []}
    {:num nil :title nil
     :problems [(problem :invalid-title file
                         "title must be `# ADR NNNN: Title`")] }))

(defn- relation-item [file field value]
  (if-let [[_ digits scope]
           (re-matches #"ADR (\d{4})(?: \[scope: ([^\]]+)\])?" value)]
    {:value {:target (Integer/parseInt digits) :scope scope} :problems []}
    {:value nil
     :problems [(problem :invalid-relation-item file
                         "relation item must be `ADR NNNN` with optional `[scope: …]`"
                         :field field :value value)]}))

(defn- criterion-bodies [body]
  (->> (str/split-lines (or body ""))
       (reduce (fn [items line]
                 (cond
                   (str/starts-with? line "- ")
                   (conj items (subs line 2))

                   (and (seq items) (re-matches #"\s+.*" line))
                   (update items (dec (count items)) str "\n" (str/trim line))

                   :else items))
               [])))

(defn- evidence [section-bodies]
  (vec
   (for [[idx criterion]
         (map-indexed vector
                      (criterion-bodies
                       (get section-bodies "Acceptance Criteria")))
         token (map second (re-seq #"`([^`]+)`" criterion))
         :when (some #(str/starts-with? token %) evidence-prefixes)]
     {:path token
      :section "Acceptance Criteria"
      :criterion-index idx})))
```

Implement private helpers with the following exact behavior:

- `header-lines` starts after the required blank line following the title and ends at the first blank line after at least one field.
- `parse-fields` accepts only single-line `Field: non-empty value` entries, preserves the first value of a duplicate field, and emits `:unknown-header-field`, `:duplicate-header-field`, `:empty-header-value`, and `:invalid-header-line` problems.
- `parse-relations` splits relation values on `#,\s*`; `Supersedes: none` becomes an empty vector and `none` is invalid in every other relation field.
- `parse-sections` recognizes exact `## Heading` lines and retains each section body for evidence extraction.
- `parse-adr` compares the filename's first four digits to the title number but records mismatch as a parse problem rather than replacing either value.
- `parse-all` calls `parse-adr` once for every `adr-files` result.

Return all parse problems in `:parse-problems`. Do not validate lifecycle or cross-file policy in this task.

- [ ] **Step 4: Add malformed-width, filename/title, and `Supersedes: none` cases**

Append tests proving:

```clojure
(deftest malformed-reference-width-never-normalizes
  (let [dir (temp-dir)]
    (write-adr! dir "0001-bad.md"
                "# ADR 0001: Bad\n\nStatus: Draft\nDate: 2026-07-10\nDepends on: ADR 6\n\n## Decision\n\nX.\n")
    (is (some #(= :invalid-relation-item (:kind %))
              (:parse-problems (adr/parse-adr (.getPath dir) "0001-bad.md"))))))

(deftest filename-title-number-mismatch-is-preserved
  (let [dir (temp-dir)]
    (write-adr! dir "0001-mismatch.md"
                "# ADR 0002: Mismatch\n\nStatus: Draft\nDate: 2026-07-10\n\n## Decision\n\nX.\n")
    (is (some #(= :filename-title-mismatch (:kind %))
              (:parse-problems
               (adr/parse-adr (.getPath dir) "0001-mismatch.md"))))))

(deftest supersedes-none-is-the-only-empty-relation-sentinel
  (let [dir (temp-dir)]
    (write-adr! dir "0001-none.md"
                "# ADR 0001: None\n\nStatus: Draft\nDate: 2026-07-10\nSupersedes: none\n\n## Decision\n\nX.\n")
    (is (= [] (get-in (adr/parse-adr (.getPath dir) "0001-none.md")
                       [:relations :supersedes])))))
```

- [ ] **Step 5: Run focused tests and formatting**

```sh
bin/kaocha --focus abc.tools.adr-test
cljfmt check src/abc/tools/adr.clj test/abc/tools/adr_test.clj
```

Expected: all parser tests PASS; cljfmt exits 0.

- [ ] **Step 6: Commit the parser**

```sh
git add abc/src/abc/tools/adr.clj abc/test/abc/tools/adr_test.clj
git commit -m "feat(adr): add closed Markdown parser"
```

### Task 2: Add lifecycle, relation, dependency, and evidence policy

**Files:**
- Modify: `abc/src/abc/tools/adr.clj`
- Modify: `abc/test/abc/tools/adr_test.clj`
- Create: `abc/src/abc/tools/adr_governance.clj`
- Modify: `abc/deps.edn`

**Interfaces:**
- Produces: `validate-adrs [adrs repo-root] -> vector<problem>`.
- Produces: `validate-repository [repo-root adr-dir] -> vector<problem>`.
- Produces: `abc.tools.adr-governance/-main`, which prints `ADR-LINT <file> <kind>: <message>` to stderr and exits 1 on any problem.
- Later tasks consume the same `parse-all` and relation values; no diagram-specific validation enters `abc.tools.adr`.

- [ ] **Step 1: Write synthetic policy tests**

Add helpers that build ADR maps through real Markdown parsing, then cover the complete policy matrix:

```clojure
(defn- kinds [problems] (set (map :kind problems)))

(defn- accepted-body [num title extra-fields]
  (str "# ADR " (format "%04d" num) ": " title "\n\n"
       "Status: Accepted\nDate: 2026-07-10\nAccepted: 2026-07-10\n"
       extra-fields
       "\n## Decision\n\nDecision.\n\n"
       "## Implementation Status\n\nImplemented.\n\n"
       "## Acceptance Criteria\n\n- `test/evidence.clj`.\n"))

(deftest status-must-be-an-exact-vocabulary-value
  (let [dir (temp-dir)]
    (write-adr! dir "0001-bad.md"
                (str "# ADR 0001: Bad\n\n"
                     "Status: Accepted now\nDate: 2026-07-10\n\n"
                     "## Decision\n\nX.\n"))
    (let [problems (adr/validate-adrs (adr/parse-all (.getPath dir)) dir)]
      (is (= #{:invalid-status} (kinds problems))))))

(deftest accepted-status-requires-date-and-sections
  (let [dir (temp-dir)]
    (write-adr! dir "0001-bad.md"
                (str "# ADR 0001: Bad\n\n"
                     "Status: Accepted\nDate: 2026-07-10\n\n"
                     "## Decision\n\nX.\n"))
    (let [problems (adr/validate-adrs (adr/parse-all (.getPath dir)) dir)]
      (is (contains? (kinds problems) :missing-accepted-date))
      (is (contains? (kinds problems) :missing-implementation-status))
      (is (contains? (kinds problems) :missing-acceptance-criteria)))))

(deftest draft-criteria-need-no-existing-evidence
  (let [dir (temp-dir)]
    (write-adr! dir "0001-draft.md"
                (str "# ADR 0001: Draft\n\nStatus: Draft\nDate: 2026-07-10\n\n"
                     "## Decision\n\nFuture.\n\n"
                     "## Acceptance Criteria\n\n- Future system exists.\n"))
    (is (empty? (adr/validate-adrs (adr/parse-all (.getPath dir)) dir)))))

(deftest accepted-evidence-must-exist
  (let [dir (temp-dir)]
    (write-adr! dir "0001-accepted.md"
                (str "# ADR 0001: Accepted\n\nStatus: Accepted\n"
                     "Date: 2026-07-10\nAccepted: 2026-07-10\n\n"
                     "## Decision\n\nX.\n\n## Implementation Status\n\nDone.\n\n"
                     "## Acceptance Criteria\n\n- `test/missing_test.clj`.\n"))
    (let [problems (adr/validate-adrs (adr/parse-all (.getPath dir)) dir)]
      (is (contains? (kinds problems) :missing-evidence-path)))))

(deftest accepted-to-draft-requires-scope
  (let [dir (temp-dir)]
    (write-path! dir "test/evidence.clj" "(ns evidence)")
    (write-adr! dir "0001-draft.md"
                "# ADR 0001: Draft\n\nStatus: Draft\nDate: 2026-07-10\n\n## Decision\n\nX.\n")
    (write-adr! dir "0002-accepted.md"
                (accepted-body 2 "Accepted" "Depends on: ADR 0001\n"))
    (is (contains? (kinds (adr/validate-adrs
                           (adr/parse-all (.getPath dir)) dir))
                   :unscoped-nonaccepted-dependency))))

(deftest supersession-reciprocity-and-status-are-distinct
  (let [dir (temp-dir)]
    (write-path! dir "test/evidence.clj" "(ns evidence)")
    (write-adr! dir "0001-base.md"
                (accepted-body
                 1 "Base"
                 "Superseded by: ADR 0002 [scope: old rule]\n"))
    (write-adr! dir "0002-next.md"
                (accepted-body
                 2 "Next"
                 "Supersedes: ADR 0001 [scope: old rule]\n"))
    (let [adrs (adr/parse-all (.getPath dir))]
      (is (empty? (adr/validate-adrs adrs dir)))
      (is (contains?
           (kinds (adr/validate-adrs
                   (update-in adrs [0 :relations]
                              dissoc :superseded-by)
                   dir))
           :missing-superseded-by))
      (is (contains?
           (kinds (adr/validate-adrs
                   (-> adrs
                       (assoc-in [0 :relations :superseded-by 0 :scope] nil)
                       (assoc-in [1 :relations :supersedes 0 :scope] nil))
                   dir))
           :unscoped-supersession-target-not-superseded)))
    (write-adr! dir "0003-orphan.md"
                "# ADR 0003: Orphan\n\nStatus: Superseded\nDate: 2026-07-10\n\n## Decision\n\nOld.\n")
    (is (contains?
         (kinds (adr/validate-adrs (adr/parse-all (.getPath dir)) dir))
         :superseded-without-successor))))

(deftest proposed-amendment-is-pending-and-reciprocal
  (let [dir (temp-dir)]
    (write-path! dir "test/evidence.clj" "(ns evidence)")
    (write-adr! dir "0001-base.md"
                (accepted-body 1 "Base" "Amended by: ADR 0002\n"))
    (write-adr! dir "0002-pending.md"
                (str "# ADR 0002: Pending\n\nStatus: Proposed\nDate: 2026-07-10\n"
                     "Amends: ADR 0001\n\n## Decision\n\nProposed amendment.\n"))
    (let [adrs (adr/parse-all (.getPath dir))]
      (is (empty? (adr/validate-adrs adrs dir)))
      (is (contains?
           (kinds (adr/validate-adrs
                   (update-in adrs [0 :relations] dissoc :amended-by)
                   dir))
           :missing-amended-by)))))
```

- [ ] **Step 2: Run the policy tests and verify they fail**

```sh
bin/kaocha --focus abc.tools.adr-test
```

Expected: FAIL because `validate-adrs` is undefined.

- [ ] **Step 3: Implement policy validation as composable problem producers**

Add private functions to `adr.clj` and concatenate their results in this order:

```clojure
(defn validate-adrs [adrs repo-root]
  (vec
   (concat
    (mapcat :parse-problems adrs)
    (duplicate-number-problems adrs)
    (mapcat lifecycle-problems adrs)
    (mapcat required-section-problems adrs)
    (relation-resolution-problems adrs)
    (relation-reciprocity-problems adrs)
    (supersession-status-problems adrs)
    (dependency-status-problems adrs)
    (mapcat #(evidence-problems repo-root %) adrs))))

(defn validate-repository
  ([repo-root] (validate-repository repo-root "docs/adr"))
  ([repo-root adr-dir]
   (validate-adrs (parse-all (str (io/file repo-root adr-dir)))
                  (io/file repo-root))))
```

Implement the helpers with these exact rules:

- `lifecycle-problems`: exact vocabulary; calendar-valid `YYYY-MM-DD`; Accepted requires acceptance date not before Date; non-Accepted forbids it.
- `required-section-problems`: Accepted requires exact headings `Decision`, `Implementation Status`, and `Acceptance Criteria`.
- `relation-resolution-problems`: every target exists; duplicate relation items are rejected.
- `relation-reciprocity-problems`: `Amends`↔`Amended by` and `Supersedes`↔`Superseded by` match target and scope in both directions.
- `supersession-status-problems`: scoped supersession leaves target status alone; unscoped target must be Superseded; every Superseded ADR has an incoming unscoped supersession.
- `dependency-status-problems`: Accepted→Draft/Proposed requires scope; Accepted→Withdrawn/Superseded always fails; other source statuses have no target-status restriction.
- `evidence-problems`: every extracted path must exist; Accepted requires at least one extracted path; a directory counts only if the same Acceptance Criteria bullet also contains an existing `test/` or `nix/` file. Preserve a bullet index in evidence values to implement this check without prose guessing.

Use `java.time.LocalDate/parse` inside a `try` so invalid calendar dates become `:invalid-date` problems.

- [ ] **Step 4: Add the CLI and alias**

Create `src/abc/tools/adr_governance.clj`:

```clojure
(ns abc.tools.adr-governance
  (:require [abc.tools.adr :as adr]))

(defn run! [repo-root]
  (let [problems (adr/validate-repository repo-root)]
    {:ok? (empty? problems) :problems problems}))

(defn -main [& [repo-root]]
  (let [{:keys [ok? problems]} (run! (or repo-root "."))]
    (binding [*out* *err*]
      (doseq [{:keys [file kind message]} problems]
        (println "ADR-LINT" file (str (name kind) ":") message)))
    (when ok? (println "ADR governance valid"))
    (System/exit (if ok? 0 1))))
```

Add to `deps.edn` beside `:abc/diagrams`:

```clojure
:abc/adr-governance {:main-opts ["-m" "abc.tools.adr-governance"]}
```

- [ ] **Step 5: Verify synthetic tests pass and the current corpus fails for known reasons**

```sh
bin/kaocha --focus abc.tools.adr-test
clojure -M:abc/adr-governance 2> /tmp/adr-governance-before.txt
```

Expected: focused tests PASS. The CLI exits 1 and `/tmp/adr-governance-before.txt` includes at least `0028-ruby-annotation-view.md invalid-status`, `0016-edtf-level1-decade-century.md missing-accepted-date`, and missing Implementation Status/evidence problems. This is the required migration red checkpoint.

- [ ] **Step 6: Commit policy and CLI**

```sh
git add abc/src/abc/tools/adr.clj abc/src/abc/tools/adr_governance.clj \
  abc/test/abc/tools/adr_test.clj abc/deps.edn
git commit -m "feat(adr): validate lifecycle relations and evidence"
```

### Task 3: Repair the ADR corpus and move the decision graph onto the shared parser

**Files:**
- Modify: `abc/docs/adr/README.md`
- Modify: `abc/docs/adr/0001-manifest-identity.md`
- Modify: `abc/docs/adr/0006-v0-design-bundle-validation.md`
- Modify: `abc/docs/adr/0007-external-parser-validation-boundary.md`
- Modify: `abc/docs/adr/0008-abc-tools-runtime.md`
- Modify: `abc/docs/adr/0009-imported-output-materialization.md`
- Modify: `abc/docs/adr/0010-manifest-identity-hardening.md`
- Modify: `abc/docs/adr/0011-generated-fixture-policy.md`
- Modify: `abc/docs/adr/0012-tei-odd-schematron-validation.md`
- Modify: `abc/docs/adr/0013-cultural-heritage-lod-profile.md`
- Modify: `abc/docs/adr/0014-iiif-applicability.md`
- Modify: `abc/docs/adr/0016-edtf-level1-decade-century.md`
- Modify: `abc/docs/adr/0017-vocabulary-review.md`
- Modify: `abc/docs/adr/0018-predicate-rename-batch-1.md`
- Modify: `abc/docs/adr/0020-person-identity-drift-data-model.md`
- Modify: `abc/docs/adr/0021-person-identity-drift-harness.md`
- Modify: `abc/docs/adr/0022-upstream-ingest-drift-awareness.md`
- Modify: `abc/docs/adr/0023-owned-aat-parser-ir-mapping.md`
- Modify: `abc/docs/adr/0024-parser-ir-span-and-ruby-direction.md`
- Modify: `abc/docs/adr/0028-ruby-annotation-view.md`
- Modify other ADR headers only where the closed parser reports a wrapped `Source` value.
- Create: `abc/docs/adr/0030-adr-governance-validation.md`
- Modify: `abc/docs/adr/0029-diagrams-as-gated-derived-views.md`
- Modify: `abc/src/abc/tools/diagram/adr_graph.clj`
- Modify: `abc/test/abc/tools/diagram/adr_graph_test.clj`
- Generated: `abc/docs/adr/adr-graph.mmd`

**Interfaces:**
- `adr-graph` re-exports no parser functions. Callers use `abc.tools.adr/parse-all` directly.
- `adr-graph/lint*` combines `abc.tools.adr/validate-adrs` with semantic-sidecar validation.
- Scoped graph-edge labels append ` — <scope>`.

- [ ] **Step 1: Update the ADR policy documentation**

Replace README header examples and acceptance-gate prose with the approved contract:

```markdown
Status: <Draft | Proposed | Accepted | Superseded | Withdrawn>
Date: YYYY-MM-DD
Accepted: YYYY-MM-DD
Supersedes: none | ADR NNNN [scope: non-empty text], ADR MMMM
Superseded by: ADR NNNN [scope: non-empty text], ADR MMMM
Amends: ADR NNNN [scope: non-empty text], ADR MMMM
Amended by: ADR NNNN [scope: non-empty text], ADR MMMM
Depends on: ADR NNNN [scope: non-empty text], ADR MMMM
Source: <one physical line>
```

Document that fields are single-line and closed, relation scopes are bracketed, Accepted ADRs require Implementation Status plus executable evidence, Draft/Proposed criteria are promotion conditions, and the Clojure governance command replaces the allowlist ratchet.

- [ ] **Step 2: Repair lifecycle headers and relation grammar**

Apply these exact migrations:

- ADR 0016: add `Accepted: 2026-04-29`; add an Implementation Status note explaining the repository history establishes Accepted status by import and the 2026-07-09 evidence backfill, while no distinct acceptance date survives, so the decision date is used.
- ADR 0028: change the whole wrapped status field to `Status: Proposed`; keep the implementation detail in its existing Implementation Status.
- ADR 0024: `Depends on: ADR 0002 [scope: source_span_coverage gate]`; move both handoff paths to a one-line `Source:` field.
- ADR 0023: keep only ADR references in `Depends on`; move its two handoff paths to `Source:`.
- ADR 0012: `Supersedes: ADR 0006 [scope: TEI stub language]`.
- ADR 0006: add `Superseded by: ADR 0012 [scope: TEI stub language]`; retain `Status: Accepted` because the supersession is scoped.
- Collapse every wrapped Source field to one physical line without changing its references.

- [ ] **Step 3: Add missing Implementation Status sections**

Add dated current-state summaries immediately after the headers of ADRs 0012, 0013, 0014, 0016, 0018, 0020, 0021, 0022, and 0023. Each summary must name the concrete live schema/tool and its covering test path; do not rewrite Decision, Hard Rule, or Consequences text.

- [ ] **Step 4: Backfill Accepted evidence using existing executable surfaces**

Add at least one code-spanned path to each Accepted allowlisted ADR according to this mapping; retain existing prose criteria and attach the path to the criterion it proves:

| ADR | Required evidence path |
| --- | --- |
| 0001 | `test/abc/tools/materialize_import_test.clj`, `test/abc/tools/jcs_test.clj` |
| 0006 | `test/abc/tools/validate_design_bundle_test.clj` |
| 0007 | `test/abc/tools/validate_design_bundle_test.clj` |
| 0008 | `test/abc/tools/validate_design_bundle_test.clj` |
| 0009 | `test/abc/tools/materialize_import_test.clj` |
| 0010 | `test/abc/tools/materialize_import_test.clj` |
| 0011 | retain `test/abc/tools/materialize_import_test.clj` |
| 0012 | `test/abc/tools/tei_test.clj`, `test/abc/tools/schematron_test.clj` |
| 0013 | `test/abc/tools/linked_art_test.clj` |
| 0014 | `test/abc/tools/iiif_test.clj` |
| 0017 | `test/abc/tools/linked_art_test.clj`, `test/abc/tools/manifest_to_rdf_test.clj` |
| 0018 | `test/abc/tools/metadata_record_test.clj`, `test/abc/tools/shacl_test.clj` |
| 0020 | `test/abc/tools/person_drift_test.clj` |
| 0021 | `test/abc/tools/person_drift_test.clj` |
| 0022 | `test/abc/tools/aozora_history_audit_test.clj` |

Do not add evidence to Draft ADRs 0002–0005 merely to satisfy the old shell rule.

- [ ] **Step 5: Add ADR 0030 in Proposed state**

Create `0030-adr-governance-validation.md` with:

```markdown
# ADR 0030: Uniform ADR Governance Validation

Status: Proposed
Date: 2026-07-10
Supersedes: none
Amends: ADR 0029 [scope: ADR header and decision-graph source validation]
Depends on: ADR 0029 [scope: generated decision graph contract]
Source: `docs/superpowers/specs/2026-07-10-adr-governance-hardening-design.md`

## Implementation Status

Implementation is in progress under the approved governance-hardening plan.

## Context

The committed Mermaid files are byte-current relative to their immediate
sources, but the current gates can still accept decorated statuses, missing
acceptance dates and sections, permanent legacy evidence exceptions, stale
architecture identity metadata, unscoped dependencies on Draft decisions, and
unvalidated workflow-run input. Byte drift is therefore necessary evidence for
a derived view, but not sufficient evidence of lifecycle or semantic validity.

## Decision

ADR Markdown uses the closed header and relation grammar documented in
`docs/adr/README.md`. `abc.tools.adr` is the sole parser and policy validator;
the decision graph consumes its values. Existing ADRs are repaired before the
legacy allowlist is removed. Accepted ADRs require existing executable
evidence, while Draft and Proposed criteria remain promotion conditions.
Amendment and supersession links are reciprocal and relation scopes use
`[scope: …]`. Architecture identity ownership is recorded per schema coordinate
and checked for totality. Runtime workflow diagrams validate their JSON Schema
and semantic invariants before rendering.

## Acceptance Criteria

- `test/abc/tools/adr_test.clj` covers parser and policy failures and proves the
  current repository is clean.
- `test/abc/tools/diagram/adr_graph_test.clj` proves the graph consumes shared
  ADR values and preserves scope labels.
- `test/abc/tools/diagram/architecture_graph_test.clj` proves coordinate and
  owner completeness.
- `test/abc/tools/diagram/workflow_graph_test.clj` proves invalid workflow runs
  cannot be rendered.

## Consequences

- ADR authoring is stricter and malformed metadata fails before diagram output.
- The permanent evidence allowlist and shell-only parser are removed.
- Adding a manifest identity coordinate also requires explicit ADR ownership.
- Proposed implementation remains distinguishable from canonical acceptance.

## Rollback

Reverting the gate requires a new ADR; generated diagrams remain non-identity
documentation views.
```

Add `Amended by: ADR 0030 [scope: ADR header and decision-graph source validation]` to ADR 0029.

- [ ] **Step 6: Move `adr_graph.clj` to shared ADR values**

Require `[abc.tools.adr :as adr]`; delete `adr-files`, title/header/field parsing, `parse-adr`, `parse-all`, status vocabulary, and lifecycle/reference lint from this namespace. Retain sidecar constants and validate only sidecar shape/ownership/references locally.

Change header edge construction to preserve relation scope:

```clojure
(defn header-edges [adrs]
  (for [a adrs
        [type items] [[:amends (get-in a [:relations :amends])]
                      [:supersedes (get-in a [:relations :supersedes])]
                      [:depends-on (get-in a [:relations :depends-on])]]
        {:keys [target scope]} items]
    {:from (:num a) :to target :type type :scope scope}))

(defn- scoped-label [label scope]
  (if scope (str label " — " scope) label))
```

Use `(adr/parse-all adr-dir)` in `build` and `lint*`. `lint*` concatenates `(adr/validate-adrs adrs ".")` with sidecar problems.

Move parser tests out of `diagram/adr_graph_test.clj`; retain graph and sidecar tests, and add one assertion that ADR 0024's dependency edge label contains `source_span_coverage gate`.

- [ ] **Step 7: Add the clean-repository regression test**

Append to `test/abc/tools/adr_test.clj` after the corpus repairs:

```clojure
(deftest current-repository-satisfies-governance
  (is (= [] (adr/validate-repository "."))))
```

Task 2's CLI run is the recorded red checkpoint. This assertion is added only
after the corpus repair and must pass without weakening policy.

- [ ] **Step 8: Run governance red-to-green and regenerate the decision graph**

```sh
clojure -M:abc/adr-governance
bin/kaocha --focus abc.tools.adr-test
bin/kaocha --focus abc.tools.diagram.adr-graph-test
clojure -M:abc/diagrams
clojure -M:abc/diagrams --check
```

Expected: all commands exit 0. The graph contains ADR 0030, the scoped 0012→0006 supersession, and the scoped 0024→0002 dependency.

- [ ] **Step 9: Commit the corpus migration**

```sh
git add abc/docs/adr abc/src/abc/tools/diagram/adr_graph.clj \
  abc/test/abc/tools/adr_test.clj \
  abc/test/abc/tools/diagram/adr_graph_test.clj
git commit -m "docs(adr): repair and uniformly validate decision history"
```

### Task 4: Replace the legacy shell gate with the Clojure governance check

**Files:**
- Modify: `abc/flake.nix`
- Delete: `abc/nix/check-acceptance-criteria.sh`
- Delete: `abc/docs/adr/.acceptance-legacy-allowlist`
- Delete: `abc/test/abc/tools/acceptance_criteria_lint_test.clj`

**Interfaces:**
- Nix check becomes `checks.<system>.adr-governance` in `abc/` and `checks.<system>.abc-adr-governance` at the monorepo root.

- [ ] **Step 1: Replace the flake check**

Replace the `adr-acceptance-criteria` derivation with:

```nix
adr-governance =
  pkgs.runCommand "abc-adr-governance"
    {
      nativeBuildInputs = [ pkgs.clojure ];
    }
    ''
      cp -R ${./.} source
      chmod -R u+w source
      cd source

      export HOME="${cljDepsCache}"
      export JAVA_TOOL_OPTIONS="-Duser.home=${cljDepsCache}"
      export CLJ_CONFIG="$HOME/.clojure"
      export CLJ_CACHE="$TMPDIR/cp-cache"
      export XDG_CONFIG_HOME="$TMPDIR/xdg-config"
      export GITLIBS="$HOME/.gitlibs"

      clojure -M:abc/adr-governance

      mkdir -p "$out"
      echo "ADR lifecycle, relation, dependency, and evidence governance passed." > "$out/result.txt"
    '';
```

- [ ] **Step 2: Delete the obsolete implementation and tests**

Delete the shell script, permanent allowlist, and `acceptance_criteria_lint_test.clj`. Confirm no references remain:

```sh
rg -n "check-acceptance-criteria|acceptance-legacy-allowlist|adr-acceptance-criteria" \
  abc/flake.nix abc/deps.edn abc/src abc/test abc/nix
test ! -e abc/docs/adr/.acceptance-legacy-allowlist
```

Expected: no matches. Historical plans/specs are intentionally outside the searched active paths.

- [ ] **Step 3: Build nested and root checks**

From `abc/`:

```sh
nix build .#checks.x86_64-linux.adr-governance --no-link
```

From the monorepo root:

```sh
nix build .#checks.x86_64-linux.abc-adr-governance --no-link
nix flake check --no-build
```

Expected: both builds and flake evaluation exit 0.

- [ ] **Step 4: Commit the gate replacement**

```sh
git add abc/flake.nix abc/nix/check-acceptance-criteria.sh \
  abc/docs/adr/.acceptance-legacy-allowlist \
  abc/test/abc/tools/acceptance_criteria_lint_test.clj
git commit -m "build(adr): replace legacy acceptance allowlist gate"
```

### Task 5: Make architecture identity attribution complete and checkable

**Files:**
- Modify: `abc/docs/architecture-stages.edn`
- Modify: `abc/docs/architecture.md`
- Modify: `abc/src/abc/tools/diagram/architecture_graph.clj`
- Modify: `abc/test/abc/tools/diagram/architecture_graph_test.clj`
- Generated: `abc/docs/architecture.mmd`

**Interfaces:**
- Produces: `manifest-identity-required [manifest-schema] -> set<string>`.
- Produces: `documented-identity-coordinates [architecture-markdown] -> set<string>`.
- `validate` accepts the whole stages document plus schema paths, ADR numbers, manifest schema, and architecture Markdown.

- [ ] **Step 1: Add failing completeness tests**

Extend `architecture_graph_test.clj`:

```clojure
(deftest manifest-identity-coordinate-ownership-is-total
  (let [doc (arch/load-stages)
        required (arch/manifest-identity-required
                  (json/read-json-file "schemas/manifest.schema.json"))]
    (is (= required
           (set (keys (get-in doc [:manifest-identity-contract :coordinates])))))
    (is (= #{1 10 23 27 28}
           (set (mapcat val
                        (get-in doc [:manifest-identity-contract :coordinates])))))))

(deftest architecture-prose-lists-the-live-identity-contract
  (is (= (arch/manifest-identity-required
          (json/read-json-file "schemas/manifest.schema.json"))
         (arch/documented-identity-coordinates
          (slurp "docs/architecture.md")))))
```

Require `abc.tools.json` in the test.

- [ ] **Step 2: Run the focused test and verify it fails**

```sh
bin/kaocha --focus abc.tools.diagram.architecture-graph-test
```

Expected: FAIL because ownership and the two missing coordinates are absent.

- [ ] **Step 3: Add per-coordinate ownership and documentation markers**

Add the exact `:manifest-identity-contract` value from the approved design to `architecture-stages.edn`, and change the manifest stage to:

```clojure
{:id :manifest :label "Artifact manifest (identity)"
 :schema "schemas/manifest.schema.json" :adr [1 10 23 27 28]
 :inputs [:parser-ir :metadata]}
```

In `architecture.md`, add `tokenizer_profile_hash` and `annotation_policy_hash`, surrounding the code block with:

````markdown
<!-- manifest-identity-coordinates:start -->
```text
manifest_schema_hash
corpus_snapshot_hash
work_content_hash
metadata_record_hash
parser_build_hash
parser_config_hash
aat_parser_ir_mapping_hash
parser_ir_schema_hash
tei_profile_hash
tokenizer_build_hash
tokenizer_dictionary_hash
tokenizer_profile_hash
analysis_recipe_hash
annotation_policy_hash
output_format_spec_hash
```
<!-- manifest-identity-coordinates:end -->
````

- [ ] **Step 4: Implement completeness validation**

In `architecture_graph.clj`, require `clojure.set` and add:

```clojure
(defn manifest-identity-required [schema]
  (set (get-in schema ["$defs" "identityObject" "required"])))

(defn documented-identity-coordinates [markdown]
  (let [[_ body]
        (re-find #"(?s)<!-- manifest-identity-coordinates:start -->\s*```text\s*(.*?)\s*```\s*<!-- manifest-identity-coordinates:end -->"
                 markdown)]
    (if body
      (->> (str/split-lines body) (remove str/blank?) set)
      #{})))

(defn identity-contract-problems [doc manifest-schema architecture-markdown]
  (let [required (manifest-identity-required manifest-schema)
        coordinates (get-in doc [:manifest-identity-contract :coordinates])
        attributed (set (keys coordinates))
        documented (documented-identity-coordinates architecture-markdown)
        owners (set (mapcat val coordinates))
        manifest-adrs (->> (:stages doc)
                           (filter #(= :manifest (:id %)))
                           first :adr set)]
    (cond-> []
      (not= required attributed)
      (conj (format "manifest identity attribution mismatch: missing=%s extra=%s"
                    (sort (set/difference required attributed))
                    (sort (set/difference attributed required))))
      (not= required documented)
      (conj (format "architecture identity list mismatch: missing=%s extra=%s"
                    (sort (set/difference required documented))
                    (sort (set/difference documented required))))
      (not= owners manifest-adrs)
      (conj (format "manifest stage ADR owners mismatch: expected=%s actual=%s"
                    (sort owners) (sort manifest-adrs))))))
```

Extend `lint*` to concatenate existing stage validation with `identity-contract-problems`. Use `abc.tools.adr/parse-all` for ADR numbers rather than reaching through `adr-graph`.

- [ ] **Step 5: Regenerate and verify architecture diagrams**

```sh
bin/kaocha --focus abc.tools.diagram.architecture-graph-test
bin/kaocha --focus abc.tools.diagram.registry-test
clojure -M:abc/diagrams
clojure -M:abc/diagrams --check
nix build .#checks.x86_64-linux.diagram-drift --no-link
```

Expected: all commands exit 0; the manifest node lists ADRs 0001, 0010, 0023, 0027, and 0028.

- [ ] **Step 6: Commit architecture completeness**

```sh
git add abc/docs/architecture-stages.edn abc/docs/architecture.md \
  abc/docs/architecture.mmd abc/src/abc/tools/diagram/architecture_graph.clj \
  abc/test/abc/tools/diagram/architecture_graph_test.clj
git commit -m "feat(diagram): verify manifest identity ownership"
```

### Task 6: Validate workflow reports before diagram rendering

**Files:**
- Modify: `abc/src/abc/tools/diagram/workflow_graph.clj`
- Modify: `abc/test/abc/tools/diagram/workflow_graph_test.clj`

**Interfaces:**
- Produces: `validation-problems [run] -> vector<map>` combining JSON Schema and workflow semantic errors.
- `run->graph` throws `ExceptionInfo` with `{:errors vector}` when validation fails.
- Rendering of valid runs remains byte-deterministic.

- [ ] **Step 1: Add failing schema and semantic rejection tests**

Add to `workflow_graph_test.clj`:

```clojure
(deftest rejects-schema-invalid-workflow-before-rendering
  (let [run (assoc (json/read-json-file
                    "examples/workflow/passed.workflow-run.json")
                   "status" "mystery")]
    (try
      (wf/run->mermaid run)
      (is false "expected schema-invalid workflow to be rejected")
      (catch clojure.lang.ExceptionInfo ex
        (is (= :workflow-run-invalid (:kind (ex-data ex))))
        (is (seq (:errors (ex-data ex))))))))

(deftest rejects-semantically-invalid-workflow-before-rendering
  (let [run (assoc (json/read-json-file
                    "examples/workflow/passed.workflow-run.json")
                   "step_count" 999)]
    (try
      (wf/run->graph run)
      (is false "expected semantic-invalid workflow to be rejected")
      (catch clojure.lang.ExceptionInfo ex
        (is (some #(= ["step_count"] (:path %))
                  (:errors (ex-data ex))))))))
```

- [ ] **Step 2: Run the focused test and verify it fails**

```sh
bin/kaocha --focus abc.tools.diagram.workflow-graph-test
```

Expected: FAIL because unknown status is rendered as skipped and semantic counters are unchecked.

- [ ] **Step 3: Reuse schema and semantic validators**

Update namespace requirements and add:

```clojure
(:require [abc.tools.diagram.mermaid :as mermaid]
          [abc.tools.json :as json]
          [abc.tools.schema :as schema]
          [abc.tools.workflow :as workflow])

(def workflow-run-schema-path "schemas/workflow-run.schema.json")

(defn validation-problems [run]
  (let [schema-errors
        (or (schema/validation-errors
             (json/read-json-file workflow-run-schema-path) run)
            [])]
    (vec (concat schema-errors
                 (when (empty? schema-errors)
                   (workflow/validate-run run))))))

(defn- validate-run! [run]
  (when-let [errors (seq (validation-problems run))]
    (throw (ex-info "workflow-run is invalid"
                    {:kind :workflow-run-invalid
                     :errors (vec errors)})))
  run)
```

Call `(validate-run! run)` at the start of `run->graph`. Replace the unknown-status fallback with direct lookup because schema validation now guarantees membership:

```clojure
:class (get status-class (get s "status"))
```

Keep error presentation in the CLI adapter rather than the graph builder:

```clojure
(defn render-path [path]
  (run->mermaid (json/read-json-file path)))

(defn -main [path & _]
  (if-not path
    (do
      (binding [*out* *err*]
        (println "usage: clojure -M:abc/workflow-graph <workflow-run.json>"))
      (System/exit 2))
    (try
      (print (render-path path))
      (flush)
      (catch clojure.lang.ExceptionInfo ex
        (binding [*out* *err*]
          (println "workflow-run validation failed:" path)
          (doseq [error (:errors (ex-data ex))]
            (println (pr-str error))))
        (System/exit 1)))))
```

Replace the producer/consumer test's shape-only value with this schema- and semantics-valid value:

```clojure
(def valid-two-step-run
  {"schema_id" "https://w3id.org/abc/schemas/workflow-run.schema.json"
   "schema_version" "soranoha-workflow-run-v1"
   "workflow_id" "diagram-test"
   "run_id" "diagram-test-1"
   "status" "passed"
   "started_at" "2026-07-10T00:00:00Z"
   "ended_at" "2026-07-10T00:00:02Z"
   "duration_ms" 2000
   "step_count" 2
   "steps_passed" 2
   "steps_failed" 0
   "steps"
   [{"id" "A" "status" "passed"
     "started_at" "2026-07-10T00:00:00Z"
     "ended_at" "2026-07-10T00:00:01Z"
     "duration_ms" 1000
     "requires" [] "produces" ["x"]
     "inputs" [] "outputs" [] "messages" []}
    {"id" "B" "status" "passed"
     "started_at" "2026-07-10T00:00:01Z"
     "ended_at" "2026-07-10T00:00:02Z"
     "duration_ms" 1000
     "requires" ["x"] "produces" []
     "inputs" [] "outputs" [] "messages" []}]})

(deftest builds-producer-consumer-edges
  (is (some (fn [edge]
              (and (= "A" (:from edge)) (= "B" (:to edge))))
            (:edges (wf/run->graph valid-two-step-run)))))
```

- [ ] **Step 4: Run focused and full workflow tests**

```sh
bin/kaocha --focus abc.tools.diagram.workflow-graph-test
bin/kaocha --focus abc.tools.workflow-test
clojure -M:abc/workflow-graph examples/workflow/passed.workflow-run.json > /tmp/workflow.mmd
rg -n "classDef passed|source_snapshot" /tmp/workflow.mmd
```

Expected: both test groups PASS; CLI exits 0 and the generated Mermaid contains the expected node/class text.

- [ ] **Step 5: Commit workflow validation**

```sh
git add abc/src/abc/tools/diagram/workflow_graph.clj \
  abc/test/abc/tools/diagram/workflow_graph_test.clj
git commit -m "fix(diagram): validate workflow reports before rendering"
```

### Task 7: Accept ADR 0030 and run the complete migration gate

**Files:**
- Modify: `abc/docs/adr/0030-adr-governance-validation.md`
- Modify: `abc/docs/adr/adr-graph.mmd`

**Interfaces:**
- ADR 0030 becomes the Accepted canonical contract only after every acceptance path exists and all gates pass.

- [ ] **Step 1: Run the pre-acceptance verification matrix**

From `abc/`:

```sh
clojure -M:abc/adr-governance
bin/kaocha --focus abc.tools.adr-test
bin/kaocha --focus abc.tools.diagram.adr-graph-test
bin/kaocha --focus abc.tools.diagram.architecture-graph-test
bin/kaocha --focus abc.tools.diagram.workflow-graph-test
bin/kaocha --focus abc.tools.diagram.registry-test
clojure -M:abc/diagrams --check
nix build .#checks.x86_64-linux.adr-governance --no-link
nix build .#checks.x86_64-linux.diagram-drift --no-link
nix build .#checks.x86_64-linux.clj-kondo --no-link
nix build .#checks.x86_64-linux.clj-nix-focused-tests --no-link
```

Expected: every command exits 0. Do not change ADR 0030 status if any command fails.

- [ ] **Step 2: Promote ADR 0030 using the verified evidence**

Change its header and implementation status:

```markdown
Status: Accepted
Date: 2026-07-10
Accepted: 2026-07-10
```

```markdown
## Implementation Status

Accepted on 2026-07-10 after the shared ADR parser, uniform corpus migration,
Nix governance gate, per-coordinate architecture validation, and workflow-run
validation passed the Acceptance Criteria below.
```

- [ ] **Step 3: Regenerate and re-run status-sensitive checks**

```sh
clojure -M:abc/diagrams
clojure -M:abc/adr-governance
clojure -M:abc/diagrams --check
bin/kaocha --focus abc.tools.adr-test
bin/kaocha --focus abc.tools.diagram.registry-test
```

Expected: all commands exit 0; ADR 0030 is rendered with the Accepted class.

- [ ] **Step 4: Run monorepo verification from the root**

```sh
nix build .#checks.x86_64-linux.abc-adr-governance --no-link
nix build .#checks.x86_64-linux.abc-diagram-drift --no-link
just validate-migration
```

Expected: all commands exit 0. If `just validate-migration` only evaluates builds by policy, retain the explicit Nix builds above as execution evidence.

- [ ] **Step 5: Confirm the worktree contains no accidental artifacts**

```sh
git status --short
git diff --check
```

Expected: only the intended ADR 0030 status/graph edits are pending; `/tmp` outputs and the pre-existing untracked Clojure remediation plan are not staged.

- [ ] **Step 6: Commit acceptance**

```sh
git add abc/docs/adr/0030-adr-governance-validation.md \
  abc/docs/adr/adr-graph.mmd
git commit -m "docs(adr): accept uniform governance validation"
```

## Final Review Checklist

- [ ] `abc.tools.adr` is the only ADR Markdown parser; `rg -n "parse-adr|header-block|adr-ref-tokens" abc/src` finds no parallel parser in diagram code.
- [ ] No permanent allowlist or shell acceptance gate remains.
- [ ] All 30 ADRs pass exact lifecycle/header/relation validation.
- [ ] Draft ADRs 0002–0005 retain honest promotion criteria without fabricated evidence.
- [ ] ADR 0028 is exactly Proposed; ADRs 0026–0029 were not promoted.
- [ ] Scoped supersession and dependency labels remain visible in `adr-graph.mmd`.
- [ ] Architecture coordinate keys equal all 15 required manifest identity fields, and stage ADR ownership is derived from those keys.
- [ ] Invalid workflow runs fail before Mermaid rendering.
- [ ] Focused tests, full Clojure tests, formatting/lint, Nix gates, and `just validate-migration` have fresh passing output.
