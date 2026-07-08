# Workflow Orchestration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a small workflow plan/run-report layer that makes Soranoha orchestration inspectable, then migrate publication rehearsal without changing artifact identity or output semantics.

**Architecture:** Start with a language-neutral `workflow-run.schema.json` and a dependency-free Clojure serial runner in `abc.tools.workflow`. Convert `publication-rehearsal!` first because it is an existing finite DAG with good tests and clear outputs. After the Clojure runner proves useful, reuse the same run-report shape in `build-publication!`, publication batch materialization, and one Bash report script.

**Tech Stack:** Clojure, JSON Schema 2020-12 via existing `abc.tools.schema`, deterministic JSON via `abc.tools.manifest`, existing Soranoha CLI tests, Bash helper in a later task, no new external dependency in the first slice.

## Global Constraints

- Workflow reports are operational provenance only; manifests, request sets, content hashes, and snapshot indexes remain canonical identity.
- Do not add Nodely, Pathom, or `core.async.flow` in this implementation plan.
- Preserve existing command outputs and printed high-value paths for `publication-rehearsal!`.
- The first runner is serial and topological; concurrency remains in existing batch code until a later slice.
- Write `workflow-plan.json` before executing steps and refresh `workflow-run.json` after each completed or failed step.
- Path values in workflow reports are relative to the workflow output root when possible.
- Keep workflow JSON bounded; do not emit per-token, per-row, or full stack-trace payloads.

---

## File Structure

- Create `abc/schemas/workflow-run.schema.json`: cross-language workflow run report schema.
- Create `abc/examples/workflow/passed.workflow-run.json`: valid schema fixture.
- Create `abc/fixtures/v0/invalid/workflow-run/invalid-status.workflow-run.json`: invalid schema fixture.
- Create `abc/src/abc/tools/workflow.clj`: serial Clojure workflow planner/runner.
- Create `abc/test/abc/tools/workflow_test.clj`: runner unit tests and schema fixture tests.
- Modify `abc/src/abc/tools/validate_design_bundle.clj`: validate workflow schema fixtures.
- Modify `abc/src/abc/tools/soranoha.clj`: migrate `publication-rehearsal!` to the runner.
- Modify `abc/test/abc/tools/soranoha_test.clj`: assert workflow reports are emitted by rehearsal/build publication paths.
- Later task creates `scripts/workflow-run-lib.sh`: Bash event helper.
- Later task modifies `ab-validator/reports/morph-warehouse/build-report.sh`: first non-Clojure workflow report adopter.

---

### Task 1: Workflow Run Schema and Fixtures

**Files:**
- Create: `abc/schemas/workflow-run.schema.json`
- Create: `abc/examples/workflow/passed.workflow-run.json`
- Create: `abc/fixtures/v0/invalid/workflow-run/invalid-status.workflow-run.json`
- Modify: `abc/test/abc/tools/workflow_test.clj`
- Modify: `abc/src/abc/tools/validate_design_bundle.clj`

**Interfaces:**
- Produces schema ID: `https://w3id.org/abc/schemas/workflow-run.schema.json`.
- Produces fixture path used by Task 2 tests: `abc/examples/workflow/passed.workflow-run.json`.

- [ ] **Step 1: Write failing schema fixture tests**

Create `abc/test/abc/tools/workflow_test.clj`:

```clojure
(ns abc.tools.workflow-test
  (:require [abc.tools.files :as files]
            [abc.tools.schema :as schema]
            [clojure.test :refer [deftest is testing]]))

(def workflow-run-schema-path
  "schemas/workflow-run.schema.json")

(deftest workflow-run-schema-fixtures-test
  (let [schema-value (files/read-json workflow-run-schema-path)]
    (testing "valid workflow run fixture passes"
      (is (nil? (schema/validation-errors
                 schema-value
                 (files/read-json "examples/workflow/passed.workflow-run.json")))))
    (testing "invalid workflow status is rejected"
      (is (seq (schema/validation-errors
                schema-value
                (files/read-json
                 "fixtures/v0/invalid/workflow-run/invalid-status.workflow-run.json")))))))
```

- [ ] **Step 2: Run the focused test to verify red**

Run:

```bash
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: failure because `schemas/workflow-run.schema.json` does not exist.

- [ ] **Step 3: Add the workflow run schema**

Create `abc/schemas/workflow-run.schema.json`:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://w3id.org/abc/schemas/workflow-run.schema.json",
  "type": "object",
  "additionalProperties": false,
  "required": [
    "schema_id",
    "schema_version",
    "workflow_id",
    "run_id",
    "status",
    "started_at",
    "ended_at",
    "duration_ms",
    "step_count",
    "steps_passed",
    "steps_failed",
    "steps"
  ],
  "properties": {
    "schema_id": {
      "const": "https://w3id.org/abc/schemas/workflow-run.schema.json"
    },
    "schema_version": {"const": "soranoha-workflow-run-v1"},
    "workflow_id": {"type": "string", "minLength": 1},
    "run_id": {"type": "string", "minLength": 1},
    "status": {"enum": ["passed", "failed", "partial", "skipped"]},
    "started_at": {"type": "string", "format": "date-time"},
    "ended_at": {"type": "string", "format": "date-time"},
    "duration_ms": {"type": "integer", "minimum": 0},
    "step_count": {"type": "integer", "minimum": 0},
    "steps_passed": {"type": "integer", "minimum": 0},
    "steps_failed": {"type": "integer", "minimum": 0},
    "steps": {
      "type": "array",
      "items": {"$ref": "#/$defs/step"}
    }
  },
  "$defs": {
    "hash": {
      "type": "string",
      "pattern": "^sha256:[0-9a-f]{64}$"
    },
    "pathRecord": {
      "type": "object",
      "additionalProperties": false,
      "required": ["role"],
      "properties": {
        "role": {"type": "string", "minLength": 1},
        "path": {"type": "string", "minLength": 1},
        "content_hash": {"$ref": "#/$defs/hash"},
        "artifact_id": {"$ref": "#/$defs/hash"},
        "request_set_id": {"$ref": "#/$defs/hash"},
        "snapshot_identity_hash": {"$ref": "#/$defs/hash"}
      }
    },
    "message": {
      "type": "object",
      "additionalProperties": false,
      "required": ["level", "message"],
      "properties": {
        "level": {"enum": ["info", "warn", "error"]},
        "message": {"type": "string", "minLength": 1},
        "data": {"type": "object"}
      }
    },
    "error": {
      "type": "object",
      "additionalProperties": false,
      "required": ["error_class", "message"],
      "properties": {
        "error_class": {"type": "string", "minLength": 1},
        "message": {"type": "string", "minLength": 1},
        "data": {"type": "object"}
      }
    },
    "step": {
      "type": "object",
      "additionalProperties": false,
      "required": [
        "id",
        "status",
        "started_at",
        "ended_at",
        "duration_ms",
        "requires",
        "produces",
        "inputs",
        "outputs",
        "messages"
      ],
      "properties": {
        "id": {"type": "string", "minLength": 1},
        "status": {"enum": ["passed", "failed", "partial", "skipped"]},
        "started_at": {"type": "string", "format": "date-time"},
        "ended_at": {"type": "string", "format": "date-time"},
        "duration_ms": {"type": "integer", "minimum": 0},
        "requires": {
          "type": "array",
          "items": {"type": "string", "minLength": 1}
        },
        "produces": {
          "type": "array",
          "items": {"type": "string", "minLength": 1}
        },
        "inputs": {
          "type": "array",
          "items": {"$ref": "#/$defs/pathRecord"}
        },
        "outputs": {
          "type": "array",
          "items": {"$ref": "#/$defs/pathRecord"}
        },
        "messages": {
          "type": "array",
          "items": {"$ref": "#/$defs/message"}
        },
        "error": {"$ref": "#/$defs/error"}
      }
    }
  }
}
```

- [ ] **Step 4: Add fixtures**

Create `abc/examples/workflow/passed.workflow-run.json`:

```json
{
  "schema_id": "https://w3id.org/abc/schemas/workflow-run.schema.json",
  "schema_version": "soranoha-workflow-run-v1",
  "workflow_id": "fixture.workflow.v1",
  "run_id": "fixture-run",
  "status": "passed",
  "started_at": "2026-07-08T00:00:00Z",
  "ended_at": "2026-07-08T00:00:01Z",
  "duration_ms": 1000,
  "step_count": 1,
  "steps_passed": 1,
  "steps_failed": 0,
  "steps": [
    {
      "id": "fixture-step",
      "status": "passed",
      "started_at": "2026-07-08T00:00:00Z",
      "ended_at": "2026-07-08T00:00:01Z",
      "duration_ms": 1000,
      "requires": ["input"],
      "produces": ["output"],
      "inputs": [{"role": "input", "path": "input.json"}],
      "outputs": [
        {
          "role": "output",
          "path": "output.json",
          "content_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000"
        }
      ],
      "messages": [{"level": "info", "message": "fixture step passed"}]
    }
  ]
}
```

Create `abc/fixtures/v0/invalid/workflow-run/invalid-status.workflow-run.json`:

```json
{
  "schema_id": "https://w3id.org/abc/schemas/workflow-run.schema.json",
  "schema_version": "soranoha-workflow-run-v1",
  "workflow_id": "fixture.workflow.v1",
  "run_id": "fixture-run",
  "status": "done",
  "started_at": "2026-07-08T00:00:00Z",
  "ended_at": "2026-07-08T00:00:01Z",
  "duration_ms": 1000,
  "step_count": 1,
  "steps_passed": 1,
  "steps_failed": 0,
  "steps": [
    {
      "id": "fixture-step",
      "status": "passed",
      "started_at": "2026-07-08T00:00:00Z",
      "ended_at": "2026-07-08T00:00:01Z",
      "duration_ms": 1000,
      "requires": ["input"],
      "produces": ["output"],
      "inputs": [{"role": "input", "path": "input.json"}],
      "outputs": [
        {
          "role": "output",
          "path": "output.json",
          "content_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000"
        }
      ],
      "messages": [{"level": "info", "message": "fixture step passed"}]
    }
  ]
}
```

- [ ] **Step 5: Include workflow schema in design-bundle validation**

Modify `abc/src/abc/tools/validate_design_bundle.clj` inside
`validate-json-schemas!` to read the workflow schema and validate the valid
fixture:

```clojure
(let [workflow-run-schema (files/read-json "schemas/workflow-run.schema.json")]
  (validate-json! workflow-run-schema
                  "examples/workflow/passed.workflow-run.json"))
```

Do not add the invalid fixture to generic schema validation; it is asserted in
`workflow_test.clj`.

- [ ] **Step 6: Verify green**

Run:

```bash
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: pass.

- [ ] **Step 7: Commit**

```bash
git add abc/schemas/workflow-run.schema.json \
        abc/examples/workflow/passed.workflow-run.json \
        abc/fixtures/v0/invalid/workflow-run/invalid-status.workflow-run.json \
        abc/test/abc/tools/workflow_test.clj \
        abc/src/abc/tools/validate_design_bundle.clj
git commit -m "feat(abc): add workflow run schema"
```

---

### Task 2: Serial Clojure Workflow Runner

**Files:**
- Create: `abc/src/abc/tools/workflow.clj`
- Modify: `abc/test/abc/tools/workflow_test.clj`

**Interfaces:**
- Produces: `abc.tools.workflow/validate-plan! [plan initial-state] -> plan`.
- Produces: `abc.tools.workflow/run-workflow! [opts] -> {:state map :run map :plan map}`.
- Consumes: step functions shaped as `(fn [state] result-map)`.

- [ ] **Step 1: Add failing runner tests**

First update the `ns` form in `abc/test/abc/tools/workflow_test.clj` so it has
these requires:

```clojure
(ns abc.tools.workflow-test
  (:require [abc.tools.files :as files]
            [abc.tools.schema :as schema]
            [abc.tools.workflow :as workflow]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))
```

Then append these tests below `workflow-run-schema-fixtures-test`:

```clojure

(defn- temp-dir [prefix]
  (.toFile (java.nio.file.Files/createTempDirectory
            prefix
            (make-array java.nio.file.attribute.FileAttribute 0))))

(deftest run-workflow-executes-topologically-and-writes-reports-test
  (let [out (temp-dir "abc-workflow")
        calls (atom [])
        clock-values (atom ["2026-07-08T00:00:00Z"
                            "2026-07-08T00:00:01Z"
                            "2026-07-08T00:00:02Z"
                            "2026-07-08T00:00:03Z"
                            "2026-07-08T00:00:04Z"
                            "2026-07-08T00:00:05Z"])
        clock (fn []
                (let [value (first @clock-values)]
                  (swap! clock-values rest)
                  value))
        result (workflow/run-workflow!
                {:workflow-id "fixture.workflow.v1"
                 :run-id "fixture-run"
                 :output-root out
                 :clock clock
                 :initial-state {:a 1}
                 :steps [{:id :first
                          :requires [:a]
                          :produces [:b]
                          :run (fn [state]
                                 (swap! calls conj :first)
                                 {:state-updates {:b (inc (:a state))}
                                  :outputs [{:role "b"
                                             :path "b.json"
                                             :content_hash
                                             "sha256:1111111111111111111111111111111111111111111111111111111111111111"
                                             :request_set_id
                                             "sha256:2222222222222222222222222222222222222222222222222222222222222222"}]})}
                         {:id :second
                          :requires [:b]
                          :produces [:c]
                          :run (fn [state]
                                 (swap! calls conj :second)
                                 {:state-updates {:c (* 2 (:b state))}
                                  :messages [{:level "info"
                                              :message "computed c"}]})}]})]
    (is (= [:first :second] @calls))
    (is (= {:a 1 :b 2 :c 4} (:state result)))
    (is (.isFile (io/file out "workflow-plan.json")))
    (is (.isFile (io/file out "workflow-run.json")))
    (let [run (files/read-json (io/file out "workflow-run.json"))
          schema-value (files/read-json workflow-run-schema-path)]
      (is (nil? (schema/validation-errors schema-value run)))
      (is (= "passed" (get run "status")))
      (is (= 2 (get run "step_count")))
      (is (= 2 (get run "steps_passed")))
      (is (= 0 (get run "steps_failed")))
      (is (= "sha256:1111111111111111111111111111111111111111111111111111111111111111"
             (get-in run ["steps" 0 "outputs" 0 "content_hash"])))
      (is (= "sha256:2222222222222222222222222222222222222222222222222222222222222222"
             (get-in run ["steps" 0 "outputs" 0 "request_set_id"]))))))

(deftest validate-plan-rejects-missing-dependency-before-running-test
  (let [calls (atom [])]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"missing workflow dependencies"
         (workflow/run-workflow!
          {:workflow-id "bad.workflow.v1"
           :run-id "bad-run"
           :output-root (temp-dir "abc-workflow-bad")
           :initial-state {}
           :steps [{:id :bad
                    :requires [:missing]
                    :produces [:out]
                    :run (fn [_]
                           (swap! calls conj :bad)
                           {:state-updates {:out true}})}]})))
    (is (= [] @calls))))
```

- [ ] **Step 2: Run tests to verify red**

Run:

```bash
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: failure because `abc.tools.workflow` does not exist.

- [ ] **Step 3: Implement plan validation and runner**

Create `abc/src/abc/tools/workflow.clj`:

```clojure
(ns abc.tools.workflow
  (:require [abc.tools.manifest :as manifest]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def workflow-run-schema-id
  "https://w3id.org/abc/schemas/workflow-run.schema.json")

(defn- key-name [k]
  (if (keyword? k) (name k) (str k)))

(defn- now-utc []
  (str (java.time.Instant/now)))

(defn- duration-ms [start end]
  (let [s (java.time.Instant/parse start)
        e (java.time.Instant/parse end)]
    (.toMillis (java.time.Duration/between s e))))

(defn- duplicate-values [xs]
  (->> xs frequencies (filter (fn [[_ n]] (> n 1))) (map first) vec))

(defn validate-plan! [{:keys [steps] :as plan} initial-state]
  (let [ids (map :id steps)
        duplicate-ids (duplicate-values ids)
        produced (mapcat :produces steps)
        duplicate-produced (duplicate-values produced)]
    (when (seq duplicate-ids)
      (throw (ex-info "duplicate workflow step ids"
                      {:duplicate_step_ids duplicate-ids})))
    (when (seq duplicate-produced)
      (throw (ex-info "duplicate workflow produced keys"
                      {:duplicate_produced_keys duplicate-produced})))
    (loop [available (set (keys initial-state))
           remaining (vec steps)
           ordered []]
      (if (empty? remaining)
        (assoc plan :steps ordered)
        (let [{ready true blocked false}
              (group-by (fn [step]
                          (set/subset? (set (:requires step)) available))
                        remaining)]
          (when (empty? ready)
            (throw (ex-info "missing workflow dependencies"
                            {:available (vec (sort-by key-name available))
                             :blocked (mapv (fn [step]
                                              {:id (key-name (:id step))
                                               :missing (vec
                                                         (sort-by key-name
                                                                  (set/difference
                                                                   (set (:requires step))
                                                                   available)))})
                                            blocked)})))
          (recur (into available (mapcat :produces ready))
                 (vec blocked)
                 (into ordered ready)))))))

(defn- path-record [record]
  (into {}
        (for [[k v] record
              :when (some? v)]
          [(key-name k) v])))

(defn- json-step-plan [step]
  {"id" (key-name (:id step))
   "requires" (mapv key-name (:requires step))
   "produces" (mapv key-name (:produces step))})

(defn- json-plan [workflow-id steps]
  {"schema_version" "soranoha-workflow-plan-v1"
   "workflow_id" workflow-id
   "steps" (mapv json-step-plan steps)})

(defn- summarize-run [workflow-id run-id started-at ended-at steps]
  (let [failed (count (filter #(= "failed" (get % "status")) steps))
        partial (count (filter #(= "partial" (get % "status")) steps))
        passed (count (filter #(= "passed" (get % "status")) steps))]
    {"schema_id" workflow-run-schema-id
     "schema_version" "soranoha-workflow-run-v1"
     "workflow_id" workflow-id
     "run_id" run-id
     "status" (cond
                (pos? failed) "failed"
                (pos? partial) "partial"
                :else "passed")
     "started_at" started-at
     "ended_at" ended-at
     "duration_ms" (duration-ms started-at ended-at)
     "step_count" (count steps)
     "steps_passed" passed
     "steps_failed" failed
     "steps" steps}))

(defn- write-run! [output-root run]
  (manifest/write-json-file! (io/file output-root "workflow-run.json") run))

(defn- step-record [{:keys [step status started-at ended-at result error]}]
  (cond-> {"id" (key-name (:id step))
           "status" status
           "started_at" started-at
           "ended_at" ended-at
           "duration_ms" (duration-ms started-at ended-at)
           "requires" (mapv key-name (:requires step))
           "produces" (mapv key-name (:produces step))
           "inputs" (mapv path-record (:inputs result []))
           "outputs" (mapv path-record (:outputs result []))
           "messages" (mapv path-record (:messages result []))}
    error
    (assoc "error" {"error_class" (.getName (class error))
                    "message" (.getMessage error)
                    "data" (or (ex-data error) {})})))

(defn run-workflow!
  [{:keys [workflow-id run-id output-root initial-state steps clock]
    :or {clock now-utc
         run-id "local-run"}}]
  (let [output-root (io/file output-root)
        _ (.mkdirs output-root)
        plan (validate-plan! {:steps steps} initial-state)
        ordered-steps (:steps plan)
        started-at (clock)]
    (manifest/write-json-file! (io/file output-root "workflow-plan.json")
                               (json-plan workflow-id ordered-steps))
    (loop [state initial-state
           remaining ordered-steps
           records []]
      (if (empty? remaining)
        (let [ended-at (clock)
              run (summarize-run workflow-id run-id started-at ended-at records)]
          (write-run! output-root run)
          {:state state :run run :plan plan})
        (let [step (first remaining)
              step-start (clock)]
          (try
            (let [result ((:run step) state)
                  status (name (or (:status result) :passed))
                  step-end (clock)
                  record (step-record {:step step
                                       :status status
                                       :started-at step-start
                                       :ended-at step-end
                                       :result result})
                  records' (conj records record)
                  interim (summarize-run workflow-id
                                         run-id
                                         started-at
                                         step-end
                                         records')]
              (write-run! output-root interim)
              (recur (merge state (:state-updates result))
                     (rest remaining)
                     records'))
            (catch Throwable t
              (let [step-end (clock)
                    record (step-record {:step step
                                         :status "failed"
                                         :started-at step-start
                                         :ended-at step-end
                                         :result {}
                                         :error t})
                    run (summarize-run workflow-id
                                       run-id
                                       started-at
                                       step-end
                                       (conj records record))]
                (write-run! output-root run)
                (throw t)))))))))
```

- [ ] **Step 4: Run focused tests**

Run:

```bash
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: pass.

- [ ] **Step 5: Run formatter**

Run:

```bash
clj-paren-repair --paths abc/src/abc/tools/workflow.clj abc/test/abc/tools/workflow_test.clj
```

Expected: files are formatted or unchanged.

- [ ] **Step 6: Commit**

```bash
git add abc/src/abc/tools/workflow.clj abc/test/abc/tools/workflow_test.clj
git commit -m "feat(abc): add serial workflow runner"
```

---

### Task 3: Migrate Publication Rehearsal to Workflow Runner

**Files:**
- Modify: `abc/src/abc/tools/soranoha.clj`
- Modify: `abc/test/abc/tools/soranoha_test.clj`

**Interfaces:**
- Consumes: `abc.tools.workflow/run-workflow!`.
- Preserves: `abc.tools.soranoha/publication-rehearsal! [input-root output-root request-set-label snapshot-scope snapshot-date] -> 0`.
- Produces: `workflow-plan.json` and `workflow-run.json` under the publication rehearsal output root.

- [ ] **Step 1: Add failing rehearsal workflow report assertions**

In the existing publication rehearsal test in
`abc/test/abc/tools/soranoha_test.clj`, after running the command, assert:

```clojure
(is (.isFile (io/file output-root "workflow-plan.json")))
(is (.isFile (io/file output-root "workflow-run.json")))
(let [run (files/read-json (io/file output-root "workflow-run.json"))]
  (is (= "soranoha.publication-rehearsal.v1" (get run "workflow_id")))
  (is (= "passed" (get run "status")))
  (is (some #(= "source-snapshot" (get % "id")) (get run "steps")))
  (is (some #(= "stage-publication" (get % "id")) (get run "steps"))))
```

Use the test's existing `output-root` binding. Ensure the namespace require
list contains:

```clojure
[abc.tools.files :as files]
```

- [ ] **Step 2: Run focused tests to verify red**

Run:

```bash
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: failure because rehearsal does not write workflow reports.

- [ ] **Step 3: Require the workflow namespace**

Modify `abc/src/abc/tools/soranoha.clj` require list:

```clojure
[abc.tools.workflow :as workflow]
```

- [ ] **Step 4: Extract publication rehearsal step definitions**

Add a private helper near `publication-rehearsal!`:

```clojure
(defn- publication-rehearsal-steps []
  [{:id :source-snapshot
    :requires [:input-root :source-snapshot-root :snapshot-scope :snapshot-date]
    :produces [:source-snapshot-result]
    :run (fn [{:keys [input-root source-snapshot-root snapshot-scope snapshot-date]}]
           (let [result (write-source-snapshot-root!
                         input-root
                         source-snapshot-root
                         snapshot-scope
                         snapshot-date)]
             {:state-updates {:source-snapshot-result result}
              :outputs [{:role "source-snapshot"
                         :path "source-snapshot/source-snapshot.json"
                         :content_hash (:snapshot-hash result)}]}))}
   {:id :resolve-request-set
    :requires [:request-set-label :source-snapshot-result :request-set-file]
    :produces [:request-set]
    :run (fn [{:keys [request-set-label source-snapshot-result request-set-file]}]
           (let [request-set (request-set-resolver/resolve-request-set
                              request-set-label
                              {:subject-source-path
                               (str (:snapshot-file source-snapshot-result))})]
             (manifest/write-json-file! request-set-file request-set)
             {:state-updates {:request-set request-set}
              :outputs [{:role "request-set"
                         :path (str request-set-file)
                         :request_set_id (get request-set "request_set_id")}]}))}
   {:id :materialize-snapshot-root
    :requires [:request-set :request-set-file :snapshot-root]
    :produces [:snapshot]
    :run (fn [{:keys [request-set request-set-file snapshot-root]}]
           (materialize-snapshot-root! (str request-set-file) snapshot-root)
           (let [snapshot (read-valid-snapshot-index snapshot-root)]
             {:state-updates {:snapshot snapshot}
              :outputs [{:role "snapshot-index"
                         :path "snapshot-root/snapshot-index.json"
                         :snapshot_identity_hash
                         (get snapshot "snapshot_identity_hash")}]}))}
   {:id :validate-snapshot-root
    :requires [:snapshot-root :snapshot]
    :produces [:snapshot-root-validation]
    :run (fn [{:keys [snapshot-root snapshot]}]
           (validate-snapshot-root-references! snapshot-root snapshot)
           (validate-run-summary! snapshot-root snapshot)
           {:state-updates {:snapshot-root-validation true}})}
   {:id :publication-report
    :requires [:snapshot-root :snapshot :publication-report-file]
    :produces [:publication-report-value]
    :run (fn [{:keys [snapshot-root snapshot publication-report-file]}]
           (let [{value :report}
                 (write-publication-report-file! snapshot-root
                                                 snapshot
                                                 publication-report-file)]
             {:state-updates {:publication-report-value value}
              :outputs [{:role "publication-report"
                         :path (str publication-report-file)
                         :content_hash (manifest/file-hash publication-report-file)}]}))}
   {:id :layout-report
    :requires [:snapshot-root :snapshot :layout-report-file]
    :produces [:layout-report-value]
    :run (fn [{:keys [snapshot-root snapshot layout-report-file]}]
           (let [{value :report}
                 (write-layout-report-file! snapshot-root
                                            snapshot
                                            layout-report-file)]
             {:state-updates {:layout-report-value value}
              :outputs [{:role "layout-report"
                         :path "reports/layout-report.json"
                         :content_hash (manifest/file-hash layout-report-file)}]}))}
   {:id :stage-publication
    :requires [:snapshot-root :staged-root :snapshot]
    :produces [:staged-result]
    :run (fn [{:keys [snapshot-root staged-root snapshot]}]
           (let [result (stage-publication/stage-publication!
                         {:snapshot-root snapshot-root
                          :staged-root staged-root
                          :snapshot snapshot})]
             {:state-updates {:staged-result result}
              :outputs [{:role "staged-index"
                         :path (str (:index-file result))}]}))}
   {:id :validate-staged-publication
    :requires [:staged-root :staged-result]
    :produces [:staged-snapshot]
    :run (fn [{:keys [staged-root staged-result]}]
           (let [staged-snapshot (validate-staged-root! staged-root)]
             {:state-updates {:staged-snapshot staged-snapshot
                              :staged-result (assoc staged-result
                                                    :snapshot
                                                    staged-snapshot)}}))}])
```

Do not change artifact paths while adding workflow output records. If an output
path assertion fails, fix only the workflow path-record formatting and keep the
existing artifact files in their current locations.

- [ ] **Step 5: Rewrite publication-rehearsal! around run-workflow!**

Replace the inner `let` in `publication-rehearsal!` with:

```clojure
(files/delete-tree! output-root-file)
(.mkdirs output-root-file)
(let [workflow-result
      (workflow/run-workflow!
       {:workflow-id "soranoha.publication-rehearsal.v1"
        :run-id (str "publication-rehearsal-" snapshot-date)
        :output-root output-root-file
        :initial-state {:input-root input-root
                        :output-root output-root-file
                        :source-snapshot-root source-snapshot-root
                        :request-set-dir request-set-dir
                        :request-set-file request-set-file
                        :request-set-label request-set-label
                        :snapshot-scope snapshot-scope
                        :snapshot-date snapshot-date
                        :snapshot-root snapshot-root
                        :reports-root reports-root
                        :publication-report-file publication-report-file
                        :layout-report-file layout-report-file
                        :staged-root staged-root}
        :steps (publication-rehearsal-steps)})
      state (:state workflow-result)
      snapshot (:snapshot state)
      request-set (:request-set state)
      source-snapshot-result (:source-snapshot-result state)
      staged-result (:staged-result state)
      report (publication-rehearsal-report
              {:input-root input-root
               :output-root output-root-file
               :source-snapshot-result source-snapshot-result
               :request-set-file request-set-file
               :request-set request-set
               :snapshot-root snapshot-root
               :snapshot snapshot
               :staged-root staged-root
               :staged-result staged-result
               :publication-report-file publication-report-file
               :publication-report-value (:publication-report-value state)
               :layout-report-file layout-report-file
               :layout-report-value (:layout-report-value state)})]
  (manifest/write-json-file! rehearsal-report-file report)
  (println "rehearsal_report:" (str rehearsal-report-file))
  (println "source_snapshot:" (str (:snapshot-file source-snapshot-result)))
  (println "request_set:" (str request-set-file))
  (println "snapshot_root:" (str snapshot-root))
  (println "staged_index:" (str (:index-file staged-result)))
  (println "snapshot_identity_hash:" (get snapshot "snapshot_identity_hash"))
  (println "request_set_id:" (get request-set "request_set_id"))
  (println "work_count:" (:works-count source-snapshot-result))
  0)
```

Keep the existing printed lines and return value.

- [ ] **Step 6: Run focused tests**

Run:

```bash
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: pass.

- [ ] **Step 7: Commit**

```bash
git add abc/src/abc/tools/soranoha.clj abc/test/abc/tools/soranoha_test.clj
git commit -m "refactor(abc): report publication rehearsal workflow"
```

---

### Task 4: Apply Workflow Runner to Build Publication

**Files:**
- Modify: `abc/src/abc/tools/soranoha_build_publication.clj`
- Modify: `abc/test/abc/tools/soranoha_test.clj`

**Interfaces:**
- Consumes: `abc.tools.workflow/run-workflow!`.
- Preserves: `abc.tools.soranoha-build-publication/build-publication! [publication-rehearsal-fn args] -> 0`.
- Produces: top-level `workflow-plan.json` and `workflow-run.json` in the promoted build output root.

- [ ] **Step 1: Add failing build-publication workflow assertions**

In `build-publication-command-materializes-and-delegates-to-rehearsal-test`,
assert:

```clojure
(is (.isFile (io/file output-root "workflow-plan.json")))
(is (.isFile (io/file output-root "workflow-run.json")))
(let [run (files/read-json (io/file output-root "workflow-run.json"))]
  (is (= "soranoha.build-publication.v1" (get run "workflow_id")))
  (is (= "passed" (get run "status")))
  (is (some #(= "materialize-selected-sources" (get % "id"))
            (get run "steps")))
  (is (some #(= "publication-rehearsal" (get % "id"))
            (get run "steps"))))
```

- [ ] **Step 2: Run focused tests to verify red**

Run:

```bash
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: failure because `build-publication!` does not write workflow reports.

- [ ] **Step 3: Require workflow namespace**

Modify `abc/src/abc/tools/soranoha_build_publication.clj` require list:

```clojure
[abc.tools.workflow :as workflow]
```

- [ ] **Step 4: Extract build-publication steps**

Add:

```clojure
(defn- build-publication-steps [publication-rehearsal-fn]
  [{:id :materialize-selected-sources
    :requires [:aozora-root :tmp-root]
    :produces [:materialization-result]
    :run (fn [{:keys [aozora-root tmp-root]}]
           (let [result (materialize-selected-sources!
                         {:aozora-root aozora-root
                          :output-root tmp-root})]
             {:state-updates {:materialization-result result}
              :outputs [{:role "source-selection-report"
                         :path "source-selection-report.json"}]
              :messages [{:level "info"
                          :message "selected source ZIPs"
                          :data {"selected_source_count"
                                 (get-in result
                                         [:report "selected_source_count"])}}]}))}
   {:id :write-build-plan
    :requires [:opts :config-value :materialization-result]
    :produces [:build-plan]
    :run (fn [{:keys [opts config-value materialization-result tmp-root]}]
           (let [plan (build-plan opts config-value materialization-result)]
             (abc-json/write-deterministic-json-file!
              (io/file tmp-root "build-config.json")
              config-value)
             (abc-json/write-deterministic-json-file!
              (io/file tmp-root "build-plan.json")
              plan)
             {:state-updates {:build-plan plan}
              :outputs [{:role "build-config"
                         :path "build-config.json"}
                        {:role "build-plan"
                         :path "build-plan.json"}]}))}
   {:id :publication-rehearsal
    :requires [:config-value :snapshot-date :materialization-result :rehearsal-root]
    :produces [:rehearsal-result]
    :run (fn [{:keys [config-value snapshot-date materialization-result rehearsal-root]}]
           (let [exit-code (publication-rehearsal-fn
                            (str (:materialized-root materialization-result))
                            (str rehearsal-root)
                            (get config-value "request_set_label")
                            (get config-value "snapshot_scope")
                            snapshot-date)]
             {:state-updates {:rehearsal-result exit-code}
              :outputs [{:role "rehearsal-report"
                         :path "rehearsal/rehearsal-report.json"}]}))}])
```

- [ ] **Step 5: Rewrite build-publication! to use the runner**

Inside the `let` that currently binds `tmp-root`, replace direct materialization
and rehearsal calls with:

```clojure
(let [opts (assoc opts :output-root tmp-root)
      rehearsal-root (io/file tmp-root "rehearsal")]
  (workflow/run-workflow!
   {:workflow-id "soranoha.build-publication.v1"
    :run-id (str "build-publication-" snapshot-date)
    :output-root tmp-root
    :initial-state {:aozora-root aozora-root
                    :tmp-root tmp-root
                    :opts opts
                    :config-value config-value
                    :snapshot-date snapshot-date
                    :rehearsal-root rehearsal-root}
    :steps (build-publication-steps publication-rehearsal-fn)})
  (let [final-root (promote-output-root! tmp-root output-root replace)]
    (println "build_publication_root:" (str final-root))
    (println "materialized_root:" (str (io/file final-root
                                                "materialized-root")))
    (println "rehearsal_root:" (str (io/file final-root "rehearsal")))
    0))
```

Preserve `--replace`, temp root promotion, printed paths, and return value.

- [ ] **Step 6: Run focused tests**

Run:

```bash
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: pass.

- [ ] **Step 7: Commit**

```bash
git add abc/src/abc/tools/soranoha_build_publication.clj \
        abc/test/abc/tools/soranoha_test.clj
git commit -m "refactor(abc): report build-publication workflow"
```

---

### Task 5: Schema-Conformant Publication Batch Workflow Sidecar

**Files:**
- Modify: `abc/src/abc/tools/materialize_publication.clj`
- Modify: `abc/test/abc/tools/materialize_publication_test.clj`

**Interfaces:**
- Preserves: `materialize-publications-batch!`.
- Produces: `workflow-run.json` beside the existing batch summary.
- Produces: `"workflow_run_path": "workflow-run.json"` in the existing batch summary when a summary file is written.

- [ ] **Step 1: Add failing batch workflow sidecar assertions**

In `abc/test/abc/tools/materialize_publication_test.clj`, update the test that
binds `summary` from `materialize/materialize-publications-batch!`. Ensure the
test namespace has these requires:

```clojure
[abc.tools.files :as files]
[abc.tools.schema :as schema]
```

After the existing `select-keys` assertion, add:

```clojure
(is (= "workflow-run.json" (get summary "workflow_run_path")))
(let [workflow-run-file (io/file (.getParentFile summary-file)
                                 "workflow-run.json")
      workflow-run (files/read-json workflow-run-file)
      workflow-run-schema (files/read-json "schemas/workflow-run.schema.json")]
  (is (.isFile workflow-run-file))
  (is (nil? (schema/validation-errors workflow-run-schema workflow-run)))
  (is (= "soranoha.materialize-publications-batch.v1"
         (get workflow-run "workflow_id")))
  (is (= (get summary "jobs_total")
         (get workflow-run "step_count")))
  (is (= (get summary "jobs_failed")
         (get workflow-run "steps_failed")))
  (is (every? #{"passed" "partial" "failed" "skipped"}
              (map #(get % "status") (get workflow-run "steps")))))
```

- [ ] **Step 2: Run focused tests to verify red**

Run:

```bash
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: failure because batch materialization does not write `workflow-run.json`.

- [ ] **Step 3: Add workflow run helper**

In `abc/src/abc/tools/materialize_publication.clj`, add:

```clojure
(def workflow-run-schema-id
  "https://w3id.org/abc/schemas/workflow-run.schema.json")

(defn- now-utc []
  (str (java.time.Instant/now)))

(defn- batch-step-record [started-at ended-at result]
  (let [status (case (get result "status")
                 "passed" "passed"
                 "partial" "partial"
                 "skipped" "skipped"
                 "failed")]
    {"id" (str (get result "id"))
     "status" status
     "started_at" started-at
     "ended_at" ended-at
     "duration_ms" 0
     "requires" []
     "produces" ["publication-output"]
     "inputs" []
     "outputs" (cond-> []
                 (get result "tei")
                 (conj {"role" "tei"
                        "path" (get result "tei")})
                 (get result "plain_text")
                 (conj {"role" "plain-text"
                        "path" (get result "plain_text")}))
     "messages" []}))

(defn- batch-workflow-run [results]
  (let [started-at (now-utc)
        ended-at started-at
        failed (count (filter #(= "failed" (get % "status")) results))
        partial (count (filter #(= "partial" (get % "status")) results))
        passed (count (filter #(= "passed" (get % "status")) results))]
    {"schema_id" workflow-run-schema-id
     "schema_version" "soranoha-workflow-run-v1"
     "workflow_id" "soranoha.materialize-publications-batch.v1"
     "run_id" "local-batch"
     "status" (cond
                (pos? failed) "failed"
                (pos? partial) "partial"
                :else "passed")
     "started_at" started-at
     "ended_at" ended-at
     "duration_ms" 0
     "step_count" (count results)
     "steps_passed" passed
     "steps_failed" failed
     "steps" (mapv #(batch-step-record started-at ended-at %) results)}))
```

- [ ] **Step 4: Write workflow sidecar and point the summary at it**

In `materialize-publications-batch!`, when `summary-path` is present, write a
schema-conformant sidecar beside it and add a relative pointer to the summary:

```clojure
(let [summary-file (some-> summary-path io/file)
      summary-dir (some-> summary-file .getParentFile)
      workflow-run-file (some-> summary-dir (io/file "workflow-run.json"))
      workflow-run (batch-workflow-run results)
      summary (cond-> existing-summary
                workflow-run-file
                (assoc "workflow_run_path" "workflow-run.json"))]
  (when workflow-run-file
    (manifest/write-json-file! workflow-run-file workflow-run))
  (when summary-file
    (manifest/write-json-file! summary-file summary))
  summary)
```

Do not embed a partial `"workflow"` object inside the summary. The sidecar is
the reusable cross-language value; the summary only carries the local pointer.

- [ ] **Step 5: Run focused tests**

Run:

```bash
nix build .#checks.x86_64-linux.abc-clj-nix-focused-tests --print-build-logs
```

Expected: pass.

- [ ] **Step 6: Commit**

```bash
git add abc/src/abc/tools/materialize_publication.clj \
        abc/test/abc/tools/materialize_publication_test.clj
git commit -m "feat(abc): add publication batch workflow sidecar"
```

---

### Task 6: Bash Workflow Event Helper and Morph Report Adoption

**Files:**
- Create: `scripts/workflow-run-lib.sh`
- Modify: `ab-validator/reports/morph-warehouse/build-report.sh`
- Create or modify: `tests/workflow-run-lib-smoke.sh`
- Modify: `flake.nix`

**Interfaces:**
- Produces shell functions:
  - `workflow_init RUN_FILE WORKFLOW_ID RUN_ID`
  - `workflow_step_pass STEP_ID OUTPUT_PATH`
  - `workflow_step_fail STEP_ID MESSAGE`
  - `workflow_finish STATUS`
- Produces `workflow-run.json` in the morph warehouse report output directory.

- [ ] **Step 1: Write shell smoke test**

Create `tests/workflow-run-lib-smoke.sh`:

```bash
#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

# shellcheck source=/dev/null
source "$repo_root/scripts/workflow-run-lib.sh"

run_file="$tmp/workflow-run.json"
workflow_init "$run_file" "shell.fixture.v1" "fixture-run"
workflow_step_pass "first" "out/first.txt"
workflow_step_fail "second" "broken"
workflow_finish "failed"

python3 - "$run_file" <<'PY'
import json
import sys

path = sys.argv[1]
data = json.load(open(path, encoding="utf-8"))
assert data["workflow_id"] == "shell.fixture.v1"
assert data["status"] == "failed"
assert data["step_count"] == 2
assert data["steps_passed"] == 1
assert data["steps_failed"] == 1
assert data["steps"][0]["id"] == "first"
assert data["steps"][1]["id"] == "second"
assert data["steps"][1]["status"] == "failed"
assert data["steps"][1]["error"]["message"] == "broken"
PY
```

- [ ] **Step 2: Add Nix check for the smoke test**

In `flake.nix`, add `tests/workflow-run-lib-smoke.sh` to the monorepo checks
using the existing `mkMonorepoCheck` pattern:

```nix
monorepo-workflow-run-lib =
  mkMonorepoCheck "soranoha-monorepo-workflow-run-lib"
    [
      pkgs.bash
      pkgs.coreutils
      pkgs.python3
    ]
    ''
      bash tests/workflow-run-lib-smoke.sh
    '';
```

- [ ] **Step 3: Run check to verify red**

Run:

```bash
nix build .#checks.x86_64-linux.monorepo-workflow-run-lib --print-build-logs
```

Expected: failure because `scripts/workflow-run-lib.sh` does not exist.

- [ ] **Step 4: Implement shell helper**

Create `scripts/workflow-run-lib.sh`:

```bash
#!/usr/bin/env bash

workflow_utc_now() {
  date -u +%Y-%m-%dT%H:%M:%SZ
}

workflow_json_escape() {
  python3 -c 'import json,sys; print(json.dumps(sys.argv[1]))' "$1"
}

workflow_init() {
  WORKFLOW_RUN_FILE="$1"
  WORKFLOW_ID="$2"
  WORKFLOW_RUN_ID="$3"
  WORKFLOW_STARTED_AT="$(workflow_utc_now)"
  WORKFLOW_STEPS_FILE="$WORKFLOW_RUN_FILE.steps.jsonl"
  mkdir -p "$(dirname "$WORKFLOW_RUN_FILE")"
  : > "$WORKFLOW_STEPS_FILE"
}

workflow_step_pass() {
  local step_id="$1"
  local output_path="$2"
  local now
  now="$(workflow_utc_now)"
  printf '{"id":%s,"status":"passed","started_at":%s,"ended_at":%s,"duration_ms":0,"requires":[],"produces":[],"inputs":[],"outputs":[{"role":"output","path":%s}],"messages":[]}\n' \
    "$(workflow_json_escape "$step_id")" \
    "$(workflow_json_escape "$now")" \
    "$(workflow_json_escape "$now")" \
    "$(workflow_json_escape "$output_path")" \
    >> "$WORKFLOW_STEPS_FILE"
}

workflow_step_fail() {
  local step_id="$1"
  local message="$2"
  local now
  now="$(workflow_utc_now)"
  printf '{"id":%s,"status":"failed","started_at":%s,"ended_at":%s,"duration_ms":0,"requires":[],"produces":[],"inputs":[],"outputs":[],"messages":[],"error":{"error_class":"shell","message":%s,"data":{}}}\n' \
    "$(workflow_json_escape "$step_id")" \
    "$(workflow_json_escape "$now")" \
    "$(workflow_json_escape "$now")" \
    "$(workflow_json_escape "$message")" \
    >> "$WORKFLOW_STEPS_FILE"
}

workflow_finish() {
  local status="$1"
  local ended_at
  ended_at="$(workflow_utc_now)"
  python3 - "$WORKFLOW_RUN_FILE" "$WORKFLOW_STEPS_FILE" "$WORKFLOW_ID" "$WORKFLOW_RUN_ID" "$status" "$WORKFLOW_STARTED_AT" "$ended_at" <<'PY'
import json
import sys

run_file, steps_file, workflow_id, run_id, status, started_at, ended_at = sys.argv[1:]
steps = []
with open(steps_file, encoding="utf-8") as handle:
    for line in handle:
        line = line.strip()
        if line:
            steps.append(json.loads(line))
failed = sum(1 for step in steps if step["status"] == "failed")
passed = sum(1 for step in steps if step["status"] == "passed")
run = {
    "schema_id": "https://w3id.org/abc/schemas/workflow-run.schema.json",
    "schema_version": "soranoha-workflow-run-v1",
    "workflow_id": workflow_id,
    "run_id": run_id,
    "status": status,
    "started_at": started_at,
    "ended_at": ended_at,
    "duration_ms": 0,
    "step_count": len(steps),
    "steps_passed": passed,
    "steps_failed": failed,
    "steps": steps,
}
with open(run_file, "w", encoding="utf-8") as handle:
    json.dump(run, handle, ensure_ascii=False, sort_keys=True, indent=2)
    handle.write("\n")
PY
}
```

- [ ] **Step 5: Adopt helper in morph warehouse report script**

Modify `ab-validator/reports/morph-warehouse/build-report.sh`:

```bash
workflow_lib="$script_dir/../../../scripts/workflow-run-lib.sh"
if [[ -f "$workflow_lib" ]]; then
  # shellcheck source=/dev/null
  source "$workflow_lib"
  workflow_init "$out_dir/workflow-run.json" "morph-warehouse.build-report.v1" "local"
  workflow_finished=false
  trap 'status=$?; if [[ $status -ne 0 && "${workflow_finished:-false}" == "false" ]]; then workflow_step_fail "script" "script failed with exit $status"; workflow_finish "failed"; fi' EXIT
else
  workflow_init() { :; }
  workflow_step_pass() { :; }
  workflow_step_fail() { :; }
  workflow_finish() { :; }
fi
```

After each DuckDB template succeeds, add:

```bash
workflow_step_pass "$name" "outputs/$name.tsv"
```

Before the final echo, add:

```bash
workflow_finished=true
workflow_finish "passed"
```

- [ ] **Step 6: Run shell smoke check**

Run:

```bash
nix build .#checks.x86_64-linux.monorepo-workflow-run-lib --print-build-logs
```

Expected: pass.

- [ ] **Step 7: Run full flake check**

Run:

```bash
nix flake check --print-build-logs
```

Expected: pass.

- [ ] **Step 8: Commit**

```bash
git add scripts/workflow-run-lib.sh \
        tests/workflow-run-lib-smoke.sh \
        ab-validator/reports/morph-warehouse/build-report.sh \
        flake.nix
git commit -m "feat: add shell workflow run helper"
```

---

### Task 7: Final Review and Follow-Up Decision

**Files:**
- Modify: no production files expected.
- Optional docs update: `abc/docs/superpowers/specs/2026-07-08-workflow-orchestration-design.md`

**Interfaces:**
- Consumes: completed Tasks 1-6.
- Produces: explicit decision whether to evaluate `core.async.flow`, Nodely, or Pathom next.

- [ ] **Step 1: Run full verification**

Run:

```bash
nix flake check --print-build-logs
```

Expected: pass.

- [ ] **Step 2: Inspect generated workflow reports in tests**

Run a focused local command that produces a workflow report, for example the
existing build-publication fixture test or a small `soranoha publication-rehearsal`
fixture command. Confirm the report contains:

- `workflow_id`;
- top-level `status`;
- at least three step records;
- one output path for a generated publication artifact or report.

- [ ] **Step 3: Decide next engine evaluation**

Add a short prose note to the design spec's follow-up section. The note must
name the files actually changed by this implementation and state one concrete
reason for each engine decision:

- whether `core.async.flow` should be evaluated next for full-corpus per-work
  materialization;
- whether Nodely should be evaluated next for lazy target realization;
- whether Pathom should be evaluated next for `explain-snapshot`.

Do not commit the note until every engine decision is written as a complete
sentence with observed evidence from Tasks 1-6.

- [ ] **Step 4: Commit review note if changed**

```bash
git add abc/docs/superpowers/specs/2026-07-08-workflow-orchestration-design.md
git commit -m "docs: record workflow follow-up decision"
```

- [ ] **Step 5: Merge and push when requested**

If the user asks to merge this slice:

```bash
git status -sb
nix flake check --print-build-logs
git checkout main
git merge --ff-only workflow-orchestration
nix flake check --print-build-logs
git push origin main
```

Expected: main is pushed only after full verification passes on main.

---

## Self-Review

- Spec coverage: Tasks 1-2 implement the language-neutral schema and Clojure
  runner. Tasks 3-5 migrate the high-fit Clojure targets. Task 6 proves
  non-Clojure reuse on Bash. Task 7 records whether external engines now earn
  evaluation.
- Placeholder scan: this plan intentionally includes no deferred blanks or
  unspecified implementation step.
- Type consistency: workflow IDs, schema IDs, status strings, and function
  names match the design spec.
- Scope control: Python adoption and Pathom inspection are deferred until the
  schema and runner prove useful across Clojure and one shell workflow.
