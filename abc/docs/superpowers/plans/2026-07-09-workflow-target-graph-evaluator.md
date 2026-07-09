# Workflow Target-Graph Evaluator Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a minimal Soranoha-owned target-graph evaluator that realizes a requested target value from a graph *data value* using lazy, memoized evaluation with conditional (branch) dependencies, avoiding the unselected branch's work and emitting order-independent provenance.

**Architecture:** A single new namespace `abc.tools.workflow.target` holds a data model (`:value` / `:leaf` / `:branch` nodes), deterministic graph validation (missing-dependency + cycle), and a lazy recursive evaluator. Provenance is a DAG value — a set of node summaries plus an edge list — never an execution sequence. No external dataflow dependency is added; nothing here touches Nix, schemas, or the workflow-run report (those are Slices 2–4, separate follow-up plans).

**Tech Stack:** Clojure (`clojure.test` + Kaocha runner at `abc/bin/kaocha`), no new dependencies.

**Spec:** `abc/docs/superpowers/specs/2026-07-09-workflow-target-graph-evaluator-design.md`

## Global Constraints

- **No new dependencies.** Do not add `dev.nu/nodely` or any dataflow library; do not regenerate `abc/deps-lock.json`. (Spec: Non-Goals, Evaluator Policy.)
- **Provenance is order-independent.** Emit a node-summary set + edge list. Never emit or rely on an execution sequence. (Spec: Node Provenance.)
- **Graph validation is Soranoha-owned and deterministic.** No third-party heuristic cycle check. (Spec: The Internal Evaluator.)
- **Status enum is exactly** `"passed"`, `"failed"`, `"partial"`, `"skipped"` — the same enum `workflow-run.schema.json` already uses. (Spec: Status Vocabulary.)
- **Failure-as-value is a value, not a throw.** A leaf returning `failed-value` becomes a realized node with `status = "partial"`; thrown exceptions are infrastructure/programming failures. (Spec: Node Result Contract, Error Model.)
- **Slice 1 is pure.** Every node in Slice 1 fixtures is a function of supplied inputs; no side effects, no Nix, no filesystem. (Spec: Slice 1.)
- All tests run from the `abc/` directory. Run all evaluator tests: `cd abc && bin/kaocha --focus abc.tools.workflow.target-test`. Run one: append `/<test-name>`.
- Commit style: conventional commits (`feat(...)`, `test(...)`), matching the repo.

## File Structure

- Create `abc/src/abc/tools/workflow/target.clj` — the evaluator: data-model constructors, `validate-graph`, `eval-target`, result constructors. Coexists with the existing `abc/src/abc/tools/workflow.clj` (namespace `abc.tools.workflow`) — a file and a same-named subdirectory are valid together in Clojure.
- Create `abc/test/abc/tools/workflow/target_test.clj` — `clojure.test` tests for validation and evaluation.

---

### Task 1: Data model + deterministic graph validation

**Files:**
- Create: `abc/src/abc/tools/workflow/target.clj`
- Test: `abc/test/abc/tools/workflow/target_test.clj`

**Interfaces:**
- Consumes: nothing (first task).
- Produces:
  - `(value-node)` → `{:kind :value}`
  - `(leaf deps impl-id impl-fn)` → `{:kind :leaf :deps [...] :impl {:impl/id impl-id :impl/fn impl-fn}}`; `impl-fn` takes a map `{dep-key dep-value}` and returns a value or a result map.
  - `(branch cond-k then-k else-k)` → `{:kind :branch :cond cond-k :then then-k :else else-k}`
  - `(passed v)`, `(failed-value v)` / `(failed-value v evidence)`, `(skipped)` → result maps.
  - `(validate-graph graph)` → `nil` when valid, else a non-empty vector of error maps `{:type :missing-dependency :node k :dep d}` and/or `{:type :cycle :cycle [k … k]}`.

- [ ] **Step 1: Write the failing validation tests**

Create `abc/test/abc/tools/workflow/target_test.clj`:

```clojure
(ns abc.tools.workflow.target-test
  (:require [abc.tools.workflow.target :as target]
            [clojure.test :refer [deftest is testing]]))

(deftest validate-graph-accepts-valid-dag-test
  (let [g {:x (target/value-node)
           :a (target/leaf [:x] :a (fn [m] (inc (:x m))))}]
    (is (nil? (target/validate-graph g)))))

(deftest validate-graph-detects-missing-dependency-test
  (let [g {:a (target/leaf [:missing] :a (fn [_] 1))}]
    (is (= [{:type :missing-dependency :node :a :dep :missing}]
           (target/validate-graph g)))))

(deftest validate-graph-detects-cycle-test
  (let [g {:a (target/leaf [:b] :a (fn [m] (:b m)))
           :b (target/leaf [:a] :b (fn [m] (:a m)))}]
    (is (some #(= :cycle (:type %)) (target/validate-graph g)))))

(deftest validate-graph-no-false-positive-on-mutually-exclusive-branch-test
  ;; The hazard a naive heuristic cycle check would flag: two sides of a branch.
  (let [g {:c (target/value-node)
           :t (target/leaf [] :t (constantly 1))
           :e (target/leaf [] :e (constantly 2))
           :d (target/branch :c :t :e)}]
    (is (nil? (target/validate-graph g)))))
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cd abc && bin/kaocha --focus abc.tools.workflow.target-test`
Expected: FAIL — namespace `abc.tools.workflow.target` cannot be loaded (does not exist yet).

- [ ] **Step 3: Write the data model + validation implementation**

Create `abc/src/abc/tools/workflow/target.clj`:

```clojure
(ns abc.tools.workflow.target
  "Internal target-graph evaluator: realizes a requested target value from a
   Soranoha-owned graph data value using lazy, memoized evaluation with
   conditional (branch) dependencies. Provenance is a DAG value (node set +
   edge list), never an execution sequence. No external dataflow dependency."
  (:require [clojure.set :as set]))

;; ---- Node constructors ---------------------------------------------------

(defn value-node
  "A node whose value is supplied at evaluation time via the `inputs` map."
  []
  {:kind :value})

(defn leaf
  "A node that realizes a value from its resolved dependencies.
   `impl-fn` receives a map {dep-key dep-value} and returns a value or a
   result map (see `passed` / `failed-value` / `skipped`)."
  [deps impl-id impl-fn]
  {:kind :leaf
   :deps (vec deps)
   :impl {:impl/id impl-id :impl/fn impl-fn}})

(defn branch
  "A conditional node: realizes `then-k` when `cond-k` is truthy, else `else-k`.
   Only the taken side is realized."
  [cond-k then-k else-k]
  {:kind :branch :cond cond-k :then then-k :else else-k})

;; ---- Node-result constructors -------------------------------------------

(defn passed
  "Wrap a successful leaf value."
  [value]
  {:result :passed :value value})

(defn failed-value
  "Wrap a domain failure-as-value (admission rejection, diagnostic row, ...)."
  ([value] (failed-value value nil))
  ([value evidence] {:result :failed-value :value value :evidence evidence}))

(defn skipped
  "An explicit local skip a graph could not express as a branch."
  []
  {:result :skipped :value nil})

;; ---- Graph validation ----------------------------------------------------

(defn- node-deps
  "Declared dependency keys of a node, regardless of kind."
  [node]
  (case (:kind node)
    :value  []
    :leaf   (vec (:deps node))
    :branch [(:cond node) (:then node) (:else node)]))

(defn- find-cycle
  "Return a cycle path [k … k] if the declared dependency edges contain one,
   else nil. Deterministic depth-first three-colour walk."
  [graph]
  (let [state  (atom {})   ; k -> :visiting | :done
        result (atom nil)]
    (letfn [(visit [k path]
              (when (nil? @result)
                (case (@state k)
                  :done nil
                  :visiting (reset! result
                                    (conj (vec (drop-while #(not= % k) path)) k))
                  (do (swap! state assoc k :visiting)
                      (doseq [d (filter graph (node-deps (graph k)))]
                        (visit d (conj path k)))
                      (swap! state assoc k :done)))))]
      (doseq [k (keys graph)] (visit k []))
      @result)))

(defn validate-graph
  "Return nil when the graph is a valid DAG whose every referenced dependency
   exists; otherwise a non-empty vector of error maps."
  [graph]
  (let [missing (vec (for [[k node] graph
                           d (node-deps node)
                           :when (not (contains? graph d))]
                       {:type :missing-dependency :node k :dep d}))
        cyc     (find-cycle graph)
        errors  (cond-> missing cyc (conj {:type :cycle :cycle cyc}))]
    (when (seq errors) errors)))
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cd abc && bin/kaocha --focus abc.tools.workflow.target-test`
Expected: PASS — 4 tests, 0 failures.

- [ ] **Step 5: Commit**

```bash
git add abc/src/abc/tools/workflow/target.clj abc/test/abc/tools/workflow/target_test.clj
git commit -m "feat(workflow): add target-graph data model and deterministic validation"
```

---

### Task 2: Lazy evaluator with branch-skip and DAG provenance

**Files:**
- Modify: `abc/src/abc/tools/workflow/target.clj` (append `eval-target` and its private helpers)
- Test: `abc/test/abc/tools/workflow/target_test.clj` (append evaluation tests)

**Interfaces:**
- Consumes: `value-node`, `leaf`, `branch`, `passed`, `failed-value`, `validate-graph` (Task 1).
- Produces:
  - `(eval-target graph target inputs)` and `(eval-target graph target inputs opts)` →
    `{:value <realized target value> :nodes [node-summary …] :edges [[from-key to-key] …]}`.
  - A node-summary is `{:key k :node_type "value"|"leaf"|"branch" :status "passed"|"partial"|"skipped" :realized bool :inputs [dep-key …]}`, and for a branch additionally `:conditional_inputs_skipped [dep-key …]`.
  - `eval-target` throws `clojure.lang.ExceptionInfo` with `:type :graph-validation-failure` on an invalid graph, `:type :graph-construction-failure` on an unknown target or a `:value` node with no supplied input.

- [ ] **Step 1: Write the failing evaluation tests**

Append to `abc/test/abc/tools/workflow/target_test.clj`:

```clojure
(deftest eval-realizes-leaf-from-value-node-test
  (let [g {:x (target/value-node)
           :a (target/leaf [:x] :a (fn [m] (* 10 (:x m))))}
        {:keys [value nodes edges]} (target/eval-target g :a {:x 5})]
    (is (= 50 value))
    (is (contains? (set edges) [:a :x]))
    (let [a (first (filter #(= :a (:key %)) nodes))]
      (is (= "leaf" (:node_type a)))
      (is (= "passed" (:status a)))
      (is (true? (:realized a)))
      (is (= [:x] (:inputs a))))))

(deftest eval-throws-on-unknown-target-test
  (is (thrown? clojure.lang.ExceptionInfo
               (target/eval-target {:a (target/value-node)} :missing {}))))

(deftest eval-throws-on-invalid-graph-test
  (is (thrown? clojure.lang.ExceptionInfo
               (target/eval-target {:a (target/leaf [:gone] :a (fn [_] 1))} :a {}))))

(deftest branch-does-not-realize-skipped-dependency-test
  (let [calls (atom #{})
        g {:c (target/value-node)
           :t (target/leaf [] :t (fn [_] (swap! calls conj :t) :then-val))
           :e (target/leaf [] :e (fn [_] (swap! calls conj :e) :else-val))
           :d (target/branch :c :t :e)}
        {:keys [value nodes]} (target/eval-target g :d {:c true})]
    (is (= :then-val value))
    (is (= #{:t} @calls) "the else leaf must not be realized")
    (let [e (first (filter #(= :e (:key %)) nodes))]
      (is (= "skipped" (:status e)))
      (is (false? (:realized e))))
    (let [d (first (filter #(= :d (:key %)) nodes))]
      (is (= [:e] (:conditional_inputs_skipped d))))))

(deftest failure-as-value-becomes-partial-node-test
  (let [g {:a (target/leaf [] :a (fn [_] (target/failed-value {:rejected "policy"})))}
        {:keys [value nodes]} (target/eval-target g :a {})]
    (is (= {:rejected "policy"} value))
    (is (= "partial" (:status (first (filter #(= :a (:key %)) nodes)))))))
```

- [ ] **Step 2: Run the new tests to verify they fail**

Run: `cd abc && bin/kaocha --focus abc.tools.workflow.target-test`
Expected: FAIL — `eval-target` is not defined (the four new tests error; the Task 1 tests still pass).

- [ ] **Step 3: Write the evaluator implementation**

Append to `abc/src/abc/tools/workflow/target.clj`:

```clojure
;; ---- Result normalisation ------------------------------------------------

(defn- normalize-result
  "Leaf fns may return a bare value (treated as :passed) or a result map."
  [r]
  (if (and (map? r) (contains? r :result))
    r
    {:result :passed :value r}))

(defn- result->status [{:keys [result]}]
  (case result
    :passed       "passed"
    :failed-value "partial"
    :skipped      "skipped"))

;; ---- Evaluation ----------------------------------------------------------

(defn eval-target
  "Realize `target` from `graph`. `inputs` supplies :value node values by key.
   Returns {:value v :nodes [summary …] :edges [[from to] …]}. Lazy: only the
   taken side of each branch is realized. Provenance is order-independent."
  ([graph target inputs] (eval-target graph target inputs {}))
  ([graph target inputs _opts]
   (when-let [errs (validate-graph graph)]
     (throw (ex-info "workflow target graph invalid"
                     {:type :graph-validation-failure :errors errs})))
   (when-not (contains? graph target)
     (throw (ex-info "unknown workflow target"
                     {:type :graph-construction-failure :target target})))
   (let [memo      (atom {})     ; k -> {:value v :status s :deps [...] :skipped-side k?}
         edges     (atom #{})    ; #{[from to]}
         skip-cand (atom #{})]   ; branch sides not taken
     (letfn [(realize [k]
               (or (@memo k)
                   (let [node  (graph k)
                         entry (case (:kind node)
                                 :value
                                 (do (when-not (contains? inputs k)
                                       (throw (ex-info "missing input for value node"
                                                       {:type :graph-construction-failure
                                                        :node k})))
                                     {:value (get inputs k) :status "passed" :deps []})

                                 :leaf
                                 (let [deps     (vec (:deps node))
                                       dep-vals (into {} (map (fn [d] [d (:value (realize d))])) deps)
                                       r        (normalize-result
                                                 ((get-in node [:impl :impl/fn]) dep-vals))]
                                   (doseq [d deps] (swap! edges conj [k d]))
                                   {:value (:value r) :status (result->status r) :deps deps})

                                 :branch
                                 (let [{ck :cond tk :then ek :else} node
                                       c     (realize ck)
                                       taken (if (:value c) tk ek)
                                       other (if (:value c) ek tk)
                                       t     (realize taken)]
                                   (swap! edges conj [k ck])
                                   (swap! edges conj [k taken])
                                   (swap! skip-cand conj other)
                                   {:value (:value t) :status (:status t)
                                    :deps [ck taken] :skipped-side other}))]
                     (swap! memo assoc k entry)
                     entry)))]
       (realize target)
       (let [memoed        @memo
             realized-keys (set (keys memoed))
             skipped-keys  (set/difference @skip-cand realized-keys)
             realized-summaries
             (for [[k entry] memoed]
               (cond-> {:key k
                        :node_type (name (:kind (graph k)))
                        :status (:status entry)
                        :realized true
                        :inputs (vec (:deps entry))}
                 (:skipped-side entry)
                 (assoc :conditional_inputs_skipped
                        (vec (filter skipped-keys [(:skipped-side entry)])))))
             skipped-summaries
             (for [k skipped-keys]
               {:key k
                :node_type (name (:kind (graph k)))
                :status "skipped"
                :realized false
                :inputs []})]
         {:value (:value (memoed target))
          :nodes (vec (concat realized-summaries skipped-summaries))
          :edges (vec @edges)})))))
```

- [ ] **Step 4: Run the full evaluator test suite to verify it passes**

Run: `cd abc && bin/kaocha --focus abc.tools.workflow.target-test`
Expected: PASS — 8 tests, 0 failures (4 from Task 1, 4 new).

- [ ] **Step 5: Commit**

```bash
git add abc/src/abc/tools/workflow/target.clj abc/test/abc/tools/workflow/target_test.clj
git commit -m "feat(workflow): lazy target-graph evaluator with branch-skip and DAG provenance"
```

---

## Slice 1 Done Criteria

After Tasks 1–2, the following spec Acceptance Criteria are met and demonstrable:

- the probe evaluates a branch without realizing the skipped dependency (`branch-does-not-realize-skipped-dependency-test`);
- the same node can be evaluated with supplied values and no side effects (all tests use pure `impl-fn`s and an `inputs` map);
- Soranoha-owned validation catches a missing dependency and a cycle, with a documented mutually-exclusive-branch fixture that does not false-positive;
- provenance is a node-set plus edge-list with no execution order;
- the node status vocabulary matches `passed`/`partial`/`skipped`;
- no dependency or lockfile change.

Run the whole `abc` unit suite once before finishing to confirm nothing else regressed:

```bash
cd abc && bin/kaocha
```
Expected: all suites pass.

---

## Follow-up Plans (Slices 2–4 — write each as its own plan when Slice 1 lands)

These slices are deliberately **not** expanded into code steps here: each touches a different subsystem whose exact fixture, schema, and helper shapes should be pinned against the code at the time it is built, not guessed now. Each becomes its own dated plan under `abc/docs/superpowers/plans/`. Interfaces below are the contract each will consume/produce.

### Slice 2 — Workflow report integration
**Deliverable:** the first target runs through `abc.tools.workflow/run-workflow!` as one step, and node summaries are written to a bounded sidecar.
- Add `abc/schemas/workflow-nodes.schema.json` (mirror to `ab-validator/data/abc-schemas/nix-schemas/` as the existing schema does), plus valid/invalid fixtures under `abc/examples/workflow/` and `abc/fixtures/v0/invalid/`.
- Add one bounded, versioned, **optional** field to `abc/schemas/workflow-run.schema.json` (a `node_summary_ref` + counts block) — an additive change that keeps `additionalProperties: false` intact; update the passing fixture accordingly.
- Consumes `eval-target`'s `{:value :nodes :edges}` result; produces `workflow-nodes.jsonl` referenced by `run_id`.
- **Guardrail:** `workflow-run.json` must stay schema-valid and bounded (no unbounded node array embedded). Reuse the existing `abc.tools.schema`/`abc.tools.files` test pattern in `abc/test/abc/tools/workflow_test.clj`.

### Slice 3 — Nix bridge leaf
**Deliverable:** one leaf realizes an explicit flake app/check/package via a Soranoha bridge and returns the structured value from the spec (`:store-path :flake-output :lock-nodes :outputs :messages`).
- No node key or branch result may name a Nix attribute without passing through a Soranoha naming function.
- Confirm no full-corpus attrset enumeration is introduced.

### Slice 4 — Cache and query
**Deliverable:** pure node-value cache + read-time validity, kept separate from materialization validity.
- Implement the pure node-value cache key from the spec's Cache Model (a); leaf identity = declared `:impl/id` + a **source/content hash** (compute via the existing `abc.tools.hash` namespace), with a test asserting stability across clj-nix and REPL execution.
- Implement `(valid-cached-node-result cached-node-result current-env)` → `{:status :ok|:stale|:invalid …}`, called on every cache read; re-hash path claims, never trust a bare path.
- For Nix-bridge leaves use the materialization validity check (b), not a second value cache.
- Add a small derived query index answering "why did this target realize these nodes?" from the JSONL sidecar.

---

## Self-Review

- **Spec coverage (Slice 1):** internal evaluator data model ✓ (Task 1), deterministic missing-dep + cycle validation with mutually-exclusive-branch fixture ✓ (Task 1), lazy branch-skip ✓ (Task 2), DAG (node-set + edge-list) provenance ✓ (Task 2), status vocabulary ✓ (Task 2), failure-as-value → partial ✓ (Task 2), no-dependency constraint ✓ (Global Constraints). Slices 2–4 spec sections are mapped to Follow-up Plans with consume/produce interfaces.
- **Placeholder scan:** every code step contains complete, runnable code; the only non-code sections (Follow-up Plans) are explicitly scoped out with contracts, not TODOs inside an executable task.
- **Type consistency:** `value-node`/`leaf`/`branch`/`passed`/`failed-value`/`skipped`/`validate-graph` (Task 1) are used verbatim by Task 2; `eval-target` returns `{:value :nodes :edges}` with the node-summary keys (`:key :node_type :status :realized :inputs :conditional_inputs_skipped`) asserted identically in the Task 2 tests and consumed unchanged by Slice 2.
