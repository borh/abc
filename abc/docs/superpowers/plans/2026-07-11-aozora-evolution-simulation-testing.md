# Aozora Evolution Simulation Testing Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the model-based generative simulation harness specified in
`abc/docs/superpowers/specs/2026-07-11-aozora-evolution-simulation-testing-design.md`
(Phases 1–2): synthetic upstream catalog histories with known ground truth,
driven through the real ingest/diff/audit tools and checked against injected
intent.

**Architecture:** Three test-only units under `abc/test/abc/sim/`: a pure
ground-truth model with a total event algebra (`abc.sim.model`), renderers
from model states to CSV/ZIP/git/corpus-dir formats (`abc.sim.render`), and
test.check generators plus property namespaces split by layer
(`abc.sim.gen`, `abc.sim.classifier-sim-test`, `abc.sim.ingest-sim-test`,
`abc.sim.audit-sim-test`). Oracle utilities (projection, confusability,
model diff, report normalization) live in `abc.sim.oracle`. A
known-divergences table (`abc.sim.divergences`) gates desired-behavior
assertions that current code fails.

**Tech Stack:** Clojure, `org.clojure/test.check 1.1.3` (already a `:test`
dep), kaocha 1.91.1392, charred (CSV), JGit via existing `abc.git`, malli
schema hashing via existing `abc.tools.malli`.

## Global Constraints

- All new code lives under `abc/test/` — this plan makes **no change under
  `abc/src/`**. Bug fixes for divergences D1–D6 are out of scope.
- Properties assert *desired* behavior; divergence-linked cases go through
  `abc.sim.divergences/expected-failure`, never by weakening a property.
- Pending/expected-failure markers are per property case (e.g.
  `P6.divergent-work-fields`), never per numbered property.
- CI seeds are exactly `[42 4242 424242]`; soak mode is
  `ABC_SIM_SOAK=1` at 15× counts.
- Simulation suite budget: ≤ ~2 minutes JVM time (≈50 cases per pure-layer
  property and ≈15 per git-layer property, split across the 3 seeds).
- The `:unit` kaocha suite must keep running exactly the namespaces it runs
  today.
- All test commands run from the `abc/` directory:
  `clojure -M:test:kaocha -m kaocha.runner --focus <suite>`.
- Commit after every task; message prefix `test(sim):` (or `build:` for
  tests.edn/justfile wiring).

---

### Task 1: Kaocha suite wiring and seed harness

**Files:**
- Modify: `abc/tests.edn`
- Create: `abc/test/abc/sim/harness.clj`
- Test: `abc/test/abc/sim/smoke_sim_test.clj`

**Interfaces:**
- Produces: `abc.sim.harness/check!` — `(check! name num-tests prop)` runs a
  test.check property across the CI seeds (or unseeded ×15 in soak mode) and
  fails the surrounding `deftest` with the shrunk counterexample.
- Produces: `abc.sim.harness/ci-seeds` (vector of 3 longs),
  `abc.sim.harness/soak?` (nullary), `abc.sim.harness/ratio-counter`
  (returns `(atom {:applied 0 :total 0})`),
  `abc.sim.harness/tick!` — `(tick! counter applied?)`,
  `abc.sim.harness/assert-applied-ratio!` — `(assert-applied-ratio! name counter)`
  asserts applied/total ≥ 9/10.
- Produces: kaocha suite id `:simulation` matching `-sim-test$` namespaces;
  `:unit` excludes them.

- [ ] **Step 1: Add the `:simulation` suite and exclude sim tests from `:unit`**

In `abc/tests.edn`, replace the single-suite `:tests` vector:

```clojure
 :tests [{:id :unit
          :source-paths ["src"]
          :test-paths ["test"]
          ;; negative lookbehind: plain *-test namespaces, NOT *-sim-test
          :ns-patterns ["(?<!-sim)-test$"]}
         {:id :simulation
          :source-paths ["src"]
          :test-paths ["test"]
          :ns-patterns ["-sim-test$"]}]
```

(Keep every other top-level key in `tests.edn` unchanged.)

- [ ] **Step 2: Write the failing smoke test**

Create `abc/test/abc/sim/smoke_sim_test.clj`:

```clojure
(ns abc.sim.smoke-sim-test
  "Suite wiring smoke test: proves the :simulation suite runs and the
  seed harness is deterministic."
  (:require [abc.sim.harness :as harness]
            [clojure.test :refer [deftest]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

(deftest harness-runs-seeded-property-sim-test
  (harness/check! "smoke" 20
                  (prop/for-all [v (gen/vector gen/small-integer)]
                    (= (count v) (count (vec v))))))
```

- [ ] **Step 3: Run to verify it fails (harness missing)**

Run (from `abc/`): `clojure -M:test:kaocha -m kaocha.runner --focus :simulation`
Expected: FAIL — namespace `abc.sim.harness` not found.

- [ ] **Step 4: Implement the harness**

Create `abc/test/abc/sim/harness.clj`:

```clojure
(ns abc.sim.harness
  "Seed discipline for the simulation suite: deterministic CI seeds,
  unseeded soak mode, and forced-event application-ratio tracking.
  Spec: docs/superpowers/specs/2026-07-11-aozora-evolution-simulation-testing-design.md"
  (:require [clojure.test :refer [is]]
            [clojure.test.check :as tc]))

(def ci-seeds
  "Checked-in seed corpus. Rotate entries when the soak run finds seeds
  exercising interesting histories; keep exactly this vector in CI."
  [42 4242 424242])

(def soak-factor 15)

(defn soak? []
  (= "1" (System/getenv "ABC_SIM_SOAK")))

(defn check!
  "Run `prop` with test.check. CI mode: once per seed in `ci-seeds`,
  `num-tests` cases each. Soak mode (ABC_SIM_SOAK=1): one unseeded run at
  soak-factor × num-tests. Reports the shrunk counterexample and the seed
  needed to replay it."
  [name num-tests prop]
  (if (soak?)
    (let [{:keys [pass? seed shrunk]} (tc/quick-check (* soak-factor num-tests) prop)]
      (is pass? (str name " (soak; replay with seed " seed "): "
                     (pr-str (:smallest shrunk)))))
    (doseq [seed ci-seeds]
      (let [{:keys [pass? shrunk]} (tc/quick-check num-tests prop :seed seed)]
        (is pass? (str name " (seed " seed "): "
                       (pr-str (:smallest shrunk))))))))

(defn ratio-counter []
  (atom {:applied 0 :total 0}))

(defn tick! [counter applied?]
  (swap! counter (fn [c] (-> c
                             (update :total inc)
                             (cond-> applied? (update :applied inc)))))
  applied?)

(defn assert-applied-ratio!
  "Acceptance criterion: forced events must actually apply in ≥ 90% of
  generated cases, so intent assertions are non-vacuous."
  [name counter]
  (let [{:keys [applied total]} @counter]
    (is (and (pos? total) (>= (/ applied total) 9/10))
        (str name ": forced-event applied ratio " applied "/" total))))
```

- [ ] **Step 5: Run the simulation suite; verify it passes**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation`
Expected: PASS, 1 test.

- [ ] **Step 6: Verify the unit suite is unchanged**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :unit`
Expected: PASS with the same test count as on the parent commit (the smoke
namespace must NOT appear). Compare against
`git stash`-free baseline by running the same command on HEAD~ if unsure.

- [ ] **Step 7: Commit**

```bash
git add abc/tests.edn abc/test/abc/sim/harness.clj abc/test/abc/sim/smoke_sim_test.clj
git commit -m "build(sim): add :simulation kaocha suite and seed harness"
```

---

### Task 2: Model core — state, invariants, benign events

**Files:**
- Create: `abc/test/abc/sim/model.clj`
- Test: `abc/test/abc/sim/model_test.clj` (runs in the `:unit` suite — the
  harness's own unit tests are deterministic)

**Interfaces:**
- Produces (all in `abc.sim.model`):
  - Model shape: `{:persons {pid fields} :works {wid fields} :edges {[wid relation] #{pid}} :next-id long}`
    where `pid`/`wid` are 6-digit strings and `fields` are keyword maps (see
    `base-person` / `base-work`).
  - `base-person : nil → fields`, `base-work : wid → fields` — schema-safe
    field templates.
  - `bootstrap : n-works → model` — n works, one sole author each.
  - `fresh-pid : model → pid`, `fresh-wid : model → wid` (read `:next-id`;
    `apply-event` advances it).
  - `apply-event : model × event → {:model model' :applied intent-or-nil}` —
    TOTAL; precondition failure returns the model unchanged, `:applied nil`.
  - `check-invariants! : model × event → nil-or-throw`.
  - `fold-history : {:initial model :events [event]} → {:states [model] :applied [intent]}`.
  - Intent shape: `{:intent <event-type> :event event :edges [[wid relation] ...]}`.
  - Benign event types (maps with `:event/type`):
    `:add-work-with-edge {:wid :work :pid :person :relation}`,
    `:add-person-with-edge {:pid :person :wid :relation}`,
    `:add-person {:pid :person}`, `:add-work {:wid :work}`,
    `:add-edge {:wid :relation :pid}`, `:remove-edge {:wid :relation :pid}`,
    `:edit-person {:pid :field :value}`, `:edit-work {:wid :field :value}`,
    `:remove-work {:wid}`.

- [ ] **Step 1: Write the failing tests**

Create `abc/test/abc/sim/model_test.clj`:

```clojure
(ns abc.sim.model-test
  (:require [abc.sim.model :as model]
            [clojure.test :refer [deftest is testing]]))

(deftest bootstrap-shape-test
  (let [m (model/bootstrap 3)]
    (is (= 3 (count (:works m))))
    (is (= 3 (count (:persons m))))
    (is (= 3 (count (:edges m))))
    (is (nil? (model/check-invariants! m nil)))))

(deftest benign-events-apply-and-no-op-test
  (let [m (model/bootstrap 1)
        wid (first (keys (:works m)))
        pid (first (keys (:persons m)))]
    (testing "add-edge applies"
      (let [e {:event/type :add-edge :wid wid :relation "翻訳者" :pid pid}
            {m' :model intent :applied} (model/apply-event m e)]
        (is (= :add-edge (:intent intent)))
        (is (contains? (:edges m') [wid "翻訳者"]))))
    (testing "add-edge no-ops when pid already on the edge"
      (let [rel (second (first (keys (:edges m))))
            e {:event/type :add-edge :wid wid :relation rel :pid pid}
            {m' :model intent :applied} (model/apply-event m e)]
        (is (nil? intent))
        (is (= m m'))))
    (testing "remove-edge deletes an emptied edge and may orphan the person"
      (let [[k pids] (first (:edges m))
            e {:event/type :remove-edge :wid (first k) :relation (second k)
               :pid (first pids)}
            {m' :model intent :applied} (model/apply-event m e)]
        (is (some? intent))
        (is (not (contains? (:edges m') k)))
        ;; person survives in the model (projection drops it later)
        (is (contains? (:persons m') (first pids)))
        (is (nil? (model/check-invariants! m' e)))))
    (testing "edit-person changes one field"
      (let [e {:event/type :edit-person :pid pid :field :family_name :value "改"}
            {m' :model intent :applied} (model/apply-event m e)]
        (is (some? intent))
        (is (= "改" (get-in m' [:persons pid :family_name])))))
    (testing "remove-work drops the work and its edges"
      (let [e {:event/type :remove-work :wid wid}
            {m' :model intent :applied} (model/apply-event m e)]
        (is (some? intent))
        (is (not (contains? (:works m') wid)))
        (is (not-any? #(= wid (first %)) (keys (:edges m'))))))))

(deftest fold-history-collects-applied-intents-test
  (let [m (model/bootstrap 1)
        wid (first (keys (:works m)))
        pid (first (keys (:persons m)))
        events [{:event/type :edit-person :pid pid :field :given_name :value "二"}
                ;; stale event: same value again -> no-op after the first applies
                {:event/type :edit-person :pid pid :field :given_name :value "二"}
                {:event/type :remove-work :wid wid}]
        {:keys [states applied]} (model/fold-history {:initial m :events events})]
    (is (= 4 (count states)))
    (is (= [:edit-person :remove-work] (mapv :intent applied)))))
```

- [ ] **Step 2: Run to verify failure**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.model-test`
Expected: FAIL — `abc.sim.model` not found.

- [ ] **Step 3: Implement the model core**

Create `abc/test/abc/sim/model.clj`:

```clojure
(ns abc.sim.model
  "Ground-truth upstream model and total event algebra for the aozora
  evolution simulation harness. Pure. The oracle reads applied intents;
  inapplicable events are recorded no-ops so sequence shrinking always
  yields valid histories.
  Spec: docs/superpowers/specs/2026-07-11-aozora-evolution-simulation-testing-design.md"
  (:require [clojure.string :as string]))

(def person-fields
  [:family_name :given_name :family_name_reading :given_name_reading
   :family_name_sort :given_name_sort :family_name_romaji :given_name_romaji
   :date_of_birth :date_of_death :copyright_expired])

(def work-fields
  [:title :title_reading :sort_reading :ndc :orthography :copyright_expired
   :available :modified :edition_title :edition_publisher])

(defn base-person []
  {:family_name "旧" :given_name "人"
   :family_name_reading "きゅう" :given_name_reading "ひと"
   :family_name_sort "きゆう" :given_name_sort "ひと"
   :family_name_romaji "Old" :given_name_romaji "Person"
   :date_of_birth "1900-01-01" :date_of_death "1970-01-01"
   :copyright_expired true})

(defn base-work [wid]
  {:title (str "作品" wid) :title_reading "てすとさくひん"
   :sort_reading "てすとさくひん" :ndc "NDC 913" :orthography "新字新仮名"
   :copyright_expired true :available "1997-10-29" :modified "2022-07-16"
   :edition_title "テスト作品" :edition_publisher "テスト出版社"})

(defn- fmt6 [n] (format "%06d" n))

(defn fresh-pid [m] (fmt6 (+ 900000 (:next-id m))))
(defn fresh-wid [m] (fmt6 (+ 800000 (:next-id m))))

(defn bootstrap
  "n works numbered 000101.., each with a distinct sole author 000001..,
  relation 著者."
  [n]
  {:persons (into (sorted-map)
                  (for [i (range 1 (inc n))]
                    [(fmt6 i) (assoc (base-person) :given_name (str "人" i))]))
   :works (into (sorted-map)
                (for [i (range 1 (inc n))] [(fmt6 (+ 100 i)) (base-work (fmt6 (+ 100 i)))]))
   :edges (into (sorted-map)
                (for [i (range 1 (inc n))] [[(fmt6 (+ 100 i)) "著者"] #{(fmt6 i)}]))
   :next-id 1})

(defn check-invariants!
  "Throws ex-info on violation; nil otherwise. Harness defect if it fires."
  [m event]
  (doseq [[[wid rel] pids] (:edges m)]
    (when (or (not (contains? (:works m) wid))
              (string/blank? rel)
              (empty? pids)
              (not-every? #(contains? (:persons m) %) pids))
      (throw (ex-info "model invariant violated"
                      {:edge [wid rel] :pids pids :event event}))))
  nil)

(defn- no-op [m] {:model m :applied nil})

(defn- applied [m' e edge-keys]
  {:model (update m' :next-id inc)
   :applied {:intent (:event/type e) :event e :edges (vec edge-keys)}})

(defmulti apply-event* (fn [_m e] (:event/type e)))

(defmethod apply-event* :add-work-with-edge
  [m {:keys [wid work pid person relation] :as e}]
  (if (or (contains? (:works m) wid) (contains? (:persons m) pid))
    (no-op m)
    (applied (-> m
                 (assoc-in [:works wid] work)
                 (assoc-in [:persons pid] person)
                 (assoc-in [:edges [wid relation]] #{pid}))
             e [[wid relation]])))

(defmethod apply-event* :add-person-with-edge
  [m {:keys [pid person wid relation] :as e}]
  (if (or (contains? (:persons m) pid) (not (contains? (:works m) wid)))
    (no-op m)
    (applied (-> m
                 (assoc-in [:persons pid] person)
                 (update-in [:edges [wid relation]] (fnil conj #{}) pid))
             e [[wid relation]])))

(defmethod apply-event* :add-person
  [m {:keys [pid person] :as e}]
  (if (contains? (:persons m) pid)
    (no-op m)
    (applied (assoc-in m [:persons pid] person) e [])))

(defmethod apply-event* :add-work
  [m {:keys [wid work] :as e}]
  (if (contains? (:works m) wid)
    (no-op m)
    (applied (assoc-in m [:works wid] work) e [])))

(defmethod apply-event* :add-edge
  [m {:keys [wid relation pid] :as e}]
  (if (or (not (contains? (:works m) wid))
          (not (contains? (:persons m) pid))
          (contains? (get-in m [:edges [wid relation]] #{}) pid))
    (no-op m)
    (applied (update-in m [:edges [wid relation]] (fnil conj #{}) pid)
             e [[wid relation]])))

(defmethod apply-event* :remove-edge
  [m {:keys [wid relation pid] :as e}]
  (let [k [wid relation]
        pids (get-in m [:edges k] #{})]
    (if-not (contains? pids pid)
      (no-op m)
      (let [pids' (disj pids pid)
            m' (if (empty? pids')
                 (update m :edges dissoc k)
                 (assoc-in m [:edges k] pids'))]
        (applied m' e [k])))))

(defmethod apply-event* :edit-person
  [m {:keys [pid field value] :as e}]
  (if (or (not (contains? (:persons m) pid))
          (= value (get-in m [:persons pid field])))
    (no-op m)
    (applied (assoc-in m [:persons pid field] value) e [])))

(defmethod apply-event* :edit-work
  [m {:keys [wid field value] :as e}]
  (if (or (not (contains? (:works m) wid))
          (= value (get-in m [:works wid field])))
    (no-op m)
    (applied (assoc-in m [:works wid field] value) e [])))

(defmethod apply-event* :remove-work
  [m {:keys [wid] :as e}]
  (if-not (contains? (:works m) wid)
    (no-op m)
    (let [edge-keys (filter #(= wid (first %)) (keys (:edges m)))]
      (applied (-> m
                   (update :works dissoc wid)
                   (update :edges #(apply dissoc % edge-keys)))
               e edge-keys))))

(defmethod apply-event* :default [m _e] (no-op m))

(defn apply-event [m e] (apply-event* m e))

(defn fold-history
  [{:keys [initial events]}]
  (reduce (fn [{:keys [states applied]} event]
            (let [m (peek states)
                  {m' :model intent :applied} (apply-event m event)]
              (check-invariants! m' event)
              {:states (conj states m')
               :applied (if intent (conj applied intent) applied)}))
          {:states [initial] :applied []}
          events))
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.model-test`
Expected: PASS (3 tests).

- [ ] **Step 5: Commit**

```bash
git add abc/test/abc/sim/model.clj abc/test/abc/sim/model_test.clj
git commit -m "test(sim): ground-truth model, invariants, benign events"
```

---

### Task 3: Drift events

**Files:**
- Modify: `abc/test/abc/sim/model.clj` (append `apply-event*` methods)
- Modify: `abc/test/abc/sim/model_test.clj` (append tests)

**Interfaces:**
- Produces (event types added to `abc.sim.model`):
  - `:clean-split {:pid :targets [pid...] :persons {pid fields}}` —
    precondition: `pid` exists, has ≥ 1 edge, and is the SOLE contributor on
    every edge containing it; ≥ 2 targets, all absent.
  - `:clean-merge {:pids [pid...] :target pid :person fields}` —
    precondition: ≥ 2 sources, every edge touching any source has contributor
    set exactly `(set pids)`; target absent.
  - `:ambiguous-replacement {:pid :target :person fields}` — precondition:
    `pid` exists with ≥ 1 edge; target absent.
  - `:impure-split {:pid :existing-target :new-target :person fields}` —
    precondition: like clean-split (sole contributor) plus `existing-target`
    already present and distinct from `pid`; `new-target` absent.
  - `:partial-split {:pid :targets [pid...] :edge-keys [[wid rel]...] :persons {pid fields}}` —
    precondition: `pid` retained; `edge-keys` a strict, non-empty, proper
    subset of `pid`'s edges; targets absent.
- Produces: `abc.sim.model/edges-of : model × pid → [[wid relation] ...]`,
  `abc.sim.model/sole-contributor? : model × pid → boolean`.

- [ ] **Step 1: Write the failing tests (append to `model_test.clj`)**

```clojure
(deftest clean-split-test
  (let [m (model/bootstrap 2)
        pid "000001"
        e {:event/type :clean-split :pid pid :targets ["900001" "900002"]
           :persons {"900001" (assoc (model/base-person) :given_name "一")
                     "900002" (assoc (model/base-person) :given_name "二")}}
        {m' :model intent :applied} (model/apply-event m e)]
    (is (= :clean-split (:intent intent)))
    (is (not (contains? (:persons m') pid)))
    (is (= #{"900001" "900002"} (get-in m' [:edges ["000101" "著者"]])))
    (is (nil? (model/check-invariants! m' e)))
    (testing "no-op when source has a co-contributor"
      (let [m2 (:model (model/apply-event m {:event/type :add-edge :wid "000101"
                                             :relation "著者" :pid "000002"}))
            r (model/apply-event m2 e)]
        (is (nil? (:applied r)))
        (is (= m2 (:model r)))))))

(deftest clean-merge-test
  (let [m0 (model/bootstrap 1)
        ;; build an edge whose full contributor set is the two sources
        m1 (:model (model/apply-event m0 {:event/type :add-person-with-edge
                                          :pid "000002"
                                          :person (model/base-person)
                                          :wid "000101" :relation "著者"}))
        e {:event/type :clean-merge :pids ["000001" "000002"] :target "900001"
           :person (assoc (model/base-person) :given_name "合")}
        {m' :model intent :applied} (model/apply-event m1 e)]
    (is (= :clean-merge (:intent intent)))
    (is (= #{"900001"} (get-in m' [:edges ["000101" "著者"]])))
    (is (not-any? #(contains? (:persons m') %) ["000001" "000002"]))))

(deftest partial-split-retains-source-test
  (let [m0 (model/bootstrap 1)
        m1 (:model (model/apply-event m0 {:event/type :add-edge :wid "000101"
                                          :relation "翻訳者" :pid "000001"}))
        e {:event/type :partial-split :pid "000001" :targets ["900001"]
           :edge-keys [["000101" "翻訳者"]]
           :persons {"900001" (model/base-person)}}
        {m' :model intent :applied} (model/apply-event m1 e)]
    (is (some? intent))
    (is (contains? (:persons m') "000001"))
    (is (= #{"900001"} (get-in m' [:edges ["000101" "翻訳者"]])))
    (is (= #{"000001"} (get-in m' [:edges ["000101" "著者"]])))
    (testing "no-op when edge-keys are ALL of the source's edges"
      (let [e-all (assoc e :edge-keys [["000101" "著者"] ["000101" "翻訳者"]])]
        (is (nil? (:applied (model/apply-event m1 e-all))))))))

(deftest impure-and-ambiguous-test
  (let [m (model/bootstrap 2)]
    (testing "impure-split reuses an existing person as one successor"
      (let [e {:event/type :impure-split :pid "000001"
               :existing-target "000002" :new-target "900001"
               :person (model/base-person)}
            {m' :model intent :applied} (model/apply-event m e)]
        (is (some? intent))
        (is (= #{"000002" "900001"} (get-in m' [:edges ["000101" "著者"]])))))
    (testing "ambiguous-replacement swaps 1→1"
      (let [e {:event/type :ambiguous-replacement :pid "000001"
               :target "900001" :person (model/base-person)}
            {m' :model intent :applied} (model/apply-event m e)]
        (is (some? intent))
        (is (= #{"900001"} (get-in m' [:edges ["000101" "著者"]])))
        (is (not (contains? (:persons m') "000001")))))))
```

- [ ] **Step 2: Run to verify failure**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.model-test`
Expected: FAIL — `:default` method no-ops the drift events, so intents are nil.

- [ ] **Step 3: Implement drift events (append to `model.clj`, before the `:default` method)**

```clojure
(defn edges-of [m pid]
  (vec (for [[k pids] (:edges m) :when (contains? pids pid)] k)))

(defn sole-contributor?
  "True when pid has ≥1 edge and every edge containing pid is exactly #{pid}."
  [m pid]
  (let [ks (edges-of m pid)]
    (and (seq ks)
         (every? #(= #{pid} (get-in m [:edges %])) ks))))

(defn- rewrite-edges [m edge-keys f]
  (reduce (fn [m k] (update-in m [:edges k] f)) m edge-keys))

(defmethod apply-event* :clean-split
  [m {:keys [pid targets persons] :as e}]
  (if (or (< (count targets) 2)
          (not (sole-contributor? m pid))
          (some #(contains? (:persons m) %) targets))
    (no-op m)
    (let [ks (edges-of m pid)]
      (applied (-> m
                   (update :persons dissoc pid)
                   (update :persons merge persons)
                   (rewrite-edges ks (constantly (set targets))))
               e ks))))

(defmethod apply-event* :clean-merge
  [m {:keys [pids target person] :as e}]
  (let [srcs (set pids)
        ks (distinct (mapcat #(edges-of m %) pids))]
    (if (or (< (count srcs) 2)
            (contains? (:persons m) target)
            (not-every? #(contains? (:persons m) %) pids)
            (empty? ks)
            (not-every? #(= srcs (get-in m [:edges %])) ks))
      (no-op m)
      (applied (-> m
                   (update :persons #(apply dissoc % pids))
                   (assoc-in [:persons target] person)
                   (rewrite-edges ks (constantly #{target})))
               e ks))))

(defmethod apply-event* :ambiguous-replacement
  [m {:keys [pid target person] :as e}]
  (let [ks (edges-of m pid)]
    (if (or (empty? ks)
            (contains? (:persons m) target)
            (not (contains? (:persons m) pid)))
      (no-op m)
      (applied (-> m
                   (update :persons dissoc pid)
                   (assoc-in [:persons target] person)
                   (rewrite-edges ks #(-> % (disj pid) (conj target))))
               e ks))))

(defmethod apply-event* :impure-split
  [m {:keys [pid existing-target new-target person] :as e}]
  (if (or (not (sole-contributor? m pid))
          (not (contains? (:persons m) existing-target))
          (= pid existing-target)
          (contains? (:persons m) new-target))
    (no-op m)
    (let [ks (edges-of m pid)]
      (applied (-> m
                   (update :persons dissoc pid)
                   (assoc-in [:persons new-target] person)
                   (rewrite-edges ks (constantly #{existing-target new-target})))
               e ks))))

(defmethod apply-event* :partial-split
  [m {:keys [pid targets edge-keys persons] :as e}]
  (let [all (set (edges-of m pid))
        chosen (set edge-keys)]
    (if (or (empty? chosen)
            (not (contains? (:persons m) pid))
            (some #(contains? (:persons m) %) targets)
            (empty? targets)
            (not (and (clojure.set/subset? chosen all) (< (count chosen) (count all)))))
      (no-op m)
      (applied (-> m
                   (update :persons merge persons)
                   (rewrite-edges chosen #(-> % (disj pid) (into targets))))
               e (vec chosen)))))
```

Add `[clojure.set]` to the ns `:require` as `(:require [clojure.set] [clojure.string :as string])`.

- [ ] **Step 4: Run tests to verify they pass**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.model-test`
Expected: PASS (7 tests).

- [ ] **Step 5: Commit**

```bash
git add abc/test/abc/sim/model.clj abc/test/abc/sim/model_test.clj
git commit -m "test(sim): drift events with edge-exhaustive preconditions"
```

---

### Task 4: Oracle utilities — projection, confusability, model diff, report normalization

**Files:**
- Create: `abc/test/abc/sim/oracle.clj`
- Test: `abc/test/abc/sim/oracle_test.clj`

**Interfaces:**
- Consumes: `abc.sim.model` model shape and event types.
- Produces (all in `abc.sim.oracle`):
  - `projection : model → model` — restrict to works/persons on ≥ 1 edge.
  - `confusable? : prev-model × cur-model → boolean` — endpoint-state
    predicate: some edge diff is candidate-shaped (1→many or many→1
    replacement backed by globally removed/added pids), computed on
    projections.
  - `model-diff : prev-model × cur-model → {:added-pids #{} :removed-pids #{} :corrected-pids #{} :edge-counts {:additions n :removals n :replacements n}}`
    over projections (P4 accounting oracle).
  - `semantic-report : report → report` — recursively remove filesystem
    locator keys: `"previous_dir" "current_dir" :previous_dir :current_dir
    :aozora-repo :work-dir :corpus-dirs :extracted-zips :ingest "input_dir"
    :input-dir "input-dir"`.
  - `expected-split-candidates : intent → [{"work_id" .. "relation_to_work" .. "source_person_ids" [..] "target_person_ids" [..]}]`
    (edge-local, one entry per rewritten edge; same for
    `expected-merge-candidates`).

- [ ] **Step 1: Write the failing tests**

Create `abc/test/abc/sim/oracle_test.clj`:

```clojure
(ns abc.sim.oracle-test
  (:require [abc.sim.model :as model]
            [abc.sim.oracle :as oracle]
            [clojure.test :refer [deftest is testing]]))

(deftest projection-drops-unattached-test
  (let [m (model/bootstrap 1)
        m' (:model (model/apply-event m {:event/type :add-person :pid "900001"
                                         :person (model/base-person)}))
        m'' (:model (model/apply-event m' {:event/type :add-work :wid "800001"
                                           :work (model/base-work "800001")}))
        p (oracle/projection m'')]
    (is (not (contains? (:persons p) "900001")))
    (is (not (contains? (:works p) "800001")))
    (is (= (:edges m'') (:edges p)))))

(deftest confusable-detects-split-shaped-benign-diff-test
  (let [m (model/bootstrap 1)
        ;; benign composition equivalent to a split: remove person A's edge +
        ;; work, then new persons take the same [work relation] key over.
        steps [{:event/type :remove-edge :wid "000101" :relation "著者" :pid "000001"}
               {:event/type :add-person-with-edge :pid "900001"
                :person (model/base-person) :wid "000101" :relation "著者"}
               {:event/type :add-person-with-edge :pid "900002"
                :person (model/base-person) :wid "000101" :relation "著者"}]
        {:keys [states]} (model/fold-history {:initial m :events steps})]
    (is (oracle/confusable? (first states) (peek states)))
    (is (not (oracle/confusable? m m)))))

(deftest model-diff-counts-test
  (let [m (model/bootstrap 2)
        {:keys [states]} (model/fold-history
                          {:initial m
                           :events [{:event/type :edit-person :pid "000001"
                                     :field :family_name :value "改"}
                                    {:event/type :add-edge :wid "000102"
                                     :relation "翻訳者" :pid "000001"}]})
        d (oracle/model-diff (first states) (peek states))]
    (is (= #{} (:added-pids d)))
    (is (= #{"000001"} (:corrected-pids d)))
    (is (= 1 (get-in d [:edge-counts :additions])))))

(deftest semantic-report-strips-locators-test
  (is (= {:drift {"summary" {"split_candidates" 0}}}
         (oracle/semantic-report
          {:drift {"previous_dir" "/tmp/x" "current_dir" "/tmp/y"
                   "summary" {"split_candidates" 0}}
           :work-dir "/tmp/w" :corpus-dirs {:previous "/a"}}))))

(deftest expected-candidates-are-edge-local-test
  (let [m (model/bootstrap 1)
        m1 (:model (model/apply-event m {:event/type :add-edge :wid "000101"
                                         :relation "翻訳者" :pid "000001"}))
        {:keys [applied]} (model/fold-history
                           {:initial m1
                            :events [{:event/type :clean-split :pid "000001"
                                      :targets ["900001" "900002"]
                                      :persons {"900001" (model/base-person)
                                                "900002" (model/base-person)}}]})
        cands (oracle/expected-split-candidates (first applied))]
    (is (= 2 (count cands)))
    (is (= #{["000101" "著者"] ["000101" "翻訳者"]}
           (set (map (juxt #(get % "work_id") #(get % "relation_to_work")) cands))))
    (is (every? #(= ["000001"] (get % "source_person_ids")) cands))
    (is (every? #(= ["900001" "900002"] (get % "target_person_ids")) cands))))
```

- [ ] **Step 2: Run to verify failure**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.oracle-test`
Expected: FAIL — `abc.sim.oracle` not found.

- [ ] **Step 3: Implement**

Create `abc/test/abc/sim/oracle.clj`:

```clojure
(ns abc.sim.oracle
  "Oracle utilities: catalog projection, endpoint confusability predicate,
  model diff for accounting, report normalization, expected candidates.
  These read model states and applied intents; they never re-derive the
  classifier's decisions."
  (:require [clojure.set :as set]
            [clojure.walk :as walk]))

(defn projection
  "Restrict a model to what the CSV can express: works and persons that
  participate in at least one edge."
  [m]
  (let [edge-pids (reduce set/union #{} (vals (:edges m)))
        edge-wids (set (map first (keys (:edges m))))]
    {:persons (into (sorted-map) (filter #(edge-pids (key %))) (:persons m))
     :works (into (sorted-map) (filter #(edge-wids (key %))) (:works m))
     :edges (:edges m)}))

(defn- id-sets [prev cur]
  (let [p (projection prev) c (projection cur)]
    {:p p :c c
     :removed (set/difference (set (keys (:persons p))) (set (keys (:persons c))))
     :added (set/difference (set (keys (:persons c))) (set (keys (:persons p))))}))

(defn confusable?
  "True when some edge's endpoint diff is candidate-shaped: a replacement
  whose previous pid-set ⊆ globally-removed and current pid-set ⊆
  globally-added, with cardinality 1→many or many→1."
  [prev cur]
  (let [{:keys [p c removed added]} (id-sets prev cur)]
    (boolean
     (some (fn [k]
             (let [ps (get (:edges p) k #{})
                   cs (get (:edges c) k #{})]
               (and (seq ps) (seq cs) (not= ps cs)
                    (not (set/subset? ps cs))
                    (not (set/subset? cs ps))
                    (set/subset? ps removed)
                    (set/subset? cs added)
                    (or (and (= 1 (count ps)) (< 1 (count cs)))
                        (and (< 1 (count ps)) (= 1 (count cs)))))))
           (set/union (set (keys (:edges p))) (set (keys (:edges c))))))))

(defn model-diff
  "Accounting oracle over projected endpoints (P4)."
  [prev cur]
  (let [{:keys [p c removed added]} (id-sets prev cur)
        shared (set/intersection (set (keys (:persons p))) (set (keys (:persons c))))
        corrected (set (filter #(not= (get-in p [:persons %]) (get-in c [:persons %]))
                               shared))
        edge-keys (set/union (set (keys (:edges p))) (set (keys (:edges c))))
        counts (reduce (fn [acc k]
                         (let [ps (get (:edges p) k #{}) cs (get (:edges c) k #{})]
                           (cond
                             (= ps cs) acc
                             (set/subset? ps cs) (update acc :additions inc)
                             (set/subset? cs ps) (update acc :removals inc)
                             :else (update acc :replacements inc))))
                       {:additions 0 :removals 0 :replacements 0}
                       edge-keys)]
    {:added-pids added :removed-pids removed :corrected-pids corrected
     :persons-previous (count (:persons p)) :persons-current (count (:persons c))
     :works-previous (count (:works p)) :works-current (count (:works c))
     :edge-counts counts}))

(def ^:private locator-keys
  ["previous_dir" "current_dir" "input_dir" "input-dir"
   :previous_dir :current_dir :input-dir
   :aozora-repo :work-dir :corpus-dirs :extracted-zips :ingest])

(defn semantic-report
  "Strip run-location fields so report equality is meaningful across
  differing work dirs (spec §Report normalization)."
  [report]
  (walk/postwalk (fn [x] (if (map? x) (apply dissoc x locator-keys) x))
                 report))

(defn- edge-candidates [intent source-ids target-ids]
  (vec (for [[wid rel] (:edges intent)]
         {"work_id" wid
          "relation_to_work" rel
          "source_person_ids" (vec (sort source-ids))
          "target_person_ids" (vec (sort target-ids))})))

(defn expected-split-candidates [intent]
  (edge-candidates intent [(-> intent :event :pid)] (-> intent :event :targets)))

(defn expected-merge-candidates [intent]
  (edge-candidates intent (-> intent :event :pids) [(-> intent :event :target)]))
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.oracle-test`
Expected: PASS (5 tests).

- [ ] **Step 5: Commit**

```bash
git add abc/test/abc/sim/oracle.clj abc/test/abc/sim/oracle_test.clj
git commit -m "test(sim): projection, confusability, diff and report oracles"
```

---

### Task 5: Render layer — CSV rows, CSV text, corpus dirs, corruptions

**Files:**
- Create: `abc/test/abc/sim/render.clj`
- Test: `abc/test/abc/sim/render_test.clj`

**Interfaces:**
- Consumes: `abc.sim.oracle/projection`, `abc.sim.model` field vocabularies.
- Produces (all in `abc.sim.render`):
  - `headers` — canonical vector of the 43 CSV column names (sorted).
  - `model->rows : model → [row-map]` — one map per work-contributor-role
    tuple of `projection(model)`, keyed by CSV column names, deterministic
    order (by `[wid relation pid]`).
  - `rows->csv : [row-map] → String` — charred-quoted CSV text with header
    (optionally `{:headers hs :bom? bool}`).
  - `corrupt-rows : [row-map] × [corruption] → [row-or-cells]` — applies
    render-layer corruptions; corruption shapes:
    `{:corrupt/type :cell :wid w :pid p-or-nil :column "生年月日" :value "1900. 1. 1"}`
    (generic single-cell rewrite — dates, names, any column),
    `{:corrupt/type :divergent-person :wid w :pid p :column "姓" :value "×"}`,
    `{:corrupt/type :divergent-work-fields :wid w :column "作品名" :value "×"}`,
    `{:corrupt/type :duplicate-row :wid w}`,
    `{:corrupt/type :ragged-short :wid w}`, `{:corrupt/type :ragged-long :wid w}`.
    Ragged corruptions return the affected row as a raw cell VECTOR so
    `rows->csv` can emit it without header alignment.
  - `write-corpus-dirs! : dir × model → nil` — writes
    `persons/<pid>.json` (person-record shaped, schema fields included) and
    `works/<wid>.json` (with `"contributors"`), from `projection(model)`.
  - `temp-dir : prefix → java.io.File` and `delete-tree! : file → nil`
    (wrappers reusing `abc.tools.files/delete-tree!`).

- [ ] **Step 1: Write the failing tests**

Create `abc/test/abc/sim/render_test.clj`:

```clojure
(ns abc.sim.render-test
  (:require [abc.sim.model :as model]
            [abc.sim.render :as render]
            [abc.tools.aozora-csv :as ac]
            [abc.tools.person-drift-history :as drift-history]
            [clojure.test :refer [deftest is testing]]))

(deftest model-rows-roundtrip-through-parser-test
  (let [m (model/bootstrap 2)
        csv (render/rows->csv (render/model->rows m))
        rows (ac/read-rows-from-string csv)]
    (is (= 2 (count rows)))
    (is (= #{"000101" "000102"} (set (map #(get % "作品ID") rows))))
    (let [{:keys [work persons-by-id contributors]}
          (ac/build-record-fragment-from-rows
           (filter #(= "000101" (get % "作品ID")) rows))]
      (is (= "作品000101" (get work "title")))
      (is (= ["000001"] (keys persons-by-id)))
      (is (= [{"person_id" "000001" "relation_to_work" "著者"}] contributors)))))

(deftest csv-quoting-test
  (let [m (model/bootstrap 1)
        m' (:model (model/apply-event m {:event/type :edit-work :wid "000101"
                                         :field :title :value "旅,\"新\"\n行"}))
        rows (ac/read-rows-from-string (render/rows->csv (render/model->rows m')))]
    (is (= "旅,\"新\"\n行" (get (first rows) "作品名")))))

(deftest corpus-dirs-feed-drift-history-test
  (let [m (model/bootstrap 2)
        prev (render/temp-dir "sim-prev")
        cur (render/temp-dir "sim-cur")]
    (try
      (render/write-corpus-dirs! prev m)
      (render/write-corpus-dirs! cur m)
      (let [r (drift-history/report {:previous-dir (str prev) :current-dir (str cur)})]
        (is (= 0 (get-in r ["summary" "split_candidates"])))
        (is (= 2 (get-in r ["summary" "persons_previous"]))))
      (finally
        (render/delete-tree! prev)
        (render/delete-tree! cur)))))

(deftest corruption-shapes-test
  (let [m (model/bootstrap 1)
        rows (render/model->rows m)]
    (testing "cell corruption replaces a single cell"
      (let [[r] (render/corrupt-rows rows [{:corrupt/type :cell :wid "000101"
                                            :column "生年月日" :value "1900. 1. 1"}])]
        (is (= "1900. 1. 1" (get r "生年月日")))))
    (testing "ragged-short renders a cell vector shorter than the header"
      (let [[r] (render/corrupt-rows rows [{:corrupt/type :ragged-short :wid "000101"}])]
        (is (vector? r))
        (is (< (count r) (count render/headers)))))))
```

- [ ] **Step 2: Run to verify failure**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.render-test`
Expected: FAIL — `abc.sim.render` not found.

- [ ] **Step 3: Implement**

Create `abc/test/abc/sim/render.clj`:

```clojure
(ns abc.sim.render
  "Projection of model states into the concrete upstream formats: CSV rows,
  quoted CSV text (charred), and post-ingest corpus directories. Dirty-data
  corruptions are applied here so the model stays well-formed."
  (:require [abc.sim.oracle :as oracle]
            [abc.tools.aozora-ingest :as ingest]
            [abc.tools.files :as files]
            [abc.tools.json :as json]
            [abc.tools.malli :as am]
            [charred.api :as charred]
            [clojure.java.io :as io])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def headers
  (vec (sort ["作品ID" "人物ID" "役割フラグ" "作品名" "作品名読み" "ソート用読み"
              "副題" "副題読み" "原題" "初出" "分類番号" "文字遣い種別"
              "作品著作権フラグ" "公開日" "最終更新日" "図書カードURL"
              "底本名1" "底本出版社名1" "底本名2" "底本出版社名2"
              "底本初版発行年1" "底本初版発行年2"
              "入力に使用した版1" "入力に使用した版2"
              "校正に使用した版1" "校正に使用した版2"
              "底本の親本名1" "底本の親本名2"
              "底本の親本出版社名1" "底本の親本出版社名2"
              "底本の親本初版発行年1" "底本の親本初版発行年2"
              "姓" "名" "姓読み" "名読み" "姓読みソート用" "名読みソート用"
              "姓ローマ字" "名ローマ字" "生年月日" "没年月日" "人物著作権フラグ"])))

(defn- flag [b] (if b "なし" "あり"))

(defn- person-cells [pid p]
  {"人物ID" pid
   "姓" (:family_name p) "名" (:given_name p)
   "姓読み" (:family_name_reading p) "名読み" (:given_name_reading p)
   "姓読みソート用" (:family_name_sort p) "名読みソート用" (:given_name_sort p)
   "姓ローマ字" (:family_name_romaji p) "名ローマ字" (:given_name_romaji p)
   "生年月日" (:date_of_birth p) "没年月日" (:date_of_death p)
   "人物著作権フラグ" (flag (:copyright_expired p))})

(defn- work-cells [wid w]
  {"作品ID" wid
   "作品名" (:title w) "作品名読み" (:title_reading w)
   "ソート用読み" (:sort_reading w)
   "分類番号" (:ndc w) "文字遣い種別" (:orthography w)
   "作品著作権フラグ" (flag (:copyright_expired w))
   "公開日" (:available w) "最終更新日" (:modified w)
   "図書カードURL" (str "https://www.aozora.gr.jp/cards/000001/card" wid ".html")
   "底本名1" (:edition_title w) "底本出版社名1" (:edition_publisher w)})

(defn model->rows
  "One row per work-contributor-role tuple of projection(model), all 43
  headers present (blank when inapplicable), sorted by [wid relation pid]."
  [m]
  (let [{:keys [persons works edges]} (oracle/projection m)]
    (vec (for [[[wid rel] pids] (sort edges)
               pid (sort pids)]
           (merge (zipmap headers (repeat ""))
                  (work-cells wid (get works wid))
                  (person-cells pid (get persons pid))
                  {"役割フラグ" rel})))))

(defn rows->csv
  "Quoted CSV text. Rows may be header-keyed maps or raw cell vectors
  (ragged corruptions)."
  ([rows] (rows->csv rows {}))
  ([rows {:keys [bom? header-cells] :or {bom? false}}]
   (let [hs (or header-cells headers)
         cells (map (fn [r] (if (vector? r) r (mapv #(get r % "") hs))) rows)
         sw (java.io.StringWriter.)]
     (charred/write-csv sw (cons hs cells))
     (str (when bom? "﻿") sw))))

(defn- rows-for-wid [rows wid] (filter #(= wid (get % "作品ID")) rows))

(defn corrupt-rows
  "Apply render-layer corruptions. Cell corruptions rewrite the first row of
  the target work; :divergent-* corruptions require the work to have ≥ 2 rows
  and rewrite only the SECOND row's cell, creating cross-row divergence;
  :duplicate-row repeats the first row; ragged corruptions replace the first
  row with a raw cell vector (short: drops the last 3 cells; long: appends 2)."
  [rows corruptions]
  (reduce
   (fn [rows {:corrupt/keys [type] :keys [wid pid column value]}]
     (let [idxs (keep-indexed (fn [i r] (when (and (map? r) (= wid (get r "作品ID"))
                                                   (or (nil? pid) (= pid (get r "人物ID"))))
                                          i))
                              rows)
           i0 (first idxs)
           i1 (second idxs)]
       (if (nil? i0)
         rows
         (case type
           :cell (update rows i0 assoc column value)
           :divergent-person (if i1 (update rows i1 assoc column value) rows)
           :divergent-work-fields (if i1 (update rows i1 assoc column value) rows)
           :duplicate-row (conj rows (nth rows i0))
           :ragged-short (assoc rows i0 (vec (drop-last 3 (mapv #(get (nth rows i0) % "") headers))))
           :ragged-long (assoc rows i0 (conj (mapv #(get (nth rows i0) % "") headers) "x" "y"))
           rows))))
   (vec rows)
   corruptions))

(defn temp-dir [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn delete-tree! [f] (files/delete-tree! (io/file f)))

(defn- person-record-json [pid p]
  {"person_record_schema_id" ingest/person-schema-id
   "person_record_schema_hash" (am/cached-schema-hash ingest/person-schema-path)
   "person_id" pid
   "family_name" (:family_name p) "given_name" (:given_name p)
   "family_name_reading" (:family_name_reading p)
   "given_name_reading" (:given_name_reading p)
   "family_name_sort" (:family_name_sort p) "given_name_sort" (:given_name_sort p)
   "family_name_romaji" (:family_name_romaji p)
   "given_name_romaji" (:given_name_romaji p)
   "date_of_birth" (:date_of_birth p) "date_of_death" (:date_of_death p)
   "person_copyright_expired" (:copyright_expired p)
   "external_links" []})

(defn write-corpus-dirs!
  "Render projection(model) as a post-ingest corpus root (persons/ +
  works/), the input contract of person-drift-history/report."
  [root m]
  (let [{:keys [persons works edges]} (oracle/projection m)
        persons-dir (io/file root "persons")
        works-dir (io/file root "works")]
    (.mkdirs persons-dir)
    (.mkdirs works-dir)
    (doseq [[pid p] persons]
      (json/write-deterministic-json-file!
       (io/file persons-dir (str pid ".json")) (person-record-json pid p)))
    (doseq [[wid w] works]
      (json/write-deterministic-json-file!
       (io/file works-dir (str wid ".json"))
       {"work" {"work_id" wid "title" (:title w)}
        "contributors" (vec (for [[[ewid rel] pids] (sort edges)
                                  :when (= ewid wid)
                                  pid (sort pids)]
                              {"person_id" pid "relation_to_work" rel}))}))))
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.render-test`
Expected: PASS (4 tests).

- [ ] **Step 5: Commit**

```bash
git add abc/test/abc/sim/render.clj abc/test/abc/sim/render_test.clj
git commit -m "test(sim): render model states to CSV, corpus dirs, corruptions"
```

---

### Task 6: Generators

**Files:**
- Create: `abc/test/abc/sim/gen.clj`
- Test: `abc/test/abc/sim/gen_test.clj`

**Interfaces:**
- Consumes: `abc.sim.model` events/`fresh-pid`/`fresh-wid`/`apply-event`,
  `abc.sim.oracle/confusable?`.
- Produces (all in `abc.sim.gen`):
  - `history-gen : opts → generator of {:initial model :events [event]}`.
    Opts: `{:length [min max] :works [min max] :forced forced-kw-or-nil}`
    with `forced` one of `:clean-split :clean-merge :ambiguous-replacement
    :impure-split :partial-split` — the forced event is inserted at a random
    position; the generator constructs preceding state so its precondition
    holds at generation time (a shrink may still no-op it).
  - `benign-history-gen : opts → generator` — benign events only, weighted;
    avoids composing person-removal and person-addition around one
    `[work relation]` key inside the history (heuristic; `confusable?` in
    properties is normative).
  - `find-applied : fold-result × event-type → intent-or-nil`.

- [ ] **Step 1: Write the failing tests**

Create `abc/test/abc/sim/gen_test.clj`:

```clojure
(ns abc.sim.gen-test
  (:require [abc.sim.gen :as sgen]
            [abc.sim.model :as model]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]))

(deftest generated-histories-are-valid-test
  (doseq [hist (gen/sample (sgen/benign-history-gen {:length [5 15] :works [5 20]}) 30)]
    ;; fold-history throws on any invariant violation
    (let [{:keys [states]} (model/fold-history hist)]
      (is (= (inc (count (:events hist))) (count states))))))

(deftest forced-events-apply-in-most-samples-test
  (doseq [forced [:clean-split :clean-merge :ambiguous-replacement
                  :impure-split :partial-split]]
    (let [hists (gen/sample (sgen/history-gen {:length [5 10] :works [5 10]
                                               :forced forced}) 40)
          applied (count (keep #(sgen/find-applied (model/fold-history %) forced)
                               hists))]
      (is (>= (/ applied 40) 9/10)
          (str forced " applied only " applied "/40")))))
```

- [ ] **Step 2: Run to verify failure**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.gen-test`
Expected: FAIL — `abc.sim.gen` not found.

- [ ] **Step 3: Implement**

Create `abc/test/abc/sim/gen.clj`:

```clojure
(ns abc.sim.gen
  "test.check generators for evolution histories. Events are generated
  concretely against the evolving state (gen/bind chain); apply-event's
  no-op totality keeps shrunk subsequences valid."
  (:require [abc.sim.model :as model]
            [clojure.test.check.generators :as gen]))

(def relations ["著者" "翻訳者" "校訂者"])

(def name-pool ["新" "改" "旧" "東" "西" "南" "北" "翠" "紅" "蒼"])

(defn- variant-person [i]
  (assoc (model/base-person)
         :family_name (nth name-pool (mod i (count name-pool)))
         :given_name (str "人" i)))

(defn find-applied [fold-result event-type]
  (first (filter #(= event-type (:intent %)) (:applied fold-result))))

;; --- single-event generators against a concrete state -----------------

(defn- gen-edit-person [m]
  (gen/let [pid (gen/elements (vec (keys (:persons m))))
            field (gen/elements [:family_name :given_name :date_of_death])
            v (gen/elements name-pool)]
    {:event/type :edit-person :pid pid :field field
     :value (if (= field :date_of_death) "1971-02-02" v)}))

(defn- gen-edit-work [m]
  (gen/let [wid (gen/elements (vec (keys (:works m))))
            v (gen/elements name-pool)]
    {:event/type :edit-work :wid wid :field :title :value (str "作品" v)}))

(defn- gen-add-work-with-edge [m]
  (let [wid (model/fresh-wid m) pid (model/fresh-pid m)]
    (gen/let [rel (gen/elements relations)]
      {:event/type :add-work-with-edge :wid wid :work (model/base-work wid)
       :pid pid :person (variant-person (:next-id m)) :relation rel})))

(defn- gen-add-person-with-edge [m]
  (let [pid (model/fresh-pid m)]
    (gen/let [wid (gen/elements (vec (keys (:works m))))
              rel (gen/elements relations)]
      {:event/type :add-person-with-edge :pid pid
       :person (variant-person (:next-id m)) :wid wid :relation rel})))

(defn- gen-add-edge [m]
  (gen/let [wid (gen/elements (vec (keys (:works m))))
            pid (gen/elements (vec (keys (:persons m))))
            rel (gen/elements relations)]
    {:event/type :add-edge :wid wid :relation rel :pid pid}))

(defn- gen-remove-edge [m]
  (if (empty? (:edges m))
    (gen-add-work-with-edge m)
    ;; sequential bindings: pids depends on the chosen edge
    (gen/let [[k pids] (gen/elements (vec (:edges m)))
              pid (gen/elements (vec pids))]
      {:event/type :remove-edge :wid (first k) :relation (second k) :pid pid})))

(defn- gen-remove-work [m]
  (gen/let [wid (gen/elements (vec (keys (:works m))))]
    {:event/type :remove-work :wid wid}))

(defn- gen-rare-unattached [m]
  (gen/one-of
   [(gen/return {:event/type :add-person :pid (model/fresh-pid m)
                 :person (variant-person (:next-id m))})
    (gen/return (let [wid (model/fresh-wid m)]
                  {:event/type :add-work :wid wid :work (model/base-work wid)}))]))

(defn- benign-event-gen
  "Weighted benign event against state m. Removal weights are low, which
  together with the fresh-id discipline keeps the confusable?-discard rate
  low (predicate in properties is normative)."
  [m]
  (if (empty? (:works m))
    (gen-add-work-with-edge m)
    (gen/frequency
     [[4 (gen-edit-person m)]
      [3 (gen-edit-work m)]
      [3 (gen-add-work-with-edge m)]
      [3 (gen-add-person-with-edge m)]
      [3 (gen-add-edge m)]
      [1 (gen-remove-edge m)]
      [1 (gen-remove-work m)]
      [1 (gen-rare-unattached m)]])))

;; --- forced drift events ----------------------------------------------
;; Each returns [setup-events forced-event] built against state m so the
;; forced precondition holds at generation time.

(defn- sole-pids [m]
  (vec (filter #(model/sole-contributor? m %) (keys (:persons m)))))

(defn- gen-forced [m forced]
  (let [t1 (model/fresh-pid m)
        t2 (model/fresh-pid (update m :next-id inc))
        mk-persons (fn [& pids]
                     (into {} (map-indexed (fn [i p] [p (variant-person (+ 50 i))]) pids)))]
    (case forced
      :clean-split
      (let [cands (sole-pids m)]
        (if (seq cands)
          (gen/let [pid (gen/elements cands)]
            [[] {:event/type :clean-split :pid pid :targets [t1 t2]
                 :persons (mk-persons t1 t2)}])
          ;; create a fresh sole-contributor work first, then split it
          (let [wid (model/fresh-wid m) pid t1
                t1' (model/fresh-pid (update m :next-id + 2))
                t2' (model/fresh-pid (update m :next-id + 3))]
            (gen/return
             [[{:event/type :add-work-with-edge :wid wid :work (model/base-work wid)
                :pid pid :person (variant-person 51) :relation "著者"}]
              {:event/type :clean-split :pid pid :targets [t1' t2']
               :persons (mk-persons t1' t2')}]))))

      :clean-merge
      (let [wid (model/fresh-wid m) p1 t1 p2 t2
            tgt (model/fresh-pid (update m :next-id + 2))]
        (gen/return
         [[{:event/type :add-work-with-edge :wid wid :work (model/base-work wid)
            :pid p1 :person (variant-person 52) :relation "著者"}
           {:event/type :add-person-with-edge :pid p2 :person (variant-person 53)
            :wid wid :relation "著者"}]
          {:event/type :clean-merge :pids [p1 p2] :target tgt
           :person (variant-person 54)}]))

      :ambiguous-replacement
      (gen/let [pid (gen/elements (vec (keys (:persons m))))]
        [[] {:event/type :ambiguous-replacement :pid pid :target t1
             :person (variant-person 55)}])

      :impure-split
      (let [wid (model/fresh-wid m) pid t1
            newt (model/fresh-pid (update m :next-id + 2))]
        (gen/let [existing (gen/elements (vec (keys (:persons m))))]
          [[{:event/type :add-work-with-edge :wid wid :work (model/base-work wid)
             :pid pid :person (variant-person 56) :relation "著者"}]
           {:event/type :impure-split :pid pid :existing-target existing
            :new-target newt :person (variant-person 57)}]))

      :partial-split
      (let [wid1 (model/fresh-wid m)
            wid2 (model/fresh-wid (update m :next-id inc))
            pid (model/fresh-pid (update m :next-id + 2))
            tgt (model/fresh-pid (update m :next-id + 3))]
        (gen/return
         [[{:event/type :add-work-with-edge :wid wid1 :work (model/base-work wid1)
            :pid pid :person (variant-person 58) :relation "著者"}
           {:event/type :add-work :wid wid2 :work (model/base-work wid2)}
           {:event/type :add-edge :wid wid2 :relation "著者" :pid pid}]
          {:event/type :partial-split :pid pid :targets [tgt]
           :edge-keys [[wid2 "著者"]] :persons (mk-persons tgt)}])))))

;; --- history assembly ---------------------------------------------------

(defn- gen-events
  "Chain n benign events against the evolving state; when i = forced-at,
  splice in [setup... forced...] instead."
  [m n i forced-at forced]
  (if (zero? n)
    (gen/return [])
    (gen/bind (if (= i forced-at)
                (gen-forced m forced)
                (gen/fmap (fn [e] [[] e])
                          (benign-event-gen m)))
              (fn [[setup e]]
                (let [es (conj (vec setup) e)
                      m' (peek (:states (model/fold-history {:initial m :events es})))]
                  (gen/fmap #(into es %)
                            (gen-events m' (dec n) (inc i) forced-at forced)))))))

(defn history-gen
  [{:keys [length works forced] :or {length [5 15] works [5 20]}}]
  (gen/let [n-works (gen/choose (first works) (second works))
            n-events (gen/choose (first length) (second length))
            forced-at (if forced (gen/choose 0 (dec n-events)) (gen/return -1))]
    (let [m0 (model/bootstrap n-works)]
      (gen/fmap (fn [events] {:initial m0 :events events})
                (gen-events m0 n-events 0 forced-at forced)))))

(defn benign-history-gen [opts]
  (history-gen (assoc opts :forced nil)))
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.gen-test`
Expected: PASS (2 tests). If `forced-events-apply-in-most-samples-test` is
below 9/10 for some event, strengthen that event's setup in `gen-forced`
(the setup must construct the precondition, not search for it).

- [ ] **Step 5: Commit**

```bash
git add abc/test/abc/sim/gen.clj abc/test/abc/sim/gen_test.clj
git commit -m "test(sim): history generators with forced drift events"
```

---

### Task 7: Known-divergences table and expected-failure gate

**Files:**
- Create: `abc/test/abc/sim/divergences.clj`
- Test: `abc/test/abc/sim/divergences_test.clj`

**Interfaces:**
- Produces (in `abc.sim.divergences`):
  - `table` — map of `:D1`..`:D6` to
    `{:case string :status (:open|:adjudicated-bug|:adjudicated-intended) :notes string}`
    exactly mirroring the spec's seed table.
  - `open? : id → boolean` (true for `:open` and `:adjudicated-bug`).
  - `expected-failure` macro:
    `(expected-failure :D1 "P6.divergent-work-fields" <desired-behavior-form>)`.
    While `open?`, the desired-behavior form (which must evaluate truthy when
    the DESIRED behavior holds; may throw) is expected NOT to hold — if it
    starts holding, the test FAILS with "now passes — adjudicate". When the
    divergence is closed, the form is asserted directly with `clojure.test/is`.

- [ ] **Step 1: Write the failing test**

Create `abc/test/abc/sim/divergences_test.clj`:

```clojure
(ns abc.sim.divergences-test
  (:require [abc.sim.divergences :as div]
            [clojure.test :refer [deftest is]]))

(deftest table-covers-spec-entries-test
  (is (= #{:D1 :D2 :D3 :D4 :D5 :D6} (set (keys div/table))))
  (is (every? #(contains? #{:open :adjudicated-bug :adjudicated-intended}
                          (:status %))
              (vals div/table))))

(deftest expected-failure-inverts-while-open-test
  ;; :D1 is open: a FALSE desired-check passes, a TRUE desired-check fails.
  (is (true? (div/expected-failure* :D1 "demo" (fn [] false))))
  (is (false? (div/expected-failure* :D1 "demo" (fn [] true)))))
```

- [ ] **Step 2: Run to verify failure**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.divergences-test`
Expected: FAIL — namespace not found.

- [ ] **Step 3: Implement**

Create `abc/test/abc/sim/divergences.clj`:

```clojure
(ns abc.sim.divergences
  "Known-divergences table (source of record: the spec §Known Divergences).
  Properties assert DESIRED behavior; while an entry is open the linked
  case runs as an expected failure and alerts when it starts passing."
  (:require [clojure.test :refer [is]]))

(def table
  {:D1 {:case "P6.divergent-work-fields" :status :open
        :notes "aozora_csv.clj:284-325 first-row-wins on divergent work fields"}
   :D2 {:case "P12.selection" :status :open
        :notes "aozora_history_audit.clj:188-193 partition-by over log order"}
   :D3 {:case "P13.ragged-row" :status :open
        :notes "aozora_csv.clj:22-32 ragged rows silently truncated"}
   :D4 {:case "P8.atomicity+order-independence" :status :open
        :notes "aozora_ingest.clj:151-213 person writes precede work failure"}
   :D5 {:case "P13.empty-csv" :status :open
        :notes "suspected: empty/header-only CSV yields silent zero-row corpus; confirm during P13"}
   :D6 {:case "P13.non-zip-bytes" :status :open
        :notes "suspected: raw ZipException from ZipFile ctor; confirm during P13"}})

(defn open? [id]
  (contains? #{:open :adjudicated-bug} (get-in table [id :status])))

(defn expected-failure*
  "Returns true when the outcome matches the table's expectation. Desired-fn
  must return truthy iff DESIRED behavior holds; throwing counts as
  not-holding."
  [id _desc desired-fn]
  (let [holds? (try (boolean (desired-fn)) (catch Exception _ false))]
    (if (open? id) (not holds?) holds?)))

(defmacro expected-failure
  [id desc & body]
  `(is (expected-failure* ~id ~desc (fn [] ~@body))
       (if (open? ~id)
         (str ~desc ": divergence " ~id " now passes — adjudicate the table")
         (str ~desc " (" ~id ") regressed"))))
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.divergences-test`
Expected: PASS (2 tests).

- [ ] **Step 5: Commit**

```bash
git add abc/test/abc/sim/divergences.clj abc/test/abc/sim/divergences_test.clj
git commit -m "test(sim): known-divergences table with expected-failure gate"
```

---

### Task 8: Classifier properties P1–P5

**Files:**
- Create: `abc/test/abc/sim/classifier_sim_test.clj`
- Delete: `abc/test/abc/sim/smoke_sim_test.clj` (superseded by real sim tests)

**Interfaces:**
- Consumes: everything above plus `abc.tools.person-drift-history/report`.
- Produces: property cases `P1.benign-quiet`, `P2.clean-split`,
  `P2.clean-merge`, `P3.ambiguous`, `P3.impure-split`, `P3.partial-split`,
  `P4.counts`, `P5.repeat`.

- [ ] **Step 1: Write the property tests**

Create `abc/test/abc/sim/classifier_sim_test.clj`:

```clojure
(ns abc.sim.classifier-sim-test
  "Pure-layer classifier properties P1–P5 over person-drift-history/report.
  Windows are rendered corpus-dir pairs of projected model states."
  (:require [abc.sim.divergences]
            [abc.sim.gen :as sgen]
            [abc.sim.harness :as harness]
            [abc.sim.model :as model]
            [abc.sim.oracle :as oracle]
            [abc.sim.render :as render]
            [abc.tools.json :as abc-json]
            [abc.tools.person-drift-history :as drift-history]
            [clojure.test :refer [deftest]]
            [clojure.test.check.properties :as prop]))

(defn- report-for-window [prev cur]
  (let [pd (render/temp-dir "sim-prev")
        cd (render/temp-dir "sim-cur")]
    (try
      (render/write-corpus-dirs! pd prev)
      (render/write-corpus-dirs! cd cur)
      (drift-history/report {:previous-dir (str pd) :current-dir (str cd)})
      (finally (render/delete-tree! pd) (render/delete-tree! cd)))))

(defn- candidate-counts [r]
  [(get-in r ["summary" "split_candidates"])
   (get-in r ["summary" "merge_candidates"])])

;; P1.benign-quiet — aggregated window over the whole benign history.
(deftest p1-benign-quiet-sim-test
  (harness/check! "P1.benign-quiet" 50
    (prop/for-all [hist (sgen/benign-history-gen {:length [5 12] :works [5 12]})]
      (let [{:keys [states]} (model/fold-history hist)
            prev (first states) cur (peek states)]
        (or (oracle/confusable? prev cur) ;; excluded by the normative predicate
            (= [0 0] (candidate-counts (report-for-window prev cur))))))))

(defn- forced-window
  "States immediately around the forced intent's position: find the first
  index where applying events reproduces the intent."
  [hist forced]
  (let [{:keys [states applied]} (model/fold-history hist)
        intent (sgen/find-applied {:applied applied} forced)]
    (when intent
      ;; locate the state pair around the forced event by replaying
      (let [idx (first (keep-indexed
                        (fn [i e] (when (identical? e (:event intent)) i))
                        (:events hist)))]
        {:intent intent :prev (nth states idx) :cur (nth states (inc idx))}))))

;; P2 — completeness, edge-local candidates.
(defn- completeness-prop [forced expected-fn candidates-key counter]
  (prop/for-all [hist (sgen/history-gen {:length [4 8] :works [4 8] :forced forced})]
    (let [w (forced-window hist forced)]
      (if-not (harness/tick! counter (some? w))
        true ;; no-op after shrink: totality only
        (let [r (report-for-window (:prev w) (:cur w))]
          (every? (set (get r candidates-key))
                  (expected-fn (:intent w))))))))

(deftest p2-clean-split-sim-test
  (let [c (harness/ratio-counter)]
    (harness/check! "P2.clean-split" 50
      (completeness-prop :clean-split oracle/expected-split-candidates
                         "split_candidates" c))
    (harness/assert-applied-ratio! "P2.clean-split" c)))

(deftest p2-clean-merge-sim-test
  (let [c (harness/ratio-counter)]
    (harness/check! "P2.clean-merge" 50
      (completeness-prop :clean-merge oracle/expected-merge-candidates
                         "merge_candidates" c))
    (harness/assert-applied-ratio! "P2.clean-merge" c)))

;; P3 — conservatism: these intents never yield candidates in their window.
(defn- conservatism-prop [forced counter]
  (prop/for-all [hist (sgen/history-gen {:length [4 8] :works [4 8] :forced forced})]
    (let [w (forced-window hist forced)]
      (if-not (harness/tick! counter (some? w))
        true
        (= [0 0] (candidate-counts (report-for-window (:prev w) (:cur w))))))))

(deftest p3-ambiguous-sim-test
  (let [c (harness/ratio-counter)]
    (harness/check! "P3.ambiguous" 50 (conservatism-prop :ambiguous-replacement c))
    (harness/assert-applied-ratio! "P3.ambiguous" c)))

(deftest p3-impure-split-sim-test
  (let [c (harness/ratio-counter)]
    (harness/check! "P3.impure-split" 50 (conservatism-prop :impure-split c))
    (harness/assert-applied-ratio! "P3.impure-split" c)))

(deftest p3-partial-split-sim-test
  (let [c (harness/ratio-counter)]
    (harness/check! "P3.partial-split" 50 (conservatism-prop :partial-split c))
    (harness/assert-applied-ratio! "P3.partial-split" c)))

;; P4.counts — summary accounting equals the projected model diff.
(deftest p4-counts-sim-test
  (harness/check! "P4.counts" 50
    (prop/for-all [hist (sgen/benign-history-gen {:length [5 12] :works [5 12]})]
      (let [{:keys [states]} (model/fold-history hist)
            prev (first states) cur (peek states)
            d (oracle/model-diff prev cur)
            s (get (report-for-window prev cur) "summary")]
        (and (= (count (:added-pids d)) (get s "added_person_ids"))
             (= (count (:removed-pids d)) (get s "removed_person_ids"))
             (= (count (:corrected-pids d)) (get s "metadata_corrections"))
             (= (:persons-previous d) (get s "persons_previous"))
             (= (:persons-current d) (get s "persons_current"))
             (= (:works-previous d) (get s "works_previous"))
             (= (:works-current d) (get s "works_current"))
             (= (get-in d [:edge-counts :additions]) (get s "contributor_edge_additions"))
             (= (get-in d [:edge-counts :removals]) (get s "contributor_edge_removals"))
             (= (get-in d [:edge-counts :replacements]) (get s "contributor_edge_replacements")))))))

;; P5.repeat — determinism, byte-identical serialization.
(deftest p5-repeat-sim-test
  (harness/check! "P5.repeat" 20
    (prop/for-all [hist (sgen/benign-history-gen {:length [3 6] :works [3 6]})]
      (let [{:keys [states]} (model/fold-history hist)
            prev (first states) cur (peek states)
            pd (render/temp-dir "sim-prev") cd (render/temp-dir "sim-cur")]
        (try
          (render/write-corpus-dirs! pd prev)
          (render/write-corpus-dirs! cd cur)
          (let [r1 (drift-history/report {:previous-dir (str pd) :current-dir (str cd)})
                r2 (drift-history/report {:previous-dir (str pd) :current-dir (str cd)})]
            (and (= r1 r2)
                 (= (abc-json/write-deterministic-json-str (oracle/semantic-report r1))
                    (abc-json/write-deterministic-json-str (oracle/semantic-report r2)))))
          (finally (render/delete-tree! pd) (render/delete-tree! cd)))))))
```

- [ ] **Step 2: Delete the smoke test**

```bash
rm abc/test/abc/sim/smoke_sim_test.clj
```

- [ ] **Step 3: Run the simulation suite**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation`
Expected: PASS. These properties test today's classifier on clean evidence —
per the spec, no known divergence applies here. If P1 fails, inspect the
shrunk history: a `confusable?` false-negative (predicate too narrow) is a
harness bug to fix in `oracle.clj`, not a suite red to accept. Record the
wall-clock: classifier properties should stay well under 1 minute.

- [ ] **Step 4: Commit**

```bash
git add abc/test/abc/sim/classifier_sim_test.clj
git rm abc/test/abc/sim/smoke_sim_test.clj
git commit -m "test(sim): classifier properties P1-P5"
```

---

### Task 9: Ingest properties P6–P9 (+ divergence cases D1, D4)

**Files:**
- Create: `abc/test/abc/sim/ingest_sim_test.clj`

**Interfaces:**
- Consumes: `abc.tools.aozora-ingest/run-corpus!`,
  `abc.tools.aozora-csv/read-rows-from-string`, everything above.
- Produces: cases `P6.clean-faithfulness`, `P6.divergent-work-fields` (D1),
  `P7.date-classes`, `P8.skip-counted`, `P8.atomicity` (D4),
  `P8.order-independence` (D4), `P9.byte-stable`.

- [ ] **Step 1: Write the tests**

Create `abc/test/abc/sim/ingest_sim_test.clj`:

```clojure
(ns abc.sim.ingest-sim-test
  "Pure-layer ingest properties P6–P9: rendered CSV → run-corpus! against
  projection(model). Divergence-linked cases D1/D4 run as expected
  failures via abc.sim.divergences."
  (:require [abc.sim.divergences :as div]
            [abc.sim.gen :as sgen]
            [abc.sim.harness :as harness]
            [abc.sim.model :as model]
            [abc.sim.oracle :as oracle]
            [abc.sim.render :as render]
            [abc.tools.aozora-csv :as ac]
            [abc.tools.aozora-ingest :as ingest]
            [abc.tools.files :as files]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.properties :as prop]))

(defn- ingest-rows! [rows dir]
  (ingest/run-corpus! {:rows rows :output-dir (str dir) :overwrite true}))

(defn- ingest-model
  "Render model (with optional corruptions) → parse → ingest into a temp
  dir; returns {:dir :result}. Caller must delete-tree! :dir."
  [m corruptions]
  (let [rows (render/corrupt-rows (render/model->rows m) corruptions)
        csv (render/rows->csv rows)
        parsed (ac/read-rows-from-string csv)
        dir (render/temp-dir "sim-ingest")]
    {:dir dir :result (ingest-rows! parsed dir)}))

(defn- ingested-person [dir pid]
  (let [f (io/file dir "persons" (str pid ".json"))]
    (when (.exists f) (files/read-json f))))

(defn- person-faithful? [dir pid p]
  (let [r (ingested-person dir pid)]
    (and r
         (= (:family_name p) (get r "family_name"))
         (= (:given_name p) (get r "given_name"))
         (= (:date_of_birth p) (get r "date_of_birth"))
         (= (:copyright_expired p) (get r "person_copyright_expired")))))

(defn- work-faithful? [dir wid w edges]
  (let [f (io/file dir "works" (str wid ".json"))]
    (and (.exists f)
         (let [r (files/read-json f)]
           (and (= (:title w) (get-in r ["work" "title"]))
                (= (:ndc w) (get-in r ["work" "ndc"]))
                (= (set (for [[[ewid rel] pids] edges :when (= ewid wid) pid pids]
                          [pid rel]))
                   (set (map (juxt #(get % "person_id") #(get % "relation_to_work"))
                             (get r "contributors")))))))))

;; P6.clean-faithfulness
(deftest p6-clean-faithfulness-sim-test
  (harness/check! "P6.clean-faithfulness" 40
    (prop/for-all [hist (sgen/benign-history-gen {:length [3 8] :works [3 8]})]
      (let [{:keys [states]} (model/fold-history hist)
            m (peek states)
            {:keys [persons works edges]} (oracle/projection m)
            {:keys [dir result]} (ingest-model m [])]
        (try
          (and (zero? (:works-skipped result))
               (= (count works) (:works-written result))
               (every? (fn [[pid p]] (person-faithful? dir pid p)) persons)
               (every? (fn [[wid w]] (work-faithful? dir wid w edges)) works))
          (finally (render/delete-tree! dir)))))))

;; P6.divergent-work-fields — D1 (desired: detected, not first-row-wins)
(deftest p6-divergent-work-fields-sim-test
  (let [m0 (model/bootstrap 1)
        ;; second row for the same work via a second contributor
        m (:model (model/apply-event m0 {:event/type :add-person-with-edge
                                         :pid "000002" :person (model/base-person)
                                         :wid "000101" :relation "翻訳者"}))
        {:keys [dir result]} (ingest-model m [{:corrupt/type :divergent-work-fields
                                               :wid "000101" :column "作品名"
                                               :value "別名"}])]
    (try
      (div/expected-failure :D1 "P6.divergent-work-fields"
        ;; DESIRED: the divergence is detected — work skipped or audited,
        ;; i.e. NOT a silently written work carrying first-row title.
        (or (pos? (:works-skipped result))
            (not= "作品000101"
                  (get-in (files/read-json (io/file dir "works" "000101.json"))
                          ["work" "title"]))))
      (finally (render/delete-tree! dir)))))

;; P7.date-classes — every parse-date input class: correction+valid, or
;; passthrough+skip; never a crash or silent third outcome.
(def date-cases
  ;; [cell expected-outcome] where outcome ∈ :corrected :verbatim-valid :skipped
  [["1900. 1. 1" :corrected]
   ["1900 - 01 - 01" :corrected]
   ["1900--01" :corrected]
   ["不詳" :corrected]          ;; null + unknown-marker correction
   ["前5" :corrected]
   ["紀元前5世紀初頭" :corrected]
   ["192X" :verbatim-valid]
   ["2020-02-31" :skipped]      ;; impossible calendar date → schema reject
   ["こんにちは" :skipped]])     ;; unparseable shape → schema reject

(deftest p7-date-classes-sim-test
  (doseq [[cell outcome] date-cases]
    (testing (pr-str cell)
      (let [m (model/bootstrap 2) ;; work 000101 dirty, 000102 clean
            {:keys [dir result]} (ingest-model m [{:corrupt/type :cell
                                                   :wid "000101"
                                                   :column "生年月日" :value cell}])]
        (try
          (case outcome
            :skipped
            (is (= ["000101"] (:skipped-work-ids result)) (pr-str cell))

            (:corrected :verbatim-valid)
            (do (is (zero? (:works-skipped result)) (pr-str cell))
                (is (some? (ingested-person dir "000001")) (pr-str cell))))
          ;; the clean work always survives
          (is (some? (ingested-person dir "000002")))
          (finally (render/delete-tree! dir)))))))

;; P8.skip-counted — generative: a within-work person divergence skips
;; exactly that work; clean works' records stay present and correct.
;; Fault shape: duplicate one contributor row of the chosen work, then
;; diverge the duplicate's 姓 — same pid, divergent bodies, one work.
;; build-record-fragment-from-rows throws BEFORE any writes for this fault,
;; so no cross-work contamination is possible and the case passes today.
(deftest p8-skip-counted-sim-test
  (harness/check! "P8.skip-counted" 30
    (prop/for-all [hist (sgen/benign-history-gen {:length [3 6] :works [4 8]})]
      (let [{:keys [states]} (model/fold-history hist)
            m (peek states)
            {:keys [works edges]} (oracle/projection m)
            [wid _] (first works)
            pid (first (sort (mapcat val (filter #(= wid (ffirst %)) edges))))
            {:keys [dir result]} (ingest-model m
                                   [{:corrupt/type :duplicate-row :wid wid :pid pid}
                                    {:corrupt/type :divergent-person :wid wid :pid pid
                                     :column "姓" :value "×"}])]
        (try
          (and (= [wid] (:skipped-work-ids result))
               (every? (fn [[owid ow]]
                         (or (= owid wid) (work-faithful? dir owid ow edges)))
                       works))
          (finally (render/delete-tree! dir)))))))

;; P8.atomicity — D4: a skipped work must leave no person records behind.
;; The fault must fail LATE (after the first person write): an impossible
;; calendar date passes parse-date verbatim and fails schema validation only
;; when the SECOND contributor's record is validated in write-person-file!,
;; by which point contributor 000001's file is already on disk.
(deftest p8-atomicity-sim-test
  (let [m0 (model/bootstrap 1)
        m (:model (model/apply-event m0 {:event/type :add-person-with-edge
                                         :pid "000002" :person (model/base-person)
                                         :wid "000101" :relation "翻訳者"}))
        {:keys [dir result]} (ingest-model m [{:corrupt/type :cell
                                               :wid "000101" :pid "000002"
                                               :column "生年月日"
                                               :value "2020-02-31"}])]
    (try
      (is (= ["000101"] (:skipped-work-ids result)))
      (div/expected-failure :D4 "P8.atomicity"
        ;; DESIRED: the skipped work wrote no person files at all.
        ;; Current: 000001 (sorted first) is written before 000002 fails.
        (and (nil? (ingested-person dir "000001"))
             (nil? (ingested-person dir "000002"))))
      (finally (render/delete-tree! dir)))))

;; P8.order-independence — D4: a dirty work must not damage a clean work
;; that shares a person. Cross-work divergence (same pid, different valid
;; bodies in two works): without :overwrite, whichever work ingests second
;; hits the refuse-to-overwrite guard and is skipped — the clean work's
;; fate depends on processing order relative to the dirty work.
(deftest p8-order-independence-sim-test
  (let [m0 (model/bootstrap 2)
        ;; shared person: 000001 also contributes to work 000102
        m (:model (model/apply-event m0 {:event/type :add-edge :wid "000102"
                                         :relation "翻訳者" :pid "000001"}))
        ;; valid-but-different 姓 only in work 000101's row for 000001:
        ;; work 000101 ingests first (sorted) and wins the person file
        rows (render/corrupt-rows (render/model->rows m)
                                  [{:corrupt/type :cell
                                    :wid "000101" :pid "000001"
                                    :column "姓" :value "別"}])
        parsed (ac/read-rows-from-string (render/rows->csv rows))
        dir (render/temp-dir "sim-isolation")
        result (ingest/run-corpus! {:rows parsed :output-dir (str dir)})] ;; NO :overwrite
    (try
      (div/expected-failure :D4 "P8.order-independence"
        ;; DESIRED: the clean work 000102 is unaffected by 000101's divergent
        ;; shared-person body. Current: 000102 is skipped by the overwrite
        ;; guard because 000101 wrote 姓=別 first.
        (and (not-any? #{"000102"} (:skipped-work-ids result))
             (some? (ingested-person dir "000002"))))
      (finally (render/delete-tree! dir)))))

;; P9.byte-stable — re-ingest is byte-identical, no overwrite errors.
(deftest p9-byte-stable-sim-test
  (harness/check! "P9.byte-stable" 30
    (prop/for-all [hist (sgen/benign-history-gen {:length [3 6] :works [3 6]})]
      (let [{:keys [states]} (model/fold-history hist)
            m (peek states)
            rows (ac/read-rows-from-string (render/rows->csv (render/model->rows m)))
            dir (render/temp-dir "sim-idem")]
        (try
          (ingest-rows! rows dir)
          (let [snapshot (fn []
                           (into {} (for [f (file-seq (io/file dir))
                                          :when (.isFile ^java.io.File f)]
                                      [(str f) (slurp f)])))
                before (snapshot)
                ;; second ingest without :overwrite must not throw
                _ (ingest/run-corpus! {:rows rows :output-dir (str dir)})
                after (snapshot)]
            (= before after))
          (finally (render/delete-tree! dir)))))))
```

- [ ] **Step 2: Run the simulation suite**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation`
Expected: PASS. Note carefully:
- `p6-divergent-work-fields`, `p8-atomicity`, `p8-order-independence` pass
  *as expected failures* (D1/D4 open). If any reports
  "now passes — adjudicate", current behavior differs from the spec's
  suspicion: STOP and update both the spec table and
  `abc.sim.divergences/table` notes before continuing.
- If `p7-date-classes` fails on a specific cell, check the expected outcome
  against `abc.tools.aozora-csv/parse-date` — the `date-cases` table encodes
  the ADR 0015/0016 contract; a mismatch here is either a wrong expectation
  (fix the table entry, cite `parse-date`) or a real finding (new divergence
  entry — add to spec + table in the same commit).

- [ ] **Step 3: Time the suite**

Run: `time clojure -M:test:kaocha -m kaocha.runner --focus :simulation`
Expected: total under ~90 s. If over budget, reduce `num-tests` in the P6/P8/P9
`check!` calls (they dominate: each case is a full ingest) and note the
final counts in the commit message.

- [ ] **Step 4: Commit**

```bash
git add abc/test/abc/sim/ingest_sim_test.clj
git commit -m "test(sim): ingest properties P6-P9 with D1/D4 expected failures"
```

---

### Task 10: Git rendering and helper unification

**Files:**
- Modify: `abc/test/abc/sim/render.clj` (append ZIP/git helpers)
- Modify: `abc/test/abc/sim/render_test.clj` (append tests)
- Modify: `abc/test/abc/tools/aozora_history_audit_test.clj` (use shared helpers)

**Interfaces:**
- Produces (appended to `abc.sim.render`):
  - `csv->zip-bytes : csv-str → bytes` and
    `(csv->zip-bytes csv {:entry-name "..." :no-entry? bool})` — single-entry
    ZIP; `:entry-name` defaults to `"list_person_all_extended_utf8.csv"`;
    `:no-entry?` true builds a valid ZIP containing only `"README.txt"`.
  - `init-repo! : dir → org.eclipse.jgit.api.Git`.
  - `commit-zip-at! : git × root-dir × bytes × message × iso-instant-str → RevCommit`
    — writes `index_pages/list_person_all_extended_utf8.zip`, commits with
    the given author/committer instant (UTC).
  - `commit-file-at! : git × root × relpath × content-str × message × iso-instant-str → RevCommit`
    (for non-ZIP commits and non-ZIP-bytes-at-ZIP-path faults).

- [ ] **Step 1: Write the failing tests (append to `render_test.clj`)**

```clojure
;; requires added to ns: [abc.git :as abc-git]

(deftest zip-and-commit-roundtrip-test
  (let [repo-dir (render/temp-dir "sim-repo")
        git (render/init-repo! repo-dir)
        m (model/bootstrap 1)
        csv (render/rows->csv (render/model->rows m))
        c1 (render/commit-zip-at! git repo-dir (render/csv->zip-bytes csv)
                                  "v1" "2024-01-01T00:00:00Z")]
    (try
      (let [bytes (abc-git/blob-bytes-at
                   git (.getName c1) "index_pages/list_person_all_extended_utf8.zip")]
        (is (pos? (count bytes))))
      (is (= 1 (count (abc-git/commits-touching-path
                       git "index_pages/list_person_all_extended_utf8.zip"))))
      (finally (.close git) (render/delete-tree! repo-dir)))))

(deftest zip-without-csv-entry-test
  (let [bytes (render/csv->zip-bytes "ignored" {:no-entry? true})]
    (is (pos? (count bytes)))))
```

- [ ] **Step 2: Run to verify failure**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.render-test`
Expected: FAIL — `init-repo!` undefined.

- [ ] **Step 3: Implement (append to `render.clj`)**

Add to the ns imports: `[java.io ByteArrayOutputStream]`,
`[java.nio.charset StandardCharsets]`, `[java.time Instant]`,
`[java.util Date TimeZone]`, `[java.util.zip ZipEntry ZipOutputStream]`,
`[org.eclipse.jgit.api Git]`, `[org.eclipse.jgit.lib PersonIdent]`.

```clojure
(def zip-path "index_pages/list_person_all_extended_utf8.zip")

(defn csv->zip-bytes
  ([csv] (csv->zip-bytes csv {}))
  ([csv {:keys [entry-name no-entry?]
         :or {entry-name "list_person_all_extended_utf8.csv"}}]
   (let [out (ByteArrayOutputStream.)
         ;; deterministic entry mtime: rendered bytes must be a pure function
         ;; of the CSV text (P11.no-diff-invisible, P14 rely on this)
         entry (fn [^String n] (doto (ZipEntry. n) (.setTime 0)))]
     (with-open [zip (ZipOutputStream. out)]
       (if no-entry?
         (do (.putNextEntry zip (entry "README.txt"))
             (.write zip (.getBytes "no csv here" StandardCharsets/UTF_8)))
         (do (.putNextEntry zip (entry entry-name))
             (.write zip (.getBytes ^String csv StandardCharsets/UTF_8))))
       (.closeEntry zip))
     (.toByteArray out))))

(defn init-repo! [dir]
  (-> (Git/init) (.setDirectory (io/file dir)) .call))

(defn- commit-at! [git message instant-str]
  (let [ident (PersonIdent. "ABC Sim" "sim@example.test"
                            (Date/from (Instant/parse instant-str))
                            (TimeZone/getTimeZone "UTC"))]
    (-> ^Git git .commit (.setMessage message)
        (.setAuthor ident) (.setCommitter ident) .call)))

(defn commit-file-at! [git root relpath content message instant-str]
  (let [f (io/file root relpath)]
    (io/make-parents f)
    (if (bytes? content)
      (with-open [o (io/output-stream f)] (.write o ^bytes content))
      (spit f content))
    (-> ^Git git .add (.addFilepattern relpath) .call)
    (commit-at! git message instant-str)))

(defn commit-zip-at! [git root zip-bytes message instant-str]
  (commit-file-at! git root zip-path zip-bytes message instant-str))
```

- [ ] **Step 4: Run render tests; verify pass**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.render-test`
Expected: PASS (6 tests).

- [ ] **Step 5: Switch the audit scenario test to the shared helpers**

In `abc/test/abc/tools/aozora_history_audit_test.clj`:
- add `[abc.sim.render :as sim-render]` to the ns `:require`;
- delete the local `zip-bytes` and `commit-zip-at!` helpers;
- replace calls: `(zip-bytes (csv-text rows))` →
  `(sim-render/csv->zip-bytes (csv-text rows))`; the local `commit-zip!`
  body becomes a call to
  `(sim-render/commit-zip-at! git root bytes message "2022-01-01T00:00:00Z")`
  and `commit-zip-at!` call sites use `sim-render/commit-zip-at!` directly.
  Keep the local `row` and `csv-text` helpers (they exercise the parser with
  a *hand-written* CSV shape on purpose; only the ZIP/commit plumbing is
  shared).

- [ ] **Step 6: Run both suites**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :unit` then `--focus :simulation`
Expected: both PASS; unit test counts unchanged.

- [ ] **Step 7: Commit**

```bash
git add abc/test/abc/sim/render.clj abc/test/abc/sim/render_test.clj \
        abc/test/abc/tools/aozora_history_audit_test.clj
git commit -m "test(sim): git/ZIP rendering; unify audit test helpers"
```

---

### Task 11: Git-layer properties P10–P11

**Files:**
- Create: `abc/test/abc/sim/audit_sim_test.clj`

**Interfaces:**
- Consumes: `abc.tools.aozora-history-audit/audit!` and `scan-history!`,
  `abc.sim.render` git helpers, `abc.sim.oracle/semantic-report`.
- Produces: cases `P10.audit-vs-scan`, `P11.pairing`,
  `P11.no-diff-invisible`, `P11.drift-localization`; shared fixture fn
  `commit-history! : git × root × [model-state] × [instant] → [RevCommit]`
  used by Task 12/13/14.

- [ ] **Step 1: Write the tests**

Create `abc/test/abc/sim/audit_sim_test.clj`:

```clojure
(ns abc.sim.audit-sim-test
  "Git-layer properties P10–P14: rendered histories committed into temp
  JGit repos and driven through audit!/scan-history!."
  (:require [abc.sim.divergences :as div]
            [abc.sim.gen :as sgen]
            [abc.sim.harness :as harness]
            [abc.sim.model :as model]
            [abc.sim.oracle :as oracle]
            [abc.sim.render :as render]
            [abc.tools.aozora-history-audit :as audit]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.properties :as prop]))

(defn commit-history!
  "Commit each model state as a ZIP-changing commit at the given instants
  (same count as states). Returns the vector of RevCommits."
  [git root states instants]
  (mapv (fn [m instant i]
          (render/commit-zip-at!
           git root
           (render/csv->zip-bytes (render/rows->csv (render/model->rows m)))
           (str "state " i) instant))
        states instants (range)))

(defn- monotone-instants [n]
  (mapv #(format "2024-%02d-01T00:00:00Z" (inc %)) (range n)))

(defmacro with-repo [[git-sym root-sym work-sym] & body]
  `(let [~root-sym (render/temp-dir "sim-repo")
         ~work-sym (render/temp-dir "sim-work")
         ~git-sym (render/init-repo! ~root-sym)]
     (try ~@body
          (finally (.close ~git-sym)
                   (render/delete-tree! ~root-sym)
                   (render/delete-tree! ~work-sym)))))

;; P10.audit-vs-scan
(deftest p10-audit-vs-scan-sim-test
  (harness/check! "P10.audit-vs-scan" 10
    (prop/for-all [hist (sgen/history-gen {:length [2 4] :works [3 5]
                                           :forced :clean-split})]
      (let [{:keys [states]} (model/fold-history hist)
            two [(first states) (peek states)]]
        (with-repo [git root work]
          (let [[c1 c2] (commit-history! git root two (monotone-instants 2))
                a (audit/audit! {:aozora-repo (str root)
                                 :previous-ref (.getName c1)
                                 :current-ref (.getName c2)
                                 :work-dir (str work)})
                s (audit/scan-history! {:aozora-repo (str root)
                                        :from-ref (.getName c1)
                                        :to-ref (.getName c2)
                                        :work-dir (str work)})]
            (= (oracle/semantic-report (:drift a))
               (oracle/semantic-report (:drift (first (:pairs s)))))))))))

;; P11.pairing + P11.no-diff-invisible + P11.drift-localization
(deftest p11-scan-coverage-sim-test
  (let [m0 (model/bootstrap 3)
        split {:event/type :clean-split :pid "000001" :targets ["900001" "900002"]
               :persons {"900001" (model/base-person) "900002" (model/base-person)}}
        m1 (:model (model/apply-event m0 {:event/type :edit-person :pid "000002"
                                          :field :family_name :value "改"}))
        m2 (:model (model/apply-event m1 split))]
    (with-repo [git root work]
      (let [[c0 c1] (commit-history! git root [m0 m1]
                                     ["2024-01-01T00:00:00Z" "2024-02-01T00:00:00Z"])
            ;; unrelated commit that must be ignored by pairing
            _ (render/commit-file-at! git root "README.md" "noise" "noise"
                                      "2024-02-15T00:00:00Z")
            ;; identical ZIP bytes recommitted: no tree diff → invisible.
            ;; JGit skips the add of an unchanged file; prove invisibility by
            ;; asserting pair count below.
            _ (render/commit-zip-at!
               git root
               (render/csv->zip-bytes (render/rows->csv (render/model->rows m1)))
               "same bytes" "2024-02-20T00:00:00Z")
            [c2] (commit-history! git root [m2] ["2024-03-01T00:00:00Z"])
            s (audit/scan-history! {:aozora-repo (str root)
                                    :from-ref (.getName c0)
                                    :work-dir (str work)})]
        (testing "P11.pairing + P11.no-diff-invisible"
          (is (= [[(.getName c0) (.getName c1)]
                  [(.getName c1) (.getName c2)]]
                 (mapv (juxt :previous_ref :current_ref) (:pairs s)))))
        (testing "P11.drift-localization"
          (is (= [0 1] (mapv :split_candidates (:pairs s)))))))))
```

- [ ] **Step 2: Run the simulation suite**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation`
Expected: PASS. P10 is expensive (4 ingests per case) — 10 cases × 3 seeds
is the budget ceiling; do not raise `num-tests` above 15.

Caveat for the "identical bytes" commit: JGit's `add` on an unchanged file
produces an empty commit rather than a path change, which is exactly the
invisible outcome P11 asserts. `csv->zip-bytes` already fixes the ZIP entry
mtime to 0 (Task 10) so re-rendering the same model yields byte-identical
ZIPs; if the pairing assertion still fails, the rendered bytes are not a
pure function of the CSV text — fix that in `render.clj` before touching
the assertion.

- [ ] **Step 3: Commit**

```bash
git add abc/test/abc/sim/audit_sim_test.clj
git commit -m "test(sim): git-layer properties P10-P11"
```

---

### Task 12: Sampling properties P12 (+ divergence D2)

**Files:**
- Modify: `abc/test/abc/sim/audit_sim_test.clj` (append)

**Interfaces:**
- Consumes: `commit-history!`, `with-repo`, `monotone-instants` from Task 11.
- Produces: cases `P12.selection` (monotone — passes today; non-monotone —
  D2 expected failure), `P12.boundary-visibility`.

- [ ] **Step 1: Append the tests**

```clojure
;; P12.selection — monotone dates: exactly one representative per period.
(deftest p12-selection-monotone-sim-test
  (let [m0 (model/bootstrap 2)
        edit (fn [m v] (:model (model/apply-event m {:event/type :edit-person
                                                     :pid "000001"
                                                     :field :family_name :value v})))
        states [m0 (edit m0 "一") (edit (edit m0 "一") "二") (edit (edit (edit m0 "一") "二") "三")]
        ;; years: 2023, 2023, 2024, 2025 → representatives: idx 1, 2, 3
        instants ["2023-01-01T00:00:00Z" "2023-12-01T00:00:00Z"
                  "2024-06-01T00:00:00Z" "2025-06-01T00:00:00Z"]]
    (with-repo [git root work]
      (let [cs (commit-history! git root states instants)
            s (audit/scan-history! {:aozora-repo (str root)
                                    :from-ref (.getName (first cs))
                                    :sample-period "year"
                                    :work-dir (str work)})]
        (is (= [[(.getName (nth cs 0)) (.getName (nth cs 1))]
                [(.getName (nth cs 1)) (.getName (nth cs 2))]
                [(.getName (nth cs 2)) (.getName (nth cs 3))]]
               (mapv (juxt :previous_ref :current_ref) (:pairs s))))))))

;; P12.selection — non-monotone dates: desired = still one rep per period (D2).
(deftest p12-selection-non-monotone-sim-test
  (let [m0 (model/bootstrap 2)
        edit (fn [m v] (:model (model/apply-event m {:event/type :edit-person
                                                     :pid "000001"
                                                     :field :family_name :value v})))
        s1 (edit m0 "一") s2 (edit s1 "二") s3 (edit s2 "三")
        ;; log order: 2023, 2024, 2023(!), 2024 — each year split across
        ;; non-contiguous log segments
        instants ["2023-01-01T00:00:00Z" "2024-03-01T00:00:00Z"
                  "2023-06-01T00:00:00Z" "2024-09-01T00:00:00Z"]]
    (with-repo [git root work]
      (let [cs (commit-history! git root [m0 s1 s2 s3] instants)
            s (audit/scan-history! {:aozora-repo (str root)
                                    :from-ref (.getName (first cs))
                                    :sample-period "year"
                                    :work-dir (str work)})]
        (div/expected-failure :D2 "P12.selection"
          ;; DESIRED: one representative per calendar year among commits 1..3
          ;; (from-ref is prepended by contract): 2023 → commit idx 2 (last in
          ;; log order with a 2023 date), 2024 → idx 3. Two pairs total.
          (= [[(.getName (nth cs 0)) (.getName (nth cs 2))]
              [(.getName (nth cs 2)) (.getName (nth cs 3))]]
             (mapv (juxt :previous_ref :current_ref) (:pairs s))))))))

;; P12.boundary-visibility — persistent drift is visible between
;; representatives; transient intra-period drift is unobservable but must
;; not crash or misattribute.
(deftest p12-boundary-visibility-sim-test
  (let [m0 (model/bootstrap 2)
        split {:event/type :clean-split :pid "000001" :targets ["900001" "900002"]
               :persons {"900001" (model/base-person) "900002" (model/base-person)}}
        m-split (:model (model/apply-event m0 split))
        states [m0
                m-split ;; drift, mid-2024
                m0      ;; reverted before the 2024 representative (transient)
                (:model (model/apply-event m0 {:event/type :edit-person :pid "000002"
                                               :field :family_name :value "改"}))]
        instants ["2023-06-01T00:00:00Z" "2024-02-01T00:00:00Z"
                  "2024-06-01T00:00:00Z" "2024-12-01T00:00:00Z"]]
    (with-repo [git root work]
      (let [cs (commit-history! git root states instants)
            s (audit/scan-history! {:aozora-repo (str root)
                                    :from-ref (.getName (first cs))
                                    :sample-period "year"
                                    :work-dir (str work)})]
        (is (= "ok" (:status s)))
        ;; transient split between representatives is invisible: summary shows
        ;; only the persistent metadata correction, zero candidates.
        (is (= 0 (get (:summary s) "split_candidates")))
        (is (pos? (get (:summary s) "pairs_scanned")))))))
```

- [ ] **Step 2: Run the simulation suite**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation`
Expected: PASS. `p12-selection-monotone` must pass unconditionally (current
code handles monotone dates). `p12-selection-non-monotone` passes as a D2
expected failure — if it reports "now passes", current `partition-by`
behavior differs from the spec's suspicion: verify manually (run the scan
with the four commits, inspect `:pairs`), then update the spec table and
`abc.sim.divergences/table` in the same commit.

- [ ] **Step 3: Commit**

```bash
git add abc/test/abc/sim/audit_sim_test.clj
git commit -m "test(sim): sampling properties P12 with D2 expected failure"
```

---

### Task 13: Failure-taxonomy properties P13 (+ divergences D3, D5, D6)

**Files:**
- Modify: `abc/test/abc/sim/audit_sim_test.clj` (append)

**Interfaces:**
- Consumes: taxonomy from the spec §Failure Taxonomy; `with-repo`,
  `commit-history!` from Task 11.
- Produces: cases `P13.ragged-row` (D3), `P13.divergent-person` (passes),
  `P13.empty-csv` (D5), `P13.no-csv-entry` (passes), `P13.non-zip-bytes` (D6).
  Each source-fault case asserts required ex-data keys, never just
  exception classes.

- [ ] **Step 1: Append the tests**

```clojure
(defn- audit-two!
  "Commit clean state then a faulted current blob (bytes) and audit the pair.
  Returns {:result r} or {:threw ex}."
  [current-bytes]
  (let [m (model/bootstrap 2)]
    (with-repo [git root work]
      (let [clean (render/csv->zip-bytes (render/rows->csv (render/model->rows m)))
            c1 (render/commit-zip-at! git root clean "clean" "2024-01-01T00:00:00Z")
            c2 (render/commit-file-at! git root render/zip-path current-bytes
                                       "faulted" "2024-02-01T00:00:00Z")]
        (try {:result (audit/audit! {:aozora-repo (str root)
                                     :previous-ref (.getName c1)
                                     :current-ref (.getName c2)
                                     :work-dir (str work)})}
             (catch Exception e {:threw e}))))))

(defn- clean-ex-info? [e required-keys]
  (and (instance? clojure.lang.ExceptionInfo e)
       (every? #(contains? (ex-data e) %) required-keys)))

;; P13.ragged-row — D3: row-level fault must not abort; desired = work skipped.
(deftest p13-ragged-row-sim-test
  (let [m (model/bootstrap 2)
        rows (render/corrupt-rows (render/model->rows m)
                                  [{:corrupt/type :ragged-short :wid "000101"}])
        {:keys [result threw]} (audit-two!
                                (render/csv->zip-bytes (render/rows->csv rows)))]
    (is (nil? threw) "row-level fault must never abort an audit")
    (div/expected-failure :D3 "P13.ragged-row"
      ;; DESIRED: the ragged work is skipped with a reason, not silently
      ;; ingested with truncated cells.
      (some #{"000101"} (get-in result [:ingest :current :skipped-work-ids])))))

;; P13.divergent-person — absorbed and counted (current behavior matches spec).
(deftest p13-divergent-person-sim-test
  (let [m0 (model/bootstrap 2)
        m (:model (model/apply-event m0 {:event/type :add-person-with-edge
                                         :pid "000009" :person (model/base-person)
                                         :wid "000101" :relation "翻訳者"}))
        rows (render/corrupt-rows (render/model->rows m)
                                  [{:corrupt/type :divergent-person :wid "000101"
                                    :column "姓" :value "×"}])
        {:keys [result threw]} (audit-two!
                                (render/csv->zip-bytes (render/rows->csv rows)))]
    (is (nil? threw))
    (is (= ["000101"] (get-in result [:ingest :current :skipped-work-ids])))))

;; P13.empty-csv — D5 (desired: explicit ex-info with :zip-path).
(deftest p13-empty-csv-sim-test
  (doseq [[label csv] [["empty" ""]
                       ["header-only" (render/rows->csv [])]]]
    (let [{:keys [threw]} (audit-two! (render/csv->zip-bytes csv))]
      (div/expected-failure :D5 (str "P13.empty-csv/" label)
        (clean-ex-info? threw [:zip-path])))))

;; P13.no-csv-entry — current behavior matches spec: ex-info {:zip-path}.
(deftest p13-no-csv-entry-sim-test
  (let [{:keys [threw]} (audit-two! (render/csv->zip-bytes "x" {:no-entry? true}))]
    (is (clean-ex-info? threw [:zip-path])
        (str "expected ex-info with :zip-path, got: " (pr-str threw)))))

;; P13.non-zip-bytes — D6 (desired: wrapped ex-info, cause chained).
(deftest p13-non-zip-bytes-sim-test
  (let [{:keys [threw]} (audit-two! "this is not a zip file")]
    (is (some? threw) "non-ZIP bytes must not produce a normal-looking report")
    (div/expected-failure :D6 "P13.non-zip-bytes"
      (clean-ex-info? threw [:zip-path]))))
```

- [ ] **Step 2: Run and adjudicate D5/D6 current behavior**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation`
Expected: PASS. If `p13-ragged-row` reports "now passes — adjudicate",
current parsing rejected the ragged work rather than silently truncating —
verify manually and adjudicate D3 exactly like D5/D6 below. This task also
**confirms or refutes** the spec's D5/D6 suspicions:
- If `p13-empty-csv` reports "now passes — adjudicate": ingest already
  throws clean `ex-info` — flip D5's notes to record confirmed-clean
  behavior, set status `:adjudicated-intended`, change the case to a plain
  `is`, and update the spec's table in the same commit.
- Same procedure for `p13-non-zip-bytes` / D6.
- If `p13-empty-csv`'s OUTER expectation fails because the audit *neither
  threw nor skipped* (silent zero-row corpus), that confirms D5 exactly as
  written — no change needed.
Record what was observed for D5/D6 in the commit message body.

- [ ] **Step 3: Update the spec's divergence table if D5/D6 changed**

If adjudication happened in Step 2, edit
`abc/docs/superpowers/specs/2026-07-11-aozora-evolution-simulation-testing-design.md`
table rows D5/D6 (status + current-behavior wording) to match observations.

- [ ] **Step 4: Commit**

```bash
git add abc/test/abc/sim/audit_sim_test.clj \
        abc/test/abc/sim/divergences.clj \
        abc/docs/superpowers/specs/2026-07-11-aozora-evolution-simulation-testing-design.md
git commit -m "test(sim): failure-taxonomy properties P13; adjudicate D5/D6"
```

---

### Task 14: P14, soak target, acceptance verification

**Files:**
- Modify: `abc/test/abc/sim/audit_sim_test.clj` (append P14)
- Modify: `justfile` (repo root — add `sim-soak` recipe)

**Interfaces:**
- Consumes: everything above.
- Produces: case `P14.rerun`; `just sim-soak`.

- [ ] **Step 1: Append the P14 test**

```clojure
;; P14.rerun — a reused --work-dir yields the same semantic report as a
;; fresh one.
(deftest p14-work-dir-hygiene-sim-test
  (let [m0 (model/bootstrap 2)
        m1 (:model (model/apply-event m0 {:event/type :edit-person :pid "000001"
                                          :field :family_name :value "改"}))]
    (with-repo [git root work]
      (let [[c1 c2] (commit-history! git root [m0 m1]
                                     ["2024-01-01T00:00:00Z" "2024-02-01T00:00:00Z"])
            run! (fn [w] (oracle/semantic-report
                          (audit/audit! {:aozora-repo (str root)
                                         :previous-ref (.getName c1)
                                         :current-ref (.getName c2)
                                         :work-dir (str w)})))
            first-run (run! work)
            reused (run! work)          ;; same dir, second run
            fresh-dir (render/temp-dir "sim-fresh-work")
            fresh (try (run! fresh-dir)
                       (finally (render/delete-tree! fresh-dir)))]
        (is (= first-run reused fresh))))))
```

- [ ] **Step 2: Add the soak recipe to the root `justfile`**

Append:

```make
# Unseeded simulation soak (15x counts); failures print the seed to replay.
sim-soak:
	cd abc && ABC_SIM_SOAK=1 clojure -M:test:kaocha -m kaocha.runner --focus :simulation
```

- [ ] **Step 3: Run the full simulation suite and time it**

Run: `time clojure -M:test:kaocha -m kaocha.runner --focus :simulation`
Expected: PASS, wall clock ≤ ~2 minutes. If over, reduce git-layer property
counts first (P10), then heavy pure-layer counts; record final counts here
and in the commit message.

- [ ] **Step 4: Acceptance check — mutation catch**

Temporarily invert the classifier's split evidence check to prove P2 bites:

In `abc/src/abc/tools/person_drift_history.clj:167-174`, change
`(= 1 (count previous))` to `(= 2 (count previous))` inside
`split-candidate?`. Run:

`clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.classifier-sim-test`

Expected: `p2-clean-split-sim-test` FAILS with a shrunk history of ≤ 3
events. Then **revert the mutation**:

```bash
git checkout -- abc/src/abc/tools/person_drift_history.clj
```

and re-run to confirm green.

- [ ] **Step 5: Verify the unit suite one final time**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :unit`
Expected: PASS, same test count as before this plan started.

- [ ] **Step 6: Commit**

```bash
git add abc/test/abc/sim/audit_sim_test.clj justfile
git commit -m "test(sim): work-dir hygiene P14 and sim-soak target"
```

---

## Self-Review Notes (already applied)

- Spec coverage: P1–P14 all have tasks (T8, T9, T11, T12, T13, T14);
  divergences D1–D6 all have expected-failure cases (T9: D1, D4; T12: D2;
  T13: D3, D5, D6); projection/confusability/applied-intent/semantic-report
  are T4; edge-exhaustive drift preconditions are T3; seed corpus + soak +
  applied-ratio acceptance criteria are T1/T14; mutation-catch acceptance is
  T14 step 4; unit-suite-unchanged constraint is checked in T1, T10, T14.
- The spec's BOM / column-reorder / quoted-field dirty events are covered by
  `rows->csv` options and the quoting test (T5); duplicate-row is exercised
  inside P8 cases (T9). Non-monotonic dates and intra-period churn are T12.
- Type consistency: `apply-event` returns `{:model :applied}` everywhere;
  intents are `{:intent :event :edges}`; `check!` signature is
  `(check! name num-tests prop)` in every property test.
