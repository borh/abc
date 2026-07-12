# Drift-Sidecar Interplay Simulation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Generative simulation coverage (P15) for ADR 0022's `--drift-persons-dir` audit path: drift sidecars authored from applied forced drift intents, audited against evolving generated histories.

**Architecture:** Observer-layer module — a new test-side `abc.sim.sidecar` namespace authors valid `_events/`/`_indexes/` sidecars from the fold's applied intents (the upstream model and generators are untouched), a pure oracle extension predicts `{person_id, change_type}` from projection presence and person-map equality, and a new `-sim-test$` namespace drives `audit!`/`scan-history!` with `:drift-persons-dir`. Spec: `abc/docs/superpowers/specs/2026-07-12-drift-sidecar-interplay-design.md`.

**Tech Stack:** Clojure, test.check (via `abc.sim.harness/check!`), kaocha `:simulation` suite, JGit temp repos (via `abc.sim.render`).

## Global Constraints

- **No production namespace is modified.** Everything lands under `abc/test/`. `abc/test/abc/sim/model.clj` and `abc/test/abc/sim/gen.clj` are also unmodified.
- All commands run from `abc/`. Unit suite: `clojure -M:test:kaocha -m kaocha.runner --focus :unit`. Simulation suite: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation`. Single namespace: `clojure -M:test:kaocha -m kaocha.runner --focus <ns-name>`.
- Properties assert **desired** behavior. If a P15 test fails because the SUT's current behavior diverges: do NOT change production code and do NOT weaken the assertion. Add a new entry (next free id, starting at `:D7`) to `abc.sim.divergences/table` with `:status :open` and a dated case note, wrap ONLY the diverging assertion with `div/expected-failure`, and report the divergence. Forbidden-class assertions stay hard (never gated): `NullPointerException`, `AssertionError`, `StackOverflowError`, raw `java.util.zip.ZipException`.
- CI seeds are `[42 4242 424242]` (already in `abc.sim.harness/ci-seeds` — do not change).
- Sidecar constants (from spec): date `"2026-07-12"`, agent `https://w3id.org/abc/agents/editorial-board`, role `abc:DriftEditor`, evidence `https://example.org/abc/drift-evidence/sim-<type>`.
- The drift-event JSON schema constrains: split ⇒ exactly 1 `used` + ≥2 `was_generated_by`; merge ⇒ ≥2 `used` + exactly 1 `was_generated_by`; participants ≥2; snapshot ids match `^(pre|post)-([0-9]{6}|abc-[0-9a-f]{12})$`. Generator-forced intents always satisfy these (splits mint exactly 2 targets; merges use 2 sources).

---

### Task 1: `abc.sim.sidecar` — authoring + fault injection

**Files:**
- Create: `abc/test/abc/sim/sidecar.clj`
- Test: `abc/test/abc/sim/sidecar_test.clj`

**Interfaces:**
- Consumes: `abc.tools.person-drift` (`materialize-event-id`, `event-schema-id`, `event-schema-path`, `index-schema-id`, `index-schema-path`, `validate-drift-events!`), `abc.tools.manifest/schema-hash`, `abc.tools.hash` (`format-sha256`, `sha256-json-jcs`), `abc.tools.json/write-deterministic-json-file!`, `abc.sim.render` (`temp-dir`, `delete-tree!`), `abc.sim.model/base-person`.
- Produces (used by Tasks 4–5):
  - `(sidecar/event-for-intent {:intent kw :event map})` → string-keyed valid drift event map with `"drift_event_id"`. `:intent` is `:clean-split` (event has `:pid`, `:targets`) or `:clean-merge` (event has `:pids`, `:target`).
  - `(sidecar/write-sidecars! dir event)` → writes `_events/<id>.json` + `_indexes/<pid>.json` under `dir`, returns `event`.
  - `(sidecar/corrupt! dir event fault)` → rewrites the written sidecars for `fault` ∈ `#{:schema-hash-mismatch :orphan-event-file :index-target-missing :participants-not-sorted}`.
  - `(sidecar/placeholder-hash person-id)` → `"sha256:<64 hex>"`.

- [ ] **Step 1: Write the failing test**

Create `abc/test/abc/sim/sidecar_test.clj`:

```clojure
(ns abc.sim.sidecar-test
  "Authoring contract for simulation drift sidecars: authored sidecars
  must pass validate-drift-events! cleanly; each fault injector must
  produce exactly its documented person-drift failure code."
  (:require [abc.sim.model :as model]
            [abc.sim.render :as render]
            [abc.sim.sidecar :as sidecar]
            [abc.tools.person-drift :as drift]
            [clojure.test :refer [deftest is]]))

(def split-intent
  {:intent :clean-split
   :event {:event/type :clean-split :pid "000001"
           :targets ["900001" "900002"]
           :persons {"900001" (model/base-person)
                     "900002" (model/base-person)}}})

(def merge-intent
  {:intent :clean-merge
   :event {:event/type :clean-merge :pids ["900003" "900004"]
           :target "900005" :person (model/base-person)}})

(deftest authored-sidecars-validate-clean-test
  (doseq [intent [split-intent merge-intent]]
    (let [dir (render/temp-dir "sim-sidecar")]
      (try
        (let [event (sidecar/write-sidecars!
                     dir (sidecar/event-for-intent intent))
              result (drift/validate-drift-events! {:persons-dir (str dir)})]
          (is (= :ok (:status result))
              (str (:intent intent) ": " (pr-str result)))
          (is (= 1 (:events result)))
          (is (= (count (distinct (map #(get % "person_id")
                                       (get event "participants"))))
                 (:indexes result))))
        (finally (render/delete-tree! dir))))))

(deftest corrupted-sidecars-fail-validation-test
  (doseq [fault [:schema-hash-mismatch :orphan-event-file
                 :index-target-missing :participants-not-sorted]]
    (let [dir (render/temp-dir "sim-sidecar")]
      (try
        (let [event (sidecar/write-sidecars!
                     dir (sidecar/event-for-intent split-intent))]
          (sidecar/corrupt! dir event fault)
          (let [result (drift/validate-drift-events! {:persons-dir (str dir)})]
            (is (= :error (:status result)) (str fault))
            (is (some #(= fault (:code %)) (:failures result))
                (str fault ": " (pr-str (:failures result))))))
        (finally (render/delete-tree! dir))))))
```

- [ ] **Step 2: Run test to verify it fails**

Run (from `abc/`): `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.sidecar-test`
Expected: FAIL — namespace `abc.sim.sidecar` does not exist (load error).

- [ ] **Step 3: Write the implementation**

Create `abc/test/abc/sim/sidecar.clj`:

```clojure
(ns abc.sim.sidecar
  "Observer-layer drift-sidecar authoring for the simulation harness:
  valid _events/ + _indexes/ sidecars built from applied forced drift
  intents, plus sampled fault injectors for the invalid-sidecar
  properties. Editorial artifacts stay OUT of abc.sim.model.
  Spec: docs/superpowers/specs/2026-07-12-drift-sidecar-interplay-design.md"
  (:require [abc.tools.hash :as hash]
            [abc.tools.json :as json]
            [abc.tools.manifest :as manifest]
            [abc.tools.person-drift :as drift]
            [clojure.java.io :as io]))

(defn placeholder-hash
  "Well-formed, deterministic, corpus-independent person_record_hash.
  validate-drift-events! pattern-checks participant hashes but never
  compares them against a corpus, so any stable sha256:<hex> is valid."
  [person-id]
  (hash/format-sha256 (hash/sha256-json-jcs person-id)))

(defn- participant [prefix person-id]
  {"person_id" person-id
   "person_record_hash" (placeholder-hash person-id)
   "snapshot_id" (str prefix person-id)})

(defn event-for-intent
  "Author a valid drift event from an applied forced intent
  ({:intent :clean-split|:clean-merge :event e}). Participants sorted by
  snapshot_id (post- sorts before pre-); used/was_generated_by sorted;
  live schema id/hash; drift_event_id materialized."
  [{:keys [intent event]}]
  (let [[pre-pids post-pids etype]
        (case intent
          :clean-split [[(:pid event)] (vec (:targets event)) "split"]
          :clean-merge [(vec (:pids event)) [(:target event)] "merge"])
        pres (mapv #(participant "pre-" %) pre-pids)
        posts (mapv #(participant "post-" %) post-pids)]
    (drift/materialize-event-id
     {"schema_id" drift/event-schema-id
      "schema_hash" (manifest/schema-hash drift/event-schema-path)
      "drift_event_type" etype
      "date" "2026-07-12"
      "evidence" [(str "https://example.org/abc/drift-evidence/sim-" etype)]
      "participants" (vec (sort-by #(get % "snapshot_id") (into pres posts)))
      "prov" {"used" (vec (sort (map #(get % "snapshot_id") pres)))
              "was_generated_by" (vec (sort (map #(get % "snapshot_id") posts)))
              "qualified_association"
              {"agent" "https://w3id.org/abc/agents/editorial-board"
               "had_role" "abc:DriftEditor"}}})))

(defn indexes-for-event [event]
  (vec (for [pid (distinct (map #(get % "person_id")
                                (get event "participants")))]
         {"schema_id" drift/index-schema-id
          "schema_hash" (manifest/schema-hash drift/index-schema-path)
          "person_id" pid
          "drift_event_ids" [(get event "drift_event_id")]})))

(defn write-sidecars!
  "Write _events/<drift_event_id>.json and one _indexes/<pid>.json per
  participant under dir. Returns event."
  [dir event]
  (let [events-dir (io/file dir "_events")
        indexes-dir (io/file dir "_indexes")]
    (.mkdirs events-dir)
    (.mkdirs indexes-dir)
    (json/write-deterministic-json-file!
     (io/file events-dir (str (get event "drift_event_id") ".json")) event)
    (doseq [index (indexes-for-event event)]
      (json/write-deterministic-json-file!
       (io/file indexes-dir (str (get index "person_id") ".json")) index))
    event))

(defn corrupt!
  "Rewrite an already-written sidecar dir so validate-drift-events!
  reports `fault` (a documented abc.tools.person-drift failure code).
  No content-hash-vs-id check exists, so rewriting the event body without
  recomputing drift_event_id is safe for :participants-not-sorted."
  [dir event fault]
  (let [indexes-dir (io/file dir "_indexes")
        event-file (io/file dir "_events"
                            (str (get event "drift_event_id") ".json"))]
    (case fault
      :schema-hash-mismatch
      (json/write-deterministic-json-file!
       event-file
       (assoc event "schema_hash" (str "sha256:" (apply str (repeat 64 "0")))))

      :orphan-event-file
      (doseq [f (.listFiles indexes-dir)] (io/delete-file f))

      :index-target-missing
      (json/write-deterministic-json-file!
       (io/file indexes-dir "999999.json")
       {"schema_id" drift/index-schema-id
        "schema_hash" (manifest/schema-hash drift/index-schema-path)
        "person_id" "999999"
        "drift_event_ids" [(str "sha256:" (apply str (repeat 64 "f")))]})

      :participants-not-sorted
      (json/write-deterministic-json-file!
       event-file (update event "participants" (comp vec reverse))))))
```

Note: `:orphan-event-file` also produces `:event-missing-from-participant-index` failures — the test asserts the target code is *among* the failures, not the only one.

- [ ] **Step 4: Run test to verify it passes**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.sidecar-test`
Expected: PASS (2 tests, 14 assertions, 0 failures).

- [ ] **Step 5: Commit**

```bash
git add abc/test/abc/sim/sidecar.clj abc/test/abc/sim/sidecar_test.clj
git commit -m "test(sim): drift-sidecar authoring and fault injection (abc.sim.sidecar)"
```

---

### Task 2: Oracle extension — `expected-participant-updates`

**Files:**
- Modify: `abc/test/abc/sim/oracle.clj` (append after `expected-replacements`, end of file)
- Test: `abc/test/abc/sim/oracle_test.clj` (MODIFY — this file already exists with five deftests covering projection, confusable?, model-diff, semantic-report, and expected candidates. Do NOT replace it; append one deftest at the end. Its existing requires — `model`, `oracle`, `deftest`/`is` — already cover the new test; add nothing to the ns form.)

**Interfaces:**
- Consumes: `abc.sim.oracle/projection` (already in the namespace).
- Produces (used by Task 4): `(oracle/expected-participant-updates prev cur participant-pids)` → vector of string-keyed `{"person_id" .. "change_type" ..}` sorted by person_id; `change_type` ∈ `"added" | "removed" | "hash_changed"`; pids absent from both projections (or present-and-equal) yield no entry.

- [ ] **Step 1: Write the failing test**

Append to the END of the existing `abc/test/abc/sim/oracle_test.clj` (leave every existing deftest untouched; the ns form stays exactly as it is):

```clojure
;; P15 participant-update oracle. bootstrap 2: works 000101/000102, sole
;; authors 000001/000002.
(deftest expected-participant-updates-test
  (let [m0 (model/bootstrap 2)
        edited (:model (model/apply-event
                        m0 {:event/type :edit-person :pid "000001"
                            :field :family_name :value "変"}))
        ;; removing 000101 detaches sole author 000001 → drops from projection
        removed (:model (model/apply-event
                         m0 {:event/type :remove-work :wid "000101"}))]
    (is (= [{"person_id" "000001" "change_type" "hash_changed"}]
           (oracle/expected-participant-updates
            m0 edited ["000001" "000002" "999999"]))
        "field edit → hash_changed; untouched and never-present pids quiet")
    (is (= [{"person_id" "000001" "change_type" "removed"}]
           (oracle/expected-participant-updates m0 removed ["000001" "000002"])))
    (is (= [{"person_id" "000001" "change_type" "added"}]
           (oracle/expected-participant-updates removed m0 ["000001"])))
    (is (= [] (oracle/expected-participant-updates m0 m0 ["000001" "999999"]))
        "identical endpoints → no entries")
    ;; cur = edit 000001, then detach 000002 (remove its sole work 000102)
    (let [cur (:model (model/apply-event edited {:event/type :remove-work
                                                 :wid "000102"}))]
      (is (= [{"person_id" "000001" "change_type" "hash_changed"}
              {"person_id" "000002" "change_type" "removed"}]
             (oracle/expected-participant-updates m0 cur ["000002" "000001"]))
          "multiple entries, sorted by person_id regardless of input order"))))
```

- [ ] **Step 2: Run test to verify it fails**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.oracle-test`
Expected: FAIL — `expected-participant-updates` unresolved var.

- [ ] **Step 3: Write the implementation**

Append to `abc/test/abc/sim/oracle.clj`:

```clojure
(defn expected-participant-updates
  "Predicted drift_participant_updates for the window prev → cur,
  restricted to {person_id, change_type} (P15). Presence in the projected
  endpoints decides added/removed; model person-map equality decides
  hash_changed — person_record_hash is a pure function of the person's
  own fields (spec §Key facts). Sorted by person_id."
  [prev cur participant-pids]
  (let [p (:persons (projection prev))
        c (:persons (projection cur))]
    (vec
     (keep (fn [pid]
             (let [pp (get p pid) cp (get c pid)]
               (cond
                 (and pp cp (not= pp cp))
                 {"person_id" pid "change_type" "hash_changed"}

                 (and pp (nil? cp))
                 {"person_id" pid "change_type" "removed"}

                 (and cp (nil? pp))
                 {"person_id" pid "change_type" "added"})))
           (sort participant-pids)))))
```

- [ ] **Step 4: Run test to verify it passes**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.oracle-test`
Expected: PASS (6 tests — the five pre-existing deftests plus the new one; the new deftest contributes 5 assertions).

- [ ] **Step 5: Commit**

```bash
git add abc/test/abc/sim/oracle.clj abc/test/abc/sim/oracle_test.clj
git commit -m "test(sim): pure participant-update oracle for P15"
```

---

### Task 3: Shared-helper consolidation (render + harness)

Mechanical refactor so Tasks 4–5 can reuse the repo/commit helpers and the failure-taxonomy predicates without requiring one test namespace from another.

**Files:**
- Modify: `abc/test/abc/sim/render.clj` (append at end)
- Modify: `abc/test/abc/sim/harness.clj` (append at end)
- Modify: `abc/test/abc/sim/audit_sim_test.clj` (delete local copies, use the moved ones)

**Interfaces:**
- Produces (used by Tasks 4–5 and by the updated `audit_sim_test`):
  - `render/with-repo` — macro `[[git-sym root-sym work-sym] & body]`, temp JGit repo + temp work dir, closes/deletes in `finally`.
  - `(render/commit-history! git root states instants)` → vector of RevCommits, one ZIP commit per model state.
  - `(render/monotone-instants n)` → n monthly 2024 instants (n ≤ 12).
  - `(harness/clean-ex-info? e required-keys)` → boolean.
  - `(harness/forbidden-throw? e)` → boolean (NPE / AssertionError / StackOverflowError / raw ZipException).

- [ ] **Step 1: Move the helpers**

Append to `abc/test/abc/sim/render.clj` (verbatim bodies from `audit_sim_test.clj:14–37`, only re-homed):

```clojure
(defn commit-history!
  "Commit each model state as a ZIP-changing commit at the given instants
  (same count as states). Returns the vector of RevCommits."
  [git root states instants]
  (mapv (fn [m instant i]
          (commit-zip-at!
           git root
           (csv->zip-bytes (rows->csv (model->rows m)))
           (str "state " i) instant))
        states instants (range)))

(defn monotone-instants
  "n monthly instants in 2024 (n ≤ 12)."
  [n]
  (mapv #(format "2024-%02d-01T00:00:00Z" (inc %)) (range n)))

(defmacro with-repo
  "Temp JGit repo + temp work dir bound to the given symbols; the repo is
  closed and both trees deleted on exit."
  [[git-sym root-sym work-sym] & body]
  `(let [~root-sym (temp-dir "sim-repo")
         ~work-sym (temp-dir "sim-work")]
     (try
       (let [~git-sym (init-repo! ~root-sym)]
         (try ~@body
              (finally (.close ~git-sym))))
       (finally
         (delete-tree! ~root-sym)
         (delete-tree! ~work-sym)))))
```

Append to `abc/test/abc/sim/harness.clj` (verbatim bodies from `audit_sim_test.clj:180–193`):

```clojure
(defn clean-ex-info?
  "Clean two-tier failure: ex-info carrying every required diagnostic key."
  [e required-keys]
  (and (instance? clojure.lang.ExceptionInfo e)
       (every? #(contains? (ex-data e) %) required-keys)))

(defn forbidden-throw?
  "True when the SUT escaped with an exception class the failure taxonomy
  forbids outright (spec §Failure Taxonomy) — asserted even for
  divergence-gated cases so a wrong-behavior regression cannot hide
  behind an open divergence."
  [e]
  (or (instance? NullPointerException e)
      (instance? AssertionError e)
      (instance? StackOverflowError e)
      (instance? java.util.zip.ZipException e)))
```

- [ ] **Step 2: Update `audit_sim_test.clj`**

In `abc/test/abc/sim/audit_sim_test.clj`:
- Delete the local `commit-history!`, `monotone-instants`, `with-repo` definitions (lines 14–37) and the local `clean-ex-info?` / `forbidden-throw?` definitions (lines 180–193).
- Replace every call site: `commit-history!` → `render/commit-history!`, `monotone-instants` → `render/monotone-instants`, `with-repo` → `render/with-repo`, `clean-ex-info?` → `harness/clean-ex-info?`, `forbidden-throw?` → `harness/forbidden-throw?`.
- `harness` is already required in the ns form; `render` too. No require changes needed.

- [ ] **Step 3: Run the simulation suite to verify the refactor is behavior-preserving**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation`
Expected: PASS with the same test/assertion counts as before the refactor (29 tests as of the D1–D6 close-out; 0 failures).

- [ ] **Step 4: Commit**

```bash
git add abc/test/abc/sim/render.clj abc/test/abc/sim/harness.clj abc/test/abc/sim/audit_sim_test.clj
git commit -m "refactor(sim): move repo/commit helpers to render, taxonomy predicates to harness"
```

---

### Task 4: P15.lifecycle (generative, both windows)

**Files:**
- Create: `abc/test/abc/sim/drift_sidecar_sim_test.clj`

**Interfaces:**
- Consumes: `sidecar/event-for-intent`, `sidecar/write-sidecars!` (Task 1); `oracle/expected-participant-updates` (Task 2); `render/with-repo`, `render/commit-history!`, `render/monotone-instants` (Task 3); `sgen/find-applied`, `harness/check!`, `harness/ratio-counter`, `harness/tick!`, `harness/assert-applied-ratio!`; `audit/audit!` with `{:aozora-repo :previous-ref :current-ref :work-dir :drift-persons-dir}`.
- Produces: private helpers `successor-pids`, `participant-pids`, `entry-shape-ok?`, `window-matches?` reused by Task 5 in the same file.

- [ ] **Step 1: Write the test**

Create `abc/test/abc/sim/drift_sidecar_sim_test.clj`:

```clojure
(ns abc.sim.drift-sidecar-sim-test
  "P15: interplay between accepted drift sidecars (_events/ + _indexes/)
  and evolving upstream history, through audit!/scan-history! with
  :drift-persons-dir. Properties assert DESIRED behavior; divergences go
  to abc.sim.divergences (D7+).
  Spec: docs/superpowers/specs/2026-07-12-drift-sidecar-interplay-design.md"
  (:require [abc.sim.divergences :as div]
            [abc.sim.gen :as sgen]
            [abc.sim.harness :as harness]
            [abc.sim.model :as model]
            [abc.sim.oracle :as oracle]
            [abc.sim.render :as render]
            [abc.sim.sidecar :as sidecar]
            [abc.tools.aozora-history-audit :as audit]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.properties :as prop]))

;; abc.sim.divergences is required from the start so the gate protocol is
;; executable without touching the ns form: if a window diverges, swap the
;; window's bare boolean for (div/expected-failure* :D7 <desc> (fn [] <bool>))
;; inside the property — expected-failure* is the composable non-`is` form.

(defn- successor-pids [applied]
  (case (:intent applied)
    :clean-split (vec (:targets (:event applied)))
    :clean-merge [(:target (:event applied))]))

(defn- participant-pids [event]
  (vec (distinct (map #(get % "person_id") (get event "participants")))))

(def ^:private hex-hash #"^sha256:[0-9a-f]{64}$")

(defn- entry-shape-ok?
  "ADR 0022 null-ness contract + drift_event_ids for one reported entry."
  [event entry]
  (let [prev (get entry "previous_hash")
        cur (get entry "current_hash")]
    (and (= [(get event "drift_event_id")] (get entry "drift_event_ids"))
         (case (get entry "change_type")
           "removed" (and (string? prev) (re-matches hex-hash prev)
                          (nil? cur))
           "added" (and (nil? prev)
                        (string? cur) (re-matches hex-hash cur))
           "hash_changed" (and (string? prev) (string? cur)
                               (re-matches hex-hash prev)
                               (re-matches hex-hash cur)
                               (not= prev cur))
           false))))

(defn- window-matches?
  "Exact-set agreement between the reported drift_participant_updates and
  the oracle for the window (prev-state → cur-state), plus per-entry
  shape. Exact equality doubles as the quietness assertion for untouched
  participants."
  [report event prev-state cur-state]
  (let [entries (:drift_participant_updates report)
        expected (oracle/expected-participant-updates
                  prev-state cur-state (participant-pids event))]
    (and (= expected
            (vec (sort-by #(get % "person_id")
                          (map #(select-keys % ["person_id" "change_type"])
                               entries))))
         (every? #(entry-shape-ok? event %) entries))))

(defn- lifecycle-results
  "Author the sidecar from the applied forced intent; audit the post-event
  window (s-after → s-final: pre-pid absent both sides, appended successor
  edit surfaces as hash_changed) and the spanning window (s-before →
  s-final: pre-pid removed, surviving successors added). Returns
  {:post-event bool :spanning bool} so each window can be asserted — and,
  if a divergence surfaces, gated — independently."
  [hist applied]
  (let [succ (first (successor-pids applied))
        ;; guarantee hash_changed coverage when the successor survives
        ;; attached; "変" is outside the generator name pool, and the
        ;; model's no-op totality makes the append harmless otherwise
        hist' (update hist :events conj
                      {:event/type :edit-person :pid succ
                       :field :family_name :value "変"})
        states (:states (model/fold-history hist'))
        ;; states has one entry per event (initial first): state after the
        ;; forced event = its event index + 1. The forced event map holds
        ;; freshly minted ids, so value-equality indexOf is unambiguous.
        after-idx (inc (.indexOf ^java.util.List (:events hist')
                                 (:event applied)))
        s-before (nth states (dec after-idx))
        s-after (nth states after-idx)
        s-final (peek states)
        event (sidecar/event-for-intent applied)
        drift-dir (render/temp-dir "sim-drift")]
    (try
      (sidecar/write-sidecars! drift-dir event)
      (render/with-repo [git root work]
        (let [cs (render/commit-history! git root [s-before s-after s-final]
                                         (render/monotone-instants 3))
              window (fn [prev-c cur-c]
                       (audit/audit! {:aozora-repo (str root)
                                      :previous-ref (.getName prev-c)
                                      :current-ref (.getName cur-c)
                                      :work-dir (str work)
                                      :drift-persons-dir (str drift-dir)}))]
          {:post-event (window-matches? (window (nth cs 1) (nth cs 2))
                                        event s-after s-final)
           :spanning (window-matches? (window (nth cs 0) (nth cs 2))
                                      event s-before s-final)}))
      (finally (render/delete-tree! drift-dir)))))

;; P15.lifecycle — accepted event, then later participant edits (post-event
;; window) and the retrospective spanning window, for both event types.
(deftest p15-lifecycle-sim-test
  (doseq [forced [:clean-split :clean-merge]]
    (let [counter (harness/ratio-counter)]
      (harness/check!
       (str "P15.lifecycle/" (name forced)) 10
       (prop/for-all [hist (sgen/history-gen {:length [2 4] :works [3 5]
                                              :forced forced})]
                     (let [fold (model/fold-history hist)
                           applied (sgen/find-applied fold forced)]
                       (if-not (harness/tick! counter (some? applied))
                         true ;; shrunk-away forced event: vacuous
                         (let [{:keys [post-event spanning]}
                               (lifecycle-results hist applied)]
                           ;; per-window verdicts: on divergence, gate only
                           ;; the diverging one via div/expected-failure*
                           ;; (see the ns-form comment), leaving the other
                           ;; window hard
                           (and post-event spanning))))))
      (harness/assert-applied-ratio!
       (str "P15.lifecycle/" (name forced)) counter))))
```

- [ ] **Step 2: Run the test**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.drift-sidecar-sim-test`
Expected: PASS (1 test; 2 variants × 3 seeds × 10 runs; roughly 1–3 min — each run commits 3 states and audits 2 windows).

If it FAILS on a window comparison (not on a harness defect like an exception from sidecar authoring): identify WHICH window diverged by rerunning the shrunk counterexample and inspecting `lifecycle-results`, then follow the Global Constraints divergence protocol — new `:D7` entry naming the window (e.g. case `"P15.lifecycle/post-event"`), and in the property replace only that window's bare boolean with `(div/expected-failure* :D7 "P15.lifecycle/<window>" (fn [] <that-boolean>))`, leaving the other window's verdict hard so an open divergence in one window cannot mask a regression in the other.

- [ ] **Step 3: Commit**

```bash
git add abc/test/abc/sim/drift_sidecar_sim_test.clj
git commit -m "test(sim): P15.lifecycle — drift-sidecar interplay over generated histories"
```

---

### Task 5: P15.localization, P15.invalid-sidecar, P15.rerun

**Files:**
- Modify: `abc/test/abc/sim/drift_sidecar_sim_test.clj` (append the three deftests)

**Interfaces:**
- Consumes: everything Task 4 defined in the same file, plus `sidecar/corrupt!` (Task 1), `harness/clean-ex-info?`, `harness/forbidden-throw?` (Task 3), `oracle/semantic-report`, `audit/scan-history!` with `{:aozora-repo :from-ref :work-dir :drift-persons-dir}`.
- Produces: nothing further.

- [ ] **Step 1: Append the three tests**

Append to `abc/test/abc/sim/drift_sidecar_sim_test.clj`:

```clojure
(def ^:private synthetic-intent
  "Split whose pre- participant is bootstrap author 000001 and whose post-
  participants are never-ingested fresh pids: only 000001 can ever produce
  an entry; 9999xx pids are absent from every projection."
  {:intent :clean-split
   :event {:pid "000001" :targets ["999901" "999902"]}})

(defmacro ^:private with-sidecar
  "Author + write the synthetic sidecar; bind [event-sym dir-sym]."
  [[event-sym dir-sym] & body]
  `(let [~event-sym (sidecar/event-for-intent synthetic-intent)
         ~dir-sym (render/temp-dir "sim-drift")]
     (try
       (sidecar/write-sidecars! ~dir-sym ~event-sym)
       ~@body
       (finally (render/delete-tree! ~dir-sym)))))

;; P15.localization — the update appears only in the pair whose window
;; spans the participant edit; summary equals the per-pair sum.
(deftest p15-localization-sim-test
  (let [m0 (model/bootstrap 2)
        edit (fn [m pid v]
               (:model (model/apply-event m {:event/type :edit-person
                                             :pid pid :field :family_name
                                             :value v})))
        s1 (edit m0 "000002" "改") ;; pair 1: non-participant edit only
        s2 (edit s1 "000001" "変")] ;; pair 2: participant edit
    (with-sidecar [event drift-dir]
      (render/with-repo [git root work]
        (let [cs (render/commit-history! git root [m0 s1 s2]
                                         (render/monotone-instants 3))
              s (audit/scan-history! {:aozora-repo (str root)
                                      :from-ref (.getName (first cs))
                                      :work-dir (str work)
                                      :drift-persons-dir (str drift-dir)})
              pairs (:pairs s)]
          (is (= "ok" (:status s)))
          (is (= [0 1] (mapv :drift_participant_update_count pairs)))
          (is (= [{"person_id" "000001" "change_type" "hash_changed"}]
                 (mapv #(select-keys % ["person_id" "change_type"])
                       (:drift_participant_updates (second pairs)))))
          (is (every? #(entry-shape-ok? event %)
                      (:drift_participant_updates (second pairs))))
          (is (= 1 (get (:summary s) "drift_participant_updates"))))))))

;; P15.invalid-sidecar — all four faults pin the shared drift-index-map
;; validation boundary through audit!; :schema-hash-mismatch additionally
;; pins scan-history! propagation (per-pair re-validation must be equally
;; loud, not absorbed into a pair entry).
(deftest p15-invalid-sidecar-sim-test
  (let [m0 (model/bootstrap 2)
        m1 (:model (model/apply-event m0 {:event/type :edit-person
                                          :pid "000001"
                                          :field :family_name :value "改"}))]
    (doseq [fault [:schema-hash-mismatch :orphan-event-file
                   :index-target-missing :participants-not-sorted]]
      (with-sidecar [event drift-dir]
        (sidecar/corrupt! drift-dir event fault)
        (render/with-repo [git root work]
          (let [cs (render/commit-history! git root [m0 m1]
                                           (render/monotone-instants 2))
                thrown (try (audit/audit! {:aozora-repo (str root)
                                           :previous-ref (.getName (first cs))
                                           :current-ref (.getName (second cs))
                                           :work-dir (str work)
                                           :drift-persons-dir (str drift-dir)})
                            nil
                            (catch Throwable e e))]
            (is (some? thrown)
                (str fault ": invalid sidecars must not yield a report"))
            (is (not (harness/forbidden-throw? thrown)) (str fault))
            (is (harness/clean-ex-info? thrown [:persons-dir :failures])
                (str fault ": " (pr-str thrown)))))))
    (with-sidecar [event drift-dir]
      (sidecar/corrupt! drift-dir event :schema-hash-mismatch)
      (render/with-repo [git root work]
        (let [cs (render/commit-history! git root [m0 m1]
                                         (render/monotone-instants 2))
              thrown (try (audit/scan-history!
                           {:aozora-repo (str root)
                            :from-ref (.getName (first cs))
                            :work-dir (str work)
                            :drift-persons-dir (str drift-dir)})
                          nil
                          (catch Throwable e e))]
          (is (some? thrown) "scan-history! must propagate, not absorb")
          (is (not (harness/forbidden-throw? thrown)))
          (is (harness/clean-ex-info? thrown [:persons-dir :failures])))))))

;; P15.rerun — P14's work-dir hygiene contract extended to the sidecar
;; path: drift_participant_updates carry no locator fields, so
;; semantic-report retains them and equality is meaningful.
(deftest p15-rerun-sim-test
  (let [m0 (model/bootstrap 2)
        m1 (:model (model/apply-event m0 {:event/type :edit-person
                                          :pid "000001"
                                          :field :family_name :value "改"}))]
    (with-sidecar [event drift-dir]
      (render/with-repo [git root work]
        (let [cs (render/commit-history! git root [m0 m1]
                                         (render/monotone-instants 2))
              run! (fn [w]
                     (oracle/semantic-report
                      (audit/audit! {:aozora-repo (str root)
                                     :previous-ref (.getName (first cs))
                                     :current-ref (.getName (second cs))
                                     :work-dir (str w)
                                     :drift-persons-dir (str drift-dir)})))
              first-run (run! work)
              reused (run! work)
              fresh-dir (render/temp-dir "sim-fresh-work")
              fresh (try (run! fresh-dir)
                         (finally (render/delete-tree! fresh-dir)))]
          (is (= [{"person_id" "000001" "change_type" "hash_changed"}]
                 (mapv #(select-keys % ["person_id" "change_type"])
                       (:drift_participant_updates first-run)))
              "sidecar entry present in the semantic report")
          (is (= first-run reused fresh)))))))
```

- [ ] **Step 2: Run the namespace**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.drift-sidecar-sim-test`
Expected: PASS (4 tests). Prediction basis: the scan loop has no try/catch around `pair-report`, so the invalid-sidecar scan case should propagate; if any assertion instead reveals absorbing/misshaped behavior, apply the divergence protocol (D7+; forbidden-class assertions stay hard).

- [ ] **Step 3: Run both full suites**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation`
Expected: PASS (33 tests: 29 existing + 4 new).

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :unit`
Expected: PASS (685 tests: 682 existing + 2 new `abc.sim.sidecar-test` deftests + 1 deftest appended to the pre-existing `abc.sim.oracle-test`).

- [ ] **Step 4: Commit**

```bash
git add abc/test/abc/sim/drift_sidecar_sim_test.clj
git commit -m "test(sim): P15 localization, invalid-sidecar, rerun properties"
```
