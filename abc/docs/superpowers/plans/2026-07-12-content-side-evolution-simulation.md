# Content-Side Evolution Simulation Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** P16 simulation coverage for the content pipeline's cross-snapshot reuse semantics (`soranoha-build-publication` skip/reuse/rebuild, hash pins, integrity faults) over generated evolving content trees, per `abc/docs/superpowers/specs/2026-07-12-content-side-evolution-simulation-design.md`.

**Architecture:** The existing sim model gains a `:contents` axis (content is the subject, so `model.clj`/`gen.clj` are touched additively). States render to plain aozora-root directories; `build-publication!` runs as a function with a realistic parser-IR stub (member-bytes hash — the real adapter contract). Oracles predict selection, per-slug statuses, and pins from the model. The pin-chain composition failure is pinned as open divergence D7.

**Tech Stack:** Clojure, test.check, kaocha (`:simulation` suite via `-sim-test$` ns pattern), existing `abc.sim.*` harness.

## Global Constraints

- No file under `abc/src/` is modified. Changes only under `abc/test/abc/sim/` (plus this plan/spec under `abc/docs/`).
- Changes to `model.clj`, `gen.clj`, `render.clj`, `oracle.clj`, `divergences.clj` are **additive** — existing functions may only change where a step shows the exact edit (headers vector, `model->rows`, `bootstrap`, `:remove-work`, `check-invariants!`, `benign-event-gen`/`gen-events`/`history-gen` cap plumbing). Never delete or rewrite existing tests.
- `:edit-content` is generated ONLY by `content-history-gen`'s seeding — it must never join `benign-event-gen`.
- The stub's `parser-ir.json` `["source"]["work_content_hash"]` is `(hash/format-sha256 (hash/sha256-bytes source-bytes))` — the member-bytes hash. Never the raw-ZIP hash.
- Content zip name is derived: work `wid` → zip `<wid>_t.zip`, member `<wid>.txt`. Content cap: `:content-cap` option, default 4.
- D7 stays `:open`; P16.3 gates ONLY composition success via `div/expected-failure*`; the throw's cleanliness and both hash values are asserted hard.
- Forbidden throw classes (assert un-gated): NullPointerException, AssertionError, StackOverflowError, raw `java.util.zip.ZipException` (`harness/forbidden-throw?`).
- All test commands run from `abc/`: `clojure -M:test:kaocha -m kaocha.runner --focus <target>`. CI seeds `[42 4242 424242]` are applied by `harness/check!` automatically.
- Non-vacuity: `harness/ratio-counter` + `harness/assert-applied-ratio!` (≥ 9/10) on P16.1 and P16.2; P16.2 additionally must keep ≥ 10 genuine-evolution applied cases across CI seeds (runs-per-seed ≥ 4).
- Model invariant: every `:contents` key is a `:works` key; `:text` is a non-blank string containing the work's wid.

---

### Task 1: Content axis in model and generators

**Files:**
- Modify: `abc/test/abc/sim/model.clj`
- Modify: `abc/test/abc/sim/gen.clj`
- Create: `abc/test/abc/sim/content_test.clj`

**Interfaces:**
- Consumes: existing `model/bootstrap`, `model/apply-event`, `model/fold-history`, `gen/history-gen`, `gen/find-applied`.
- Produces: model events `:add-content {:wid :text}` / `:edit-content {:wid :text}` / `:remove-content {:wid}`; state key `:contents` (sorted-map `wid → {:text s}`); `sgen/content-history-gen [opts]` (defaults `:works [2 6] :length [4 10]`; first two events are `:add-content` on the two lowest bootstrap works, exactly one `:edit-content` on the first of them spliced at a generated position); `history-gen` accepts `:content-cap` (default 4).

- [ ] **Step 1: Write the failing tests**

Create `abc/test/abc/sim/content_test.clj`:

```clojure
(ns abc.sim.content-test
  "Unit tests for the content axis (model events, generators, rendering,
  selection/status oracles) added by the content-side evolution spec."
  (:require [abc.sim.gen :as sgen]
            [abc.sim.model :as model]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]))

(deftest content-events-test
  (let [m0 (model/bootstrap 2)]
    (testing "bootstrap has empty contents"
      (is (= {} (:contents m0))))
    (testing "add-content applies once to an existing work"
      (let [{m1 :model a1 :applied}
            (model/apply-event m0 {:event/type :add-content :wid "000101"
                                   :text "作品000101 本文"})]
        (is (= :add-content (:intent a1)))
        (is (= "作品000101 本文" (get-in m1 [:contents "000101" :text])))
        (is (nil? (:applied (model/apply-event
                             m1 {:event/type :add-content :wid "000101"
                                 :text "作品000101 別文"}))))
        (testing "edit-content changes text; identical text no-ops"
          (let [{m2 :model a2 :applied}
                (model/apply-event m1 {:event/type :edit-content :wid "000101"
                                       :text "作品000101 改"})]
            (is (= :edit-content (:intent a2)))
            (is (= "作品000101 改" (get-in m2 [:contents "000101" :text])))
            (is (nil? (:applied (model/apply-event
                                 m2 {:event/type :edit-content :wid "000101"
                                     :text "作品000101 改"}))))))
        (testing "remove-content applies once"
          (let [{m3 :model a3 :applied}
                (model/apply-event m1 {:event/type :remove-content :wid "000101"})]
            (is (= :remove-content (:intent a3)))
            (is (not (contains? (:contents m3) "000101")))
            (is (nil? (:applied (model/apply-event
                                 m3 {:event/type :remove-content :wid "000101"}))))))
        (testing "remove-work drops the work's content"
          (let [{m4 :model} (model/apply-event
                             m1 {:event/type :remove-work :wid "000101"})]
            (is (not (contains? (:contents m4) "000101")))))))
    (testing "no-ops: missing work, blank text, text without wid, edit without content"
      (is (nil? (:applied (model/apply-event
                           m0 {:event/type :add-content :wid "999999"
                               :text "作品999999 本文"}))))
      (is (nil? (:applied (model/apply-event
                           m0 {:event/type :add-content :wid "000101" :text "  "}))))
      (is (nil? (:applied (model/apply-event
                           m0 {:event/type :add-content :wid "000101"
                               :text "本文のみ"}))))
      (is (nil? (:applied (model/apply-event
                           m0 {:event/type :edit-content :wid "000101"
                               :text "作品000101 改"})))))))

(deftest content-history-gen-shape-test
  (testing "structure: two leading adds on the two lowest bootstrap works, one edit"
    (let [{:keys [initial events]} (gen/generate (sgen/content-history-gen {}) 30 42)
          [e1 e2] events
          [w1 w2] (vec (take 2 (keys (:works initial))))
          edits (filterv #(= :edit-content (:event/type %)) events)]
      (is (= :add-content (:event/type e1)))
      (is (= :add-content (:event/type e2)))
      (is (= [w1 w2] [(:wid e1) (:wid e2)]))
      (is (= 1 (count edits)))
      (is (= w1 (:wid (first edits))))
      (is (not= (:text e1) (:text (first edits))))))
  (testing "with no benign events the seeded edit always applies"
    (let [hist (gen/generate (sgen/content-history-gen {:length [0 0] :works [2 2]}) 30 7)
          fold (model/fold-history hist)]
      (is (= 3 (count (:events hist))))
      (is (some? (sgen/find-applied fold :edit-content))))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.content-test`
Expected: FAIL (`:contents` nil in bootstrap; `:add-content` hits `:default` no-op so `(:intent a1)` is nil; `content-history-gen` unresolved).

- [ ] **Step 3: Implement the model changes**

In `abc/test/abc/sim/model.clj`:

3a. In `bootstrap`, add a `:contents` entry to the returned map (after `:edges`):

```clojure
   :contents (sorted-map)
```

3b. In `check-invariants!`, after the existing `doseq` over `:edges` and before the final `nil`, add:

```clojure
  (doseq [[wid {:keys [text]}] (:contents m)]
    (when (or (not (contains? (:works m) wid))
              (not (string? text))
              (string/blank? text)
              (not (string/includes? text wid)))
      (throw (ex-info "model invariant violated"
                      {:content wid :text text :event event}))))
```

3c. In `apply-event* :remove-work`, extend the threading so content is dropped with the work — replace the `(applied (-> m ...))` form with:

```clojure
      (applied (-> m
                   (update :works dissoc wid)
                   (update :contents dissoc wid)
                   (update :edges #(apply dissoc % edge-keys)))
               e edge-keys)
```

3d. Add three defmethods after `:remove-work` (before the drift events), with a short comment block:

```clojure
;; --- content events (content-side evolution spec) ----------------------
;; :contents is sorted-map wid → {:text s}. Text must embed the wid
;; (uniqueness of member bytes across works — the invariant enforces it).

(defmethod apply-event* :add-content
  [m {:keys [wid text] :as e}]
  (if (or (not (contains? (:works m) wid))
          (contains? (:contents m) wid)
          (not (string? text)) (string/blank? text)
          (not (string/includes? text wid)))
    (no-op m)
    (applied (assoc-in m [:contents wid] {:text text}) e [])))

(defmethod apply-event* :edit-content
  [m {:keys [wid text] :as e}]
  (if (or (not (contains? (:contents m) wid))
          (not (string? text)) (string/blank? text)
          (not (string/includes? text wid))
          (= text (get-in m [:contents wid :text])))
    (no-op m)
    (applied (assoc-in m [:contents wid :text] text) e [])))

(defmethod apply-event* :remove-content
  [m {:keys [wid] :as e}]
  (if-not (contains? (:contents m) wid)
    (no-op m)
    (applied (update m :contents dissoc wid) e [])))
```

- [ ] **Step 4: Implement the generator changes**

In `abc/test/abc/sim/gen.clj`:

4a. Add after `variant-person`:

```clojure
(def ^:private text-pool ["春" "夏" "秋" "冬" "花" "鳥" "風" "月" "雪" "星"])

(defn- gen-text [wid]
  (gen/fmap (fn [s] (str "作品" wid " 本文 " s)) (gen/elements text-pool)))
```

4b. Add after `gen-rare-unattached`:

```clojure
(defn- gen-add-content
  "Content only below the cap: real materialize-publication! runs per
  content work, so generated volume is bounded here, not in the model."
  [m cap]
  (let [cands (vec (remove #(contains? (:contents m) %) (keys (:works m))))]
    (if (or (empty? cands) (>= (count (:contents m)) cap))
      (gen-edit-work m)
      (gen/let [wid (gen/elements cands)
                text (gen-text wid)]
        {:event/type :add-content :wid wid :text text}))))

(defn- gen-remove-content [m]
  (let [cands (vec (keys (:contents m)))]
    (if (empty? cands)
      (gen-edit-work m)
      (gen/let [wid (gen/elements cands)]
        {:event/type :remove-content :wid wid}))))
```

4c. Change `benign-event-gen` to take the cap and include the two new events (NOT `:edit-content` — seeded-only, see Global Constraints):

```clojure
(defn- benign-event-gen
  "Weighted benign event against state m. Removal weights are low, which
  together with the fresh-id discipline keeps the confusable?-discard rate
  low (predicate in properties is normative). :edit-content is deliberately
  absent — it is seeded only by content-history-gen so find-applied can
  locate it uniquely."
  [m cap]
  (if (empty? (:works m))
    (gen-add-work-with-edge m)
    (gen/frequency
     [[4 (gen-edit-person m)]
      [3 (gen-edit-work m)]
      [3 (gen-add-work-with-edge m)]
      [3 (gen-add-person-with-edge m)]
      [3 (gen-add-edge m)]
      [2 (gen-add-content m cap)]
      [1 (gen-remove-content m)]
      [1 (gen-remove-edge m)]
      [1 (gen-remove-work m)]
      [1 (gen-rare-unattached m)]])))
```

4d. Thread the cap through `gen-events` and `history-gen`:

```clojure
(defn- gen-events
  "Chain n benign events against the evolving state; when i = forced-at,
  splice in [setup... forced...] instead."
  [m n i forced-at forced cap]
  (if (zero? n)
    (gen/return [])
    (gen/bind (if (= i forced-at)
                (gen-forced m forced)
                (gen/fmap (fn [e] [[] e])
                          (benign-event-gen m cap)))
              (fn [[setup e]]
                (let [es (conj (vec setup) e)
                      m' (peek (:states (model/fold-history {:initial m :events es})))]
                  (gen/fmap #(into es %)
                            (gen-events m' (dec n) (inc i) forced-at forced cap)))))))

(defn history-gen
  [{:keys [length works forced content-cap]
    :or {length [5 15] works [5 20] content-cap 4}}]
  (gen/let [n-works (gen/choose (first works) (second works))
            n-events (gen/choose (first length) (second length))
            forced-at (if forced (gen/choose 0 (dec n-events)) (gen/return -1))]
    (let [m0 (model/bootstrap n-works)]
      (gen/fmap (fn [events] {:initial m0 :events events})
                (gen-events m0 n-events 0 forced-at forced content-cap)))))
```

4e. Add `content-history-gen` after `benign-history-gen`:

```clojure
(defn content-history-gen
  "history-gen plus the seeded content shape for the P16 properties: two
  :add-content events on the two lowest bootstrap works up front, and one
  :edit-content on the FIRST of them spliced at a generated position — so
  when the edit applies, the second work's unchanged content is positioned
  to be reused while the edited work rebuilds. :edit-content is generated
  ONLY here (never in the benign mix), so (find-applied fold :edit-content)
  locates the seeded edit uniquely. Splicing post-generation is safe:
  content events touch only :contents, and no apply-event consults
  :next-id, so later benign events keep their applicability.
  Requires ≥ 2 bootstrap works; defaults keep histories small because the
  properties run real builds."
  [opts]
  (let [opts (merge {:works [2 6] :length [4 10]} opts)]
    (gen/bind
     (history-gen (assoc opts :forced nil))
     (fn [{:keys [initial events]}]
       (let [[w1 w2] (vec (take 2 (keys (:works initial))))]
         (gen/let [t1 (gen-text w1)
                   t2 (gen-text w2)
                   t1' (gen-text w1)
                   pos (gen/choose 0 (count events))]
           (let [edited (if (= t1' t1) (str t1' "改") t1')]
             {:initial initial
              :events (vec (concat
                            [{:event/type :add-content :wid w1 :text t1}
                             {:event/type :add-content :wid w2 :text t2}]
                            (take pos events)
                            [{:event/type :edit-content :wid w1 :text edited}]
                            (drop pos events)))})))))))
```

- [ ] **Step 5: Run the new tests**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.content-test`
Expected: PASS.

- [ ] **Step 6: Run the simulation and unit suites**

The benign-mix change shifts every seeded generated history; all existing properties are oracle-based and must stay green. Run:
`clojure -M:test:kaocha -m kaocha.runner --focus :simulation`
Expected: PASS. Any failure here is an implementation bug in this task (most likely the invariant or a partial cap-plumbing edit) — do NOT adjust seeds or existing tests.

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.oracle-test --focus abc.sim.sidecar-test`
Expected: PASS.

- [ ] **Step 7: Commit**

```bash
git add abc/test/abc/sim/model.clj abc/test/abc/sim/gen.clj abc/test/abc/sim/content_test.clj
git commit -m "feat(sim): content axis in model and generators"
```

---

### Task 2: Content rendering and card-pid

**Files:**
- Modify: `abc/test/abc/sim/render.clj`
- Modify: `abc/test/abc/sim/oracle.clj` (only `card-pid` here)
- Modify: `abc/test/abc/sim/content_test.clj` (append)

**Interfaces:**
- Consumes: Task 1's `:contents`; existing `render/headers`, `render/model->rows`, `render/csv->zip-bytes`, `render/zip-path`, `render/temp-dir`, `render/delete-tree!`; `abc.tools.hash` (`sha256-bytes`, `format-sha256`); `abc.tools.files/sha256-file`.
- Produces: `render/content-zip-name [wid]` → `"<wid>_t.zip"`; `render/text->zip-bytes [text wid]` → deterministic bytes; `render/content-sources [m]` → sorted-map `wid → {:card-pid :basename :relpath :source-hash}`; `render/write-aozora-root! [dir m]`; `oracle/card-pid [proj wid]` → smallest pid over the work's edges; `headers` now 44 columns incl. テキストファイルURL.

- [ ] **Step 1: Write the failing tests** (append to `content_test.clj`; extend the ns `:require` with `[abc.sim.oracle :as oracle] [abc.sim.render :as render] [abc.tools.files :as files] [abc.tools.hash :as hash] [clojure.java.io :as io]`)

```clojure
(defn- two-work-content-state []
  (-> (model/bootstrap 2)
      (assoc-in [:contents "000101"] {:text "作品000101 本文 春"})))

(deftest card-pid-test
  (let [m (-> (model/bootstrap 2)
              (assoc-in [:edges ["000101" "翻訳者"]] #{"000002"}))
        proj (oracle/projection m)]
    (is (= "000001" (oracle/card-pid proj "000101")))
    (is (= "000002" (oracle/card-pid proj "000102")))))

(deftest content-render-test
  (let [m (two-work-content-state)
        rows (render/model->rows m)
        row101 (first (filter #(= "000101" (get % "作品ID")) rows))
        row102 (first (filter #(= "000102" (get % "作品ID")) rows))]
    (is (some #(= "テキストファイルURL" %) render/headers))
    (is (= "https://www.aozora.gr.jp/cards/000001/files/000101_t.zip"
           (get row101 "テキストファイルURL")))
    (is (= "" (get row102 "テキストファイルURL")))))

(deftest text->zip-bytes-deterministic-test
  (let [a (render/text->zip-bytes "作品000101 本文" "000101")
        b (render/text->zip-bytes "作品000101 本文" "000101")
        c (render/text->zip-bytes "作品000101 改" "000101")]
    (is (java.util.Arrays/equals ^bytes a ^bytes b))
    (is (not (java.util.Arrays/equals ^bytes a ^bytes c)))))

(deftest write-aozora-root-test
  (let [dir (render/temp-dir "sim-aroot")]
    (try
      (let [m (two-work-content-state)]
        (render/write-aozora-root! dir m)
        (is (.isFile (io/file dir render/zip-path)))
        (is (.isFile (io/file dir "cards/000001/files/000101_t.zip")))
        (is (.isFile (io/file dir "cards/999999/files/decoy.zip")))
        (is (.isFile (io/file dir "support/tools.zip")))
        (is (= "sim-fixture-head\n" (slurp (io/file dir ".git/HEAD"))))
        (let [srcs (render/content-sources m)]
          (is (= ["000101"] (vec (keys srcs))))
          (is (= "cards/000001/files/000101_t.zip"
                 (get-in srcs ["000101" :relpath])))
          ;; the oracle pin equals the sha256 of the file actually written
          (is (= (hash/format-sha256
                  (files/sha256-file (io/file dir "cards/000001/files/000101_t.zip")))
                 (get-in srcs ["000101" :source-hash])))))
      (finally (render/delete-tree! dir)))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.content-test`
Expected: FAIL (`card-pid`, `content-sources`, `write-aozora-root!`, `text->zip-bytes` unresolved; テキストファイルURL absent from headers).

- [ ] **Step 3: Implement `oracle/card-pid`** (append to `oracle.clj`)

```clojure
(defn card-pid
  "Person directory for a work's content zip: smallest pid over all of the
  work's edges in the projection. Deterministic; shared by render (URL and
  on-disk path) and the selection oracle. Intentionally NOT the slug's
  person_id, which follows catalog-index's last-row-wins rule."
  [proj wid]
  (->> (:edges proj)
       (keep (fn [[[ewid _rel] pids]] (when (= ewid wid) pids)))
       (reduce into (sorted-set))
       first))
```

- [ ] **Step 4: Implement the render changes**

In `abc/test/abc/sim/render.clj`:

4a. Add `"テキストファイルURL"` to the `headers` literal set (inside the `sort` vector — order is recomputed). Update the `model->rows` docstring `43` → `44`.

4b. Add `[abc.tools.hash :as hash]` to the ns `:require`.

4c. After `work-cells`, add:

```clojure
(defn content-zip-name [wid] (str wid "_t.zip"))

(defn- content-cells [m proj wid]
  (if (contains? (:contents m) wid)
    {"テキストファイルURL"
     (str "https://www.aozora.gr.jp/cards/" (oracle/card-pid proj wid)
          "/files/" (content-zip-name wid))}
    {}))
```

4d. In `model->rows`, bind the projection and merge the content cell:

```clojure
(defn model->rows
  "One row per work-contributor-role tuple of projection(model), all 44
  headers present (blank when inapplicable), sorted by [wid relation pid]."
  [m]
  (let [{:keys [persons works edges] :as proj} (oracle/projection m)]
    (vec (for [[[wid rel] pids] (sort edges)
               pid (sort pids)]
           (merge (zipmap headers (repeat ""))
                  (work-cells wid (get works wid))
                  (person-cells pid (get persons pid))
                  (content-cells m proj wid)
                  {"役割フラグ" rel})))))
```

4e. After `csv->zip-bytes`, add:

```clojure
(defn text->zip-bytes
  "Deterministic ZIP bytes for a work's text content: single member
  <wid>.txt, UTF-8 text bytes, fixed entry mtime (same discipline as
  csv->zip-bytes — byte-identical text ⇒ byte-identical zip, which is what
  makes the reuse oracle sound)."
  [text wid]
  (let [out (ByteArrayOutputStream.)]
    (with-open [zip (ZipOutputStream. out)]
      (.putNextEntry zip (doto (ZipEntry. (str wid ".txt")) (.setTime 0)))
      (.write zip (.getBytes ^String text StandardCharsets/UTF_8))
      (.closeEntry zip))
    (.toByteArray out)))

(defn content-sources
  "Per projected content-bearing work: card dir, basename, relpath, and the
  sha256 pin of the exact ZIP bytes write-aozora-root! writes. Single source
  of truth for paths and hashes on the oracle side."
  [m]
  (let [proj (oracle/projection m)]
    (into (sorted-map)
          (for [[wid {:keys [text]}] (:contents m)
                :when (contains? (:works proj) wid)]
            (let [cp (oracle/card-pid proj wid)]
              [wid {:card-pid cp
                    :basename (content-zip-name wid)
                    :relpath (str "cards/" cp "/files/" (content-zip-name wid))
                    :source-hash (hash/format-sha256
                                  (hash/sha256-bytes (text->zip-bytes text wid)))}])))))

(defn write-aozora-root!
  "Render a model state as a plain aozora-root: catalog ZIP, per-work
  content ZIPs (from content-sources, so paths/hashes agree with the
  oracle), two deterministic rejection decoys, and a fake .git/HEAD (git
  provenance is best-effort in the SUT)."
  [dir m]
  (let [write-bytes! (fn [relpath ^bytes bs]
                       (let [f (io/file dir relpath)]
                         (io/make-parents f)
                         (with-open [o (io/output-stream f)] (.write o bs))))]
    (write-bytes! zip-path (csv->zip-bytes (rows->csv (model->rows m))))
    (doseq [[wid {:keys [relpath]}] (content-sources m)]
      (write-bytes! relpath (text->zip-bytes (get-in m [:contents wid :text]) wid)))
    (write-bytes! "cards/999999/files/decoy.zip" (text->zip-bytes "decoy 999999" "999999"))
    (write-bytes! "support/tools.zip" (text->zip-bytes "tools 000000" "000000"))
    (let [head (io/file dir ".git/HEAD")]
      (io/make-parents head)
      (spit head "sim-fixture-head\n"))))
```

- [ ] **Step 5: Run the new tests**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.content-test`
Expected: PASS.

- [ ] **Step 6: Run the simulation suite** (the new CSV column flows through every existing property)

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation`
Expected: PASS.

- [ ] **Step 7: Commit**

```bash
git add abc/test/abc/sim/render.clj abc/test/abc/sim/oracle.clj abc/test/abc/sim/content_test.clj
git commit -m "feat(sim): content rendering — テキストファイルURL column, work zips, aozora-root writer"
```

---

### Task 3: Selection and status oracles

**Files:**
- Modify: `abc/test/abc/sim/oracle.clj` (append)
- Modify: `abc/test/abc/sim/content_test.clj` (append)

**Interfaces:**
- Consumes: Task 2's `render/model->rows` (44 cols) and `render/content-sources`.
- Produces: `oracle/expected-selection [rows sources]` → `{:selected [...] :rejected #{[path reason] ...}}` with `:selected` sorted by `:text_zip_relpath` (the SUT's `selected_sources` order); `oracle/expected-statuses [prev-selection cur-selection]` → sorted-map `slug → "reused"|"passed"`.

- [ ] **Step 1: Write the failing tests** (append to `content_test.clj`)

```clojure
(deftest expected-selection-test
  ;; person_id follows last-row-wins over the shared row projection; the
  ;; work's rows sort by [wid relation pid] and 著者 (U+8457) sorts after
  ;; 翻訳者 (U+7FFB), so the 著者 row wins here — pid 000002 — while
  ;; card-pid stays the edge-minimum 000001.
  (let [m (-> (model/bootstrap 2)
              (assoc-in [:contents "000101"] {:text "作品000101 本文 春"})
              (assoc-in [:edges ["000101" "著者"]] #{"000002"})
              (assoc-in [:edges ["000101" "翻訳者"]] #{"000001"}))
        sources (render/content-sources m)
        sel (oracle/expected-selection (render/model->rows m) sources)]
    (is (= [{:work_id "000101"
             :person_id "000002"
             :slug "000101_000002_000101_t"
             :text_zip_relpath "cards/000001/files/000101_t.zip"
             :source_hash (get-in sources ["000101" :source-hash])}]
           (:selected sel)))
    (is (= #{["cards/999999/files/decoy.zip" "not-catalog-text-zip"]
             ["support/tools.zip" "not-under-cards-files"]
             ["index_pages/list_person_all_extended_utf8.zip" "not-under-cards-files"]}
           (:rejected sel)))))

(deftest expected-selection-order-test
  ;; selected is sorted by relpath (card-pid dir), not by wid
  (let [m (-> (model/bootstrap 3)
              (assoc-in [:contents "000101"] {:text "作品000101 本文 春"})
              (assoc-in [:contents "000103"] {:text "作品000103 本文 冬"})
              ;; move 000101's only author to pid 000003 → card dir 000003
              (assoc-in [:edges ["000101" "著者"]] #{"000003"}))
        sel (oracle/expected-selection (render/model->rows m) (render/content-sources m))]
    (is (= ["cards/000003/files/000101_t.zip" "cards/000003/files/000103_t.zip"]
           (mapv :text_zip_relpath (:selected sel))))))

(deftest expected-statuses-test
  (let [m1 (-> (model/bootstrap 3)
               (assoc-in [:contents "000101"] {:text "作品000101 本文 春"})
               (assoc-in [:contents "000102"] {:text "作品000102 本文 秋"}))
        m2 (-> m1
               (assoc-in [:contents "000101" :text] "作品000101 本文 改")
               (assoc-in [:contents "000103"] {:text "作品000103 本文 冬"}))
        sel1 (oracle/expected-selection (render/model->rows m1) (render/content-sources m1))
        sel2 (oracle/expected-selection (render/model->rows m2) (render/content-sources m2))]
    (is (= {"000101_000001_000101_t" "passed"   ;; text changed
            "000102_000002_000102_t" "reused"   ;; untouched
            "000103_000003_000103_t" "passed"}  ;; new
           (oracle/expected-statuses sel1 sel2)))
    (testing "identical states are all reused; slug change forces passed"
      (is (every? #(= "reused" %) (vals (oracle/expected-statuses sel1 sel1))))
      (let [m2' (assoc-in m1 [:edges ["000102" "著者"]] #{"000003"})
            sel2' (oracle/expected-selection (render/model->rows m2')
                                             (render/content-sources m2'))]
        ;; same bytes, but 000102's winning row pid changed → new slug → passed
        (is (= "passed" (get (oracle/expected-statuses sel1 sel2')
                             "000102_000003_000102_t")))))))
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.content-test`
Expected: FAIL (`expected-selection`, `expected-statuses` unresolved).

- [ ] **Step 3: Implement the oracles** (append to `oracle.clj`; add `[clojure.string :as string]` to its ns `:require`)

```clojure
(defn- winning-rows
  "basename → row via catalog-index's documented last-row-wins reduction
  over the shared row projection (render/model->rows). The oracle consumes
  the SUT's actual input rows — it never reconstructs their ordering."
  [rows]
  (reduce (fn [acc row]
            (let [url (get row "テキストファイルURL")]
              (if (string/blank? url)
                acc
                (assoc acc (last (string/split url #"/")) row))))
          {}
          rows))

(defn expected-selection
  "Predicted build-publication source selection. rows = (render/model->rows m);
  sources = (render/content-sources m). :selected is sorted by
  :text_zip_relpath (the SUT sorts selected_sources by relpath). :rejected is
  the exact [path reason] set: the two decoys write-aozora-root! always
  plants plus the catalog ZIP itself."
  [rows sources]
  (let [wins (winning-rows rows)
        selected (for [[wid {:keys [basename relpath source-hash]}] sources
                       :let [row (get wins basename)]
                       :when row
                       :let [pid (get row "人物ID")]]
                   {:work_id wid
                    :person_id pid
                    :slug (str wid "_" pid "_" wid "_t")
                    :text_zip_relpath relpath
                    :source_hash source-hash})]
    {:selected (vec (sort-by :text_zip_relpath selected))
     :rejected #{["cards/999999/files/decoy.zip" "not-catalog-text-zip"]
                 ["support/tools.zip" "not-under-cards-files"]
                 ["index_pages/list_person_all_extended_utf8.zip" "not-under-cards-files"]}}))

(defn expected-statuses
  "slug → \"reused\"|\"passed\" over the current selection: reused iff the
  identical slug existed previously with the identical source hash
  (byte-identical zip). Slugs absent from the current selection are absent.
  \"skipped\" is never predicted — pub-dirs are built in a fresh temp root,
  so the skip branch is unreachable in normal runs; properties assert its
  count is 0."
  [prev-selection cur-selection]
  (let [prev (into {} (map (juxt :slug :source_hash)) (:selected prev-selection))]
    (into (sorted-map)
          (map (fn [{:keys [slug source_hash]}]
                 [slug (if (= source_hash (get prev slug)) "reused" "passed")]))
          (:selected cur-selection))))
```

- [ ] **Step 4: Run the tests**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.content-test`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add abc/test/abc/sim/oracle.clj abc/test/abc/sim/content_test.clj
git commit -m "feat(sim): content selection and status oracles"
```

---

### Task 4: Build harness, P16.4 faults, calibration benchmark

**Files:**
- Create: `abc/test/abc/sim/content_sim_test.clj`

**Interfaces:**
- Consumes: everything from Tasks 1–3; `abc.tools.soranoha-build-publication/build-publication!` + dynamic `*derive-parser-ir!*`; `abc.tools.json/{write-deterministic-json-file!,read-json-file}`; `abc.tools.manifest/schema-hash`; `abc.tools.files/example-hash`; `harness/{forbidden-throw?,clean-ex-info?}`.
- Produces (private, used by Tasks 5–6 in the same ns): `realistic-stub`, `write-config!`, `run-build!`, `statuses`, `marker`, `official-source`, `ex-chain`, `chain-clean-ex-info?`, `with-temp-dirs`, `synthetic-state`, `pub-files`.

- [ ] **Step 1: Create the namespace with helpers and the three fault tests**

Create `abc/test/abc/sim/content_sim_test.clj`:

```clojure
(ns abc.sim.content-sim-test
  "P16 content-side evolution properties (spec
  2026-07-12-content-side-evolution-simulation-design.md): cross-snapshot
  skip/reuse/rebuild of soranoha-build-publication over generated content
  trees, pin-chain composition (D7), and sampled integrity faults.

  The parser adapter is stubbed with the REAL hash contract: parser-IR
  source.work_content_hash = sha256 of the member bytes the adapter
  receives (ab-aozora-aat decode_source_bytes hashes stdin;
  ab-aat-to-parser-ir copies meta.source_hash). official-source.json
  source_hash is the raw-ZIP hash, so the two can never agree — the D7
  divergence gated in the pin-chain property."
  (:require [abc.sim.divergences :as div]
            [abc.sim.gen :as sgen]
            [abc.sim.harness :as harness]
            [abc.sim.model :as model]
            [abc.sim.oracle :as oracle]
            [abc.sim.render :as render]
            [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.json :as abc-json]
            [abc.tools.manifest :as manifest]
            [abc.tools.materialize-source-snapshot :as snapshot]
            [abc.tools.soranoha-build-publication :as build-publication]
            [abc.tools.source-snapshot-workset :as workset]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.properties :as prop])
  (:import [java.io ByteArrayOutputStream]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.util.zip ZipEntry ZipOutputStream]))

;; --- adapter stub -------------------------------------------------------

(defn- realistic-stub
  "Adapter-chain double with the real hash contract (member-bytes hash;
  see ns docstring and D7). Other fields mirror soranoha_test.clj's stub."
  [{:keys [source-bytes aat-file parser-ir-file divergence-file]}]
  (let [member-hash (hash/format-sha256 (hash/sha256-bytes source-bytes))]
    (abc-json/write-deterministic-json-file!
     aat-file
     {"version" 1 "work_id" "stub" "blocks" []
      "meta" {"adapter" "stub" "adapter_version" "test"
              "source_encoding" "utf-8" "source_hash" member-hash
              "parse_complete" true "warnings" []}})
    (abc-json/write-deterministic-json-file!
     parser-ir-file
     {"schema_hash" (manifest/schema-hash "schemas/parser-ir.schema.json")
      "source" {"work_content_hash" member-hash
                "encoding" "utf-8" "normalization" "source"}
      "derived_from" {"aat_adapter" "stub" "aat_adapter_version" "test-stub"
                      "aat_version" 1
                      "mapping_id" (str "https://w3id.org/abc/mappings/"
                                        "aat-v1-to-parser-ir-v1/generated-probe")
                      "mapping_schema_hash" (files/example-hash "38")
                      "mapping_version" "0.2.0"}
      "sentence_segmentation" {"schema_version" "sentence-segmentation-v1"
                               "splitter_id" "ab-plaintext-japanese-v1"
                               "coordinate_system" "decoded_utf8"
                               "coverage" "body-paragraphs"}
      "nodes" [] "warnings" [] "errors" []})
    (abc-json/write-deterministic-json-file! divergence-file {"stub" true})))

;; --- invocation helpers --------------------------------------------------

(defn- write-config!
  "Schema-valid build config; parser_profile is never resolved (stub bound)."
  [dir continue-on-failure?]
  (let [f (io/file dir "config.json")]
    (abc-json/write-deterministic-json-file!
     f
     {"config_schema_id" "https://w3id.org/abc/schemas/soranoha-publication-build-config.schema.json"
      "request_set_label" "sim-content"
      "snapshot_scope" "sim"
      "parser_profile" "aozora2html"
      "publication_profile" "tei-publication-basic-ja-v1"
      "continue_on_failure" continue-on-failure?
      "materialization_scope" "smoke"})
    (str f)))

(defn- run-build!
  "build-publication! with the realistic stub bound; returns the parsed
  selection + publications reports. stdout (three root-path lines) is
  swallowed into a throwaway writer."
  [{:keys [aozora-root out-root config-path snapshot-date replace?]}]
  (binding [build-publication/*derive-parser-ir!* realistic-stub
            *out* (java.io.StringWriter.)]
    (build-publication/build-publication!
     (cond-> ["--aozora-root" (str aozora-root)
              "--config" config-path
              "--output-root" (str out-root)
              "--snapshot-date" snapshot-date]
       replace? (conj "--replace"))))
  {:selection (abc-json/read-json-file
               (io/file out-root "source-selection-report.json"))
   :publications (abc-json/read-json-file
                  (io/file out-root "publications" "publications-report.json"))})

(defn- statuses [reports]
  (into (sorted-map)
        (map (juxt #(get % "slug") #(get % "status")))
        (get-in reports [:publications "publications"])))

(defn- marker [out-root slug]
  (string/trim (slurp (io/file out-root "publications" slug
                               "source_work_content_hash.txt"))))

(defn- official-source [out-root slug]
  (abc-json/read-json-file
   (io/file out-root "materialized-root" "works" slug "official-source.json")))

(defn- pub-files
  "name → bytes (as vec) of the flat files in a slug's publication dir."
  [out-root slug]
  (into (sorted-map)
        (keep (fn [^java.io.File f]
                (when (.isFile f)
                  [(.getName f) (vec (Files/readAllBytes (.toPath f)))])))
        (file-seq (io/file out-root "publications" slug))))

(defn- ex-chain [t]
  (take-while some? (iterate #(.getCause ^Throwable %) t)))

(defn- chain-clean-ex-info?
  "True when a clean ExceptionInfo carrying all required keys appears
  anywhere in the cause chain (the workflow layer may wrap step throws)."
  [t ks]
  (boolean (some #(harness/clean-ex-info? % ks) (ex-chain t))))

(defmacro ^:private with-temp-dirs
  "Fresh aozora-root and config temp dirs plus a NOT-yet-created out-root
  child (prepare-output-root! requires the first build's target to not
  exist); everything deleted on exit."
  [[aozora-sym out-sym cfg-sym] & body]
  `(let [~aozora-sym (render/temp-dir "sim-aozora")
         out-parent# (render/temp-dir "sim-out")
         ~out-sym (io/file out-parent# "out")
         ~cfg-sym (render/temp-dir "sim-cfg")]
     (try
       ~@body
       (finally
         (render/delete-tree! ~aozora-sym)
         (render/delete-tree! out-parent#)
         (render/delete-tree! ~cfg-sym)))))

;; --- P16.4 fixtures ------------------------------------------------------

(def ^:private text-a "作品000101 本文 春")
(def ^:private text-b "作品000102 本文 秋")
(def ^:private slug-a "000101_000001_000101_t")
(def ^:private slug-b "000102_000002_000102_t")

(defn- synthetic-state []
  (-> (model/bootstrap 2)
      (assoc-in [:contents "000101"] {:text text-a})
      (assoc-in [:contents "000102"] {:text text-b})))

(defn- overwrite-zip! [aozora-root m wid ^bytes zip-bytes]
  (let [rel (get-in (render/content-sources m) [wid :relpath])
        f (io/file aozora-root rel)]
    (with-open [o (io/output-stream f)] (.write o zip-bytes))))

(defn- no-text-zip-bytes
  "Structurally valid ZIP whose only member is a .png — exercises the
  'work ZIP contains no .txt member' guard without touching the
  ZipException→7zz fallback (environment-dependent, out of scope)."
  []
  (let [out (ByteArrayOutputStream.)]
    (with-open [zip (ZipOutputStream. out)]
      (.putNextEntry zip (doto (ZipEntry. "cover.png") (.setTime 0)))
      (.write zip (.getBytes "png" StandardCharsets/UTF_8))
      (.closeEntry zip))
    (.toByteArray out)))

;; --- P16.4 faults --------------------------------------------------------

;; F1: ZIP tampered after pin → rebuild, never stale-reuse; pins updated.
(deftest p16-4-tamper-rebuild-test
  (with-temp-dirs [aozora out cfg]
    (let [m (synthetic-state)
          config (write-config! cfg false)]
      (render/write-aozora-root! aozora m)
      (run-build! {:aozora-root aozora :out-root out :config-path config
                   :snapshot-date "2026-07-12"})
      (let [tampered (render/text->zip-bytes "作品000101 本文 改変" "000101")
            tampered-hash (hash/format-sha256 (hash/sha256-bytes tampered))]
        (overwrite-zip! aozora m "000101" tampered)
        (let [st (statuses (run-build! {:aozora-root aozora :out-root out
                                        :config-path config
                                        :snapshot-date "2026-07-13" :replace? true}))]
          (is (= "passed" (get st slug-a)) "tampered zip must rebuild")
          (is (= "reused" (get st slug-b)) "untouched zip must reuse")
          (is (= tampered-hash (marker out slug-a)))
          (is (= tampered-hash (get (official-source out slug-a) "source_hash"))))))))

;; F3: corrupt, then missing, prior reuse marker → fail-safe rebuild.
(deftest p16-4-prior-marker-fault-test
  (with-temp-dirs [aozora out cfg]
    (let [m (synthetic-state)
          config (write-config! cfg false)
          marker-file #(io/file out "publications" slug-a
                                "source_work_content_hash.txt")]
      (render/write-aozora-root! aozora m)
      (run-build! {:aozora-root aozora :out-root out :config-path config
                   :snapshot-date "2026-07-12"})
      (testing "corrupt marker → rebuild, never reuse"
        (spit (marker-file)
              "sha256:0000000000000000000000000000000000000000000000000000000000000000")
        (let [st (statuses (run-build! {:aozora-root aozora :out-root out
                                        :config-path config
                                        :snapshot-date "2026-07-13" :replace? true}))]
          (is (= "passed" (get st slug-a)))
          (is (= "reused" (get st slug-b)))
          (is (= (get-in (render/content-sources m) ["000101" :source-hash])
                 (marker out slug-a)) "rebuild restores the true pin")))
      (testing "missing marker → rebuild"
        (is (.delete (marker-file)))
        (let [st (statuses (run-build! {:aozora-root aozora :out-root out
                                        :config-path config
                                        :snapshot-date "2026-07-14" :replace? true}))]
          (is (= "passed" (get st slug-a))))))))

;; F4: work ZIP with no .txt member → loud clean throw (continue_on_failure
;; false), never a silent skip.
(deftest p16-4-no-text-member-test
  (with-temp-dirs [aozora out cfg]
    (let [m (synthetic-state)
          config (write-config! cfg false)]
      (render/write-aozora-root! aozora m)
      (overwrite-zip! aozora m "000101" (no-text-zip-bytes))
      (let [t (try (run-build! {:aozora-root aozora :out-root out
                                :config-path config :snapshot-date "2026-07-12"})
                   nil
                   (catch Throwable t t))]
        (is (some? t) "no-.txt zip must abort the build loudly")
        (is (not (harness/forbidden-throw? t)))
        (is (chain-clean-ex-info? t [:path]))
        (is (some #(string/includes? (str (ex-message %)) "no .txt member")
                  (ex-chain t)))))))
```

- [ ] **Step 2: Run the fault tests**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.content-sim-test`
Expected: PASS (these pin current behavior, which the spec's Key Facts predict is already correct; a failure here means a helper bug — fix before proceeding, do not weaken assertions).

- [ ] **Step 3: Add the DISPOSABLE calibration benchmark, run it, record numbers**

Append temporarily:

```clojure
;; DISPOSABLE calibration probe (spec §Runtime bounds) — removed after the
;; measurement is recorded in the task report.
(deftest zz-benchmark-representative-case-test
  (let [m4 (reduce (fn [m i]
                     (let [wid (format "%06d" (+ 101 i))]
                       (assoc-in m [:contents wid]
                                 {:text (str "作品" wid " 本文 " i)})))
                   (model/bootstrap 4)
                   (range 4))
        m4' (assoc-in m4 [:contents "000101" :text] "作品000101 本文 改")]
    (doseq [i (range 3)]
      (with-temp-dirs [aozora out cfg]
        (let [config (write-config! cfg false)
              t0 (System/nanoTime)]
          (render/write-aozora-root! aozora m4)
          (run-build! {:aozora-root aozora :out-root out :config-path config
                       :snapshot-date "2026-07-01"})
          (render/write-aozora-root! aozora m4')
          (run-build! {:aozora-root aozora :out-root out :config-path config
                       :snapshot-date "2026-07-02" :replace? true})
          (run-build! {:aozora-root aozora :out-root out :config-path config
                       :snapshot-date "2026-07-03" :replace? true})
          (println "P16.2-representative-case iter" i "ms:"
                   (long (/ (- (System/nanoTime) t0) 1000000.0))))))))
```

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.content-sim-test`
Record in the task report: the three per-iteration times (iteration 0 ≈ cold incl. classloading, 2 ≈ warm) and the total focused wall time.

- [ ] **Step 4: Remove the benchmark deftest** (delete the whole `zz-benchmark-representative-case-test` block including its comment), re-run the focused ns, expect the three fault tests PASS.

- [ ] **Step 5: Commit**

```bash
git add abc/test/abc/sim/content_sim_test.clj
git commit -m "test(sim): P16.4 content integrity faults + build harness"
```

---

### Task 5: P16.1 build property, P16.3 pin-chain, D7 entry

**Files:**
- Modify: `abc/test/abc/sim/divergences.clj` (append `:D7` to `table`)
- Modify: `abc/test/abc/sim/content_sim_test.clj` (append)

**Interfaces:**
- Consumes: Task 4 helpers; `oracle/expected-selection`; `workset/write-workset!` (`{:input-root :output-path :snapshot-scope :snapshot-date}`); `snapshot/materialize-source-snapshot!` (`{:workset-path :output-path}`); `div/expected-failure*` (`[id desc desired-fn]` → boolean).
- Produces: `p16-1-build-sim-test`, `p16-3-pin-chain-sim-test`; divergence `:D7 :open`.

- [ ] **Step 1: Add D7 to `divergences.clj`** (append inside `table` after `:D6`)

```clojure
   :D7 {:case "P16.3 pin-chain" :status :open
        :notes "2026-07-12: parser-IR source.work_content_hash is the member-bytes hash (ab-aozora-aat decode_source_bytes hashes adapter stdin; ab-aat-to-parser-ir copies meta.source_hash), but official-source.json source_hash is the raw-ZIP hash; materialize-source-snapshot!'s snapshot-input requires equality, so build-publication → workset → source-snapshot always throws. Desired: the two tools compose; which hash is canonical is ADR-level adjudication (spec §Future work)."}
```

- [ ] **Step 2: Append the two properties to `content_sim_test.clj`**

```clojure
;; --- P16.1 build ---------------------------------------------------------

(defn- build-checks
  "Boolean checks for one build against the oracle. All keys must be true."
  [out reports expected]
  (let [sel-actual (get-in reports [:selection "selected_sources"])
        st (statuses reports)]
    {:relpaths (= (mapv :text_zip_relpath (:selected expected))
                  (mapv #(get % "text_zip_relpath") sel-actual))
     :identities (= (mapv (juxt :work_id :person_id :slug) (:selected expected))
                    (mapv (juxt #(get % "work_id") #(get % "person_id")
                                #(get % "slug"))
                          sel-actual))
     :rejected (= (:rejected expected)
                  (set (map (juxt #(get % "path") #(get % "reason"))
                            (get-in reports [:selection "rejected_sources"]))))
     :pins (every? (fn [{:keys [slug source_hash]}]
                     (and (= source_hash (get (official-source out slug) "source_hash"))
                          (= source_hash
                             (get-in (abc-json/read-json-file
                                      (io/file out "materialized-root" "works" slug
                                               "source.manifest.json"))
                                     ["manifest_identity_object" "work_content_hash"]))
                          (= source_hash (marker out slug))))
                   (:selected expected))
     :statuses (and (every? #(= "passed" %) (vals st))
                    (= (count (:selected expected)) (count st))
                    (zero? (get-in reports [:publications "failed"]))
                    (zero? (get-in reports [:publications "skipped"])))}))

(deftest p16-1-build-sim-test
  (let [counter (harness/ratio-counter)]
    (harness/check!
     "P16.1 build" 10
     (prop/for-all [hist (sgen/content-history-gen {})]
       (let [m (peek (:states (model/fold-history hist)))
             expected (oracle/expected-selection (render/model->rows m)
                                                 (render/content-sources m))]
         (with-temp-dirs [aozora out cfg]
           (render/write-aozora-root! aozora m)
           (let [config (write-config! cfg false)
                 args {:aozora-root aozora :out-root out :config-path config
                       :snapshot-date "2026-07-12"}]
             (if (harness/tick! counter (seq (:selected expected)))
               (let [checks (build-checks out (run-build! args) expected)]
                 (when-not (every? val checks)
                   (println "P16.1 failing checks:"
                            (vec (keep (fn [[k v]] (when-not v k)) checks))))
                 (every? val checks))
               ;; empty selection: the SUT must refuse loudly
               (let [t (try (run-build! args) nil (catch Throwable t t))]
                 (and (some? t)
                      (not (harness/forbidden-throw? t))
                      (chain-clean-ex-info? t [:aozora_root])))))))))
    (harness/assert-applied-ratio! "P16.1 build" counter)))

;; --- P16.3 pin-chain (D7) -------------------------------------------------

(deftest p16-3-pin-chain-sim-test
  (harness/check!
   "P16.3 pin-chain" 5
   (prop/for-all [hist (sgen/content-history-gen {})]
     (let [m (peek (:states (model/fold-history hist)))
           expected (oracle/expected-selection (render/model->rows m)
                                               (render/content-sources m))]
       (if (empty? (:selected expected))
         true ;; vacuous run; generation non-vacuity is enforced by P16.1's ratio
         (with-temp-dirs [aozora out cfg]
           (render/write-aozora-root! aozora m)
           (run-build! {:aozora-root aozora :out-root out
                        :config-path (write-config! cfg false)
                        :snapshot-date "2026-07-12"})
           (let [ws (io/file cfg "workset.edn")
                 _ (workset/write-workset!
                    {:input-root (str (io/file out "materialized-root"))
                     :output-path (str ws)
                     :snapshot-scope "sim" :snapshot-date "2026-07-12"})
                 res (try {:ok (snapshot/materialize-source-snapshot!
                                {:workset-path (str ws)
                                 :output-path (str (io/file cfg "snapshot.json"))})}
                          (catch Throwable t {:thrown t}))
                 ;; workset works sort by [work_id slug]; snapshot-input
                 ;; throws on the first mismatch
                 first-sel (first (sort-by (juxt :work_id :slug)
                                           (:selected expected)))
                 wid (:work_id first-sel)
                 member-hash (hash/format-sha256
                              (hash/sha256-bytes
                               (.getBytes ^String (get-in m [:contents wid :text])
                                          StandardCharsets/UTF_8)))
                 hard-ok?
                 (if-let [t (:thrown res)]
                   (let [d (some #(let [dd (ex-data %)]
                                    (when (contains? dd :work-content-hash) dd))
                                 (ex-chain t))]
                     (and (not (harness/forbidden-throw? t))
                          (chain-clean-ex-info?
                           t [:work :parser-ir-path :official-source-path
                              :work-content-hash :official-source-hash])
                          ;; pin WHY it fails: member hash vs raw-ZIP hash
                          (= (:work d) (:slug first-sel))
                          (= (:work-content-hash d) member-hash)
                          (= (:official-source-hash d) (:source_hash first-sel))))
                   true)]
             (and hard-ok?
                  (div/expected-failure*
                   :D7
                   "P16.3: build-publication output composes with materialize-source-snapshot!"
                   (fn [] (contains? res :ok)))))))))))
```

- [ ] **Step 3: Run the focused namespace**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.content-sim-test`
Expected: PASS — P16.1 green against current behavior; P16.3 green because D7 is `:open` (the composition throw is the expected failure) with the hard throw-shape assertions passing. If P16.3 reports "divergence :D7 now passes", current behavior composes — stop and report; the D7 premise needs re-verification, do not flip the entry yourself.

- [ ] **Step 4: Run the simulation suite**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add abc/test/abc/sim/divergences.clj abc/test/abc/sim/content_sim_test.clj
git commit -m "test(sim): P16.1 build property; P16.3 pin-chain pinned as open divergence D7"
```

---

### Task 6: P16.2 evolution property, calibration, full suites

**Files:**
- Modify: `abc/test/abc/sim/content_sim_test.clj` (append)

**Interfaces:**
- Consumes: Task 4/5 helpers; `oracle/expected-statuses`; `sgen/find-applied`; fold `:states` indexing (`states[i]` is the state BEFORE `events[i]` applies).
- Produces: `p16-2-evolution-sim-test`; final calibrated `check!` counts.

- [ ] **Step 1: Append the evolution property**

```clojure
;; --- P16.2 evolution (the core) -------------------------------------------

(defn- evolution-checks
  "Runs the three build legs and returns boolean checks (all must be true).
  s-before/s-after per the spec: around the seeded edit."
  [s-before s-after exp-status]
  (with-temp-dirs [aozora out cfg]
    (let [aozora2 (render/temp-dir "sim-aozora2")]
      (try
        (let [config (write-config! cfg false)
              cur-hash (into {} (map (juxt :slug :source_hash))
                             (:selected (oracle/expected-selection
                                         (render/model->rows s-after)
                                         (render/content-sources s-after))))]
          (render/write-aozora-root! aozora s-before)
          (let [r1 (run-build! {:aozora-root aozora :out-root out
                                :config-path config :snapshot-date "2026-07-01"})
                st1 (statuses r1)
                prior (into {} (map (fn [s] [s (pub-files out s)])) (keys st1))]
            (render/write-aozora-root! aozora2 s-after)
            (let [r2 (run-build! {:aozora-root aozora2 :out-root out
                                  :config-path config :snapshot-date "2026-07-02"
                                  :replace? true})
                  st2 (statuses r2)
                  r3 (run-build! {:aozora-root aozora2 :out-root out
                                  :config-path config :snapshot-date "2026-07-03"
                                  :replace? true})
                  st3 (statuses r3)]
              {:leg1-all-passed (every? #(= "passed" %) (vals st1))
               :leg2-statuses (= exp-status st2)
               :leg2-counts (and (zero? (get-in r2 [:publications "failed"]))
                                 (zero? (get-in r2 [:publications "skipped"])))
               ;; no-stale-reuse invariant: every marker equals the CURRENT hash
               :markers (every? (fn [[slug h]] (= h (marker out slug))) cur-hash)
               ;; reused slugs' publication files are byte-identical to prior
               :reused-bytes (every? (fn [[slug status]]
                                       (or (not= "reused" status)
                                           (= (get prior slug) (pub-files out slug))))
                                     st2)
               ;; zero-change leg: everything reuses
               :leg3-all-reused (every? #(= "reused" %) (vals st3))})))
        (finally (render/delete-tree! aozora2))))))

(deftest p16-2-evolution-sim-test
  (let [counter (harness/ratio-counter)]
    (harness/check!
     "P16.2 evolution" 5
     (prop/for-all [hist (sgen/content-history-gen {})]
       (let [fold (model/fold-history hist)
             applied-edit (sgen/find-applied fold :edit-content)]
         (if (nil? applied-edit)
           (do (harness/tick! counter false) true) ;; seeded edit no-opped
           (let [i (.indexOf ^java.util.List (:events hist) (:event applied-edit))
                 s-before (nth (:states fold) i)
                 s-after (peek (:states fold))
                 sel-b (oracle/expected-selection (render/model->rows s-before)
                                                  (render/content-sources s-before))
                 sel-a (oracle/expected-selection (render/model->rows s-after)
                                                  (render/content-sources s-after))]
             (if (empty? (:selected sel-b))
               (do (harness/tick! counter false) true)
               (let [exp (oracle/expected-statuses sel-b sel-a)
                     statuses-set (set (vals exp))]
                 ;; applied = genuine evolution: at least one rebuild AND one
                 ;; reuse expected (all-reused runs still assert fully below)
                 (harness/tick! counter (and (contains? statuses-set "passed")
                                             (contains? statuses-set "reused")))
                 (let [checks (evolution-checks s-before s-after exp)]
                   (when-not (every? val checks)
                     (println "P16.2 failing checks:"
                              (vec (keep (fn [[k v]] (when-not v k)) checks))))
                   (every? val checks)))))))))
    (harness/assert-applied-ratio! "P16.2 evolution" counter)))
```

- [ ] **Step 2: Run the focused namespace and calibrate**

Run: `time clojure -M:test:kaocha -m kaocha.runner --focus abc.sim.content-sim-test`
Expected: PASS.

Calibration (spec §Runtime bounds): target ≤ ~90 s for the focused namespace. Using Task 4's recorded representative-case time T ms: P16.2 costs ≈ 3 seeds × 5 runs × T, P16.1 ≈ 3 × 10 × (T/3), plus faults. If over target, reduce in this order, re-running after each: (1) P16.1 `check!` count 10 → 6; (2) `content-history-gen` defaults are already small — do NOT shrink `:works` below `[2 4]`; (3) P16.2 count 5 → 4 at minimum (floor: 3 seeds × 4 runs × ~0.9 applied ≈ 11 ≥ 10 genuine-evolution cases — never go below 4). Record the final counts and wall time in the task report.

- [ ] **Step 3: Run the full suites**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation`
Expected: PASS (now includes all P16 tests).

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :unit`
Expected: PASS (existing tests unmodified; `abc.sim.content-test` joins the suite).

- [ ] **Step 4: Commit**

```bash
git add abc/test/abc/sim/content_sim_test.clj
git commit -m "test(sim): P16.2 evolution property over generated content histories"
```
