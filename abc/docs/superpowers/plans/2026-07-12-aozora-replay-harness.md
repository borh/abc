# Aozora Replay Harness Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Run the real pinned aozorabunko history through `scan-history!` (year-sampled) and pin the per-pair findings as a committed, diff-classified baseline.

**Architecture:** New tool ns `abc.tools.aozora-replay` composed of four units — `ensure-clone!` (managed blobless partial clone), `scan-plan` (public planning promoted into the audit ns), `prefetch-and-prevalidate!` (tiered blob availability + catalog sanity, producing pinned exclusions), and `replay!` (scan → digest → `--check`/`--update`). `scan-history!` gains an optional `:refs` override so excluded representatives cannot re-enter the scan. Spec: `abc/docs/superpowers/specs/2026-07-12-aozora-replay-harness-design.md`.

**Tech Stack:** Clojure (abc deps.edn), JGit via `abc.git`, CLI git via `clojure.java.shell` (clone/fetch/promisor-prefetch only), kaocha `:unit` suite, deterministic JSON via `abc.tools.json`.

## Global Constraints

- Production changes ONLY in: `abc/src/abc/tools/aozora_history_audit.clj` (scan-plan + `:refs`), new `abc/src/abc/tools/aozora_replay.clj`, `abc/deps.edn` (one alias), root `justfile` (two recipes).
- Failure tiers (spec §Failure handling, verbatim): environment tier is LOUD `ex-info` (clone/fetch/network, cache provenance mismatch, promised-but-unavailable local object after one promisor fetch attempt, unreadable baseline); source-fact tier is ABSORBED and PINNED (`excluded` entries with reason ∈ `missing-at-ref | unreadable-zip | no-csv-entry | no-data-rows`; per-work skips; `validation_failed`). Forbidden classes propagate as themselves: `NullPointerException`, `AssertionError`, `StackOverflowError`, raw `java.util.zip.ZipException`.
- Verdicts (spec §diff classification, verbatim semantics): `configuration-change` (header fields `baseline_format|zip_path|sample_period|remote_url` differ) / `pin-bump-shaped` (`pin_rev` changed; common pairs byte-identical except the final old pair may be replaced ONLY as same `previous_ref` + different `current_ref`; adds only appended; exclusions only for periods newer than the old final period) / `behavioral-change` (everything else, incl. ANY change while `pin_rev` unchanged).
- Authoritative lock: `abc/flake.lock` (`nodes.aozorabunko-src.locked.rev`); root `flake.lock` must agree.
- Baseline document: `baseline_format` 1; fields exactly as spec §Baseline document; deterministic JSON; no timestamps, no filesystem paths.
- Defaults: remote-url `https://github.com/aozorabunko/aozorabunko.git`; zip-path `index_pages/list_person_all_extended_utf8.zip`; cache dir `$XDG_CACHE_HOME/abc/aozorabunko` (fallback `~/.cache/abc/aozorabunko`); baseline `test/resources/aozora-replay-baseline.json`; work-dir `out/aozora-replay`; sample-period `year`.
- `--aozora-repo` disables ONLY `ensure-clone!`; prefetch/pre-validation always run.
- `--update` of the DEFAULT baseline path refuses non-default `--sample-period`/`--from-ref`/explicit `--to-ref` differing from the lock pin.
- Git shell-outs use argv vectors (never string interpolation); exactly: `clone --filter=blob:none --no-checkout`, `remote get-url origin`, `fetch`, `cat-file`.
- After every task, from `abc/`: `clojure -M:test:kaocha -m kaocha.runner --focus :unit` and `--focus :simulation` both green.
- Commits: conventional (`feat(aozora-replay): …`, `refactor(aozora-history-audit): …`, `test(aozora-replay): …`, `chore: …`).

---

### Task 1: `scan-plan` + `:refs` in the audit namespace

**Files:**
- Modify: `abc/src/abc/tools/aozora_history_audit.clj` (replace private `history-scan-refs`, extend `scan-history!`)
- Test: `abc/test/abc/tools/aozora_history_audit_test.clj`

**Interfaces:**
- Consumes: existing private `sample-commits-by-period`, `commit-period-key`, `abc-git/commits-touching-path`.
- Produces: public `scan-plan` `[repo {:keys [zip-path from-ref to-ref sample-period]}] → [{:ref <40-hex string> :period <string-or-nil>} …]` in log order, `from-ref` (when supplied) prepended with `:period nil`; `scan-history!` accepts optional `:refs` (vector of ref strings) that bypasses internal planning, everything else unchanged.

- [ ] **Step 1: Write the failing test**

Append to `abc/test/abc/tools/aozora_history_audit_test.clj` (helpers `row`, `csv-text`, `temp-dir`, `delete-recursive`, `sim-render` already exist in this file; add `[abc.git :as abc-git]` to the ns `:require`):

```clojure
(deftest scan-plan-and-refs-override-test
  (testing "scan-plan returns sampled {:ref :period} entries; scan-history! :refs reproduces internal planning"
    (let [repo-dir (temp-dir "abc-audit-scan-plan")
          work-dir (temp-dir "abc-audit-scan-plan-work")
          work-dir-2 (temp-dir "abc-audit-scan-plan-work2")
          git (sim-render/init-repo! repo-dir)
          state (fn [family] (sim-render/csv->zip-bytes
                              (csv-text [(row {"姓" family})])))
          c0 (sim-render/commit-zip-at! git repo-dir (state "壱") "s0" "2023-01-01T00:00:00Z")
          c1 (sim-render/commit-zip-at! git repo-dir (state "弐") "s1" "2023-06-01T00:00:00Z")
          c2 (sim-render/commit-zip-at! git repo-dir (state "参") "s2" "2024-06-01T00:00:00Z")]
      (try
        (let [repo (abc-git/load-git-repo (str repo-dir))
              plan (try (audit/scan-plan repo {:sample-period "year"})
                        (finally (.close repo)))]
          ;; year sampling: 2023 → c1 (last in log order), 2024 → c2
          (is (= [{:ref (.getName c1) :period "2023"}
                  {:ref (.getName c2) :period "2024"}]
                 plan))
          (let [internal (audit/scan-history! {:aozora-repo (str repo-dir)
                                               :from-ref (.getName c0)
                                               :sample-period "year"
                                               :work-dir (str work-dir)})
                explicit (audit/scan-history! {:aozora-repo (str repo-dir)
                                               :refs (into [(.getName c0)] (mapv :ref plan))
                                               :work-dir (str work-dir-2)})]
            (is (= (mapv (juxt :previous_ref :current_ref) (:pairs internal))
                   (mapv (juxt :previous_ref :current_ref) (:pairs explicit))))))
        (finally
          (.close git)
          (delete-recursive repo-dir)
          (delete-recursive work-dir)
          (delete-recursive work-dir-2))))))
```

- [ ] **Step 2: Run to verify it fails**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :unit`
Expected: FAIL — `scan-plan` unresolved var.

- [ ] **Step 3: Implement**

In `abc/src/abc/tools/aozora_history_audit.clj`, replace the private `history-scan-refs` fn with a public `scan-plan` placed right after `sample-commits-by-period`:

```clojure
(defn scan-plan
  "Public scan planning: the sampled representative commits for a history
  scan, as [{:ref <sha> :period <period-key-or-nil>} ...] in log order.
  `from-ref`, when supplied, is prepended with :period nil (it is a
  comparison base, not a sampled representative). Used by scan-history!
  internally and by abc.tools.aozora-replay to plan prefetch/pre-validation."
  [repo {:keys [zip-path from-ref to-ref sample-period]
         :or {zip-path default-zip-path}}]
  (let [opts (cond-> {}
               from-ref (assoc :from-ref from-ref)
               to-ref (assoc :to-ref to-ref))
        sampled (sample-commits-by-period
                 (abc-git/commits-touching-path repo zip-path opts)
                 sample-period)
        entries (mapv (fn [c] {:ref (.getName c)
                               :period (when sample-period
                                         (commit-period-key sample-period c))})
                      sampled)]
    (if from-ref
      (vec (cons {:ref from-ref :period nil} entries))
      entries)))
```

In `scan-history!`: add `refs` to the destructuring key list, and replace the line

```clojure
      (let [refs (history-scan-refs repo zip-path from-ref to-ref sample-period)
```

with

```clojure
      (let [refs (or refs
                     (mapv :ref (scan-plan repo {:zip-path zip-path
                                                 :from-ref from-ref
                                                 :to-ref to-ref
                                                 :sample-period sample-period})))
```

Also extend the `scan-history!` docstring with one sentence: "An explicit `:refs` vector (already-planned ref strings) bypasses internal planning; pairing, reporting, and work-dir semantics are unchanged."

Add per-pair progress logging (operationally required for the 1–2 h replay;
per-pair wall time is then derivable from the log timestamps, and the
baseline stays timing-free). Add `[taoensso.telemere :as tel]` to the ns
`:require`, and in the `scan-history!` loop insert as the first binding of
the `let` that computes `current-ingest`:

```clojure
                            (let [_ (tel/log! :info
                                              (str "history-scan pair "
                                                   (inc (count acc)) "/"
                                                   (count pairs-to-scan) " "
                                                   previous-ref ".." current-ref))
```

(i.e. the existing `(let [current-ingest (ingest-ref! …)` gains a leading
`_` binding with the log call; nothing else moves.)

- [ ] **Step 4: Run to verify green**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :unit` → PASS (including all pre-existing scan-history tests)
Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation` → PASS (P10–P12 pin the no-`:refs` path)

- [ ] **Step 5: Commit**

```bash
git add src/abc/tools/aozora_history_audit.clj test/abc/tools/aozora_history_audit_test.clj
git commit -m "refactor(aozora-history-audit): public scan-plan and :refs override for scan-history!"
```

---

### Task 2: replay pure core (pin parsing, pre-validation predicate, digest, classifier)

**Files:**
- Create: `abc/src/abc/tools/aozora_replay.clj` (pure fns only in this task)
- Test: `abc/test/abc/tools/aozora_replay_test.clj` (new)

**Interfaces:**
- Consumes: `abc.tools.json` (`read-json-file`), `abc.tools.aozora-csv/read-rows-from-string`.
- Produces (used verbatim by Task 3):
  - `locked-pin [lock-path] → 40-hex string` (throws ex-info `{:lock-path}` otherwise)
  - `catalog-bytes-fault [^bytes bs] → nil | "unreadable-zip" | "no-csv-entry" | "no-data-rows"`
  - `pair-digest [period-by-ref pair] → string-keyed map` (spec §Baseline document pair shape)
  - `baseline-doc [{:keys [remote-url pin-rev zip-path sample-period excluded pairs period-by-ref]}] → string-keyed map`
  - `classify-diff [old-doc new-doc] → {:verdict :unchanged|:configuration-change|:pin-bump-shaped|:behavioral-change :pair-changes [{"index" i "change" "unchanged|replaced|added|removed"} …]}`
  - defs: `baseline-format` (1), `default-remote-url`, `default-zip-path`, `default-baseline-path`

- [ ] **Step 1: Create the ns with pure fns**

Create `abc/src/abc/tools/aozora_replay.clj`:

```clojure
(ns abc.tools.aozora-replay
  "Replay abc's audit machinery over the real pinned aozorabunko history
  and pin the per-pair findings as a committed baseline. Design:
  docs/superpowers/specs/2026-07-12-aozora-replay-harness-design.md."
  (:require [abc.tools.aozora-csv :as ac]
            [abc.tools.json :as abc-json]
            [clojure.string :as string])
  (:import [java.io ByteArrayInputStream IOException]
           [java.util.zip ZipException ZipInputStream]))

(def baseline-format 1)
(def default-remote-url "https://github.com/aozorabunko/aozorabunko.git")
(def default-zip-path "index_pages/list_person_all_extended_utf8.zip")
(def default-baseline-path "test/resources/aozora-replay-baseline.json")

(defn locked-pin
  "The aozorabunko-src locked rev from a flake.lock file."
  [lock-path]
  (let [lock (abc-json/read-json-file lock-path)
        rev (get-in lock ["nodes" "aozorabunko-src" "locked" "rev"])]
    (when-not (and (string? rev) (re-matches #"[0-9a-f]{40}" rev))
      (throw (ex-info (str "no aozorabunko-src locked rev in " lock-path)
                      {:lock-path (str lock-path)})))
    rev))

(defn- zip-signature?
  "True when the bytes begin with a ZIP local-file-header (PK\\x03\\x04) or
  empty-archive end-of-central-directory (PK\\x05\\x06) signature.
  ZipInputStream.getNextEntry silently returns nil on most non-ZIP bytes,
  which would misreport garbage as no-csv-entry — so the signature is
  checked explicitly first."
  [^bytes bs]
  (and (>= (alength bs) 4)
       (= 0x50 (bit-and 0xff (aget bs 0)))
       (= 0x4B (bit-and 0xff (aget bs 1)))
       (contains? #{[3 4] [5 6]}
                  [(bit-and 0xff (aget bs 2)) (bit-and 0xff (aget bs 3))])))

(defn catalog-bytes-fault
  "nil when the bytes are a usable catalog ZIP; otherwise the source-fact
  reason string. Only ZIP-structural problems are absorbed here; anything
  else escapes as a harness/environment concern."
  [^bytes bs]
  (if-not (zip-signature? bs)
    "unreadable-zip"
    (try
      (with-open [zin (ZipInputStream. (ByteArrayInputStream. bs))]
        (loop []
          (if-let [entry (.getNextEntry zin)]
            (if (string/ends-with? (.getName entry) ".csv")
              (let [csv (String. (.readAllBytes zin) "UTF-8")]
                (if (seq (ac/read-rows-from-string csv)) nil "no-data-rows"))
              (recur))
            "no-csv-entry")))
      (catch ZipException _ "unreadable-zip")
      (catch IOException _ "unreadable-zip"))))

(defn pair-digest
  "Digest one scan pair-report into the pinned baseline pair shape."
  [period-by-ref pair]
  (let [ingest (:current_ingest pair)]
    {"previous_ref" (:previous_ref pair)
     "current_ref" (:current_ref pair)
     "period" (get period-by-ref (:current_ref pair))
     "status" (:status pair)
     "drift_summary" (get-in pair [:drift "summary"])
     "ingest" {"works_written" (:works-written ingest)
               "works_skipped" (:works-skipped ingest)
               "skipped_work_ids" (vec (:skipped-work-ids ingest))
               "persons_written" (:persons-written ingest)
               "person_conflicts" (mapv #(get % "person_id")
                                        (:person-conflicts ingest))}}))

(defn baseline-doc
  [{:keys [remote-url pin-rev zip-path sample-period excluded pairs
           period-by-ref]}]
  {"baseline_format" baseline-format
   "remote_url" remote-url
   "pin_rev" pin-rev
   "zip_path" zip-path
   "sample_period" sample-period
   "excluded" (vec excluded)
   "pairs" (mapv #(pair-digest period-by-ref %) pairs)})

(defn- header [doc]
  (select-keys doc ["baseline_format" "zip_path" "sample_period" "remote_url"]))

(defn- pair-changes [old-pairs new-pairs]
  (let [n (max (count old-pairs) (count new-pairs))]
    (vec
     (for [i (range n)
           :let [o (get old-pairs i)
                 nw (get new-pairs i)]]
       {"index" i
        "change" (cond
                   (nil? o) "added"
                   (nil? nw) "removed"
                   (= o nw) "unchanged"
                   :else "replaced")}))))

(defn- strict-final-replacement?
  "The ONLY replacement pin-bump-shaped tolerates: same period, same
  previous_ref, different current_ref (the final period gained a later
  representative). A digest change on unchanged input refs is never
  pin-bump-shaped, and neither is a replacement that moves the pair to a
  different period. Ancestry of the new current_ref is not provable in a
  pure comparison; it is implied by the plan (representatives are sampled
  from commits reachable from the fetched pin) and by human review of the
  update diff."
  [old-pair new-pair]
  (and (= (get old-pair "period") (get new-pair "period"))
       (= (get old-pair "previous_ref") (get new-pair "previous_ref"))
       (not= (get old-pair "current_ref") (get new-pair "current_ref"))))

(defn- exclusions-only-newer? [old-doc new-doc]
  (let [old-ex (set (get old-doc "excluded"))
        new-ex (set (get new-doc "excluded"))
        last-period (get (peek (get old-doc "pairs")) "period")]
    (and (every? new-ex old-ex)
         (every? (fn [e] (and (some? (get e "period"))
                              (some? last-period)
                              (pos? (compare (get e "period") last-period))))
                 (remove old-ex new-ex)))))

(defn classify-diff
  "Compare a committed baseline doc against a freshly produced one.
  Verdict semantics per the design spec's diff-classification section."
  [old-doc new-doc]
  (let [old-pairs (vec (get old-doc "pairs"))
        new-pairs (vec (get new-doc "pairs"))
        changes (pair-changes old-pairs new-pairs)
        n (count old-pairs)]
    {:pair-changes changes
     :verdict
     (cond
       (not= (header old-doc) (header new-doc))
       :configuration-change

       (= old-doc new-doc)
       :unchanged

       (= (get old-doc "pin_rev") (get new-doc "pin_rev"))
       :behavioral-change

       (and (>= (count new-pairs) n)
            (= (subvec new-pairs 0 (max 0 (dec n)))
               (subvec old-pairs 0 (max 0 (dec n))))
            (or (zero? n)
                (let [o (peek old-pairs) nw (get new-pairs (dec n))]
                  (or (= o nw) (strict-final-replacement? o nw))))
            (exclusions-only-newer? old-doc new-doc))
       :pin-bump-shaped

       :else
       :behavioral-change)}))
```

- [ ] **Step 2: Write the unit tests**

Create `abc/test/abc/tools/aozora_replay_test.clj`:

```clojure
(ns abc.tools.aozora-replay-test
  (:require [abc.sim.render :as sim-render]
            [abc.tools.aozora-replay :as replay]
            [abc.tools.json :as abc-json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- delete-recursive [^java.io.File f]
  (when (.isDirectory f)
    (doseq [child (.listFiles f)] (delete-recursive child)))
  (.delete f))

(deftest locked-pin-test
  (let [dir (temp-dir "abc-replay-lock")
        write! (fn [name value]
                 (let [f (io/file dir name)]
                   (abc-json/write-deterministic-json-file! f value)
                   (str f)))]
    (try
      (is (= (apply str (repeat 40 "a"))
             (replay/locked-pin
              (write! "good.lock"
                      {"nodes" {"aozorabunko-src"
                                {"locked" {"rev" (apply str (repeat 40 "a"))}}}}))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (replay/locked-pin (write! "bad.lock" {"nodes" {}}))))
      (finally (delete-recursive dir)))))

(deftest catalog-bytes-fault-test
  (let [m (abc.sim.model/bootstrap 1)
        good-csv (sim-render/rows->csv (sim-render/model->rows m))]
    (is (nil? (replay/catalog-bytes-fault (sim-render/csv->zip-bytes good-csv))))
    (is (= "no-data-rows"
           (replay/catalog-bytes-fault
            (sim-render/csv->zip-bytes (sim-render/rows->csv [])))))
    (is (= "no-csv-entry"
           (replay/catalog-bytes-fault
            (sim-render/csv->zip-bytes "x" {:no-entry? true}))))
    (is (= "unreadable-zip"
           (replay/catalog-bytes-fault (.getBytes "this is not a zip" "UTF-8"))))))

(defn- pair [prev cur period split-count]
  {"previous_ref" prev "current_ref" cur "period" period
   "status" "ok"
   "drift_summary" {"split_candidates" split-count}
   "ingest" {"works_written" 1 "works_skipped" 0 "skipped_work_ids" []
             "persons_written" 1 "person_conflicts" []}})

(defn- doc [pin pairs excluded]
  {"baseline_format" replay/baseline-format
   "remote_url" replay/default-remote-url
   "pin_rev" pin
   "zip_path" replay/default-zip-path
   "sample_period" "year"
   "excluded" excluded
   "pairs" pairs})

(deftest classify-diff-test
  (let [p1 (pair "r0" "r1" "2023" 0)
        p2 (pair "r1" "r2" "2024" 1)
        old (doc "pinA" [p1 p2] [])]
    (testing "unchanged"
      (is (= :unchanged (:verdict (replay/classify-diff old old)))))
    (testing "configuration-change wins over everything"
      (is (= :configuration-change
             (:verdict (replay/classify-diff
                        old (assoc (doc "pinB" [p1 p2] []) "sample_period" "month"))))))
    (testing "any change with unchanged pin is behavioral"
      (is (= :behavioral-change
             (:verdict (replay/classify-diff
                        old (doc "pinA" [p1 (pair "r1" "r2" "2024" 2)] []))))))
    (testing "pin-bump-shaped: strict final replacement + append"
      (is (= :pin-bump-shaped
             (:verdict (replay/classify-diff
                        old (doc "pinB" [p1 (pair "r1" "r3" "2024" 1)
                                         (pair "r3" "r4" "2025" 0)] []))))))
    (testing "digest change on unchanged final refs is NOT pin-bump-shaped"
      (is (= :behavioral-change
             (:verdict (replay/classify-diff
                        old (doc "pinB" [p1 (pair "r1" "r2" "2024" 9)] []))))))
    (testing "final replacement that moves to a different period is NOT pin-bump-shaped"
      (is (= :behavioral-change
             (:verdict (replay/classify-diff
                        old (doc "pinB" [p1 (pair "r1" "r3" "2025" 1)] []))))))
    (testing "historical pair change is behavioral even with pin bump"
      (is (= :behavioral-change
             (:verdict (replay/classify-diff
                        old (doc "pinB" [(pair "r0" "r1" "2023" 5) p2] []))))))
    (testing "exclusion for a historical period is behavioral"
      (is (= :behavioral-change
             (:verdict (replay/classify-diff
                        old (doc "pinB" [p1 p2]
                                 [{"ref" "rX" "period" "2023" "reason" "no-csv-entry"}]))))))
    (testing "exclusion for a NEW period is pin-bump-shaped"
      (is (= :pin-bump-shaped
             (:verdict (replay/classify-diff
                        old (doc "pinB" [p1 p2]
                                 [{"ref" "rX" "period" "2025" "reason" "no-csv-entry"}]))))))
    (testing "pair-changes classification"
      (is (= ["unchanged" "replaced" "added"]
             (mapv #(get % "change")
                   (:pair-changes (replay/classify-diff
                                   old (doc "pinB" [p1 (pair "r1" "r3" "2024" 1)
                                                    (pair "r3" "r4" "2025" 0)] [])))))))))

(deftest pair-digest-test
  (let [digest (replay/pair-digest
                {"cur-sha" "2024"}
                {:previous_ref "prev-sha" :current_ref "cur-sha" :status "ok"
                 :drift {"summary" {"split_candidates" 2}}
                 :current_ingest {:works-written 3 :works-skipped 1
                                  :skipped-work-ids ["000101"]
                                  :persons-written 4
                                  :person-conflicts [{"person_id" "000009"
                                                      "chosen_work_id" "000101"
                                                      "work_ids" ["000101" "000102"]}]}})]
    (is (= {"previous_ref" "prev-sha" "current_ref" "cur-sha" "period" "2024"
            "status" "ok"
            "drift_summary" {"split_candidates" 2}
            "ingest" {"works_written" 3 "works_skipped" 1
                      "skipped_work_ids" ["000101"] "persons_written" 4
                      "person_conflicts" ["000009"]}}
           digest))))
```

Add `[abc.sim.model]` to the ns require vector (used by `catalog-bytes-fault-test`): the `:require` list gains `[abc.sim.model]` (no alias needed; it is referenced fully qualified).

- [ ] **Step 3: Run to verify green**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :unit` → PASS (new tests included)
Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation` → PASS

- [ ] **Step 4: Commit**

```bash
git add src/abc/tools/aozora_replay.clj test/abc/tools/aozora_replay_test.clj
git commit -m "feat(aozora-replay): pure core — pin parsing, pre-validation predicate, digest, diff classifier"
```

---

### Task 3: git effects, replay orchestration, CLI, alias, recipes

**Files:**
- Modify: `abc/src/abc/tools/aozora_replay.clj` (append effectful half)
- Modify: `abc/deps.edn` (alias, after line `:abc/aozora-history-audit …`)
- Modify: root `justfile` (two recipes, after the `sim-soak` recipe)
- Test: `abc/test/abc/tools/aozora_replay_test.clj` (plumbing integration)

**Interfaces:**
- Consumes: Task 1's `audit/scan-plan` + `scan-history!` `:refs`; Task 2's pure fns; `abc.git/{load-git-repo,blob-bytes-at,resolve-ref}`.
- Produces: `ensure-clone!`, `prefetch-and-prevalidate!`, `replay-doc!`, `run-replay!`, `-main`; CLI alias `:abc/aozora-replay`; recipes `replay-aozora`, `replay-aozora-update`.

- [ ] **Step 1: Append the effectful half to the ns**

Extend the ns form of `abc/src/abc/tools/aozora_replay.clj`:

```clojure
(ns abc.tools.aozora-replay
  "Replay abc's audit machinery over the real pinned aozorabunko history
  and pin the per-pair findings as a committed baseline. Design:
  docs/superpowers/specs/2026-07-12-aozora-replay-harness-design.md."
  (:require [abc.git :as abc-git]
            [abc.tools.aozora-csv :as ac]
            [abc.tools.aozora-history-audit :as audit]
            [abc.tools.cli :as abc-cli]
            [abc.tools.json :as abc-json]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as string]
            [taoensso.telemere :as tel])
  (:import [java.io ByteArrayInputStream IOException]
           [java.util.zip ZipException ZipInputStream]
           [org.eclipse.jgit.errors MissingObjectException]))
```

Append after the pure fns:

```clojure
;; ---------------------------------------------------------------------
;; Git effects: managed partial clone + tiered blob availability.

(defn default-cache-dir []
  (str (io/file (or (System/getenv "XDG_CACHE_HOME")
                    (str (System/getProperty "user.home") "/.cache"))
                "abc" "aozorabunko")))

(defn- git*
  "Run git with argv `args` (strings), optionally in `dir`. Returns the
  clojure.java.shell result map; never throws on nonzero exit."
  [args {:keys [dir out-enc]}]
  (apply shell/sh
         (concat ["git"]
                 (when dir ["-C" (str dir)])
                 (map str args)
                 (when out-enc [:out-enc out-enc]))))

(defn- git!
  "Like git*, but nonzero exit throws ex-info with the command context."
  [args {:keys [dir] :as opts}]
  (let [{:keys [exit err] :as res} (git* args opts)]
    (if (zero? exit)
      res
      (throw (ex-info (str "git " (string/join " " args) " failed ("
                           exit "): " (string/trim (or err "")))
                      {:git-args (vec (map str args))
                       :dir (some-> dir str)
                       :exit exit})))))

(defn- ensure-commit!
  "Ensure `ref` resolves to a commit object in `dir`, fetching if needed."
  [dir ref]
  (when-not (zero? (:exit (git* ["cat-file" "-e" (str ref "^{commit}")]
                                {:dir dir})))
    (when-not (zero? (:exit (git* ["fetch" "origin" ref] {:dir dir})))
      (git! ["fetch" "origin"] {:dir dir}))
    (git! ["cat-file" "-e" (str ref "^{commit}")] {:dir dir})))

(defn verify-origin!
  "Verify a repo's origin URL matches the expected remote-url. The
  baseline records remote_url as the upstream identity, so a repo whose
  origin disagrees must be refused. A repo with NO origin remote (e.g. a
  locally built test repo) is allowed with a warning: the recorded
  remote_url is then a declared, not observed, upstream."
  [repo-dir remote-url]
  (let [{:keys [exit out]} (git* ["remote" "get-url" "origin"]
                                 {:dir repo-dir})]
    (if (zero? exit)
      (let [actual (string/trim out)]
        (when (not= actual remote-url)
          (throw (ex-info (str "repo at " repo-dir " has origin " actual
                               ", expected " remote-url)
                          {:cache-dir (str repo-dir)
                           :expected-url remote-url
                           :actual-url actual}))))
      (tel/log! :warn (str "repo at " repo-dir " has no origin remote; "
                           "recording remote_url as declared upstream: "
                           remote-url)))))

(defn ensure-clone!
  "Create or update the managed blobless partial clone. Verifies cache
  provenance (origin URL must equal remote-url) before fetching — the
  baseline must never record one source while replaying another.
  Returns cache-dir as a string."
  [{:keys [cache-dir remote-url to-ref]}]
  (let [dir (io/file cache-dir)]
    (if (.exists (io/file dir ".git"))
      (verify-origin! (str dir) remote-url)
      (do (io/make-parents (io/file dir "placeholder"))
          (git! ["clone" "--filter=blob:none" "--no-checkout"
                 remote-url (str dir)]
                {})))
    (when to-ref (ensure-commit! (str dir) to-ref))
    (str dir)))

(defn blob-availability
  "Tiered read of zip-path bytes at ref (spec unit 3):
  {:bytes bs} | {:excluded \"missing-at-ref\"} | :missing-object."
  [repo ref zip-path]
  (try
    {:bytes (abc-git/blob-bytes-at repo ref zip-path)}
    (catch clojure.lang.ExceptionInfo e
      (if (= zip-path (:path (ex-data e)))
        {:excluded "missing-at-ref"}
        (throw e)))
    (catch MissingObjectException _ :missing-object)))

(defn ensure-blob-bytes
  "Bytes of zip-path at ref, attempting ONE CLI promisor fetch when the
  object is promised but locally absent. A still-missing object is an
  environment failure (loud), never an exclusion."
  [repo repo-dir ref zip-path]
  (let [r (blob-availability repo ref zip-path)]
    (if (not= :missing-object r)
      r
      (do
        (try
          (git! ["cat-file" "blob" (str ref ":" zip-path)]
                {:dir repo-dir :out-enc :bytes})
          (catch Exception e
            (throw (ex-info (str "object for " zip-path " at " ref
                                 " is unavailable locally and the promisor "
                                 "fetch failed")
                            {:ref ref :zip-path zip-path :repo (str repo-dir)
                             :cause-tier :missing-local-object}
                            e))))
        (let [r2 (blob-availability repo ref zip-path)]
          (if (= :missing-object r2)
            (throw (ex-info (str "object for " zip-path " at " ref
                                 " still missing after promisor fetch")
                            {:ref ref :zip-path zip-path :repo (str repo-dir)
                             :cause-tier :missing-local-object}))
            r2))))))

(defn prefetch-and-prevalidate!
  "Partition the plan into surviving refs and pinned source-fact
  exclusions. Runs identically for managed caches and --aozora-repo."
  [repo repo-dir plan zip-path]
  (reduce
   (fn [acc {:keys [ref period]}]
     (let [{:keys [bytes excluded]} (ensure-blob-bytes repo repo-dir ref zip-path)
           reason (or excluded (catalog-bytes-fault bytes))]
       (if reason
         (update acc :excluded conj {"ref" ref "period" period "reason" reason})
         (update acc :refs conj {:ref ref :period period}))))
   {:refs [] :excluded []}
   plan))

;; ---------------------------------------------------------------------
;; Orchestration + CLI.

(defn- log-phase!
  "Progress + timing telemetry for the long-running phases. Timing lives
  ONLY in logs (telemere timestamps), never in the baseline."
  [phase started-ms detail]
  (tel/log! :info (str "replay " phase " ("
                       (- (System/currentTimeMillis) started-ms) " ms): "
                       detail)))

(defn replay-doc!
  "Plan, prefetch/pre-validate, scan, digest. Returns the baseline doc."
  [{:keys [aozora-repo cache-dir remote-url to-ref from-ref sample-period
           zip-path work-dir]}]
  (let [t0 (System/currentTimeMillis)
        repo-dir (if aozora-repo
                   (do (verify-origin! (str aozora-repo) remote-url)
                       aozora-repo)
                   (ensure-clone! {:cache-dir cache-dir
                                   :remote-url remote-url
                                   :to-ref to-ref}))
        _ (log-phase! "clone-ready" t0 (str repo-dir))
        repo (abc-git/load-git-repo (str repo-dir))]
    (try
      (let [t1 (System/currentTimeMillis)
            pin-rev (.getName (abc-git/resolve-ref repo (or to-ref "HEAD")))
            plan (audit/scan-plan repo {:zip-path zip-path
                                        :from-ref from-ref
                                        :to-ref to-ref
                                        :sample-period sample-period})
            _ (log-phase! "plan" t1 (str (count plan) " representatives"))
            t2 (System/currentTimeMillis)
            {:keys [refs excluded]} (prefetch-and-prevalidate!
                                     repo repo-dir plan zip-path)
            _ (log-phase! "prefetch" t2 (str (count refs) " usable, "
                                             (count excluded) " excluded"))
            t3 (System/currentTimeMillis)
            scan (audit/scan-history! {:aozora-repo (str repo-dir)
                                       :refs (mapv :ref refs)
                                       :zip-path zip-path
                                       :work-dir work-dir})
            _ (log-phase! "scan" t3 (str (count (:pairs scan)) " pairs"))]
        (baseline-doc {:remote-url remote-url
                       :pin-rev pin-rev
                       :zip-path zip-path
                       :sample-period sample-period
                       :excluded excluded
                       :pairs (:pairs scan)
                       :period-by-ref (into {} (map (juxt :ref :period)) refs)}))
      (finally (.close repo)))))

(defn run-replay!
  "Run one replay in :check or :update mode. Returns a result map with
  ::exit-fail? set for CLI dispatch."
  [{:keys [check update baseline] :as opts}]
  (let [doc (replay-doc! opts)]
    (cond
      update
      (do (abc-json/write-deterministic-json-file! (io/file baseline) doc)
          {:mode "update" :baseline (str baseline)
           :pairs (count (get doc "pairs"))
           :excluded (count (get doc "excluded"))
           ::exit-fail? false})

      check
      (let [old (try (abc-json/read-json-file baseline)
                     (catch Exception e
                       (throw (ex-info (str "baseline unreadable: " baseline)
                                       {:baseline (str baseline)} e))))
            {:keys [verdict pair-changes]} (classify-diff old doc)]
        {:mode "check" :baseline (str baseline)
         :verdict (name verdict)
         :pair_changes pair-changes
         ::exit-fail? (not= :unchanged verdict)}))))

(def ^:private cli-options
  [[nil "--check" "Compare a fresh replay against the committed baseline"]
   [nil "--update" "Rewrite the baseline from a fresh replay"]
   [nil "--sample-period PERIOD" "Sampling period: month or year"
    :default "year"]
   [nil "--from-ref REF" "Optional window start (ad-hoc runs only)"]
   [nil "--to-ref REF" "Replay end ref; defaults to the abc/flake.lock pin"]
   [nil "--cache-dir DIR" "Managed partial-clone location"]
   [nil "--remote-url URL" "Upstream remote for the managed clone"]
   [nil "--aozora-repo DIR" "Use an existing clone (skips clone management only)"]
   [nil "--baseline FILE" "Baseline path"]
   [nil "--work-dir DIR" "Tool-owned scan work dir"
    :default "out/aozora-replay"]
   [nil "--zip-path PATH" "Catalog ZIP path inside the upstream repo"]
   ["-h" "--help"]])

(defn- usage [summary]
  (str "Usage: clojure -M:abc/aozora-replay -- (--check | --update) [options]\n\n"
       "Replays the pinned aozorabunko history (year-sampled) through the\n"
       "audit machinery and checks/updates the committed baseline.\n\n"
       summary))

(defn resolve-options
  "Fill defaults that need runtime context and enforce mode guards.
  Public: this is the boundary that protects the committed baseline from
  accidental ad-hoc overwrites, and it is tested directly."
  [{:keys [check update baseline sample-period from-ref to-ref] :as options}]
  (when (= (boolean check) (boolean update))
    (throw (ex-info "exactly one of --check / --update is required"
                    {:check (boolean check) :update (boolean update)})))
  (let [pin (locked-pin "flake.lock")
        baseline (or baseline default-baseline-path)
        default-baseline? (= baseline default-baseline-path)]
    (when (and update default-baseline?
               (or (not= "year" sample-period)
                   (some? from-ref)
                   (and (some? to-ref) (not= to-ref pin))))
      (throw (ex-info (str "refusing --update of the default baseline with "
                           "non-default sampling/window flags; pass --baseline "
                           "for ad-hoc runs")
                      {:sample-period sample-period :from-ref from-ref
                       :to-ref to-ref :pin pin})))
    (-> options
        (assoc :baseline baseline)
        (update :to-ref #(or % pin))
        (update :cache-dir #(or % (default-cache-dir)))
        (update :remote-url #(or % default-remote-url))
        (update :zip-path #(or % default-zip-path)))))

(defn -main [& args]
  (abc-cli/run-cli!
   args
   {:cli-options cli-options
    :usage-fn usage
    :run (fn [{:keys [options]}]
           (let [result (run-replay! (resolve-options options))]
             (println (abc-json/write-deterministic-json-str
                       (dissoc result ::exit-fail?)))
             result))
    :fail? ::exit-fail?}))
```

- [ ] **Step 2: Add the deps.edn alias**

In `abc/deps.edn`, directly under the line
`:abc/aozora-history-audit {:main-opts ["-m" "abc.tools.aozora-history-audit"]}`
add:

```clojure
  :abc/aozora-replay {:main-opts ["-m" "abc.tools.aozora-replay"]}
```

- [ ] **Step 3: Add the just recipes**

Append to the root `justfile` after the `sim-soak` recipe:

```make
# Replay the pinned aozorabunko history through the audit machinery and
# diff against the committed baseline (needs network on a cold cache).
replay-aozora:
	cd abc && clojure -M:abc/aozora-replay -- --check

# Re-run the replay and rewrite the committed baseline (adjudicate the
# diff in the same PR as whatever caused it).
replay-aozora-update:
	cd abc && clojure -M:abc/aozora-replay -- --update
```

- [ ] **Step 4: Write the plumbing integration tests**

Append to `abc/test/abc/tools/aozora_replay_test.clj`:

```clojure
(defn- commit-state! [git dir family instant]
  (sim-render/commit-zip-at!
   git dir
   (sim-render/csv->zip-bytes
    (sim-render/rows->csv
     (sim-render/model->rows
      (:model (abc.sim.model/apply-event
               (abc.sim.model/bootstrap 1)
               {:event/type :edit-person :pid "000001"
                :field :family_name :value family})))))
   (str "state " family) instant))

(deftest replay-plumbing-integration-test
  (let [repo-dir (temp-dir "abc-replay-repo")
        work-dir (temp-dir "abc-replay-work")
        baseline (io/file (temp-dir "abc-replay-base") "baseline.json")
        git (sim-render/init-repo! repo-dir)]
    (try
      (commit-state! git repo-dir "壱" "2023-03-01T00:00:00Z")
      (commit-state! git repo-dir "弐" "2024-03-01T00:00:00Z")
      (commit-state! git repo-dir "参" "2025-03-01T00:00:00Z")
      (let [opts {:aozora-repo (str repo-dir)
                  :remote-url replay/default-remote-url
                  :sample-period "year"
                  :zip-path sim-render/zip-path
                  :work-dir (str work-dir)}
            doc1 (replay/replay-doc! opts)]
        (testing "--update writes a well-formed baseline; immediate re-run is :unchanged"
          (is (= ["2024" "2025"] (mapv #(get % "period") (get doc1 "pairs"))))
          (is (= [] (get doc1 "excluded")))
          (abc-json/write-deterministic-json-file! baseline doc1)
          (is (= :unchanged
                 (:verdict (replay/classify-diff
                            (abc-json/read-json-file (str baseline))
                            (replay/replay-doc! opts))))))
        (testing "a new upstream-like commit classifies as pin-bump-shaped"
          (commit-state! git repo-dir "肆" "2026-03-01T00:00:00Z")
          (is (= :pin-bump-shaped
                 (:verdict (replay/classify-diff
                            doc1 (replay/replay-doc! opts))))))
        (testing "a doctored historical pair classifies as behavioral-change"
          (let [doctored (update-in doc1 ["pairs" 0 "drift_summary"
                                          "metadata_corrections"]
                                    (fnil inc 0))
                fresh (replay/replay-doc! opts)]
            (is (= :behavioral-change
                   (:verdict (replay/classify-diff doctored fresh)))))))
      (finally
        (.close git)
        (delete-recursive repo-dir)
        (delete-recursive work-dir)
        (delete-recursive (.getParentFile baseline))))))

(deftest ensure-clone-provenance-test
  (let [origin (temp-dir "abc-replay-origin")
        cache (temp-dir "abc-replay-cache")]
    (try
      (let [git (sim-render/init-repo! origin)]
        (commit-state! git origin "壱" "2023-03-01T00:00:00Z")
        (.close git))
      ;; a cache cloned from one URL...
      (clojure.java.shell/sh "git" "clone" "--no-checkout"
                             (str "file://" origin) (str (io/file cache "clone")))
      ;; ...must be refused when the replay expects another
      (let [e (try (replay/ensure-clone!
                    {:cache-dir (str (io/file cache "clone"))
                     :remote-url "https://example.invalid/other.git"})
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (some? e))
        (is (= "https://example.invalid/other.git"
               (:expected-url (ex-data e))))
        (is (string? (:actual-url (ex-data e)))))
      (finally
        (delete-recursive origin)
        (delete-recursive cache)))))
```

Append the destructive-default-baseline guard tests (this boundary prevents
accidental replacement of checked-in evidence; note `:sample-period` is
passed explicitly because tools.cli's default is absent in direct calls):

```clojure
(deftest resolve-options-guards-test
  (let [pin (replay/locked-pin "flake.lock")]
    (testing "exactly one of --check/--update"
      (is (thrown? clojure.lang.ExceptionInfo
                   (replay/resolve-options {:sample-period "year"})))
      (is (thrown? clojure.lang.ExceptionInfo
                   (replay/resolve-options {:check true :update true
                                            :sample-period "year"}))))
    (testing "default-baseline --update refuses non-default sampling/window"
      (is (thrown? clojure.lang.ExceptionInfo
                   (replay/resolve-options {:update true
                                            :sample-period "month"})))
      (is (thrown? clojure.lang.ExceptionInfo
                   (replay/resolve-options {:update true
                                            :sample-period "year"
                                            :from-ref "somewhere"})))
      (is (thrown? clojure.lang.ExceptionInfo
                   (replay/resolve-options {:update true
                                            :sample-period "year"
                                            :to-ref (apply str (repeat 40 "d"))}))))
    (testing "explicit ad-hoc baseline is permitted with any flags"
      (is (= "month" (:sample-period
                      (replay/resolve-options {:update true
                                               :sample-period "month"
                                               :baseline "/tmp/adhoc.json"})))))
    (testing "defaults resolve from runtime context"
      (let [r (replay/resolve-options {:check true :sample-period "year"})]
        (is (= pin (:to-ref r)))
        (is (= replay/default-baseline-path (:baseline r)))
        (is (= replay/default-remote-url (:remote-url r)))
        (is (= replay/default-zip-path (:zip-path r)))))))
```

Also append the two tiering tests (spec test section: "path absent at ref vs
missing local object"):

```clojure
(deftest replay-missing-at-ref-exclusion-test
  (let [repo-dir (temp-dir "abc-replay-noref")
        work-dir (temp-dir "abc-replay-noref-work")
        git (sim-render/init-repo! repo-dir)]
    (try
      ;; from-ref commit predates the ZIP path entirely
      (let [c-nozip (sim-render/commit-file-at! git repo-dir "README.md"
                                                "no zip yet" "init"
                                                "2022-01-01T00:00:00Z")]
        (commit-state! git repo-dir "壱" "2023-03-01T00:00:00Z")
        (commit-state! git repo-dir "弐" "2024-03-01T00:00:00Z")
        (let [doc (replay/replay-doc! {:aozora-repo (str repo-dir)
                                       :remote-url replay/default-remote-url
                                       :from-ref (.getName c-nozip)
                                       :sample-period "year"
                                       :zip-path sim-render/zip-path
                                       :work-dir (str work-dir)})]
          (is (= [{"ref" (.getName c-nozip) "period" nil
                   "reason" "missing-at-ref"}]
                 (get doc "excluded"))
              "path absent in the historical tree is a pinned exclusion")
          (is (= ["2024"] (mapv #(get % "period") (get doc "pairs")))
              "pairing runs over the survivors")))
      (finally
        (.close git)
        (delete-recursive repo-dir)
        (delete-recursive work-dir)))))

(deftest ensure-blob-bytes-missing-object-is-loud-test
  ;; Deleting the loose blob fabricates a promised-but-absent object; the
  ;; repo has no promisor remote, so the tier must be LOUD, never an
  ;; exclusion.
  (let [repo-dir (temp-dir "abc-replay-missing-obj")
        git (sim-render/init-repo! repo-dir)]
    (try
      (let [c (commit-state! git repo-dir "壱" "2023-03-01T00:00:00Z")
            blob-sha (clojure.string/trim
                      (:out (clojure.java.shell/sh
                             "git" "-C" (str repo-dir) "rev-parse"
                             (str (.getName c) ":" sim-render/zip-path))))
            obj (io/file repo-dir ".git" "objects"
                         (subs blob-sha 0 2) (subs blob-sha 2))]
        (is (.exists obj) "fresh commits leave loose objects")
        (is (.delete obj))
        (let [repo (abc-git/load-git-repo (str repo-dir))]
          (try
            (is (= :missing-object
                   (replay/blob-availability repo (.getName c)
                                             sim-render/zip-path)))
            (let [e (try (replay/ensure-blob-bytes repo (str repo-dir)
                                                   (.getName c)
                                                   sim-render/zip-path)
                         nil
                         (catch clojure.lang.ExceptionInfo e e))]
              (is (some? e))
              (is (= :missing-local-object (:cause-tier (ex-data e)))))
            (finally (.close repo)))))
      (finally
        (.close git)
        (delete-recursive repo-dir)))))
```

Add `[clojure.java.shell]`, `[clojure.string]`, and `[abc.git :as abc-git]` to the test ns `:require` (the first two are referenced fully qualified).

- [ ] **Step 5: Run to verify green**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :unit` → PASS
Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation` → PASS
Run: `clojure -M:abc/aozora-replay -- --help` → usage text, exit 0.

- [ ] **Step 6: Commit**

```bash
git add src/abc/tools/aozora_replay.clj test/abc/tools/aozora_replay_test.clj deps.edn ../justfile
git commit -m "feat(aozora-replay): clone management, tiered prefetch, replay CLI, recipes"
```

---

### Task 4: first real replay run + enforcement test (one atomic commit)

**Files:**
- Create: `abc/test/resources/aozora-replay-baseline.json` (tool output, human-reviewed)
- Test: `abc/test/abc/tools/aozora_replay_test.clj` (append enforcement test)

This task is executed by the session controller directly (it needs network
and ~1–2 h wall clock — not a subagent transcription task). The baseline and
the test that enforces its upstream identity land in ONE commit, so no
intermediate state exists where the evidence is committed but unenforced.

- [ ] **Step 1:** From the repo root: `just replay-aozora-update`, run in the background. Progress and phase/per-pair timing stream to the log (Task 1's per-pair lines + Task 3's `log-phase!` lines). Record `du -sh` of the cache dir after the clone.
- [ ] **Step 2:** Record in the PR/ledger, from the telemere log timestamps: clone wall time and size, plan time, prefetch time, per-pair time range, total time, and the `excluded` entries with reasons (settles spec assumptions A5/A6).
- [ ] **Step 3:** Human review of the produced baseline pair-by-pair — this review IS the adjudication of what the classifier historically reports on real data. Sanity expectations: `pin_rev` equals `abc/flake.lock`'s rev; ~13 pairs, periods 2013/2014…2026 (minus exclusions); recent pairs show `works_skipped` 0 and `person_conflicts` [].
- [ ] **Step 4:** `just replay-aozora` → exits 0, verdict `unchanged` (same-machine determinism).
- [ ] **Step 5: Append the enforcement test**

Append to `abc/test/abc/tools/aozora_replay_test.clj`:

```clojure
(def ^:private drift-summary-required-keys
  ["persons_previous" "persons_current" "works_previous" "works_current"
   "added_person_ids" "removed_person_ids" "metadata_corrections"
   "contributor_edge_additions" "contributor_edge_removals"
   "contributor_edge_replacements" "split_candidates" "merge_candidates"
   "ambiguous_replacements"])

(deftest baseline-pin-coupling-test
  (let [doc (abc-json/read-json-file replay/default-baseline-path)
        pairs (vec (get doc "pairs"))
        excluded (vec (get doc "excluded"))
        sha? (fn [s] (and (string? s) (re-matches #"[0-9a-f]{40}" s)))
        count? (fn [v] (and (int? v) (<= 0 v)))]
    (testing "header shape"
      (is (= replay/baseline-format (get doc "baseline_format")))
      (is (= replay/default-zip-path (get doc "zip_path")))
      (is (= replay/default-remote-url (get doc "remote_url")))
      (is (= "year" (get doc "sample_period")))
      (is (sha? (get doc "pin_rev"))))
    (testing "pair shape"
      (is (seq pairs) "committed baseline must contain pairs")
      (is (= (mapv #(get % "period") pairs)
             (vec (sort (mapv #(get % "period") pairs))))
          "pairs sorted by period")
      (doseq [p pairs]
        (is (sha? (get p "previous_ref")) (pr-str p))
        (is (sha? (get p "current_ref")) (pr-str p))
        (is (re-matches #"\d{4}(-\d{2})?" (or (get p "period") ""))
            "committed pairs carry non-nil period keys")
        (is (contains? #{"ok" "validation_failed"} (get p "status")))
        (doseq [k drift-summary-required-keys]
          (is (count? (get-in p ["drift_summary" k]))
              (str "drift_summary." k " in " (pr-str (get p "period")))))
        (let [ingest (get p "ingest")]
          (is (count? (get ingest "works_written")) (pr-str p))
          (is (count? (get ingest "works_skipped")) (pr-str p))
          (is (count? (get ingest "persons_written")) (pr-str p))
          (is (vector? (get ingest "skipped_work_ids")) (pr-str p))
          (is (every? string? (get ingest "skipped_work_ids")) (pr-str p))
          (is (vector? (get ingest "person_conflicts")) (pr-str p))
          (is (every? string? (get ingest "person_conflicts")) (pr-str p)))))
    (testing "exclusion shape: unique, complete, known reasons"
      (is (= (count excluded) (count (distinct (map #(get % "ref") excluded))))
          "no duplicate excluded refs")
      (doseq [e excluded]
        (is (sha? (get e "ref")) (pr-str e))
        (is (contains? e "period") (pr-str e))
        (is (contains? #{"missing-at-ref" "unreadable-zip"
                         "no-csv-entry" "no-data-rows"}
                       (get e "reason")))))
    (testing "pin coupling: baseline pin == abc/flake.lock pin"
      (is (= (replay/locked-pin "flake.lock") (get doc "pin_rev"))
          (str "baseline pin_rev disagrees with abc/flake.lock — after a pin "
               "bump, re-run `just replay-aozora-update`, adjudicate the "
               "diff, and commit the new baseline")))
    (testing "root flake.lock agrees with abc/flake.lock"
      (is (= (replay/locked-pin "flake.lock")
             (replay/locked-pin "../flake.lock"))
          "partial pin bump: abc/flake.lock and the root flake.lock carry different aozorabunko-src revs"))))
```

Negative-path note: `locked-pin-test` (Task 2) already proves mismatched or
missing revs throw; the coupling assertions are plain `=` over `locked-pin`
calls, so no lock-file editing is needed here.

- [ ] **Step 6: Run to verify green**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :unit` → PASS
Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation` → PASS

- [ ] **Step 7: Commit (baseline + enforcement together)**

```bash
git add test/resources/aozora-replay-baseline.json test/abc/tools/aozora_replay_test.clj
git commit -m "feat(aozora-replay): pin first real-history baseline with enforcement test"
```
