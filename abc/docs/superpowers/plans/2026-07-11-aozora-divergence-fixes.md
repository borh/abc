# Aozora Evolution Divergence Fixes (D1–D6) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fix the six open production divergences (D1–D6) recorded in `abc/test/abc/sim/divergences.clj`, flipping each gated simulation case from expected-failure to a hard assertion of desired behavior.

**Architecture:** Each fix follows the spec's failure taxonomy (spec: `abc/docs/superpowers/specs/2026-07-11-aozora-evolution-simulation-testing-design.md` §Failure Taxonomy, §Known Divergences): row/work faults are absorbed and counted per work; source faults throw `ex-info` with required ex-data keys. D4 restructures corpus ingest into build→reconcile→write phases so skipped works leave no files behind and shared-person conflicts resolve deterministically. Every task flips the divergence's table status FIRST (test goes red with "now passes — adjudicate" inverted, i.e. the gate now demands desired behavior), then fixes production code (green).

**Tech Stack:** Clojure (deps.edn project under `abc/`), kaocha suites `:unit` and `:simulation`, charred CSV, JGit (tests only).

## Global Constraints

- Production changes ONLY in `abc/src/abc/tools/aozora_csv.clj`, `abc/src/abc/tools/aozora_ingest.clj`, `abc/src/abc/tools/aozora_history_audit.clj`. No other `src/` file may change.
- Simulation-harness changes are limited to: flipping statuses/notes in `abc/test/abc/sim/divergences.clj`, and updating stale comments in `abc/test/abc/sim/ingest_sim_test.clj` / `abc/test/abc/sim/audit_sim_test.clj`. Gated-test PREDICATES must not change (the desired-behavior encodings are already approved).
- Divergence status flip: `:open` → `:fixed`, note becomes `"fixed 2026-07-11: <one-line what changed>"`. `abc.sim.divergences/open?` treats `:fixed` as closed, so the gate then asserts desired behavior and reports " (Dn) regressed" on failure.
- Two-tier failure contract (spec §Failure Taxonomy, verbatim): row/work faults are absorbed per work, counted in `:works-skipped` with ids in `:skipped-work-ids` and a logged reason; source faults throw `ex-info` with required ex-data keys; forbidden exception classes are `NullPointerException`, `AssertionError`, `StackOverflowError`, raw `java.util.zip.ZipException`.
- Production-safety fact (measured 2026-07-11 against the pinned catalog `github:aozorabunko/aozorabunko/0e9ea3e5…`, 19,470 rows / 17,810 works): **0 ragged rows, 0 divergent-work-field works**. The D1/D3 "skip the work" policies therefore change nothing on current real data.
- After EVERY task, both suites must pass from `abc/`:
  - `clojure -M:test:kaocha -m kaocha.runner --focus :unit`
  - `clojure -M:test:kaocha -m kaocha.runner --focus :simulation`
- All test commands run from the `abc/` directory.
- Commit messages: conventional commits (`fix(aozora-ingest): …`, `fix(aozora-csv): …`, `fix(aozora-history-audit): …`, `docs(specs): …`).

---

### Task 1: D6 — wrap raw ZipException as ex-info

**Files:**
- Modify: `abc/src/abc/tools/aozora_ingest.clj` (fn `read-zip-csv`, ~line 28)
- Modify: `abc/test/abc/sim/divergences.clj` (D6 entry)
- Modify: `abc/test/abc/sim/audit_sim_test.clj` (`p13-non-zip-bytes-sim-test`)
- Test: `abc/test/abc/tools/aozora_ingest_test.clj`

**Interfaces:**
- Consumes: existing `read-zip-csv` internals.
- Produces: non-ZIP bytes at `:zip-path` now throw `ex-info` with ex-data `{:zip-path <path>}` and the `ZipException` chained as cause. No signature changes.

- [ ] **Step 1: Flip the divergence gate (red)**

In `abc/test/abc/sim/divergences.clj` replace the D6 entry:

```clojure
   :D6 {:case "P13.non-zip-bytes" :status :fixed
        :notes "fixed 2026-07-11: read-zip-csv wraps ZipException as ex-info {:zip-path} with cause chained"}
```

In `abc/test/abc/sim/audit_sim_test.clj`, simplify `p13-non-zip-bytes-sim-test` — with D6 fixed, a raw ZipException is forbidden outright, so the hand-rolled three-class check collapses into `forbidden-throw?`:

```clojure
;; P13.non-zip-bytes — D6 fixed: wrapped ex-info, cause chained.
(deftest p13-non-zip-bytes-sim-test
  (let [{:keys [threw]} (audit-two! "this is not a zip file")]
    (is (some? threw) "non-ZIP bytes must not produce a normal-looking report")
    (is (not (forbidden-throw? threw)) "P13.non-zip-bytes")
    (div/expected-failure :D6 "P13.non-zip-bytes"
                          (clean-ex-info? threw [:zip-path]))))
```

- [ ] **Step 2: Write the failing unit test**

Append to `abc/test/abc/tools/aozora_ingest_test.clj` (the file already has `temp-dir`, `delete-recursive`, and requires `[clojure.java.io :as io]`):

```clojure
(deftest ingest-non-zip-bytes-wrapped-test
  (testing "non-ZIP bytes at the zip path throw ex-info {:zip-path} with ZipException cause"
    (let [dir (temp-dir "abc-ingest-notzip")
          fake (io/file dir "fake.zip")
          out (io/file dir "out")]
      (try
        (spit fake "this is not a zip file")
        (let [e (try (ingest/run-corpus-from-zip! {:zip-path (str fake)
                                                   :output-dir (str out)})
                     nil
                     (catch Exception e e))]
          (is (instance? clojure.lang.ExceptionInfo e))
          (is (= (str fake) (:zip-path (ex-data e))))
          (is (instance? java.util.zip.ZipException (ex-cause e))))
        (finally (delete-recursive dir))))))
```

- [ ] **Step 3: Run to verify failures**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :unit --focus-meta :kaocha/pending 2>/dev/null; clojure -M:test:kaocha -m kaocha.runner --focus :unit`
Expected: `ingest-non-zip-bytes-wrapped-test` FAILS (raw ZipException escapes, not ExceptionInfo).

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation`
Expected: `p13-non-zip-bytes-sim-test` FAILS with "P13.non-zip-bytes (:D6) regressed".

- [ ] **Step 4: Implement the wrap**

In `abc/src/abc/tools/aozora_ingest.clj`, add a private helper above `read-zip-csv` and use it:

```clojure
(defn- open-zip
  "Open zip-path as a ZipFile, wrapping the unreadable-archive case as
  ex-info {:zip-path} with the ZipException chained (failure taxonomy:
  ZIP source validation)."
  ^ZipFile [^String zip-path]
  (try
    (ZipFile. (io/file zip-path))
    (catch java.util.zip.ZipException e
      (throw (ex-info (str zip-path " is not a readable ZIP archive")
                      {:zip-path zip-path}
                      e)))))
```

and in `read-zip-csv` change:

```clojure
  (with-open [zf (open-zip zip-path)]
```

- [ ] **Step 5: Run both suites to verify pass**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :unit` → PASS
Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation` → PASS

- [ ] **Step 6: Commit**

```bash
git add src/abc/tools/aozora_ingest.clj test/abc/tools/aozora_ingest_test.clj test/abc/sim/divergences.clj test/abc/sim/audit_sim_test.clj
git commit -m "fix(aozora-ingest): wrap non-ZIP source as ex-info with cause (D6)"
```

---

### Task 2: D5 — fail loud on empty/header-only CSV

**Files:**
- Modify: `abc/src/abc/tools/aozora_ingest.clj` (fns `run!`, `run-corpus-from-zip!`, ~lines 273–293)
- Modify: `abc/test/abc/sim/divergences.clj` (D5 entry)
- Test: `abc/test/abc/tools/aozora_ingest_test.clj`

**Interfaces:**
- Consumes: `read-zip-csv` (Task 1's shape), `ac/read-rows-from-string`.
- Produces: both ZIP-sourced entry points (`run!`, `run-corpus-from-zip!`) throw `ex-info` with ex-data `{:zip-path <path> :row-count 0}` when the CSV entry parses to zero data rows. New private helper `rows-from-zip`.

- [ ] **Step 1: Flip the divergence gate (red)**

In `abc/test/abc/sim/divergences.clj` replace the D5 entry:

```clojure
   :D5 {:case "P13.empty-csv" :status :fixed
        :notes "fixed 2026-07-11: zip entry points throw ex-info {:zip-path :row-count} when the CSV has no data rows"}
```

- [ ] **Step 2: Write the failing unit test**

Append to `abc/test/abc/tools/aozora_ingest_test.clj`. Add `[abc.sim.render :as sim-render]` to the ns `:require` (the test classpath already includes the sim namespaces; `abc.tools.aozora-history-audit-test` requires it the same way):

```clojure
(deftest ingest-empty-csv-fails-loud-test
  (testing "empty and header-only CSVs throw ex-info with :zip-path and :row-count"
    (doseq [[label csv] [["empty" ""]
                         ["header-only" "作品ID,人物ID,役割フラグ"]]]
      (let [dir (temp-dir "abc-ingest-empty")
            zip (io/file dir "catalog.zip")
            out (io/file dir "out")]
        (try
          (with-open [o (io/output-stream zip)]
            (.write o ^bytes (sim-render/csv->zip-bytes csv)))
          (let [e (try (ingest/run-corpus-from-zip! {:zip-path (str zip)
                                                     :output-dir (str out)})
                       nil
                       (catch Exception e e))]
            (is (instance? clojure.lang.ExceptionInfo e) label)
            (is (= (str zip) (:zip-path (ex-data e))) label)
            (is (= 0 (:row-count (ex-data e))) label))
          (finally (delete-recursive dir)))))))
```

- [ ] **Step 3: Run to verify failures**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :unit`
Expected: `ingest-empty-csv-fails-loud-test` FAILS (no exception thrown; silent zero-row corpus).

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation`
Expected: `p13-empty-csv-sim-test` FAILS with "P13.empty-csv/… (:D5) regressed".

- [ ] **Step 4: Implement the guard**

In `abc/src/abc/tools/aozora_ingest.clj`, add a private helper above `run!`:

```clojure
(defn- rows-from-zip
  "Read and parse the CSV entry at zip-path. Fails loudly when the entry
  has no data rows (failure taxonomy: corpus source validation) —
  a silent zero-row corpus must never look like a successful run."
  [zip-path source-url]
  (let [{:keys [csv provenance]} (read-zip-csv zip-path source-url)
        rows (ac/read-rows-from-string csv)]
    (when (empty? rows)
      (throw (ex-info (str "CSV entry in " zip-path " has no data rows")
                      {:zip-path zip-path :row-count (count rows)})))
    {:rows rows :provenance provenance}))
```

Rewrite both entry points to use it:

```clojure
(defn run!
  "CLI-shaped entry: reads the CSV from a ZIP path and delegates to
  run-from-rows!. Same return value."
  [{:keys [zip-path source-url] :as opts}]
  (let [{:keys [rows provenance]} (rows-from-zip zip-path source-url)]
    (run-from-rows! (-> opts
                        (dissoc :zip-path :source-url)
                        (assoc :rows rows
                               :source-csv-provenance provenance)))))

(defn run-corpus-from-zip!
  "CLI-shaped entry: reads the CSV from a ZIP path and delegates to
  run-corpus!."
  [{:keys [zip-path source-url] :as opts}]
  (let [{:keys [rows provenance]} (rows-from-zip zip-path source-url)]
    (run-corpus! (-> opts
                     (dissoc :zip-path :source-url)
                     (assoc :rows rows
                            :source-csv-provenance provenance)))))
```

- [ ] **Step 5: Run both suites to verify pass**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :unit` → PASS
Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation` → PASS

- [ ] **Step 6: Commit**

```bash
git add src/abc/tools/aozora_ingest.clj test/abc/tools/aozora_ingest_test.clj test/abc/sim/divergences.clj
git commit -m "fix(aozora-ingest): fail loud on empty/header-only CSV (D5)"
```

---

### Task 3: D2 — period sampling robust to non-monotonic dates

**Files:**
- Modify: `abc/src/abc/tools/aozora_history_audit.clj` (fn `sample-commits-by-period`, ~lines 188–193)
- Modify: `abc/test/abc/sim/divergences.clj` (D2 entry)
- Test: `abc/test/abc/tools/aozora_history_audit_test.clj`

**Interfaces:**
- Consumes: `commit-period-key` (unchanged), JGit RevCommits from `abc-git/commits-touching-path`.
- Produces: `sample-commits-by-period` selects, for each calendar period, the LAST commit in log order among that period's commits (grouping is global, not contiguous-run based); representatives are returned in log order. Signature unchanged.

- [ ] **Step 1: Flip the divergence gate (red)**

In `abc/test/abc/sim/divergences.clj` replace the D2 entry:

```clojure
   :D2 {:case "P12.selection" :status :fixed
        :notes "fixed 2026-07-11: sample-commits-by-period groups globally by period key (was contiguous partition-by), keeping the last commit in log order per period"}
```

- [ ] **Step 2: Write the failing unit test**

Append to `abc/test/abc/tools/aozora_history_audit_test.clj` (helpers `row`, `csv-text`, `temp-dir`, `delete-recursive` and `sim-render` already exist in this file):

```clojure
(deftest scan-history-year-sampling-non-monotone-test
  (testing "year sampling picks one representative per year even when author dates are non-monotonic in log order"
    (let [repo-dir (temp-dir "abc-audit-nonmono")
          work-dir (temp-dir "abc-audit-nonmono-work")
          git (sim-render/init-repo! repo-dir)
          state (fn [family] (sim-render/csv->zip-bytes
                              (csv-text [(row {"姓" family})])))
          ;; log order: 2023, 2024, 2023(!), 2024 — years interleaved
          c0 (sim-render/commit-zip-at! git repo-dir (state "壱") "s0" "2023-01-01T00:00:00Z")
          _c1 (sim-render/commit-zip-at! git repo-dir (state "弐") "s1" "2024-03-01T00:00:00Z")
          c2 (sim-render/commit-zip-at! git repo-dir (state "参") "s2" "2023-06-01T00:00:00Z")
          c3 (sim-render/commit-zip-at! git repo-dir (state "肆") "s3" "2024-09-01T00:00:00Z")]
      (try
        (let [result (audit/scan-history! {:aozora-repo (str repo-dir)
                                           :from-ref (.getName c0)
                                           :sample-period "year"
                                           :work-dir (str work-dir)})]
          ;; 2023 → c2 (last 2023 commit in log order), 2024 → c3
          (is (= [[(.getName c0) (.getName c2)]
                  [(.getName c2) (.getName c3)]]
                 (mapv (juxt :previous_ref :current_ref) (:pairs result)))))
        (finally
          (.close git)
          (delete-recursive repo-dir)
          (delete-recursive work-dir))))))
```

- [ ] **Step 3: Run to verify failures**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :unit`
Expected: `scan-history-year-sampling-non-monotone-test` FAILS (partition-by yields a representative per contiguous run: three pairs, not two).

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation`
Expected: `p12-selection-non-monotone-sim-test` FAILS with "P12.selection (:D2) regressed".

- [ ] **Step 4: Implement global grouping**

Replace `sample-commits-by-period` in `abc/src/abc/tools/aozora_history_audit.clj`:

```clojure
(defn- sample-commits-by-period
  "One representative per calendar period: the last commit in log order
  among that period's commits. Grouping is global (not partition-by over
  contiguous runs), so non-monotonic author dates cannot yield multiple
  representatives for one period. Representatives keep log order."
  [commits sample-period]
  (if-not sample-period
    commits
    (->> (map-indexed vector commits)
         (group-by (fn [[_ c]] (commit-period-key sample-period c)))
         vals
         (map peek)
         (sort-by first)
         (mapv second))))
```

- [ ] **Step 5: Run both suites to verify pass**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :unit` → PASS (including the existing monotone `scan-history` sampling test, whose expectations are unchanged under global grouping)
Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation` → PASS

- [ ] **Step 6: Commit**

```bash
git add src/abc/tools/aozora_history_audit.clj test/abc/tools/aozora_history_audit_test.clj test/abc/sim/divergences.clj
git commit -m "fix(aozora-history-audit): sample one representative per period under non-monotonic dates (D2)"
```

---

### Task 4: D1 — reject divergent work fields across rows

**Files:**
- Modify: `abc/src/abc/tools/aozora_csv.clj` (fn `build-record-fragment-from-rows`, ~lines 284–325)
- Modify: `abc/test/abc/sim/divergences.clj` (D1 entry)
- Test: `abc/test/abc/tools/aozora_csv_test.clj`, `abc/test/abc/tools/aozora_ingest_test.clj`

**Interfaces:**
- Consumes: existing fragment builder internals.
- Produces: `build-record-fragment-from-rows` throws `ex-info` with ex-data `{:work-id <wid> :divergent-works [<parsed maps>]}` when the same work_id parses to more than one distinct work-field map. `run-corpus!`'s existing per-work `ExceptionInfo` catch absorbs it (work skipped + counted); the single-work CLI path stays fail-loud. Adjudication basis: pinned real catalog has 0 such works (Global Constraints), so skipping is production-safe and the spec's "work skipped or audit entry" choice resolves to skip.

- [ ] **Step 1: Flip the divergence gate (red)**

In `abc/test/abc/sim/divergences.clj` replace the D1 entry:

```clojure
  {:D1 {:case "P6.divergent-work-fields" :status :fixed
        :notes "fixed 2026-07-11: build-record-fragment-from-rows throws ex-info on divergent work fields; corpus ingest skips + counts the work"}
```

- [ ] **Step 2: Write the failing unit tests**

Append to `abc/test/abc/tools/aozora_csv_test.clj` (file has `csv-text` as a def of real-shaped CSV text; build rows by parsing then assoc):

```clojure
(deftest build-record-fragment-divergent-work-fields-test
  (testing "divergent work fields across same-work rows throw ex-info, not first-row-wins"
    (let [base (first (ac/read-rows-from-string csv-text))
          rows [base (assoc base "作品名" "別の題名"
                            "人物ID" "000880"
                            "役割フラグ" "翻訳者")]
          e (try (ac/build-record-fragment-from-rows rows)
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (some? e))
      (is (= "000127" (:work-id (ex-data e))))
      (is (= 2 (count (:divergent-works (ex-data e))))))))
```

Append to `abc/test/abc/tools/aozora_ingest_test.clj`:

```clojure
(deftest ingest-corpus-skips-divergent-work-fields-test
  (testing "a work whose rows disagree on work fields is skipped + counted; clean works survive"
    (let [out-dir (temp-dir "abc-ingest-divergent-work")
          rows (conj synthetic-corpus-rows
                     ;; second row for work 000127 with a different 作品名
                     (person-row {"作品ID" "000127"
                                  "作品名" "別の羅生門"
                                  "人物ID" "000888"
                                  "役割フラグ" "翻訳者"
                                  "姓" "夏目" "名" "漱石"
                                  "姓読み" "なつめ" "名読み" "そうせき"
                                  "姓読みソート用" "なつめ" "名読みソート用" "そうせき"
                                  "姓ローマ字" "Natsume" "名ローマ字" "Soseki"
                                  "生年月日" "1867-02-09" "没年月日" "1916-12-09"
                                  "底本名1" "羅生門" "底本出版社名1" "テスト出版社"}))]
      (try
        (let [{:keys [works-written works-skipped skipped-work-ids]}
              (ingest/run-corpus! {:rows rows :output-dir (str out-dir)})]
          (is (= 2 works-written))
          (is (= 1 works-skipped))
          (is (= ["000127"] skipped-work-ids))
          (is (not (.exists (io/file out-dir "works" "000127.json")))))
        (finally (delete-recursive out-dir))))))
```

- [ ] **Step 3: Run to verify failures**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :unit`
Expected: both new tests FAIL (no exception; first-row-wins writes 000127).

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation`
Expected: `p6-divergent-work-fields-sim-test` FAILS with "P6.divergent-work-fields (:D1) regressed".

- [ ] **Step 4: Implement the rejection**

In `abc/src/abc/tools/aozora_csv.clj` `build-record-fragment-from-rows`: update the docstring line "Asserts work-level fields are consistent across rows" to "Throws ex-info if work-level fields diverge across rows", and insert after the existing work-ids assert:

```clojure
    (let [unique-works (vec (distinct works))]
      (when (< 1 (count unique-works))
        (throw (ex-info (str "work " (first work-ids)
                             " has divergent work fields across CSV rows")
                        {:work-id (first work-ids)
                         :divergent-works unique-works}))))
```

(The subsequent `{:work (first works) …}` return stays; after the guard, `(first works)` is the unique parse.)

- [ ] **Step 5: Run both suites to verify pass**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :unit` → PASS
Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation` → PASS

- [ ] **Step 6: Commit**

```bash
git add src/abc/tools/aozora_csv.clj test/abc/tools/aozora_csv_test.clj test/abc/tools/aozora_ingest_test.clj test/abc/sim/divergences.clj
git commit -m "fix(aozora-csv): reject divergent work fields instead of first-row-wins (D1)"
```

---

### Task 5: D3 — mark ragged rows at the reader, reject at work assembly

**Files:**
- Modify: `abc/src/abc/tools/aozora_csv.clj` (fn `read-rows*` ~lines 22–32, fn `build-record-fragment-from-rows`)
- Modify: `abc/test/abc/sim/divergences.clj` (D3 entry)
- Test: `abc/test/abc/tools/aozora_csv_test.clj`, `abc/test/abc/tools/aozora_ingest_test.clj`

**Interfaces:**
- Consumes: Task 4's fragment guard structure.
- Produces: public var `abc.tools.aozora-csv/ragged-key` (the namespaced keyword `::ragged?`); `read-rows*` assocs `ragged-key true` onto any row whose cell count ≠ header count (keyword key — can never collide with string header columns; invisible to all `(get row "…")` consumers). `build-record-fragment-from-rows` throws `ex-info` `{:work-id <wid> :ragged-rows <n>}` when any input row is ragged, BEFORE the divergent-work-fields guard (a truncated row must be reported as ragged, not as field divergence). Corpus ingest absorbs it per work.

- [ ] **Step 1: Flip the divergence gate (red)**

In `abc/test/abc/sim/divergences.clj` replace the D3 entry:

```clojure
   :D3 {:case "P13.ragged-row" :status :fixed
        :notes "fixed 2026-07-11: read-rows* marks ragged rows (ragged-key); work assembly rejects them, corpus ingest skips + counts the work"}
```

- [ ] **Step 2: Write the failing unit tests**

Append to `abc/test/abc/tools/aozora_csv_test.clj`:

```clojure
(deftest read-rows-marks-ragged-test
  (testing "rows whose cell count differs from the header are marked with ragged-key"
    (let [csv "a,b,c\n1,2,3\n1,2\n1,2,3,4"
          [ok short long] (ac/read-rows-from-string csv)]
      (is (not (contains? ok ac/ragged-key)))
      (is (true? (get short ac/ragged-key)))
      (is (true? (get long ac/ragged-key)))
      (is (= "1" (get short "a")) "surviving cells still parse")
      (is (nil? (get short "c")) "missing trailing cells stay absent"))))

(deftest build-record-fragment-rejects-ragged-test
  (testing "a ragged row rejects the work with a ragged reason, not a field-divergence reason"
    (let [base (first (ac/read-rows-from-string csv-text))
          rows [base (assoc base ac/ragged-key true)]
          e (try (ac/build-record-fragment-from-rows rows)
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (some? e))
      (is (= "000127" (:work-id (ex-data e))))
      (is (= 1 (:ragged-rows (ex-data e)))))))
```

Append to `abc/test/abc/tools/aozora_ingest_test.clj` (raw-CSV path, proving the end-to-end skip):

```clojure
(deftest ingest-corpus-skips-ragged-row-work-test
  (testing "a ragged CSV row skips exactly its work; clean works survive"
    (let [out-dir (temp-dir "abc-ingest-ragged")
          headers (vec (sort (keys (first synthetic-corpus-rows))))
          cells (fn [r] (mapv #(get r % "") headers))
          csv (->> (concat [headers]
                           (map cells (take 2 synthetic-corpus-rows))
                           ;; work 000129's row loses its last 3 cells
                           [(vec (drop-last 3 (cells (nth synthetic-corpus-rows 2))))])
                   (map #(clojure.string/join "," %))
                   (clojure.string/join "\n"))
          rows (ac/read-rows-from-string csv)]
      (try
        (let [{:keys [works-written works-skipped skipped-work-ids]}
              (ingest/run-corpus! {:rows rows :output-dir (str out-dir)})]
          (is (= 2 works-written))
          (is (= 1 works-skipped))
          (is (= ["000129"] skipped-work-ids))
          (is (not (.exists (io/file out-dir "works" "000129.json"))))
          (is (not (.exists (io/file out-dir "persons" "000888.json")))
              "the ragged work's sole person is never written"))
        (finally (delete-recursive out-dir))))))
```

Note: `synthetic-corpus-rows` cells contain no commas/quotes/newlines, so plain string-join CSV is well-formed here. Add `[abc.tools.aozora-csv :as ac]` and `[clojure.string]` to the ingest test ns `:require` if not present.

- [ ] **Step 3: Run to verify failures**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :unit`
Expected: all three new tests FAIL (`ragged-key` unresolved / no throw / silent truncation).

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation`
Expected: `p13-ragged-row-sim-test` FAILS with "P13.ragged-row/… (:D3) regressed".

- [ ] **Step 4: Implement marking + rejection**

In `abc/src/abc/tools/aozora_csv.clj`:

```clojure
(def ragged-key
  "Marker key assoc'd (true) onto a parsed row whose cell count differed
  from the header row's. A namespaced keyword, so it can never collide
  with a string header column and is invisible to (get row \"…\") users."
  ::ragged?)

(defn- read-rows*
  "Internal: takes a charred result (vector of vectors, first row is
  header) and returns a seq of maps keyed by header column. Strips
  the UTF-8 BOM from the first header cell. Ragged rows (cell count ≠
  header count) are marked with ragged-key; surviving cells still parse."
  [rows]
  (when (seq rows)
    (let [header (mapv (fn [c] (strip-bom (or c ""))) (first rows))
          width (count header)]
      (mapv (fn [r]
              (cond-> (into {} (map (fn [k v] [k (or v "")]) header r))
                (not= width (count r)) (assoc ragged-key true)))
            (rest rows)))))
```

In `build-record-fragment-from-rows`, insert BEFORE the divergent-work-fields guard from Task 4 (and mention ragged rejection in the docstring):

```clojure
    (let [ragged (count (filter ragged-key rows))]
      (when (pos? ragged)
        (throw (ex-info (str "work " (first work-ids)
                             " has " ragged " ragged CSV row(s)")
                        {:work-id (first work-ids)
                         :ragged-rows ragged}))))
```

- [ ] **Step 5: Run both suites to verify pass**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :unit` → PASS
Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation` → PASS

- [ ] **Step 6: Commit**

```bash
git add src/abc/tools/aozora_csv.clj test/abc/tools/aozora_csv_test.clj test/abc/tools/aozora_ingest_test.clj test/abc/sim/divergences.clj
git commit -m "fix(aozora-csv): reject ragged rows instead of silent truncation (D3)"
```

---

### Task 6: D4 — atomic per-work ingest with deterministic shared-person policy

**Files:**
- Modify: `abc/src/abc/tools/aozora_ingest.clj` (fns `run-from-rows!`, `run-corpus!`; new fn `build-work-plan`)
- Modify: `abc/test/abc/sim/divergences.clj` (D4 entry)
- Modify: `abc/test/abc/sim/ingest_sim_test.clj` (stale "Current:" comments on P8 cases; predicates unchanged)
- Test: `abc/test/abc/tools/aozora_ingest_test.clj`

**Interfaces:**
- Consumes: `ac/build-record-fragment-from-rows` (with Task 4/5 guards), `build-person-record`, `write-person-file!`, `self-consistency-check!`.
- Produces:
  - `build-work-plan` (private): `{:rows :work-id :source-csv-provenance}` → `{:work-id :person-records {pid record} :metadata-rec}`; ALL validation (fragment guards, `person-record/validate!` per contributor, `self-consistency-check!`) happens here, NO filesystem writes. Throws `ex-info` on any row/work fault.
  - `run-from-rows!`: same signature/return (`metadata_record_hash` string); now validates everything before the first write. Legacy fail-loud + refuse-to-overwrite semantics kept for the single-work CLI path.
  - `run-corpus!`: same required/optional keys; return map gains `:person-conflicts` (vector, possibly empty). **Stated corpus-level policy** (docstring, verbatim intent): works are fully built and validated before any writes, so a skipped work leaves no new or modified files; when surviving works carry divergent bodies for the same person_id, the body from the smallest work_id wins deterministically, every affected work is still written with its contributor entries referencing the winning record's hash, and each conflict is warn-logged and reported in `:person-conflicts` as `{"person_id" pid "chosen_work_id" wid "work_ids" [wids…]}`; pre-existing on-disk divergence without `:overwrite` still fails loudly via `write-person-file!` (cross-RUN conflicts are an environment fault, not a row fault).

- [ ] **Step 1: Flip the divergence gate (red)**

In `abc/test/abc/sim/divergences.clj` replace the D4 entry:

```clojure
   :D4 {:case "P8.atomicity, P8.order-independence" :status :fixed
        :notes "fixed 2026-07-11: run-corpus! builds+validates all works before writing; shared-person conflicts resolve to the smallest work_id deterministically and are reported in :person-conflicts"}
```

In `abc/test/abc/sim/ingest_sim_test.clj`, update ONLY comments (predicates and structure unchanged):
- `p8-atomicity-sim-test`: replace the comment lines "P8.atomicity — D4: a skipped work must leave no person records behind." … and the inner "DESIRED: … Current: 000001 (sorted first) is written before 000002 fails." with:

```clojure
;; P8.atomicity — D4 fixed: a skipped work leaves no person records behind.
;; The fault fails LATE in the old flow (second contributor's schema
;; validation); the build-before-write structure must keep both files off
;; disk regardless.
```
and inside the gate:
```clojure
        ;; DESIRED (holds since D4 fix): the skipped work wrote no person
        ;; files at all.
```
- `p8-order-independence-sim-test`: replace the gate's trailing comment "Current: when the dirty work ingests first, the clean work is skipped by the guard." with "Holds since the D4 fix: conflicts resolve deterministically at the corpus level; no work is skipped for a cross-work conflict."

- [ ] **Step 2: Write the failing unit tests**

Append to `abc/test/abc/tools/aozora_ingest_test.clj`:

```clojure
(deftest ingest-corpus-atomic-skip-test
  (testing "a work that fails late (second contributor invalid) leaves NO person files behind"
    (let [out-dir (temp-dir "abc-ingest-atomic")
          rows [(person-row {"作品ID" "000131"
                             "底本名1" "作品甲" "底本出版社名1" "テスト出版社"})
                ;; second contributor of the same work, invalid date that
                ;; passes parse-date verbatim and fails schema validation
                (person-row {"作品ID" "000131"
                             "人物ID" "000999"
                             "役割フラグ" "翻訳者"
                             "姓" "テスト" "名" "次郎"
                             "姓読み" "てすと" "名読み" "じろう"
                             "姓読みソート用" "てすと" "名読みソート用" "しろう"
                             "姓ローマ字" "Test" "名ローマ字" "Jiro"
                             "生年月日" "2020-02-31"
                             "底本名1" "作品甲" "底本出版社名1" "テスト出版社"})]]
      (try
        (let [{:keys [works-written works-skipped skipped-work-ids]}
              (ingest/run-corpus! {:rows rows :output-dir (str out-dir)})]
          (is (= 0 works-written))
          (is (= 1 works-skipped))
          (is (= ["000131"] skipped-work-ids))
          (is (not (.exists (io/file out-dir "persons" "000879.json")))
              "the VALID first contributor must not be left behind")
          (is (not (.exists (io/file out-dir "persons" "000999.json")))))
        (finally (delete-recursive out-dir))))))

(deftest ingest-corpus-shared-person-conflict-policy-test
  (testing "divergent shared-person bodies: smallest work_id wins, both works written, conflict reported"
    (let [out-dir (temp-dir "abc-ingest-conflict")
          rows [(person-row {"作品ID" "000127"
                             "姓" "旧" ;; body A carried by the smaller work id
                             "底本名1" "羅生門" "底本出版社名1" "テスト出版社"})
                (person-row {"作品ID" "000128"
                             "作品名" "鼻" "作品名読み" "はな" "ソート用読み" "はな"
                             "姓" "新" ;; divergent body for the SAME person 000001
                             "底本名1" "鼻" "底本出版社名1" "テスト出版社"})]
          ;; NOTE person-row defaults 人物ID to 000001-equivalent — both rows
          ;; share the default person id.
          {:keys [works-written works-skipped person-conflicts]}
          (ingest/run-corpus! {:rows rows :output-dir (str out-dir)})]
      (try
        (is (= 2 works-written))
        (is (= 0 works-skipped))
        (is (= 1 (count person-conflicts)))
        (let [conflict (first person-conflicts)
              pid (get conflict "person_id")
              person (files/read-json (str (io/file out-dir "persons" (str pid ".json"))))
              hash-of (fn [wid]
                        (-> (files/read-json (str (io/file out-dir "works" (str wid ".json"))))
                            (get "contributors") first (get "person_record_hash")))]
          (is (= "000127" (get conflict "chosen_work_id")))
          (is (= ["000127" "000128"] (get conflict "work_ids")))
          (is (= "旧" (get person "family_name")) "smallest work_id's body wins")
          (is (= (person-record/record-hash person) (hash-of "000127") (hash-of "000128"))
              "both works reference the winning record's hash"))
        (finally (delete-recursive out-dir))))))
```

Add `[abc.tools.person-record :as person-record]` to the ingest test ns `:require` if absent. Check `person-row` defaults: it defaults `人物ID` per the file's `row` helper — use whatever pid that default is; if it is not shared automatically, set `"人物ID" "000879"` explicitly on both rows and use `"000879"` in the assertions.

- [ ] **Step 3: Run to verify failures**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :unit`
Expected: `ingest-corpus-atomic-skip-test` FAILS (000879.json left behind); `ingest-corpus-shared-person-conflict-policy-test` FAILS (`person-conflicts` nil; work 000128 skipped by refuse-to-overwrite).

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation`
Expected: `p8-atomicity-sim-test` and `p8-order-independence-sim-test` FAIL with "… (:D4) regressed".

- [ ] **Step 4: Implement build→reconcile→write**

In `abc/src/abc/tools/aozora_ingest.clj`:

**4a.** Add `build-work-plan` above `run-from-rows!` (it absorbs the fragment/validation portion of the old `run-from-rows!`, including the parse-corrections debug logging):

```clojure
(defn- build-work-plan
  "Pure build phase for one work: parse the fragment, build and
  schema-validate every contributor person record, assemble the
  metadata-record, and self-consistency-check it. NO filesystem writes —
  any row/work fault throws ex-info here, before a single byte lands on
  disk. Returns {:work-id :person-records {pid record} :metadata-rec}."
  [{:keys [rows work-id source-csv-provenance]}]
  (let [matching (filter #(= work-id (get % "作品ID")) rows)]
    (when-not (seq matching)
      (throw (ex-info (str "no rows for work_id " work-id " in supplied rows")
                      {:work-id work-id})))
    (let [{:keys [work persons-by-id contributors corrections-by-pid]}
          (ac/build-record-fragment-from-rows matching)
          _ (doseq [[pid corrs] (sort-by key corrections-by-pid)
                    c corrs]
              (tel/log! :debug
                        (str "parse-correction person=" pid
                             " field=" (get c "field")
                             " rule=" (get c "rule")
                             " raw=" (pr-str (get c "raw"))
                             " corrected=" (pr-str (get c "corrected")))))
          person-records
          (into (sorted-map)
                (map (fn [[pid body]]
                       (let [record (build-person-record
                                     body (get corrections-by-pid pid)
                                     source-csv-provenance)]
                         (person-record/validate! record)
                         [pid record])))
                persons-by-id)
          contributor-entries
          (vec (for [c contributors
                     :let [pid (get c "person_id")]]
                 {"person_id" pid
                  "person_record_hash" (person-record/record-hash
                                        (get person-records pid))
                  "relation_to_work" (get c "relation_to_work")}))
          metadata-rec {"metadata_record_schema_id" schema-id
                        "metadata_record_schema_hash" (am/cached-schema-hash schema-path)
                        "work" work
                        "contributors" (vec (sort-by #(get % "person_id")
                                                     contributor-entries))}]
      (self-consistency-check! metadata-rec)
      {:work-id work-id
       :person-records person-records
       :metadata-rec metadata-rec})))
```

**4b.** Rewrite `run-from-rows!` as build-then-write (same docstring contract, plus one sentence: "All validation happens before the first write; a validation failure leaves no files behind."):

```clojure
(defn run-from-rows!
  [{:keys [rows work-id output persons-output-dir overwrite refresh-manifest
           source-csv-provenance]}]
  (let [{:keys [person-records metadata-rec]}
        (build-work-plan {:rows rows :work-id work-id
                          :source-csv-provenance source-csv-provenance})
        persons-dir (or persons-output-dir
                        (str (.getParent (io/file output)) "/persons"))]
    (doseq [[_pid record] person-records]
      (write-person-file! persons-dir record (boolean overwrite)))
    (.mkdirs (.getParentFile (io/file output)))
    (json/write-deterministic-json-file! (io/file output) metadata-rec)
    (let [new-hash (metadata-record/record-hash metadata-rec)]
      (tel/log! :debug (str "metadata_record_hash: " new-hash))
      (when refresh-manifest
        (let [m (files/read-json refresh-manifest)
              m' (assoc-in m ["manifest_identity_object" "metadata_record_hash"] new-hash)
              identity-obj (get m' "manifest_identity_object")
              artifact-id (hash/format-sha256 (hash/sha256-json-jcs identity-obj))
              m'' (assoc m' "artifact_id" artifact-id)]
          (json/write-deterministic-json-file! (io/file refresh-manifest) m'')
          (tel/log! :info (str "refreshed manifest " refresh-manifest))))
      new-hash)))
```

(Keep the original docstring's Required/Optional keys text.)

**4c.** Rewrite `run-corpus!`. Docstring must state the corpus-level policy from the Interfaces block above. Return map: `{:works-written :persons-written :works-skipped :skipped-work-ids :person-conflicts}`.

```clojure
(defn run-corpus!
  [{:keys [rows output-dir overwrite source-csv-provenance]}]
  (let [works-dir (io/file output-dir "works")
        persons-dir (io/file output-dir "persons")
        rows-by-work (group-by #(get % "作品ID") rows)
        {:keys [plans skipped]}
        (reduce
         (fn [acc [work-id work-rows]]
           (try
             (update acc :plans conj
                     (build-work-plan {:rows work-rows
                                       :work-id work-id
                                       :source-csv-provenance source-csv-provenance}))
             (catch clojure.lang.ExceptionInfo e
               (let [{:keys [errors-humanized field value]} (ex-data e)
                     hint (or (some-> errors-humanized first)
                              (when (and field value)
                                (str field "=" (pr-str value)))
                              "no detail")]
                 (tel/log! :warn
                           (str "skipped work " work-id ": "
                                (.getMessage e) " — " hint)))
               (update acc :skipped conj work-id))))
         {:plans [] :skipped []}
         (sort-by key rows-by-work))
        ;; corpus-level shared-person reconciliation: plans arrive sorted by
        ;; work id, so the FIRST carrier of a pid is the smallest work id.
        carriers-by-pid
        (reduce (fn [m {:keys [work-id person-records]}]
                  (reduce-kv (fn [m pid record]
                               (update m pid (fnil conj [])
                                       {:work-id work-id :record record}))
                             m person-records))
                (sorted-map)
                plans)
        resolutions
        (mapv (fn [[pid carriers]]
                (let [winner (first carriers)
                      hashes (distinct (map #(person-record/record-hash (:record %))
                                            carriers))]
                  {:pid pid
                   :record (:record winner)
                   :hash (first hashes)
                   :conflict (when (< 1 (count hashes))
                               {"person_id" pid
                                "chosen_work_id" (:work-id winner)
                                "work_ids" (vec (distinct (map :work-id carriers)))})}))
              carriers-by-pid)
        chosen-hash (into {} (map (juxt :pid :hash)) resolutions)
        conflicts (vec (keep :conflict resolutions))]
    (doseq [c conflicts]
      (tel/log! :warn
                (str "person " (get c "person_id")
                     " has divergent bodies across works "
                     (get c "work_ids")
                     "; keeping the body from work " (get c "chosen_work_id"))))
    (.mkdirs works-dir)
    (.mkdirs persons-dir)
    (doseq [{:keys [record]} resolutions]
      (write-person-file! persons-dir record (boolean overwrite)))
    (doseq [{:keys [work-id metadata-rec]} plans
            :let [rec (update metadata-rec "contributors"
                              (fn [cs]
                                (mapv #(assoc % "person_record_hash"
                                              (chosen-hash (get % "person_id")))
                                      cs)))]]
      (json/write-deterministic-json-file!
       (io/file works-dir (str work-id ".json")) rec))
    {:works-written (count plans)
     :persons-written (count resolutions)
     :works-skipped (count skipped)
     :skipped-work-ids skipped
     :person-conflicts conflicts}))
```

Notes for the implementer:
- The contributor-hash patch is a no-op for unconflicted pids (their chosen hash IS their own hash), so clean corpora produce byte-identical output to the old code — the existing determinism and P6/P9 tests pin this.
- `self-consistency-check!` already ran per plan in `build-work-plan`; the hash patch cannot change schema validity (same sha256 lexical format).
- Do NOT catch exceptions around the write loops: a cross-RUN on-disk conflict without `:overwrite` (thrown by `write-person-file!`) is an environment fault and fails the whole corpus run loudly — that is the stated policy.

- [ ] **Step 5: Run both suites to verify pass**

Run: `clojure -M:test:kaocha -m kaocha.runner --focus :unit` → PASS (including `ingest-corpus-emits-work-and-person-files-test`, `ingest-corpus-skips-invalid-work-and-continues-test`, `ingest-corpus-deterministic-test`, and Tasks 4–5's skip tests, all unchanged)
Run: `clojure -M:test:kaocha -m kaocha.runner --focus :simulation` → PASS (P6/P8/P9/P10/P13/P14 all green with D4 `:fixed`)

- [ ] **Step 6: Commit**

```bash
git add src/abc/tools/aozora_ingest.clj test/abc/tools/aozora_ingest_test.clj test/abc/sim/divergences.clj test/abc/sim/ingest_sim_test.clj
git commit -m "fix(aozora-ingest): atomic per-work builds and deterministic shared-person policy (D4)"
```

---

### Task 7: Spec + docs alignment and full acceptance run

**Files:**
- Modify: `abc/docs/superpowers/specs/2026-07-11-aozora-evolution-simulation-testing-design.md` (§Failure Taxonomy table, §Known Divergences seed table)

**Interfaces:**
- Consumes: all six fixes landed.
- Produces: spec tables record `fixed (2026-07-11)` statuses so the spec (the table's source of record) and `divergences.clj` agree.

- [ ] **Step 1: Update the spec's Known Divergences seed table**

For each of the six rows in the seed table (§Known Divergences and Triage), change the Status column from `open` to `fixed (2026-07-11)`. In the D5 and D6 rows, the "(confirmed 2026-07-11)" parentheticals stay.

- [ ] **Step 2: Update the spec's Failure Taxonomy table**

In the §Failure Taxonomy table, change the Divergence column entries `D3`, `D1`, `D5`, `D6` to `D3 (fixed)`, `D1 (fixed)`, `D5 (fixed)`, `D6 (fixed)`.

- [ ] **Step 3: Full acceptance run**

From `abc/`:

```bash
clojure -M:test:kaocha -m kaocha.runner --focus :unit
clojure -M:test:kaocha -m kaocha.runner --focus :simulation
ABC_SIM_SOAK=1 clojure -M:test:kaocha -m kaocha.runner --focus :simulation
```

Expected: all three PASS (soak = unseeded 15×; any new failure it finds is reported, not silently retried).

- [ ] **Step 4: Commit**

```bash
git add docs/superpowers/specs/2026-07-11-aozora-evolution-simulation-testing-design.md
git commit -m "docs(specs): mark divergences D1-D6 fixed in the simulation-testing spec"
```
