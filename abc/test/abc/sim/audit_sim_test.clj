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

;; P10.audit-vs-scan
(deftest p10-audit-vs-scan-sim-test
  (harness/check! "P10.audit-vs-scan" 10
                  (prop/for-all [hist (sgen/history-gen {:length [2 4] :works [3 5]
                                                         :forced :clean-split})]
                                (let [{:keys [states]} (model/fold-history hist)
                                      two [(first states) (peek states)]]
                                  (render/with-repo [git root work]
                                    (let [[c1 c2] (render/commit-history! git root two (render/monotone-instants 2))
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
    (render/with-repo [git root work]
      (let [[c0 c1] (render/commit-history! git root [m0 m1]
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
            [c2] (render/commit-history! git root [m2] ["2024-03-01T00:00:00Z"])
            s (audit/scan-history! {:aozora-repo (str root)
                                    :from-ref (.getName c0)
                                    :work-dir (str work)})]
        (testing "P11.pairing + P11.no-diff-invisible"
          (is (= [[(.getName c0) (.getName c1)]
                  [(.getName c1) (.getName c2)]]
                 (mapv (juxt :previous_ref :current_ref) (:pairs s)))))
        (testing "P11.drift-localization"
          (is (= [0 1] (mapv :split_candidates (:pairs s)))))))))

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
    (render/with-repo [git root work]
      (let [cs (render/commit-history! git root states instants)
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
    (render/with-repo [git root work]
      (let [cs (render/commit-history! git root [m0 s1 s2 s3] instants)
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
    (render/with-repo [git root work]
      (let [cs (render/commit-history! git root states instants)
            s (audit/scan-history! {:aozora-repo (str root)
                                    :from-ref (.getName (first cs))
                                    :sample-period "year"
                                    :work-dir (str work)})]
        (is (= "ok" (:status s)))
        ;; transient split between representatives is invisible: summary shows
        ;; only the persistent metadata correction, zero candidates.
        (is (= 0 (get (:summary s) "split_candidates")))
        (is (pos? (get (:summary s) "pairs_scanned")))))))

(defn- audit-two!
  "Commit clean state then a faulted current blob (bytes) and audit the pair.
  Returns {:result r} or {:threw ex}."
  [current-bytes]
  (let [m (model/bootstrap 2)]
    (render/with-repo [git root work]
      (let [clean (render/csv->zip-bytes (render/rows->csv (render/model->rows m)))
            c1 (render/commit-zip-at! git root clean "clean" "2024-01-01T00:00:00Z")
            c2 (render/commit-file-at! git root render/zip-path current-bytes
                                       "faulted" "2024-02-01T00:00:00Z")]
        (try {:result (audit/audit! {:aozora-repo (str root)
                                     :previous-ref (.getName c1)
                                     :current-ref (.getName c2)
                                     :work-dir (str work)})}
             (catch Exception e {:threw e}))))))

;; P13.ragged-row — D3: row-level fault must not abort; desired = work
;; skipped. Both directions of raggedness get a defined outcome.
(deftest p13-ragged-row-sim-test
  (doseq [corrupt [:ragged-short :ragged-long]]
    (let [m (model/bootstrap 2)
          rows (render/corrupt-rows (render/model->rows m)
                                    [{:corrupt/type corrupt :wid "000101"}])
          {:keys [result threw]} (audit-two!
                                  (render/csv->zip-bytes (render/rows->csv rows)))]
      (is (nil? threw) (str corrupt ": row-level fault must never abort an audit"))
      (div/expected-failure :D3 (str "P13.ragged-row/" (name corrupt))
        ;; DESIRED: the ragged work is skipped with a reason, not silently
        ;; ingested with truncated/dropped cells.
                            (boolean (some #{"000101"}
                                           (get-in result [:ingest :current :skipped-work-ids])))))))

;; P13.divergent-person — absorbed and counted (current behavior matches
;; spec). The fault must be two bodies for the SAME person id within one
;; work: duplicate person 000001's row, then diverge the duplicate's 姓.
(deftest p13-divergent-person-sim-test
  (let [m (model/bootstrap 2)
        rows (render/corrupt-rows (render/model->rows m)
                                  [{:corrupt/type :duplicate-row :wid "000101"
                                    :pid "000001"}
                                   {:corrupt/type :divergent-person :wid "000101"
                                    :pid "000001" :column "姓" :value "×"}])
        {:keys [result threw]} (audit-two!
                                (render/csv->zip-bytes (render/rows->csv rows)))]
    (is (nil? threw))
    (is (= ["000101"] (get-in result [:ingest :current :skipped-work-ids])))))

;; P13.empty-csv — D5 (desired: explicit ex-info with :zip-path).
(deftest p13-empty-csv-sim-test
  (doseq [[label csv] [["empty" ""]
                       ["header-only" (render/rows->csv [])]]]
    (let [{:keys [threw]} (audit-two! (render/csv->zip-bytes csv))]
      (is (not (harness/forbidden-throw? threw)) (str "P13.empty-csv/" label))
      (div/expected-failure :D5 (str "P13.empty-csv/" label)
                            (harness/clean-ex-info? threw [:zip-path])))))

;; P13.no-csv-entry — current behavior matches spec: ex-info {:zip-path}.
(deftest p13-no-csv-entry-sim-test
  (let [{:keys [threw]} (audit-two! (render/csv->zip-bytes "x" {:no-entry? true}))]
    (is (harness/clean-ex-info? threw [:zip-path])
        (str "expected ex-info with :zip-path, got: " (pr-str threw)))))

;; P13.non-zip-bytes — D6 fixed: wrapped ex-info, cause chained.
(deftest p13-non-zip-bytes-sim-test
  (let [{:keys [threw]} (audit-two! "this is not a zip file")]
    (is (some? threw) "non-ZIP bytes must not produce a normal-looking report")
    (is (not (harness/forbidden-throw? threw)) "P13.non-zip-bytes")
    (div/expected-failure :D6 "P13.non-zip-bytes"
                          (harness/clean-ex-info? threw [:zip-path]))))

;; P14.rerun — a reused --work-dir yields the same semantic report as a
;; fresh one.
(deftest p14-work-dir-hygiene-sim-test
  (let [m0 (model/bootstrap 2)
        m1 (:model (model/apply-event m0 {:event/type :edit-person :pid "000001"
                                          :field :family_name :value "改"}))]
    (render/with-repo [git root work]
      (let [[c1 c2] (render/commit-history! git root [m0 m1]
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
