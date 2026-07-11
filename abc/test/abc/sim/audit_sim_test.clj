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
