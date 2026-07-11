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
;; Confusable endpoints are excluded by the normative predicate, but the
;; exclusion rate is measured: ≥ 90% of cases must actually be checked, so
;; a generator-distribution change cannot silently make P1 vacuous.
(deftest p1-benign-quiet-sim-test
  (let [c (harness/ratio-counter)]
    (harness/check! "P1.benign-quiet" 50
                    (prop/for-all [hist (sgen/benign-history-gen {:length [5 12] :works [5 12]})]
                                  (let [{:keys [states]} (model/fold-history hist)
                                        prev (first states) cur (peek states)]
                                    (if-not (harness/tick! c (not (oracle/confusable? prev cur)))
                                      true
                                      (= [0 0] (candidate-counts (report-for-window prev cur)))))))
    (harness/assert-applied-ratio! "P1.benign-quiet non-confusable rate" c)))

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

;; P2 — completeness: the window is exactly the forced-event pair, so the
;; report's candidate set must EQUAL the injected candidates (a classifier
;; that adds false positives fails), and the opposite candidate list must
;; be empty.
(defn- completeness-prop [forced expected-fn candidates-key other-key counter]
  (prop/for-all [hist (sgen/history-gen {:length [4 8] :works [4 8] :forced forced})]
                (let [w (forced-window hist forced)]
                  (if-not (harness/tick! counter (some? w))
                    true ;; no-op after shrink: totality only
                    (let [r (report-for-window (:prev w) (:cur w))]
                      (and (= (set (expected-fn (:intent w)))
                              (set (get r candidates-key)))
                           (empty? (get r other-key))))))))

(deftest p2-clean-split-sim-test
  (let [c (harness/ratio-counter)]
    (harness/check! "P2.clean-split" 50
                    (completeness-prop :clean-split oracle/expected-split-candidates
                                       "split_candidates" "merge_candidates" c))
    (harness/assert-applied-ratio! "P2.clean-split" c)))

(deftest p2-clean-merge-sim-test
  (let [c (harness/ratio-counter)]
    (harness/check! "P2.clean-merge" 50
                    (completeness-prop :clean-merge oracle/expected-merge-candidates
                                       "merge_candidates" "split_candidates" c))
    (harness/assert-applied-ratio! "P2.clean-merge" c)))

;; P3 — conservatism is positive classification, not mere absence: the
;; rewritten edges must appear in ambiguous_replacements with the exact
;; endpoint pid sets (a classifier that silently drops them fails), no
;; candidates, and partial-split's source must not be globally removed.
(defn- conservatism-prop [forced counter]
  (prop/for-all [hist (sgen/history-gen {:length [4 8] :works [4 8] :forced forced})]
                (let [w (forced-window hist forced)]
                  (if-not (harness/tick! counter (some? w))
                    true
                    (let [r (report-for-window (:prev w) (:cur w))
                          expected (set (oracle/expected-replacements
                                         (:prev w) (:cur w) (:intent w)))]
                      (and (= [0 0] (candidate-counts r))
                           (= expected (set (get r "ambiguous_replacements")))
                           (or (not= forced :partial-split)
                               (not-any? #{(-> w :intent :event :pid)}
                                         (get r "removed_person_ids")))))))))

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
