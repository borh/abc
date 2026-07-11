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
