(ns abc.tools.diagram.adr-graph-test
  (:require [abc.tools.diagram.adr-graph :as adr]
            [clojure.test :refer [deftest is]]))

(def ^:private corpus
  {:decisions
   [{:slug "a-decision" :title "A" :status :superseded
     :date "2026-06-01" :topics [] :relations [] :claims []}
    {:slug "b-decision" :title "B" :status :accepted
     :date "2026-07-01" :accepted "2026-07-01"
     :validation-scope :structural :release-authority :none
     :topics []
     :relations [{:class :lifecycle :type :amends :to "a-decision"
                  :scope "governance source"}
                 {:class :lifecycle :type :supersedes :to "a-decision"}
                 {:class :annotation :type :restates-hard-rule
                  :to "a-decision"}]
     :claims [{:id :c1 :kind :k :statement "s" :evidence ["test/x"]}]}]})

(deftest graph-from-builds-lifecycle-and-annotation-edges
  (let [g (adr/graph-from corpus)]
    (is (= "LR" (:direction g)))
    (is (some #(and (= "b-decision" (:from %))
                    (= "a-decision" (:to %))
                    (= :solid (:style %))
                    (= "amends — governance source" (:label %)))
              (:edges g)))
    (is (some #(and (= :thick (:style %)) (= "supersedes" (:label %)))
              (:edges g)))
    (is (some #(and (= :dashed (:style %))
                    (= "restates hard rule" (:label %)))
              (:edges g))
        "annotation edge labels derive from the open type keyword")))

(deftest graph-nodes-carry-slug-title-and-status-class
  (let [g (adr/graph-from corpus)]
    (is (= #{{:id "a-decision" :label "A" :class "superseded"}
             {:id "b-decision" :label "B" :class "accepted"}}
           (set (:nodes g))))))

(deftest lint-is-clean-on-current-corpus
  (is (= [] (adr/lint*))))

(deftest build-is-deterministic-flowchart-input
  (let [g (adr/build)]
    (is (= "LR" (:direction g)))
    (is (= (adr/build) g))
    (is (seq (:nodes g)))
    (is (some #(= "restates hard rule" (:label %)) (:edges g))
        "committed corpus still renders its annotation edges")
    (is (some #(= "depends on — source_span_coverage gate" (:label %))
              (:edges g))
        "scoped dependency labels survive the migration")))
