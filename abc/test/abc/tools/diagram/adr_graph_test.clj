(ns abc.tools.diagram.adr-graph-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [abc.tools.diagram.adr-graph :as adr]))

(deftest parses-header-fields
  (let [by-num (into {} (map (juxt :num identity)) (adr/parse-all "docs/adr"))]
    (is (= "Accepted" (:status (by-num 1))))
    (is (= [10 23 27] (sort (:amended-by (by-num 1)))))
    (is (= [7 12 23 24] (sort (:depends-on (by-num 25)))))
    (is (= [] (:supersedes (by-num 1))))))

(deftest header-lint-clean-on-current-set
  (is (= [] (adr/lint*))))

(deftest lint-catches-missing-reciprocal
  (let [adrs [{:num 1 :title "A" :status "Accepted" :supersedes [] :amends [] :amended-by [] :depends-on []}
              {:num 2 :title "B" :status "Accepted" :supersedes [] :amends [1] :amended-by [] :depends-on []}]]
    (is (some #(str/includes? % "0001") (adr/lint-adrs adrs [])))))

(deftest lint-catches-dangling-reference
  (let [adrs [{:num 1 :title "A" :status "Accepted" :supersedes [] :amends [] :amended-by [] :depends-on [99]}]]
    (is (some #(str/includes? % "0099") (adr/lint-adrs adrs [])))))

(deftest graph-from-builds-header-edges
  (let [adrs [{:num 1 :title "A" :status "Accepted" :supersedes [] :amends [] :amended-by [2] :depends-on []}
              {:num 2 :title "B" :status "Accepted" :supersedes [] :amends [1] :amended-by [] :depends-on []}]
        g (adr/graph-from adrs [])]
    (is (= "LR" (:direction g)))
    (is (some #(and (= "ADR0002" (:from %)) (= "ADR0001" (:to %)) (= "amends" (:label %)))
              (:edges g)))))

(deftest build-is-deterministic-flowchart-input
  (let [g (adr/build)]
    (is (= "LR" (:direction g)))
    (is (= (adr/build) g))
    (is (seq (:nodes g)))))
