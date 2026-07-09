(ns abc.tools.workflow.target-test
  (:require [abc.tools.workflow.target :as target]
            [clojure.test :refer [deftest is testing]]))

(deftest validate-graph-accepts-valid-dag-test
  (let [g {:x (target/value-node)
           :a (target/leaf [:x] :a (fn [m] (inc (:x m))))}]
    (is (nil? (target/validate-graph g)))))

(deftest validate-graph-detects-missing-dependency-test
  (let [g {:a (target/leaf [:missing] :a (fn [_] 1))}]
    (is (= [{:type :missing-dependency :node :a :dep :missing}]
           (target/validate-graph g)))))

(deftest validate-graph-detects-cycle-test
  (let [g {:a (target/leaf [:b] :a (fn [m] (:b m)))
           :b (target/leaf [:a] :b (fn [m] (:a m)))}]
    (is (some #(= :cycle (:type %)) (target/validate-graph g)))))

(deftest validate-graph-no-false-positive-on-mutually-exclusive-branch-test
  ;; The hazard a naive heuristic cycle check would flag: two sides of a branch.
  (let [g {:c (target/value-node)
           :t (target/leaf [] :t (constantly 1))
           :e (target/leaf [] :e (constantly 2))
           :d (target/branch :c :t :e)}]
    (is (nil? (target/validate-graph g)))))
