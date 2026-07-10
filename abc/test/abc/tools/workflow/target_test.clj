(ns abc.tools.workflow.target-test
  (:require [abc.tools.workflow.target :as target]
            [clojure.test :refer [deftest is]]))

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

(deftest eval-realizes-leaf-from-value-node-test
  (let [g {:x (target/value-node)
           :a (target/leaf [:x] :a (fn [m] (* 10 (:x m))))}
        {:keys [value nodes edges]} (target/eval-target g :a {:x 5})]
    (is (= 50 value))
    (is (contains? (set edges) [:a :x]))
    (let [a (first (filter #(= :a (:key %)) nodes))]
      (is (= "leaf" (:node_type a)))
      (is (= "passed" (:status a)))
      (is (true? (:realized a)))
      (is (= [:x] (:inputs a))))))

(deftest eval-throws-on-unknown-target-test
  (is (thrown? clojure.lang.ExceptionInfo
               (target/eval-target {:a (target/value-node)} :missing {}))))

(deftest eval-throws-on-invalid-graph-test
  (is (thrown? clojure.lang.ExceptionInfo
               (target/eval-target {:a (target/leaf [:gone] :a (fn [_] 1))} :a {}))))

(deftest branch-does-not-realize-skipped-dependency-test
  (let [calls (atom #{})
        g {:c (target/value-node)
           :t (target/leaf [] :t (fn [_] (swap! calls conj :t) :then-val))
           :e (target/leaf [] :e (fn [_] (swap! calls conj :e) :else-val))
           :d (target/branch :c :t :e)}
        {:keys [value nodes]} (target/eval-target g :d {:c true})]
    (is (= :then-val value))
    (is (= #{:t} @calls) "the else leaf must not be realized")
    (let [e (first (filter #(= :e (:key %)) nodes))]
      (is (= "skipped" (:status e)))
      (is (false? (:realized e))))
    (let [d (first (filter #(= :d (:key %)) nodes))]
      (is (= [:e] (:conditional_inputs_skipped d))))))

(deftest failure-as-value-becomes-partial-node-test
  (let [g {:a (target/leaf [] :a (fn [_] (target/failed-value {:rejected "policy"})))}
        {:keys [value nodes]} (target/eval-target g :a {})]
    (is (= {:rejected "policy"} value))
    (is (= "partial" (:status (first (filter #(= :a (:key %)) nodes)))))))
