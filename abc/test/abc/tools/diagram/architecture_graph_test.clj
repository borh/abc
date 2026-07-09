(ns abc.tools.diagram.architecture-graph-test
  (:require [clojure.test :refer [deftest is]]
            [abc.tools.diagram.architecture-graph :as arch]))

(deftest schemas-and-adrs-and-inputs-resolve
  (is (= [] (arch/lint*))))

(deftest build-is-deterministic-flowchart-input
  (let [g (arch/build)]
    (is (= "TD" (:direction g)))
    (is (= (arch/build) g))
    (is (seq (:nodes g)))))
