(ns abc.tools.diagram.workflow-graph-evidence-test
  (:require [abc.tools.diagram.workflow-graph :as workflow-graph]
            [abc.tools.files :as files]
            [clojure.test :refer [deftest is]]))

(deftest workflow-graph-fixtures-contract
  (let [run (files/read-json "examples/workflow/passed.workflow-run.json")
        graph (workflow-graph/run->graph run)]
    (is (empty? (workflow-graph/validation-problems run)))
    (is (= graph (workflow-graph/run->graph run)))
    (is (seq (:nodes graph)))
    (is (every? #(= "passed" (:class %)) (:nodes graph)))))
