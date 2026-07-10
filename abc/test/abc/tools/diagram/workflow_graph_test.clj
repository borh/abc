(ns abc.tools.diagram.workflow-graph-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [abc.tools.json :as json]
            [abc.tools.diagram.workflow-graph :as wf]))

(deftest renders-example-run-deterministically
  (let [run (json/read-json-file "examples/workflow/passed.workflow-run.json")
        mmd (wf/run->mermaid run)]
    (is (= mmd (wf/run->mermaid run)))
    (is (str/starts-with? mmd "%% GENERATED"))
    (is (str/includes? mmd "source-snapshot"))
    (is (str/includes? mmd "classDef passed"))))

(deftest builds-producer-consumer-edges
  (let [run {"steps" [{"id" "A" "status" "passed" "requires" [] "produces" ["x"] "inputs" [] "outputs" []}
                      {"id" "B" "status" "passed" "requires" ["x"] "produces" [] "inputs" [] "outputs" []}]}
        {:keys [edges]} (wf/run->graph run)]
    (is (some (fn [e] (and (= "A" (:from e)) (= "B" (:to e)))) edges))))
