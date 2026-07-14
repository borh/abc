(ns abc.tools.diagram.workflow-graph-evidence-test
  (:require [abc.tools.adr-evidence-runtime-inputs :as runtime-inputs]
            [abc.tools.diagram.workflow-graph :as workflow-graph]
            [abc.tools.files :as files]
            [clojure.test :refer [deftest is]]))

(deftest workflow-graph-fixtures-contract
  (runtime-inputs/with-validated-read-trace!
    {:identity-root "." :cwd-root "." :repo-root "." :workspace-root ".."
     :descriptor {:path "docs/evidence/adr-capture/workflow-graph-fixtures.edn"
                  :value (files/read-edn "docs/evidence/adr-capture/workflow-graph-fixtures.edn")}}
    (fn []
      (let [run (files/read-json "examples/workflow/passed.workflow-run.json")
            graph (workflow-graph/run->graph run)]
        (is (empty? (workflow-graph/validation-problems run)))
        (is (= graph (workflow-graph/run->graph run)))
        (is (seq (:nodes graph)))
        (is (every? #(= "passed" (:class %)) (:nodes graph)))))))
