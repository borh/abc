(ns abc.tools.workflow.query-test
  (:require [abc.tools.workflow.query :as query]
            [abc.tools.files :as files]
            [clojure.test :refer [deftest is]]))

(def records (files/read-json-lines "examples/workflow/admission.workflow-nodes.jsonl"))

(deftest realization-summary-counts-test
  (is (= {:node_count 2 :realized_count 1 :skipped_count 1}
         (query/realization-summary records))))

(deftest explain-realization-partitions-and-explains-test
  (let [{:keys [realized skipped why]} (query/explain-realization records)]
    (is (= ["admission-decision"] realized))
    (is (= ["tei-validation-report"] skipped))
    ;; the reason a node realized = the inputs it depended on
    (is (= {:status "passed" :inputs ["request-set" "snapshot-index"]}
           (get why "admission-decision")))
    (is (nil? (get why "tei-validation-report")))))

(deftest read-run-nodes-reads-jsonl-test
  ;; convenience reader returns the same records
  (is (= records (query/read-run-nodes "examples/workflow/admission.workflow-nodes.jsonl"))))
