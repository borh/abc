(ns abc.tools.workflow.report-integration-test
  "End-to-end: an eval-target-step run through run-workflow! writes the
   workflow-nodes.jsonl sidecar and a schema-valid workflow-run.json whose
   step carries node_summary_ref."
  (:require [abc.test-fs :as tfs]
            [abc.tools.files :as files]
            [abc.tools.schema :as schema]
            [abc.tools.workflow :as workflow]
            [abc.tools.workflow.report :as report]
            [abc.tools.workflow.target :as target]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(deftest eval-target-step-through-run-workflow-records-node-summary-ref-test
  (tfs/with-temp-dir [out]
    (let [g {:c (target/value-node)
             :t (target/leaf [] :t (constantly :then))
             :e (target/leaf [] :e (constantly :else))
             :d (target/branch :c :t :e)}
          step (report/eval-target-step
                {:id :admission :requires [] :produces [:decision]
                 :graph g :target :d :inputs-fn (constantly {:c true})
                 :output-root out :nodes-filename "workflow-nodes.jsonl"
                 :run-id "run-1" :workflow-id "wf-1"})
          _ (workflow/run-workflow!
             {:workflow-id "wf-1" :run-id "run-1" :output-root out
              :initial-state {} :steps [step]})
          run-schema   (files/read-json "schemas/workflow-run.schema.json")
          nodes-schema (files/read-json "schemas/workflow-nodes.schema.json")
          written (files/read-json (io/file out "workflow-run.json"))
          lines   (files/read-json-lines (io/file out "workflow-nodes.jsonl"))]
      (testing "workflow-run.json is schema-valid and carries node_summary_ref"
        (is (nil? (schema/validation-errors run-schema written)))
        (is (= "workflow-nodes.jsonl"
               (get-in written ["steps" 0 "node_summary_ref" "path"])))
        (is (= 1 (get-in written ["steps" 0 "node_summary_ref" "skipped_count"])))
        (is (= 4 (get-in written ["steps" 0 "node_summary_ref" "node_count"]))))
      (testing "sidecar lines are schema-valid and the skipped branch side is recorded"
        (doseq [line lines]
          (is (nil? (schema/validation-errors nodes-schema line))))
        (is (some #(and (= "e" (get % "key")) (= "skipped" (get % "status"))) lines))))))
