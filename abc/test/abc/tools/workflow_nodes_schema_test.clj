(ns abc.tools.workflow-nodes-schema-test
  (:require [abc.tools.files :as files]
            [abc.tools.schema :as schema]
            [clojure.test :refer [deftest is testing]]))

(def nodes-schema-path
  "schemas/workflow-nodes.schema.json")

(def run-schema-path
  "schemas/workflow-run.schema.json")

(deftest workflow-nodes-example-fixture-validates-test
  (let [nodes-schema (files/read-json nodes-schema-path)
        lines (files/read-json-lines "examples/workflow/admission.workflow-nodes.jsonl")]
    (testing "every line of the example jsonl validates clean"
      (doseq [line lines]
        (is (nil? (schema/validation-errors nodes-schema line)))))))

(deftest workflow-nodes-invalid-fixture-fails-test
  (let [nodes-schema (files/read-json nodes-schema-path)
        lines (files/read-json-lines
               "fixtures/v0/invalid/workflow-nodes/invalid-node-type.workflow-nodes.jsonl")]
    (testing "invalid node_type fails validation"
      (doseq [line lines]
        (is (seq (schema/validation-errors nodes-schema line)))))))

(deftest workflow-run-node-summary-ref-test
  (let [run-schema (files/read-json run-schema-path)
        base-run (files/read-json "examples/workflow/passed.workflow-run.json")
        valid-ref {"schema_version" "soranoha-workflow-nodes-v1"
                   "path" "admission.workflow-nodes.jsonl"
                   "node_count" 2
                   "realized_count" 1
                   "skipped_count" 1}
        run-with-valid-ref (assoc-in base-run ["steps" 0 "node_summary_ref"] valid-ref)
        malformed-ref (dissoc valid-ref "path")
        run-with-malformed-ref (assoc-in base-run ["steps" 0 "node_summary_ref"] malformed-ref)]
    (testing "valid node_summary_ref on a step validates clean"
      (is (nil? (schema/validation-errors run-schema run-with-valid-ref))))
    (testing "malformed node_summary_ref (missing path) fails validation"
      (is (seq (schema/validation-errors run-schema run-with-malformed-ref))))))
