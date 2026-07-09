(ns abc.tools.workflow.report-test
  (:require [abc.tools.files :as files]
            [abc.tools.schema :as schema]
            [abc.tools.workflow.report :as report]
            [abc.tools.workflow.target :as target]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir [prefix]
  (.toFile (Files/createTempDirectory
            prefix
            (make-array FileAttribute 0))))

(def nodes-schema
  (files/read-json "schemas/workflow-nodes.schema.json"))

(def run-schema
  (files/read-json "schemas/workflow-run.schema.json"))

(deftest node-record-leaf-test
  (let [node {:key :admission-decision
              :node_type "leaf"
              :status "passed"
              :realized true
              :inputs [:request-set :snapshot-index]}
        ids {:run-id "r" :workflow-id "wf" :step-id "admission"}
        record (report/node-record node ids)]
    (is (= {"schema_version" "soranoha-workflow-nodes-v1"
            "run_id" "r"
            "workflow_id" "wf"
            "step_id" "admission"
            "key" "admission-decision"
            "node_type" "leaf"
            "status" "passed"
            "realized" true
            "inputs" ["request-set" "snapshot-index"]}
           record))
    (is (nil? (schema/validation-errors nodes-schema record)))))

(deftest node-record-skipped-test
  (let [node {:key :tei-report
              :node_type "leaf"
              :status "skipped"
              :realized false
              :inputs []}
        record (report/node-record node {:run-id "r"})]
    (is (= "skipped" (get record "status")))
    (is (false? (get record "realized")))
    (is (nil? (schema/validation-errors nodes-schema record)))))

(deftest node-record-branch-conditional-inputs-skipped-test
  (let [node {:key :d
              :node_type "branch"
              :status "passed"
              :realized true
              :inputs [:c :t]
              :conditional_inputs_skipped [:e]}
        record (report/node-record node {:run-id "r"})]
    (is (= ["e"] (get record "conditional_inputs_skipped")))
    (is (nil? (schema/validation-errors nodes-schema record)))))

(def g
  {:c (target/value-node)
   :t (target/leaf [] :t (constantly :then))
   :e (target/leaf [] :e (constantly :else))
   :d (target/branch :c :t :e)})

(deftest write-node-summaries-writes-valid-sidecar-and-ref-test
  (let [dir (temp-dir "abc-workflow-report")
        res (target/eval-target g :d {:c true})
        ref (report/write-node-summaries! dir "workflow-nodes.jsonl" res
                                          {:run-id "r" :workflow-id "wf" :step-id "d"})
        lines (files/read-json-lines (io/file dir "workflow-nodes.jsonl"))]
    (testing "every line is schema-valid"
      (is (seq lines))
      (doseq [line lines]
        (is (nil? (schema/validation-errors nodes-schema line)))))
    (testing "skipped node present with expected status/realized"
      (let [e-line (first (filter #(= "e" (get % "key")) lines))]
        (is (some? e-line))
        (is (= "skipped" (get e-line "status")))
        (is (false? (get e-line "realized")))))
    (testing "ref block counts match"
      (let [node-count (count (:nodes res))
            realized-count (count (filter :realized (:nodes res)))
            skipped-count (count (filter #(= "skipped" (:status %)) (:nodes res)))]
        (is (= {"schema_version" "soranoha-workflow-nodes-v1"
                "path" "workflow-nodes.jsonl"
                "node_count" node-count
                "realized_count" realized-count
                "skipped_count" skipped-count}
               ref))))
    (testing "ref block is accepted as a step's node_summary_ref"
      (let [run (files/read-json "examples/workflow/passed.workflow-run.json")
            modified (assoc-in run ["steps" 0 "node_summary_ref"] ref)]
        (is (nil? (schema/validation-errors run-schema modified)))))))

(deftest eval-target-step-end-to-end-test
  (let [dir (temp-dir "abc-workflow-report-step")
        step (report/eval-target-step
              {:id :admission
               :requires []
               :produces [:decision]
               :graph g
               :target :d
               :inputs-fn (constantly {:c true})
               :output-root dir
               :nodes-filename "workflow-nodes.jsonl"
               :run-id "r"
               :workflow-id "wf"})]
    (is (= :admission (:id step)))
    (is (= [] (:requires step)))
    (is (= [:decision] (:produces step)))
    (let [result ((:run step) {})
          lines (files/read-json-lines (io/file dir "workflow-nodes.jsonl"))]
      (is (= {:decision :then} (:state-updates result)))
      (is (some? (:node-summary-ref result)))
      (is (= "workflow-nodes.jsonl" (get (:node-summary-ref result) "path")))
      (is (.isFile (io/file dir "workflow-nodes.jsonl")))
      (is (seq lines))
      (doseq [line lines]
        (is (nil? (schema/validation-errors nodes-schema line)))))))
