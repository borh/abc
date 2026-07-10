(ns abc.tools.diagram.workflow-graph-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [clojure.java.shell :as shell]
            [abc.tools.json :as json]
            [abc.tools.diagram.workflow-graph :as wf]))

(deftest renders-example-run-deterministically
  (let [run (json/read-json-file "examples/workflow/passed.workflow-run.json")
        mmd (wf/run->mermaid run)]
    (is (= mmd (wf/run->mermaid run)))
    (is (str/starts-with? mmd "%% GENERATED"))
    (is (str/includes? mmd "source-snapshot"))
    (is (str/includes? mmd "classDef passed"))))

(def valid-two-step-run
  {"schema_id" "https://w3id.org/abc/schemas/workflow-run.schema.json"
   "schema_version" "soranoha-workflow-run-v1"
   "workflow_id" "diagram-test"
   "run_id" "diagram-test-1"
   "status" "passed"
   "started_at" "2026-07-10T00:00:00Z"
   "ended_at" "2026-07-10T00:00:02Z"
   "duration_ms" 2000
   "step_count" 2
   "steps_passed" 2
   "steps_failed" 0
   "steps"
   [{"id" "A" "status" "passed"
     "started_at" "2026-07-10T00:00:00Z"
     "ended_at" "2026-07-10T00:00:01Z"
     "duration_ms" 1000
     "requires" [] "produces" ["x"]
     "inputs" [] "outputs" [] "messages" []}
    {"id" "B" "status" "passed"
     "started_at" "2026-07-10T00:00:01Z"
     "ended_at" "2026-07-10T00:00:02Z"
     "duration_ms" 1000
     "requires" ["x"] "produces" []
     "inputs" [] "outputs" [] "messages" []}]})

(deftest builds-producer-consumer-edges
  (is (some (fn [edge]
              (and (= "A" (:from edge)) (= "B" (:to edge))))
            (:edges (wf/run->graph valid-two-step-run)))))

(deftest rejects-schema-invalid-workflow-before-rendering
  (let [run (assoc (json/read-json-file
                    "examples/workflow/passed.workflow-run.json")
                   "status" "mystery")]
    (try
      (wf/run->mermaid run)
      (is false "expected schema-invalid workflow to be rejected")
      (catch clojure.lang.ExceptionInfo ex
        (is (= :workflow-run-invalid (:kind (ex-data ex))))
        (is (seq (:errors (ex-data ex))))))))

(deftest rejects-semantically-invalid-workflow-before-rendering
  (let [run (assoc (json/read-json-file
                    "examples/workflow/passed.workflow-run.json")
                   "step_count" 999)]
    (try
      (wf/run->graph run)
      (is false "expected semantic-invalid workflow to be rejected")
      (catch clojure.lang.ExceptionInfo ex
        (is (some #(= ["step_count"] (:path %))
                  (:errors (ex-data ex))))))))

(deftest cli-without-path-prints-usage
  (let [{:keys [exit err]} (shell/sh "clojure" "-M:abc/workflow-graph")]
    (is (= 2 exit))
    (is (str/includes? err
                       "usage: clojure -M:abc/workflow-graph <workflow-run.json>"))))
