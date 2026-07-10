(ns abc.tools.workflow.report
  "Convert a `abc.tools.workflow.target/eval-target` result into node-summary
   JSONL records (schema: workflow-nodes.schema.json), write the sidecar, and
   produce the bounded `node_summary_ref` block a workflow-run step embeds.
   Also packages an `eval-target` call as a `run-workflow!` step. Pure aside
   from the sidecar write; does not require the runner namespace."
  (:require [abc.tools.json :as json]
            [abc.tools.workflow.target :as target]
            [clojure.java.io :as io]))

(def node-summary-schema-version
  "soranoha-workflow-nodes-v1")

(defn- id-str
  "Stringify an id that may arrive as a keyword (e.g. a step's :id) or a
   plain string."
  [id]
  (if (keyword? id) (name id) (str id)))

(defn node-record
  "Build the string-keyed node-summary record for one eval-target node summary.
   `ids` is {:run-id :workflow-id :step-id}; workflow-id and step-id are
   optional and only included when non-nil."
  [node {:keys [run-id workflow-id step-id]}]
  (cond-> {"schema_version" node-summary-schema-version
           "run_id" run-id
           "key" (name (:key node))
           "node_type" (:node_type node)
           "status" (:status node)
           "realized" (:realized node)
           "inputs" (mapv name (:inputs node))}
    (some? workflow-id) (assoc "workflow_id" (id-str workflow-id))
    (some? step-id) (assoc "step_id" (id-str step-id))
    (contains? node :conditional_inputs_skipped)
    (assoc "conditional_inputs_skipped" (mapv name (:conditional_inputs_skipped node)))
    (contains? node :duration_ms) (assoc "duration_ms" (:duration_ms node))
    (contains? node :cache) (assoc "cache" (:cache node))))

(defn node-records
  "Build node-summary records for every node in an eval-target result."
  [eval-result ids]
  (mapv #(node-record % ids) (:nodes eval-result)))

(defn- write-jsonl! [file records]
  (io/make-parents file)
  (with-open [writer (io/writer file)]
    (doseq [record records]
      (.write writer (json/write-deterministic-jsonl-line record))
      (.write writer "\n")))
  file)

(defn write-node-summaries!
  "Write one deterministic JSON object per line (trailing newline) to
   `(io/file output-root filename)`, one line per node in `eval-result`.
   Returns the bounded node_summary_ref block for embedding on a workflow-run
   step."
  [output-root filename eval-result ids]
  (let [records (node-records eval-result ids)]
    (write-jsonl! (io/file output-root filename) records)
    {"schema_version" node-summary-schema-version
     "path" filename
     "node_count" (count records)
     "realized_count" (count (filter #(true? (get % "realized")) records))
     "skipped_count" (count (filter #(= "skipped" (get % "status")) records))}))

(defn eval-target-step
  "Package an `eval-target` graph evaluation as a `run-workflow!` step. The
   returned step's `:run` evaluates `target` in `graph` using inputs from
   `inputs-fn` (applied to the workflow state), writes the node-summary
   sidecar under `output-root`/`nodes-filename`, and reports the target value
   under the first key in `produces`."
  [{:keys [id requires produces graph target inputs-fn output-root
           nodes-filename run-id workflow-id]}]
  {:id id
   :requires (vec requires)
   :produces (vec produces)
   :run (fn [state]
          (let [inputs ((or inputs-fn (constantly {})) state)
                res (target/eval-target graph target inputs)
                ref (write-node-summaries! output-root nodes-filename res
                                           {:run-id run-id
                                            :workflow-id workflow-id
                                            :step-id id})]
            {:state-updates {(first produces) (:value res)}
             :node-summary-ref ref
             :outputs [{:role "workflow-nodes" :path nodes-filename}]}))})
