(ns abc.tools.workflow.query
  "Derived queries over workflow-nodes.jsonl node summaries.
   Pure functions answering 'why did this target realize these nodes?'"
  (:require [abc.tools.files :as files]))

(defn read-run-nodes
  "Read workflow nodes from a JSONL file."
  [path]
  (files/read-json-lines path))

(defn realization-summary
  "Summarize realization counts across node records."
  [records]
  {:node_count (count records)
   :realized_count (count (filter #(get % "realized") records))
   :skipped_count (count (filter #(= "skipped" (get % "status")) records))})

(defn explain-realization
  "Partition realized and skipped nodes, with per-realized-node realization reasons."
  [records]
  {:realized (->> records (filter #(get % "realized")) (mapv #(get % "key")))
   :skipped  (->> records (filter #(= "skipped" (get % "status")))
                  (mapv #(get % "key")))
   :why (into {} (for [r records :when (get r "realized")]
                   [(get r "key") {:status (get r "status")
                                   :inputs (vec (get r "inputs" []))}]))})
