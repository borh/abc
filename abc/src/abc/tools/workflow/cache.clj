(ns abc.tools.workflow.cache
  (:require [abc.tools.hash :as hash]
            [babashka.fs :as fs]))

(defn node-cache-key [{:keys [workflow-id target-key node-key node-kind
                              graph-version impl-id impl-hash input-value-hashes
                              path-content-hashes policy-hashes]}]
  (-> {"cache_version" "soranoha-cache-v1"
       "workflow_id" (str workflow-id)
       "target_key" (str target-key)
       "node_key" (str node-key)
       "node_kind" (str node-kind)
       "graph_version" (str graph-version)
       "impl_id" (str impl-id)
       "impl_hash" (str impl-hash)
       "input_value_hashes" (or input-value-hashes {})
       "path_content_hashes" (or path-content-hashes {})
       "policy_hashes" (or policy-hashes {})}
      hash/sha256-json-jcs
      hash/format-sha256))

(defn valid-cached-node-result [cached env]
  (cond
    (not (map? cached))
    {:status :invalid :reason "malformed cached entry" :evidence {}}

    (not (contains? cached :outputs))
    {:status :invalid :reason "malformed cached entry" :evidence {}}

    :else
    (let [outputs (:outputs cached)]
      (or
        ;; Check each output
       (some
        (fn [output]
          (cond
            (or (not (contains? output :path))
                (not (contains? output :content_hash)))
            {:status :invalid :reason "output missing path/content_hash" :evidence {}}

            :else
            (let [path (:path output)
                  recorded (:content_hash output)
                  f (fs/file (:base-dir env) path)]
              (cond
                (not (fs/exists? f))
                {:status :invalid :reason "missing output path" :evidence {:path path}}

                :else
                (let [actual (hash/format-sha256 (hash/sha256-file f))]
                  (when (not= actual recorded)
                    {:status :stale :reason "content hash mismatch"
                     :evidence {:path path :recorded recorded :actual actual}}))))))
        outputs)
        ;; Check config drift
       (when (and (contains? cached :config)
                  (not= (:config cached) (:config env)))
         {:status :stale :reason "identity-relevant config drift"
          :evidence {:cached (:config cached) :current (:config env)}})
        ;; All good
       {:status :ok}))))
