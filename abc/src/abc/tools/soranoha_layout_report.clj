(ns abc.tools.soranoha-layout-report
  (:require [abc.tools.files :as files]
            [babashka.fs :as fs]
            [clojure.java.io :as io]))

(defn- regular-files [root]
  (->> (files/sorted-path-seq root)
       (filter fs/regular-file?)
       (sort-by #(str (fs/relativize root %)))
       (map fs/file)))

(defn- actual-root-summary [root snapshot]
  (let [files (regular-files root)]
    {"file_count" (count files)
     "byte_count" (reduce + 0 (map #(.length %) files))
     "referenced_manifest_count" (count (get snapshot "artifact_references"
                                             []))}))

(defn- manifest-file-for-reference [root reference]
  (case (get-in reference ["locator" "kind"])
    "loose" (io/file root (get-in reference ["locator" "path"]))
    nil))

(defn- referenced-artifacts [root snapshot]
  (mapv (fn [reference]
          (let [manifest-file (manifest-file-for-reference root reference)
                manifest-value (when manifest-file
                                 (files/read-json manifest-file))]
            {"artifact_id" (get reference "artifact_id")
             "artifact_kind" (get reference "artifact_kind")
             "validation_status" (get reference "validation_status")
             "content_hash" (get reference "content_hash")
             "content_byte_length" (get-in manifest-value
                                           ["content" "byte_length"])
             "manifest_content_hash" (get reference "manifest_content_hash")
             "manifest_byte_length" (when manifest-file
                                      (.length (io/file manifest-file)))}))
        (get snapshot "artifact_references" [])))

(defn- ceil-div [n divisor]
  (if (zero? n)
    0
    (long (Math/ceil (/ (double n) divisor)))))

(defn- layout-estimate
  [{:keys [strategy-id artifacts loose-kinds batched-kinds batch-size]}]
  (let [loose-kinds (set loose-kinds)
        batched-kinds (set batched-kinds)
        loose-artifacts (filter #(contains? loose-kinds
                                            (get % "artifact_kind"))
                                artifacts)
        batched-artifacts (filter #(contains? batched-kinds
                                              (get % "artifact_kind"))
                                  artifacts)
        batched-by-kind (frequencies (map #(get % "artifact_kind")
                                          batched-artifacts))
        archive-count (reduce + 0
                              (map #(ceil-div % batch-size)
                                   (vals batched-by-kind)))]
    {"strategy_id" strategy-id
     "loose_artifact_kinds" (vec (sort loose-kinds))
     "batched_artifact_kinds" (vec (sort batched-kinds))
     "loose_content_file_count" (count loose-artifacts)
     "archive_count" archive-count
     "batched_member_count" (count batched-artifacts)
     "manifest_file_count" (count artifacts)
     "content_byte_count" (reduce + 0
                                  (keep #(get % "content_byte_length")
                                        artifacts))
     "manifest_byte_count" (reduce + 0
                                   (keep #(get % "manifest_byte_length")
                                         artifacts))}))

(defn- layout-strategy-estimates [snapshot artifacts]
  (let [kinds (sort (set (map #(get % "artifact_kind") artifacts)))
        layout-policy (get snapshot "layout_policy" {})
        configured-loose (set (get layout-policy "loose_artifact_kinds" []))
        configured-batched (set (get layout-policy "batched_artifact_kinds"
                                     []))
        batch-size (or (get layout-policy "batch_target_work_count") 250)]
    [(layout-estimate {:strategy-id "mixed-default-v1"
                       :artifacts artifacts
                       :loose-kinds configured-loose
                       :batched-kinds configured-batched
                       :batch-size batch-size})
     (layout-estimate {:strategy-id "all-loose-v1"
                       :artifacts artifacts
                       :loose-kinds kinds
                       :batched-kinds []
                       :batch-size batch-size})
     (layout-estimate {:strategy-id "all-batched-v1"
                       :artifacts artifacts
                       :loose-kinds []
                       :batched-kinds kinds
                       :batch-size batch-size})]))

(defn artifact-kind-counts [snapshot]
  (into (sorted-map)
        (frequencies (map #(get % "artifact_kind")
                          (get snapshot "artifact_references" [])))))

(defn build-report [root snapshot validation]
  (let [artifacts (referenced-artifacts root snapshot)
        identity-object (get snapshot "snapshot_index_identity_object")]
    {"schema_id" "https://w3id.org/abc/soranoha-layout-report-v0.json"
     "report_version" "0.2.0"
     "generated_at" (get snapshot "generated_at")
     "snapshot_date" (get snapshot "snapshot_date")
     "source_selection_hash" (get identity-object "source_selection_hash")
     "candidate_ref" (get identity-object "candidate_ref")
     "qualification_identity_ref" (get identity-object "qualification_identity_ref")
     "parser_config_hash" (get identity-object "parser_config_hash")
     "failure_set_hash" (get identity-object "failure_set_hash")
     "snapshot_identity_hash" (get snapshot "snapshot_identity_hash")
     "snapshot_summary" (get snapshot "summary")
     "layout_policy" (get snapshot "layout_policy")
     "artifact_kind_counts" (artifact-kind-counts snapshot)
     "actual_root" (actual-root-summary root snapshot)
     "strategy_estimates" (layout-strategy-estimates snapshot artifacts)
     "validation" validation
     "notes" "Static layout cost report. Archive counts are logical estimates from manifest references and configured batch size; compression ratios are measured only after archive materialization."}))
