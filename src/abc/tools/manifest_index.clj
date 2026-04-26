(ns abc.tools.manifest-index
  (:require [abc.tools.files :as files]))

(def successful-statuses #{"passed" "warning"})

(defn successful-entry? [entry]
  (contains? successful-statuses (get entry "validation_status")))

(defn manifest->index-entry [manifest-path manifest]
  {"manifest_path" (str manifest-path)
   "artifact_id" (get manifest "artifact_id")
   "artifact_kind" (get manifest "artifact_kind")
   "validation_status" (get manifest "validation_status")
   "content_hash" (get-in manifest ["content" "content_hash"])
   "media_type" (get-in manifest ["content" "media_type"])})

(defn index-entries [manifest-path->manifest]
  (->> manifest-path->manifest
       (map (fn [[path manifest]]
              (manifest->index-entry path manifest)))
       (sort-by (juxt #(get % "artifact_id")
                      #(get % "manifest_path")))
       vec))

(defn index-manifest-files [manifest-paths]
  (index-entries
   (into {}
         (map (fn [path]
                [(str path) (files/read-json path)]))
         manifest-paths)))

(defn reproducibility-conflicts [entries]
  (->> entries
       (filter successful-entry?)
       (filter #(get % "content_hash"))
       (group-by #(get % "artifact_id"))
       (keep (fn [[artifact-id grouped]]
               (let [content-hashes (->> grouped
                                         (map #(get % "content_hash"))
                                         distinct
                                         sort
                                         vec)]
                 (when (< 1 (count content-hashes))
                   {"artifact_id" artifact-id
                    "content_hashes" content-hashes
                    "manifest_paths" (->> grouped
                                          (map #(get % "manifest_path"))
                                          sort
                                          vec)}))))
       (sort-by #(get % "artifact_id"))
       vec))

(defn validate-no-reproducibility-conflicts! [entries]
  (let [conflicts (reproducibility-conflicts entries)]
    (when (seq conflicts)
      (throw (ex-info "Reproducibility conflict: artifact_id maps to multiple content hashes"
                      {:type :abc/reproducibility-conflict
                       :conflicts conflicts}))))
  true)
