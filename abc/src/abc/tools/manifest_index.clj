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
   "media_type" (get-in manifest ["content" "media_type"])
   "manifest_identity_object" (get manifest "manifest_identity_object")
   "provenance_used" (get-in manifest ["provenance" "used"] [])
   "provenance_was_derived_from" (get-in manifest ["provenance" "was_derived_from"] [])})

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

(defn tokenized-release-guardrail-errors [entries]
  (->> entries
       (filter successful-entry?)
       (filter #(= "tokenized" (get % "artifact_kind")))
       (map (fn [entry]
              {:artifact_id (get entry "artifact_id")
               :manifest_path (get entry "manifest_path")
               :validation_status (get entry "validation_status")}))
       (sort-by (juxt :artifact_id :manifest_path))
       vec))

(defn validate-tokenized-release-guardrail! [entries]
  (let [errors (tokenized-release-guardrail-errors entries)]
    (when (seq errors)
      (throw (ex-info "Successful tokenized manifests require tokenizer_profile_hash in manifest identity schema"
                      {:type :abc/tokenized-release-before-profile-hash
                       :errors errors}))))
  true)

(def parser-ir-copied-fields
  ["parser_build_hash"
   "parser_config_hash"
   "aat_parser_ir_mapping_hash"
   "parser_ir_schema_hash"])

(defn- identity-value [entry field]
  (get-in entry ["manifest_identity_object" field]))

(defn- successful-parser-ir-entry? [entry]
  (and (successful-entry? entry)
       (= "parser-ir" (get entry "artifact_kind"))))

(defn parser-ir-producer-candidates [entries coordinate]
  (->> entries
       (filter successful-parser-ir-entry?)
       (filter (fn [entry]
                 (every? (fn [[field expected]]
                           (= expected (identity-value entry field)))
                         coordinate)))
       (sort-by #(get % "artifact_id"))
       vec))

(defn- entry-by-artifact-id [entries]
  (into {}
        (map (fn [entry] [(get entry "artifact_id") entry]))
        entries))

(defn- find-producer-entry [by-artifact-id analysis-entry]
  (some (fn [artifact-id]
          (let [entry (get by-artifact-id artifact-id)]
            (when (successful-parser-ir-entry? entry)
              entry)))
        (concat (get analysis-entry "provenance_was_derived_from")
                (get analysis-entry "provenance_used"))))

(defn- copied-field-errors [analysis-entry producer-entry]
  (->> parser-ir-copied-fields
       (keep (fn [field]
               (let [analysis-value (identity-value analysis-entry field)
                     producer-value (identity-value producer-entry field)]
                 (when-not (= analysis-value producer-value)
                   {:analysis_artifact_id (get analysis-entry "artifact_id")
                    :producer_artifact_id (get producer-entry "artifact_id")
                    :field field
                    :analysis_value analysis-value
                    :producer_value producer-value}))))
       vec))

(defn analysis-copied-field-errors [entries]
  (let [by-artifact-id (entry-by-artifact-id entries)]
    (->> entries
         (filter successful-entry?)
         (filter #(= "analysis" (get % "artifact_kind")))
         (mapcat (fn [analysis-entry]
                   (if-let [producer-entry (find-producer-entry by-artifact-id analysis-entry)]
                     (copied-field-errors analysis-entry producer-entry)
                     [{:analysis_artifact_id (get analysis-entry "artifact_id")
                       :producer_artifact_id nil
                       :field "__producer__"
                       :analysis_value (vec (concat (get analysis-entry "provenance_was_derived_from")
                                                    (get analysis-entry "provenance_used")))
                       :producer_value nil}])))
         vec)))

(defn validate-analysis-copied-fields! [entries]
  (let [errors (analysis-copied-field-errors entries)]
    (when (seq errors)
      (throw (ex-info "Analysis manifest copied identity fields differ from producer"
                      {:type :abc/analysis-copied-field-conflict
                       :errors errors}))))
  true)
