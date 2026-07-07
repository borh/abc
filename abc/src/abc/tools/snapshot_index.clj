(ns abc.tools.snapshot-index
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.manifest :as manifest]
            [clojure.java.io :as io]))

(def snapshot-index-schema-id
  "https://w3id.org/abc/schemas/snapshot-index.schema.json")

(def snapshot-label-pattern
  #"^soranoha-snapshot-\d{4}-\d{2}-\d{2}-\d{2}$")

(def snapshot-plans-dir
  "data/snapshot-plans")

(def identity-array-fields
  ["schema_hashes"
   "parser_evidence_hashes"
   "tokenizer_profile_hashes"
   "analysis_recipe_hashes"])

(def identity-hash-fields
  ["snapshot_index_schema_hash"
   "request_set_id"
   "source_snapshot_hash"
   "manifest_index_hash"
   "artifact_set_hash"
   "failure_policy_hash"
   "layout_policy_hash"])

(def successful-statuses #{"passed" "warning"})

(defn snapshot-label? [value]
  (boolean (and (string? value)
                (re-matches snapshot-label-pattern value))))

(defn hash-json-value [value]
  (hash/format-sha256 (hash/sha256-json-jcs value)))

(def policy-hash hash-json-value)

(defn- require-hash! [_label value]
  (hash/parse-sha256 value)
  value)

(defn- require-hash-or-json-hash! [label value]
  (cond
    (and (string? value) (re-matches hash/hash-pattern value))
    value

    (nil? value)
    (throw (ex-info (str label " is required")
                    {:label label}))

    :else
    (policy-hash value)))

(defn- canonical-hashes [label values]
  (when-not (vector? values)
    (throw (ex-info "snapshot_index_identity_object array fields must use empty arrays, not null"
                    {:field label
                     :value values})))
  (->> values
       (map #(require-hash! label %))
       sort
       distinct
       vec))

(defn- artifact-reference-identity [reference]
  {"artifact_id" (require-hash! "artifact_id" (get reference "artifact_id"))
   "artifact_kind" (get reference "artifact_kind")
   "sidecar_role" (get reference "sidecar_role")
   "validation_status" (get reference "validation_status")
   "manifest_content_hash" (require-hash! "manifest_content_hash"
                                          (get reference "manifest_content_hash"))
   "content_hash" (when-let [content-hash (get reference "content_hash")]
                    (require-hash! "content_hash" content-hash))})

(defn- artifact-sort-key [reference]
  [(get reference "artifact_id")
   (get reference "artifact_kind")
   (or (get reference "sidecar_role") "")])

(defn canonical-artifact-references [references]
  (->> references
       (map artifact-reference-identity)
       (sort-by artifact-sort-key)
       distinct
       vec))

(defn sort-artifact-references [references]
  (->> references
       (sort-by artifact-sort-key)
       distinct
       vec))

(defn artifact-set-hash [artifact-references]
  (hash-json-value (canonical-artifact-references artifact-references)))

(defn artifact-reference-from-manifest-path
  ([manifest-path locator]
   (artifact-reference-from-manifest-path manifest-path locator nil))
  ([manifest-path locator sidecar-role]
   (let [manifest-value (files/read-json manifest-path)]
     {"artifact_id" (get manifest-value "artifact_id")
      "artifact_kind" (get manifest-value "artifact_kind")
      "sidecar_role" sidecar-role
      "validation_status" (get manifest-value "validation_status")
      "manifest_content_hash" (manifest/file-hash manifest-path)
      "content_hash" (get-in manifest-value ["content" "content_hash"])
      "locator" locator})))

(defn- artifact-reference-from-manifest-reference
  [{:keys [manifest-path locator sidecar-role]}]
  (artifact-reference-from-manifest-path manifest-path locator sidecar-role))

(defn snapshot-summary [artifact-references]
  (let [total-artifacts (count artifact-references)
        success-count (count (filter #(contains? successful-statuses
                                                 (get % "validation_status"))
                                     artifact-references))
        failure-count (- total-artifacts success-count)]
    {"total_artifacts" total-artifacts
     "success_count" success-count
     "failure_count" failure-count
     "failure_rate" (if (zero? total-artifacts)
                      0.0
                      (double (/ failure-count total-artifacts)))}))

(defn snapshot-index-identity-object
  [{:keys [snapshot-index-schema-hash request-set-id source-snapshot-hash
           manifest-index-hash artifact-references
           failure-policy failure-policy-hash layout-policy layout-policy-hash
           schema-hashes parser-evidence-hashes tokenizer-profile-hashes
           analysis-recipe-hashes]
    declared-artifact-set-hash :artifact-set-hash}]
  {"snapshot_index_schema_hash" (require-hash! "snapshot_index_schema_hash"
                                               snapshot-index-schema-hash)
   "request_set_id" (require-hash! "request_set_id" request-set-id)
   "source_snapshot_hash" (require-hash! "source_snapshot_hash"
                                         source-snapshot-hash)
   "manifest_index_hash" (require-hash! "manifest_index_hash"
                                        manifest-index-hash)
   "artifact_set_hash" (or declared-artifact-set-hash
                           (artifact-set-hash artifact-references))
   "failure_policy_hash" (or failure-policy-hash
                             (require-hash-or-json-hash! "failure_policy"
                                                         failure-policy))
   "layout_policy_hash" (or layout-policy-hash
                            (require-hash-or-json-hash! "layout_policy"
                                                        layout-policy))
   "schema_hashes" (canonical-hashes "schema_hashes" schema-hashes)
   "parser_evidence_hashes" (canonical-hashes "parser_evidence_hashes"
                                              parser-evidence-hashes)
   "tokenizer_profile_hashes" (canonical-hashes "tokenizer_profile_hashes"
                                                tokenizer-profile-hashes)
   "analysis_recipe_hashes" (canonical-hashes "analysis_recipe_hashes"
                                              analysis-recipe-hashes)})

(defn build-snapshot-index
  [{:keys [snapshot-label request-set-label request-set generated-at
           manifest-index-hash failure-policy failure-policy-hash
           layout-policy layout-policy-hash schema-hashes parser-evidence-hashes
           tokenizer-profile-hashes analysis-recipe-hashes artifact-references
           manifest-references]}]
  (let [snapshot-index-schema-hash (manifest/schema-hash
                                    "schemas/snapshot-index.schema.json")
        request-set-identity (get request-set "request_set_identity_object")
        artifact-references (sort-artifact-references
                             (vec (concat artifact-references
                                          (map artifact-reference-from-manifest-reference
                                               manifest-references))))
        identity-object (snapshot-index-identity-object
                         {:snapshot-index-schema-hash snapshot-index-schema-hash
                          :request-set-id (get request-set "request_set_id")
                          :source-snapshot-hash (get request-set-identity
                                                     "corpus_snapshot_hash")
                          :manifest-index-hash manifest-index-hash
                          :artifact-references artifact-references
                          :failure-policy failure-policy
                          :failure-policy-hash failure-policy-hash
                          :layout-policy layout-policy
                          :layout-policy-hash layout-policy-hash
                          :schema-hashes schema-hashes
                          :parser-evidence-hashes parser-evidence-hashes
                          :tokenizer-profile-hashes (or tokenizer-profile-hashes
                                                        (get request-set-identity
                                                             "tokenizer_profile_hashes"))
                          :analysis-recipe-hashes (or analysis-recipe-hashes
                                                      (get request-set-identity
                                                           "analysis_recipe_hashes"))})]
    {"schema_id" snapshot-index-schema-id
     "schema_hash" snapshot-index-schema-hash
     "snapshot_label" snapshot-label
     "snapshot_identity_hash" (manifest/artifact-id identity-object)
     "snapshot_index_identity_object" identity-object
     "request_set_label" request-set-label
     "generated_at" generated-at
     "failure_policy" failure-policy
     "layout_policy" layout-policy
     "artifact_references" artifact-references
     "summary" (snapshot-summary artifact-references)}))

(defn snapshot-plan-path [request-set-label]
  (str snapshot-plans-dir "/" request-set-label ".json"))

(defn read-snapshot-plan [request-set-label]
  (let [path (snapshot-plan-path request-set-label)]
    (when-not (.isFile (io/file path))
      (throw (ex-info "Unknown snapshot plan"
                      {:request_set_label request-set-label
                       :path path})))
    (files/read-json path)))

(defn- plan-schema-hashes [plan]
  (vec (concat (get plan "schema_hashes" [])
               (map manifest/schema-hash
                    (get plan "schema_hash_paths" [])))))

(defn- plan-manifest-reference [reference]
  {:manifest-path (get reference "manifest_path")
   :locator (get reference "locator")
   :sidecar-role (get reference "sidecar_role")})

(defn build-snapshot-index-from-plan [request-set plan]
  (let [request-set-label (get request-set "label")
        plan-label (get plan "request_set_label")]
    (when-not (= request-set-label plan-label)
      (throw (ex-info "Snapshot plan request_set_label does not match request set"
                      {:request_set_label request-set-label
                       :plan_request_set_label plan-label})))
    (build-snapshot-index
     {:snapshot-label (get plan "snapshot_label")
      :request-set-label request-set-label
      :request-set request-set
      :generated-at (get plan "generated_at")
      :manifest-index-hash (get plan "manifest_index_hash")
      :failure-policy (get plan "failure_policy")
      :layout-policy (get plan "layout_policy")
      :schema-hashes (plan-schema-hashes plan)
      :parser-evidence-hashes (get plan "parser_evidence_hashes" [])
      :manifest-references (mapv plan-manifest-reference
                                 (get plan "manifest_references" []))})))

(defn write-snapshot-index! [snapshot-index output-path]
  (let [output-file (io/file output-path)]
    (when-let [parent (.getParentFile output-file)]
      (.mkdirs parent))
    (manifest/write-json-file! output-file snapshot-index)
    output-file))

(defn snapshot-identity-hash [snapshot-index-or-identity-object]
  (manifest/artifact-id
   (or (get snapshot-index-or-identity-object
            "snapshot_index_identity_object")
       snapshot-index-or-identity-object)))

(defn validate-snapshot-index-identity-object! [identity-object]
  (doseq [field identity-hash-fields]
    (require-hash! field (get identity-object field)))
  (doseq [field identity-array-fields]
    (canonical-hashes field (get identity-object field)))
  true)

(defn- compare-hash! [label expected actual]
  (when-not (= expected actual)
    (throw (ex-info (str label " mismatch")
                    {:label label
                     :expected expected
                     :actual actual}))))

(defn validate-snapshot-index! [snapshot-index]
  (when-not (snapshot-label? (get snapshot-index "snapshot_label"))
    (throw (ex-info "Invalid snapshot label"
                    {:snapshot_label (get snapshot-index "snapshot_label")})))
  (let [identity-object (get snapshot-index "snapshot_index_identity_object")]
    (validate-snapshot-index-identity-object! identity-object)
    (compare-hash! "snapshot_identity_hash"
                   (snapshot-identity-hash identity-object)
                   (get snapshot-index "snapshot_identity_hash"))
    (when-let [artifact-references (get snapshot-index "artifact_references")]
      (compare-hash! "artifact_set_hash"
                     (artifact-set-hash artifact-references)
                     (get identity-object "artifact_set_hash")))
    (when-let [failure-policy (get snapshot-index "failure_policy")]
      (compare-hash! "failure_policy_hash"
                     (policy-hash failure-policy)
                     (get identity-object "failure_policy_hash")))
    (when-let [layout-policy (get snapshot-index "layout_policy")]
      (compare-hash! "layout_policy_hash"
                     (policy-hash layout-policy)
                     (get identity-object "layout_policy_hash"))))
  true)
