(ns abc.tools.snapshot-index
  (:require [abc.tools.hash :as hash]
            [abc.tools.manifest :as manifest]))

(def snapshot-index-schema-id
  "https://w3id.org/abc/schemas/snapshot-index.schema.json")

(def snapshot-label-pattern
  #"^soranoha-snapshot-\d{4}-\d{2}-\d{2}-\d{2}$")

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

(defn snapshot-label? [value]
  (boolean (and (string? value)
                (re-matches snapshot-label-pattern value))))

(defn hash-json-value [value]
  (hash/format-sha256 (hash/sha256-json-jcs value)))

(def policy-hash hash-json-value)

(defn- require-hash! [label value]
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

(defn artifact-set-hash [artifact-references]
  (hash-json-value (canonical-artifact-references artifact-references)))

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
