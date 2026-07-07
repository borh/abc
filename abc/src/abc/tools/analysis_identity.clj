(ns abc.tools.analysis-identity
  (:require [abc.tools.hash :as hash]
            [abc.tools.manifest :as manifest]
            [clojure.string :as string]))

(def allowed-missing-policies
  #{"require-existing" "build-missing-only" "record-missing-status"})

(def allowed-input-view-kinds
  #{"parser-ir-plaintext-body-v1"})

(defn hash-json-value [value]
  (hash/format-sha256 (hash/sha256-json-jcs value)))

(def analysis-recipe-hash hash-json-value)

(def tokenizer-profile-hash hash-json-value)

(defn- required-string [label value]
  (when (or (not (string? value)) (string/blank? value))
    (throw (ex-info (str label " must be a non-empty string")
                    {:label label
                     :value value})))
  value)

(defn- subject-sort-key [subject]
  [(required-string "source_id" (get subject "source_id"))
   (required-string "work_content_hash" (get subject "work_content_hash"))
   (or (get subject "metadata_record_hash") "")])

(defn- normalize-subject [subject]
  (assoc subject "metadata_record_hash" (get subject "metadata_record_hash")))

(defn canonical-subjects [subjects]
  (->> subjects
       (map normalize-subject)
       (sort-by subject-sort-key)
       distinct
       vec))

(defn- canonical-input-views [input-views]
  (->> input-views
       (map (fn [input-view]
              (let [input-view-kind (get input-view "input_view_kind")]
                (when-not (contains? allowed-input-view-kinds input-view-kind)
                  (throw (ex-info "Invalid input view kind"
                                  {:input_view_kind input-view-kind
                                   :allowed_input_view_kinds allowed-input-view-kinds})))
                input-view)))
       (sort-by (juxt #(get % "input_view_kind")
                      #(get % "policy_hash")))
       distinct
       vec))

(defn- canonical-hashes [values]
  (->> values
       sort
       distinct
       vec))

(defn request-set-identity-object
  [{:keys [schema-hash corpus-snapshot-hash subjects input-views
           tokenizer-profile-hashes analysis-recipe-hashes missing-policy
           pack-policy-hash]}]
  (when-not (contains? allowed-missing-policies missing-policy)
    (throw (ex-info "Invalid missing policy"
                    {:missing_policy missing-policy})))
  {"schema_hash" schema-hash
   "corpus_snapshot_hash" corpus-snapshot-hash
   "subjects" (canonical-subjects subjects)
   "input_views" (canonical-input-views input-views)
   "tokenizer_profile_hashes" (canonical-hashes tokenizer-profile-hashes)
   "analysis_recipe_hashes" (canonical-hashes analysis-recipe-hashes)
   "missing_policy" missing-policy
   "pack_policy_hash" pack-policy-hash})

(defn request-set-id [request-set]
  (manifest/artifact-id (get request-set "request_set_identity_object")))

(defn resolved-recipe-label
  [{:keys [recipe-id analysis-recipe-hash registry-entry-hash resolved-at]}]
  {"recipe_id" recipe-id
   "analysis_recipe_hash" analysis-recipe-hash
   "registry_entry_hash" registry-entry-hash
   "resolved_at" resolved-at})

(defn resolved-tokenizer-profile-label
  [{:keys [profile-id tokenizer-profile-hash registry-entry-hash resolved-at]}]
  {"profile_id" profile-id
   "tokenizer_profile_hash" tokenizer-profile-hash
   "registry_entry_hash" registry-entry-hash
   "resolved_at" resolved-at})
