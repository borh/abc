(ns abc.tools.materialize-analysis
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [clojure.java.io :as io]))

(def activity-id "https://w3id.org/abc/activity/materialize-analysis")
(def tool-agent "abc.tools.materialize-analysis")

(def ^:private copied-parser-ir-fields
  ["parser_build_hash"
   "parser_config_hash"
   "aat_parser_ir_mapping_hash"
   "parser_ir_schema_hash"])

(def ^:private copied-tokenized-analysis-fields
  (conj copied-parser-ir-fields
        "tei_profile_hash"
        "tokenizer_build_hash"
        "tokenizer_dictionary_hash"
        "tokenizer_profile_hash"))

(defn analysis-identity-object
  [{:keys [producer-manifest recipe]}]
  (let [producer-identity (get producer-manifest "manifest_identity_object")
        analysis-recipe-hash (analysis-identity/analysis-recipe-hash recipe)]
    (merge
     {"manifest_schema_hash" (manifest/schema-hash "schemas/manifest.schema.json")
      "corpus_snapshot_hash" (get producer-identity "corpus_snapshot_hash")
      "work_content_hash" (get producer-identity "work_content_hash")
      "metadata_record_hash" nil
      "tei_profile_hash" nil
      "tokenizer_build_hash" nil
      "tokenizer_dictionary_hash" nil
      "tokenizer_profile_hash" nil
      "analysis_recipe_hash" analysis-recipe-hash
      "output_format_spec_hash" (manifest/schema-hash "schemas/analysis-result.schema.json")}
     (select-keys producer-identity copied-parser-ir-fields))))

(defn token-backed-analysis-identity-object
  [{:keys [producer-manifest recipe]}]
  (let [producer-identity (get producer-manifest "manifest_identity_object")
        analysis-recipe-hash (analysis-identity/analysis-recipe-hash recipe)]
    (merge
     {"manifest_schema_hash" (manifest/schema-hash "schemas/manifest.schema.json")
      "corpus_snapshot_hash" (get producer-identity "corpus_snapshot_hash")
      "work_content_hash" (get producer-identity "work_content_hash")
      "metadata_record_hash" (get producer-identity "metadata_record_hash")
      "analysis_recipe_hash" analysis-recipe-hash
      "output_format_spec_hash" (manifest/schema-hash "schemas/analysis-result.schema.json")}
     (select-keys producer-identity copied-tokenized-analysis-fields))))

(defn- result-subject
  [producer-identity subject metadata-record-hash]
  {"source_id" (get subject "source_id")
   "logical_path" (get subject "logical_path")
   "git_ref" (get subject "git_ref")
   "work_id" (get subject "work_id")
   "corpus_snapshot_hash" (get producer-identity "corpus_snapshot_hash")
   "work_content_hash" (get producer-identity "work_content_hash")
   "metadata_record_hash" metadata-record-hash})

(defn analysis-result-value
  [{:keys [producer-manifest recipe subject metrics]}]
  (let [producer-identity (get producer-manifest "manifest_identity_object")
        output-schema-hash (manifest/schema-hash "schemas/analysis-result.schema.json")]
    {"schema_id" "https://w3id.org/abc/schemas/analysis-result.schema.json"
     "schema_hash" output-schema-hash
     "subject" (result-subject producer-identity subject nil)
     "input_view" {"input_view_kind" "parser-ir-plaintext-body-v1"
                   "producer_artifact_id" (get producer-manifest "artifact_id")
                   "producer_content_hash" (get-in producer-manifest ["content" "content_hash"])
                   "plaintext_policy_hash" (get recipe "plaintext_policy_hash")
                   "coordinate_system" "unicode-scalar-value"}
     "tokenizer_profile_hash" nil
     "analysis_recipe_hash" (analysis-identity/analysis-recipe-hash recipe)
     "metrics" (vec metrics)
     "warnings" []}))

(defn token-backed-analysis-result-value
  [{:keys [producer-manifest recipe subject metrics]}]
  (let [producer-identity (get producer-manifest "manifest_identity_object")
        output-schema-hash (manifest/schema-hash "schemas/analysis-result.schema.json")]
    {"schema_id" "https://w3id.org/abc/schemas/analysis-result.schema.json"
     "schema_hash" output-schema-hash
     "subject" (result-subject producer-identity
                               subject
                               (get producer-identity "metadata_record_hash"))
     "input_view" {"input_view_kind" "token-stream-v1"
                   "producer_artifact_id" (get producer-manifest "artifact_id")
                   "producer_content_hash" (get-in producer-manifest ["content" "content_hash"])
                   "token_output_schema_hash" (get producer-identity
                                                   "output_format_spec_hash")
                   "coordinate_system" "token-index-v1+unicode-scalar-value-input-spans"}
     "tokenizer_profile_hash" (get producer-identity "tokenizer_profile_hash")
     "analysis_recipe_hash" (analysis-identity/analysis-recipe-hash recipe)
     "metrics" (vec metrics)
     "warnings" []}))

(defn- sidecar [file]
  {"role" "analysis-result"
   "hash" (str "sha256:" (files/sha256-file file))
   "media_type" "application/json"
   "path_hint" "analysis-result.json"})

(defn analysis-manifest
  [{:keys [producer-manifest recipe result-file generated-at]}]
  (let [identity-object (analysis-identity-object {:producer-manifest producer-manifest
                                                   :recipe recipe})
        producer-artifact-id (get producer-manifest "artifact_id")]
    (manifest/artifact-manifest
     {:artifact-kind "analysis"
      :validation-status "passed"
      :identity-object identity-object
      :content (manifest/content result-file
                                 "application/json"
                                 "analysis-result.json"
                                 files/sha256-file)
      :sidecars [(sidecar result-file)]
      :generated-at generated-at
      :activity-id activity-id
      :agent tool-agent
      :plan-hash nil
      :used [producer-artifact-id
             (get recipe "plaintext_policy_hash")
             (analysis-identity/analysis-recipe-hash recipe)
             (manifest/schema-hash "schemas/analysis-result.schema.json")]
      :was-derived-from [producer-artifact-id]
      :notes "Generated from parser-ir-plaintext-body-v1 analysis fixture input."})))

(defn token-backed-analysis-manifest
  [{:keys [producer-manifest recipe result-file generated-at]}]
  (let [identity-object (token-backed-analysis-identity-object
                         {:producer-manifest producer-manifest
                          :recipe recipe})
        producer-artifact-id (get producer-manifest "artifact_id")
        producer-identity (get producer-manifest "manifest_identity_object")]
    (manifest/artifact-manifest
     {:artifact-kind "analysis"
      :validation-status "passed"
      :identity-object identity-object
      :content (manifest/content result-file
                                 "application/json"
                                 "analysis-result.json"
                                 files/sha256-file)
      :sidecars [(sidecar result-file)]
      :generated-at generated-at
      :activity-id activity-id
      :agent tool-agent
      :plan-hash nil
      :used [producer-artifact-id
             (get-in producer-manifest ["content" "content_hash"])
             (get producer-identity "tokenizer_profile_hash")
             (get producer-identity "output_format_spec_hash")
             (analysis-identity/analysis-recipe-hash recipe)
             (manifest/schema-hash "schemas/analysis-result.schema.json")]
      :was-derived-from [producer-artifact-id]
      :notes "Generated from token-stream-v1 analysis fixture input."})))

(defn- materialize-analysis-with!
  [{:keys [producer-manifest recipe subject metrics output-dir generated-at
           result-value-fn manifest-fn]}]
  (let [output-dir (io/file output-dir)
        result-file (io/file output-dir "analysis-result.json")
        manifest-file (io/file output-dir "analysis.manifest.json")
        result-value (result-value-fn {:producer-manifest producer-manifest
                                       :recipe recipe
                                       :subject subject
                                       :metrics metrics})]
    (manifest/write-json-file! result-file result-value)
    (manifest/write-json-file! manifest-file
                               (manifest-fn {:producer-manifest producer-manifest
                                             :recipe recipe
                                             :result-file result-file
                                             :generated-at generated-at}))
    {:analysis-result result-file
     :manifest manifest-file}))

(defn materialize-analysis!
  [opts]
  (materialize-analysis-with! (assoc opts
                                     :result-value-fn analysis-result-value
                                     :manifest-fn analysis-manifest)))

(defn materialize-token-backed-analysis!
  [opts]
  (materialize-analysis-with! (assoc opts
                                     :result-value-fn token-backed-analysis-result-value
                                     :manifest-fn token-backed-analysis-manifest)))
