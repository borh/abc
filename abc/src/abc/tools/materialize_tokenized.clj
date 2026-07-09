(ns abc.tools.materialize-tokenized
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [clojure.java.io :as io]))

(def activity-id "https://w3id.org/abc/activity/materialize-tokenized")
(def tool-agent "abc.tools.materialize-tokenized")

(def ^:private copied-parser-ir-fields
  ["parser_build_hash"
   "parser_config_hash"
   "aat_parser_ir_mapping_hash"
   "parser_ir_schema_hash"])

(def ^:private coordinate-system "token-index-v1+unicode-scalar-value-input-spans")
(def ^:private input-view-kind "parser-ir-plaintext-body-v1")

(defn tokenized-identity-object
  [{:keys [producer-manifest tokenizer-profile]}]
  (let [producer-identity (get producer-manifest "manifest_identity_object")
        profile-hash (analysis-identity/tokenizer-profile-hash tokenizer-profile)]
    (merge
     {"manifest_schema_hash" (manifest/schema-hash "schemas/manifest.schema.json")
      "corpus_snapshot_hash" (get producer-identity "corpus_snapshot_hash")
      "work_content_hash" (get producer-identity "work_content_hash")
      "metadata_record_hash" (get producer-identity "metadata_record_hash")
      "tei_profile_hash" nil
      "tokenizer_build_hash" (get-in tokenizer-profile ["tokenizer" "build_hash"])
      "tokenizer_dictionary_hash" (get-in tokenizer-profile ["dictionary" "archive_hash"])
      "tokenizer_profile_hash" profile-hash
      "analysis_recipe_hash" nil
      "annotation_policy_hash" nil
      "output_format_spec_hash" (get tokenizer-profile "token_output_schema_hash")}
     (select-keys producer-identity copied-parser-ir-fields))))

(defn token-stream-value
  [{:keys [producer-manifest tokenizer-profile input-plaintext-policy-hash
           tokens warnings]}]
  (let [profile-hash (analysis-identity/tokenizer-profile-hash tokenizer-profile)
        output-schema-hash (manifest/schema-hash "schemas/token-output.schema.json")]
    {"schema_id" "https://w3id.org/abc/schemas/token-output.schema.json"
     "schema_hash" output-schema-hash
     "tokenizer_profile_hash" profile-hash
     "producer" {"input_view_kind" input-view-kind
                 "artifact_id" (get producer-manifest "artifact_id")
                 "content_hash" (get-in producer-manifest ["content" "content_hash"])}
     "input_plaintext_policy_hash" input-plaintext-policy-hash
     "token_output_schema_hash" (get tokenizer-profile "token_output_schema_hash")
     "coordinate_system" coordinate-system
     "tokens" (vec tokens)
     "warnings" (vec (or warnings []))}))

(defn- sidecar [file]
  {"role" "token-stream"
   "hash" (str "sha256:" (files/sha256-file file))
   "media_type" "application/json"
   "path_hint" "token-stream.json"})

(defn tokenized-manifest
  [{:keys [producer-manifest tokenizer-profile input-plaintext-policy-hash
           token-stream-file generated-at]}]
  (let [identity-object (tokenized-identity-object
                         {:producer-manifest producer-manifest
                          :tokenizer-profile tokenizer-profile})
        producer-artifact-id (get producer-manifest "artifact_id")
        profile-hash (analysis-identity/tokenizer-profile-hash tokenizer-profile)]
    (manifest/artifact-manifest
     {:artifact-kind "tokenized"
      :validation-status "passed"
      :identity-object identity-object
      :content (manifest/content token-stream-file
                                 "application/json"
                                 "token-stream.json"
                                 files/sha256-file)
      :sidecars [(sidecar token-stream-file)]
      :generated-at generated-at
      :activity-id activity-id
      :agent tool-agent
      :plan-hash nil
      :used [producer-artifact-id
             (get-in producer-manifest ["content" "content_hash"])
             profile-hash
             (get tokenizer-profile "input_normalization_policy_hash")
             (get-in tokenizer-profile ["tokenizer" "build_hash"])
             (get-in tokenizer-profile ["dictionary" "archive_hash"])
             (get tokenizer-profile "token_output_schema_hash")
             input-plaintext-policy-hash]
      :was-derived-from [producer-artifact-id]
      :notes "Generated from parser-ir-plaintext-body-v1 tokenized fixture input."})))

(defn materialize-tokenized!
  [{:keys [producer-manifest tokenizer-profile input-plaintext-policy-hash
           applied-normalization-policy-hash tokens warnings output-dir generated-at]}]
  ;; F1 agreement: the run must have applied the normalization the profile
  ;; declares. `applied-normalization-policy-hash` is the Rust run-provenance
  ;; value (T1); when absent (pure source-identity fixtures with no run), it
  ;; defaults to the profile's declared hash and the check is a no-op.
  (analysis-identity/assert-input-normalization-agreement!
   {:declared (get tokenizer-profile "input_normalization_policy_hash")
    :applied (or applied-normalization-policy-hash
                 (get tokenizer-profile "input_normalization_policy_hash"))
    :context {:activity activity-id
              :producer-artifact-id (get producer-manifest "artifact_id")}})
  (let [output-dir (io/file output-dir)
        token-stream-file (io/file output-dir "token-stream.json")
        manifest-file (io/file output-dir "tokenized.manifest.json")
        token-stream (token-stream-value
                      {:producer-manifest producer-manifest
                       :tokenizer-profile tokenizer-profile
                       :input-plaintext-policy-hash input-plaintext-policy-hash
                       :tokens tokens
                       :warnings warnings})]
    (manifest/write-json-file! token-stream-file token-stream)
    (manifest/write-json-file! manifest-file
                               (tokenized-manifest
                                {:producer-manifest producer-manifest
                                 :tokenizer-profile tokenizer-profile
                                 :input-plaintext-policy-hash input-plaintext-policy-hash
                                 :token-stream-file token-stream-file
                                 :generated-at generated-at}))
    {:token-stream token-stream-file
     :manifest manifest-file}))
