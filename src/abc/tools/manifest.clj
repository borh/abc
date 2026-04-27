(ns abc.tools.manifest
  (:require [abc.tools.hash :as hash]
            [abc.tools.jcs :as jcs]
            [abc.tools.json :as abc-json]
            [abc.tools.schema :as schema]
            [charred.api :as json]
            [clojure.java.io :as io]))

(def manifest-schema-id "https://w3id.org/abc/schemas/manifest.schema.json")

(defn bytes->hex [bytes]
  (hash/bytes->hex bytes))

(defn sha256-string [s]
  (hash/sha256-string s))

(defn file-hash [file]
  (hash/format-sha256 (hash/sha256-file file)))

(defn json-string [s]
  (json/write-json-str s))

(defn jcs-json [value]
  (jcs/canonical-json-string value))

(def v0-identity-json jcs-json)

(defn schema-value-hash [schema-value]
  (hash/format-sha256 (hash/sha256-json-jcs schema-value)))

(defn schema-hash [file]
  (schema/schema-hash file))

(defn artifact-id [identity-object]
  (hash/format-sha256 (hash/sha256-json-jcs identity-object)))

(def identity-keys
  ["manifest_schema_hash"
   "corpus_snapshot_hash"
   "work_content_hash"
   "metadata_record_hash"
   "parser_build_hash"
   "parser_config_hash"
   "parser_ir_schema_hash"
   "tei_profile_hash"
   "tokenizer_build_hash"
   "tokenizer_dictionary_hash"
   "analysis_recipe_hash"
   "output_format_spec_hash"])

(defn identity-object [manifest-inputs {:keys [manifest-schema-hash output-format-spec-hash]}]
  (into (sorted-map)
        (map (fn [k]
               [k (case k
                    "manifest_schema_hash" manifest-schema-hash
                    "corpus_snapshot_hash" (get manifest-inputs k)
                    "work_content_hash" (get manifest-inputs k)
                    "metadata_record_hash" nil
                    "parser_build_hash" (get manifest-inputs k)
                    "parser_config_hash" (get manifest-inputs k)
                    "parser_ir_schema_hash" (get manifest-inputs k)
                    "tei_profile_hash" nil
                    "tokenizer_build_hash" nil
                    "tokenizer_dictionary_hash" nil
                    "analysis_recipe_hash" nil
                    "output_format_spec_hash" output-format-spec-hash)]))
        identity-keys))

(defn content [file media-type path-hint sha256-file-fn]
  {"content_hash" (str "sha256:" (sha256-file-fn file))
   "media_type" media-type
   "byte_length" (.length (io/file file))
   "path_hint" path-hint})

(defn artifact-manifest
  [{:keys [artifact-kind validation-status identity-object content sidecars
           generated-at activity-id agent plan-hash used was-derived-from notes]}]
  (let [manifest {"manifest_schema_id" manifest-schema-id
                  "artifact_id" (artifact-id identity-object)
                  "artifact_kind" artifact-kind
                  "validation_status" validation-status
                  "manifest_identity_object" identity-object
                  "content" content
                  "sidecars" (vec sidecars)
                  "provenance" {"generated_at" generated-at
                                "activity_id" activity-id
                                "agent" agent
                                "plan_hash" plan-hash
                                "used" (vec (sort used))
                                "was_derived_from" (vec (sort was-derived-from))}
                  "license" nil
                  "signatures" []
                  "superseded_by" nil
                  "invalidated_at" nil
                  "replacement_reason" nil
                  "notes" notes}]
    manifest))

(defn stable-json-value [value]
  (abc-json/prepare-deterministic-json value))

(defn write-json-file! [file value]
  (abc-json/write-deterministic-json-file! file value))
