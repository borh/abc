(ns abc.tools.manifest
  (:require [abc.tools.hash :as hash]
            [abc.tools.jcs :as jcs]
            [abc.tools.json :as abc-json]
            [abc.tools.schema :as schema]))

(def manifest-schema-id "https://w3id.org/abc/schemas/manifest.schema.json")

(defn file-hash [file]
  (hash/format-sha256 (hash/sha256-file file)))

(defn jcs-json [value]
  (jcs/canonical-json-string value))

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
   "aat_parser_ir_mapping_hash"
   "parser_ir_schema_hash"
   "tei_profile_hash"
   "tokenizer_build_hash"
   "tokenizer_dictionary_hash"
   "tokenizer_profile_hash"
   "analysis_recipe_hash"
   "annotation_policy_hash"
   "output_format_spec_hash"])

(defn identity-object [manifest-inputs {:keys [manifest-schema-hash output-format-spec-hash]}]
  (reduce (fn [result k]
            (assoc result k
                   (case k
                     "manifest_schema_hash" manifest-schema-hash
                     "corpus_snapshot_hash" (get manifest-inputs k)
                     "work_content_hash" (get manifest-inputs k)
                     "metadata_record_hash" nil
                     "parser_build_hash" (get manifest-inputs k)
                     "parser_config_hash" (get manifest-inputs k)
                     "aat_parser_ir_mapping_hash" (get manifest-inputs "mapping_hash")
                     "parser_ir_schema_hash" (get manifest-inputs k)
                     "tei_profile_hash" nil
                     "tokenizer_build_hash" nil
                     "tokenizer_dictionary_hash" nil
                     "tokenizer_profile_hash" (get manifest-inputs k)
                     "analysis_recipe_hash" nil
                     "annotation_policy_hash" nil
                     "output_format_spec_hash" output-format-spec-hash)))
          (sorted-map)
          identity-keys))

(defn content [file media-type path-hint sha256-file-fn]
  {"content_hash" (str "sha256:" (sha256-file-fn file))
   "media_type" media-type
   "byte_length" (hash/byte-length file)
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

(def ^:private manifest-schema-path "schemas/manifest.schema.json")

(defn source-bundle-artifact-manifest
  "Reusable `source` artifact manifest that names source-bundle.json as its
  content. Every identity value is explicit: `corpus-snapshot-hash`,
  `work-content-hash`, `metadata-record-hash`, the pre-built `content` map, the
  `validation-status`, and `generated-at`. No CLI directory layout is assumed,
  so both the release build and any other caller that supplies the same value
  obtain the same manifest. `output_format_spec_hash` is the manifest schema
  hash (a source bundle has no downstream output-format spec of its own)."
  [{:keys [corpus-snapshot-hash work-content-hash metadata-record-hash
           content validation-status sidecars generated-at activity-id agent
           used was-derived-from notes]
    :or {validation-status "passed"
         sidecars []
         activity-id "https://w3id.org/abc/activity/materialize-source-bundle"
         agent "abc.tools.manifest"}}]
  (let [manifest-schema-hash (schema-hash manifest-schema-path)
        io (assoc (identity-object
                   {"corpus_snapshot_hash" corpus-snapshot-hash
                    "work_content_hash" work-content-hash
                    "metadata_record_hash" metadata-record-hash}
                   {:manifest-schema-hash manifest-schema-hash
                    :output-format-spec-hash manifest-schema-hash})
                  "metadata_record_hash" metadata-record-hash)]
    (artifact-manifest
     {:artifact-kind "source"
      :validation-status validation-status
      :identity-object io
      :content content
      :sidecars (vec sidecars)
      :generated-at generated-at
      :activity-id activity-id
      :agent agent
      :plan-hash nil
      :used (or used
                (vec (keep identity [corpus-snapshot-hash work-content-hash
                                     metadata-record-hash])))
      :was-derived-from (or was-derived-from
                            (vec (keep identity [work-content-hash])))
      :notes notes})))

(defn parser-ir-artifact-manifest
  "Reusable `parser-ir` artifact manifest that names parser-ir.json as its
  content. `manifest-inputs` carries the explicit corpus/work/parser/mapping
  identity values, `content` is the pre-built content map, and `sidecars`,
  `validation-status`, and `generated-at` are explicit. No CLI directory layout
  is assumed: the release build and the materialize-import CLI produce the same
  manifest for the same explicit value. `output_format_spec_hash` is the
  parser-IR schema hash."
  [{:keys [manifest-inputs content validation-status sidecars generated-at
           activity-id agent used was-derived-from notes]
    :or {validation-status "warning"
         sidecars []
         activity-id "https://w3id.org/abc/activity/materialize-parser-ir"
         agent "abc.tools.manifest"}}]
  (let [manifest-schema-hash (schema-hash manifest-schema-path)
        io (identity-object
            manifest-inputs
            {:manifest-schema-hash manifest-schema-hash
             :output-format-spec-hash (get manifest-inputs "parser_ir_schema_hash")})]
    (artifact-manifest
     {:artifact-kind "parser-ir"
      :validation-status validation-status
      :identity-object io
      :content content
      :sidecars (vec sidecars)
      :generated-at generated-at
      :activity-id activity-id
      :agent agent
      :plan-hash nil
      :used (or used
                [(get manifest-inputs "work_content_hash")
                 (get manifest-inputs "parser_build_hash")
                 (get manifest-inputs "parser_config_hash")
                 (get manifest-inputs "mapping_hash")
                 (get manifest-inputs "parser_ir_schema_hash")])
      :was-derived-from (or was-derived-from
                            [(get manifest-inputs "work_content_hash")])
      :notes notes})))

(defn write-json-file! [file value]
  (abc-json/write-deterministic-json-file! file value))
