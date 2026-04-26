(ns abc.tools.manifest
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [java.io StringWriter]
           [java.security MessageDigest]))

(def manifest-schema-id "https://example.org/abc/schemas/manifest.schema.json")

(def corpus-snapshot-hash
  "sha256:1111111111111111111111111111111111111111111111111111111111111111")

(defn bytes->hex [bytes]
  (apply str (map #(format "%02x" (bit-and % 0xff)) bytes)))

(defn sha256-string [s]
  (let [digest (MessageDigest/getInstance "SHA-256")]
    (.update digest (.getBytes s "UTF-8"))
    (bytes->hex (.digest digest))))

(defn schema-file-hash [file]
  (with-open [in (io/input-stream (io/file file))]
    (let [digest (MessageDigest/getInstance "SHA-256")
          buffer (byte-array 8192)]
      (loop []
        (let [n (.read in buffer)]
          (when (pos? n)
            (.update digest buffer 0 n)
            (recur))))
      (str "sha256:" (bytes->hex (.digest digest))))))

(defn json-string [s]
  (json/write-json-str s))

(defn v0-identity-json [value]
  (cond
    (nil? value) "null"
    (string? value) (json-string value)
    (map? value) (str "{"
                      (->> value
                           (sort-by key)
                           (map (fn [[k v]]
                                  (str (json-string k) ":" (v0-identity-json v))))
                           (string/join ","))
                      "}")
    :else (throw (ex-info "Unsupported canonical JSON value"
                          {:value value}))))

(defn artifact-id [identity-object]
  (str "sha256:" (sha256-string (v0-identity-json identity-object))))

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
                    "corpus_snapshot_hash" (get manifest-inputs k corpus-snapshot-hash)
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
  (cond
    (map? value)
    (into (sorted-map)
          (map (fn [[k v]]
                 [k (stable-json-value v)]))
          value)

    (vector? value)
    (mapv stable-json-value value)

    (sequential? value)
    (mapv stable-json-value value)

    :else
    value))

(defn write-json-file! [file value]
  (io/make-parents file)
  (with-open [writer (io/writer file)]
    (.write writer (json/write-json-str (stable-json-value value) :indent-str "  "))
    (.write writer "\n"))
  file)
