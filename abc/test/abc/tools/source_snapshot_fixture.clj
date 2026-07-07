(ns abc.tools.source-snapshot-fixture
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.json :as abc-json]
            [abc.tools.manifest :as manifest]
            [clojure.java.io :as io])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn temp-dir [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn delete-tree! [file]
  (let [file (io/file file)]
    (when (.exists file)
      (doseq [entry (reverse (file-seq file))]
        (.delete entry)))))

(defn example-hash [suffix]
  (files/example-hash suffix))

(defn source-snapshot!
  [file snapshot-inputs]
  (let [identity-object {"snapshot_scope" "unit-test-source-snapshot"
                         "snapshot_date" "2026-07-07"
                         "snapshot_inputs" snapshot-inputs}
        snapshot {"snapshot_schema_id" "https://w3id.org/abc/source-corpus-snapshot-v0.json"
                  "snapshot_hash_algorithm" "sha256-rfc8785-jcs-v0"
                  "snapshot_hash" (analysis-identity/hash-json-value
                                   identity-object)
                  "snapshot_identity_object" identity-object
                  "notes" "unit test fixture"}]
    (abc-json/write-deterministic-json-file! file snapshot)
    snapshot))

(defn parser-ir [work-hash]
  {"schema_hash" (example-hash "41")
   "source" {"work_content_hash" work-hash
             "encoding" "Shift_JIS"
             "normalization" "source"}
   "derived_from" {"aat_adapter" "aozora2html"
                   "aat_adapter_version" "aozora2html-adapter 0.1.0 gem-3.0.1"
                   "aat_version" 1
                   "mapping_id" "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"
                   "mapping_schema_hash" (example-hash "38")
                   "mapping_version" "0.2.0"}
   "nodes" []
   "warnings" []
   "errors" []})

(defn card-url [work-id person-id]
  (str "https://www.aozora.gr.jp/cards/" person-id "/card" work-id ".html"))

(defn metadata-record [work-id title person-id]
  {"metadata_record_schema_id" "https://w3id.org/abc/schemas/metadata-record.schema.json"
   "metadata_record_schema_hash" (manifest/schema-hash
                                  "schemas/metadata-record.schema.json")
   "work" {"work_id" work-id
           "title" title
           "title_reading" "てすと"
           "subtitle" nil
           "subtitle_reading" nil
           "original_title" nil
           "sort_reading" "てすと"
           "card_url" (card-url work-id person-id)
           "aozora_available" "2000-01-01"
           "aozora_modified" "2026-07-04"
           "copyright_expired" true
           "orthographic_style" "新字新仮名"
           "first_published" nil
           "ndc" "NDC 913"
           "source_editions" []}
   "contributors" [{"person_id" person-id
                    "person_record_hash" (example-hash "35")
                    "relation_to_work" "著者"}]})

(defn write-work-files!
  [work-dir {:keys [title work-id person-id work-hash]}]
  (abc-json/write-deterministic-json-file! (io/file work-dir "aat.json")
                                           {"version" 1
                                            "work_id" work-id
                                            "blocks" []})
  (abc-json/write-deterministic-json-file! (io/file work-dir "parser-ir.json")
                                           (parser-ir work-hash))
  (abc-json/write-deterministic-json-file!
   (io/file work-dir "metadata-record.json")
   (metadata-record work-id title person-id))
  work-dir)

(defn materialized-work!
  [root {:keys [slug] :as opts}]
  (write-work-files! (io/file root "works" slug) opts))

(defn workset-entry!
  [root {:keys [slug title work-id person-id] :as opts}]
  (let [work-dir (write-work-files! (io/file root slug) opts)]
    {:slug slug
     :title title
     :work_id work-id
     :person_id person-id
     :card_url (card-url work-id person-id)
     :aat_path (str (io/file work-dir "aat.json"))
     :parser_ir_path (str (io/file work-dir "parser-ir.json"))
     :metadata_record_path (str (io/file work-dir "metadata-record.json"))
     :source_manifest_path (str (io/file work-dir "source.manifest.json"))}))
