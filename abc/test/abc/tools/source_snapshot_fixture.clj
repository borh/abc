(ns abc.tools.source-snapshot-fixture
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.json :as abc-json]
            [abc.tools.manifest :as manifest]
            [abc.tools.source-bundle :as source-bundle]
            [babashka.fs :as fs]
            [clojure.java.io :as io])
  (:import [java.nio.charset StandardCharsets]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.zip ZipEntry ZipOutputStream]))

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

(defn parser-ir
  ([work-hash] (parser-ir work-hash nil))
  ([work-hash primary-text-hash]
   {"schema_hash" (example-hash "41")
    "source" (cond-> {"work_content_hash" work-hash
                      "encoding" "Shift_JIS"
                      "normalization" "source"}
               primary-text-hash
               (assoc "primary_text_hash" primary-text-hash))
    "derived_from" {"aat_adapter" "aozora2html"
                    "aat_adapter_version" "aozora2html-adapter 0.1.0 gem-3.0.1"
                    "aat_version" 1
                    "mapping_id" "https://w3id.org/abc/mappings/aat-v1-to-parser-ir-v1/generated-probe"
                    "mapping_schema_hash" (example-hash "38")
                    "mapping_version" "0.2.0"}
    "sentence_segmentation" {"schema_version" "sentence-segmentation-v1"
                             "splitter_id" "ab-plaintext-japanese-v1"
                             "coordinate_system" "decoded_utf8"
                             "coverage" "body-paragraphs"}
    "nodes" []
    "warnings" []
    "errors" []}))

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

(defn official-source [work-id person-id work-hash]
  {"work_id" work-id
   "card_person_id" person-id
   "text_url" (str "https://www.aozora.gr.jp/cards/" person-id
                   "/files/" work-id "_ruby_fixture.zip")
   "text_zip_relpath" (str "cards/" person-id "/files/"
                           work-id "_ruby_fixture.zip")
   "zip_member" (str work-id ".txt")
   "source_hash" work-hash
   "source_bytes" 1})

(defn- official-source-v1
  [work-id person-id {:keys [archive-hash bundle-hash
                             primary-text-member primary-text-hash]}]
  (assoc (official-source work-id person-id archive-hash)
         "archive_hash" archive-hash
         "bundle_hash" bundle-hash
         "primary_text_member" primary-text-member
         "primary_text_hash" primary-text-hash
         "zip_member" primary-text-member))

(defn- utf8-bytes [value]
  (.getBytes value StandardCharsets/UTF_8))

(defn- write-source-zip! [file work-id]
  (files/create-dirs! (fs/parent file))
  (with-open [out (ZipOutputStream. (io/output-stream file))]
    (doseq [[path bytes] [[(str work-id ".txt")
                           (utf8-bytes (str "本文 " work-id))]
                          ["images/cover.png" (byte-array [1 2 3 4])]]]
      (let [entry (doto (ZipEntry. path) (.setTime 0))]
        (.putNextEntry out entry)
        (.write out bytes)
        (.closeEntry out))))
  file)

(defn- new-source-fixture! [work-dir work-id]
  (let [zip-file (write-source-zip! (io/file work-dir "source.zip") work-id)
        inspection (source-bundle/inspect-zip zip-file)
        source-bundle-file (io/file work-dir "source-bundle.json")]
    (source-bundle/write-manifest! source-bundle-file inspection)
    {:archive-hash (:archive-hash inspection)
     :bundle-hash (:bundle-hash inspection)
     :primary-text-member (:primary-text-member inspection)
     :primary-text-hash (:primary-text-hash inspection)
     :source-bundle-file source-bundle-file}))

(defn write-work-files!
  [work-dir {:keys [title work-id person-id work-hash legacy?]}]
  (files/create-dirs! (io/file work-dir))
  (let [identity (when-not legacy? (new-source-fixture! work-dir work-id))
        parser-work-hash (or (:bundle-hash identity) work-hash)
        parser-primary-hash (:primary-text-hash identity)]
    (abc-json/write-deterministic-json-file! (io/file work-dir "aat.json")
                                             {"version" 1
                                              "work_id" work-id
                                              "blocks" []})
    (abc-json/write-deterministic-json-file! (io/file work-dir "parser-ir.json")
                                             (parser-ir parser-work-hash
                                                        parser-primary-hash))
    (abc-json/write-deterministic-json-file!
     (io/file work-dir "metadata-record.json")
     (metadata-record work-id title person-id))
    (abc-json/write-deterministic-json-file!
     (io/file work-dir "official-source.json")
     (if legacy?
       (official-source work-id person-id work-hash)
       (official-source-v1 work-id person-id identity)))
    work-dir))

(defn materialized-work!
  [root {:keys [slug] :as opts}]
  (write-work-files! (fs/file root "works" slug) opts))

(defn workset-entry!
  [root {:keys [slug title work-id person-id] :as opts}]
  (let [work-dir (write-work-files! (fs/file root slug) opts)]
    {:slug slug
     :title title
     :work_id work-id
     :person_id person-id
     :card_url (card-url work-id person-id)
     :aat_path (str (io/file work-dir "aat.json"))
     :parser_ir_path (str (io/file work-dir "parser-ir.json"))
     :metadata_record_path (str (io/file work-dir "metadata-record.json"))
     :official_source_path (str (io/file work-dir "official-source.json"))
     :source_bundle_path (str (io/file work-dir "source-bundle.json"))
     :source_manifest_path (str (io/file work-dir "source.manifest.json"))}))

(defn legacy-workset-entry!
  [root opts]
  (dissoc (workset-entry! root (assoc opts :legacy? true))
          :source_bundle_path))
