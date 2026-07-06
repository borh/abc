(ns abc.tools.materialize-source-snapshot-test
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.json :as abc-json]
            [abc.tools.manifest :as manifest]
            [abc.tools.materialize-source-snapshot :as materialize]
            [abc.tools.metadata-record :as metadata-record]
            [abc.tools.schema :as schema]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def generated-at "2026-07-04T00:00:00Z")

(defn- temp-dir [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- delete-tree! [dir]
  (when dir
    (doseq [f (reverse (file-seq dir))]
      (.delete f))))

(defn- example-hash [suffix]
  (files/example-hash suffix))

(defn- parser-ir [work-hash]
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

(defn- metadata-record [work-id title person-id]
  {"metadata_record_schema_id" "https://w3id.org/abc/schemas/metadata-record.schema.json"
   "metadata_record_schema_hash" (manifest/schema-hash "schemas/metadata-record.schema.json")
   "work" {"work_id" work-id
           "title" title
           "title_reading" "てすと"
           "subtitle" nil
           "subtitle_reading" nil
           "original_title" nil
           "sort_reading" "てすと"
           "card_url" (str "https://www.aozora.gr.jp/cards/" person-id "/card" work-id ".html")
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

(defn- fixture-work! [root {:keys [slug title work-id person-id work-hash]}]
  (let [work-dir (io/file root slug)
        aat-file (io/file work-dir "aat.json")
        parser-file (io/file work-dir "parser-ir.json")
        metadata-file (io/file work-dir "metadata-record.json")
        source-manifest-file (io/file work-dir "source.manifest.json")]
    (abc-json/write-deterministic-json-file! aat-file
                                             {"version" 1
                                              "work_id" work-id
                                              "blocks" []})
    (abc-json/write-deterministic-json-file! parser-file (parser-ir work-hash))
    (abc-json/write-deterministic-json-file! metadata-file
                                             (metadata-record work-id title person-id))
    {:slug slug
     :title title
     :work_id work-id
     :person_id person-id
     :card_url (str "https://www.aozora.gr.jp/cards/" person-id "/card" work-id ".html")
     :aat_path (str aat-file)
     :parser_ir_path (str parser-file)
     :metadata_record_path (str metadata-file)
     :source_manifest_path (str source-manifest-file)}))

(defn- workset! [root works]
  (let [workset-file (io/file root "workset.edn")]
    (spit workset-file
          (pr-str {:snapshot_scope "unit-test-source-snapshot"
                   :snapshot_date "2026-07-04"
                   :works works}))
    workset-file))

(deftest materialize-source-snapshot-test
  (let [root (temp-dir "abc-source-snapshot")
        out-file (io/file root "snapshot.json")]
    (try
      (let [works [(fixture-work! root {:slug "one"
                                        :title "一"
                                        :work-id "000001"
                                        :person-id "000101"
                                        :work-hash (example-hash "a1")})
                   (fixture-work! root {:slug "two"
                                        :title "二"
                                        :work-id "000002"
                                        :person-id "000102"
                                        :work-hash (example-hash "a2")})]
            workset-file (workset! root works)
            result (materialize/materialize-source-snapshot!
                    {:workset-path (str workset-file)
                     :output-path (str out-file)
                     :generated-at generated-at})
            snapshot (files/read-json out-file)
            manifest-schema (files/read-json "schemas/manifest.schema.json")
            manifest-files (map :source-manifest (:works result))
            manifests (map files/read-json manifest-files)
            snapshot-hash (get snapshot "snapshot_hash")
            expected-snapshot-hash
            (hash/format-sha256
             (hash/sha256-json-jcs (get snapshot "snapshot_identity_object")))]
        (is (= out-file (:snapshot result)))
        (is (= expected-snapshot-hash snapshot-hash))
        (is (= "https://w3id.org/abc/source-corpus-snapshot-v0.json"
               (get snapshot "snapshot_schema_id")))
        (is (= ["000001" "000002"]
               (mapv #(get % "work_id")
                     (get-in snapshot ["snapshot_identity_object"
                                       "snapshot_inputs"]))))
        (doseq [[work manifest-file manifest] (map vector works manifest-files manifests)]
          (testing (:slug work)
            (is (.exists manifest-file))
            (is (nil? (schema/validation-errors manifest-schema manifest)))
            (is (= "source" (get manifest "artifact_kind")))
            (is (= snapshot-hash
                   (get-in manifest ["manifest_identity_object"
                                     "corpus_snapshot_hash"])))
            (is (= (get-in (files/read-json (:parser_ir_path work))
                           ["source" "work_content_hash"])
                   (get-in manifest ["manifest_identity_object"
                                     "work_content_hash"])))
            (is (= (metadata-record/record-hash
                    (files/read-json (:metadata_record_path work)))
                   (get-in manifest ["manifest_identity_object"
                                     "metadata_record_hash"])))
            (is (= "index-entry"
                   (get-in manifest ["sidecars" 0 "role"])))
            (is (= "../snapshot.json"
                   (get-in manifest ["sidecars" 0 "path_hint"]))))))
      (finally
        (delete-tree! root)))))

(deftest materialized-source-snapshot-is-deterministic-test
  (let [root (temp-dir "abc-source-snapshot-deterministic")]
    (try
      (let [work (fixture-work! root {:slug "one"
                                      :title "一"
                                      :work-id "000001"
                                      :person-id "000101"
                                      :work-hash (example-hash "a1")})
            workset-file (workset! root [work])
            out-a (io/file root "a" "snapshot.json")]
        (materialize/materialize-source-snapshot!
         {:workset-path (str workset-file)
          :output-path (str out-a)
          :generated-at generated-at})
        (let [snapshot-a (slurp out-a)
              manifest-a (slurp (:source_manifest_path work))]
          (materialize/materialize-source-snapshot!
           {:workset-path (str workset-file)
            :output-path (str out-a)
            :generated-at generated-at})
          (is (= snapshot-a (slurp out-a)))
          (is (= manifest-a
                 (slurp (:source_manifest_path work))))))
      (finally
        (delete-tree! root)))))
