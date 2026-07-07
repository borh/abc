(ns abc.tools.source-snapshot-workset-test
  (:require [abc.tools.files :as files]
            [abc.tools.json :as abc-json]
            [abc.tools.manifest :as manifest]
            [abc.tools.materialize-source-snapshot :as materialize]
            [abc.tools.source-snapshot-workset :as workset]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

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
  (let [work-dir (io/file root "works" slug)
        aat-file (io/file work-dir "aat.json")
        parser-file (io/file work-dir "parser-ir.json")
        metadata-file (io/file work-dir "metadata-record.json")]
    (abc-json/write-deterministic-json-file! aat-file
                                             {"version" 1
                                              "work_id" work-id
                                              "blocks" []})
    (abc-json/write-deterministic-json-file! parser-file (parser-ir work-hash))
    (abc-json/write-deterministic-json-file! metadata-file
                                             (metadata-record work-id title person-id))
    work-dir))

(deftest workset-from-materialized-root-test
  (let [root (temp-dir "abc-source-snapshot-workset")
        output (io/file root "workset.edn")]
    (try
      (fixture-work! root {:slug "zeta"
                           :title "二"
                           :work-id "000002"
                           :person-id "000102"
                           :work-hash (example-hash "a2")})
      (fixture-work! root {:slug "alpha"
                           :title "一"
                           :work-id "000001"
                           :person-id "000101"
                           :work-hash (example-hash "a1")})
      (let [result (workset/write-workset!
                    {:input-root (str root)
                     :output-path (str output)
                     :snapshot-scope "unit-test-generated-source-snapshot"
                     :snapshot-date "2026-07-07"})
            generated (edn/read-string (slurp output))]
        (is (= output (:output result)))
        (is (= 2 (:works-count result)))
        (is (= "unit-test-generated-source-snapshot"
               (:snapshot_scope generated)))
        (is (= "2026-07-07"
               (:snapshot_date generated)))
        (is (= ["000001" "000002"]
               (mapv :work_id (:works generated))))
        (is (= ["alpha" "zeta"]
               (mapv :slug (:works generated))))
        (testing "work entries are relative to the output workset"
          (is (= [{:slug "alpha"
                   :title "一"
                   :work_id "000001"
                   :person_id "000101"
                   :card_url "https://www.aozora.gr.jp/cards/000101/card000001.html"
                   :aat_path "works/alpha/aat.json"
                   :parser_ir_path "works/alpha/parser-ir.json"
                   :metadata_record_path "works/alpha/metadata-record.json"
                   :source_manifest_path "works/alpha/source.manifest.json"}
                  {:slug "zeta"
                   :title "二"
                   :work_id "000002"
                   :person_id "000102"
                   :card_url "https://www.aozora.gr.jp/cards/000102/card000002.html"
                   :aat_path "works/zeta/aat.json"
                   :parser_ir_path "works/zeta/parser-ir.json"
                   :metadata_record_path "works/zeta/metadata-record.json"
                   :source_manifest_path "works/zeta/source.manifest.json"}]
                 (:works generated)))))
      (finally
        (delete-tree! root)))))

(deftest missing-required-file-is-rejected-test
  (let [root (temp-dir "abc-source-snapshot-workset-missing")]
    (try
      (let [work-dir (io/file root "works" "broken")]
        (abc-json/write-deterministic-json-file! (io/file work-dir "aat.json")
                                                 {"version" 1})
        (abc-json/write-deterministic-json-file! (io/file work-dir "metadata-record.json")
                                                 (metadata-record "000001"
                                                                  "一"
                                                                  "000101"))
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"missing parser-ir.json"
             (workset/workset-from-root
              {:input-root (str root)
               :snapshot-scope "unit-test"
               :snapshot-date "2026-07-07"}))))
      (finally
        (delete-tree! root)))))

(deftest generated-workset-materializes-source-snapshot-test
  (let [root (temp-dir "abc-source-snapshot-workset-materialize")
        workset-file (io/file root "workset.edn")
        snapshot-file (io/file root "snapshot.json")]
    (try
      (fixture-work! root {:slug "alpha"
                           :title "一"
                           :work-id "000001"
                           :person-id "000101"
                           :work-hash (example-hash "a1")})
      (workset/write-workset!
       {:input-root (str root)
        :output-path (str workset-file)
        :snapshot-scope "unit-test-generated-source-snapshot"
        :snapshot-date "2026-07-07"})
      (let [result (materialize/materialize-source-snapshot!
                    {:workset-path (str workset-file)
                     :output-path (str snapshot-file)})
            snapshot (files/read-json snapshot-file)
            source-manifest (io/file root "works" "alpha" "source.manifest.json")]
        (is (= snapshot-file (:snapshot result)))
        (is (= "works/alpha/aat.json"
               (get-in snapshot ["snapshot_identity_object"
                                 "snapshot_inputs"
                                 0
                                 "aat_path"])))
        (is (.exists source-manifest))
        (is (= "source"
               (get (files/read-json source-manifest)
                    "artifact_kind"))))
      (finally
        (delete-tree! root)))))
