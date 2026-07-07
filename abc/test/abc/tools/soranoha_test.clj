(ns abc.tools.soranoha-test
  (:require [abc.tools.files :as files]
            [abc.tools.json :as abc-json]
            [abc.tools.manifest :as manifest]
            [abc.tools.request-set-resolver :as resolver]
            [abc.tools.snapshot-index :as snapshot-index]
            [abc.tools.soranoha :as soranoha]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is]]))

(defn- temp-json-path [prefix]
  (let [file (java.io.File/createTempFile prefix ".json")]
    (.delete file)
    (str file)))

(defn- temp-dir [prefix]
  (.toFile (java.nio.file.Files/createTempDirectory prefix
                                                    (make-array
                                                     java.nio.file.attribute.FileAttribute
                                                     0))))

(defn- delete-tree! [file]
  (let [file (io/file file)]
    (when (.exists file)
      (doseq [entry (reverse (file-seq file))]
        (.delete entry)))))

(defn- example-hash [suffix]
  (files/example-hash suffix))

(defn- source-snapshot-parser-ir [work-hash]
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

(defn- source-snapshot-metadata-record [work-id title person-id]
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
           "card_url" (str "https://www.aozora.gr.jp/cards/"
                           person-id
                           "/card"
                           work-id
                           ".html")
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

(defn- source-snapshot-work! [root {:keys [slug title work-id person-id
                                           work-hash]}]
  (let [work-dir (io/file root "works" slug)]
    (abc-json/write-deterministic-json-file! (io/file work-dir "aat.json")
                                             {"version" 1
                                              "work_id" work-id
                                              "blocks" []})
    (abc-json/write-deterministic-json-file!
     (io/file work-dir "parser-ir.json")
     (source-snapshot-parser-ir work-hash))
    (abc-json/write-deterministic-json-file!
     (io/file work-dir "metadata-record.json")
     (source-snapshot-metadata-record work-id title person-id))
    work-dir))

(deftest list-request-sets-prints-checked-in-labels-test
  (let [out (with-out-str
              (is (zero? (soranoha/run! ["list-request-sets"]))))]
    (is (string/includes? out "smoke-basic-ja"))
    (is (string/includes? out "full-corpus-basic-ja"))))

(deftest explain-request-set-prints-request-set-id-test
  (let [resolved (resolver/resolve-request-set "smoke-basic-ja")
        out (with-out-str
              (is (zero? (soranoha/run! ["explain-request-set"
                                         "smoke-basic-ja"]))))]
    (is (string/includes? out "smoke-basic-ja"))
    (is (string/includes? out (get resolved "request_set_id")))
    (is (not (string/includes? out "fixture_role")))))

(deftest snapshot-index-command-generates-index-from-plan-test
  (let [output-path (temp-json-path "abc-soranoha-snapshot-index")
        request-set (resolver/resolve-request-set "smoke-basic-ja")
        out (with-out-str
              (is (zero? (soranoha/run! ["snapshot-index"
                                         "smoke-basic-ja"
                                         output-path]))))
        snapshot (files/read-json output-path)]
    (try
      (is (string/includes? out output-path))
      (is (= (get request-set "request_set_id")
             (get-in snapshot ["snapshot_index_identity_object"
                               "request_set_id"])))
      (is (= (manifest/schema-hash "schemas/snapshot-index.schema.json")
             (get snapshot "schema_hash")))
      (is (true? (snapshot-index/validate-snapshot-index! snapshot)))
      (finally
        (.delete (io/file output-path))))))

(deftest explain-snapshot-command-validates-and-prints-identity-test
  (let [output-path (temp-json-path "abc-soranoha-explain-snapshot")]
    (try
      (with-out-str
        (is (zero? (soranoha/run! ["snapshot-index" "smoke-basic-ja" output-path]))))
      (let [snapshot (files/read-json output-path)
            out (with-out-str
                  (is (zero? (soranoha/run! ["explain-snapshot"
                                             output-path]))))]
        (is (string/includes? out (get snapshot "snapshot_label")))
        (is (string/includes? out (get snapshot "snapshot_identity_hash")))
        (is (string/includes? out "failure_rate:")))
      (finally
        (.delete (io/file output-path))))))

(deftest validate-command-accepts-snapshot-root-test
  (let [root (io/file "target/soranoha/smoke-basic-ja")]
    (try
      (delete-tree! root)
      (with-out-str
        (is (zero? (soranoha/run! ["reproduce" "smoke-basic-ja"]))))
      (let [out (with-out-str
                  (is (zero? (soranoha/run! ["validate" (str root)]))))]
        (is (string/includes? out "snapshot_valid: true")))
      (finally
        (delete-tree! root)))))

(deftest validate-command-rejects-missing-referenced-manifest-test
  (let [root (io/file "target/soranoha/smoke-basic-ja")
        missing-manifest (io/file root
                                  "artifacts/analysis/analysis.manifest.json")]
    (try
      (delete-tree! root)
      (with-out-str
        (is (zero? (soranoha/run! ["reproduce" "smoke-basic-ja"]))))
      (is (.delete missing-manifest))
      (let [err (java.io.StringWriter.)]
        (binding [*err* err]
          (is (= 1 (soranoha/run! ["validate" (str root)]))))
        (is (string/includes? (str err)
                              "Referenced snapshot manifest does not exist")))
      (finally
        (delete-tree! root)))))

(deftest reproduce-command-writes-default-smoke-snapshot-index-test
  (let [root (io/file "target/soranoha/smoke-basic-ja")
        output-path (io/file root "snapshot-index.json")
        expected-files ["artifacts/parser-ir/parser-ir.json"
                        "artifacts/parser-ir/parser-ir.manifest.json"
                        "artifacts/plaintext/plain.txt"
                        "artifacts/plaintext/plaintext.manifest.json"
                        "artifacts/tei/tei.xml"
                        "artifacts/tei/tei.manifest.json"
                        "artifacts/analysis/analysis-result.json"
                        "artifacts/analysis/analysis.manifest.json"]]
    (try
      (delete-tree! root)
      (let [out (with-out-str
                  (is (zero? (soranoha/run! ["reproduce" "smoke-basic-ja"]))))]
        (is (.exists output-path))
        (doseq [path expected-files]
          (is (.exists (io/file root path))
              (str path " should be materialized into the smoke snapshot root")))
        (is (string/includes? out (str output-path)))
        (let [snapshot (files/read-json output-path)
              references (get snapshot "artifact_references")
              reference-kinds (set (map #(get % "artifact_kind") references))
              locators (set (map #(get-in % ["locator" "path"]) references))]
          (is (true? (snapshot-index/validate-snapshot-index! snapshot)))
          (is (contains? reference-kinds "parser-ir"))
          (is (contains? reference-kinds "plaintext"))
          (is (contains? reference-kinds "tei"))
          (is (contains? reference-kinds "analysis"))
          (is (contains? locators "artifacts/analysis/analysis.manifest.json"))))
      (finally
        (delete-tree! root)))))

(deftest reproduce-command-materializes-demo-request-set-test
  (let [root (io/file "target/soranoha/demo-basic-ja")
        output-path (io/file root "snapshot-index.json")
        expected-work-hashes #{"sha256:2323232323232323232323232323232323232323232323232323232323232323"
                               "sha256:2424242424242424242424242424242424242424242424242424242424242424"}
        expected-files ["artifacts/works/demo-fixture-a/parser-ir/parser-ir.manifest.json"
                        "artifacts/works/demo-fixture-a/plaintext/plaintext.manifest.json"
                        "artifacts/works/demo-fixture-a/tei/tei.manifest.json"
                        "artifacts/works/demo-fixture-a/analysis/analysis.manifest.json"
                        "artifacts/works/demo-fixture-b/parser-ir/parser-ir.manifest.json"
                        "artifacts/works/demo-fixture-b/plaintext/plaintext.manifest.json"
                        "artifacts/works/demo-fixture-b/tei/tei.manifest.json"
                        "artifacts/works/demo-fixture-b/analysis/analysis.manifest.json"]]
    (try
      (delete-tree! root)
      (with-out-str
        (is (zero? (soranoha/run! ["reproduce" "demo-basic-ja"]))))
      (is (.exists output-path))
      (doseq [path expected-files]
        (is (.exists (io/file root path))
            (str path " should be materialized into the demo snapshot root")))
      (when (.exists output-path)
        (let [snapshot (files/read-json output-path)
              references (get snapshot "artifact_references")
              reference-kinds (frequencies (map #(get % "artifact_kind")
                                                references))
              manifest-work-hashes (set
                                    (for [path expected-files]
                                      (get-in (files/read-json
                                               (io/file root path))
                                              ["manifest_identity_object"
                                               "work_content_hash"])))]
          (is (true? (snapshot-index/validate-snapshot-index! snapshot)))
          (is (= {"parser-ir" 2
                  "plaintext" 2
                  "tei" 2
                  "analysis" 2}
                 reference-kinds))
          (is (= expected-work-hashes manifest-work-hashes))
          (with-out-str
            (is (zero? (soranoha/run! ["validate" (str root)]))))))
      (finally
        (delete-tree! root)))))

(deftest source-snapshot-command-generates-workset-and-snapshot-test
  (let [root (temp-dir "abc-soranoha-source-snapshot")
        input-root (io/file root "materialized")
        output-root (io/file root "source-snapshot")]
    (try
      (source-snapshot-work! input-root {:slug "alpha"
                                         :title "一"
                                         :work-id "000001"
                                         :person-id "000101"
                                         :work-hash (example-hash "a1")})
      (let [out (with-out-str
                  (is (zero? (soranoha/run!
                              ["source-snapshot"
                               (str input-root)
                               (str output-root)
                               "unit-test-source-snapshot"
                               "2026-07-07"]))))
            workset-file (io/file output-root "source-snapshot.workset.edn")
            snapshot-file (io/file output-root "source-snapshot.json")
            source-manifest (io/file input-root
                                     "works"
                                     "alpha"
                                     "source.manifest.json")]
        (is (.exists workset-file))
        (is (.exists snapshot-file))
        (is (.exists source-manifest))
        (let [snapshot (files/read-json snapshot-file)]
          (is (= "unit-test-source-snapshot"
                 (get-in snapshot ["snapshot_identity_object"
                                   "snapshot_scope"])))
          (is (= "source"
                 (get (files/read-json source-manifest)
                      "artifact_kind")))
          (is (string/includes? out (str snapshot-file)))
          (is (string/includes? out (get snapshot "snapshot_hash")))))
      (finally
        (delete-tree! root)))))

(deftest unknown-command-returns-nonzero-test
  (let [err (java.io.StringWriter.)]
    (binding [*err* err]
      (is (= 2 (soranoha/run! ["nope"]))))
    (is (string/includes? (str err) "unknown command"))))
