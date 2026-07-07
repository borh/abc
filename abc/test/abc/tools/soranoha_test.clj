(ns abc.tools.soranoha-test
  (:require [abc.tools.files :as files]
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

(deftest unknown-command-returns-nonzero-test
  (let [err (java.io.StringWriter.)]
    (binding [*err* err]
      (is (= 2 (soranoha/run! ["nope"]))))
    (is (string/includes? (str err) "unknown command"))))
