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
  (let [root (temp-dir "abc-soranoha-snapshot-root")
        output-path (str (io/file root "snapshot-index.json"))]
    (try
      (with-out-str
        (is (zero? (soranoha/run! ["snapshot-index" "smoke-basic-ja" output-path]))))
      (let [out (with-out-str
                  (is (zero? (soranoha/run! ["validate" (str root)]))))]
        (is (string/includes? out "snapshot_valid: true")))
      (finally
        (.delete (io/file output-path))
        (.delete root)))))

(deftest reproduce-command-writes-default-smoke-snapshot-index-test
  (let [output-path (io/file "target/soranoha/smoke-basic-ja/snapshot-index.json")]
    (try
      (when (.exists output-path)
        (.delete output-path))
      (let [out (with-out-str
                  (is (zero? (soranoha/run! ["reproduce" "smoke-basic-ja"]))))]
        (is (.exists output-path))
        (is (string/includes? out (str output-path)))
        (is (true? (snapshot-index/validate-snapshot-index!
                    (files/read-json output-path)))))
      (finally
        (when (.exists output-path)
          (.delete output-path))))))

(deftest unknown-command-returns-nonzero-test
  (let [err (java.io.StringWriter.)]
    (binding [*err* err]
      (is (= 2 (soranoha/run! ["nope"]))))
    (is (string/includes? (str err) "unknown command"))))
