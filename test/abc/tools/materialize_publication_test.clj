(ns abc.tools.materialize-publication-test
  (:require [abc.tools.files :as files]
            [abc.tools.materialize-publication :as materialize]
            [abc.tools.parser-ir-publication-policy :as policy]
            [abc.tools.schema :as schema]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def generated-at "2026-07-03T00:00:00Z")

(defn- temp-dir [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- delete-tree! [dir]
  (when dir
    (doseq [f (reverse (file-seq dir))]
      (.delete f))))

(defn- materialize-example! [out-dir]
  (materialize/materialize-publication!
   {:parser-ir-path "examples/v0/example-work/parser-ir.json"
    :source-manifest-path "examples/v0/example-work/source.manifest.json"
    :metadata-record-path "examples/v0/example-work/metadata-record.json"
    :persons-dir "examples/v0/example-persons"
    :output-dir out-dir
    :generated-at generated-at}))

(deftest manifest-schema-accepts-plaintext-artifact-kind-test
  (let [manifest-schema (files/read-json "schemas/manifest.schema.json")
        example-manifest (files/read-json "examples/v0/example-work/manifest.json")]
    (testing "plaintext is an allowed artifact kind"
      (is (nil? (schema/validation-errors
                 manifest-schema
                 (assoc example-manifest "artifact_kind" "plaintext")))))
    (testing "unknown artifact kinds are still rejected"
      (is (seq (schema/validation-errors
                manifest-schema
                (assoc example-manifest "artifact_kind" "not-a-kind")))))))

(deftest materialize-publication-test
  (let [out-dir (temp-dir "abc-materialize-publication")
        out-file out-dir]
    (try
      (let [result (materialize-example! out-file)
            plain-file (io/file out-file "plain.txt")
            tei-file (io/file out-file "tei.xml")
            plaintext-manifest-file (io/file out-file "plaintext.manifest.json")
            tei-manifest-file (io/file out-file "tei.manifest.json")
            tei-validation-result-file (io/file out-file "tei-validation-result.json")
            manifest-schema (files/read-json "schemas/manifest.schema.json")
            validation-result-schema (files/read-json "schemas/tei-validation-result.schema.json")
            plaintext-manifest (files/read-json plaintext-manifest-file)
            tei-manifest (files/read-json tei-manifest-file)
            tei-validation-result (files/read-json tei-validation-result-file)
            source-manifest (files/read-json "examples/v0/example-work/source.manifest.json")
            source-corpus-hash (get-in source-manifest
                                       ["manifest_identity_object"
                                        "corpus_snapshot_hash"])]
        (is (= {:plaintext plain-file
                :tei tei-file
                :plaintext-manifest plaintext-manifest-file
                :tei-manifest tei-manifest-file
                :tei-validation-result tei-validation-result-file}
               result))
        (doseq [file [plain-file tei-file plaintext-manifest-file
                      tei-manifest-file tei-validation-result-file]]
          (is (.exists file) (str file " should exist")))
        (is (string/includes? (slurp plain-file) "吾輩猫"))
        (is (re-find #"<(?:[A-Za-z0-9_-]+:)?ruby(?:\s|>)"
                     (slurp tei-file)))
        (is (nil? (schema/validation-errors manifest-schema plaintext-manifest)))
        (is (nil? (schema/validation-errors manifest-schema tei-manifest)))
        (is (nil? (schema/validation-errors
                   validation-result-schema
                   tei-validation-result)))
        (is (= "plaintext" (get plaintext-manifest "artifact_kind")))
        (is (= "tei" (get tei-manifest "artifact_kind")))
        (is (= "passed" (get tei-validation-result "status")))
        (is (= "passed" (get tei-manifest "validation_status")))
        (is (= source-corpus-hash
               (get-in plaintext-manifest
                       ["manifest_identity_object" "corpus_snapshot_hash"])))
        (is (= source-corpus-hash
               (get-in tei-manifest
                       ["manifest_identity_object" "corpus_snapshot_hash"])))
        (is (= (policy/policy-hash "data/parser-ir-publication-policy-v0.json")
               (get-in plaintext-manifest
                       ["manifest_identity_object" "output_format_spec_hash"]))))
      (finally
        (delete-tree! out-dir)))))

(deftest materialized-publication-output-is-deterministic-test
  (let [out-dir-a (temp-dir "abc-materialize-publication-a")
        out-dir-b (temp-dir "abc-materialize-publication-b")]
    (try
      (materialize-example! out-dir-a)
      (materialize-example! out-dir-b)
      (doseq [name ["plain.txt"
                    "tei.xml"
                    "plaintext.manifest.json"
                    "tei.manifest.json"
                    "tei-validation-result.json"]]
        (is (= (slurp (io/file out-dir-a name))
               (slurp (io/file out-dir-b name)))
            (str name " should be deterministic")))
      (finally
        (delete-tree! out-dir-a)
        (delete-tree! out-dir-b)))))
