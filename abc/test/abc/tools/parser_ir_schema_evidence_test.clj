(ns abc.tools.parser-ir-schema-evidence-test
  (:require [abc.tools.files :as files]
            [abc.tools.json :as abc-json]
            [abc.tools.materialize-publication :as materialize]
            [abc.tools.parser-ir-plaintext :as plaintext]
            [abc.tools.parser-ir-tei :as tei]
            [abc.tools.schema :as schema]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private parser-ir-schema-path "schemas/parser-ir.schema.json")
(def ^:private fixture-dir "fixtures/parser-ir")
(def ^:private generated-at "2026-07-03T00:00:00Z")

(defn- temp-dir [prefix]
  (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0))))

(defn- delete-tree! [dir]
  (when dir
    (doseq [file (reverse (file-seq dir))]
      (.delete file))))

(defn- fixture-span [name]
  (files/read-json (io/file fixture-dir name)))

(defn- parser-ir-with-span [span]
  (-> (files/read-json "examples/v0/example-work/parser-ir.json")
      (assoc "nodes" [{"type" "text" "span" span "text" "本文"}]
             "paragraphs" []
             "sentences" [])
      (dissoc "sentence_segmentation" "orthographic_annotations")))

(deftest bounded-span-shapes-remain-compatible-test
  (let [parser-schema (files/read-json parser-ir-schema-path)]
    (testing "current decoded UTF-8 spans carry bounded line and column coordinates"
      (let [span (fixture-span "span-current.json")]
        (is (= "decoded_utf8" (get span "coordinate_system")))
        (is (pos-int? (get span "column")))
        (is (nil? (schema/validation-errors parser-schema
                                            (parser-ir-with-span span))))))
    (testing "legacy bounded span shapes remain accepted"
      (doseq [name ["span-legacy-start-end.json"
                    "span-legacy-line-column.json"]]
        (is (nil? (schema/validation-errors parser-schema
                                            (parser-ir-with-span
                                             (fixture-span name))))
            name)))
    (testing "unknown coordinate systems are rejected"
      (is (seq (schema/validation-errors
                parser-schema
                (parser-ir-with-span
                 (fixture-span "span-invalid-coordinate.json"))))))))

(deftest committed-publication-fixture-consumes-decoded-utf8-spans-test
  (let [parser-ir (files/read-json "examples/v0/example-work/parser-ir.json")
        decoded-spans (->> (tree-seq coll? seq parser-ir)
                           (filter map?)
                           (map #(get % "span"))
                           (filter #(= "decoded_utf8"
                                       (get % "coordinate_system"))))]
    (is (seq decoded-spans))
    (is (every? #(and (integer? (get % "start"))
                      (integer? (get % "end")))
                decoded-spans))
    (is (seq (:text (plaintext/render parser-ir))))
    (is (seq (:body (tei/render parser-ir))))))

(deftest parser-ir-schema-hash-propagates-to-materialized-manifest-test
  (let [work-dir (temp-dir "abc-parser-ir-schema-propagation")
        imported-dir (io/file work-dir "imported")
        output-dir (io/file work-dir "publication")
        mutated-schema-file (io/file work-dir "parser-ir.schema.json")]
    (try
      (.mkdirs imported-dir)
      (let [current-schema (files/read-json parser-ir-schema-path)
            current-hash (schema/schema-hash parser-ir-schema-path)
            mutated-schema (assoc current-schema "$comment"
                                  "deterministic parser-IR schema propagation fixture")]
        (abc-json/write-deterministic-json-file! mutated-schema-file mutated-schema)
        (let [mutated-hash (schema/schema-hash mutated-schema-file)
              parser-ir (assoc (files/read-json "examples/v0/example-work/parser-ir.json")
                               "schema_hash" mutated-hash)
              manifest-inputs (assoc (files/read-json
                                      "examples/ab-validator-output/manifest-inputs.json")
                                     "parser_ir_schema_hash" mutated-hash)
              parser-ir-file (io/file imported-dir "parser-ir.json")
              manifest-inputs-file (io/file imported-dir "manifest-inputs.json")]
          (is (not= current-hash mutated-hash))
          (abc-json/write-deterministic-json-file! parser-ir-file parser-ir)
          (abc-json/write-deterministic-json-file! manifest-inputs-file manifest-inputs)
          (is (= mutated-hash (get (files/read-json parser-ir-file) "schema_hash")))
          (is (= mutated-hash
                 (get (files/read-json manifest-inputs-file)
                      "parser_ir_schema_hash")))
          (materialize/materialize-publication!
           {:parser-ir-path (str parser-ir-file)
            :source-manifest-path "examples/v0/example-work/source.manifest.json"
            :metadata-record-path "examples/v0/example-work/metadata-record.json"
            :persons-dir "examples/v0/example-persons"
            :output-dir output-dir
            :generated-at generated-at})
          (doseq [name ["plaintext.manifest.json" "tei.manifest.json"]]
            (is (= mutated-hash
                   (get-in (files/read-json (io/file output-dir name))
                           ["manifest_identity_object"
                            "parser_ir_schema_hash"]))
                name))))
      (finally
        (delete-tree! work-dir)))))
