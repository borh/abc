(ns abc.tools.parser-ir-schema-evidence-test
  (:require [abc.tools.files :as files]
            [abc.tools.json :as abc-json]
            [abc.tools.materialize-publication :as materialize]
            [abc.tools.parser-ir-plaintext :as plaintext]
            [abc.tools.parser-ir-tei :as tei]
            [abc.tools.schema :as schema]
            [clojure.java.io :as io]
            [clojure.string :as string]
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

(defn- parser-ir-with-ruby-direction [direction]
  (-> (files/read-json "examples/v0/example-work/parser-ir.json")
      (assoc "nodes" [{"type" "ruby"
                       "span" {"start" 0 "end" 6
                               "coordinate_system" "parser_text_utf8"}
                       "ruby" {"base" "本文"
                               "reading" "ほんぶん"
                               "scope" "explicit"
                               "direction" direction}}]
             "paragraphs" []
             "sentences" [])
      (dissoc "sentence_segmentation" "orthographic_annotations")))

(deftest parser-text-and-source-coordinate-shapes-are-distinct-test
  (let [parser-schema (files/read-json parser-ir-schema-path)
        span (fixture-span "span-current.json")]
    (is (= "parser_text_utf8" (get span "coordinate_system")))
    (is (nil? (schema/validation-errors parser-schema (parser-ir-with-span span))))
    (doseq [invalid [(dissoc span "coordinate_system")
                     (assoc span "coordinate_system" "decoded_utf8")
                     (assoc span "line" 1)
                     (fixture-span "span-invalid-coordinate.json")]]
      (is (seq (schema/validation-errors parser-schema (parser-ir-with-span invalid)))))
    (let [ir (assoc-in (parser-ir-with-span span) ["nodes" 0 "source_span"]
                       {"start" 24 "end" 39 "line" 4 "coordinate_system" "decoded_utf8"})]
      (is (nil? (schema/validation-errors parser-schema ir)))
      (let [records (get (materialize/publication-preservation
                          {:parser-ir ir
                           :source-manifest (files/read-json "examples/v0/example-work/source.manifest.json")
                           :generated-at generated-at}) "records")]
        (is (some #(and (= "/nodes/0/source_span" (get % "ir_pointer"))
                        (= (pr-str (get-in ir ["nodes" 0 "source_span"])) (get % "value"))) records)))
      (is (seq (schema/validation-errors parser-schema
                                         (assoc-in ir ["nodes" 0 "source_span" "coordinate_system"] "parser_text_utf8")))))))

(deftest complete-parser-ir-envelopes-accept-supported-ruby-directions-test
  (let [parser-schema (files/read-json parser-ir-schema-path)]
    (doseq [direction ["left" "right" nil]]
      (is (nil? (schema/validation-errors
                 parser-schema
                 (parser-ir-with-ruby-direction direction)))
          (str "complete parser-IR envelope accepts ruby direction "
               (pr-str direction))))
    (is (seq (schema/validation-errors
              parser-schema
              (parser-ir-with-ruby-direction "horizontal")))
        "a direction outside the ruby enum is rejected")))

(deftest committed-publication-fixture-consumes-parser-text-utf8-spans-test
  (let [parser-ir (files/read-json "examples/v0/example-work/parser-ir.json")
        projected-nodes (->> (get parser-ir "nodes")
                             (keep-indexed
                              (fn [index node]
                                (when (= "parser_text_utf8"
                                         (get-in node ["span" "coordinate_system"]))
                                  [index node]))))
        plaintext-output (plaintext/render-string parser-ir)
        tei-output (pr-str (:body (tei/render parser-ir)))
        preservation-records (get (materialize/publication-preservation
                                   {:parser-ir parser-ir
                                    :source-manifest
                                    (files/read-json
                                     "examples/v0/example-work/source.manifest.json")
                                    :generated-at generated-at})
                                  "records")]
    (is (= (count (get parser-ir "nodes")) (count projected-nodes)))
    (is (string/includes? plaintext-output "第二段")
        "visible text selected by a parser text span reaches plaintext")
    (is (string/includes? tei-output "第二段")
        "visible text selected by a parser text span reaches TEI")
    (is (not (string/includes? plaintext-output "古伝説"))
        "source-note source-note text remains outside visible plaintext")
    (doseq [[index node] projected-nodes]
      (is (some #(and (= (str "/nodes/" index "/span")
                         (get % "ir_pointer"))
                      (= (pr-str (get node "span")) (get % "value")))
                preservation-records)
          (str "parser text span for node " index
               " is preserved with its parser-IR pointer")))))

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
