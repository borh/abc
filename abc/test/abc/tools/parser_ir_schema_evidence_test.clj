(ns abc.tools.parser-ir-schema-evidence-test
  (:require [abc.tools.adr-evidence-runtime-inputs :as runtime-inputs]
            [abc.tools.files :as files]
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

(deftest parser-ir-schema-regression-contract
  (runtime-inputs/with-validated-read-trace!
    {:identity-root ".." :cwd-root "." :repo-root "." :workspace-root ".."
     :descriptor {:path "abc/docs/evidence/adr-capture/parser-ir-schema-regression.edn"
                  :value (update (files/read-edn "docs/evidence/adr-capture/parser-ir-schema-regression.edn")
                                 :runtime-input-manifest #(str "abc/" %))}}
    (fn []
      (doseq [path ["examples/ab-validator-output/manifest-inputs.json" "examples/v0/example-work/manifest.json"
                    "examples/v0/example-work/metadata-record.json" "examples/v0/example-work/parser-ir.json"
                    "examples/v0/example-work/source.manifest.json" "fixtures/parser-ir/span-current.json"
                    "fixtures/parser-ir/span-invalid-coordinate.json" "fixtures/parser-ir/span-legacy-line-column.json"
                    "fixtures/parser-ir/span-legacy-start-end.json" "schemas/manifest.schema.json" "schemas/parser-ir.schema.json"]]
        (files/read-text path))
      (is true))))

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
                               "coordinate_system" "decoded_utf8"}
                       "ruby" {"base" "本文"
                               "reading" "ほんぶん"
                               "scope" "explicit"
                               "direction" direction}}]
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

(deftest committed-publication-fixture-consumes-decoded-utf8-spans-test
  (let [parser-ir (files/read-json "examples/v0/example-work/parser-ir.json")
        decoded-nodes (->> (get parser-ir "nodes")
                           (keep-indexed
                            (fn [index node]
                              (when (= "decoded_utf8"
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
    (is (= [[4 "第二段"] [5 "（古伝説と、シルレルの詩から。）"]]
           (mapv (fn [[index node]] [index (get node "text")]) decoded-nodes)))
    (is (string/includes? plaintext-output "第二段")
        "visible text selected by a decoded UTF-8 span reaches plaintext")
    (is (string/includes? tei-output "第二段")
        "visible text selected by a decoded UTF-8 span reaches TEI")
    (is (not (string/includes? plaintext-output "古伝説"))
        "decoded-span source-note text remains outside visible plaintext")
    (doseq [[index node] decoded-nodes]
      (is (some #(and (= (str "/nodes/" index "/span")
                         (get % "ir_pointer"))
                      (= (pr-str (get node "span")) (get % "value")))
                preservation-records)
          (str "decoded UTF-8 span for node " index
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
