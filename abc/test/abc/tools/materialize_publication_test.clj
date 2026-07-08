(ns abc.tools.materialize-publication-test
  (:require [abc.tools.files :as files]
            [abc.tools.json :as abc-json]
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

(defn- tei-element-body [tei local-name]
  (second
   (re-find (re-pattern (str "(?s)<(?:[A-Za-z0-9_-]+:)?" local-name
                             "(?:\\s[^>]*)?>(.*?)</(?:[A-Za-z0-9_-]+:)?"
                             local-name ">"))
            tei)))

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
            preservation-file (io/file out-file "preservation.json")
            plaintext-manifest-file (io/file out-file "plaintext.manifest.json")
            tei-manifest-file (io/file out-file "tei.manifest.json")
            tei-validation-result-file (io/file out-file "tei-validation-result.json")
            manifest-schema (files/read-json "schemas/manifest.schema.json")
            preservation-schema (files/read-json "schemas/parser-ir-publication-preservation.schema.json")
            validation-result-schema (files/read-json "schemas/tei-validation-result.schema.json")
            plaintext-manifest (files/read-json plaintext-manifest-file)
            tei-manifest (files/read-json tei-manifest-file)
            preservation (files/read-json preservation-file)
            tei-validation-result (files/read-json tei-validation-result-file)
            source-manifest (files/read-json "examples/v0/example-work/source.manifest.json")
            source-corpus-hash (get-in source-manifest
                                       ["manifest_identity_object"
                                        "corpus_snapshot_hash"])]
        (is (= {:plaintext plain-file
                :tei tei-file
                :preservation preservation-file
                :plaintext-manifest plaintext-manifest-file
                :tei-manifest tei-manifest-file
                :tei-validation-result tei-validation-result-file}
               result))
        (doseq [file [plain-file tei-file plaintext-manifest-file
                      tei-manifest-file tei-validation-result-file
                      preservation-file]]
          (is (.exists file) (str file " should exist")))
        (is (string/includes? (slurp plain-file) "吾輩猫"))
        (is (re-find #"<(?:[A-Za-z0-9_-]+:)?ruby(?:\s|>)"
                     (slurp tei-file)))
        (let [plain-text (slurp plain-file)
              tei-text (slurp tei-file)
              body-text (tei-element-body tei-text "body")
              back-text (tei-element-body tei-text "back")]
          (is (string/includes? tei-text "xmlns:abc=\"https://w3id.org/abc/ns/tei\""))
          (is (string/includes? tei-text "abc:vocab-version=\"0\""))
          (is (not (string/includes? tei-text "?><")))
          (is (re-find #"\n\s*<" tei-text))
          (is (= 2 (count (re-seq #"<(?:[A-Za-z0-9_-]+:)?p(?:\s|>)"
                                  body-text))))
          (is (not (string/includes? body-text "（古伝説と、シルレルの詩から。）")))
          (is (string/includes? back-text "type=\"source-attribution\""))
          (is (string/includes? back-text "（古伝説と、シルレルの詩から。）"))
          (is (not (string/includes?
                    plain-text
                    "（古伝説と、シルレルの詩から。）"))))
        (is (nil? (schema/validation-errors manifest-schema plaintext-manifest)))
        (is (nil? (schema/validation-errors manifest-schema tei-manifest)))
        (is (nil? (schema/validation-errors preservation-schema preservation)))
        (is (nil? (schema/validation-errors
                   validation-result-schema
                   tei-validation-result)))
        (is (= "https://w3id.org/abc/schemas/parser-ir-publication-preservation.schema.json"
               (get preservation "schema_id")))
        (is (= "0.3.0" (get preservation "schema_version")))
        (is (= source-corpus-hash
               (get-in preservation ["source" "corpus_snapshot_hash"])))
        (is (pos? (get-in preservation ["coverage" "record_count"])))
        (is (some #{"custom_sidecar"}
                  (get-in preservation ["coverage" "classes"])))
        (is (some #{"tei_profile_projection"}
                  (get-in preservation ["coverage" "classes"])))
        (is (some #(= "paragraph.node_range" (get % "construct"))
                  (get preservation "records")))
        (is (some #(= "tei_profile_projection" (get % "class"))
                  (get preservation "records")))
        (is (some #(and (= "tei_profile_projection" (get % "class"))
                        (= "heading_jisage_structure" (get % "construct")))
                  (get preservation "records")))
        ;; B3: sentence-segmentation provenance is recorded in the sidecar.
        (is (some #(and (= "sentence_segmentation" (get % "construct"))
                        (= "/sentence_segmentation" (get % "ir_pointer")))
                  (get preservation "records")))
        (is (= "plaintext" (get plaintext-manifest "artifact_kind")))
        (is (= "tei" (get tei-manifest "artifact_kind")))
        (is (some #(and (= "preservation" (get % "role"))
                        (= "preservation.json" (get % "path_hint")))
                  (get tei-manifest "sidecars")))
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

(deftest materialize-publication-batch-test
  (let [out-dir (temp-dir "abc-materialize-publication-batch")
        batch-file (io/file out-dir "batch.json")
        summary-file (io/file out-dir "summary.json")
        out-a (io/file out-dir "a")
        out-b (io/file out-dir "b")]
    (try
      (abc-json/write-deterministic-json-file!
       batch-file
       {"jobs" [{"id" "a"
                 "parser_ir_path" "examples/v0/example-work/parser-ir.json"
                 "source_manifest_path" "examples/v0/example-work/source.manifest.json"
                 "metadata_record_path" "examples/v0/example-work/metadata-record.json"
                 "persons_dir" "examples/v0/example-persons"
                 "output_dir" (str out-a)
                 "generated_at" generated-at}
                {"id" "b"
                 "parser_ir_path" "examples/v0/example-work/parser-ir.json"
                 "source_manifest_path" "examples/v0/example-work/source.manifest.json"
                 "metadata_record_path" "examples/v0/example-work/metadata-record.json"
                 "persons_dir" "examples/v0/example-persons"
                 "output_dir" (str out-b)
                 "generated_at" generated-at}]})
      (let [summary (materialize/materialize-publications-batch!
                     {:batch-path (str batch-file)
                      :summary-path (str summary-file)
                      :jobs 2})]
        (is (= {"jobs_total" 2
                "jobs_succeeded" 2
                "jobs_failed" 0
                "jobs_concurrency" 2}
               (select-keys summary ["jobs_total" "jobs_succeeded" "jobs_failed" "jobs_concurrency"])))
        (is (= ["a" "b"] (mapv #(get % "id") (get summary "jobs"))))
        (is (= ["passed" "passed"] (mapv #(get % "status") (get summary "jobs"))))
        (is (= "workflow-run.json" (get summary "workflow_run_path")))
        (let [workflow-run-file (io/file (.getParentFile summary-file)
                                         "workflow-run.json")
              workflow-run (files/read-json workflow-run-file)
              workflow-run-schema (files/read-json
                                   "schemas/workflow-run.schema.json")]
          (is (.isFile workflow-run-file))
          (is (nil? (schema/validation-errors workflow-run-schema workflow-run)))
          (is (= "soranoha.materialize-publications-batch.v1"
                 (get workflow-run "workflow_id")))
          (is (= (get summary "jobs_total")
                 (get workflow-run "step_count")))
          (is (= (get summary "jobs_failed")
                 (get workflow-run "steps_failed")))
          (is (every? #{"passed" "partial" "failed" "skipped"}
                      (map #(get % "status") (get workflow-run "steps")))))
        (is (= summary (files/read-json summary-file)))
        (doseq [dir [out-a out-b]]
          (is (.exists (io/file dir "plain.txt")))
          (is (.exists (io/file dir "tei.xml")))
          (is (= "passed"
                 (get (files/read-json (io/file dir "tei-validation-result.json"))
                      "status")))))
      (finally
        (delete-tree! out-dir)))))

(deftest materialize-publication-declares-orthographic-sentence-normalization-test
  (let [work-dir (temp-dir "abc-materialize-publication-sentences")
        out-dir (io/file work-dir "out")
        parser-ir-file (io/file work-dir "parser-ir.json")]
    (try
      (abc-json/write-deterministic-json-file!
       parser-ir-file
       {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
        "schema_hash" "sha256:0b495bb5c12c4d76482afefdaedb5464a74672ffbd5282f9c67d5f419d39a340"
        "source" {"work_content_hash" (files/example-hash "11")
                  "encoding" "UTF-8"
                  "normalization" "source"}
        "nodes" [{"type" "text"
                  "span" {"start" 0 "end" 24 "coordinate_system" "decoded_utf8"}
                  "text" "吾輩ハ猫デアル。"}
                 {"type" "text"
                  "span" {"start" 24 "end" 48 "coordinate_system" "decoded_utf8"}
                  "text" "名前はまだ無い。"}]
        "paragraphs" [{"id" "p000000"
                       "span" {"start" 0 "end" 48 "coordinate_system" "decoded_utf8"}
                       "span_source" "direct"
                       "node_range" {"start" 0 "end" 2}
                       "role" "body"
                       "source_pointer" "blocks[0]"
                       "classification" "direct"}]
        "sentence_segmentation" {"schema_version" "sentence-segmentation-v1"
                                 "splitter_id" "ab-plaintext-japanese-v1"
                                 "coordinate_system" "decoded_utf8"
                                 "coverage" "body-paragraphs"}
        "sentences" [{"id" "s000000"
                      "paragraph_id" "p000000"
                      "span" {"start" 0 "end" 24 "coordinate_system" "decoded_utf8"}
                      "node_range" {"start" 0 "end" 1}
                      "tags" ["orthographic-katakana"]
                      "orthographic_annotation_indices" [0]}
                     {"id" "s000001"
                      "paragraph_id" "p000000"
                      "span" {"start" 24 "end" 48 "coordinate_system" "decoded_utf8"}
                      "node_range" {"start" 1 "end" 2}
                      "tags" []
                      "orthographic_annotation_indices" []}]
        "orthographic_annotations" {"work_id" "000000"
                                    "work_content_hash" (files/example-hash "11")
                                    "coordinate_system" "decoded_utf8"
                                    "detector_id" "HeuristicV1"
                                    "annotations" [{"source_byte_range" {"start" 0 "end" 24}
                                                    "normalized_text" "吾輩は猫である。"
                                                    "kind" "ScriptKatakanaToHiragana"
                                                    "confidence" nil}]}
        "warnings" []
        "errors" []})
      (materialize/materialize-publication!
       {:parser-ir-path (str parser-ir-file)
        :source-manifest-path "examples/v0/example-work/source.manifest.json"
        :metadata-record-path "examples/v0/example-work/metadata-record.json"
        :persons-dir "examples/v0/example-persons"
        :output-dir out-dir
        :generated-at generated-at})
      (let [tei-text (slurp (io/file out-dir "tei.xml"))]
        (is (re-find #"<(?:[A-Za-z0-9_-]+:)?normalization\s+method=\"markup\""
                     tei-text))
        (is (string/includes? tei-text "orthographic-katakana"))
        (is (= "passed"
               (get (files/read-json (io/file out-dir "tei-validation-result.json"))
                    "status"))))
      ;; B3: orthographic detector provenance is recorded in the sidecar, linking
      ;; the annotation to the sentence(s) it tags.
      (let [preservation (files/read-json (io/file out-dir "preservation.json"))
            ortho-record (some #(when (= "orthographic_annotation" (get % "construct")) %)
                               (get preservation "records"))]
        (is (some? ortho-record))
        (is (= "ScriptKatakanaToHiragana" (get ortho-record "value")))
        (is (string/includes? (get ortho-record "message") "HeuristicV1"))
        (is (string/includes? (get ortho-record "message") "s000000")))
      (finally
        (delete-tree! work-dir)))))

(deftest materialize-publication-does-not-declare-orthographic-normalization-for-plain-sentences-test
  (let [work-dir (temp-dir "abc-materialize-publication-plain-sentences")
        out-dir (io/file work-dir "out")
        parser-ir-file (io/file work-dir "parser-ir.json")]
    (try
      (abc-json/write-deterministic-json-file!
       parser-ir-file
       {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
        "schema_hash" "sha256:0b495bb5c12c4d76482afefdaedb5464a74672ffbd5282f9c67d5f419d39a340"
        "source" {"work_content_hash" (files/example-hash "11")
                  "encoding" "UTF-8"
                  "normalization" "source"}
        "nodes" [{"type" "text"
                  "span" {"start" 0 "end" 24 "coordinate_system" "decoded_utf8"}
                  "text" "吾輩は猫である。"}]
        "paragraphs" [{"id" "p000000"
                       "span" {"start" 0 "end" 24 "coordinate_system" "decoded_utf8"}
                       "span_source" "direct"
                       "node_range" {"start" 0 "end" 1}
                       "role" "body"
                       "source_pointer" "blocks[0]"
                       "classification" "direct"}]
        "sentence_segmentation" {"schema_version" "sentence-segmentation-v1"
                                 "splitter_id" "ab-plaintext-japanese-v1"
                                 "coordinate_system" "decoded_utf8"
                                 "coverage" "body-paragraphs"}
        "sentences" [{"id" "s000000"
                      "paragraph_id" "p000000"
                      "span" {"start" 0 "end" 24 "coordinate_system" "decoded_utf8"}
                      "node_range" {"start" 0 "end" 1}
                      "tags" []
                      "orthographic_annotation_indices" []}]
        "orthographic_annotations" {"work_id" "000000"
                                    "work_content_hash" (files/example-hash "11")
                                    "coordinate_system" "decoded_utf8"
                                    "detector_id" "HeuristicV1"
                                    "annotations" []}
        "warnings" []
        "errors" []})
      (materialize/materialize-publication!
       {:parser-ir-path (str parser-ir-file)
        :source-manifest-path "examples/v0/example-work/source.manifest.json"
        :metadata-record-path "examples/v0/example-work/metadata-record.json"
        :persons-dir "examples/v0/example-persons"
        :output-dir out-dir
        :generated-at generated-at})
      (let [tei-text (slurp (io/file out-dir "tei.xml"))]
        (is (not (re-find #"<(?:[A-Za-z0-9_-]+:)?normalization\s+method=\"markup\""
                          tei-text)))
        (is (not (string/includes? tei-text "orthographic-katakana")))
        (is (= "passed"
               (get (files/read-json (io/file out-dir "tei-validation-result.json"))
                    "status"))))
      (finally
        (delete-tree! work-dir)))))

(deftest materialize-publication-requires-sentence-rows-test
  (let [work-dir (temp-dir "abc-materialize-publication-missing-sentences")
        out-dir (io/file work-dir "out")
        parser-ir-file (io/file work-dir "parser-ir.json")]
    (try
      (abc-json/write-deterministic-json-file!
       parser-ir-file
       {"schema_id" "https://w3id.org/abc/schemas/parser-ir.schema.json"
        "schema_hash" "sha256:0b495bb5c12c4d76482afefdaedb5464a74672ffbd5282f9c67d5f419d39a340"
        "source" {"work_content_hash" (files/example-hash "11")
                  "encoding" "UTF-8"
                  "normalization" "source"}
        "nodes" [{"type" "text"
                  "span" {"start" 0 "end" 24 "coordinate_system" "decoded_utf8"}
                  "text" "吾輩ハ猫デアル。"}]
        "paragraphs" [{"id" "p000000"
                       "span" {"start" 0 "end" 24 "coordinate_system" "decoded_utf8"}
                       "span_source" "direct"
                       "node_range" {"start" 0 "end" 1}
                       "role" "body"
                       "source_pointer" "blocks[0]"
                       "classification" "direct"}]
        "warnings" []
        "errors" []})
      (let [err (try
                  (materialize/materialize-publication!
                   {:parser-ir-path (str parser-ir-file)
                    :source-manifest-path "examples/v0/example-work/source.manifest.json"
                    :metadata-record-path "examples/v0/example-work/metadata-record.json"
                    :persons-dir "examples/v0/example-persons"
                    :output-dir out-dir
                    :generated-at generated-at})
                  nil
                  (catch clojure.lang.ExceptionInfo e
                    e))]
        (is (some? err))
        (is (= ["parser IR publication requires sentence_segmentation"
                "parser IR body paragraph p000000 has no sentence rows"]
               (:errors (ex-data err)))))
      (finally
        (delete-tree! work-dir)))))

(deftest materialized-publication-output-is-deterministic-test
  (let [out-dir-a (temp-dir "abc-materialize-publication-a")
        out-dir-b (temp-dir "abc-materialize-publication-b")]
    (try
      (materialize-example! out-dir-a)
      (materialize-example! out-dir-b)
      (doseq [name ["plain.txt"
                    "tei.xml"
                    "preservation.json"
                    "plaintext.manifest.json"
                    "tei.manifest.json"
                    "tei-validation-result.json"]]
        (is (= (slurp (io/file out-dir-a name))
               (slurp (io/file out-dir-b name)))
            (str name " should be deterministic")))
      (finally
        (delete-tree! out-dir-a)
        (delete-tree! out-dir-b)))))
