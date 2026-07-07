(ns abc.tools.materialize-analysis-test
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.materialize-analysis :as materialize-analysis]
            [abc.tools.schema :as schema]
            [clojure.test :refer [deftest is]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn producer-manifest []
  {"artifact_id" (files/example-hash "31")
   "artifact_kind" "parser-ir"
   "validation_status" "passed"
   "manifest_identity_object" {"manifest_schema_hash" (manifest/schema-hash "schemas/manifest.schema.json")
                               "corpus_snapshot_hash" (files/example-hash "01")
                               "work_content_hash" (files/example-hash "02")
                               "metadata_record_hash" nil
                               "parser_build_hash" (files/example-hash "03")
                               "parser_config_hash" (files/example-hash "04")
                               "aat_parser_ir_mapping_hash" (files/example-hash "05")
                               "parser_ir_schema_hash" (files/example-hash "06")
                               "tei_profile_hash" nil
                               "tokenizer_build_hash" nil
                               "tokenizer_dictionary_hash" nil
                               "analysis_recipe_hash" nil
                               "output_format_spec_hash" (files/example-hash "06")}
   "content" {"content_hash" (files/example-hash "07")
              "media_type" "application/json"
              "byte_length" 100
              "path_hint" "parser-ir.json"}
   "sidecars" []
   "provenance" {"generated_at" "2026-07-07T00:00:00Z"
                 "activity_id" "https://w3id.org/abc/activity/materialize-imported-parser-ir"
                 "agent" "abc.tools.materialize-import"
                 "plan_hash" nil
                 "used" [(files/example-hash "02")]
                 "was_derived_from" [(files/example-hash "02")]}
   "license" nil
   "signatures" []
   "superseded_by" nil
   "invalidated_at" nil
   "replacement_reason" nil
   "notes" nil})

(deftest materialize-analysis-copies-parser-ir-identity-test
  (let [dir (Files/createTempDirectory "abc-analysis" (make-array FileAttribute 0))
        out-dir (.toFile dir)
        recipe (files/read-json "data/analysis-recipes/literary-basic-ja-v1.json")
        result (materialize-analysis/materialize-analysis!
                {:producer-manifest (producer-manifest)
                 :recipe recipe
                 :subject {"source_id" "aozora:example-work"
                           "logical_path" "aozora/example-work.txt"
                           "git_ref" "refs/heads/fixture"
                           "work_id" "aozora:example-work"}
                 :metrics [{"metric_id" "fixture-line-count"
                            "value" 3
                            "value_type" "integer"
                            "denominator" nil
                            "unit" "line"
                            "status" "passed"}]
                 :output-dir out-dir
                 :generated-at "2026-07-07T00:00:00Z"})]
    (try
      (let [manifest-file (:manifest result)
            result-file (:analysis-result result)
            analysis-manifest (files/read-json manifest-file)
            analysis-result (files/read-json result-file)
            manifest-schema (files/read-json "schemas/manifest.schema.json")
            result-schema (files/read-json "schemas/analysis-result.schema.json")
            identity-object (get analysis-manifest "manifest_identity_object")
            recipe-hash (analysis-identity/analysis-recipe-hash recipe)
            result-schema-hash (manifest/schema-hash "schemas/analysis-result.schema.json")]
        (is (nil? (schema/validation-errors manifest-schema analysis-manifest)))
        (is (nil? (schema/validation-errors result-schema analysis-result)))
        (is (= "analysis" (get analysis-manifest "artifact_kind")))
        (is (= "analysis-result"
               (get-in analysis-manifest ["sidecars" 0 "role"])))
        (is (= (files/example-hash "03")
               (get identity-object "parser_build_hash")))
        (is (= (files/example-hash "04")
               (get identity-object "parser_config_hash")))
        (is (= (files/example-hash "05")
               (get identity-object "aat_parser_ir_mapping_hash")))
        (is (= (files/example-hash "06")
               (get identity-object "parser_ir_schema_hash")))
        (is (nil? (get identity-object "tokenizer_build_hash")))
        (is (nil? (get identity-object "tokenizer_dictionary_hash")))
        (is (= recipe-hash
               (get identity-object "analysis_recipe_hash")))
        (is (= result-schema-hash
               (get identity-object "output_format_spec_hash")))
        (is (= result-schema-hash
               (get analysis-result "schema_hash")))
        (is (= recipe-hash
               (get analysis-result "analysis_recipe_hash")))
        (is (nil? (get analysis-result "tokenizer_profile_hash")))
        (is (some #{(files/example-hash "31")}
                  (get-in analysis-manifest ["provenance" "used"])))
        (is (some #{(files/example-hash "31")}
                  (get-in analysis-manifest ["provenance" "was_derived_from"])))
        (is (= (get analysis-manifest "artifact_id")
               (manifest/artifact-id (get analysis-manifest "manifest_identity_object")))))
      (finally
        (doseq [file (reverse (file-seq out-dir))]
          (.delete file))))))
