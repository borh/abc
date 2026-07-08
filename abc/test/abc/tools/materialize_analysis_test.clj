(ns abc.tools.materialize-analysis-test
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.manifest-index :as manifest-index]
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
                               "tokenizer_profile_hash" nil
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

(defn tokenized-producer-manifest []
  (let [profile (files/read-json "data/tokenizer-profiles/fixture-tokenizer-ja-v1.json")
        profile-hash (analysis-identity/tokenizer-profile-hash profile)]
    {"artifact_id" (files/example-hash "41")
     "artifact_kind" "tokenized"
     "validation_status" "passed"
     "manifest_identity_object" {"manifest_schema_hash" (manifest/schema-hash
                                                         "schemas/manifest.schema.json")
                                 "corpus_snapshot_hash" (files/example-hash "01")
                                 "work_content_hash" (files/example-hash "02")
                                 "metadata_record_hash" nil
                                 "parser_build_hash" (files/example-hash "03")
                                 "parser_config_hash" (files/example-hash "04")
                                 "aat_parser_ir_mapping_hash" (files/example-hash "05")
                                 "parser_ir_schema_hash" (files/example-hash "06")
                                 "tei_profile_hash" nil
                                 "tokenizer_build_hash" (get-in profile ["tokenizer" "build_hash"])
                                 "tokenizer_dictionary_hash" (get-in profile ["dictionary" "archive_hash"])
                                 "tokenizer_profile_hash" profile-hash
                                 "analysis_recipe_hash" nil
                                 "output_format_spec_hash" (get profile "token_output_schema_hash")}
     "content" {"content_hash" (files/example-hash "42")
                "media_type" "application/json"
                "byte_length" 100
                "path_hint" "token-stream.json"}
     "sidecars" [{"role" "token-stream"
                  "hash" (files/example-hash "42")
                  "media_type" "application/json"
                  "path_hint" "token-stream.json"}]
     "provenance" {"generated_at" "2026-07-08T00:00:00Z"
                   "activity_id" "https://w3id.org/abc/activity/materialize-tokenized"
                   "agent" "abc.tools.materialize-tokenized"
                   "plan_hash" nil
                   "used" [(files/example-hash "31")
                           profile-hash]
                   "was_derived_from" [(files/example-hash "31")]}
     "license" nil
     "signatures" []
     "superseded_by" nil
     "invalidated_at" nil
     "replacement_reason" nil
     "notes" nil}))

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
        (is (nil? (get identity-object "tokenizer_profile_hash")))
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

(deftest materialize-token-backed-analysis-copies-tokenized-identity-test
  (let [dir (Files/createTempDirectory "abc-token-analysis" (make-array FileAttribute 0))
        out-dir (.toFile dir)
        recipe (files/read-json "data/analysis-recipes/token-basic-ja-v1.json")
        tokenized-producer (tokenized-producer-manifest)
        result (materialize-analysis/materialize-token-backed-analysis!
                {:producer-manifest tokenized-producer
                 :recipe recipe
                 :subject {"source_id" "aozora:example-work"
                           "logical_path" "aozora/example-work.txt"
                           "git_ref" "refs/heads/fixture"
                           "work_id" "aozora:example-work"}
                 :metrics [{"metric_id" "fixture-token-count"
                            "value" 2
                            "value_type" "integer"
                            "denominator" nil
                            "unit" "token"
                            "status" "passed"}]
                 :output-dir out-dir
                 :generated-at "2026-07-08T00:00:00Z"})]
    (try
      (let [manifest-file (:manifest result)
            result-file (:analysis-result result)
            analysis-manifest (files/read-json manifest-file)
            analysis-result (files/read-json result-file)
            manifest-schema (files/read-json "schemas/manifest.schema.json")
            result-schema (files/read-json "schemas/analysis-result.schema.json")
            identity-object (get analysis-manifest "manifest_identity_object")
            producer-identity (get tokenized-producer "manifest_identity_object")
            recipe-hash (analysis-identity/analysis-recipe-hash recipe)
            result-schema-hash (manifest/schema-hash "schemas/analysis-result.schema.json")
            entries (manifest-index/index-entries
                     {"tokenized.manifest.json" tokenized-producer
                      "analysis.manifest.json" analysis-manifest})]
        (is (nil? (schema/validation-errors manifest-schema analysis-manifest)))
        (is (nil? (schema/validation-errors result-schema analysis-result)))
        (is (= "analysis" (get analysis-manifest "artifact_kind")))
        (doseq [field ["parser_build_hash"
                       "parser_config_hash"
                       "aat_parser_ir_mapping_hash"
                       "parser_ir_schema_hash"
                       "tei_profile_hash"
                       "tokenizer_build_hash"
                       "tokenizer_dictionary_hash"
                       "tokenizer_profile_hash"]]
          (is (= (get producer-identity field)
                 (get identity-object field))
              field))
        (is (= recipe-hash
               (get identity-object "analysis_recipe_hash")))
        (is (= result-schema-hash
               (get identity-object "output_format_spec_hash")))
        (is (= "token-stream-v1"
               (get-in analysis-result ["input_view" "input_view_kind"])))
        (is (= (get tokenized-producer "artifact_id")
               (get-in analysis-result ["input_view" "producer_artifact_id"])))
        (is (= (get producer-identity "tokenizer_profile_hash")
               (get analysis-result "tokenizer_profile_hash")))
        (is (some #{(get tokenized-producer "artifact_id")}
                  (get-in analysis-manifest ["provenance" "used"])))
        (is (some #{(get tokenized-producer "artifact_id")}
                  (get-in analysis-manifest ["provenance" "was_derived_from"])))
        (is (empty? (manifest-index/analysis-copied-field-errors entries)))
        (is (= (get analysis-manifest "artifact_id")
               (manifest/artifact-id identity-object))))
      (finally
        (doseq [file (reverse (file-seq out-dir))]
          (.delete file))))))
