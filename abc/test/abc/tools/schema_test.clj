(ns abc.tools.schema-test
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.schema :as schema]
            [clojure.test :refer [deftest is testing]]))

(def ^:private cross-project-schema-versions
  {"schemas/parser-ir.schema.json" "0.6.0"
   "schemas/aat-parser-ir-divergence.schema.json" "0.3.0"
   "schemas/aat-parser-ir-mapping.schema.json" "0.2.4"
   "schemas/analysis-recipe.schema.json" "0.1.1"
   "schemas/analysis-result.schema.json" "0.1.1"
   "schemas/manifest.schema.json" "0.4.3"
   "schemas/parser-ir-publication-preservation.schema.json" "0.3.0"
   "schemas/request-set.schema.json" "0.1.2"
   "schemas/source-region-coverage.schema.json" "0.2.1"
   "schemas/snapshot-index.schema.json" "0.1.0"
   "schemas/pack-policy.schema.json" "0.1.0"
   "schemas/tokenizer-profile.schema.json" "0.1.0"
   "schemas/token-output.schema.json" "0.1.0"})

(deftest cross-project-schemas-carry-explicit-versions-test
  (doseq [[path expected-version] cross-project-schema-versions]
    (is (= expected-version
           (get (files/read-json path) "version"))
        (str path " must expose the cross-repo schema contract version"))))

(deftest schema-hash-test
  (testing "hashes are over parsed canonical JSON values, not source bytes"
    (let [file-a (java.io.File/createTempFile "abc-schema-a" ".json")
          file-b (java.io.File/createTempFile "abc-schema-b" ".json")
          file-c (java.io.File/createTempFile "abc-schema-c" ".json")]
      (try
        (spit file-a "{\"type\":\"object\",\"properties\":{\"b\":{\"type\":\"string\"},\"a\":{\"type\":\"null\"}}}\n")
        (spit file-b "{\n  \"properties\": {\n    \"a\": {\"type\": \"null\"},\n    \"b\": {\"type\": \"string\"}\n  },\n  \"type\": \"object\"\n}\n")
        (spit file-c "{\"type\":\"array\"}\n")
        (is (= (schema/schema-hash file-a)
               (schema/schema-hash file-b)))
        (is (not= (schema/schema-hash file-a)
                  (schema/schema-hash file-c)))
        (finally
          (doseq [file [file-a file-b file-c]]
            (.delete file)))))))

(deftest schema-validation-test
  (let [schema-file (java.io.File/createTempFile "abc-schema" ".json")
        value-file (java.io.File/createTempFile "abc-value" ".json")]
    (try
      (spit schema-file "{\"type\":\"object\",\"required\":[\"a\"],\"properties\":{\"a\":{\"type\":\"string\"}}}")
      (spit value-file "{\"a\":\"ok\"}")
      (is (nil? (schema/validate-json! (schema/read-schema schema-file) value-file)))
      (spit value-file "{\"a\":1}")
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"JSON Schema validation failed"
                            (schema/validate-json! (schema/read-schema schema-file) value-file)))
      (finally
        (doseq [file [schema-file value-file]]
          (.delete file))))))

(deftest manifest-schema-requires-tokenizer-profile-hash-coordinate-test
  (let [manifest-schema (schema/read-schema "schemas/manifest.schema.json")
        identity-object {"manifest_schema_hash" (manifest/schema-hash
                                                 "schemas/manifest.schema.json")
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
                         "output_format_spec_hash" (files/example-hash "07")}
        manifest {"manifest_schema_id" "https://w3id.org/abc/schemas/manifest.schema.json"
                  "artifact_id" (manifest/artifact-id identity-object)
                  "artifact_kind" "parser-ir"
                  "validation_status" "passed"
                  "manifest_identity_object" identity-object
                  "content" {"content_hash" (files/example-hash "08")
                             "media_type" "application/json"}
                  "sidecars" []
                  "provenance" {"generated_at" "2026-07-07T00:00:00Z"
                                "activity_id" "https://w3id.org/abc/activity/test"
                                "agent" "abc.tools.schema-test"
                                "plan_hash" nil
                                "used" []
                                "was_derived_from" []}
                  "license" nil
                  "signatures" []
                  "superseded_by" nil
                  "invalidated_at" nil
                  "replacement_reason" nil
                  "notes" nil}]
    (is (nil? (schema/validation-errors manifest-schema manifest)))
    (is (seq (schema/validation-errors
              manifest-schema
              (update manifest
                      "manifest_identity_object"
                      dissoc
                      "tokenizer_profile_hash"))))))

(deftest analysis-schema-fixtures-validate-test
  (let [recipe-schema (schema/read-schema "schemas/analysis-recipe.schema.json")
        result-schema (schema/read-schema "schemas/analysis-result.schema.json")
        recipe (files/read-json "data/analysis-recipes/literary-basic-ja-v1.json")
        result (files/read-json "examples/v0/example-work/analysis-result.json")]
    (is (= "https://w3id.org/abc/schemas/analysis-recipe.schema.json"
           (get recipe "schema_id")))
    (is (= "https://w3id.org/abc/schemas/analysis-result.schema.json"
           (get result "schema_id")))
    (is (= "cards/000000/files/example.txt"
           (get-in result ["subject" "logical_path"])))
    (is (= "0e9ea3e586"
           (get-in result ["subject" "git_ref"])))
    (is (= (manifest/schema-hash "schemas/analysis-result.schema.json")
           (get recipe "required_output_schema_hash")))
    (is (= (manifest/schema-hash "schemas/analysis-result.schema.json")
           (get result "schema_hash")))
    (is (= (analysis-identity/analysis-recipe-hash recipe)
           (get result "analysis_recipe_hash")))
    (is (nil? (schema/validation-errors recipe-schema recipe)))
    (is (nil? (schema/validation-errors result-schema result)))))

(deftest analysis-schemas-accept-token-stream-input-view-test
  (let [recipe-schema (schema/read-schema "schemas/analysis-recipe.schema.json")
        result-schema (schema/read-schema "schemas/analysis-result.schema.json")
        recipe {"schema_id" "https://w3id.org/abc/schemas/analysis-recipe.schema.json"
                "recipe_id" "token-basic-ja-v1"
                "recipe_version" "0.1.0"
                "supported_input_view_kinds" ["token-stream-v1"]
                "tokenizer_required" true
                "plaintext_policy_hash" nil
                "normalization_policy" {"unicode_normalization" "producer-preserved"
                                        "newline_policy" "token-stream-v1"}
                "metrics" [{"metric_id" "fixture-token-count"
                            "formula_version" "fixture-token-input-v1"
                            "unit" "token"
                            "value_type" "integer"}]
                "required_output_schema_hash" (manifest/schema-hash
                                               "schemas/analysis-result.schema.json")
                "determinism_tier" "exact"
                "error_behavior" {"missing_required_identity_field" "failed-manifest-or-none"
                                  "metric_failure" "record-failed-metric"}}
        result {"schema_id" "https://w3id.org/abc/schemas/analysis-result.schema.json"
                "schema_hash" (manifest/schema-hash
                               "schemas/analysis-result.schema.json")
                "subject" {"source_id" "aozora:example-work"
                           "logical_path" "cards/000000/files/example.txt"
                           "git_ref" "refs/heads/fixture"
                           "work_id" "aozora:example-work"
                           "corpus_snapshot_hash" (files/example-hash "01")
                           "work_content_hash" (files/example-hash "02")
                           "metadata_record_hash" nil}
                "input_view" {"input_view_kind" "token-stream-v1"
                              "producer_artifact_id" (files/example-hash "03")
                              "producer_content_hash" (files/example-hash "04")
                              "token_output_schema_hash" (manifest/schema-hash
                                                          "schemas/token-output.schema.json")
                              "coordinate_system" "token-index-v1+unicode-scalar-value-input-spans"}
                "tokenizer_profile_hash" (files/example-hash "05")
                "analysis_recipe_hash" (analysis-identity/analysis-recipe-hash recipe)
                "metrics" [{"metric_id" "fixture-token-count"
                            "value" 2
                            "value_type" "integer"
                            "denominator" nil
                            "unit" "token"
                            "status" "passed"}]
                "warnings" []}]
    (is (nil? (schema/validation-errors recipe-schema recipe)))
    (is (nil? (schema/validation-errors result-schema result)))))

(deftest tokenizer-profile-schema-fixtures-validate-test
  (let [profile-schema (schema/read-schema "schemas/tokenizer-profile.schema.json")
        profile (files/read-json "data/tokenizer-profiles/fixture-tokenizer-ja-v1.json")]
    (is (= "https://w3id.org/abc/schemas/tokenizer-profile.schema.json"
           (get profile "schema_id")))
    (is (= (manifest/schema-hash "schemas/tokenizer-profile.schema.json")
           (get profile "schema_hash")))
    (is (= (manifest/schema-hash "schemas/token-output.schema.json")
           (get profile "token_output_schema_hash")))
    (is (re-matches files/hash-pattern
                    (analysis-identity/tokenizer-profile-hash profile)))
    (is (nil? (schema/validation-errors profile-schema profile)))))

(deftest pack-policy-schema-fixtures-validate-test
  (let [policy-schema (schema/read-schema "schemas/pack-policy.schema.json")
        no-pack (files/read-json "data/pack-policies/no-pack-v1.json")
        parquet-pack (files/read-json "data/pack-policies/parquet-basic-v1.json")]
    (is (= "https://w3id.org/abc/schemas/pack-policy.schema.json"
           (get no-pack "schema_id")))
    (is (= (manifest/schema-hash "schemas/pack-policy.schema.json")
           (get no-pack "schema_hash")))
    (is (= "none" (get no-pack "pack_kind")))
    (is (= [] (get no-pack "included_artifact_kinds")))
    (is (= [] (get no-pack "included_sidecar_roles")))
    (is (nil? (get no-pack "pack_index_output_format_spec_hash")))
    (is (re-matches files/hash-pattern
                    (analysis-identity/pack-policy-hash no-pack)))
    (is (= "parquet-pack-v1" (get parquet-pack "pack_kind")))
    (is (seq (get parquet-pack "included_artifact_kinds")))
    (is (seq (get parquet-pack "metric_table_output_format_spec_hashes")))
    (is (nil? (schema/validation-errors policy-schema no-pack)))
    (is (nil? (schema/validation-errors policy-schema parquet-pack)))))

(deftest token-output-schema-fixture-validates-test
  (let [token-output-schema (schema/read-schema "schemas/token-output.schema.json")
        token-stream (files/read-json "examples/v0/example-work/token-stream.json")]
    (is (= "https://w3id.org/abc/schemas/token-output.schema.json"
           (get token-stream "schema_id")))
    (is (= (manifest/schema-hash "schemas/token-output.schema.json")
           (get token-stream "schema_hash")))
    (is (= "token-index-v1+unicode-scalar-value-input-spans"
           (get token-stream "coordinate_system")))
    (is (nil? (schema/validation-errors token-output-schema token-stream)))))
