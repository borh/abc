(ns abc.tools.manifest-index-test
  (:require [abc.tools.files :as files]
            [abc.tools.manifest-index :as manifest-index]
            [clojure.test :refer [deftest is testing]]))

(defn manifest
  [artifact-id content-hash validation-status]
  {"artifact_id" artifact-id
   "artifact_kind" "parser-ir"
   "validation_status" validation-status
   "content" {"content_hash" content-hash
              "media_type" "application/json"}})

(defn analysis-manifest
  [artifact-id producer-artifact-id parser-build-hash]
  {"artifact_id" artifact-id
   "artifact_kind" "analysis"
   "validation_status" "passed"
   "manifest_identity_object" {"manifest_schema_hash" (files/example-hash "01")
                               "corpus_snapshot_hash" (files/example-hash "02")
                               "work_content_hash" (files/example-hash "03")
                               "metadata_record_hash" nil
                               "parser_build_hash" parser-build-hash
                               "parser_config_hash" (files/example-hash "05")
                               "aat_parser_ir_mapping_hash" (files/example-hash "06")
                               "parser_ir_schema_hash" (files/example-hash "07")
                               "tei_profile_hash" nil
                               "tokenizer_build_hash" nil
                               "tokenizer_dictionary_hash" nil
                               "analysis_recipe_hash" (files/example-hash "08")
                               "output_format_spec_hash" (files/example-hash "09")}
   "content" {"content_hash" (files/example-hash "10")
              "media_type" "application/json"}
   "provenance" {"used" [producer-artifact-id (files/example-hash "20")]
                 "was_derived_from" [producer-artifact-id]}})

(defn analysis-manifest-with-provenance
  [artifact-id provenance-used provenance-was-derived-from parser-build-hash]
  {"artifact_id" artifact-id
   "artifact_kind" "analysis"
   "validation_status" "passed"
   "manifest_identity_object" {"manifest_schema_hash" (files/example-hash "01")
                               "corpus_snapshot_hash" (files/example-hash "02")
                               "work_content_hash" (files/example-hash "03")
                               "metadata_record_hash" nil
                               "parser_build_hash" parser-build-hash
                               "parser_config_hash" (files/example-hash "05")
                               "aat_parser_ir_mapping_hash" (files/example-hash "06")
                               "parser_ir_schema_hash" (files/example-hash "07")
                               "tei_profile_hash" nil
                               "tokenizer_build_hash" nil
                               "tokenizer_dictionary_hash" nil
                               "analysis_recipe_hash" (files/example-hash "08")
                               "output_format_spec_hash" (files/example-hash "09")}
   "content" {"content_hash" (files/example-hash "10")
              "media_type" "application/json"}
   "provenance" {"used" provenance-used
                 "was_derived_from" provenance-was-derived-from}})

(defn parser-ir-manifest
  [artifact-id parser-build-hash]
  {"artifact_id" artifact-id
   "artifact_kind" "parser-ir"
   "validation_status" "passed"
   "manifest_identity_object" {"manifest_schema_hash" (files/example-hash "01")
                               "corpus_snapshot_hash" (files/example-hash "02")
                               "work_content_hash" (files/example-hash "03")
                               "metadata_record_hash" nil
                               "parser_build_hash" parser-build-hash
                               "parser_config_hash" (files/example-hash "05")
                               "aat_parser_ir_mapping_hash" (files/example-hash "06")
                               "parser_ir_schema_hash" (files/example-hash "07")
                               "tei_profile_hash" nil
                               "tokenizer_build_hash" nil
                               "tokenizer_dictionary_hash" nil
                               "analysis_recipe_hash" nil
                               "output_format_spec_hash" (files/example-hash "07")}
   "content" {"content_hash" (files/example-hash "11")
              "media_type" "application/json"}
   "provenance" {"used" []
                 "was_derived_from" [(files/example-hash "03")]}})

(deftest index-entries-test
  (is (= [{"manifest_path" "a.manifest.json"
           "artifact_id" "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
           "artifact_kind" "parser-ir"
           "validation_status" "passed"
           "content_hash" "sha256:1111111111111111111111111111111111111111111111111111111111111111"
           "media_type" "application/json"
           "manifest_identity_object" nil
           "provenance_used" []
           "provenance_was_derived_from" []}
          {"manifest_path" "b.manifest.json"
           "artifact_id" "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
           "artifact_kind" "parser-ir"
           "validation_status" "warning"
           "content_hash" "sha256:2222222222222222222222222222222222222222222222222222222222222222"
           "media_type" "application/json"
           "manifest_identity_object" nil
           "provenance_used" []
           "provenance_was_derived_from" []}]
         (manifest-index/index-entries
          {"b.manifest.json" (manifest "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                                       "sha256:2222222222222222222222222222222222222222222222222222222222222222"
                                       "warning")
           "a.manifest.json" (manifest "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                                       "sha256:1111111111111111111111111111111111111111111111111111111111111111"
                                       "passed")}))))

(deftest reproducibility-conflicts-test
  (testing "same artifact_id and content_hash is not a conflict"
    (is (empty?
         (manifest-index/reproducibility-conflicts
          [(manifest-index/manifest->index-entry
            "a.manifest.json"
            (manifest "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                      "sha256:1111111111111111111111111111111111111111111111111111111111111111"
                      "passed"))
           (manifest-index/manifest->index-entry
            "b.manifest.json"
            (manifest "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                      "sha256:1111111111111111111111111111111111111111111111111111111111111111"
                      "warning"))]))))
  (testing "same artifact_id and different content_hash is a conflict"
    (is (= [{"artifact_id" "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
             "content_hashes" ["sha256:1111111111111111111111111111111111111111111111111111111111111111"
                               "sha256:2222222222222222222222222222222222222222222222222222222222222222"]
             "manifest_paths" ["a.manifest.json" "b.manifest.json"]}]
           (manifest-index/reproducibility-conflicts
            [(manifest-index/manifest->index-entry
              "a.manifest.json"
              (manifest "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                        "sha256:1111111111111111111111111111111111111111111111111111111111111111"
                        "passed"))
             (manifest-index/manifest->index-entry
              "b.manifest.json"
              (manifest "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                        "sha256:2222222222222222222222222222222222222222222222222222222222222222"
                        "warning"))])))))

(deftest parser-ir-producer-lookup-test
  (let [producer-id (files/example-hash "31")
        entries (manifest-index/index-entries
                 {"parser.manifest.json" (parser-ir-manifest producer-id (files/example-hash "04"))
                  "analysis.manifest.json" (analysis-manifest (files/example-hash "32")
                                                              producer-id
                                                              (files/example-hash "04"))})]
    (is (= [producer-id]
           (mapv #(get % "artifact_id")
                 (manifest-index/parser-ir-producer-candidates
                  entries
                  {"work_content_hash" (files/example-hash "03")
                   "corpus_snapshot_hash" (files/example-hash "02")
                   "parser_build_hash" (files/example-hash "04")
                   "parser_config_hash" (files/example-hash "05")
                   "aat_parser_ir_mapping_hash" (files/example-hash "06")
                   "parser_ir_schema_hash" (files/example-hash "07")}))))))

(deftest analysis-copied-field-validation-test
  (let [producer-id (files/example-hash "31")
        good-analysis (analysis-manifest (files/example-hash "32")
                                         producer-id
                                         (files/example-hash "04"))
        bad-analysis (analysis-manifest (files/example-hash "33")
                                        producer-id
                                        (files/example-hash "99"))
        producer (parser-ir-manifest producer-id (files/example-hash "04"))]
    (is (empty? (manifest-index/analysis-copied-field-errors
                 (manifest-index/index-entries {"parser.manifest.json" producer
                                                "analysis.manifest.json" good-analysis}))))
    (is (= [{:analysis_artifact_id (files/example-hash "33")
             :producer_artifact_id producer-id
             :field "parser_build_hash"
             :analysis_value (files/example-hash "99")
             :producer_value (files/example-hash "04")}]
           (manifest-index/analysis-copied-field-errors
            (manifest-index/index-entries {"parser.manifest.json" producer
                                           "analysis.manifest.json" bad-analysis}))))))

(deftest analysis-copied-field-validation-uses-successful-parser-ir-producer-test
  (let [producer-id (files/example-hash "41")
        fallback-id (files/example-hash "42")
        analysis-id (files/example-hash "43")
        producer (parser-ir-manifest producer-id (files/example-hash "04"))
        failed-parser-ir {"artifact_id" fallback-id
                          "artifact_kind" "parser-ir"
                          "validation_status" "failed"
                          "manifest_identity_object" {"manifest_schema_hash" (files/example-hash "01")
                                                      "corpus_snapshot_hash" (files/example-hash "02")
                                                      "work_content_hash" (files/example-hash "03")
                                                      "metadata_record_hash" nil
                                                      "parser_build_hash" (files/example-hash "99")
                                                      "parser_config_hash" (files/example-hash "05")
                                                      "aat_parser_ir_mapping_hash" (files/example-hash "06")
                                                      "parser_ir_schema_hash" (files/example-hash "07")
                                                      "tei_profile_hash" nil
                                                      "tokenizer_build_hash" nil
                                                      "tokenizer_dictionary_hash" nil
                                                      "analysis_recipe_hash" nil
                                                      "output_format_spec_hash" (files/example-hash "07")}
                          "content" {"content_hash" (files/example-hash "12")
                                     "media_type" "application/json"}
                          "provenance" {"used" []
                                        "was_derived_from" []}}
        non-parser-entry {"artifact_id" (files/example-hash "44")
                          "artifact_kind" "analysis"
                          "validation_status" "failed"
                          "manifest_identity_object" {"manifest_schema_hash" (files/example-hash "01")
                                                      "corpus_snapshot_hash" (files/example-hash "02")
                                                      "work_content_hash" (files/example-hash "03")
                                                      "metadata_record_hash" nil
                                                      "parser_build_hash" (files/example-hash "98")
                                                      "parser_config_hash" (files/example-hash "05")
                                                      "aat_parser_ir_mapping_hash" (files/example-hash "06")
                                                      "parser_ir_schema_hash" (files/example-hash "07")
                                                      "tei_profile_hash" nil
                                                      "tokenizer_build_hash" nil
                                                      "tokenizer_dictionary_hash" nil
                                                      "analysis_recipe_hash" (files/example-hash "08")
                                                      "output_format_spec_hash" (files/example-hash "09")}
                          "content" {"content_hash" (files/example-hash "13")
                                     "media_type" "application/json"}
                          "provenance" {"used" []
                                        "was_derived_from" []}}
        analysis (analysis-manifest-with-provenance analysis-id
                                                    [(get failed-parser-ir "artifact_id")
                                                     (get non-parser-entry "artifact_id")
                                                     producer-id]
                                                    [(get failed-parser-ir "artifact_id")
                                                     (get non-parser-entry "artifact_id")
                                                     producer-id]
                                                    (files/example-hash "04"))
        entries (manifest-index/index-entries
                 {"analysis.manifest.json" analysis
                  "fallback-parser.manifest.json" failed-parser-ir
                  "non-parser.manifest.json" non-parser-entry
                  "parser.manifest.json" producer})]
    (is (empty? (manifest-index/analysis-copied-field-errors entries)))))

(deftest analysis-copied-field-validation-missing-producer-test
  (let [analysis-id (files/example-hash "51")
        failed-parser-ir {"artifact_id" (files/example-hash "52")
                          "artifact_kind" "parser-ir"
                          "validation_status" "failed"
                          "manifest_identity_object" {"manifest_schema_hash" (files/example-hash "01")
                                                      "corpus_snapshot_hash" (files/example-hash "02")
                                                      "work_content_hash" (files/example-hash "03")
                                                      "metadata_record_hash" nil
                                                      "parser_build_hash" (files/example-hash "04")
                                                      "parser_config_hash" (files/example-hash "05")
                                                      "aat_parser_ir_mapping_hash" (files/example-hash "06")
                                                      "parser_ir_schema_hash" (files/example-hash "07")
                                                      "tei_profile_hash" nil
                                                      "tokenizer_build_hash" nil
                                                      "tokenizer_dictionary_hash" nil
                                                      "analysis_recipe_hash" nil
                                                      "output_format_spec_hash" (files/example-hash "07")}
                          "content" {"content_hash" (files/example-hash "14")
                                     "media_type" "application/json"}
                          "provenance" {"used" []
                                        "was_derived_from" []}}
        non-parser-entry {"artifact_id" (files/example-hash "53")
                          "artifact_kind" "analysis"
                          "validation_status" "failed"
                          "manifest_identity_object" {"manifest_schema_hash" (files/example-hash "01")
                                                      "corpus_snapshot_hash" (files/example-hash "02")
                                                      "work_content_hash" (files/example-hash "03")
                                                      "metadata_record_hash" nil
                                                      "parser_build_hash" (files/example-hash "04")
                                                      "parser_config_hash" (files/example-hash "05")
                                                      "aat_parser_ir_mapping_hash" (files/example-hash "06")
                                                      "parser_ir_schema_hash" (files/example-hash "07")
                                                      "tei_profile_hash" nil
                                                      "tokenizer_build_hash" nil
                                                      "tokenizer_dictionary_hash" nil
                                                      "analysis_recipe_hash" (files/example-hash "08")
                                                      "output_format_spec_hash" (files/example-hash "09")}
                          "content" {"content_hash" (files/example-hash "15")
                                     "media_type" "application/json"}
                          "provenance" {"used" []
                                        "was_derived_from" []}}
        analysis (analysis-manifest-with-provenance analysis-id
                                                    [(get non-parser-entry "artifact_id")]
                                                    [(get failed-parser-ir "artifact_id")]
                                                    (files/example-hash "04"))
        errors (manifest-index/analysis-copied-field-errors
                (manifest-index/index-entries
                 {"analysis.manifest.json" analysis
                  "fallback-parser.manifest.json" failed-parser-ir
                  "non-parser.manifest.json" non-parser-entry}))]
    (is (= [{:analysis_artifact_id analysis-id
             :producer_artifact_id nil
             :field "__producer__"
             :analysis_value [(files/example-hash "52")
                              (files/example-hash "53")]
             :producer_value nil}]
           errors))))
