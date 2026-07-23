(ns abc.tools.manifest-index-test
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.manifest-index :as manifest-index]
            [clojure.test :refer [deftest is]]))

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

(defn tokenized-manifest
  ([artifact-id validation-status]
   (tokenized-manifest artifact-id validation-status :missing))
  ([artifact-id validation-status tokenizer-profile-hash]
   {"artifact_id" artifact-id
    "artifact_kind" "tokenized"
    "validation_status" validation-status
    "manifest_identity_object" (cond-> {"manifest_schema_hash" (files/example-hash "01")
                                        "corpus_snapshot_hash" (files/example-hash "02")
                                        "work_content_hash" (files/example-hash "03")
                                        "metadata_record_hash" nil
                                        "parser_build_hash" (files/example-hash "04")
                                        "parser_config_hash" (files/example-hash "05")
                                        "aat_parser_ir_mapping_hash" (files/example-hash "06")
                                        "parser_ir_schema_hash" (files/example-hash "07")
                                        "tei_profile_hash" nil
                                        "tokenizer_build_hash" (files/example-hash "08")
                                        "tokenizer_dictionary_hash" (files/example-hash "09")
                                        "analysis_recipe_hash" nil
                                        "output_format_spec_hash" (files/example-hash "10")}
                                 (not= :missing tokenizer-profile-hash)
                                 (assoc "tokenizer_profile_hash"
                                        tokenizer-profile-hash))
    "content" {"content_hash" (files/example-hash "11")
               "media_type" "application/json"}
    "provenance" {"used" []
                  "was_derived_from" [(files/example-hash "03")]}}))

(defn tokenizer-profile []
  (files/read-json "data/tokenizer-profiles/fixture-tokenizer-ja-v1.json"))

(defn tokenized-manifest-from-producer
  [artifact-id producer-artifact-id profile & [identity-overrides provenance-overrides]]
  (let [profile-hash (analysis-identity/tokenizer-profile-hash profile)]
    {"artifact_id" artifact-id
     "artifact_kind" "tokenized"
     "validation_status" "passed"
     "manifest_identity_object" (merge
                                 {"manifest_schema_hash" (files/example-hash "01")
                                  "corpus_snapshot_hash" (files/example-hash "02")
                                  "work_content_hash" (files/example-hash "03")
                                  "metadata_record_hash" nil
                                  "parser_build_hash" (files/example-hash "04")
                                  "parser_config_hash" (files/example-hash "05")
                                  "aat_parser_ir_mapping_hash" (files/example-hash "06")
                                  "parser_ir_schema_hash" (files/example-hash "07")
                                  "tei_profile_hash" nil
                                  "tokenizer_build_hash" (get-in profile ["tokenizer" "build_hash"])
                                  "tokenizer_dictionary_hash" (get-in profile ["dictionary" "archive_hash"])
                                  "tokenizer_profile_hash" profile-hash
                                  "analysis_recipe_hash" nil
                                  "output_format_spec_hash" (get profile "token_output_schema_hash")}
                                 identity-overrides)
     "content" {"content_hash" (files/example-hash "11")
                "media_type" "application/json"}
     "provenance" (merge {"used" [producer-artifact-id profile-hash]
                          "was_derived_from" [producer-artifact-id]}
                         provenance-overrides)}))

(defn annotation-manifest-from-producer
  [artifact-id producer-artifact-id policy-hash & [identity-overrides provenance-overrides]]
  {"artifact_id" artifact-id
   "artifact_kind" "annotation"
   "validation_status" "passed"
   "manifest_identity_object" (merge
                               {"manifest_schema_hash" (files/example-hash "01")
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
                                "tokenizer_profile_hash" nil
                                "analysis_recipe_hash" nil
                                "annotation_policy_hash" policy-hash
                                "output_format_spec_hash" (files/example-hash "09")}
                               identity-overrides)
   "content" {"content_hash" (files/example-hash "11")
              "media_type" "application/json"}
   "provenance" (merge {"used" [producer-artifact-id policy-hash]
                        "was_derived_from" [producer-artifact-id]}
                       provenance-overrides)})

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

(defn- reproducibility-conflicts-assertions []
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
                    "warning"))])))
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
                      "warning"))]))))

(deftest reproducibility-conflicts-test
  (reproducibility-conflicts-assertions))

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

(deftest tokenized-release-guardrail-test
  (let [passed-tokenized (tokenized-manifest (files/example-hash "61")
                                             "passed"
                                             (files/example-hash "64"))
        warning-tokenized (tokenized-manifest (files/example-hash "62")
                                              "warning"
                                              (files/example-hash "65"))
        failed-tokenized (tokenized-manifest (files/example-hash "63") "failed")
        missing-profile-tokenized (tokenized-manifest (files/example-hash "66")
                                                      "passed")
        null-profile-tokenized (tokenized-manifest (files/example-hash "67")
                                                   "warning"
                                                   nil)
        entries (manifest-index/index-entries
                 {"passed-tokenized.manifest.json" passed-tokenized
                  "warning-tokenized.manifest.json" warning-tokenized
                  "failed-tokenized.manifest.json" failed-tokenized
                  "missing-profile-tokenized.manifest.json" missing-profile-tokenized
                  "null-profile-tokenized.manifest.json" null-profile-tokenized})]
    (is (= [{:artifact_id (files/example-hash "66")
             :manifest_path "missing-profile-tokenized.manifest.json"
             :validation_status "passed"}
            {:artifact_id (files/example-hash "67")
             :manifest_path "null-profile-tokenized.manifest.json"
             :validation_status "warning"}]
           (manifest-index/tokenized-release-guardrail-errors entries)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Successful tokenized manifests require tokenizer_profile_hash"
                          (manifest-index/validate-tokenized-release-guardrail! entries)))))

(deftest tokenized-copied-field-validation-test
  (let [producer-id (files/example-hash "71")
        tokenized-id (files/example-hash "72")
        profile (tokenizer-profile)
        profile-hash (analysis-identity/tokenizer-profile-hash profile)
        producer (parser-ir-manifest producer-id (files/example-hash "04"))
        good-tokenized (tokenized-manifest-from-producer tokenized-id
                                                         producer-id
                                                         profile)
        bad-parser-copy (tokenized-manifest-from-producer
                         (files/example-hash "73")
                         producer-id
                         profile
                         {"parser_build_hash" (files/example-hash "99")})
        bad-profile-copy (tokenized-manifest-from-producer
                          (files/example-hash "74")
                          producer-id
                          profile
                          {"tokenizer_build_hash" (files/example-hash "98")})
        bad-dictionary-copy (tokenized-manifest-from-producer
                             (files/example-hash "75")
                             producer-id
                             profile
                             {"tokenizer_dictionary_hash" (files/example-hash "97")})]
    (is (empty? (manifest-index/tokenized-copied-field-errors
                 (manifest-index/index-entries {"parser.manifest.json" producer
                                                "tokenized.manifest.json" good-tokenized})
                 {profile-hash profile})))
    (is (= [{:tokenized_artifact_id (files/example-hash "73")
             :producer_artifact_id producer-id
             :source :producer
             :field "parser_build_hash"
             :tokenized_value (files/example-hash "99")
             :expected_value (files/example-hash "04")}
            {:tokenized_artifact_id (files/example-hash "74")
             :tokenizer_profile_hash profile-hash
             :source :tokenizer-profile
             :field "tokenizer_build_hash"
             :tokenized_value (files/example-hash "98")
             :expected_value (get-in profile ["tokenizer" "build_hash"])}
            {:tokenized_artifact_id (files/example-hash "75")
             :tokenizer_profile_hash profile-hash
             :source :tokenizer-profile
             :field "tokenizer_dictionary_hash"
             :tokenized_value (files/example-hash "97")
             :expected_value (get-in profile ["dictionary" "archive_hash"])}]
           (manifest-index/tokenized-copied-field-errors
            (manifest-index/index-entries {"parser.manifest.json" producer
                                           "bad-parser.manifest.json" bad-parser-copy
                                           "bad-profile.manifest.json" bad-profile-copy
                                           "bad-dictionary.manifest.json" bad-dictionary-copy})
            {profile-hash profile})))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Tokenized manifest copied identity fields differ from producer or tokenizer profile"
                          (manifest-index/validate-tokenized-copied-fields!
                           (manifest-index/index-entries {"parser.manifest.json" producer
                                                          "bad-profile.manifest.json" bad-profile-copy})
                           {profile-hash profile})))))

(deftest tokenized-copied-field-validation-requires-profile-provenance-test
  (let [producer-id (files/example-hash "81")
        tokenized-id (files/example-hash "82")
        profile (tokenizer-profile)
        profile-hash (analysis-identity/tokenizer-profile-hash profile)
        producer (parser-ir-manifest producer-id (files/example-hash "04"))
        tokenized (tokenized-manifest-from-producer tokenized-id
                                                    producer-id
                                                    profile
                                                    {}
                                                    {"used" [producer-id]})]
    (is (= [{:tokenized_artifact_id tokenized-id
             :tokenizer_profile_hash profile-hash
             :source :provenance
             :field "tokenizer_profile_hash"
             :tokenized_value nil
             :expected_value profile-hash}]
           (manifest-index/tokenized-copied-field-errors
            (manifest-index/index-entries {"parser.manifest.json" producer
                                           "tokenized.manifest.json" tokenized})
            {profile-hash profile})))))

(deftest annotation-copied-field-validation-test
  (let [producer-id (files/example-hash "91")
        annotation-id (files/example-hash "92")
        policy-hash (files/example-hash "93")
        producer (parser-ir-manifest producer-id (files/example-hash "04"))
        good-annotation (annotation-manifest-from-producer annotation-id
                                                           producer-id
                                                           policy-hash)
        bad-parser-copy (annotation-manifest-from-producer
                         (files/example-hash "94")
                         producer-id
                         policy-hash
                         {"parser_build_hash" (files/example-hash "99")})]
    (is (empty? (manifest-index/annotation-copied-field-errors
                 (manifest-index/index-entries {"parser.manifest.json" producer
                                                "annotation.manifest.json" good-annotation}))))
    (is (= [{:annotation_artifact_id (files/example-hash "94")
             :producer_artifact_id producer-id
             :source :producer
             :field "parser_build_hash"
             :annotation_value (files/example-hash "99")
             :expected_value (files/example-hash "04")}]
           (manifest-index/annotation-copied-field-errors
            (manifest-index/index-entries {"parser.manifest.json" producer
                                           "bad-parser.manifest.json" bad-parser-copy}))))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Annotation manifest copied identity fields differ from producer"
                          (manifest-index/validate-annotation-copied-fields!
                           (manifest-index/index-entries {"parser.manifest.json" producer
                                                          "bad-parser.manifest.json" bad-parser-copy}))))))

(deftest annotation-release-guardrail-test
  (let [producer-id (files/example-hash "95")
        passed-annotation (annotation-manifest-from-producer (files/example-hash "96")
                                                             producer-id
                                                             (files/example-hash "97"))
        null-policy-annotation (annotation-manifest-from-producer
                                (files/example-hash "98")
                                producer-id
                                nil
                                {}
                                {"used" [producer-id]})
        failed-annotation (-> (annotation-manifest-from-producer
                               (files/example-hash "93")
                               producer-id
                               nil
                               {}
                               {"used" [producer-id]})
                              (assoc "validation_status" "failed"))
        missing-key-annotation (-> (annotation-manifest-from-producer
                                    (files/example-hash "94")
                                    producer-id
                                    (files/example-hash "97"))
                                   (update "manifest_identity_object"
                                           dissoc "annotation_policy_hash"))
        entries (manifest-index/index-entries
                 {"passed-annotation.manifest.json" passed-annotation
                  "null-policy-annotation.manifest.json" null-policy-annotation
                  "failed-annotation.manifest.json" failed-annotation
                  "missing-key-annotation.manifest.json" missing-key-annotation})]
    (is (= [{:artifact_id (files/example-hash "94")
             :manifest_path "missing-key-annotation.manifest.json"
             :validation_status "passed"}
            {:artifact_id (files/example-hash "98")
             :manifest_path "null-policy-annotation.manifest.json"
             :validation_status "passed"}]
           (manifest-index/annotation-release-guardrail-errors entries))
        "explicit-null AND missing-key both reported; failed-status excluded")
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Successful annotation manifests require annotation_policy_hash"
                          (manifest-index/validate-annotation-release-guardrail! entries)))))
