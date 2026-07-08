(ns abc.tools.materialize-tokenized-test
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.manifest-index :as manifest-index]
            [abc.tools.materialize-tokenized :as materialize-tokenized]
            [abc.tools.schema :as schema]
            [clojure.test :refer [deftest is]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn producer-manifest []
  {"artifact_id" (files/example-hash "91")
   "artifact_kind" "parser-ir"
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

(defn fixture-tokens []
  [{"token_index" 0
    "input_span" {"start" 0
                  "end" 2}
    "text" "吾輩"}
   {"token_index" 1
    "input_span" {"start" 2
                  "end" 3}
    "text" "猫"}])

(deftest materialize-tokenized-copies-producer-and-profile-identity-test
  (let [dir (Files/createTempDirectory "abc-tokenized" (make-array FileAttribute 0))
        out-dir (.toFile dir)
        profile (files/read-json "data/tokenizer-profiles/fixture-tokenizer-ja-v1.json")
        profile-hash (analysis-identity/tokenizer-profile-hash profile)
        result (materialize-tokenized/materialize-tokenized!
                {:producer-manifest (producer-manifest)
                 :tokenizer-profile profile
                 :input-plaintext-policy-hash (files/example-hash "aa")
                 :tokens (fixture-tokens)
                 :output-dir out-dir
                 :generated-at "2026-07-08T00:00:00Z"})]
    (try
      (let [manifest-file (:manifest result)
            token-stream-file (:token-stream result)
            tokenized-manifest (files/read-json manifest-file)
            token-stream (files/read-json token-stream-file)
            manifest-schema (files/read-json "schemas/manifest.schema.json")
            token-output-schema (files/read-json "schemas/token-output.schema.json")
            identity-object (get tokenized-manifest "manifest_identity_object")
            entries (manifest-index/index-entries
                     {"parser.manifest.json" (producer-manifest)
                      "tokenized.manifest.json" tokenized-manifest})]
        (is (nil? (schema/validation-errors manifest-schema tokenized-manifest)))
        (is (nil? (schema/validation-errors token-output-schema token-stream)))
        (is (= "tokenized" (get tokenized-manifest "artifact_kind")))
        (is (= "token-stream"
               (get-in tokenized-manifest ["sidecars" 0 "role"])))
        (is (= (files/example-hash "03")
               (get identity-object "parser_build_hash")))
        (is (= (files/example-hash "04")
               (get identity-object "parser_config_hash")))
        (is (= (files/example-hash "05")
               (get identity-object "aat_parser_ir_mapping_hash")))
        (is (= (files/example-hash "06")
               (get identity-object "parser_ir_schema_hash")))
        (is (= (get-in profile ["tokenizer" "build_hash"])
               (get identity-object "tokenizer_build_hash")))
        (is (= (get-in profile ["dictionary" "archive_hash"])
               (get identity-object "tokenizer_dictionary_hash")))
        (is (= profile-hash
               (get identity-object "tokenizer_profile_hash")))
        (is (nil? (get identity-object "analysis_recipe_hash")))
        (is (= (manifest/schema-hash "schemas/token-output.schema.json")
               (get identity-object "output_format_spec_hash")))
        (is (= profile-hash
               (get token-stream "tokenizer_profile_hash")))
        (is (= (get-in (producer-manifest) ["content" "content_hash"])
               (get-in token-stream ["producer" "content_hash"])))
        (is (= (fixture-tokens)
               (get token-stream "tokens")))
        (is (some #{(get (producer-manifest) "artifact_id")}
                  (get-in tokenized-manifest ["provenance" "was_derived_from"])))
        (is (some #{profile-hash}
                  (get-in tokenized-manifest ["provenance" "used"])))
        (is (empty? (manifest-index/tokenized-copied-field-errors
                     entries
                     {profile-hash profile})))
        (is (= (get tokenized-manifest "artifact_id")
               (manifest/artifact-id identity-object))))
      (finally
        (doseq [file (reverse (file-seq out-dir))]
          (.delete file))))))
