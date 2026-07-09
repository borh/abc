(ns abc.tools.materialize-annotations-test
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.materialize-annotations :as mat-ann]
            [abc.tools.schema :as schema]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

;; Producer fixture mirrors the inline parser-ir producer-manifest built by
;; materialize_tokenized_test.clj (examples/ab-validator-output/manifest-inputs.json
;; is a flat producer-inputs record, not a parser-ir manifest, so it is not
;; reusable here — this materializer, like materialize-tokenized, consumes a
;; parser-ir producer manifest).
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

;; Same small ruby+gaiji parser-ir fixture as parser_ir_annotations_test.clj
;; (Task 3) — reused rather than inventing a new one.
(defn fixture-parser-ir []
  {"nodes"
   [{"type" "text" "text" "冒頭"}
    {"type" "ruby" "span" {"start" 6 "end" 12}
     "ruby" {"base" "吾輩" "reading" "わがはい" "scope" "explicit" "direction" "right"}}
    {"type" "text" "text" "は"}
    {"type" "gaiji" "span" {"start" 15 "end" 23}
     "gaiji" {"raw_marker" "※［＃「口＋七」］" "unicode" "𠮟" "resolved" true}}
    {"type" "text" "text" "る"}]})

(deftest materialize-annotations-copies-producer-and-policy-identity-test
  (let [dir (Files/createTempDirectory "abc-ann" (make-array FileAttribute 0))
        out-dir (.toFile dir)
        policy (files/read-json "data/annotation-policies/ruby-gaiji-v1.json")
        policy-hash (analysis-identity/annotation-policy-hash policy)
        result (mat-ann/materialize-annotations!
                {:producer-manifest (producer-manifest)
                 :parser-ir (fixture-parser-ir)
                 :annotation-policy policy
                 :input-plaintext-policy-hash (files/example-hash "aa")
                 :output-dir out-dir
                 :generated-at "2026-07-09T00:00:00Z"})]
    (try
      (let [manifest-file (:manifest result)
            annotations-file (:annotations result)
            manifest-value (files/read-json manifest-file)
            annotations-value (files/read-json annotations-file)
            manifest-schema (files/read-json "schemas/manifest.schema.json")
            annotation-output-schema (files/read-json "schemas/annotation-output.schema.json")
            ident (get manifest-value "manifest_identity_object")]
        (testing "manifest validates against the manifest schema"
          (is (nil? (schema/validation-errors manifest-schema manifest-value))))
        (testing "sidecar validates against the annotation output schema"
          (is (nil? (schema/validation-errors annotation-output-schema annotations-value))))
        (testing "identity: annotation kind, policy hash present, tokenizer/analysis fields null"
          (is (= "annotation" (get manifest-value "artifact_kind")))
          (is (re-matches #"sha256:[0-9a-f]{64}" (get ident "annotation_policy_hash")))
          (is (= policy-hash (get ident "annotation_policy_hash")))
          (is (nil? (get ident "tei_profile_hash")))
          (is (nil? (get ident "tokenizer_build_hash")))
          (is (nil? (get ident "tokenizer_dictionary_hash")))
          (is (nil? (get ident "tokenizer_profile_hash")))
          (is (nil? (get ident "analysis_recipe_hash")))
          (testing "parser/mapping fields copied byte-for-byte incl. nulls"
            (doseq [k ["corpus_snapshot_hash" "work_content_hash" "metadata_record_hash"
                       "parser_build_hash" "parser_config_hash"
                       "aat_parser_ir_mapping_hash" "parser_ir_schema_hash"]]
              (is (= (get-in (producer-manifest) ["manifest_identity_object" k])
                     (get ident k))
                  k))))
        (testing "provenance cites producer in used and was_derived_from"
          (let [prov (get manifest-value "provenance")]
            (is (some #{(get (producer-manifest) "artifact_id")} (get prov "used")))
            (is (some #{(get (producer-manifest) "artifact_id")} (get prov "was_derived_from")))))
        (testing "sidecar role"
          (is (= "body-annotations" (get-in manifest-value ["sidecars" 0 "role"]))))
        (testing "artifact_id is derived from the identity object"
          (is (= (get manifest-value "artifact_id")
                 (manifest/artifact-id ident))))
        (testing "ruby.scope withheld from output (mapping invention I-01 / D4)"
          (is (not (re-find #"\"scope\"" (slurp annotations-file))))))
      (finally
        (doseq [file (reverse (file-seq out-dir))]
          (.delete file))))))
