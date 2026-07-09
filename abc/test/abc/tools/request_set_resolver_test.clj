(ns abc.tools.request-set-resolver-test
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.json :as abc-json]
            [abc.tools.manifest :as manifest]
            [abc.tools.request-set-resolver :as resolver]
            [abc.tools.schema :as schema]
            [abc.tools.source-snapshot-fixture :as fixture]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(defn- source-snapshot-definition [snapshot-path]
  {"request_set_definition_version" "request-set-definition-v1"
   "label" "source-snapshot-basic-ja"
   "subject_source" {"kind" "source-corpus-snapshot-v0"
                     "path" snapshot-path
                     "source_id_prefix" "aozora:"
                     "work_id_prefix" "aozora:"}
   "input_views" [{"input_view_kind" "parser-ir-plaintext-body-v1"
                   "policy_hash" "sha256:df21c590fd8d5b934fd426e632a3d8a09c2bd3fa299ca6b4c8fe4c797e1d1391"
                   "input_normalization_policy_hash" "sha256:530c59689dd909c171790036cddc7916f8685897b6342bfa794d4611816d3813"}]
   "analysis_recipe_ids" ["literary-basic-ja-v1"]
   "tokenizer_profile_ids" []
   "missing_policy" "build-missing-only"
   "pack_policy_id" "no-pack-v1"})

(defn- tokenizer-profile-definition []
  (assoc (source-snapshot-definition "unused-source-snapshot.json")
         "label" "tokenizer-profile-basic-ja"
         "corpus_snapshot_hash" (files/example-hash "aa")
         "subjects" [{"source_id" "aozora:000001"
                      "work_id" "aozora:000001"
                      "work_content_hash" (files/example-hash "bb")
                      "metadata_record_hash" nil}]
         "subject_source" nil
         "analysis_recipe_ids" []
         "tokenizer_profile_ids" ["fixture-tokenizer-ja-v1"]))

(deftest resolve-request-set-computes-identity-from-definition-test
  (let [resolved (resolver/resolve-request-set "smoke-basic-ja")
        identity-object (get resolved "request_set_identity_object")
        recipe (files/read-json "data/analysis-recipes/literary-basic-ja-v1.json")
        pack-policy (files/read-json "data/pack-policies/no-pack-v1.json")
        recipe-hash (analysis-identity/analysis-recipe-hash recipe)
        recipe-label (first (get resolved "resolved_recipe_labels"))
        pack-policy-label (get resolved "resolved_pack_policy_label")]
    (is (= "smoke-basic-ja" (get resolved "label")))
    (is (= "request-set-v1" (get resolved "request_set_schema_version")))
    (is (nil? (get resolved "fixture_role")))
    (is (= (manifest/schema-hash "schemas/request-set.schema.json")
           (get identity-object "schema_hash")))
    (is (= [recipe-hash]
           (get identity-object "analysis_recipe_hashes")))
    (is (= (analysis-identity/pack-policy-hash pack-policy)
           (get identity-object "pack_policy_hash")))
    (is (= [] (get identity-object "tokenizer_profile_hashes")))
    (is (= (analysis-identity/request-set-id resolved)
           (get resolved "request_set_id")))
    (is (= "literary-basic-ja-v1" (get recipe-label "recipe_id")))
    (is (= recipe-hash (get recipe-label "analysis_recipe_hash")))
    (is (re-matches files/hash-pattern
                    (get recipe-label "registry_entry_hash")))
    (is (= "no-pack-v1" (get pack-policy-label "policy_id")))
    (is (= (analysis-identity/pack-policy-hash pack-policy)
           (get pack-policy-label "pack_policy_hash")))
    (is (re-matches files/hash-pattern
                    (get pack-policy-label "registry_entry_hash")))))

(deftest resolve-request-set-golden-files-match-resolver-output-test
  (doseq [label (resolver/request-set-labels)]
    (testing label
      (is (= (resolver/resolve-request-set label)
             (files/read-json (str "data/request-sets/" label ".json")))))))

(deftest resolve-request-set-rejects-unknown-label-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Unknown request set"
       (resolver/resolve-request-set "missing-basic-ja"))))

(deftest resolve-request-set-resolves-tokenizer-profile-labels-test
  (with-redefs [resolver/read-request-set-definition
                (fn [_] (tokenizer-profile-definition))
                resolver/request-set-definition-path
                (fn [_] "unit/tokenizer-profile-basic-ja.json")]
    (let [resolved (resolver/resolve-request-set "tokenizer-profile-basic-ja")
          identity-object (get resolved "request_set_identity_object")
          profile (files/read-json "data/tokenizer-profiles/fixture-tokenizer-ja-v1.json")
          profile-hash (analysis-identity/tokenizer-profile-hash profile)
          profile-label (first (get resolved "resolved_tokenizer_profile_labels"))]
      (is (= [profile-hash]
             (get identity-object "tokenizer_profile_hashes")))
      (is (= "fixture-tokenizer-ja-v1"
             (get profile-label "profile_id")))
      (is (= profile-hash
             (get profile-label "tokenizer_profile_hash")))
      (is (re-matches files/hash-pattern
                      (get profile-label "registry_entry_hash")))
      (is (nil? (schema/validation-errors
                 (schema/read-schema "schemas/request-set.schema.json")
                 resolved)))
      (is (= (analysis-identity/request-set-id resolved)
             (get resolved "request_set_id"))))))

(deftest resolve-request-set-rejects-unknown-pack-policy-test
  (with-redefs [resolver/read-request-set-definition
                (fn [_] (assoc (source-snapshot-definition "unused-source-snapshot.json")
                               "label" "unknown-pack-policy-basic-ja"
                               "corpus_snapshot_hash" (files/example-hash "aa")
                               "subjects" [{"source_id" "aozora:000001"
                                            "work_id" "aozora:000001"
                                            "work_content_hash" (files/example-hash "bb")
                                            "metadata_record_hash" nil}]
                               "subject_source" nil
                               "pack_policy_id" "missing-pack-v1"))
                resolver/request-set-definition-path
                (fn [_] "unit/unknown-pack-policy-basic-ja.json")]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Unknown pack policy"
         (resolver/resolve-request-set "unknown-pack-policy-basic-ja")))))

(deftest full-corpus-request-sets-use-source-snapshot-subject-source-test
  (doseq [label ["full-corpus-publication-basic-ja"
                 "full-corpus-analysis-basic-ja"
                 "full-corpus-basic-ja"]]
    (testing label
      (let [definition (resolver/read-request-set-definition label)
            resolved (resolver/resolve-request-set label)
            identity-object (get resolved "request_set_identity_object")
            source-snapshot (files/read-json
                             (get-in definition ["subject_source" "path"]))]
        (is (nil? (get definition "subjects")))
        (is (= "source-corpus-snapshot-v0"
               (get-in definition ["subject_source" "kind"])))
        (is (= (get source-snapshot "snapshot_hash")
               (get identity-object "corpus_snapshot_hash")))
        (is (= 2 (count (get identity-object "subjects"))))))))

(deftest resolve-request-set-expands-subjects-from-source-snapshot-test
  (let [root (fixture/temp-dir "abc-source-snapshot-subjects")
        snapshot-file (io/file root "source-snapshot.json")]
    (try
      (let [snapshot (fixture/source-snapshot!
                      snapshot-file
                      [{"work_id" "000002"
                        "work_content_hash" (files/example-hash "b2")
                        "metadata_record_hash" nil}
                       {"work_id" "000001"
                        "work_content_hash" (files/example-hash "a1")
                        "metadata_record_hash" (files/example-hash "c1")}])]
        (with-redefs [resolver/read-request-set-definition
                      (fn [_] (source-snapshot-definition (str snapshot-file)))
                      resolver/request-set-definition-path
                      (fn [_] "unit/source-snapshot-basic-ja.json")]
          (let [resolved (resolver/resolve-request-set "source-snapshot-basic-ja")
                identity-object (get resolved "request_set_identity_object")]
            (is (= (get snapshot "snapshot_hash")
                   (get identity-object "corpus_snapshot_hash")))
            (is (= [{"source_id" "aozora:000001"
                     "work_id" "aozora:000001"
                     "work_content_hash" (files/example-hash "a1")
                     "metadata_record_hash" (files/example-hash "c1")}
                    {"source_id" "aozora:000002"
                     "work_id" "aozora:000002"
                     "work_content_hash" (files/example-hash "b2")
                     "metadata_record_hash" nil}]
                   (get identity-object "subjects")))
            (is (= (analysis-identity/request-set-id resolved)
                   (get resolved "request_set_id"))))))
      (finally
        (fixture/delete-tree! root)))))

(deftest resolve-request-set-can-use-supplied-source-snapshot-test
  (let [root (fixture/temp-dir "abc-supplied-source-snapshot")
        snapshot-file (io/file root "source-snapshot.json")]
    (try
      (let [snapshot (fixture/source-snapshot!
                      snapshot-file
                      [{"work_id" "000777"
                        "work_content_hash" (files/example-hash "77")
                        "metadata_record_hash" (files/example-hash "78")}])
            resolved (resolver/resolve-request-set
                      "full-corpus-basic-ja"
                      {:subject-source-path (str snapshot-file)})
            identity-object (get resolved "request_set_identity_object")]
        (is (= (get snapshot "snapshot_hash")
               (get identity-object "corpus_snapshot_hash")))
        (is (= [{"source_id" "aozora:000777"
                 "work_id" "aozora:000777"
                 "work_content_hash" (files/example-hash "77")
                 "metadata_record_hash" (files/example-hash "78")}]
               (get identity-object "subjects")))
        (is (= (str snapshot-file)
               (get-in resolved ["resolution" "subject_source_path"])))
        (is (= (analysis-identity/request-set-id resolved)
               (get resolved "request_set_id"))))
      (finally
        (fixture/delete-tree! root)))))

(deftest resolve-request-set-rejects-stale-source-snapshot-hash-test
  (let [root (fixture/temp-dir "abc-stale-source-snapshot")
        snapshot-file (io/file root "source-snapshot.json")]
    (try
      (let [snapshot (fixture/source-snapshot!
                      snapshot-file
                      [{"work_id" "000001"
                        "work_content_hash" (files/example-hash "a1")
                        "metadata_record_hash" nil}])]
        (abc-json/write-deterministic-json-file!
         snapshot-file
         (assoc snapshot "snapshot_hash" (files/example-hash "ff")))
        (with-redefs [resolver/read-request-set-definition
                      (fn [_] (source-snapshot-definition (str snapshot-file)))
                      resolver/request-set-definition-path
                      (fn [_] "unit/source-snapshot-basic-ja.json")]
          (is (thrown-with-msg?
               clojure.lang.ExceptionInfo
               #"Source snapshot hash mismatch"
               (resolver/resolve-request-set "source-snapshot-basic-ja")))))
      (finally
        (fixture/delete-tree! root)))))

(defn- token-recipe-definition [tokenizer-profile-ids]
  (assoc (source-snapshot-definition "unused-source-snapshot.json")
         "label" "token-recipe-coverage-basic-ja"
         "corpus_snapshot_hash" (files/example-hash "aa")
         "subjects" [{"source_id" "aozora:000001"
                      "work_id" "aozora:000001"
                      "work_content_hash" (files/example-hash "bb")
                      "metadata_record_hash" nil}]
         "subject_source" nil
         "analysis_recipe_ids" ["token-basic-ja-v1"]
         "tokenizer_profile_ids" tokenizer-profile-ids))

(deftest resolve-request-set-rejects-unconsumable-recipe-test
  ;; token-basic-ja-v1 supports only token-stream-v1; with no tokenizer
  ;; profile the request set provides nothing it can consume.
  (with-redefs [resolver/read-request-set-definition
                (fn [_] (token-recipe-definition []))
                resolver/request-set-definition-path
                (fn [_] "unit/token-recipe-coverage-basic-ja.json")]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Analysis recipe cannot consume any provided input view"
         (resolver/resolve-request-set "token-recipe-coverage-basic-ja")))))

(deftest resolve-request-set-accepts-token-recipe-with-tokenizer-profile-test
  (with-redefs [resolver/read-request-set-definition
                (fn [_] (token-recipe-definition ["fixture-tokenizer-ja-v1"]))
                resolver/request-set-definition-path
                (fn [_] "unit/token-recipe-coverage-basic-ja.json")]
    (let [resolved (resolver/resolve-request-set "token-recipe-coverage-basic-ja")]
      (is (= (analysis-identity/request-set-id resolved)
             (get resolved "request_set_id"))))))
