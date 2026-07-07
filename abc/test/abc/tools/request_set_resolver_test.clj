(ns abc.tools.request-set-resolver-test
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.json :as abc-json]
            [abc.tools.manifest :as manifest]
            [abc.tools.request-set-resolver :as resolver]
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
                   "policy_hash" "sha256:df21c590fd8d5b934fd426e632a3d8a09c2bd3fa299ca6b4c8fe4c797e1d1391"}]
   "analysis_recipe_ids" ["literary-basic-ja-v1"]
   "tokenizer_profile_ids" []
   "missing_policy" "build-missing-only"
   "pack_policy" {"schema_id" "https://w3id.org/abc/policies/request-set-pack-policy-v1"
                  "policy_id" "no-pack-v1"
                  "pack_kind" "none"}})

(deftest resolve-request-set-computes-identity-from-definition-test
  (let [resolved (resolver/resolve-request-set "smoke-basic-ja")
        identity-object (get resolved "request_set_identity_object")
        recipe (files/read-json "data/analysis-recipes/literary-basic-ja-v1.json")
        recipe-hash (analysis-identity/analysis-recipe-hash recipe)
        recipe-label (first (get resolved "resolved_recipe_labels"))]
    (is (= "smoke-basic-ja" (get resolved "label")))
    (is (= "request-set-v1" (get resolved "request_set_schema_version")))
    (is (nil? (get resolved "fixture_role")))
    (is (= (manifest/schema-hash "schemas/request-set.schema.json")
           (get identity-object "schema_hash")))
    (is (= [recipe-hash]
           (get identity-object "analysis_recipe_hashes")))
    (is (= [] (get identity-object "tokenizer_profile_hashes")))
    (is (= (analysis-identity/request-set-id resolved)
           (get resolved "request_set_id")))
    (is (= "literary-basic-ja-v1" (get recipe-label "recipe_id")))
    (is (= recipe-hash (get recipe-label "analysis_recipe_hash")))
    (is (re-matches files/hash-pattern
                    (get recipe-label "registry_entry_hash")))))

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
