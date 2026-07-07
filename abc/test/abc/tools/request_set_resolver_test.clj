(ns abc.tools.request-set-resolver-test
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.request-set-resolver :as resolver]
            [clojure.test :refer [deftest is testing]]))

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
