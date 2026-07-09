(ns abc.tools.fixture-hash-pinning-test
  "Committed example-work fixtures embed hashes of other committed sources;
  these pins recompute them so silent drift fails loudly. Family-wide
  extension of the annotation pin in annotation_identity_test.clj
  (example-work-fixture-embedded-hashes-pinned-test). Values that are
  fixture placeholders (matching no committed source) are deliberately
  not pinned — see comments."
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [clojure.test :refer [deftest is testing]]))

(deftest token-stream-fixture-embedded-hashes-pinned-test
  (let [fixture (files/read-json "examples/v0/example-work/token-stream.json")]
    (testing "schema_hash recomputes from the committed token-output schema"
      (is (= (manifest/schema-hash "schemas/token-output.schema.json")
             (get fixture "schema_hash"))
          "schema/policy changed: re-embed the freshly recomputed hash in the fixture (or revert the unintended edit)"))
    ;; tokenizer_profile_hash ("sha256:e4ecbc7a…") is a fixture placeholder —
    ;; it does not match the committed fixture-tokenizer-ja-v1 profile
    ;; ("sha256:ebda1b25…") — deliberately not pinned.
    ))

(deftest analysis-result-fixture-embedded-hashes-pinned-test
  (let [fixture (files/read-json "examples/v0/example-work/analysis-result.json")
        recipe (files/read-json "data/analysis-recipes/literary-basic-ja-v1.json")]
    (testing "schema_hash recomputes from the committed analysis-result schema"
      (is (= (manifest/schema-hash "schemas/analysis-result.schema.json")
             (get fixture "schema_hash"))
          "schema/policy changed: re-embed the freshly recomputed hash in the fixture (or revert the unintended edit)"))
    (testing "analysis_recipe_hash recomputes from the committed recipe"
      (is (= (analysis-identity/analysis-recipe-hash recipe)
             (get fixture "analysis_recipe_hash"))
          "analysis recipe changed: re-embed the freshly recomputed analysis_recipe_hash in the fixture (or revert the unintended edit)"))
    (testing "input normalization is the identity constant"
      (is (= analysis-identity/identity-normalization-policy-hash
             (get-in fixture ["input_view" "input_normalization_policy_hash"]))
          "identity-normalization-policy-hash constant changed: re-embed the freshly recomputed hash in the fixture (or revert the unintended edit)"))))
