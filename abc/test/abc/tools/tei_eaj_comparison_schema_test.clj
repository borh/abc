(ns abc.tools.tei-eaj-comparison-schema-test
  (:require [abc.tools.files :as files]
            [abc.tools.schema :as schema]
            [clojure.test :refer [deftest is testing]]))

(def ^:private comparison-schema-path
  "schemas/tei-eaj-comparison.schema.json")

(def ^:private comparison-fixture-path
  "fixtures/tei-eaj-comparison/workset-export.json")

(deftest tei-eaj-comparison-export-fixture-validates-test
  (let [comparison-schema (schema/read-schema comparison-schema-path)
        fixture (files/read-json comparison-fixture-path)]
    (is (= "tei-eaj-aozora-workset-export-v1"
           (get fixture "schema_version")))
    (is (nil? (schema/validation-errors comparison-schema fixture)))))

(deftest tei-eaj-comparison-export-rejects-unknown-base-text-relation-test
  (testing "comparison relation classes are a tracked vocabulary"
    (let [comparison-schema (schema/read-schema comparison-schema-path)
          fixture (files/read-json comparison-fixture-path)
          invalid-fixture (assoc-in fixture
                                    ["files" 0 "base_text_relation"]
                                    "almost_equal")]
      (is (seq (schema/validation-errors comparison-schema invalid-fixture))))))

(deftest tei-eaj-comparison-export-rejects-unknown-relation-count-buckets-test
  (testing "summary relation counts share the per-row relation vocabulary"
    (let [comparison-schema (schema/read-schema comparison-schema-path)
          fixture (files/read-json comparison-fixture-path)
          invalid-fixture (assoc-in fixture
                                    ["summary" "base_text_relation_counts" "almost_equal"]
                                    1)]
      (is (seq (schema/validation-errors comparison-schema invalid-fixture))))))
