(ns abc.tools.malli-test
  (:require [abc.tools.malli :as am]
            [abc.tools.schema :as schema]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [malli.core :as m]))

(use-fixtures :once (fn [f] (am/install!) (f)))

(deftest install!-is-idempotent
  (testing "calling install! twice produces an equal composite registry"
    (let [first-call (am/install!)
          second-call (am/install!)]
      (is (= first-call second-call)))))

(deftest install!-merges-project-registries
  (testing "annotation schema reachable through default registry"
    (is (m/validate :document/paragraphs
                    [{:paragraph/sentences
                      [{:sentence/annotated-text "x"
                        :sentence/text "x"}]}]))))

(deftest cached-schema-returns-identical-value
  (let [first-read (am/cached-schema "schemas/manifest.schema.json")
        second-read (am/cached-schema "schemas/manifest.schema.json")]
    (is (identical? first-read second-read))))

(deftest humanize-validation-errors-returns-readable-strings
  (let [schema (am/cached-schema "schemas/person-record.schema.json")
        errs (schema/validation-errors schema {"person_id" "not-six-digits"})
        humanized (am/humanize-validation-errors errs)]
    (is (sequential? humanized))
    (is (every? string? humanized))
    (is (some #(re-find #"person_id" %) humanized))))
