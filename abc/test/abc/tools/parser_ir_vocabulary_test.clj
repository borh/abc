(ns abc.tools.parser-ir-vocabulary-test
  (:require [abc.tools.files :as files]
            [abc.tools.parser-ir-vocabulary :as vocab]
            [clojure.test :refer [deftest is testing]]))

(def expected-node-types
  #{"text" "ruby" "gaiji" "editor-note" "emphasis" "heading"
    "indentation" "page-break" "line-break" "image" "caption" "quote" "source-note"})

(deftest node-types-come-from-parser-ir-schema-test
  (testing "current parser-IR node vocabulary is derived from schema consts"
    (is (= expected-node-types
           (vocab/node-types (files/read-json "schemas/parser-ir.schema.json"))))))

(deftest coverage-errors-test
  (testing "missing node policies are named"
    (is (= ["plaintext renderer is missing parser-IR node policy for image"]
           (vocab/coverage-errors #{"text" "image"} "plaintext" #{"text"})))))
