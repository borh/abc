(ns abc.tools.parser-ir-publication-policy-test
  (:require [abc.tools.files :as files]
            [abc.tools.hash :as hash]
            [abc.tools.parser-ir-publication-policy :as policy]
            [abc.tools.parser-ir-vocabulary :as vocab]
            [clojure.test :refer [deftest is testing]]))

(def policy-path "data/parser-ir-publication-policy-v0.json")

(deftest renderer-policy-covers-schema-node-types-test
  (let [schema-node-types (vocab/node-types
                           (files/read-json "schemas/parser-ir.schema.json"))
        p (policy/load-policy policy-path)]
    (doseq [renderer-name ["plaintext" "tei"]]
      (testing renderer-name
        (is (empty? (vocab/coverage-errors
                     schema-node-types
                     renderer-name
                     (policy/renderer-covered-node-types p renderer-name))))))))

(deftest policy-hash-is-jcs-json-test
  (testing "publication policy identity is canonical JSON, not source bytes"
    (is (= (hash/format-sha256
            (hash/sha256-json-jcs (files/read-json policy-path)))
           (policy/policy-hash policy-path)))))
