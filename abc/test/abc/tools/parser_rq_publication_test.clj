(ns abc.tools.parser-rq-publication-test
  (:require [abc.tools.files :as files]
            [abc.tools.schema :as schema]
            [clojure.test :refer [deftest is]]))

(def schema-paths
  ["schemas/parser-rq-publication-policy.schema.json"
   "schemas/parser-rq-publication-work.schema.json"
   "schemas/parser-rq-publication-index.schema.json"
   "schemas/parser-rq-publication-aggregate.schema.json"])

(deftest publication-contracts-exist-and-are-closed
  (doseq [path schema-paths]
    (let [contract (files/read-json path)]
      (is (= false (get contract "additionalProperties")))
      (is (seq (schema/validation-errors contract {"attacker_field" true}))))))
