(ns abc.tools.request-set-fixture-test
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [clojure.test :refer [deftest is testing]]))

(def request-set-labels
  ["smoke-basic-ja"
   "demo-basic-ja"
   "full-corpus-publication-basic-ja"
   "full-corpus-analysis-basic-ja"
   "full-corpus-basic-ja"])

(deftest request-set-fixtures-carry-computed-request-set-ids-test
  (doseq [label request-set-labels]
    (testing label
      (let [request-set (files/read-json (str "data/request-sets/" label ".json"))]
        (is (= label (get request-set "label")))
        (is (= "request-set-shape-fixture" (get request-set "fixture_role")))
        (is (= (analysis-identity/request-set-id request-set)
               (get request-set "request_set_id")))
        (is (vector? (get-in request-set ["request_set_identity_object"
                                          "tokenizer_profile_hashes"])))))))
