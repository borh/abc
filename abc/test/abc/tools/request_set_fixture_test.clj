(ns abc.tools.request-set-fixture-test
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(defn request-set-labels []
  (->> (file-seq (io/file "data/request-sets"))
       (filter #(.isFile %))
       (map #(.getName %))
       (filter #(.endsWith % ".json"))
       (map #(subs % 0 (- (count %) (count ".json"))))
       sort
       vec))

(deftest request-set-fixtures-carry-computed-request-set-ids-test
  (is (seq (request-set-labels)))
  (doseq [label (request-set-labels)]
    (testing label
      (let [request-set (files/read-json (str "data/request-sets/" label ".json"))]
        (is (= label (get request-set "label")))
        (is (= "request-set-shape-fixture" (get request-set "fixture_role")))
        (is (= (analysis-identity/request-set-id request-set)
               (get request-set "request_set_id")))
        (is (= [] (get-in request-set ["request_set_identity_object"
                                       "tokenizer_profile_hashes"])))))))
