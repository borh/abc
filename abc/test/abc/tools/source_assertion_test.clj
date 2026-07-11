(ns abc.tools.source-assertion-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(def ^:private valid-present
  {"source" "aozora"
   "field" "作品著作権フラグ"
   "lexical_value" "なし"
   "snapshot_hash" (str "sha256:" (apply str (repeat 64 "a")))})

(defn- validation-errors [value]
  (when-let [f (try
                 (requiring-resolve 'abc.tools.source-assertion/validation-errors)
                 (catch java.io.FileNotFoundException _ nil))]
    (f value)))

(deftest source-assertion-contract-exists-test
  (testing "the shared schema and validator are present"
    (is (.isFile (io/file "schemas/source-assertion.schema.json")))
    (is (some? (try
                 (requiring-resolve 'abc.tools.source-assertion/validation-errors)
                 (catch java.io.FileNotFoundException _ nil))))))

(deftest source-assertion-valid-values-test
  (testing "present and missing lexical values retain complete provenance"
    (is (nil? (validation-errors valid-present)))
    (is (nil? (validation-errors (assoc valid-present "lexical_value" nil))))))

(deftest source-assertion-invalid-values-test
  (testing "the envelope is closed and requires provenance"
    (is (seq (validation-errors (dissoc valid-present "source"))))
    (is (seq (validation-errors (dissoc valid-present "field"))))
    (is (seq (validation-errors (dissoc valid-present "snapshot_hash"))))
    (is (seq (validation-errors (assoc valid-present "unexpected" true)))))
  (testing "snapshot identity is a formatted SHA-256 value"
    (is (seq (validation-errors (assoc valid-present
                                       "snapshot_hash" "not-a-hash"))))))
