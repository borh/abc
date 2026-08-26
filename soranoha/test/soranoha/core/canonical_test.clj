(ns soranoha.core.canonical-test
  (:require [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [soranoha.core.canonical :as canonical]
            [soranoha.core.hash :as hash]))

;; The shared cross-language vector fixture is the binding contract between
;; this copy of the canonicalizer and abc's original: one canonicalizer, one
;; vector set. Path is relative to the soranoha/ component root.
(def shared-vectors-path
  "../abc/test/fixtures/canonicalization/rfc8785-safe-integer-domain-abc-v1-vectors.json")

(deftest shared-cross-language-vectors-test
  (let [fixture (json/read-json (io/file shared-vectors-path))]
    (testing "fixture identifies the algorithm this component implements"
      (is (= "sha256-rfc8785-safe-integer-domain-abc-v1"
             (get fixture "algorithm_id"))))
    (testing "every shared vector canonicalizes and hashes identically"
      (doseq [{:strs [name input canonical_json sha256]} (get fixture "vectors")]
        (is (= canonical_json
               (canonical/rfc8785-safe-integer-json-string-v1 input))
            name)
        (is (= sha256 (hash/sha256-canonical-json input))
            name)))))

(deftest safe-integer-domain-test
  (testing "exact boundaries accepted"
    (is (= "[-9007199254740991,9007199254740991]"
           (canonical/rfc8785-safe-integer-json-string-v1
            [-9007199254740991 9007199254740991]))))
  (testing "floats and unsafe integers fail closed"
    (doseq [value [1.0 1.5 Double/NaN Double/POSITIVE_INFINITY
                   9007199254740992 -9007199254740992]]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"outside the supported JSON domain"
           (canonical/rfc8785-safe-integer-json-string-v1 {"n" value}))))))
