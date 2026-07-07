(ns abc.tools.parser-evidence-test
  (:require [abc.tools.malli :as am]
            [abc.tools.parser-evidence :as parser-evidence]
            [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [malli.generator :as mg]))

(def valid-entry
  {:evidence_id "ab-validator/example"
   :evidence_class :conversion-compatibility
   :producer_component "ab-validator"
   :logical_path "ab-validator/docs/example.md"
   :current_external_path "docs/example.md"
   :sha256 "sha256:bf0910f5316efc2cd528f504c0cbd16816ca993e7ccb2b99122aab8a71359527"
   :status :citable
   :summary "Example evidence."})

(defn- has-error?
  [pattern errors]
  (boolean (some #(re-find pattern %) errors)))

(deftest parser-evidence-index-validation-test
  (testing "accepts the committed evidence index"
    (is (= :ok (parser-evidence/validate-index!
                (parser-evidence/load-index)))))
  (testing "rejects missing required keys"
    (is (has-error? #"entry 0 is missing :sha256"
                    (parser-evidence/index-errors
                     {:entries [(dissoc valid-entry :sha256)]}))))
  (testing "rejects physical relative paths as identity paths"
    (is (has-error? #":logical_path must be workspace-relative"
                    (parser-evidence/index-errors
                     {:entries [(assoc valid-entry
                                       :logical_path
                                       "../ab-validator/docs/example.md")]}))))
  (testing "rejects invalid evidence classes"
    (is (has-error? #":evidence_class must be"
                    (parser-evidence/index-errors
                     {:entries [(assoc valid-entry
                                       :evidence_class
                                       :unknown)]}))))
  (testing "rejects duplicate evidence ids"
    (is (has-error? #"duplicates :evidence_id"
                    (parser-evidence/index-errors
                     {:entries [valid-entry
                                (assoc valid-entry
                                       :logical_path
                                       "ab-validator/docs/other.md")]})))))

(deftest parser-evidence-nullable-external-path-test
  (testing "current_external_path may be absent or nil but not blank"
    (is (empty? (parser-evidence/index-errors
                 {:entries [(dissoc valid-entry :current_external_path)]})))
    (is (empty? (parser-evidence/index-errors
                 {:entries [(assoc valid-entry :current_external_path nil)]})))
    (is (has-error? #":current_external_path must be null or a non-empty string"
                    (parser-evidence/index-errors
                     {:entries [(assoc valid-entry :current_external_path "")]})))))

(deftest parser-evidence-generator-backed-validation-test
  (am/install!)
  (testing "generated parser evidence entries pass the public validator"
    (let [entry (mg/generate ::am/parser-evidence-entry)]
      (is (m/validate ::am/parser-evidence-entry entry))
      (is (empty? (parser-evidence/index-errors {:entries [entry]})))))
  (testing "mutating a generated entry to violate a scalar contract is rejected"
    (let [entry (mg/generate ::am/parser-evidence-entry)
          invalid-entry (assoc entry :sha256 "sha256:not-real")]
      (is (has-error? #":sha256 must be a sha256 hash"
                      (parser-evidence/index-errors
                       {:entries [invalid-entry]}))))))
