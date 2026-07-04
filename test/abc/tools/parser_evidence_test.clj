(ns abc.tools.parser-evidence-test
  (:require [abc.tools.parser-evidence :as parser-evidence]
            [clojure.test :refer [deftest is testing]]))

(def valid-entry
  {:evidence_id "ab-validator/example"
   :evidence_class :conversion-compatibility
   :producer_component "ab-validator"
   :logical_path "ab-validator/docs/example.md"
   :current_external_path "../ab-validator/docs/example.md"
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
