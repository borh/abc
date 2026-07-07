(ns abc.tools.malli-test
  (:require [abc.tools.malli :as am]
            [abc.tools.schema :as schema]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [malli.core :as m]
            [malli.generator :as mg]))

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

(deftest contract-scalar-schemas-test
  (am/install!)
  (testing "shared scalar schemas accept valid values"
    (is (m/validate ::am/nonblank-string "abc"))
    (is (m/validate ::am/nullable-nonblank-string nil))
    (is (m/validate ::am/nullable-nonblank-string "abc"))
    (is (m/validate ::am/sha256-hash
                    "sha256:bf0910f5316efc2cd528f504c0cbd16816ca993e7ccb2b99122aab8a71359527"))
    (is (m/validate ::am/semver "0.2.3"))
    (is (m/validate ::am/positive-int 1))
    (is (m/validate ::am/nonnegative-int 0))
    (is (m/validate ::am/workspace-logical-path "abc/docs/report.md"))
    (is (m/validate ::am/concrete-adapter "aozora2html")))
  (testing "shared scalar schemas reject invalid values"
    (is (not (m/validate ::am/nonblank-string "")))
    (is (not (m/validate ::am/nullable-nonblank-string "")))
    (is (not (m/validate ::am/sha256-hash "sha256:not-real")))
    (is (not (m/validate ::am/semver "0.2")))
    (is (not (m/validate ::am/positive-int 0)))
    (is (not (m/validate ::am/nonnegative-int -1)))
    (is (not (m/validate ::am/workspace-logical-path "../abc/docs/report.md")))
    (is (not (m/validate ::am/workspace-logical-path "/abc/docs/report.md")))
    (is (not (m/validate ::am/concrete-adapter "*")))))

(deftest registry-entry-schemas-generate-valid-examples-test
  (am/install!)
  (testing "parser evidence schema has a valid generated example"
    (let [entry (mg/generate ::am/parser-evidence-entry)]
      (is (m/validate ::am/parser-evidence-entry entry))
      (is (string? (:evidence_id entry)))))
  (testing "compatibility schema has a valid generated example"
    (let [entry (mg/generate ::am/aat-parser-ir-compat-entry)]
      (is (m/validate ::am/aat-parser-ir-compat-entry entry))
      (is (= (get entry :aat_adapter)
             (get-in entry [:evidence_scope :adapter]))))))

(deftest explanation-messages-return-schema-messages-test
  (am/install!)
  (let [explanation (m/explain ::am/parser-evidence-entry
                               {:evidence_id ""
                                :evidence_class :unknown
                                :producer_component "ab-validator"
                                :logical_path "../bad.md"
                                :sha256 "sha256:not-real"
                                :status :citable
                                :summary "x"})
        messages (am/explanation-messages explanation)]
    (is (some #(re-find #"must be a non-empty string" %) messages))
    (is (some #(re-find #"must be workspace-relative" %) messages))
    (is (some #(re-find #"must be a sha256 hash" %) messages))))
