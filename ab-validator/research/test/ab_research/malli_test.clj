(ns ab-research.malli-test
  (:require [ab-research.malli :as am]
            [clojure.test :refer [deftest is testing]]
            [malli.core :as m]
            [malli.registry :as mr]))

(def ^:private registry
  (mr/composite-registry (m/default-schemas) am/scalar-schemas))

(defn- valid? [schema value]
  (m/validate schema value {:registry registry}))

(deftest contract-scalar-schemas-test
  (testing "shared scalar schemas accept valid values"
    (is (valid? ::am/nonblank-string "ab-validator/research"))
    (is (valid? ::am/nullable-nonblank-string nil))
    (is (valid? ::am/nullable-nonblank-string "ab-validator/research"))
    (is (valid? ::am/sha256-hash
                "sha256:bf0910f5316efc2cd528f504c0cbd16816ca993e7ccb2b99122aab8a71359527"))
    (is (valid? ::am/semver "0.2.3"))
    (is (valid? ::am/positive-int 1))
    (is (valid? ::am/nonnegative-int 0))
    (is (valid? ::am/workspace-logical-path "ab-validator/research/docs/report.md"))
    (is (valid? ::am/concrete-adapter "aozora2html")))
  (testing "shared scalar schemas reject invalid values"
    (is (not (valid? ::am/nonblank-string "")))
    (is (not (valid? ::am/nullable-nonblank-string "")))
    (is (not (valid? ::am/sha256-hash "sha256:not-real")))
    (is (not (valid? ::am/semver "0.2")))
    (is (not (valid? ::am/positive-int 0)))
    (is (not (valid? ::am/nonnegative-int -1)))
    (is (not (valid? ::am/workspace-logical-path "../research/docs/report.md")))
    (is (not (valid? ::am/workspace-logical-path "/ab-validator/research/docs/report.md")))
    (is (not (valid? ::am/concrete-adapter "*")))))

(deftest explanation-messages-return-schema-messages-test
  (let [explanation (m/explain [:map
                                [:evidence_id ::am/nonblank-string]
                                [:logical_path ::am/workspace-logical-path]
                                [:sha256 ::am/sha256-hash]]
                               {:evidence_id ""
                                :logical_path "../bad.md"
                                :sha256 "sha256:not-real"}
                               {:registry registry})
        messages (am/explanation-messages explanation)]
    (is (some #(re-find #"must be a non-empty string" %) messages))
    (is (some #(re-find #"must be workspace-relative" %) messages))
    (is (some #(re-find #"must be a sha256 hash" %) messages))))

(deftest explanation-messages-render-missing-keys-test
  (let [explanation (m/explain [:map [:sha256 ::am/sha256-hash]]
                               {}
                               {:registry registry})]
    (is (= ["is missing :sha256"]
           (am/explanation-messages explanation)))))

(deftest explain-or-throw!-uses-the-explicit-registry-test
  (is (= :ok (am/explain-or-throw! ::am/sha256-hash
                                   "sha256:bf0910f5316efc2cd528f504c0cbd16816ca993e7ccb2b99122aab8a71359527"
                                   "test"
                                   registry)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"failed malli validation"
                        (am/explain-or-throw! ::am/sha256-hash
                                              "sha256:not-real"
                                              "test"
                                              registry))))
