(ns abc.tools.schema-validation-evidence-test
  (:require [abc.tools.evidence-io :as evidence-io]
            [abc.tools.files :as files]
            [abc.tools.validate-design-bundle :as validate]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(defn- with-repository-read-trace [thunk]
  (let [traced (evidence-io/with-read-trace
                 {:identity-root "." :cwd-root "."}
                 thunk)]
    (is (every? string? (:repository-paths traced)))
    (:value traced)))

(deftest broken-manifest-is-rejected-test
  (with-repository-read-trace
    (fn []
      (let [schema (files/read-json "schemas/manifest.schema.json")]
        (is (seq (validate/validation-errors schema {})))))))

(deftest canonicalization-hash-mismatch-is-rejected-test
  (with-repository-read-trace
    (fn []
      (evidence-io/with-owned-ephemeral-root
        (fn [dir]
          (let [identity-json (io/file (str dir) "identity.json")
                array-a (io/file (str dir) "array-a.json")
                array-b (io/file (str dir) "array-b.json")]
            (spit identity-json "{}\n")
            (spit array-a "[]\n")
            (spit array-b "[1]\n")
            (is (thrown-with-msg?
                 clojure.lang.ExceptionInfo
                 #"canonical identity fixture hash mismatch"
                 (validate/validate-canonicalization!
                  {:expected (apply str (repeat 64 "0"))
                   :identity-json (str identity-json)
                   :array-a (str array-a)
                   :array-b (str array-b)})))))))))

(deftest supported-entrypoint-delegates-test
  (with-repository-read-trace
    (fn []
      (is (str/includes? (files/read-text "bin/validate-design-bundle.sh")
                         "exec clojure -M:abc/validate-design-bundle")))))

(deftest supported-ci-wiring-test
  (with-repository-read-trace
    (fn []
      (is (str/includes? (files/read-text ".github/workflows/validation.yml")
                         ".#validate-design-bundle")))))
