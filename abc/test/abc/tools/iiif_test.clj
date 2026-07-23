(ns abc.tools.iiif-test
  (:require [abc.tools.files :as files]
            [abc.tools.iiif :as iiif]
            [abc.tools.schema :as schema]
            [clojure.test :refer [deftest is testing]]))

(deftest applicability-schema-is-valid-draft-2020-12-test
  (let [path "schemas/iiif-applicability.schema.json"
        value (files/read-json path)]
    (is (= "https://json-schema.org/draft/2020-12/schema"
           (get value "$schema")))
    (is (nil? (schema/schema-valid! value path)))))

(deftest iiif-text-only-values-contract-test
  (let [value (files/read-json
               "examples/v0/example-work/iiif/applicability.json")]
    (is (contains? value "derived_manifest"))
    (is (= ["000127" "not_applicable" nil]
           [(get value "work_id")
            (get value "status")
            (get value "derived_manifest")]))
    (is (re-find #"text-only" (get value "reason")))))

(deftest applicability-schema-accepts-not-applicable-text-only-test
  (testing "the committed example bundle's applicability record validates"
    (is (nil? (iiif/validate-applicability!
               "examples/v0/example-work/iiif/applicability.json")))))

(deftest applicability-schema-accepts-applicable-with-manifest-test
  (is (nil? (iiif/validate-applicability!
             "fixtures/iiif/applicable.json"))))

(deftest applicability-schema-accepts-rights-blocker-test
  (is (nil? (iiif/validate-applicability!
             "fixtures/iiif/rights_blocker.json"))))

(deftest applicability-schema-rejects-missing-work-id-test
  (is (thrown-with-msg? clojure.lang.ExceptionInfo
                        #"JSON Schema validation failed"
                        (iiif/validate-applicability!
                         "fixtures/iiif/invalid/missing-work-id.json"))))

(deftest applicability-schema-rejects-applicable-without-manifest-test
  (testing "status=applicable with derived_manifest=null fails the conditional"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"JSON Schema validation failed"
                          (iiif/validate-applicability!
                           "fixtures/iiif/invalid/applicable-without-manifest.json")))))

(deftest applicability-schema-rejects-not-applicable-with-manifest-test
  (testing "non-applicable records must not carry a derived_manifest path"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"JSON Schema validation failed"
                          (iiif/validate-applicability!
                           "fixtures/iiif/invalid/not-applicable-with-manifest.json")))))
