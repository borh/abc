(ns abc.tools.iiif-test
  (:require [abc.tools.iiif :as iiif]
            [clojure.test :refer [deftest is testing]]))

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
