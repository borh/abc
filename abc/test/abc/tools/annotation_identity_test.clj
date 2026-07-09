(ns abc.tools.annotation-identity-test
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [clojure.test :refer [deftest is testing]]))

(deftest annotation-policy-hash-test
  (testing "policy value hashes with JCS discipline and is key-order independent"
    (let [policy (files/read-json "data/annotation-policies/ruby-gaiji-v1.json")
          reordered (into (sorted-map-by (comp - compare)) policy)]
      (is (re-matches #"sha256:[0-9a-f]{64}"
                      (analysis-identity/annotation-policy-hash policy)))
      (is (= (analysis-identity/annotation-policy-hash policy)
             (analysis-identity/annotation-policy-hash reordered))))))

(deftest ruby-gaiji-policy-shape-test
  (let [policy (files/read-json "data/annotation-policies/ruby-gaiji-v1.json")]
    (is (= "ruby-gaiji-v1" (get policy "policy_id")))
    (is (= ["gaiji" "ruby"] (vec (sort (get policy "annotation_kinds")))))
    (is (= "parser-ir-plaintext-body-v1" (get policy "aligns_to")))))

(deftest manifest-schema-accepts-annotation-kind-test
  (let [schema (files/read-json "schemas/manifest.schema.json")]
    (is (= "0.4.4" (get schema "version")))
    (is (some #{"annotation"} (get-in schema ["properties" "artifact_kind" "enum"])))
    (is (some #{"body-annotations"}
              (get-in schema ["$defs" "sidecar" "properties" "role" "enum"])))
    (is (some #{"annotation_policy_hash"}
              (get-in schema ["$defs" "identityObject" "required"])))))
