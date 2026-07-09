(ns abc.tools.soranoha-annotation-test
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.schema :as schema]
            [abc.tools.snapshot-index :as snapshot-index]
            [abc.tools.soranoha :as soranoha]
            [abc.tools.source-snapshot-fixture :as fixture]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(def ^:private ruby-gaiji-policy-hash
  "sha256:6b2b9f29b742d434a2a114ace68549c3ad2611454785cb7a6924f5eaf95babde")

(deftest reproduce-demo-annotation-ja-materializes-annotation-artifacts-test
  (let [root (fixture/temp-dir "abc-demo-annotation-root")]
    (try
      (let [{:keys [snapshot]} (soranoha/materialize-snapshot-root!
                                "demo-annotation-ja" root)
            manifest-schema (schema/read-schema "schemas/manifest.schema.json")
            output-schema (schema/read-schema "schemas/annotation-output.schema.json")]
        (doseq [slug ["demo-fixture-a" "demo-fixture-b"]]
          (testing slug
            (let [base (io/file root "artifacts" "works" slug "annotations")
                  manifest (files/read-json (io/file base "annotation.manifest.json"))
                  value (files/read-json (io/file base "body-annotations.json"))
                  producer (files/read-json
                            (io/file root "artifacts" "works" slug
                                     "parser-ir" "parser-ir.manifest.json"))]
              (is (nil? (schema/validation-errors manifest-schema manifest)))
              (is (nil? (schema/validation-errors output-schema value)))
              (is (= "annotation" (get manifest "artifact_kind")))
              (is (= ruby-gaiji-policy-hash
                     (get-in manifest ["manifest_identity_object"
                                       "annotation_policy_hash"])))
              (is (= ruby-gaiji-policy-hash
                     (get value "annotation_policy_hash")))
              (testing "input_plaintext_policy_hash is the plaintext view's policy_hash"
                (let [request-set (files/read-json
                                   "data/request-sets/demo-annotation-ja.json")
                      plaintext-view (first
                                      (filter #(= "parser-ir-plaintext-body-v1"
                                                  (get % "input_view_kind"))
                                              (get-in request-set
                                                      ["request_set_identity_object"
                                                       "input_views"])))]
                  (is (= (get plaintext-view "policy_hash")
                         (get value "input_plaintext_policy_hash")))))
              (testing "copied parser-IR identity fields match the producer"
                (doseq [field ["parser_build_hash" "parser_config_hash"
                               "aat_parser_ir_mapping_hash" "parser_ir_schema_hash"]]
                  (is (= (get-in producer ["manifest_identity_object" field])
                         (get-in manifest ["manifest_identity_object" field]))
                      field))))))
        (testing "snapshot index references the annotation manifests"
          (let [kinds (frequencies
                       (map #(get % "artifact_kind")
                            (get snapshot "artifact_references")))]
            (is (= 2 (get kinds "annotation")))))
        (testing "snapshot index remains internally consistent"
          (is (true? (snapshot-index/validate-snapshot-index! snapshot)))))
      (finally
        (fixture/delete-tree! root)))))

(deftest reproduce-demo-basic-ja-produces-no-annotation-artifacts-test
  (let [root (fixture/temp-dir "abc-demo-basic-root")]
    (try
      (soranoha/materialize-snapshot-root! "demo-basic-ja" root)
      (doseq [slug ["demo-fixture-a" "demo-fixture-b"]]
        (is (not (.exists (io/file root "artifacts" "works" slug "annotations")))
            slug))
      (finally
        (fixture/delete-tree! root)))))

(deftest validate-annotation-manifests-passes-on-materialized-root-test
  (let [root (fixture/temp-dir "abc-annotation-guardrail-root")]
    (try
      (soranoha/materialize-snapshot-root! "demo-annotation-ja" root)
      (let [manifests (->> (file-seq (io/file root "artifacts"))
                           (filter #(and (.isFile %)
                                         (.endsWith (.getName %) ".manifest.json")))
                           (mapv str))]
        (is (nil? (soranoha/validate-annotation-manifests! manifests))))
      (finally
        (fixture/delete-tree! root)))))

(deftest validate-annotation-manifests-rejects-stripped-policy-hash-test
  (let [root (fixture/temp-dir "abc-annotation-guardrail-bad-root")]
    (try
      (soranoha/materialize-snapshot-root! "demo-annotation-ja" root)
      (let [manifest-file (io/file root "artifacts" "works" "demo-fixture-a"
                                   "annotations" "annotation.manifest.json")
            manifest-value (files/read-json manifest-file)]
        (manifest/write-json-file!
         manifest-file
         (update manifest-value "manifest_identity_object"
                 dissoc "annotation_policy_hash"))
        (let [manifests (->> (file-seq (io/file root "artifacts"))
                             (filter #(and (.isFile %)
                                           (.endsWith (.getName %) ".manifest.json")))
                             (mapv str))]
          (is (thrown? clojure.lang.ExceptionInfo
                       (soranoha/validate-annotation-manifests! manifests)))))
      (finally
        (fixture/delete-tree! root)))))
