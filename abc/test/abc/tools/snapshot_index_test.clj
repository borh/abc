(ns abc.tools.snapshot-index-test
  (:require [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.request-set-resolver :as request-set-resolver]
            [abc.tools.schema :as schema]
            [abc.tools.snapshot-index :as snapshot-index]
            [clojure.test :refer [deftest is testing]]))

(def artifact-a
  {"artifact_id" (files/example-hash "21")
   "artifact_kind" "plaintext"
   "sidecar_role" nil
   "validation_status" "passed"
   "manifest_content_hash" (files/example-hash "22")
   "content_hash" (files/example-hash "23")
   "locator" {"kind" "loose"
              "path" "artifacts/plaintext/0001.txt"}})

(def artifact-b
  {"artifact_id" (files/example-hash "31")
   "artifact_kind" "analysis"
   "sidecar_role" "analysis-result"
   "validation_status" "passed"
   "manifest_content_hash" (files/example-hash "32")
   "content_hash" (files/example-hash "33")
   "locator" {"kind" "archive-member"
              "archive_path" "artifacts/analysis/0000.tar.zst"
              "member_path" "0001/analysis-result.json"}})

(defn identity-args []
  {:snapshot-index-schema-hash (files/example-hash "01")
   :request-set-id (files/example-hash "02")
   :source-snapshot-hash (files/example-hash "03")
   :manifest-index-hash (files/example-hash "04")
   :artifact-references [artifact-b artifact-a artifact-a]
   :failure-policy {"allow_nonzero_failures" true
                    "max_failure_rate" nil
                    "per_diagnostic_tolerances" {}}
   :layout-policy {"loose_artifact_kinds" ["tei" "plaintext"]
                   "batched_artifact_kinds" ["analysis" "tokenized"]
                   "batch_target_work_count" 250
                   "archive_format" "tar.zst"}
   :schema-hashes [(files/example-hash "09") (files/example-hash "08")]
   :parser-evidence-hashes [(files/example-hash "07")]
   :tokenizer-profile-hashes []
   :analysis-recipe-hashes [(files/example-hash "06")]})

(defn- temp-json-file [prefix value]
  (let [file (java.io.File/createTempFile prefix ".json")]
    (manifest/write-json-file! file value)
    file))

(defn- manifest-fixture
  [{:keys [artifact-id artifact-kind validation-status content-hash]}]
  (cond-> {"artifact_id" artifact-id
           "artifact_kind" artifact-kind
           "validation_status" validation-status
           "sidecars" []
           "provenance" {"used" []
                         "was_derived_from" []}}
    content-hash
    (assoc "content" {"content_hash" content-hash
                      "media_type" "application/json"
                      "byte_length" 2
                      "path_hint" "artifact.json"})))

(deftest snapshot-label-requires-date-and-sequence-test
  (is (snapshot-index/snapshot-label? "soranoha-snapshot-2026-07-07-01"))
  (is (not (snapshot-index/snapshot-label? "soranoha-snapshot-2026-07-07")))
  (is (not (snapshot-index/snapshot-label? "latest"))))

(deftest snapshot-index-identity-canonicalizes-arrays-and-artifacts-test
  (let [identity (snapshot-index/snapshot-index-identity-object (identity-args))]
    (is (= [(files/example-hash "08") (files/example-hash "09")]
           (get identity "schema_hashes")))
    (is (= [] (get identity "tokenizer_profile_hashes")))
    (is (= (snapshot-index/artifact-set-hash [artifact-a artifact-b])
           (get identity "artifact_set_hash")))
    (is (= (snapshot-index/policy-hash (get (identity-args) :failure-policy))
           (get identity "failure_policy_hash")))
    (is (= (manifest/artifact-id identity)
           (snapshot-index/snapshot-identity-hash
            {"snapshot_label" "soranoha-snapshot-2026-07-07-01"
             "generated_at" "2026-07-07T00:00:00Z"
             "snapshot_index_identity_object" identity})))))

(deftest snapshot-index-identity-rejects-null-array-fields-test
  (let [identity (assoc (snapshot-index/snapshot-index-identity-object (identity-args))
                        "tokenizer_profile_hashes" nil)]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"array fields"
         (snapshot-index/validate-snapshot-index-identity-object! identity)))))

(deftest artifact-reference-from-manifest-path-test
  (let [manifest-value (manifest-fixture
                        {:artifact-id (files/example-hash "41")
                         :artifact-kind "failure"
                         :validation-status "failed"})
        manifest-file (temp-json-file "abc-snapshot-manifest" manifest-value)
        locator {"kind" "loose"
                 "path" "manifests/failure.json"}]
    (try
      (is (= {"artifact_id" (files/example-hash "41")
              "artifact_kind" "failure"
              "sidecar_role" nil
              "validation_status" "failed"
              "manifest_content_hash" (manifest/file-hash manifest-file)
              "content_hash" nil
              "locator" locator}
             (snapshot-index/artifact-reference-from-manifest-path manifest-file
                                                                   locator)))
      (finally
        (.delete manifest-file)))))

(deftest build-snapshot-index-from-request-set-and-manifests-test
  (let [request-set (request-set-resolver/resolve-request-set "smoke-basic-ja")
        passed-manifest (temp-json-file
                         "abc-snapshot-passed-manifest"
                         (manifest-fixture
                          {:artifact-id (files/example-hash "81")
                           :artifact-kind "plaintext"
                           :validation-status "passed"
                           :content-hash (files/example-hash "82")}))
        warning-manifest (temp-json-file
                          "abc-snapshot-warning-manifest"
                          (manifest-fixture
                           {:artifact-id (files/example-hash "71")
                            :artifact-kind "tei"
                            :validation-status "warning"
                            :content-hash (files/example-hash "72")}))
        failed-manifest (temp-json-file
                         "abc-snapshot-failed-manifest"
                         (manifest-fixture
                          {:artifact-id (files/example-hash "91")
                           :artifact-kind "failure"
                           :validation-status "failed"}))
        failure-policy {"allow_nonzero_failures" true
                        "max_failure_rate" nil
                        "per_diagnostic_tolerances" {}}
        layout-policy {"loose_artifact_kinds" ["tei" "plaintext"]
                       "batched_artifact_kinds" ["analysis" "tokenized"]
                       "batch_target_work_count" 250
                       "archive_format" "tar.zst"}]
    (try
      (let [snapshot (snapshot-index/build-snapshot-index
                      {:snapshot-label "soranoha-snapshot-2026-07-07-01"
                       :request-set-label "smoke-basic-ja"
                       :request-set request-set
                       :generated-at "2026-07-07T00:00:00Z"
                       :manifest-index-hash (files/example-hash "44")
                       :failure-policy failure-policy
                       :layout-policy layout-policy
                       :schema-hashes [(files/example-hash "09")
                                       (manifest/schema-hash
                                        "schemas/snapshot-index.schema.json")]
                       :parser-evidence-hashes []
                       :manifest-references [{:manifest-path warning-manifest
                                              :locator {"kind" "loose"
                                                        "path" "artifacts/tei/0001.xml"}}
                                             {:manifest-path passed-manifest
                                              :locator {"kind" "loose"
                                                        "path" "artifacts/plaintext/0001.txt"}}
                                             {:manifest-path failed-manifest
                                              :locator {"kind" "loose"
                                                        "path" "failures/0001.json"}}]})]
        (is (nil? (schema/validation-errors
                   (files/read-json "schemas/snapshot-index.schema.json")
                   snapshot)))
        (is (true? (snapshot-index/validate-snapshot-index! snapshot)))
        (is (= (manifest/schema-hash "schemas/snapshot-index.schema.json")
               (get snapshot "schema_hash")))
        (is (= (get request-set "request_set_id")
               (get-in snapshot ["snapshot_index_identity_object"
                                 "request_set_id"])))
        (is (= (get-in request-set ["request_set_identity_object"
                                    "corpus_snapshot_hash"])
               (get-in snapshot ["snapshot_index_identity_object"
                                 "source_snapshot_hash"])))
        (is (= (get-in request-set ["request_set_identity_object"
                                    "analysis_recipe_hashes"])
               (get-in snapshot ["snapshot_index_identity_object"
                                 "analysis_recipe_hashes"])))
        (is (= {"total_artifacts" 3
                "success_count" 2
                "failure_count" 1
                "failure_rate" (/ 1.0 3.0)}
               (get snapshot "summary")))
        (is (= [(files/example-hash "71")
                (files/example-hash "81")
                (files/example-hash "91")]
               (mapv #(get % "artifact_id")
                     (get snapshot "artifact_references"))))
        (is (nil? (get-in snapshot ["artifact_references" 2 "content_hash"]))))
      (finally
        (doseq [file [passed-manifest warning-manifest failed-manifest]]
          (.delete file))))))

(deftest checked-in-snapshot-index-fixture-validates-identity-test
  (let [fixture (files/read-json "examples/v0/snapshot/snapshot-index.json")]
    (testing "the public fixture is internally hash-consistent"
      (is (true? (snapshot-index/validate-snapshot-index! fixture))))))
