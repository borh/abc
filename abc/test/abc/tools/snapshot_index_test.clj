(ns abc.tools.snapshot-index-test
  (:require [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
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

(deftest checked-in-snapshot-index-fixture-validates-identity-test
  (let [fixture (files/read-json "examples/v0/snapshot/snapshot-index.json")]
    (testing "the public fixture is internally hash-consistent"
      (is (true? (snapshot-index/validate-snapshot-index! fixture))))))
