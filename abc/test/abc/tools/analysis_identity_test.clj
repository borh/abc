(ns abc.tools.analysis-identity-test
  (:require [abc.tools.analysis-identity :as analysis-identity]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [clojure.test :refer [deftest is]]))

(def subject-a
  {"source_id" "aozora:1"
   "work_id" "aozora:1-a"
   "work_content_hash" (files/example-hash "11")
   "metadata_record_hash" nil})

(def subject-b
  {"source_id" "aozora:1"
   "work_id" "aozora:1-b"
   "work_content_hash" (files/example-hash "12")
   "metadata_record_hash" (files/example-hash "13")})

(def subject-c
  {"source_id" "aozora:1"
   "work_id" "aozora:1-c"
   "work_content_hash" (files/example-hash "14")
   "metadata_record_hash" nil})

(def subject-d
  {"source_id" "aozora:1"
   "work_id" "aozora:1-c"
   "work_content_hash" (files/example-hash "14")})

(def subject-e
  {"source_id" "aozora:1"
   "work_id" "aozora:1-e"
   "work_content_hash" (files/example-hash "15")
   "metadata_record_hash" (files/example-hash "16")})

(deftest canonical-subjects-sort-and-coalesce-test
  (is (= [subject-a subject-b]
         (analysis-identity/canonical-subjects
          [subject-b subject-a subject-a]))))

(deftest canonical-subjects-preserves-nil-and-normalizes-missing-test
  (is (= [subject-c]
         (analysis-identity/canonical-subjects [subject-d subject-c])))
  (is (= (analysis-identity/canonical-subjects [subject-c])
         (analysis-identity/canonical-subjects [subject-d]))))

(deftest subject-sort-key-omits-work-id-test
  (is (= [(get subject-e "source_id")
          (files/example-hash "15")
          (files/example-hash "16")]
         ((var-get #'analysis-identity/subject-sort-key) subject-e))))

(deftest request-set-id-excludes-derived-and-label-fields-test
  (let [identity-object (analysis-identity/request-set-identity-object
                         {:schema-hash (files/example-hash "01")
                          :corpus-snapshot-hash (files/example-hash "02")
                          :subjects [subject-b subject-a subject-a]
                          :input-views [{"input_view_kind" "parser-ir-plaintext-body-v1"
                                         "policy_hash" (files/example-hash "03")}]
                          :tokenizer-profile-hashes []
                          :analysis-recipe-hashes [(files/example-hash "04")]
                          :missing-policy "build-missing-only"
                          :pack-policy-hash (files/example-hash "05")})
        request-set-a {"request_set_identity_object" identity-object
                       "request_set_id" (files/example-hash "98")
                       "resolved_labels" {"analysis_recipes" []}
                       "batch_policy" "100-works-or-512mb"}
        request-set-b (assoc request-set-a
                             "request_set_id" (files/example-hash "99")
                             "batch_policy" "1-work")]
    (is (= (analysis-identity/request-set-id request-set-a)
           (analysis-identity/request-set-id request-set-b)))
    (is (= (manifest/artifact-id identity-object)
           (analysis-identity/request-set-id request-set-a)))
    (is (nil? (get-in identity-object ["subjects" 0 "metadata_record_hash"])))))

(deftest request-set-identity-rejects-unsupported-input-view-kind-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"Invalid input view kind"
       (analysis-identity/request-set-identity-object
        {:schema-hash (files/example-hash "01")
         :corpus-snapshot-hash (files/example-hash "02")
         :subjects [subject-a]
         :input-views [{"input_view_kind" "tei-body-text-v1"
                        "policy_hash" (files/example-hash "03")}]
         :tokenizer-profile-hashes []
         :analysis-recipe-hashes []
         :missing-policy "build-missing-only"
         :pack-policy-hash (files/example-hash "05")}))))

(deftest resolved-recipe-label-is-non-identity-audit-data-test
  (is (= {"recipe_id" "literary-basic-ja-v1"
          "analysis_recipe_hash" (files/example-hash "21")
          "registry_entry_hash" (files/example-hash "22")
          "resolved_at" "2026-07-07T00:00:00Z"}
         (analysis-identity/resolved-recipe-label
          {:recipe-id "literary-basic-ja-v1"
           :analysis-recipe-hash (files/example-hash "21")
           :registry-entry-hash (files/example-hash "22")
           :resolved-at "2026-07-07T00:00:00Z"}))))

(deftest assert-input-normalization-agreement-passes-on-match-test
  (is (= "sha256:530c59689dd909c171790036cddc7916f8685897b6342bfa794d4611816d3813"
         (analysis-identity/assert-input-normalization-agreement!
          {:declared "sha256:530c59689dd909c171790036cddc7916f8685897b6342bfa794d4611816d3813"
           :applied "sha256:530c59689dd909c171790036cddc7916f8685897b6342bfa794d4611816d3813"}))))

(deftest assert-input-normalization-agreement-throws-on-mismatch-test
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo
       #"input-normalization policy mismatch"
       (analysis-identity/assert-input-normalization-agreement!
        {:declared "sha256:530c59689dd909c171790036cddc7916f8685897b6342bfa794d4611816d3813"
         :applied (files/example-hash "77")}))))
