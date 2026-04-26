(ns abc.tools.manifest-index-test
  (:require [abc.tools.manifest-index :as manifest-index]
            [clojure.test :refer [deftest is testing]]))

(defn manifest
  [artifact-id content-hash validation-status]
  {"artifact_id" artifact-id
   "artifact_kind" "parser-ir"
   "validation_status" validation-status
   "content" {"content_hash" content-hash
              "media_type" "application/json"}})

(deftest index-entries-test
  (is (= [{"manifest_path" "a.manifest.json"
           "artifact_id" "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
           "artifact_kind" "parser-ir"
           "validation_status" "passed"
           "content_hash" "sha256:1111111111111111111111111111111111111111111111111111111111111111"
           "media_type" "application/json"}
          {"manifest_path" "b.manifest.json"
           "artifact_id" "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
           "artifact_kind" "parser-ir"
           "validation_status" "warning"
           "content_hash" "sha256:2222222222222222222222222222222222222222222222222222222222222222"
           "media_type" "application/json"}]
         (manifest-index/index-entries
          {"b.manifest.json" (manifest "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                                       "sha256:2222222222222222222222222222222222222222222222222222222222222222"
                                       "warning")
           "a.manifest.json" (manifest "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                                       "sha256:1111111111111111111111111111111111111111111111111111111111111111"
                                       "passed")}))))

(deftest reproducibility-conflicts-test
  (testing "same artifact_id and content_hash is not a conflict"
    (is (empty?
         (manifest-index/reproducibility-conflicts
          [(manifest-index/manifest->index-entry
            "a.manifest.json"
            (manifest "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                      "sha256:1111111111111111111111111111111111111111111111111111111111111111"
                      "passed"))
           (manifest-index/manifest->index-entry
            "b.manifest.json"
            (manifest "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                      "sha256:1111111111111111111111111111111111111111111111111111111111111111"
                      "warning"))]))))
  (testing "same artifact_id and different content_hash is a conflict"
    (is (= [{"artifact_id" "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
             "content_hashes" ["sha256:1111111111111111111111111111111111111111111111111111111111111111"
                               "sha256:2222222222222222222222222222222222222222222222222222222222222222"]
             "manifest_paths" ["a.manifest.json" "b.manifest.json"]}]
           (manifest-index/reproducibility-conflicts
            [(manifest-index/manifest->index-entry
              "a.manifest.json"
              (manifest "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                        "sha256:1111111111111111111111111111111111111111111111111111111111111111"
                        "passed"))
             (manifest-index/manifest->index-entry
              "b.manifest.json"
              (manifest "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                        "sha256:2222222222222222222222222222222222222222222222222222222222222222"
                        "warning"))])))))
