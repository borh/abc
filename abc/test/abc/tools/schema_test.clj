(ns abc.tools.schema-test
  (:require [abc.tools.files :as files]
            [abc.tools.schema :as schema]
            [clojure.test :refer [deftest is testing]]))

(def ^:private cross-project-schema-versions
  {"schemas/parser-ir.schema.json" "0.5.0"
   "schemas/aat-parser-ir-divergence.schema.json" "0.3.0"
   "schemas/aat-parser-ir-mapping.schema.json" "0.2.3"
   "schemas/manifest.schema.json" "0.4.0"
   "schemas/parser-ir-publication-preservation.schema.json" "0.2.0"
   "schemas/source-region-coverage.schema.json" "0.2.1"})

(deftest cross-project-schemas-carry-explicit-versions-test
  (doseq [[path expected-version] cross-project-schema-versions]
    (is (= expected-version
           (get (files/read-json path) "version"))
        (str path " must expose the cross-repo schema contract version"))))

(deftest schema-hash-test
  (testing "hashes are over parsed canonical JSON values, not source bytes"
    (let [file-a (java.io.File/createTempFile "abc-schema-a" ".json")
          file-b (java.io.File/createTempFile "abc-schema-b" ".json")
          file-c (java.io.File/createTempFile "abc-schema-c" ".json")]
      (try
        (spit file-a "{\"type\":\"object\",\"properties\":{\"b\":{\"type\":\"string\"},\"a\":{\"type\":\"null\"}}}\n")
        (spit file-b "{\n  \"properties\": {\n    \"a\": {\"type\": \"null\"},\n    \"b\": {\"type\": \"string\"}\n  },\n  \"type\": \"object\"\n}\n")
        (spit file-c "{\"type\":\"array\"}\n")
        (is (= (schema/schema-hash file-a)
               (schema/schema-hash file-b)))
        (is (not= (schema/schema-hash file-a)
                  (schema/schema-hash file-c)))
        (finally
          (doseq [file [file-a file-b file-c]]
            (.delete file)))))))

(deftest schema-validation-test
  (let [schema-file (java.io.File/createTempFile "abc-schema" ".json")
        value-file (java.io.File/createTempFile "abc-value" ".json")]
    (try
      (spit schema-file "{\"type\":\"object\",\"required\":[\"a\"],\"properties\":{\"a\":{\"type\":\"string\"}}}")
      (spit value-file "{\"a\":\"ok\"}")
      (is (nil? (schema/validate-json! (schema/read-schema schema-file) value-file)))
      (spit value-file "{\"a\":1}")
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"JSON Schema validation failed"
                            (schema/validate-json! (schema/read-schema schema-file) value-file)))
      (finally
        (doseq [file [schema-file value-file]]
          (.delete file))))))
