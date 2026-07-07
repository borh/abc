(ns abc.tools.schema-test
  (:require [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.schema :as schema]
            [clojure.test :refer [deftest is testing]]))

(def ^:private cross-project-schema-versions
  {"schemas/parser-ir.schema.json" "0.5.0"
   "schemas/aat-parser-ir-divergence.schema.json" "0.3.0"
   "schemas/aat-parser-ir-mapping.schema.json" "0.2.3"
   "schemas/analysis-recipe.schema.json" "0.1.0"
   "schemas/analysis-result.schema.json" "0.1.0"
   "schemas/manifest.schema.json" "0.4.1"
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

(deftest analysis-schema-fixtures-validate-test
  (let [recipe-schema (schema/read-schema "schemas/analysis-recipe.schema.json")
        result-schema (schema/read-schema "schemas/analysis-result.schema.json")
        recipe (files/read-json "data/analysis-recipes/literary-basic-ja-v1.json")
        result (files/read-json "examples/v0/example-work/analysis-result.json")]
    (is (= "https://w3id.org/abc/schemas/analysis-recipe.schema.json"
           (get recipe "schema_id")))
    (is (= "https://w3id.org/abc/schemas/analysis-result.schema.json"
           (get result "schema_id")))
    (is (= "cards/000000/files/example.txt"
           (get-in result ["subject" "logical_path"])))
    (is (= "0e9ea3e586"
           (get-in result ["subject" "git_ref"])))
    (is (= (manifest/schema-hash "schemas/analysis-result.schema.json")
           (get recipe "required_output_schema_hash")))
    (is (= (manifest/schema-hash "schemas/analysis-result.schema.json")
           (get result "schema_hash")))
    (is (nil? (schema/validation-errors recipe-schema recipe)))
    (is (nil? (schema/validation-errors result-schema result)))))
