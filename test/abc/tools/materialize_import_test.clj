(ns abc.tools.materialize-import-test
  (:require [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.materialize-import :as materialize]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(deftest v0-identity-json-test
  (is (= "{\"a\":null,\"b\":\"x\",\"c\":\"quote\\\"slash\\\\\"}"
         (manifest/v0-identity-json {"b" "x"
                                     "a" nil
                                     "c" "quote\"slash\\"}))))

(deftest schema-hash-test
  (is (= "sha256:4fa4f16a2c28481b04bed11b298ec2e2ebe7cf35263fe26888dbfff4df59b9a7"
         (manifest/schema-hash "schemas/manifest.schema.json")))
  (is (not= (str "sha256:" (files/sha256-file "schemas/manifest.schema.json"))
            (manifest/schema-hash "schemas/manifest.schema.json"))))

(deftest artifact-id-test
  (let [identity-object {"b" "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                         "a" nil}
        artifact-id (manifest/artifact-id identity-object)]
    (is (re-matches files/hash-pattern artifact-id))
    (is (not= (get identity-object "b") artifact-id))))

(deftest deterministic-json-writer-test
  (let [dir (Files/createTempDirectory "abc-json-writer" (make-array FileAttribute 0))
        file-a (.toFile (.resolve dir "a.json"))
        file-b (.toFile (.resolve dir "b.json"))
        value-a {"z" [{"b" "2" "a" "1"}]
                 "a" {"d" "4" "c" "3"}}
        value-b {"a" {"c" "3" "d" "4"}
                 "z" [{"a" "1" "b" "2"}]}]
    (try
      (manifest/write-json-file! file-a value-a)
      (manifest/write-json-file! file-b value-b)
      (is (= (slurp file-a) (slurp file-b)))
      (is (= "{\n  \"a\": \n  {\n    \"c\": \"3\",\n    \"d\": \"4\"\n  },\n  \"z\": [\n    {\n      \"a\": \"1\",\n      \"b\": \"2\"\n    }]\n}\n"
             (slurp file-a)))
      (finally
        (doseq [file (reverse (file-seq (.toFile dir)))]
          (.delete file))))))

(deftest materialize-import-test
  (let [out-dir (java.nio.file.Files/createTempDirectory "abc-materialize-import" (make-array java.nio.file.attribute.FileAttribute 0))
        out-file (.toFile out-dir)]
    (try
      (let [result (materialize/materialize-import!
                    {:input-dir (io/file "examples/ab-validator-output")
                     :output-dir out-file
                     :generated-at "2026-04-26T00:00:00Z"})
            parser-manifest-file (io/file out-file "parser-ir.manifest.json")
            warnings-manifest-file (io/file out-file "warnings.manifest.json")
            parser-manifest (files/read-json parser-manifest-file)
            warnings-manifest (files/read-json warnings-manifest-file)]
        (is (= {:parser-ir parser-manifest-file
                :warnings warnings-manifest-file}
               result))
        (is (= "parser-ir" (get parser-manifest "artifact_kind")))
        (is (= "warnings" (get warnings-manifest "artifact_kind")))
        (is (= "warning" (get parser-manifest "validation_status")))
        (is (= "warning" (get warnings-manifest "validation_status")))
        (is (= (manifest/schema-hash "schemas/manifest.schema.json")
               (get-in parser-manifest ["manifest_identity_object" "manifest_schema_hash"])))
        (is (= (manifest/schema-hash "schemas/manifest.schema.json")
               (get-in warnings-manifest ["manifest_identity_object" "manifest_schema_hash"])))
        (is (= (get-in parser-manifest ["manifest_identity_object" "parser_ir_schema_hash"])
               (get-in warnings-manifest ["manifest_identity_object" "parser_ir_schema_hash"])))
        (is (= (str "sha256:" (files/sha256-file "examples/ab-validator-output/parser-ir.json"))
               (get-in parser-manifest ["content" "content_hash"])))
        (is (= (str "sha256:" (files/sha256-file "examples/ab-validator-output/warnings.jsonl"))
               (get-in warnings-manifest ["content" "content_hash"])))
        (is (not= (get parser-manifest "artifact_id")
                  (get-in parser-manifest ["content" "content_hash"])))
        (is (not= (get warnings-manifest "artifact_id")
                  (get-in warnings-manifest ["content" "content_hash"])))
        (is (not= (get parser-manifest "artifact_id")
                  (get warnings-manifest "artifact_id"))))
      (finally
        (doseq [file (reverse (file-seq out-file))]
          (.delete file))))))

(deftest materialized-output-is-deterministic-test
  (let [out-dir (Files/createTempDirectory "abc-materialize-fixture" (make-array FileAttribute 0))
        out-file (.toFile out-dir)
        out-dir-2 (Files/createTempDirectory "abc-materialize-fixture" (make-array FileAttribute 0))
        out-file-2 (.toFile out-dir-2)]
    (try
      (materialize/materialize-import!
       {:input-dir (io/file "examples/ab-validator-output")
        :output-dir out-file
        :generated-at "2026-04-26T00:00:00Z"})
      (materialize/materialize-import!
       {:input-dir (io/file "examples/ab-validator-output")
        :output-dir out-file-2
        :generated-at "2026-04-26T00:00:00Z"})
      (is (= (slurp (io/file out-file "parser-ir.manifest.json"))
             (slurp (io/file out-file-2 "parser-ir.manifest.json"))))
      (is (= (slurp (io/file out-file "warnings.manifest.json"))
             (slurp (io/file out-file-2 "warnings.manifest.json"))))
      (finally
        (doseq [file (reverse (file-seq out-file))]
          (.delete file))
        (doseq [file (reverse (file-seq out-file-2))]
          (.delete file))))))
