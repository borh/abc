(ns abc.tools.materialize-import-test
  (:require [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.materialize-import :as materialize]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private mapping-hash
  "sha256:4c0d3eb53942b4e1e14a6efc614bab99e391e90d85b817e090b42d02c05ba22e")

(defn- temp-manifest-inputs [overrides]
  (merge
   {"producer" "ab-validator"
    "producer_version" "0.0.0"
    "work_id" "fixture"
    "corpus_snapshot_hash" "sha256:f00000000000000000000000000000000000000000000000000000000000000f"
    "work_content_hash" "sha256:f100000000000000000000000000000000000000000000000000000000000001"
    "parser_build_hash" "sha256:f200000000000000000000000000000000000000000000000000000000000002"
    "parser_config_hash" "sha256:f300000000000000000000000000000000000000000000000000000000000003"
    "mapping_hash" mapping-hash
    "parser_ir_schema_hash" "sha256:41c43f0c88a66c31ae4fbf9b9eeb04de92756082acaaaa1c2e21f1a5bf74a396"
    "diagnostic_schema_hash" "sha256:e21ef2abdbf64b6fc920b4ef9a3df0e426b7bcc1cad0a6bbdd654f41e8ff302d"
    "warning_sidecar_hash" "sha256:f500000000000000000000000000000000000000000000000000000000000005"
    "run_summary_hash" "sha256:f600000000000000000000000000000000000000000000000000000000000006"
    "comparison_report_hash" "sha256:f700000000000000000000000000000000000000000000000000000000000007"}
   overrides))

(deftest v0-identity-json-test
  (is (= "{\"a\":null,\"b\":\"x\",\"c\":\"quote\\\"slash\\\\\"}"
         (manifest/v0-identity-json {"b" "x"
                                     "a" nil
                                     "c" "quote\"slash\\"}))))

(deftest schema-hash-test
  (is (= "sha256:8813e7142aa81afebb7de89f2c760afd0068cd72c1357ce0a255ea88f7650bc3"
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
      (is (= "{\n  \"a\":\n  {\n    \"c\": \"3\",\n    \"d\": \"4\"\n  },\n  \"z\": [\n    {\n      \"a\": \"1\",\n      \"b\": \"2\"\n    }]\n}\n"
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
        (is (= mapping-hash
               (get-in parser-manifest ["manifest_identity_object" "aat_parser_ir_mapping_hash"])))
        (is (= mapping-hash
               (get-in warnings-manifest ["manifest_identity_object" "aat_parser_ir_mapping_hash"])))
        (is (some #{mapping-hash} (get-in parser-manifest ["provenance" "used"])))
        (is (= {"role" "mapping-divergence"
                "hash" (str "sha256:" (files/sha256-file "examples/ab-validator-output/divergence.json"))
                "media_type" "application/json"
                "path_hint" "divergence.json"}
               (first (filter #(= "mapping-divergence" (get % "role"))
                              (get parser-manifest "sidecars")))))
        (is (= {"role" "source-region-coverage"
                "hash" (str "sha256:" (files/sha256-file "examples/ab-validator-output/source-region-coverage.json"))
                "media_type" "application/json"
                "path_hint" "source-region-coverage.json"}
               (first (filter #(= "source-region-coverage" (get % "role"))
                              (get parser-manifest "sidecars")))))
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

(deftest materialize-derives-status-from-run-summary-test
  (let [input-dir (Files/createTempDirectory "abc-materialize-status" (make-array FileAttribute 0))
        out-dir (Files/createTempDirectory "abc-materialize-status-out" (make-array FileAttribute 0))
        input-file (.toFile input-dir)
        out-file (.toFile out-dir)]
    (try
      ;; Write minimal supporting files into a temp input dir
      (spit (io/file input-file "manifest-inputs.json")
            (charred.api/write-json-str
             (temp-manifest-inputs {"work_id" "fixture-status"})))
      (spit (io/file input-file "parser-ir.json") "{}")
      (spit (io/file input-file "warnings.jsonl") "")
      (spit (io/file input-file "run-summary.jsonl")
            (str "{\"event\":\"run-start\",\"run_id\":\"r1\"}\n"
                 "{\"event\":\"run-complete\",\"run_id\":\"r1\",\"status\":\"passed\"}\n"))
      (materialize/materialize-import!
       {:input-dir input-file
        :output-dir out-file
        :generated-at "2026-04-26T00:00:00Z"})
      (let [parser-manifest (files/read-json (io/file out-file "parser-ir.manifest.json"))
            warnings-manifest (files/read-json (io/file out-file "warnings.manifest.json"))]
        (is (= "passed" (get parser-manifest "validation_status")))
        (is (= "passed" (get warnings-manifest "validation_status"))))
      (finally
        (doseq [f (reverse (file-seq out-file))] (.delete f))
        (doseq [f (reverse (file-seq input-file))] (.delete f))))))

(deftest materialize-falls-back-to-warning-when-run-summary-missing-test
  (let [input-dir (Files/createTempDirectory "abc-materialize-fallback" (make-array FileAttribute 0))
        out-dir (Files/createTempDirectory "abc-materialize-fallback-out" (make-array FileAttribute 0))
        input-file (.toFile input-dir)
        out-file (.toFile out-dir)]
    (try
      (spit (io/file input-file "manifest-inputs.json")
            (charred.api/write-json-str
             (temp-manifest-inputs {"work_id" "fixture-fallback"})))
      (spit (io/file input-file "parser-ir.json") "{}")
      (spit (io/file input-file "warnings.jsonl") "")
      ;; No run-summary.jsonl
      (materialize/materialize-import!
       {:input-dir input-file
        :output-dir out-file
        :generated-at "2026-04-26T00:00:00Z"})
      (let [parser-manifest (files/read-json (io/file out-file "parser-ir.manifest.json"))]
        (is (= "warning" (get parser-manifest "validation_status"))))
      (finally
        (doseq [f (reverse (file-seq out-file))] (.delete f))
        (doseq [f (reverse (file-seq input-file))] (.delete f))))))

(deftest parser-manifest-includes-legacy-divergence-sidecar-when-jsonl-present-test
  (let [input-dir (Files/createTempDirectory "abc-materialize-divergence" (make-array FileAttribute 0))
        out-dir (Files/createTempDirectory "abc-materialize-divergence-out" (make-array FileAttribute 0))
        input-file (.toFile input-dir)
        out-file (.toFile out-dir)]
    (try
      (spit (io/file input-file "manifest-inputs.json")
            (charred.api/write-json-str
             (temp-manifest-inputs {"work_id" "fixture-divergence"})))
      (spit (io/file input-file "parser-ir.json") "{}")
      (spit (io/file input-file "warnings.jsonl") "")
      (spit (io/file input-file "divergence.jsonl") "{\"mapping_rule_id\":\"r1\"}\n")
      (materialize/materialize-import!
       {:input-dir input-file
        :output-dir out-file
        :generated-at "2026-04-26T00:00:00Z"})
      (let [parser-manifest (files/read-json (io/file out-file "parser-ir.manifest.json"))
            sidecar (first (filter #(= "mapping-divergence" (get % "role"))
                                   (get parser-manifest "sidecars")))]
        (is (= {"role" "mapping-divergence"
                "hash" (str "sha256:" (files/sha256-file (io/file input-file "divergence.jsonl")))
                "media_type" "application/jsonl"
                "path_hint" "divergence.jsonl"}
               sidecar)))
      (finally
        (doseq [f (reverse (file-seq out-file))] (.delete f))
        (doseq [f (reverse (file-seq input-file))] (.delete f))))))

(deftest artifact-id-independent-of-generated-at-test
  (let [out-dir (Files/createTempDirectory "abc-materialize-gen" (make-array FileAttribute 0))
        out-file (.toFile out-dir)
        out-dir-2 (Files/createTempDirectory "abc-materialize-gen" (make-array FileAttribute 0))
        out-file-2 (.toFile out-dir-2)]
    (try
      (materialize/materialize-import!
       {:input-dir (io/file "examples/ab-validator-output")
        :output-dir out-file
        :generated-at "2026-04-26T00:00:00Z"})
      (materialize/materialize-import!
       {:input-dir (io/file "examples/ab-validator-output")
        :output-dir out-file-2
        :generated-at "2026-04-27T12:34:56Z"})
      (is (= (get (files/read-json (io/file out-file "parser-ir.manifest.json")) "artifact_id")
             (get (files/read-json (io/file out-file-2 "parser-ir.manifest.json")) "artifact_id")))
      (is (= (get (files/read-json (io/file out-file "warnings.manifest.json")) "artifact_id")
             (get (files/read-json (io/file out-file-2 "warnings.manifest.json")) "artifact_id")))
      (finally
        (doseq [f (reverse (file-seq out-file))] (.delete f))
        (doseq [f (reverse (file-seq out-file-2))] (.delete f))))))

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
