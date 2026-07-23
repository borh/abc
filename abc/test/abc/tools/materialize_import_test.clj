(ns abc.tools.materialize-import-test
  (:require [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.materialize-import :as materialize]
            [abc.tools.validate-design-bundle :as validate]
            [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private mapping-hash
  "sha256:7249cd727ef2da90dcd591e6009bead9235fe1c140697ee6bd70aacd9e85ee40")

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

(defn- materialize-fixture! [output-dir]
  (materialize/materialize-import!
   {:input-dir "examples/ab-validator-output"
    :output-dir output-dir
    :generated-at materialize/default-generated-at}))

(deftest v0-identity-json-test
  (is (= "{\"a\":null,\"b\":\"x\",\"c\":\"quote\\\"slash\\\\\"}"
         (manifest/jcs-json {"b" "x"
                             "a" nil
                             "c" "quote\"slash\\"}))))

(deftest schema-hash-test
  (is (= "sha256:8813e7142aa81afebb7de89f2c760afd0068cd72c1357ce0a255ea88f7650bc3"
         (manifest/schema-hash "schemas/manifest.schema.json")))
  (is (not= (str "sha256:" (files/sha256-file "schemas/manifest.schema.json"))
            (manifest/schema-hash "schemas/manifest.schema.json"))))

(deftest diagnostic-schema-hash-requires-the-exact-current-contract-test
  (let [mismatch "sha256:9999999999999999999999999999999999999999999999999999999999999999"
        errors (validate/schema-hash-errors
                (temp-manifest-inputs {"diagnostic_schema_hash" mismatch}))]
    (is (= 1 (count errors)))
    (is (re-find (re-pattern (str "diagnostic_schema_hash " mismatch
                                  " does not match ABC diagnostic schema hash"))
                 (first errors)))))

(deftest artifact-id-test
  (let [identity-object {"b" "sha256:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                         "a" nil}
        artifact-id (manifest/artifact-id identity-object)]
    (is (re-matches files/hash-pattern artifact-id))
    (is (not= (get identity-object "b") artifact-id))))

(deftest deterministic-json-writer-test
  (fs/with-temp-dir [dir {:prefix "abc-test-"}]
    (let [file-a (fs/file dir "a.json")
          file-b (fs/file dir "b.json")
          value-a {"z" [{"b" "2" "a" "1"}]
                   "a" {"d" "4" "c" "3"}}
          value-b {"a" {"c" "3" "d" "4"}
                   "z" [{"a" "1" "b" "2"}]}]
      (manifest/write-json-file! file-a value-a)
      (manifest/write-json-file! file-b value-b)
      (is (= (files/read-text file-a) (files/read-text file-b)))
      (is (= "{\n  \"a\":\n  {\n    \"c\": \"3\",\n    \"d\": \"4\"\n  },\n  \"z\": [\n    {\n      \"a\": \"1\",\n      \"b\": \"2\"\n    }]\n}\n"
             (files/read-text file-a))))))

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

(deftest materialized-content-hashes-match-imported-files-test
  (fs/with-temp-dir [output {:prefix "abc-test-"}]
    (let [generated (materialize-fixture! output)
          parser (files/read-json (:parser-ir generated))
          warnings (files/read-json (:warnings generated))]
      (is (= (str "sha256:" (files/sha256-file
                             "examples/ab-validator-output/parser-ir.json"))
             (get-in parser ["content" "content_hash"])))
      (is (= (str "sha256:" (files/sha256-file
                             "examples/ab-validator-output/warnings.jsonl"))
             (get-in warnings ["content" "content_hash"]))))))

(deftest generated-artifact-ids-differ-from-content-hashes-test
  (fs/with-temp-dir [output {:prefix "abc-test-"}]
    (doseq [manifest-path (vals (materialize-fixture! output))
            :let [value (files/read-json manifest-path)]]
      (is (not= (get value "artifact_id")
                (get-in value ["content" "content_hash"]))))))

(deftest materialized-manifest-schema-hash-matches-bundled-jcs-test
  (fs/with-temp-dir [output {:prefix "abc-test-"}]
    (let [expected (manifest/schema-hash "schemas/manifest.schema.json")]
      (doseq [manifest-path (vals (materialize-fixture! output))]
        (is (= expected
               (get-in (files/read-json manifest-path)
                       ["manifest_identity_object" "manifest_schema_hash"])))))))

(deftest materialized-parser-and-warning-artifact-ids-are-distinct-test
  (fs/with-temp-dir [output {:prefix "abc-test-"}]
    (let [generated (materialize-fixture! output)]
      (is (not= (get (files/read-json (:parser-ir generated)) "artifact_id")
                (get (files/read-json (:warnings generated)) "artifact_id"))))))

(deftest mapping-divergence-sidecar-selection-test
  (fs/with-temp-dir [input {:prefix "abc-test-"}]
    (doseq [name ["parser-ir.json" "warnings.jsonl"
                  "divergence.json" "divergence.jsonl"]]
      (manifest/write-json-file! (fs/file input name) {}))
    (let [parser-manifest (materialize/parser-ir-manifest
                           input (temp-manifest-inputs {})
                           materialize/default-generated-at)
          canonical (->> (get parser-manifest "sidecars")
                         (filter (fn [item]
                                   (= "mapping-divergence" (get item "role"))))
                         first)]
      (is (= "divergence.json" (get canonical "path_hint")))))
  (fs/with-temp-dir [input {:prefix "abc-test-"}]
    (doseq [name ["parser-ir.json" "warnings.jsonl" "divergence.jsonl"]]
      (manifest/write-json-file! (fs/file input name) {}))
    (let [parser-manifest (materialize/parser-ir-manifest
                           input (temp-manifest-inputs {})
                           materialize/default-generated-at)
          legacy (->> (get parser-manifest "sidecars")
                      (filter (fn [item]
                                (= "mapping-divergence" (get item "role"))))
                      first)]
      (is (= "divergence.jsonl" (get legacy "path_hint"))))))

(deftest materialized-output-is-deterministic-test
  (fs/with-temp-dir [root {:prefix "abc-test-"}]
    (let [output-a (fs/file root "a")
          output-b (fs/file root "b")]
      (materialize-fixture! output-a)
      (materialize-fixture! output-b)
      (is (= (files/read-text (fs/file output-a "parser-ir.manifest.json"))
             (files/read-text (fs/file output-b "parser-ir.manifest.json"))))
      (is (= (files/read-text (fs/file output-a "warnings.manifest.json"))
             (files/read-text (fs/file output-b "warnings.manifest.json")))))))
