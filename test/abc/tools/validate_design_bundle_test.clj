(ns abc.tools.validate-design-bundle-test
  (:require [abc.tools.files :as files]
            [abc.tools.validate-design-bundle :as validate]
            [clojure.test :refer [deftest is testing]]))

(deftest sha256-file-test
  (let [file (java.io.File/createTempFile "abc-sha256" ".txt")]
    (try
      (spit file "abc")
      (is (= "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
             (files/sha256-file file)))
      (finally
        (.delete file)))))

(deftest read-json-lines-test
  (let [file (java.io.File/createTempFile "abc-jsonl" ".jsonl")]
    (try
      (spit file "{\"a\":1}\n\n{\"b\":2}\n")
      (is (= [{"a" 1} {"b" 2}]
             (files/read-json-lines file)))
      (finally
        (.delete file)))))

(deftest validate-run-summary-test
  (testing "accepts start, work result, complete"
    (is (empty?
         (validate/run-summary-errors
          [{"event" "run-start" "run_id" "r1"}
           {"event" "work-result" "run_id" "r1"}
           {"event" "run-complete" "run_id" "r1"}]))))
  (testing "accepts start and complete without work results"
    (is (empty?
         (validate/run-summary-errors
          [{"event" "run-start" "run_id" "r1"}
           {"event" "run-complete" "run_id" "r1"}]))))
  (testing "rejects mismatched run ids"
    (is (= ["ab-validator run summary events must all use the same run_id"]
           (validate/run-summary-errors
            [{"event" "run-start" "run_id" "r1"}
             {"event" "work-result" "run_id" "r2"}
             {"event" "run-complete" "run_id" "r1"}]))))
  (testing "rejects extra lifecycle events"
    (is (= ["ab-validator run summary must contain exactly one run-start event"
            "ab-validator run summary must contain exactly one run-complete event"]
           (validate/run-summary-errors
            [{"event" "run-start" "run_id" "r1"}
             {"event" "run-start" "run_id" "r1"}
             {"event" "run-complete" "run_id" "r1"}
             {"event" "run-complete" "run_id" "r1"}]))))
  (testing "rejects missing run id"
    (is (= ["run summary event is missing run_id: {\"event\" \"work-result\"}"]
           (validate/run-summary-errors
            [{"event" "run-start" "run_id" "r1"}
             {"event" "work-result"}
             {"event" "run-complete" "run_id" "r1"}])))))

(deftest manifest-input-errors-test
  (testing "accepts complete manifest inputs"
    (is (empty?
         (validate/manifest-input-errors
          {"producer" "ab-validator"
           "producer_version" "0.0.0"
           "work_id" "fixture"
           "corpus_snapshot_hash" (files/example-hash "00")
           "work_content_hash" (files/example-hash "01")
           "parser_build_hash" (files/example-hash "02")
           "parser_config_hash" (files/example-hash "03")
           "parser_ir_schema_hash" (files/example-hash "04")
           "diagnostic_schema_hash" (files/example-hash "08")
           "warning_sidecar_hash" (files/example-hash "05")
           "run_summary_hash" (files/example-hash "06")
           "comparison_report_hash" (files/example-hash "07")}))))
  (testing "reports missing keys"
    (let [actual (validate/manifest-input-errors {"producer" "ab-validator"})]
      (is (= 1 (count actual)))
      (is (re-find #"corpus_snapshot_hash" (first actual)))))
  (testing "reports invalid hash values"
    (is (= ["ab-validator manifest input work_content_hash is not a sha256 hash: nope"]
           (validate/manifest-input-errors
            {"producer" "ab-validator"
             "producer_version" "0.0.0"
             "work_id" "fixture"
             "corpus_snapshot_hash" (files/example-hash "00")
             "work_content_hash" "nope"
             "parser_build_hash" (files/example-hash "02")
             "parser_config_hash" (files/example-hash "03")
             "parser_ir_schema_hash" (files/example-hash "04")
             "diagnostic_schema_hash" (files/example-hash "08")
             "warning_sidecar_hash" (files/example-hash "05")
             "run_summary_hash" (files/example-hash "06")
             "comparison_report_hash" (files/example-hash "07")})))))

(deftest comparison-report-errors-test
  (is (empty?
       (validate/comparison-report-errors
        {"report_schema" "abc.ab-validator-comparison.v0"
         "parser_candidates" [{"parser_id" "fixture"}]})))
  (is (= ["ab-validator comparison report has an unexpected report_schema"
          "ab-validator comparison report must list parser_candidates"]
         (validate/comparison-report-errors
          {"report_schema" "wrong"
           "parser_candidates" []}))))

(deftest schema-hash-errors-test
  (is (empty?
       (validate/schema-hash-errors
        {"parser_ir_schema_hash" "sha256:13e3127fe8eaa0649f83fd5c12e11923115810b454c6d3d22996b00e1218623f"
         "diagnostic_schema_hash" "sha256:e21ef2abdbf64b6fc920b4ef9a3df0e426b7bcc1cad0a6bbdd654f41e8ff302d"})))
  (is (= ["ab-validator parser_ir_schema_hash sha256:0000000000000000000000000000000000000000000000000000000000000004 does not match ABC parser IR schema hash sha256:13e3127fe8eaa0649f83fd5c12e11923115810b454c6d3d22996b00e1218623f"
          "ab-validator diagnostic_schema_hash sha256:0000000000000000000000000000000000000000000000000000000000000008 does not match ABC diagnostic schema hash sha256:e21ef2abdbf64b6fc920b4ef9a3df0e426b7bcc1cad0a6bbdd654f41e8ff302d"]
         (validate/schema-hash-errors
          {"parser_ir_schema_hash" (files/example-hash "04")
           "diagnostic_schema_hash" (files/example-hash "08")}))))

(deftest parser-ir-schema-hash-errors-test
  (is (empty?
       (validate/parser-ir-schema-hash-errors
        {"schema_hash" "sha256:13e3127fe8eaa0649f83fd5c12e11923115810b454c6d3d22996b00e1218623f"})))
  (is (= ["ab-validator parser IR schema_hash sha256:0000000000000000000000000000000000000000000000000000000000000004 does not match ABC parser IR schema hash sha256:13e3127fe8eaa0649f83fd5c12e11923115810b454c6d3d22996b00e1218623f"]
         (validate/parser-ir-schema-hash-errors
          {"schema_hash" (files/example-hash "04")}))))
