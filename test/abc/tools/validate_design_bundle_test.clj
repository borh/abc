(ns abc.tools.validate-design-bundle-test
  (:require [abc.tools.validate-design-bundle :as validate]
            [clojure.test :refer [deftest is testing]]))

(deftest sha256-file-test
  (let [file (java.io.File/createTempFile "abc-sha256" ".txt")]
    (try
      (spit file "abc")
      (is (= "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
             (validate/sha256-file file)))
      (finally
        (.delete file)))))

(deftest read-json-lines-test
  (let [file (java.io.File/createTempFile "abc-jsonl" ".jsonl")]
    (try
      (spit file "{\"a\":1}\n\n{\"b\":2}\n")
      (is (= [{"a" 1} {"b" 2}]
             (validate/read-json-lines file)))
      (finally
        (.delete file)))))

(deftest validate-run-summary-test
  (testing "accepts start, work result, complete"
    (is (empty?
         (validate/run-summary-errors
          [{"event" "run-start" "run_id" "r1"}
           {"event" "work-result" "run_id" "r1"}
           {"event" "run-complete" "run_id" "r1"}]))))
  (testing "rejects missing work result"
    (is (= ["ab-validator run summary must include a work-result event"]
           (validate/run-summary-errors
            [{"event" "run-start" "run_id" "r1"}
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
           "work_content_hash" (validate/example-hash "01")
           "parser_build_hash" (validate/example-hash "02")
           "parser_config_hash" (validate/example-hash "03")
           "parser_ir_schema_hash" (validate/example-hash "04")
           "warning_sidecar_hash" (validate/example-hash "05")
           "run_summary_hash" (validate/example-hash "06")
           "comparison_report_hash" (validate/example-hash "07")}))))
  (testing "reports missing keys"
    (is (= ["ab-validator manifest inputs missing keys: comparison_report_hash, parser_build_hash, parser_config_hash, parser_ir_schema_hash, producer_version, run_summary_hash, warning_sidecar_hash, work_content_hash, work_id"]
           (validate/manifest-input-errors {"producer" "ab-validator"}))))
  (testing "reports invalid hash values"
    (is (= ["ab-validator manifest input work_content_hash is not a sha256 hash: nope"]
           (validate/manifest-input-errors
            {"producer" "ab-validator"
             "producer_version" "0.0.0"
             "work_id" "fixture"
             "work_content_hash" "nope"
             "parser_build_hash" (validate/example-hash "02")
             "parser_config_hash" (validate/example-hash "03")
             "parser_ir_schema_hash" (validate/example-hash "04")
             "warning_sidecar_hash" (validate/example-hash "05")
             "run_summary_hash" (validate/example-hash "06")
             "comparison_report_hash" (validate/example-hash "07")})))))

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
