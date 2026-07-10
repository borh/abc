(ns abc.tools.annotation-join-stats-run-test
  (:require [abc.tools.annotation-join-stats-run :as run]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.schema :as schema]
            [abc.tools.source-snapshot-fixture :as fixture]
            [abc.tools.workflow :as workflow]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(deftest work-id-from-aat-filename-test
  (is (= "000050_50770"
         (run/work-id-from-aat-filename "000050_50770-ec42438f68c8.json"))))

(deftest work-id-from-aat-filename-rejects-unexpected-test
  (let [ex (try
             (run/work-id-from-aat-filename "no-hash-suffix.json")
             nil
             (catch clojure.lang.ExceptionInfo e e))]
    (is (some? ex))
    (is (= "AAT filename does not match <work-id>-<hash12>.json"
           (ex-message ex)))))

(deftest sample-aat-files-strides-sorted-test
  (let [root (fixture/temp-dir "abc-join-stats-run-sample")]
    (try
      (doseq [n ["c-cccccccccccc" "a-aaaaaaaaaaaa" "e-eeeeeeeeeeee"
                 "b-bbbbbbbbbbbb" "d-dddddddddddd"]]
        (spit (io/file root (str n ".json")) "{}"))
      (spit (io/file root "not-json.txt") "ignored")
      (let [{:keys [total sampled]} (run/sample-aat-files root 2)]
        (is (= 5 total))
        (is (= ["a-aaaaaaaaaaaa.json" "c-cccccccccccc.json"
                "e-eeeeeeeeeeee.json"]
               (mapv #(.getName ^java.io.File %) sampled))))
      (finally
        (fixture/delete-tree! root)))))

(deftest sample-aat-files-rejects-empty-dir-test
  (let [root (fixture/temp-dir "abc-join-stats-run-empty")]
    (try
      (let [ex (try
                 (run/sample-aat-files root 1)
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
        (is (some? ex))
        (is (= "AAT directory contains no .json files" (ex-message ex))))
      (finally
        (fixture/delete-tree! root)))))

(deftest split-eos-groups-test
  (is (= [["a"] [] ["b" "c"]]
         (run/split-eos-groups
          "a\t名詞\nEOS\nEOS\nb\t動詞\nc\t助詞\nEOS\n"))))

(deftest run-rejects-missing-binaries-test
  (let [ex (try
             (run/run-annotation-join-stats-run!
              {:plan-file "unused.json"
               :out-root "unused"
               :converter-bin nil
               :tokenizer-bin "/bin/true"})
             nil
             (catch clojure.lang.ExceptionInfo e e))]
    (is (some? ex))
    (is (= "Missing required binary: converter-bin" (ex-message ex)))))

(deftest run-rejects-incomplete-plan-test
  (let [root (fixture/temp-dir "abc-join-stats-run-plan")]
    (try
      (let [plan-file (io/file root "plan.json")]
        (manifest/write-json-file! plan-file {"label" "x" "aat_dir" "y"})
        (let [ex (try
                   (run/run-annotation-join-stats-run!
                    {:plan-file (str plan-file)
                     :out-root (str (io/file root "out"))
                     :converter-bin "/bin/true"
                     :tokenizer-bin "/bin/true"})
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
          (is (some? ex))
          (is (= "Join-stats plan is missing required keys" (ex-message ex)))
          (is (= ["mapping" "stride" "tokenizer_dict"]
                 (:missing (ex-data ex))))))
      (finally
        (fixture/delete-tree! root)))))

(defn- write-stub! [file content]
  (spit file content)
  (.setExecutable ^java.io.File file true)
  file)

(deftest run-surfaces-tokenizer-failure-with-stderr-test
  (let [root (fixture/temp-dir "abc-join-stats-run-tokfail")
        aat-dir (io/file root "aat")
        out-root (io/file root "out")
        plan-file (io/file root "plan.json")
        parser-ir-fixture (.getAbsolutePath
                           (io/file "examples/ab-validator-output/parser-ir.json"))
        converter (write-stub!
                   (io/file root "stub-converter.sh")
                   (str "#!/usr/bin/env bash\nset -eu\n"
                        "out=\"\"; div=\"\"\n"
                        "while [ $# -gt 0 ]; do\n"
                        "  case \"$1\" in\n"
                        "    --parser-ir-out) out=\"$2\"; shift 2 ;;\n"
                        "    --divergence-out) div=\"$2\"; shift 2 ;;\n"
                        "    *) shift ;;\n"
                        "  esac\n"
                        "done\n"
                        "cp " parser-ir-fixture " \"$out\"\n"
                        "printf '{}' > \"$div\"\n"))
        ;; dies without reading stdin — the run must surface the child's
        ;; exit code and stderr, not a stdin write failure
        tokenizer (write-stub!
                   (io/file root "stub-tokenizer.sh")
                   "#!/usr/bin/env bash\necho 'dictionary exploded' >&2\nexit 7\n")]
    (try
      (.mkdirs aat-dir)
      (spit (io/file aat-dir "000001_1-aaaaaaaaaaaa.json") "{}")
      (manifest/write-json-file!
       plan-file
       {"label" "tokfail-test"
        "aat_dir" (str aat-dir)
        "mapping" parser-ir-fixture
        "stride" 1
        "tokenizer_dict" "stub-dict"})
      (let [ex (try
                 (run/run-annotation-join-stats-run!
                  {:plan-file (str plan-file)
                   :out-root (str out-root)
                   :converter-bin (str converter)
                   :tokenizer-bin (str tokenizer)})
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
        (is (some? ex))
        (is (= "Tokenizer failed" (ex-message ex)))
        (is (= 7 (:exit (ex-data ex))))
        (is (re-find #"dictionary exploded" (:stderr (ex-data ex))))
        (testing "workflow run records the failed step before rethrowing"
          (let [run-record (files/read-json
                            (io/file out-root "workflow-run.json"))]
            (is (= "failed" (get run-record "status")))
            (is (= "failed"
                   (->> (get run-record "steps")
                        (filter #(= "tokenize" (get % "id")))
                        first
                        (#(get % "status"))))))))
      (finally
        (fixture/delete-tree! root)))))

(deftest run-annotation-join-stats-run-end-to-end-test
  (let [root (fixture/temp-dir "abc-join-stats-run-e2e")
        aat-dir (io/file root "aat")
        out-root (io/file root "out")
        plan-file (io/file root "plan.json")
        parser-ir-fixture (.getAbsolutePath
                           (io/file "examples/ab-validator-output/parser-ir.json"))
        ;; stub converter: ignores the AAT payload and materializes the
        ;; committed parser-IR fixture at --parser-ir-out
        converter (write-stub!
                   (io/file root "stub-converter.sh")
                   (str "#!/usr/bin/env bash\nset -eu\n"
                        "out=\"\"; div=\"\"\n"
                        "while [ $# -gt 0 ]; do\n"
                        "  case \"$1\" in\n"
                        "    --parser-ir-out) out=\"$2\"; shift 2 ;;\n"
                        "    --divergence-out) div=\"$2\"; shift 2 ;;\n"
                        "    *) shift ;;\n"
                        "  esac\n"
                        "done\n"
                        "cp " parser-ir-fixture " \"$out\"\n"
                        "printf '{}' > \"$div\"\n"))
        ;; stub tokenizer: MeCab-format protocol — the whole input line as
        ;; one token, one EOS per input line (blank lines included)
        tokenizer (write-stub!
                   (io/file root "stub-tokenizer.sh")
                   (str "#!/usr/bin/env bash\nset -eu\n"
                        "while IFS= read -r line; do\n"
                        "  if [ -n \"$line\" ]; then\n"
                        "    printf '%s\\t形容詞\\n' \"$line\"\n"
                        "  fi\n"
                        "  echo EOS\n"
                        "done\n"))]
    (try
      (.mkdirs aat-dir)
      (spit (io/file aat-dir "000001_1-aaaaaaaaaaaa.json") "{}")
      (spit (io/file aat-dir "000002_2-bbbbbbbbbbbb.json") "{}")
      (manifest/write-json-file!
       plan-file
       {"label" "e2e-test"
        "aat_dir" (str aat-dir)
        ;; stub converter ignores the mapping; any existing file works
        "mapping" parser-ir-fixture
        "stride" 1
        "tokenizer_dict" "stub-dict"})
      (let [exit (run/run-annotation-join-stats-run!
                  {:plan-file (str plan-file)
                   :out-root (str out-root)
                   :converter-bin (str converter)
                   :tokenizer-bin (str tokenizer)})
            run-record (files/read-json (io/file out-root "workflow-run.json"))
            aggregate (files/read-json
                       (io/file out-root "stats" "aggregate.json"))]
        (is (= 0 exit))
        (testing "workflow run record is engine-valid"
          (is (= "passed" (get run-record "status")))
          (is (= "soranoha.annotation-join-stats.v1"
                 (get run-record "workflow_id")))
          (is (= [] (workflow/validate-run run-record)))
          (is (nil? (schema/validation-errors
                     (files/read-json "schemas/workflow-run.schema.json")
                     run-record))))
        (testing "run copies its plan beside the outputs"
          (is (= (files/read-json plan-file)
                 (files/read-json (io/file out-root "join-stats-plan.json")))))
        (testing "pipeline artifacts exist per work"
          (doseq [work-id ["000001_1" "000002_2"]]
            (is (.isFile (io/file out-root "parser-ir" work-id
                                  "parser-ir.json")))
            (is (.isFile (io/file out-root "plaintext" (str work-id ".txt"))))
            (is (.isFile (io/file out-root "tokens"
                                  (str work-id ".tokens.jsonl"))))))
        (testing "stats cover both sampled works"
          (is (= 2 (get aggregate "work_count")))
          (is (= [] (get aggregate "skipped_work_ids")))))
      (finally
        (fixture/delete-tree! root)))))
