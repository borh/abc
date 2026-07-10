(ns abc.tools.annotation-join-stats-run-test
  (:require [abc.tools.annotation-join-stats-run :as run]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.schema :as schema]
            [abc.tools.source-snapshot-fixture :as fixture]
            [abc.tools.workflow :as workflow]
            [clojure.java.io :as io]
            [clojure.string :as string]
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

(defn- absolute-bash-path []
  (let [process (.start (ProcessBuilder. ^java.util.List
                         ["bash" "-c" "command -v bash"]))
        stdout (string/trim (slurp (.getInputStream process)))
        stderr (string/trim (slurp (.getErrorStream process)))
        exit (.waitFor process)]
    (when (or (not (zero? exit)) (string/blank? stdout))
      (throw (ex-info "Unable to resolve an absolute Bash path"
                      {:exit exit :stderr stderr})))
    stdout))

(defn- write-stub! [file content]
  (spit file (string/replace-first content
                                   #"^#!/usr/bin/env bash"
                                   (str "#!" (absolute-bash-path))))
  (.setExecutable ^java.io.File file true)
  file)

(deftest write-stub-resolves-portable-bash-shebang-test
  (let [root (fixture/temp-dir "abc-join-stats-run-stub")
        stub (io/file root "stub.sh")]
    (try
      (write-stub! stub "#!/usr/bin/env bash\nexit 0\n")
      (let [first-line (first (string/split-lines (slurp stub)))
            bash-path (subs first-line 2)
            bash-file (io/file bash-path)]
        (is (not= "#!/usr/bin/env bash" first-line))
        (is (.isAbsolute bash-file))
        (is (.canExecute bash-file))
        (is (= "bash" (.getName bash-file))))
      (finally
        (fixture/delete-tree! root)))))

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
        ;; dies immediately — the run must surface the child's exit code
        ;; and stderr
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

(deftest run-surfaces-missing-tokenizer-output-test
  (let [root (fixture/temp-dir "abc-join-stats-run-missing-tokens")
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
        ;; exits 0 but silently skips one work's output file — the run must
        ;; fail closed on the missing tokens.jsonl
        tokenizer (write-stub!
                   (io/file root "stub-tokenizer.sh")
                   (str "#!/usr/bin/env bash\nset -eu\n"
                        "pd=\"\"; od=\"\"\n"
                        "while [ $# -gt 0 ]; do\n"
                        "  case \"$1\" in\n"
                        "    --plaintext-dir) pd=\"$2\"; shift 2 ;;\n"
                        "    --out-dir) od=\"$2\"; shift 2 ;;\n"
                        "    *) shift ;;\n"
                        "  esac\n"
                        "done\n"
                        "mkdir -p \"$od\"\n"
                        "works=0\n"
                        "for f in \"$pd\"/*.txt; do\n"
                        "  stem=$(basename \"$f\" .txt)\n"
                        "  case \"$stem\" in 000002*) continue ;; esac\n"
                        "  : > \"$od/$stem.tokens.jsonl\"\n"
                        "  works=$((works+1))\n"
                        "done\n"
                        "printf '{\"works\":%d,\"tokens\":0,\"analyzer\":\"stub\",\"warnings\":0}\\n' \"$works\"\n"))]
    (try
      (.mkdirs aat-dir)
      (spit (io/file aat-dir "000001_1-aaaaaaaaaaaa.json") "{}")
      (spit (io/file aat-dir "000002_2-bbbbbbbbbbbb.json") "{}")
      (manifest/write-json-file!
       plan-file
       {"label" "missing-tokens-test"
        "aat_dir" (str aat-dir)
        "mapping" parser-ir-fixture
        "stride" 1
        "tokenizer_dict" "vibrato:stub-dict"})
      (let [ex (try
                 (run/run-annotation-join-stats-run!
                  {:plan-file (str plan-file)
                   :out-root (str out-root)
                   :converter-bin (str converter)
                   :tokenizer-bin (str tokenizer)})
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
        (is (some? ex))
        (is (= "Tokenizer output missing for works" (ex-message ex)))
        (is (= ["000002_2"] (:missing (ex-data ex)))))
      (finally
        (fixture/delete-tree! root)))))

(deftest run-tolerates-per-work-tokenizer-errors-test
  (let [root (fixture/temp-dir "abc-join-stats-run-tok-errors")
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
        ;; tokenizes 000001_* but records 000002_* as a per-work error, the
        ;; way tokenize-plaintext reports span-reconstruction failures
        tokenizer (write-stub!
                   (io/file root "stub-tokenizer.sh")
                   (str "#!/usr/bin/env bash\nset -eu\n"
                        "pd=\"\"; od=\"\"\n"
                        "while [ $# -gt 0 ]; do\n"
                        "  case \"$1\" in\n"
                        "    --plaintext-dir) pd=\"$2\"; shift 2 ;;\n"
                        "    --out-dir) od=\"$2\"; shift 2 ;;\n"
                        "    *) shift ;;\n"
                        "  esac\n"
                        "done\n"
                        "mkdir -p \"$od\"\n"
                        "works=0; tokens=0; errors=0\n"
                        ": > \"$od/tokenize-errors.jsonl\"\n"
                        "for f in \"$pd\"/*.txt; do\n"
                        "  stem=$(basename \"$f\" .txt)\n"
                        "  case \"$stem\" in\n"
                        "    000002*)\n"
                        "      printf '{\"work_id\":\"%s\",\"error\":\"surface mismatch\"}\\n' \"$stem\" >> \"$od/tokenize-errors.jsonl\"\n"
                        "      errors=$((errors+1)); continue ;;\n"
                        "  esac\n"
                        "  out=\"$od/$stem.tokens.jsonl\"\n"
                        "  : > \"$out\"\n"
                        "  while IFS= read -r line || [ -n \"$line\" ]; do\n"
                        "    if [ -n \"$line\" ]; then\n"
                        "      printf '{\"surface\":\"%s\"}\\n' \"$line\" >> \"$out\"\n"
                        "      tokens=$((tokens+1))\n"
                        "    fi\n"
                        "  done < \"$f\"\n"
                        "  works=$((works+1))\n"
                        "done\n"
                        "printf '{\"works\":%d,\"tokens\":%d,\"analyzer\":\"stub\",\"warnings\":0,\"errors\":%d}\\n' \"$works\" \"$tokens\" \"$errors\"\n"))]
    (try
      (.mkdirs aat-dir)
      (spit (io/file aat-dir "000001_1-aaaaaaaaaaaa.json") "{}")
      (spit (io/file aat-dir "000002_2-bbbbbbbbbbbb.json") "{}")
      (manifest/write-json-file!
       plan-file
       {"label" "tok-errors-test"
        "aat_dir" (str aat-dir)
        "mapping" parser-ir-fixture
        "stride" 1
        "tokenizer_dict" "vibrato:stub-dict"})
      (let [exit (run/run-annotation-join-stats-run!
                  {:plan-file (str plan-file)
                   :out-root (str out-root)
                   :converter-bin (str converter)
                   :tokenizer-bin (str tokenizer)})
            run-record (files/read-json (io/file out-root "workflow-run.json"))
            aggregate (files/read-json
                       (io/file out-root "stats" "aggregate.json"))]
        (is (= 0 exit))
        (is (= "passed" (get run-record "status")))
        (testing "the errored work is excluded and lands in skipped_work_ids"
          (is (= 1 (get aggregate "work_count")))
          (is (= ["000002_2"] (get aggregate "skipped_work_ids"))))
        (testing "the tokenize step records a warning about errored works"
          (let [tokenize-step (->> (get run-record "steps")
                                   (filter #(= "tokenize" (get % "id")))
                                   first)]
            (is (some #(and (= "warn" (get % "level"))
                            (re-find #"1 works failed tokenization"
                                     (get % "message")))
                      (get tokenize-step "messages"))))))
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
        ;; stub tokenize-plaintext: each non-blank plaintext line becomes one
        ;; token in that work's tokens.jsonl, plus the summary line on stdout
        tokenizer (write-stub!
                   (io/file root "stub-tokenizer.sh")
                   (str "#!/usr/bin/env bash\nset -eu\n"
                        "[ \"$1\" = tokenize-plaintext ]\n"
                        "pd=\"\"; od=\"\"\n"
                        "while [ $# -gt 0 ]; do\n"
                        "  case \"$1\" in\n"
                        "    --plaintext-dir) pd=\"$2\"; shift 2 ;;\n"
                        "    --out-dir) od=\"$2\"; shift 2 ;;\n"
                        "    *) shift ;;\n"
                        "  esac\n"
                        "done\n"
                        "mkdir -p \"$od\"\n"
                        "works=0; tokens=0\n"
                        "for f in \"$pd\"/*.txt; do\n"
                        "  stem=$(basename \"$f\" .txt)\n"
                        "  out=\"$od/$stem.tokens.jsonl\"\n"
                        "  : > \"$out\"\n"
                        "  while IFS= read -r line || [ -n \"$line\" ]; do\n"
                        "    if [ -n \"$line\" ]; then\n"
                        "      printf '{\"surface\":\"%s\"}\\n' \"$line\" >> \"$out\"\n"
                        "      tokens=$((tokens+1))\n"
                        "    fi\n"
                        "  done < \"$f\"\n"
                        "  works=$((works+1))\n"
                        "done\n"
                        "printf '{\"works\":%d,\"tokens\":%d,\"analyzer\":\"stub\",\"warnings\":0}\\n' \"$works\" \"$tokens\"\n"))]
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
        (testing "streamed demux keeps each work's tokens aligned to its
                  own plaintext lines"
          (doseq [work-id ["000001_1" "000002_2"]]
            (let [plain-lines (->> (string/split
                                    (slurp (io/file out-root "plaintext"
                                                    (str work-id ".txt")))
                                    #"\n" -1)
                                   (remove string/blank?))
                  token-surfaces (->> (files/read-json-lines
                                       (io/file out-root "tokens"
                                                (str work-id ".tokens.jsonl")))
                                      (mapv #(get % "surface")))]
              (is (= (vec plain-lines) token-surfaces)))))
        (testing "stats cover both sampled works"
          (is (= 2 (get aggregate "work_count")))
          (is (= [] (get aggregate "skipped_work_ids")))))
      (finally
        (fixture/delete-tree! root)))))
