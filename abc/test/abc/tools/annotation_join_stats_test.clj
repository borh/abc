(ns abc.tools.annotation-join-stats-test
  (:require [abc.tools.annotation-join-stats :as stats]
            [abc.tools.files :as files]
            [abc.tools.manifest :as manifest]
            [abc.tools.parser-ir-plaintext :as plaintext]
            [abc.tools.source-snapshot-fixture :as fixture]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(defn- token-row [surface start end]
  {"surface" surface "char_start" start "char_end" end})

(defn- per-scalar-token-lines
  "tokens.jsonl content with one token per unicode scalar of text and
  tokenizer-reported char offsets — guarantees valid spans regardless of
  fixture content."
  [^String text]
  (apply str
         (map-indexed
          (fn [i cp]
            (str "{\"surface\":"
                 (pr-str (String. (Character/toChars (int cp))))
                 ",\"char_start\":" i
                 ",\"char_end\":" (inc i) "}\n"))
          (iterator-seq (.iterator (.codePoints text))))))

(deftest token-spans-builds-spans-from-reported-offsets-test
  (let [{:keys [tokens failure]} (stats/token-spans
                                  "吾輩は\n猫である"
                                  [(token-row "吾輩" 0 2)
                                   (token-row "は" 2 3)
                                   (token-row "猫" 4 5)
                                   (token-row "で" 5 6)
                                   (token-row "ある" 6 8)])]
    (is (nil? failure))
    (is (= [{"token_index" 0 "input_span" {"start" 0 "end" 2} "text" "吾輩"}
            {"token_index" 1 "input_span" {"start" 2 "end" 3} "text" "は"}
            {"token_index" 2 "input_span" {"start" 4 "end" 5} "text" "猫"}
            {"token_index" 3 "input_span" {"start" 5 "end" 6} "text" "で"}
            {"token_index" 4 "input_span" {"start" 6 "end" 8} "text" "ある"}]
           tokens))))

(deftest token-spans-accepts-whitespace-leading-surfaces-test
  ;; the full-corpus qkana failure class under surface re-walking: the
  ;; tokenizer-reported offsets make leading-whitespace surfaces
  ;; unremarkable
  (let [{:keys [tokens failure]} (stats/token-spans
                                  "あ　　～х"
                                  [(token-row "あ" 0 1)
                                   (token-row "　　～" 1 4)])]
    (is (nil? failure))
    (is (= {"start" 1 "end" 4} (get-in tokens [1 "input_span"])))))

(deftest token-spans-uses-scalar-offsets-for-bounds-test
  ;; "𠮟" is one unicode scalar (two UTF-16 units); char_end 2 must be
  ;; in bounds for the two-scalar text
  (let [{:keys [tokens failure]} (stats/token-spans
                                  "𠮟る" [(token-row "𠮟る" 0 2)])]
    (is (nil? failure))
    (is (= {"start" 0 "end" 2} (get-in tokens [0 "input_span"])))))

(deftest token-spans-records-failures-test
  (testing "out-of-bounds span"
    (let [{:keys [tokens failure]} (stats/token-spans
                                    "吾輩は猫" [(token-row "吾輩は猫と犬" 0 6)])]
      (is (nil? tokens))
      (is (= 0 (:token-index failure)))
      (is (= 4 (:text-scalar-count failure)))))
  (testing "overlapping spans"
    (let [{:keys [failure]} (stats/token-spans
                             "吾輩は猫" [(token-row "吾輩" 0 2)
                                     (token-row "輩は" 1 3)])]
      (is (= 1 (:token-index failure)))
      (is (= 2 (:prev-end failure)))))
  (testing "empty and reversed spans"
    (is (some? (:failure (stats/token-spans "吾輩" [(token-row "" 1 1)]))))
    (is (some? (:failure (stats/token-spans "吾輩" [(token-row "吾" 1 0)])))))
  (testing "missing offsets"
    (let [{:keys [failure]} (stats/token-spans "吾輩" [{"surface" "吾輩"}])]
      (is (= 0 (:token-index failure))))))

(deftest token-spans-rejects-non-string-surface-test
  (let [ex (try
             (stats/token-spans "吾輩は" [(token-row "吾輩" 0 2)
                                       (token-row nil 2 3)])
             nil
             (catch clojure.lang.ExceptionInfo e e))]
    (is (some? ex))
    (is (= "Token surface must be a string" (ex-message ex)))
    (is (= 1 (:token-index (ex-data ex))))
    (is (nil? (:surface (ex-data ex))))))

(deftest work-stats-classifies-fixture-annotations-test
  (let [parser-ir (files/read-json "examples/ab-validator-output/parser-ir.json")
        {:keys [annotations]} (plaintext/render-with-annotations parser-ir)
        ;; one token exactly covering the ruby base = aligned-single; the
        ;; rest of the text as whatever tokens the walk yields
        ruby-span (get-in (first (filter #(= "ruby" (get % "annotation_kind"))
                                         annotations))
                          ["span"])
        result (stats/work-stats
                {:annotations annotations
                 :tokens [{"token_index" 0
                           "input_span" ruby-span
                           "text" "x"}]})]
    (is (pos? (get-in result [:annotation_counts "ruby"])))
    (is (= 1 (get-in result [:classifications "ruby" "aligned-single"])))
    (testing "every annotation is classified"
      (is (= (reduce + (vals (:annotation_counts result)))
             (reduce + (mapcat vals (vals (:classifications result)))))))))

(deftest aggregate-sums-and-rates-test
  (let [agg (stats/aggregate
             [{:work-id "a"
               :annotation_counts {"ruby" 2 "gaiji" 1}
               :classifications {"ruby" {"aligned-single" 1 "stem-prefix" 1}
                                 "gaiji" {"conflict" 1}}}
              {:work-id "b"
               :annotation_counts {"ruby" 1}
               :classifications {"ruby" {"aligned-multi" 1}}}])]
    (is (= 2 (:work_count agg)))
    (is (= {"ruby" 3 "gaiji" 1} (:annotation_counts agg)))
    (is (= 1 (get-in agg [:classifications "ruby" "aligned-single"])))
    (is (= 1 (get-in agg [:classifications "ruby" "aligned-multi"])))
    ;; rates are fractions of the kind's corpus-wide total (ruby total = 3),
    ;; so a kind's rates always sum to 1
    (is (= (double (/ 1 3))
           (get-in agg [:classification_rates "ruby" "stem-prefix"])))
    (is (= 1.0 (reduce + (vals (get-in agg [:classification_rates "ruby"])))))))

(deftest bare-work-id-strips-content-hash-test
  (is (= "000050_50770" (stats/bare-work-id "000050_50770-ec42438f68c8")))
  (testing "stems without a hash suffix pass through unchanged"
    (is (= "work-a" (stats/bare-work-id "work-a")))))

(deftest work-level-rows-merges-file-stems-test
  (let [rows [{"work_id" "000001_1-aaaaaaaaaaaa"
               "text_scalar_count" 10
               "annotation_counts" {"ruby" 2}
               "classifications" {"ruby" {"aligned-single" 1 "conflict" 1}}}
              {"work_id" "000001_1-bbbbbbbbbbbb"
               "text_scalar_count" 5
               "annotation_counts" {"ruby" 1 "gaiji" 1}
               "classifications" {"ruby" {"aligned-single" 1}
                                  "gaiji" {"stem-prefix" 1}}}
              {"work_id" "000002_2-cccccccccccc"
               "text_scalar_count" 3
               "annotation_counts" {"ruby" 1}
               "classifications" {"ruby" {"aligned-multi" 1}}}]
        work-rows (stats/work-level-rows rows)]
    (is (= 2 (count work-rows)))
    (is (= {"work_id" "000001_1"
            "file_count" 2
            "file_stems" ["000001_1-aaaaaaaaaaaa" "000001_1-bbbbbbbbbbbb"]
            "text_scalar_count" 15
            "annotation_counts" {"ruby" 3 "gaiji" 1}
            "classifications" {"ruby" {"aligned-single" 2 "conflict" 1}
                               "gaiji" {"stem-prefix" 1}}}
           (first work-rows)))
    (is (= {"work_id" "000002_2"
            "file_count" 1
            "file_stems" ["000002_2-cccccccccccc"]
            "text_scalar_count" 3
            "annotation_counts" {"ruby" 1}
            "classifications" {"ruby" {"aligned-multi" 1}}}
           (second work-rows)))))

(deftest run-join-stats-over-fixture-corpus-test
  (let [root (fixture/temp-dir "abc-join-stats")
        parser-ir-dir (io/file root "parser-ir")
        tokens-dir (io/file root "tokens")
        out-dir (io/file root "out")
        parser-ir (files/read-json "examples/ab-validator-output/parser-ir.json")
        {:keys [text]} (plaintext/render-with-annotations parser-ir)]
    (try
      (.mkdirs (io/file parser-ir-dir "work-a"))
      (.mkdirs tokens-dir)
      (manifest/write-json-file! (io/file parser-ir-dir "work-a" "parser-ir.json")
                                 parser-ir)
      (spit (io/file tokens-dir "work-a.tokens.jsonl")
            (per-scalar-token-lines text))
      ;; work-b has parser-IR but no token file → recorded as skipped
      (.mkdirs (io/file parser-ir-dir "work-b"))
      (manifest/write-json-file! (io/file parser-ir-dir "work-b" "parser-ir.json")
                                 parser-ir)
      ;; work-c is an empty subdir (no parser-ir.json, no tokens) → recorded
      ;; as skipped rather than silently excluded
      (.mkdirs (io/file parser-ir-dir "work-c"))
      (let [exit (stats/run-join-stats! (str parser-ir-dir)
                                        (str tokens-dir)
                                        (str out-dir))
            aggregate (files/read-json (io/file out-dir "aggregate.json"))
            per-work (files/read-json-lines (io/file out-dir "per-work.jsonl"))]
        (is (= 0 exit))
        (is (.exists (io/file out-dir "report.md")))
        (is (= 1 (get aggregate "work_count")))
        (is (= ["work-b" "work-c"] (get aggregate "skipped_work_ids")))
        (is (= 1 (count per-work)))
        (is (= "work-a" (get (first per-work) "work_id")))
        (is (pos? (get-in aggregate ["annotation_counts" "ruby"]))))
      (finally
        (fixture/delete-tree! root)))))

(deftest run-join-stats-rolls-up-multi-file-works-test
  (let [root (fixture/temp-dir "abc-join-stats-work-level")
        parser-ir-dir (io/file root "parser-ir")
        tokens-dir (io/file root "tokens")
        out-dir (io/file root "out")
        parser-ir (files/read-json "examples/ab-validator-output/parser-ir.json")
        {:keys [text]} (plaintext/render-with-annotations parser-ir)
        token-lines (per-scalar-token-lines text)]
    (try
      ;; two file stems of the same bare work id (fragments with distinct
      ;; content hashes) plus one single-file work
      (doseq [stem ["000001_1-aaaaaaaaaaaa"
                    "000001_1-bbbbbbbbbbbb"
                    "000002_2-cccccccccccc"]]
        (.mkdirs (io/file parser-ir-dir stem))
        (manifest/write-json-file! (io/file parser-ir-dir stem "parser-ir.json")
                                   parser-ir)
        (.mkdirs tokens-dir)
        (spit (io/file tokens-dir (str stem ".tokens.jsonl")) token-lines))
      (let [exit (stats/run-join-stats! (str parser-ir-dir)
                                        (str tokens-dir)
                                        (str out-dir))
            aggregate (files/read-json (io/file out-dir "aggregate.json"))
            per-work (files/read-json-lines (io/file out-dir "per-work.jsonl"))
            work-level (files/read-json-lines
                        (io/file out-dir "work-level.jsonl"))]
        (is (= 0 exit))
        (is (= 3 (get aggregate "work_count")))
        (is (= 2 (get aggregate "distinct_work_count")))
        (is (= 3 (count per-work)))
        (is (= 2 (count work-level)))
        (let [merged (first work-level)]
          (is (= "000001_1" (get merged "work_id")))
          (is (= 2 (get merged "file_count")))
          (is (= ["000001_1-aaaaaaaaaaaa" "000001_1-bbbbbbbbbbbb"]
                 (get merged "file_stems")))
          (testing "counts are summed across the work's file stems"
            (is (= (* 2 (get-in (first per-work) ["annotation_counts" "ruby"]))
                   (get-in merged ["annotation_counts" "ruby"])))
            (is (= (* 2 (get (first per-work) "text_scalar_count"))
                   (get merged "text_scalar_count")))))
        (testing "single-file works roll up unchanged"
          (let [single (second work-level)]
            (is (= "000002_2" (get single "work_id")))
            (is (= 1 (get single "file_count")))
            (is (= (get (last per-work) "annotation_counts")
                   (get single "annotation_counts"))))))
      (finally
        (fixture/delete-tree! root)))))

(deftest run-join-stats-skips-failed-span-walk-test
  (let [root (fixture/temp-dir "abc-join-stats-failed-walk")
        parser-ir-dir (io/file root "parser-ir")
        tokens-dir (io/file root "tokens")
        out-dir (io/file root "out")
        parser-ir (files/read-json "examples/ab-validator-output/parser-ir.json")]
    (try
      (.mkdirs (io/file parser-ir-dir "work-d"))
      (.mkdirs tokens-dir)
      (manifest/write-json-file! (io/file parser-ir-dir "work-d" "parser-ir.json")
                                 parser-ir)
      ;; the reported span is far out of bounds for the rendered plaintext
      ;; → span validation fails, so work-d must land in skipped_work_ids
      ;; and never appear in per-work.jsonl
      (spit (io/file tokens-dir "work-d.tokens.jsonl")
            "{\"surface\":\"ZZZ\",\"char_start\":0,\"char_end\":999999999}\n")
      (let [exit (stats/run-join-stats! (str parser-ir-dir)
                                        (str tokens-dir)
                                        (str out-dir))
            aggregate (files/read-json (io/file out-dir "aggregate.json"))
            per-work (files/read-json-lines (io/file out-dir "per-work.jsonl"))]
        (is (= 0 exit))
        (is (= 0 (get aggregate "work_count")))
        (is (= ["work-d"] (get aggregate "skipped_work_ids")))
        (is (= 0 (count per-work)))
        (is (not (contains? (set (map #(get % "work_id") per-work)) "work-d"))))
      (finally
        (fixture/delete-tree! root)))))
