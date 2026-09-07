(ns soranoha.ori.accountability-test
  (:require [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [soranoha.core.hash :as hash]
            [soranoha.kura.cas :as cas]
            [soranoha.kura.engine :as engine]
            [soranoha.ori.stages :as stages]
            [soranoha.ori.accountability :as accountability]))

(deftest native-source-stage-keeps-lexical-evidence-independent
  (let [tool (accountability/resolve-tool)
        stage (accountability/source-stage tool)
        source (.getBytes "漢字《かんじ》［＃］" "UTF-8")
        result ((:f stage) {:blob {"source-id" source}} {"source" "source-id"})
        report (json/read-json (String. ^bytes (get result "source-accountability") "UTF-8"))]
    (is (= "source-accountability" (:stage-id stage)))
    (is (= (hash/sha256-canonical-json
            {"binary" (hash/sha256-file (:bin tool))
             "matrix" (hash/sha256-file (:matrix tool))})
           (:toolchain-id stage)))
    (is (= "aozora-source-accountability/1" (get report "schema")))
    (is (= (str "sha256:" (hash/sha256-bytes source)) (get report "source_sha256")))
    (is (= "not-assessed" (get report "semantic_coverage")))
    (is (= [["ruby.basic"] []] (mapv #(get % "families") (get report "occurrences"))))
    (is (= ["《かんじ》" "［＃］"] (mapv #(get % "raw") (get report "occurrences"))))))

(defn- span [start end]
  {"start" start "end" end "coordinate_system" "decoded_utf8"})

(defn- oracle [occurrences]
  {"schema" "aozora-source-accountability/1" "source_sha256" "sha256:source"
   "encoding" "utf-8" "decode_outcome" "lossless" "occurrences" occurrences})

(defn- interpretation [facts]
  {"source" {"primary_text_hash" "sha256:source" "decode_outcome" "utf-8"}
   "interpretation_facts" facts "interpretation_problems" []})

(deftest claims-need-compatible-kinds-and-spans-and-do-not-certify-semantics
  (let [ruby {"source_span" (span 6 21) "kind" "RubyImplicit" "region" "body"
              "raw" "《かんじ》" "families" ["ruby.basic"]}
        emphasis {"kind" "emphasis" "outcome" "established" "aspects" ["layout"]
                  "source_span" (span 0 80)}
        fact {"kind" "ruby" "outcome" "established" "aspects" ["content" "structure"]
              "source_span" (span 0 21)}
        unmatched (accountability/coverage-report (oracle [ruby]) (interpretation [emphasis]))
        matched (accountability/coverage-report (oracle [ruby]) (interpretation [emphasis fact]))]
    (is (= 1 (get-in unmatched ["families" "ruby.basic" "unaccounted"])))
    (is (= 1 (get-in matched ["families" "ruby.basic" "interpreter_claimed"])))
    (is (= ["ruby"] (mapv #(get % "kind") (get-in matched ["occurrences" 0 "claims"]))))
    (is (= "not-assessed" (get matched "semantic_certification")))
    (is (= 1 (get-in (accountability/coverage-report
                      (oracle [ruby]) (interpretation [(assoc fact "source_span" (span 0 20))]))
                     ["families" "ruby.basic" "unaccounted"])))
    (is (= 1 (get-in (accountability/coverage-report
                      (assoc (oracle [ruby]) "decode_outcome" "lossy") (interpretation [fact]))
                     ["families" "ruby.basic" "unaccounted"])))))

(deftest absence-of-facts-and-explicit-problems-remain-visible
  (let [unknown {"source_span" (span 0 9) "kind" "CommandFullwidth" "region" "body"
                 "raw" "［＃］" "families" []}
        problem {"kind" "unknown-notation" "aspects" ["content" "structure" "layout"]
                 "source_span" (span 0 9) "influence" {"kind" "document"}}
        report (accountability/coverage-report (oracle [unknown])
                                               (assoc (interpretation []) "interpretation_problems" [problem]))]
    (is (= 1 (get report "unclassified_occurrences")))
    (is (= [problem] (get report "interpretation_problems")))
    (is (empty? (get-in report ["occurrences" 0 "claims"])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (accountability/coverage-report (oracle [unknown])
                                                 (dissoc (interpretation []) "interpretation_facts"))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (accountability/coverage-report (assoc (oracle [unknown]) "source_sha256" "different")
                                                 (interpretation []))))))

(deftest native-interpretation-and-oracle-share-the-decoded-coordinate-axis
  (let [dir (fs/create-temp-dir {:prefix "accountability-conformance"})
        store (engine/open-store! {:cas-dir (str (fs/path dir "objects"))
                                   :db-path (str (fs/path dir "trace.sqlite"))})
        adapter (stages/resolve-adapter)
        source-stage (accountability/source-stage (accountability/resolve-tool))
        text "題\n作者\n\n----------\n【テキスト中に現れる記号について】\n----------\n漢字《かんじ》。\n\n底本：本\n"]
    (try
      (doseq [[label ^String encoded-text ^String encoding]
              [["UTF-8" text "UTF-8"]
               ["BOM" (str "\uFEFF" text) "UTF-8"]
               ["CRLF" (str/replace text "\n" "\r\n") "UTF-8"]
               ["ShiftJIS" text "windows-31j"]
               ["lossy" text "windows-31j"]]]
        (testing label
          (let [lossy? (= "lossy" label)
                bytes (if lossy?
                        (byte-array (concat (.getBytes encoded-text encoding) [(unchecked-byte 0x81)]))
                        (.getBytes encoded-text encoding))
                source-id (cas/put-bytes! (:cas-dir store) bytes)
                run #(engine/run-stage! store %1 %2)
                read-output (fn [result name]
                              (json/read-json
                               (String. ^bytes (cas/get-bytes (:cas-dir store) (get-in result [:outputs name])) "UTF-8")))
                parsed (run (stages/parse-stage adapter) {"source" source-id})
                converted (run (stages/convert-stage adapter)
                               {"aat" (get-in parsed [:outputs "aat"])
                                "work_content_hash" (hash/format-sha256 source-id)})
                oracle-result (run source-stage {"source" source-id})
                coverage (run (accountability/coverage-stage "test-runtime")
                              {"source-accountability" (get-in oracle-result [:outputs "source-accountability"])
                               "parser-ir" (get-in converted [:outputs "parser-ir"])})
                oracle-value (read-output oracle-result "source-accountability")
                report (read-output coverage "interpretation-coverage")
                ^String decoded (str (str/replace encoded-text #"^\uFEFF" "") (when lossy? "�"))
                expected-start (alength (.getBytes (subs decoded 0 (.indexOf decoded "《")) "UTF-8"))]
            (is (= (if lossy? "lossy" "lossless") (get oracle-value "decode_outcome")))
            (is (= (str "sha256:" (hash/sha256-bytes (.getBytes decoded "UTF-8")))
                   (get oracle-value "decoded_sha256")))
            (is (= expected-start (get-in oracle-value ["occurrences" 0 "source_span" "start"])))
            (is (= 1 (get-in report ["families" "ruby.basic"
                                     (if lossy? "unaccounted" "interpreter_claimed")])))
            (is (= "not-assessed" (get report "semantic_certification"))))))
      (finally (engine/close-store! store) (fs/delete-tree dir)))))
