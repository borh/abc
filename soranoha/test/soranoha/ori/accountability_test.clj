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
    (is (= "aozora-source-accountability/2" (get report "schema")))
    (is (= (str "sha256:" (hash/sha256-bytes source)) (get report "source_sha256")))
    (is (= "not-assessed" (get report "semantic_coverage")))
    (is (= [["ruby.basic"] []] (mapv #(get % "families") (get report "occurrences"))))
    (is (= ["《かんじ》" "［＃］"] (mapv #(get % "raw") (get report "occurrences"))))))

(defn- span [start end]
  {"start" start "end" end "coordinate_system" "decoded_utf8"})

(defn- oracle [occurrences]
  {"schema" "aozora-source-accountability/2" "source_sha256" "sha256:source"
   "encoding" "utf-8" "decode_outcome" "lossless" "occurrences" occurrences})

(defn- interpretation [facts]
  {"source" {"primary_text_hash" "sha256:source" "decode_outcome" "utf-8"}
   "interpretation_facts" facts "interpretation_problems" []})

(deftest variant-claims-do-not-resolve-embedded-gaiji
  (doseq [raw ["［＃「κιν※［＃鋭アクセント付きη、U+1F75、171-10］σεω※［＃ギリシア小文字ファイナルSIGMA、1-6-57］」は底本では「κιν※［＃鋭アクセント付きη、U+1F75、171-10］σε※［＃上方に不鮮明な符号が付いているω、171-10］※［＃ギリシア小文字ファイナルSIGMA、1-6-57］」］"
               "［＃「醇※［＃「广＋龍」、第3水準1-94-86］博朗」は底本では「醇※［＃「厂＋龍」、348-9］博朗」］"]]
    (let [source (.getBytes (str "題\n作者\n\n" raw "\n\n底本：本\n") "UTF-8")
          scanned ((:f (accountability/source-stage (accountability/resolve-tool)))
                   {:blob {"source-id" source}} {"source" "source-id"})
          evidence (json/read-json (String. ^bytes (get scanned "source-accountability") "UTF-8"))
          marker (first (filter #(= raw (get % "raw")) (get evidence "occurrences")))
          fact {"kind" "text-variant" "outcome" "established" "aspects" ["content" "structure"]
                "source_span" (get marker "source_span")}
          parsed (assoc-in (interpretation [fact]) ["source" "primary_text_hash"]
                           (get evidence "source_sha256"))
          report (accountability/coverage-report evidence parsed)
          result (first (filter #(= raw (get % "raw")) (get report "occurrences")))]
      (is (some #{"annotation.chuuki"} (get marker "families")))
      (is (= [["annotation.chuuki"]] (mapv #(get % "families") (get result "claims"))))
      (is (every? #(empty? (get % "claims")) (get result "components")))
      (is (some #{"gaiji.marker"} (get result "unaccounted_families")))
      (is (= "not-assessed" (get report "semantic_certification"))))))

(deftest enclosing-claims-cannot-establish-another-marker
  (let [marker (fn [start end] {"source_span" (span start end)
                                "kind" "CommandFullwidth" "region" "body"
                                "families" ["emphasis.basic"]})
        fact (fn [start end] {"source_span" (span start end) "kind" "emphasis"
                              "outcome" "established" "aspects" ["layout"]})
        report (accountability/coverage-report
                (oracle [(marker 15 30) (marker 0 9)])
                (interpretation [(fact 0 60) (fact 0 9)]))]
    (is (= {"interpreter_claimed" 1 "unaccounted" 1}
           (get-in report ["families" "emphasis.basic"])))
    (is (= [0 15] (mapv #(get-in % ["source_span" "start"]) (get report "occurrences"))))
    (is (= [(span 0 9)] (mapv #(get % "source_span") (get-in report ["occurrences" 0 "claims"]))))
    (is (empty? (get-in report ["occurrences" 1 "claims"])))
    (is (= ["emphasis.basic"] (get-in report ["occurrences" 1 "unaccounted_families"])))))

(deftest claims-need-compatible-kinds-and-spans-and-do-not-certify-semantics
  (let [ruby {"source_span" (span 6 21) "kind" "RubyImplicit" "region" "body"
              "raw" "《かんじ》" "families" ["ruby.basic"]}
        emphasis {"kind" "emphasis" "outcome" "established" "aspects" ["layout"]
                  "source_span" (span 0 80)}
        fact {"kind" "ruby" "outcome" "established" "aspects" ["content" "structure"]
              "source_span" (span 6 21)}
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

(deftest native-claims-identify-their-own-markers-through-the-independent-oracle
  (let [dir (fs/create-temp-dir {:prefix "marker-ownership"})
        store (engine/open-store! {:cas-dir (str (fs/path dir "objects"))
                                   :db-path (str (fs/path dir "trace.sqlite"))})
        adapter (stages/resolve-adapter)
        scanner (accountability/source-stage (accountability/resolve-tool))
        read-output (fn [result name]
                      (json/read-json
                       (String. ^bytes (cas/get-bytes (:cas-dir store) (get-in result [:outputs name])) "UTF-8")))]
    (try
      (doseq [[body unknown expected-claimed]
              [["［＃斜体］字［＃「字」に白四角傍点］［＃斜体終わり］" "［＃「字」に白四角傍点］" 2]
               ["［＃斜体］字［＃「字」の部分はイタリック体］［＃斜体終わり］" "［＃「字」の部分はイタリック体］" 2]
               ["［＃太字］字［＃「字」は斜体］［＃太字終わり］" nil 3]
               ["｜漢字《かんじ》、漢字《かんじ》。" nil 2]
               ["※［＃「需＋頁」、第3水準1-94-6］《じゅ》" nil 2]
               ["｜前※［＃「需＋頁」、第3水準1-94-6］《ぜんじゅ》" nil 1]
               ["※［＃「鬟」の「口」の下の部分に代えて「小」、33-17］" "※［＃「鬟」の「口」の下の部分に代えて「小」、33-17］" 0]
               ["※［＃ローマ数字1、1-13-21］" nil 1]
               ["※［＃「藹」の「言」に代えて「月」、第3水準1-91-26］" nil 1]
               ["２）［＃「２）」は縦中横、行右小書き］" nil 1]
               ["漢［＃レ］字［＃（ノ）］。" nil 2]
               ["［＃キャプション］字［＃キャプション終わり］" nil 2]
               ["［＃ここから割り注］字［＃ここで割り注終わり］" nil 2]
               ["本文［＃「章」は中見出し］" "［＃「章」は中見出し］" 0]
               ["［＃ここから表］\n甲／乙\n［＃ここで表終わり］" nil 2]
               ["［＃ここから５字下げ、ここから数式］\nx = 1\n［＃ここで字下げ終わり、ここで数式終わり］" nil 2]
               ["［＃ここから２段組み］\n甲［＃改段］乙\n［＃ここで段組み終わり］" nil 3]
               ["甲［＃改丁］乙［＃改ページ］丙" nil 2]
               ["甲［＃改行］乙\n丙" nil 1]
               ["甲\n乙" nil 0]
               ["前［＃挿絵（fig1.png）入る］後" nil 1]
               ["前［＃「漢字」のキャプション付きの図（fig1.png、横20×縦30）入る］後" nil 1]
               ["章［＃「章」は中見出し］" nil 1]
               ["章［＃「章」は同行中見出し］" nil 1]
               ["章［＃「章」は窓中見出し］" nil 1]
               ["字［＃「字」はキャプション］" nil 1]
               ["字［＃「字」は罫囲み］" nil 1]
               ["字［＃「字」は横組み］" nil 1]
               ["２［＃「２」は縦中横］" nil 1]
               ["字［＃「字」は底本では「宇」］" nil 1]
               ["懲々《こり／″＼》［＃ルビの「／″＼」は底本では「こり／＼」］" nil 2]
               ["※［＃「口＋愛」、第3水準1-15-23］《おくび》［＃「※［＃「口＋愛」、第3水準1-15-23］《おくび》」は底本では「※［＃「口＋愛」、第3水準1-15-23］《あくび》」］" nil 3]
               ["｜前※［＃「口＋愛」、第3水準1-15-23］《まえおくび》［＃「｜前※［＃「口＋愛」、第3水準1-15-23］《まえおくび》」は底本では「別」］" nil 2]
               ["小突《こづか》かれるので［＃「小突《こづか》かれるので」はママ］［＃「小突《こづか》かれるので［＃「小突《こづか》かれるので」はママ］」は底本では「かれるので小突《こづか》［＃「かれるので小突《こづか》」はママ］」］" nil 3]
               ["甲［＃「甲」はママ］［＃「甲［＃底本のまま］」は底本では「乙」］"
                "［＃「甲［＃底本のまま］」は底本では「乙」］" 1]
               ["甲［＃「甲」はママ］別［＃「甲［＃「甲」はママ］」は底本では「乙」］"
                "［＃「甲［＃「甲」はママ］」は底本では「乙」］" 1]
               ["〔Hu:lshoff［＃「Hu:lshoff」は底本では「Hu:lshoffs」］〕" nil 1]
               ["〔schla:gt［＃「〔schla:gt〕」は底本では「〔scha:gt〕」］〕" nil 1]
               ["〔Der Mu:s&iggang wird〕［＃「〔Mu:s&iggang〕」は底本では「〔Mu:s&igang〕」］" "〔Der Mu:s&iggang wird〕" 1]
               ["字［＃底本では傍点］" nil 1]
               ["キタ［＃お手伝いさん］" nil 1]
               ["ワフタンゴフ［＃劇場名］" nil 1]
               ["字［＃海野家のお手伝いさん］" "［＃海野家のお手伝いさん］" 0]
               ["字［＃父、太字］" "［＃父、太字］" 0]
               ["字［＃「字」に「ママ」注記］" nil 1]
               ["字［＃「字」に「注」の注記］" nil 1]
               ["三五頁［＃「三五頁」は「須佐の男の神」の「穀物の種」］" nil 1]
               ["井伏鱒二［＃「井伏鱒二」はゴチック］" nil 1]
               ["阿［＃「阿」は一段階小さな文字］" nil 1]
               ["［＃ここから６字下げ、折り返して７字下げ、２１字詰め］\n字\n［＃ここで字下げ終わり］" nil 2]
               ["［＃中見出し］章［＃中見出終わり］" nil 2]]]
        (testing body
          (let [bytes (.getBytes (str "題\n作者\n\n" body "\n\n底本：本\n") "UTF-8")
                source-id (cas/put-bytes! (:cas-dir store) bytes)
                parsed (engine/run-stage! store (stages/parse-stage adapter) {"source" source-id})
                converted (engine/run-stage! store (stages/convert-stage adapter)
                                             {"aat" (get-in parsed [:outputs "aat"])
                                              "work_content_hash" (hash/format-sha256 source-id)})
                scanned (engine/run-stage! store scanner {"source" source-id})
                report (accountability/coverage-report (read-output scanned "source-accountability")
                                                       (read-output converted "parser-ir"))
                occurrences (filterv #(= "body" (get % "region")) (get report "occurrences"))
                claimed (filterv #(seq (get % "claims")) occurrences)]
            (is (= expected-claimed (count claimed)))
            (is (= (if unknown [unknown] [])
                   (mapv #(get % "raw") (filter #(seq (get % "unaccounted_families")) occurrences))))
            (doseq [occurrence claimed claim (get occurrence "claims")]
              (is (contains? (into #{(select-keys (get occurrence "source_span") ["start" "end"])}
                                   (map #(select-keys (get % "source_span") ["start" "end"]))
                                   (get occurrence "components"))
                             (select-keys (get claim "source_span") ["start" "end"])))))))
      (finally (engine/close-store! store) (fs/delete-tree dir)))))

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
            (is (= "not-assessed" (get report "semantic_certification")))
            (when (= "UTF-8" label)
              (testing "independent stage identities and content cutoff through the join"
                (let [parsed-again (run (assoc (stages/parse-stage adapter) :stage-version "cache-probe")
                                        {"source" source-id})
                      converted-again (run (assoc (stages/convert-stage adapter) :stage-version "cache-probe")
                                           {"aat" (get-in parsed-again [:outputs "aat"])
                                            "work_content_hash" (hash/format-sha256 source-id)})
                      oracle-cached (run source-stage {"source" source-id})
                      oracle-again (run (assoc source-stage :stage-version "cache-probe")
                                        {"source" source-id})
                      coverage-inputs {"source-accountability" (get-in oracle-again [:outputs "source-accountability"])
                                       "parser-ir" (get-in converted-again [:outputs "parser-ir"])}
                      coverage-cached (run (accountability/coverage-stage "test-runtime") coverage-inputs)
                      coverage-again (run (assoc (accountability/coverage-stage "test-runtime")
                                                 :stage-version "cache-probe") coverage-inputs)]
                  (is (false? (:cached? parsed-again)))
                  (is (false? (:cached? converted-again)))
                  (is (true? (:cached? oracle-cached)))
                  (is (false? (:cached? oracle-again)))
                  (is (= (:outputs oracle-result) (:outputs oracle-again)))
                  (is (= (:outputs converted) (:outputs converted-again)))
                  (is (true? (:cached? coverage-cached)))
                  (is (false? (:cached? coverage-again)))
                  (is (= (:outputs coverage) (:outputs coverage-again)))
                  (is (true? (:cached? (run source-stage {"source" source-id}))))
                  (is (true? (:cached? (run (stages/parse-stage adapter) {"source" source-id}))))
                  (is (true? (:cached? (run (stages/convert-stage adapter)
                                            {"aat" (get-in parsed [:outputs "aat"])
                                             "work_content_hash" (hash/format-sha256 source-id)}))))))))))
      (finally (engine/close-store! store) (fs/delete-tree dir)))))

(deftest composite-families-require-every-located-component
  (let [component (fn [start end] {"source_span" (span start end) "kind" "CommandFullwidth"
                                   "families" ["kunten.kaeriten"]})
        occurrence {"source_span" (span 0 100) "kind" "RubyExplicit" "region" "body"
                    "families" ["ruby.basic" "kunten.kaeriten"]
                    "components" [(component 10 20) (component 30 40)]}
        ruby {"kind" "ruby" "outcome" "established" "aspects" ["content" "structure"]
              "source_span" (span 0 100)}
        kunten (fn [start end] {"kind" "kunten" "outcome" "established" "aspects" ["content" "structure" "layout"]
                                "source_span" (span start end)})
        report #(accountability/coverage-report (oracle [occurrence]) (interpretation (into [ruby] %)))
        partial (report [(kunten 10 20)])
        complete (report [(kunten 10 20) (kunten 30 40)])]
    (is (= ["kunten.kaeriten"] (get-in partial ["occurrences" 0 "unaccounted_families"])))
    (is (= 1 (get-in partial ["families" "kunten.kaeriten" "unaccounted"])))
    (is (= 1 (count (get-in partial ["occurrences" 0 "components" 0 "claims"]))))
    (is (empty? (get-in complete ["occurrences" 0 "unaccounted_families"])))
    (is (= 1 (get-in complete ["families" "kunten.kaeriten" "interpreter_claimed"])))
    (is (= 1 (count (get complete "occurrences"))))
    (is (= ["kunten.kaeriten"]
           (get-in (report [(kunten 0 100) (kunten 10 20)]) ["occurrences" 0 "unaccounted_families"])))))
