(ns soranoha.ori.source-conformance-test
  (:require [babashka.fs :as fs]
            [charred.api :as json]
            [clojure.string :as string]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [soranoha.annotations.view :as view]
            [soranoha.core.hash :as hash]
            [soranoha.kura.cas :as cas]
            [soranoha.kura.engine :as engine]
            [soranoha.ori.projection :as projection]
            [soranoha.ori.render :as render]
            [soranoha.ori.stages :as stages])
  (:import [org.w3c.dom Document Element Node]))

(def ^:dynamic *store* nil)
(def ^:dynamic *adapter* nil)

(use-fixtures :once
  (fn [run]
    (let [dir (fs/create-temp-dir {:prefix "source-conformance"})
          store (engine/open-store! {:cas-dir (str (fs/path dir "objects"))
                                     :db-path (str (fs/path dir "trace.sqlite"))})]
      (try
        (binding [*store* store *adapter* (stages/resolve-adapter)] (run))
        (finally (engine/close-store! store) (fs/delete-tree dir))))))

(defn- source [body]
  (str "題\n作者\n\n--------------------\n【テキスト中に現れる記号について】\n--------------------\n"
       body "\n\n底本：本\n　　　初刷\n入力：人\n"))

(defn- transcribe
  ([text] (transcribe text "UTF-8"))
  ([^String text encoding]
   (let [bytes (.getBytes text ^String encoding)
         source-id (cas/put-bytes! (:cas-dir *store*) bytes)
         parsed (engine/run-stage! *store* (stages/parse-stage *adapter*) {"source" source-id})
         converted (engine/run-stage! *store* (stages/convert-stage *adapter*)
                                      {"aat" (get-in parsed [:outputs "aat"])
                                       "work_content_hash" (hash/format-sha256 source-id)})
         ir (json/read-json (String. ^bytes (cas/get-bytes (:cas-dir *store*) (get-in converted [:outputs "parser-ir"])) "UTF-8"))
         tei (:tei (render/render-work {:parser-ir ir
                                        :metadata-record {"work" {"work_id" "1" "title" "題" "aozora_modified" "2026-09-07"}
                                                          "contributors" []}
                                        :persons-by-id {}}))
         reading (view/from-tei tei)]
     {:ir ir :tei tei :view reading :plaintext (projection/plaintext reading)})))

(defn- elements [result tag]
  (let [^Document doc (get-in result [:view :view/document])
        nodes (.getElementsByTagNameNS doc view/tei-namespace tag)]
    (mapv #(.item nodes %) (range (.getLength nodes)))))

(defn- attribute [^Element node ^String name] (.getAttribute node name))

(defn- enclosing-style [^Node node style]
  (when node
    (if (and (instance? Element node) (= style (attribute node "style")))
      node
      (recur (.getParentNode node) style))))
(defn- texts [result tag] (mapv view/visible-text (elements result tag)))

(deftest unmapped-witness-glyph-remains-apparatus
  (let [result (transcribe (source "貳朱《にしゅ》を［＃「貳朱を」は底本では「※［＃「弋＋頁」、74-10］朱を」］"))
        witness (first (elements result "rdg"))
        glyph (first (elements result "g"))]
    (is (= "貳朱を" (:plaintext result)))
    (is (= ["貳朱を"] (texts result "lem")))
    (is (= "rdg" (.getLocalName ^Node (.getParentNode ^Node glyph))))
    (is (= "朱を" (.getTextContent ^Node witness)))
    (is (not (string/blank? (attribute glyph "ref"))))
    (is (string/includes? (:tei result) "弋＋頁"))
    (is (seq (get-in result [:view :view/eligible-spans])))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest normalized-and-warichu-variants-preserve-the-principal-stream
  (doseq [[body plain supplied witness]
          [["前〔Annette von Droste=Hu:lshoff［＃「Hu:lshoff」は底本では「Hu:lshoffs」］〕後"
            "前Annette von Droste=Hülshoff後" "Hülshoff" "Hülshoffs"]
           ["（［＃割り注］「前篇」の「五　インド征服」［＃割り注終わり］）［＃「（［＃割り注］「前篇」の「五　インド征服」［＃割り注終わり］）」は底本では「（［＃割り注］五一頁参照［＃割り注終わり］）」］"
            "（「前篇」の「五　インド征服」）" "（「前篇」の「五　インド征服」）" "（五一頁参照）"]]]
    (let [result (transcribe (source body))]
      (is (= plain (:plaintext result)))
      (is (= [supplied] (texts result "lem")))
      (is (= [witness] (texts result "rdg")))
      (is (empty? (get-in result [:ir "interpretation_problems"]))))))

(deftest variant-subranges-preserve-enclosing-ruby-and-emphasis
  (let [result (transcribe (source "蠣崎波響《かきざきはきやう》［＃ルビの「かきざき」は底本では「かきさき」］"))]
    (is (= "蠣崎波響" (:plaintext result)))
    (is (= ["蠣崎波響"] (texts result "rb")))
    (is (= ["かきざきはきやう"] (texts result "rt")))
    (is (= ["かきざき"] (texts result "lem")))
    (is (= ["かきさき"] (texts result "rdg"))))
  (let [result (transcribe (source "非買同盟は不可能である［＃「非買同盟は不可能である」に傍点］［＃「非買同盟は」は底本では「非賣同盟は」］"))
        emphasis (first (elements result "hi"))]
    (is (= "非買同盟は不可能である" (:plaintext result)))
    (is (= ["非買同盟は"] (texts result "lem")))
    (is (= "非買同盟は不可能である" (view/visible-text emphasis)))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest supplied-table-retains-lines-and-literal-separators
  (let [result (transcribe (source "［＃ここから表］\n人口の表\n年次／出生／死亡\n一七五七年／八一八七八／六九〇五四\n［＃ここで表終わり］"))
        table (first (filter #(= "table" (attribute % "type")) (elements result "div")))]
    (is (some? table))
    (is (= ["人口の表" "年次／出生／死亡" "一七五七年／八一八七八／六九〇五四"] (texts result "p")))
    (is (empty? (elements result "cell")))
    (is (= "人口の表\n年次／出生／死亡\n一七五七年／八一八七八／六九〇五四" (:plaintext result)))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest table-scope-alias-preserves-literal-lines
  (let [result (transcribe (source "［＃ここから表組］\nbeat\tbutu\nlaugh\twalahu\n［＃ここで表組終わり］"))
        table (first (filter #(= "table" (attribute % "type")) (elements result "div")))]
    (is (some? table))
    (is (= ["beat\tbutu" "laugh\twalahu"] (texts result "p")))
    (is (= "beat\tbutu\nlaugh\twalahu" (:plaintext result)))
    (is (= "beat\tbutu\n\nlaugh\twalahu" (projection/markdown (:view result))))
    (is (empty? (elements result "cell")))
    (is (empty? (get-in result [:ir "interpretation_problems"]))))
  (doseq [body ["［＃ここから表組］\n甲"
                "［＃ここから表組、不明］\n甲\n［＃ここで表組終わり］"]]
    (let [result (transcribe (source body))]
      (is (not-any? #(= "table" (attribute % "type")) (elements result "div")))
      (is (seq (get-in result [:ir "interpretation_problems"]))))))

(deftest supplied-columns-retain-only-explicit-column-breaks
  (doseq [[body count text] [["甲乙" 0 "甲乙"] ["甲［＃改段］乙" 1 "甲\n乙"]]]
    (let [result (transcribe (source (str "［＃ここから２段組み］\n" body "\n［＃ここで段組み終わり］")))]
      (is (= count (clojure.core/count (elements result "cb"))))
      (is (some #(= "column-count: 2" (attribute % "style")) (elements result "div")))
      (is (= [text] (texts result "p")))
      (is (= text (:plaintext result)))
      (is (empty? (get-in result [:ir "interpretation_problems"]))))))

(deftest supplied-inline-layout-retains-visible-target-without-invented-reading
  (doseq [[body text rend] [["花［＃「花」は罫囲み］" "花" "keigakomi border(rule)"]
                            ["обед［＃「обед」は横組み］" "обед" "yokogumi horizontal"]
                            ["1/4πDt［＃「1/4πDt」は分数］" "1/4πDt" "fraction"]]]
    (let [result (transcribe (source body))]
      (is (= text (:plaintext result)))
      (is (some #(= rend (attribute % "rend")) (elements result "hi")))
      (is (empty? (get-in result [:ir "interpretation_problems"])))
      (is (= ["layout"] (mapv #(get % "kind") (get-in result [:ir "interpretation_facts"])))))))

(deftest partial-paragraph-typography-retains-source-paragraphs
  (let [result (transcribe (source "　［＃ここから斜体］First speech.\n\n　Second speech.［＃ここで斜体終わり］＊"))
        scopes (filterv #(= "italic" (attribute % "rend")) (elements result "hi"))]
    (is (= "　First speech.\n　Second speech.＊" (:plaintext result)))
    (is (= ["First speech." "Second speech.＊"] (texts result "p")))
    (is (= ["First speech." "Second speech."] (mapv view/visible-text scopes)))
    (is (= 1 (count (set (map #(attribute % "source") scopes)))))
    (is (= 1 (count (get-in result [:ir "layout_blocks"]))))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest partial-typography-retains-a-normal-heading
  (let [result (transcribe (source "前［＃ここから斜体］甲\n［＃中見出し］章［＃中見出し終わり］後\n乙［＃ここで斜体終わり］外"))
        heading (first (elements result "head"))]
    (is (= ["章"] (texts result "head")))
    (is (= "2" (attribute heading "n")))
    (is (= ["前甲" "後" "乙外"] (texts result "p")))
    (is (= ["甲" "章" "後" "乙"]
           (mapv view/visible-text (filter #(= "italic" (attribute % "rend")) (elements result "hi")))))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest typography-scope-retains-a-normal-heading-between-source-paragraphs
  (let [result (transcribe (source (str "前\n［＃ここから１段階小さな文字］\n"
                                        "第一段落\n［＃中見出し］章《しょう》［＃中見出し終わり］\n"
                                        "第二段落\n［＃ここで小さな文字終わり］\n後")))
        heading (first (elements result "head"))
        parent (.getParentNode ^Node heading)
        scope (.getParentNode ^Node parent)]
    (is (= "前\n第一段落\n章\n第二段落\n後" (:plaintext result)))
    (is (= "章" (view/visible-text heading)))
    (is (= "2" (attribute heading "n")))
    (is (= "layout" (attribute scope "type")))
    (is (= "font-size small(1)" (attribute scope "rend")))
    (is (= ["章"] (texts result "rb")))
    (is (= ["しょう"] (texts result "rt")))))

(deftest heading-regions-preserve-presentation-and-rich-reading
  (doseq [[prefix style] [["同行" "dogyo"] ["窓" "mado"]]]
    (let [result (transcribe (source (str "前［＃" prefix "中見出し］漢字《かんじ》［＃"
                                          prefix "中見出し終わり］後")))
          heading (first (filter #(= "heading" (attribute % "type")) (elements result "seg")))]
      (is (= "前漢字後" (:plaintext result)))
      (is (= style (attribute heading "rend")))
      (is (= "2" (attribute heading "n")))
      (is (= ["漢字"] (texts result "rb")))
      (is (= ["かんじ"] (texts result "rt")))
      (is (empty? (elements result "head")))
      (is (= 2 (count (filter #(= "heading" (get % "kind"))
                              (get-in result [:ir "interpretation_facts"]))))))))

(deftest gaiji-ruby-indentation-and-apparatus-retain-independent-values
  (doseq [encoding ["UTF-8" "windows-31j"]]
    (let [result (transcribe (source (str "［＃８字下げ］一［＃「一」は中見出し］\n\n"
                                          "　池《いけ》の　底に、※［＃「特のへん＋廴＋聿」、第3水準1-87-71］陀多《かんだた》。")) encoding)]
      (is (= "一\n　池の　底に、犍陀多。" (:plaintext result)))
      (is (= ["池" "犍陀多"] (texts result "rb")))
      (is (= ["いけ" "かんだた"] (texts result "rt")))
      (is (= ["犍"] (texts result "g")))
      (is (some #{"犍"} (texts result "mapping")))
      (is (= "2" (attribute (first (elements result "head")) "n")))
      (is (some? (enclosing-style (first (elements result "head")) "padding-inline-start: 8em")))
      (is (= "text-indent: 1em" (attribute (first (filter #(string/starts-with? (view/visible-text %) "池") (elements result "p"))) "style")))
      (let [lines (filter #(= "source-line" (attribute % "type")) (elements result "seg"))]
        (is (= ["底本：本" "初刷" "入力：人"] (mapv #(.getTextContent ^Node %) lines)))
        (is (= "padding-inline-start: 3em" (attribute (second lines) "style")))))))

(deftest glyph-realizations-cover-both-jis-planes
  (doseq [[notation expected]
          [["「てへん＋丑」、第4水準2-12-93" "扭"]
           ["「にんべん＋參」、第4水準2-1-79" "傪"]
           ["「口＋「皐」の「白」にかえて「自」、第4水準2-4-33" "嘷"]
           ["「やまいだれ＋低のつくり」、第4水準2-81-42" "疷"]
           ["「言＋墟のつくり」、第4水準2-88-74" "譃"]
           ["「鹵」、1-83-35" "鹵"]
           ["二の字点、1-2-22" "〻"]]]
    (testing notation
      (let [result (transcribe (source (str "※［＃" notation "］")))]
        (is (= expected (:plaintext result)))
        (is (= [expected] (texts result "g")))
        (is (some #{expected} (texts result "mapping")))))))

(defn- within? [^Node parent ^Node node]
  (when node
    (or (identical? parent node) (recur parent (.getParentNode node)))))

(defn- reading-start [result element]
  (:view/start (first (filter #(within? element (:view/node %)) (get-in result [:view :view/segments])))))

(deftest retrospective-targets-do-not-move-or-duplicate-reading-content
  (doseq [[body expected emphasized offset]
          [["しだ、しだ［＃「しだ」に傍点］。" "しだ、しだ。" "しだ" 9]
           ["池《いけ》［＃「池」に傍点］" "池" "池" 0]
           ["牛《ベゴ》の舌［＃「牛の舌」に傍点］" "牛の舌" "牛の舌" 0]]]
    (let [result (transcribe (source body))]
      (is (= expected (:plaintext result)))
      (is (= [emphasized] (texts result "hi")))
      (is (= offset (reading-start result (first (elements result "hi")))))
      (is (= ["bouten 傍点 right"] (mapv #(attribute % "rend") (elements result "hi")))))))

(deftest source-boundaries-and-accent-punctuation-have-fixed-readings
  (doseq [[body expected]
          [["≪外≪内≫後≫。" "《外《内》後》。"]
           ["≪池《いけ》≫、外。≪未閉。" "《池》、外。≪未閉。"]
           ["〔C'est me^me〕" "C'est même"]
           ["〔LE MAC,ON〕" "LE MAÇON"]
           ["｜字《〔C'est〕》" "字"]]]
    (is (= expected (:plaintext (transcribe (source body)))) body))
  (let [result (transcribe "こころ\n今野大力\n\nこころ　こころ\nくるしいこころ\n\n底本：本\n")]
    (is (= "こころ　こころ\nくるしいこころ" (:plaintext result)))))

(deftest unfinished-accent-scopes-do-not-change-later-paragraphs
  (doseq [encoding ["UTF-8" "windows-31j"]]
    (let [result (transcribe
                  (source (str "註二　〔Antonelli: Le'on Walras\n"
                               "一　この書物は、〔Le'on Walras: Ele'ments d'e'conomie politique pure.〕 を訳出した。"))
                  encoding)]
      (is (= "註二　〔Antonelli: Le'on Walras\n一　この書物は、Léon Walras: Eléments d'économie politique pure. を訳出した。"
             (:plaintext result)))
      (is (some #{"一　この書物は、Léon Walras: Eléments d'économie politique pure. を訳出した。"}
                (texts result "p"))))))

(deftest editorial-quotations-have-independent-accent-scopes
  (doseq [encoding ["UTF-8" "windows-31j"]
          [body expected]
          [["〔amicitiae&［＃「〔amicitiae&〕」は底本では「amiticiae」］ inimica〕" "amicitiæ inimica"]
           ["〔schla:gt［＃「〔schla:gt〕」は底本では「〔scha:gt〕」］ noch〕" "schlägt noch"]]]
    (let [result (transcribe (source body) encoding)]
      (is (= expected (:plaintext result)))
      (is (some #{expected} (texts result "p"))))))

(deftest explicit-body-end-keeps-translation-apparatus-outside-reading-text
  (let [result (transcribe "作品\n作者\n\n本文\n\n［＃本文終わり］\n翻訳の底本：原書\n※利用条件\n翻訳者：訳者\n")]
    (is (= "本文" (:plaintext result)))
    (is (= "本文" (projection/markdown (:view result))))
    (is (= ["本文"] (texts result "p")))
    (is (= ["［＃本文終わり］" "翻訳の底本：原書" "※利用条件" "翻訳者：訳者"]
           (mapv view/visible-text
                 (filter #(= "source-line" (attribute % "type")) (elements result "seg")))))))

(deftest unsupported-multiline-accents-retain-text-and-mark-analysis-uncertain
  (let [body "〔Pardonnez a` mon bavardage\nA line without accent decomposition.〕"
        result (transcribe (source body))
        problems (get-in result [:view :view/problems])]
    (is (= body (:plaintext result)))
    (is (some #(= body (get-in % [:view/evidence "raw"])) problems))
    (is (empty? (get-in result [:view :view/eligible-spans])))))

(deftest malformed-notation-retains-visible-neighbors-and-uncertainty
  (doseq [[body expected raw]
          [["前〔cafe'〕［＃tail\r\n" "前café" "［＃tail"]
           ["前※後" "前後" "※"]
           ["前［＃未閉\n後" "前\n後" "［＃未閉"]]]
    (let [result (transcribe (source body))
          problem (first (filter #(= raw (get-in % [:view/evidence "raw"]))
                                 (get-in result [:view :view/problems])))]
      (is (= expected (:plaintext result)))
      (is (some? problem))
      (is (= "document" (get-in problem [:view/evidence "influence" "kind"]))))))

(deftest enclosing-layout-and-closing-alignment-are-independent
  (let [result (transcribe (source "前。\n［＃ここから２字下げ］\n附記。\n［＃地から２字上げ］（大正四年八月）\n［＃ここで字下げ終わり］\n後。"))
        enclosure (first (filter #(string/includes? (attribute % "style") "padding-inline-start: 2em")
                                 (elements result "div")))]
    (is (= "前。\n附記。\n（大正四年八月）\n後。" (:plaintext result)))
    (is (= "附記。\n（大正四年八月）" (view/visible-text enclosure)))
    (let [date (first (filter #(= "（大正四年八月）" (view/visible-text %)) (elements result "p")))]
      (is (some? (enclosing-style date "padding-inline-end: 2em; text-align: right"))))))

(deftest inline-scopes-preserve-enclosing-paragraph-layout
  (doseq [[opening rendition body expected]
          [["［＃ここから１字下げ］" "padding-inline-start: 1em"
            "ビタミン［＃縦中横］B1［＃「1」は下付き小文字］［＃縦中横終わり］　二ミリグラム"
            "ビタミンB1　二ミリグラム"]
           ["［＃ここから改行天付き、折り返して１字下げ］" "padding-inline-start: 1em; text-indent: -1em"
            "（［＃縦中横］10［＃縦中横終わり］）注。" "（10）注。"]]]
    (let [result (transcribe (source (str opening "\n" body "\n次の行。\n［＃ここで字下げ終わり］")))
          paragraph (first (filter #(= expected (view/visible-text %)) (elements result "p")))
          following (first (filter #(= "次の行。" (view/visible-text %)) (elements result "p")))
          tcy (first (filter #(= "text-combine-upright" (attribute % "rend")) (elements result "hi")))]
      (is (= (str expected "\n次の行。") (:plaintext result)))
      (is (some? paragraph))
      (when paragraph
        (is (some? (enclosing-style paragraph rendition)))
        (is (within? paragraph tcy)))
      (is (some? following))
      (when following
        (is (some? (enclosing-style following rendition)))))))

(deftest inline-font-scope-retains-leading-indentation
  (let [result (transcribe (source (str "［＃ここから改行天付き、折り返して１字下げ］\n"
                                        "　［＃１段階小さな文字］〔中略〕［＃小さな文字終わり］\n"
                                        "次の行。\n［＃ここで字下げ終わり］")))
        paragraph (first (filter #(= "〔中略〕" (view/visible-text %)) (elements result "p")))
        font (first (filter #(= "font-size small(1)" (attribute % "rend")) (elements result "hi")))]
    (is (= "　〔中略〕\n次の行。" (:plaintext result)))
    (is (some? paragraph))
    (is (some? font))
    (when paragraph
      (is (= "first-line-indent(1)" (attribute paragraph "rend")))
      (is (= "text-indent: 0em" (attribute paragraph "style")))
      (is (within? paragraph font)))))

(deftest supplied-ruby-variant-is-not-an-asserted-source-error
  (let [result (transcribe (source "私は籠《ざる》［＃ルビの「ざる」は底本では「さる」］をさげ"))]
    (is (= "私は籠をさげ" (:plaintext result)))
    (is (= ["ざる"] (texts result "lem")))
    (is (= ["さる"] (texts result "rdg")))
    (is (= "rt" (view/local-name (.getParentNode ^Node (first (elements result "app"))))))
    (is (empty? (elements result "sic")))))

(deftest supplied-principal-variants-retain-the-reading-and-rich-target
  (doseq [[body plain lemma alternative]
          [["積る甍の［＃「甍の」は底本では「薨の」］雪。" "積る甍の雪。" "甍の" "薨の"]
           ["前｜東京《とうきょう》の町［＃「東京の町」は底本では「東亰の町」］後。"
            "前東京の町後。" "東京の町" "東亰の町"]]]
    (let [result (transcribe (source body))]
      (is (= plain (:plaintext result)))
      (is (= [lemma] (texts result "lem")))
      (is (= [alternative] (texts result "rdg")))
      (is (empty? (elements result "sic")))
      (is (empty? (get-in result [:view :view/problems])))
      (is (not (string/includes? (projection/markdown (:view result)) alternative)))
      (when (string/includes? body "《")
        (is (= ["とうきょう"] (texts result "rt")))
        (is (within? (first (elements result "lem")) (first (elements result "ruby"))))))))

(deftest unknown-notation-retains-evidence-without-certifying-its-neighbours
  (let [raw "［＃未定義の範囲指定開始］"
        result (transcribe (source (str "前" raw "後")))
        problems (get-in result [:view :view/problems])]
    (is (= "前後" (:plaintext result)))
    (is (= [] (get-in result [:view :view/eligible-spans])))
    (is (= [raw] (mapv #(get-in % [:view/evidence "raw"]) problems)))
    (is (= [{"kind" "document"}] (mapv #(get-in % [:view/evidence "influence"]) problems)))))

(deftest literal-brackets-preserve-body-and-nested-notation
  (let [result (transcribe (source "前［※［＃下側の右ダブル引用符、U+201E］思想。］後。"))]
    (is (= "前［„思想。］後。" (:plaintext result)))
    (is (= ["„"] (texts result "g")))
    (is (empty? (get-in result [:view :view/problems]))))
  (let [marker "［＃未定義の指定］"
        result (transcribe (source (str "前［中" marker "後］末。")))]
    (is (= "前［中後］末。" (:plaintext result)))
    (is (= [marker] (mapv #(get-in % [:view/evidence "raw"])
                          (get-in result [:view :view/problems]))))))

(deftest scoped-upright-text-retains-rich-content
  (let [result (transcribe (source "前［＃縦中横］（※［＃ローマ数字1、1-13-21］）［＃縦中横終わり］後。"))
        upright (first (filter #(= "text-combine-upright" (attribute % "rend")) (elements result "hi")))]
    (is (= "前（Ⅰ）後。" (:plaintext result)))
    (is (= "（Ⅰ）" (some-> upright view/visible-text)))
    (is (within? upright (first (elements result "g"))))
    (is (empty? (get-in result [:view :view/problems])))))

(deftest source-font-and-script-distinctions-survive-transcription
  (doseq [[notation rendition]
          [["ゴシック体" "gothic"] ["斜体" "italic"]
           ["上付き小文字" "superscript"] ["下付き小文字" "subscript"]
           ["２段階小さな文字" "font-size small(2)"] ["小文字" "font-size absolute(small)"]
           ["行右小書き" "small-script right"] ["行左小書き" "small-script left"]]]
    (testing notation
      (let [result (transcribe (source (str "前字［＃「字」は" notation "］後。")))]
        (is (= "前字後。" (:plaintext result)))
        (is (= ["字"] (texts result "hi")))
        (is (= [rendition] (mapv #(attribute % "rend") (elements result "hi"))))
        (is (empty? (get-in result [:view :view/problems])))))))

(deftest warichu-does-not-invent-upper-and-lower-readings
  (let [result (transcribe (source "［＃ここから２字下げ］\n前［＃割り注］上。※［＃歌記号、1-3-28］下。［＃割り注終わり］後\n［＃ここで字下げ終わり］"))
        wrapper (first (filter #(= "warichu" (attribute % "type")) (elements result "seg")))]
    (is (= "前上。〽下。後" (:plaintext result)))
    (is (= "上。〽下。" (view/visible-text wrapper)))
    (is (= "two-line" (attribute wrapper "rend")))
    (is (empty? (elements result "s")))
    (is (some? (enclosing-style wrapper "padding-inline-start: 2em")))
    (is (empty? (filter #(#{"upper" "lower"} (attribute % "type")) (elements result "seg"))))
    (is (empty? (get-in result [:view :view/problems])))))

(deftest gaiji-membership-is-defined-by-source-ruby-boundaries
  (doseq [[marker glyph prefix suffix reading]
          [["歌記号、1-3-28" "〽" "" "銚子" "ちょうし"]
           ["全角CC、1-13-53" "㏄" "二〇" "入" "いり"]
           ["始め二重括弧、1-2-54" "｟" "" "誰" "た"]
           ["ます記号、1-2-23" "〼" "" "定" "ますさだ"]]]
    (let [result (transcribe (source (str prefix "※［＃" marker "］" suffix "《" reading "》")))]
      (is (= [(str prefix glyph suffix)] (texts result "rb")))
      (is (= [reading] (texts result "rt")))))
  (doseq [body ["〽銚子《ちょうし》" "※［＃歌記号、1-3-28］｜銚子《ちょうし》"]]
    (let [result (transcribe (source body))]
      (is (= "〽銚子" (:plaintext result)))
      (is (= ["銚子"] (texts result "rb"))))))

(deftest sign-layout-retains-content-and-property-scope
  (let [result (transcribe (source "［＃ここから４字下げ、横書き、中央揃え、罫囲み］\nRESTAURANT\n西洋料理店\n［＃ここで字下げ終わり］\nといふ札。"))
        sign (first (filter #(= "keigakomi border(rule)" (attribute % "rend")) (elements result "div")))]
    (is (= "RESTAURANT\n西洋料理店\nといふ札。" (:plaintext result)))
    (is (= "RESTAURANT\n西洋料理店" (view/visible-text sign)))
    (is (= "padding-inline-start: 4em; writing-mode: horizontal-tb; text-align: center; border-style: solid"
           (attribute sign "style")))))

(deftest transcription-statements-do-not-insert-additional-line-breaks
  (doseq [separator ["\n" "\r\n" ""]]
    (let [result (transcribe (source (str "甲［＃改行を挿入］" separator "乙")))
          note (first (filter #(= "transcription" (attribute % "type")) (elements result "note")))]
      (is (= (if (empty? separator) "甲乙" "甲\n乙") (:plaintext result)))
      (is (= (if (empty? separator) "甲乙" "甲\n\n乙") (projection/markdown (:view result))))
      (is (= "改行を挿入" (.getTextContent ^Node note)))
      (is (= 1 (count (filter #(= "editorial-note" (get % "kind"))
                              (get-in result [:ir "interpretation_facts"])))))
      (is (empty? (filter #(= "layout-break" (get % "kind"))
                          (get-in result [:ir "interpretation_facts"])))))))

(deftest supplied-omission-and-incompleteness-remain-source-statements
  (doseq [[statement kind] [["「註」略" "omission"] ["未完" "incompleteness"]
                            ["図が入るが省略。底本43ページ" "omission"]
                            ["この後、改ページに続いて「VI.　文例」の章があるが、著作権の状態が不明なため、省略する。" "omission"]]]
    (let [marker (str "［＃" statement "］")
          text (source (str "前\n" marker "\n後"))
          result (transcribe text)
          note (first (filter #(= kind (attribute % "type")) (elements result "note")))
          fact (first (filter #(= "editorial-note" (get % "kind"))
                              (get-in result [:ir "interpretation_facts"])))
          start (alength (.getBytes (subs text 0 (string/index-of text marker)) "UTF-8"))]
      (is (= "前\n後" (:plaintext result)))
      (is (= "前\n\n\n\n後" (projection/markdown (:view result))))
      (is (= statement (.getTextContent ^Node note)))
      (is (= start (get-in fact ["source_span" "start"])))
      (is (= (+ start (alength (.getBytes marker "UTF-8")))
             (get-in fact ["source_span" "end"])))
      (is (empty? (elements result "gap")))
      (is (empty? (get-in result [:ir "interpretation_problems"]))))))

(deftest supplied-body-absence-declarations-remain-explanations
  (doseq [[body statement expected]
          [["" "この作品は表題と副題のみで、本文はありません。" ""]
           ["かきくらす涙か雲かしらねどもひかり\n見せねばかかぬ一章　　　　（晶子）"
            "「雲隠れ」の帖は冒頭の晶子詞のみで本文はありません。"
            "かきくらす涙か雲かしらねどもひかり\n見せねばかかぬ一章　　　　（晶子）"]]]
    (let [marker (str "［＃" statement "］")
          text (source (str body "\n" marker))
          result (transcribe text)
          notes (filter #(= "explanation" (attribute % "type")) (elements result "note"))
          fact (first (filter #(= "editorial-note" (get % "kind")) (get-in result [:ir "interpretation_facts"])))
          start (alength (.getBytes (subs text 0 (string/index-of text marker)) "UTF-8"))]
      (is (= expected (:plaintext result)))
      (is (= [statement] (mapv #(.getTextContent ^Node %) notes)))
      (is (= start (get-in fact ["source_span" "start"])))
      (is (= (+ start (alength (.getBytes marker "UTF-8"))) (get-in fact ["source_span" "end"])))
      (is (empty? (elements result "gap")))
      (is (empty? (get-in result [:ir "interpretation_problems"]))))))

(deftest supplied-labels-remain-explanations-without-inferred-identities
  (doseq [label ["劇場名" "ホテル名" "お手伝いさん" "夫人" "スカーフ"
                 "長男" "三男" "次男" "小説家" "長女" "父" "母" "甥" "次女"]]
    (let [result (transcribe (source (str "名［＃" label "］後")))
          notes (filter #(= "explanation" (attribute % "type")) (elements result "note"))]
      (is (= "名後" (:plaintext result)))
      (is (= "名後" (projection/markdown (:view result))))
      (is (= [label] (mapv #(.getTextContent ^Node %) notes)))
      (is (every? #(empty? (attribute % "target")) notes))
      (is (empty? (elements result "persName")))
      (is (empty? (get-in result [:ir "interpretation_problems"]))))))

(deftest quoted-variants-do-not-create-body-ruby-or-gaiji
  (doseq [[body plain expected-ruby raw]
          [["煖爐《ストーブ》には［＃「煖爐《ストーブ》には」は底本では「煖燼《ストーブ》には」］、後。"
            "煖爐には、後。" ["煖爐"] nil]
           ["目［＃「※［＃「目＋旬」、第3水準1-88-80］《めくば》せを」は底本では「※［＃「目＋句」、第4水準2-81-91］《めくば》せを」］後。"
            "目後。" [] "「※［＃「目＋旬」、第3水準1-88-80］《めくば》せを」は底本では「※［＃「目＋句」、第4水準2-81-91］《めくば》せを」"]]]
    (let [result (transcribe (source body))
          notes (filter #(#{"variant" "misc"} (attribute % "type")) (elements result "note"))]
      (is (= plain (:plaintext result)))
      (is (= expected-ruby
             (mapv view/visible-text
                   (remove (fn [node]
                             (loop [parent (.getParentNode ^Node node)]
                               (cond (nil? parent) false
                                     (= "rdg" (.getLocalName ^Node parent)) true
                                     :else (recur (.getParentNode ^Node parent)))))
                           (elements result "rb")))))
      (is (empty? (elements result "g")))
      (is (= (if raw [(str "［＃" raw "］")] []) (mapv #(.getTextContent ^Node %) notes)))
      (is (= (if raw [] ["煖燼には"]) (texts result "rdg"))))))

(deftest shared-left-underline-preserves-ruby-and-decoration
  (let [result (transcribe (source "青空文庫《あおぞらぶんこ》［＃「青空文庫」の左に傍線］"))
        emphasis (first (elements result "hi"))]
    (is (= "青空文庫" (:plaintext result)))
    (is (= ["青空文庫"] (texts result "rb")))
    (is (= ["あおぞらぶんこ"] (texts result "rt")))
    (is (= "bosen 傍線 left" (attribute emphasis "rend")))
    (is (within? emphasis (first (elements result "ruby"))))))

(deftest unresolved-base-target-retains-exact-note-at-its-source-position
  (let [result (transcribe (source "甍《いらか》［＃「甍の」は底本では「薨の」］先。明《あ》［＃ルビの「あ」は底本では「あか」］かさう。"))
        ^Node note (first (filter #(= "variant" (attribute % "type")) (elements result "note")))
        following (first (filter #(pos? (bit-and Node/DOCUMENT_POSITION_FOLLOWING
                                                 (.compareDocumentPosition note ^Node (:view/node %))))
                                 (get-in result [:view :view/segments])))]
    (is (= "甍先。明かさう。" (:plaintext result)))
    (is (= "［＃「甍の」は底本では「薨の」］" (.getTextContent note)))
    (is (= "unresolved" (attribute note "subtype")))
    (is (= 3 (:view/start following)))
    (is (= ["あ"] (texts result "lem")))
    (is (= ["あか"] (texts result "rdg")))))

(deftest colophon-explanations-credits-and-accents-remain-apparatus
  (let [explanation "※「□」には、底本では「◆」が内接しています。"
        result (transcribe (str (source "本文。") explanation "\n　　　〔DIE FLU:CHTLINGE〕 〔本全集〕\n"))
        lines (filter #(= "source-line" (attribute % "type")) (elements result "seg"))
        accent (first (filter #(= "DIE FLÜCHTLINGE 〔本全集〕" (.getTextContent ^Node %)) lines))]
    (is (= "本文。" (:plaintext result)))
    (is (some #{explanation} (map #(.getTextContent ^Node %) lines)))
    (is (some #{"入力：人"} (map #(.getTextContent ^Node %) lines)))
    (is (= "padding-inline-start: 3em" (attribute accent "style")))))

(deftest nested-warichu-preserves-forced-break-and-supplied-kunten
  (let [result (transcribe (source "［＃地から３字上げ］［＃割り注］磯。此云［＃レ］志。［＃改行］次［＃割り注終わり］後。"))
        wrapper (first (filter #(= "warichu" (attribute % "type")) (elements result "seg")))
        note (first (filter #(= "kunten" (attribute % "type")) (elements result "note")))
        problems (get-in result [:view :view/problems])]
    (is (= "磯。此云志。\n次後。" (:plaintext result)))
    (is (= "磯。此云志。  \n次後。" (projection/markdown (:view result))))
    (is (= "磯。此云志。\n次" (view/visible-text wrapper)))
    (is (= 1 (count (filter #(within? wrapper %) (elements result "lb")))))
    (is (within? wrapper note))
    (is (= "return-mark" (attribute note "subtype")))
    (is (empty? problems))
    (is (seq (get-in result [:view :view/eligible-spans])))))

(deftest supplied-kunten-is-preserved-outside-principal-text
  (doseq [encoding ["UTF-8" "windows-31j"]]
    (let [result (transcribe (source "漢［＃レ］字［＃（レ）］給［＃（弖）］［＃一レ］。") encoding)
          notes (filterv #(= "kunten" (attribute % "type")) (elements result "note"))]
      (is (= "漢字給。" (:plaintext result)))
      (is (= ["レ" "レ" "弖" "一レ"] (mapv #(.getTextContent ^Node %) notes)))
      (is (= ["return-mark" "okurigana" "okurigana" "return-mark"] (mapv #(attribute % "subtype") notes)))
      (is (= ["subscript" "superscript" "superscript" "subscript"] (mapv #(attribute % "rend") notes)))
      (is (empty? (get-in result [:ir "interpretation_problems"]))))))

(deftest retrospective-return-marks-retain-apparatus-without-duplicating-targets
  (doseq [encoding ["UTF-8" "windows-31j"]]
    (let [result (transcribe (source "飾レ［＃「レ」は返り点］中舍レ［＃「レ」は返り点］執二［＃「二」は返り点］其禮事一［＃「一」は返り点］。") encoding)
          notes (filterv #(= "kunten" (attribute % "type")) (elements result "note"))
          facts (filter #(= "kunten" (get % "kind")) (get-in result [:ir "interpretation_facts"]))]
      (is (= "飾中舍執其禮事。" (:plaintext result)))
      (is (= ["レ" "レ" "二" "一"] (mapv #(.getTextContent ^Node %) notes)))
      (is (every? #(= "return-mark" (attribute % "subtype")) notes))
      (is (= 4 (count facts)))
      (is (empty? (get-in result [:ir "interpretation_problems"])))))
  (let [result (transcribe (source "｜漢レ［＃「レ」は返り点］字《かんじ》。"))]
    (is (= "漢字。" (:plaintext result)))
    (is (= ["かんじ"] (texts result "rt")))
    (is (= ["レ"] (mapv #(.getTextContent ^Node %)
                       (filter #(= "kunten" (attribute % "type")) (elements result "note"))))))
  (doseq [body ["漢［＃「レ」は返り点］" "漢レ別［＃「レ」は返り点］" "漢レ［＃「一」は返り点］"]]
    (let [result (transcribe (source body))]
      (is (empty? (filter #(= "kunten" (attribute % "type")) (elements result "note"))))
      (is (seq (get-in result [:ir "interpretation_problems"]))))))

(deftest paired-inline-scopes-retain-text-and-both-source-markers
  (let [result (transcribe (source "前［＃斜体］本文［＃斜体終わり］後"))
        facts (filter #(= "emphasis" (get % "kind")) (get-in result [:ir "interpretation_facts"]))]
    (is (= "前本文後" (:plaintext result)))
    (is (= ["本文"] (texts result "hi")))
    (is (= "italic" (attribute (first (elements result "hi")) "rend")))
    (is (= 2 (count facts)))))

(deftest compound-formatting-applies-independent-attributes-to-one-target
  (let [result (transcribe (source "前１）［＃「１）」は縦中横、行右小書き］後。"))
        spans (elements result "hi")]
    (is (= "前１）後。" (:plaintext result)))
    (is (= ["１）"] (texts result "hi")))
    (is (= "text-combine-upright small-script right" (attribute (first spans) "rend")))))

(deftest indentation-closer-aliases-preserve-the-following-paragraph
  (doseq [close ["［＃字下げ終わり］" "［＃ここで字下げおわり］"]]
    (let [result (transcribe (source (str "［＃ここから３字下げ］\n本文\n" close "\n後")))
          scope (first (filter #(= "padding-inline-start: 3em" (attribute % "style"))
                               (elements result "div")))]
      (is (some? scope))
      (is (= ["本文"] (mapv view/visible-text (filter #(within? scope %) (elements result "p")))))
      (is (= "本文\n後" (:plaintext result)))
      (is (empty? (get-in result [:ir "interpretation_problems"]))))))

(deftest compound-layout-keeps-page-placement-independent-of-line-alignment
  (doseq [[clauses style placement]
          [["地より１字上げ" "padding-inline-start: 3em; padding-inline-end: 1em" nil]
           ["横組み右揃えで" "padding-inline-start: 3em; writing-mode: horizontal-tb; text-align: right" nil]
           ["ページの左右中央、中央揃え" "padding-inline-start: 3em; text-align: center" "page-horizontal-center"]
           ["ページの左右中央" "padding-inline-start: 3em" "page-horizontal-center"]]]
    (let [result (transcribe (source (str "［＃ここから３字下げ、" clauses "］\n本文\n［＃ここで字下げ終わり］\n後")))
          scope (first (filter #(= style (attribute % "style")) (elements result "div")))]
      (is (some? scope))
      (is (= (or placement "") (attribute scope "rend")))
      (is (= "本文\n後" (:plaintext result)))
      (is (empty? (get-in result [:ir "interpretation_problems"]))))))

(deftest multiline-typography-preserves-source-paragraph-boundaries
  (let [result (transcribe (source "前。\n［＃ここから１段階小さな文字］\n第一。\n第二。\n［＃ここで小さな文字終わり］\n後。"))
        wrapper (first (filter #(= "font-size small(1)" (attribute % "rend")) (elements result "div")))
        paragraphs (filter #(within? wrapper %) (elements result "p"))]
    (is (= "前。\n第一。\n第二。\n後。" (:plaintext result)))
    (is (= ["第一。" "第二。"] (mapv #(.getTextContent ^Node %) paragraphs)))
    (is (= 2 (count (filter #(= "emphasis" (get % "kind")) (get-in result [:ir "interpretation_facts"])))))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest captions-and-generic-warichu-retain-inline-content-and-role
  (doseq [[open close role]
          [["［＃キャプション］" "［＃キャプション終わり］" "caption"]
           ["［＃ここから割り注］" "［＃ここで割り注終わり］" "warichu"]]]
    (let [result (transcribe (source (str "前" open "漢字《かんじ》" close "後。")))
          wrapper (first (filter #(= role (attribute % "type")) (elements result "seg")))]
      (is (= "前漢字後。" (:plaintext result)))
      (is (= "漢字" (view/visible-text wrapper)))
      (is (within? wrapper (first (elements result "ruby"))))
      (is (empty? (elements result "figure")))
      (is (empty? (get-in result [:ir "interpretation_problems"]))))))

(deftest generic-warichu-accepts-the-bare-closing-marker
  (let [result (transcribe (source "前［＃ここから割り注］隱五年［＃割り注終わり］後"))
        wrapper (first (filter #(= "warichu" (attribute % "type")) (elements result "seg")))]
    (is (= "隱五年" (some-> wrapper view/visible-text)))
    (is (= "前隱五年後" (:plaintext result)))
    (is (= "前隱五年後" (projection/markdown (:view result))))
    (is (= 2 (count (filter #(= "warichu" (get % "kind"))
                            (get-in result [:ir "interpretation_facts"])))))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest multiline-caption-and-warichu-preserve-source-paragraphs
  (doseq [[name role] [["キャプション" "caption"] ["割り注" "warichu"]]]
    (let [result (transcribe (source (str "前。\n［＃ここから" name "］\n第一。\n第二。\n［＃ここで" name "終わり］\n後。")))
          wrapper (first (filter #(= role (attribute % "type")) (elements result "div")))]
      (is (= "前。\n第一。\n第二。\n後。" (:plaintext result)))
      (is (= ["第一。" "第二。"]
             (mapv #(.getTextContent ^Node %) (filter #(within? wrapper %) (elements result "p")))))
      (is (empty? (get-in result [:ir "interpretation_problems"]))))))

(deftest caption-markers-can-share-the-first-and-last-source-lines
  (let [result (transcribe (source "前。\n［＃ここからキャプション］図３　患者。\n　説明。［＃ここでキャプション終わり］\n後。"))
        wrapper (first (filter #(= "caption" (attribute % "type")) (elements result "div")))
        paragraphs (filter #(within? wrapper %) (elements result "p"))]
    (is (= "前。\n図３　患者。\n　説明。\n後。" (:plaintext result)))
    (is (= 2 (count paragraphs)))
    (is (= "説明。" (.getTextContent ^Node (second paragraphs))))
    (is (= "text-indent: 1em" (attribute (second paragraphs) "style")))))

(deftest variants-retain-their-targets-through-formatting-and-reading-suffixes
  (let [styled (transcribe (source "ΩIV［＃「IV」は上付き小文字］［＃「IV」は底本では「VI」］k［＃「k」は下付き小文字］"))
        lemma (first (elements styled "lem"))
        reading (transcribe (source "人を過《あや》め［＃ルビの「あや」は底本では「なや」］後。"))]
    (is (= "ΩIVk" (:plaintext styled)))
    (is (= ["VI"] (texts styled "rdg")))
    (is (within? lemma (first (elements styled "hi"))))
    (is (= "人を過め後。" (:plaintext reading)))
    (is (= ["あや"] (texts reading "lem")))
    (is (= ["なや"] (texts reading "rdg")))
    (is (within? (first (elements reading "rt")) (first (elements reading "app"))))))

(deftest explicit-witness-omission-preserves-principal-punctuation
  (let [result (transcribe (source "出来ない。［＃「。」は底本では欠落］後。"))
        alternative (first (elements result "rdg"))]
    (is (= "出来ない。後。" (:plaintext result)))
    (is (= "omission" (attribute alternative "subtype")))
    (is (= "" (.getTextContent ^Node alternative)))))

(deftest retrospective-formatting-keeps-kunten-out-of-the-body-reading
  (let [result (transcribe (source "野［＃（ノ）］宮ごもり［＃「野［＃（ノ）］宮ごもり」に傍線］"))
        note (first (filter #(= "kunten" (attribute % "type")) (elements result "note")))
        emphasis (first (elements result "hi"))]
    (is (= "野宮ごもり" (:plaintext result)))
    (is (= "ノ" (.getTextContent ^Node note)))
    (is (within? emphasis note))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest source-indentation-survives-hanging-layout-and-its-end
  (let [result (transcribe (source "［＃ここから改行天付き、折り返して１字下げ］\n　内。\n［＃ここで字下げ終わり］\n　外。"))
        paragraphs (elements result "p")]
    (is (= "　内。\n　外。" (:plaintext result)))
    (is (= ["text-indent: 0em" "text-indent: 1em"] (mapv #(attribute % "style") paragraphs)))
    (is (= ["first-line-indent(1)" "first-line-indent(1)"] (mapv #(attribute % "rend") paragraphs)))))

(deftest partial-layout-retains-known-axes-and-exact-uncertainty
  (let [opening "［＃ここから３字下げ、未対応指定、２０字詰め］"
        closing "［＃ここで字下げ終わり］"
        result (transcribe (source (str opening "\n本文\n" closing)))
        paragraph (first (filter #(= "本文" (view/visible-text %)) (elements result "p")))]
    (is (= "本文" (:plaintext result)))
    (is (some? (enclosing-style paragraph "padding-inline-start: 3em; inline-size: 20em")))
    (is (some #(string/includes? % "未対応指定") (map #(.getTextContent ^Node %) (elements result "note"))))
    (is (not-any? #(string/includes? % closing) (map #(.getTextContent ^Node %) (elements result "note"))))
    (is (seq (get-in result [:ir "interpretation_problems"])))
    (is (= ["line-layout"] (mapv #(get % "kind") (get-in result [:ir "interpretation_facts"]))))))
(deftest scoped-frames-and-horizontal-writing-retain-partial-paragraphs
  (doseq [[name rend] [["罫囲み" "keigakomi border(rule)"] ["横組み" "yokogumi horizontal"]]]
    (let [result (transcribe (source (str "前［＃ここから" name "］甲\n乙［＃ここで" name "終わり］後")))
          wrappers (filterv #(= rend (attribute % "rend")) (elements result "hi"))]
      (is (= "前甲\n乙後" (:plaintext result)))
      (is (= ["前甲" "乙後"] (texts result "p")))
      (is (= ["甲" "乙"] (mapv view/visible-text wrappers)))
      (is (= 1 (count (set (map #(attribute % "source") wrappers)))))
      (is (empty? (get-in result [:ir "interpretation_problems"]))))))

(deftest source-line-layout-contains-inline-heading-and-its-following-text
  (let [result (transcribe (source "［＃１字下げ］［＃同行大見出し］優しき歌［＃同行大見出し終わり］叢書"))
        scope (first (filter #(= "padding-inline-start: 1em" (attribute % "style")) (elements result "div")))]
    (is (= "優しき歌叢書" (:plaintext result)))
    (is (= ["優しき歌叢書"] (texts result "p")))
    (is (within? scope (first (elements result "p"))))))

(deftest supplied-indentation-can-end-inside-a-source-paragraph
  (let [result (transcribe (source "［＃ここから４字下げ］\n（どういふ形にするのです？）\n（正方形にやりますか。）［＃ここで字下げ終わり］院長は云った。"))
        scopes (filterv #(= "padding-inline-start: 4em" (attribute % "style")) (elements result "seg"))]
    (is (= "（どういふ形にするのです？）\n（正方形にやりますか。）院長は云った。" (:plaintext result)))
    (is (= ["（どういふ形にするのです？）" "（正方形にやりますか。）院長は云った。"] (texts result "p")))
    (is (= ["（どういふ形にするのです？）" "（正方形にやりますか。）"] (mapv view/visible-text scopes)))
    (is (= 1 (count (set (map #(attribute % "source") scopes)))))))

(deftest noncanonical-gaiji-keeps-surrounding-prose-visible
  (let [marker "※［「纏」の「广」に代えて「厂」、54-14］"
        result (transcribe (source (str "前" marker "後。")))]
    (is (= "前後。" (:plaintext result)))
    (is (= [marker] (mapv #(get % "raw") (get-in result [:ir "interpretation_problems"]))))
    (is (some #(= marker (.getTextContent ^Node %)) (elements result "note")))))

(deftest supplied-concealment-preserves-placeholder-and-stated-extent
  (doseq [[body text note-kind quantity extent]
          [["前□□［＃底本２字伏字］後" "前□□後" "base-edition" "2" ""]
           ["前＊［＃「＊」は伏せ字］後" "前＊後" "source-concealment" "" "unknown"]]]
    (let [result (transcribe (source body))
          gap (first (elements result "gap"))]
      (is (= text (:plaintext result) (projection/markdown (:view result))))
      (is (= quantity (attribute gap "quantity")))
      (is (= extent (attribute gap "extent")))
      (is (= "concealed" (attribute gap "reason")))
      (is (= note-kind (attribute (.getParentNode ^Node gap) "type")))
      (is (empty? (get-in result [:ir "interpretation_problems"]))))))

(deftest conflicting-concealment-count-remains-explicit
  (let [result (transcribe (source "□□□［＃底本２字伏字］"))]
    (is (= "□□□" (:plaintext result)))
    (is (empty? (elements result "gap")))
    (is (seq (get-in result [:ir "interpretation_problems"])))
    (is (empty? (get-in result [:view :view/eligible-spans])))))

(deftest supplied-rich-image-description-stays-outside-principal-text
  (let [result (transcribe (source "前［＃漢《かん》の図（fig1.png）入る］後"))
        description (first (filter #(= "image-description" (attribute % "type")) (elements result "note")))
        image (first (elements result "graphic"))]
    (is (= "前後" (:plaintext result)))
    (is (= ["前後"] (texts result "p")))
    (is (= "fig1.png" (attribute image "url")))
    (is (some? description))
    (is (string/includes? (.getTextContent ^Node description) "漢"))
    (is (string/includes? (.getTextContent ^Node description) "かん"))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest explicit-ruby-base-preserves-internal-formatting
  (doseq [[body base styled]
          [["｜宜引［＃「引」は小書き右寄せ］縞《いいしま》" "宜引縞" "引"]
           ["｜羯阿［＃「阿」は一段階小さな文字］迦《ぎゃあぎあ》" "羯阿迦" "阿"]
           ["｜咳［＃「咳」は罫囲み］声《しわぶき》" "咳声" "咳"]]]
    (let [result (transcribe (source body))
          base-node (first (elements result "rb"))
          style (first (filter #(= styled (view/visible-text %)) (elements result "hi")))]
      (is (= base (:plaintext result)))
      (is (= [base] (texts result "rb")))
      (is (within? base-node style))
      (is (empty? (get-in result [:ir "interpretation_problems"])))
      (is (some #(= "ruby" (get % "kind"))
                (get-in result [:ir "interpretation_facts"]))))))

(deftest supplied-gaiji-code-is-independent-of-source-locator-spelling
  (doseq [[marker glyph]
          [["※［＃「衙」の「吾」に代えて「干」、U+884E、225-図のキャプション］" "衎"]
           ["※［＃「喪」の「畏－田」に代えて「冖／貝」、U+8CF7、16-本文-7］" "賷"]]]
    (let [result (transcribe (source (str "前" marker "《よみ》後")))]
      (is (= (str "前" glyph "後") (:plaintext result)))
      (is (= [glyph] (texts result "g")))
      (is (empty? (get-in result [:ir "interpretation_problems"])))
      (is (some #(= "gaiji-ruby" (get % "kind"))
                (get-in result [:ir "interpretation_facts"]))))))

(deftest source-page-reference-keeps-the-printed-label
  (let [result (transcribe (source "三五頁［＃「三五頁」は「須佐の男の神」の「穀物の種」］にある。"))
        note (first (filter #(= "cross-reference" (attribute % "type")) (elements result "note")))]
    (is (= "三五頁にある。" (:plaintext result)))
    (is (= "「須佐の男の神」の「穀物の種」" (.getTextContent ^Node note)))
    (is (= "" (attribute note "target"))))
  (is (seq (get-in (transcribe (source "本文［＃「三五頁」は「章」］")) [:ir "interpretation_problems"]))))

(deftest implicit-ruby-retains-cyrillic-and-decimal-source-bases
  (doseq [[body base reading]
          [["現代のСССР《エスエスエスエル》" "СССР" "エスエスエスエル"]
           ["８《エイト》" "８" "エイト"]
           ["９｜８《はち》" "８" "はち"]]]
    (let [result (transcribe (source body))]
      (is (= [base] (texts result "rb")))
      (is (= [reading] (texts result "rt")))
      (is (empty? (get-in result [:ir "interpretation_problems"])))
      (is (some #(= "ruby" (get % "kind"))
                (get-in result [:ir "interpretation_facts"]))))))

(deftest annotations-retain-source-selected-rich-targets
  (doseq [[target visible]
          [["どふ／＼" "どふ〱"]
           ["※［＃濁点付き片仮名ヱ、1-7-84］" "ヹ"]
           ["白《タク》衾" "白衾"]]]
    (let [result (transcribe (source (str "前、" target "［＃「" target "」に「マヽ」の注記］後")))]
      (is (= (str "前、" visible "後") (:plaintext result)))
      (is (empty? (get-in result [:ir "interpretation_problems"])))
      (is (some #(= "マヽ" (.getTextContent ^Node %)) (elements result "note"))))))

(deftest partial-ruby-note-preserves-base-and-reading
  (let [result (transcribe (source "前、菌毒《きんどく》［＃「菌」の左に「キノコ」の注記］後"))
        note (first (filter #(= "gloss" (attribute % "type")) (elements result "note")))]
    (is (= "前、菌毒後" (:plaintext result)))
    (is (= ["菌毒"] (texts result "rb")))
    (is (= ["きんどく"] (texts result "rt")))
    (is (= "キノコ" (.getTextContent ^Node note)))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest supplied-multiline-accent-scope-retains-physical-lines
  (let [result (transcribe (source "〔Pardonnez a` mon bavardage\nJ'en suis a` mon premier voyage.〕"))]
    (is (= "Pardonnez à mon bavardage\nJ'en suis à mon premier voyage." (:plaintext result)))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest source-encoded-latin-accents-retain-the-whole-ruby-base
  (doseq [[body base reading]
          [["〔e'galite'〕《エガリテエ》" "égalité" "エガリテエ"]
           ["〔Charite'〕《シヤリテエ》" "Charité" "シヤリテエ"]
           ["〔Ske^ne^〕《スケーネ》" "Skênê" "スケーネ"]
           ["〔Orche^stra〕《オルケストラ》" "Orchêstra" "オルケストラ"]]]
    (let [result (transcribe (source (str "前" body "後")))]
      (is (= (str "前" base "後") (:plaintext result)))
      (is (= [base] (texts result "rb")))
      (is (= [reading] (texts result "rt")))
      (is (empty? (get-in result [:ir "interpretation_problems"]))))))

(deftest literal-reference-signs-survive-visible-text-projections
  (doseq [body ["句　※１／書簡" "［※２］" "（※三）" "※記号。この※番号。" "※"]]
    (let [result (transcribe (source body))]
      (is (= body (:plaintext result)))
      (is (empty? (get-in result [:ir "interpretation_problems"])))
      (is (= [body] (texts result "p"))))))

(deftest separately-transcribed-notes-become-target-associated-apparatus
  (doseq [[body principal expected]
          [["　なごりイ\n飯蛸の手をひろげたる檐端哉\n　り檐の花イ\n［＃「なごりイ」は「檐端哉」の右側に、「り檐の花イ」は左側に、注記するような形で］\n次の句"
            "飯蛸の手をひろげたる檐端哉\n次の句"
            #{["なごりイ" "right"] ["り檐の花イ" "left"]}]
           ["家根の上にどこの哀れぞ揚燈籠\n　よそイ　やイ\n［＃「よそイ」は「どこ」の左側に、「やイ」は「ぞ」の左側に注記するような形で］\n次の句"
            "家根の上にどこの哀れぞ揚燈籠\n次の句"
            #{["よそイ" "left"] ["やイ" "left"]}]]]
    (let [result (transcribe (string/replace (source body) "\n" "\r\n"))
          notes (filterv #(= "gloss" (attribute % "type")) (elements result "note"))]
      (is (= principal (:plaintext result)))
      (is (= 2 (count notes)))
      (is (= expected (set (map (fn [^Node note] [(.getTextContent note) (attribute note "place")]) notes))))
      (is (every? #(string/starts-with? (attribute (.getParentNode ^Node %) "source") "#source-") notes))
      (is (empty? (get-in result [:ir "interpretation_problems"]))))))

(deftest nested-horizontal-scopes-preserve-neighboring-source-layout
  (let [result (transcribe (source "ラテン語で［＃横組み］“ambitus”［＃横組み終わり］が［＃横組み］［＃横組み］‘ambition’［＃横組み終わり］［＃横組み終わり］を意味せず"))
        scopes (filterv #(= "yokogumi horizontal" (attribute % "rend")) (elements result "hi"))]
    (is (= "ラテン語で“ambitus”が‘ambition’を意味せず" (:plaintext result)))
    (is (= ["“ambitus”" "‘ambition’" "‘ambition’"] (mapv view/visible-text scopes)))
    (is (identical? (second scopes) (.getParentNode ^Node (nth scopes 2))))
    (is (= 6 (count (get-in result [:ir "interpretation_facts"]))))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest detached-fraction-preserves-the-source-target-and-space
  (let [result (transcribe (source "ν = 1/n ［＃「1/n」は分数］"))
        scopes (filterv #(= "fraction" (attribute % "rend")) (elements result "hi"))]
    (is (= "ν = 1/n " (:plaintext result)))
    (is (= ["1/n"] (mapv view/visible-text scopes)))
    (is (= 1 (count (get-in result [:ir "interpretation_facts"]))))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest retrospective-frame-preserves-rich-source-targets
  (let [result (transcribe (source "高天《タカマ》［＃（个）］原《ハラ》［＃「高天［＃（个）］原」は罫囲み］"))
        frame (first (filter #(= "keigakomi border(rule)" (attribute % "rend")) (elements result "hi")))]
    (is (= "高天原" (:plaintext result)))
    (is (some? frame))
    (is (= ["タカマ" "ハラ"] (texts result "rt")))
    (is (some #(and (= "kunten" (attribute % "type")) (= "个" (.getTextContent ^Node %))) (elements result "note")))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest retrospective-frame-preserves-inline-heading-and-enclosing-indent
  (let [result (transcribe (source "［＃１字下げ］二月二十五日［＃「二月二十五日」は同行大見出し］（水）［＃「二月二十五日（水）」は罫囲み］"))
        frame (first (filter #(= "keigakomi border(rule)" (attribute % "rend")) (elements result "hi")))
        heading (first (filter #(= "heading" (attribute % "type")) (elements result "seg")))]
    (is (= "二月二十五日（水）" (:plaintext result)))
    (is (= "二月二十五日（水）" (view/visible-text frame)))
    (is (= "二月二十五日" (view/visible-text heading)))
    (is (= "dogyo" (attribute heading "rend")))
    (is (= 1 (get-in result [:ir "layout_blocks" 0 "indent"])))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest shared-column-and-frame-closure-preserves-both-source-scopes
  (doseq [closer ["［＃ここで２段組み、罫囲み終わり］" "［＃ここで段組、罫囲み終わり］"]]
    (let [result (transcribe (source (str "［＃ここから罫囲み］\n［＃ここから２段組］\n甲《こう》［＃改段］乙\n" closer)))
          scopes (get-in result [:ir "layout_blocks"])]
      (is (= "甲\n乙" (:plaintext result)))
      (is (= 1 (count (elements result "cb"))))
      (is (some #(= "column-count: 2" (attribute % "style")) (elements result "div")))
      (is (empty? (get-in result [:ir "interpretation_problems"])))
      (is (= 2 (count scopes)))
      (is (apply = (map #(get-in % ["source_span" "end"]) scopes))))))

(deftest unresolved-opening-clause-does-not-invalidate-established-shared-close
  (let [closer "［＃ここで２段組み、罫囲み終わり］"
        input (source (str "［＃ここから罫囲み］\n［＃ここから２段組み、段間に未知の罫］\n本文\n" closer))
        result (transcribe input)
        start (.indexOf ^String input closer)
        start-bytes (alength (.getBytes (subs input 0 start) java.nio.charset.StandardCharsets/UTF_8))
        close-facts (filter #(= start-bytes (get-in % ["source_span" "start"]))
                            (get-in result [:ir "interpretation_facts"]))]
    (is (= "本文" (:plaintext result)))
    (is (= #{"layout" "line-layout"} (set (map #(get % "kind") close-facts))))
    (is (= ["段間に未知の罫"] (mapv #(get % "raw") (get-in result [:ir "interpretation_problems"]))))))

(deftest column-rule-records-only-supplied-layout
  (let [result (transcribe (source "［＃ここから罫囲み］\n［＃ここから２段組み、段間に罫］\n甲［＃改段］乙\n［＃ここで２段組み、罫囲み終わり］"))
        columns (first (filter #(= "column-count: 2" (attribute % "style")) (elements result "div")))]
    (is (= "甲\n乙" (:plaintext result)))
    (is (= "column-rule" (attribute columns "rend")))
    (is (= 1 (count (elements result "cb"))))
    (is (empty? (get-in result [:ir "interpretation_problems"])))
    (is (= {"layout" 2 "line-layout" 2}
           (frequencies (filter #{"layout" "line-layout"}
                                (map #(get % "kind") (get-in result [:ir "interpretation_facts"]))))))))

(deftest parenthesized-tcy-alias-retains-target-and-visible-text
  (let [result (transcribe (source "前（イ）［＃（イ）は縦中横］後"))
        upright (first (filter #(= "text-combine-upright" (attribute % "rend")) (elements result "hi")))]
    (is (= "前（イ）後" (:plaintext result)))
    (is (= "（イ）" (view/visible-text upright)))
    (is (= 1 (count (get-in result [:ir "interpretation_facts"]))))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest horizontal-writing-aliases-retain-supplied-targets-and-source-lines
  (let [result (transcribe (source "前しらおか［＃「しらおか」は横書き］後\n［＃ここから横書き］\n甲\n乙\n［＃ここで横書き終わり］"))
        horizontal? #(= "yokogumi horizontal" (attribute % "rend"))]
    (is (= "前しらおか後\n甲\n乙" (:plaintext result)))
    (is (= ["しらおか"] (mapv view/visible-text (filter horizontal? (elements result "hi")))))
    (is (= 1 (count (filter horizontal? (elements result "div")))))
    (is (= 3 (count (get-in result [:ir "interpretation_facts"]))))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest horizontal-writing-without-a-target-cannot-introduce-principal-text
  (doseq [spelling ["横書き" "横組み"]]
    (let [result (transcribe (source (str "前［＃「不在」は" spelling "］後")))]
      (is (= "前後" (:plaintext result)))
      (is (empty? (get-in result [:ir "interpretation_facts"])))
      (is (= ["uninterpreted-notation"] (mapv #(get % "kind") (get-in result [:ir "interpretation_problems"])))))))

(deftest quoted-closing-delimiter-stays-in-edition-apparatus
  (let [marker "［＃「〕」は底本では「］」］"
        result (transcribe (source (str "前〕" marker "後")))]
    (is (= "前〕後" (:plaintext result)))
    (is (= ["〕"] (texts result "lem")))
    (is (= ["］"] (texts result "rdg")))
    (is (= 1 (count (get-in result [:ir "interpretation_facts"]))))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest horizontal-presentation-ends-before-enclosing-indentation
  (let [result (transcribe (source "［＃ここから２字下げ、横組み右揃えで］\n横向き\n［＃ここで横組み終わり］\n字下げだけ\n［＃ここで字下げ終わり］\n外側"))
        scopes (elements result "div")
        indent (first (filter #(string/includes? (attribute % "style") "padding-inline-start: 2em") scopes))
        horizontal (first (filter #(string/includes? (attribute % "style") "writing-mode: horizontal-tb") scopes))]
    (is (= "横向き\n字下げだけ\n外側" (:plaintext result)))
    (is (= "横向き\n\n字下げだけ\n\n外側" (projection/markdown (:view result))))
    (is (= ["横向き" "字下げだけ" "外側"] (texts result "p")))
    (is (= "横向き" (view/visible-text horizontal)))
    (is (string/includes? (view/visible-text indent) "字下げだけ"))
    (is (not (string/includes? (view/visible-text indent) "外側")))
    (is (identical? indent (.getParentNode ^Node horizontal)))
    (is (string/includes? (attribute horizontal "style") "text-align: right"))
    (is (not (string/includes? (attribute indent "style") "text-align")))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest heading-edition-apparatus-preserves-heading-placement
  (let [result (transcribe (source "［＃１字下げ］欧洲婦人の髪（晶子）［＃「欧洲婦人の髪（晶子）」は大見出し］［＃「欧洲婦人の髪（晶子）」は底本では「欧洲婦人の髪」］\n本文"))
        heading (first (elements result "head"))]
    (is (= "欧洲婦人の髪（晶子）\n本文" (:plaintext result)))
    (is (= ["欧洲婦人の髪（晶子）"] (texts result "lem")))
    (is (= ["欧洲婦人の髪"] (texts result "rdg")))
    (is (= "欧洲婦人の髪（晶子）" (view/visible-text heading)))
    (is (empty? (get-in result [:ir "interpretation_problems"]))))
  (let [result (transcribe (source "ロダン翁《をう》［＃「ロダン翁」は大見出し］［＃ルビの「をう」は底本では「おう」］\n本文"))]
    (is (= "ロダン翁\n本文" (:plaintext result)))
    (is (= ["をう"] (texts result "lem")))
    (is (= ["おう"] (texts result "rdg")))
    (is (= ["ロダン翁"] (texts result "head"))))
  (let [result (transcribe (source "前［＃「不在」は大見出し］後"))]
    (is (= "前後" (:plaintext result)))
    (is (empty? (elements result "head"))))
  (doseq [separator ["\n" "［＃未知の意味］"]]
    (let [result (transcribe (source (str "見出し［＃「見出し」は大見出し］" separator "［＃「見出し」は底本では「別題」］")))]
      (is (empty? (elements result "app")))
      (is (seq (get-in result [:ir "interpretation_problems"]))))))

(deftest exact-unmapped-glyph-target-preserves-both-source-alternatives
  (let [glyph "※［＃濁点付き井、379-1］"
        current (str "ダ・" glyph "ンチ等の")
        result (transcribe (source (str current "［＃「" current "」は底本では「ダ" glyph "ンチ等の」］")))
        glyphs (elements result "g")]
    (is (= "ダ・\uFFFCンチ等の" (:plaintext result)))
    (is (= 2 (count glyphs)))
    (is (= [[0 6] [9 21]] (get-in result [:view :view/eligible-spans])))
    (is (every? #(string/blank? (.getTextContent ^Node %)) glyphs))
    (is (every? #(not (string/blank? (attribute % "ref"))) glyphs))
    (is (= 1 (count (elements result "app")))))
  (let [result (transcribe (source "甲※［＃濁点付き井、379-1］乙［＃「甲※［＃濁点付き中、379-1］乙」は底本では「別」］"))]
    (is (= "甲\uFFFC乙" (:plaintext result)))
    (is (empty? (elements result "app")))
    (is (seq (get-in result [:ir "interpretation_problems"])))))

(deftest rich-heading-quotation-retains-both-original-ruby-readings
  (let [result (transcribe (source "一〇、失せ物は巽《たつみ》の方の栗《マロニエ》の根元を探すべし。［＃「一〇、失せ物は巽《たつみ》の方の栗の根元を探すべし。」は同行中見出し］後。"))
        heading (first (filter #(= "heading" (attribute % "type")) (elements result "seg")))]
    (is (= "一〇、失せ物は巽の方の栗の根元を探すべし。後。" (:plaintext result)))
    (is (= "一〇、失せ物は巽の方の栗の根元を探すべし。" (view/visible-text heading)))
    (is (= ["たつみ" "マロニエ"] (texts result "rt")))
    (is (= "dogyo" (attribute heading "rend")))
    (is (empty? (get-in result [:ir "interpretation_problems"]))))
  (doseq [[text marker] [["ギリシャの医師たち" "［＃「ギリシヤの医師たち」は同行小見出し］"]
                         ["キリスト教は愛他主義の第一要因" "［＃「キリスト教は愛他主義の第一要員」は同行小見出し］"]]]
    (let [result (transcribe (source (str text marker)))
          problems (get-in result [:ir "interpretation_problems"])]
      (is (= text (:plaintext result)))
      (is (not-any? #(= "heading" (attribute % "type")) (elements result "seg")))
      (is (= [marker] (mapv #(get % "raw") problems)))
      (is (= [["structure" "layout"]] (mapv #(get % "aspects") problems))))))

(deftest relative-placement-links-source-anchors-without-absolute-indentation
  (let [result (transcribe (source "複\n\n［＃「複」の文字の下から２字下げ、横組み右揃えで］\n1500\n7×2\n［＃ここで横組み終わり］\n外側"))
        placed (first (filter #(= "placement-below anchor-kind(text) anchor-offset-chars(2)" (attribute % "rend")) (elements result "div")))
        reference (subs (attribute placed "corresp") 1)
        anchor (first (filter #(= reference (attribute % "xml:id")) (elements result "note")))]
    (is (= "複\n1500\n7×2\n外側" (:plaintext result)))
    (is (= "1500\n7×2" (view/visible-text placed)))
    (is (= "source-span" (attribute anchor "type")))
    (is (not (string/includes? (attribute placed "style") "padding-inline-start")))
    (is (empty? (get-in result [:ir "interpretation_problems"]))))
  (let [result (transcribe (source "［＃ここから２字下げ、横組み右揃えで］\n2000K\n500km\n［＃横組みの下に、左右中央縦組みで］\n逆カモメ型Ｗ\n［＃ここで字下げ、横組み終わり］\n外側"))
        placed (first (filter #(= "placement-below anchor-kind(horizontal-block)" (attribute % "rend")) (elements result "div")))
        horizontal (first (filter #(string/includes? (attribute % "style") "writing-mode: horizontal-tb") (elements result "div")))]
    (is (= "逆カモメ型Ｗ" (view/visible-text placed)))
    (is (= "2000K\n500km" (view/visible-text horizontal)))
    (is (= "writing-mode: vertical-rl; text-align: center" (attribute placed "style")))
    (is (= "2000K\n500km\n逆カモメ型Ｗ\n外側" (:plaintext result)))
    (is (= "2000K\n\n500km\n\n逆カモメ型Ｗ\n\n外側" (projection/markdown (:view result))))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest edition-layout-ranges-reference-source-without-applying-layout
  (let [result (transcribe (source "［＃ここから２字下げ］\n［＃ここから底本では上段］\n北海道の羆《ひぐま》。\n［＃ここまで底本では上段］\n［＃ここから底本では下段］\n下の文。\n［＃ここまで底本では下段］\n［＃ここで字下げ終わり］"))
        notes (filterv #(and (= "base-edition" (attribute % "type"))
                             (not (string/blank? (attribute % "target"))))
                       (elements result "note"))
        records (into {} (map #(vector (str "#" (attribute % "xml:id")) %))
                      (filter #(= "source-span" (attribute % "type")) (elements result "note")))]
    (is (= ["底本では上段" "底本では下段"] (mapv #(.getTextContent ^Node %) notes)))
    (is (= ["ひぐま"] (texts result "rt")))
    (is (= 1 (count (get-in result [:ir "layout_blocks"]))))
    (is (= 2 (get-in result [:ir "layout_blocks" 0 "indent"])))
    (doseq [note notes]
      (is (contains? records (attribute note "target")))
      (let [sources (string/split (attribute note "source") #" ")]
        (is (= 2 (count sources)))
        (is (every? records sources))))
    (is (empty? (get-in result [:ir "interpretation_problems"]))))
  (let [result (transcribe (source "［＃ここから底本では上段］本文［＃ここまで底本では下段］"))]
    (is (= "本文" (:plaintext result)))
    (is (not-any? #(not (string/blank? (attribute % "target"))) (elements result "note")))
    (is (= 2 (count (get-in result [:ir "interpretation_problems"]))))))

(deftest qualitative-font-size-preserves-supplied-degree-without-numeric-stages
  (doseq [[clause direction qualifier]
          [["小さい活字" "smaller" nil]
           ["字のポイントはやや小さくしてある。" "smaller" "やや"]
           ["本文よりひとまわり大きい太ゴシック体" "larger" "ひとまわり"]]]
    (let [result (transcribe (source (str "［＃ここから２字下げ、" clause "］\n本文\n［＃ここで字下げ終わり］")))
          styled (first (filter #(string/includes? (attribute % "rend") "font-size qualitative") (elements result "div")))
          rend (attribute styled "rend")]
      (is (= "本文" (:plaintext result)))
      (is (= "本文" (projection/markdown (:view result))))
      (is (string/includes? rend (str "qualitative(" direction ")")))
      (is (= (boolean qualifier) (string/includes? rend "qualifier(")))
      (when qualifier
        (is (string/includes? rend (str "qualifier(" qualifier ")"))))
      (when (= qualifier "ひとまわり")
        (is (string/includes? rend "bold"))
        (is (string/includes? rend "gothic")))
      (is (not (string/includes? (:tei result) "level=1")))
      (is (empty? (get-in result [:ir "interpretation_problems"]))))))

(deftest edition-variant-across-physical-breaks-keeps-principal-text
  (let [result (transcribe (source "指の白よ、［＃「白よ、［＃改行］［＃改行］」は底本では「白よ、［＃改行］」］\n\n次。"))
        note (first (filter #(= "base-edition" (attribute % "type")) (elements result "note")))
        lemma (first (elements result "lem"))
        witness (first (elements result "rdg"))]
    (is (= "指の白よ、\n次。" (:plaintext result)))
    (is (= 2 (count (string/split (attribute note "target") #" "))))
    (is (= 1 (count (elements result "app"))))
    (is (= "白よ、\n\n" (view/visible-text lemma)))
    (is (= "白よ、\n" (view/visible-text witness)))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest embedded-gaiji-edition-assertion-stays-apparatus
  (let [statement "底本はこの字を「さんずい＋「仰」のつくり」と作字上の誤り"
        result (transcribe (source (str "田口※［＃「※」は「さんずい＋卯」、第4水準2-78-35、17-上-9、" statement "］三郎")))
        note (first (filter #(= "base-edition" (attribute % "type")) (elements result "note")))
        annotated (first (filter #(= "annotated-text" (attribute % "type")) (elements result "seg")))]
    (is (= "田口泖三郎" (:plaintext result)))
    (is (= statement (.getTextContent ^Node note)))
    (is (= ["泖"] (texts result "g")))
    (is (not (string/blank? (attribute annotated "source"))))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest supplied-baseline-position-and-exponent-do-not-invent-size
  (doseq [[body expected rend kind]
          [["“［＃「“」は下付き］MON”［＃「”」は下付き］" "“MON”" "baseline-lowered" "baseline-position"]
           ["A2［＃「2」は指数］－B2［＃「2」は指数］" "A2－B2" "exponent" "exponent"]]]
    (let [result (transcribe (source body))
          marked (filterv #(= rend (attribute % "rend")) (elements result "hi"))]
      (is (= expected (:plaintext result)))
      (let [markdown (projection/markdown (:view result))]
        (is (= expected (string/replace markdown #"<[^>]+>" "")))
        (is (string/includes? markdown (str "data-tei-rend=\"" rend "\"")))
        (is (string/includes? markdown "font-size: inherit")))
      (is (= 2 (count marked)))
      (is (every? #(not (string/blank? (attribute % "source"))) marked))
      (is (not (string/includes? (:tei result) "small-script")))
      (is (= 2 (count (filter #(= kind (get % "kind")) (get-in result [:ir "interpretation_facts"])))))
      (is (empty? (get-in result [:ir "interpretation_problems"]))))))

(deftest supplied-modern-translation-remains-rich-apparatus
  (let [result (transcribe (source "前［＃現代語訳「松籟《しょうらい》を聞かせる。」］後"))
        note (first (filter #(= "explanation" (attribute % "type")) (elements result "note")))]
    (is (= "前後" (:plaintext result)))
    (is (= "前後" (projection/markdown (:view result))))
    (is (= ["松籟"] (texts result "rb")))
    (is (= ["しょうらい"] (texts result "rt")))
    (is (= "現代語訳「松籟しょうらいを聞かせる。」" (.getTextContent ^Node note)))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest source-note-roles-preserve-operands-without-invented-links
  (let [result (transcribe (source "前（１）［＃「（１）」は注釈番号］中（一）［＃（一）は自注］後（１）［＃「（１）」は注釈番号］"))
        notes (filterv #(= "source-role" (attribute % "type")) (elements result "note"))]
    (is (= "前（１）中（一）後（１）" (:plaintext result)))
    (is (= (:plaintext result) (projection/markdown (:view result))))
    (is (= ["annotation-number" "author-note" "annotation-number"] (mapv #(attribute % "subtype") notes)))
    (is (= ["注釈番号" "自注" "注釈番号"] (mapv #(.getTextContent ^Node %) notes)))
    (doseq [note notes field ["place" "target" "resp"]]
      (is (string/blank? (attribute note field))))
    (is (= 3 (count (distinct (map #(attribute (.getParentNode ^Node %) "source") notes)))))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))
(deftest figure-explanations-preserve-below-placement-without-figure-binding
  (let [result (transcribe (source "前。\n［＃ここから図表下部解説文］\n説明一。\n説明二。\n［＃ここで図表下部解説文終わり］\n後。"))
        wrapper (first (filter #(= "figure-explanation" (attribute % "type")) (elements result "div")))]
    (is (some? wrapper))
    (is (= "below" (attribute wrapper "rend")))
    (is (= "" (attribute wrapper "corresp")))
    (is (= ["説明一。" "説明二。"] (mapv view/visible-text (filter #(= "p" (view/local-name %)) (view/children wrapper)))))
    (is (= "前。\n説明一。\n説明二。\n後。" (:plaintext result)))
    (is (empty? (get-in result [:ir "interpretation_problems"])))
    (is (= 2 (count (filter #(= "caption" (get % "kind")) (get-in result [:ir "interpretation_facts"])))))))
(deftest supplied-indentation-spellings-retain-geometry-and-close-constraints
  (let [result (transcribe (source "［＃ここから改行一字下げ、折り返して二字下げ］\n本文\n［＃ここで１字下げ終わり］"))
        layout (first (get-in result [:ir "layout_blocks"]))]
    (is (= "本文" (:plaintext result)))
    (is (= 1 (get layout "indent")))
    (is (= 2 (get layout "continuation_indent")))
    (is (string/includes? (:tei result) "padding-inline-start: 2em")))
  (let [result (transcribe (source "［＃ここから２字下げ］\n本文\n［＃ここで１字下げ終わり］"))]
    (is (= "本文" (:plaintext result)))
    (is (seq (get-in result [:ir "interpretation_problems"])))
    (is (empty? (get-in result [:ir "layout_blocks"])))))

(deftest banknote-translation-preserves-lines-independently-of-omitted-image
  (let [lines ["　　国王の名において" "十リーヴル兌換券" "　軍需品代として交付す"
               "　平和確立とともに償還す" "第三部　第一〇三九〇号" "　　　ストフレー"
               "　　正教王党軍（欄外に）"]
        result (transcribe (source (str "前。\n［＃王家の紙幣の図、図省略］\n［＃ここから紙幣の文字の訳文］\n"
                                        (string/join "\n" lines) "\n［＃ここで訳文終わり］\n後。")))
        wrapper (first (filter #(= "translation" (attribute % "type")) (elements result "div")))
        paragraphs (filter #(= "p" (view/local-name %)) (view/children wrapper))]
    (is (= "banknote-text" (attribute wrapper "subtype")))
    (is (= "" (attribute wrapper "corresp")))
    (is (= 7 (count paragraphs)))
    (is (= ["first-line-indent(2)" "" "first-line-indent(1)" "first-line-indent(1)" "" "first-line-indent(3)" "first-line-indent(2)"]
           (mapv #(attribute % "rend") paragraphs)))
    (is (= (str "前。\n" (string/join "\n" lines) "\n後。") (:plaintext result)))
    (is (empty? (elements result "figure"))))
  (let [result (transcribe (source "［＃ここから別の訳文］\n訳文\n［＃ここで訳文終わり］"))]
    (is (empty? (filter #(= "translation" (attribute % "type")) (elements result "div"))))
    (is (seq (get-in result [:ir "interpretation_problems"])))))

(deftest editorial-gloss-preserves-supplied-edition-provenance
  (let [statement "校注、「枕橋の架してある堀の奥のところ」、ただし底本では校注が脱落、底本の親本にて確認"
        result (transcribe (source (str "本所｜〆切《しめきり》［＃「〆切」に" statement "］後")))
        note (first (filter #(= "gloss" (attribute % "type")) (elements result "note")))]
    (is (= "本所〆切後" (:plaintext result)))
    (is (= ["しめきり"] (texts result "rt")))
    (is (= statement (.getTextContent ^Node note)))
    (is (string/blank? (attribute note "place")))
    (is (string/blank? (attribute note "resp")))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest cryptarithm-letter-clarification-preserves-the-puzzle
  (let [result (transcribe (source "ＯＯＩＲ\nＲＩＤ\n［＃「Ｏ」は覆面の英字です。］"))
        note (first (filter #(= "explanation" (attribute % "type")) (elements result "note")))]
    (is (= "ＯＯＩＲ\nＲＩＤ" (:plaintext result)))
    (is (= "「Ｏ」は覆面の英字です。" (.getTextContent ^Node note)))
    (is (string/blank? (attribute note "target")))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest rich-sic-apparatus-stays-with-its-supplied-variant
  (let [result (transcribe (source "小突《こづか》かれるので［＃「小突《こづか》かれるので」はママ］［＃「小突《こづか》かれるので［＃「小突《こづか》かれるので」はママ］」は底本では「かれるので小突《こづか》［＃「かれるので小突《こづか》」はママ］」］"))
        notes (filterv #(= "sic" (attribute % "type")) (elements result "note"))]
    (is (= "小突かれるので" (:plaintext result)))
    (is (= ["小突かれるので"] (texts result "lem")))
    (is (= ["かれるので小突"] (texts result "rdg")))
    (is (= 2 (count notes)))
    (is (= ["こづか" "こづか" "こづか" "こづか"] (texts result "rt")))
    (is (every? #(not (string/blank? (attribute % "source"))) notes))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest closing-quotation-target-retains-supplied-note
  (let [result (transcribe (source "「本文」［＃「」」に「ママ」の注記］後"))
        note (first (filter #(= "gloss" (attribute % "type")) (elements result "note")))]
    (is (= "「本文」後" (:plaintext result)))
    (is (= "ママ" (.getTextContent ^Node note)))
    (is (= "」ママ" (.getTextContent (.getParentNode ^Node note))))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest external-table-reference-retains-prose-and-reports-the-external-slot
  (let [marker "［＃ここに表組入る、別ファイル（densyanokonzatsu_table.txt）参照］"
        result (transcribe (source (str "前。" marker "後。")))
        note (first (filter #(= "external-table-reference" (attribute % "type")) (elements result "note")))
        reference (first (elements result "ref"))
        problem (first (get-in result [:ir "interpretation_problems"]))]
    (is (= "前。後。" (:plaintext result)))
    (is (= "前。後。" (projection/markdown (:view result))))
    (is (= "densyanokonzatsu_table.txt" (attribute reference "target")))
    (is (= note (.getParentNode ^Node reference)))
    (is (not (string/blank? (attribute note "source"))))
    (is (= marker (get problem "raw")))
    (is (= "content-outside-primary-input" (get problem "kind")))
    (is (= {"kind" "source-location"} (get problem "influence")))
    (is (= [[0 12]] (:view/eligible-spans (:view result))))
    (doseq [profile [:projection/plaintext :projection/markdown]]
      (is (= "limited" (get (projection/report profile (:view result)) "status"))))))

(deftest supplied-formula-purpose-preserves-indentation-and-physical-lines
  (let [result (transcribe (source "前。\n［＃ここから５字下げ、ここから数式］\nW(r,t) = x2［＃「2」は上付き小文字］\ny = 1\n［＃ここで字下げ終わり、ここで数式終わり］\n後。"))
        formula (first (filter #(= "formula" (attribute % "type")) (elements result "div")))
        indentation (first (filter #(= "div" (view/local-name %)) (view/children formula)))
        paragraphs (filter #(= "p" (view/local-name %)) (view/children indentation))]
    (is (some? formula))
    (is (= "padding-inline-start: 5em" (attribute indentation "style")))
    (is (= ["W(r,t) = x2" "y = 1"] (mapv view/visible-text paragraphs)))
    (is (= "前。\nW(r,t) = x2\ny = 1\n後。" (:plaintext result)))
    (is (empty? (get-in result [:ir "interpretation_problems"])))
    (is (= 2 (count (filter #(= "formula" (get % "kind")) (get-in result [:ir "interpretation_facts"])))))))

(deftest a-reading-variant-preserves-its-unselected-prefix
  (let [result (transcribe (source "懲々《こり／″＼》［＃ルビの「／″＼」は底本では「こり／＼」］"))]
    (is (= "懲々" (:plaintext result)))
    (is (= ["こり〲"] (texts result "rt")))
    (is (= ["〲"] (texts result "lem")))
    (is (= ["こり〱"] (texts result "rdg")))
    (is (empty? (get-in result [:ir "interpretation_problems"])))
    (is (every? #(not (string/blank? (attribute % "source"))) (elements result "app")))))

(deftest glyph-shape-statements-preserve-source-scalars-without-realizing-the-shape
  (let [result (transcribe (source "Yule（ユール［＃「ル」は上に「⌒」付き］）♂［＃「♂」は矢印が下向き］例"))
        notes (filterv #(= "glyph-shape" (attribute % "type")) (elements result "note"))]
    (is (= "Yule（ユール）♂例" (:plaintext result)))
    (is (= (:plaintext result) (projection/markdown (:view result))))
    (is (= ["上に「⌒」付き" "矢印が下向き"] (mapv #(.getTextContent ^Node %) notes)))
    (is (every? #(string/blank? (attribute % "place")) notes))
    (is (empty? (elements result "g")))
    (is (empty? (get-in result [:ir "interpretation_problems"])))
    (doseq [profile [:projection/plaintext :projection/markdown]]
      (let [report (projection/report profile (:view result))]
        (is (= "limited" (get report "status")))
        (is (some #{{"family" "glyph-shape" "disposition" "unsupported" "count" 2}} (get report "counts")))))))

(deftest parenthesized-source-note-preserves-the-principal-spelling
  (let [result (transcribe (source "病殺［＃「殺」に（死）の注記］とするも可。"))
        notes (filterv #(= "gloss" (attribute % "type")) (elements result "note"))]
    (is (= "病殺とするも可。" (:plaintext result)))
    (is (= (:plaintext result) (projection/markdown (:view result))))
    (is (= ["（死）"] (mapv #(.getTextContent ^Node %) notes)))
    (is (every? #(string/blank? (attribute % "place")) notes))
    (is (empty? (get-in result [:ir "interpretation_problems"])))))

(deftest repeated-end-offset-assertions-share-one-line-layout
  (let [result (transcribe (source "前。\n［＃地から８字上げ］美作守内［＃地付き、地より８字アキ］\n後。"))
        layouts (filter #(string/includes? (attribute % "style") "padding-inline-end: 8em")
                        (elements result "div"))]
    (is (= 1 (count layouts)))
    (is (= "美作守内" (view/visible-text (first layouts))))
    (is (= "前。\n美作守内\n後。" (:plaintext result)))
    (is (empty? (get-in result [:ir "interpretation_problems"])))
    (is (= 2 (count (filter #(= "line-layout" (get % "kind"))
                            (get-in result [:ir "interpretation_facts"])))))))
