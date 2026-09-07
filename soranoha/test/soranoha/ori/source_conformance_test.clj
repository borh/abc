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
(defn- texts [result tag] (mapv view/visible-text (elements result tag)))

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
      (is (= "padding-inline-start: 8em" (attribute (first (elements result "head")) "style")))
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
  (let [body "〔Pardonnez a` mon bavardage\nJ'en suis a` mon premier voyage.〕"
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
    (is (= ["chitsuki align(right) offset-from-end(2)"]
           (into [] (keep #(let [rend (attribute % "rend")] (when (string/includes? rend "chitsuki") rend)))
                 (elements result "p"))))))

(deftest inline-scopes-preserve-enclosing-paragraph-layout
  (doseq [[opening rendition body expected]
          [["［＃ここから１字下げ］" "jisage indent(1)"
            "ビタミン［＃縦中横］B1［＃「1」は下付き小文字］［＃縦中横終わり］　二ミリグラム"
            "ビタミンB1　二ミリグラム"]
           ["［＃ここから改行天付き、折り返して１字下げ］" "burasage first(0) rest(1)"
            "（［＃縦中横］10［＃縦中横終わり］）注。" "（10）注。"]]]
    (let [result (transcribe (source (str opening "\n" body "\n次の行。\n［＃ここで字下げ終わり］")))
          paragraph (first (filter #(= expected (view/visible-text %)) (elements result "p")))
          following (first (filter #(= "次の行。" (view/visible-text %)) (elements result "p")))
          tcy (first (filter #(= "text-combine-upright" (attribute % "rend")) (elements result "hi")))]
      (is (= (str expected "\n次の行。") (:plaintext result)))
      (is (some? paragraph))
      (when paragraph
        (is (= rendition (attribute paragraph "rend")))
        (is (within? paragraph tcy)))
      (is (some? following))
      (when following
        (is (= rendition (attribute following "rend")))))))

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
      (is (= "burasage first(0) rest(1)" (attribute paragraph "rend")))
      (is (string/includes? (attribute paragraph "style") "text-indent: 1em"))
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
    (is (some #(= "jisage indent(2)" (attribute % "rend")) (elements result "p")))
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
        sign (first (elements result "floatingText"))]
    (is (= "RESTAURANT\n西洋料理店\nといふ札。" (:plaintext result)))
    (is (= "RESTAURANT\n西洋料理店" (view/visible-text sign)))
    (is (= "padding-inline-start: 4em; writing-mode: horizontal-tb; text-align: center; border-style: solid"
           (attribute sign "style")))))

(deftest quoted-variants-do-not-create-body-ruby-or-gaiji
  (doseq [[body plain expected-ruby raw]
          [["煖爐《ストーブ》には［＃「煖爐《ストーブ》には」は底本では「煖燼《ストーブ》には」］、後。"
            "煖爐には、後。" ["煖爐"] "「煖爐《ストーブ》には」は底本では「煖燼《ストーブ》には」"]
           ["目［＃「※［＃「目＋旬」、第3水準1-88-80］《めくば》せを」は底本では「※［＃「目＋句」、第4水準2-81-91］《めくば》せを」］後。"
            "目後。" [] "「※［＃「目＋旬」、第3水準1-88-80］《めくば》せを」は底本では「※［＃「目＋句」、第4水準2-81-91］《めくば》せを」"]]]
    (let [result (transcribe (source body))
          notes (filter #(#{"variant" "misc"} (attribute % "type")) (elements result "note"))]
      (is (= plain (:plaintext result)))
      (is (= expected-ruby (texts result "rb")))
      (is (empty? (elements result "g")))
      (is (= [(str "［＃" raw "］")] (mapv #(.getTextContent ^Node %) notes))))))

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

(deftest multiline-caption-and-warichu-preserve-source-paragraphs
  (doseq [[name role] [["キャプション" "caption"] ["割り注" "warichu"]]]
    (let [result (transcribe (source (str "前。\n［＃ここから" name "］\n第一。\n第二。\n［＃ここで" name "終わり］\n後。")))
          wrapper (first (filter #(= role (attribute % "type")) (elements result "div")))]
      (is (= "前。\n第一。\n第二。\n後。" (:plaintext result)))
      (is (= ["第一。" "第二。"]
             (mapv #(.getTextContent ^Node %) (filter #(within? wrapper %) (elements result "p")))))
      (is (empty? (get-in result [:ir "interpretation_problems"]))))))
