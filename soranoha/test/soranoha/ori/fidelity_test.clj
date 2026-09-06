(ns soranoha.ori.fidelity-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [soranoha.ori.fidelity :as fidelity]))

(def source
  (str "題\n作者\n\n--------------------\n記号説明\n--------------------\n"
       "［＃８字下げ］一［＃「一」は中見出し］\n\n"
       "　池《いけ》の　底に、※［＃「特のへん＋廴＋聿」、第3水準1-87-71］陀多《かんだた》。\n\n"
       "底本：本\n　　　初刷\n入力：人\n"))

(def tei
  (str "<TEI xmlns='http://www.tei-c.org/ns/1.0'><teiHeader><charDecl>"
       "<char xml:id='g1'><mapping type='unicode'>犍</mapping></char></charDecl></teiHeader>"
       "<text><body><div><head n='2' style='padding-inline-start: 8em'>一</head><p style='text-indent: 1em'>"
       "<s><ruby><rb>池</rb><rt>いけ</rt></ruby>の　底に、"
       "<ruby><rb><g ref='#g1'>犍</g>陀多</rb><rt>かんだた</rt></ruby>。</s></p>"
       "<note type='source-attribution'><seg type='source-line'>底本：本</seg><lb/>"
       "<seg type='source-line' style='padding-inline-start: 3em'>初刷</seg></note>"
       "<note type='transcriber-note'><seg type='source-line'>入力：人</seg></note>"
       "</div></body></text></TEI>"))

(def plaintext "一\n\n　池の　底に、犍陀多。\n")

(defn report [source tei plaintext]
  (fidelity/check (.getBytes source "UTF-8") (.getBytes tei "UTF-8") (.getBytes plaintext "UTF-8")))

(defn status [r id]
  (some #(when (= id (get % "id")) (get % "status")) (get r "checks")))

(deftest independent-source-values-detect-export-mutations
  (is (= "passed" (get (report source tei plaintext) "status")))
  (doseq [[label id transform]
          [["empty resolved gaiji" "tei-gaiji" #(str/replace % "<g ref='#g1'>犍</g>" "<g ref='#g1'/>")]
           ["wrong resolved gaiji" "tei-gaiji" #(str/replace % ">犍</g>" ">牛</g>")]
           ["heading omission" "tei-headings" #(str/replace % "<head n='2' style='padding-inline-start: 8em'>一</head>" "")]
           ["heading level mutation" "tei-heading-layout" #(str/replace % "head n='2'" "head n='1'")]
           ["heading indent mutation" "tei-heading-layout" #(str/replace % "padding-inline-start: 8em" "padding-inline-start: 7em")]
           ["visible omission" "tei-body-text" #(str/replace % "の　底に、" "の　に、")]
           ["meaningful interior space" "tei-body-text" #(str/replace % "の　底" "の底")]
           ["serializer whitespace" "tei-body-text" #(str/replace % "<s>" "<s>\n   ")]
           ["ruby base mutation" "tei-ruby" #(str/replace % "<rb>池</rb>" "<rb>海</rb>")]
           ["ruby reading mutation" "tei-ruby" #(str/replace % "<rt>いけ</rt>" "<rt>うみ</rt>")]
           ["gaiji mapping mutation" "tei-gaiji" #(str/replace % ">犍</mapping>" ">牛</mapping>")]
           ["lexical paragraph indent" "tei-paragraph-indentation" #(str/replace % "<s>" "<s>　")]
           ["missing paragraph rendition" "tei-paragraph-indentation" #(str/replace % "text-indent: 1em" "")]
           ["lexical note indent" "tei-source-note-layout" #(str/replace % ">初刷</seg>" ">　　　初刷</seg>")]
           ["missing note rendition" "tei-source-note-layout" #(str/replace % "padding-inline-start: 3em" "")]]]
    (testing label (is (= "failed" (status (report source (transform tei) plaintext) id)))))
  (is (= "failed" (status (report source tei (str/replace plaintext "底" "")) "plaintext-body")))
  (is (= "failed" (status (report source tei (str "\n" plaintext)) "plaintext-start"))))

(deftest unsupported-source-does-not-certify-exports
  (doseq [s [(str/replace source "池《いけ》" "池［＃傍点］")
             (str/replace source "底本：" "出典：")
             (str/replace source "池《いけ》" "いけ《いけ》")]]
    (is (= "not-evaluated" (get (report s tei plaintext) "status"))))
  (let [r (fidelity/check (byte-array [(unchecked-byte 0x81)])
                          (.getBytes tei "UTF-8") (.getBytes plaintext "UTF-8"))]
    (is (= "not-evaluated" (get r "status"))))
  (is (= "failed" (get (report source "<!DOCTYPE TEI SYSTEM 'file:///etc/passwd'><TEI/>" plaintext) "status"))))

(deftest retrospective-emphasis-compares-location-text-and-rendition
  (let [s (str/replace source "の　底に、" "の　底に、しだ、しだ［＃「しだ」に傍点］。")
        t (str/replace tei "の　底に、" "の　底に、しだ、<hi rend='bouten'>しだ</hi>。")
        p (str/replace plaintext "の　底に、" "の　底に、しだ、しだ。")]
    (is (= "passed" (get (report s t p) "status")))
    (doseq [[label mutation]
            [["missing emphasis" #(str/replace % "<hi rend='bouten'>しだ</hi>" "しだ")]
             ["wrong rendition" #(str/replace % "rend='bouten'" "rend='underline'")]
             ["same text at wrong position" #(str/replace % "しだ、<hi rend='bouten'>しだ</hi>"
                                                          "<hi rend='bouten'>しだ</hi>、しだ")]
             ["wrong extent" #(str/replace % "<hi rend='bouten'>しだ</hi>" "<hi rend='bouten'>し</hi>だ")]
             ["extra emphasis" #(str/replace % "の　底に、" "<hi rend='bouten'>の</hi>　底に、")]]]
      (testing label
        (is (= "failed" (status (report s (mutation t) p) "tei-emphasis")))))
    (doseq [annotation ["［＃「ちがう」に傍点］" "［＃「しだ」に白ゴマ傍点］"
                        "［＃「しだ」に傍点］［＃「しだ」に傍点］"]]
      (is (= "not-evaluated"
             (get (report (str/replace s "［＃「しだ」に傍点］" annotation) t p) "status")))))
  (let [s (str/replace source "池《いけ》" "池《いけ》［＃「池」に傍点］")
        t (str/replace tei "<ruby><rb>池</rb><rt>いけ</rt></ruby>"
                       "<hi rend='bouten'><ruby><rb>池</rb><rt>いけ</rt></ruby></hi>")]
    (is (= "passed" (get (report s t plaintext) "status"))))
  (is (= "failed" (status (report source (str/replace tei "の　底に、" "<hi rend='bouten'>の</hi>　底に、")
                                  plaintext) "tei-emphasis"))))

(deftest supported-encoding-and-source-boundary-layout
  (let [r (fidelity/check (.getBytes source "windows-31j")
                          (.getBytes tei "UTF-8") (.getBytes plaintext "UTF-8"))]
    (is (= "passed" (get r "status")))
    (is (= "windows-31j" (get r "source_encoding"))))
  (let [s (str/replace source "\n\n底本：" "\n［＃地から１字上げ］（日付）\n\n底本：")
        t (str/replace-first tei "<note " "<p xmlns:abc='https://w3id.org/abc/ns/tei' abc:layout-kind='chitsuki' abc:layout-params='align=right;offset-from-end=1'>（日付）</p><note ")
        r (report s t (str plaintext "（日付）\n"))]
    (is (= "passed" (status r "closing-date-layout")))
    (is (= "passed" (get r "status")))
    (is (= "failed" (status (report s (str/replace t "offset-from-end=1" "offset-from-end=2")
                                    (str plaintext "（日付）\n")) "closing-date-layout")))))

(deftest structure-and-zero-indentation-are-source-dependent
  (let [s (str/replace source "\n\n底本：" "\n次。\n\n底本：")
        t (str/replace-first tei "<note " "<p>次。</p><note ")
        p (str plaintext "次。\n")]
    (is (= "passed" (get (report s t p) "status"))))
  (let [moved (-> tei
                  (str/replace "<head n='2' style='padding-inline-start: 8em'>一</head>" "")
                  (str/replace-first "<note " "<head n='2' style='padding-inline-start: 8em'>一</head><note "))]
    (is (= "failed" (status (report source moved plaintext) "tei-block-order"))))
  (let [s (str/replace source "\n\n底本：" "\n次。\n\n底本：")
        t (-> tei
              (str/replace "。</s></p>" "</s></p><p>。次。</p>"))]
    (is (= "failed" (status (report s t (str plaintext "次。\n")) "tei-body-text"))))
  (is (= "failed"
         (status (report (str/replace source "　池" "池") tei
                         (str/replace plaintext "　池" "池")) "tei-paragraph-indentation")))
  (is (= "failed"
         (status (report source (str/replace tei ">底本：本</seg>"
                                             " style='padding-inline-start: 2em'>底本：本</seg>")
                         plaintext) "tei-source-note-layout")))
  (is (= "failed" (get (report source (str/replace tei "http://www.tei-c.org/ns/1.0" "urn:other")
                               plaintext) "status"))))

(deftest colophon-explanations-and-credits-are-preserved
  (let [note "※「□」には、底本では「◆」が内接しています。"
        s (str/replace source "入力：人" (str note "\n入力：人"))
        t (str/replace tei "<seg type='source-line'>入力：人</seg>"
                       (str "<seg type='source-line'>" note "</seg><lb/>"
                            "<seg type='source-line'>入力：人</seg>"))]
    (is (= "passed" (get (report s t plaintext) "status")))
    (is (= "failed" (status (report s tei plaintext) "tei-source-note-layout")))
    (is (= "failed" (status (report s (str/replace t "入力：人" "入力：別人") plaintext)
                            "tei-source-note-layout")))))

(defn- compact-source [body]
  (str "題\n作者\n\n--------------------\n記号説明\n--------------------\n"
       body "\n\n底本：本\n"))

(defn- compact-tei [body]
  (str "<TEI xmlns='http://www.tei-c.org/ns/1.0' xmlns:abc='https://w3id.org/abc/ns/tei'>"
       "<text><body>" body "</body><back><note type='source-attribution'>"
       "<seg type='source-line'>底本：本</seg></note></back></text></TEI>"))

(deftest numeric-gaiji-references-cover-both-jis-planes
  (doseq [[annotation character]
          [["「てへん＋丑」、第4水準2-12-93" "扭"]
           ["「にんべん＋參」、第4水準2-1-79" "傪"]
           ["「口＋「皐」の「白」にかえて「自」、第4水準2-4-33" "嘷"]
           ["「やまいだれ＋低のつくり」、第4水準2-81-42" "疷"]
           ["「言＋墟のつくり」、第4水準2-88-74" "譃"]
           ["二の字点、1-2-22" "〻"]]]
    (let [s (compact-source (str "※［＃" annotation "］"))
          t (str/replace (compact-tei (str "<p><g ref='#g'>" character "</g></p>"))
                         "<text>" (str "<teiHeader><charDecl><char xml:id='g'><mapping type='unicode'>"
                                       character "</mapping></char></charDecl></teiHeader><text>"))]
      (testing annotation
        (is (= "passed" (get (report s t (str character "\n")) "status")))
        (is (= "failed" (status (report s (str/replace t (str ">" character "</g>") ">字</g>")
                                        (str character "\n")) "tei-gaiji")))
        (is (= "failed" (status (report s (str/replace t (str ">" character "</mapping>") ">字</mapping>")
                                        (str character "\n")) "tei-gaiji"))))))
  (doseq [annotation ["第3水準2-1-79" "第4水準2-2-1" "第4水準2-1-95"]]
    (is (= "not-evaluated" (get (report (compact-source (str "※［＃字、" annotation "］"))
                                        (compact-tei "<p>字</p>") "字\n") "status")))))

(deftest source-correction-notes-retain-exact-wording-and-position
  (let [body "甍《いらか》［＃「甍の」は底本では「薨の」］先。明《あ》［＃ルビの「あ」は底本では「あか」］かさう。"
        rendered "<p><ruby><rb>甍</rb><rt>いらか</rt></ruby><note type='correction'>「甍の」は底本では「薨の」</note>先。<ruby><rb>明</rb><rt>あ</rt></ruby><note type='correction'>ルビの「あ」は底本では「あか」</note>かさう。</p>"
        s (compact-source body) t (compact-tei rendered) p "甍先。明かさう。\n"]
    (is (= "passed" (get (report s t p) "status")))
    (doseq [mutation [#(str/replace % #"<note type='correction'>[^<]+</note>" "")
                      #(str/replace % "薨の" "甍の")
                      #(str/replace % "</note>先。" "先。</note>")
                      #(str/replace % "</note>先。" "</note>先。<note type='correction'>追加</note>")
                      #(str/replace % "</ruby><note type='correction'>「甍の」は底本では「薨の」</note>先。"
                                    "</ruby>先。<note type='correction'>「甍の」は底本では「薨の」</note>")]]
      (is (= "failed" (status (report s (mutation t) p) "tei-correction-notes"))))))

(deftest accent-notation-is-scoped-and-its-delimiters-are-not-body-text
  (let [s (compact-source "〔a` la Huysmans〕 〔ma^ts de'gou^t〕 jusqu'〔a`〕 〔ae& s& o/〕 〔本全集〕 a`。")
        text "à la Huysmans mâts dégoût jusqu'à æ ß ø 〔本全集〕 a`。"
        t (compact-tei (str "<p>" text "</p>")) p (str text "\n")]
    (is (= "passed" (get (report s t p) "status")))
    (doseq [mutation [#(str/replace % "à la Huysmans" "〔à la Huysmans〕")
                      #(str/replace % "mâts" "mats")
                      #(str/replace % "æ" "aē")
                      #(str/replace % "〔本全集〕" "本全集")
                      #(str/replace % "a`。" "à。")]]
      (is (= "failed" (status (report s t (mutation p)) "plaintext-body")))
      (is (= "failed" (status (report s (mutation t) p) "tei-body-text"))))))

(deftest correction-quotations-do-not-add-body-ruby-or-gaiji
  (let [correction "「煖爐《ストーブ》には」は底本では「煖燼《ストーブ》には」"
        s (compact-source (str "煖爐《ストーブ》には［＃" correction "］、後。"))
        t (compact-tei (str "<p><ruby><rb>煖爐</rb><rt>ストーブ</rt></ruby>には<note type='correction'>"
                            correction "</note>、後。</p>"))
        p "煖爐には、後。\n"]
    (is (= "passed" (get (report s t p) "status")))
    (is (= "failed" (status (report s (str/replace t "<rt>ストーブ</rt>" "<rt>誤読</rt>") p) "tei-ruby")))
    (is (= "failed" (status (report s (str/replace t "底本では「煖燼《ストーブ》" "底本では「煖燼《誤読》") p)
                            "tei-correction-notes"))))
  (let [correction "「※［＃「目＋旬」、第3水準1-88-80］《めくば》せを」は底本では「※［＃「目＋句」、第4水準2-81-91］《めくば》せを」"
        s (compact-source (str "目［＃" correction "］後。"))
        t (compact-tei (str "<p>目<note type='correction'>" correction "</note>後。</p>"))]
    (is (= "passed" (get (report s t "目後。\n") "status")))
    (is (= "failed" (status (report s (str/replace t "1-88-80" "1-88-81") "目後。\n") "tei-correction-notes")))))

(deftest scoped-layout-preserves-every-line-and-property
  (let [s (compact-source "［＃ここから４字下げ、横書き、中央揃え、罫囲み］\nRESTAURANT\n西洋料理店\n［＃ここで字下げ終わり］\nといふ札。")
        attrs " style='padding-inline-start: 4em; writing-mode: horizontal-tb; text-align: center; border-style: solid'"
        t (compact-tei (str "<div><floatingText" attrs "><body><p>RESTAURANT</p><p>西洋料理店</p></body></floatingText><p>といふ札。</p></div>"))
        p "RESTAURANT\n西洋料理店\nといふ札。\n"]
    (is (= "passed" (get (report s t p) "status")))
    (doseq [mutation [#(str/replace % "padding-inline-start: 4em" "padding-inline-start: 3em")
                      #(str/replace-first % "horizontal-tb" "vertical-rl")
                      #(str/replace-first % "text-align: center" "text-align: center-invalid")
                      #(str/replace-first % "border-style: solid" "")
                      #(str/replace % "</body></floatingText><p>といふ札。</p>" "<p>といふ札。</p></body></floatingText>")
                      #(str/replace % "</p><p>西洋料理店" (str "</p></body></floatingText><floatingText" attrs "><body><p>西洋料理店"))]]
      (is (= "failed" (status (report s (mutation t) p) "tei-enclosing-layout")))))
  (let [s (compact-source "［＃ここから２字下げ］\n偶因狂疾成殊類　　災患相仍不可逃\n［＃ここで字下げ終わり］\n次。")
        t (compact-tei "<p abc:layout-kind='jisage' abc:layout-params='indent=2'>偶因狂疾成殊類　　災患相仍不可逃</p><p>次。</p>")
        p "偶因狂疾成殊類　　災患相仍不可逃\n次。\n"]
    (is (= "passed" (get (report s t p) "status")))
    (is (= "failed" (status (report s t (str/replace p "逃\n次" "逃次")) "plaintext-body")))
    (doseq [bad [(str/replace s "［＃ここで字下げ終わり］" "")
                 (str/replace s "［＃ここから２字下げ］" "［＃ここから２字下げ］\n［＃ここから３字下げ］")
                 (str/replace s "［＃ここから２字下げ］" "［＃ここから２字下げ、未知］")]]
      (is (= "not-evaluated" (get (report bad t p) "status"))))))

(deftest enclosing-indentation-and-line-alignment-are-independent
  (let [s (compact-source "前。\n［＃ここから２字下げ］\n附記。\n［＃地から２字上げ］（大正四年八月）\n［＃ここで字下げ終わり］\n後。")
        t (compact-tei (str "<p>前。</p><floatingText style='padding-inline-start: 2em'><body><p>附記。</p>"
                            "<p abc:layout-kind='chitsuki' abc:layout-params='align=right;offset-from-end=2'>（大正四年八月）</p>"
                            "</body></floatingText><p>後。</p>"))
        p "前。\n附記。\n（大正四年八月）\n後。\n"]
    (is (= "passed" (get (report s t p) "status")))
    (doseq [mutation [#(str/replace % "padding-inline-start: 2em" "padding-inline-start: 3em")
                      #(str/replace % "</body></floatingText><p>後。</p>" "<p>後。</p></body></floatingText>")
                      #(-> % (str/replace "</body></floatingText>" "")
                           (str/replace "<p abc:layout-kind" "</body></floatingText><p abc:layout-kind"))]]
      (is (= "failed" (status (report s (mutation t) p) "tei-enclosing-layout")))))
  (let [s (compact-source "［＃ここから２字下げ］\n附記。\n［＃地から２字上げ］日付\n［＃ここで字下げ終わり］")
        t (compact-tei "<floatingText style='padding-inline-start: 2em'><body><p>附記。</p><p abc:layout-kind='chitsuki' abc:layout-params='align=right;offset-from-end=3'>日付</p></body></floatingText>")
        report (report s t "附記。\n日付\n")]
    (is (= "passed" (status report "tei-enclosing-layout")))
    (is (= "failed" (status report "closing-date-layout")))))

(deftest plain-preamble-and-two-character-closing-offset
  (let [body "こころ　こころ\nくるしいこころ"
        s (str "こころ\n今野大力\n\n" body "\n\n底本：本\n")
        t (compact-tei "<p>こころ　こころ</p><p>くるしいこころ</p>")]
    (is (= "passed" (get (report s t (str body "\n")) "status")))
    (is (= "failed" (status (report s t (str "こころ\n今野大力\n" body "\n")) "plaintext-body")))
    (is (= "failed" (status (report s (str/replace t "<body>" "<body><p>こころ</p><p>今野大力</p>")
                                    (str body "\n")) "tei-body-text")))
    (is (= "not-evaluated" (get (report (str/replace s "大力\n\n" "大力\n") t (str body "\n")) "status"))))
  (let [s (compact-source "［＃地から２字上げ］――四年九月――")
        t (compact-tei "<p abc:layout-kind='chitsuki' abc:layout-params='align=right;offset-from-end=2'>――四年九月――</p>")
        p "――四年九月――\n"]
    (is (= "passed" (get (report s t p) "status")))
    (is (= "failed" (status (report s (str/replace t "offset-from-end=2" "offset-from-end=1") p)
                            "closing-date-layout")))
    (is (= "failed" (status (report (str/replace s "［＃地から２字上げ］" "") t p)
                            "closing-date-layout")))))
