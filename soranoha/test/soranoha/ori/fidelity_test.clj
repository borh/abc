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
       "<text><body><div><head n='2'>一</head><p style='text-indent: 1em'>"
       "<s><ruby><rb>池</rb><rt>いけ</rt></ruby>の　底に、"
       "<ruby><rb><g ref='#g1'/>陀多</rb><rt>かんだた</rt></ruby>。</s></p>"
       "<note type='source-attribution'><seg type='source-line'>底本：本</seg><lb/>"
       "<seg type='source-line' style='padding-inline-start: 3em'>初刷</seg></note>"
       "</div></body></text></TEI>"))

(def plaintext "一\n\n　池の　底に、犍陀多。\n")

(defn report [source tei plaintext]
  (fidelity/check (.getBytes source "UTF-8") (.getBytes tei "UTF-8") (.getBytes plaintext "UTF-8")))

(defn status [r id]
  (some #(when (= id (get % "id")) (get % "status")) (get r "checks")))

(deftest independent-source-values-detect-export-mutations
  (is (= "passed" (get (report source tei plaintext) "status")))
  (doseq [[label id transform]
          [["heading omission" "tei-headings" #(str/replace % "<head n='2'>一</head>" "")]
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
  (is (= "failed" (status (report source tei (str/replace plaintext "底" "")) "plaintext-body"))))

(deftest unsupported-source-does-not-certify-exports
  (doseq [s [(str/replace source "池《いけ》" "池［＃傍点］")
             (str/replace source "底本：" "出典：")
             (str/replace source "池《いけ》" "いけ《いけ》")]]
    (is (= "not-evaluated" (get (report s tei plaintext) "status"))))
  (let [r (fidelity/check (byte-array [(unchecked-byte 0x81)])
                          (.getBytes tei "UTF-8") (.getBytes plaintext "UTF-8"))]
    (is (= "not-evaluated" (get r "status"))))
  (is (= "failed" (get (report source "<!DOCTYPE TEI SYSTEM 'file:///etc/passwd'><TEI/>" plaintext) "status"))))

(deftest supported-encoding-and-source-boundary-layout
  (let [r (fidelity/check (.getBytes source "windows-31j")
                          (.getBytes tei "UTF-8") (.getBytes plaintext "UTF-8"))]
    (is (= "passed" (get r "status")))
    (is (= "windows-31j" (get r "source_encoding"))))
  (let [s (str/replace source "\n\n底本：" "\n［＃地から１字上げ］（日付）\n\n底本：")
        t (str/replace tei "<note " "<p>（日付）</p><note ")
        r (report s t (str plaintext "（日付）\n"))]
    (is (= "not-evaluated" (status r "closing-date-layout")))
    (is (= "not-evaluated" (get r "status")))))
