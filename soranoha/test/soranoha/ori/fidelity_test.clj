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
          [["heading omission" "tei-headings" #(str/replace % "<head n='2' style='padding-inline-start: 8em'>一</head>" "")]
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

(deftest supported-encoding-and-source-boundary-layout
  (let [r (fidelity/check (.getBytes source "windows-31j")
                          (.getBytes tei "UTF-8") (.getBytes plaintext "UTF-8"))]
    (is (= "passed" (get r "status")))
    (is (= "windows-31j" (get r "source_encoding"))))
  (let [s (str/replace source "\n\n底本：" "\n［＃地から１字上げ］（日付）\n\n底本：")
        t (str/replace tei "<note " "<p xmlns:abc='https://w3id.org/abc/ns/tei' abc:layout-kind='chitsuki' abc:layout-params='align=right;offset-from-end=1'>（日付）</p><note ")
        r (report s t (str plaintext "（日付）\n"))]
    (is (= "passed" (status r "closing-date-layout")))
    (is (= "passed" (get r "status")))
    (is (= "failed" (status (report s (str/replace t "offset-from-end=1" "offset-from-end=2")
                                    (str plaintext "（日付）\n")) "closing-date-layout")))))

(deftest structure-and-zero-indentation-are-source-dependent
  (let [s (str/replace source "\n\n底本：" "\n次。\n\n底本：")
        t (str/replace tei "<note " "<p>次。</p><note ")
        p (str plaintext "次。\n")]
    (is (= "passed" (get (report s t p) "status"))))
  (let [moved (-> tei
                  (str/replace "<head n='2' style='padding-inline-start: 8em'>一</head>" "")
                  (str/replace "<note " "<head n='2' style='padding-inline-start: 8em'>一</head><note "))]
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
