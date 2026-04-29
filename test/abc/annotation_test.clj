(ns abc.annotation-test
  (:require [abc.annotation :refer :all]
            [abc.tools.malli :as am]
            [clojure.test :as t :refer [deftest is use-fixtures]]
            [clojure.string :as string]
            [malli.core :as m]
            [malli.dev :as dev]
            [malli.dev.pretty :as pretty]
            [abc.test-utils :refer :all]))

(def ^:dynamic ^:private *parser* nil)

(defn fixture [f]
  (am/install!)
  (dev/start! {:report (pretty/reporter)})
  (try (f)
       (finally (dev/stop!))))

(use-fixtures :once fixture)

(def test-strings
  ["青空文庫《あおぞらぶんこ》\n"
   "青空｜ぶんこ《文庫》\n"
   "青空文庫［＃「青空文庫」に傍点］\n"
   "感覚と科学\n寺田寅彦\n\n-------------------------------------------------------\n【テキスト中に現れる記号について】\n\n《》：ルビ\n（例）杜絶《とぜつ》する\n\n［＃］：入力者注　主に外字の説明や、傍点の位置の指定\n（例）［＃地から３字上げ］（昭和八年八月、科学）\n-------------------------------------------------------\n\n　近代の物理科学は、自然を研究するための道具として五官の役割をなるべく切り詰め自然を記載する言葉の中からあらゆる人間的なものを削除する事を目標として進んで来た。そうしてその意図はある程度までは遂げられたように見える。この「anthropomorphism からの解放」という合い言葉が合理的でまた目的にかなうものだということは、この旗じるしを押し立てて進んで来た近代科学の収穫の豊富さを見ても明白である。しかし、それがただの夢であることは自明的である。五官を杜絶《とぜつ》すると同時に人間は無くなり、従って世界は無くなるであろう。しかし、この、近代科学から見放された人間の感覚器を子細に研究しているものの目から見ると、これらの器官の機構は、あらゆる科学の粋を集めたいかなる器械と比べても到底比較にならないほど精緻《せいち》をきわめたものである。\n　思うに五官の認識の方法は一面分析的であると同時にまた総合的である。たとえば耳は音響を調和分析にかける。そうして、めんどうな積分的計算をわれわれの無意識の間に安々と仕上げて、音の成分を認識すると同時に、またそれを総合した和弦《かげん》や不協和音を一つの全体として認識する。\n　これはしかし、修練による人間そのものの進化によって救われないものであろうか、要するに観測器械としての感官を生理的心理的効果の係蹄《けいてい》から解放することが、ここに予想される総合的実験科学への歩みを進めるために通過すべき第一関門であろうと思われる。\n［＃地から３字上げ］（昭和八年八月、科学）\n\n\n\n底本：「寺田寅彦随筆集　第四巻」小宮豊隆編、岩波文庫、岩波書店\n　　　1948（昭和23）年5月15日第1刷発行\n　　　1963（昭和38）年5月16日第20刷改版発行\n　　　1997（平成9）年6月13日第65刷発行\n※底本の誤記等を確認するにあたり、「寺田寅彦全集」（岩波書店）を参照しました。\n入力：(株)モモ\n校正：かとうかおり\n2000年10月3日公開\n2003年10月30日修正\n青空文庫作成ファイル：\nこのファイルは、インターネットの図書館、青空文庫（http://www.aozora.gr.jp/）で作られました。入力、校正、制作にあたったのは、ボランティアの皆さんです。\n"])

(def parsed-strings
  [#:document{:paragraphs [#:paragraph{:tags #{}, :sentences [#:sentence{:tags #{}, :annotated-text [{:sentence/fragment "青空文庫", :fragment/annotation {:annotation/type :ruby, :ruby/reading "あおぞらぶんこ"}}]}]}]} #:document{:paragraphs [#:paragraph{:tags #{}, :sentences [#:sentence{:tags #{}, :annotated-text ["青空" {:sentence/fragment "ぶんこ", :fragment/annotation {:annotation/type :ruby, :ruby/reading "文庫"}}]}]}]} #:document{:paragraphs [#:paragraph{:tags #{}, :sentences [#:sentence{:tags #{}, :annotated-text ["青空文庫" #:annotation{:type :annotation-2, :content "「青空文庫」に傍点"}]}]}]} #:document{:paragraphs [#:paragraph{:tags #{}, :sentences [#:sentence{:tags #{}, :annotated-text "近代の物理科学は、自然を研究するための道具として五官の役割をなるべく切り詰め自然を記載する言葉の中からあらゆる人間的なものを削除する事を目標として進んで来た。"} #:sentence{:tags #{}, :annotated-text "そうしてその意図はある程度までは遂げられたように見える。"} #:sentence{:tags #{}, :annotated-text "この「anthropomorphism からの解放」という合い言葉が合理的でまた目的にかなうものだということは、この旗じるしを押し立てて進んで来た近代科学の収穫の豊富さを見ても明白である。"} #:sentence{:tags #{}, :annotated-text "しかし、それがただの夢であることは自明的である。"} #:sentence{:tags #{}, :annotated-text ["五官を" {:sentence/fragment "杜絶", :fragment/annotation {:annotation/type :ruby, :ruby/reading "とぜつ"}} "すると同時に人間は無くなり、従って世界は無くなるであろう。"]} #:sentence{:tags #{}, :annotated-text ["しかし、この、近代科学から見放された人間の感覚器を子細に研究しているものの目から見ると、これらの器官の機構は、あらゆる科学の粋を集めたいかなる器械と比べても到底比較にならないほど" {:sentence/fragment "精緻", :fragment/annotation {:annotation/type :ruby, :ruby/reading "せいち"}} "をきわめたものである。"]}]} #:paragraph{:tags #{}, :sentences [#:sentence{:tags #{}, :annotated-text "思うに五官の認識の方法は一面分析的であると同時にまた総合的である。"} #:sentence{:tags #{}, :annotated-text "たとえば耳は音響を調和分析にかける。"} #:sentence{:tags #{}, :annotated-text ["そうして、めんどうな積分的計算をわれわれの無意識の間に安々と仕上げて、音の成分を認識すると同時に、またそれを総合した" {:sentence/fragment "和弦", :fragment/annotation {:annotation/type :ruby, :ruby/reading "かげん"}} "や不協和音を一つの全体として認識する。"]}]} #:paragraph{:tags #{}, :sentences [#:sentence{:tags #{}, :annotated-text ["これはしかし、修練による人間そのものの進化によって救われないものであろうか、要するに観測器械としての感官を生理的心理的効果の" {:sentence/fragment "係蹄", :fragment/annotation {:annotation/type :ruby, :ruby/reading "けいてい"}} "から解放することが、ここに予想される総合的実験科学への歩みを進めるために通過すべき第一関門であろうと思われる。"]}]} #:paragraph{:tags #{}, :sentences [#:sentence{:tags #{}, :annotated-text [#:annotation{:type :annotation-2, :content "地から３字上げ"} "（昭和八年八月、科学）"]}]}]}])

(deftest simple-annotation-tests
  (let [feature-string "A［＃B］C｜D《E》F※［＃1-86-29］G［H］I｜J《K》L"
        parsed-sentence (aozora-annotation->tags feature-string)
        parsed-doc (parse-text feature-string)
        test ["A"
              #:annotation{:type :annotation-2, :content "B"}
              "C"
              {:sentence/fragment "D", :fragment/annotation {:annotation/type :ruby, :ruby/reading "E"}}
              "F欞G"
              #:annotation{:type :annotation, :content "H"}
              "I"
              {:sentence/fragment "J", :fragment/annotation {:annotation/type :ruby, :ruby/reading "K"}}
              "L"]]
    (is (= test parsed-sentence))
    (is (schema-valid :sentence/annotated-text parsed-sentence))
    (is (schema-valid :document/body parsed-doc))))

(deftest parse-annotations-tests
  (doseq [s (drop-last test-strings)]
    (is (schema-valid :sentence/annotated-text (aozora-annotation->tags (string/trim s))))))

;; Skipped: Aozora text parsing moves out of Clojure; consumed as JSON AST
;; from an external parser per the parser-IR contract.
(deftest ^:kaocha/skip parse-aozora-text
  (is (= (count test-strings) (count parsed-strings)))
  (doseq [[s s-gold] (map vector test-strings parsed-strings)]
    (let [s-parsed (parse-text s)]
      (is (= s-gold (clojure.walk/prewalk
                     (fn [x]
                       (if (map? x)
                         (dissoc x :document/metadata :sentence/tokens :sentence/text)
                         x))
                     s-parsed)))
      (is (schema-validate :document/body s-parsed)))))

(deftest parse-aozora-plaintext
  (doseq [s test-strings]
    (is (schema-valid :string (doc->plaintext (parse-text s))))))

#_(deftest parse-tests
    (is (map parse-bnf test-strings)))
