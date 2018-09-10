(ns abc.annotation-test
  (:require [abc.annotation :refer :all]
            [clojure.test :as t :refer [deftest is use-fixtures]]
            [clojure.spec.test.alpha :as stest]
            [clojure.spec.alpha :as s]
            [orchestra.spec.test :as st]
            [expound.alpha :as expound]))

(alter-var-root #'s/*explain-out* (constantly expound/printer))

(st/instrument)

(stest/check (stest/enumerate-namespace 'abc.annotation))

(def ^:dynamic ^:private *parser* nil)

(defn fixture [f]
  (binding []
    (f)))

(use-fixtures :once fixture)

(def test-strings
  ["青空文庫《あおぞらぶんこ》\n"
   "青空|ぶんこ《文庫》\n"
   "青空文庫[#「青空文庫」に傍点]\n"
   "感覚と科学\n寺田寅彦\n\n-------------------------------------------------------\n【テキスト中に現れる記号について】\n\n《》：ルビ\n（例）杜絶《とぜつ》する\n\n［＃］：入力者注　主に外字の説明や、傍点の位置の指定\n（例）［＃地から３字上げ］（昭和八年八月、科学）\n-------------------------------------------------------\n\n　近代の物理科学は、自然を研究するための道具として五官の役割をなるべく切り詰め自然を記載する言葉の中からあらゆる人間的なものを削除する事を目標として進んで来た。そうしてその意図はある程度までは遂げられたように見える。この「anthropomorphism からの解放」という合い言葉が合理的でまた目的にかなうものだということは、この旗じるしを押し立てて進んで来た近代科学の収穫の豊富さを見ても明白である。科学はたよりない人間の官能から独立した「科学的客観的人間」の所得となって永遠の落ちつき所に安置されたようにも見える。\n　われわれ「生理的主観的人間」は目も耳も指も切り取って、あらゆる外界との出入り口をふさいで、そうして、ただ、生きていることと、考えることとだけで科学を追究し、自然を駆使することができるのではないかという空想さえいだかせられる恐れがある。しかし、それがただの夢であることは自明的である。五官を杜絶《とぜつ》すると同時に人間は無くなり、従って世界は無くなるであろう。しかし、この、近代科学から見放された人間の感覚器を子細に研究しているものの目から見ると、これらの器官の機構は、あらゆる科学の粋を集めたいかなる器械と比べても到底比較にならないほど精緻《せいち》をきわめたものである。これほど精巧な器械を捨てて顧みないのは誠にもったいないような気がする。この天成の妙機を捨てる代わりに、これを活用してその長所を発揮するような、そういう「科学の分派」を設立することは不可能であろうか。こういう疑問を起こさないではいられないほどにわれわれの感覚器官はその機構の巧妙さによってわれわれを誘惑するのである。もしも、そういう学問の分派が可能だとすれば、それはどういう方面にその領域を求めるべきであろうか。この問題より前にまず五官による認識の本質的特徴に注目する必要がある。\n　思うに五官の認識の方法は一面分析的であると同時にまた総合的である。たとえば耳は音響を調和分析にかける。そうして、めんどうな積分的計算をわれわれの無意識の間に安々と仕上げて、音の成分を認識すると同時に、またそれを総合した和弦《かげん》や不協和音を一つの全体として認識する。また目は、たとえば、リヒテンベルグの陽像と陰像とを一瞬時に識別する。これを客観的に識別しようとすればめんどうな分析法によって多数の係数を算出し、さらにそれを統計にかけて表示しなければならない。さらにまた、盲人の触感は猫《ねこ》の毛の「光沢」を識別し、贋造紙幣《がんぞうしへい》を「発見」する。しかし、物の表面の「粗度」の物理的研究はまだ揺籃《ようらん》時代を過ぎない。これほどに有力な感官の分析総合能力が捨てて顧みられない一つの理由は、その与えるデータが数量的でないためである。しかし、数量的のデータを与える事が必ずしも不可能とは思われない。適当なスケールさえ作ればこれは可能になる。たとえばピアノの鍵盤《けんばん》や、オストワルドの色見本は、言わばそういう方向への最初の試歩である。金相学上の顕微鏡写真帳も、そういうスケールを作るための素材の堆積《たいせき》であるとも言われよう、もし、あの複雑な模様を調和分析にかけた上で、これにさらに統計的分析を加えれば、系統的な分類に基づくスケールを設定することも、少なくも原理的には可能である。これにやや近いものを求めれば、指紋鑑別のスケールのごときものがそれである。「あたわざるにあらず、成さざるなり」と言ってもさしつかえはないであろう。\n　それはとにかく、感官のもう一つの弱点は、個人個人による多少の差別の存在である。しかし、われわれは「考える器械」としての個人性を科学の上に認めている。「見る器械」、「聞く器械」としての優劣の存在を許容するのもやむを得まい。高価な器械を持つ人と、粗製の器械をもつ人との相違と本質的に同じとも言われる。多くのすぐれた器械の結果が互いに一致し、そうしてその結果が全系統に適合する時に、その結果を「事実」と名づけることがいけなければ、科学はその足場を失うであろう。\n　もう一つの困難は、感官の「読み取り」が生理的心理的効果と結びついて、いろいろな障害を起こす心配のあるということである。これはしかし、修練による人間そのものの進化によって救われないものであろうか、要するに観測器械としての感官を生理的心理的効果の係蹄《けいてい》から解放することが、ここに予想される総合的実験科学への歩みを進めるために通過すべき第一関門であろうと思われる。\n［＃地から３字上げ］（昭和八年八月、科学）\n\n\n\n底本：「寺田寅彦随筆集　第四巻」小宮豊隆編、岩波文庫、岩波書店\n　　　1948（昭和23）年5月15日第1刷発行\n　　　1963（昭和38）年5月16日第20刷改版発行\n　　　1997（平成9）年6月13日第65刷発行\n※底本の誤記等を確認するにあたり、「寺田寅彦全集」（岩波書店）を参照しました。\n入力：(株)モモ\n校正：かとうかおり\n2000年10月3日公開\n2003年10月30日修正\n青空文庫作成ファイル：\nこのファイルは、インターネットの図書館、青空文庫（http://www.aozora.gr.jp/）で作られました。入力、校正、制作にあたったのは、ボランティアの皆さんです。\n"])

(deftest parse-tests
  (is (map parse-bnf test-strings)))
