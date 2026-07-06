(ns abc.text-test
  (:require [abc.stats :as stats]
            [abc.text :as text]
            [clojure.test :refer [deftest is testing]]))

(deftest split-japanese-sentence-test
  (testing "splits Japanese sentence delimiters"
    (is (= ["吾輩は猫である。" "名前はまだ無い。"]
           (text/split-japanese-sentence "吾輩は猫である。名前はまだ無い。"))))
  (testing "does not split decimal points"
    (is (= ["これは3.14です。" "終わり。"]
           (text/split-japanese-sentence "これは3.14です。終わり。"))))
  (testing "does not split before closing quotation"
    (is (= ["彼は言った。）次。"]
           (text/split-japanese-sentence "彼は言った。）次。"))))
  (testing "keeps adjacent delimiters together"
    (is (= ["え！？" "本当。"]
           (text/split-japanese-sentence "え！？本当。")))))

(deftest normalization-test
  (is (= "ABC123"
         (text/normalize-nfkc "ＡＢＣ１２３")))
  (is (= "ＡＢＣ１２３"
         (text/convert-half-to-fullwidth "ABC123"))))

(deftest compute-text-splits-lines-into-sentence-lengths-test
  (testing "compute-text returns per-line token counts as sentence-lengths"
    (let [result (stats/compute-text {"猫" [#:mecab.features{:orth "token"}]
                                      "犬" [#:mecab.features{:orth "token"}]})]
      (is (= 1 (get-in result ["猫" :sentence-lengths])))
      (is (= 1 (get-in result ["犬" :sentence-lengths]))))))
