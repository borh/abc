(ns soranoha.aozora.csv-test
  "The boundary where Aozora Bunko's catalog becomes a work record. What is
  tested here is what the catalog carries that a published field must not."
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [soranoha.aozora.csv :as csv]))

(def ^:private row
  {"作品ID" "001790"
   "作品名" "宿命"
   "作品名読み" "しゅくめい"
   "ソート用読み" "しゆくめい"
   "副題" ""
   "副題読み" ""
   "原題" ""
   "初出" "「赤い鳥」1918（大正7）年7月"
   "分類番号" "NDC 913"
   "文字遣い種別" "新字新仮名"
   "作品著作権フラグ" "なし"
   "公開日" "2000-01-01"
   "最終更新日" "2000-01-01"
   "図書カードURL" "https://www.aozora.gr.jp/cards/000067/card1790.html"})

(deftest a-single-first-publication-statement-is-carried-as-it-stands
  (is (= "「赤い鳥」1918（大正7）年7月"
         (get (csv/parse-work-fields-from-row row) "first_published"))))

(deftest several-first-publication-statements-become-several-lines
  ;; Aozora Bunko separates them with a literal <br>, which is markup from
  ;; the card pages its CSV is built alongside. 166 rows carry it.
  (let [many (assoc row "初出" "ああ固い氷を破つて「新しき欲情」1922年4月刊<br>婦人と雨「新しき欲情」1922年4月刊")
        value (get (csv/parse-work-fields-from-row many) "first_published")]
    (is (= ["ああ固い氷を破つて「新しき欲情」1922年4月刊"
            "婦人と雨「新しき欲情」1922年4月刊"]
           (string/split-lines value)))
    (testing "and no work record carries the tag"
      (is (not (string/includes? value "<"))))))

(deftest the-tag-is-read-however-the-catalog-writes-it
  (doseq [tag ["<br>" "<br/>" "<br />"]]
    (testing tag
      (is (= 2 (count (string/split-lines
                       (get (csv/parse-work-fields-from-row (assoc row "初出" (str "甲" tag "乙")))
                            "first_published"))))))))

(deftest a-separator-with-nothing-between-two-of-them-adds-no-statement
  (let [value (get (csv/parse-work-fields-from-row (assoc row "初出" "甲<br><br> <br>乙"))
                   "first_published")]
    (is (= ["甲" "乙"] (string/split-lines value))))
  (testing "and a field holding only separators is empty rather than blank"
    (is (nil? (get (csv/parse-work-fields-from-row (assoc row "初出" "<br> <br>"))
                   "first_published")))))

(deftest an-empty-first-publication-column-stays-null
  (is (nil? (get (csv/parse-work-fields-from-row (assoc row "初出" "")) "first_published"))))
