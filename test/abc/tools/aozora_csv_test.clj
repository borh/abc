(ns abc.tools.aozora-csv-test
  (:require [abc.tools.aozora-csv :as ac]
            [clojure.test :refer [deftest is testing]]))

(def ^:private header-line
  (str "﻿作品ID,作品名,作品名読み,ソート用読み,副題,副題読み,原題,初出,"
       "分類番号,文字遣い種別,作品著作権フラグ,公開日,最終更新日,図書カードURL,"
       "人物ID,姓,名,姓読み,名読み,姓読みソート用,名読みソート用,"
       "姓ローマ字,名ローマ字,役割フラグ,生年月日,没年月日,人物著作権フラグ,"
       "底本名1,底本出版社名1,底本初版発行年1,入力に使用した版1,校正に使用した版1,"
       "底本の親本名1,底本の親本出版社名1,底本の親本初版発行年1,"
       "底本名2,底本出版社名2,底本初版発行年2,入力に使用した版2,校正に使用した版2,"
       "底本の親本名2,底本の親本出版社名2,底本の親本初版発行年2,"
       "入力者,校正者,テキストファイルURL,テキストファイル最終更新日,"
       "テキストファイル符号化方式,テキストファイル文字集合,テキストファイル修正回数,"
       "XHTML/HTMLファイルURL,XHTML/HTMLファイル最終更新日,"
       "XHTML/HTMLファイル符号化方式,XHTML/HTMLファイル文字集合,"
       "XHTML/HTMLファイル修正回数"))

(def ^:private rashomon-row
  (str
   "\"000127\",\"羅生門\",\"らしょうもん\",\"らしようもん\",\"\",\"\",\"\","
   "\"「帝国文学」1915（大正4）年11月号\",\"NDC 913\",\"新字新仮名\","
   "\"なし\",1997-10-29,2022-07-16,"
   "\"https://www.aozora.gr.jp/cards/000879/card127.html\",\"000879\","
   "\"芥川\",\"竜之介\",\"あくたがわ\",\"りゅうのすけ\","
   "\"あくたかわ\",\"りゆうのすけ\",\"Akutagawa\",\"Ryunosuke\","
   "\"著者\",\"1892-03-01\",\"1927-07-24\",\"なし\","
   "\"芥川龍之介全集1\",\"ちくま文庫、筑摩書房\","
   "\"1986（昭和61）年9月24日\",\"1997（平成9）年4月15日第14刷\",\"\","
   "\"筑摩全集類聚　芥川龍之介全集第一巻\",\"筑摩書房\","
   "\"1971（昭和46）年3月5日\",\"\",\"\","
   "\"\",\"\",\"\",\"\",\"\",\"\","
   "\"野口英司、平山誠\",\"もりみつじゅんじ\","
   "\"https://www.aozora.gr.jp/cards/000879/files/127_ruby_150.zip\","
   "\"2022-07-16\",\"ShiftJIS\",\"JIS X 0208\",\"4\","
   "\"https://www.aozora.gr.jp/cards/000879/files/127_15260.html\","
   "\"2022-07-16\",\"ShiftJIS\",\"JIS X 0208\",\"2\""))

(def ^:private csv-text
  (str header-line "\n" rashomon-row "\n"))

(deftest read-rows-strips-bom-and-keys-by-header-test
  (testing "read-rows returns column-name-keyed maps; BOM stripped"
    (let [rows (ac/read-rows-from-string csv-text)]
      (is (= 1 (count rows)))
      (is (= "000127" (get (first rows) "作品ID"))
          "BOM must not infect the first column key")
      (is (= "芥川" (get (first rows) "姓"))))))

(deftest parse-work-fields-test
  (testing "parse-work-fields-from-row maps Aozora columns to JSON shape"
    (let [row (first (ac/read-rows-from-string csv-text))
          work (ac/parse-work-fields-from-row row)]
      (is (= "000127" (get work "work_id")))
      (is (= "羅生門" (get work "title")))
      (is (= "らしょうもん" (get work "title_reading")))
      (is (= "NDC 913" (get work "ndc")))
      (is (= "新字新仮名" (get work "orthographic_style")))
      (is (true? (get work "copyright_expired")))
      (is (= "1997-10-29" (get work "aozora_available")))
      (is (= "2022-07-16" (get work "aozora_modified")))
      (is (= "https://www.aozora.gr.jp/cards/000879/card127.html"
             (get work "card_url")))
      (is (vector? (get work "source_editions")))
      (is (= "芥川龍之介全集1" (get-in work ["source_editions" 0 "title"])))
      (is (= "ちくま文庫、筑摩書房" (get-in work ["source_editions" 0 "publisher"]))))))

(deftest parse-person-fields-test
  (testing "parse-person-fields-from-row maps person columns + role"
    (let [row (first (ac/read-rows-from-string csv-text))
          person (ac/parse-person-fields-from-row row)]
      (is (= "000879" (get person "person_id")))
      (is (= "芥川" (get person "family_name")))
      (is (= "竜之介" (get person "given_name")))
      (is (= "あくたがわ" (get person "family_name_reading")))
      (is (= "りゅうのすけ" (get person "given_name_reading")))
      (is (= "Akutagawa" (get person "family_name_romaji")))
      (is (= "Ryunosuke" (get person "given_name_romaji")))
      (is (= "著者" (get person "relation_to_work")))
      (is (= "1892-03-01" (get person "date_of_birth")))
      (is (= "1927-07-24" (get person "date_of_death")))
      (is (true? (get person "person_copyright_expired")))
      (is (= [] (get person "external_links"))))))

(deftest build-record-fragment-test
  (testing "build-record-fragment-from-rows produces {:work :persons} from one row"
    (let [rows (ac/read-rows-from-string csv-text)
          {:keys [work persons]} (ac/build-record-fragment-from-rows rows)]
      (is (= "000127" (get work "work_id")))
      (is (= 1 (count persons)))
      (is (= "000879" (get (first persons) "person_id"))))))
