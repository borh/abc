(ns soranoha.yomi.select-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [soranoha.aozora.csv :as csv]
            [soranoha.yomi.catalog :as catalog]
            [soranoha.yomi.select :as select])
  (:import [java.io FileOutputStream]
           [java.util.zip ZipEntry ZipOutputStream]))

(defn- write-zip! [path entries]
  (fs/create-dirs (fs/parent path))
  (with-open [out (ZipOutputStream. (FileOutputStream. (str path)))]
    (doseq [[name ^String content] entries]
      (.putNextEntry out (ZipEntry. ^String name))
      (.write out (.getBytes content "UTF-8"))
      (.closeEntry out))))

(def csv-header
  "作品ID,人物ID,テキストファイルURL,作品著作権フラグ")

(defn- fixture-root! []
  (let [root (fs/create-temp-dir {:prefix "yomi-select-test"})]
    (write-zip! (fs/path root "cards" "000001" "files" "100_ruby_200.zip")
                [["100_ruby_200.txt" "本文"]])
    ;; A work whose copyright subsists and whose colophon licenses it, which
    ;; is admitted on the licence rather than on the flag.
    (write-zip! (fs/path root "cards" "000002" "files" "300_ruby_400.zip")
                [["300_ruby_400.txt"
                  (str "本文\n※本作品は「クリエイティブ・コモンズ 表示 4.0 国際 "
                       "ライセンス」（https://creativecommons.org/licenses/by/4.0/）"
                       "の下に提供されています。")]])
    ;; The same, under terms the release cannot honour.
    (write-zip! (fs/path root "cards" "000004" "files" "500_ruby_600.zip")
                [["500_ruby_600.txt"
                  (str "本文\nhttps://creativecommons.org/licenses/by-nc-nd/4.0/")]])
    ;; The same again, stating no terms at all.
    (write-zip! (fs/path root "cards" "000005" "files" "700_ruby_800.zip")
                [["700_ruby_800.txt" "本文\n底本：ある本"]])
    ;; A work zip with no catalog row, and a zip outside the work shape.
    (write-zip! (fs/path root "cards" "000003" "files" "999_ruby_999.zip")
                [["999_ruby_999.txt" "本文"]])
    (write-zip! (fs/path root "cards" "000001" "misc.zip")
                [["x.txt" "x"]])
    root))

(def fixture-rows
  (csv/read-rows-from-string
   (str csv-header "\n"
        "000100,000001,https://example.org/cards/000001/files/100_ruby_200.zip,なし\n"
        "000300,000002,https://example.org/cards/000002/files/300_ruby_400.zip,あり\n"
        "000500,000004,https://example.org/cards/000004/files/500_ruby_600.zip,あり\n"
        "000700,000005,https://example.org/cards/000005/files/700_ruby_800.zip,あり\n")))

(deftest selection-join-test
  (let [root (fixture-root!)
        {:keys [candidates rejected]} (select/select-candidates root fixture-rows)]
    (testing "catalog-joined work zips selected in relpath order with slugs"
      (is (= ["cards/000001/files/100_ruby_200.zip"
              "cards/000002/files/300_ruby_400.zip"]
             (mapv :relpath candidates)))
      (is (= ["000100_000001"
              "000300_000002"]
             (mapv :slug candidates))))
    (testing "each admitted work carries the standing it is published under"
      (is (= ["public-domain" "CC-BY-4.0"] (mapv :rights candidates))))
    (testing "non-catalog and non-work-shape zips are rejected with reasons"
      (is (= {"cards/000003/files/999_ruby_999.zip" "not-catalog-text-zip"
              "cards/000001/misc.zip" "not-under-cards-files"
              "cards/000004/files/500_ruby_600.zip" "rights-restricted-licence"
              "cards/000005/files/700_ruby_800.zip" "rights-unstated-licence"}
             (into {} (map (juxt #(get % "path") #(get % "reason")))
                   rejected))))
    (testing "a refusal names the terms it declined"
      (is (= "CC-BY-NC-ND-4.0"
             (some #(when (= "cards/000004/files/500_ruby_600.zip" (get % "path"))
                      (get % "licence"))
                   rejected))))))

(deftest slug-function-test
  (testing "work id and card directory only; the archive stem is not identity"
    (is (= "000100_000123"
           (select/slug "000100" "cards/000123/files/100_ruby_200.zip")))
    (testing "two format variants of one work under one card are one identifier"
      (is (= (select/slug "000100" "cards/000123/files/100_ruby_200.zip")
             (select/slug "000100" "cards/000123/files/100_txt_201.zip")))))
  (testing "both components fail closed rather than being guessed"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"names no card directory"
                          (select/slug "000100" "elsewhere/100.zip")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not the six-digit form"
                          (select/slug "100" "cards/000123/files/100_ruby_200.zip")))))

(deftest injectivity-assert-test
  (let [claim {:row {"作品ID" "000100" "人物ID" "000001"}
               :relpath "cards/000001/files/100_ruby_200.zip"}]
    (testing "duplicate slug claims throw before any slug-addressed write"
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo #"duplicate publication slugs"
           (select/assert-candidate-slugs-unique! [claim claim]))))
    (testing "distinct claims pass through"
      (is (= [claim]
             (select/assert-candidate-slugs-unique! [claim]))))))

(deftest catalog-parsing-test
  (testing "BOM stripped from header; ragged rows marked, not dropped"
    (let [rows (csv/read-rows-from-string
                (str "﻿" csv-header "\n"
                     "1,2,https://example.org/f/a.zip\n"
                     "3,4\n"))]
      (is (= "1" (catalog/row-work-id (first rows))))
      (is (= "a.zip" (catalog/text-url-basename (first rows))))
      (is (true? (get (second rows) csv/ragged-key))))))
