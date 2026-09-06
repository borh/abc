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
  "作品ID,人物ID,テキストファイルURL")

(defn- fixture-root! []
  (let [root (fs/create-temp-dir {:prefix "yomi-select-test"})]
    (write-zip! (fs/path root "cards" "000001" "files" "100_ruby_200.zip")
                [["100_ruby_200.txt" "本文"]])
    (write-zip! (fs/path root "cards" "000002" "files" "300_ruby_400.zip")
                [["300_ruby_400.txt" "本文"]])
    ;; A work zip with no catalog row, and a zip outside the work shape.
    (write-zip! (fs/path root "cards" "000003" "files" "999_ruby_999.zip")
                [["999_ruby_999.txt" "本文"]])
    (write-zip! (fs/path root "cards" "000001" "misc.zip")
                [["x.txt" "x"]])
    root))

(def fixture-rows
  (csv/read-rows-from-string
   (str csv-header "\n"
        "000100,000001,https://example.org/cards/000001/files/100_ruby_200.zip\n"
        "000300,000002,https://example.org/cards/000002/files/300_ruby_400.zip\n")))

(deftest selection-join-test
  (let [root (fixture-root!)
        {:keys [candidates rejected]} (select/select-candidates root fixture-rows)]
    (testing "catalog-joined work zips selected in relpath order with slugs"
      (is (= ["cards/000001/files/100_ruby_200.zip"
              "cards/000002/files/300_ruby_400.zip"]
             (mapv :relpath candidates)))
      (is (= ["000100_000001_000001_100_ruby_200"
              "000300_000002_000002_300_ruby_400"]
             (mapv :slug candidates))))
    (testing "non-catalog and non-work-shape zips are rejected with reasons"
      (is (= {"cards/000003/files/999_ruby_999.zip" "not-catalog-text-zip"
              "cards/000001/misc.zip" "not-under-cards-files"}
             (into {} (map (juxt #(get % "path") #(get % "reason")))
                   rejected))))))

(deftest slug-function-test
  (is (= "000100_000001_000123_100_ruby_200"
         (select/slug "000100" "000001" "cards/000123/files/100_ruby_200.zip")))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"names no card directory"
                        (select/slug "1" "2" "elsewhere/100.zip"))))

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
