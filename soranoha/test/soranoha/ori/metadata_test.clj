(ns soranoha.ori.metadata-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is]]
            [soranoha.main :as main]
            [soranoha.za.corpus :as corpus]))

(deftest selected-ragged-row-is-refused-before-the-metadata-build
  (let [work {:work-id "000100" :person-id "000001" :card "000001"
              :title "試験" :text "本文"}
        root (corpus/init-corpus! [work])
        store (fs/create-temp-dir {:prefix "metadata-rejection"})]
    (try
      (corpus/write-zip! (fs/path root "index_pages" "list_person_all_extended_utf8.zip")
                         [["catalog.csv"
                           (str "作品ID,人物ID,役割フラグ,作品名,テキストファイルURL,姓\n"
                                "000100,000001,著者,試験,https://example.org/cards/000001/files/"
                                (corpus/work-basename work) ".zip\n")]])
      (corpus/commit-corpus! root)
      (is (= :ragged-metadata-row
             (try (main/build! {:root (str store) :aozora-root root
                                :assets-root "unused" :clj-toolchain-id "test" :concurrency 1})
                  :accepted
                  (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
      (finally (fs/delete-tree root) (fs/delete-tree store)))))
