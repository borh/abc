(ns abc.tools.json-test
  (:require [abc.tools.json :as abc-json]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :refer [deftest is]]))

(deftest write-deterministic-json-file-removes-trailing-whitespace-test
  (let [file (java.io.File/createTempFile "abc-json" ".json")]
    (try
      (abc-json/write-deterministic-json-file! file {"object" {"empty" {}}})
      (is (not-any? #(re-find #"[ \t]+$" %)
                    (string/split-lines (slurp file))))
      (finally
        (.delete (io/file file))))))

(deftest write-deterministic-json-str-matches-file-writer-test
  (let [value {"b" 2 "a" [3 1] "nested" {"z" 1 "y" 2}}
        file (java.io.File/createTempFile "abc-json-str" ".json")]
    (try
      (abc-json/write-deterministic-json-file! file value)
      (is (= (str (abc-json/write-deterministic-json-str value) "\n")
             (slurp file)))
      (finally
        (.delete (io/file file))))))

(deftest read-json-str-round-trips-test
  (is (= {"a" 1 "b" [2 3]}
         (abc-json/read-json-str
          (abc-json/write-deterministic-json-str {"a" 1 "b" [2 3]})))))

(deftest jsonl-line-is-single-line-and-sorted-test
  (let [line (abc-json/write-deterministic-jsonl-line {"b" 2 "a" 1})]
    (is (not (re-find #"\n" line)))
    (is (= {"a" 1 "b" 2} (abc-json/read-json-str line)))))
