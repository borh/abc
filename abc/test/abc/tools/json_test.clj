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

(deftest write-deterministic-json-file-is-atomic-under-concurrency-test
  (let [dir (java.io.File/createTempFile "abc-json-atomic" "")
        _ (.delete dir)
        _ (.mkdirs dir)
        file (io/file dir "person.json")
        value-a {"person_id" "000879" "name" (apply str (repeat 5000 "あ"))}
        value-b {"person_id" "000879" "name" (apply str (repeat 5000 "い"))}
        expected #{(abc-json/write-deterministic-json-str value-a)
                   (abc-json/write-deterministic-json-str value-b)}
        stop (promise)
        writers (mapv (fn [value]
                        (future
                          (dotimes [_ 100]
                            (abc-json/write-deterministic-json-file! file value))))
                      [value-a value-b])
        reader (future
                 (loop [seen []]
                   (if (realized? stop)
                     seen
                     (recur (if (.exists file)
                              (conj seen (string/trimr (slurp file)))
                              seen)))))]
    (try
      (run! deref writers)
      (deliver stop true)
      (let [observed @reader]
        (is (seq observed) "reader must have observed at least one read")
        (is (every? expected observed)
            "every observed read must be one complete value, never a torn mix"))
      ;; no leftover temp files
      (is (= ["person.json"] (mapv #(.getName %) (.listFiles (io/file dir)))))
      ;; the final file is world-readable like a plain io/writer file
      (let [perms (java.nio.file.Files/getPosixFilePermissions
                   (.toPath file)
                   (make-array java.nio.file.LinkOption 0))]
        (is (contains? perms java.nio.file.attribute.PosixFilePermission/OTHERS_READ)))
      (finally
        (run! #(.delete %) (.listFiles (io/file dir)))
        (.delete dir)))))
