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
