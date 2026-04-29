(ns abc.html-test
  (:require [clojure.test :as t :refer [deftest testing is use-fixtures]]
            [babashka.fs :as fs]
            [babashka.process :refer [process check sh pipeline pb]]))

;; Skipped: Aozora text parsing moves out of Clojure; consumed as JSON AST
;; from an external parser per the parser-IR contract.
(deftest ^:kaocha/skip aozora2html-test
  (testing "serialization"))
