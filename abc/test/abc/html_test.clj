(ns abc.html-test
  (:require [clojure.test :refer [deftest testing]]))

;; Skipped: Aozora text parsing moves out of Clojure; consumed as JSON AST
;; from an external parser per the parser-IR contract.
(deftest ^:kaocha/skip aozora2html-test
  (testing "serialization"))
