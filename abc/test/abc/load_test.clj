(ns abc.load-test
  (:require [clojure.test :as t :refer [deftest testing is use-fixtures]]
            [abc.test-utils :refer :all]
            [abc.tools.malli :as am]
            [abc.load :as load :refer :all]
            [abc.config :as config]))

(def ^:dynamic ^:private *db* nil)

(defn db-fixture [f]
  (let [db (aozora-bunko-db config/aozora-bunko-path)]
    (binding [*db* db]
      (f))))

(use-fixtures :once
  (fn [f] (am/install!) (f))
  db-fixture)

;; Skipped in CI: body needs the aozora-bunko DB zip on disk (sandbox lacks it).
(deftest ^:kaocha/skip load-test
  (testing "Loading AB database fixture"
    (is *db*)
    (is (= (set (keys *db*)) #{:works :persons})))
  (testing "Database schema"
    ;; A full database validation is slow
    (is (schema-valid :abc.aozora/db-entries *db*))))

;; Skipped: Aozora text parsing moves out of Clojure; consumed as JSON AST
;; from an external parser per the parser-IR contract.
(deftest ^:kaocha/skip extract-texts-test
  (testing "Extracting texts from database fixture"
    #_(is (schema-validate :document/body :TODO))))
