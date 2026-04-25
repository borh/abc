(ns abc.load-test
  (:require [clojure.test :as t :refer [deftest testing is use-fixtures]]
            [abc.test-utils :refer :all]
            [abc.load :as load :refer :all]
            [abc.config :as config]))

(def ^:dynamic ^:private *db* nil)

(defn db-fixture [f]
  (let [db (aozora-bunko-db config/aozora-bunko-path)
        #_(->
            (update :works (fn [m] (reduce (fn [a [k v]] (assoc a k v)) {} (take 10000 m))))
            (update :persons (fn [m] (reduce (fn [a [k v]] (assoc a k v)) {} (take 10000 m)))))]
    (binding [*db* db]
      (f))))

(use-fixtures :once db-fixture)

(deftest load-test
  (testing "Loading AB database fixture"
    (is *db*)
    (is (= (set (keys *db*)) #{:works :persons})))
  (testing "Database schema"
    ;; A full database validation is slow
    (is (schema-valid :abc.aozora/db-entries *db* abc.aozora/registry))))

(deftest extract-texts-test
  (testing "Extracting texts from database fixture"
    ;; Good testcase:
    ;; :abc.aozora/w043688
    #_(is (schema-validate :document/body :TODO))
    #_(doseq [doc (take 10 (extract-texts *db*))]
        (is (schema-valid :document/body doc abc.annotation/registry)
            #_(nil? (dorun (extract-texts *db*)))))))
